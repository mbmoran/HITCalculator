##################################################################################################
## Shiny App server.R
## HITCalculator v1.1
##
## Changes to v1.0 include:
## 1.  Added code for reading/cleaning raw data file from the CDC and Census website
## 2.  Add new vars to data frame for 95% CI intervals around MMR vaccine rates: Low, Med, High
## 3.  Add New bubble plot showing state=bubble, size=population rank, color above/below HIT thresh
##
##################################################################################################
# Steps 1-3 - Load required R packages, Read in raw data files, clean create tidy data frames
##################################################################################################

# Step 1 - Load required R packages, Read in raw data file, assign to data frame
  library(shiny); library(maptools); library(classInt); 
  library(RColorBrewer); library(ggplot2); library(dplyr); library(colorspace)
  library(utility); library(downloader); library(tools); require(xlsx)

# Step 2 - Download raw Excel file from the CDC
  downloaded.file<-"tab03_antigen_state_2013.xls"

# NOTE: this section is commented out for purposes of deployment onto RStudio's servers. To run the
# code which downloads the raw data file, uncomment the following 4 lines:

  # URL <- "http://www2a.cdc.gov/nip/coverage/nis/CountNIS.asp?fmt=v&rpt=tab03_antigen_state_2013.xlsx&qtr=Q1/2013-Q4/2013"
  # extracted.data <- download(URL, downloaded.file, mode="wb")     # Download Excel file from CDC site
  # URL2<-"https://www.census.gov/popest/data/state/asrh/2013/files/SC-EST2013-AGESEX-CIV.csv"
  # extracted.data2 <- download(URL2, downloaded.Census, mode="wb") # Download csv from Census Bureau site

# Step 3 - Create clean, tidy data frame tf remove all columns except MMR and remove row 9 Dist. of Columbia
  readExcel.data <- read.xlsx(downloaded.file,sheetIndex=1,startRow=4,endRow=55)
  tf<-as.data.frame(readExcel.data)    
  tf<-tf[,c(1,5)]
  tf<-tf[-9,]
  colnames(tf)<-c("STATE_NAME","num")
  tf$STATE_NAME<-as.character(tf$STATE_NAME)
  tf$num<-as.character(tf$num)
  row.names(tf)<-NULL

# Step 3.1 - Create new columns for low, med and high values of MMR vaccination levels using the CI given in the raw data
# sigma calculated from the 95% 2-tailed CI, assume robust sample sizes (average size is 266)
  library(stringr)
  split.num<-str_split_fixed(tf$num, substr(tf[,2],5,6), 2)
  tf$interval<-as.numeric(split.num[,2])
  med<-as.numeric(split.num[,1])
  tf$lowCI<-med-tf$interval
  tf$med<-as.numeric(split.num[,1])
  tf$highCI<-tf$med+tf$interval
  tf$sigma<-tf$interval/qnorm(0.975)

# Step 3.2 - Merge US Census Data with CDC Vaccine Data
  downloaded.Census<-"US Census 2013 by age.csv"
  readCsv.Census <- read.csv(downloaded.Census)

  pop<-as.data.frame(readCsv.Census[which(readCsv.Census$SEX>0),])
  row.names(pop)<-NULL
  pop$age1935mos<-round((ifelse(pop$AGE==1,0.50,1)*pop$POPEST2013_CIV)/1000,1)

  pop<-aggregate(age1935mos~NAME+AGE, data=pop, sum, na.rm=TRUE)
  row.names(pop)<-NULL

  pop<-as.data.frame(pop[which(pop$AGE >0 & pop$AGE<3),])
  row.names(pop)<-NULL

  pop<-aggregate(age1935mos~NAME, data=pop, sum, na.rm=TRUE)
  row.names(pop)<-NULL

# Step 3.3 - Remove US total in row 45 and DC in row 9
  pop<-pop[-45,]
  pop<-pop[-9,]
  row.names(pop)<-NULL
  pop$NAME<-as.character(pop$NAME)

##################################################################################################
# Step 4 - Create shinyServer code which feeds the ui.R interface
##################################################################################################

shinyServer(function(input, output) {
    
    output$myPlot <- renderPlot({
        
        r0  <<- as.numeric(input$r0)
        e   <<- as.numeric(input$e)
        hit <- (1-(1/r0))/e
        
        dd <- with(density(tf$med), data.frame(x,y))
        g<-ggplot(data=tf, aes(tf$med),environment = environment()) + 
            geom_histogram(breaks=seq(85, 100, by =0.5), 
                           col="red", 
                           aes(fill=..count..))
        p <- g + 
            scale_x_continuous(breaks=seq(85,100,by=1),limits=c(85,100),name="MMR Vacination Rate (per 100)")+
            scale_y_continuous(breaks = round(seq(0,max(dd$y)*length(tf$med)+1,by=1),0), name="Number of States") +
            geom_vline(aes(xintercept = hit*100), color = "red", linetype="dashed",show_guide=FALSE) +
            geom_text(aes(99, 7, label="Calculated HIT"),show_guide=FALSE,color="red") +
            geom_text(aes(99, 6.5, label=paste(round(hit*100,digits=1))),show_guide=FALSE,color="red") +
            theme_classic()
        print(p)
        
    })
    
    output$myMap <- renderPlot({
        
        library(maptools)
        library(mapproj)
        library(ggplot2)
        
        ### Start  K Kapphahn's code
        centerState <- function(.df) {
            .df$x <- .df$x - (diff(range(.df$x, na.rm = T))/2 + min(.df$x, na.rm = T))
            .df$y <- .df$y - (diff(range(.df$y, na.rm = T))/2 + min(.df$y, na.rm = T))
            return(.df)
        }
        
        scaleState <- function(.df, scale_matrix, scale_factor, x_shift, y_shift) {
            .df <- centerState(.df)
            coords <- t(cbind(.df$x, .df$y))
            scaled_coord <- t(scale_factor*scale_matrix %*% coords)
            
            .df$x <- scaled_coord[,1] + x_shift
            .df$y <- scaled_coord[,2] + y_shift
            return(.df)
        }
        us50_shp <- readShapePoly("states.shp")
        us50_df <- as.data.frame(us50_shp)
        row.names(us50_df)<-NULL
        
        us50_points <- sp2tmap(us50_shp)
        names(us50_points) <- c("id", "x", "y")
        
        us50 <- merge(x = us50_df, y = us50_points, by.x = "DRAWSEQ", by.y = "id")
        
        cont_us <- us50[us50$STATE_ABBR != "HI" & us50$STATE_ABBR != "AK", ]
        ak <- us50[us50$STATE_ABBR == "AK", ]
        hi <- us50[us50$STATE_ABBR == "HI", ]
        
        scale_mat <- matrix(c(1,0,0,1.25), ncol = 2, byrow = T)
        ak_scale <- scaleState(ak, scale_mat, 0.4, x_shift = -120, y_shift = 25)
        hi_scale <- scaleState(hi, scale_mat, 1.5, x_shift = -107, y_shift = 25)
        
        all_us <- rbind(cont_us, ak_scale, hi_scale)
        
        proj_type <- "azequalarea"
        projected <- mapproject(x = all_us$x, y = all_us$y, projection=proj_type)
        all_us$x_proj <- projected[["x"]]
        all_us$y_proj <- projected[["y"]]
        ### End K Kapphahn's code
        
        all_us$STATE_NAME<-as.character(all_us$STATE_NAME)
        tfmerged <- inner_join(all_us, tf, by = c("STATE_NAME" ="STATE_NAME"))
        tfmerged <- tfmerged[order(tfmerged$DRAWSEQ), ]
        
        p<-ggplot(data = tfmerged, aes(x=x_proj, y=y_proj, group = DRAWSEQ)) + geom_polygon(color="white",data=tfmerged, aes(fill=med),environment = environment())
        p + scale_fill_gradient(low = "#2cb922",high="#edfedf")+
            theme(axis.text.x=element_blank(),
                  axis.text.y=element_blank(),
                  axis.title.x=element_blank(),
                  axis.title.y=element_blank(),
                  axis.ticks=element_blank())
    })
    
    output$myMap2 <- renderPlot({
        
        library(maptools)
        library(mapproj)
        library(ggplot2)
        
        ### Start  K Kapphahn's code
        centerState <- function(.df) {
            .df$x <- .df$x - (diff(range(.df$x, na.rm = T))/2 + min(.df$x, na.rm = T))
            .df$y <- .df$y - (diff(range(.df$y, na.rm = T))/2 + min(.df$y, na.rm = T))
            return(.df)
        }
        
        scaleState <- function(.df, scale_matrix, scale_factor, x_shift, y_shift) {
            .df <- centerState(.df)
            coords <- t(cbind(.df$x, .df$y))
            scaled_coord <- t(scale_factor*scale_matrix %*% coords)
            
            .df$x <- scaled_coord[,1] + x_shift
            .df$y <- scaled_coord[,2] + y_shift
            return(.df)
        }
        us50_shp <- readShapePoly("states.shp")
        us50_df <- as.data.frame(us50_shp)
        row.names(us50_df)<-NULL
        
        us50_points <- sp2tmap(us50_shp)
        names(us50_points) <- c("id", "x", "y")
        
        us50 <- merge(x = us50_df, y = us50_points, by.x = "DRAWSEQ", by.y = "id")
        
        cont_us <- us50[us50$STATE_ABBR != "HI" & us50$STATE_ABBR != "AK", ]
        ak <- us50[us50$STATE_ABBR == "AK", ]
        hi <- us50[us50$STATE_ABBR == "HI", ]
        
        scale_mat <- matrix(c(1,0,0,1.25), ncol = 2, byrow = T)
        ak_scale <- scaleState(ak, scale_mat, 0.4, x_shift = -120, y_shift = 25)
        hi_scale <- scaleState(hi, scale_mat, 1.5, x_shift = -107, y_shift = 25)
        
        all_us <- rbind(cont_us, ak_scale, hi_scale)
        
        proj_type <- "azequalarea"
        projected <- mapproject(x = all_us$x, y = all_us$y, projection=proj_type)
        all_us$x_proj <- projected[["x"]]
        all_us$y_proj <- projected[["y"]]
        ### End K Kapphahn's code
        
        all_us$STATE_NAME<-as.character(all_us$STATE_NAME)
        
        r0 <<- as.numeric(input$r0)
        e <<- as.numeric(input$e)
        west<<-as.numeric(input$whichEst) +12
        hit2<-(1-(1/r0))/e
        
        tfmerged <- inner_join(all_us, tf, by = c("STATE_NAME" ="STATE_NAME"))
        tfmerged$hit<-ifelse(tfmerged[,west]<=hit2*100,"below HIT","at or above HIT")
        tfmerged <- tfmerged[order(tfmerged$DRAWSEQ), ]
        row.names(tfmerged)<-NULL
        
        p<-ggplot(data = tfmerged, aes(x=x_proj, y=y_proj, group = DRAWSEQ)) + geom_polygon(color="white",data=tfmerged, aes(fill=hit),environment = environment())
        
        p + theme(axis.text.x=element_blank(),
                  axis.text.y=element_blank(),
                  axis.title.x=element_blank(),
                  axis.title.y=element_blank(),
                  axis.ticks=element_blank()) +
        scale_fill_manual(values = c("grey80","#2cb922"))
    })
    
    output$myChart <- renderPlot({
        
        library(ggplot2)
        
        r0 <<- as.numeric(input$r0)
        e <<- as.numeric(input$e)
        west3 <<- as.numeric(input$whichEst3)+5
        hit3<-(1-(1/r0))/e
        
        tfmergedpop <- inner_join(pop, tf, by = c("NAME" ="STATE_NAME"))
        tfmergedpop$rankpop<-rank(-tfmergedpop$age1935mos,na.last=TRUE,ties.method="first")
        tfmergedpop$stAbbr<-setNames(state.abb, state.name)[tfmergedpop$NAME]
        tfmergedpop$hit<-as.factor(ifelse(tfmergedpop[,west3]<=hit3*100,"below HIT","at or above HIT"))
        tfmergedpop$pointsize<-quantile(tfmergedpop$age1935mos,probs=seq(0,1,0.25))
        tfmergedpop$JitCoOr <- jitter(as.numeric(factor(tfmergedpop$rankpop)))
                
        ggplot(data=tfmergedpop, aes(x = tfmergedpop[,west3], y = rankpop),environment=environment()) +
            geom_point(data=tfmergedpop, aes(x=tfmergedpop[,west3], y=JitCoOr,size=-rankpop,colour = hit), alpha=0.5,show_guide=FALSE,environment=environment())+
            geom_text(data=tfmergedpop,aes(x=tfmergedpop[,west3], y=JitCoOr,label=stAbbr)) +
            coord_cartesian(xlim = 79:101, ylim= 0:55)+
            scale_size('Population 19-35mos Rank(1 to 50)', range = c(10,50)) +
            scale_y_reverse(lim=c(55,0))+
            xlab("State MMR Vaccine Rate per 100") +
            ylab("State Population Ranking (Ages 19-35 mos)") +
            labs(title = "MMR Vaccination Levels by State, Sized by Census Population Rank 19-35mos") +
            geom_vline(aes(xintercept = hit3*100), color = "red", linetype="dashed",show_guide=FALSE) +
            geom_text(aes(round(hit3*100-1,digits=0), 52, label=paste(round(hit3*100,digits=1)," Calculated HIT")),show_guide=FALSE,color="red")+
            theme_bw()
        
    })
    
    output$Text1 <- renderText(input$r0)
    output$Text2 <- renderText({
        r0  <<- as.numeric(input$r0)
        e   <<- as.numeric(input$e)
        hit <<- (1-1/r0)/e
        paste(as.character(round(100*hit,digits=1)),"%",sep="")
    })
    output$Text3 <- renderText({
        r0 <<- as.numeric(input$r0)
        e <<- as.numeric(input$e)
        hit<-(1-1/r0)/e        
        nstates <- sum(tf$med<hit*100)
        pstates <- round(nstates/length(tf$med), digits = 3)*100
        paste(c(as.character(nstates)," states, or ", as.character(pstates),"% of all states"), collapse = " ")
    })
    output$Text4 <- renderText({
        r0  <<- as.numeric(input$r0)
        e   <<- as.numeric(input$e)
        hit <<- (1-1/r0)/e
        paste(as.character(round(100*hit,digits=1)),"%",sep="")
    })
    
})