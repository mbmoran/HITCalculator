## Shiny App server.R
## Author: MB Moran
## Date: April 14, 2015

# load packages
library(shiny); library(maptools); library(classInt); 
library(RColorBrewer); library(ggplot2); library(dplyr); library(colorspace)

# set directory and load data file
# Need to add code to download Excel file from the CDC, clean file, create final csv
#setwd("~/Actuarial/Coursera/Data Science/Developing Data Products/Project/myshinyapp")

tf<-read.csv("tab03_antigen_state_2013_MMR_50states.csv",stringsAsFactors = FALSE)

# Set colours for plotting using quintiles
cols <- rev(brewer.pal(5, "RdYlGn"))

shinyServer(function(input, output) {
    
    output$myPlot <- renderPlot({
        
        # Create global variables
        r0 <<- as.numeric(input$r0)
        e <<- as.numeric(input$e)
        hit<-(1-1/r0)/e
        
        dd <- with(density(tf$num), data.frame(x,y))
        g<-ggplot(data=tf, aes(tf$num),environment = environment()) + 
            geom_histogram(breaks=seq(85, 100, by =0.5), 
                           col="red", 
                           aes(fill=..count..))
        
        p <- g + 
            scale_x_continuous(breaks=seq(85,100,by=1),limits=c(85,100),name="MMR Vacination Rate (per 100)")+
            scale_y_continuous(breaks = round(seq(0,max(dd$y)*length(tf$num)+1,by=1),0), name="Number of States") +
            geom_vline(aes(xintercept = hit*100), color = "red", linetype="dashed",show_guide=FALSE) +
            geom_text(aes(99, 7, label="Calculated HIT"),show_guide=FALSE,color="red") +
            geom_text(aes(99, 6.5, label=paste(round(hit*100,digits=1))),show_guide=FALSE,color="red") +
            theme_classic()
        print(p)
        
    })

    # A clever way to move AK and HI shapes by
    # Author: Kristopher Kapphahn
    # Website: http://loloflargenumbers.com/blog/?p=206#.VS6rnPnF_T8
    # arcGIS shapefile: http://www.arcgis.com/home/item.html?id=f7f805eb65eb4ab787a0a3e1116ca7e5
    
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
        
    
        tfmerged <- inner_join(all_us, tf, by = c("STATE_NAME" ="STATE_NAME"))
        tfmerged <- tfmerged[order(tfmerged$DRAWSEQ), ]
                
        p<-ggplot(data = tfmerged, aes(x=x_proj, y=y_proj, group = DRAWSEQ)) + geom_polygon(color="white",data=tfmerged, aes(fill=num))
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
        
        r0 <<- as.numeric(input$r0)
        e <<- as.numeric(input$e)
        hit<-(1-1/r0)/e
        
        tfmerged <- inner_join(all_us, tf, by = c("STATE_NAME" ="STATE_NAME"))
        tfmerged$hit<-ifelse(tfmerged$num<=hit*100,"below HIT","at or above HIT")
        tfmerged <- tfmerged[order(tfmerged$DRAWSEQ), ]
        
        #Map2
        p<-ggplot(data = tfmerged, aes(x=x_proj, y=y_proj, group = DRAWSEQ)) + geom_polygon(color="white",data=tfmerged, aes(fill=hit))
        
        p+   theme(axis.text.x=element_blank(),
                   axis.text.y=element_blank(),
                   axis.title.x=element_blank(),
                   axis.title.y=element_blank(),
                   axis.ticks=element_blank())+
            scale_fill_manual(values = c("grey80","#2cb922"))
        
    })
    
    output$Text1 <- renderText(input$r0)
    output$Text2 <- renderText({
        r0 <<- as.numeric(input$r0)
        e <<- as.numeric(input$e)
        hit<-(1-1/r0)/e
        paste(as.character(round(100*hit,digits=1)),"%",sep="")
    })
    output$Text3 <- renderText({
        r0 <<- as.numeric(input$r0)
        e <<- as.numeric(input$e)
        hit<-(1-1/r0)/e        
        nstates <- sum(tf$num<hit*100)
        pstates <- round(nstates/length(tf$num), digits = 3)*100
        #paste(as.character(cat(as.character(nstates)," states, or ",as.character(pstates), "% of all states", sep = "")))
        #paste(as.character(pstates), "%", sep = "")
        paste(c(as.character(nstates)," states, or ", as.character(pstates)," % of all states"), collapse = " ")
    })
})