## Shiny App ui.R
## HITCalculator v1.1
## Changes to v1.0 include:
## 1.  Added code for reading/cleaning raw data file from the CDC and Census website
## 2.  Add new vars to data frame for 95% CI intervals around MMR vaccine rates: Low, Med, High
## 3.  Add New bubble plot showing state=bubble, size=population rank, color above/below HIT thresh 

# load packages
library(markdown); library(shiny); library(colorspace)

# UI structure defined here with tabs for input/plot, USA map and documentation
shinyUI(navbarPage("USA Estimated MMR Vaccinations Children Aged 19-35 Mos - National Immunization Survey 2013",
                   
  position=("static-top"),
                   
  tabPanel("Plot",
    h4("HITCalculator v1.1:  How Many States Fall Below the Vaccination Threshold? ",align="center"),                                                                           
    h4(withMathJax(helpText("Vaccine (or Herd) Immunity Threshold Calculation (HIT) $$V_c=\\cfrac{1-1/R_0}{E}$$")),align="center"),
    sidebarPanel(
      h4("How to Run This App"),
                                         
      h3(withMathJax("$$\\text{1.  Choose a Value for: }R_0$$"),style = "font-family: 'baskerville'; font-si16pt",align="left"),
      h6("this is the Basic Reproduction Number",align="center"),
      selectInput("r0",  "",
                   choices = c("12", "13","14","15","16","17","18")),
      h3(withMathJax("$$\\text{2.  Choose a Value for : }E$$"),style = "font-family: 'baskerville'; font-si16pt",align="left"),
      h6("this is the Effectiveness of the Vaccine number and it is constrained so that it is always greater than (1-1/R0)",align="center"),
      selectInput("e", "", 
                   choices = c("1.00", "0.99", "0.98", "0.97", "0.96","0.95")),
      h4("How to View Output"),
      p("Go to the Menu bar at the top of page, click on a tab"),
      p("PLOT: On this tab,  view a histogram of the number of states falling above or below the immunity threshold, based on midpoint MMR vaccine level.",style = "font-family: 'baskerville'; font-si16pt"),
      p("View the calculated HIT and # states below HIT, under the graph.",style = "font-family: 'baskerville'; font-si16pt"),
      p("MAP1: View a map of the USA colored by midpoint vaccination level.",style = "font-family: 'baskerville'; font-si16pt"),
      p("MAP2: View a map of the USA highlighting states falling above or below the immunity threshold.",style = "font-family: 'baskerville'; font-si16pt"),
      p("StateChart: View a bubble chart highlighting states falling above or below the immunity threshold.",style = "font-family: 'baskerville'; font-si16pt")
    ),
                            
    mainPanel(
      h4("Histogram of Number of US States by MMR Vaccination Rates:"),
      plotOutput("myPlot"),
      h4("Calculated Vaccination Immunity Threshold:"),
      verbatimTextOutput("Text2"),
      h4("Number of US States with MMR (midpoint of CI) vaccination rate below calculated threshold:"),
      verbatimTextOutput("Text3")
    )
  ),
                   
  tabPanel("Map1",
    sidebarPanel(
      h4("MMR 2013 Estimated Vaccination Rates by State"),
      p("This map shows midpoint estimates of MMR vaccination rates by state.", 
      style = "font-family: 'baskerville'; font-si16pt"),
      p("The lighter colors indicate states with the highest rates, darker colors indicate the lowest.", 
      style = "font-family: 'baskerville'; font-si16pt"),
      p("PLEASE WAIT as long as 1-2 minutes for the map to load.", 
      style = "font-family: 'baskerville'; font-si16pt")
    ),
    mainPanel(
      plotOutput("myMap")
    )
  ),
                   
  tabPanel("Map2",
    sidebarPanel(
      h4("MMR 2013 State Vaccination Rates Below Calculated HIT"),
      p("This map shows which states fall below the calculated HIT for MMR vaccination rates.", 
      style = "font-family: 'baskerville'; font-si16pt"),
      p("States falling below the HIT threshold are colored in dark green. Those at or above the HIT are colored in grey.", 
      style = "font-family: 'baskerville'; font-si16pt"),
      p("PLEASE WAIT as long as 1-2 minutes for the map to load.", 
      style = "font-family: 'baskerville'; font-si16pt"),
      radioButtons('whichEst', "Select Which MMR Vaccine Estimate to View:",
                    choices = list("Lower 95% CI" = 0, "Midpoint" = 1, "Upper 95% CI" = 2), selected = 1),
      p("The CDC uses a survey technique and provides a 95% confidence interval due to the sampling method. Choosing the lower CI will display more states below the (constant) HIT, upper CI will display more states above the HIT.",style = "font-family: 'baskerville'; font-si16pt"),
      p("Because of the high variances of state MMR rates vs HIT, the map shows many more states fall below HIT on the lower CI basis, and many more fall above on the upper CI basis.",style = "font-family: 'baskerville'; font-si16pt")
    ),
    mainPanel(
      plotOutput("myMap2"),
      h4("Calculated Vaccination Immunity Threshold:"),
      verbatimTextOutput("Text4")
    )
  ),
                   
  tabPanel("StateChart",
    sidebarPanel(
      h4("MMR 2013 State Vaccination Rates Above or Below Calculated HIT"),
      p("This chart shows which states fall above/below the calculated HIT for MMR vaccination rates.", 
      style = "font-family: 'baskerville'; font-si16pt"),
      p("States falling below the HIT threshold are colored in blue. Those above the HIT are colored in pink.", 
      style = "font-family: 'baskerville'; font-si16pt"),
      p("The size of the bubble and y-axis indicates relative population rank based on 7/1/2013 Census Data (ages 19-35 mos).", 
      style = "font-family: 'baskerville'; font-si16pt"),
      radioButtons('whichEst3', "Select Which MMR Vaccine Estimate to View:",
                    choices = list("Lower 95% CI" = 0, "Midpoint" = 1, "Upper 95% CI" = 2), selected = 1),
      p("The CDC uses a survey technique and provides a 95% confidence interval due to the sampling method. Choosing the lower CI will display more states below the (constant) HIT, upper CI will display more states above the HIT.",style = "font-family: 'baskerville'; font-si16pt"),
      p("Because of the high variances of state MMR rates vs HIT, the map shows many more states fall below HIT on the lower CI basis, and many more fall above on the upper CI basis.",style = "font-family: 'baskerville'; font-si16pt")
    ),
    mainPanel(
      plotOutput("myChart",height="550px")
    )
  ),
                   
  tabPanel("Documentation",
    mainPanel(
    h4("I.  Purpose of this app (HITCalculator v1.1)"),
                                
      p("With a resurgence of measles cases in the United States in the 21st century, 
         after near erradication in the last one, vaccination rates and public health policy
         for mandatory vs optional childhood vaccination programs has become an important debate.
         This app calculates a vaccination threshold, also known as the Herd Immunity Threshold (HIT)
         for measles and compares US state-wide MMR vaccination rates for children (19-35 months old)
         to the threshold based on the National Immunization Survey ( at least 1 dose of measles-mumps-rubella (MMR) vaccine).
         An important aim of the app is to account for the variation in both the HIT 
         calculation and the vaccination rates, and visually display the results in a way that makes
         the uncertainty and wide range of outcomes, apparent visually.", 
         style = "font-family: 'baskerville'; font-si16pt"),
                                
      p("This app is a simple prototype for calculating HIT and comparisons of MMR vaccination rates by state.
         Although beyond the scope of this app, future versions may address some of the pitfalls of using HIT to assess
         human community immunity.  For example, for greater homogeneity, focus on smaller, more homogeneous populations at the county or local town level.  
         Incorporate agent based modeling using time-dynamic social network contagion theory algorithms for a variety of social network cluster types for a more accurate,
         and robust calculation, and lastly, utilize other sources of information such as Google API (key word search clouds) to augment
         the CDC NIS (National Health Interview survey) data.", 
         style = "font-family: 'baskerville'; font-si16pt"),
                                
    h4("II.  Data Sources"),
      p("A.  The Vaccination Rate data used in this app comes from the CDC's National Immunization Survey. It is available at the cdc.gov website. The file can be 
        downloaded from the link below:", style = "font-family: 'baskerville'; font-si16pt"),
                                
      a(href = "http://www2a.cdc.gov/nip/coverage/nis/CountNIS.asp?fmt=v&rpt=tab03_antigen_state_2013.xlsx&qtr=Q1/2013-Q4/2013",
      "Link to Data file in Excel format, 2013 Table 03 at cdc.gov"),
                                
      p("B.  The 7/1/13 Census data used in this app comes from the US Census Bureau. It is available at the census.gov website. The file can be 
      downloaded from the link below:", style = "font-family: 'baskerville'; font-si16pt"),
                                
      a(href = "https://www.census.gov/popest/data/state/asrh/2013/files/SC-EST2013-AGESEX-CIV.csv",
      "Link to Census Data file in CSV format, SC-EST2013-AGESEX-CIV at census.gov"),
                                
    h4("III.   Calculation Details"),
      h4(withMathJax(helpText("Vaccine (or Herd) Immunity Threshold Calculation (HIT) $$V_c=\\cfrac{1-1/R_0}{E}$$")),align="center"),
      p("The formula for HIT employed in this app is a common measure used to benchmark the point at which a particular disease 
         may be considered no longer endemic to a population.  The R value and E value are the two inputs to this app.  The R value,
         basic reproduction number is 'The average number of new infections caused by each case in an entirely susceptible population that 
         is homogeneous, or well-mixed, meaning each individual can come into contact with every other susceptible individual in the population.'(citation #1 below).
         The higher the R0 value, as for measles (12-18 new infections), the higher the vaccination or immunity threshold.
         The E value, is the Effectiveness measure (> (1-1/R0) and <=100%). The E factor accounts for vaccines that are not 100% effective, and
         is a way to recognize uncertainty in the HIT calculation.", 
         style = "font-family: 'baskerville'; font-si16pt"),
                                
      a(href = "http://en.wikipedia.org/wiki/Herd_immunity",
       "Link to more information about herd immunity and the HIT formula on wikipedia"),
      p(" "),
      a(href = "http://en.wikipedia.org/wiki/Herd_immunity#cite_note-pmid15627236-10",
        "Citation #1 Link to one of several paper citations on wikipedia"),
                                
    h4("IV.  Types of Output"),
      p("The HIT calculation output and a histogram of MMR vaccination rates by state with HIT as a vertical line
        are given on the first tab named 'Plot'.  On Map1 and Map2 are 2 chloropleth maps, where states are color-coded by level of  
	    MMR vaccination rate.  Map1 splits the states into several (gradient) levels, while Map2 shows only 2 levels: at/above HIT or below HIT.
        This makes it easy to visualize both state and regional geographic locations.  The StateChart tab contains a bubble chart of state MMR vaccine rate relative to the HIT 
        threshold and relative to other states. The bubbles are sized according to Census Data (relative rank) for children (aged 12-24 mos) * 0.5 + ages (25-36 mos). 
        Both the Map2 and StateChart tabs have radio dials allowing the user to view MMR vaccination rates for low, med or high confidence interval points as given by the CDC.", 
        style = "font-family: 'baskerville'; font-si16pt"),	
                                
    h4("V.  Reproducible R Code"),
      p("This app uses fully reproduceable R code.  Note, the deployed app on the R studio server does not run the download.file R code but reads the cleaned CDC and Census data files directly. By reading the files directly, the app runs much faster and the risk of the app failing (ie third party removal/renaming of the source files) is greatly reduced. 
         The R code documenting how the files were downloaded and created from the raw data is included in the github repository under server.R (annotated and commented out). 
         All code for this app and for the accompanying Slidify R presentation are available here:", 
         style = "font-family: 'baskerville'; font-si16pt"),
                                
      a(href = "https://github.com/mbmoran/myshinyapp/tree/gh-pages",
        "Link to source code on GitHub"),
                                
    h4("VI.  Intended Use, License and Disclaimer"),
      p("The primary usage of this app is for educational purposes as part of a Johns Hopkins Bloomberg School of Public Health 
         Data Science class, providing other students with an example for how to create a data product using open source Code from R.  It is not intended
         to be used for any other purpose. Software license gpl-3, link below." , style = "font-family: 'baskerville'; font-si16pt"),
                                
      a(href = "https://gnu.org/licenses/gpl-3.0.txt", "Link to gpl-3 license")
    ))
  )
)