library(readr)
#library(tidyr)
#library(dplyr)
#library(lubridate)
#library(ggplot2)
library(shinythemes)
library(leaflet)

# Load the ggplot2 package which provides
# the 'mpg' dataset.
df <- readr::read_csv('fatal_police_shootings.csv',
                      col_types = 'ccDccncccclcclcnn')

fluidPage(
  theme = shinythemes::shinytheme(theme = 'journal'),
  titlePanel("Police Shootings Data"),
  fluidRow (
    column(4,
           downloadButton("downloadData", "Download"),
           helpText("This site is built to help explore the police shootings dataset assembled by the Washington Post.
             The app is intended to aid in exploration rather than convey a particular message.
             There are three parts to the site: the first is the datatable and tools for filtering it.
             This data is fed into four barplots and one map.
             The map has popups for both states, and 'hotspots'.
             Hotspots are defind as places with a certain number of shootings: 5 by default.
             To refresh the data used, simply use the refresh button.
             Finally, one can has some control over how the data is binned for the plots."),
           fluidRow(
             column(3),
             column(9,
                    dateRangeInput('dateRange',
                                   label = 'Date range input: yyyy-mm-dd',
                                   start = min(df$DATE), end = Sys.Date() ))
           )
    ),
    column(2,
           checkboxGroupInput("race", "Race", unique(df$RACE) )
    ),
    column(2,
           selectInput('age','AGE',c('All', sort(unique(df$AGE)) ), multiple = TRUE, selected = 'All', selectize = FALSE ),
           selectInput('state','State',c('All', sort(unique(df$STATE)) ), multiple = TRUE, selected = 'All', selectize = FALSE )
    ),
    column(2,
           selectInput("threat", "Threat Level:",
                       c("All", unique(as.character(df$THREAT))),
                       multiple = TRUE, selected = 'All', selectize = FALSE),
           selectInput("manner_of_death", "Manner of Death:",
                       c("All", unique(as.character(df$MANNER_OF_DEATH)))),
           selectInput("gender","Gender:",
                       c("All", unique(as.character(df$GENDER))))
    ),
    column(2,
           selectInput("flee", "Flee:",
                       c("All", unique(as.character(df$FLEE))),
                       multiple = TRUE, selected = 'All', selectize = FALSE),
           selectInput("bCamera", "Body Camera:",
                       c("All", unique(as.character(df$BODY_CAM)))),
           selectInput("mental", "Signs of Mental Illness:",
                       c("All",
                         unique(as.character(df$MENTAL_ILLNESS_SIGNS))))
    ),
  ),
  fluidRow(
    column(1),
    column(3,
           wellPanel( shiny::textInput("name_input", "Name: ", "", placeholder = "") ),
    ),
    column(3,
           wellPanel( shiny::textInput("city", "Cities: ", "", placeholder = "EX: New, -Orleans") ),
    ),
    column(3,
           wellPanel( shiny::textInput("weapon", "Weapons: ", "", placeholder = "EX: gun , -nail, -BB") )
    ),
    column(2,
           wellPanel( shiny::selectInput("armed_bin","Armed Bin", c('All', sort(unique(df$ARMED_BIN)) ),  multiple = TRUE, selected = 'All', selectize = FALSE) ) )
  ),
  fluidRow(
    column(12,
           DT::dataTableOutput("table")
    )
  )
  ,
  fluidRow(
    column(2,
           actionButton('refresh','Refresh Visuals & Map')
           ),
    column(1,
           radioButtons('age_bin_size','Age Bin Size (Years)',
                        choices = c(1,2,4,5,10,20,25,33,50),
                        inline = TRUE, selected = 10)
           ),
    column(1,
           radioButtons('date_bins','Date Bins',c( 'Years','Months', 'Years & Months' ), selected = 'Years' )
           ),
    column(2,
           sliderInput('flee_bin','Pool fleeing for cases under percentage', min = 0, max = 50, value = 10, step = 0.5)
           ),
    column(2,
           sliderInput('race_bins','Pool Races Under Percentage', min = 0, max = 50, value = 10, step = 0.5)
           ),
    column(2,
           sliderInput('armed_bin2','Pool Armaments Under Percentage', min = 0, max = 50, value = 5, step = 0.5)
    )
    ),
  fluidRow(
    column(6,
           shiny::plotOutput('plot')
           ),
    column(6,
           shiny::plotOutput('plot2')
           )
  ),
  fluidRow(
    column(6,
           shiny::plotOutput('plot3')
           ),
    column(6,
           shiny::plotOutput('plot4')
           )
  ),
  fluidRow(
    column(2,
           sliderInput('hotspot','Hotspot Threshold', min = 5, max = 100, value = 5, step = 1)
    )
  ),
  fluidRow(
    column(11,
           leaflet::leafletOutput('map',height=1200,width=1800)
           )
  )
)