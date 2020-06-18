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
             The data has been cleaned a little to work better with this app.
             The data can be filtered using the buttoms below, or by employing the table\'s builtin search function.
             When you have the data you want, use the Download button to save your work.
             Dashboard with visuals to come in next version."),
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
    column(3,
           radioButtons('age_bin_size','Age Bin Size (Years)',
                        choices = c(1,2,4,5,10,20,25,33,50),
                        inline = TRUE, selected = 10)
           ),
    column(3,
           radioButtons('date_bins','Date Bins',c( 'Years','Months', 'Years & Months' ), selected = 'Years' )
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
    column(6
           )
  ),
  fluidRow(
    column(11,
           leaflet::leafletOutput('map',height=1200,width=1800)
           )
  )
)