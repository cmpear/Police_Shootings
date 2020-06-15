# CURRENT GOALS
# FINISH CREATING INPUTS RELATED TO EACH PART OF THE DATASET
# ADD A DL BUTTON
# ADD THE UNSELECT OPTIONS FOR NAMES

library(readr)
library(ggplot2)
#library(dplyr)
df <- readr::read_csv('fatal_police_shootings.csv',
                      col_types = 'ccDccncccclcclc')


string_eval <- function(s, vec){
  if(length(s) > 1){
    s <- (sapply(s, string_eval, vec = vec )  )
    return(apply(s, MARGIN = 1, FUN = 'all'))
  }
  s <- stringr::str_trim(s, 'left')
  if (grepl('-', s) ){
    s <- stringr::str_replace(s, '-', '')
    return(!grepl(s, vec) )
  }
  return(grepl(s, vec))
}

function(input, output) {
  my_df <- NULL
  #  logi <- rep(TRUE, nrow(df) )
  
  # Filter data based on selections
  output$table <- DT::renderDataTable(DT::datatable({
    # for checklists, easier not to use an if statement
    # RACE ####
    if (length(input$race) > 0){
      logi <-  df$RACE %in% input$race
    }
    else{
      logi <- rep(TRUE, nrow(df) )
    }
    
    # THREAT ####
    if (input$threat != "All") {
      logi <- (df$THREAT_LEVEL == input$threat) & logi
    }
    
    # FLEE ####
    if (input$flee != "All") {
      logi <- (df$FLEE == input$flee) & logi
    }
    
    # BODY CAMERA ####
    if (input$bCamera != "All") {
      logi <- (df$BODY_CAMERA == input$bCamera) & logi
    }
    
    # SIGNS OF MENTAL ILLNESS ####
    if (input$mental != "All") {
      logi <- (df$SIGNS_OF_MENTAL_ILLNESS == input$mental) & logi
    }
    
    # GENDER ####
    if (input$gender != "All") {
      logi <- (df$GENDER == input$gender) & logi
    }
    
    # MANNER OF DEATH ####
    if (input$manner_of_death != "All") {
      logi <- (df$MANNER_OF_DEATH == input$manner_of_death) & logi
    }
    
    # DATE ####
    if (input$dateRange[1] >= min(df$DATE) || input$dateRange[2] <= max(df$DATE) ){
      logi <- ( df$DATE >= input$dateRange[1] | df$DATE <= input$dateRange[2] ) & logi
    }
    
    # AGE ####
    if ( input$age != 'All' ){
      s <- strsplit(input$age, ',')[[1]]
      logi <- (df$AGE %in% input$age) & logi
      rm(s)
    }
    
    # STATE ####
    if ( input$state != 'All' ){
      s <- strsplit(input$state, ',')[[1]]
      logi <- (df$STATE %in% input$state) & logi
      rm(s)
    }
    
    # CITY ####
    if (input$city != "" ){
      s <- strsplit(input$city, ',')[[1]]
      logi <- string_eval(tolower(s), tolower(df$CITY)) & logi
      rm(s)
    }
    
    # NAME ####
    if (input$name_input != "" ){
      s <- strsplit(input$name_input, ',')[[1]]
      logi <- string_eval(tolower(s), tolower(df$NAME)) & logi
      rm(s)
    }
    # WEAPON ####
    if (input$weapon != "" ){
      s <- strsplit(tolower(input$weapon), ',')[[1]]
      logi <- string_eval(tolower(s), tolower(df$ARMED)) & logi
      rm(s)
    }
    
    my_df <<- df[logi,]  # more space for faster speeds
    
    return(my_df )
  }))
  output$downloadData <- downloadHandler(
    filename = 'police_shootings.csv',
    content = function(file){
      write.csv(my_df, file, row.names = FALSE)
    }
  )
  # output$plot <- shiny::renderPlot({
  #   my_df %>%
  #     mutate( 'PERIOD' = if_else(rep(input$date_bins, nrow(my_df) ) == 'Years',
  #                                lubridate::year(DATE), paste(lubridate::year(DATE), lubridate::month(DATE) ) )  ) %>%
  #     group_by( PERIOD) %>%
  #     summarize( DEPARTED = length(PERIOD) ) %>%
  #     ggplot( aes(x = YEAR, y = PERIOD) ) + geom_bar(stat = 'identity') %>%
  #     return()
  # }) 
}
