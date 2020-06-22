# CURRENT GOALS
# FINISH CREATING INPUTS RELATED TO EACH PART OF THE DATASET
# ADD A DL BUTTON
# ADD THE UNSELECT OPTIONS FOR NAMES

library(readr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(leaflet)
library(rgdal)

df <- readr::read_csv('fatal_police_shootings.csv',
                      col_types = 'ccDccncccclcclcnn')


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

state_to_abb <- function(state){
  if (length(state)>1){
    return(sapply(state, state_to_abb) )
  }
  else if(state == 'District of Columbia') {
    return('DC')
  }
  return(state.abb[state.name == state] )
}

function(input, output) {
  full_df <- TRUE
  my_df <- NULL
  #  logi <- rep(TRUE, nrow(df) )
  
  # Filter data based on selections
  output$table <- DT::renderDataTable(DT::datatable(data = {
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
      logi <- (df$THREAT == input$threat) & logi
    }
    
    # FLEE ####
    if (input$flee != "All") {
      logi <- (df$FLEE == input$flee) & logi
    }
    
    # BODY CAMERA ####
    if (input$bCamera != "All") {
      logi <- (df$BODY_CAM == input$bCamera) & logi
    }
    
    # SIGNS OF MENTAL ILLNESS ####
    if (input$mental != "All") {
      logi <- (df$MENTAL_ILLNESS_SIGNS == input$mental) & logi
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
    # WEAPON BIN ####
    if ( input$armed_bin != 'All' ){
      s <- strsplit(input$state, ',')[[1]]
      logi <- (df$ARMED_BIN %in% input$armed_bin) & logi
      rm(s)
    }
    
    
    my_df <<- df[logi,]  # more space for faster speeds
    full_df <<- nrow(my_df) == nrow(df)
    return(my_df )
  }))
  output$downloadData <- downloadHandler(
    filename = 'police_shootings.csv',
    content = function(file){
      write.csv(my_df, file, row.names = FALSE)
    }
  )
  # PLOT 1 ####
  output$plot <- shiny::renderPlot(expr = {
    input$refresh
    if(input$date_bins == 'Months'){
      rPlot <- my_df %>%
        mutate( 'PERIOD' = lubridate::month(DATE) ) %>%
        ggplot( aes(x = PERIOD) ) + ggtitle('Monthly Police Shootings by Race (years pooled')
    }
    else if (input$date_bins == 'Years'){
      rPlot <- my_df %>%
        mutate( 'PERIOD' = lubridate::year(DATE) ) %>%
        ggplot( aes(x = PERIOD, col = hcl(240), fill = hcl(240, alpha = 0.6) ) )  + ggtitle('Yearly Police Shootings by Race')
    }
    else{
      rPlot <- my_df %>%
        mutate( 'YEAR' = lubridate::year(DATE), 'MONTH' = lubridate::month(DATE) ) %>%
        mutate( 'PERIOD' = paste(YEAR, stringr::str_pad(MONTH, width = 2, side = 'left', pad = '0') ), sep = '-' ) %>%
        ggplot( aes(x = PERIOD) ) + ggtitle('Police Shootings by Year, Month and Race')
    }
    rPlot <- rPlot + facet_wrap( RACE ~ .)
    return(rPlot  + geom_bar(stat = 'count', col = hcl(120), fill = hcl(120, alpha = 0.6)) + theme(axis.text.x = element_text(angle = 90) ) )
  })
  # PLOT 2 ####
  output$plot2 <- shiny::renderPlot(expr = {
    input$refresh
    k <- as.numeric(input$age_bin_size)
    rPlot <- my_df %>%
      mutate( 'AGE' = trunc( AGE / k ) * k  ) %>%
      mutate( 'AGE' = paste( stringr::str_pad(as.character(AGE), width = 2, pad = '0' ),
                             stringr::str_pad(as.character(AGE + k - 1), width = 2, pad = '0'), sep = ' to ') ) %>%
      mutate( 'AGE' = if_else(AGE == 'NA to NA', 'NA', AGE) ) %>%
      ggplot( aes(x = AGE, group = GENDER, col = GENDER, fill = GENDER ) ) + geom_bar(stat = 'count', position = 'stack', alpha = 0.6) +
      scale_fill_brewer(palette = 'Spectral') + facet_wrap(RACE ~ .) + ggtitle('Police Shootings by Age and Race')
    return(rPlot  + theme(axis.text.x = element_text(angle = 90) ) )
  })
  # PLOT 3 ####
  output$plot3 <- shiny::renderPlot(expr = {
    input$refresh
    rPlot <- my_df %>%
      ggplot( aes(x = RACE, col = RACE, fill = RACE ) ) + geom_bar(stat = 'count', alpha = 0.6) + facet_grid(ARMED_BIN ~ FLEE) + 
      scale_fill_brewer(palette = 'Spectral') + ggtitle('Police shootings by Weapon, Race, and Whether/How Suspect Flees')
    return(rPlot  + theme(axis.text.x = element_text(angle = 90) ) )
  })
  # PLOT 4 ####
  output$plot4 <- shiny::renderPlot(expr = {
    input$refresh
    print(names(my_df) )
    rPlot <- my_df %>%
      transmute('BODY_CAM' = BODY_CAM,
                'ILLNESS' = MENTAL_ILLNESS_SIGNS,
                'ATTACK' = THREAT == 'attack',
                'MALE' = GENDER == 'M',
                'UNARMED'= ARMED_BIN == 'unarmed',
                'GUN' = ARMED_BIN == 'guns',
                'RACE' = RACE) %>%
      tidyr::pivot_longer(cols = c('BODY_CAM','ILLNESS','ATTACK','MALE','UNARMED','GUN'), names_to = 'Aspect', values_to = 'Value' ) %>%
      ggplot( aes(x = Aspect, group = Value, col = Value, fill = Value) ) + geom_bar(stat = 'count', pos = 'stack', alpha = 0.6) +
      scale_fill_brewer(palette = 'Spectral') + ggtitle('Distribution of Shootings by Other Variables')
    return(rPlot)
  })
  # to fix the map: include only cities with a certain threshold of shootings (10?); represent those cities with markers;
  #  then, add a chloropleth for deaths in the rest of the state
  # MAP ####
  output$map <- leaflet::renderLeaflet({
    input$refresh
    geo <- readOGR(dsn = 'cb_2018_us_state_500k/cb_2018_us_state_500k.shp', stringsAsFactors = FALSE)
    temp <- my_df %>%
      group_by(STATE) %>%
      mutate( STATEWIDE_DEATHS = length(STATE) ) %>%
      ungroup() %>%
      group_by(CITY, LAT, LONG) %>%
      mutate(DEPARTED = length(CITY) ) %>%
      mutate(GROUPING = if_else(DEPARTED >= 5, CITY, STATE),
             GROUPED_BY = if_else(DEPARTED >= 5, 'CITY', 'STATE') ) %>%
      ungroup() %>%
      group_by(GROUPING, GROUPED_BY) %>%
      summarize(DEPARTED = length(GROUPING), LAT = mean(LAT), LONG = mean(LONG), STATEWIDE_DEATHS = mean(STATEWIDE_DEATHS) )
    
    cities <- temp[temp$GROUPED_BY == 'CITY', c('GROUPING', 'DEPARTED', 'LAT', 'LONG')] %>%
      rename('CITY' = GROUPING)
    states <- temp[temp$GROUPED_BY == 'STATE', c('GROUPING', 'DEPARTED', 'LAT', 'LONG', 'STATEWIDE_DEATHS')] %>%
      rename('STUSPS' = GROUPING)
    
    geo@data <- geo@data %>%
      full_join(states, by = 'STUSPS') %>%
      rename('STATE' = STUSPS)
    
    mypalette <- leaflet::colorNumeric(palette = 'magma',
                                       domain = geo@data$DEPARTED, na.color = 'transparent', alpha = 1)
    # can add data with addMarkers and addPolygons rather than leaflet()
    leaflet(geo) %>% 
      addMarkers(lng = cities$LONG, lat = cities$LAT,
                  popup = ~ paste( sep = "<br/>",
                           paste0('<b>',toupper(cities$CITY),'</b>'),
                           paste0('<b>DEPARTED: </b>',  as.character(cities$DEPARTED) )
                           ) ) %>%
      addPolygons(fillColor = ~mypalette(geo@data$DEPARTED), fillOpacity = 0.7, weight = 1,
                  popup = ~ paste( sep = "<br/>",
                                   paste0('<b>',toupper(NAME),'</b>'),
                                   paste0('<b>DEPARTED (no hotspots): </b>',  as.character(DEPARTED) ),
                                   paste0('<b>DEPARTED (w/ hotspots): </b>',  as.character(STATEWIDE_DEATHS) )
                  ) ) %>%
      addProviderTiles(providers$Stamen.Toner) %>% 
      setView(lng = mean(range(cities$LONG)), lat = mean(range(cities$LAT)), zoom = 4)
  })
}