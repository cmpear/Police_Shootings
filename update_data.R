library(dplyr)
library(readr)
library(lubridate)

df <- readr::read_csv('https://raw.githubusercontent.com/washingtonpost/data-police-shootings/master/fatal-police-shootings-data.csv',
                      col_types = 'ccDccncccclccl')
names(df) <- toupper(names(df) )

gun_bin <- grepl('gun', df$ARMED) & !(df$ARMED %in% c('nail gun', 'BB gun', 'pellet gun', 'bean-bag gun') )
vehicle_bin <- grepl('vehicle', df$ARMED ) | grepl('car', df$ARMED )
unknown_unlisted_bin <- (grepl('unknown', df$ARMED) | grepl('unlisted', df$ARMED)  ) & df$ARMED != 'unknown weapon'
unarmed_bin <- df$ARMED == 'unarmed' | df$ARMED == 'pen'  # don't think pen should count as armed...could change views if pen proved incredibly deadly
#other_armed <- !any(gun_bin, vehicle_bin, unknown_unlisted_bin, unarmed_bin )


df <- df %>%
  dplyr::mutate( 'GENDER' = if_else(is.na(GENDER), 'Unlisted', GENDER ),
                 'ARMED' = if_else(is.na(ARMED), 'unlisted', ARMED) ) %>%
  dplyr::mutate( 'ARMED_CAT' = if_else(ARMED %in% gun_bin, 'gun_bin',
                                       if_else(ARMED %in% vehicle_bin, 'vechicle_bin',
                                               if_else(ARMED %in% unknown_unlisted_bin, 'unknown_unlisted_bin',
                                                       if_else(ARMED %in% unarmed_bin, 'unarmed_bin',
                                                               'other_armed_bin')))) ) %>%
  dplyr::mutate( 'ARMED' = paste0(ARMED, ' '),
                 'NAME'  = paste0(NAME, ' '),
                 'CITY'  = paste0(CITY, ' ') ) %>%
  dplyr::mutate( 'RACE' = if_else(RACE == 'W', 'White',
                                  if_else(RACE == 'B', 'Black',
                                          if_else(RACE == 'H', 'Hispanic',
                                                  if_else(RACE == 'A', 'Asian',
                                                          if_else(RACE == 'N', 'Native American',
                                                                  if_else(RACE == 'O', 'Other',
                                                                          'Unlisted' ))))))) %>%
  dplyr::mutate( 'RACE' = if_else(is.na(RACE), 'Unlisted', RACE ) ) %>%
  dplyr::mutate( 'DATE' = ymd(DATE) ) 

readr::write_csv(df, 'fatal_police_shootings.csv')
