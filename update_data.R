library(dplyr)
library(readr)
library(lubridate)
library(ggmap)


key <- readr::read_csv('../.api.csv')[[1]]
register_google(key = key)
rm(key)
ggmap::geocode('Seattle, Washington')


df <- readr::read_csv('https://raw.githubusercontent.com/washingtonpost/data-police-shootings/master/fatal-police-shootings-data.csv',
                      col_types = 'ccDccncccclccl')
names(df) <- toupper(names(df) )

df_gps <- readr::read_csv('Geocodes_USA_with_Counties.csv') %>%
  dplyr::mutate(primary_city = str_replace(primary_city, 'Saint', 'St.'))
  dplyr::transmute('CITY_STATE' = paste(primary_city, state),
                   'LAT' = latitude,
                   'LONG' = longitude)

df <- df %>%
  dplyr::rename( 'MENTAL_ILLNESS_SIGNS' = SIGNS_OF_MENTAL_ILLNESS,
                 'BODY_CAM' = BODY_CAMERA,
                 'THREAT' = THREAT_LEVEL) %>%
  dplyr::mutate( 'GENDER' = dplyr::if_else(is.na(GENDER),
                                                'Unlisted', GENDER ),
                 'ARMED' = dplyr::if_else(is.na(ARMED), 'unlisted', ARMED) ) %>%
  dplyr::mutate( 'ARMED_BIN' = dplyr::if_else( grepl('gun', ARMED) &
                                                 !(grepl('nail', ARMED) |
                                                     grepl('BB', ARMED) | 
                                                     grepl('pellet', ARMED) | 
                                                     grepl('bean-bag', ARMED) ),
                                               'guns',
                                               dplyr::if_else(grepl('vehicle', ARMED) |
                                                                grepl('car', ARMED),
                                                              'vehicles',
                                                              dplyr::if_else(grepl('unarmed', ARMED) |
                                                                               grepl('pen', ARMED),
                                                                             'unarmed',
                                                                             dplyr::if_else((grepl('unknown', ARMED) |
                                                                                               grepl('unlisted', ARMED) ) &
                                                                                              !grepl('weapon', ARMED),
                                                                                            'unknown_unlisted',
                                                                                            'other' ) ) ) ) ) %>%
  dplyr::mutate('CITY_STATE' = paste(CITY, STATE) ) %>%
  dplyr::left_join(df_gps, by = 'CITY_STATE') %>%
  dplyr::mutate( 'ARMED' = paste0(ARMED, ' '),
                 'NAME'  = paste0(NAME, ' '),
                 'CITY'  = paste0(CITY, ' ') ) %>%
  dplyr::mutate( 'RACE' = dplyr::if_else(RACE == 'W', 'White',
                                         dplyr::if_else(RACE == 'B', 'Black',
                                                        dplyr::if_else(RACE == 'H', 'Hispanic',
                                                                       dplyr::if_else(RACE == 'A', 'Asian',
                                                                                      dplyr::if_else(RACE == 'N', 'Native American',
                                                                                                     dplyr::if_else(RACE == 'O', 'Other',
                                                                                                                    'Unlisted' ))))))) %>%
  dplyr::mutate( 'RACE' = dplyr::if_else(is.na(RACE), 'Unlisted', RACE ),
                 'GENDER' = dplyr::if_else(is.na(GENDER), 'Unlisted', GENDER),
                 'FLEE' = dplyr::if_else(is.na(FLEE), 'Unlisted', FLEE ) ) %>%
  dplyr::mutate( 'DATE' = lubridate::ymd(DATE) ) 
rm(df_gps)

for (city_state in unique(df$CITY_STATE[is.na(df$LAT)]))
{
  temp <- geocode(city_state)
  if(any(is.na(city_state) ) ) print(paste('NA at:', city_state) )
  df[df$CITY_STATE == city_state, c('LONG','LAT')] <- temp
}
rm(temp)

# the multiple lat-long addresses issue comes from Geocodes_USA_with_Counties.csv;
# which is best used with FIPS codes rather than city-names, as many of the city-names have multiple entries
# unfortunately, the Washington Post did not use FIPS codes

df %>%
  select( - CITY_STATE) %>%
  group_by(ID, NAME, DATE, MANNER_OF_DEATH, ARMED, AGE, GENDER, RACE, CITY, STATE, MENTAL_ILLNESS_SIGNS, THREAT, FLEE, BODY_CAM, ARMED_BIN) %>%
  summarize(LAT = mean(LAT), LONG = mean(LONG) ) %>%
  ungroup() %>%
  mutate('LAT' = round(LAT, 2),
         'LONG' = round(LONG, 2) ) %>%
  distinct() %>%
  readr::write_csv('fatal_police_shootings.csv')
