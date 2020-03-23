library(tidyverse)
library(httr)
library(jsonlite)
library(lubridate)
library(gganimate)
library(gifski)

ice_aaa <- "BAMLC0A1CAAAEY"
ice_bbb <- "BAMLC0A4CBBBEY"
ice_ccc <- "BAMLH0A3HYCEY"


key <- "2ef6e02d5856dad0af454847a5d49332"
url <- "https://api.stlouisfed.org/fred/series/observations?"


get_series <- function(id,name){
  r <- url %>%
    GET(query = list(series_id=id,api_key=key,file_type="json")) %>%
    content(as='text') %>%
    fromJSON()
  
  df <- r$observations %>%
    select(date,value)
  
  df$value <- as.numeric(df$value)
  df$date <- as.Date(df$date)
  
  df <- df %>%
    filter(!is.na(value))
  
  colnames(df)[2] <- name
  
  df
}

df <- get_series(ice_aaa,"AAA") %>%
  left_join(get_series(ice_bbb,"BBB")) %>%
  left_join(get_series(ice_ccc,"CCC")) %>%
  gather("rating","value",-date)


setwd(paste0(getwd(),"/gif_files"))

df %>%
  filter(year(date) >= "2020") %>%
  ggplot(aes(rating,value)) +
  geom_col() +
  labs(title="Date: {closest_state}") +
  transition_states(date)

anim_save("spreads.gif",animation = last_animation())

