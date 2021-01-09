library(dplyr)
library(tibble)
library(tidyr)
library(purrr)
library(lubridate)
library(ggplot2)
library(ggrepel)
library(kableExtra)
library(jsonlite)
library(formattable)

# Getting moj data --------------------------------------------------------


ajd <- today() 
demain <- today() + 12
max_events <- 500
max_makerts <- 100
sport <- 11 

teams_to_reconsider <- NULL# mettre NULL si on veut garder toutes les equipes

url <- paste0("https://content.mojp-sgdigital-jel.com/content-service/api/v1/q/event-list?startTimeFrom=",
              ajd,"T05%3A00%3A00Z&startTimeTo=",
              demain,
              "T04%3A59%3A59Z&started=false&maxEvents=",
              max_events,
              "&orderEventsBy=startTime&maxMarkets=",
              max_makerts,
              "&orderMarketsBy=displayOrder&marketSortsIncluded=HH%2CHL%2CMR%2CWH&eventSortsIncluded=MTCH&includeChildMarkets=true&prioritisePrimaryMarkets=true&includeMedia=true&drilldownTagIds=",
              sport)


table_soccer <- fromJSON(url)

table_soccer%>%
  pluck("data") %>% 
  pluck("events") %>% 
  as.data.frame -> table_intermediaire

table_intermediaire %>% 
  select(c(name, startTime, markets)) %>% 
  bind_cols(table_intermediaire$type$name) %>% 
  rename("GameName" = "name",
         "League" = "...4") %>% 
  unnest(cols = c(markets)) %>% 
  rename("EventName" = "name") %>% 
  filter(EventName == "Winner 3 Way")%>% 
  select(eventId,League, GameName, EventName, outcomes) %>% 
  unnest(cols = c(outcomes)) %>% 
  select(-displayOrder) %>% 
  unnest(cols = c(prices)) %>% 
  select(eventId,
         League,
         GameName,
         LongName = name,
         decimal) -> prices_per_market


prices_per_market %>% 
  count(League) %>% 
  arrange(-n)


prices_per_market %>% 
  filter(League == "Belgium Pro League") %>% 
  distinct(LongName) %>% View()
  

# Getting 538 data --------------------------------------------------------

library(readr)
library(tictoc)


fte_soccer <- read_csv(url("https://projects.fivethirtyeight.com/soccer-api/club/spi_matches.csv"),
                       guess_max = 42000) # nécessaire sinon y'a des parsings failures


fte_soccer %>% 
  filter(date == today()) %>% 
  distinct(league) %>% View()


fte_soccer %>% 
  filter(league == "Belgian Jupiler League") %>% 
  distinct(team1) %>% View()
