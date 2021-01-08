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
demain <- today() + 1
max_events <- 100
max_makerts <- 100
sport <- 55 #NBA - si jamais on voulait checker la NFL -> 55


teams_to_reconsider <- NULL # mettre NULL si on veut garder toutes les equipes

url <- paste0("https://content.mojp-sgdigital-jel.com/content-service/api/v1/q/event-list?startTimeFrom=",
              ajd,"T05%3A00%3A00Z&startTimeTo=",
              demain,
              "T04%3A59%3A59Z&started=false&maxEvents=",
              max_events,
              "&orderEventsBy=startTime&maxMarkets=",
              max_makerts,
              "&orderMarketsBy=displayOrder&marketSortsIncluded=HH%2CHL%2CMR%2CWH&eventSortsIncluded=MTCH&includeChildMarkets=true&prioritisePrimaryMarkets=true&includeMedia=true&drilldownTagIds=",
              sport)


table_nfl <- fromJSON(url)

table_nfl%>%
  pluck("data") %>% 
  pluck("events") %>% 
  as.data.frame() %>%
  select(c(name, startTime, markets)) %>% 
  rename("GameName" = "name") %>% 
  unnest(cols = c(markets)) %>% 
  rename("EventName" = "name") %>% 
  filter(EventName == "Winner 2 Way")%>% 
  select(eventId, GameName, EventName, outcomes) %>% 
  unnest(cols = c(outcomes)) %>% 
  select(-displayOrder) %>% 
  unnest(cols = c(prices)) %>% 
  select(eventId,
         GameName,
         LongName = name,
         decimal) -> prices_per_market



# Getting 538 data --------------------------------------------------------

library(readr)
library(tictoc)


fte_nfl <- read_csv(url("https://projects.fivethirtyeight.com/nfl-api/nfl_elo.csv"),
                    guess_max = 16000) # nécessaire sinon y'a des parsings failures


fte_nfl %>% 
  filter(date == today()) %>% 
select(date, team1, team2, qbelo_prob1, qbelo_prob2) %>%
  rename("x1" = "team1",
         "x2" = "team2",
         "y1" = "qbelo_prob1",
         "y2" = "qbelo_prob2") %>% # un peu sloppy mais nécessaire pour le names_pattern
  pivot_longer(x1:y2,
               names_to = c(".value", "id"),
               names_pattern = "(.)(.)")%>% 
  rename("Team" = "x",
         "Prob_qbelo" = "y") -> teams_prob
