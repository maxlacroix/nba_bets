library(dplyr)
library(tibble)
library(tidyr)
library(purrr)
library(lubridate)
library(ggplot2)
library(ggrepel)
library(jsonlite)
# Getting moj data --------------------------------------------------------


ajd <- today()
demain <- today() + 1

url <- paste0("https://content.mojp-sgdigital-jel.com/content-service/api/v1/q/event-list?startTimeFrom=",
              ajd,
              "T05%3A00%3A00Z&startTimeTo=",
              demain,
              "T04%3A59%3A59Z&started=false&maxEvents=100&orderEventsBy=startTime&maxMarkets=100&orderMarketsBy=displayOrder&marketSortsIncluded=HH%2CHL%2CMR%2CWH&eventSortsIncluded=MTCH&includeChildMarkets=true&prioritisePrimaryMarkets=true&includeMedia=true&drilldownTagIds=584")


table_nba <- fromJSON(url)

table_nba%>%
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


fte_nba <- read_csv(url("https://projects.fivethirtyeight.com/nba-model/nba_elo.csv"),
                    guess_max = 70000) # nécessaire sinon y'a des parsings failures

fte_nba %>% 
  filter(date == today()) %>% 
  select(date, team1, team2, elo_prob1, elo_prob2, raptor_prob1, raptor_prob2)%>%
  rename("x1" = "team1",
         "x2" = "team2",
         "y1" = "elo_prob1",
         "y2" = "elo_prob2",
         "z1" = "raptor_prob1",
         "z2" = "raptor_prob2") %>% # un peu sloppy mais nécessaire pour le names_pattern
  pivot_longer(x1:z2,
               names_to = c(".value", "id"),
               names_pattern = "(.)(.)")%>% 
  rename("Team" = "x",
         "Prob_elo" = "y",
         "Prob_raptor" = "z") -> teams_prob



# Load mapping table ------------------------------------------------------

nba_mapping <- readxl::read_excel("nba_teams.xlsx")


teams_prob <- teams_prob %>% 
  left_join(nba_mapping, by = c("Team" = "ABB"))

prices_per_market %>% 
  left_join(teams_prob, by = c("LongName" = "LongName")) %>% 
  mutate(Elo_in_decimal = 1/Prob_elo,
         Raptor_in_decimal = 1/Prob_raptor) -> data_for_comparison 
  
data_for_comparison %>% 
ggplot(aes(x = Raptor_in_decimal, y = decimal))+
  geom_point() +
  geom_abline(slope = 1) +
  geom_label_repel(aes(label = Team), size = 2.3) +
  labs(x = "Cote selon 538",
       y = "Cote selon moj") +
  coord_fixed() +
  theme_bw()

data_for_comparison %>% 
  select(-Prob_elo,
         -id) %>% 
  filter(Raptor_in_decimal < decimal) 

  

