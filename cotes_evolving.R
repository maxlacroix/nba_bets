library(dplyr)
library(tibble)
library(tidyr)
library(purrr)
library(lubridate)
library(ggplot2)
library(ggrepel)
library(jsonlite)

# Getting moj data --------------------------------------------------------


updated_data <- data.frame(
  
  time = rep(NA, 10000),
  cote = rep(NA, 10000),
  stringsAsFactors = FALSE
  
  
)


ajd <- today()
demain <- today() + 1
started <- "true"

game_to_observe <- "Boston Celtics at Indiana Pacers"
team_to_observe <- "Boston Celtics"


i <- 1 

while(i > 0){

  
  
  updated_data$time[i] <- now()
  
  print(Sys.time())
  


url <- paste0("https://content.mojp-sgdigital-jel.com/content-service/api/v1/q/event-list?startTimeFrom=",
              ajd,
              "T05%3A00%3A00Z&startTimeTo=",
              demain,
              "T04%3A59%3A59Z&started=",
              started,
              "&maxEvents=100&orderEventsBy=startTime&maxMarkets=100&orderMarketsBy=displayOrder&marketSortsIncluded=HH%2CHL%2CMR%2CWH&eventSortsIncluded=MTCH&includeChildMarkets=true&prioritisePrimaryMarkets=true&includeMedia=true&drilldownTagIds=584")


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
  unnest(cols = c(prices))  -> prices_per_market


table(prices_per_market$GameName)


prices_per_market %>%
  filter(GameName == game_to_observe) %>% 
  filter(name == team_to_observe) %>% 
  pluck("decimal") -> decimal

updated_data$cote[i] <- decimal

Sys.sleep(15)

i <- i + 1
}


updated_data$time <- as.POSIXct(strptime(updated_data$time, format="%H:%M:%S"))


updated_data %>% 
  filter(!is.na(cote)) %>% 
  ggplot(aes(y = cote,
             x = time,
             group = 1)) +
  geom_point() +
  geom_line() +
  theme_bw()
