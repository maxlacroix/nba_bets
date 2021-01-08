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



table_nba <- fromJSON('http://site.api.espn.com/apis/site/v2/sports/basketball/nba/scoreboard')


table_nba%>%
  pluck("events") -> t

t[1,] -> t

t %>%
  select(-id,
         -uid,
         -date,
         -status) %>% 
  unnest(cols = c("competitions")) %>% 
  select(-id,
         -uid,
         -type) %>% 
  unnest(cols = c("competitors")) %>% View()
  unnest(cols = c(odds)) %>%
  unnest(cols = c(links)) %>% View()
