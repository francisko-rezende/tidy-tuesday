library(tidyverse)
library(here)

christmas_songs <- read_csv(here("2019", "week-52", "data", "raw", "christmas_songs.csv"))

glimpse(christmas_songs)
