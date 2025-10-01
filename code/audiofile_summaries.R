# Data exploration code 
library(dplyr)
library(lubridate)
k9_2019 <- read.csv("data/K9_2019_detections.csv")
# Data cleaning- Adding Julian Day
k9_2019 <- k9_2019 |>
  mutate(julian_day = yday(recording_date))|>
  reco
