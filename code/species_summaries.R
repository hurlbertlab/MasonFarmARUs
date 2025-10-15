#Species data exploration 
library(dplyr)
library(tidyr)
library(lubridate)

#Top total detections 
top_species <- files_species_sum |>
  group_by(Common_Name) |>
  summarize(Total_Detections = sum(Total_Detections)) |>
  arrange(desc(Total_Detections)) |>
  head(15)

ggplot(top_species, aes(x = reorder(Common_Name, Total_Detections), y = Total_Detections)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(x = "Species", y = "Total Detections", 
       title = "Top 15 Most Detected Species") +
  theme_minimal()

#Species detection by confidence level
conf_summary <- files_species_sum |>
  group_by(Common_Name) |>
  summarize(
    All = sum(Total_Detections),
    Conf_20 = sum(Detections_20conf),
    Conf_40 = sum(Detections_40conf),
    Conf_60 = sum(Detections_60conf),
    Conf_80 = sum(Detections_80conf)
  ) |>
  arrange(desc(All)) |>
  head(10) |>
  pivot_longer(cols = c(All, Conf_20, Conf_40, Conf_60, Conf_80),
               names_to = "Confidence_Level",
               values_to = "Detections")

ggplot(conf_summary, aes(x = reorder(Common_Name, Detections), y = Detections, fill = Confidence_Level)) +
  geom_col(position = "dodge") +
  coord_flip() +
  labs(x = "Species", y = "Number of Detections",
       title = "Detections by Confidence Level for Top 10 Species") +
  theme_minimal()
