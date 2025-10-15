# Load required libraries
library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyr)

directory_path <- "C:/git/MasonFarmARUs/data/analyzed_files"

# Get list of CSV files
file_list <- list.files(
  path = directory_path, 
  pattern = "\\.csv$", 
  full.names = TRUE
)

# Initialize empty summary data frame
files_sum <- data.frame(
  Year = character(),
  Date = character(),
  Audiofile = character(),
  Stake = character(),
  Detections = integer(),
  dects_20conf = integer(),
  dects_40conf = integer(),
  dects_60conf= integer(),
  dects_80conf = integer(),
  Unique_species = integer(),
  species_20conf = integer(),
  species_40conf = integer(),
  species_60conf = integer(),
  species_80conf = integer(),
  First_day = character(),
  Last_day = character(),
  Days_recorded = integer(),
  stringsAsFactors = FALSE
)

for (file_path in file_list) {
  cat("Processing:", basename(file_path), "\n")
  
  tryCatch({
    data <- read.csv(file_path)
    
    # Check if file has data
    if (nrow(data) == 0) {
      cat("  File is empty, skipping\n")
      next
    }
    
    audiofile_name <- basename(file_path)
    stake <- as.character(data$stake[1])
    year <- as.character(data$year[1])
    
    # Grouping by recording_date within each file 
    daily_sum <- data |>
      group_by(recording_date) |>
      summarize(
        Detections = n(),
        dects_20conf = sum(confidence >= 0.20),
        dects_40conf = sum(confidence >= 0.40),
        dects_60conf = sum(confidence >= 0.60),
        dects_80conf = sum(confidence >= 0.80),
        Unique_species = length(unique(common_name)),
        species_20conf = length(unique(common_name[confidence >= .20])),
        species_40conf = length(unique(common_name[confidence >= .40])),
        species_60conf = length(unique(common_name[confidence >= .60])),
        species_80conf = length(unique(common_name[confidence >= .80])))|>
      ungroup()
    
    # Convert to date and calculate ranges
    dates <- as.Date(data$recording_date)
    first_day <- as.character(min(dates))
    last_day <- as.character(max(dates))
    days_recorded <- length(unique(dates))
    
    # Add each day to the summary
    for (i in 1:nrow(daily_sum)) {
      day_data <- daily_sum[i, ]
      
      new_row <- data.frame(
        Year = year,
        Date = as.character(day_data$recording_date),
        Audiofile = audiofile_name,
        Stake = stake,
        Detections = day_data$Detections,
        dects_20conf = day_data$dects_20conf,
        dects_40conf = day_data$dects_40conf,
        dects_60conf = day_data$dects_60conf,
        dects_80conf = day_data$dects_80conf,
        Unique_species = day_data$Unique_species,
        species_20conf = day_data$species_20conf,
        species_40conf = day_data$species_40conf,
        species_60conf = day_data$species_60conf,
        species_80conf = day_data$species_80conf,
        First_day = first_day,
        Last_day = last_day,
        Days_recorded = days_recorded,
        stringsAsFactors = FALSE
      )
      
      files_sum <- rbind(files_sum, new_row)
    }
    
    cat("  ✓ Processed", nrow(daily_sum), "days from file\n")
    
  }, error = function(e) {
    cat("  ✗ Error:", e$message, "\n")
  })
}

# View results
cat("\n=== PROCESSING COMPLETE ===\n")
cat("Total daily records:", nrow(files_sum), "\n")
print(head(files_sum))

########## Species level summary
files_species_sum <- data.frame(
  Year = character(),
  Audiofile = character(),
  Stake = character(),
  Species = character(),
  Common_name = character(),
  Total_Detections = integer(),
  Detections_20conf = integer(),
  Detections_40conf = integer(),
  Detections_60conf = integer(),
  Detections_80conf = integer(),
  First_dect_date = character(),
  Last_dect_date = character(),
  Days_Detected = integer(),
  stringsAsFactors = FALSE
)

for (file_path in file_list){
  cat("Processing species for:", basename(file_path), "\n")
  
  tryCatch({
    data <- read.csv(file_path)
    
    if(nrow(data)==0){
      cat("File empty, skipping\n")
      next
    }
    
    audiofile_name <- basename(file_path)
    stake <- as.character(data$stake[1])
    year <- as.character(data$year[1])
    
    # Group by species and calculate detection counts by confidence level
    species_sum <- data |>
      group_by(common_name, scientific_name) |>
      summarize(
        Total_Detections = n(),
        Detections_20conf = sum(confidence >= 0.20),
        Detections_40conf = sum(confidence >= 0.40),
        Detections_60conf = sum(confidence >= 0.60),
        Detections_80conf = sum(confidence >= 0.80),
        First_Detection_Date = as.character(min(as.Date(recording_date))),
        Last_Detection_Date = as.character(max(as.Date(recording_date))),
        Days_Detected = length(unique(recording_date)),
        .groups = 'drop'
      )
    
    # Add each species to the summary
    for (i in 1:nrow(species_sum)) {
      species_data <- species_sum[i, ]
      
      new_row <- data.frame(
        Year = year,
        Audiofile = audiofile_name,
        Stake = stake,
        Species = as.character(species_data$scientific_name),
        Common_Name = as.character(species_data$common_name),
        Total_Detections = species_data$Total_Detections,
        Detections_20conf = species_data$Detections_20conf,
        Detections_40conf = species_data$Detections_40conf,
        Detections_60conf = species_data$Detections_60conf,
        Detections_80conf = species_data$Detections_80conf,
        First_Detection_Date = species_data$First_Detection_Date,
        Last_Detection_Date = species_data$Last_Detection_Date,
        Days_Detected = species_data$Days_Detected,
        stringsAsFactors = FALSE
      )
      
      files_species_sum <- rbind(files_species_sum, new_row)
    }
    
    cat("  ✓ Processed", nrow(species_sum), "species from file\n")
    
  }, error = function(e) {
    cat("  ✗ Error:", e$message, "\n")
  })
}

# View results
cat("\n=== SPECIES PROCESSING COMPLETE ===\n")
cat("Total species records:", nrow(files_species_sum), "\n")
print(head(files_species_sum))

########## Ploting # of detections
years_sum<- files_sum |>
  group_by(Stake_letter_num, Stake_number)|>
  summarize(years_available = length(unique(Year)),
            .groups = 'drop')
years_sum2 <- years_sum |>
  mutate(years_available = case_when(
    Stake_letter_num == 9 & Stake_number == 11 ~ 3,
    Stake_letter_num == 9 & Stake_number == 15 ~ 3,
    Stake_letter_num == 9 & Stake_number == 17 ~ 3, 
    Stake_letter_num == 11 & Stake_number == 5 ~ 3,
    TRUE ~ years_available
  ))
## Graph of years avail data (excluding 2024)
ggplot(years_sum2, aes(x = Stake_letter_num, y = Stake_number, 
                      fill = as.factor(years_available))) +
  geom_tile(color = "white", linewidth = 0.8, width = 1, height = 1) +  # 1x1 tiles
  geom_text(aes(label = years_available), color = "white", fontface = "bold", size = 4) +
  scale_fill_brewer(palette = "Blues", name = "Years of Data") +
  scale_x_continuous(
    breaks = unique(years_sum2$Stake_letter_num),
    labels = LETTERS[unique(years_sum2$Stake_letter_num)]
  ) +
  scale_y_continuous(breaks = unique(years_sum$Stake_number)) +
  labs(
    title = "Years of Data Available by Stake Location",
    x = "Stake Letter", 
    y = "Stake Number"
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

# Comparing # of unqiue species per minimum conf level
confidence_dist <- files_sum |>
  summarize(
    conf_20 = mean(species_20conf, na.rm = TRUE),
    conf_40 = mean(species_40conf, na.rm = TRUE),
    conf_60 = mean(species_60conf, na.rm = TRUE),
    conf_80 = mean(species_80conf, na.rm = TRUE)
  ) |>
  pivot_longer( 
    cols = everything(),
    names_to = "confidence_level",
    values_to = "mean_unique_species")

ggplot(confidence_dist, aes(x = confidence_level,
                            y = mean_unique_species))+
  geom_col(fill= "skyblue")+
  labs(title = "Average Unique Species Identified per Minimum Confidence Interval", x = "Minimum Confidence Interval",
       y= "Mean # of Unique Species")

########### Relating Weather Data to # of Detections 
weather_2019 <- read.csv("data/2019_weather.csv")
weather_2021 <- read.csv("data/2021_weather.csv")
weather_2022 <- read.csv("data/2022_weather.csv")
weather_2024 <- read.csv("data/2024_weather.csv")
weather_2025 <- read.csv("data/2025_weather.csv")
weather_files <- list(weather_2019, weather_2021, weather_2022,
                      weather_2024, weather_2025)
all_weather <- bind_rows(weather_files)

daily_avg_precip <- all_weather |>
  group_by(DATE)|>
  summarize(avg_precip = mean(PRCP, na.rm = TRUE),
            n_stations = n(),
            stations_reporting = paste(unique(STATION), collapse = ","))|>
  arrange(DATE)
avg_dect_day <- files_sum |>
  group_by(Year, Date)|>
  summarize(avg_dect = mean(Detections), avg_speciesct = mean(Unique_species))

#Plotting avg precipitation versus detections for each conf level
detection_long <- precip_detection_data |>
  select(Year, Date, avg_precip, Detections, dects_20conf, 
         dects_40conf, dects_60conf, dects_80conf)|>
  pivot_longer(cols = c(Detections, dects_20conf, dects_40conf,
                        dects_60conf, dects_80conf),
               names_to = "confidence_level",
               values_to = "detection_count")
detection_long <- detection_long |>
  mutate(confidence_level = case_when(
    confidence_level == "Detections" ~ "All",
    confidence_level == "dects_20conf" ~ "≥.20",
    confidence_level == "dects_40conf" ~ "≥.40",
    confidence_level == "dects_60conf" ~ "≥.60",
    confidence_level == "dects_80conf" ~ "≥.80"
  ))
ggplot(detection_long, aes(x = avg_precip, y = detection_count)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", se = TRUE, color = "steelblue") +
  facet_wrap(~ confidence_level, scales = "free_y") +
  stat_cor(method = "pearson", label.x.npc = "center", label.y.npc = "top") +
  labs(x = "Average Precipitation", 
       y = "Number of Detections",
       title = "Effect of Precipitation on Detection Counts by Confidence Interval") +
  theme_minimal()

ggplot(percip_dect_avgs, aes(x = avg_precip, y = avg_dect))+
  geom_point()+
  geom_smooth(method = "lm")+
  labs(x = "Daily Average Precipitation Across CH Weather Stations",
       y = "Average # of Detections per Day at Mason Farm",
       title = "Daily Precipitation vs. Average # of Detections Per Day Across Mason Farm")
# Plotting avg precipitation verses average unique species 
ggplot(percip_dect_avgs, aes(x = avg_precip, y = avg_speciesct))+
  geom_point()+
  geom_smooth(method = "lm")+
  labs(x = "Daily Average Precipitation Across CH Weather Stations",
       y = "Average # of Unique Species Detected per Day at Mason Farm",
       title = "Daily Precipitation vs. Average # of Unique Species Per Day Across Mason Farm")
