# Script to combine results for each stake for each year
install.packages("dplyr")
install.packages("readr")
install.packages("stringr")
install.packages("purrr")
library(dplyr)
library(readr)
library(stringr)
library(purrr)

#function to convert single file 
detection_file <- function(file_path){
  filename <- basename(file_path)
  filename_parts <- str_split(filename, "_")[[1]]
  
  location <- filename_parts[1]
  stake <- filename_parts[2]
  date_str <- filename_parts[3]
  year <- str_sub(date_str, 1, 4)
  month <- str_sub(date_str, 5, 6)
  day <- str_sub(date_str, 7, 8)
  recording_date <- as.Date(paste(month,day,year, sep = "/"))
  
  detections <-read.csv(file_path, col.names = c("common_names", "scientific_name", "start_time",
                                                 "end_time", "confidence", "label"))
  detections <- detections %>%
    mutate_(
      location = location, 
      stake = stake,
      year = year,
      recording_date = recording_date
    ) %>%
    select(location, stake, year, recording_date, common_name, scientific_name, 
           start_time, end_time, confidence)
  return(detections)
}

#function to process all detection files & create file
process_all_detections <- function(input_dir, results){
  
  if(!dir.exists(results)) {
    dir.create(results, recursive = TRUE)
  }
  
  #all csv list
  detection_files <- list.files(input_dir, pattern = "*_detections\\.csv$", full.names = TRUE)
  
  if(length(detection_files)==0){
    stop("No detection files found in the specified directory")
  }
  
  #process all files and combine into one dataframe 
  
  all_detections <- map_df(detection_files, process_detection_file)
  
  #unique stake-year combos 
  stake_year_combos <- all_detections %>%
    distinct(stake, year)%>%
    arrange(stake, year)
  
  #combined csv for each stake-year combo
  for(i in 1:nrow(stake_year_combos)){
    current_stake <- stake_year_combos$stake[i]
    current_year <- stake_year_combos$year[i]
    
    stake_year_data <- all_detections%>%
      filter(stake ==current_stake, year==current_year)%>%
      arrange(recording_date, start_time)
    
    ouput_filename <- paste0("birdnet_", current_stake, "_", current_year, ".csv")
    output_path <- file.path(results, output_filename)
    write.csv(stake_year_data, output_path)
    print("Saved:", output_filename, "(", nrow(stake_year_data), "detections)")
  }
  
  years <- unique(all_detections$year)
  for (yr in years) {
    yearly_data <- all_detections %>% filter(year == yr)
    yearly_file <- file.path(results, paste0("birdnet_all_stakes_", yr, ".csv"))
    write.csv(yearly_data, yearly_file)
    message("Saved yearly file: birdnet_all_stake", yr, ".csv")
  }
  
  stakes <- unique(all_detections$stake)
  for (stk in stakes) {
    stake_data <- all_detections %>% filter(stake == stk)
    stake_file <- file.path(output_dir, paste0("birdnet_", stk, "_all_years.csv"))
    write_csv(stake_data, stake_file)
    message("Saved stake file: birdnet_", stk, "_all_years.csv")
  }
  
  complete_file <- file.path(results, "birdnet_detections_complete.csv")
  write.csv(all_detections, complete_file)
  message("Saved complete dataset: birdnet_detections_complete. csv")
}

# Function for a specific stake-year combo (convinence function)
get_stake_year_data <- function(input_dir, stake, year){
  detection_files <- list.files(input_dir, pattern = "*_detections\\.csv$", full.names = TRUE)
  
  target_files <- detection_files[str_detect(detection_files, paste0("_", stake, "_", year))]
  
  if(length(target_files)==0){
    stop("No files found for stake ", stake, "and year", year)
  }
  
  combined_data <- map_df(target_files, process_detection_file)
  
  return(combined_data)
}
