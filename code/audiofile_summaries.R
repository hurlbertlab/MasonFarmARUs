# Load required libraries
library(dplyr)
library(lubridate)

# Initialize empty summary data frame
files_sum <- data.frame(
  Year = character(),
  Date = character(),
  Audiofile = character(),
  Stake = character(),
  Detections = integer(),
  Unique_species = integer(),
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
        Unique_species = length(unique(common_name))
      ) |>
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
        Unique_species = day_data$Unique_species,
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
