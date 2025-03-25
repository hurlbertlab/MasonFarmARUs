#############################

# Combining BirdNet Results Tables for K9

#############################
library(dplyr)
library(stringr)
#first need to go through each file and extract the data and name of file and add to new column
files = list.files(path = "C:/git/MasonFarmARUs/data/2022_k9_birdnet/", full.names = TRUE)

# Initialize an empty data frame to store combined results
combined_data <- data.frame()
for (f in files) {
  # Check if the file exists
  if (file.exists(f)) {
    # Read the data from the file (adjust the read function based on your file type)
    data <- read.csv(f)  # Use read_csv() for CSV files or another appropriate function
    
    # Extract the file name (without the full path)
    file_name <- basename(f)
    
    # Add a new column with the file name and extract date
    data <- data %>%
      mutate(file_name = file_name) %>%
      mutate(date = str_extract(file_name, "\\d{8}"),
             date = as.Date(date, format = "%Y%m%d"))  # Corrected date extraction
    
    # Append the current data frame to the combined data frame
    combined_data <- bind_rows(combined_data, data)
  } else {
    warning(paste("File does not exist:", f))
  }
}

write.csv(combined_data, "data/2022_k9_totalobvs.csv")

##############################

# Creating hour bins

combined_data <- combined_data |>
  mutate(hr_st = start/360, hr_end = end/3600)
