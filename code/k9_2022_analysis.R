#############################

# Combining BirdNet Results Tables for K9

#############################
library(dplyr)
library(stringr)
library(lubridate)
library(ggplot2)
library(tidyr)
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

write.csv(combined_data, "data/k9_analysis/2022_k9_totalobvs.csv")

##############################

# Analyzing number of hourly calls 


##############################
combined_data <- read.csv("data/k9_analysis/2022_k9_totalobvs.csv")

combined_data <- combined_data |>
  rename("start" = "Start..s.", "end" = "End..s.")|> # Comment this out if you have already run this line
  mutate(hr_st = start/360, hr_end = end/3600)|>
  mutate(date = as.Date(date),  # Convert to Date object
                  julian_day = yday(date)) # Convert to julian day

# Creating hour bins 
combined_data <- combined_data |>
  mutate(hr_bin = ifelse(hr_end < 1, 3,
                         ifelse(hr_end < 2, 4,
                                ifelse(hr_end < 3, 5, NA))))

# Creating a dataframe with the total number of vocalizations for each day at each hour
hrly_data <- combined_data |>
  group_by(julian_day) |>
  summarize(
    total = n(),  # Count total rows in each group
    three_am_total = sum(hr_bin == 3, na.rm = TRUE),  # Count rows where hr_bin == 3
    four_am_total = sum(hr_bin == 4, na.rm = TRUE),   # Count rows where hr_bin == 4
    five_am_total = sum(hr_bin == 5, na.rm = TRUE)    # Count rows where hr_bin == 5
  )

write.csv(hrly_data, "data/k9_analysis/hourlyVocalData.csv")
#######

# Making box plots of calls made in each hour 

#######
long_data <- hrly_data |>
  pivot_longer(cols = c(three_am_total, four_am_total, five_am_total),
               names_to = "hour_bin",
               values_to = "total_calls")

# Create the boxplot
ggplot(long_data, aes(x = hour_bin, y = total_calls)) +
  geom_boxplot(fill = c("lightblue", "lightgreen", "lightcoral")) +
  labs(title = "Boxplot of Vocalizations by Hour for K9 2022",
       x = "Hour of Day",
       y = "Number of Vocalizations") +
  theme_minimal()

############

# Graphing how number of calls changes by Julian day- GGplot version
jdvcalls <- ggplot(hrly_data, aes(x = julian_day)) +
  geom_line(aes(y = total), color = "black") + 
  geom_line(aes(y = three_am_total), color = "lightblue") + 
  geom_line(aes(y = four_am_total), color = "lightgreen") + 
  geom_line(aes(y = five_am_total), color = "lightcoral")

jdvcalls

#other version
plot(hrly_data$julian_day,
     hrly_data$total, 
     xlab = "Julian Day",
     ylab = "Number of Vocalizations", 
     ylim = c(0, 900), xlim = c(151,201),
     type = 'l',
     col = 'black', 
     lwd = 2)
points(hrly_data$julian_day, 
       hrly_data$three_am_total,
       type = 'l',
       col = "lightblue",
       lwd = 2)
points(hrly_data$julian_day, 
       hrly_data$four_am_total,
       type = 'l',
       col = "lightgreen",
       lwd = 2)
points(hrly_data$julian_day, 
       hrly_data$five_am_total,
       type = 'l',
       col = "lightcoral",
       lwd = 2)
legend("topright", legend = c("Total",
                              "Three AM to Four AM",
                              "Four AM to Five AM",
                              "Five AM to Six AM"),
       col = c("black", "lightblue", "lightgreen", "lightcoral"),
       lwd = 2)
######################

# Analyzing how abundance of species vocalizations changes

######################