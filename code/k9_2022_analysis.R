#############################

# Combining BirdNet Results Tables for K9

#############################
library(dplyr)
library(stringr)
library(lubridate)
library(ggplot2)
library(tidyr)
#first need to go through each file and extract the data and name of file and add to new column
files = list.files(path = "C:/git/MasonFarmARUs/data/2022_k9_birdnet_allConfs/", full.names = TRUE)

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

combined_data <- combined_data |>
  mutate(date = as.Date(date),  # Convert to Date object
         julian_day = yday(date)) # Convert to julian day

write.csv(combined_data, "data/k9_analysis/2022_k9_totalobvs.csv")

##############################

# Analyzing number of hourly calls 


##############################
combined_data <- read.csv("data/k9_analysis/2022_k9_totalobvs.csv") %>%
  rename("start" = "Start..s.", "end" = "End..s.")|> # Comment this out if you have already run this line
  mutate(hr_bin = ceiling(start/3600))
  

# Creating a dataframe with the total number of vocalizations for each day at each hour
hrly_sp_cts <- combined_data |>
  count(julian_day, hr_bin, Common.name)

# summarize(numSpp = n_distinct(Common.name))

write.csv(hrly_data, "data/k9_analysis/hourlyVocalData.csv")
#######

# Descriptive Statistics for Hourly Calls

####### 
hrly_data <- read.csv("data/k9_analysis/hourlyVocalData.csv")

#Total Vocalizations 

summary_total <- hrly_data |>
  summarize("smallest number of vocalizations" = min(total),
            "25% quantile" = quantile(total, probs=c(.25)),
            "average number" = mean(total),
            "median number" = median(total),
            "75% quantile" = quantile(total, probs=c(.75)),
            "largest number of vocalizations" = max(total),
            variance=var(total))|>
  glimpse()

write.csv(summary_total, "data/k9_analysis/stats_results/summary_total.csv")

#Three AM Vocalizations 

summary_threeAM <- hrly_data |>
  summarize("smallest number of vocalizations" = min(three_am_total),
            "25% quantile" = quantile(three_am_total, probs=c(.25)),
            "average number" = mean(three_am_total),
            "median number" = median(three_am_total),
            "75% quantile" = quantile(three_am_total, probs=c(.75)),
            "largest number of vocalizations" = max(three_am_total),
            variance=var(three_am_total))|>
  glimpse()
write.csv(summary_threeAM, "data/k9_analysis/stats_results/summary_threeAM.csv")
#Four AM Vocalizations

summary_fourAM <- hrly_data |>
  summarize("smallest number of vocalizations" = min(four_am_total),
            "25% quantile" = quantile(four_am_total, probs=c(.25)),
            "average number" = mean(four_am_total),
            "median number" = median(four_am_total),
            "75% quantile" = quantile(four_am_total, probs=c(.75)),
            "largest number of vocalizations" = max(four_am_total),
            variance=var(four_am_total))|>
  glimpse()

write.csv(summary_fourAM, "data/k9_analysis/stats_results/summary_fourAM.csv")

#Five AM Vocalizations

summary_fiveAM <- hrly_data |>
  summarize("smallest number of vocalizations" = min(five_am_total),
            "25% quantile" = quantile(five_am_total, probs=c(.25)),
            "average number" = mean(five_am_total),
            "median number" = median(five_am_total),
            "75% quantile" = quantile(five_am_total, probs=c(.75)),
            "largest number of vocalizations" = max(five_am_total),
            variance=var(five_am_total))|>
  glimpse()

write.csv(summary_fiveAM, "data/k9_analysis/stats_results/summary_fiveAM.csv")

######

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

# Distinct species identification versus jd and hours difference 

######

# overall 

######
total_obvs <- read.csv("data/k9_analysis/2022_k9_totalobvs.csv")

# Finding the amount of species observed for each day
unique_obvs <- total_obvs |>
  group_by(julian_day) |>
  summarize(unique= n_distinct(Common.name))

# graphing this 
plot(unique_obvs$julian_day,
     unique_obvs$unique,
     xlab = "Julian Day",
     ylab = "Number of Species Identified", 
     ylim = c(0, 16), xlim = c(151,201),
     type = 'l',
     col = 'black', 
     lwd = 2)

#####

# Breaking this down further into hour blocks 

#####

hrly_unique <- total_obvs |>
  rename("start" = "Start..s.", "end" = "End..s.")|> # Comment this out if you have already run this line
  mutate(hr_st = start/3600, hr_end = end/3600)|>
  mutate(hr_bin = ifelse(hr_end < 1, 3,
                         ifelse(hr_end < 2, 4,
                                ifelse(hr_end < 3, 5, NA))))|>
  group_by(julian_day, hr_bin)|>
  summarize(num_unique_species = n_distinct(Common.name), .groups = "drop")

write.csv(hrly_unique, "data/k9_analysis/unique_obvs.csv")

#graphing this 
ggplot(hrly_unique, aes(x = julian_day, y = num_unique_species, color = as.factor(hr_bin))) +
  geom_line() +  # Add lines for each hr_bin
  labs(
    title = "Unique Species Observed Over Julian Days",
    x = "Julian Day",
    y = "Number of Unique Species",
    color = "Hour of the Morning"
  ) +
  theme_minimal()

#######

# Descriptive Statistics for Hourly Unique Species

####### 
hrly_unique2 <- hrly_unique |>
  mutate(three_hr_bin = ifelse(hr_bin==3, TRUE, FALSE))|>
  mutate(four_hr_bin = ifelse(hr_bin==4, TRUE, FALSE))|>
  mutate(five_hr_bin = ifelse(hr_bin==5, TRUE, FALSE))

#Total Vocalizations 

summary_total <- unique_obvs |>
  summarize("smallest number of vocalizations" = min(unique),
            "25% quantile" = quantile(unique, probs=c(.25)),
            "average number" = mean(unique),
            "median number" = median(unique),
            "75% quantile" = quantile(unique, probs=c(.75)),
            "largest number of vocalizations" = max(unique),
            variance=var(unique))|>
  glimpse()

write.csv(summary_total, "data/k9_analysis/stats_results/summary_species_overall.csv")

#Three AM Vocalizations 

summary_threeAM <- hrly_unique2 |>
  summarize("smallest number of vocalizations" = min(num_unique_species[three_hr_bin==TRUE], na.rm = TRUE),
            "25% quantile" = quantile(num_unique_species[three_hr_bin==TRUE],na.rm = TRUE, probs=c(.25)),
            "average number" = mean(num_unique_species[three_hr_bin==TRUE],na.rm = TRUE),
            "median number" = median(num_unique_species[three_hr_bin==TRUE],na.rm = TRUE),
            "75% quantile" = quantile(num_unique_species[three_hr_bin==TRUE],na.rm = TRUE, probs=c(.75)),
            "largest number of vocalizations" = max(num_unique_species[three_hr_bin==TRUE],na.rm = TRUE),
            variance=var(num_unique_species[three_hr_bin==TRUE],na.rm = TRUE))|>
  glimpse()
write.csv(summary_threeAM, "data/k9_analysis/stats_results/summary_threeAM_species.csv")

#Four AM Vocalizations

summary_fourAM <- hrly_unique2 |>
  summarize("smallest number of vocalizations" = min(num_unique_species[four_hr_bin==TRUE], na.rm = TRUE),
            "25% quantile" = quantile(num_unique_species[four_hr_bin==TRUE],na.rm = TRUE, probs=c(.25)),
            "average number" = mean(num_unique_species[four_hr_bin==TRUE],na.rm = TRUE),
            "median number" = median(num_unique_species[four_hr_bin==TRUE],na.rm = TRUE),
            "75% quantile" = quantile(num_unique_species[four_hr_bin==TRUE],na.rm = TRUE, probs=c(.75)),
            "largest number of vocalizations" = max(num_unique_species[four_hr_bin==TRUE],na.rm = TRUE),
            variance=var(num_unique_species[four_hr_bin==TRUE],na.rm = TRUE))|>
  glimpse()
write.csv(summary_fourAM, "data/k9_analysis/stats_results/summary_fourAM_species.csv")

#Five AM Vocalizations

summary_fiveAM <- hrly_unique2 |>
  summarize("smallest number of vocalizations" = min(num_unique_species[five_hr_bin==TRUE], na.rm = TRUE),
            "25% quantile" = quantile(num_unique_species[five_hr_bin==TRUE],na.rm = TRUE, probs=c(.25)),
            "average number" = mean(num_unique_species[five_hr_bin==TRUE],na.rm = TRUE),
            "median number" = median(num_unique_species[five_hr_bin==TRUE],na.rm = TRUE),
            "75% quantile" = quantile(num_unique_species[five_hr_bin==TRUE],na.rm = TRUE, probs=c(.75)),
            "largest number of vocalizations" = max(num_unique_species[five_hr_bin==TRUE],na.rm = TRUE),
            variance=var(num_unique_species[five_hr_bin==TRUE],na.rm = TRUE))|>
  glimpse()
write.csv(summary_fiveAM, "data/k9_analysis/stats_results/summary_fiveAM_species.csv")
