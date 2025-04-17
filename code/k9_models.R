# Linear models
library(ggplot2)
library(dplyr)
library(patchwork)

full_counts_clean <- read.csv("C:/git/MasonFarmARUs/data/full_counts_clean.csv")
full_counts_clean <- full_counts_clean |>
  mutate(min_bin_sq = min_bin^2)

lm_model <- function(species_name, data){
  species_data <- data |>
    filter(Bird.Call == species_name) 
  
  model <- lm(n ~ julian_day + min_bin + min_bin_sq, data = species_data)
  
  return(summary(model))
}

lm_model_no_signif <- function(species_name, data){
  species_data <- data |>
    filter(Bird.Call == species_name) 
  
  model <- lm(n ~ julian_day + min_bin, data = species_data)
  
  return(summary(model))
}

# Acadian Flycatcher
AF <- lm_model("Acadian Flycatcher", full_counts_clean)
AF

#Time of Day Effects
AF_time <- ggplot(full_counts_clean |>
         filter(Bird.Call == "Acadian Flycatcher"), 
       aes(x = min_bin, y = n)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), color = "lightgoldenrod", se = FALSE) +
  labs(title = "Effect of Time of Day on Acadian Flycatcher Counts",
       x = "Time of Day (15-min bins)",
       y = "Detection Count")+
  theme_minimal()

#Julian Day Effects
AF_jd<-ggplot(full_counts_clean |>
         filter(Bird.Call == "Acadian Flycatcher"),
         aes(x = julian_day, y = n))+
         geom_point(alpha = 0.4, color = "skyblue2") +
           geom_smooth(method = "lm", se = TRUE, color = "lightgoldenrod") +
           labs(
             title = "Seasonal Trend in Detections",
             subtitle = "Acadian Flycatcher - Julian Day vs Detection Count",
             x = "Julian Day",
             y = "Detection Count"
           ) +
           theme_minimal()


#Summer Tanager
ST <- lm_model("Summer Tanager", full_counts_clean)
ST

# Time of day effects 
ST_time <- ggplot(full_counts_clean |>
         filter(Bird.Call == "Summer Tanager"), 
       aes(x = min_bin, y = n)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), color = "lightblue", se = FALSE) +
  labs(title = "Effect of Time of Day on Summer Tanager Counts",
       x = "Time of Day (15-min bins)",
       y = "Detection Count")+ 
  theme_minimal()

#Julian Day effects
ST_jd<-ggplot(full_counts_clean |>
                filter(Bird.Call == "Summer Tanager"),
              aes(x = julian_day, y = n))+
  geom_point(alpha = 0.4, color = "skyblue2") +
  geom_smooth(method = "lm", se = TRUE, color = "navy") +
  labs(
    title = "Seasonal Trend in Detections",
    subtitle = "Summer Tanager - Julian Day vs Detection Count",
    x = "Julian Day",
    y = "Detection Count"
  ) +
  theme_minimal()

ST_no_min <- lm_model_no_signif("Summer Tanager", full_counts_clean)
ST_no_min

#Yellow-throated Vireo
YV <- lm_model("Yellow-throated Vireo", full_counts_clean)
YV

#Time of Day Effects
YV_time <- ggplot(full_counts_clean |>
         filter(Bird.Call == "Yellow-throated Vireo"), 
       aes(x = min_bin, y = n)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), color = "lightgreen", se = FALSE) +
  labs(title = "Effect of Time of Day on Yellow-throated Vireo Counts",
       x = "Time of Day (15-min bins)",
       y = "Detection Count")+
  theme_minimal()

#Julian Day effects
YV_jd <- ggplot(full_counts_clean |>
         filter(Bird.Call == "Yellow-throated Vireo"), 
       aes(x = julian_day, y = n)) +
  geom_point(alpha = 0.4, color = "skyblue2") +
  geom_smooth(method = "lm", se = TRUE, color = "darkgreen") +
  labs(
    title = "Seasonal Trend in Detections",
    subtitle = "Yellow-throated Vireo - Julian Day vs Detection Count",
    x = "Julian Day",
    y = "Detection Count"
  ) +
  theme_minimal()


#Eastern Wood-Pewee
EP <- lm_model("Eastern Wood-Pewee", full_counts_clean)
EP

#Time of Day effects
EP_time <- ggplot(full_counts_clean |>
         filter(Bird.Call == "Eastern Wood-Pewee"), 
       aes(x = min_bin, y = n)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), color = "lightcoral", se = FALSE) +
  labs(title = "Effect of Time of Day on Eastern Wood-Pewee Counts",
       x = "Time of Day (15-min bins)",
       y = "Vocalization Count")+
  theme_minimal()

#julian day effects
EP_jd <- ggplot(full_counts_clean |>
         filter(Bird.Call == "Eastern Wood-Pewee"), 
       aes(x = julian_day, y = n)) +
  geom_point(alpha = 0.4, color = "skyblue2") +
  geom_smooth(method = "lm", se = TRUE, color = "maroon") +
  labs(
    title = "Seasonal Trend in Detections",
    subtitle = "Yellow-throated Vireo - Julian Day vs Vocalization",
    x = "Julian Day",
    y = "Vocalization Count"
  ) +
  theme_minimal()

#Tufted Titmouse
TT <- lm_model("Tufted Titmouse", full_counts_clean)
TT

TT_no_min <- lm_model_no_signif("Tufted Titmouse", full_counts_clean)
TT_no_min

# time of day effects 
TT_time <- ggplot(full_counts_clean |>
         filter(Bird.Call == "Tufted Titmouse"), 
       aes(x = min_bin, y = n)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), color = "plum3", se = FALSE) +
  labs(title = "Effect of Time of Day on Tufted Titmouse Counts",
       x = "Time of Day (15-min bins)",
       y = "Vocalization Count")+
  theme_minimal()

#Julian Day Effects 
TT_jd <-ggplot(full_counts_clean |>
         filter(Bird.Call == "Tufted Titmouse"), 
       aes(x = julian_day, y = n)) +
  geom_point(alpha = 0.4, color = "skyblue2") +
  geom_smooth(method = "lm", se = TRUE, color = "purple4") +
  labs(
    title = "Seasonal Trend in Vocalizations",
    subtitle = "Tufted Titmouse - Julian Day vs Vocalization Count",
    x = "Julian Day",
    y = "Vocalization Count"
  ) +
  theme_minimal()

#Scarlet Tanager
SCT <- lm_model("Scarlet Tanager", full_counts_clean)
SCT

#time of day effects 
SCT_time <- ggplot(full_counts_clean |>
         filter(Bird.Call == "Scarlet Tanager"), 
       aes(x = min_bin, y = n)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), color = "royalblue", se = FALSE) +
  labs(title = "Effect of Time of Day on Scarlet Tanager Counts",
       x = "Time of Day (15-min bins)",
       y = "Vocalization Count")+
  theme_minimal()

#julian day effects
SCT_jd <- ggplot(full_counts_clean |>
         filter(Bird.Call == "Scarlet Tanager"), 
       aes(x = julian_day, y = n)) +
  geom_point(alpha = 0.4, color = "skyblue2") +
  geom_smooth(method = "lm", se = TRUE, color = "turquoise4") +
  labs(
    title = "Seasonal Trend in Detections",
    subtitle = "Scarlet Tanager - Julian Day vs Detection Count",
    x = "Julian Day",
    y = "Detection Count"
  ) +
  theme_minimal()

##################################

# Arranging these into a panel graph

##################################
# Plotting first half in a panel graph
png("C:/git/MasonFarmARUs/figures/linear_models.png", width = 800, height = 1200)
layout1 <- (AF_time | AF_jd ) / ( EP_time| EP_jd) / (YV_time | YV_jd)
layout1 + plot_annotation(title = "Bird Detection Patterns",
                         subtitle = "By Species Across Julian Day and Time of Day")
dev.off()

# Plotting second half in a panel graph 
png("C:/git/MasonFarmARUs/figures/linear_models2.png", width = 800, height = 1200)
layout1 <- (ST_time | ST_jd ) / ( TT_time| TT_jd) / (SCT_time | SCT_jd)
layout1 + plot_annotation(title = "Bird Detection Patterns",
                          subtitle = "By Species Across Julian Day and Time of Day")
dev.off()


##################################

# Creating a smaller graph to include in results

##################################
# Eastern Wood-Pewee (min sig) & Scarlet Tanager (min not sig)
png("C:/git/MasonFarmARUs/figures/linear_models_ex.png", width = 800, height = 800)
layout2 <- (EP_time | EP_jd) / (TT_time | TT_jd)
layout2 + plot_annotation()
dev.off()