# Linear models

full_counts_clean <- full_counts_clean |>
  mutate(min_bin_sq = min_bin^2)

lm_model <- function(species_name, data){
  species_data <- data |>
    filter(Bird.Call == species_name) 
  
  model <- lm(n ~ julian_day + min_bin + min_bin_sq, data = species_data)
  
  return(summary(model))
}

AF <- lm_model("Acadian Flycatcher", full_counts_clean)
AF

ST <- lm_model("Summer Tanager", full_counts_clean)
ST

YV <- lm_model("Yellow-throated Vireo", full_counts_clean)
YV

EP <- lm_model("Eastern Wood-Pewee", full_counts_clean)
EP

TT <- lm_model("Tufted Titmouse", full_counts_clean)
TT

SCT <- lm_model("Scarlet Tanager", full_counts_clean)
SCT
SCT