#####################

# Using BirdNet to analyze one audiofile

#####################
library(birdnetR)
library(dplyr)
model <- birdnet_model_tflite("v2.4")

audio_path <- "C:/git/MasonFarmARUs/data/2022_k9_birdnet/20220531_093000.BirdNET.results.csv"

# Predict species in the audio file
results <- predict_species_from_audio_file(model = model, 
                                           audio_path, 
                                           min_confidence = 0.0, 
                                           keep_empty = FALSE)
