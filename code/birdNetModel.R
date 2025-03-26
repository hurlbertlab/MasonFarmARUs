#####################

# Using BirdNet to analyze one audiofile

#####################
library(birdnetR)
library(dplyr)
model <- birdnet_model_tflite("v2.4")

audio_path <- "E:/2022/BigOakWoods/K9/20220622_093000.WAV"
# Predict species in the audio file
results <- predict_species_from_audio_file(model = model, 
                                           audio_path, 
                                           min_confidence = 0.0, 
                                           keep_empty = FALSE)
