# Set working directory with spectra
setwd("/Users/hectorguillenalonso/Desktop/MQ_Assistant-main/Sample_dataset/mzML")

### Load file with functions:###

# File path to "MQ_Assistant-Functions.R" file
source("/Users/hectorguillenalonso/Desktop/MQ_Assistant-main/MQ_Assistant-Functions.R", encoding = 'UTF-8') 
# File path to "MQ_Assistant-Functions.R" file
source("/Users/hectorguillenalonso/Desktop/MQ_Assistant-main/Auto_MQ_Assistant.R", encoding = 'UTF-8')

### Run ###
# Execute MALDIquant workflow automatically and save data matrix and settings at the end
pretreatment <- autoMCAssistant()

