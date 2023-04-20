# Set working directory with spectra
setwd("~/Nextcloud/agraphia (3)/projects/Hector/Scrips R/MQ_Assistant/Sample_dataset/mzML")

### Load file with functions:###

# File path to "MQ_Assistant-Functions.R" file
source("~/Nextcloud/agraphia (3)/projects/Hector/Scrips R/MQ_Assistant/MQ_Assistant-Functions.R", encoding = 'UTF-8') 
# File path to "MQ_Assistant-Functions.R" file
source("~/Nextcloud/agraphia (3)/projects/Hector/Scrips R/MQ_Assistant/Auto_MQ_Assistant.R", encoding = 'UTF-8')


### Set working directory to export results at Desktop
desktopWD <- "/Users/hectorguillenalonso/Desktop"

### Run ###
# Execute MALDIquant workflow automatically and save data matrix and settings at the end
pretreatment <- autoMQAssistant()
 
