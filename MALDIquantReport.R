# Set working directory with spectra
setwd("~/Nextcloud/agraphia (3)/projects/Hector/Scrips R/MALDIquantReport/Dataset")

### Load file with functions:###

# File path to "MALDIquantReport-Functions.R" file
source("~/Nextcloud/agraphia (3)/projects/Hector/Scrips R/MALDIquantReport/MALDIquantReport-Functions.R", encoding = 'UTF-8') 
# File path to "MALDIquantReport-Functions.R" file
source("~/Nextcloud/agraphia (3)/projects/Hector/Scrips R/MALDIquantReport/Auto-MALDIquantReport.R", encoding = 'UTF-8')

### Run ###
# Execute MALDIquant workflow automatically and save data matrix and settings at the end
featureMatrix <- autoMALDIquantReport()