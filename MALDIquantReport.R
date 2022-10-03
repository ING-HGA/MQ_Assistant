source("~/Nextcloud/agraphia (3)/projects/Hector/Scrips R/MALDIquantReport/MALDIquantReport-Functions.R", encoding = 'UTF-8') # File path to "MALDIquantReport-Functions.R" file
source("~/Nextcloud/agraphia (3)/projects/Hector/Scrips R/MALDIquantReport/Auto-MALDIquantReport.R", encoding = 'UTF-8') # File path to "Autorun.R"
setwd("~/Nextcloud/agraphia (3)/projects/Hector/Scrips R/MALDIquantReport/Dataset") # Working directory and spectra

featureMatrix <- autoMALDIquantReport() # Run automatically and save matrix and settings