# *************************************************************************
#                     AUTOMATIC DLI-ESI-LCQ
# A function to autorun the software
# *************************************************************************

autoMALDIquantReport <- function () {
  settings <- loadUserSettings() # Load or create user settings
  rawData  <- suppressWarnings(spectra.load())  # Load and mean of microscans per spectra
  settings <- spectra.plot(rawData, settings) # Firs spectra
  importedDataPlot <- recordPlot()
  message <- paste0("Insert a number between 1 and ", length(rawData$spectraList), " for plot another spectra, type c to continue with the workflow")
  settings <- spectra.plot.loop(rawData, settings, message)
  processedData <- spectra.varianceStabilization(rawData, settings)
  rawData <- list(spectraList = processedData$spectraList, metadata = processedData$metadata)
  settings <- processedData$settings
  processedData <- suppressWarnings(spectra.smoothing(rawData, settings))
  rawData$spectraList  <- processedData$spectraList
  settings <- processedData$settings
  processedData <- spectra.baseLineCorrection(rawData, settings)
  rawData$spectraList  <- processedData$spectraList
  settings <- processedData$settings
  processedData <- spectra.normalization(rawData, settings)
  rawData$spectraList  <- processedData$spectraList
  settings <- processedData$settings
  processedData <- spectra.alignment(rawData, settings)
  rawData$spectraList  <- processedData$spectraList
  settings <- processedData$settings
  processedData <- spectra.signalToNoise(rawData, settings)
  settings <- processedData$settings
  peaks <- processedData$peaks
  noise <- processedData$estimatedNoise
  processedData <- spectra.mode(rawData, settings, peaks, noise)
  featureMatrix <- processedData$featureMatrix
  settings <- processedData$settings 
  processedData <- featureMatrix.adjust.resolution(featureMatrix, settings)
  featureMatrix <- processedData$featureMatrix
  settings <- processedData$settings
  return(list(featureMatrix = featureMatrix, settings = settings))
}
