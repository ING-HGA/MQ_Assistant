#MQ_Assistant v 0.1
#Laboratorio de Análisis Bioquímico e Instrumental 
#Héctor Guillén-Alonso, Nancy Shyrley García-Rojas & Robert Winkler

# *************************************************************************
#                               LIBRARIES
# MALDIquant
# MALDIquantForeign
# *************************************************************************
# Search and install libraries
if ("MALDIquant" %in% rownames(installed.packages()) == FALSE){
  install.packages("MALDIquant")
}
if ("MALDIquantForeign" %in% rownames(installed.packages()) == FALSE){
  install.packages("MALDIquantForeign")
}

# Load libraries
if( is.loaded( "MALDIquantForeign" ) == FALSE ) 
  library( MALDIquantForeign )

# *************************************************************************
#                       Get packages versions
# MALDIquant
# MALDIquantForeign
# *************************************************************************
packages.version <- list(MALDIquant = packageVersion("MALDIquant"), 
                         MALDIquantForeign = packageVersion("MALDIquantForeign"), 
                         MALDIquantSummary = 0.1)

# *************************************************************************
#                   Settings file creation and read
# Create a list to save the options
# *************************************************************************
#Ask to the user if he want to load their settings
loadUserSettings <- function(){
  loadSettings <- console.read("Type Yes to load settings; type any other key to use defaults")
  if (loadSettings == "Yes"|| loadSettings =="y"| loadSettings == "yes"| 
      loadSettings =="YES"| loadSettings =="Y") {
    settingsFile <- console.read("File name")
    loadSettings <- TRUE
  } else {
    loadSettings <- FALSE
  }
  
  # Determinate if settings will be create or load
  if (loadSettings == FALSE) {
    # Create settings
    settings <- list(tolerance = 0.002, halfWindowSize = 10, SNR = 2, minFrequency = 0, resolution = 2,
                     smoodIntensityMethod = "SavitzkyGolay", calibrateIntensitiesMethod = "TIC",
                     warpingMethod = "lowess", detectPeacksMethod = "MAD", spectraToPlot = NULL, firstPlot = TRUE, 
                     varianceStabilization = NULL, smoothing = NULL, baseLineMethod = "SNIP", iterations = 100, baseLine = NULL, 
                     calibrationMethod = "TIC", calibration = NULL, warpingMethod = "lowess", warping = NULL, S2N = 3, peakDetectionMethod = "MAD", 
                     peakDetection = NULL, featureMatrix = NULL, massRound = NULL, minFreqPeaks = 0.25, freqPeaks = NULL, spectraMode = NULL)
  } else {
    #Load settings
    settings.csv <- read.csv(settingsFile)
    settings <- list(tolerance = settings.csv$tolerance, halfWindowSize = settings.csv$halfWindowSize, resolution = settings.csv$resolution,
                     SNR = settings.csv$SNR, minFrequency = settings.csv$minFrequency, 
                     smoodIntensityMethod = settings.csv$smoodIntensityMethod, 
                     calibrateIntensitiesMethod = settings.csv$calibrateIntensitiesMethod,
                     warpingMethod = settings.csv$warpingMethod, detectPeacksMethod = settings.csv$detectPeacksMethod,
                     iterations = settings.csv$iterations, baseLineMethod = settings.csv$baseLineMethod, calibrationMethod = settings.csv$calibrationMethod,
                     warpingMethod = settings.csv$warping, S2N = settings.csv$S2N, peakDetectionMethod = settings.csv$peakDetectionMethod,
                     minFreqPeaks = settings.csv$minFreqPeaks,
                     spectraToPlot = NULL, firstPlot = TRUE, varianceStabilization = NULL, smoothing = NULL, baseLine = NULL, calibration = NULL, 
                     warping = NULL, peakDetection = NULL, featureMatrix = NULL, freqPeaks = NULL, massRound = NULL, spectraMode = NULL)
  }
  return(settings)
}

# *************************************************************************
#                   LOAD mzML FILES & MEAN SPECTRUM
# Loiad mzML files from the working directory and return a list composed by
# list of mean spectra  per file and a 
# *************************************************************************

spectra.load <- function() {
  
  files    <-list.files(pattern=".mzML")
  metaData <- c()
  rawData  <- c()
  
  for (i in 1:(length(files))){
    importspectrum <- import(files[i])
    sumspectrum    <- averageMassSpectra(importspectrum, method="mean")
    metaData       <- append(metaData,gsub(".mzML","",files[i]))
    rawData        <- append(rawData,sumspectrum) 
  }
  
  emptyData <- any(sapply(rawData, isEmpty))
  
  if (emptyData == TRUE){
    print("There is at least one empty spectrum")
  }
  
  return(list(spectraList=rawData, metadata=metaData))
}


# *************************************************************************
#                               READ CONSOLE
# Show a message for the user and add ": " to the message, and read de 
# values typed by the user
# *************************************************************************

console.read <- function (messageForUser) {
  
  parametro <- readline(prompt=paste(messageForUser, ": ", sep = ""))
  return(parametro)
  
}


# *************************************************************************
#                           CHECK FOR A NUMERIC INPUT
# Try to convert the input to a numeric and return the input as numeric and 
# success as true, if not return success = false
# *************************************************************************

input.is.numeric <- function (input) {
  
  input   <- suppressWarnings({ as.numeric(input) })
  success <- FALSE
  
  if (is.na(input) == FALSE){
    
    success <- TRUE
    
    return(list(input = input, success = success))
    
  } else {
    
    return(list(success = success))
    
  }
  
}


# *************************************************************************
#                           CHECK FOR RANGE
# Check for input to be in range with the object length and return the next
# value nearest to the range in case of out og range
# *************************************************************************

input.is.on.range <- function (input, objectToRange) {
  
  warningToUser <- FALSE
  success       <- FALSE
  maxRange      <- length(objectToRange)
  
  if (input < 1){
    
    input         <- 1
    warningToUser <- TRUE
    success       <- TRUE
    
    return(list(input = input, warningToUser = warningToUser, success = success))
    
  } else if (input > maxRange) {
    
    input         <- maxRange
    warningToUser <- TRUE
    success       <- TRUE
    
    message ("Warning: Typed number is out of range. Closest spectrum was ploted" )
    return(list(input = input, warningToUser = warningToUser, success = success))
    
    
  } else if (input >= 1 && input <= maxRange) {
    
    success <- TRUE
    
    return(list(input = input, success = success, warningToUser = warningToUser))
    
  } else {
    
    return(list(input = input, success = success, warningToUser = warningToUser))
    
  } 
}


# *************************************************************************
#                           PLOT SPECTRA
# Ask to the user a spectra number to be ploted 
# *************************************************************************

spectra.plot <- function(spectraObject, settings){
  if (settings$firstPlot == TRUE){
    plot(spectraObject$spectraList[[1]], main = paste("Spectra: ", spectraObject$metadata[1], sep = ""))
    settings$spectraToPlot <- 1
    settings$firstPlot <- FALSE
  } else {
    plot(spectraObject$spectraList[[as.numeric(settings$spectraToPlot)]], main = paste("Spectra: ", spectraObject$metadata[as.numeric(settings$spectraToPlot)], sep = ""))
  }
  return(settings)
}


# *************************************************************************
#                           LOOP PLOT SPECTRA
# Create a loop to plot as many spectra as user desire
# *************************************************************************

spectra.plot.loop <- function(spectraObject, settings, message){
  newPlot <- TRUE
  
  while (newPlot == TRUE) {
    
    numberOfPlot <- console.read(message)
    stepDone     <- NULL
    
    if (input.is.numeric(numberOfPlot)$success){
      numberOfPlot <- input.is.on.range(numberOfPlot, spectraObject$spectraList)$input
      settings$spectraToPlot<-as.numeric(numberOfPlot)
      spectra.plot(spectraObject, settings)
    } 
    
    if (input.is.numeric(numberOfPlot)$success == FALSE) {
      newPlot <- FALSE
      if (numberOfPlot == "c"){
        return(settings)
      }
    }
  }
}

# *************************************************************************
#                  VARIANCE STABILIZATION
# Preview in a specific plot of the appliance of variance stabilization 
# loop preview, apply function or slip function 
# *************************************************************************

spectra.varianceStabilization <- function(spectraData, settings){
  print("Variance stabilization")
  newPlot                <- NULL
  transformedSpectra     <- NULL
  varianceStabilization  <- NULL
  tempTransformedSpectra <- transformIntensity(spectraData$spectraList[settings$spectraToPlot], method = "sqrt")
  
  plot(spectraData$spectraList[[settings$spectraToPlot]], main = spectraData$metadata[[settings$spectraToPlot]])
  lines(mass(tempTransformedSpectra[[1]]),intensity(tempTransformedSpectra[[1]]), col = "red")
  
  consoleValue           <- console.read("Type a number to preview variance stabilization on spectra; type c to apply and continue; or s to skip step")
  
  if(input.is.numeric(consoleValue)$success){
    newPlot <- TRUE
    while (newPlot == TRUE) {
      spectraToPlot          <- consoleValue
      settings$spectraToPlot <- as.numeric(spectraToPlot)
      tempTransformedSpectra <-  transformIntensity(spectraData$spectraList[as.numeric(input.is.on.range(consoleValue, spectraData$spectraList)$input)], method = "sqrt")
      
      plot(spectraData$spectraList[[as.numeric(input.is.on.range(consoleValue, spectraData$spectraList)$input)]], main = spectraData$metadata[as.numeric(input.is.on.range(consoleValue, spectraData$spectraList)$input)])
      lines(mass(tempTransformedSpectra[[1]]), intensity(tempTransformedSpectra[[1]]), col = "red")
      consoleValue <- console.read("Type a number to preview variance stabilization on spectra; type c to apply and continue; or s to skip step")
      if(input.is.numeric(consoleValue)$success == FALSE){
        newPlot <- FALSE
      }
    }
  }
  if(consoleValue == "c" | consoleValue == "C"){
    transformedSpectra              <- transformIntensity(spectraData$spectraList, method = "sqrt")
    settings$varianceStabilization  <- TRUE
    return(list(spectraList = transformedSpectra, metadata = spectraData$metadata, settings = settings))
  } 
  if(consoleValue == "s" | consoleValue == "S"){
    settings$varianceStabilization  <- FALSE
    return(list(spectraList = spectraData$spectraList, metadata = spectraData$metadata, settings = settings))
  }
}

# *************************************************************************
#                               Smoothing
# Preview in a specific plot of the appliance of variance stabilization 
# loop preview, apply function or slip function 
# *************************************************************************
spectra.smoothing <- function(spectraData, settings) {
  print("Smoothing")
  transformedSpectra     <- NULL
  smoothing              <- NULL
  tempTransformedSpectra <- smoothIntensity(spectraData$spectraList[settings$spectraToPlot], method="SavitzkyGolay", halfWindowSize=settings$halfWindowSize)
  
  plot(spectraData$spectraList[[settings$spectraToPlot]], main = spectraData$metadata[[settings$spectraToPlot]])
  lines(mass(tempTransformedSpectra[[1]]),intensity(tempTransformedSpectra[[1]]), col = "red")
  
  consoleValue           <- console.read("Type a number to preview smoothing on spectra; type c to apply and continue, or s to skip step")
  
  if(input.is.numeric(consoleValue)$success){
    newPlot <- TRUE
    while (newPlot == TRUE) {
      spectraToPlot          <- consoleValue
      tempTransformedSpectra <- smoothIntensity(spectraData$spectraList[as.numeric(input.is.on.range(consoleValue, spectraData$spectraList)$input)], 
                                                method = "SavitzkyGolay", halfWindowSize=settings$halfWindowSize)
      
      plot(spectraData$spectraList[[as.numeric(input.is.on.range(consoleValue, spectraData$spectraList)$input)]], main = spectraData$metadata[as.numeric(input.is.on.range(consoleValue, spectraData$spectraList)$input)])
      lines(mass(tempTransformedSpectra[[1]]), intensity(tempTransformedSpectra[[1]]), col = "red")
      consoleValue <- console.read("Type a number to preview smoothing on spectra; type c to apply and continue, or s to skip step")
      
      if (input.is.numeric(consoleValue)$success == FALSE) {
        
        newPlot <- FALSE
        
        if(consoleValue == "c" | consoleValue == "C"){
          transformedSpectra     <- smoothIntensity(spectraData$spectraList, method = "SavitzkyGolay", halfWindowSize=settings$halfWindowSize)
          settings$smoothing     <- TRUE
          settings$spectraToPlot <- as.numeric(spectraToPlot)
        } 
        if(consoleValue == "s" | consoleValue == "S"){
          settings$smoothing  <- FALSE
        } 
        
      }
    }
  }  
  
  if (consoleValue == "c" | consoleValue == "C") {
    transformedSpectra     <- smoothIntensity(spectraData$spectraList, method = "SavitzkyGolay", halfWindowSize=settings$halfWindowSize)
    settings$smoothing     <- TRUE
    return(list(spectraList = transformedSpectra, metadata = spectraData$metadata, settings = settings))
    
  } 
  
  if(consoleValue == "s" | consoleValue == "S"){
    settings$smoothing  <- FALSE
    return(list(spectraList = spectraData$spectraList, metadata = spectraData$metadata, settings = settings))
  } 
  
}

# *************************************************************************
#                    BASE LINE CORRECTION
# Ask to the user number of interations and spectra to be apply the 
# base line correction
# *************************************************************************

spectra.baseLineCorrection <- function(spectraData, settings){
  print("Baseline")
  settingsBackUp         <- settings
  newPlotIterations      <- TRUE
  tempTransformedSpectra <- estimateBaseline(spectraData$spectraList[settings$spectraToPlot][[1]], method=settings$baseLineMethod, iterations = settings$iterations)
  plot(spectraData$spectraList[[settings$spectraToPlot]], main = spectraData$metadata[[settings$spectraToPlot]])
  lines(tempTransformedSpectra[,1],tempTransformedSpectra[,2], col = "red", lwd = 2)
  
  while (newPlotIterations) {
    
    consoleValueIteration<-console.read("Type a number of iterations to generate and preview baseline correction on the actual spectrum; type c to apply and continue; p to change the preview of spectrum, or s to skip step")
    
    if (input.is.numeric(consoleValueIteration)$succes){
      
      settings$iterations    <- as.numeric(consoleValueIteration)
      tempTransformedSpectra <- estimateBaseline(spectraData$spectraList[settings$spectraToPlot][[1]], method=settings$baseLineMethod, iterations = settings$iterations)
      
      plot(spectraData$spectraList[[settings$spectraToPlot]], main = spectraData$metadata[[settings$spectraToPlot]])
      lines(tempTransformedSpectra[,1],tempTransformedSpectra[,2], col = "red", lwd = 2)
    }
    
    if (consoleValueIteration == "c" | consoleValueIteration == "C"){
      
      newPlotIterations       <- FALSE
      settings$baseLine       <- TRUE
      spectraData$spectraList <- removeBaseline(spectraData$spectraList, method=settings$baseLineMethod, iterations = settings$iterations)
      
      plot(spectraData$spectraList[[settings$spectraToPlot]], main = spectraData$metadata[[settings$spectraToPlot]])
      
    }
    if (consoleValueIteration == "s" | consoleValueIteration == "S"){
      
      settings          <- settingsBackUp
      newPlotIterations <- FALSE
      settings$baseLine <- FALSE
      
      plot(spectraData$spectraList[[settings$spectraToPlot]], main = spectraData$metadata[[settings$spectraToPlot]])
      
    }
    
    if (consoleValueIteration == "p" | consoleValueIteration == "P"){
      
      newPlot <- TRUE
      
      while (newPlot) {
        
        consoleValue <- console.read("Type a number of iterations to generate and preview baseline correction on the actual spectrum; type c to apply and continue; p to change the preview of spectrum, or s to skip step")
        
        if (input.is.numeric(consoleValue)$succes){
          
          settings$spectraToPlot <- as.numeric(input.is.on.range(consoleValue, spectraData$spectraList)$input)
          
          plot(spectraData$spectraList[[settings$spectraToPlot]], main = spectraData$metadata[[settings$spectraToPlot]])
          lines(tempTransformedSpectra[,1],tempTransformedSpectra[,2], col = "red", lwd = 2)
          
        }
        
        if (consoleValue == "s" | consoleValue == "S"){
          
          settings          <- settingsBackUp
          newPlotIterations <- FALSE
          newPlot           <- FALSE
          settings$baseLine <- FALSE
          
          plot(spectraData$spectraList[[settings$spectraToPlot]], main = spectraData$metadata[[settings$spectraToPlot]])
          
        }
        
        if (consoleValue == "c" | consoleValue == "C"){
          
          newPlotIterations <- FALSE
          newPlot           <- FALSE
          settings$baseLine <- TRUE
          spectraData$spectraList <- removeBaseline(spectraData$spectraList, method=settings$baseLineMethod, iterations = settings$iterations)
          
          plot(spectraData$spectraList[[settings$spectraToPlot]], main = spectraData$metadata[[settings$spectraToPlot]])
          
        }
        
        if (consoleValue == "i" | consoleValue == "I"){
          
          newPlotIterations <- TRUE
          newPlot           <- FALSE
          
        }
      }
    }
  }
  
  return(list(spectraList = spectraData$spectraList, metadata = spectraData$metadata, settings = settings))
  
}

# *************************************************************************
#                      INTENSITY CALIBRATION/NORMALIZATION
# Preview normalization
# *************************************************************************
spectra.normalization <- function (spectraData, settings) {
  
  print("Normalization")
  
  settingsBackUp         <- settings
  newPlot                <- TRUE
  tempTransformedSpectra <- calibrateIntensity(spectraData$spectraList[settings$spectraToPlot][[1]], method=settings$calibrateIntensitiesMethod)
  
  plot(spectraData$spectraList[[settings$spectraToPlot]], main = spectraData$metadata[[settings$spectraToPlot]])
  lines(mass(tempTransformedSpectra),intensity(tempTransformedSpectra), col = "red")
  
  while (newPlot) {
    
    consoleValue<-console.read("Type a number to preview normalization on that spectrum; type c to apply and continue, or s to skip step")
    
    if (input.is.numeric(consoleValue)$succes){
      
      settings$spectraToPlot <- as.numeric(input.is.on.range(consoleValue, spectraData$spectraList)$input)
      tempTransformedSpectra <- calibrateIntensity(spectraData$spectraList[settings$spectraToPlot][[1]], method=settings$calibrateIntensitiesMethod)
      
      plot(spectraData$spectraList[[settings$spectraToPlot]], main = spectraData$metadata[[settings$spectraToPlot]])
      lines(mass(tempTransformedSpectra),intensity(tempTransformedSpectra), col = "red")
    }
    
    if (consoleValue == "c" | consoleValue == "C"){
      
      newPlot                 <- FALSE
      settings$calibration    <- TRUE
      spectraData$spectraList <- calibrateIntensity(spectraData$spectraList, method=settings$calibrateIntensitiesMethod)
      
      plot(spectraData$spectraList[[settings$spectraToPlot]], main = spectraData$metadata[[settings$spectraToPlot]])
      
    }
    if (consoleValue == "s" | consoleValue == "S"){
      
      settings             <- settingsBackUp
      newPlot              <- FALSE
      settings$calibration <- FALSE
      
      plot(spectraData$spectraList[[settings$spectraToPlot]], main = spectraData$metadata[[settings$spectraToPlot]])
      
    }
    
  }
  
  return(list(spectraList = spectraData$spectraList, metadata = spectraData$metadata, settings = settings))
  
}

# *************************************************************************
#                        WARPING/ALIGNMENT
# Create a loop to plot as many warpings estimation spectra as user desire
# *************************************************************************

spectra.alignment <- function (spectraData, settings) {
  
  print("Alignment")
  
  settingsBackUp         <- settings
  newPlot                <- TRUE
  tempTransformedSpectra <- alignSpectra(spectraData$spectraList, warpingMethod = settings$warpingMethod, halfWindowSize = settings$halfWindowSize,
                                         SNR = settings$SNR, tolerance = settings$tolerance)
  
  plot(spectraData$spectraList[[settings$spectraToPlot]], main = spectraData$metadata[[settings$spectraToPlot]])
  lines(mass(tempTransformedSpectra[[settings$spectraToPlot]]), intensity(tempTransformedSpectra[[settings$spectraToPlot]]), col = rgb(red = 1, green = 0, blue = 0, alpha = 0.5))
  
  while (newPlot) {
    
    consoleValue<-console.read("Type a number to preview alignment on that spectrum; type c to apply and continue, or s to skip step")
    
    if (input.is.numeric(consoleValue)$succes){
      
      settings$spectraToPlot <- as.numeric(input.is.on.range(consoleValue, spectraData$spectraList)$input)
      
      plot(spectraData$spectraList[[settings$spectraToPlot]], main = spectraData$metadata[[settings$spectraToPlot]])
      lines(mass(tempTransformedSpectra[[settings$spectraToPlot]]), intensity(tempTransformedSpectra[[settings$spectraToPlot]]), col = rgb(red = 1, green = 0, blue = 0, alpha = 0.5))
    }
    
    if (consoleValue == "c" | consoleValue == "C"){
      
      newPlot                 <- FALSE
      settings$warping        <- TRUE
      spectraData$spectraList <- tempTransformedSpectra
      
      plot(spectraData$spectraList[[settings$spectraToPlot]], main = spectraData$metadata[[settings$spectraToPlot]])
      
    }
    if (consoleValue == "s" | consoleValue == "S"){
      
      settings             <- settingsBackUp
      newPlot              <- FALSE
      settings$warping     <- FALSE
      
      plot(spectraData$spectraList[[settings$spectraToPlot]], main = spectraData$metadata[[settings$spectraToPlot]])
      
    }
    
  }
  
  return(list(spectraList = spectraData$spectraList, metadata = spectraData$metadata, settings = settings))
  
}

# *************************************************************************
#                        Signal to noise
# Create a loop to plot as many signal to noise estimation spectra as user desire
# *************************************************************************

spectra.signalToNoise    <- function(spectraData, settings){
  
  print("Signal to noise")
  
  settingsBackUp         <- settings
  NewPlotSN              <- TRUE
  peaks                  <- NULL
  noise                  <- estimateNoise(spectraData$spectraList[settings$spectraToPlot][[1]])
  
  plot(spectraData$spectraList[[settings$spectraToPlot]], main = spectraData$metadata[[settings$spectraToPlot]])
  lines(noise[,1], noise[, 2]*2, col = rgb(red = 0, green = 0, blue = 1, alpha = 0.5), lwd = 2)
  lines(noise[,1], noise[, 2]*3, col = rgb(red = 1, green = 0, blue = 0, alpha = 0.5), lwd = 2)
  legend("topright", inset = .01, legend = c("s/n = 3", "s/n = 2"), col = c("red", "blue"), lty = 1)
  
  while (NewPlotSN) {
    
    consoleValueSN<-console.read("Type a number of signal-to-noise ratio to preview peak peaking on actual spectrum; type c to apply and continue; p to change spectra preview, or s to skip step")
    
    if (input.is.numeric(consoleValueSN)$succes){
      
      settings$S2N <- as.numeric(consoleValueSN)
      noise        <- estimateNoise(spectraData$spectraList[settings$spectraToPlot][[1]])
      peaks        <- detectPeaks(spectraData$spectraList[settings$spectraToPlot], method=settings$peakDetectionMethod, 
                                  halfWindowSize=settings$halfWindowSize, SNR=settings$S2N)
      
      plot(spectraData$spectraList[[settings$spectraToPlot]], main = spectraData$metadata[[settings$spectraToPlot]])
      lines(noise[,1], noise[, 2]*settings$S2N, col = rgb(red = 1, green = 0, blue = 0, alpha = 0.5), lwd = 2)
      points(peaks[[1]], col = rgb(red = 1, green = 0, blue = 0, alpha = 0.5), pch=4)
      
    }
    
    if (consoleValueSN == "c" | consoleValueSN == "C"){
      
      NewPlotSN               <- FALSE
      peaks                   <- detectPeaks(spectraData$spectraList, method=settings$peakDetectionMethod,
                                             halfWindowSize=settings$halfWindowSize, SNR=settings$S2N)
      peaks                   <- binPeaks(peaks, tolerance= settings$tolerance)
      noise                   <- lapply(spectraData$spectraList, estimateNoise)
      noise                   <- lapply(noise, function(x){x[1,2]})
  
    }
    
    if (consoleValueSN == "s" | consoleValueSN == "S"){
      
      settings               <- settingsBackUp
      NewPlotSN              <- FALSE
      
    }
    
    if (consoleValueSN == "p" | consoleValueSN == "P"){
      
      newPlot <- TRUE
      
      while (newPlot) {
        
        consoleValue <- console.read("Type a number for preview peak peaking on different spectrum; type c to apply and continue; i to change iterations, or s to skip step")
        
        if (input.is.numeric(consoleValue)$succes){
          
          settings$spectraToPlot <- as.numeric(input.is.on.range(consoleValue, spectraData$spectraList)$input)
          noise                  <- estimateNoise(spectraData$spectraList[settings$spectraToPlot][[1]])
          peaks                  <- detectPeaks(spectraData$spectraList[settings$spectraToPlot], method=settings$peakDetectionMethod, 
                                                halfWindowSize=settings$halfWindowSize, SNR=settings$S2N)
          
          plot(spectraData$spectraList[[settings$spectraToPlot]], main = spectraData$metadata[[settings$spectraToPlot]])
          lines(noise[,1], noise[, 2]*settings$S2N, col = rgb(red = 1, green = 0, blue = 0, alpha = 0.5), lwd = 2)
          points(peaks[[1]], col = rgb(red = 1, green = 0, blue = 0, alpha = 0.5), pch=4)
          
        }
        
        if (consoleValue == "s" | consoleValue == "S"){
          
          settings               <- settingsBackUp
          NewPlotSN              <- FALSE
          newPlot                <- FALSE
          
        }
        
        if (consoleValue == "c" | consoleValue == "C"){
          
          NewPlotSN               <- FALSE
          newPlot                 <- FALSE
          peaks                   <- detectPeaks(spectraData$spectraList, method=settings$peakDetectionMethod,
                                                 halfWindowSize=settings$halfWindowSize, SNR=settings$S2N)
          peaks                   <- binPeaks(peaks, tolerance= settings$tolerance)
          noise                   <- lapply(spectraData$spectraList, estimateNoise)
          noise                   <- lapply(noise, function(x){x[1,2]})
          
        }
        
        if (consoleValue == "i" | consoleValue == "I"){
          
          NewPlotSN <- TRUE
          newPlot   <- FALSE
          
        }
      }
    }
  }
  return(list(peaks = peaks, settings = settings, estimatedNoise = noise))
}

# *************************************************************************
#                        CHOSE TYPE OF MATRIX
# loop to preview spectra selected in profile and centroid mode
# and chose a mode to create a matrix
# *************************************************************************

spectra.mode <- function(rawData, settings, peaks, noise){
  
  print("Profile or centroid")
  
  newPlot <- TRUE
  matrix  <- NULL
  
  while (newPlot == TRUE) {
    
    message              <- "Chose the number of spectrum to compare between profile and centroid matrix creation mode; type p to choose a profile, or c for centroid"
    numberOfPlot         <- settings$spectraToPlot
    settings$spectraMode <- NULL
    intensityesProfile   <- intensity(rawData$spectraList[[settings$spectraToPlot]]) - as.numeric(noise[[settings$spectraToPlot]])
    intensityesProfile[which(intensityesProfile <= 0)] <- 0
    
    par(mfrow = c(1,2))
    plot(peaks[[settings$spectraToPlot]], main = "Centroid mode")
    plot(mass(rawData$spectraList[[settings$spectraToPlot]]), intensityesProfile, 
    main = "Profile mode", type = "l", ylab = "intensity", xlab = "m/z" )
    mtext(rawData$metadata[[settings$spectraToPlot]], outer = TRUE, cex = 1.2, adj = 0.5, padj=2)
    
    numberOfPlot <- console.read(message)
    
    if (input.is.numeric(numberOfPlot)$success){
      numberOfPlot <- input.is.on.range(numberOfPlot, rawData$spectraList)$input
      settings$spectraToPlot<-as.numeric(numberOfPlot)
    } 
    
    if (input.is.numeric(numberOfPlot)$success == FALSE) {
      
      newPlot <- FALSE
      
      if (numberOfPlot == "P" | numberOfPlot == "p" ){
        
        par(mfrow = c(1,1))
        
        matrix               <- spectra.featureMatrix.profile(rawData, settings, noise)
        settings$spectraMode <- "Profile"
        
        } else if (numberOfPlot == "C" | numberOfPlot == "c" ){
         
          par(mfrow = c(1,1))
          
          matrix               <- spectra.featureMatrix.centroid(rawData, peaks, settings)
          settings$spectraMode <- "Centroid"
        
        } else {
          
        print("Please verify your selection")
        spectra.mode(rawData, settings, peaks, noise)
        
      }
    }
  }
  
  return(list(featureMatrix = matrix$featureMatrix, settings = matrix$settings))
}

# *************************************************************************
#                        CENTROID MATRIX
# Create a loop to plot as many min frequencies estimation spectra as user desire
# and create a matrix in centroid after accept it
# *************************************************************************

spectra.featureMatrix.centroid <- function(spectraData, peaks, settings){
  settingsBackUp         <- settings
  NewPlotFreq            <- TRUE
  peaksTemp              <- filterPeaks(peaks, minFrequency = settings$minFreqPeaks)
  
  plot(peaks[[settings$spectraToPlot]], main = spectraData$metadata[[settings$spectraToPlot]])
  lines(peaksTemp[[settings$spectraToPlot]], main = spectraData$metadata[[settings$spectraToPlot]], 
        col = rgb(red = 1, green = 0, blue = 0, alpha = 1))
  
  while (NewPlotFreq) {
    
    consoleValueFreq<-console.read("Type a number between 0 and 1 for a minimum frequency of peaks on all spectra to be considered; type c to apply and continue; i to change iterations, or s to skip step")
    
    if (input.is.numeric(consoleValueFreq)$succes){
      
      if (consoleValueFreq < 0){
        consoleValueFreq <- 0
        warning("You inserted a number out of range, it was adjusted to a minimum of the range (0)")
      }
      
      if (consoleValueFreq > 1){
        consoleValueFreq <- 1
        warning("You inserted a number out of range, it was adjusted to the maximum of the range (1)")
      }
      
      settings$minFreqPeaks <- as.numeric(consoleValueFreq)
      peaksTemp             <- filterPeaks(peaks, minFrequency = settings$minFreqPeaks)
      
      plot(peaks[[settings$spectraToPlot]], main = spectraData$metadata[[settings$spectraToPlot]])
      lines(peaksTemp[[settings$spectraToPlot]], main = spectraData$metadata[[settings$spectraToPlot]], 
            col = rgb(red = 1, green = 0, blue = 0, alpha = 1))
      
    }
    
    if (consoleValueFreq == "c" | consoleValueFreq == "C"){
      
      NewPlotFreq             <- FALSE
      settings$freqPeaks      <- TRUE
      peaks                   <- filterPeaks(peaks, minFrequency = settings$minFreqPeaks)
      featureMatrix           <- intensityMatrix(peaks, spectraData$spectraList)
      
      plot(peaks[[settings$spectraToPlot]], main = spectraData$metadata[[settings$spectraToPlot]])
      
    }
    
    if (consoleValueFreq == "s" | consoleValueFreq == "S"){
      
      settings                <- settingsBackUp
      peaks                   <- peaksBackUp
      NewPlotFreq             <- FALSE
      settings$freqPeaks      <- FALSE
      
      plot(peaks[[settings$spectraToPlot]], main = spectraData$metadata[[settings$spectraToPlot]])
      
    }
    
    if (consoleValueFreq == "p" | consoleValueFreq == "P"){
      
      newPlot <- TRUE
      
      while (newPlot) {
        
        consoleValue <- console.read("Type a number between 0 and 1 for a minimum frequency of peaks on all spectra to be considered; type c to apply and continue; i to change iterations, or s to skip step")
        
        if (input.is.numeric(consoleValue)$succes){
          
          settings$spectraToPlot <- as.numeric(input.is.on.range(consoleValue, spectraData$spectraList)$input)
          peaksTemp              <- filterPeaks(peaks, minFrequency = settings$minFreqPeaks)
          
          plot(peaks[[settings$spectraToPlot]], main = spectraData$metadata[[settings$spectraToPlot]])
          lines(peaksTemp[[settings$spectraToPlot]], main = spectraData$metadata[[settings$spectraToPlot]], 
                col = rgb(red = 1, green = 0, blue = 0, alpha = 1))
          
        }
        
        if (consoleValue == "s" | consoleValue == "S"){
          
          settings               <- settingsBackUp
          peaks                  <- peaksBackUp
          NewPlotFreq            <- FALSE
          newPlot                <- FALSE
          settings$freqPeaks     <- FALSE
          
          plot(peaks[[settings$spectraToPlot]], main = spectraData$metadata[[settings$spectraToPlot]])
          
        }
        
        if (consoleValue == "c" | consoleValue == "C"){
          
          NewPlotFreq             <- FALSE
          newPlot                 <- FALSE
          settings$freqPeaks      <- TRUE
          peaks                   <- filterPeaks(peaks, minFrequency = settings$minFreqPeaks)
          featureMatrix           <- intensityMatrix(peaks, spectraData$spectraList)
          
          plot(peaks[[settings$spectraToPlot]], main = spectraData$metadata[[settings$spectraToPlot]])
          
        }
        
        if (consoleValue == "i" | consoleValue == "I"){
          
          NewPlotFreq <- TRUE
          newPlot   <- FALSE
          
        }
      }
    }
  }
  return(list(featureMatrix = featureMatrix, settings = settings))
}

# *************************************************************************
#                        PROFILE MATRIX
# Create a data feature matrix on prifile mode
# *************************************************************************

spectra.featureMatrix.profile <- function(spectraData, settings, noise){
  
  mass          <- mass(spectraData$spectraList[[1]])
  featureMatrix <- matrix(nrow = length(spectraData$spectraList), ncol = length(mass))
  
  
  for (i in 1:length(spectraData$spectraList)) {
    
    intensity <- intensity(spectraData$spectraList[[i]])
    intensity <- intensity - as.numeric(noise[[i]])
    intensity[which(intensity <= 0)] <- 0
    
    featureMatrix[i,] <-  intensity
    }
  
  colnames(featureMatrix) <- mass
  zeroCols                <- which(apply(featureMatrix, 2, max)<=0)
  featureMatrix           <- featureMatrix[,-zeroCols]
  
  plot(featureMatrix[settings$spectraToPlot,], type = "l", main = spectraData$metadata[settings$spectraToPlot])
  
  featureMatrix <- as.data.frame(featureMatrix)
  
  return(list(featureMatrix = featureMatrix, settings = settings))
}


# *************************************************************************
#                 MASS RESOLUTION IN MATRIX and ROW NAMES
# Create a loop to ask to the user for mass resolution according with their
# equipment
# *************************************************************************

featureMatrix.adjust.resolution <-  function(featureMatrix, settings) {
  
  print("Number of significant digits")
  
  settingsBackUp <- settings
  newResolution  <- NULL
  newPlot        <- TRUE
  
  while (newPlot) {
    
    resolution <- console.read("Insert new mass accuracy (number of significant digits): type c to apply and continue, or s to skip")
    
    if (input.is.numeric(resolution)$succes){
      
      newResolution <- resolution
      masses        <- round(as.numeric(colnames(featureMatrix)),as.numeric(resolution))
      
      print("Masses will be roud to:")
      print(masses)
      
    } else {
      
      newPlot <- FALSE
    
    }
  }
    
  if (resolution == "c" | resolution == "C") {
      
    if (is.null(newResolution) == FALSE) {
      
      settings$resolution     <- newResolution
      settings$massRound      <- TRUE
      colnames(featureMatrix) <- masses
      
    } else {
      
      masses                  <- round(as.numeric(colnames(featureMatrix)),as.numeric(settings$resolution))
      colnames(featureMatrix) <- masses
      
    }
    
  }
  
  if (resolution == "s" | resolution == "S") {
    settings$massRound <- FALSE
  }
  
  return(list(featureMatrix = featureMatrix, settings = settings))

}

# *************************************************************************
#                        MATRIX FORMAT
# Add row names and columns with metadata
# 
# *************************************************************************



 format.matrix <- function(featureMatrix, settings) {
  
   # Rownames
   
   rowNames                <- list.files()
   rowNames                <- gsub(".mzML", "", rowNames)
   rownames(featureMatrix) <- rowNames
   actualWD                <- getwd()
   
   # Add metadata to de matrix
   
   setwd(file.path(path.expand('~'),'Desktop'))
   
   if(any(list.files()=="Metadata.csv")){
     metadataMatrix <- matrix(ncol = (lengths(regmatches(rowNames[1], gregexpr("_", rowNames[1])))+1), nrow = nrow(featureMatrix))
     
     for (i in 1:nrow(metadataMatrix)) {
       
       tempMetaData <- strsplit(rowNames[i], "_")
       
       for (j in 1:ncol(metadataMatrix)) {
         
         metadataMatrix[i,j] <- tempMetaData[[1]][j]
         
       }
       
     }
     
     # Give colnames from CSV file
     
     metadataColNames         <- read.csv("Metadata.csv", header = FALSE)
     colnames(metadataMatrix) <- metadataColNames
     featureMatrix            <- cbind(metadataMatrix, featureMatrix)
   }
   
   setwd(actualWD)
   
   return(list(featureMatrix = featureMatrix, settings = settings))
   
 }
 
 
 
 


# *************************************************************************
#               Export feature matrix to CSV
# If necessary Create a new folder in the desktop and
# Automatically export feature matrix to a CSV file
# *************************************************************************

export.matrix <- function () {
  
  actualWD <- getwd()
  
  setwd(file.path(path.expand('~'),'Desktop'))
  
  if (any(list.files () == "Exported")) {
    setwd(file.path(path.expand('~'),'Desktop/Exported'))
    write.csv(pretreatment$featureMatrix, file = "FeatureMatrix.csv")
  } else {
    dir.create("Exported")
    setwd(file.path(path.expand('~'),'Desktop/Exported'))
    write.csv(pretreatment$featureMatrix, file = "FeatureMatrix.csv")
  }
  
  setwd(actualWD)
  print("Feature matrix has been exported")
  
}

# *************************************************************************
#                       Export settings to CSV
# If necessary Create a new folder in the desktop and
# Automatically export feature matrix to a CSV file
# *************************************************************************

export.settings <- function () {
  
  actualWD <- getwd()
  settings <- as.matrix(pretreatment$settings)
  colnames(settings) <- "Value"
  
  setwd(file.path(path.expand('~'),'Desktop'))
  
  if (any(list.files () == "Exported")) {
    setwd(file.path(path.expand('~'),'Desktop/Exported'))
    write.csv(settings, file = "Settings.csv")
  } else {
    dir.create("Exported")
    setwd(file.path(path.expand('~'),'Desktop/Exported'))
    write.csv(settings, file = "Settings.csv")
  }
  
  setwd(actualWD)
  print("Settings has been exported")
  
}

# *************************************************************************
#               Export feature matrix and settings to CSV
# If necessary Create a new folder in the desktop and
# Automatically export feature matrix to a CSV file
# *************************************************************************
export.all <- function() {
  export.matrix()
  export.settings()
}

