# MALDIquantReport

MALDIquantReport versión alfa: 1

Laboratorio de Análisis Bioquímico e Instrumental

CINVESTAV-Irapuato, UGA-LANGEBIO

Héctor Guillén-Alonso, Francisco Villaseñor Ortega & Robert Winkler

MALDIquantReport will be an r-package for visual analysis and automatic report of mass spectra files in mzML format, based on MALDIquant workflow.

## Thank you!
First of all. Thank you for your help! I really appreciate your time to improve this software. 
As an alfa version, bugs could be very "pese off", be a little patient. Please, ensure to try all you think, in order to test possible ways users could use the software and prevent possible bugs.

## How to start MALDIquantReport

1. Open "Auto-MALDIquantReport.R" file and set required directories to MALDIquantReport-Functions.R, Auto-MALDIquantReport.R and dataset. **Just chanche text inside ""**
2. Execute **featureMatrix <- autoDLIESILCQ()** line to star MALDIquantReport
3. Code will run on console, a preview of each step will be plotted in the plot section in R studio, or in an emergent window in R

**NOTES:**

* Matrix with masses and intensities will be saved on featureMatrix r-object, with other parameters that are used for internal functions. In a feature, other parameters will not be accessible to users. To visualize the feature Matrix, write the name of the variable (featureMatrix by default) followed by "$featureMatrix”. If you want to save it as an r-object use the following command: “yourNewVariable <- featureMatrix$featureMatrix”

 **How to report problems:** 
 * Pay attention to what you type, if the error appears, make it happen again, following the previous step and registering all the steps you do to obtain it. If possible make a screenshot.
**How to make suggestions:**
* Make a screenshot of where your suggestion is (if applicable) and write the suggestion.

**MALDIquant workflow:** https://cran.r-project.org/web/packages/MALDIquant/vignettes/MALDIquant-intro.pdf
