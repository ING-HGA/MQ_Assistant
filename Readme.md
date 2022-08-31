# MALDIquantReport

MALDIquantReport versión alfa: 1
Laboratorio de Análisis Bioquímico e Instrumental
CINVESTAV-Irapuato, UGA-LANGEBIO
Héctor Guillén-Alonso & Robert Winkler

MALDIquantReport will be a r-package for automated workflow data analysis and report based on MALDIquant r-package

## Thank you!
First of all, thank you for your help in the develptment of this easy-to-use and automatic data pre-treatment software. I really appreciate your help to improve this software. 
As an alfa version of the software, bugs could be very "pese off" producing frustration, I ask you for patience. Please ensure to try all you think in order to destroy the program, to get a better final version

## Steeps to test MALDIquantReport

1. Open "Auto-MALDIquantReport.R" file and set required directories to MALDIquantReport-Functions.R, Auto-MALDIquantReport.R and dataset. **Just chanche text inside ""**
2. Execute **featureMatrix <- autoDLIESILCQ()** line to star MALDIquantReport
3. Code will run on console and results will be a preview on plot seccion in R studio or in a different window in R

**NOTES:**

* Matrix with masses and intensities will be saved on featureMatrix r object with other internal parameters, to visualize the feature Matrix, write the name of the variable (featureMatrix by default) followed by "$featureMatrix" (default complete command: newVariable <- featureMatrix$featureMatrix

 **How to report problems:** 
 * pay attention to what you are typing and if the error appears, please reproduce it, if you can do it crash again, and write all the steps required to produce the problem. If possible make a screenshot
**How to make suggestions:**
* Make a screenshot of where your suggestion will be applied (if apply) and write the suggestion

**MALDIquant workflow:** https://cran.r-project.org/web/packages/MALDIquant/vignettes/MALDIquant-intro.pdf