# PREDICT CVD Risk Scores

This is an R package featuring functions for generating PREDICT CVD risk scores.

## Features
-	Functions can be used as a calculator by inputting individual values to calculate the risk score for an single person.
- Functions can produce a vector of multiple risk scores if a dataset or table of inputs is provided.
-	Package offer functions for primary prevention (people without prior CVD) and secondary prevention (people with prior CVD).

## Functions

#### Cleaning
- `PriorCVDRisk` Creates a 5 year CVD risk estimate for people with prior CVD
- `NoPriorCVDRisk` Creates a 5 year CVD risk estimate for people without prior CVD using published Lancet equation
- `NoPriorCVDRisk_BMI` Creates a 5 year CVD risk estimate for people without prior CVD using Ministry of Health BMI equation

## Installation
The R package "devtools" is required to enable things from GitHub to be installed directly into your R environment.
```
# If devtools is not yet installed, then:
install.packages("devtools")

# When devtools is installed, then:
devtools::install_github("billy-nz/PredictRiskScores")

# Load library:
library(PredictRiskScores)
```

## Author
- Billy Wu // billy.wu@auckland.ac.nz
