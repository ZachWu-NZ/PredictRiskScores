# PREDICT CVD Risk Scores

This is an R package featuring functions for generating PREDICT CVD risk scores.

## Features
-	Functions can be used as a calculator by inputting individual values to calculate the risk score for an single person.
- If a dataset or table of inputs is provided, then functions can return a vector of risk estimates.
-	Package offer functions for primary prevention (people without prior CVD) and secondary prevention (people with prior CVD).

## Functions
- `NoPriorCVDRisk` Creates a 5 year CVD risk estimate for people without prior CVD using the published Lancet equation
- `NoPriorCVDRisk_BMI` Creates a 5 year CVD risk estimate for people without prior CVD using the Ministry of Health's HISO equation containing BMI 
- `PriorT2DRisk` Creates a 5 year CVD risk estimate for people with prior Type-II diabetes using the Ministry of Health's HISO equation
- `MajorBleedRisk` Creates a 5 year major bleeding risk estimate for people without prior CVD using the published AnnIntMed equation
- `PriorCVDRisk` Creates a 5 year CVD risk estimate for people with prior CVD using the published Heart equation

## Installation
The R package "devtools" is required to enable things from GitHub to be installed directly into your R environment.

If devtools is not yet installed, then:
```
install.packages("devtools")
```

When devtools is installed:
```
devtools::install_github("billy-nz/PredictRiskScores")
```

To load the library:
```
library(PredictRiskScores)
```

## Maintainer / Developer 
- Billy Wu / billy.wu@auckland.ac.nz
