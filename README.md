# PREDICT CVD Risk Scores

This is an R package featuring functions for generating PREDICT CVD risk scores.

## Features
-	Functions can be used as a calculator by inputting individual values to calculate the risk score for an single person
- If a dataset or table of inputs is provided, then functions can return a vector of risk estimates
-	Package offer functions for primary prevention (people without prior CVD) and secondary prevention (people with prior CVD)
- All parameters can be encoded as numeric input (e.g. Male = 1, Indian = 3, etc.)

## Functions
1. `NoPriorCVDRisk` 
    + Creates a 5 year CVD risk estimate for people without prior CVD using the published Lancet equation
2. `NoPriorCVDRisk_BMI` 
    + Creates a 5 year CVD risk estimate for people without prior CVD using the Ministry of Health's HISO equation containing BMI 
3. `PriorT2DRisk` 
    + Creates a 5 year CVD risk estimate for people with prior Type-II diabetes using the Ministry of Health's HISO equation
4. `MajorBleedRisk` 
    + Creates a 5 year major bleeding risk estimate for people without prior CVD using the published AnnIntMed equation
5. `PriorCVDRisk` 
    + Creates a 5 year CVD risk estimate for people with prior CVD using the published Heart equation
6. `PolicyCVDRisk` 
    + Creates a 5 year CVD policy risk estimate for people in the general population using the publish IJE equation

## Installation
The R package "devtools" is required to enable things from GitHub to be installed directly into your R environment.

If devtools is not yet installed, then:
```
install.packages("devtools")
```

To install package, when devtools is available:
```
devtools::install_github("billy-nz/PredictRiskScores")
```

To load the package:
```
library(PredictRiskScores)
```

Usage with dataset:
```
FUN(dat = DF, sex = sex_var, age = age_var, argument = variable_name, ...)
```

Usage as a calculator:
```
FUN(sex = "F", age = 60, argument = value, ...)
```

Help:
```
?FUN
```

## Maintainer / Developer 
- Billy Wu / billy.wu@auckland.ac.nz
