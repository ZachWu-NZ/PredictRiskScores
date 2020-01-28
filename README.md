# PREDICT CVD Risk Scores

This is an R package featuring functions for generating PREDICT CVD risk scores.

## Features
-	Functions can be used as a calculator by inputting individual values to calculate the risk score for an single person
- If a dataset or table of inputs is provided, then functions can return a vector of risk estimates
-	Package offer functions for primary prevention (people without prior CVD) and secondary prevention (people with prior CVD)
- All parameters can be encoded as numeric input (e.g. Male = 1, Pacific = 3, etc.)

## Functions
1. `NoPriorCVDRisk` 
    + Creates a 5 year CVD risk estimate for people without prior CVD using the published Lancet equation
    ([Full Article](https://www.thelancet.com/journals/lancet/article/PIIS0140-6736(18)30664-0/fulltext))
2. `NoPriorCVDRisk_BMI` 
    + Creates a 5 year CVD risk estimate for people without prior CVD using the Ministry of Health's HISO equation containing BMI 
    ([HISO Standard](https://www.health.govt.nz/publication/hiso-100712019-cardiovascular-disease-risk-assessment-data-standard))
3. `PriorT2DRisk` 
    + Creates a 5 year CVD risk estimate for people with prior Type-II diabetes using the Ministry of Health's HISO equation
    ([HISO Standard](https://www.health.govt.nz/publication/hiso-100712019-cardiovascular-disease-risk-assessment-data-standard))
4. `MajorBleedRisk` 
    + Creates a 5 year major bleeding risk estimate for people without prior CVD using the published AnnIntMed equation
    ([Full Article](https://www.annals.org/aim/fullarticle/doi/10.7326/M18-2808))
5. `PriorCVDRisk` 
    + Creates a 5 year CVD risk estimate for people with prior CVD using the updated Heart equation (unpublished)
6. `PolicyCVDRisk` 
    + Creates a 5 year CVD policy risk estimate for people in the general population using the published IJE equation
    ([Full Article](https://academic.oup.com/ije/article/47/5/1571/5053287))
7. `PostACSRisk` 
    + Creates a 5 year CVD risk estimate for people after an ACS event using the published Heart equation
    ([Full Article](https://heart.bmj.com/content/early/2019/12/10/heartjnl-2019-315809.full))

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

## See Also
I built a web-based risk calculator using the bleeding risk equation developed by Selak et al (2019) and the general CVD risk equation developed by Pylypchuk et al (2018), that provides clinicians with an individualised estimate of the CVD benefit and bleeding harms of aspirin for their patients without established CVD. See [Full Article](https://annals.org/aim/fullarticle/2751452/personalized-prediction-cardiovascular-benefits-bleeding-harms-from-aspirin-primary-prevention).

<a href="https://aspirinbenefitharmcalculator.shinyapps.io/calculator/" target="_blank">Link to the risk calculator here</a>
