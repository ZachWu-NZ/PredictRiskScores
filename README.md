# PREDICT CVD Risk Scores

This is an R package featuring functions for generating PREDICT CVD risk scores.

## Features
-	Package offer 5 year CVD risk estimates for primary prevention in people without established CVD or people without established type 2 diabetes; and a 5 year major bleed risk estimate in people without established CVD.
-   Package offers 5 year CVD risk estimates for secondary prevention; used for people with prior established atherosclerotic CVD or hospitalisation of acute coronary sydnrome. 
-	Each functions can be used as either a calculator (by inputting individual values for an single person) or as a vectoriser (by inputting a dataset containing required parameters).
-   By default, risk estimates are provided to 4 decimal placee; with the option to specify _n_ decimals. 
-   All parameters can be encoded as numeric input (e.g. Male = 1, Pacific = 3, smoker = 1, ...)

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
## Example 

Usage with dataset:
```
NoPriorCVDRisk(dat=DF, sex=sex, age=age, eth=ethnicity, smoker=smoking_status, nzdep=nzdep_quint, 
               diabetes=diab_status, af=hx_af, familyhx=fam_hx, lld=lld_drugs, athromb=antithrombics, 
               bpl=bplt, sbp=systolic_bp, tchdl=tchdl_ratio)
```

Usage as a calculator:
```
PostACSRisk(sex="F", age=65, eth="Indian", nzdep=5, smoker=0, diabetes=0,
            af=0, hf=1, acsdays=65, acstype="NSTEMI", bmi=NA, sbp=118,
            tchdl=3.3, hba1c=NA, scr=52, bpl=1, lld=1, athromb=1)
```

Help:
```
?NoPriorCVDRisk
?NoPriorCVDRisk_BMI
?PriorT2DRisk
?MajorBleedRisk
?PriorCVDRisk
?PolicyCVDRisk
?PostACSRisk
```

## Maintainer / Developer 
- Billy Wu / billy.wu@auckland.ac.nz

## See Also
I built a web-based risk calculator using the bleeding risk equation developed by Selak et al (2019) and the general CVD risk equation developed by Pylypchuk et al (2018), that provides clinicians with an individualised estimate of the CVD benefit and bleeding harms of aspirin for their patients without established CVD. See [Full Article](https://annals.org/aim/fullarticle/2751452/personalized-prediction-cardiovascular-benefits-bleeding-harms-from-aspirin-primary-prevention).

<a href="https://aspirinbenefitharmcalculator.shinyapps.io/calculator/" target="_blank">Link to the risk calculator here</a>

## Coming Soon
-	5 year CVD risk estimates for primary prevention in people with serious mental illness
-   5 year diabetes risk estimates for primary prevention in people without established diabetes
-   5 year acute CVD risk estimates for secondary prevention in people with established ACS
-   5 year acute bleeding risk estimates for secondary prevention in people with established ACS
