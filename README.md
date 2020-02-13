## PREDICT CVD Risk Scores

This is an R package featuring functions for generating PREDICT cardiovascular disease (CVD) risk scores.

### Features
-	Estimate 5 year CVD risk in people with and without established CVD
-   Estimate 5 year CVD risk in people who have experienced an acute coronary syndrome (ACS)
-   Estimate 5 year CVD risk in people without established Type-II diabetes
-   Estimate 5 year major bleeding risk in people without established CVD

### Functions
1. `NoPriorCVDRisk` 
    calculates the 5 year absolute risk of CVD for people without a history of atherosclerotic CVD. Published in the Lancet (Pylypchuk et al. 2018) and does not include BMI as a predictor. 
    [Full Article](https://www.thelancet.com/journals/lancet/article/PIIS0140-6736(18)30664-0/fulltext)
    
2. `NoPriorCVDRisk_BMI` 
    calculates the 5 year absolute risk of CVD for people without a history of atherosclerotic CVD. Published as a Ministry of Health's HISO standard and adds BMI as a predictor to the Lancet equation.
    [HISO Standard](https://www.health.govt.nz/publication/hiso-100712019-cardiovascular-disease-risk-assessment-data-standard)
    
3. `NoPriorCVDRisk_Policy` 
    calculates the 5 year absolute risk of CVD for people without a history of atherosclerotic CVD that is intended for use at policy or general population level. Publushed in the 
    IJE (Mehta et al. 2018) and uses routinely-collected health information as predictors.
    [Full Article](https://academic.oup.com/ije/article/47/5/1571/5053287)
    
4. `NoPriorCVDRiskBleedRisk` 
    calculates the 5 year absolute risk of major bleeding for people without a history of atherosclerotic CVD. Published in AnnIntMed (Selak et al. 2019).
    [Full Article](https://www.annals.org/aim/fullarticle/doi/10.7326/M18-2808)
    
5. `NoPriorT2DRisk` 
    creates a 5 year absolute risk of CVD for people without a history of type-II diabetes. Published as a Ministry of Health's HISO standard and includes BMI as a predictor.
    [HISO Standard](https://www.health.govt.nz/publication/hiso-100712019-cardiovascular-disease-risk-assessment-data-standard)
    
6. `PostCVDRisk` 
    creates a 5 year absolute risk of CVD for people with prior CVD. This equation is yet to be published. However, it is an update to the previously published equation in Heart (Poppe et al. 2017).
    [Full Article](https://heart.bmj.com/content/103/12/891.1)
    
7. `PostACSRisk` 
    creates a 5 year absolute risk for people for people who have experienced an ACS event. Published in heart (Poppe et al. 2019).
    [Full Article](https://heart.bmj.com/content/early/2019/12/10/heartjnl-2019-315809.full)

### Installation
The R package "devtools" is required to enable things from GitHub to be installed directly into your R environment.

If devtools is not yet installed, then:
```r
install.packages("devtools")
```

To install package, when devtools is available:
```r
devtools::install_github("billy-nz/PredictRiskScores")
```

To load the package:
```r
library(PredictRiskScores)
```
### Usage
#
#### One-off Calculator
Each function can be used as either a calculator or vectoriser. When used as a one-off calculator, a dataset is not required. This can be handy when checking the risk estimate for 
an individual person or for quickly seeing the effect of changing a single parameter value. 

```r
PostACSRisk(sex="F", age=65, eth=Indian, nzdep=5, smoker=0, diabetes=0,
            af=0, hf=1, acsdays=65, acstype="NSTEMI", bmi=NA, sbp=118,
            tchdl=3.3, hba1c=NA, scr=52, bpl=1, lld=1, athrombi=1)
```
```
[1] 0.4877
```
All parameter values can be numeric or encoded. Values for some parameters such as BMI and HbA1C can be `NA` if the value is unknown. See *values* in R documentation.
```r
PostACSRisk(sex=0, age=65, eth=43, nzdep=5, smoker=0, diabetes=0,
            af=0, hf=1, acsdays=65, acstype=1, bmi=NA, sbp=118,
            tchdl=3.3, hba1c=NA, scr=52, bpl=1, lld=1, athrombi=1)
```
```
[1] 0.4877
```
#
#### Vectorise Dataset
When a dataset is supplied, a risk score is produced for each row of data. Each argument requires the variable name from the dataset.
This can be handy when risk estimates are required for each row of data, or when data requires vectorisation.

```r
NoPriorCVDRisk(dat=DATA, sex=sex, age=age, eth=ethnicity, smoker=smoking_status, nzdep=nzdep, 
                diabetes=diab_status, af=hx_af, familyhx=familyhx, lld=lld, athrombi=athrombotics, 
                bpl=bpl, sbp=sbp, tchdl=tchdl)
```
```
[1] 0.3327 0.0489 0.0620 0.5441 0.0688 0.1672 0.5054 0.0442 0.0150 0.0387
```
When a dataset is supplied, the function returns a numeric vector of risk scores. Each element of the vector is positioned as per row index. As such, the resulting numeric vector can be 
assigned back to the dataset as a new variable.
```r
DATA$riskscores <- NoPriorCVDRisk(dat=DATA, sex=sex, age=age, eth=ethnicity, smoker=smoking_status, 
                                    nzdep=nzdep, diabetes=diab_status, af=hx_af, familyhx=familyhx, 
                                    lld=lld, athrombi=athrombotics, bpl=bpl, sbp=sbp, tchdl=tchdl)
```
The suite of functions in this package can be integrated into both `data.table` and `dplyr`. For example, if datasets are extremely large, then consider 
using `data.table` along with `:=` notation. In the example below, a new column called `riskscore` is created.
The syntax might seem confusing at first but performance very fast and is suited to large datasets.

```r
library(data.table); setDT(DATA)

DATA[, riskscore := NoPriorCVDRisk(.SD, sex=sex, age=age, eth=ethnicity, smoker=smoking_status, 
                                    nzdep=nzdep, diabetes=diab_status, af=hx_af, familyhx=familyhx, 
                                    lld=lld, athrombi=athrombotics, bpl=bpl, sbp=sbp, tchdl=tchdl)]
```
Users who are more comfortable with `dplyr` will find that each function works with `mutate`. In the example below, a new column called `riskscore` is created.
```r
library(dplyr)

DATA %>%
  mutate(riskscore = NoPriorCVDRisk(DATA, sex=sex, age=age, eth=ethnicity, smoker=smoking_status, 
                                    nzdep=nzdep, diabetes=diab_status, af=hx_af, familyhx=familyhx, 
                                    lld=lld, athrombi=athrombotics, bpl=bpl, sbp=sbp, tchdl=tchdl))
```
#
#### R Documentation
```r
?NoPriorCVDRisk
?NoPriorCVDRisk_BMI
?NoPriorCVDRisk_Policy
?NoPriorCVDRiskBleedRisk
?NoPriorT2DRisk
?PostCVDRisk
?PostACSRisk
```

### Maintainer / Developer 
- Billy Wu / billy.wu@auckland.ac.nz

### See Also
I built a web-based risk calculator using the bleeding risk equation developed by Selak et al (2019) and the general CVD risk equation developed by Pylypchuk et al (2018), to provides clinicians with an individualised estimate of the CVD benefit and bleeding harms of aspirin for their patients without established CVD. [Full Article](https://annals.org/aim/fullarticle/2751452/personalized-prediction-cardiovascular-benefits-bleeding-harms-from-aspirin-primary-prevention).

<a href="https://aspirinbenefitharmcalculator.shinyapps.io/calculator/" target="_blank">Link to the risk calculator here</a>

### Coming Soon
-	Estimate 5 year CVD risk in people with severe mental illness
-   Estimate 5 year diabetes risk in people without established diabetes
-   Estimate 5 year acute CVD risk in people with established ACS
-   Estimate 5 year acute bleeding risk in people with established ACS

