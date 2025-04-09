# Description
The TSB threshold (NICE) is based on the type of treatment (Phototherapy/Exchange Transfusion), gastational age when baby was born, and the actual age of baby. Users may input the treatment types, gastational age, actual age(days), actual age(hours) to get TSB threshold (mg/dL)

# Variables
```r
library(TSB.NICE)
#threshold:
###"P0":Phototherapy thresholds with no hyperbilirubinemia neurotoxicity risk factor (NICE guideline)
###"E0":Exchange transfusion thresholds with no recognized hyperbilirubinemia neurotoxicity risk factors other than gestational age (NICE guideline)

#GA:
###"23 weeks"
###"24 weeks"
###"25 weeks"
...
###"36 weeks"
###"37 weeks"
###">= 38 weeks"

#days:
###From 0 to 14

#hours:
###From 0 to 23
```
# Example
```r
TSB_NICE(threshold = "P0", GA = "37 weeks", days = 2, hours = 15)
[1] 14.1
```
