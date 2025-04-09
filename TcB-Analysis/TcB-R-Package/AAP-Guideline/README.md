# Description
The TSB threshold (AAP) is based on the type of treatment (Phototherapy/Exchange Transfusion), gastational age when baby was born, and the actual age of baby. Users may input the treatment types, gastational age, actual age(days), actual age(hours) to get TSB threshold (mg/dL)

# Variables
```r
library(TSB.AAP)
#threshold:
###"P0":Phototherapy thresholds with no hyperbilirubinemia neurotoxicity risk factor (AAP guideline)
###"P1":Phototherapy thresholds with a recognized hyperbilirubinemia neurotoxicity risk factor (AAP guideline) 
###"E0":Exchange transfusion thresholds with no recognized hyperbilirubinemia neurotoxicity risk factors other than gestational age (AAP guideline)
###"E1":Exchange transfusion thresholds with any recognized hyperbilirubinemia neurotoxicity risk factors other than gestational age (AAP guideline)

#GA (threshold = "P0"):
###"35 weeks"
###"36 weeks"
###"37 weeks"
###"38 weeks"
###"39 weeks"
###">= 40 weeks"

#GA (threshold = "P1","E0","E1"):
###"35 weeks"
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
TSB_AAP(threshold = "P0", GA = "37 weeks", days = 2, hours = 15)
[1] 17.2
```
