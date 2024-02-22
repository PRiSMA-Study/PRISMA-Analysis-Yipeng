# Description
The TSB threshold is based on the type of threshold(Phototherapy/Exchange Transfusion), risk factors, gastational age when baby was born, and actual age of baby. Users may input the threshold types, gastational age, actual age(days), actual age(hours) to get TSB threshold (mg/dL)

# Variables
```r
library(TCB)
#threshold:
###"P0":Phototherapy thresholds with no hyperbilirubinemia neurotoxicity risk factor
###"P1":Phototherapy thresholds with a recognized hyperbilirubinemia neurotoxicity risk factor
###"E0":Exchange transfusion thresholds with no recognized hyperbilirubinemia neurotoxicity risk factors other than gestational age
###"E1":Exchange transfusion thresholds with any recognized hyperbilirubinemia neurotoxicity risk factors other than gestational age

#GA:
###"35 weeks"
###"36 weeks"
###"37 weeks"
###">= 38 weeks"
###"38 weeks"(Only applies to Phototherapy thresholds with no hyperbilirubinemia neurotoxicity risk factor, i.e. threshold="P0")
###"39 weeks"(Only applies to Phototherapy thresholds with no hyperbilirubinemia neurotoxicity risk factor, i.e. threshold="P0")
###">= 40 weeks"(Only applies to Phototherapy thresholds with no hyperbilirubinemia neurotoxicity risk factor, i.e. threshold="P0")

#days:
###From 0 to 14

#hours:
###From 0 to 23
```
# Example
```r
TSB(threshold = "P0", GA = "39 weeks", days = 2, hours = 15)
[1] 18.5
```
