library(TCB)

###threshold:
###"P0":Phototherapy thresholds with no hyperbilirubinemia neurotoxicity risk factor
###"P1":Phototherapy thresholds with a recognized hyperbilirubinemia neurotoxicity risk factor
###"E0":Exchange transfusion thresholds with no recognized hyperbilirubinemia neurotoxicity risk factors other than gestational age
###"E1":Exchange transfusion thresholds with any recognized hyperbilirubinemia neurotoxicity risk factors other than gestational age

###GA:
###From "35 weeks" to ">= 40 weeks"

###days:
###From 0 to 14

###hours:
###From 0 to 23



###Example:
TSB(threshold="P0",GA=">= 40 weeks",days=2,hours=6)
TSB(threshold = "P0", GA = "39 weeks", days = 2, hours = 15)




