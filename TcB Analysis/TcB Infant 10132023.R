#*****************************************************************************
#*derived variables for statistical analysis plan
#*
#*input: matData_Wide.RData, 
#*       matData_Anc_Visits.RData, 
#*       infData_Wide. Rdata
#*       
#*output: df_maternal.rda (keep PRiSMA enrolled only)
#*        df_infant.rda
#*        df_baseline.rda
#*        df_hb_long.rda
#*        df_hb_wide.rda
#*        df_sensitvie.rda
#*        
#*****************************************************************************
rm(list = ls())

library(tidyverse)
library(lubridate)
library(naniar)
library(TCB)
library(xtable)
#*****************************************************************************
#*set directory and prepare data 
#*****************************************************************************
uploadDate = "2023-10-13"
#******read data 
load(paste0("Z:/Processed Data/", uploadDate, "/MatData_Wide_", uploadDate, ".RData"))
load(paste0("Z:/Processed Data/", uploadDate, "/InfData_Wide_", uploadDate, ".RData"))

#******prepare maternal and infant data 
#df_maternal
df_maternal <- MatData_Wide %>% 
  distinct() %>% 
  #keep PRiSMA enrolled only
  filter(M02_CONSENT_IEORRES == 1)

df_infant <- InfData_Wide %>% 
  #add all M09 birthoutcome variables
  left_join(df_maternal %>% dplyr::select("MOMID", "PREGID", "SITE", matches("_INF1_")), 
            by = c("MOMID", "PREGID", "INFANTID"="M09_INFANTID_INF1_6", "SITE")) %>% 
  left_join(df_maternal %>% dplyr::select("MOMID", "PREGID", "SITE", matches("_INF2_")), 
            by = c("MOMID", "PREGID", "INFANTID"="M09_INFANTID_INF2_6", "SITE")) %>% 
  left_join(df_maternal %>% dplyr::select("MOMID", "PREGID", "SITE", matches("_INF3_")), 
            by = c("MOMID", "PREGID", "INFANTID"="M09_INFANTID_INF3_6", "SITE")) %>% 
  left_join(df_maternal %>% dplyr::select("MOMID", "PREGID", "SITE", matches("_INF4_")), 
            by = c("MOMID", "PREGID", "INFANTID"="M09_INFANTID_INF4_6", "SITE")) %>% 
  #add M01 ultrasound GA, date (using BOE_GA_DAYS_ENROLL should be better than using individual ga days?)
  left_join(df_maternal %>% dplyr::select("MOMID", "PREGID", "SITE", "M01_US_OHOSTDAT_1", "BOE_GA_DAYS_ENROLL"), 
            by = c("MOMID", "PREGID", "SITE")) 

df_infant_tcb<-df_infant %>% dplyr::select("MOMID", "PREGID", "SITE","INFANTID",
                                           matches("M09_DELIV_DSSTDAT_INF"),matches("M09_DELIV_DSSTTIM_INF"),"M01_US_OHOSTDAT_1","BOE_GA_DAYS_ENROLL",
                                           matches("M11_VISIT_OBSSTDAT"),matches("M11_BILIRUBIN_LBPERF"),matches("M11_TBILIRUBIN_UMOLL_LBORRES"),matches("M11_TBILIRUBIN_OBSSTTIM"),matches("M11_JAUND_CESTDAT"),
                                           matches("JAUND_CEOCCUR"),matches("YELLOW_CEOCCUR"),matches("YELL_CEOCCUR"),matches("JAUND_CESTTIM"),
                                           matches("M14_TCB_VSSTAT"),matches("M14_TCB_OBSSTTIM"),matches("M14_TCB_UMOLL_LBORRES"),matches("M14_VISIT_OBSSTDAT"))

#Transform TcB measurement units for Zambia site
df_infant_tcb[df_infant_tcb$SITE=="Zambia",]$M14_TCB_UMOLL_LBORRES_7<-df_infant_tcb[df_infant_tcb$SITE=="Zambia",]$M14_TCB_UMOLL_LBORRES_7/17.1
df_infant_tcb[df_infant_tcb$SITE=="Zambia",]$M14_TCB_UMOLL_LBORRES_8<-df_infant_tcb[df_infant_tcb$SITE=="Zambia",]$M14_TCB_UMOLL_LBORRES_8/17.1
df_infant_tcb[df_infant_tcb$SITE=="Zambia",]$M14_TCB_UMOLL_LBORRES_9<-df_infant_tcb[df_infant_tcb$SITE=="Zambia",]$M14_TCB_UMOLL_LBORRES_9/17.1
df_infant_tcb[df_infant_tcb$SITE=="Zambia",]$M14_TCB_UMOLL_LBORRES_10<-df_infant_tcb[df_infant_tcb$SITE=="Zambia",]$M14_TCB_UMOLL_LBORRES_10/17.1
df_infant_tcb[df_infant_tcb$SITE=="Zambia",]$M14_TCB_UMOLL_LBORRES_11<-df_infant_tcb[df_infant_tcb$SITE=="Zambia",]$M14_TCB_UMOLL_LBORRES_11/17.1
df_infant_tcb[df_infant_tcb$SITE=="Zambia",]$M14_TCB_UMOLL_LBORRES_12<-df_infant_tcb[df_infant_tcb$SITE=="Zambia",]$M14_TCB_UMOLL_LBORRES_12/17.1
df_infant_tcb[df_infant_tcb$SITE=="Zambia",]$M11_TBILIRUBIN_UMOLL_LBORRES_6<-df_infant_tcb[df_infant_tcb$SITE=="Zambia",]$M11_TBILIRUBIN_UMOLL_LBORRES_6/17.1

rm(df_maternal)
rm(MatData_Wide)