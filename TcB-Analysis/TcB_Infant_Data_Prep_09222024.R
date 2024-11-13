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
uploadDate = "2024-06-28"
#******read data 
load(paste0("Z:/Processed Data/", uploadDate, "/MatData_Wide_", uploadDate, ".RData"))
load(paste0("Z:/Processed Data/", uploadDate, "/InfData_Wide_", uploadDate, ".RData"))
inf_outcome<-read.csv(paste0('D:/Users/yipeng_wei/Documents/ReMAPP aim 3 data/',uploadDate,"/INF_OUTCOMES.csv"))
inf_outcome<-inf_outcome %>% filter(LIVEBIRTH==1)

###Import data
path_to_data <- paste0('D:/Users/yipeng_wei/Documents/Stacked data/',uploadDate)# - for AWS data
setwd(path_to_data)

mnh09<-read.csv('mnh09_merged.csv')

#******prepare maternal and infant data 
#df_maternal
df_maternal <- MatData_Wide %>% 
  distinct() %>% 
  #keep PRiSMA enrolled only
  filter(M02_CONSENT_IEORRES == 1)

df_infant <- InfData_Wide %>%
  #add all M09 birthoutcome variables
  left_join(mnh09 %>% dplyr::select("MOMID", "PREGID", "SITE", matches("_INF1")), 
            by = c("MOMID", "PREGID", "INFANTID"="M09_INFANTID_INF1", "SITE")) %>% 
  left_join(mnh09 %>% dplyr::select("MOMID", "PREGID", "SITE", matches("_INF2")), 
            by = c("MOMID", "PREGID", "INFANTID"="M09_INFANTID_INF2", "SITE")) %>% 
  left_join(mnh09 %>% dplyr::select("MOMID", "PREGID", "SITE", matches("_INF3")), 
            by = c("MOMID", "PREGID", "INFANTID"="M09_INFANTID_INF3", "SITE")) %>% 
  left_join(mnh09 %>% dplyr::select("MOMID", "PREGID", "SITE", matches("_INF4")), 
            by = c("MOMID", "PREGID", "INFANTID"="M09_INFANTID_INF4", "SITE")) %>% 
  left_join(inf_outcome %>% dplyr::select("MOMID", "PREGID", "SITE","INFANTID","LIVEBIRTH"), 
            by = c("MOMID", "PREGID","INFANTID", "SITE")) %>%
  #add M01 ultrasound GA, date (using BOE_GA_DAYS_ENROLL should be better than using individual ga days?)
  left_join(df_maternal %>% dplyr::select("MOMID", "PREGID", "SITE", "M01_US_OHOSTDAT_1", "BOE_GA_DAYS_ENROLL"), 
            by = c("MOMID", "PREGID", "SITE")) %>% filter(LIVEBIRTH==1)  

df_infant_tcb<-df_infant %>% dplyr::select("MOMID", "PREGID", "SITE","INFANTID",
                                           matches("M09_DELIV_DSSTDAT_INF"),matches("M09_DELIV_DSSTTIM_INF"),"M01_US_OHOSTDAT_1","BOE_GA_DAYS_ENROLL",
                                           matches("M11_VISIT_OBSSTDAT"),matches("M11_BILIRUBIN_LBPERF"),matches("M11_TBILIRUBIN_UMOLL_LBORRES"),matches("M11_TBILIRUBIN_OBSSTTIM"),matches("M11_JAUND_CESTDAT"),
                                           matches("JAUND_CEOCCUR"),matches("YELLOW_CEOCCUR"),matches("YELL_CEOCCUR"),matches("JAUND_CESTTIM"),
                                           matches("M14_TCB_VSSTAT"),matches("M14_TCB_OBSSTTIM"),matches("M14_TCB_UMOLL_LBORRES"),matches("M14_VISIT_OBSSTDAT"))

###Check for unit of TcB measurements
#df_infant_tcb_Kenya<-df_infant_tcb%>%filter(SITE=="Kenya")%>%select("MOMID", "PREGID", "SITE","INFANTID",matches("M14_TCB_UMOLL_LBORRES"))
#df_infant_tcb_Zambia<-df_infant_tcb%>%filter(SITE=="Zambia")%>%select("MOMID", "PREGID", "SITE","INFANTID",matches("M14_TCB_UMOLL_LBORRES"))
#df_infant_tcb_India_CMC<-df_infant_tcb%>%filter(SITE=="India-CMC")%>%select("MOMID", "PREGID", "SITE","INFANTID",matches("M14_TCB_UMOLL_LBORRES"))
#df_infant_tcb_India_SAS<-df_infant_tcb%>%filter(SITE=="India-SAS")%>%select("MOMID", "PREGID", "SITE","INFANTID",matches("M14_TCB_UMOLL_LBORRES"))
#df_infant_tcb_Ghana<-df_infant_tcb%>%filter(SITE=="Ghana")%>%select("MOMID", "PREGID", "SITE","INFANTID",matches("M14_TCB_UMOLL_LBORRES"))
#df_infant_tcb_Pakistan<-df_infant_tcb%>%filter(SITE=="Pakistan")%>%select("MOMID", "PREGID", "SITE","INFANTID",matches("M14_TCB_UMOLL_LBORRES"))

#Transform TcB measurement units for Zambia site
df_infant_tcb[df_infant_tcb$SITE=="Zambia",]$M14_TCB_UMOLL_LBORRES_7<-df_infant_tcb[df_infant_tcb$SITE=="Zambia",]$M14_TCB_UMOLL_LBORRES_7/17.1
df_infant_tcb[df_infant_tcb$SITE=="Zambia",]$M14_TCB_UMOLL_LBORRES_8<-df_infant_tcb[df_infant_tcb$SITE=="Zambia",]$M14_TCB_UMOLL_LBORRES_8/17.1
df_infant_tcb[df_infant_tcb$SITE=="Zambia",]$M14_TCB_UMOLL_LBORRES_9<-df_infant_tcb[df_infant_tcb$SITE=="Zambia",]$M14_TCB_UMOLL_LBORRES_9/17.1
df_infant_tcb[df_infant_tcb$SITE=="Zambia",]$M14_TCB_UMOLL_LBORRES_10<-df_infant_tcb[df_infant_tcb$SITE=="Zambia",]$M14_TCB_UMOLL_LBORRES_10/17.1
df_infant_tcb[df_infant_tcb$SITE=="Zambia",]$M14_TCB_UMOLL_LBORRES_11<-df_infant_tcb[df_infant_tcb$SITE=="Zambia",]$M14_TCB_UMOLL_LBORRES_11/17.1
df_infant_tcb[df_infant_tcb$SITE=="Zambia",]$M14_TCB_UMOLL_LBORRES_12<-df_infant_tcb[df_infant_tcb$SITE=="Zambia",]$M14_TCB_UMOLL_LBORRES_12/17.1
df_infant_tcb[df_infant_tcb$SITE=="Zambia",]$M11_TBILIRUBIN_UMOLL_LBORRES_6<-df_infant_tcb[df_infant_tcb$SITE=="Zambia",]$M11_TBILIRUBIN_UMOLL_LBORRES_6/17.1

#Transform TcB measurement units for Kenya site
df_infant_tcb[df_infant_tcb$SITE=="Kenya",]$M14_TCB_UMOLL_LBORRES_7<-df_infant_tcb[df_infant_tcb$SITE=="Kenya",]$M14_TCB_UMOLL_LBORRES_7/17.1
df_infant_tcb[df_infant_tcb$SITE=="Kenya",]$M14_TCB_UMOLL_LBORRES_8<-df_infant_tcb[df_infant_tcb$SITE=="Kenya",]$M14_TCB_UMOLL_LBORRES_8/17.1
df_infant_tcb[df_infant_tcb$SITE=="Kenya",]$M14_TCB_UMOLL_LBORRES_9<-df_infant_tcb[df_infant_tcb$SITE=="Kenya",]$M14_TCB_UMOLL_LBORRES_9/17.1
df_infant_tcb[df_infant_tcb$SITE=="Kenya",]$M14_TCB_UMOLL_LBORRES_10<-df_infant_tcb[df_infant_tcb$SITE=="Kenya",]$M14_TCB_UMOLL_LBORRES_10/17.1
df_infant_tcb[df_infant_tcb$SITE=="Kenya",]$M14_TCB_UMOLL_LBORRES_11<-df_infant_tcb[df_infant_tcb$SITE=="Kenya",]$M14_TCB_UMOLL_LBORRES_11/17.1
df_infant_tcb[df_infant_tcb$SITE=="Kenya",]$M14_TCB_UMOLL_LBORRES_12<-df_infant_tcb[df_infant_tcb$SITE=="Kenya",]$M14_TCB_UMOLL_LBORRES_12/17.1
df_infant_tcb[df_infant_tcb$SITE=="Kenya",]$M11_TBILIRUBIN_UMOLL_LBORRES_6<-df_infant_tcb[df_infant_tcb$SITE=="Kenya",]$M11_TBILIRUBIN_UMOLL_LBORRES_6/17.1

#Transform TcB measurement units for India-SAS site
df_infant_tcb[df_infant_tcb$SITE=="India-SAS",]$M14_TCB_UMOLL_LBORRES_7<-df_infant_tcb[df_infant_tcb$SITE=="India-SAS",]$M14_TCB_UMOLL_LBORRES_7/17.1
df_infant_tcb[df_infant_tcb$SITE=="India-SAS",]$M14_TCB_UMOLL_LBORRES_8<-df_infant_tcb[df_infant_tcb$SITE=="India-SAS",]$M14_TCB_UMOLL_LBORRES_8/17.1
df_infant_tcb[df_infant_tcb$SITE=="India-SAS",]$M14_TCB_UMOLL_LBORRES_9<-df_infant_tcb[df_infant_tcb$SITE=="India-SAS",]$M14_TCB_UMOLL_LBORRES_9/17.1
df_infant_tcb[df_infant_tcb$SITE=="India-SAS",]$M14_TCB_UMOLL_LBORRES_10<-df_infant_tcb[df_infant_tcb$SITE=="India-SAS",]$M14_TCB_UMOLL_LBORRES_10/17.1
df_infant_tcb[df_infant_tcb$SITE=="India-SAS",]$M14_TCB_UMOLL_LBORRES_11<-df_infant_tcb[df_infant_tcb$SITE=="India-SAS",]$M14_TCB_UMOLL_LBORRES_11/17.1
df_infant_tcb[df_infant_tcb$SITE=="India-SAS",]$M14_TCB_UMOLL_LBORRES_12<-df_infant_tcb[df_infant_tcb$SITE=="India-SAS",]$M14_TCB_UMOLL_LBORRES_12/17.1
df_infant_tcb[df_infant_tcb$SITE=="India-SAS",]$M11_TBILIRUBIN_UMOLL_LBORRES_6<-df_infant_tcb[df_infant_tcb$SITE=="India-SAS",]$M11_TBILIRUBIN_UMOLL_LBORRES_6/17.1

rm(df_maternal)
rm(MatData_Wide)