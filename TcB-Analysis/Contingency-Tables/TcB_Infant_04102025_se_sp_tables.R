library(openxlsx)
library(TSB.NICE)
library(gt)
library(dplyr)
library(stringr)
library(tibble)

#Delivery time
delivery.time_1 <- paste(df_infant_tcb$M09_DELIV_DSSTDAT_INF1, df_infant_tcb$M09_DELIV_DSSTTIM_INF1)
delivery.time_2 <- paste(df_infant_tcb$M09_DELIV_DSSTDAT_INF2, df_infant_tcb$M09_DELIV_DSSTTIM_INF2)
delivery.time_3 <- paste(df_infant_tcb$M09_DELIV_DSSTDAT_INF3, df_infant_tcb$M09_DELIV_DSSTTIM_INF3)
delivery.time_4 <- paste(df_infant_tcb$M09_DELIV_DSSTDAT_INF4, df_infant_tcb$M09_DELIV_DSSTTIM_INF4)

delivery.time_1<-as.POSIXct(delivery.time_1, format = "%Y-%m-%d %H:%M")
delivery.time_2<-as.POSIXct(delivery.time_2, format = "%Y-%m-%d %H:%M")
delivery.time_3<-as.POSIXct(delivery.time_3, format = "%Y-%m-%d %H:%M")
delivery.time_4<-as.POSIXct(delivery.time_4, format = "%Y-%m-%d %H:%M")

df_infant_tcb$delivery.time<-coalesce(delivery.time_1,delivery.time_2,delivery.time_3,delivery.time_4)
df_infant_tcb$delivery.date<-coalesce(df_infant_tcb$M09_DELIV_DSSTDAT_INF1,df_infant_tcb$M09_DELIV_DSSTDAT_INF2,df_infant_tcb$M09_DELIV_DSSTDAT_INF3,df_infant_tcb$M09_DELIV_DSSTDAT_INF4)

#GA
df_infant_tcb$birth_GA_day<-difftime(df_infant_tcb$delivery.date,df_infant_tcb$M01_US_OHOSTDAT_1)+df_infant_tcb$BOE_GA_DAYS_ENROLL
df_infant_tcb$birth_GA_day<-ifelse(df_infant_tcb$birth_GA_day>0,df_infant_tcb$birth_GA_day,NA)
df_infant_tcb$birth_GA_week<-floor(df_infant_tcb$birth_GA_day/7)

#Exclude missing values in times
df_infant_tcb$M14_TCB_OBSSTTIM_7<-ifelse(df_infant_tcb$M14_TCB_OBSSTTIM_7 %in% c("55:55","66:66","77:77","88:88","99:99"),NA,df_infant_tcb$M14_TCB_OBSSTTIM_7)
df_infant_tcb$M14_TCB_OBSSTTIM_8<-ifelse(df_infant_tcb$M14_TCB_OBSSTTIM_8 %in% c("55:55","66:66","77:77","88:88","99:99"),NA,df_infant_tcb$M14_TCB_OBSSTTIM_8)
df_infant_tcb$M14_TCB_OBSSTTIM_9<-ifelse(df_infant_tcb$M14_TCB_OBSSTTIM_9 %in% c("55:55","66:66","77:77","88:88","99:99"),NA,df_infant_tcb$M14_TCB_OBSSTTIM_9)

df_infant_tcb$M11_TBILIRUBIN_OBSSTTIM_6<-ifelse(df_infant_tcb$M11_TBILIRUBIN_OBSSTTIM_6 %in% c("55:55","66:66","77:77","88:88","99:99"),NA,df_infant_tcb$M11_TBILIRUBIN_OBSSTTIM_6)


#MNH13
PNC.time_1 <- paste(df_infant_tcb$M14_VISIT_OBSSTDAT_7, df_infant_tcb$M14_TCB_OBSSTTIM_7)
PNC.time_2 <- paste(df_infant_tcb$M14_VISIT_OBSSTDAT_8, df_infant_tcb$M14_TCB_OBSSTTIM_8)
PNC.time_3 <- paste(df_infant_tcb$M14_VISIT_OBSSTDAT_9, df_infant_tcb$M14_TCB_OBSSTTIM_9)


df_infant_tcb$PNC.time_1<-as.POSIXct(PNC.time_1, format = "%Y-%m-%d %H:%M")
df_infant_tcb$PNC.time_2<-as.POSIXct(PNC.time_2, format = "%Y-%m-%d %H:%M")
df_infant_tcb$PNC.time_3<-as.POSIXct(PNC.time_3, format = "%Y-%m-%d %H:%M")


df_infant_tcb$PNC_age_1<-floor(difftime(df_infant_tcb$PNC.time_1,df_infant_tcb$delivery.time,units = "hours"))
df_infant_tcb$PNC_age_2<-floor(difftime(df_infant_tcb$PNC.time_2,df_infant_tcb$delivery.time,units = "hours"))
df_infant_tcb$PNC_age_3<-floor(difftime(df_infant_tcb$PNC.time_3,df_infant_tcb$delivery.time,units = "hours"))


df_infant_tcb$PNC_age_1<-ifelse(df_infant_tcb$PNC_age_1>=0,df_infant_tcb$PNC_age_1,NA)
df_infant_tcb$PNC_age_2<-ifelse(df_infant_tcb$PNC_age_2>=0,df_infant_tcb$PNC_age_2,NA)
df_infant_tcb$PNC_age_3<-ifelse(df_infant_tcb$PNC_age_3>=0,df_infant_tcb$PNC_age_3,NA)


df_infant_tcb$PNC_age_1_days<-as.numeric(df_infant_tcb$PNC_age_1) %/% 24
df_infant_tcb$PNC_age_1_hours<-as.numeric(df_infant_tcb$PNC_age_1) %% 24
df_infant_tcb$PNC_age_2_days<-as.numeric(df_infant_tcb$PNC_age_2) %/% 24
df_infant_tcb$PNC_age_2_hours<-as.numeric(df_infant_tcb$PNC_age_2) %% 24
df_infant_tcb$PNC_age_3_days<-as.numeric(df_infant_tcb$PNC_age_3) %/% 24
df_infant_tcb$PNC_age_3_hours<-as.numeric(df_infant_tcb$PNC_age_3) %% 24

#MNH11
birth_outcome.time<-paste(df_infant_tcb$M11_VISIT_OBSSTDAT_6,df_infant_tcb$M11_TBILIRUBIN_OBSSTTIM_6)
birth_outcome.time<-as.POSIXct(birth_outcome.time, format = "%Y-%m-%d %H:%M")
df_infant_tcb$birth_outcome.time<-birth_outcome.time

df_infant_tcb$birth_outcome_age<-difftime(df_infant_tcb$birth_outcome.time,df_infant_tcb$delivery.time,units = "hours")
df_infant_tcb$birth_outcome_age<-ifelse(df_infant_tcb$birth_outcome_age>=0,df_infant_tcb$birth_outcome_age,NA)

df_infant_tcb$birth_outcome_days<-as.numeric(df_infant_tcb$birth_outcome_age) %/% 24
df_infant_tcb$birth_outcome_hours<-as.numeric(df_infant_tcb$birth_outcome_age) %% 24

df_infant_tcb$M11_TBILIRUBIN_UMOLL_LBORRES_6<-ifelse(df_infant_tcb$M11_TBILIRUBIN_UMOLL_LBORRES_6>0,df_infant_tcb$M11_TBILIRUBIN_UMOLL_LBORRES_6,NA)

df_infant_tcb$M14_TCB_UMOLL_LBORRES_7<-ifelse(df_infant_tcb$M14_TCB_UMOLL_LBORRES_7>0,df_infant_tcb$M14_TCB_UMOLL_LBORRES_7,NA)
df_infant_tcb$M14_TCB_UMOLL_LBORRES_8<-ifelse(df_infant_tcb$M14_TCB_UMOLL_LBORRES_8>0,df_infant_tcb$M14_TCB_UMOLL_LBORRES_8,NA)
df_infant_tcb$M14_TCB_UMOLL_LBORRES_9<-ifelse(df_infant_tcb$M14_TCB_UMOLL_LBORRES_9>0,df_infant_tcb$M14_TCB_UMOLL_LBORRES_9,NA)

TCB_threshold_fun<-function(GA,Days,Hours){
  if (is.na(GA)|is.na(Days)|is.na(Hours)){
    result<-NA
  }
  else if (GA>=38) {
    result<-TSB_NICE("P0",">= 38 weeks",Days,Hours)
  }
  else if (GA==37) {
    result<-TSB_NICE("P0","37 weeks",Days,Hours)
  }
  else if (GA==36) {
    result<-TSB_NICE("P0","36 weeks",Days,Hours)
  }
  else if (GA==35) {
    result<-TSB_NICE("P0","35 weeks",Days,Hours)
  }
  else if (GA==34) {
    result<-TSB_NICE("P0","34 weeks",Days,Hours)
  }
  else if (GA==33) {
    result<-TSB_NICE("P0","33 weeks",Days,Hours)
  }
  else if (GA==32) {
    result<-TSB_NICE("P0","32 weeks",Days,Hours)
  }
  else if (GA==31) {
    result<-TSB_NICE("P0","31 weeks",Days,Hours)
  }
  else if (GA==30) {
    result<-TSB_NICE("P0","30 weeks",Days,Hours)
  }
  else if (GA==29) {
    result<-TSB_NICE("P0","29 weeks",Days,Hours)
  }
  else if (GA==28) {
    result<-TSB_NICE("P0","28 weeks",Days,Hours)
  }
  else if (GA==27) {
    result<-TSB_NICE("P0","27 weeks",Days,Hours)
  }
  else if (GA==26) {
    result<-TSB_NICE("P0","26 weeks",Days,Hours)
  }
  else if (GA==25) {
    result<-TSB_NICE("P0","25 weeks",Days,Hours)
  }
  else if (GA==24) {
    result<-TSB_NICE("P0","24 weeks",Days,Hours)
  }
  else if (GA==23) {
    result<-TSB_NICE("P0","23 weeks",Days,Hours)
  }
  else {
    result<-NA
  }
  return(result)
}

n.obs<-nrow(df_infant_tcb)

df_infant_tcb$TCB_threshold_PNC_1<-rep(1,n.obs)
df_infant_tcb$TCB_threshold_PNC_2<-rep(1,n.obs)
df_infant_tcb$TCB_threshold_PNC_3<-rep(1,n.obs)
df_infant_tcb$TCB_threshold_birth_outcome<-rep(1,n.obs)

for (i in 1:n.obs) {
  df_infant_tcb$TCB_threshold_PNC_1[i]<-TCB_threshold_fun(df_infant_tcb$birth_GA_week[i],df_infant_tcb$PNC_age_1_days[i],df_infant_tcb$PNC_age_1_hours[i])
  df_infant_tcb$TCB_threshold_PNC_2[i]<-TCB_threshold_fun(df_infant_tcb$birth_GA_week[i],df_infant_tcb$PNC_age_2_days[i],df_infant_tcb$PNC_age_2_hours[i]) 
  df_infant_tcb$TCB_threshold_PNC_3[i]<-TCB_threshold_fun(df_infant_tcb$birth_GA_week[i],df_infant_tcb$PNC_age_3_days[i],df_infant_tcb$PNC_age_3_hours[i]) 
  df_infant_tcb$TCB_threshold_birth_outcome[i]<-TCB_threshold_fun(df_infant_tcb$birth_GA_week[i],df_infant_tcb$birth_outcome_days[i],df_infant_tcb$birth_outcome_hours[i])
}

#TCB threshold based on NICE
df_infant_tcb$TCB_threshold_PNC_1_ind<-ifelse(df_infant_tcb$M14_TCB_UMOLL_LBORRES_7>=(df_infant_tcb$TCB_threshold_PNC_1-3),1,0)
df_infant_tcb$TCB_threshold_PNC_2_ind<-ifelse(df_infant_tcb$M14_TCB_UMOLL_LBORRES_8>=(df_infant_tcb$TCB_threshold_PNC_2-3),1,0)
df_infant_tcb$TCB_threshold_PNC_3_ind<-ifelse(df_infant_tcb$M14_TCB_UMOLL_LBORRES_9>=(df_infant_tcb$TCB_threshold_PNC_3-3),1,0)
df_infant_tcb$TCB_threshold_birth_outcome_ind<-ifelse(df_infant_tcb$M11_TBILIRUBIN_UMOLL_LBORRES_6>=(df_infant_tcb$TCB_threshold_birth_outcome-3),1,0)

#TCB>=15
df_infant_tcb$TCB_15_PNC_1_ind<-ifelse(df_infant_tcb$M14_TCB_UMOLL_LBORRES_7>=15,1,0)
df_infant_tcb$TCB_15_PNC_2_ind<-ifelse(df_infant_tcb$M14_TCB_UMOLL_LBORRES_8>=15,1,0)
df_infant_tcb$TCB_15_PNC_3_ind<-ifelse(df_infant_tcb$M14_TCB_UMOLL_LBORRES_9>=15,1,0)
df_infant_tcb$TCB_15_birth_outcome_ind<-ifelse(df_infant_tcb$M11_TBILIRUBIN_UMOLL_LBORRES_6>=15,1,0)

#Jaundice diagnosis
df_infant_tcb$M13_JAUND_CEOCCUR_7<-ifelse(df_infant_tcb$M13_JAUND_CEOCCUR_7 %in% c(0,1),df_infant_tcb$M13_JAUND_CEOCCUR_7,NA)
df_infant_tcb$M13_JAUND_CEOCCUR_8<-ifelse(df_infant_tcb$M13_JAUND_CEOCCUR_8 %in% c(0,1),df_infant_tcb$M13_JAUND_CEOCCUR_8,NA)
df_infant_tcb$M13_JAUND_CEOCCUR_9<-ifelse(df_infant_tcb$M13_JAUND_CEOCCUR_9 %in% c(0,1),df_infant_tcb$M13_JAUND_CEOCCUR_9,NA)
df_infant_tcb$M11_JAUND_CEOCCUR_6<-ifelse(df_infant_tcb$M11_JAUND_CEOCCUR_6 %in% c(0,1),df_infant_tcb$M11_JAUND_CEOCCUR_6,NA)

df_infant_tcb_Kenya<-df_infant_tcb%>%filter(SITE=="Kenya")
df_infant_tcb_Zambia<-df_infant_tcb%>%filter(SITE=="Zambia")
df_infant_tcb_Pakistan<-df_infant_tcb%>%filter(SITE=="Pakistan")
df_infant_tcb_Ghana<-df_infant_tcb%>%filter(SITE=="Ghana")
df_infant_tcb_India_CMC<-df_infant_tcb%>%filter(SITE=="India-CMC")
df_infant_tcb_India_SAS<-df_infant_tcb%>%filter(SITE=="India-SAS")


#Extract percent values
extract_pct <- function(x) {
  x <- as.character(x)
  as.numeric(stringr::str_extract(x, "(?<=\\().*?(?=%\\))"))
}


###Build up se/sp tables
se_sp_fun <- function(data, variable1, variable2) {
  TP <- sum(data[[variable1]] == 1 & data[[variable2]] == 1, na.rm = TRUE)
  FP <- sum(data[[variable1]] == 0 & data[[variable2]] == 1, na.rm = TRUE)
  FN <- sum(data[[variable1]] == 1 & data[[variable2]] == 0, na.rm = TRUE)
  TN <- sum(data[[variable1]] == 0 & data[[variable2]] == 0, na.rm = TRUE)
  
  deno_se <- sum(TP, FN)
  deno_sp <- sum(FP, TN)
  
  se <- paste0(TP, "/", deno_se, "(", round(TP / deno_se * 100, 2), "%)")
  sp <- paste0(TN, "/", deno_sp, "(", round(TN / deno_sp * 100, 2), "%)")
  
  return(c(se,sp))
}

# TCB ??? NICE (reference) vs TCB ??? 15 (test)
table_NICE_vs_15 <- matrix(NA, nrow = 7, ncol = 6)
rownames(table_NICE_vs_15) <- c("Pooled", "India CMC", "India SAS", "Pakistan", "Ghana", "Kenya", "Zambia")
colnames(table_NICE_vs_15) <- c("IPC Se", "IPC Sp", "PNC-0 Se", "PNC-0 Sp", "PNC-1 Se", "PNC-1 Sp")

table_NICE_vs_15[1, 1:2] <- se_sp_fun(df_infant_tcb, "TCB_15_birth_outcome_ind", "TCB_threshold_birth_outcome_ind")
table_NICE_vs_15[1, 3:4] <- se_sp_fun(df_infant_tcb, "TCB_15_PNC_1_ind", "TCB_threshold_PNC_1_ind")
table_NICE_vs_15[1, 5:6] <- se_sp_fun(df_infant_tcb, "TCB_15_PNC_2_ind", "TCB_threshold_PNC_2_ind")

table_NICE_vs_15[2, 1:2] <- se_sp_fun(df_infant_tcb_India_CMC, "TCB_15_birth_outcome_ind", "TCB_threshold_birth_outcome_ind")
table_NICE_vs_15[2, 3:4] <- se_sp_fun(df_infant_tcb_India_CMC, "TCB_15_PNC_1_ind", "TCB_threshold_PNC_1_ind")
table_NICE_vs_15[2, 5:6] <- se_sp_fun(df_infant_tcb_India_CMC, "TCB_15_PNC_2_ind", "TCB_threshold_PNC_2_ind")

table_NICE_vs_15[3, 1:2] <- se_sp_fun(df_infant_tcb_India_SAS, "TCB_15_birth_outcome_ind", "TCB_threshold_birth_outcome_ind")
table_NICE_vs_15[3, 3:4] <- se_sp_fun(df_infant_tcb_India_SAS, "TCB_15_PNC_1_ind", "TCB_threshold_PNC_1_ind")
table_NICE_vs_15[3, 5:6] <- se_sp_fun(df_infant_tcb_India_SAS, "TCB_15_PNC_2_ind", "TCB_threshold_PNC_2_ind")

table_NICE_vs_15[4, 1:2] <- se_sp_fun(df_infant_tcb_Pakistan, "TCB_15_birth_outcome_ind", "TCB_threshold_birth_outcome_ind")
table_NICE_vs_15[4, 3:4] <- se_sp_fun(df_infant_tcb_Pakistan, "TCB_15_PNC_1_ind", "TCB_threshold_PNC_1_ind")
table_NICE_vs_15[4, 5:6] <- se_sp_fun(df_infant_tcb_Pakistan, "TCB_15_PNC_2_ind", "TCB_threshold_PNC_2_ind")

table_NICE_vs_15[5, 1:2] <- se_sp_fun(df_infant_tcb_Ghana, "TCB_15_birth_outcome_ind", "TCB_threshold_birth_outcome_ind")
table_NICE_vs_15[5, 3:4] <- se_sp_fun(df_infant_tcb_Ghana, "TCB_15_PNC_1_ind", "TCB_threshold_PNC_1_ind")
table_NICE_vs_15[5, 5:6] <- se_sp_fun(df_infant_tcb_Ghana, "TCB_15_PNC_2_ind", "TCB_threshold_PNC_2_ind")

table_NICE_vs_15[6, 1:2] <- se_sp_fun(df_infant_tcb_Kenya, "TCB_15_birth_outcome_ind", "TCB_threshold_birth_outcome_ind")
table_NICE_vs_15[6, 3:4] <- se_sp_fun(df_infant_tcb_Kenya, "TCB_15_PNC_1_ind", "TCB_threshold_PNC_1_ind")
table_NICE_vs_15[6, 5:6] <- se_sp_fun(df_infant_tcb_Kenya, "TCB_15_PNC_2_ind", "TCB_threshold_PNC_2_ind")

table_NICE_vs_15[7, 1:2] <- se_sp_fun(df_infant_tcb_Zambia, "TCB_15_birth_outcome_ind", "TCB_threshold_birth_outcome_ind")
table_NICE_vs_15[7, 3:4] <- se_sp_fun(df_infant_tcb_Zambia, "TCB_15_PNC_1_ind", "TCB_threshold_PNC_1_ind")
table_NICE_vs_15[7, 5:6] <- se_sp_fun(df_infant_tcb_Zambia, "TCB_15_PNC_2_ind", "TCB_threshold_PNC_2_ind")


#TCB ??? NICE(reference) vs Jaundice (test)
table_NICE_vs_Jaundice <- matrix(NA, nrow = 7, ncol = 6)
rownames(table_NICE_vs_Jaundice) <- c("Pooled", "India CMC", "India SAS", "Pakistan", "Ghana", "Kenya", "Zambia")
colnames(table_NICE_vs_Jaundice) <- c("IPC Se", "IPC Sp", "PNC-0 Se", "PNC-0 Sp", "PNC-1 Se", "PNC-1 Sp")

table_NICE_vs_Jaundice[1, 1:2] <- se_sp_fun(df_infant_tcb, "TCB_threshold_birth_outcome_ind", "M11_JAUND_CEOCCUR_6")
table_NICE_vs_Jaundice[1, 3:4] <- se_sp_fun(df_infant_tcb, "TCB_threshold_PNC_1_ind", "M13_JAUND_CEOCCUR_7")
table_NICE_vs_Jaundice[1, 5:6] <- se_sp_fun(df_infant_tcb, "TCB_threshold_PNC_2_ind", "M13_JAUND_CEOCCUR_8")

table_NICE_vs_Jaundice[2, 1:2] <- se_sp_fun(df_infant_tcb_India_CMC, "TCB_threshold_birth_outcome_ind", "M11_JAUND_CEOCCUR_6")
table_NICE_vs_Jaundice[2, 3:4] <- se_sp_fun(df_infant_tcb_India_CMC, "TCB_threshold_PNC_1_ind", "M13_JAUND_CEOCCUR_7")
table_NICE_vs_Jaundice[2, 5:6] <- se_sp_fun(df_infant_tcb_India_CMC, "TCB_threshold_PNC_2_ind", "M13_JAUND_CEOCCUR_8")

table_NICE_vs_Jaundice[3, 1:2] <- se_sp_fun(df_infant_tcb_India_SAS, "TCB_threshold_birth_outcome_ind", "M11_JAUND_CEOCCUR_6")
table_NICE_vs_Jaundice[3, 3:4] <- se_sp_fun(df_infant_tcb_India_SAS, "TCB_threshold_PNC_1_ind", "M13_JAUND_CEOCCUR_7")
table_NICE_vs_Jaundice[3, 5:6] <- se_sp_fun(df_infant_tcb_India_SAS, "TCB_threshold_PNC_2_ind", "M13_JAUND_CEOCCUR_8")

table_NICE_vs_Jaundice[4, 1:2] <- se_sp_fun(df_infant_tcb_Pakistan, "TCB_threshold_birth_outcome_ind", "M11_JAUND_CEOCCUR_6")
table_NICE_vs_Jaundice[4, 3:4] <- se_sp_fun(df_infant_tcb_Pakistan, "TCB_threshold_PNC_1_ind", "M13_JAUND_CEOCCUR_7")
table_NICE_vs_Jaundice[4, 5:6] <- se_sp_fun(df_infant_tcb_Pakistan, "TCB_threshold_PNC_2_ind", "M13_JAUND_CEOCCUR_8")

table_NICE_vs_Jaundice[5, 1:2] <- se_sp_fun(df_infant_tcb_Ghana, "TCB_threshold_birth_outcome_ind", "M11_JAUND_CEOCCUR_6")
table_NICE_vs_Jaundice[5, 3:4] <- se_sp_fun(df_infant_tcb_Ghana, "TCB_threshold_PNC_1_ind", "M13_JAUND_CEOCCUR_7")
table_NICE_vs_Jaundice[5, 5:6] <- se_sp_fun(df_infant_tcb_Ghana, "TCB_threshold_PNC_2_ind", "M13_JAUND_CEOCCUR_8")

table_NICE_vs_Jaundice[6, 1:2] <- se_sp_fun(df_infant_tcb_Kenya, "TCB_threshold_birth_outcome_ind", "M11_JAUND_CEOCCUR_6")
table_NICE_vs_Jaundice[6, 3:4] <- se_sp_fun(df_infant_tcb_Kenya, "TCB_threshold_PNC_1_ind", "M13_JAUND_CEOCCUR_7")
table_NICE_vs_Jaundice[6, 5:6] <- se_sp_fun(df_infant_tcb_Kenya, "TCB_threshold_PNC_2_ind", "M13_JAUND_CEOCCUR_8")

table_NICE_vs_Jaundice[7, 1:2] <- se_sp_fun(df_infant_tcb_Zambia, "TCB_threshold_birth_outcome_ind", "M11_JAUND_CEOCCUR_6")
table_NICE_vs_Jaundice[7, 3:4] <- se_sp_fun(df_infant_tcb_Zambia, "TCB_threshold_PNC_1_ind", "M13_JAUND_CEOCCUR_7")
table_NICE_vs_Jaundice[7, 5:6] <- se_sp_fun(df_infant_tcb_Zambia, "TCB_threshold_PNC_2_ind", "M13_JAUND_CEOCCUR_8")


#TCB ??? 15(reference) vs Jaundice (test)
table_15_vs_Jaundice <- matrix(NA, nrow = 7, ncol = 6)
rownames(table_15_vs_Jaundice) <- c("Pooled", "India CMC", "India SAS", "Pakistan", "Ghana", "Kenya", "Zambia")
colnames(table_15_vs_Jaundice) <- c("IPC Se", "IPC Sp", "PNC-0 Se", "PNC-0 Sp", "PNC-1 Se", "PNC-1 Sp")

table_15_vs_Jaundice[1, 1:2] <- se_sp_fun(df_infant_tcb, "TCB_15_birth_outcome_ind", "M11_JAUND_CEOCCUR_6")
table_15_vs_Jaundice[1, 3:4] <- se_sp_fun(df_infant_tcb, "TCB_15_PNC_1_ind", "M13_JAUND_CEOCCUR_7")
table_15_vs_Jaundice[1, 5:6] <- se_sp_fun(df_infant_tcb, "TCB_15_PNC_2_ind", "M13_JAUND_CEOCCUR_8")

table_15_vs_Jaundice[2, 1:2] <- se_sp_fun(df_infant_tcb_India_CMC, "TCB_15_birth_outcome_ind", "M11_JAUND_CEOCCUR_6")
table_15_vs_Jaundice[2, 3:4] <- se_sp_fun(df_infant_tcb_India_CMC, "TCB_15_PNC_1_ind", "M13_JAUND_CEOCCUR_7")
table_15_vs_Jaundice[2, 5:6] <- se_sp_fun(df_infant_tcb_India_CMC, "TCB_15_PNC_2_ind", "M13_JAUND_CEOCCUR_8")

table_15_vs_Jaundice[3, 1:2] <- se_sp_fun(df_infant_tcb_India_SAS, "TCB_15_birth_outcome_ind", "M11_JAUND_CEOCCUR_6")
table_15_vs_Jaundice[3, 3:4] <- se_sp_fun(df_infant_tcb_India_SAS, "TCB_15_PNC_1_ind", "M13_JAUND_CEOCCUR_7")
table_15_vs_Jaundice[3, 5:6] <- se_sp_fun(df_infant_tcb_India_SAS, "TCB_15_PNC_2_ind", "M13_JAUND_CEOCCUR_8")

table_15_vs_Jaundice[4, 1:2] <- se_sp_fun(df_infant_tcb_Pakistan, "TCB_15_birth_outcome_ind", "M11_JAUND_CEOCCUR_6")
table_15_vs_Jaundice[4, 3:4] <- se_sp_fun(df_infant_tcb_Pakistan, "TCB_15_PNC_1_ind", "M13_JAUND_CEOCCUR_7")
table_15_vs_Jaundice[4, 5:6] <- se_sp_fun(df_infant_tcb_Pakistan, "TCB_15_PNC_2_ind", "M13_JAUND_CEOCCUR_8")

table_15_vs_Jaundice[5, 1:2] <- se_sp_fun(df_infant_tcb_Ghana, "TCB_15_birth_outcome_ind", "M11_JAUND_CEOCCUR_6")
table_15_vs_Jaundice[5, 3:4] <- se_sp_fun(df_infant_tcb_Ghana, "TCB_15_PNC_1_ind", "M13_JAUND_CEOCCUR_7")
table_15_vs_Jaundice[5, 5:6] <- se_sp_fun(df_infant_tcb_Ghana, "TCB_15_PNC_2_ind", "M13_JAUND_CEOCCUR_8")

table_15_vs_Jaundice[6, 1:2] <- se_sp_fun(df_infant_tcb_Kenya, "TCB_15_birth_outcome_ind", "M11_JAUND_CEOCCUR_6")
table_15_vs_Jaundice[6, 3:4] <- se_sp_fun(df_infant_tcb_Kenya, "TCB_15_PNC_1_ind", "M13_JAUND_CEOCCUR_7")
table_15_vs_Jaundice[6, 5:6] <- se_sp_fun(df_infant_tcb_Kenya, "TCB_15_PNC_2_ind", "M13_JAUND_CEOCCUR_8")

table_15_vs_Jaundice[7, 1:2] <- se_sp_fun(df_infant_tcb_Zambia, "TCB_15_birth_outcome_ind", "M11_JAUND_CEOCCUR_6")
table_15_vs_Jaundice[7, 3:4] <- se_sp_fun(df_infant_tcb_Zambia, "TCB_15_PNC_1_ind", "M13_JAUND_CEOCCUR_7")
table_15_vs_Jaundice[7, 5:6] <- se_sp_fun(df_infant_tcb_Zambia, "TCB_15_PNC_2_ind", "M13_JAUND_CEOCCUR_8")

###Generate output tables 
render_styled_table <- function(tbl, title_text) {
  df <- as.data.frame(tbl)
  df <- tibble::rownames_to_column(df, var = "Site")
  
  df_colored <- df %>%
    mutate(
      IPC_Se_pct    = extract_pct(`IPC Se`),
      IPC_Sp_pct    = extract_pct(`IPC Sp`),
      PNC0_Se_pct   = extract_pct(`PNC-0 Se`),
      PNC0_Sp_pct   = extract_pct(`PNC-0 Sp`),
      PNC1_Se_pct   = extract_pct(`PNC-1 Se`),
      PNC1_Sp_pct   = extract_pct(`PNC-1 Sp`)
    ) %>%
    mutate(across(ends_with("_pct"), ~ replace_na(., 0)))
  
  gt(df_colored) %>%
    tab_spanner("IPC (0-3 days)", columns = c(IPC_Se_pct, IPC_Sp_pct)) %>%
    tab_spanner("PNC-0 (4-6 days)", columns = c(PNC0_Se_pct, PNC0_Sp_pct)) %>%
    tab_spanner("PNC-1 (7-14 days)", columns = c(PNC1_Se_pct, PNC1_Sp_pct)) %>%
    
    data_color(
      columns = c(IPC_Se_pct, IPC_Sp_pct, PNC0_Se_pct, PNC0_Sp_pct, PNC1_Se_pct, PNC1_Sp_pct),
      apply_to = "fill",
      colors = scales::col_bin(
        palette = c("mistyrose", "lightyellow", "lightgreen"),
        bins = c(0, 40, 70, 100)
      )
    ) %>%
    
    cols_merge(columns = c(IPC_Se_pct, `IPC Se`), pattern = "{2}") %>%
    cols_merge(columns = c(IPC_Sp_pct, `IPC Sp`), pattern = "{2}") %>%
    cols_merge(columns = c(PNC0_Se_pct, `PNC-0 Se`), pattern = "{2}") %>%
    cols_merge(columns = c(PNC0_Sp_pct, `PNC-0 Sp`), pattern = "{2}") %>%
    cols_merge(columns = c(PNC1_Se_pct, `PNC-1 Se`), pattern = "{2}") %>%
    cols_merge(columns = c(PNC1_Sp_pct, `PNC-1 Sp`), pattern = "{2}") %>%
    
    cols_label(
      IPC_Se_pct = "Sens (%)", IPC_Sp_pct = "Spec (%)",
      PNC0_Se_pct = "Sens (%)", PNC0_Sp_pct = "Spec (%)",
      PNC1_Se_pct = "Sens (%)", PNC1_Sp_pct = "Spec (%)"
    ) %>%
    
    tab_header(
      title = md(paste0("**", title_text, "**"))
    ) %>%
    tab_options(
      table.font.size = px(13),
      column_labels.font.weight = "bold"
    )
}

render_styled_table(table_15_vs_Jaundice, "Visual Inspection vs. TCB >= 15")
render_styled_table(table_NICE_vs_Jaundice, "Visual Inspection vs. TCB >= NICE")
render_styled_table(table_NICE_vs_15, "TCB >= 15 vs. TCB >= NICE")
