library(openxlsx)
library(TSB.NICE)
library(dplyr)

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
df_infant_tcb$M14_TCB_OBSSTTIM_7<-ifelse(df_infant_tcb$M14_TCB_OBSSTTIM_7=="05:05",NA,df_infant_tcb$M14_TCB_OBSSTTIM_7)
df_infant_tcb$M14_TCB_OBSSTTIM_8<-ifelse(df_infant_tcb$M14_TCB_OBSSTTIM_8=="05:05",NA,df_infant_tcb$M14_TCB_OBSSTTIM_8)
df_infant_tcb$M14_TCB_OBSSTTIM_9<-ifelse(df_infant_tcb$M14_TCB_OBSSTTIM_9=="05:05",NA,df_infant_tcb$M14_TCB_OBSSTTIM_9)

df_infant_tcb$M11_TBILIRUBIN_OBSSTTIM_6<-ifelse(df_infant_tcb$M11_TBILIRUBIN_OBSSTTIM_6=="05:05",NA,df_infant_tcb$M11_TBILIRUBIN_OBSSTTIM_6)
df_infant_tcb$M11_TBILIRUBIN_OBSSTTIM_6<-ifelse(df_infant_tcb$M11_TBILIRUBIN_OBSSTTIM_6=="07:07",NA,df_infant_tcb$M11_TBILIRUBIN_OBSSTTIM_6)

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

df_infant_tcb$PNC_age_days_exact1<-difftime(df_infant_tcb$PNC.time_1,df_infant_tcb$delivery.time,units = "days")
df_infant_tcb$PNC_age_days_exact2<-difftime(df_infant_tcb$PNC.time_2,df_infant_tcb$delivery.time,units = "days")
df_infant_tcb$PNC_age_days_exact3<-difftime(df_infant_tcb$PNC.time_3,df_infant_tcb$delivery.time,units = "days")

df_infant_tcb$PNC_age_days_exact1<-ifelse(df_infant_tcb$PNC_age_days_exact1>=0,df_infant_tcb$PNC_age_days_exact1,NA)
df_infant_tcb$PNC_age_days_exact2<-ifelse(df_infant_tcb$PNC_age_days_exact2>=0,df_infant_tcb$PNC_age_days_exact2,NA)
df_infant_tcb$PNC_age_days_exact3<-ifelse(df_infant_tcb$PNC_age_days_exact3>=0,df_infant_tcb$PNC_age_days_exact3,NA)

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

df_infant_tcb$birth_outcome_age_days_exact<-difftime(df_infant_tcb$birth_outcome.time,df_infant_tcb$delivery.time,units = "days")
df_infant_tcb$birth_outcome_age_days_exact<-ifelse(df_infant_tcb$birth_outcome_age_days_exact>=0,df_infant_tcb$birth_outcome_age_days_exact,NA)

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

#Jaundice and Severe Jaundice
df_infant_tcb <- df_infant_tcb %>%
  mutate(
    PNC_severe_jaund_ind1 = case_when(
      (M13_JAUND_CEOCCUR_7 == 1 & PNC_age_1 < 24) ~ 1,
      M13_JAUND_CESTTIM_7 == 1 ~ 1,
      (M13_JAUND_CEOCCUR_7 == 1 & PNC_age_1 >= (21 * 24)) ~ 1,
      M13_YELL_CEOCCUR_7 == 1 ~ 1,
      TRUE ~ 0
    ),
    PNC_severe_jaund_ind2 = case_when(
      (M13_JAUND_CEOCCUR_8 == 1 & PNC_age_2 < 24) ~ 1,
      M13_JAUND_CESTTIM_8 == 1 ~ 1,
      (M13_JAUND_CEOCCUR_8 == 1 & PNC_age_2 >= (21 * 24)) ~ 1,
      M13_YELL_CEOCCUR_8 == 1 ~ 1,
      TRUE ~ 0
    ),
    PNC_severe_jaund_ind3 = case_when(
      (M13_JAUND_CEOCCUR_9 == 1 & PNC_age_3 < 24) ~ 1,
      M13_JAUND_CESTTIM_9 == 1 ~ 1,
      (M13_JAUND_CEOCCUR_9 == 1 & PNC_age_3 >= (21 * 24)) ~ 1,
      M13_YELL_CEOCCUR_9 == 1 ~ 1,
      TRUE ~ 0
    ),
    birth_outcome_severe_jaund_ind = case_when(
      (M11_JAUND_CEOCCUR_6 == 1 & birth_outcome_age < 24) ~ 1,
      M11_BILIRUBIN_LBPERF_6 == 1 ~ 1,
      (M11_JAUND_CEOCCUR_6 == 1 & birth_outcome_age >= (21 * 24)) ~ 1,
      M11_YELLOW_CEOCCUR_6 == 1 ~ 1,
      TRUE ~ 0
    ),
    PNC_jaund_ind1 = case_when(
      (M13_JAUND_CEOCCUR_7  ==1 & PNC_age_1 >= 24 & PNC_age_1 < (21*24) & M13_YELL_CEOCCUR_7==0)
      ~ 1,
      TRUE ~ 0
    ),
    PNC_jaund_ind2 = case_when(
      (M13_JAUND_CEOCCUR_8  ==1 & PNC_age_2 >= 24 & PNC_age_2 < (21*24) & M13_YELL_CEOCCUR_8==0)
      ~ 1,
      TRUE ~ 0
    ),
    PNC_jaund_ind3 = case_when(
      (M13_JAUND_CEOCCUR_9  ==1 & PNC_age_3 >= 24 & PNC_age_3 < (21*24) & M13_YELL_CEOCCUR_9==0)
      ~ 1,
      TRUE ~ 0
    ),
    birth_outcome_jaund_ind = case_when(
      (M11_JAUND_CEOCCUR_6  ==1 & birth_outcome_age >= 24 & birth_outcome_age < (21*24) & M11_YELLOW_CEOCCUR_6==0)
      ~ 1,
      TRUE ~ 0
    ),
  )

df_infant_tcb <- df_infant_tcb %>%
  mutate(
#IPC
    jaundice_category_IPC = case_when(
      birth_outcome_severe_jaund_ind == 1 ~ "Severe Jaundice",
      birth_outcome_jaund_ind == 1 ~ "Jaundice",
      M11_JAUND_CEOCCUR_6 == 0 ~ "No Jaundice",
      TRUE ~ NA_character_
    ),
#PNC-0
    jaundice_category_PNC_0 = case_when(
      PNC_severe_jaund_ind1 == 1 ~ "Severe Jaundice",
      PNC_jaund_ind1 == 1 ~ "Jaundice",
      M13_JAUND_CEOCCUR_7 == 0 ~ "No Jaundice",
      TRUE ~ NA_character_
    ),
#PNC-1
    jaundice_category_PNC_1 = case_when(
      PNC_severe_jaund_ind2 == 1 ~ "Severe Jaundice",
      PNC_jaund_ind2 == 1 ~ "Jaundice",
      M13_JAUND_CEOCCUR_8 == 0 ~ "No Jaundice",
      TRUE ~ NA_character_
    )
  )

result_IPC_fun<-function(data,variable1,variable2,category){

  data <- data %>%
    filter(jaundice_category_IPC == category) %>%
    filter(!is.na(.data[[variable1]]) & !is.na(.data[[variable2]]))  # Remove NA rows in key variables
  
  # Step 2: Handle edge case where no rows remain
  if (nrow(data) == 0) {
    return(matrix("NA", nrow = 2, ncol = 3,
                  dimnames = list(c("Referred by TCB15", "Not referred by TCB15"),
                                  c("Referred by NICE", "Not referred by NICE", "McNemar's test (p-value)"))))
  }
  
  table<-matrix(0,2,3)
  
  mcnemar.pvalue <- tryCatch({
    p <- mcnemar.test(table(data[[variable1]], data[[variable2]]))$p.value
    if (p < 0.001) "<0.001" else round(p, 3)
  }, error = function(e) "NA")
  
  table[1,1]<-sum(data[,variable1]==1&data[,variable2]==1,na.rm=TRUE)
  table[1,2]<-sum(data[,variable1]==0&data[,variable2]==1,na.rm=TRUE)
  table[2,1]<-sum(data[,variable1]==1&data[,variable2]==0,na.rm=TRUE)
  table[2,2]<-sum(data[,variable1]==0&data[,variable2]==0,na.rm=TRUE)
  table[1,3]<-mcnemar.pvalue
  table[2,3]<-NA
  
  Total<-sum(as.numeric(table[1:2, 1:2]))
  
  table[1,1]<-paste0(as.numeric(table[1,1]),"(",round(as.numeric(table[1,1])/as.numeric(Total)*100,2),"%)")
  table[1,2]<-paste0(as.numeric(table[1,2]),"(",round(as.numeric(table[1,2])/as.numeric(Total)*100,2),"%)")
  table[2,1]<-paste0(as.numeric(table[2,1]),"(",round(as.numeric(table[2,1])/as.numeric(Total)*100,2),"%)")
  table[2,2]<-paste0(as.numeric(table[2,2]),"(",round(as.numeric(table[2,2])/as.numeric(Total)*100,2),"%)")
  
  rownames(table)<-c("Referred by TCB15", "Not referred by TCB15")
  colnames(table)<-c("Referred by NICE", "Not referred by NICE", "McNemar's test (p-value)")
  return(table)
}

result_PNC0_fun<-function(data,variable1,variable2,category){
  
  data <- data %>%
    filter(jaundice_category_PNC_0 == category) %>%
    filter(!is.na(.data[[variable1]]) & !is.na(.data[[variable2]]))  # Remove NA rows in key variables
  
  if (nrow(data) == 0) {
    return(matrix("NA", nrow = 2, ncol = 3,
                  dimnames = list(c("Referred by TCB15", "Not referred by TCB15"),
                                  c("Referred by NICE", "Not referred by NICE", "McNemar's test (p-value)"))))
  }
  
  table<-matrix(0,2,3)
  
  mcnemar.pvalue <- tryCatch({
    p <- mcnemar.test(table(data[[variable1]], data[[variable2]]))$p.value
    if (p < 0.001) "<0.001" else round(p, 3)
  }, error = function(e) "NA")
  
  table[1,1]<-sum(data[,variable1]==1&data[,variable2]==1,na.rm=TRUE)
  table[1,2]<-sum(data[,variable1]==0&data[,variable2]==1,na.rm=TRUE)
  table[2,1]<-sum(data[,variable1]==1&data[,variable2]==0,na.rm=TRUE)
  table[2,2]<-sum(data[,variable1]==0&data[,variable2]==0,na.rm=TRUE)
  table[1,3]<-mcnemar.pvalue
  table[2,3]<-NA
  
  Total<-sum(as.numeric(table[1:2, 1:2]))
  
  table[1,1]<-paste0(as.numeric(table[1,1]),"(",round(as.numeric(table[1,1])/as.numeric(Total)*100,2),"%)")
  table[1,2]<-paste0(as.numeric(table[1,2]),"(",round(as.numeric(table[1,2])/as.numeric(Total)*100,2),"%)")
  table[2,1]<-paste0(as.numeric(table[2,1]),"(",round(as.numeric(table[2,1])/as.numeric(Total)*100,2),"%)")
  table[2,2]<-paste0(as.numeric(table[2,2]),"(",round(as.numeric(table[2,2])/as.numeric(Total)*100,2),"%)")
  
  
  rownames(table)<-c("Referred by TCB15", "Not referred by TCB15")
  colnames(table)<-c("Referred by NICE", "Not referred by NICE", "McNemar's test (p-value)")
  return(table)
}

result_PNC1_fun<-function(data,variable1,variable2,category){
  
  data <- data %>%
    filter(jaundice_category_PNC_1 == category) %>%
    filter(!is.na(.data[[variable1]]) & !is.na(.data[[variable2]]))  # Remove NA rows in key variables
  
  if (nrow(data) == 0) {
    return(matrix("NA", nrow = 2, ncol = 3,
                  dimnames = list(c("Referred by TCB15", "Not referred by TCB15"),
                                  c("Referred by NICE", "Not referred by NICE", "McNemar's test (p-value)"))))
  }
  
  table<-matrix(0,2,3)
  
  mcnemar.pvalue <- tryCatch({
    p <- mcnemar.test(table(data[[variable1]], data[[variable2]]))$p.value
    if (p < 0.001) "<0.001" else round(p, 3)
  }, error = function(e) "NA")
  
  table[1,1]<-sum(data[,variable1]==1&data[,variable2]==1,na.rm=TRUE)
  table[1,2]<-sum(data[,variable1]==0&data[,variable2]==1,na.rm=TRUE)
  table[2,1]<-sum(data[,variable1]==1&data[,variable2]==0,na.rm=TRUE)
  table[2,2]<-sum(data[,variable1]==0&data[,variable2]==0,na.rm=TRUE)
  table[1,3]<-mcnemar.pvalue
  table[2,3]<-NA
  
  Total<-sum(as.numeric(table[1:2, 1:2]))
  
  table[1,1]<-paste0(as.numeric(table[1,1]),"(",round(as.numeric(table[1,1])/as.numeric(Total)*100,2),"%)")
  table[1,2]<-paste0(as.numeric(table[1,2]),"(",round(as.numeric(table[1,2])/as.numeric(Total)*100,2),"%)")
  table[2,1]<-paste0(as.numeric(table[2,1]),"(",round(as.numeric(table[2,1])/as.numeric(Total)*100,2),"%)")
  table[2,2]<-paste0(as.numeric(table[2,2]),"(",round(as.numeric(table[2,2])/as.numeric(Total)*100,2),"%)")
  
  
  rownames(table)<-c("Referred by TCB15", "Not referred by TCB15")
  colnames(table)<-c("Referred by NICE", "Not referred by NICE", "McNemar's test (p-value)")
  return(table)
}

df_infant_tcb_Kenya<-df_infant_tcb%>%filter(SITE=="Kenya")
df_infant_tcb_Zambia<-df_infant_tcb%>%filter(SITE=="Zambia")
df_infant_tcb_Pakistan<-df_infant_tcb%>%filter(SITE=="Pakistan")
df_infant_tcb_Ghana<-df_infant_tcb%>%filter(SITE=="Ghana")
df_infant_tcb_India_CMC<-df_infant_tcb%>%filter(SITE=="India-CMC")
df_infant_tcb_India_SAS<-df_infant_tcb%>%filter(SITE=="India-SAS")

excel_fun<-function(data){
  #IPC
  result_IPC_severe<-result_IPC_fun(data,"TCB_threshold_birth_outcome_ind","TCB_15_birth_outcome_ind","Severe Jaundice")
  result_IPC_jaundice<-result_IPC_fun(data,"TCB_threshold_birth_outcome_ind","TCB_15_birth_outcome_ind","Jaundice")
  result_IPC_no<-result_IPC_fun(data,"TCB_threshold_birth_outcome_ind","TCB_15_birth_outcome_ind","No Jaundice")
  
  #PNC-0
  result_PNC0_severe<-result_PNC0_fun(data,"TCB_threshold_PNC_1_ind","TCB_15_PNC_1_ind","Severe Jaundice")
  result_PNC0_jaundice<-result_PNC0_fun(data,"TCB_threshold_PNC_1_ind","TCB_15_PNC_1_ind","Jaundice")
  result_PNC0_no<-result_PNC0_fun(data,"TCB_threshold_PNC_1_ind","TCB_15_PNC_1_ind","No Jaundice")
  
  #PNC-1
  result_PNC1_severe<-result_PNC1_fun(data,"TCB_threshold_PNC_2_ind","TCB_15_PNC_2_ind","Severe Jaundice")
  result_PNC1_jaundice<-result_PNC1_fun(data,"TCB_threshold_PNC_2_ind","TCB_15_PNC_2_ind","Jaundice")
  result_PNC1_no<-result_PNC1_fun(data,"TCB_threshold_PNC_2_ind","TCB_15_PNC_2_ind","No Jaundice")
  
  # Helper to write title and table
  write_data_with_title <- function(wb, sheet, title, table, col, row) {
    writeData(wb, sheet, x = title, startCol = col, startRow = row)                  # Title row
    writeData(wb, sheet, x = as.data.frame(table), startCol = col, startRow = row+1, rowNames = TRUE)  # Table below
  }
  
  # Create workbook
  wb <- createWorkbook()
  addWorksheet(wb, "Results")
  
  # Write all tables with titles using layout from your grid
  write_data_with_title(wb, "Results", "IPC - Severe Jaundice",       result_IPC_severe,   1,  1)
  write_data_with_title(wb, "Results", "IPC - Jaundice",     result_IPC_jaundice, 1,  6)
  write_data_with_title(wb, "Results", "IPC - No Jaundice",  result_IPC_no,       1,  11)
  
  write_data_with_title(wb, "Results", "PNC-0 - Severe Jaundice",     result_PNC0_severe,   6,  1)
  write_data_with_title(wb, "Results", "PNC-0 - Jaundice",   result_PNC0_jaundice, 6,  6)
  write_data_with_title(wb, "Results", "PNC-0 - No Jaundice",result_PNC0_no,       6,  11)
  
  write_data_with_title(wb, "Results", "PNC-1 - Severe Jaundice",     result_PNC1_severe,   12, 1)
  write_data_with_title(wb, "Results", "PNC-1 - Jaundice",   result_PNC1_jaundice, 11, 6)
  write_data_with_title(wb, "Results", "PNC-1 - No Jaundice",result_PNC1_no,       11, 11)
  
  return(wb)
}

# Save the workbook
###Pool
saveWorkbook(excel_fun(df_infant_tcb), "D:/Users/yipeng_wei/Documents/Output/2 by 2 McNemar tables/McNemar Pooled.xlsx", overwrite = TRUE)
###Kenya
saveWorkbook(excel_fun(df_infant_tcb_Kenya), "D:/Users/yipeng_wei/Documents/Output/2 by 2 McNemar tables/McNemar Kenya.xlsx", overwrite = TRUE)
###Pakistan
saveWorkbook(excel_fun(df_infant_tcb_Pakistan), "D:/Users/yipeng_wei/Documents/Output/2 by 2 McNemar tables/McNemar Pakistan.xlsx", overwrite = TRUE)
###Ghana
saveWorkbook(excel_fun(df_infant_tcb_Ghana), "D:/Users/yipeng_wei/Documents/Output/2 by 2 McNemar tables/McNemar Ghana.xlsx", overwrite = TRUE)
###Zambia
saveWorkbook(excel_fun(df_infant_tcb_Zambia), "D:/Users/yipeng_wei/Documents/Output/2 by 2 McNemar tables/McNemar Zambia.xlsx", overwrite = TRUE)
###India_CMC
saveWorkbook(excel_fun(df_infant_tcb_India_CMC), "D:/Users/yipeng_wei/Documents/Output/2 by 2 McNemar tables/McNemar India_CMC.xlsx", overwrite = TRUE)
###India_SAS
saveWorkbook(excel_fun(df_infant_tcb_India_SAS), "D:/Users/yipeng_wei/Documents/Output/2 by 2 McNemar tables/McNemar India_SAS.xlsx", overwrite = TRUE)