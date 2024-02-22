#Delivery time
delivery.time_1 <- paste(df_infant_tcb$M09_DELIV_DSSTDAT_INF1_6, df_infant_tcb$M09_DELIV_DSSTTIM_INF1_6)
delivery.time_2 <- paste(df_infant_tcb$M09_DELIV_DSSTDAT_INF2_6, df_infant_tcb$M09_DELIV_DSSTTIM_INF2_6)
delivery.time_3 <- paste(df_infant_tcb$M09_DELIV_DSSTDAT_INF3_6, df_infant_tcb$M09_DELIV_DSSTTIM_INF3_6)
delivery.time_4 <- paste(df_infant_tcb$M09_DELIV_DSSTDAT_INF4_6, df_infant_tcb$M09_DELIV_DSSTTIM_INF4_6)

delivery.time_1<-as.POSIXct(delivery.time_1, format = "%Y-%m-%d %H:%M")
delivery.time_2<-as.POSIXct(delivery.time_2, format = "%Y-%m-%d %H:%M")
delivery.time_3<-as.POSIXct(delivery.time_3, format = "%Y-%m-%d %H:%M")
delivery.time_4<-as.POSIXct(delivery.time_4, format = "%Y-%m-%d %H:%M")

df_infant_tcb$delivery.time<-coalesce(delivery.time_1,delivery.time_2,delivery.time_3,delivery.time_4)
df_infant_tcb$delivery.date<-coalesce(df_infant_tcb$M09_DELIV_DSSTDAT_INF1_6,df_infant_tcb$M09_DELIV_DSSTDAT_INF2_6,df_infant_tcb$M09_DELIV_DSSTDAT_INF3_6,df_infant_tcb$M09_DELIV_DSSTDAT_INF4_6)

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
  else if (GA>=40) {
    result<-TSB("P0",">= 40 weeks",Days,Hours)
  }
  else if (GA==39) {
    result<-TSB("P0","39 weeks",Days,Hours)
  }
  else if (GA==38) {
    result<-TSB("P0","38 weeks",Days,Hours)
  }
  else if (GA==37) {
    result<-TSB("P0","37 weeks",Days,Hours)
  }
  else if (GA==36) {
    result<-TSB("P0","36 weeks",Days,Hours)
  }
  else if (GA==35) {
    result<-TSB("P0","35 weeks",Days,Hours)
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

#TCB threshold based on AAP
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

#PNC-0
table1<-matrix(0,3,3)
table1[1,1]<-sum(df_infant_tcb$TCB_threshold_PNC_1_ind==1&df_infant_tcb$TCB_15_PNC_1_ind==1,na.rm=TRUE)
table1[1,2]<-sum(df_infant_tcb$TCB_threshold_PNC_1_ind==0&df_infant_tcb$TCB_15_PNC_1_ind==1,na.rm=TRUE)
table1[2,1]<-sum(df_infant_tcb$TCB_threshold_PNC_1_ind==1&df_infant_tcb$TCB_15_PNC_1_ind==0,na.rm=TRUE)
table1[2,2]<-sum(df_infant_tcb$TCB_threshold_PNC_1_ind==0&df_infant_tcb$TCB_15_PNC_1_ind==0,na.rm=TRUE)
table1[1,3]<-sum(table1[1,1],table1[1,2])
table1[2,3]<-sum(table1[2,1],table1[2,2])
table1[3,1]<-sum(table1[1,1],table1[2,1])
table1[3,2]<-sum(table1[1,2],table1[2,2])
table1[3,3]<-sum(table1[3,1],table1[3,2])
colnames(table1)<-c("TcB >= AAP threshold","TcB < AAP threshold", "Total")
rownames(table1)<-c("TcB >= 15","TcB < 15", "Total")

table2<-matrix(0,3,3)
table2[1,1]<-sum(df_infant_tcb$TCB_threshold_PNC_1_ind==1&df_infant_tcb$M13_JAUND_CEOCCUR_7==1,na.rm=TRUE)
table2[1,2]<-sum(df_infant_tcb$TCB_threshold_PNC_1_ind==0&df_infant_tcb$M13_JAUND_CEOCCUR_7==1,na.rm=TRUE)
table2[2,1]<-sum(df_infant_tcb$TCB_threshold_PNC_1_ind==1&df_infant_tcb$M13_JAUND_CEOCCUR_7==0,na.rm=TRUE)
table2[2,2]<-sum(df_infant_tcb$TCB_threshold_PNC_1_ind==0&df_infant_tcb$M13_JAUND_CEOCCUR_7==0,na.rm=TRUE)
table2[1,3]<-sum(table2[1,1],table2[1,2])
table2[2,3]<-sum(table2[2,1],table2[2,2])
table2[3,1]<-sum(table2[1,1],table2[2,1])
table2[3,2]<-sum(table2[1,2],table2[2,2])
table2[3,3]<-sum(table2[3,1],table2[3,2])
colnames(table2)<-c("TcB >= AAP threshold","TcB < AAP threshold", "Total")
rownames(table2)<-c("Jaundice present","Jaundice absent", "Total")


table3<-matrix(0,3,3)
table3[1,1]<-sum(df_infant_tcb$TCB_15_PNC_1_ind==1&df_infant_tcb$M13_JAUND_CEOCCUR_7==1,na.rm=TRUE)
table3[1,2]<-sum(df_infant_tcb$TCB_15_PNC_1_ind==0&df_infant_tcb$M13_JAUND_CEOCCUR_7==1,na.rm=TRUE)
table3[2,1]<-sum(df_infant_tcb$TCB_15_PNC_1_ind==1&df_infant_tcb$M13_JAUND_CEOCCUR_7==0,na.rm=TRUE)
table3[2,2]<-sum(df_infant_tcb$TCB_15_PNC_1_ind==0&df_infant_tcb$M13_JAUND_CEOCCUR_7==0,na.rm=TRUE)
table3[1,3]<-sum(table3[1,1],table3[1,2])
table3[2,3]<-sum(table3[2,1],table3[2,2])
table3[3,1]<-sum(table3[1,1],table3[2,1])
table3[3,2]<-sum(table3[1,2],table3[2,2])
table3[3,3]<-sum(table3[3,1],table3[3,2])
colnames(table3)<-c("TcB >= 15","TcB < 15", "Total")
rownames(table3)<-c("Jaundice present","Jaundice absent", "Total")

print(xtable(table1,digits=0,caption = "PNC-0 AAP threshold vs TcB $>$= 15"),table.placement="H")
print(xtable(table2,digits=0,caption = "PNC-0 AAP threshold vs Jaundice diagnosis"),table.placement="H")
print(xtable(table3,digits=0,caption = "PNC-0 TcB $>$= 15 vs Jaundice diagnosis"),table.placement="H")

#PNC-1
table1<-matrix(0,3,3)
table1[1,1]<-sum(df_infant_tcb$TCB_threshold_PNC_2_ind==1&df_infant_tcb$TCB_15_PNC_2_ind==1,na.rm=TRUE)
table1[1,2]<-sum(df_infant_tcb$TCB_threshold_PNC_2_ind==0&df_infant_tcb$TCB_15_PNC_2_ind==1,na.rm=TRUE)
table1[2,1]<-sum(df_infant_tcb$TCB_threshold_PNC_2_ind==1&df_infant_tcb$TCB_15_PNC_2_ind==0,na.rm=TRUE)
table1[2,2]<-sum(df_infant_tcb$TCB_threshold_PNC_2_ind==0&df_infant_tcb$TCB_15_PNC_2_ind==0,na.rm=TRUE)
table1[1,3]<-sum(table1[1,1],table1[1,2])
table1[2,3]<-sum(table1[2,1],table1[2,2])
table1[3,1]<-sum(table1[1,1],table1[2,1])
table1[3,2]<-sum(table1[1,2],table1[2,2])
table1[3,3]<-sum(table1[3,1],table1[3,2])
colnames(table1)<-c("TcB >= AAP threshold","TcB < AAP threshold", "Total")
rownames(table1)<-c("TcB >= 15","TcB < 15", "Total")

table2<-matrix(0,3,3)
table2[1,1]<-sum(df_infant_tcb$TCB_threshold_PNC_2_ind==1&df_infant_tcb$M13_JAUND_CEOCCUR_8==1,na.rm=TRUE)
table2[1,2]<-sum(df_infant_tcb$TCB_threshold_PNC_2_ind==0&df_infant_tcb$M13_JAUND_CEOCCUR_8==1,na.rm=TRUE)
table2[2,1]<-sum(df_infant_tcb$TCB_threshold_PNC_2_ind==1&df_infant_tcb$M13_JAUND_CEOCCUR_8==0,na.rm=TRUE)
table2[2,2]<-sum(df_infant_tcb$TCB_threshold_PNC_2_ind==0&df_infant_tcb$M13_JAUND_CEOCCUR_8==0,na.rm=TRUE)
table2[1,3]<-sum(table2[1,1],table2[1,2])
table2[2,3]<-sum(table2[2,1],table2[2,2])
table2[3,1]<-sum(table2[1,1],table2[2,1])
table2[3,2]<-sum(table2[1,2],table2[2,2])
table2[3,3]<-sum(table2[3,1],table2[3,2])
colnames(table2)<-c("TcB >= AAP threshold","TcB < AAP threshold", "Total")
rownames(table2)<-c("Jaundice present","Jaundice absent", "Total")

table3<-matrix(0,3,3)
table3[1,1]<-sum(df_infant_tcb$TCB_15_PNC_2_ind==1&df_infant_tcb$M13_JAUND_CEOCCUR_8==1,na.rm=TRUE)
table3[1,2]<-sum(df_infant_tcb$TCB_15_PNC_2_ind==0&df_infant_tcb$M13_JAUND_CEOCCUR_8==1,na.rm=TRUE)
table3[2,1]<-sum(df_infant_tcb$TCB_15_PNC_2_ind==1&df_infant_tcb$M13_JAUND_CEOCCUR_8==0,na.rm=TRUE)
table3[2,2]<-sum(df_infant_tcb$TCB_15_PNC_2_ind==0&df_infant_tcb$M13_JAUND_CEOCCUR_8==0,na.rm=TRUE)
table3[1,3]<-sum(table3[1,1],table3[1,2])
table3[2,3]<-sum(table3[2,1],table3[2,2])
table3[3,1]<-sum(table3[1,1],table3[2,1])
table3[3,2]<-sum(table3[1,2],table3[2,2])
table3[3,3]<-sum(table3[3,1],table3[3,2])
colnames(table3)<-c("TcB >= 15","TcB < 15", "Total")
rownames(table3)<-c("Jaundice present","Jaundice absent", "Total")

print(xtable(table1,digits=0,caption = "PNC-1 AAP threshold vs TcB $>$= 15"),table.placement="H")
print(xtable(table2,digits=0,caption = "PNC-1 AAP threshold vs Jaundice diagnosis"),table.placement="H")
print(xtable(table3,digits=0,caption = "PNC-1 TcB $>$= 15 vs Jaundice diagnosis"),table.placement="H")

#PNC-4
table1<-matrix(0,3,3)
table1[1,1]<-sum(df_infant_tcb$TCB_threshold_PNC_3_ind==1&df_infant_tcb$TCB_15_PNC_3_ind==1,na.rm=TRUE)
table1[1,2]<-sum(df_infant_tcb$TCB_threshold_PNC_3_ind==0&df_infant_tcb$TCB_15_PNC_3_ind==1,na.rm=TRUE)
table1[2,1]<-sum(df_infant_tcb$TCB_threshold_PNC_3_ind==1&df_infant_tcb$TCB_15_PNC_3_ind==0,na.rm=TRUE)
table1[2,2]<-sum(df_infant_tcb$TCB_threshold_PNC_3_ind==0&df_infant_tcb$TCB_15_PNC_3_ind==0,na.rm=TRUE)
table1[1,3]<-sum(table1[1,1],table1[1,2])
table1[2,3]<-sum(table1[2,1],table1[2,2])
table1[3,1]<-sum(table1[1,1],table1[2,1])
table1[3,2]<-sum(table1[1,2],table1[2,2])
table1[3,3]<-sum(table1[3,1],table1[3,2])
colnames(table1)<-c("TcB >= AAP threshold","TcB < AAP threshold", "Total")
rownames(table1)<-c("TcB >= 15","TcB < 15", "Total")

table2<-matrix(0,3,3)
table2[1,1]<-sum(df_infant_tcb$TCB_threshold_PNC_3_ind==1&df_infant_tcb$M13_JAUND_CEOCCUR_9==1,na.rm=TRUE)
table2[1,2]<-sum(df_infant_tcb$TCB_threshold_PNC_3_ind==0&df_infant_tcb$M13_JAUND_CEOCCUR_9==1,na.rm=TRUE)
table2[2,1]<-sum(df_infant_tcb$TCB_threshold_PNC_3_ind==1&df_infant_tcb$M13_JAUND_CEOCCUR_9==0,na.rm=TRUE)
table2[2,2]<-sum(df_infant_tcb$TCB_threshold_PNC_3_ind==0&df_infant_tcb$M13_JAUND_CEOCCUR_9==0,na.rm=TRUE)
table2[1,3]<-sum(table2[1,1],table2[1,2])
table2[2,3]<-sum(table2[2,1],table2[2,2])
table2[3,1]<-sum(table2[1,1],table2[2,1])
table2[3,2]<-sum(table2[1,2],table2[2,2])
table2[3,3]<-sum(table2[3,1],table2[3,2])
colnames(table2)<-c("TcB >= AAP threshold","TcB < AAP threshold", "Total")
rownames(table2)<-c("Jaundice present","Jaundice absent", "Total")

table3<-matrix(0,3,3)
table3[1,1]<-sum(df_infant_tcb$TCB_15_PNC_3_ind==1&df_infant_tcb$M13_JAUND_CEOCCUR_9==1,na.rm=TRUE)
table3[1,2]<-sum(df_infant_tcb$TCB_15_PNC_3_ind==0&df_infant_tcb$M13_JAUND_CEOCCUR_9==1,na.rm=TRUE)
table3[2,1]<-sum(df_infant_tcb$TCB_15_PNC_3_ind==1&df_infant_tcb$M13_JAUND_CEOCCUR_9==0,na.rm=TRUE)
table3[2,2]<-sum(df_infant_tcb$TCB_15_PNC_3_ind==0&df_infant_tcb$M13_JAUND_CEOCCUR_9==0,na.rm=TRUE)
table3[1,3]<-sum(table3[1,1],table3[1,2])
table3[2,3]<-sum(table3[2,1],table3[2,2])
table3[3,1]<-sum(table3[1,1],table3[2,1])
table3[3,2]<-sum(table3[1,2],table3[2,2])
table3[3,3]<-sum(table3[3,1],table3[3,2])
colnames(table3)<-c("TcB >= 15","TcB < 15", "Total")
rownames(table3)<-c("Jaundice present","Jaundice absent", "Total")

print(xtable(table1,digits=0,caption = "PNC-4 AAP threshold vs TcB $>$= 15"),table.placement="H")
print(xtable(table2,digits=0,caption = "PNC-4 AAP threshold vs Jaundice diagnosis"),table.placement="H")
print(xtable(table3,digits=0,caption = "PNC-4 TcB $>$= 15 vs Jaundice diagnosis"),table.placement="H")

#Birth outcome
table1<-matrix(0,3,3)
table1[1,1]<-sum(df_infant_tcb$TCB_threshold_birth_outcome_ind==1&df_infant_tcb$TCB_15_birth_outcome_ind==1,na.rm=TRUE)
table1[1,2]<-sum(df_infant_tcb$TCB_threshold_birth_outcome_ind==0&df_infant_tcb$TCB_15_birth_outcome_ind==1,na.rm=TRUE)
table1[2,1]<-sum(df_infant_tcb$TCB_threshold_birth_outcome_ind==1&df_infant_tcb$TCB_15_birth_outcome_ind==0,na.rm=TRUE)
table1[2,2]<-sum(df_infant_tcb$TCB_threshold_birth_outcome_ind==0&df_infant_tcb$TCB_15_birth_outcome_ind==0,na.rm=TRUE)
table1[1,3]<-sum(table1[1,1],table1[1,2])
table1[2,3]<-sum(table1[2,1],table1[2,2])
table1[3,1]<-sum(table1[1,1],table1[2,1])
table1[3,2]<-sum(table1[1,2],table1[2,2])
table1[3,3]<-sum(table1[3,1],table1[3,2])
colnames(table1)<-c("TcB >= AAP threshold","TcB < AAP threshold", "Total")
rownames(table1)<-c("TcB >= 15","TcB < 15", "Total")

table2<-matrix(0,3,3)
table2[1,1]<-sum(df_infant_tcb$TCB_threshold_birth_outcome_ind==1&df_infant_tcb$M11_JAUND_CEOCCUR_6==1,na.rm=TRUE)
table2[1,2]<-sum(df_infant_tcb$TCB_threshold_birth_outcome_ind==0&df_infant_tcb$M11_JAUND_CEOCCUR_6==1,na.rm=TRUE)
table2[2,1]<-sum(df_infant_tcb$TCB_threshold_birth_outcome_ind==1&df_infant_tcb$M11_JAUND_CEOCCUR_6==0,na.rm=TRUE)
table2[2,2]<-sum(df_infant_tcb$TCB_threshold_birth_outcome_ind==0&df_infant_tcb$M11_JAUND_CEOCCUR_6==0,na.rm=TRUE)
table2[1,3]<-sum(table2[1,1],table2[1,2])
table2[2,3]<-sum(table2[2,1],table2[2,2])
table2[3,1]<-sum(table2[1,1],table2[2,1])
table2[3,2]<-sum(table2[1,2],table2[2,2])
table2[3,3]<-sum(table2[3,1],table2[3,2])
colnames(table2)<-c("TcB >= AAP threshold","TcB < AAP threshold", "Total")
rownames(table2)<-c("Jaundice present","Jaundice absent", "Total")

table3<-matrix(0,3,3)
table3[1,1]<-sum(df_infant_tcb$TCB_15_birth_outcome_ind==1&df_infant_tcb$M11_JAUND_CEOCCUR_6==1,na.rm=TRUE)
table3[1,2]<-sum(df_infant_tcb$TCB_15_birth_outcome_ind==0&df_infant_tcb$M11_JAUND_CEOCCUR_6==1,na.rm=TRUE)
table3[2,1]<-sum(df_infant_tcb$TCB_15_birth_outcome_ind==1&df_infant_tcb$M11_JAUND_CEOCCUR_6==0,na.rm=TRUE)
table3[2,2]<-sum(df_infant_tcb$TCB_15_birth_outcome_ind==0&df_infant_tcb$M11_JAUND_CEOCCUR_6==0,na.rm=TRUE)
table3[1,3]<-sum(table3[1,1],table3[1,2])
table3[2,3]<-sum(table3[2,1],table3[2,2])
table3[3,1]<-sum(table3[1,1],table3[2,1])
table3[3,2]<-sum(table3[1,2],table3[2,2])
table3[3,3]<-sum(table3[3,1],table3[3,2])
colnames(table3)<-c("TcB >= 15","TcB < 15", "Total")
rownames(table3)<-c("Jaundice present","Jaundice absent", "Total")

print(xtable(table1,digits=0,caption = "Birth outcome AAP threshold vs TcB $>$= 15"),table.placement="H")
print(xtable(table2,digits=0,caption = "Birth outcome AAP threshold vs Jaundice diagnosis"),table.placement="H")
print(xtable(table3,digits=0,caption = "Birth outcome TcB $>$= 15 vs Jaundice diagnosis"),table.placement="H")

df_infant_tcb_Zambia<-df_infant_tcb %>% filter(SITE=="Zambia")
df_infant_tcb_Ghana<-df_infant_tcb %>% filter(SITE=="Ghana")
df_infant_tcb_Kenya<-df_infant_tcb %>% filter(SITE=="Kenya")
df_infant_tcb_Pakistan<-df_infant_tcb %>% filter(SITE=="Pakistan")

###Percentage table
#Birth
paste0(sum(na.omit(df_infant_tcb_Zambia$M11_JAUND_CEOCCUR_6)==1),"(",round(sum(na.omit(df_infant_tcb_Zambia$M11_JAUND_CEOCCUR_6)==1)/length(na.omit(df_infant_tcb_Zambia$M11_JAUND_CEOCCUR_6))*100,2),"%",")")
paste0(sum(na.omit(df_infant_tcb_Zambia$TCB_15_birth_outcome_ind)==1),"(",round(sum(na.omit(df_infant_tcb_Zambia$TCB_15_birth_outcome_ind)==1)/length(na.omit(df_infant_tcb_Zambia$TCB_15_birth_outcome_ind))*100,2),"%",")")
paste0(sum(na.omit(df_infant_tcb_Zambia$TCB_threshold_birth_outcome_ind)==1),"(",round(sum(na.omit(df_infant_tcb_Zambia$TCB_threshold_birth_outcome_ind)==1)/length(na.omit(df_infant_tcb_Zambia$TCB_threshold_birth_outcome_ind))*100,2),"%",")")

#PNC-0
paste0(sum(na.omit(df_infant_tcb_Zambia$M13_JAUND_CEOCCUR_7)==1),"(",round(sum(na.omit(df_infant_tcb_Zambia$M13_JAUND_CEOCCUR_7)==1)/length(na.omit(df_infant_tcb_Zambia$M13_JAUND_CEOCCUR_7))*100,2),"%",")")
paste0(sum(na.omit(df_infant_tcb_Zambia$TCB_15_PNC_1_ind)==1),"(",round(sum(na.omit(df_infant_tcb_Zambia$TCB_15_PNC_1_ind)==1)/length(na.omit(df_infant_tcb_Zambia$TCB_15_PNC_1_ind))*100,2),"%",")")
paste0(sum(na.omit(df_infant_tcb_Zambia$TCB_threshold_PNC_1_ind)==1),"(",round(sum(na.omit(df_infant_tcb_Zambia$TCB_threshold_PNC_1_ind)==1)/length(na.omit(df_infant_tcb_Zambia$TCB_threshold_PNC_1_ind))*100,2),"%",")")

#PNC-1
paste0(sum(na.omit(df_infant_tcb_Zambia$M13_JAUND_CEOCCUR_8)==1),"(",round(sum(na.omit(df_infant_tcb_Zambia$M13_JAUND_CEOCCUR_8)==1)/length(na.omit(df_infant_tcb_Zambia$M13_JAUND_CEOCCUR_8))*100,2),"%",")")
paste0(sum(na.omit(df_infant_tcb_Zambia$TCB_15_PNC_2_ind)==1),"(",round(sum(na.omit(df_infant_tcb_Zambia$TCB_15_PNC_2_ind)==1)/length(na.omit(df_infant_tcb_Zambia$TCB_15_PNC_2_ind))*100,2),"%",")")
paste0(sum(na.omit(df_infant_tcb_Zambia$TCB_threshold_PNC_2_ind)==1),"(",round(sum(na.omit(df_infant_tcb_Zambia$TCB_threshold_PNC_2_ind)==1)/length(na.omit(df_infant_tcb_Zambia$TCB_threshold_PNC_2_ind))*100,2),"%",")")

###Missing Percentage table
#Birth
paste0(length(na.omit(df_infant_tcb$M11_JAUND_CEOCCUR_6)),"(",round(length(na.omit(df_infant_tcb$M11_JAUND_CEOCCUR_6))/nrow(df_infant_tcb)*100,2),"%",")")
paste0(length(na.omit(df_infant_tcb$TCB_15_birth_outcome_ind)),"(",round(length(na.omit(df_infant_tcb$TCB_15_birth_outcome_ind))/nrow(df_infant_tcb)*100,2),"%",")")
paste0(length(na.omit(df_infant_tcb$TCB_threshold_birth_outcome_ind)),"(",round(length(na.omit(df_infant_tcb$TCB_threshold_birth_outcome_ind))/nrow(df_infant_tcb)*100,2),"%",")")

#PNC-0
paste0(length(na.omit(df_infant_tcb$M13_JAUND_CEOCCUR_7)),"(",round(length(na.omit(df_infant_tcb$M13_JAUND_CEOCCUR_7))/nrow(df_infant_tcb)*100,2),"%",")")
paste0(length(na.omit(df_infant_tcb$TCB_15_PNC_1_ind)),"(",round(length(na.omit(df_infant_tcb$TCB_15_PNC_1_ind))/nrow(df_infant_tcb)*100,2),"%",")")
paste0(length(na.omit(df_infant_tcb$TCB_threshold_PNC_1_ind)),"(",round(length(na.omit(df_infant_tcb$TCB_threshold_PNC_1_ind))/nrow(df_infant_tcb)*100,2),"%",")")

#PNC-1
paste0(length(na.omit(df_infant_tcb$M13_JAUND_CEOCCUR_8)),"(",round(length(na.omit(df_infant_tcb$M13_JAUND_CEOCCUR_8))/nrow(df_infant_tcb)*100,2),"%",")")
paste0(length(na.omit(df_infant_tcb$TCB_15_PNC_2_ind)),"(",round(length(na.omit(df_infant_tcb$TCB_15_PNC_2_ind))/nrow(df_infant_tcb)*100,2),"%",")")
paste0(length(na.omit(df_infant_tcb$TCB_threshold_PNC_2_ind)),"(",round(length(na.omit(df_infant_tcb$TCB_threshold_PNC_2_ind))/nrow(df_infant_tcb)*100,2),"%",")")

