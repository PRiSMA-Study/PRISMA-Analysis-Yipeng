library(openxlsx)

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

result_fun<-function(data,variable1,variable2,condition11,condition12,condition21,condition22){
  table<-matrix(0,3,3)
  table[1,1]<-sum(data[,variable1]==1&data[,variable2]==1,na.rm=TRUE)
  table[1,2]<-sum(data[,variable1]==0&data[,variable2]==1,na.rm=TRUE)
  table[2,1]<-sum(data[,variable1]==1&data[,variable2]==0,na.rm=TRUE)
  table[2,2]<-sum(data[,variable1]==0&data[,variable2]==0,na.rm=TRUE)
  table[1,3]<-sum(table[1,1],table[1,2])
  table[2,3]<-sum(table[2,1],table[2,2])
  table[3,1]<-sum(table[1,1],table[2,1])
  table[3,2]<-sum(table[1,2],table[2,2])
  table[3,3]<-sum(table[3,1],table[3,2])
  
  table[3,1]<-paste0(as.numeric(table[3,1]),"(",round(as.numeric(table[1,1])/as.numeric(table[3,1])*100,2),"%)")
  table[3,2]<-paste0(as.numeric(table[3,2]),"(",round(as.numeric(table[2,2])/as.numeric(table[3,2])*100,2),"%)")
  
  table[1,3]<-paste0(as.numeric(table[1,3]),"(",round(as.numeric(table[1,1])/as.numeric(table[1,3])*100,2),"%)")
  table[2,3]<-paste0(as.numeric(table[2,3]),"(",round(as.numeric(table[2,2])/as.numeric(table[2,3])*100,2),"%)")
  
  table[1,1]<-paste0(as.numeric(table[1,1]),"(",round(as.numeric(table[1,1])/as.numeric(table[3,3])*100,2),"%)")
  table[1,2]<-paste0(as.numeric(table[1,2]),"(",round(as.numeric(table[1,2])/as.numeric(table[3,3])*100,2),"%)")
  table[2,1]<-paste0(as.numeric(table[2,1]),"(",round(as.numeric(table[2,1])/as.numeric(table[3,3])*100,2),"%)")
  table[2,2]<-paste0(as.numeric(table[2,2]),"(",round(as.numeric(table[2,2])/as.numeric(table[3,3])*100,2),"%)")
  
  rownames(table)<-c(condition11,condition12, "Total (sensitivity / specificity)")
  colnames(table)<-c(condition21,condition22, "Total (ppv/npv)")
  
  return(table)
}

df_infant_tcb_Kenya<-df_infant_tcb%>%filter(SITE=="Kenya")
df_infant_tcb_Zambia<-df_infant_tcb%>%filter(SITE=="Zambia")
df_infant_tcb_Pakistan<-df_infant_tcb%>%filter(SITE=="Pakistan")
df_infant_tcb_Ghana<-df_infant_tcb%>%filter(SITE=="Ghana")
df_infant_tcb_India_CMC<-df_infant_tcb%>%filter(SITE=="India-CMC")
df_infant_tcb_India_SAS<-df_infant_tcb%>%filter(SITE=="India-SAS")

excel_fun<-function(data){
  #Birth outcome
  result_15_AAP_birth<-result_fun(data,"TCB_threshold_birth_outcome_ind","TCB_15_birth_outcome_ind","TcB >= 15","TcB < 15","TcB >= AAP threshold","TcB < AAP threshold")
  result_Jaund_AAP_birth<-result_fun(data,"TCB_threshold_birth_outcome_ind","M11_JAUND_CEOCCUR_6","Jaundice present","Jaundice absent","TcB >= AAP threshold","TcB < AAP threshold")
  result_Jaund_15_birth<-result_fun(data,"TCB_15_birth_outcome_ind","M11_JAUND_CEOCCUR_6","Jaundice present","Jaundice absent","TcB >= 15","TcB < 15")
  result_birth<-cbind(result_15_AAP_birth,result_Jaund_AAP_birth,result_Jaund_15_birth)
  #PNC-0
  result_15_AAP_0<-result_fun(data,"TCB_threshold_PNC_1_ind","TCB_15_PNC_1_ind","TcB >= 15","TcB < 15","TcB >= AAP threshold","TcB < AAP threshold")
  result_Jaund_AAP_0<-result_fun(data,"TCB_threshold_PNC_1_ind","M13_JAUND_CEOCCUR_7","Jaundice present","Jaundice absent","TcB >= AAP threshold","TcB < AAP threshold")
  result_Jaund_15_0<-result_fun(data,"TCB_15_PNC_1_ind","M13_JAUND_CEOCCUR_7","Jaundice present","Jaundice absent","TcB >= 15","TcB < 15")
  
  #PNC-1
  result_15_AAP_1<-result_fun(data,"TCB_threshold_PNC_2_ind","TCB_15_PNC_2_ind","TcB >= 15","TcB < 15","TcB >= AAP threshold","TcB < AAP threshold")
  result_Jaund_AAP_1<-result_fun(data,"TCB_threshold_PNC_2_ind","M13_JAUND_CEOCCUR_8","Jaundice present","Jaundice absent","TcB >= AAP threshold","TcB < AAP threshold")
  result_Jaund_15_1<-result_fun(data,"TCB_15_PNC_2_ind","M13_JAUND_CEOCCUR_8","Jaundice present","Jaundice absent","TcB >= 15","TcB < 15")
  
  #PNC-4
  result_15_AAP_4<-result_fun(data,"TCB_threshold_PNC_3_ind","TCB_15_PNC_3_ind","TcB >= 15","TcB < 15","TcB >= AAP threshold","TcB < AAP threshold")
  result_Jaund_AAP_4<-result_fun(data,"TCB_threshold_PNC_3_ind","M13_JAUND_CEOCCUR_9","Jaundice present","Jaundice absent","TcB >= AAP threshold","TcB < AAP threshold")
  result_Jaund_15_4<-result_fun(data,"TCB_15_PNC_3_ind","M13_JAUND_CEOCCUR_9","Jaundice present","Jaundice absent","TcB >= 15","TcB < 15")
  
  # Helper function to check and write data
  write_data_check <- function(wb, sheet, data, startCol, startRow) {
    # Convert matrix to data frame if necessary
    data <- as.data.frame(data)
    writeData(wb, sheet, data, startCol = startCol, startRow = startRow, rowNames = TRUE)
  }
  
  # Create a workbook and add a worksheet
  wb <- createWorkbook()
  addWorksheet(wb, "Results")
  
  # Write results to the worksheet with checks and appropriate spacing
  write_data_check(wb, "Results", result_15_AAP_birth, startCol = 1, startRow = 1)
  write_data_check(wb, "Results", result_Jaund_AAP_birth, startCol = 1, startRow = 6)
  write_data_check(wb, "Results", result_Jaund_15_birth, startCol = 1, startRow = 11)
  
  write_data_check(wb, "Results", result_15_AAP_0, startCol = 6, startRow = 1)
  write_data_check(wb, "Results", result_Jaund_AAP_0, startCol = 6, startRow = 6)
  write_data_check(wb, "Results", result_Jaund_15_0, startCol = 6, startRow = 11)
  
  write_data_check(wb, "Results", result_15_AAP_1, startCol = 11, startRow = 1)
  write_data_check(wb, "Results", result_Jaund_AAP_1, startCol = 11, startRow = 6)
  write_data_check(wb, "Results", result_Jaund_15_1, startCol = 11, startRow = 11)
  
  write_data_check(wb, "Results", result_15_AAP_4, startCol = 16, startRow = 1)
  write_data_check(wb, "Results", result_Jaund_AAP_4, startCol = 16, startRow = 6)
  write_data_check(wb, "Results", result_Jaund_15_4, startCol = 16, startRow = 11)
  
  return(wb)
}

# Save the workbook
###Pool
saveWorkbook(excel_fun(df_infant_tcb), "D:/Users/yipeng_wei/Documents/Output/TCB vs IMCI contingency tables/TCB vs IMCI Pooled.xlsx", overwrite = TRUE)
###Kenya
saveWorkbook(excel_fun(df_infant_tcb_Kenya), "D:/Users/yipeng_wei/Documents/Output/TCB vs IMCI contingency tables/TCB vs IMCI Kenya.xlsx", overwrite = TRUE)
###Pakistan
saveWorkbook(excel_fun(df_infant_tcb_Pakistan), "D:/Users/yipeng_wei/Documents/Output/TCB vs IMCI contingency tables/TCB vs IMCI Pakistan.xlsx", overwrite = TRUE)
###Ghana
saveWorkbook(excel_fun(df_infant_tcb_Ghana), "D:/Users/yipeng_wei/Documents/Output/TCB vs IMCI contingency tables/TCB vs IMCI Ghana.xlsx", overwrite = TRUE)
###Zambia
saveWorkbook(excel_fun(df_infant_tcb_Zambia), "D:/Users/yipeng_wei/Documents/Output/TCB vs IMCI contingency tables/TCB vs IMCI Zambia.xlsx", overwrite = TRUE)
###India_CMC
saveWorkbook(excel_fun(df_infant_tcb_India_CMC), "D:/Users/yipeng_wei/Documents/Output/TCB vs IMCI contingency tables/TCB vs IMCI India_CMC.xlsx", overwrite = TRUE)
###India_SAS
saveWorkbook(excel_fun(df_infant_tcb_India_SAS), "D:/Users/yipeng_wei/Documents/Output/TCB vs IMCI contingency tables/TCB vs IMCI India_SAS.xlsx", overwrite = TRUE)