library(ggplot2)
library(TSB.NICE)
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
df_infant_tcb$PNC_severe_jaund_ind1<-ifelse((df_infant_tcb$M13_JAUND_CESTTIM_7==1 & df_infant_tcb$M13_JAUND_CEOCCUR_7==1)|df_infant_tcb$M13_YELL_CEOCCUR_7==1,1,0)
df_infant_tcb$PNC_severe_jaund_ind2<-ifelse((df_infant_tcb$M13_JAUND_CESTTIM_8==1 & df_infant_tcb$M13_JAUND_CEOCCUR_8==1)|df_infant_tcb$M13_YELL_CEOCCUR_8==1,1,0)
df_infant_tcb$PNC_severe_jaund_ind3<-ifelse((df_infant_tcb$M13_JAUND_CESTTIM_9==1 & df_infant_tcb$M13_JAUND_CEOCCUR_9==1)|df_infant_tcb$M13_YELL_CEOCCUR_9==1,1,0)
df_infant_tcb$birth_outcome_severe_jaund_ind<-ifelse((df_infant_tcb$M11_BILIRUBIN_LBPERF_6==1 & df_infant_tcb$M11_JAUND_CEOCCUR_6==1)|df_infant_tcb$M11_YELLOW_CEOCCUR_6==1,1,0)

#df_infant_tcb$PNC_jaund_ind1<-ifelse(df_infant_tcb$M13_JAUND_CESTTIM_7==2 & df_infant_tcb$M13_YELL_CEOCCUR_7==0 & df_infant_tcb$M13_JAUND_CEOCCUR_7==1,1,0)
#df_infant_tcb$PNC_jaund_ind2<-ifelse(df_infant_tcb$M13_JAUND_CESTTIM_8==2 & df_infant_tcb$M13_YELL_CEOCCUR_8==0 & df_infant_tcb$M13_JAUND_CEOCCUR_8==1,1,0)
#df_infant_tcb$PNC_jaund_ind3<-ifelse(df_infant_tcb$M13_JAUND_CESTTIM_9==2 & df_infant_tcb$M13_YELL_CEOCCUR_9==0 & df_infant_tcb$M13_JAUND_CEOCCUR_9==1,1,0)

df_infant_tcb$PNC_jaund_ind1<-ifelse(df_infant_tcb$PNC_severe_jaund_ind1!=1 & df_infant_tcb$M13_JAUND_CEOCCUR_7==1,1,0)
df_infant_tcb$PNC_jaund_ind2<-ifelse(df_infant_tcb$PNC_severe_jaund_ind2!=1 & df_infant_tcb$M13_JAUND_CEOCCUR_8==1,1,0)
df_infant_tcb$PNC_jaund_ind3<-ifelse(df_infant_tcb$PNC_severe_jaund_ind3!=1 & df_infant_tcb$M13_JAUND_CEOCCUR_9==1,1,0)
df_infant_tcb$birth_outcome_jaund_ind<-ifelse(df_infant_tcb$birth_outcome_severe_jaund_ind!=1 & df_infant_tcb$M11_JAUND_CEOCCUR_6==1,1,0)

#PNC-0
TCB_group_PNC_1<-NULL
for (i in 1:n.obs) {
  if (is.na(df_infant_tcb$TCB_threshold_PNC_1_ind[i]) | is.na(df_infant_tcb$TCB_15_PNC_1_ind[i])) {
    TCB_group_PNC_1[i]<-NA
  }
  else if(df_infant_tcb$TCB_threshold_PNC_1_ind[i]==1 & df_infant_tcb$TCB_15_PNC_1_ind[i]==1) {
    TCB_group_PNC_1[i]<-1
  }
  else if (df_infant_tcb$TCB_threshold_PNC_1_ind[i]==0 & df_infant_tcb$TCB_15_PNC_1_ind[i]==1) {
    TCB_group_PNC_1[i]<-2
  }
  else if (df_infant_tcb$TCB_threshold_PNC_1_ind[i]==1 & df_infant_tcb$TCB_15_PNC_1_ind[i]==0) {
    TCB_group_PNC_1[i]<-3
  }
  else if (df_infant_tcb$TCB_threshold_PNC_1_ind[i]==0 & df_infant_tcb$TCB_15_PNC_1_ind[i]==0) {
    TCB_group_PNC_1[i]<-4
  }
}

df_infant_tcb$TCB_group_PNC_1<-as.character(TCB_group_PNC_1)

#remove missing observation
jaundice_category_PNC_1<-NULL
jaundice_category_PNC_1<-ifelse(df_infant_tcb$M13_JAUND_CEOCCUR_7==0,"No Jaundice",NA)
jaundice_category_PNC_1<-ifelse(df_infant_tcb$PNC_jaund_ind1==1,"Jaundice",jaundice_category_PNC_1)
jaundice_category_PNC_1<-ifelse(df_infant_tcb$PNC_severe_jaund_ind1==1,"Severe Jaundice",jaundice_category_PNC_1)

#data cleaning
df_infant_tcb$jaundice_category_PNC_1<-jaundice_category_PNC_1
df_infant_tcb_jaund_cat_PNC_1<-df_infant_tcb[!is.na(df_infant_tcb$jaundice_category_PNC_1),]
df_infant_tcb_jaund_cat_PNC_1<-df_infant_tcb_jaund_cat_PNC_1[!is.na(df_infant_tcb_jaund_cat_PNC_1$TCB_group_PNC_1),]

#data cleaning, The TcB measurement >30 might miss the decimal point and could be a typo
df_infant_tcb_jaund_cat_PNC_1[which(df_infant_tcb_jaund_cat_PNC_1$M14_TCB_UMOLL_LBORRES_7>30),]$M14_TCB_UMOLL_LBORRES_7<-NA

df_infant_tcb_jaund_cat_PNC_1$jaundice_category_PNC_1<-factor(df_infant_tcb_jaund_cat_PNC_1$jaundice_category_PNC_1, levels = c("Severe Jaundice", "Jaundice", "No Jaundice"))

df_infant_tcb_jaund_cat_PNC_1 %>% 
  ggplot(aes(x = M14_TCB_UMOLL_LBORRES_7, y = PNC_age_days_exact1, color = TCB_group_PNC_1))+
  geom_point(alpha = 1, size = 1.5)+
  geom_hline(aes(yintercept = 1), color = "purple1", alpha=0.6, linetype="F1") +
  geom_hline(aes(yintercept = 7), color = "purple2", alpha=0.6, linetype="F1") + 
  geom_vline(aes(xintercept = 15), color = "orange1", alpha=0.6, linetype="F1") +
  facet_grid(rows = vars(jaundice_category_PNC_1))+
  xlab("TCB level (mg/dl)")+
  ylab("Age days")+
  theme_bw() +
  theme(strip.background=element_rect(fill="white")) + 
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1),
        legend.position="bottom",
        legend.title = element_blank(),
        legend.text=element_text(size=12),
        strip.text = element_text(size=14),   # Increase facet label size
        axis.title.x = element_text(size=14),  # Increase x-axis label size
        axis.title.y = element_text(size=14),  # Increase y-axis label size
        panel.grid.minor = element_blank())+
  scale_x_continuous(breaks=seq(0,20))+
  scale_y_continuous(breaks = c(1, 4, 7), limits = c(0, 7))+
  scale_color_manual(labels=c("1"="Referred by both","2"="Referred by TCB15 only","3"="Referred by NICE only","4"="Not referred"),values = c("1"="green","2"="blue","3"="red", "4"="black"),guide = guide_legend(override.aes = list(size=3)))+
  ggtitle("Pooled data: PNC-0")

#PNC-1
TCB_group_PNC_2<-NULL
for (i in 1:n.obs) {
  if (is.na(df_infant_tcb$TCB_threshold_PNC_2_ind[i]) | is.na(df_infant_tcb$TCB_15_PNC_2_ind[i])) {
    TCB_group_PNC_2[i]<-NA
  }
  else if(df_infant_tcb$TCB_threshold_PNC_2_ind[i]==1 & df_infant_tcb$TCB_15_PNC_2_ind[i]==1) {
    TCB_group_PNC_2[i]<-1
  }
  else if (df_infant_tcb$TCB_threshold_PNC_2_ind[i]==0 & df_infant_tcb$TCB_15_PNC_2_ind[i]==1) {
    TCB_group_PNC_2[i]<-2
  }
  else if (df_infant_tcb$TCB_threshold_PNC_2_ind[i]==1 & df_infant_tcb$TCB_15_PNC_2_ind[i]==0) {
    TCB_group_PNC_2[i]<-3
  }
  else if (df_infant_tcb$TCB_threshold_PNC_2_ind[i]==0 & df_infant_tcb$TCB_15_PNC_2_ind[i]==0) {
    TCB_group_PNC_2[i]<-4
  }
}

df_infant_tcb$TCB_group_PNC_2<-as.character(TCB_group_PNC_2)

#remove missing observation
jaundice_category_PNC_2<-NULL
jaundice_category_PNC_2<-ifelse(df_infant_tcb$M13_JAUND_CEOCCUR_8==0,"No Jaundice",NA)
jaundice_category_PNC_2<-ifelse(df_infant_tcb$PNC_jaund_ind2==1,"Jaundice",jaundice_category_PNC_2)
jaundice_category_PNC_2<-ifelse(df_infant_tcb$PNC_severe_jaund_ind2==1,"Severe Jaundice",jaundice_category_PNC_2)

#data cleaning
df_infant_tcb$jaundice_category_PNC_2<-jaundice_category_PNC_2
df_infant_tcb_jaund_cat_PNC_2<-df_infant_tcb[!is.na(df_infant_tcb$jaundice_category_PNC_2),]
df_infant_tcb_jaund_cat_PNC_2<-df_infant_tcb_jaund_cat_PNC_2[!is.na(df_infant_tcb_jaund_cat_PNC_2$TCB_group_PNC_2),]

#data cleaning, The TcB measurement >30 might miss the decimal point and could be a typo
df_infant_tcb_jaund_cat_PNC_2[which(df_infant_tcb_jaund_cat_PNC_2$M14_TCB_UMOLL_LBORRES_8>30),]$M14_TCB_UMOLL_LBORRES_8<-NA

df_infant_tcb_jaund_cat_PNC_2$jaundice_category_PNC_2<-factor(df_infant_tcb_jaund_cat_PNC_2$jaundice_category_PNC_2, levels = c("Severe Jaundice", "Jaundice", "No Jaundice"))

df_infant_tcb_jaund_cat_PNC_2 %>% 
  ggplot(aes(x = M14_TCB_UMOLL_LBORRES_8, y = PNC_age_days_exact2, color = TCB_group_PNC_2))+
  geom_point(alpha = 1, size = 1.5)+
  geom_hline(aes(yintercept = 7), color = "purple2", alpha=0.6, linetype="F1") +
  geom_hline(aes(yintercept = 14), color = "purple3", alpha=0.6, linetype="F1") +
  geom_vline(aes(xintercept = 15), color = "orange1", alpha=0.6, linetype="F1") +
  facet_grid(rows = vars(jaundice_category_PNC_2))+
  xlab("TCB level (mg/dl)")+
  ylab("Age days")+
  theme_bw() +
  theme(strip.background=element_rect(fill="white")) + 
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1),
        legend.position="bottom",
        legend.title = element_blank(),
        legend.text=element_text(size=12),
        strip.text = element_text(size=14),   # Increase facet label size
        axis.title.x = element_text(size=14),  # Increase x-axis label size
        axis.title.y = element_text(size=14),  # Increase y-axis label size
        panel.grid.minor = element_blank())+
  scale_x_continuous(breaks=seq(0,20))+
  scale_y_continuous(breaks = c(7,11,14), limits = c(7, 14))+
  scale_color_manual(labels=c("1"="Referred by both","2"="Referred by TCB15 only","3"="Referred by NICE only","4"="Not referred"),values = c("1"="green","2"="blue","3"="red", "4"="black"),guide = guide_legend(override.aes = list(size=3)))+
  ggtitle("Pooled data: PNC-1")

#Birth outcome
TCB_group_birth_outcome<-NULL
for (i in 1:n.obs) {
  if (is.na(df_infant_tcb$TCB_threshold_birth_outcome_ind[i]) | is.na(df_infant_tcb$TCB_15_birth_outcome_ind[i])) {
    TCB_group_birth_outcome[i]<-NA
  }
  else if(df_infant_tcb$TCB_threshold_birth_outcome_ind[i]==1 & df_infant_tcb$TCB_15_birth_outcome_ind[i]==1) {
    TCB_group_birth_outcome[i]<-1
  }
  else if (df_infant_tcb$TCB_threshold_birth_outcome_ind[i]==0 & df_infant_tcb$TCB_15_birth_outcome_ind[i]==1) {
    TCB_group_birth_outcome[i]<-2
  }
  else if (df_infant_tcb$TCB_threshold_birth_outcome_ind[i]==1 & df_infant_tcb$TCB_15_birth_outcome_ind[i]==0) {
    TCB_group_birth_outcome[i]<-3
  }
  else if (df_infant_tcb$TCB_threshold_birth_outcome_ind[i]==0 & df_infant_tcb$TCB_15_birth_outcome_ind[i]==0) {
    TCB_group_birth_outcome[i]<-4
  }
}

df_infant_tcb$TCB_group_birth_outcome<-as.character(TCB_group_birth_outcome)

#remove missing observation
jaundice_category_birth_outcome<-NULL
jaundice_category_birth_outcome<-ifelse(df_infant_tcb$M11_JAUND_CEOCCUR_6==0,"No Jaundice",NA)
jaundice_category_birth_outcome<-ifelse(df_infant_tcb$birth_outcome_jaund_ind,"Jaundice",jaundice_category_birth_outcome)
jaundice_category_birth_outcome<-ifelse(df_infant_tcb$birth_outcome_severe_jaund_ind==1,"Severe Jaundice",jaundice_category_birth_outcome)

#data cleaning
df_infant_tcb$jaundice_category_birth_outcome<-jaundice_category_birth_outcome
df_infant_tcb_jaund_cat_birth_outcome<-df_infant_tcb[!is.na(df_infant_tcb$jaundice_category_birth_outcome),]
df_infant_tcb_jaund_cat_birth_outcome<-df_infant_tcb_jaund_cat_birth_outcome[!is.na(df_infant_tcb_jaund_cat_birth_outcome$TCB_group_birth_outcome),]

#data cleaning, The TcB measurement >30 might miss the decimal point and could be a typo
df_infant_tcb_jaund_cat_birth_outcome[which(df_infant_tcb_jaund_cat_birth_outcome$M11_TBILIRUBIN_UMOLL_LBORRES_6>30),]$M11_TBILIRUBIN_UMOLL_LBORRES_6<-NA

df_infant_tcb_jaund_cat_birth_outcome$jaundice_category_birth_outcome<-factor(df_infant_tcb_jaund_cat_birth_outcome$jaundice_category_birth_outcome, levels = c("Severe Jaundice", "Jaundice", "No Jaundice"))

df_infant_tcb_jaund_cat_birth_outcome %>% 
  ggplot(aes(x = M11_TBILIRUBIN_UMOLL_LBORRES_6, y = birth_outcome_age_days_exact, color = TCB_group_birth_outcome))+
  geom_point(alpha = 1, size = 1.5)+
  geom_hline(aes(yintercept = 1), color = "purple1", alpha=0.6, linetype="F1") +
  geom_hline(aes(yintercept = 7), color = "purple2", alpha=0.6, linetype="F1") + 
  geom_vline(aes(xintercept = 15), color = "orange1", alpha=0.6, linetype="F1") +
  facet_grid(rows = vars(jaundice_category_birth_outcome))+
  xlab("TCB level (mg/dl)")+
  ylab("Age days")+
  theme_bw() +
  theme(strip.background=element_rect(fill="white")) + 
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1),
        legend.position="bottom",
        legend.title = element_blank(),
        legend.text=element_text(size=12),
        strip.text = element_text(size=14),   # Increase facet label size
        axis.title.x = element_text(size=14),  # Increase x-axis label size
        axis.title.y = element_text(size=14),  # Increase y-axis label size
        panel.grid.minor = element_blank())+
  scale_x_continuous(breaks=seq(0,20))+
  scale_y_continuous(breaks = c(1,4,7), limits = c(0, 7))+
  scale_color_manual(labels=c("1"="Referred by both","2"="Referred by TCB15 only","3"="Referred by NICE only","4"="Not referred"),values = c("1"="green","2"="blue","3"="red", "4"="black"),guide = guide_legend(override.aes = list(size=3)))+
  ggtitle("Pooled data: IPC")

####Ghana
df_infant_tcb_Ghana<-df_infant_tcb %>% filter(SITE=="Ghana")
#PNC-0
df_infant_tcb_Ghana_jaund_cat_PNC_1<-df_infant_tcb_Ghana[!is.na(df_infant_tcb_Ghana$jaundice_category_PNC_1),]
df_infant_tcb_Ghana_jaund_cat_PNC_1<-df_infant_tcb_Ghana_jaund_cat_PNC_1[!is.na(df_infant_tcb_Ghana_jaund_cat_PNC_1$TCB_group_PNC_1),]

#data cleaning, The TcB measurement >30 might miss the decimal point and could be a typo
df_infant_tcb_Ghana_jaund_cat_PNC_1[which(df_infant_tcb_Ghana_jaund_cat_PNC_1$M14_TCB_UMOLL_LBORRES_7>30),]$M14_TCB_UMOLL_LBORRES_7<-NA

df_infant_tcb_Ghana_jaund_cat_PNC_1$jaundice_category_PNC_1<-factor(df_infant_tcb_Ghana_jaund_cat_PNC_1$jaundice_category_PNC_1, levels = c("Severe Jaundice", "Jaundice", "No Jaundice"))

df_infant_tcb_Ghana_jaund_cat_PNC_1 %>% 
  ggplot(aes(x = M14_TCB_UMOLL_LBORRES_7, y = PNC_age_days_exact1, color = TCB_group_PNC_1))+
  geom_point(alpha = 1, size = 1.5)+
  geom_hline(aes(yintercept = 1), color = "purple1", alpha=0.6, linetype="F1") +
  geom_hline(aes(yintercept = 7), color = "purple2", alpha=0.6, linetype="F1") +
  geom_hline(aes(yintercept = 14), color = "purple3", alpha=0.6, linetype="F1") +
  geom_vline(aes(xintercept = 15), color = "orange1", alpha=0.6, linetype="F1") +
  facet_grid(rows = vars(jaundice_category_PNC_1))+
  xlab("TCB level (mg/dl)")+
  ylab("Age days")+
  theme_bw() +
  theme(strip.background=element_rect(fill="white")) + 
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1),
        legend.position="bottom",
        legend.title = element_blank(),
        legend.text=element_text(size=12),
        strip.text = element_text(size=14),   # Increase facet label size
        axis.title.x = element_text(size=14),  # Increase x-axis label size
        axis.title.y = element_text(size=14),  # Increase y-axis label size
        panel.grid.minor = element_blank())+
  scale_x_continuous(breaks=seq(0,20))+
  scale_y_continuous(breaks = c(1,4,7), limits = c(0, 7))+
  scale_color_manual(labels=c("1"="Referred by both","2"="Referred by TCB15 only","3"="Referred by NICE only","4"="Not referred"),values = c("1"="green","2"="blue","3"="red", "4"="black"),guide = guide_legend(override.aes = list(size=3)))+
  ggtitle("Ghana: PNC-0")

#PNC-1
df_infant_tcb_Ghana_jaund_cat_PNC_2<-df_infant_tcb_Ghana[!is.na(df_infant_tcb_Ghana$jaundice_category_PNC_2),]
df_infant_tcb_Ghana_jaund_cat_PNC_2<-df_infant_tcb_Ghana_jaund_cat_PNC_2[!is.na(df_infant_tcb_Ghana_jaund_cat_PNC_2$TCB_group_PNC_2),]

#data cleaning, The TcB measurement >30 might miss the decimal point and could be a typo
df_infant_tcb_Ghana_jaund_cat_PNC_2[which(df_infant_tcb_Ghana_jaund_cat_PNC_2$M14_TCB_UMOLL_LBORRES_8>30),]$M14_TCB_UMOLL_LBORRES_8<-NA

df_infant_tcb_Ghana_jaund_cat_PNC_2$jaundice_category_PNC_2<-factor(df_infant_tcb_Ghana_jaund_cat_PNC_2$jaundice_category_PNC_2, levels = c("Severe Jaundice", "Jaundice", "No Jaundice"))

df_infant_tcb_Ghana_jaund_cat_PNC_2 %>% 
  ggplot(aes(x = M14_TCB_UMOLL_LBORRES_8, y = PNC_age_days_exact2, color = TCB_group_PNC_2))+
  geom_point(alpha = 1, size = 1.5)+
  geom_hline(aes(yintercept = 1), color = "purple1", alpha=0.6, linetype="F1") +
  geom_hline(aes(yintercept = 7), color = "purple2", alpha=0.6, linetype="F1") +
  geom_hline(aes(yintercept = 14), color = "purple3", alpha=0.6, linetype="F1") +
  geom_vline(aes(xintercept = 15), color = "orange1", alpha=0.6, linetype="F1") +
  facet_grid(rows = vars(jaundice_category_PNC_2))+
  xlab("TCB level (mg/dl)")+
  ylab("Age days")+
  theme_bw() +
  theme(strip.background=element_rect(fill="white")) + 
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1),
        legend.position="bottom",
        legend.title = element_blank(),
        legend.text=element_text(size=12),
        strip.text = element_text(size=14),   # Increase facet label size
        axis.title.x = element_text(size=14),  # Increase x-axis label size
        axis.title.y = element_text(size=14),  # Increase y-axis label size
        panel.grid.minor = element_blank())+
  scale_x_continuous(breaks=seq(0,20))+
  scale_y_continuous(breaks = c(7,11,14), limits = c(7, 14))+
  scale_color_manual(labels=c("1"="Referred by both","2"="Referred by TCB15 only","3"="Referred by NICE only","4"="Not referred"),values = c("1"="green","2"="blue","3"="red", "4"="black"),guide = guide_legend(override.aes = list(size=3)))+
  ggtitle("Ghana: PNC-1")

#Birth outcome
df_infant_tcb_Ghana_jaund_cat_birth_outcome<-df_infant_tcb_Ghana[!is.na(df_infant_tcb_Ghana$jaundice_category_birth_outcome),]
df_infant_tcb_Ghana_jaund_cat_birth_outcome<-df_infant_tcb_Ghana_jaund_cat_birth_outcome[!is.na(df_infant_tcb_Ghana_jaund_cat_birth_outcome$TCB_group_birth_outcome),]

#data cleaning, The TcB measurement >30 might miss the decimal point and could be a typo
df_infant_tcb_Ghana_jaund_cat_birth_outcome[which(df_infant_tcb_Ghana_jaund_cat_birth_outcome$M11_TBILIRUBIN_UMOLL_LBORRES_6>30),]$M11_TBILIRUBIN_UMOLL_LBORRES_6<-NA

df_infant_tcb_Ghana_jaund_cat_birth_outcome$jaundice_category_birth_outcome<-factor(df_infant_tcb_Ghana_jaund_cat_birth_outcome$jaundice_category_birth_outcome, levels = c("Severe Jaundice", "Jaundice", "No Jaundice"))

df_infant_tcb_Ghana_jaund_cat_birth_outcome %>% 
  ggplot(aes(x = M11_TBILIRUBIN_UMOLL_LBORRES_6, y = birth_outcome_age_days_exact, color = TCB_group_birth_outcome))+
  geom_point(alpha = 1, size = 1.5)+
  geom_hline(aes(yintercept = 1), color = "purple1", alpha=0.6, linetype="F1") +
  geom_hline(aes(yintercept = 7), color = "purple2", alpha=0.6, linetype="F1") + 
  geom_vline(aes(xintercept = 15), color = "orange1", alpha=0.6, linetype="F1") +
  facet_grid(rows = vars(jaundice_category_birth_outcome))+
  xlab("TCB level (mg/dl)")+
  ylab("Age days")+
  theme_bw() +
  theme(strip.background=element_rect(fill="white")) + 
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1),
        legend.position="bottom",
        legend.title = element_blank(),
        legend.text=element_text(size=12),
        strip.text = element_text(size=14),   # Increase facet label size
        axis.title.x = element_text(size=14),  # Increase x-axis label size
        axis.title.y = element_text(size=14),  # Increase y-axis label size
        panel.grid.minor = element_blank())+
  scale_x_continuous(breaks=seq(0,20))+
  scale_y_continuous(breaks = c(1,4,7), limits = c(0, 7))+
  scale_color_manual(labels=c("1"="Referred by both","2"="Referred by TCB15 only","3"="Referred by NICE only","4"="Not referred"),values = c("1"="green","2"="blue","3"="red", "4"="black"),guide = guide_legend(override.aes = list(size=3)))+
  ggtitle("Ghana: IPC")

####Kenya
df_infant_tcb_Kenya<-df_infant_tcb %>% filter(SITE=="Kenya")
#PNC-0
df_infant_tcb_Kenya_jaund_cat_PNC_1<-df_infant_tcb_Kenya[!is.na(df_infant_tcb_Kenya$jaundice_category_PNC_1),]
df_infant_tcb_Kenya_jaund_cat_PNC_1<-df_infant_tcb_Kenya_jaund_cat_PNC_1[!is.na(df_infant_tcb_Kenya_jaund_cat_PNC_1$TCB_group_PNC_1),]

#data cleaning, The TcB measurement >30 might miss the decimal point and could be a typo
df_infant_tcb_Kenya_jaund_cat_PNC_1[which(df_infant_tcb_Kenya_jaund_cat_PNC_1$M14_TCB_UMOLL_LBORRES_7>30),]$M14_TCB_UMOLL_LBORRES_7<-NA

df_infant_tcb_Kenya_jaund_cat_PNC_1$jaundice_category_PNC_1<-factor(df_infant_tcb_Kenya_jaund_cat_PNC_1$jaundice_category_PNC_1, levels = c("Severe Jaundice", "Jaundice", "No Jaundice"))

df_infant_tcb_Kenya_jaund_cat_PNC_1 %>% 
  ggplot(aes(x = M14_TCB_UMOLL_LBORRES_7, y = PNC_age_days_exact1, color = TCB_group_PNC_1))+
  geom_point(alpha = 1, size = 1.5)+
  geom_hline(aes(yintercept = 1), color = "purple1", alpha=0.6, linetype="F1") +
  geom_hline(aes(yintercept = 7), color = "purple2", alpha=0.6, linetype="F1") +
  geom_hline(aes(yintercept = 14), color = "purple3", alpha=0.6, linetype="F1") +
  geom_vline(aes(xintercept = 15), color = "orange1", alpha=0.6, linetype="F1") +
  facet_grid(rows = vars(jaundice_category_PNC_1))+
  xlab("TCB level (mg/dl)")+
  ylab("Age days")+
  theme_bw() +
  theme(strip.background=element_rect(fill="white")) + 
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1),
        legend.position="bottom",
        legend.title = element_blank(),
        legend.text=element_text(size=12),
        strip.text = element_text(size=14),   # Increase facet label size
        axis.title.x = element_text(size=14),  # Increase x-axis label size
        axis.title.y = element_text(size=14),  # Increase y-axis label size
        panel.grid.minor = element_blank())+
  scale_x_continuous(breaks=seq(0,20))+
  scale_y_continuous(breaks = c(1,4,7), limits = c(0, 7))+
  scale_color_manual(labels=c("1"="Referred by both","2"="Referred by TCB15 only","3"="Referred by NICE only","4"="Not referred"),values = c("1"="green","2"="blue","3"="red", "4"="black"),guide = guide_legend(override.aes = list(size=3)))+
  ggtitle("Kenya: PNC-0")

#PNC-1
df_infant_tcb_Kenya_jaund_cat_PNC_2<-df_infant_tcb_Kenya[!is.na(df_infant_tcb_Kenya$jaundice_category_PNC_2),]
df_infant_tcb_Kenya_jaund_cat_PNC_2<-df_infant_tcb_Kenya_jaund_cat_PNC_2[!is.na(df_infant_tcb_Kenya_jaund_cat_PNC_2$TCB_group_PNC_2),]

#data cleaning, The TcB measurement >30 might miss the decimal point and could be a typo
df_infant_tcb_Kenya_jaund_cat_PNC_2[which(df_infant_tcb_Kenya_jaund_cat_PNC_2$M14_TCB_UMOLL_LBORRES_8>30),]$M14_TCB_UMOLL_LBORRES_8<-NA

df_infant_tcb_Kenya_jaund_cat_PNC_2$jaundice_category_PNC_2<-factor(df_infant_tcb_Kenya_jaund_cat_PNC_2$jaundice_category_PNC_2, levels = c("Severe Jaundice", "Jaundice", "No Jaundice"))

df_infant_tcb_Kenya_jaund_cat_PNC_2 %>% 
  ggplot(aes(x = M14_TCB_UMOLL_LBORRES_8, y = PNC_age_days_exact2, color = TCB_group_PNC_2))+
  geom_point(alpha = 1, size = 1.5)+
  geom_hline(aes(yintercept = 1), color = "purple1", alpha=0.6, linetype="F1") +
  geom_hline(aes(yintercept = 7), color = "purple2", alpha=0.6, linetype="F1") +
  geom_hline(aes(yintercept = 14), color = "purple3", alpha=0.6, linetype="F1") +
  geom_vline(aes(xintercept = 15), color = "orange1", alpha=0.6, linetype="F1") +
  facet_grid(rows = vars(jaundice_category_PNC_2))+
  xlab("TCB level (mg/dl)")+
  ylab("Age days")+
  theme_bw() +
  theme(strip.background=element_rect(fill="white")) + 
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1),
        legend.position="bottom",
        legend.title = element_blank(),
        legend.text=element_text(size=12),
        strip.text = element_text(size=14),   # Increase facet label size
        axis.title.x = element_text(size=14),  # Increase x-axis label size
        axis.title.y = element_text(size=14),  # Increase y-axis label size
        panel.grid.minor = element_blank())+
  scale_x_continuous(breaks=seq(0,20))+
  scale_y_continuous(breaks = c(7,11,14), limits = c(7, 14))+
  scale_color_manual(labels=c("1"="Referred by both","2"="Referred by TCB15 only","3"="Referred by NICE only","4"="Not referred"),values = c("1"="green","2"="blue","3"="red", "4"="black"),guide = guide_legend(override.aes = list(size=3)))+
  ggtitle("Kenya: PNC-1")

#Birth outcome
df_infant_tcb_Kenya_jaund_cat_birth_outcome<-df_infant_tcb_Kenya[!is.na(df_infant_tcb_Kenya$jaundice_category_birth_outcome),]
df_infant_tcb_Kenya_jaund_cat_birth_outcome<-df_infant_tcb_Kenya_jaund_cat_birth_outcome[!is.na(df_infant_tcb_Kenya_jaund_cat_birth_outcome$TCB_group_birth_outcome),]

#data cleaning, The TcB measurement >30 might miss the decimal point and could be a typo
df_infant_tcb_Kenya_jaund_cat_birth_outcome[which(df_infant_tcb_Kenya_jaund_cat_birth_outcome$M11_TBILIRUBIN_UMOLL_LBORRES_6>30),]$M11_TBILIRUBIN_UMOLL_LBORRES_6<-NA

df_infant_tcb_Kenya_jaund_cat_birth_outcome$jaundice_category_birth_outcome<-factor(df_infant_tcb_Kenya_jaund_cat_birth_outcome$jaundice_category_birth_outcome, levels = c("Severe Jaundice", "Jaundice", "No Jaundice"))

df_infant_tcb_Kenya_jaund_cat_birth_outcome %>% 
  ggplot(aes(x = M11_TBILIRUBIN_UMOLL_LBORRES_6, y = birth_outcome_age_days_exact, color = TCB_group_birth_outcome))+
  geom_point(alpha = 1, size = 1.5)+
  geom_hline(aes(yintercept = 1), color = "purple1", alpha=0.6, linetype="F1") +
  geom_hline(aes(yintercept = 7), color = "purple2", alpha=0.6, linetype="F1") + 
  geom_vline(aes(xintercept = 15), color = "orange1", alpha=0.6, linetype="F1") +
  facet_grid(rows = vars(jaundice_category_birth_outcome))+
  xlab("TCB level (mg/dl)")+
  ylab("Age days")+
  theme_bw() +
  theme(strip.background=element_rect(fill="white")) + 
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1),
        legend.position="bottom",
        legend.title = element_blank(),
        legend.text=element_text(size=12),
        strip.text = element_text(size=14),   # Increase facet label size
        axis.title.x = element_text(size=14),  # Increase x-axis label size
        axis.title.y = element_text(size=14),  # Increase y-axis label size
        panel.grid.minor = element_blank())+
  scale_x_continuous(breaks=seq(0,20))+
  scale_y_continuous(breaks = c(1,4,7), limits = c(0, 7))+
  scale_color_manual(labels=c("1"="Referred by both","2"="Referred by TCB15 only","3"="Referred by NICE only","4"="Not referred"),values = c("1"="green","2"="blue","3"="red", "4"="black"),guide = guide_legend(override.aes = list(size=3)))+
  ggtitle("Kenya: IPC")

####Pakistan
df_infant_tcb_Pakistan<-df_infant_tcb %>% filter(SITE=="Pakistan")
#PNC-0
df_infant_tcb_Pakistan_jaund_cat_PNC_1<-df_infant_tcb_Pakistan[!is.na(df_infant_tcb_Pakistan$jaundice_category_PNC_1),]
df_infant_tcb_Pakistan_jaund_cat_PNC_1<-df_infant_tcb_Pakistan_jaund_cat_PNC_1[!is.na(df_infant_tcb_Pakistan_jaund_cat_PNC_1$TCB_group_PNC_1),]

#data cleaning, The TcB measurement >30 might miss the decimal point and could be a typo
df_infant_tcb_Pakistan_jaund_cat_PNC_1[which(df_infant_tcb_Pakistan_jaund_cat_PNC_1$M14_TCB_UMOLL_LBORRES_7>30),]$M14_TCB_UMOLL_LBORRES_7<-NA

df_infant_tcb_Pakistan_jaund_cat_PNC_1$jaundice_category_PNC_1<-factor(df_infant_tcb_Pakistan_jaund_cat_PNC_1$jaundice_category_PNC_1, levels = c("Severe Jaundice", "Jaundice", "No Jaundice"))

df_infant_tcb_Pakistan_jaund_cat_PNC_1 %>% 
  ggplot(aes(x = M14_TCB_UMOLL_LBORRES_7, y = PNC_age_days_exact1, color = TCB_group_PNC_1))+
  geom_point(alpha = 1, size = 1.5)+
  geom_hline(aes(yintercept = 1), color = "purple1", alpha=0.6, linetype="F1") +
  geom_hline(aes(yintercept = 7), color = "purple2", alpha=0.6, linetype="F1") +
  geom_hline(aes(yintercept = 14), color = "purple3", alpha=0.6, linetype="F1") +
  geom_vline(aes(xintercept = 15), color = "orange1", alpha=0.6, linetype="F1") +
  facet_grid(rows = vars(jaundice_category_PNC_1))+
  xlab("TCB level (mg/dl)")+
  ylab("Age days")+
  theme_bw() +
  theme(strip.background=element_rect(fill="white")) + 
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1),
        legend.position="bottom",
        legend.title = element_blank(),
        legend.text=element_text(size=12),
        strip.text = element_text(size=14),   # Increase facet label size
        axis.title.x = element_text(size=14),  # Increase x-axis label size
        axis.title.y = element_text(size=14),  # Increase y-axis label size
        panel.grid.minor = element_blank())+
  scale_x_continuous(breaks=seq(0,20))+
  scale_y_continuous(breaks = c(1,4,7), limits = c(0, 7))+
  scale_color_manual(labels=c("1"="Referred by both","2"="Referred by TCB15 only","3"="Referred by NICE only","4"="Not referred"),values = c("1"="green","2"="blue","3"="red", "4"="black"),guide = guide_legend(override.aes = list(size=3)))+
  ggtitle("Pakistan: PNC-0")

#PNC-1
df_infant_tcb_Pakistan_jaund_cat_PNC_2<-df_infant_tcb_Pakistan[!is.na(df_infant_tcb_Pakistan$jaundice_category_PNC_2),]
df_infant_tcb_Pakistan_jaund_cat_PNC_2<-df_infant_tcb_Pakistan_jaund_cat_PNC_2[!is.na(df_infant_tcb_Pakistan_jaund_cat_PNC_2$TCB_group_PNC_2),]

#data cleaning, The TcB measurement >30 might miss the decimal point and could be a typo
df_infant_tcb_Pakistan_jaund_cat_PNC_2[which(df_infant_tcb_Pakistan_jaund_cat_PNC_2$M14_TCB_UMOLL_LBORRES_8>30),]$M14_TCB_UMOLL_LBORRES_8<-NA

df_infant_tcb_Pakistan_jaund_cat_PNC_2$jaundice_category_PNC_2<-factor(df_infant_tcb_Pakistan_jaund_cat_PNC_2$jaundice_category_PNC_2, levels = c("Severe Jaundice", "Jaundice", "No Jaundice"))

df_infant_tcb_Pakistan_jaund_cat_PNC_2 %>% 
  ggplot(aes(x = M14_TCB_UMOLL_LBORRES_8, y = PNC_age_days_exact2, color = TCB_group_PNC_2))+
  geom_point(alpha = 1, size = 1.5)+
  geom_hline(aes(yintercept = 1), color = "purple1", alpha=0.6, linetype="F1") +
  geom_hline(aes(yintercept = 7), color = "purple2", alpha=0.6, linetype="F1") +
  geom_hline(aes(yintercept = 14), color = "purple3", alpha=0.6, linetype="F1") +
  geom_vline(aes(xintercept = 15), color = "orange1", alpha=0.6, linetype="F1") +
  facet_grid(rows = vars(jaundice_category_PNC_2))+
  xlab("TCB level (mg/dl)")+
  ylab("Age days")+
  theme_bw() +
  theme(strip.background=element_rect(fill="white")) + 
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1),
        legend.position="bottom",
        legend.title = element_blank(),
        legend.text=element_text(size=12),
        strip.text = element_text(size=14),   # Increase facet label size
        axis.title.x = element_text(size=14),  # Increase x-axis label size
        axis.title.y = element_text(size=14),  # Increase y-axis label size
        panel.grid.minor = element_blank())+
  scale_x_continuous(breaks=seq(0,20))+
  scale_y_continuous(breaks = c(7,11,14), limits = c(7, 14))+
  scale_color_manual(labels=c("1"="Referred by both","2"="Referred by TCB15 only","3"="Referred by NICE only","4"="Not referred"),values = c("1"="green","2"="blue","3"="red", "4"="black"),guide = guide_legend(override.aes = list(size=3)))+
  ggtitle("Pakistan: PNC-1")

#Birth outcome
df_infant_tcb_Pakistan_jaund_cat_birth_outcome<-df_infant_tcb_Pakistan[!is.na(df_infant_tcb_Pakistan$jaundice_category_birth_outcome),]
df_infant_tcb_Pakistan_jaund_cat_birth_outcome<-df_infant_tcb_Pakistan_jaund_cat_birth_outcome[!is.na(df_infant_tcb_Pakistan_jaund_cat_birth_outcome$TCB_group_birth_outcome),]

#data cleaning, The TcB measurement >30 might miss the decimal point and could be a typo
df_infant_tcb_Pakistan_jaund_cat_birth_outcome[which(df_infant_tcb_Pakistan_jaund_cat_birth_outcome$M11_TBILIRUBIN_UMOLL_LBORRES_6>30),]$M11_TBILIRUBIN_UMOLL_LBORRES_6<-NA

df_infant_tcb_Pakistan_jaund_cat_birth_outcome$jaundice_category_birth_outcome<-factor(df_infant_tcb_Pakistan_jaund_cat_birth_outcome$jaundice_category_birth_outcome, levels = c("Severe Jaundice", "Jaundice", "No Jaundice"))

df_infant_tcb_Pakistan_jaund_cat_birth_outcome %>% 
  ggplot(aes(x = M11_TBILIRUBIN_UMOLL_LBORRES_6, y = birth_outcome_age_days_exact, color = TCB_group_birth_outcome))+
  geom_point(alpha = 1, size = 1.5)+
  geom_hline(aes(yintercept = 1), color = "purple1", alpha=0.6, linetype="F1") +
  geom_hline(aes(yintercept = 7), color = "purple2", alpha=0.6, linetype="F1") + 
  geom_vline(aes(xintercept = 15), color = "orange1", alpha=0.6, linetype="F1") +
  facet_grid(rows = vars(jaundice_category_birth_outcome))+
  xlab("TCB level (mg/dl)")+
  ylab("Age days")+
  theme_bw() +
  theme(strip.background=element_rect(fill="white")) + 
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1),
        legend.position="bottom",
        legend.title = element_blank(),
        legend.text=element_text(size=12),
        strip.text = element_text(size=14),   # Increase facet label size
        axis.title.x = element_text(size=14),  # Increase x-axis label size
        axis.title.y = element_text(size=14),  # Increase y-axis label size
        panel.grid.minor = element_blank())+
  scale_x_continuous(breaks=seq(0,20))+
  scale_y_continuous(breaks = c(1,4,7), limits = c(0, 7))+
  scale_color_manual(labels=c("1"="Referred by both","2"="Referred by TCB15 only","3"="Referred by NICE only","4"="Not referred"),values = c("1"="green","2"="blue","3"="red", "4"="black"),guide = guide_legend(override.aes = list(size=3)))+
  ggtitle("Pakistan: IPC")

####Zambia
df_infant_tcb_Zambia<-df_infant_tcb %>% filter(SITE=="Zambia")
#PNC-0
df_infant_tcb_Zambia_jaund_cat_PNC_1<-df_infant_tcb_Zambia[!is.na(df_infant_tcb_Zambia$jaundice_category_PNC_1),]
df_infant_tcb_Zambia_jaund_cat_PNC_1<-df_infant_tcb_Zambia_jaund_cat_PNC_1[!is.na(df_infant_tcb_Zambia_jaund_cat_PNC_1$TCB_group_PNC_1),]

#data cleaning, The TcB measurement >30 might miss the decimal point and could be a typo
df_infant_tcb_Zambia_jaund_cat_PNC_1[which(df_infant_tcb_Zambia_jaund_cat_PNC_1$M14_TCB_UMOLL_LBORRES_7>30),]$M14_TCB_UMOLL_LBORRES_7<-NA

df_infant_tcb_Zambia_jaund_cat_PNC_1$jaundice_category_PNC_1<-factor(df_infant_tcb_Zambia_jaund_cat_PNC_1$jaundice_category_PNC_1, levels = c("Severe Jaundice", "Jaundice", "No Jaundice"))

df_infant_tcb_Zambia_jaund_cat_PNC_1 %>% 
  ggplot(aes(x = M14_TCB_UMOLL_LBORRES_7, y = PNC_age_days_exact1, color = TCB_group_PNC_1))+
  geom_point(alpha = 1, size = 1.5)+
  geom_hline(aes(yintercept = 1), color = "purple1", alpha=0.6, linetype="F1") +
  geom_hline(aes(yintercept = 7), color = "purple2", alpha=0.6, linetype="F1") +
  geom_hline(aes(yintercept = 14), color = "purple3", alpha=0.6, linetype="F1") +
  geom_vline(aes(xintercept = 15), color = "orange1", alpha=0.6, linetype="F1") +
  facet_grid(rows = vars(jaundice_category_PNC_1))+
  xlab("TCB level (mg/dl)")+
  ylab("Age days")+
  theme_bw() +
  theme(strip.background=element_rect(fill="white")) + 
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1),
        legend.position="bottom",
        legend.title = element_blank(),
        legend.text=element_text(size=12),
        strip.text = element_text(size=14),   # Increase facet label size
        axis.title.x = element_text(size=14),  # Increase x-axis label size
        axis.title.y = element_text(size=14),  # Increase y-axis label size
        panel.grid.minor = element_blank())+
  scale_x_continuous(breaks=seq(0,20))+
  scale_y_continuous(breaks = c(1,4,7), limits = c(0, 7))+
  scale_color_manual(labels=c("1"="Referred by both","2"="Referred by TCB15 only","3"="Referred by NICE only","4"="Not referred"),values = c("1"="green","2"="blue","3"="red", "4"="black"),guide = guide_legend(override.aes = list(size=3)))+
  ggtitle("Zambia: PNC-0")

#PNC-1
df_infant_tcb_Zambia_jaund_cat_PNC_2<-df_infant_tcb_Zambia[!is.na(df_infant_tcb_Zambia$jaundice_category_PNC_2),]
df_infant_tcb_Zambia_jaund_cat_PNC_2<-df_infant_tcb_Zambia_jaund_cat_PNC_2[!is.na(df_infant_tcb_Zambia_jaund_cat_PNC_2$TCB_group_PNC_2),]

#data cleaning, The TcB measurement >30 might miss the decimal point and could be a typo
df_infant_tcb_Zambia_jaund_cat_PNC_2[which(df_infant_tcb_Zambia_jaund_cat_PNC_2$M14_TCB_UMOLL_LBORRES_8>30),]$M14_TCB_UMOLL_LBORRES_8<-NA

df_infant_tcb_Zambia_jaund_cat_PNC_2$jaundice_category_PNC_2<-factor(df_infant_tcb_Zambia_jaund_cat_PNC_2$jaundice_category_PNC_2, levels = c("Severe Jaundice", "Jaundice", "No Jaundice"))

df_infant_tcb_Zambia_jaund_cat_PNC_2 %>% 
  ggplot(aes(x = M14_TCB_UMOLL_LBORRES_8, y = PNC_age_days_exact2, color = TCB_group_PNC_2))+
  geom_point(alpha = 1, size = 1.5)+
  geom_hline(aes(yintercept = 1), color = "purple1", alpha=0.6, linetype="F1") +
  geom_hline(aes(yintercept = 7), color = "purple2", alpha=0.6, linetype="F1") +
  geom_hline(aes(yintercept = 14), color = "purple3", alpha=0.6, linetype="F1") +
  geom_vline(aes(xintercept = 15), color = "orange1", alpha=0.6, linetype="F1") +
  facet_grid(rows = vars(jaundice_category_PNC_2))+
  xlab("TCB level (mg/dl)")+
  ylab("Age days")+
  theme_bw() +
  theme(strip.background=element_rect(fill="white")) + 
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1),
        legend.position="bottom",
        legend.title = element_blank(),
        legend.text=element_text(size=12),
        strip.text = element_text(size=14),   # Increase facet label size
        axis.title.x = element_text(size=14),  # Increase x-axis label size
        axis.title.y = element_text(size=14),  # Increase y-axis label size
        panel.grid.minor = element_blank())+
  scale_x_continuous(breaks=seq(0,20))+
  scale_y_continuous(breaks = c(7,11,14), limits = c(7, 14))+
  scale_color_manual(labels=c("1"="Referred by both","2"="Referred by TCB15 only","3"="Referred by NICE only","4"="Not referred"),values = c("1"="green","2"="blue","3"="red", "4"="black"),guide = guide_legend(override.aes = list(size=3)))+
  ggtitle("Zambia: PNC-1")

#Birth outcome
df_infant_tcb_Zambia_jaund_cat_birth_outcome<-df_infant_tcb_Zambia[!is.na(df_infant_tcb_Zambia$jaundice_category_birth_outcome),]
df_infant_tcb_Zambia_jaund_cat_birth_outcome<-df_infant_tcb_Zambia_jaund_cat_birth_outcome[!is.na(df_infant_tcb_Zambia_jaund_cat_birth_outcome$TCB_group_birth_outcome),]

#data cleaning, The TcB measurement >30 might miss the decimal point and could be a typo
df_infant_tcb_Zambia_jaund_cat_birth_outcome[which(df_infant_tcb_Zambia_jaund_cat_birth_outcome$M11_TBILIRUBIN_UMOLL_LBORRES_6>30),]$M11_TBILIRUBIN_UMOLL_LBORRES_6<-NA

df_infant_tcb_Zambia_jaund_cat_birth_outcome$jaundice_category_birth_outcome<-factor(df_infant_tcb_Zambia_jaund_cat_birth_outcome$jaundice_category_birth_outcome, levels = c("Severe Jaundice", "Jaundice", "No Jaundice"))

df_infant_tcb_Zambia_jaund_cat_birth_outcome %>% 
  ggplot(aes(x = M11_TBILIRUBIN_UMOLL_LBORRES_6, y = birth_outcome_age_days_exact, color = TCB_group_birth_outcome))+
  geom_point(alpha = 1, size = 1.5)+
  geom_hline(aes(yintercept = 1), color = "purple1", alpha=0.6, linetype="F1") +
  geom_hline(aes(yintercept = 7), color = "purple2", alpha=0.6, linetype="F1") + 
  geom_vline(aes(xintercept = 15), color = "orange1", alpha=0.6, linetype="F1") +
  facet_grid(rows = vars(jaundice_category_birth_outcome))+
  xlab("TCB level (mg/dl)")+
  ylab("Age days")+
  theme_bw() +
  theme(strip.background=element_rect(fill="white")) + 
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1),
        legend.position="bottom",
        legend.title = element_blank(),
        legend.text=element_text(size=12),
        strip.text = element_text(size=14),   # Increase facet label size
        axis.title.x = element_text(size=14),  # Increase x-axis label size
        axis.title.y = element_text(size=14),  # Increase y-axis label size
        panel.grid.minor = element_blank())+
  scale_x_continuous(breaks=seq(0,20))+
  scale_y_continuous(breaks = c(1,4,7), limits = c(0, 7))+
  scale_color_manual(labels=c("1"="Referred by both","2"="Referred by TCB15 only","3"="Referred by NICE only","4"="Not referred"),values = c("1"="green","2"="blue","3"="red", "4"="black"),guide = guide_legend(override.aes = list(size=3)))+
  ggtitle("Zambia: IPC")

####India_CMC
df_infant_tcb_India_CMC<-df_infant_tcb %>% filter(SITE=="India-CMC")
#PNC-0
df_infant_tcb_India_CMC_jaund_cat_PNC_1<-df_infant_tcb_India_CMC[!is.na(df_infant_tcb_India_CMC$jaundice_category_PNC_1),]
df_infant_tcb_India_CMC_jaund_cat_PNC_1<-df_infant_tcb_India_CMC_jaund_cat_PNC_1[!is.na(df_infant_tcb_India_CMC_jaund_cat_PNC_1$TCB_group_PNC_1),]

#data cleaning, The TcB measurement >30 might miss the decimal point and could be a typo
df_infant_tcb_India_CMC_jaund_cat_PNC_1[which(df_infant_tcb_India_CMC_jaund_cat_PNC_1$M14_TCB_UMOLL_LBORRES_7>30),]$M14_TCB_UMOLL_LBORRES_7<-NA

df_infant_tcb_India_CMC_jaund_cat_PNC_1$jaundice_category_PNC_1<-factor(df_infant_tcb_India_CMC_jaund_cat_PNC_1$jaundice_category_PNC_1, levels = c("Severe Jaundice", "Jaundice", "No Jaundice"))

df_infant_tcb_India_CMC_jaund_cat_PNC_1 %>% 
  ggplot(aes(x = M14_TCB_UMOLL_LBORRES_7, y = PNC_age_days_exact1, color = TCB_group_PNC_1))+
  geom_point(alpha = 1, size = 1.5)+
  geom_hline(aes(yintercept = 1), color = "purple1", alpha=0.6, linetype="F1") +
  geom_hline(aes(yintercept = 7), color = "purple2", alpha=0.6, linetype="F1") +
  geom_hline(aes(yintercept = 14), color = "purple3", alpha=0.6, linetype="F1") +
  geom_vline(aes(xintercept = 15), color = "orange1", alpha=0.6, linetype="F1") +
  facet_grid(rows = vars(jaundice_category_PNC_1))+
  xlab("TCB level (mg/dl)")+
  ylab("Age days")+
  theme_bw() +
  theme(strip.background=element_rect(fill="white")) + 
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1),
        legend.position="bottom",
        legend.title = element_blank(),
        legend.text=element_text(size=12),
        strip.text = element_text(size=14),   # Increase facet label size
        axis.title.x = element_text(size=14),  # Increase x-axis label size
        axis.title.y = element_text(size=14),  # Increase y-axis label size
        panel.grid.minor = element_blank())+
  scale_x_continuous(breaks=seq(0,20))+
  scale_y_continuous(breaks = c(1,4,7), limits = c(0, 7))+
  scale_color_manual(labels=c("1"="Referred by both","2"="Referred by TCB15 only","3"="Referred by NICE only","4"="Not referred"),values = c("1"="green","2"="blue","3"="red", "4"="black"),guide = guide_legend(override.aes = list(size=3)))+
  ggtitle("India-CMC: PNC-0")

#PNC-1
df_infant_tcb_India_CMC_jaund_cat_PNC_2<-df_infant_tcb_India_CMC[!is.na(df_infant_tcb_India_CMC$jaundice_category_PNC_2),]
df_infant_tcb_India_CMC_jaund_cat_PNC_2<-df_infant_tcb_India_CMC_jaund_cat_PNC_2[!is.na(df_infant_tcb_India_CMC_jaund_cat_PNC_2$TCB_group_PNC_2),]

#data cleaning, The TcB measurement >30 might miss the decimal point and could be a typo
df_infant_tcb_India_CMC_jaund_cat_PNC_2[which(df_infant_tcb_India_CMC_jaund_cat_PNC_2$M14_TCB_UMOLL_LBORRES_8>30),]$M14_TCB_UMOLL_LBORRES_8<-NA

df_infant_tcb_India_CMC_jaund_cat_PNC_2$jaundice_category_PNC_2<-factor(df_infant_tcb_India_CMC_jaund_cat_PNC_2$jaundice_category_PNC_2, levels = c("Severe Jaundice", "Jaundice", "No Jaundice"))

df_infant_tcb_India_CMC_jaund_cat_PNC_2 %>% 
  ggplot(aes(x = M14_TCB_UMOLL_LBORRES_8, y = PNC_age_days_exact2, color = TCB_group_PNC_2))+
  geom_point(alpha = 1, size = 1.5)+
  geom_hline(aes(yintercept = 1), color = "purple1", alpha=0.6, linetype="F1") +
  geom_hline(aes(yintercept = 7), color = "purple2", alpha=0.6, linetype="F1") +
  geom_hline(aes(yintercept = 14), color = "purple3", alpha=0.6, linetype="F1") +
  geom_vline(aes(xintercept = 15), color = "orange1", alpha=0.6, linetype="F1") +
  facet_grid(rows = vars(jaundice_category_PNC_2))+
  xlab("TCB level (mg/dl)")+
  ylab("Age days")+
  theme_bw() +
  theme(strip.background=element_rect(fill="white")) + 
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1),
        legend.position="bottom",
        legend.title = element_blank(),
        legend.text=element_text(size=12),
        strip.text = element_text(size=14),   # Increase facet label size
        axis.title.x = element_text(size=14),  # Increase x-axis label size
        axis.title.y = element_text(size=14),  # Increase y-axis label size
        panel.grid.minor = element_blank())+
  scale_x_continuous(breaks=seq(0,20))+
  scale_y_continuous(breaks = c(7,11,14), limits = c(7, 14))+
  scale_color_manual(labels=c("1"="Referred by both","2"="Referred by TCB15 only","3"="Referred by NICE only","4"="Not referred"),values = c("1"="green","2"="blue","3"="red", "4"="black"),guide = guide_legend(override.aes = list(size=3)))+
  ggtitle("India-CMC: PNC-1")

#Birth outcome
df_infant_tcb_India_CMC_jaund_cat_birth_outcome<-df_infant_tcb_India_CMC[!is.na(df_infant_tcb_India_CMC$jaundice_category_birth_outcome),]
df_infant_tcb_India_CMC_jaund_cat_birth_outcome<-df_infant_tcb_India_CMC_jaund_cat_birth_outcome[!is.na(df_infant_tcb_India_CMC_jaund_cat_birth_outcome$TCB_group_birth_outcome),]

#data cleaning, The TcB measurement >30 might miss the decimal point and could be a typo
df_infant_tcb_India_CMC_jaund_cat_birth_outcome[which(df_infant_tcb_India_CMC_jaund_cat_birth_outcome$M11_TBILIRUBIN_UMOLL_LBORRES_6>30),]$M11_TBILIRUBIN_UMOLL_LBORRES_6<-NA

df_infant_tcb_India_CMC_jaund_cat_birth_outcome$jaundice_category_birth_outcome<-factor(df_infant_tcb_India_CMC_jaund_cat_birth_outcome$jaundice_category_birth_outcome, levels = c("Severe Jaundice", "Jaundice", "No Jaundice"))

df_infant_tcb_India_CMC_jaund_cat_birth_outcome %>% 
  ggplot(aes(x = M11_TBILIRUBIN_UMOLL_LBORRES_6, y = birth_outcome_age_days_exact, color = TCB_group_birth_outcome))+
  geom_point(alpha = 1, size = 1.5)+
  geom_hline(aes(yintercept = 1), color = "purple1", alpha=0.6, linetype="F1") +
  geom_hline(aes(yintercept = 7), color = "purple2", alpha=0.6, linetype="F1") + 
  geom_vline(aes(xintercept = 15), color = "orange1", alpha=0.6, linetype="F1") +
  facet_grid(rows = vars(jaundice_category_birth_outcome))+
  xlab("TCB level (mg/dl)")+
  ylab("Age days")+
  theme_bw() +
  theme(strip.background=element_rect(fill="white")) + 
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1),
        legend.position="bottom",
        legend.title = element_blank(),
        legend.text=element_text(size=12),
        strip.text = element_text(size=14),   # Increase facet label size
        axis.title.x = element_text(size=14),  # Increase x-axis label size
        axis.title.y = element_text(size=14),  # Increase y-axis label size
        panel.grid.minor = element_blank())+
  scale_x_continuous(breaks=seq(0,20))+
  scale_y_continuous(breaks = c(1,4,7), limits = c(0, 7))+
  scale_color_manual(labels=c("1"="Referred by both","2"="Referred by TCB15 only","3"="Referred by NICE only","4"="Not referred"),values = c("1"="green","2"="blue","3"="red", "4"="black"),guide = guide_legend(override.aes = list(size=3)))+
  ggtitle("India-CMC: IPC")

####India_SAS
df_infant_tcb_India_SAS<-df_infant_tcb %>% filter(SITE=="India-SAS")
#PNC-0
df_infant_tcb_India_SAS_jaund_cat_PNC_1<-df_infant_tcb_India_SAS[!is.na(df_infant_tcb_India_SAS$jaundice_category_PNC_1),]
df_infant_tcb_India_SAS_jaund_cat_PNC_1<-df_infant_tcb_India_SAS_jaund_cat_PNC_1[!is.na(df_infant_tcb_India_SAS_jaund_cat_PNC_1$TCB_group_PNC_1),]

#data cleaning, The TcB measurement >30 might miss the decimal point and could be a typo
df_infant_tcb_India_SAS_jaund_cat_PNC_1[which(df_infant_tcb_India_SAS_jaund_cat_PNC_1$M14_TCB_UMOLL_LBORRES_7>30),]$M14_TCB_UMOLL_LBORRES_7<-NA

df_infant_tcb_India_SAS_jaund_cat_PNC_1$jaundice_category_PNC_1<-factor(df_infant_tcb_India_SAS_jaund_cat_PNC_1$jaundice_category_PNC_1, levels = c("Severe Jaundice", "Jaundice", "No Jaundice"))

df_infant_tcb_India_SAS_jaund_cat_PNC_1 %>% 
  ggplot(aes(x = M14_TCB_UMOLL_LBORRES_7, y = PNC_age_days_exact1, color = TCB_group_PNC_1))+
  geom_point(alpha = 1, size = 1.5)+
  geom_hline(aes(yintercept = 1), color = "purple1", alpha=0.6, linetype="F1") +
  geom_hline(aes(yintercept = 7), color = "purple2", alpha=0.6, linetype="F1") +
  geom_hline(aes(yintercept = 14), color = "purple3", alpha=0.6, linetype="F1") +
  geom_vline(aes(xintercept = 15), color = "orange1", alpha=0.6, linetype="F1") +
  facet_grid(rows = vars(jaundice_category_PNC_1))+
  xlab("TCB level (mg/dl)")+
  ylab("Age days")+
  theme_bw() +
  theme(strip.background=element_rect(fill="white")) + 
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1),
        legend.position="bottom",
        legend.title = element_blank(),
        legend.text=element_text(size=12),
        strip.text = element_text(size=14),   # Increase facet label size
        axis.title.x = element_text(size=14),  # Increase x-axis label size
        axis.title.y = element_text(size=14),  # Increase y-axis label size
        panel.grid.minor = element_blank())+
  scale_x_continuous(breaks=seq(0,20))+
  scale_y_continuous(breaks = c(1,4,7), limits = c(0, 7))+
  scale_color_manual(labels=c("1"="Referred by both","2"="Referred by TCB15 only","3"="Referred by NICE only","4"="Not referred"),values = c("1"="green","2"="blue","3"="red", "4"="black"),guide = guide_legend(override.aes = list(size=3)))+
  ggtitle("India-SAS: PNC-0")

#PNC-1
df_infant_tcb_India_SAS_jaund_cat_PNC_2<-df_infant_tcb_India_SAS[!is.na(df_infant_tcb_India_SAS$jaundice_category_PNC_2),]
df_infant_tcb_India_SAS_jaund_cat_PNC_2<-df_infant_tcb_India_SAS_jaund_cat_PNC_2[!is.na(df_infant_tcb_India_SAS_jaund_cat_PNC_2$TCB_group_PNC_2),]

#data cleaning, The TcB measurement >30 might miss the decimal point and could be a typo
df_infant_tcb_India_SAS_jaund_cat_PNC_2[which(df_infant_tcb_India_SAS_jaund_cat_PNC_2$M14_TCB_UMOLL_LBORRES_8>30),]$M14_TCB_UMOLL_LBORRES_8<-NA

df_infant_tcb_India_SAS_jaund_cat_PNC_2$jaundice_category_PNC_2<-factor(df_infant_tcb_India_SAS_jaund_cat_PNC_2$jaundice_category_PNC_2, levels = c("Severe Jaundice", "Jaundice", "No Jaundice"))

df_infant_tcb_India_SAS_jaund_cat_PNC_2 %>% 
  ggplot(aes(x = M14_TCB_UMOLL_LBORRES_8, y = PNC_age_days_exact2, color = TCB_group_PNC_2))+
  geom_point(alpha = 1, size = 1.5)+
  geom_hline(aes(yintercept = 1), color = "purple1", alpha=0.6, linetype="F1") +
  geom_hline(aes(yintercept = 7), color = "purple2", alpha=0.6, linetype="F1") +
  geom_hline(aes(yintercept = 14), color = "purple3", alpha=0.6, linetype="F1") +
  geom_vline(aes(xintercept = 15), color = "orange1", alpha=0.6, linetype="F1") +
  facet_grid(rows = vars(jaundice_category_PNC_2))+
  xlab("TCB level (mg/dl)")+
  ylab("Age days")+
  theme_bw() +
  theme(strip.background=element_rect(fill="white")) + 
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1),
        legend.position="bottom",
        legend.title = element_blank(),
        legend.text=element_text(size=12),
        strip.text = element_text(size=14),   # Increase facet label size
        axis.title.x = element_text(size=14),  # Increase x-axis label size
        axis.title.y = element_text(size=14),  # Increase y-axis label size
        panel.grid.minor = element_blank())+
  scale_x_continuous(breaks=seq(0,20))+
  scale_y_continuous(breaks = c(7,11,14), limits = c(7, 14))+
  scale_color_manual(labels=c("1"="Referred by both","2"="Referred by TCB15 only","3"="Referred by NICE only","4"="Not referred"),values = c("1"="green","2"="blue","3"="red", "4"="black"),guide = guide_legend(override.aes = list(size=3)))+
  ggtitle("India-SAS: PNC-1")

#Birth outcome
df_infant_tcb_India_SAS_jaund_cat_birth_outcome<-df_infant_tcb_India_SAS[!is.na(df_infant_tcb_India_SAS$jaundice_category_birth_outcome),]
df_infant_tcb_India_SAS_jaund_cat_birth_outcome<-df_infant_tcb_India_SAS_jaund_cat_birth_outcome[!is.na(df_infant_tcb_India_SAS_jaund_cat_birth_outcome$TCB_group_birth_outcome),]

#data cleaning, The TcB measurement >30 might miss the decimal point and could be a typo
df_infant_tcb_India_SAS_jaund_cat_birth_outcome[which(df_infant_tcb_India_SAS_jaund_cat_birth_outcome$M11_TBILIRUBIN_UMOLL_LBORRES_6>30),]$M11_TBILIRUBIN_UMOLL_LBORRES_6<-NA

df_infant_tcb_India_SAS_jaund_cat_birth_outcome$jaundice_category_birth_outcome<-factor(df_infant_tcb_India_SAS_jaund_cat_birth_outcome$jaundice_category_birth_outcome, levels = c("Severe Jaundice", "Jaundice", "No Jaundice"))

df_infant_tcb_India_SAS_jaund_cat_birth_outcome %>% 
  ggplot(aes(x = M11_TBILIRUBIN_UMOLL_LBORRES_6, y = birth_outcome_age_days_exact, color = TCB_group_birth_outcome))+
  geom_point(alpha = 1, size = 1.5)+
  geom_hline(aes(yintercept = 1), color = "purple1", alpha=0.6, linetype="F1") +
  geom_hline(aes(yintercept = 7), color = "purple2", alpha=0.6, linetype="F1") + 
  geom_vline(aes(xintercept = 15), color = "orange1", alpha=0.6, linetype="F1") +
  facet_grid(rows = vars(jaundice_category_birth_outcome))+
  xlab("TCB level (mg/dl)")+
  ylab("Age days")+
  theme_bw() +
  theme(strip.background=element_rect(fill="white")) + 
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1),
        legend.position="bottom",
        legend.title = element_blank(),
        legend.text=element_text(size=12),
        strip.text = element_text(size=14),   # Increase facet label size
        axis.title.x = element_text(size=14),  # Increase x-axis label size
        axis.title.y = element_text(size=14),  # Increase y-axis label size
        panel.grid.minor = element_blank())+
  scale_x_continuous(breaks=seq(0,20))+
  scale_y_continuous(breaks = c(1,4,7), limits = c(0, 7))+
  scale_color_manual(labels=c("1"="Referred by both","2"="Referred by TCB15 only","3"="Referred by NICE only","4"="Not referred"),values = c("1"="green","2"="blue","3"="red", "4"="black"),guide = guide_legend(override.aes = list(size=3)))+
  ggtitle("India-SAS: IPC")