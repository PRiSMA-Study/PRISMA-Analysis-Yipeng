library(openxlsx)
library(TSB.NICE)
library(gt)
library(dplyr)
library(stringr)
library(tibble)
library(sigmoid)
library(lme4)
library(irr)
library(survival)

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

###Measure of agreement
df_tcb_long<-df_infant_tcb %>% select("MOMID","PREGID","SITE","INFANTID","M11_TBILIRUBIN_UMOLL_LBORRES_6", "M14_TCB_UMOLL_LBORRES_7", "M14_TCB_UMOLL_LBORRES_8", "M11_JAUND_CEOCCUR_6", "M13_JAUND_CEOCCUR_7", "M13_JAUND_CEOCCUR_8","TCB_threshold_birth_outcome","TCB_threshold_PNC_1","TCB_threshold_PNC_2") %>%
  pivot_longer(
    cols = c(
      M11_TBILIRUBIN_UMOLL_LBORRES_6, M14_TCB_UMOLL_LBORRES_7, M14_TCB_UMOLL_LBORRES_8,
      M11_JAUND_CEOCCUR_6, M13_JAUND_CEOCCUR_7, M13_JAUND_CEOCCUR_8,
      TCB_threshold_birth_outcome, TCB_threshold_PNC_1, TCB_threshold_PNC_2
    ),
    names_to = "variable",
    values_to = "value"
  ) %>%
  mutate(
    visit = case_when(
      grepl("_6$", variable) | grepl("birth_outcome", variable) ~ "IPC",
      grepl("_7$", variable) | grepl("PNC_1", variable) ~ "PNC-0",
      grepl("_8$", variable) | grepl("PNC_2", variable) ~ "PNC-1"
    ),
    variable = case_when(
      grepl("UMOLL_LBORRES", variable) ~ "tcb",
      grepl("JAUND", variable) ~ "jaundice",
      grepl("TCB_threshold", variable) ~ "tcb_threshold",
      TRUE ~ variable
    )
  ) %>%
  pivot_wider(
    names_from = variable,
    values_from = value
  ) %>% mutate(
    tcb15=ifelse(tcb>15,1,0),
    tcb_threshold_ind=ifelse(tcb>tcb_threshold-3,1,0)
   )%>% filter(!is.na(jaundice)&!is.na(tcb))

df_tcb_long_threshold1 <- df_tcb_long %>%
  filter(jaundice==1) %>%
  mutate(Response=ifelse(tcb_threshold_ind==1,1,0))

df_tcb_long_threshold0 <- df_tcb_long %>%
  filter(jaundice==0) %>%
  mutate(Response=ifelse(tcb_threshold_ind==0,1,0))

df_tcb_long_15_1 <- df_tcb_long %>%
  filter(jaundice==1) %>%
  mutate(Response=ifelse(tcb15==1,1,0))

df_tcb_long_15_0 <- df_tcb_long %>%
  filter(jaundice==0) %>%
  mutate(Response=ifelse(tcb15==0,1,0))

###PPA/NPA
print.PPA_NPA <- function(data1, data0) {
  sigmoid <- function(x) 1 / (1 + exp(-x))
  
  get_point_and_ci <- function(model) {
    est <- fixef(model)[1]
    se <- sqrt(vcov(model)[1, 1])
    
    point <- sigmoid(est)
    se_delta <- point * (1 - point) * se
    
    ci_low <- max(point - 1.96 * se_delta, 1e-6)
    ci_up  <- min(point + 1.96 * se_delta, 1)
    
    return(paste0(
      round(point * 100, 1), "% (",
      round(ci_low * 100, 1), "%, ",
      round(ci_up * 100, 1), "%)"
    ))
  }
  
  model1 <- glmer(Response ~ (1 | INFANTID) + (1 | SITE) + (1 | visit),
                  data = data1, family = binomial)
  model0 <- glmer(Response ~ (1 | INFANTID) + (1 | SITE) + (1 | visit),
                  data = data0, family = binomial)
  
  PPA <- get_point_and_ci(model1)
  NPA <- get_point_and_ci(model0)
  
  return(c(PPA,NPA))
}

print.PPA_NPA(df_tcb_long_threshold1,df_tcb_long_threshold0)
print.PPA_NPA(df_tcb_long_15_1,df_tcb_long_15_0)

###Cohen's kappa

glm_model_TCB <- lmer(tcb ~ (1 | INFANTID) + (1 | SITE) + (1 | visit),
                      data = df_tcb_long)
glm_model_JAUNDICE <- glmer(jaundice ~ (1 | INFANTID) + (1 | SITE) + (1 | visit),
                      data = df_tcb_long, family = binomial)

df_tcb_long$predicted_tcb <- predict(glm_model_TCB, newdata = df_tcb_long)
df_tcb_long$predicted_prob_jaundice <- predict(glm_model_JAUNDICE, type = "response")

jaundice_threshold<-sum(df_tcb_long$jaundice==1)/nrow(df_tcb_long)

df_tcb_long$predicted_tcb_threshold_ind <- ifelse(df_tcb_long$predicted_tcb > df_tcb_long$tcb_threshold,1,0) 
df_tcb_long$predicted_tcb15 <- ifelse(df_tcb_long$predicted_tcb > 15,1,0) 
df_tcb_long$predicted_jaundice <- ifelse(df_tcb_long$predicted_prob_jaundice > jaundice_threshold,1,0) 

kappa_result_threshold <- kappa2(df_tcb_long[, c( "predicted_tcb_threshold_ind", "predicted_jaundice")])
kappa_result_15 <- kappa2(df_tcb_long[, c( "predicted_tcb15", "predicted_jaundice")])

###conditional LR
df_tcb_long_threshold_method<-df_tcb_long%>%
  select("MOMID","PREGID","SITE","INFANTID","tcb_threshold_ind","jaundice","visit")%>%
  pivot_longer(
    cols = c(jaundice, tcb_threshold_ind),
    names_to = "method",
    values_to = "value"
  ) %>% mutate(
    strata_id = paste(SITE, visit, INFANTID, sep = "_")
  )

#set jaundice method as the reference group
df_tcb_long_threshold_method$method <- relevel(factor(df_tcb_long_threshold_method$method), ref = "jaundice")

clogit(formula = value ~ method + strata(strata_id),data = df_tcb_long_threshold_method)

df_tcb_long_15_method<-df_tcb_long%>%
  select("MOMID","PREGID","SITE","INFANTID","tcb15","jaundice","visit")%>%
  pivot_longer(
    cols = c(jaundice, tcb15),
    names_to = "method",
    values_to = "value"
  ) %>% mutate(
    strata_id = paste(SITE, visit, INFANTID, sep = "_")
  )

#set jaundice method as the reference group
df_tcb_long_15_method$method <- relevel(factor(df_tcb_long_15_method$method), ref = "jaundice")

clogit(formula = value ~ method + strata(strata_id),data = df_tcb_long_15_method)