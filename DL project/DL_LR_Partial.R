library(haven)
library(readxl)
library(dplyr)
library(ggplot2)
library(knitr)
library(stringr)
library(purrr)
library(lubridate)
library(readr)
library(tidyr)
library(haven)
library(lme4)
library(Matrix) # dependency for lme4
library(tableone)
library(kableExtra)
library(xtable)
library(gee)
library(tidyverse)
library(naniar)
library(fastDummies)
library(zoo)

data.dl.continous<- read.csv("D:/Users/yipeng_wei/Documents/dl data/2025-04-18/df_dl_continous.csv")
data.dl.continous.wide<- read.csv("D:/Users/yipeng_wei/Documents/dl data/2025-04-18/df_dl_continous_wide.csv")

data.dl.continous.wide.Africa<-data.dl.continous.wide%>%filter(SITE%in% c("Zambia","Ghana","Kenya"))
data.dl.continous.wide.Asia<-data.dl.continous.wide%>%filter(SITE%in% c("Pakistan","India-SAS","India-CMC"))

# Logistic regression function
lr.function <- function(data1,data2,outcome) {
  # Define features based on outcome
  if (outcome == "STILLBIRTH_SIGNS_LIFE") {
    features_lr <- c("STILLBIRTH_IND","PRETERM_IND","CESARIAN_IND",
                     "NUM_MISCARRIAGE","PAID_WORK","GPARITY_2","GPARITY_1","WEALTH_QUINT_1","WEALTH_QUINT_2","WEALTH_QUINT_3","WEALTH_QUINT_4","SCHOOL_MORE10","WATER_IMPROVED","TOILET_IMPROVED","STOVE_FUEL","HH_SMOKE","SMOKE","CHEW_TOBACCO","CHEW_BETELNUT","CROWDING_IND",
                     "MAT_AGE","BMI_ENROLL","MUAC_ENROLL","NUM_FETUS",
                     "HTN","DIAB_OVERT_ANY",
                     "GES_HTN","DIAB_GEST_ANY",
                     "MAT_CES_ANY","BIRTH_FACILITY",
                     "INF_SEX","GESTAGEBIRTH_ANY")
  } else if (outcome == "PRETERMBIRTH_LT37") {
    features_lr <- c("STILLBIRTH_IND","PRETERM_IND","CESARIAN_IND",
                     "NUM_MISCARRIAGE","PAID_WORK","GPARITY_2","GPARITY_1","WEALTH_QUINT_1","WEALTH_QUINT_2","WEALTH_QUINT_3","WEALTH_QUINT_4","SCHOOL_MORE10","WATER_IMPROVED","TOILET_IMPROVED","STOVE_FUEL","HH_SMOKE","SMOKE","CHEW_TOBACCO","CHEW_BETELNUT","CROWDING_IND",
                     "MAT_AGE","BMI_ENROLL","MUAC_ENROLL","NUM_FETUS",
                     "HTN","DIAB_OVERT_ANY",
                     "GES_HTN","DIAB_GEST_ANY",
                     "INF_SEX")
  } else if (outcome == "LBW2500_ANY") {
    features_lr <- c("STILLBIRTH_IND","PRETERM_IND","CESARIAN_IND",
                     "NUM_MISCARRIAGE","PAID_WORK","GPARITY_2","GPARITY_1","WEALTH_QUINT_1","WEALTH_QUINT_2","WEALTH_QUINT_3","WEALTH_QUINT_4","SCHOOL_MORE10","WATER_IMPROVED","TOILET_IMPROVED","STOVE_FUEL","HH_SMOKE","SMOKE","CHEW_TOBACCO","CHEW_BETELNUT","CROWDING_IND",
                     "MAT_AGE","BMI_ENROLL","MUAC_ENROLL","NUM_FETUS",
                     "HTN","DIAB_OVERT_ANY",
                     "GES_HTN","DIAB_GEST_ANY",
                     "INF_SEX","GESTAGEBIRTH_ANY")
  } else if (outcome == "INF_PSBI_IPC") {
    features_lr <- c("STILLBIRTH_IND","PRETERM_IND","CESARIAN_IND",
                     "NUM_MISCARRIAGE","PAID_WORK","GPARITY_2","GPARITY_1","WEALTH_QUINT_1","WEALTH_QUINT_2","WEALTH_QUINT_3","WEALTH_QUINT_4","SCHOOL_MORE10","WATER_IMPROVED","TOILET_IMPROVED","STOVE_FUEL","HH_SMOKE","SMOKE","CHEW_TOBACCO","CHEW_BETELNUT","CROWDING_IND",
                     "MAT_AGE","BMI_ENROLL","MUAC_ENROLL","NUM_FETUS",
                     "MAT_CES_ANY","BIRTH_FACILITY",
                     "HTN","DIAB_OVERT_ANY",
                     "GES_HTN","DIAB_GEST_ANY",
                     "INF_SEX","GESTAGEBIRTH_ANY","BWEIGHT_ANY","BREASTFED")
  } else if (outcome == "SVN") {
    features_lr <- c("STILLBIRTH_IND","PRETERM_IND","CESARIAN_IND",
                     "NUM_MISCARRIAGE","PAID_WORK","GPARITY_2","GPARITY_1","WEALTH_QUINT_1","WEALTH_QUINT_2","WEALTH_QUINT_3","WEALTH_QUINT_4","SCHOOL_MORE10","WATER_IMPROVED","TOILET_IMPROVED","STOVE_FUEL","HH_SMOKE","SMOKE","CHEW_TOBACCO","CHEW_BETELNUT","CROWDING_IND",
                     "MAT_AGE","BMI_ENROLL","MUAC_ENROLL","NUM_FETUS",
                     "HTN","DIAB_OVERT_ANY",
                     "GES_HTN","DIAB_GEST_ANY")
  } else if (outcome == "NEO_DTH") {
    features_lr <- c("STILLBIRTH_IND","PRETERM_IND","CESARIAN_IND",
                     "NUM_MISCARRIAGE","PAID_WORK","GPARITY_2","GPARITY_1","WEALTH_QUINT_1","WEALTH_QUINT_2","WEALTH_QUINT_3","WEALTH_QUINT_4","SCHOOL_MORE10","WATER_IMPROVED","TOILET_IMPROVED","STOVE_FUEL","HH_SMOKE","SMOKE","CHEW_TOBACCO","CHEW_BETELNUT","CROWDING_IND",
                     "MAT_AGE","BMI_ENROLL","MUAC_ENROLL","NUM_FETUS",
                     "MAT_CES_ANY","BIRTH_FACILITY",
                     "HTN","DIAB_OVERT_ANY",
                     "GES_HTN","DIAB_GEST_ANY",
                     "INF_SEX","GESTAGEBIRTH_ANY","BWEIGHT_ANY","BREASTFED","INF_PSBI_IPC","INF_HYPERBILI_NICE")
  } else if (outcome == "NEARMISS") {
    features_lr <- c("STILLBIRTH_IND","PRETERM_IND","CESARIAN_IND",
                     "NUM_MISCARRIAGE","PAID_WORK","GPARITY_2","GPARITY_1","WEALTH_QUINT_1","WEALTH_QUINT_2","WEALTH_QUINT_3","WEALTH_QUINT_4","SCHOOL_MORE10","WATER_IMPROVED","TOILET_IMPROVED","STOVE_FUEL","HH_SMOKE","SMOKE","CHEW_TOBACCO","CHEW_BETELNUT","CROWDING_IND",
                     "MAT_AGE","BMI_ENROLL","MUAC_ENROLL","NUM_FETUS",
                     "MAT_CES_ANY","BIRTH_FACILITY",
                     "HTN","DIAB_OVERT_ANY",
                     "GES_HTN","DIAB_GEST_ANY")
  }
  
  # Combine outcome and features
  all_vars <- c(outcome, features_lr)
  
  # Drop rows with any missing data
  data_model <- data1 %>% select(all_of(all_vars)) %>% na.omit()
  
  # Create formula object
  formula_str <- paste(outcome, "~", paste(features_lr, collapse = " + "))
  formula_obj <- as.formula(formula_str)
  
  # Fit logistic regression model
  model <- glm(formula_obj, data = data_model, family = binomial(link = "logit"))
  
  predicted_probs <- predict(model, newdata = data2, type = "response")
  
  return(predicted_probs)
}


data.dl.continous$svn.prob<-rep(lr.function(data.dl.continous.wide.Africa,data.dl.continous.wide,"SVN"),each=5)
data.dl.continous$stillbirth.prob<-rep(lr.function(data.dl.continous.wide.Africa,data.dl.continous.wide,"STILLBIRTH_SIGNS_LIFE"),each=5)
data.dl.continous$psbi.prob<-rep(lr.function(data.dl.continous.wide.Africa,data.dl.continous.wide,"INF_PSBI_IPC"),each=5)
data.dl.continous$lbw.prob<-rep(lr.function(data.dl.continous.wide.Africa,data.dl.continous.wide,"LBW2500_ANY"),each=5)
data.dl.continous$preterm.prob<-rep(lr.function(data.dl.continous.wide.Africa,data.dl.continous.wide,"PRETERMBIRTH_LT37"),each=5)
data.dl.continous$neo_dth.prob<-rep(lr.function(data.dl.continous.wide.Africa,data.dl.continous.wide,"NEO_DTH"),each=5)
data.dl.continous$nearmiss.prob<-rep(lr.function(data.dl.continous.wide.Africa,data.dl.continous.wide,"NEARMISS"),each=5)

write.csv(data.dl.continous,"D:/Users/yipeng_wei/Documents/dl data/2025-04-18/df_dl_continous_lr.csv")