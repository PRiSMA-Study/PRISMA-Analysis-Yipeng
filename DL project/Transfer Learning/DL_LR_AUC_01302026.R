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
library(pROC)

data.dl.continuous<- read.csv("D:/Users/yipeng_wei/Documents/dl data/2025-10-31/df_dl_continuous_transfer.csv")
data.dl.continuous.wide<- read.csv("D:/Users/yipeng_wei/Documents/dl data/2025-10-31/df_dl_continuous_wide_transfer.csv")
data.dl.continuous.mask<- read.csv("D:/Users/yipeng_wei/Documents/dl data/2025-10-31/df_dl_continuous_mask_transfer.csv")
data.dl.continuous.delta<- read.csv("D:/Users/yipeng_wei/Documents/dl data/2025-10-31/df_dl_continuous_delta_transfer.csv")

data.dl.continuous.wide.Africa<-data.dl.continuous.wide%>%filter(IMP_GROUP %in% c("AFR_TRAIN","AFR_TEST"))
data.dl.continuous.wide.Asia<-data.dl.continuous.wide%>%filter(IMP_GROUP == "ASIA_ALL")

data.dl.continuous.wide.Africa.lr <- data.dl.continuous.wide.Africa[data.dl.continuous.wide.Africa[,"IMP_GROUP"]=="AFR_TRAIN",]
data.dl.continuous.wide.Africa.dl <- data.dl.continuous.wide.Africa[data.dl.continuous.wide.Africa[,"IMP_GROUP"]=="AFR_TEST",]

id_select_dl <- data.dl.continuous.wide.Africa.dl %>%
  dplyr::select(INFANTID, MOMID, PREGID)

data.dl.continuous.Africa.dl <- data.dl.continuous %>%
  dplyr::filter(SITE %in% c("Zambia", "Ghana", "Kenya")) %>%
  dplyr::semi_join(id_select_dl, by = c("INFANTID", "MOMID", "PREGID"))

data.dl.continuous.mask.Africa.dl <- data.dl.continuous.mask %>%
  dplyr::filter(SITE %in% c("Zambia", "Ghana", "Kenya")) %>%
  dplyr::semi_join(id_select_dl, by = c("INFANTID", "MOMID", "PREGID"))

data.dl.continuous.delta.Africa.dl <- data.dl.continuous.delta %>%
  dplyr::filter(SITE %in% c("Zambia", "Ghana", "Kenya")) %>%
  dplyr::semi_join(id_select_dl, by = c("INFANTID", "MOMID", "PREGID"))

id_select_lr <- data.dl.continuous.wide.Africa.lr %>%
  dplyr::select(INFANTID, MOMID, PREGID)

data.dl.continuous.Africa.lr <- data.dl.continuous %>%
  dplyr::filter(SITE %in% c("Zambia", "Ghana", "Kenya")) %>%
  dplyr::semi_join(id_select_lr, by = c("INFANTID", "MOMID", "PREGID"))

data.dl.continuous.mask.Africa.lr <- data.dl.continuous.mask %>%
  dplyr::filter(SITE %in% c("Zambia", "Ghana", "Kenya")) %>%
  dplyr::semi_join(id_select_lr, by = c("INFANTID", "MOMID", "PREGID"))

data.dl.continuous.delta.Africa.lr <- data.dl.continuous.delta %>%
  dplyr::filter(SITE %in% c("Zambia", "Ghana", "Kenya")) %>%
  dplyr::semi_join(id_select_lr, by = c("INFANTID", "MOMID", "PREGID"))

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
                     "INF_SEX","GESTAGEBIRTH_ANY","BWEIGHT_ANY","BREASTFED","INF_PSBI_ANY","INF_HYPERBILI_NICE")
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


data.dl.continuous.Africa.dl$svn.prob<-lr.function(data.dl.continuous.wide.Africa.lr,data.dl.continuous.Africa.dl,"SVN")
data.dl.continuous.Africa.dl$stillbirth.prob<-lr.function(data.dl.continuous.wide.Africa.lr,data.dl.continuous.Africa.dl,"STILLBIRTH_SIGNS_LIFE")
data.dl.continuous.Africa.dl$lbw.prob<-lr.function(data.dl.continuous.wide.Africa.lr,data.dl.continuous.Africa.dl,"LBW2500_ANY")
data.dl.continuous.Africa.dl$preterm.prob<-lr.function(data.dl.continuous.wide.Africa.lr,data.dl.continuous.Africa.dl,"PRETERMBIRTH_LT37")
data.dl.continuous.Africa.dl$neo_dth.prob<-lr.function(data.dl.continuous.wide.Africa.lr,data.dl.continuous.Africa.dl,"NEO_DTH")
data.dl.continuous.Africa.dl$nearmiss.prob<-lr.function(data.dl.continuous.wide.Africa.lr,data.dl.continuous.Africa.dl,"NEARMISS")

# Define outcome-probability pairs
outcome_prob_pairs <- list(
  SVN = "svn.prob",
  STILLBIRTH_SIGNS_LIFE = "stillbirth.prob",
  LBW2500_ANY = "lbw.prob",
  PRETERMBIRTH_LT37 = "preterm.prob",
  NEO_DTH = "neo_dth.prob",
  NEARMISS = "nearmiss.prob"
)

# Compute and print AUC for each
for (pair in names(outcome_prob_pairs)) {
  prob_col <- outcome_prob_pairs[[pair]]
  auc_val <- auc(roc(data.dl.continuous.Africa.dl[[pair]], 
                     data.dl.continuous.Africa.dl[[prob_col]]))
  cat(pair, "AUC:", round(auc_val, 3), "\n")
}

