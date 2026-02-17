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


data.dl.continuous$svn.prob<-rep(lr.function(data.dl.continuous.wide.Africa.lr,data.dl.continuous.wide,"SVN"),each=5)
data.dl.continuous$stillbirth.prob<-rep(lr.function(data.dl.continuous.wide.Africa.lr,data.dl.continuous.wide,"STILLBIRTH_SIGNS_LIFE"),each=5)
data.dl.continuous$lbw.prob<-rep(lr.function(data.dl.continuous.wide.Africa.lr,data.dl.continuous.wide,"LBW2500_ANY"),each=5)
data.dl.continuous$preterm.prob<-rep(lr.function(data.dl.continuous.wide.Africa.lr,data.dl.continuous.wide,"PRETERMBIRTH_LT37"),each=5)
data.dl.continuous$neo_dth.prob<-rep(lr.function(data.dl.continuous.wide.Africa.lr,data.dl.continuous.wide,"NEO_DTH"),each=5)
data.dl.continuous$nearmiss.prob<-rep(lr.function(data.dl.continuous.wide.Africa.lr,data.dl.continuous.wide,"NEARMISS"),each=5)

write.csv(data.dl.continuous,"D:/Users/yipeng_wei/Documents/dl data/2025-10-31/df_dl_continuous_lr.csv")
write.csv(data.dl.continuous.Africa.dl,"D:/Users/yipeng_wei/Documents/dl data/2025-10-31/df_dl_continuous_Africa.csv")
write.csv(data.dl.continuous.mask.Africa.dl,"D:/Users/yipeng_wei/Documents/dl data/2025-10-31/df_dl_continuous_mask_Africa.csv")
write.csv(data.dl.continuous.delta.Africa.dl,"D:/Users/yipeng_wei/Documents/dl data/2025-10-31/df_dl_continuous_delta_Africa.csv")

write.csv(data.dl.continuous.Africa.lr,"D:/Users/yipeng_wei/Documents/dl data/2025-10-31/df_dl_continuous_Africa_train.csv")
write.csv(data.dl.continuous.mask.Africa.lr,"D:/Users/yipeng_wei/Documents/dl data/2025-10-31/df_dl_continuous_mask_Africa_train.csv")
write.csv(data.dl.continuous.delta.Africa.lr,"D:/Users/yipeng_wei/Documents/dl data/2025-10-31/df_dl_continuous_delta_Africa_train.csv")

# === Function to print prevalence of binary outcome (0/1) ===
print_prevalence <- function(data1, data2, outcome) {
  outcome_name <- deparse(substitute(outcome))
  
  cat("\n==============================\n")
  cat("Outcome:", outcome_name, "\n")
  cat("==============================\n\n")
  
  # --- Data 1 ---
  cat("Data 1:\n")
  tab1 <- table(data1[[outcome_name]])
  print(tab1)
  n1 <- sum(tab1[names(tab1) %in% c("0", "1")])   # denominator = 0 + 1
  p1 <- ifelse("1" %in% names(tab1), tab1["1"] / n1, 0)
  cat("Prevalence (Data 1):", sprintf("%.2f%%", p1 * 100), "\n\n")
  
  # --- Data 2 ---
  cat("Data 2:\n")
  tab2 <- table(data2[[outcome_name]])
  print(tab2)
  n2 <- sum(tab2[names(tab2) %in% c("0", "1")])   # denominator = 0 + 1
  p2 <- ifelse("1" %in% names(tab2), tab2["1"] / n2, 0)
  cat("Prevalence (Data 2):", sprintf("%.2f%%", p2 * 100), "\n\n")
}

# === print prevalence ===
print_prevalence(data.dl.continuous.wide.Asia, data.dl.continuous.wide.Africa, STILLBIRTH_SIGNS_LIFE)
print_prevalence(data.dl.continuous.wide.Asia, data.dl.continuous.wide.Africa, LBW2500_ANY)
print_prevalence(data.dl.continuous.wide.Asia, data.dl.continuous.wide.Africa, PRETERMBIRTH_LT37)
print_prevalence(data.dl.continuous.wide.Asia, data.dl.continuous.wide.Africa, SVN)
print_prevalence(data.dl.continuous.wide.Asia, data.dl.continuous.wide.Africa, NEARMISS)
print_prevalence(data.dl.continuous.wide.Asia, data.dl.continuous.wide.Africa, NEO_DTH)