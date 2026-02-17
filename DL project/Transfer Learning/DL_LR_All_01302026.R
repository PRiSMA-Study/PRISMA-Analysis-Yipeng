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
data.dl.continuous.wide<- read.csv("D:/Users/yipeng_wei/Documents/dl data/2025-10-31/df_dl_continuous_wide_transfer.csv") %>% mutate(
  INFECTION_POSITIVE = case_when(
    # If any variable is 1 ~ Positive
    rowSums(across(c(
      HIV_POSITIVE, TB_SYMP_POSITIVE, MAL_POSITIVE, HBV_POSITIVE_ENROLL, HCV_POSITIVE_ENROLL, STI_POSITIVE
    ), ~ .x == 1), na.rm = TRUE) > 0 ~ 1,
    
    # If none are 1, and at least one is -1 ~ Negative
    rowSums(across(c(
      HIV_POSITIVE, TB_SYMP_POSITIVE, MAL_POSITIVE, HBV_POSITIVE_ENROLL, HCV_POSITIVE_ENROLL, STI_POSITIVE
    ), ~ .x == 1), na.rm = TRUE) == 0 &
      rowSums(across(c(
      HIV_POSITIVE, TB_SYMP_POSITIVE, MAL_POSITIVE, HBV_POSITIVE_ENROLL, HCV_POSITIVE_ENROLL, STI_POSITIVE
      ), ~ .x == -1), na.rm = TRUE) > 0 ~ -1,
    
    # All NA or unknown ~ NA
    TRUE ~ 0
  ),
  M01_AFI_PERES = rowMeans(
    select(., M01_AFI_PERES_1:M01_AFI_PERES_5),
    na.rm = TRUE
  ),
  PREVIA = case_when(
    # If any variable is 1 ~ Positive
    rowSums(across(c(
      PREVIA_1, PREVIA_2, PREVIA_3, PREVIA_4, PREVIA_5
    ), ~ .x == 1), na.rm = TRUE) > 0 ~ 1,
    
    # If none are 1, and at least one is 0 ~ Negative
    rowSums(across(c(
      PREVIA_1, PREVIA_2, PREVIA_3, PREVIA_4, PREVIA_5
    ), ~ .x == 1), na.rm = TRUE) == 0 &
      rowSums(across(c(
        PREVIA_1, PREVIA_2, PREVIA_3, PREVIA_4, PREVIA_5
      ), ~ .x == 0), na.rm = TRUE) > 0 ~ -1,
    
    # All NA or unknown ~ NA
    TRUE ~ 0
  ),
  M08_FERRITIN_LBORRES = rowMeans(
    select(., M08_FERRITIN_LBORRES_1:M08_FERRITIN_LBORRES_5),
    na.rm = TRUE
  ),
  M08_IRON_TOT_UGDL_LBORRES = rowMeans(
    select(., M08_IRON_TOT_UGDL_LBORRES_1:M08_IRON_TOT_UGDL_LBORRES_5),
    na.rm = TRUE
  ),
  M08_IRON_HEP_LBORRES = rowMeans(
    select(., M08_IRON_HEP_LBORRES_1:M08_IRON_HEP_LBORRES_5),
    na.rm = TRUE
  ),
  M08_ANEMIA = case_when(
    # If any variable is 1 ~ Positive
    rowSums(across(c(
      M08_ANEMIA_1, M08_ANEMIA_2, M08_ANEMIA_3, M08_ANEMIA_4, M08_ANEMIA_5
    ), ~ .x == 1), na.rm = TRUE) > 0 ~ 1,
    
    # If none are 1, and at least one is 0 ~ Negative
    rowSums(across(c(
      M08_ANEMIA_1, M08_ANEMIA_2, M08_ANEMIA_3, M08_ANEMIA_4, M08_ANEMIA_5
    ), ~ .x == 1), na.rm = TRUE) == 0 &
      rowSums(across(c(
        M08_ANEMIA_1, M08_ANEMIA_2, M08_ANEMIA_3, M08_ANEMIA_4, M08_ANEMIA_5
      ), ~ .x == 0), na.rm = TRUE) > 0 ~ -1,
    
    # All NA or unknown ~ NA
    TRUE ~ 0
  ),
)

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

# Logistic regression function
lr.function <- function(data1,data2,outcome) {
  # Define features based on outcome
  if (outcome == "STILLBIRTH_SIGNS_LIFE") {
    features_lr <- c("STILLBIRTH_IND","PRETERM_IND","CESARIAN_IND",
                     "NUM_MISCARRIAGE","PAID_WORK","GPARITY_2","GPARITY_1","WEALTH_QUINT_1","WEALTH_QUINT_2","WEALTH_QUINT_3","WEALTH_QUINT_4","SCHOOL_MORE10","WATER_IMPROVED","TOILET_IMPROVED","STOVE_FUEL","HH_SMOKE","SMOKE","CHEW_TOBACCO","CHEW_BETELNUT","CROWDING_IND",
                     "MAT_AGE","BMI_ENROLL","MUAC_ENROLL","NUM_FETUS","GWG_ADEQUACY_inadequate","GWG_ADEQUACY_excessive",
                     "HTN","DIAB_OVERT_ANY",
                     "GES_HTN","DIAB_GEST_ANY",
                     "MAT_CES_ANY","BIRTH_FACILITY",
                     "INF_SEX","GESTAGEBIRTH_ANY",
                     "DEPR_EVER",
                     "INFECTION_POSITIVE",
                     "M01_AFI_PERES",
                     "PREVIA",
                     "M08_FERRITIN_LBORRES",
                     "M08_IRON_TOT_UGDL_LBORRES", 
                     "M08_IRON_HEP_LBORRES",
                     "M08_ANEMIA")
  } else if (outcome == "PRETERMBIRTH_LT37") {
    features_lr <- c("STILLBIRTH_IND","PRETERM_IND","CESARIAN_IND",
                     "NUM_MISCARRIAGE","PAID_WORK","GPARITY_2","GPARITY_1","WEALTH_QUINT_1","WEALTH_QUINT_2","WEALTH_QUINT_3","WEALTH_QUINT_4","SCHOOL_MORE10","WATER_IMPROVED","TOILET_IMPROVED","STOVE_FUEL","HH_SMOKE","SMOKE","CHEW_TOBACCO","CHEW_BETELNUT","CROWDING_IND",
                     "MAT_AGE","BMI_ENROLL","MUAC_ENROLL","NUM_FETUS","GWG_ADEQUACY_inadequate","GWG_ADEQUACY_excessive",
                     "HTN","DIAB_OVERT_ANY",
                     "GES_HTN","DIAB_GEST_ANY",
                     "INF_SEX",
                     "DEPR_EVER",
                     "INFECTION_POSITIVE",
                     "M01_AFI_PERES",
                     "PREVIA",
                     "M08_FERRITIN_LBORRES",
                     "M08_IRON_TOT_UGDL_LBORRES", 
                     "M08_IRON_HEP_LBORRES",
                     "M08_ANEMIA")
  } else if (outcome == "LBW2500_ANY") {
    features_lr <- c("STILLBIRTH_IND","PRETERM_IND","CESARIAN_IND",
                     "NUM_MISCARRIAGE","PAID_WORK","GPARITY_2","GPARITY_1","WEALTH_QUINT_1","WEALTH_QUINT_2","WEALTH_QUINT_3","WEALTH_QUINT_4","SCHOOL_MORE10","WATER_IMPROVED","TOILET_IMPROVED","STOVE_FUEL","HH_SMOKE","SMOKE","CHEW_TOBACCO","CHEW_BETELNUT","CROWDING_IND",
                     "MAT_AGE","BMI_ENROLL","MUAC_ENROLL","NUM_FETUS","GWG_ADEQUACY_inadequate","GWG_ADEQUACY_excessive",
                     "HTN","DIAB_OVERT_ANY",
                     "GES_HTN","DIAB_GEST_ANY",
                     "INF_SEX","GESTAGEBIRTH_ANY",
                     "DEPR_EVER",
                     "INFECTION_POSITIVE",
                     "M01_AFI_PERES",
                     "PREVIA",
                     "M08_FERRITIN_LBORRES",
                     "M08_IRON_TOT_UGDL_LBORRES", 
                     "M08_IRON_HEP_LBORRES",
                     "M08_ANEMIA")
  } else if (outcome == "INF_PSBI_OUTCOME") {
    features_lr <- c("STILLBIRTH_IND","PRETERM_IND","CESARIAN_IND",
                     "NUM_MISCARRIAGE","PAID_WORK","GPARITY_2","GPARITY_1","WEALTH_QUINT_1","WEALTH_QUINT_2","WEALTH_QUINT_3","WEALTH_QUINT_4","SCHOOL_MORE10","WATER_IMPROVED","TOILET_IMPROVED","STOVE_FUEL","HH_SMOKE","SMOKE","CHEW_TOBACCO","CHEW_BETELNUT","CROWDING_IND",
                     "MAT_AGE","BMI_ENROLL","MUAC_ENROLL","NUM_FETUS","GWG_ADEQUACY_inadequate","GWG_ADEQUACY_excessive",
                     "MAT_CES_ANY","BIRTH_FACILITY",
                     "HTN","DIAB_OVERT_ANY",
                     "GES_HTN","DIAB_GEST_ANY",
                     "INF_SEX","GESTAGEBIRTH_ANY","BWEIGHT_ANY","BREASTFED",
                     "DEPR_EVER",
                     "INFECTION_POSITIVE",
                     "M01_AFI_PERES",
                     "M08_FERRITIN_LBORRES",
                     "M08_IRON_TOT_UGDL_LBORRES", 
                     "M08_IRON_HEP_LBORRES",
                     "M08_ANEMIA")
  } else if (outcome == "SVN") {
    features_lr <- c("STILLBIRTH_IND","PRETERM_IND","CESARIAN_IND",
                     "NUM_MISCARRIAGE","PAID_WORK","GPARITY_2","GPARITY_1","WEALTH_QUINT_1","WEALTH_QUINT_2","WEALTH_QUINT_3","WEALTH_QUINT_4","SCHOOL_MORE10","WATER_IMPROVED","TOILET_IMPROVED","STOVE_FUEL","HH_SMOKE","SMOKE","CHEW_TOBACCO","CHEW_BETELNUT","CROWDING_IND",
                     "MAT_AGE","BMI_ENROLL","MUAC_ENROLL","NUM_FETUS","GWG_ADEQUACY_inadequate","GWG_ADEQUACY_excessive",
                     "HTN","DIAB_OVERT_ANY",
                     "GES_HTN","DIAB_GEST_ANY",
                     "DEPR_EVER",
                     "INFECTION_POSITIVE",
                     "M01_AFI_PERES",
                     "PREVIA",
                     "M08_FERRITIN_LBORRES",
                     "M08_IRON_TOT_UGDL_LBORRES", 
                     "M08_IRON_HEP_LBORRES",
                     "M08_ANEMIA")
  } else if (outcome == "NEO_DTH") {
    features_lr <- c("STILLBIRTH_IND","PRETERM_IND","CESARIAN_IND",
                     "NUM_MISCARRIAGE","PAID_WORK","GPARITY_2","GPARITY_1","WEALTH_QUINT_1","WEALTH_QUINT_2","WEALTH_QUINT_3","WEALTH_QUINT_4","SCHOOL_MORE10","WATER_IMPROVED","TOILET_IMPROVED","STOVE_FUEL","HH_SMOKE","SMOKE","CHEW_TOBACCO","CHEW_BETELNUT","CROWDING_IND",
                     "MAT_AGE","BMI_ENROLL","MUAC_ENROLL","NUM_FETUS","GWG_ADEQUACY_inadequate","GWG_ADEQUACY_excessive",
                     "MAT_CES_ANY","BIRTH_FACILITY",
                     "HTN","DIAB_OVERT_ANY",
                     "GES_HTN","DIAB_GEST_ANY",
                     "INF_SEX","GESTAGEBIRTH_ANY","BWEIGHT_ANY","BREASTFED","INF_PSBI_ANY","INF_HYPERBILI_NICE",
                     "DEPR_EVER",
                     "INFECTION_POSITIVE",
                     "M01_AFI_PERES",
                     "PREVIA",
                     "M08_FERRITIN_LBORRES",
                     "M08_IRON_TOT_UGDL_LBORRES", 
                     "M08_IRON_HEP_LBORRES",
                     "M08_ANEMIA")
  } else if (outcome == "NEARMISS") {
    features_lr <- c("STILLBIRTH_IND","PRETERM_IND","CESARIAN_IND",
                     "NUM_MISCARRIAGE","PAID_WORK","GPARITY_2","GPARITY_1","WEALTH_QUINT_1","WEALTH_QUINT_2","WEALTH_QUINT_3","WEALTH_QUINT_4","SCHOOL_MORE10","WATER_IMPROVED","TOILET_IMPROVED","STOVE_FUEL","HH_SMOKE","SMOKE","CHEW_TOBACCO","CHEW_BETELNUT","CROWDING_IND",
                     "MAT_AGE","BMI_ENROLL","MUAC_ENROLL","NUM_FETUS","GWG_ADEQUACY_inadequate","GWG_ADEQUACY_excessive",
                     "MAT_CES_ANY","BIRTH_FACILITY",
                     "HTN","DIAB_OVERT_ANY",
                     "GES_HTN","DIAB_GEST_ANY",
                     "DEPR_EVER",
                     "INFECTION_POSITIVE",
                     "M01_AFI_PERES",
                     "PREVIA",
                     "M08_FERRITIN_LBORRES",
                     "M08_IRON_TOT_UGDL_LBORRES", 
                     "M08_IRON_HEP_LBORRES",
                     "M08_ANEMIA")
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