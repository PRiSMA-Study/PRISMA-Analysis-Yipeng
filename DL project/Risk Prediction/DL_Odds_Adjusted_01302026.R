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
library(openxlsx)

data.static<-read.csv("D:/Users/yipeng_wei/Documents/dl data/2025-10-31/df_dl_static.csv")
data.temporal.wide<-read.csv("D:/Users/yipeng_wei/Documents/dl data/2025-10-31/df_dl_temporal_wide.csv")

data.static<-data.static%>%filter(!is.na(INFANTID))
data.temporal.wide<-data.temporal.wide%>%filter(!is.na(INFANTID))

variable_static<-c(
  "STILLBIRTH_IND","PRETERM_IND","CESARIAN_IND",
  "NUM_MISCARRIAGE_ind","PAID_WORK","GPARITY_2","GPARITY_1","WEALTH_QUINT_1","WEALTH_QUINT_2","WEALTH_QUINT_3","WEALTH_QUINT_4","SCHOOL_MORE10","WATER_IMPROVED","TOILET_IMPROVED","STOVE_FUEL","HH_SMOKE","SMOKE","CHEW_TOBACCO","CHEW_BETELNUT","CROWDING_IND",
  "MAT_AGE","MUAC_ENROLL",
  "NUM_FETUS_2","NUM_FETUS_3",
  "BMI_LEVEL_ENROLL_underweight","BMI_LEVEL_ENROLL_overweight","BMI_LEVEL_ENROLL_obese",
  "GWG_ADEQUACY_inadequate","GWG_ADEQUACY_excessive",
  "MEM_CES","MEM_ART","MEM_SPON","LABOR_ANY","PRO_LABOR","OBS_LABOR","MAT_CES_ANY","BIRTH_FACILITY",
  "HTN","DIAB_OVERT_ANY",
  "GES_HTN","DIAB_GEST_ANY",
  "INF_SEX","GESTAGEBIRTH_ANY","BWEIGHT_ANY","BREASTFED","INF_PSBI_ANY","INF_HYPERBILI_NICE",
  "DEPR_EVER",
  "HIV_POSITIVE","TB_SYMP_POSITIVE","MAL_POSITIVE","HBV_POSITIVE_ENROLL","HCV_POSITIVE_ENROLL","STI_POSITIVE")

variable_temporal<-c(
  "M01_AFI_PERES_1","M01_AFI_PERES_2","M01_AFI_PERES_3","M01_AFI_PERES_4","M01_AFI_PERES_5",
  "PREVIA_1","PREVIA_2","PREVIA_3","PREVIA_4","PREVIA_5",
  "M08_FERRITIN_LBORRES_1", "M08_FERRITIN_LBORRES_2", "M08_FERRITIN_LBORRES_3", "M08_FERRITIN_LBORRES_4", "M08_FERRITIN_LBORRES_5",
  "M08_IRON_TOT_UGDL_LBORRES_1", "M08_IRON_TOT_UGDL_LBORRES_2", "M08_IRON_TOT_UGDL_LBORRES_3", "M08_IRON_TOT_UGDL_LBORRES_4", "M08_IRON_TOT_UGDL_LBORRES_5",
  "M08_IRON_HEP_LBORRES_1", "M08_IRON_HEP_LBORRES_2", "M08_IRON_HEP_LBORRES_3", "M08_IRON_HEP_LBORRES_4","M08_IRON_HEP_LBORRES_5",
  "M08_CBC_HB_LBORRES_1", "M08_CBC_HB_LBORRES_2", "M08_CBC_HB_LBORRES_3", "M08_CBC_HB_LBORRES_4", "M08_CBC_HB_LBORRES_5",
  "M08_ANEMIA_1", "M08_ANEMIA_2", "M08_ANEMIA_3", "M08_ANEMIA_4", "M08_ANEMIA_5")

# Vector of outcome variables
outcome_vars <- c("STILLBIRTH_SIGNS_LIFE", "LBW2500_ANY", "SVN", "PRETERMBIRTH_LT37","NEO_DTH","NEARMISS")

# Subset relevant columns from data.static
static_data <- data.static %>%
  select(INFANTID, all_of(outcome_vars), all_of(variable_static))

# Merge into data.temporal.wide by PREGID
data.temporal.wide <- data.temporal.wide %>%
  left_join(static_data, by = "INFANTID")

transform_negative_to_zero<-function(variable){
  variable[variable==-1]<-0
  return(variable)
}

normalize_quantile <- function(x) {
  q <- quantile(x, probs = c(0, 1), na.rm = TRUE)
  (x - q[1]) / (q[2] - q[1])
}

data.static[variable_static] <- data.static[variable_static] %>%
  mutate(across(everything(), transform_negative_to_zero))

data.static$BWEIGHT_ANY<-data.static$BWEIGHT_ANY/100

data.temporal.wide[variable_temporal] <- data.temporal.wide[variable_temporal] %>%
  mutate(across(everything(), transform_negative_to_zero))

data.temporal.wide <- data.temporal.wide %>% 
  mutate(
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
        ), ~ .x == 0), na.rm = TRUE) > 0 ~ 0,
      
      # All NA or unknown ~ NA
      TRUE ~ NA_real_
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
    M08_CBC_HB_LBORRES = rowMeans(
      select(., M08_CBC_HB_LBORRES_1:M08_CBC_HB_LBORRES_5),
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
        ), ~ .x == 0), na.rm = TRUE) > 0 ~ 0,
      
      # All NA or unknown ~ NA
      TRUE ~ NA_real_
    ),
  )

data.temporal.wide$M08_FERRITIN_LBORRES<-data.temporal.wide$M08_FERRITIN_LBORRES/10

glm_function <- function(data, predictor) {
  
  outcome_vars <- c("STILLBIRTH_SIGNS_LIFE", "LBW2500_ANY",
                    "SVN", "PRETERMBIRTH_LT37", "NEO_DTH", "NEARMISS")
  
  get_formula <- function(predictor, outcome) {
    if (predictor %in% c("GWG_ADEQUACY_inadequate", "GWG_ADEQUACY_excessive")) {
      as.formula(paste(outcome, "~  SITE + GWG_ADEQUACY_inadequate + GWG_ADEQUACY_excessive + MUAC_ENROLL + SCHOOL_MORE10 + MAT_AGE + PAID_WORK + GPARITY_2 + GPARITY_1"))
    } else if (predictor %in% c("NUM_FETUS_2", "NUM_FETUS_3")) {
      as.formula(paste(outcome, "~ SITE + NUM_FETUS_2 + NUM_FETUS_3 + MUAC_ENROLL + SCHOOL_MORE10 + MAT_AGE + PAID_WORK + GPARITY_2 + GPARITY_1"))
    } else if (predictor %in% c("GPARITY_2", "GPARITY_1")) {
      as.formula(paste(outcome, "~  SITE + GPARITY_2 + GPARITY_1 + MUAC_ENROLL + SCHOOL_MORE10 + MAT_AGE + PAID_WORK + GPARITY_2 + GPARITY_1"))
    } else if (predictor %in% c("WEALTH_QUINT_1", "WEALTH_QUINT_2", "WEALTH_QUINT_3", "WEALTH_QUINT_4")) {
      as.formula(paste(outcome, "~  SITE + WEALTH_QUINT_1 + WEALTH_QUINT_2 + WEALTH_QUINT_3 + WEALTH_QUINT_4 + MUAC_ENROLL + SCHOOL_MORE10 + MAT_AGE + PAID_WORK + GPARITY_2 + GPARITY_1"))
    } else if (predictor %in% c("MUAC_ENROLL","SCHOOL_MORE10","MAT_AGE","PAID_WORK","GPARITY_2","GPARITY_1")) {
      as.formula(paste(outcome, "~  SITE + MUAC_ENROLL + SCHOOL_MORE10 + MAT_AGE + PAID_WORK + GPARITY_2 + GPARITY_1"))
    } else {
      as.formula(paste(outcome, "~", predictor, "+ SITE + MUAC_ENROLL + SCHOOL_MORE10 + MAT_AGE + PAID_WORK + GPARITY_2 + GPARITY_1"))
    }
  }
  
  result_row <- list()
  
  for (outcome in outcome_vars) {
    formula <- get_formula(predictor, outcome)
    
    model <- tryCatch({
      glm(formula, data = data, family = binomial(link = "logit"))
    }, error = function(e) {
      message(paste("Model fitting failed for", predictor, "on", outcome, ":", e$message))
      return(NULL)
    })
    
    if (is.null(model)) {
      result_row[[paste0(outcome, "_ORCI")]] <- NA
      next
    }
    
    coef_summary <- summary(model)$coefficients
    
    if (!(predictor %in% rownames(coef_summary))) {
      result_row[[paste0(outcome, "_ORCI")]] <- NA
      next
    }
    
    estimate <- coef_summary[predictor, "Estimate"]
    se <- coef_summary[predictor, "Std. Error"]
    
    OR <- ifelse(round(exp(estimate), 2) > 10000, "Inf", round(exp(estimate), 2))
    CI_lower <- round(exp(estimate - 1.96 * se), 2)
    CI_upper_raw <- exp(estimate + 1.96 * se)
    CI_upper <- ifelse(CI_upper_raw > 10000, "Inf", round(CI_upper_raw, 2))
    
    result_row[[paste0(outcome, "_ORCI")]] <- paste0(OR, " (", CI_lower, ", ", CI_upper, ")")
  }
  
  return(as.data.frame(result_row, stringsAsFactors = FALSE))
}



static_variables <- list(
  # === Socioeconomic & Demographics
  "### Socioeconomic & Demographics" = "Socioeconomic & Demographics",
  "DEPR_EVER" = "Maternal Depression",
  "PAID_WORK" = "Paid Work",
  "GPARITY_1" = "Parous (1-4)",
  "GPARITY_2" = "Grand Multiparous (>=5)",
  "WEALTH_QUINT_1" = "Wealth Quintile 1 (Poorest)",
  "WEALTH_QUINT_2" = "Wealth Quintile 2 (Poor)",
  "WEALTH_QUINT_3" = "Wealth Quintile 3 (Middle)",
  "WEALTH_QUINT_4" = "Wealth Quintile 4 (Rich)",
  "SCHOOL_MORE10" = "Schooling >= 10 Years",
  "WATER_IMPROVED" = "Improved Water",
  "TOILET_IMPROVED" = "Improved Toilet",
  "STOVE_FUEL" = "Clean Stove Fuel",
  "HH_SMOKE" = "Household Smoking",
  "SMOKE" = "Smoking",
  "CHEW_TOBACCO" = "Chews Tobacco",
  "CHEW_BETELNUT" = "Chews Betel Nut",
  "CROWDING_IND" = "Crowding Index",
  
  # === Maternal Characteristics
  "### Maternal Characteristics" = "Maternal Characteristics",
  "MAT_AGE" = "Maternal Age (Years)",
  "MUAC_ENROLL" = "MUAC at Enrollment (cm)",
  
  # === BMI & Weight Gain
  "### BMI & Weight Gain" = "BMI & Weight Gain",
  "BMI_LEVEL_ENROLL_underweight" = "BMI Underweight (<18.5)",
  "BMI_LEVEL_ENROLL_overweight" = "BMI Overweight (25-29.9)",
  "BMI_LEVEL_ENROLL_obese" = "BMI Obese (>=30)",
  "GWG_ADEQUACY_inadequate" = "Inadequate GWG",
  "GWG_ADEQUACY_excessive" = "Excessive GWG",
  
  # === Pregnancy History
  "### Pregnancy History" = "Pregnancy History",
  "STILLBIRTH_IND" = "Stillbirth History",
  "PRETERM_IND" = "Preterm Birth History",
  "CESARIAN_IND" = "Cesarean Delivery History",
  "NUM_MISCARRIAGE_ind" = "Previous Miscarriages >= 3",
  
  # === Comorbidities
  "### Comorbidities" = "Comorbidities",
  "HTN" = "Hypertension (Chronic)",
  "DIAB_OVERT_ANY" = "Overt Diabetes",
  "HIV_POSITIVE" = "HIV Positive",
  "TB_SYMP_POSITIVE" = "TB Symptoms",
  "MAL_POSITIVE" = "Malaria Positive",
  "HBV_POSITIVE_ENROLL" = "Hepatitis B Positive (Enrollment)",
  "HCV_POSITIVE_ENROLL" = "Hepatitis C Positive (Enrollment)",
  "STI_POSITIVE" = "Any STI Positive",
  
  # === Comorbidities during ANC
  "### Comorbidities during ANC" = "Comorbidities during ANC",
  "GES_HTN" = "Gestational Hypertension",
  "DIAB_GEST_ANY" = "Gestational Diabetes",
  
  # === Delivery
  "###Delivery" = "Delivery",
  "MAT_CES_ANY" = "Cesarean Delivery",
  "BIRTH_FACILITY" = "Delivered at Facility",
  
  # === Infant Outcomes
  "### Infant Outcomes" = "Infant Outcomes",
  "INF_SEX" = "Infant Sex",
  "BREASTFED" = "Exclusively Breastfed",
  "GESTAGEBIRTH_ANY" = "Gestational Age at Birth",
  "BWEIGHT_ANY" = "Birthweight (100g)",
  "INF_ANOMALY" = "Congenital Anomaly",
  "INF_PSBI_ANY" = "PSBI",
  "INF_HYPERBILI_NICE" = "TCB exceeds NICE threshold",
  
  # === Ultrasound
  "### Ultrasound" = "Ultrasound",
  "NUM_FETUS_2" = "Twin Pregnancy",
  "NUM_FETUS_3" = "Triplet Pregnancy"
)

temporal_variables <- list(
  # === Amniotic Fluid Index (AFI)
  "### Amniotic Fluid Index" = "Amniotic Fluid Index",
  "M01_AFI_PERES" = "Amniotic Fluid Index (AFI)",
  
  # === Placenta Previa
  "### Placenta Previa" = "Placenta Previa",
  "PREVIA" = "Placental Previa Anomalies",
  
  # === Ferritin
  "### Ferritin" = "Ferritin",
  "M08_FERRITIN_LBORRES" = "Ferritin (ug/dL)",
  
  # === Hemoglobin
  "### Hemoglobin" = "Hemoglobin",
  "M08_CBC_HB_LBORRES" = "Hemoglobin (g/dL)",
  
  # === Anemia
  "### Moderate or Severe Anemia" = "Moderate or Severe Anemia",
  "M08_ANEMIA" = "Moderate or Severe Anemia"
)

# Initialize result table
result_table_glm <- data.frame(
  Heading = character(),
  `Risk_Factor` = character(),
  `STILLBIRTH_SIGNS_LIFE_ORCI` = character(),
  `LBW2500_ANY_ORCI` = character(),
  `SVN_ORCI` = character(),
  `PRETERMBIRTH_LT37_ORCI` = character(),
  `NEO_DTH_ORCI` = character(),
  `NEARMISS_ORCI` = character(),
  stringsAsFactors = FALSE
)

all_vars <- c(static_variables, temporal_variables)

for (var in names(all_vars)) {
  label <- all_vars[[var]]
  
  if (startsWith(var, "###")) {
    # Interleaved column names
    col_names <- as.vector(rbind(paste0(outcome_vars, "_ORCI")))
    
    # Empty row with proper names
    empty_row <- setNames(as.list(rep(NA, length(col_names))), col_names)
    
    # Append to result table
    result_table_glm <- rbind(result_table_glm, data.frame(
      Heading = label,
      `Risk_Factor` = "",
      empty_row,
      stringsAsFactors = FALSE
    ))
  }else {
    data_source <- if (var %in% names(static_variables)) data.static else data.temporal.wide
    
    row_result <- glm_function(data_source, var)
    
    row_result$Heading <- ""
    row_result$`Risk_Factor` <- label
    
    # Reorder column
    row_result <- row_result[, c("Heading", "Risk_Factor", 
                                 paste0(outcome_vars, "_ORCI"))]
    
    result_table_glm <- rbind(result_table_glm, row_result)
  }
}

col_rename_map <- c(
  "STILLBIRTH_SIGNS_LIFE_ORCI" = "Stillbirth OR (CI)",
  "LBW2500_ANY_ORCI" = "LBW OR (CI)",
  "SVN_ORCI" = "SVN OR (CI)",
  "PRETERMBIRTH_LT37_ORCI" = "Preterm Birth OR (CI)",
  "NEO_DTH_ORCI" = "Neonatal Death OR (CI)",
  "NEARMISS_ORCI" = "Near Miss OR (CI)"
)

colnames(result_table_glm) <- ifelse(
  colnames(result_table_glm) %in% names(col_rename_map),
  col_rename_map[colnames(result_table_glm)],
  colnames(result_table_glm)
)

write.xlsx(result_table_glm, file = "D:/Users/yipeng_wei/Documents/Output/DL/Odds_results_adjusted.xlsx", overwrite = TRUE)