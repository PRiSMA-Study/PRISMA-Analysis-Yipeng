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

data.static<-read.csv("D:/Users/yipeng_wei/Documents/dl data/2025-04-18/df_dl_static.csv")
data.temporal.wide<-read.csv("D:/Users/yipeng_wei/Documents/dl data/2025-04-18/df_dl_temporal_wide.csv")

data.static<-data.static%>%filter(!is.na(INFANTID))
data.temporal.wide<-data.temporal.wide%>%filter(!is.na(INFANTID))

variable_static<-c(
  "STILLBIRTH_IND","PRETERM_IND","CESARIAN_IND",
  "NUM_MISCARRIAGE_ind","PAID_WORK","GPARITY_2","GPARITY_1","GPARITY_0","WEALTH_QUINT_1","WEALTH_QUINT_2","WEALTH_QUINT_3","WEALTH_QUINT_4","WEALTH_QUINT_5","SCHOOL_MORE10","WATER_IMPROVED","TOILET_IMPROVED","STOVE_FUEL","HH_SMOKE","SMOKE","CHEW_TOBACCO","CHEW_BETELNUT","CROWDING_IND",
  "MAT_AGE","MUAC_ENROLL",
  "NUM_FETUS_1","NUM_FETUS_2","NUM_FETUS_3",
  "BMI_LEVEL_ENROLL_normal","BMI_LEVEL_ENROLL_underweight","BMI_LEVEL_ENROLL_overweight","BMI_LEVEL_ENROLL_obese",
  "GWG_ADEQUACY_adequate","GWG_ADEQUACY_inadequate","GWG_ADEQUACY_excessive",
  "MAT_CES_ANY","BIRTH_FACILITY",
  "HTN","DIAB_OVERT_ANY",
  "GES_HTN","DIAB_GEST_ANY",
  "INF_SEX","GESTAGEBIRTH_ANY","BWEIGHT_ANY","BREASTFED","INF_PSBI_IPC","INF_HYPERBILI_NICE",
  "DEPR_EVER",
  "HIV_POSITIVE_ENROLL","TB_SYMP_POSITIVE_ENROLL","MAL_POSITIVE_ENROLL","STI_POSITIVE_ENROLL",
  "HIV_POSITIVE_ANY_ANC","TB_SYMP_POSITIVE_ANY_ANC","MAL_POSITIVE_ANY_ANC","HBV_POSITIVE_ANY_ANC","HCV_POSITIVE_ANY_ANC","STI_POSITIVE_ANY_ANC",
  "STILLBIRTH_SIGNS_LIFE", "LBW2500_ANY", "INF_PSBI_IPC", "SVN", "PRETERMBIRTH_LT37","NEO_DTH","NEARMISS")

variable_temporal<-c(
  "M01_AFI_PERES_1","M01_AFI_PERES_2","M01_AFI_PERES_3","M01_AFI_PERES_4","M01_AFI_PERES_5",
  "PREVIA_1","PREVIA_2","PREVIA_3","PREVIA_4","PREVIA_5",
  "M08_FERRITIN_LBORRES_1", "M08_FERRITIN_LBORRES_2", "M08_FERRITIN_LBORRES_3", "M08_FERRITIN_LBORRES_4", "M08_FERRITIN_LBORRES_5",
  "M08_IRON_TOT_UGDL_LBORRES_1", "M08_IRON_TOT_UGDL_LBORRES_2", "M08_IRON_TOT_UGDL_LBORRES_3", "M08_IRON_TOT_UGDL_LBORRES_4", "M08_IRON_TOT_UGDL_LBORRES_5",
  "M08_IRON_HEP_LBORRES_1", "M08_IRON_HEP_LBORRES_2", "M08_IRON_HEP_LBORRES_3", "M08_IRON_HEP_LBORRES_4","M08_IRON_HEP_LBORRES_5")

#####Continuous and categorical variables
variable_continuous<-c("MAT_AGE","MUAC_ENROLL","GESTAGEBIRTH_ANY","BWEIGHT_ANY","CROWDING_IND",
                       "M01_AFI_PERES_1","M01_AFI_PERES_2","M01_AFI_PERES_3","M01_AFI_PERES_4","M01_AFI_PERES_5",
                       "M08_FERRITIN_LBORRES",
                       "M08_FERRITIN_LBORRES_1", "M08_FERRITIN_LBORRES_2", "M08_FERRITIN_LBORRES_3", "M08_FERRITIN_LBORRES_4", "M08_FERRITIN_LBORRES_5",
                       "M08_IRON_TOT_UGDL_LBORRES_1", "M08_IRON_TOT_UGDL_LBORRES_2", "M08_IRON_TOT_UGDL_LBORRES_3", "M08_IRON_TOT_UGDL_LBORRES_4", "M08_IRON_TOT_UGDL_LBORRES_5",
                       "M08_IRON_HEP_LBORRES_1", "M08_IRON_HEP_LBORRES_2", "M08_IRON_HEP_LBORRES_3", "M08_IRON_HEP_LBORRES_4","M08_IRON_HEP_LBORRES_5")

variable_categorical <- c(
  "STILLBIRTH_IND","PRETERM_IND","CESARIAN_IND",
  "NUM_MISCARRIAGE_ind","PAID_WORK","GPARITY_2","GPARITY_1","GPARITY_0","WEALTH_QUINT_1","WEALTH_QUINT_2","WEALTH_QUINT_3","WEALTH_QUINT_4","WEALTH_QUINT_5","SCHOOL_MORE10","WATER_IMPROVED","TOILET_IMPROVED","STOVE_FUEL","HH_SMOKE","SMOKE","CHEW_TOBACCO","CHEW_BETELNUT",
  "NUM_FETUS_1","NUM_FETUS_2","NUM_FETUS_3",
  "BMI_LEVEL_ENROLL_normal","BMI_LEVEL_ENROLL_underweight","BMI_LEVEL_ENROLL_overweight","BMI_LEVEL_ENROLL_obese",
  "GWG_ADEQUACY_adequate","GWG_ADEQUACY_inadequate","GWG_ADEQUACY_excessive",
  "MAT_CES_ANY","BIRTH_FACILITY",
  "HTN","DIAB_OVERT_ANY",
  "GES_HTN","DIAB_GEST_ANY",
  "INF_SEX","BREASTFED","INF_PSBI_IPC","INF_HYPERBILI_NICE",
  "DEPR_EVER",
  "HIV_POSITIVE_ENROLL","TB_SYMP_POSITIVE_ENROLL","MAL_POSITIVE_ENROLL","STI_POSITIVE_ENROLL",
  "HIV_POSITIVE_ANY_ANC","TB_SYMP_POSITIVE_ANY_ANC","MAL_POSITIVE_ANY_ANC","HBV_POSITIVE_ANY_ANC","HCV_POSITIVE_ANY_ANC","STI_POSITIVE_ANY_ANC",
  "STILLBIRTH_SIGNS_LIFE", "LBW2500_ANY", "INF_PSBI_IPC", "SVN", "PRETERMBIRTH_LT37","NEO_DTH","NEARMISS",
  "SMOKE_COMP","INFECTION_POSITIVE_ENROLL","INFECTION_POSITIVE_ANY_ANC")

# Subset relevant columns from data.static
data.static <- data.static %>%
  select(INFANTID, SITE, MOMID, PREGID, all_of(variable_static))

# Merge into data.temporal.wide by PREGID
data.all <- data.temporal.wide %>%
  left_join(data.static, by = c("INFANTID", "SITE", "MOMID", "PREGID"))

transform_negative_to_zero<-function(variable){
  variable[variable==-1]<-0
  return(variable)
}

# Apply to temporal variables
data.all <- data.all %>%
  mutate(across(everything(), transform_negative_to_zero))

data.all <- data.all %>% 
  mutate(
    SMOKE_COMP = case_when(
      # If any variable is 1 ~ Positive
      rowSums(across(c(
        SMOKE, CHEW_TOBACCO, CHEW_BETELNUT
      ), ~ .x == 1), na.rm = TRUE) > 0 ~ 1,
      
      # If none are 1, and at least one is 0 ~ Negative
      rowSums(across(c(
        SMOKE, CHEW_TOBACCO, CHEW_BETELNUT
      ), ~ .x == 1), na.rm = TRUE) == 0 &
        rowSums(across(c(
          SMOKE, CHEW_TOBACCO, CHEW_BETELNUT
        ), ~ .x == 0), na.rm = TRUE) > 0 ~ 0,
      
      # All NA or unknown ~ NA
      TRUE ~ NA_real_
    ),
    INFECTION_POSITIVE_ENROLL = case_when(
      # If any variable is 1 ~ Positive
      rowSums(across(c(
        HIV_POSITIVE_ENROLL, TB_SYMP_POSITIVE_ENROLL,
        MAL_POSITIVE_ENROLL, STI_POSITIVE_ENROLL
      ), ~ .x == 1), na.rm = TRUE) > 0 ~ 1,
      
      # If none are 1, and at least one is 0 ~ Negative
      rowSums(across(c(
        HIV_POSITIVE_ENROLL, TB_SYMP_POSITIVE_ENROLL,
        MAL_POSITIVE_ENROLL, STI_POSITIVE_ENROLL
      ), ~ .x == 1), na.rm = TRUE) == 0 &
        rowSums(across(c(
          HIV_POSITIVE_ENROLL, TB_SYMP_POSITIVE_ENROLL,
          MAL_POSITIVE_ENROLL, STI_POSITIVE_ENROLL
        ), ~ .x == 0), na.rm = TRUE) > 0 ~ 0,
      
      # All NA or unknown ~ NA
      TRUE ~ NA_real_
    ),
    INFECTION_POSITIVE_ANY_ANC = case_when(
      # If any variable is 1 ~ Positive
      rowSums(across(c(
        HIV_POSITIVE_ANY_ANC, TB_SYMP_POSITIVE_ANY_ANC,
        MAL_POSITIVE_ANY_ANC, HBV_POSITIVE_ANY_ANC, HCV_POSITIVE_ANY_ANC, STI_POSITIVE_ANY_ANC
      ), ~ .x == 1), na.rm = TRUE) > 0 ~ 1,
      
      # If none are 1, and at least one is 0 ~ Negative
      rowSums(across(c(
        HIV_POSITIVE_ANY_ANC, TB_SYMP_POSITIVE_ANY_ANC,
        MAL_POSITIVE_ANY_ANC, HBV_POSITIVE_ANY_ANC, HCV_POSITIVE_ANY_ANC, STI_POSITIVE_ANY_ANC
      ), ~ .x == 1), na.rm = TRUE) == 0 &
        rowSums(across(c(
          HIV_POSITIVE_ANY_ANC, TB_SYMP_POSITIVE_ANY_ANC,
          MAL_POSITIVE_ANY_ANC, HBV_POSITIVE_ANY_ANC, HCV_POSITIVE_ANY_ANC, STI_POSITIVE_ANY_ANC
        ), ~ .x == 0), na.rm = TRUE) > 0 ~ 0,
      
      # All NA or unknown ~ NA
      TRUE ~ NA_real_
    ),
    M08_FERRITIN_LBORRES = rowMeans(
      select(., M08_FERRITIN_LBORRES_1:M08_FERRITIN_LBORRES_5),
      na.rm = TRUE
    )
  )

static_variables <- list(
  # === Outcome variables
  "### Outcome variables" = "Outcome variables",
  "STILLBIRTH_SIGNS_LIFE" = "Stillbirth",
  "LBW2500_ANY" = "LBW (<2500g)",
  "INF_PSBI_IPC" = "PSBI at IPC",
  "SVN" = "SVN",
  "PRETERMBIRTH_LT37" = "Preterm Birth",
  "NEO_DTH" = "Neonatal Death",
  "NEARMISS" = "Maternal Nearmiss",
  
  # === Socioeconomic & Demographics
  "### Socioeconomic & Demographics" = "Socioeconomic & Demographics",
  "PAID_WORK" = "Paid Work",
  "SCHOOL_MORE10" = "Schooling >= 10 Years",
  "SMOKE_COMP" = "Smoking, Chews Tobacco/Betel Nut",
  "CROWDING_IND" = "Crowding Index",
  
  # === Parity
  "### Parity" = "Parity",
  "GPARITY_0" = "Nulliparous",
  "GPARITY_1" = "Parous (1-4 prior pregnancies)",
  "GPARITY_2" = "Grand Multiparous (>=5 prior pregnancies)",
  # === Maternal Characteristics
  "### Maternal Characteristics" = "Maternal Characteristics",
  "MAT_AGE" = "Maternal Age (Years)",
  "MUAC_ENROLL" = "MUAC at Enrollment (cm)",
  
  # === Weight Gain
  "### Weight Gain" = "Weight Gain",
  "GWG_ADEQUACY_adequate" = "Adequate GWG",
  "GWG_ADEQUACY_inadequate" = "Inadequate GWG",
  "GWG_ADEQUACY_excessive" = "Excessive GWG",
  
  # === Pregnancy History
  "### Pregnancy History" = "Pregnancy History",
  "STILLBIRTH_IND" = "Stillbirth History",
  "PRETERM_IND" = "Preterm Birth History",
  "CESARIAN_IND" = "Cesarean Delivery History",
  "NUM_MISCARRIAGE_ind" = "Miscarriage (early pregnancy loss >= 3)",
  
  # === Comorbidities at Enrollment
  "### Comorbidities at Enrollment" = "Comorbidities at Enrollment",
  "HTN" = "Hypertension (Chronic)",
  "DIAB_OVERT_ANY" = "Overt Diabetes",
  "INFECTION_POSITIVE_ENROLL" = "Any Infection Positive (Enrollment)",
  
  # === Comorbidities during ANC
  "### Comorbidities during ANC" = "Comorbidities during ANC",
  "GES_HTN" = "Gestational Hypertension",
  "DIAB_GEST_ANY" = "Gestational Diabetes",
  "INFECTION_POSITIVE_ANY_ANC" = "Any Infection Positive (Any ANC)",
  
  # === Delivery
  "###Delivery" = "Delivery",
  "MAT_CES_ANY" = "Cesarean Delivery",
  "BIRTH_FACILITY" = "Delivered at Facility",
  
  # === Infant Outcomes
  "### Infant Outcomes" = "Infant Outcomes",
  "INF_SEX" = "Infant Sex (Male)",
  "BREASTFED" = "Exclusively Breastfed",
  "GESTAGEBIRTH_ANY" = "Gestational Age at Birth (Weeks)",
  "BWEIGHT_ANY" = "Birthweight (g)",
  "INF_ANOMALY" = "Congenital Anomaly",
  "INF_HYPERBILI_NICE" = "TCB exceeds NICE threshold between IPC to PNC1",
  
  # === Ultrasound
  "### Ultrasound" = "Ultrasound",
  "NUM_FETUS_1" = "Singleton",
  "NUM_FETUS_2" = "Twin Pregnancy",
  "NUM_FETUS_3" = "Triplet Pregnancy"
)

temporal_variables <- list(
  # === Ferritin
  "### Ferritin" = "Ferritin",
  "M08_FERRITIN_LBORRES" = "Ferritin during ANC (µg/dL)"
)

# Combine label dictionaries
total_label_dict <- c(static_variables, temporal_variables)

# Order and flatten into a named vector
ordered_vars <- names(total_label_dict)
label_dict <- total_label_dict[!startsWith(ordered_vars, "###")]
section_headers <- total_label_dict[startsWith(ordered_vars, "###")]

# Define summary functions
summarize_continuous <- function(data, var) {
  total_value <- paste0(
    round(mean(data[[var]], na.rm = TRUE), 1), " (",
    round(sd(data[[var]], na.rm = TRUE), 1), ")"
  )
  
  site_values <- data %>%
    group_by(SITE) %>%
    summarise(
      value = paste0(
        round(mean(.data[[var]], na.rm = TRUE), 1), " (",
        round(sd(.data[[var]], na.rm = TRUE), 1), ")"
      ),
      .groups = "drop"
    )
  
  bind_rows(
    data.frame(SITE = "Total", value = total_value, variable = var),
    site_values %>% mutate(variable = var)
  )
}

summarize_categorical <- function(data, var) {
  total_n <- sum(!is.na(data[[var]]))
  total_1 <- sum(data[[var]] == 1, na.rm = TRUE)
  total_pct <- round(100 * total_1 / total_n, 1)
  total_value <- paste0(total_1, " (", total_pct, "%)")
  
  site_values <- data %>%
    group_by(SITE) %>%
    summarise(
      n_1 = sum(.data[[var]] == 1, na.rm = TRUE),
      total = sum(!is.na(.data[[var]])),
      pct = round(100 * n_1 / total, 1),
      value = paste0(n_1, " (", pct, "%)"),
      .groups = "drop"
    )
  
  bind_rows(
    data.frame(SITE = "Total", value = total_value, variable = var),
    site_values %>% mutate(variable = var)
  )
}


# Initialize result table
site_columns <- sort(unique(data.all$SITE))
summary_table <- data.frame(
  Heading = character(),
  Risk_Factor = character(),
  Total = character(),
  matrix(NA_character_, nrow = 0, ncol = length(site_columns)),
  stringsAsFactors = FALSE
)
colnames(summary_table)[4:ncol(summary_table)] <- site_columns


# Loop through variables
all_vars <- c(static_variables, temporal_variables)

for (var in names(all_vars)) {
  label <- all_vars[[var]]
  
  if (startsWith(var, "###")) {
    empty_row <- setNames(as.list(rep(NA, 2 + length(site_columns))),
                          c("Heading", "Risk_Factor", site_columns))
    empty_row[["Heading"]] <- label
    summary_table <- bind_rows(summary_table, empty_row)
    
  } else {
    if (var %in% variable_continuous) {
      values <- summarize_continuous(data.all, var)
    } else if (var %in% variable_categorical) {
      values <- summarize_categorical(data.all, var)
    } else {
      next
    }
    
    value_vec <- setNames(values$value, values$SITE)
    
    row <- setNames(as.list(rep(NA, 2 + 1 + length(site_columns))),
                    c("Heading", "Risk_Factor", "Total", site_columns))
    
    row[["Heading"]] <- ""
    row[["Risk_Factor"]] <- label
    row[["Total"]] <- value_vec[["Total"]]
    
    for (site in site_columns) {
      row[[site]] <- ifelse(site %in% names(value_vec), value_vec[[site]], NA)
    }
    summary_table <- bind_rows(summary_table, row)
  }
}

table(data.all$SITE)

write.xlsx(summary_table, file = "D:/Users/yipeng_wei/Documents/Output/DL/Summary_results.xlsx", overwrite = TRUE)
