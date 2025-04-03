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

transform_negative_to_NA<-function(variable){
  variable[variable==-7]<-NA
  variable[variable==-5]<-NA
  variable[variable==-6]<-NA
  variable[variable==-9]<-NA
  return(variable)
}

transform_to_NA<-function(variable){
  variable[variable==77]<-NA
  variable[variable==55]<-NA
  variable[variable==66]<-NA
  variable[variable==88]<-NA
  variable[variable==99]<-NA
  return(variable)
}

transform_date_to_NA<-function(variable){
  variable[variable < 0]<-NA
  variable[variable > 730]<-NA
  return(variable)
}

transform_continous_to_NA<-function(variable){
  variable[variable < 0]<-NA
  variable[variable > 5000]<-NA
  return(variable)
}

#Site names
site_names <- c("Ghana", "India-CMC", "India-SAS", "Kenya", "Pakistan", "Zambia")

###Import data
UploadDate = "2025-01-10"
path_to_data <- paste0('D:/Users/yipeng_wei/Documents/Stacked data/',UploadDate)# - for AWS data

uploadDate <- as.Date(UploadDate, format = "%Y-%m-%d")
setwd(path_to_data)

###Load data
########################################################################################
recreate_mnh_list <- FALSE #if I want to recreate the big data list then set this to TRUE
########################################################################################
tic <- Sys.time()
if(recreate_mnh_list == TRUE){
  mnh_names <- c()
  list_mnh <- dir(path = path_to_data, pattern = "*.csv", full.names = TRUE) #creates a list of all the csv files in the directory
  for (data_file in list_mnh[]) { #can test by just bringing in a small number (add 1:2 inside the bracket to do so)
    form_name <- substr(basename(data_file), 1,5) #substr pulls out the 1:5 spaces in a char (will pull out "mnh00" etc);
    #basename() pulls out just the name of the file from the entire directory/path.
    print(paste("Reading", form_name))
    assign(form_name, read.csv(data_file))
    mnh_names <- c(mnh_names, form_name) # it's buidling up a vector of obj names.
  }
  save(list=mnh_names, file = paste0("MNH_dfs_", basename(path_to_data), ".RDS"))
} else{
  load(file = paste0('MNH_dfs_',  basename(path_to_data), ".RDS"))
}

toc <- Sys.time()
print(toc - tic)

###Create a vector of women who are enrolled. 
###Get ReMAPP full cohort population
mnh02 <- mnh02 %>% 
  mutate(ENROLLED_FF = ifelse(M02_AGE_IEORRES==1 & M02_PC_IEORRES==1 & 
                                M02_CATCHMENT_IEORRES==1 & M02_CATCH_REMAIN_IEORRES==1 & 
                                M02_CONSENT_IEORRES==1, 1,0),) %>% filter(ENROLLED_FF ==1) # THIS DOES DROP SOME WOMEN!

### Hard code TYPE_VISIT for M00=1, M02=1, M03=1, M09=6, M10=6, M11=6, M17=6, M18=12 ***
if (exists("mnh00")) {
  mnh00 <- mnh00 %>% 
    mutate(M00_TYPE_VISIT = 1)
} else{
  "MNH00 dataframe doesn't exist"
}

if (exists("mnh02")) {
  mnh02 <- mnh02 %>% 
    mutate(M02_TYPE_VISIT = 1)
} else{
  "MNH02 dataframe doesn't exist"
}

if (exists("mnh03")) {
  mnh03 <- mnh03 %>% 
    mutate(M03_TYPE_VISIT = 1)
} else{
  "MNH03 dataframe doesn't exist"
}

if (exists("mnh09")) {
  mnh09 <- mnh09 %>% 
    mutate(M09_TYPE_VISIT = 6)
} else{
  "MNH09 dataframe doesn't exist"
}

if (exists("mnh10")) {
  mnh10 <- mnh10 %>% 
    mutate(M10_TYPE_VISIT = 6)
} else{
  "MNH10 dataframe doesn't exist"
}

if (exists("mnh11")) {
  mnh11 <- mnh11 %>% 
    mutate(M11_TYPE_VISIT = 6)
} else{
  "MNH11 dataframe doesn't exist"
}

if (exists("mnh16")) {
  mnh16 <- mnh16 %>% 
    mutate(M16_TYPE_VISIT = 5)
} else{
  "MNH16 dataframe doesn't exist"
}

if (exists("mnh17")) {
  mnh17 <- mnh17 %>% 
    mutate(M17_TYPE_VISIT = 6)
} else{
  "MNH17 dataframe doesn't exist"
}

if (exists("mnh18")) {
  mnh18 <- mnh18 %>% 
    mutate(M18_TYPE_VISIT = 12)
} else{
  "MNH18 dataframe doesn't exist"
}

if (exists("mnh19")) {
  mnh19 <- mnh19 %>% 
    mutate(M19_TYPE_VISIT = 13)
} else{
  "MNH19 dataframe doesn't exist"
}


mnh03 <- mnh03 %>% rename(TYPE_VISIT = M03_TYPE_VISIT)
mnh04 <- mnh04 %>% rename(TYPE_VISIT = M04_TYPE_VISIT)
mnh08 <- mnh08 %>% rename(TYPE_VISIT = M08_TYPE_VISIT)

mnh03 <- mnh03 %>%
  filter(MOMID!="") %>% #Moved filter to the end so we can create all the variables with TYPE_VISIT at the end of the var name before rows get dropped out
  drop_na(MOMID, PREGID) %>%
  distinct (MOMID, PREGID, SITE, TYPE_VISIT, .keep_all = TRUE)

mnh04 <- mnh04 %>%
  filter(MOMID!="") %>% #Moved filter to the end so we can create all the variables with TYPE_VISIT at the end of the var name before rows get dropped out
  drop_na(MOMID, PREGID) %>%
  distinct (MOMID, PREGID, SITE, TYPE_VISIT, .keep_all = TRUE)%>% 
  filter(TYPE_VISIT %in% c(1,2,3,4,5))

mnh08 <- mnh08 %>%
  filter(MOMID!="") %>% #Moved filter to the end so we can create all the variables with TYPE_VISIT at the end of the var name before rows get dropped out
  drop_na(MOMID, PREGID) %>%
  distinct (MOMID, PREGID, SITE, TYPE_VISIT, .keep_all = TRUE)%>% 
  filter(TYPE_VISIT %in% c(1,2,3,4,5))

data.enroll <- read_dta("D:/Users/yipeng_wei/Documents/dl data/2025-01-10/MAT_ENROLL.dta")

merged_df03 <- left_join(data.enroll, mnh03, by = c("SITE", "MOMID", "PREGID"))
merged_df04 <- left_join(data.enroll, mnh04, by = c("SITE", "MOMID", "PREGID"))
merged_df08 <- left_join(data.enroll, mnh08, by = c("SITE", "MOMID", "PREGID"))

merged_df08 <- merged_df08 %>% mutate(
  #GA days
  M08_GA_days = transform_date_to_NA(as.numeric(as.Date(M08_LBSTDAT) - as.Date(PREG_START_DATE))),
  across(c(M08_CBC_MCV_LBORRES, M08_VITB12_COB_LBORRES, M08_VITB12_HOL_LBORRES, 
           M08_FOLATE_PLASMA_NMOLL_LBORRES, M08_IRON_TOT_UGDL_LBORRES, 
           M08_VITA_UGDL_LBORRES, M08_FERRITIN_LBORRES, M08_IODINE_LBORRES, 
           M08_RBP4_LBORRES, M08_CRP_LBORRES, M08_AGP_LBORRES,
           M08_ZINC_LBORRES,M08_IRON_HEP_LBORRES,
           M08_CBC_HB_LBORRES, M08_HBA1C_LBORRES), 
         ~ transform_continous_to_NA(.)),
  M08_HBA1C_LBORRES = transform_negative_to_NA(M08_HBA1C_LBORRES)
) %>%
  dplyr::select("MOMID", "PREGID", "SITE", PREG_START_DATE, TYPE_VISIT, M08_LBSTDAT, M08_GA_days,
                M08_CBC_MCV_LBORRES, M08_VITB12_COB_LBORRES, M08_VITB12_HOL_LBORRES,
                M08_FOLATE_PLASMA_NMOLL_LBORRES,
                M08_ZINC_LBORRES,M08_IRON_HEP_LBORRES,
                M08_IRON_TOT_UGDL_LBORRES,
                M08_VITA_UGDL_LBORRES,
                #M08_FOLATE_RBC_NMOLL_LBORRES,
                M08_FERRITIN_LBORRES,M08_IODINE_LBORRES,M08_RBP4_LBORRES,M08_CRP_LBORRES,M08_AGP_LBORRES,
                M08_CBC_HB_LBORRES,M08_HBA1C_LBORRES)

merged_df04<- merged_df04 %>%
  mutate(
    #GA days
    M04_GA_days = transform_date_to_NA(as.numeric(as.Date(M04_ANC_OBSSTDAT) - as.Date(PREG_START_DATE))),
    #Iron supplement
    # Iron supplement
    M04_IRON_CMOCCUR = case_when(
      M04_IRON_CMOCCUR == 1 ~ 1,
      M04_IRON_CMOCCUR == 0 ~ -1,
      TRUE ~ NA_real_
    ),
    M04_IRON_ORAL_CMOCCUR = case_when(
      M04_IRON_ORAL_CMOCCUR == 1 ~ 1,
      M04_IRON_ORAL_CMOCCUR == 0 ~ -1,
      TRUE ~ NA_real_
    ),
    M04_IRON_IV_CMOCCUR = case_when(
      M04_IRON_IV_CMOCCUR == 1 ~ 1,
      M04_IRON_IV_CMOCCUR == 0 ~ -1,
      TRUE ~ NA_real_
    ),
    # Folic acid supplement
    M04_IFA_CMOCCUR = case_when(
      M04_IFA_CMOCCUR == 1 ~ 1,
      M04_IFA_CMOCCUR == 0 ~ -1,
      TRUE ~ NA_real_
    ),
    
    # Calcium supplement
    M04_CALCIUM_CMOCCUR = case_when(
      M04_CALCIUM_CMOCCUR == 1 ~ 1,
      M04_CALCIUM_CMOCCUR == 0 ~ -1,
      TRUE ~ NA_real_
    ),
    
    # Vitamin A supplement
    M04_VITAMIN_A_CMOCCUR = case_when(
      M04_VITAMIN_A_CMOCCUR == 1 ~ 1,
      M04_VITAMIN_A_CMOCCUR == 0 ~ -1,
      TRUE ~ NA_real_
    ),
    
    # Multiple micronutrient supplement
    M04_MICRONUTRIENT_CMOCCUR = case_when(
      M04_MICRONUTRIENT_CMOCCUR == 1 ~ 1,
      M04_MICRONUTRIENT_CMOCCUR == 0 ~ -1,
      TRUE ~ NA_real_
    ),
    
    # Anthelmintic treatment
    M04_ANTHELMINTHIC_CMOCCUR = case_when(
      M04_ANTHELMINTHIC_CMOCCUR == 1 ~ 1,
      M04_ANTHELMINTHIC_CMOCCUR == 0 ~ -1,
      TRUE ~ NA_real_
    ),
    STILLBIRTH_IND = case_when(
      M04_STILLBIRTH_RPORRES == 1 ~ 1, #stillbirth,
      M04_PH_PREV_RPORRES == 0 | M04_PH_OTH_RPORRES == 0 | M04_STILLBIRTH_RPORRES == 0 ~ -1,
      TRUE ~ NA_real_
    ),
    ASPHYXIA_IND = case_when(
      M04_BIRTH_ASPHYXIA_MHOCCUR == 1 ~ 1, #asphyxia,
      M04_PH_PREV_RPORRES == 0 | M04_BIRTH_ASPHYXIA_MHOCCUR == 0 ~ -1,
      TRUE ~ NA_real_
    ),
    PRETERM_IND = case_when(
      M04_PRETERM_RPORRES == 1 ~ 1, #preterm birth,
      M04_PH_PREV_RPORRES == 0 | M04_PRETERM_RPORRES == 0 ~ -1,
      TRUE ~ NA_real_
    ),
    POSTTERM_IND = case_when(
      M04_POSTTERM_RPORRES == 1 ~ 1, #postterm birth,
      M04_PH_PREV_RPORRES == 0 | M04_POSTTERM_RPORRES == 0 ~ -1,
      TRUE ~ NA_real_
    ),
    GEST_HTN_IND = case_when(
      M04_GEST_HTN_RPORRES == 1 ~ 1, #Gestational hypertension,
      M04_PH_PREV_RPORRES == 0 | M04_GEST_HTN_RPORRES == 0 ~ -1,
      TRUE ~ NA_real_
    ),
    PREECLAMPSIA_IND = case_when(
      M04_PREECLAMPSIA_RPORRES == 1 ~ 1, #Preeclampsia,
      M04_PH_PREV_RPORRES == 0 | M04_PREECLAMPSIA_RPORRES == 0 ~ -1,
      TRUE ~ NA_real_
    ),
    GEST_DIAB_IND = case_when(
      M04_GEST_DIAB_RPORRES == 1 ~ 1, #Gestational diabetes,
      M04_PH_PREV_RPORRES == 0 | M04_GEST_DIAB_RPORRES == 0 ~ -1,
      TRUE ~ NA_real_
    ),
    PREMATURE_RUPTURE_IND = case_when(
      M04_PREMATURE_RUPTURE_RPORRES == 1 ~ 1, #Premature rupture of membranes,
      M04_PH_PREV_RPORRES == 0 | M04_PREMATURE_RUPTURE_RPORRES == 0 ~ -1,
      TRUE ~ NA_real_
    ),
    OBSTR_LABOR_IND = case_when(
      M04_OBSTR_LABOR_RPORRES == 1 ~ 1, #Obstructed or prolonged labor,
      M04_PH_PREV_RPORRES == 0 | M04_OBSTR_LABOR_RPORRES == 0 ~ -1,
      TRUE ~ NA_real_
    )
  ) %>% dplyr::select( "MOMID", "PREGID", "SITE", PREG_START_DATE, TYPE_VISIT, M04_ANC_OBSSTDAT, M04_GA_days,
         #Iron supplement
         M04_IRON_CMOCCUR,
         M04_IRON_ORAL_CMOCCUR,
         M04_IRON_IV_CMOCCUR,
         #Folic acid supplement
         M04_IFA_CMOCCUR,
         #Calcium supplement
         M04_CALCIUM_CMOCCUR,
         #Vitamin A supplement
         M04_VITAMIN_A_CMOCCUR,
         #Multiple micronutrient supplement
         M04_MICRONUTRIENT_CMOCCUR,
         #Anthelmintic treatment
         M04_ANTHELMINTHIC_CMOCCUR,
         #Birth asphyxia
         ASPHYXIA_IND,
         #Stillbirth
         STILLBIRTH_IND,
         #Pre-term delivery
         PRETERM_IND,
         #post-term delivery
         POSTTERM_IND,
         #Gestational hypertension
         GEST_HTN_IND,
         #Preeclampsia/eclampsia
         PREECLAMPSIA_IND,
         #Gestational diabetes
         GEST_DIAB_IND,
         #Premature rupture of membranes
         PREMATURE_RUPTURE_IND,
         #Obstructed or prolonged labor
         OBSTR_LABOR_IND,)

df_history <- merged_df04 %>% filter(TYPE_VISIT==1) %>% 
  dplyr::select( "MOMID", "PREGID", "SITE",
                "ASPHYXIA_IND","STILLBIRTH_IND","PRETERM_IND","POSTTERM_IND","GEST_HTN_IND","PREECLAMPSIA_IND","GEST_DIAB_IND","PREMATURE_RUPTURE_IND","OBSTR_LABOR_IND")

df_supplement <- merged_df04 %>%
  dplyr::select( "MOMID", "PREGID", "SITE", PREG_START_DATE, TYPE_VISIT, M04_ANC_OBSSTDAT, M04_GA_days,
                #Iron supplement
                M04_IRON_CMOCCUR,
                M04_IRON_ORAL_CMOCCUR,
                M04_IRON_IV_CMOCCUR,
                #Folic acid supplement
                M04_IFA_CMOCCUR,
                #Calcium supplement
                M04_CALCIUM_CMOCCUR,
                #Vitamin A supplement
                M04_VITAMIN_A_CMOCCUR,
                #Multiple micronutrient supplement
                M04_MICRONUTRIENT_CMOCCUR,
                #Anthelmintic treatment
                M04_ANTHELMINTHIC_CMOCCUR)

  
df_fuel <- merged_df03 %>% 
  mutate(
    #clean fuel
    M03_STOVE_FCORRES = transform_to_NA(M03_STOVE_FCORRES),
    STOVE_FUEL = case_when(
      M03_STOVE_FCORRES %in% c(1,2,3,4,5,10) ~ 1,
      M03_STOVE_FCORRES %in% c(7,8,9,11) ~ -1,
      M03_STOVE_FCORRES==6 & M03_STOVE_FUEL_FCORRES_1==1 ~ 1,
      M03_STOVE_FCORRES==6 & M03_STOVE_FUEL_FCORRES_2==1 ~ 1,
      M03_STOVE_FCORRES==6 & M03_STOVE_FUEL_FCORRES_3==1 ~ 1,
      M03_STOVE_FCORRES==6 & M03_STOVE_FUEL_FCORRES_4==1 ~ -1,
      M03_STOVE_FCORRES==6 & M03_STOVE_FUEL_FCORRES_5==1 ~ -1,
      M03_STOVE_FCORRES==6 & M03_STOVE_FUEL_FCORRES_6==1 ~ -1,
      M03_STOVE_FCORRES==6 & M03_STOVE_FUEL_FCORRES_7==1 ~ -1,
      M03_STOVE_FCORRES==6 & M03_STOVE_FUEL_FCORRES_8==1 ~ -1,
      M03_STOVE_FCORRES==6 & M03_STOVE_FUEL_FCORRES_9==1 ~ -1,
      M03_STOVE_FCORRES==6 & M03_STOVE_FUEL_FCORRES_10==1 ~ -1,
      M03_STOVE_FCORRES==6 & M03_STOVE_FUEL_FCORRES_11==1 ~ -1,
      M03_STOVE_FCORRES==6 & M03_STOVE_FUEL_FCORRES_12==1 ~ -1,
      M03_STOVE_FCORRES==6 & M03_STOVE_FUEL_FCORRES_13==1 ~ -1,
      M03_STOVE_FCORRES==6 & M03_STOVE_FUEL_FCORRES_14==1 ~ -1,
      M03_STOVE_FCORRES==6 & M03_STOVE_FUEL_FCORRES_15==1 ~ 1,
      TRUE ~ NA_real_),
  ) %>% dplyr::select( "MOMID", "PREGID", "SITE", TYPE_VISIT,
                      STOVE_FUEL)

df_inf_outcome<-read.csv("D:/Users/yipeng_wei/Documents/dl data/2025-01-10/INF_OUTCOMES.csv")
data.risk <- read_dta("D:/Users/yipeng_wei/Documents/dl data/2025-01-10/MAT_RISKS.dta")
data.demographic<-read.csv("D:/Users/yipeng_wei/Documents/dl data/2025-01-10/MAT_DEMOGRAPHIC.csv")
data.demographic <- distinct(data.demographic, SITE, PREGID,.keep_all = TRUE)
data.infection<-read.csv("D:/Users/yipeng_wei/Documents/dl data/2025-01-10/MAT_INFECTION.csv")
data.infection <- distinct(data.infection, SITE, MOMID,.keep_all = TRUE)
data.hemorr <- read.csv("D:/Users/yipeng_wei/Documents/dl data/2025-01-10/MAT_HEMORRHAGE.csv")
data.labor <- read_dta("D:/Users/yipeng_wei/Documents/dl data/2025-01-10/MAT_LABOR.dta")
data.preterm <- read_dta("D:/Users/yipeng_wei/Documents/dl data/2025-01-10/MAT_PRETERM.dta")
df_inf_presentation <- read_dta("D:/Users/yipeng_wei/Documents/dl data/2025-01-10/INF_PRESENTATION.dta")

data.static <- df_inf_outcome %>% 
  left_join(df_inf_presentation, by = c("SITE","MOMID", "PREGID","INFANTID")) %>%
  left_join(data.risk, by = c("SITE", "MOMID", "PREGID")) %>%
  left_join(data.demographic, by = c("SITE", "MOMID", "PREGID")) %>%
  left_join(data.infection, by = c("SITE", "MOMID", "PREGID")) %>%
  left_join(data.hemorr, by = c("SITE", "MOMID", "PREGID")) %>%
  left_join(data.labor, by = c("SITE", "MOMID", "PREGID")) %>%
  left_join(data.preterm, by = c("SITE", "MOMID", "PREGID")) %>%
  left_join(df_fuel, by = c("SITE", "MOMID", "PREGID")) %>% 
  left_join(df_history, by = c("SITE", "MOMID", "PREGID")) %>% 
  mutate(
    #Preterm
    MAT_PRETERM_ANY=case_when(
      MAT_PRETERM_ANY == 1 ~ 1,
      MAT_PRETERM_ANY == 0 ~ -1,
      TRUE ~ NA_real_
    ),
    #Labor
    PRO_LABOR=case_when(
      PRO_LABOR == 1 ~ 1,
      PRO_LABOR == 0 ~ -1,
      TRUE ~ NA_real_
    ),
    OBS_LABOR=case_when(
      OBS_LABOR == 1 ~ 1,
      OBS_LABOR == 0 ~ -1,
      TRUE ~ NA_real_
    ),
    LABOR_ANY=case_when(
      LABOR_ANY == 1 ~ 1,
      LABOR_ANY == 0 ~ -1,
      TRUE ~ NA_real_
    ),
    
    MEM_SPON=case_when(
      MEM_SPON == 1 ~ 1,
      MEM_SPON == 0 ~ -1,
      TRUE ~ NA_real_
    ),
    MEM_ART=case_when(
      MEM_ART == 1 ~ 1,
      MEM_ART == 0 ~ -1,
      TRUE ~ NA_real_
    ),
    MEM_CES=case_when(
      MEM_CES == 1 ~ 1,
      MEM_CES == 0 ~ -1,
      TRUE ~ NA_real_
    ),
    # Hemorrhage 
    HEM_APH = case_when(
      HEM_APH == 1 ~ 1,
      HEM_APH == 0 ~ -1,
      TRUE ~ NA_real_
    ),
    ##Demographic
    HH_SMOKE=case_when(
      HH_SMOKE==1 ~ 1,
      HH_SMOKE==0 ~ -1,
      TRUE ~ NA_real_),
    SMOKE=case_when(
      SMOKE==1 ~ 1,
      SMOKE==0 ~ -1,
      TRUE ~ NA_real_),
    CHEW_TOBACCO=case_when(
      CHEW_TOBACCO==1 ~ 1,
      CHEW_TOBACCO==0 ~ -1,
      TRUE ~ NA_real_),
    CHEW_BETELNUT=case_when(
      CHEW_BETELNUT==1 ~ 1,
      CHEW_BETELNUT==0 ~ -1,
      TRUE ~ NA_real_),
    SCHOOL_MORE10=case_when(
      SCHOOL_MORE10==1 ~ 1,
      SCHOOL_MORE10==0 ~ -1,
      TRUE ~ NA_real_),
    WATER_IMPROVED = case_when(
      WATER_IMPROVED == 1 ~ 1,
      WATER_IMPROVED == 0 ~ -1,
      TRUE ~ NA_real_
    ),
    TOILET_IMPROVED = case_when(
      TOILET_IMPROVED == 1 ~ 1,
      TOILET_IMPROVED == 0 ~ -1,
      TRUE ~ NA_real_
    ),
    WEALTH_QUINT_5 = case_when(
      WEALTH_QUINT_5 == 1 ~ 1,
      WEALTH_QUINT_5 == 0 ~ -1,
      TRUE ~ NA_real_
    ),
    WEALTH_QUINT_4 = case_when(
      WEALTH_QUINT_4 == 1 ~ 1,
      WEALTH_QUINT_4 == 0 ~ -1,
      TRUE ~ NA_real_
    ),
    WEALTH_QUINT_3 = case_when(
      WEALTH_QUINT_3 == 1 ~ 1,
      WEALTH_QUINT_3 == 0 ~ -1,
      TRUE ~ NA_real_
    ),
    WEALTH_QUINT_2 = case_when(
      WEALTH_QUINT_2 == 1 ~ 1,
      WEALTH_QUINT_2 == 0 ~ -1,
      TRUE ~ NA_real_
    ),
    WEALTH_QUINT_1 = case_when(
      WEALTH_QUINT_1 == 1 ~ 1,
      WEALTH_QUINT_1 == 0 ~ -1,
      TRUE ~ NA_real_
    ),
    PARITY_0 = case_when(
      PARITY_0 == 1 ~ 1,
      PARITY_0 == 0 ~ -1,
      TRUE ~ NA_real_
    ),
    PARITY_1 = case_when(
      PARITY_1 == 1 ~ 1,
      PARITY_1 == 0 ~ -1,
      TRUE ~ NA_real_
    ),
    PARITY_2 = case_when(
      PARITY_2 == 1 ~ 1,
      PARITY_2 == 0 ~ -1,
      TRUE ~ NA_real_
    ),
    
    PAID_WORK = case_when(
      PAID_WORK == 1 ~ 1,
      PAID_WORK == 2 ~ -1,
      TRUE ~ NA_real_
    ),
    MISCARRIAGE = case_when(
      MISCARRIAGE == 1 ~ 1,
      MISCARRIAGE == 0 ~ -1,
      TRUE ~ NA_real_
    ),
    
    AGE_GROUP = transform_to_NA(AGE_GROUP),
    BMI_LEVEL_ENROLL = transform_to_NA(BMI_LEVEL_ENROLL),
    
    MAT_AGE = transform_negative_to_NA(MAT_AGE),
    BMI_ENROLL = transform_negative_to_NA(BMI_ENROLL),
    MUAC_ENROLL = transform_negative_to_NA(MUAC_ENROLL),
    ##Fetel Presentation
    INF_PRES_CEPH = case_when(
      INF_PRES_CEPH == 1 ~ 1,
      INF_PRES_CEPH == 0 ~ -1,
      TRUE ~ NA_real_
    ),
    INF_PRES_BREECH = case_when(
      INF_PRES_BREECH == 1 ~ 1,
      INF_PRES_BREECH == 0 ~ -1,
      TRUE ~ NA_real_
    ),
    INF_PRES_TRANS = case_when(
      INF_PRES_TRANS == 1 ~ 1,
      INF_PRES_TRANS == 0 ~ -1,
      TRUE ~ NA_real_
    ),
    INF_PRES_BROW = case_when(
      INF_PRES_BROW == 1 ~ 1,
      INF_PRES_BROW == 0 ~ -1,
      TRUE ~ NA_real_
    ),
    INF_PRES_OTHER = case_when(
      INF_PRES_OTHER == 1 ~ 1,
      INF_PRES_OTHER == 0 ~ -1,
      TRUE ~ NA_real_
    ), # HIV Positive Enrollment
    HIV_POSITIVE_ENROLL = case_when(
      HIV_POSITIVE_ENROLL == 1 ~ 1,
      HIV_POSITIVE_ENROLL == 0 ~ -1,
      TRUE ~ NA_real_
    ),
    
    # Syphilis Positive Enrollment
    SYPH_POSITIVE_ENROLL = case_when(
      SYPH_POSITIVE_ENROLL == 1 ~ 1,
      SYPH_POSITIVE_ENROLL == 0 ~ -1,
      TRUE ~ NA_real_
    ),
    
    # Gonorrhea Positive Enrollment
    GON_POSITIVE_ENROLL = case_when(
      GON_POSITIVE_ENROLL == 1 ~ 1,
      GON_POSITIVE_ENROLL == 0 ~ -1,
      TRUE ~ NA_real_
    ),
    
    # Chlamydia Positive Enrollment
    CHL_POSITIVE_ENROLL = case_when(
      CHL_POSITIVE_ENROLL == 1 ~ 1,
      CHL_POSITIVE_ENROLL == 0 ~ -1,
      TRUE ~ NA_real_
    ),
    
    # Genital Ulcers Positive Enrollment
    GENU_POSITIVE_ENROLL = case_when(
      GENU_POSITIVE_ENROLL == 1 ~ 1,
      GENU_POSITIVE_ENROLL == 0 ~ -1,
      TRUE ~ NA_real_
    ),
    
    # Other Infections Positive Enrollment
    OTHR_POSITIVE_ENROLL = case_when(
      OTHR_POSITIVE_ENROLL == 1 ~ 1,
      OTHR_POSITIVE_ENROLL == 0 ~ -1,
      TRUE ~ NA_real_
    ),
    
    # Malaria Positive Enrollment
    MAL_POSITIVE_ENROLL = case_when(
      MAL_POSITIVE_ENROLL == 1 ~ 1,
      MAL_POSITIVE_ENROLL == 0 ~ -1,
      TRUE ~ NA_real_
    ),
    
    # Hepatitis B Positive Enrollment
    HBV_POSITIVE_ENROLL = case_when(
      HBV_POSITIVE_ENROLL == 1 ~ 1,
      HBV_POSITIVE_ENROLL == 0 ~ -1,
      TRUE ~ NA_real_
    ),
    
    # Hepatitis C Positive Enrollment
    HCV_POSITIVE_ENROLL = case_when(
      HCV_POSITIVE_ENROLL == 1 ~ 1,
      HCV_POSITIVE_ENROLL == 0 ~ -1,
      TRUE ~ NA_real_
    ),
    
    # Tuberculosis Symptoms Positive Enrollment
    TB_SYMP_POSITIVE_ENROLL = case_when(
      TB_SYMP_POSITIVE_ENROLL == 1 ~ 1,
      TB_SYMP_POSITIVE_ENROLL == 0 ~ -1,
      TRUE ~ NA_real_
    ),
    #Outcome
    INF_ASPH=transform_to_NA(INF_ASPH),
    STILLBIRTH_SIGNS_LIFE=abs(transform_to_NA(STILLBIRTH_SIGNS_LIFE)-1),
    PRETERMBIRTH_LT37=transform_to_NA(PRETERMBIRTH_LT37),
    PRETERMBIRTH_LT34=transform_to_NA(PRETERMBIRTH_LT34),
    PRETERMBIRTH_LT32=transform_to_NA(PRETERMBIRTH_LT32),
    INF_PSBI_IPC=transform_to_NA(INF_PSBI_IPC),
    INF_PSBI_PNC0=transform_to_NA(INF_PSBI_PNC0),
    INF_PSBI_PNC1=transform_to_NA(INF_PSBI_PNC1),
    INF_JAUN_NON_SEV_ANY=transform_to_NA(INF_JAUN_NON_SEV_ANY),
    composed_outcome=case_when(
      INF_ASPH==1|STILLBIRTH_SIGNS_LIFE==1 ~ 1,
      is.na(INF_ASPH)&is.na(STILLBIRTH_SIGNS_LIFE) ~ NA_real_,
      TRUE ~ 0
    )) %>% select("SITE","INFANTID","MOMID", "PREGID",
                  "INF_ASPH","STILLBIRTH_SIGNS_LIFE","composed_outcome","INF_PSBI_IPC","INF_PSBI_PNC0","INF_PSBI_PNC1","INF_JAUN_NON_SEV_ANY","PRETERMBIRTH_LT37","PRETERMBIRTH_LT34","PRETERMBIRTH_LT32",
                "INF_PRES_CEPH","INF_PRES_BREECH","INF_PRES_TRANS","INF_PRES_BROW","INF_PRES_OTHER",
                "ASPHYXIA_IND","STILLBIRTH_IND","PRETERM_IND","POSTTERM_IND","GEST_HTN_IND","PREECLAMPSIA_IND","GEST_DIAB_IND","PREMATURE_RUPTURE_IND","OBSTR_LABOR_IND",
                "MISCARRIAGE","PAID_WORK","PARITY_2","PARITY_1","PARITY_0","WEALTH_QUINT_1","WEALTH_QUINT_2","WEALTH_QUINT_3","WEALTH_QUINT_4","WEALTH_QUINT_5","WEALTH_QUINT_55","SCHOOL_MORE10","WATER_IMPROVED","TOILET_IMPROVED","STOVE_FUEL","HH_SMOKE","SMOKE","CHEW_TOBACCO","CHEW_BETELNUT","AGE_GROUP","BMI_LEVEL_ENROLL",
                "MAT_AGE","BMI_ENROLL","MUAC_ENROLL",
                "MEM_CES","MEM_ART","MEM_SPON","LABOR_ANY","PRO_LABOR","OBS_LABOR","MAT_PRETERM_ANY",
                "HEM_APH","HIV_POSITIVE_ENROLL","SYPH_POSITIVE_ENROLL","GON_POSITIVE_ENROLL","CHL_POSITIVE_ENROLL","GENU_POSITIVE_ENROLL","OTHR_POSITIVE_ENROLL","MAL_POSITIVE_ENROLL","HBV_POSITIVE_ENROLL","HCV_POSITIVE_ENROLL","TB_SYMP_POSITIVE_ENROLL",
                "HIV_MISSING_ENROLL","SYPH_MISSING_ENROLL","GON_DIAG_MISSING_ENROLL","CHL_DIAG_MISSING_ENROLL","GENU_DIAG_MISSING_ENROLL","OTHR_DIAG_MISSING_ENROLL","MAL_MISSING_ENROLL","HBV_MEAS_MISSING_ENROLL","HCV_MEAS_MISSING_ENROLL",)

subject.list <- data.static %>%
  expand_grid(TYPE_VISIT = c(1, 2, 3, 4, 5)) %>% 
  select("SITE","INFANTID","MOMID", "PREGID", "TYPE_VISIT")

data.temporal_pre <- df_supplement %>%
  full_join(merged_df08, by = c("SITE", "MOMID", "PREGID", "TYPE_VISIT","PREG_START_DATE"))
data.temporal<- subject.list %>% 
  left_join(data.temporal_pre, by = c("SITE", "MOMID", "PREGID", "TYPE_VISIT"))

data.static.mask <- data.static %>%
  mutate(across(-c(SITE, MOMID, INFANTID, PREGID), ~ ifelse(is.na(.), 1, 0)))

data.temporal.mask <- data.temporal %>%
  mutate(across(-c(SITE, MOMID, INFANTID, PREGID, TYPE_VISIT), ~ ifelse(is.na(.), 1, 0)))

data.temporal.mean <- data.temporal %>%
summarise(across(-c(SITE, MOMID, INFANTID ,PREGID, TYPE_VISIT, PREG_START_DATE, M04_ANC_OBSSTDAT, M08_LBSTDAT), ~ round(mean(.x, na.rm = TRUE), 2))) %>%
  mutate(across(c( 
    # Iron supplement
    M04_IRON_CMOCCUR,
    M04_IRON_ORAL_CMOCCUR,
    M04_IRON_IV_CMOCCUR,
    # Folic acid supplement
    M04_IFA_CMOCCUR,
    # Calcium supplement
    M04_CALCIUM_CMOCCUR,
    # Vitamin A supplement
    M04_VITAMIN_A_CMOCCUR,
    # Multiple micronutrient supplement
    M04_MICRONUTRIENT_CMOCCUR,
    # Anthelmintic treatment
    M04_ANTHELMINTHIC_CMOCCUR), ~ 0))

data.temporal.GA <- data.temporal %>% group_by(TYPE_VISIT) %>%
  summarise(across(c(M04_GA_days, M08_GA_days), ~ round(mean(.x, na.rm = TRUE), 2)))

# Apply LOCF to each subject while excluding specific columns
data.temporal.impute.pre <- data.temporal %>%
  group_by(SITE, INFANTID, MOMID, PREGID) %>%
  mutate(across(-c(TYPE_VISIT, PREG_START_DATE, M04_ANC_OBSSTDAT, M08_LBSTDAT, M04_GA_days, M08_GA_days), 
                ~ na.locf(.x, na.rm = FALSE))) %>% ungroup()

# Join the calculated means by TYPE_VISIT to the main dataset
data.temporal.impute.pre <- data.temporal.impute.pre %>%
  left_join(data.temporal.GA, by = "TYPE_VISIT", suffix = c("", "_mean")) %>%
  mutate(
    # Impute missing values in M04_GA_days and M08_GA_days with the calculated means by TYPE_VISIT
    M04_GA_days = coalesce(M04_GA_days, M04_GA_days_mean),
    M08_GA_days = coalesce(M08_GA_days, M08_GA_days_mean)
  ) %>%
  select(-M04_GA_days_mean, -M08_GA_days_mean)  # Remove the mean columns after imputation

data.temporal.impute <- data.temporal.impute.pre %>%
  mutate(across(everything(), ~ coalesce(.x, data.temporal.mean[[cur_column()]])))

data.temporal.delta <- data.temporal.impute %>% 
  group_by(SITE, INFANTID , MOMID, PREGID) %>%
  mutate(
    M04_delta_time = case_when(
    TYPE_VISIT == 1 ~ -1,
    TYPE_VISIT %in% c(2,3,4,5) ~ M04_GA_days - lag(M04_GA_days),
    ),
    M08_delta_time = case_when(
      TYPE_VISIT == 1 ~ -1,
      TYPE_VISIT %in% c(2,3,4,5) ~ M08_GA_days - lag(M08_GA_days),
    )) %>% ungroup() %>%
  mutate(
    across(
      c(M04_IRON_CMOCCUR, M04_IRON_ORAL_CMOCCUR, M04_IRON_IV_CMOCCUR,
        M04_IFA_CMOCCUR, M04_CALCIUM_CMOCCUR, M04_VITAMIN_A_CMOCCUR, 
        M04_MICRONUTRIENT_CMOCCUR, M04_ANTHELMINTHIC_CMOCCUR),
      ~ M04_delta_time
    ),
    across(
      c(M08_GA_days, M08_CBC_MCV_LBORRES, M08_VITB12_COB_LBORRES, M08_VITB12_HOL_LBORRES, 
        M08_FOLATE_PLASMA_NMOLL_LBORRES, M08_IRON_TOT_UGDL_LBORRES, M08_VITA_UGDL_LBORRES, 
        M08_FERRITIN_LBORRES, M08_IODINE_LBORRES, M08_RBP4_LBORRES, M08_CRP_LBORRES,
        M08_ZINC_LBORRES,M08_IRON_HEP_LBORRES,
        M08_AGP_LBORRES, M08_CBC_HB_LBORRES, M08_HBA1C_LBORRES),
      ~ M08_delta_time
    )
  )

data.static.impute <- data.static %>%
  mutate(across(-c(SITE, INFANTID, MOMID, PREGID, INF_ASPH, STILLBIRTH_SIGNS_LIFE, composed_outcome), 
                ~ ifelse(is.na(.x), 0, .x)))

data.static.merge <- data.static %>% 
  mutate(across(-c(SITE, INFANTID, MOMID, PREGID, INF_ASPH, STILLBIRTH_SIGNS_LIFE, composed_outcome), 
                ~ ifelse(is.na(.x), 0, .x))) %>%
  slice(rep(1:n(), each = 5)) %>% select(-c(SITE ,INFANTID, MOMID, PREGID))

data.dl.continous<-cbind(data.temporal.impute,data.static.merge)
data.dl.continous.delta<-cbind(data.temporal.delta,data.static.merge)
data.dl.continous.mask<-cbind(data.temporal.mask,data.static.merge)

write.csv(data.dl.continous,"D:/Users/yipeng_wei/Documents/dl data/2025-01-10/df_dl_continous.csv")
write.csv(data.static,"D:/Users/yipeng_wei/Documents/dl data/2025-01-10/df_dl_static.csv")

# Pivot the data to wide format
data.temporal.impute.wide  <- data.temporal.impute  %>% 
  select(-c(PREG_START_DATE, M04_ANC_OBSSTDAT, M08_LBSTDAT)) %>% 
  pivot_wider(
    names_from = TYPE_VISIT,  # Use TYPE_VISIT as the basis for new columns
    values_from = -c(SITE,INFANTID,MOMID,PREGID,TYPE_VISIT)  # Columns to widen
  )

data.static.merge.wide<-data.static %>%
  select(-c(SITE ,INFANTID, MOMID, PREGID))

data.dl.continous.wide<-cbind(data.temporal.impute.wide,data.static.merge.wide)

data.temporal.wide  <- data.temporal%>% 
  select(-c(PREG_START_DATE, M04_ANC_OBSSTDAT, M08_LBSTDAT)) %>% 
  pivot_wider(
    names_from = TYPE_VISIT,  # Use TYPE_VISIT as the basis for new columns
    values_from = -c(SITE,INFANTID,MOMID,PREGID,TYPE_VISIT)  # Columns to widen
  )


write.csv(data.temporal.wide,"D:/Users/yipeng_wei/Documents/dl data/2025-01-10/df_dl_temporal_wide.csv")
write.csv(data.dl.continous.wide,"D:/Users/yipeng_wei/Documents/dl data/2025-01-10/df_dl_continous_wide.csv")

write.csv(data.dl.continous.delta,"D:/Users/yipeng_wei/Documents/dl data/2025-01-10/df_dl_continous_delta.csv")
write.csv(data.dl.continous.mask,"D:/Users/yipeng_wei/Documents/dl data/2025-01-10/df_dl_continous_mask.csv")

