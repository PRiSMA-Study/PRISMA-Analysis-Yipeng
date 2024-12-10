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

transform_to_NA<-function(variable){
  variable[variable==-7]<-NA
  variable[variable==-5]<-NA
  variable[variable==-6]<-NA
  variable[variable==-9]<-NA
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
UploadDate = "2024-06-28"
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

data.enroll <- read_dta("D:/Users/yipeng_wei/Documents/dl data/2024-06-28/MAT_ENROLL.dta")

merged_df03 <- left_join(data.enroll, mnh03, by = c("SITE", "MOMID", "PREGID"))
merged_df04 <- left_join(data.enroll, mnh04, by = c("SITE", "MOMID", "PREGID"))
merged_df08 <- left_join(data.enroll, mnh08, by = c("SITE", "MOMID", "PREGID"))

merged_df08 <- merged_df08 %>% mutate(
  #GA days
  M08_GA_days = transform_date_to_NA(as.numeric(as.Date(M08_LBSTDAT) - as.Date(EST_CONCEP_DATE))),
  across(c(M08_CBC_MCV_LBORRES, M08_VITB12_COB_LBORRES, M08_VITB12_HOL_LBORRES, 
           M08_FOLATE_PLASMA_NMOLL_LBORRES, M08_IRON_TOT_UGDL_LBORRES, 
           M08_VITA_UGDL_LBORRES, M08_FERRITIN_LBORRES, M08_IODINE_LBORRES, 
           M08_RBP4_LBORRES, M08_CRP_LBORRES, M08_AGP_LBORRES, 
           M08_CBC_HB_LBORRES, M08_HBA1C_LBORRES), 
         ~ transform_continous_to_NA(.)),
  M08_HBA1C_LBORRES = transform_to_NA(M08_HBA1C_LBORRES)
) %>%
  dplyr::select("MOMID", "PREGID", "SITE", EST_CONCEP_DATE, TYPE_VISIT, M08_LBSTDAT, M08_GA_days,
                M08_CBC_MCV_LBORRES, M08_VITB12_COB_LBORRES, M08_VITB12_HOL_LBORRES,
                M08_FOLATE_PLASMA_NMOLL_LBORRES,
                #M08_ZINC_LBORRES,M08_IRON_HEP_LBORRES,
                M08_IRON_TOT_UGDL_LBORRES,
                M08_VITA_UGDL_LBORRES,
                #M08_FOLATE_RBC_NMOLL_LBORRES,
                M08_FERRITIN_LBORRES,M08_IODINE_LBORRES,M08_RBP4_LBORRES,M08_CRP_LBORRES,M08_AGP_LBORRES,
                M08_CBC_HB_LBORRES,M08_HBA1C_LBORRES)

merged_df04<- merged_df04 %>%
  mutate(
    #GA days
    M04_GA_days = transform_date_to_NA(as.numeric(as.Date(M04_ANC_OBSSTDAT) - as.Date(EST_CONCEP_DATE))),
    #Iron supplement
    M04_IRON_CMOCCUR = transform_to_NA(M04_IRON_CMOCCUR),
    M04_IRON_ORAL_CMOCCUR = transform_to_NA(M04_IRON_ORAL_CMOCCUR),
    M04_IRON_IV_CMOCCUR = transform_to_NA(M04_IRON_IV_CMOCCUR),
    M04_IRON_Supplement=case_when(
      M04_IRON_IV_CMOCCUR==1|M04_IRON_ORAL_CMOCCUR==1 ~ 1,
      (M04_IRON_IV_CMOCCUR==0|is.na(M04_IRON_IV_CMOCCUR))&(M04_IRON_ORAL_CMOCCUR==0|is.na(M04_IRON_ORAL_CMOCCUR)) ~ 0,
      TRUE ~ NA_real_,
    ),
    #Folic acid supplement
    M04_IFA_CMOCCUR = transform_to_NA(M04_IFA_CMOCCUR),
    #Calcium supplement
    M04_CALCIUM_CMOCCUR = transform_to_NA(M04_CALCIUM_CMOCCUR),
    #Vitamin A supplement
    M04_VITAMIN_A_CMOCCUR = transform_to_NA(M04_VITAMIN_A_CMOCCUR),
    #Multiple micronutrient supplement
    M04_MICRONUTRIENT_CMOCCUR = transform_to_NA(M04_MICRONUTRIENT_CMOCCUR),
    #Anthelmintic treatment
    M04_ANTHELMINTHIC_CMOCCUR = transform_to_NA(M04_ANTHELMINTHIC_CMOCCUR),
    STILLBIRTH_IND = case_when(
      M04_STILLBIRTH_RPORRES == 1 ~ 1, #stillbirth,
      M04_PH_PREV_RPORRES == 0 | M04_PH_OTH_RPORRES == 0 | M04_STILLBIRTH_RPORRES == 0 ~ 0,
      TRUE ~ NA_real_
    ),
    ASPHYXIA_IND = case_when(
      M04_BIRTH_ASPHYXIA_MHOCCUR == 1 ~ 1, #asphyxia,
      M04_PH_PREV_RPORRES == 0 | M04_BIRTH_ASPHYXIA_MHOCCUR == 0 ~ 0,
      TRUE ~ NA_real_
    ),
    PRETERM_IND = case_when(
      M04_PRETERM_RPORRES == 1 ~ 1, #preterm birth,
      M04_PH_PREV_RPORRES == 0 | M04_PRETERM_RPORRES == 0 ~ 0,
      TRUE ~ NA_real_
    ),
    POSTTERM_IND = case_when(
      M04_POSTTERM_RPORRES == 1 ~ 1, #postterm birth,
      M04_PH_PREV_RPORRES == 0 | M04_POSTTERM_RPORRES == 0 ~ 0,
      TRUE ~ NA_real_
    ),
    GEST_HTN_IND = case_when(
      M04_GEST_HTN_RPORRES == 1 ~ 1, #Gestational hypertension,
      M04_PH_PREV_RPORRES == 0 | M04_GEST_HTN_RPORRES == 0 ~ 0,
      TRUE ~ NA_real_
    ),
    PREECLAMPSIA_IND = case_when(
      M04_PREECLAMPSIA_RPORRES == 1 ~ 1, #Preeclampsia,
      M04_PH_PREV_RPORRES == 0 | M04_PREECLAMPSIA_RPORRES == 0 ~ 0,
      TRUE ~ NA_real_
    ),
    GEST_DIAB_IND = case_when(
      M04_GEST_DIAB_RPORRES == 1 ~ 1, #Gestational diabetes,
      M04_PH_PREV_RPORRES == 0 | M04_GEST_DIAB_RPORRES == 0 ~ 0,
      TRUE ~ NA_real_
    ),
    PREMATURE_RUPTURE_IND = case_when(
      M04_PREMATURE_RUPTURE_RPORRES == 1 ~ 1, #Premature rupture of membranes,
      M04_PH_PREV_RPORRES == 0 | M04_PREMATURE_RUPTURE_RPORRES == 0 ~ 0,
      TRUE ~ NA_real_
    ),
    OBSTR_LABOR_IND = case_when(
      M04_OBSTR_LABOR_RPORRES == 1 ~ 1, #Obstructed or prolonged labor,
      M04_PH_PREV_RPORRES == 0 | M04_OBSTR_LABOR_RPORRES == 0 ~ 0,
      TRUE ~ NA_real_
    )
  ) %>% dplyr::select( "MOMID", "PREGID", "SITE", EST_CONCEP_DATE, TYPE_VISIT, M04_ANC_OBSSTDAT, M04_GA_days,
         #Iron supplement
         M04_IRON_CMOCCUR,
         M04_IRON_ORAL_CMOCCUR,
         M04_IRON_IV_CMOCCUR,
         M04_IRON_Supplement,
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
  dplyr::select( "MOMID", "PREGID", "SITE", EST_CONCEP_DATE, TYPE_VISIT, M04_ANC_OBSSTDAT, M04_GA_days,
                #Iron supplement
                M04_IRON_CMOCCUR,
                M04_IRON_ORAL_CMOCCUR,
                M04_IRON_IV_CMOCCUR,
                M04_IRON_Supplement,
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
    M03_STOVE_FCORRESR = transform_to_NA(M03_STOVE_FCORRES),
    M03_STOVE_FCORRESR_ind = case_when(
      M03_STOVE_FCORRESR %in% c(1,2,3,4,5) ~ 1,
      M03_STOVE_FCORRESR %in% c(7,8,9,11) ~ 0,
      M03_STOVE_FCORRESR==6 & M03_STOVE_FUEL_FCORRES_1==1 ~ 1,
      M03_STOVE_FCORRESR==6 & M03_STOVE_FUEL_FCORRES_2==1 ~ 1,
      M03_STOVE_FCORRESR==6 & M03_STOVE_FUEL_FCORRES_3==1 ~ 1,
      M03_STOVE_FCORRESR==6 & M03_STOVE_FUEL_FCORRES_4==1 ~ 0,
      M03_STOVE_FCORRESR==6 & M03_STOVE_FUEL_FCORRES_5==1 ~ 0,
      M03_STOVE_FCORRESR==6 & M03_STOVE_FUEL_FCORRES_6==1 ~ 0,
      M03_STOVE_FCORRESR==6 & M03_STOVE_FUEL_FCORRES_7==1 ~ 0,
      M03_STOVE_FCORRESR==6 & M03_STOVE_FUEL_FCORRES_8==1 ~ 0,
      M03_STOVE_FCORRESR==6 & M03_STOVE_FUEL_FCORRES_9==1 ~ 0,
      M03_STOVE_FCORRESR==6 & M03_STOVE_FUEL_FCORRES_10==1 ~ 0,
      M03_STOVE_FCORRESR==6 & M03_STOVE_FUEL_FCORRES_11==1 ~ 0,
      M03_STOVE_FCORRESR==6 & M03_STOVE_FUEL_FCORRES_12==1 ~ 0,
      M03_STOVE_FCORRESR==6 & M03_STOVE_FUEL_FCORRES_13==1 ~ 0,
      M03_STOVE_FCORRESR==6 & M03_STOVE_FUEL_FCORRES_14==1 ~ 0,
      M03_STOVE_FCORRESR==6 & M03_STOVE_FUEL_FCORRES_15==1 ~ 1,
      TRUE ~ NA_real_),
  ) %>% dplyr::select( "MOMID", "PREGID", "SITE", TYPE_VISIT,
                      M03_STOVE_FCORRESR_ind)

df_inf_outcome<-read.csv("D:/Users/yipeng_wei/Documents/dl data/2024-06-28/INF_OUTCOMES.csv")
data.risk <- read_dta("D:/Users/yipeng_wei/Documents/dl data/2024-06-28/MAT_RISKS.dta")
data.mortality<-read_dta("D:/Users/yipeng_wei/Documents/dl data/2024-06-28/MAT_MORTALITY.dta")
data.demographic<-read.csv("D:/Users/yipeng_wei/Documents/dl data/2024-06-28/MAT_DEMOGRAPHIC.csv")
data.infection<-read.csv("D:/Users/yipeng_wei/Documents/dl data/2024-06-28/MAT_INFECTION.csv")
data.infection <- distinct(data.infection, SITE, MOMID,.keep_all = TRUE)
data.hemorr <- read.csv("D:/Users/yipeng_wei/Documents/dl data/2024-06-28/MAT_HEMORRHAGE.csv")
data.labor <- read_dta("D:/Users/yipeng_wei/Documents/dl data/2024-06-28/MAT_LABOR.dta")
data.preterm <- read_dta("D:/Users/yipeng_wei/Documents/dl data/2024-06-28/MAT_PRETERM.dta")
df_inf_presentation <- read_dta("D:/Users/yipeng_wei/Documents/dl data/2024-06-28/INF_PRESENTATION.dta")

data.static <- df_inf_outcome %>% 
  left_join(df_inf_presentation, by = c("MOMID", "PREGID","INFANTID")) %>%
  left_join(data.risk, by = c("SITE", "MOMID", "PREGID")) %>%
  left_join(data.mortality, by = c("SITE", "MOMID", "PREGID")) %>%
  left_join(data.demographic, by = c("SITE", "MOMID", "PREGID")) %>%
  left_join(data.infection, by = c("SITE", "MOMID", "PREGID")) %>%
  left_join(data.hemorr, by = c("SITE", "MOMID", "PREGID")) %>%
  left_join(data.labor, by = c("SITE", "MOMID", "PREGID")) %>%
  left_join(data.preterm, by = c("SITE", "MOMID", "PREGID")) %>%
  left_join(df_fuel, by = c("SITE", "MOMID", "PREGID")) %>% 
  left_join(df_history, by = c("SITE", "MOMID", "PREGID")) %>% 
  mutate(
    #Preterm
    PRETERM_ANY=transform_to_NA(PRETERM_ANY),
    #Labor
    PRO_LABOR=transform_to_NA(PRO_LABOR),
    OBS_LABOR=transform_to_NA(OBS_LABOR),
    LABOR_ANY=transform_to_NA(LABOR_ANY),
    
    MEM_SPON=transform_to_NA(MEM_SPON),
    MEM_ART=transform_to_NA(MEM_ART),
    MEM_CES=transform_to_NA(MEM_CES),
    #Hemorrhage
    HEM_APH=transform_to_NA(HEM_APH),
    ##Demographic
    hh_smoke=case_when(
      hh_smoke==1 ~ 1,
      hh_smoke==0 ~ 0,
      TRUE ~ NA_real_),
    SCHOOL_MORE10=case_when(
      SCHOOL_MORE10==1 ~ 1,
      SCHOOL_MORE10==0 ~ 0,
      TRUE ~ NA_real_),
    water_improved = case_when(
      water_improved == 1 ~ 1,
      water_improved == 0 ~ 0,
      TRUE ~ NA_real_
    ),
    toilet_improved = case_when(
      toilet_improved == 1 ~ 1,
      toilet_improved == 0 ~ 0,
      TRUE ~ NA_real_
    ),
    WEALTH_QUINT_5 = case_when(
      WEALTH_QUINT_5 == 1 ~ 1,
      WEALTH_QUINT_5 == 0 ~ 0,
      TRUE ~ NA_real_
    ),
    WEALTH_QUINT_4 = case_when(
      WEALTH_QUINT_4 == 1 ~ 1,
      WEALTH_QUINT_4 == 0 ~ 0,
      TRUE ~ NA_real_
    ),
    WEALTH_QUINT_3 = case_when(
      WEALTH_QUINT_3 == 1 ~ 1,
      WEALTH_QUINT_3 == 0 ~ 0,
      TRUE ~ NA_real_
    ),
    WEALTH_QUINT_2 = case_when(
      WEALTH_QUINT_2 == 1 ~ 1,
      WEALTH_QUINT_2 == 0 ~ 0,
      TRUE ~ NA_real_
    ),
    WEALTH_QUINT_1 = case_when(
      WEALTH_QUINT_1 == 1 ~ 1,
      WEALTH_QUINT_1 == 0 ~ 0,
      TRUE ~ NA_real_
    ),
    PARITY_0 = transform_to_NA(PARITY_0),
    PARITY_1 = transform_to_NA(PARITY_1),
    PARITY_2 = transform_to_NA(PARITY_2),
    
    paid_work = transform_to_NA(paid_work),
    miscarriage = transform_to_NA(miscarriage),
    
    AGE_GROUP = transform_to_NA(AGE_GROUP),
    bmi_index = transform_to_NA(bmi_index),
    
    age = transform_continous_to_NA(age),
    bmi_enroll = transform_continous_to_NA(bmi_enroll),
    muac = transform_continous_to_NA(muac),
    ##Fetel Presentation
    INF_PRES_CEPH=transform_to_NA(INF_PRES_CEPH),
    INF_PRES_BREECH=transform_to_NA(INF_PRES_BREECH),
    INF_PRES_TRANS=transform_to_NA(INF_PRES_TRANS),
    INF_PRES_BROW=transform_to_NA(INF_PRES_BROW),
    INF_PRES_OTHER=transform_to_NA(INF_PRES_OTHER),
    #Outcome
    INF_ASPH=abs(transform_to_NA(INF_ASPH)-1),
    STILLBIRTH_SIGNS_LIFE=transform_to_NA(STILLBIRTH_SIGNS_LIFE),
    MAT_MORTALITY=transform_to_NA(MAT_MORTALITY),
    composed_outcome=case_when(
      INF_ASPH==0|STILLBIRTH_SIGNS_LIFE==0|MAT_MORTALITY==1 ~ 0,
      is.na(INF_ASPH)&is.na(STILLBIRTH_SIGNS_LIFE)& MAT_MORTALITY %in% c(3,NA) ~ NA_real_,
      TRUE ~ 1
    )) %>% select("SITE","INFANTID","MOMID", "PREGID",
                "INF_ASPH","STILLBIRTH_SIGNS_LIFE","MAT_MORTALITY","composed_outcome",
                "INF_PRES_CEPH","INF_PRES_BREECH","INF_PRES_TRANS","INF_PRES_BROW","INF_PRES_OTHER",
                "ASPHYXIA_IND","STILLBIRTH_IND","PRETERM_IND","POSTTERM_IND","GEST_HTN_IND","PREECLAMPSIA_IND","GEST_DIAB_IND","PREMATURE_RUPTURE_IND","OBSTR_LABOR_IND",
                "miscarriage","paid_work","PARITY_2","PARITY_1","PARITY_0","WEALTH_QUINT_1","WEALTH_QUINT_2","WEALTH_QUINT_3","WEALTH_QUINT_4","WEALTH_QUINT_5","SCHOOL_MORE10","water_improved","toilet_improved","M03_STOVE_FCORRESR_ind","hh_smoke","AGE_GROUP","bmi_index",
                "age","bmi_enroll","muac",
                "MEM_CES","MEM_ART","MEM_SPON","LABOR_ANY","PRO_LABOR","OBS_LABOR","PRETERM_ANY",
                "HEM_APH","HIV_POSITIVE_ENROLL","SYPH_POSITIVE_ENROLL","GON_POSITIVE_ENROLL","CHL_POSITIVE_ENROLL","GENU_POSITIVE_ENROLL","OTHR_POSITIVE_ENROLL","MAL_POSITIVE_ENROLL","HBV_POSITIVE_ENROLL","HCV_POSITIVE_ENROLL","TB_SYMP_POSITIVE_ENROLL")

#transform paid_work variable to binary variable
data.static$paid_work<-abs(data.static$paid_work-2)

subject.list <- data.static %>%
  expand_grid(TYPE_VISIT = c(1, 2, 3, 4, 5)) %>% 
  select("SITE","INFANTID","MOMID", "PREGID", "TYPE_VISIT")
  
data.temporal_pre <- df_supplement %>%
  full_join(merged_df08, by = c("SITE", "MOMID", "PREGID", "TYPE_VISIT","EST_CONCEP_DATE"))
data.temporal<- subject.list %>% 
  left_join(data.temporal_pre, by = c("SITE", "MOMID", "PREGID", "TYPE_VISIT"))

data.static.mask <- data.static %>%
  mutate(across(-c(SITE, MOMID, INFANTID, PREGID), ~ ifelse(is.na(.), 1, 0)))

data.temporal.mask <- data.temporal %>%
  mutate(across(-c(SITE, MOMID, INFANTID, PREGID, TYPE_VISIT), ~ ifelse(is.na(.), 1, 0)))

data.temporal.mean <- data.temporal %>%
summarise(across(-c(SITE, MOMID, INFANTID ,PREGID, TYPE_VISIT, EST_CONCEP_DATE, M04_ANC_OBSSTDAT, M08_LBSTDAT), ~ round(mean(.x, na.rm = TRUE), 2)))

data.temporal.GA <- data.temporal %>% group_by(TYPE_VISIT) %>%
  summarise(across(c(M04_GA_days, M08_GA_days), ~ round(mean(.x, na.rm = TRUE), 2)))

# Apply LOCF to each subject while excluding specific columns
data.temporal.impute.pre <- data.temporal %>%
  group_by(SITE, INFANTID, MOMID, PREGID) %>%
  mutate(across(-c(TYPE_VISIT, EST_CONCEP_DATE, M04_ANC_OBSSTDAT, M08_LBSTDAT, M04_GA_days, M08_GA_days), 
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
    TYPE_VISIT == 1 ~ 0,
    TYPE_VISIT %in% c(2,3,4,5) ~ M04_GA_days - lag(M04_GA_days),
    ),
    M08_delta_time = case_when(
      TYPE_VISIT == 1 ~ 0,
      TYPE_VISIT %in% c(2,3,4,5) ~ M08_GA_days - lag(M08_GA_days),
    )) %>% ungroup() %>%
  mutate(
    across(
      c(M04_IRON_CMOCCUR, M04_IRON_ORAL_CMOCCUR, M04_IRON_IV_CMOCCUR, M04_IRON_Supplement, 
        M04_IFA_CMOCCUR, M04_CALCIUM_CMOCCUR, M04_VITAMIN_A_CMOCCUR, 
        M04_MICRONUTRIENT_CMOCCUR, M04_ANTHELMINTHIC_CMOCCUR),
      ~ M04_delta_time
    ),
    across(
      c(M08_GA_days, M08_CBC_MCV_LBORRES, M08_VITB12_COB_LBORRES, M08_VITB12_HOL_LBORRES, 
        M08_FOLATE_PLASMA_NMOLL_LBORRES, M08_IRON_TOT_UGDL_LBORRES, M08_VITA_UGDL_LBORRES, 
        M08_FERRITIN_LBORRES, M08_IODINE_LBORRES, M08_RBP4_LBORRES, M08_CRP_LBORRES, 
        M08_AGP_LBORRES, M08_CBC_HB_LBORRES, M08_HBA1C_LBORRES),
      ~ M08_delta_time
    )
  )

data.static.impute <- data.static %>%
  mutate(across(-c(SITE, INFANTID, MOMID, PREGID, INF_ASPH, STILLBIRTH_SIGNS_LIFE, MAT_MORTALITY, composed_outcome), 
                ~ ifelse(is.na(.x), round(mean(.x, na.rm = TRUE),2), .x)))

data.static.merge <- data.static %>% 
  mutate(across(-c(SITE, INFANTID, MOMID, PREGID, INF_ASPH, STILLBIRTH_SIGNS_LIFE, MAT_MORTALITY, composed_outcome), 
                ~ ifelse(is.na(.x), round(mean(.x, na.rm = TRUE),2), .x))) %>%
  slice(rep(1:n(), each = 5)) %>% select(-c(SITE ,INFANTID, MOMID, PREGID))

data.dl.continous<-cbind(data.temporal.impute,data.static.merge)
data.dl.continous.delta<-cbind(data.temporal.delta,data.static.merge)
data.dl.continous.mask<-cbind(data.temporal.mask,data.static.merge)

write.csv(data.dl.continous,"D:/Users/yipeng_wei/Documents/dl data/2024-06-28/df_dl_continous.csv")

# Pivot the data to wide format
data.temporal.impute.wide  <- data.temporal.impute  %>% 
  select(-c(EST_CONCEP_DATE, M04_ANC_OBSSTDAT, M08_LBSTDAT)) %>% 
  pivot_wider(
    names_from = TYPE_VISIT,  # Use TYPE_VISIT as the basis for new columns
    values_from = -c(SITE,INFANTID,MOMID,PREGID,TYPE_VISIT)  # Columns to widen
  )

data.static.merge.wide<-data.static %>%
  select(-c(SITE ,INFANTID, MOMID, PREGID))

data.dl.continous.wide<-cbind(data.temporal.impute.wide,data.static.merge.wide)

write.csv(data.dl.continous.wide,"D:/Users/yipeng_wei/Documents/dl data/2024-06-28/df_dl_continous_wide.csv")

write.csv(data.dl.continous.delta,"D:/Users/yipeng_wei/Documents/dl data/2024-06-28/df_dl_continous_delta.csv")
write.csv(data.dl.continous.mask,"D:/Users/yipeng_wei/Documents/dl data/2024-06-28/df_dl_continous_mask.csv")

