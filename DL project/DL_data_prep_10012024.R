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

transform_negative_to_NA<-function(variable){
  variable[variable==-7]<-NA
  variable[variable==-5]<-NA
  variable[variable==-6]<-NA
  variable[variable==-9]<-NA
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

###Rename TYPE_VISIT variables:
mnh02 <- mnh02 %>% rename(TYPE_VISIT = M02_TYPE_VISIT)
mnh03 <- mnh03 %>% rename(TYPE_VISIT = M03_TYPE_VISIT)
mnh04 <- mnh04 %>% rename(TYPE_VISIT = M04_TYPE_VISIT)

###Clean up the data frames: 
mnh02 <- mnh02 %>%
  filter(MOMID!="") %>% #Moved filter to the end so we can create all the variables with TYPE_VISIT at the end of the var name before rows get dropped out
  drop_na(MOMID, PREGID) %>%
  distinct (MOMID, PREGID, SITE, TYPE_VISIT, .keep_all = TRUE) %>% select(-TYPE_VISIT)

mnh03 <- mnh03 %>%
  filter(MOMID!="") %>% #Moved filter to the end so we can create all the variables with TYPE_VISIT at the end of the var name before rows get dropped out
  drop_na(MOMID, PREGID) %>%
  distinct (MOMID, PREGID, SITE, TYPE_VISIT, .keep_all = TRUE)

mnh04 <- mnh04 %>%
  filter(MOMID!="") %>% #Moved filter to the end so we can create all the variables with TYPE_VISIT at the end of the var name before rows get dropped out
  drop_na(MOMID, PREGID) %>%
  distinct (MOMID, PREGID, SITE, TYPE_VISIT, .keep_all = TRUE) %>% 
  filter(TYPE_VISIT==1)

merged_df03 <- left_join(mnh02, mnh03, by = c("SITE", "MOMID", "PREGID"))
merged_df04 <- left_join(mnh02, mnh04, by = c("SITE", "MOMID", "PREGID"))

merged_df04 <- merged_df04 %>% 
  mutate(
    #Iron supplement
    M04_IRON_CMOCCUR = transform_to_NA(M04_IRON_CMOCCUR),
    M04_IRON_ORAL_CMOCCUR = transform_to_NA(M04_IRON_ORAL_CMOCCUR),
    M04_IRON_IV_CMOCCUR = transform_to_NA(M04_IRON_IV_CMOCCUR),
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
    #Stillbirth
    M04_STILLBIRTH_RPORRES = transform_to_NA(M04_STILLBIRTH_RPORRES),
    M04_PH_PREV_RPORRES = transform_to_NA(M04_PH_PREV_RPORRES),
    M04_PH_OTH_RPORRES = transform_to_NA(M04_PH_OTH_RPORRES),
    #Birth asphyxia
    M04_BIRTH_ASPHYXIA_MHOCCUR = transform_to_NA(M04_BIRTH_ASPHYXIA_MHOCCUR)
  )

merged_df03 <- merged_df03 %>% 
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
  )


df_fuel<-merged_df03 %>% select(
  "SCRNID", "MOMID", "PREGID", "SITE",M03_STOVE_FCORRESR,M03_STOVE_FCORRESR_ind
)

df_supplement<-merged_df04 %>% select("SCRNID", "MOMID", "PREGID", "SITE",TYPE_VISIT,
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
                                      M04_ANTHELMINTHIC_CMOCCUR
)

df_birth<-merged_df04 %>% select("SCRNID", "MOMID", "PREGID", "SITE",TYPE_VISIT,
                                 #Stillbirth
                                 M04_STILLBIRTH_RPORRES,
                                 M04_PH_PREV_RPORRES,
                                 M04_PH_OTH_RPORRES,
                                 #Birth asphyxia
                                 M04_BIRTH_ASPHYXIA_MHOCCUR,
                                 #Pre-term delivery
                                 M04_PRETERM_RPORRES,
                                 #post-term delivery
                                 M04_POSTTERM_RPORRES,
                                 #Gestational hypertension
                                 M04_GEST_HTN_RPORRES,
                                 #Preeclampsia/eclampsia
                                 M04_PREECLAMPSIA_RPORRES,
                                 #Gestational diabetes
                                 M04_GEST_DIAB_RPORRES,
                                 #Premature rupture of membranes 
                                 M04_PREMATURE_RUPTURE_RPORRES,
                                 #Obstructed or prolonged labor
                                 M04_OBSTR_LABOR_RPORRES
)

df_inf_outcome<-read.csv("D:/Users/yipeng_wei/Documents/dl data/2024-06-28/INF_OUTCOMES.csv")
data.nutr <- read_dta("D:/Users/yipeng_wei/Documents/dl data/2024-06-28/MAT_NUTR.dta")
data.risk <- read_dta("D:/Users/yipeng_wei/Documents/dl data/2024-06-28/MAT_RISKS.dta")
data.anemia <- read_dta("D:/Users/yipeng_wei/Documents/dl data/2024-06-28/MAT_ANEMIA.dta")
data.mortality<-read_dta("D:/Users/yipeng_wei/Documents/dl data/2024-06-28/MAT_MORTALITY.dta")
data.demographic<-read.csv("D:/Users/yipeng_wei/Documents/dl data/2024-06-28/MAT_DEMOGRAPHIC.csv")
data.infection<-read.csv("D:/Users/yipeng_wei/Documents/dl data/2024-06-28/MAT_INFECTION.csv")
data.infection <- distinct(data.infection, SITE, MOMID,.keep_all = TRUE)
data.depr <- read_dta("D:/Users/yipeng_wei/Documents/dl data/2024-06-28/MAT_DEPR.dta")
data.gdm <- read_dta("D:/Users/yipeng_wei/Documents/dl data/2024-06-28/MAT_GDM.dta")
data.hdp <- read_dta("D:/Users/yipeng_wei/Documents/dl data/2024-06-28/MAT_HDP.dta")
data.hemorr <- read.csv("D:/Users/yipeng_wei/Documents/dl data/2024-06-28/MAT_HEMORRHAGE.csv")
data.labor <- read_dta("D:/Users/yipeng_wei/Documents/dl data/2024-06-28/MAT_LABOR.dta")
data.preterm <- read_dta("D:/Users/yipeng_wei/Documents/dl data/2024-06-28/MAT_PRETERM.dta")
df_inf_presentation <- read_dta("D:/Users/yipeng_wei/Documents/dl data/2024-06-28/INF_PRESENTATION.dta")

data.dl <- df_inf_outcome %>% 
  left_join(df_inf_presentation, by = c("MOMID", "PREGID","INFANTID")) %>%
  left_join(data.nutr, by = c("SITE", "MOMID", "PREGID")) %>%
  left_join(data.risk, by = c("SITE", "MOMID", "PREGID")) %>%
  left_join(data.anemia, by = c("SITE", "MOMID", "PREGID")) %>%
  left_join(data.mortality, by = c("SITE", "MOMID", "PREGID")) %>%
  left_join(data.demographic, by = c("SITE", "MOMID", "PREGID")) %>%
  left_join(data.infection, by = c("SITE", "MOMID", "PREGID")) %>%
  left_join(data.depr, by = c("SITE", "MOMID", "PREGID")) %>%
  left_join(data.gdm, by = c("SITE", "MOMID", "PREGID")) %>%
  left_join(data.hdp, by = c("SITE", "MOMID", "PREGID")) %>%
  left_join(data.hemorr, by = c("SITE", "MOMID", "PREGID")) %>%
  left_join(data.labor, by = c("SITE", "MOMID", "PREGID")) %>%
  left_join(data.preterm, by = c("SITE", "MOMID", "PREGID")) %>%
  left_join(df_supplement, by = c("SITE", "MOMID", "PREGID")) %>% 
  left_join(df_fuel, by = c("SITE", "MOMID", "PREGID")) %>%
  left_join(df_birth, by = c("SITE", "MOMID", "PREGID")) %>%
  mutate(
    #Depression
    DEPR_ANC20=transform_to_NA(DEPR_ANC20_STND),
    DEPR_ANC32=transform_to_NA(DEPR_ANC32_STND),
    #Diabetes
    DIAB_OVERT=transform_to_NA(DIAB_OVERT),
    #Preterm
    PRETERM_ANY=transform_to_NA(PRETERM_ANY),
    #Labor
    PRO_LABOR=transform_to_NA(PRO_LABOR),
    OBS_LABOR=transform_to_NA(OBS_LABOR),
    LABOR_ANY=transform_to_NA(LABOR_ANY),
    
    MEM_SPON=transform_to_NA(MEM_SPON),
    MEM_ART=transform_to_NA(MEM_ART),
    MEM_CES=transform_to_NA(MEM_CES),
    
    #Anemia
    ANEMIA_ANC20=transform_to_NA(ANEMIA_ANC20),
    ANEMIA_ANC32=transform_to_NA(ANEMIA_ANC32),
    
    ##Nutrition
    FERRITIN70_ANC20=case_when(
      FERRITIN70_ANC20==2 ~ 1,
      FERRITIN70_ANC20==1 ~ 0,
      TRUE ~ NA_real_),
    STFR_ANC20=case_when(
      STFR_ANC20==2 ~ 0,
      STFR_ANC20 %in% c(1,3) ~ 1,
      TRUE ~ NA_real_),
    RBP4_ANC20=case_when(
      RBP4_ANC20==4 ~ 0,
      RBP4_ANC20 %in% c(1,2,3) ~ 1,
      TRUE ~ NA_real_),
    VITB12_COB_ANC20=case_when(
      VITB12_COB_ANC20==1 ~ 0,
      VITB12_COB_ANC20 %in% c(2,3) ~ 1,
      TRUE ~ NA_real_),
    VITB12_HOL_ANC20=case_when(
      VITB12_HOL_ANC20==2 ~ 0,
      VITB12_HOL_ANC20==1 ~ 1,
      TRUE ~ NA_real_),
    FOL_SERUM_ANC20=case_when(
      FOL_SERUM_ANC20==3 ~ 0,
      FOL_SERUM_ANC20 %in% c(1,2,4) ~ 1,
      TRUE ~ NA_real_),
    HIGH_TG_44_ANC20=case_when(
      HIGH_TG_44_ANC20==2 ~ 1,
      HIGH_TG_44_ANC20==1 ~ 0,
      TRUE ~ NA_real_),
    FERRITIN70_ANC32=case_when(
      FERRITIN70_ANC32==2 ~ 1,
      FERRITIN70_ANC32==1 ~ 0,
      TRUE ~ NA_real_),
    STFR_ANC32=case_when(
      STFR_ANC32==2 ~ 0,
      STFR_ANC32 %in% c(1,3) ~ 1,
      TRUE ~ NA_real_),
    RBP4_ANC32=case_when(
      RBP4_ANC32==4 ~ 0,
      RBP4_ANC32 %in% c(1,2,3) ~ 1,
      TRUE ~ NA_real_),
    VITB12_COB_ANC32=case_when(
      VITB12_COB_ANC32==1 ~ 0,
      VITB12_COB_ANC32 %in% c(2,3) ~ 1,
      TRUE ~ NA_real_),
    VITB12_HOL_ANC32=case_when(
      VITB12_HOL_ANC32==2 ~ 0,
      VITB12_HOL_ANC32==1 ~ 1,
      TRUE ~ NA_real_),
    FOL_SERUM_ANC32=case_when(
      FOL_SERUM_ANC32==3 ~ 0,
      FOL_SERUM_ANC32 %in% c(1,2,4) ~ 1,
      TRUE ~ NA_real_),
    HIGH_TG_44_ANC32=case_when(
      HIGH_TG_44_ANC32==2 ~ 1,
      HIGH_TG_44_ANC32==1 ~ 0,
      TRUE ~ NA_real_),
    ###Inflammation
    CRP_ANC20=case_when(
      CRP_ANC20==2 ~ 1,
      CRP_ANC20==1 ~ 0,
      TRUE ~ NA_real_),
    AGP_ANC20=case_when(
      AGP_ANC20==2 ~ 1,
      AGP_ANC20==1 ~ 0,
      TRUE ~ NA_real_),
    CRP_ANC32=case_when(
      CRP_ANC32==2 ~ 1,
      CRP_ANC32==1 ~ 0,
      TRUE ~ NA_real_),
    AGP_ANC32=case_when(
      AGP_ANC32==2 ~ 1,
      AGP_ANC32==1 ~ 0,
      TRUE ~ NA_real_),
    ##RBC
    MCV_ANC20=case_when(
      MCV_ANC20==2 ~ 0,
      MCV_ANC20 %in% c(1,3) ~ 1,
      TRUE ~ NA_real_),
    MCV_ANC32=case_when(
      MCV_ANC32==2 ~ 0,
      MCV_ANC32 %in% c(1,3) ~ 1,
      TRUE ~ NA_real_),
    #Hemorrhage
    HEM_APH=transform_to_NA(HEM_APH),
    ##Supplement
    IRON_Supplement=case_when(
      M04_IRON_IV_CMOCCUR==1|M04_IRON_ORAL_CMOCCUR==1 ~ 1,
      (M04_IRON_IV_CMOCCUR==0|is.na(M04_IRON_IV_CMOCCUR))&(M04_IRON_ORAL_CMOCCUR==0|is.na(M04_IRON_ORAL_CMOCCUR)) ~ 0,
      TRUE ~ NA_real_,
    ),
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
    
    age = transform_negative_to_NA(age),
    bmi_enroll = transform_negative_to_NA(bmi_enroll),
    muac = transform_negative_to_NA(muac),
    ##Fetel Presentation
    INF_PRES_CEPH=transform_to_NA(INF_PRES_CEPH),
    INF_PRES_BREECH=transform_to_NA(INF_PRES_BREECH),
    INF_PRES_TRANS=transform_to_NA(INF_PRES_TRANS),
    INF_PRES_BROW=transform_to_NA(INF_PRES_BROW),
    INF_PRES_OTHER=transform_to_NA(INF_PRES_OTHER),
    ##Medical history
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
    ),
    #Outcome
    INF_ASPH=abs(transform_to_NA(INF_ASPH)-1),
    STILLBIRTH_SIGNS_LIFE=transform_to_NA(STILLBIRTH_SIGNS_LIFE),
    MAT_MORTALITY=transform_to_NA(MAT_MORTALITY),
    composed_outcome=case_when(
      INF_ASPH==0|STILLBIRTH_SIGNS_LIFE==0|MAT_MORTALITY==1 ~ 0,
      is.na(INF_ASPH)&is.na(STILLBIRTH_SIGNS_LIFE)& MAT_MORTALITY %in% c(3,NA) ~ NA_real_,
      TRUE ~ 1
    ))%>%select("SITE","INFANTID","MOMID", "PREGID",
             "INF_ASPH","STILLBIRTH_SIGNS_LIFE","MAT_MORTALITY","composed_outcome",
             "INF_PRES_CEPH","INF_PRES_BREECH","INF_PRES_TRANS","INF_PRES_BROW","INF_PRES_OTHER",
             "ASPHYXIA_IND","STILLBIRTH_IND","PRETERM_IND","POSTTERM_IND","GEST_HTN_IND","PREECLAMPSIA_IND","GEST_DIAB_IND","PREMATURE_RUPTURE_IND","OBSTR_LABOR_IND",
             "miscarriage","paid_work","PARITY_2","PARITY_1","PARITY_0","WEALTH_QUINT_1","WEALTH_QUINT_2","WEALTH_QUINT_3","WEALTH_QUINT_4","WEALTH_QUINT_5","SCHOOL_MORE10","water_improved","toilet_improved","M03_STOVE_FCORRESR_ind","hh_smoke","AGE_GROUP","bmi_index",
             "age","bmi_enroll","muac",
             "MCV_ANC20","AGP_ANC20","CRP_ANC20","HIGH_TG_44_ANC20","FOL_SERUM_ANC20","VITB12_HOL_ANC20","VITB12_COB_ANC20","RBP4_ANC20","STFR_ANC20","FERRITIN70_ANC20",
             "MCV_ANC32","AGP_ANC32","CRP_ANC32","HIGH_TG_44_ANC32","FOL_SERUM_ANC32","VITB12_HOL_ANC32","VITB12_COB_ANC32","RBP4_ANC32","STFR_ANC32","FERRITIN70_ANC32",
             "ANEMIA_ANC20","ANEMIA_ANC32",
             "MEM_CES","MEM_ART","MEM_SPON","LABOR_ANY","PRO_LABOR","OBS_LABOR","PRETERM_ANY","DIAB_OVERT","DEPR_ANC32","DEPR_ANC20",
             "HEM_APH","HIV_POSITIVE_ENROLL","SYPH_POSITIVE_ENROLL","GON_POSITIVE_ENROLL","CHL_POSITIVE_ENROLL","GENU_POSITIVE_ENROLL","OTHR_POSITIVE_ENROLL","MAL_POSITIVE_ENROLL","HBV_POSITIVE_ENROLL","HCV_POSITIVE_ENROLL","TB_SYMP_POSITIVE_ENROLL",
             "IRON_Supplement","M04_IFA_CMOCCUR","M04_CALCIUM_CMOCCUR","M04_VITAMIN_A_CMOCCUR","M04_MICRONUTRIENT_CMOCCUR","M04_ANTHELMINTHIC_CMOCCUR")


analysis_data.dl <- data.dl %>%
  pivot_longer(cols = ends_with("ANC20") | ends_with("ANC32"),
               names_to = c(".value", "Visit"),
               names_pattern = "(.*)_(ANC20|ANC32)")%>%
  mutate(
    ##Outcome
    ANEMIA_OUTCOME1=case_when(
      ANEMIA %in% c(1,2,3) ~ 1,
      ANEMIA == 0 ~ 0,
      TRUE ~ NA_real_),
    ANEMIA_OUTCOME2=case_when(
      ANEMIA %in% c(2,3) ~ 1,
      ANEMIA %in% c(0,1) ~ 0,
      TRUE ~ NA_real_))

analysis_data.ml <- data.dl %>%
  mutate(
    ANEMIA_OUTCOME1_ANC20=case_when(
      ANEMIA_ANC20  %in% c(1,2,3) ~ 1,
      ANEMIA_ANC20 == 0 ~ 0,
      TRUE ~ NA_real_),
    ANEMIA_OUTCOME1_ANC32=case_when(
      ANEMIA_ANC32  %in% c(1,2,3) ~ 1,
      ANEMIA_ANC32 == 0 ~ 0,
      TRUE ~ NA_real_))

data.dl.impute<-data.dl%>%mutate(
  ANEMIA_impute_ANC20=case_when(
    is.na(ANEMIA_ANC20) & is.na(ANEMIA_ANC32) ~ NA,
    is.na(ANEMIA_ANC20) & !is.na(ANEMIA_ANC32) ~ ANEMIA_ANC32,
    !is.na(ANEMIA_ANC20) ~ ANEMIA_ANC20,
  ),
  ANEMIA_impute_ANC32=case_when(
    is.na(ANEMIA_ANC32) & is.na(ANEMIA_ANC20) ~ NA,
    is.na(ANEMIA_ANC32) & !is.na(ANEMIA_ANC20) ~ ANEMIA_ANC20,
    !is.na(ANEMIA_ANC32) ~ ANEMIA_ANC32,
  )
)

analysis_data.dl.impute <- data.dl.impute %>%
  pivot_longer(cols = ends_with("ANC20") | ends_with("ANC32"),
               names_to = c(".value", "Visit"),
               names_pattern = "(.*)_(ANC20|ANC32)")%>%
  mutate(
    ##Outcome
    ANEMIA_OUTCOME1_impute=case_when(
      ANEMIA_impute %in% c(1,2,3) ~ 1,
      ANEMIA_impute == 0 ~ 0,
      TRUE ~ NA_real_),
    ANEMIA_OUTCOME2_impute=case_when(
      ANEMIA_impute %in% c(2,3) ~ 1,
      ANEMIA_impute %in% c(0,1) ~ 0,
      TRUE ~ NA_real_))

write.csv(analysis_data.dl,"D:/Users/yipeng_wei/Documents/dl data/2024-06-28/df_dl.csv")
write.csv(analysis_data.ml,"D:/Users/yipeng_wei/Documents/dl data/2024-06-28/df_ml.csv")
write.csv(analysis_data.dl.impute,"D:/Users/yipeng_wei/Documents/dl data/2024-06-28/df_impute.csv")