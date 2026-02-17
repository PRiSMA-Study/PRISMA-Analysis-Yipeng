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

transform_continuous_to_NA<-function(variable){
  variable[variable < 0]<-NA
  variable[variable > 5000]<-NA
  return(variable)
}

#Site names
site_names <- c("Ghana", "India-CMC", "India-SAS", "Kenya", "Pakistan", "Zambia")

###Import data
UploadDate = "2025-10-31"
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
mnh09 <- mnh09 %>% rename(TYPE_VISIT = M09_TYPE_VISIT)

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

mnh09 <- mnh09 %>%
  filter(MOMID!="") %>% #Moved filter to the end so we can create all the variables with TYPE_VISIT at the end of the var name before rows get dropped out
  drop_na(MOMID, PREGID) %>%
  distinct (MOMID, PREGID, SITE, TYPE_VISIT, .keep_all = TRUE)

data.enroll <- read.csv("D:/Users/yipeng_wei/Documents/dl data/2025-10-31/MAT_ENROLL.csv")%>%select(-"M01_US_OHOSTDAT")

merged_df01 <- left_join(data.enroll, mnh01, by = c("SITE","SCRNID" ,"MOMID", "PREGID"))
merged_df03 <- left_join(data.enroll, mnh03, by = c("SITE", "MOMID", "PREGID"))
merged_df04 <- left_join(data.enroll, mnh04, by = c("SITE", "MOMID", "PREGID"))
merged_df08 <- left_join(data.enroll, mnh08, by = c("SITE", "MOMID", "PREGID"))
merged_df09 <- left_join(data.enroll, mnh09, by = c("SITE","MOMID", "PREGID"))
merged_df13 <- left_join(data.enroll, mnh13, by = c("SITE","MOMID", "PREGID"))

merged_df08 <- merged_df08 %>% mutate(
  M08_UA_PROT_ind = case_when(
    M08_UA_PROT_LBORRES %in% c(2,3,4,5) ~ 1,
    M08_UA_PROT_LBORRES %in% c(0,1) ~ -1,
    TRUE ~ NA_real_
  ),
  M08_UA_LEUK_ind = case_when(
    M08_UA_LEUK_LBORRES %in% c(2,3,4) ~ 1,
    M08_UA_LEUK_LBORRES %in% c(0,1) ~ -1,
    TRUE ~ NA_real_
  ),
  M08_UA_NITRITE_LBORRES = case_when(
    M08_UA_NITRITE_LBORRES == 1 ~ 1,
    M08_UA_NITRITE_LBORRES == 0 ~ -1,
    TRUE ~ NA_real_
  ),
  #GA days
  M08_GA_days = transform_date_to_NA(as.numeric(as.Date(M08_LBSTDAT) - as.Date(PREG_START_DATE))),
  across(c(M08_CBC_MCV_LBORRES, M08_VITB12_COB_LBORRES, M08_VITB12_HOL_LBORRES, 
           M08_FOLATE_PLASMA_NMOLL_LBORRES, M08_IRON_TOT_UGDL_LBORRES, 
           M08_VITA_UGDL_LBORRES, M08_FERRITIN_LBORRES, M08_IODINE_LBORRES, 
           M08_RBP4_LBORRES, M08_CRP_LBORRES, M08_AGP_LBORRES,
           M08_ZINC_LBORRES,M08_IRON_HEP_LBORRES,
           M08_CBC_HB_LBORRES), 
         ~ transform_continuous_to_NA(.)),
  M08_ANEMIA = case_when(
    M08_CBC_HB_LBORRES <= 10 ~ 1,
    M08_CBC_HB_LBORRES > 10 ~ -1,
    TRUE ~ NA_real_
  ),
) %>%
  dplyr::select("MOMID", "PREGID", "SITE", PREG_START_DATE, TYPE_VISIT, M08_LBSTDAT, M08_GA_days,
                M08_CBC_MCV_LBORRES, M08_VITB12_COB_LBORRES, M08_VITB12_HOL_LBORRES,
                M08_FOLATE_PLASMA_NMOLL_LBORRES,
                M08_ZINC_LBORRES,M08_IRON_HEP_LBORRES,
                M08_IRON_TOT_UGDL_LBORRES,
                M08_VITA_UGDL_LBORRES,
                #M08_FOLATE_RBC_NMOLL_LBORRES,
                M08_FERRITIN_LBORRES,M08_IODINE_LBORRES,M08_RBP4_LBORRES,M08_CRP_LBORRES,M08_AGP_LBORRES,
                M08_CBC_HB_LBORRES,M08_ANEMIA,
                M08_UA_PROT_ind, M08_UA_LEUK_ind, M08_UA_NITRITE_LBORRES)

#Correct the unit difference for Ferritin across site
p50_ferritin_gh  <- median(merged_df08$M08_FERRITIN_LBORRES[merged_df08$SITE == "Ghana"], na.rm = TRUE)
p50_ferritin_cmc  <- median(merged_df08$M08_FERRITIN_LBORRES[merged_df08$SITE == "India-CMC"], na.rm = TRUE)
p50_ferritin_sas  <- median(merged_df08$M08_FERRITIN_LBORRES[merged_df08$SITE == "India-SAS"], na.rm = TRUE)
p50_ferritin_ky  <- median(merged_df08$M08_FERRITIN_LBORRES[merged_df08$SITE == "Kenya"], na.rm = TRUE)
p50_ferritin_pk  <- median(merged_df08$M08_FERRITIN_LBORRES[merged_df08$SITE == "Pakistan"], na.rm = TRUE)
p50_ferritin_zm  <- median(merged_df08$M08_FERRITIN_LBORRES[merged_df08$SITE == "Zambia"], na.rm = TRUE)

if (p50_ferritin_gh < 10 ) {
  merged_df08$M08_FERRITIN_LBORRES[merged_df08$SITE == "Ghana"] <- merged_df08$M08_FERRITIN_LBORRES[merged_df08$SITE == "Ghana"] * 10
} else  { merged_df08$M08_FERRITIN_LBORRES[merged_df08$SITE == "Ghana"] <- merged_df08$M08_FERRITIN_LBORRES[merged_df08$SITE == "Ghana"]
}

if (p50_ferritin_cmc < 10 ) {
  merged_df08$M08_FERRITIN_LBORRES[merged_df08$SITE == "India-CMC"] <- merged_df08$M08_FERRITIN_LBORRES[merged_df08$SITE == "India-CMC"] * 10
} else  { merged_df08$M08_FERRITIN_LBORRES[merged_df08$SITE == "India-CMC"] <- merged_df08$M08_FERRITIN_LBORRES[merged_df08$SITE == "India-CMC"]
}


if (p50_ferritin_sas < 10 ) {
  merged_df08$M08_FERRITIN_LBORRES[merged_df08$SITE == "India-SAS"] <- merged_df08$M08_FERRITIN_LBORRES[merged_df08$SITE == "India-SAS"] * 10
} else  { merged_df08$M08_FERRITIN_LBORRES[merged_df08$SITE == "India-SAS"] <- merged_df08$M08_FERRITIN_LBORRES[merged_df08$SITE == "India-SAS"]
}


if (p50_ferritin_ky < 15 ) {
  merged_df08$M08_FERRITIN_LBORRES[merged_df08$SITE == "Kenya"] <- merged_df08$M08_FERRITIN_LBORRES[merged_df08$SITE == "Kenya"] * 10
} else  { merged_df08$M08_FERRITIN_LBORRES[merged_df08$SITE == "Kenya"] <- merged_df08$M08_FERRITIN_LBORRES[merged_df08$SITE == "Kenya"]
}


if (p50_ferritin_pk < 10 ) {
  merged_df08$M08_FERRITIN_LBORRES[merged_df08$SITE == "Pakistan"] <- merged_df08$M08_FERRITIN_LBORRES[merged_df08$SITE == "Pakistan"] * 10
} else  { merged_df08$M08_FERRITIN_LBORRES[merged_df08$SITE == "Pakistan"] <- merged_df08$M08_FERRITIN_LBORRES[merged_df08$SITE == "Pakistan"]
}

if (p50_ferritin_zm < 10 ) {
  merged_df08$M08_FERRITIN_LBORRES[merged_df08$SITE == "Zambia"] <- merged_df08$M08_FERRITIN_LBORRES[merged_df08$SITE == "Zambia"] * 10
} else  { merged_df08$M08_FERRITIN_LBORRES[merged_df08$SITE == "Zambia"] <- merged_df08$M08_FERRITIN_LBORRES[merged_df08$SITE == "Zambia"]
}


merged_df01<- merged_df01 %>%
  mutate(
    M01_GA_days = transform_date_to_NA(as.numeric(as.Date(M01_US_OHOSTDAT) - as.Date(PREG_START_DATE))),
  )

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
    ),
    CESARIAN_IND = case_when(
      M04_CESARIAN_RPORRES == 1 ~ 1, #C-section,
      M04_PH_PREV_RPORRES == 0 | M04_CESARIAN_RPORRES == 0 ~ -1,
      TRUE ~ NA_real_
    ),
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
                       OBSTR_LABOR_IND,
                       #C-section
                       CESARIAN_IND)

df_infant_id<-merged_df09%>%select("MOMID", "PREGID", "SITE",matches("INFANTID"))

df_ultrasound_pre <- merged_df01 %>%
  select(
    MOMID, PREGID, SITE, M01_US_OHOSTDAT,
    M01_GA_days,
    TYPE_VISIT = M01_TYPE_VISIT,
    matches("PREVIA"),
    matches("M01_AFI_PERES_FTS")
  ) %>%
  mutate(
    across(matches("M01_AFI_PERES_FTS"), ~ transform_continuous_to_NA(.)),
    across(matches("PREVIA"), ~ transform_to_NA(.))) %>%
  mutate(
    M01_AFI_PERES_FTS1 = ifelse(
      is.nan(rowMeans(select(., starts_with("M01_AFI_PERES_FTS1_Q")), na.rm = TRUE)),
      NA,
      rowMeans(select(., starts_with("M01_AFI_PERES_FTS1_Q")), na.rm = TRUE)
    ),
    M01_AFI_PERES_FTS2 = ifelse(
      is.nan(rowMeans(select(., starts_with("M01_AFI_PERES_FTS2_Q")), na.rm = TRUE)),
      NA,
      rowMeans(select(., starts_with("M01_AFI_PERES_FTS2_Q")), na.rm = TRUE)
    ),
    M01_AFI_PERES_FTS3 = ifelse(
      is.nan(rowMeans(select(., starts_with("M01_AFI_PERES_FTS3_Q")), na.rm = TRUE)),
      NA,
      rowMeans(select(., starts_with("M01_AFI_PERES_FTS3_Q")), na.rm = TRUE)
    ),
    M01_AFI_PERES_FTS4 = ifelse(
      is.nan(rowMeans(select(., starts_with("M01_AFI_PERES_FTS4_Q")), na.rm = TRUE)),
      NA,
      rowMeans(select(., starts_with("M01_AFI_PERES_FTS4_Q")), na.rm = TRUE)
    )
  ) %>%
  select(-matches("_Q\\d$"))

df_infant_id_long <- df_infant_id %>%
  pivot_longer(
    cols = starts_with("M09_INFANTID_INF"),
    names_to = "FETUS_INDEX",
    names_pattern = "M09_INFANTID_INF(\\d)",
    values_to = "INFANTID"
  ) %>%
  mutate(FETUS_INDEX = as.integer(FETUS_INDEX)) %>%
  filter(!is.na(INFANTID) & INFANTID != "n/a" & INFANTID != "")

df_breastfed<-merged_df13%>%select("SITE","SCRNID" ,"MOMID", "PREGID","INFANTID",TYPE_VISIT="M13_TYPE_VISIT","M13_BRSTFEED_EXCL_FAORRES")%>%filter(TYPE_VISIT %in% c(7,8,9)) %>%
  group_by(PREGID) %>%
  mutate(
    M13_BRSTFEED = ifelse(all(is.na(transform_to_NA(M13_BRSTFEED_EXCL_FAORRES))), NA_integer_, as.integer(max(transform_to_NA(M13_BRSTFEED_EXCL_FAORRES), na.rm = TRUE)))
  ) %>% ungroup() %>% distinct(PREGID, .keep_all = TRUE) %>% select(-TYPE_VISIT)

df_ultrasound <-  df_ultrasound_pre %>%
  pivot_longer(
    cols = starts_with("M01_AFI_PERES_FTS") | starts_with("M01_PREVIA_PERES_FTS"),
    names_to   = c(".value", "FETUS_INDEX"),
    names_pattern = "(M01_[A-Z_]+)_FTS(\\d)",
    values_drop_na = TRUE
  ) %>%
  mutate(
    FETUS_INDEX = as.integer(FETUS_INDEX)
  ) %>%
  relocate(FETUS_INDEX, .after = TYPE_VISIT) %>%
  left_join(
    df_infant_id_long,
    by = c("MOMID", "PREGID", "SITE", "FETUS_INDEX")
  )%>%filter(!is.na(INFANTID))%>% relocate(INFANTID, .after = PREGID) %>% mutate(
   AFI= case_when(
     M01_AFI_PERES < 5 ~ 1,
     M01_AFI_PERES > 5 ~ -1,
     TRUE ~ NA_real_
  ),
  PREVIA= case_when(
    M01_PREVIA_PERES == 3 ~ 1,
    M01_PREVIA_PERES %in% c(1,2) ~ -1,
    TRUE ~ NA_real_
  ))

df_history <- merged_df04 %>% filter(TYPE_VISIT==1) %>% 
  dplyr::select( "MOMID", "PREGID", "SITE",
                 "ASPHYXIA_IND","STILLBIRTH_IND","PRETERM_IND","POSTTERM_IND","GEST_HTN_IND","PREECLAMPSIA_IND","GEST_DIAB_IND","PREMATURE_RUPTURE_IND","OBSTR_LABOR_IND","CESARIAN_IND")

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

df_room <- merged_df03 %>% 
  mutate(
    #num of room
    M03_HOUSE_ROOMS_FCORRES= transform_negative_to_NA(M03_HOUSE_ROOMS_FCORRES)
  ) %>% dplyr::select( "MOMID", "PREGID", "SITE", TYPE_VISIT,
                       M03_HOUSE_ROOMS_FCORRES)

df_psbi<-read.csv("D:/Users/yipeng_wei/Documents/dl data/2025-10-31/INF_PSBI_LONG.csv") %>% filter (TYPE_VISIT%in%c(6,7,8,9)) %>% 
  select("SITE","MOMID", "PREGID","INFANTID","TYPE_VISIT","INF_PSBI","INF_PSBI_IPC","INF_PSBI_PNC0","INF_PSBI_PNC1","INF_PSBI_PNC4") %>% 
  group_by(INFANTID) %>%
  mutate(
    INF_PSBI_ANY = ifelse(all(is.na(transform_to_NA(INF_PSBI))), NA_integer_, as.integer(max(transform_to_NA(INF_PSBI), na.rm = TRUE))),
    INF_PSBI_OUTCOME = ifelse(all(is.na(transform_to_NA(INF_PSBI_PNC4))), NA_integer_, as.integer(max(transform_to_NA(INF_PSBI_PNC4), na.rm = TRUE)))
  ) %>% ungroup() %>% distinct(INFANTID, .keep_all = TRUE) %>% select(-c("TYPE_VISIT","INF_PSBI","INF_PSBI_IPC","INF_PSBI_PNC0","INF_PSBI_PNC1","INF_PSBI_PNC4"))

data.facility <- merged_df09 %>% select("SITE","MOMID", "PREGID","M09_MAT_LD_OHOLOC")

df_inf_outcome<-read.csv("D:/Users/yipeng_wei/Documents/dl data/2025-10-31/INF_OUTCOMES.csv")
data.risk <- read_dta("D:/Users/yipeng_wei/Documents/dl data/2025-10-31/MAT_RISKS.dta")
data.demographic<-read.csv("D:/Users/yipeng_wei/Documents/dl data/2025-10-31/MAT_DEMOGRAPHIC.csv")
data.infection<-read.csv("D:/Users/yipeng_wei/Documents/dl data/2025-10-31/MAT_INFECTION.csv")
data.hemorr <- read.csv("D:/Users/yipeng_wei/Documents/dl data/2025-10-31/MAT_HEMORRHAGE.csv")
data.labor <- read_dta("D:/Users/yipeng_wei/Documents/dl data/2025-10-31/MAT_LABOR.dta")
data.preterm <- read_dta("D:/Users/yipeng_wei/Documents/dl data/2025-10-31/MAT_PRETERM.dta")
data.hdp <- read_dta("D:/Users/yipeng_wei/Documents/dl data/2025-10-31/MAT_HDP.dta")
data.gdm <- read_dta("D:/Users/yipeng_wei/Documents/dl data/2025-10-31/MAT_GDM.dta")
data.nearmiss <- read_dta("D:/Users/yipeng_wei/Documents/dl data/2025-10-31/MAT_NEAR_MISS.dta")
data.depr <- read_dta("D:/Users/yipeng_wei/Documents/dl data/2025-10-31/MAT_DEPR.dta")
data.gwg <- read_dta("D:/Users/yipeng_wei/Documents/dl data/2025-10-31/MAT_GWG.dta")
df_inf_anomaly <- read_dta("D:/Users/yipeng_wei/Documents/dl data/2025-10-31/INF_ANOMALY.dta")
df_inf_presentation <- read_dta("D:/Users/yipeng_wei/Documents/dl data/2025-10-31/INF_PRESENTATION.dta")

data.nearmiss<-data.nearmiss%>%select(c("SITE","MOMID","PREGID","NEARMISS"))

prisma.id<-inner_join(mnh02, data.enroll, by = c("SITE","MOMID","PREGID"))
prisma.id<-prisma.id %>% select("SITE", "MOMID", "PREGID")
prisma.id <- distinct(prisma.id, SITE, MOMID, PREGID)

data.static <- df_inf_outcome %>% 
  left_join(df_inf_presentation, by = c("SITE","MOMID", "PREGID","INFANTID")) %>%
  left_join(df_inf_anomaly, by = c("SITE", "MOMID", "PREGID", "INFANTID")) %>%
  left_join(df_breastfed, by = c("SITE", "MOMID", "PREGID", "INFANTID")) %>%
  left_join(df_psbi, by = c("SITE","MOMID", "PREGID","INFANTID")) %>%
  left_join(prisma.id, by = c("SITE", "MOMID","PREGID")) %>%
  left_join(data.facility, by = c("SITE", "MOMID", "PREGID")) %>%
  left_join(data.risk, by = c("SITE", "MOMID", "PREGID")) %>%
  left_join(data.demographic, by = c("SITE", "MOMID", "PREGID")) %>%
  left_join(data.infection, by = c("SITE", "MOMID", "PREGID")) %>%
  left_join(data.hemorr, by = c("SITE", "MOMID", "PREGID")) %>%
  left_join(data.labor, by = c("SITE", "MOMID", "PREGID")) %>%
  left_join(data.preterm, by = c("SITE", "MOMID", "PREGID")) %>%
  left_join(data.hdp, by = c("SITE", "MOMID", "PREGID")) %>%
  left_join(data.gdm, by = c("SITE", "MOMID", "PREGID")) %>%
  left_join(data.depr, by = c("SITE", "MOMID", "PREGID")) %>%
  left_join(data.gwg, by = c("SITE", "MOMID", "PREGID")) %>%
  left_join(data.nearmiss, by = c("SITE", "MOMID", "PREGID")) %>%
  left_join(df_fuel, by = c("SITE", "MOMID", "PREGID"))%>% 
  left_join(df_room, by = c("SITE", "MOMID", "PREGID"))%>% 
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
    HIGH_BP_SEVERE_ANY = case_when(
      HIGH_BP_SEVERE_ANY == 1 ~ 1,
      HIGH_BP_SEVERE_ANY == 0 ~ -1,
      TRUE ~ NA_real_
    ),
    MAT_CES_ANY = case_when(
      MAT_CES_ANY == 1 ~ 1,
      MAT_CES_ANY == 0 ~ -1,
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
    GPARITY_0 = case_when(
      GPARITY_0 == 1 ~ 1,
      GPARITY_0 == 0 ~ -1,
      TRUE ~ NA_real_
    ),
    GPARITY_1 = case_when(
      GPARITY_1 == 1 ~ 1,
      GPARITY_1 == 0 ~ -1,
      TRUE ~ NA_real_
    ),
    GPARITY_2 = case_when(
      GPARITY_2 == 1 ~ 1,
      GPARITY_2 == 0 ~ -1,
      TRUE ~ NA_real_
    ),
    PAID_WORK = case_when(
      PAID_WORK == 1 ~ 1,
      PAID_WORK == 0 ~ -1,
      TRUE ~ NA_real_
    ),
    INF_SEX = case_when(
      SEX == 1 ~ 1,
      SEX == 2 ~ -1,
      TRUE ~ NA_real_
    ),
    INF_HYPERBILI_NICE = case_when(
      # If any variable is 1 ~ Positive
      rowSums(across(c(
        INF_HYPERBILI_NICE_24HR, INF_HYPERBILI_NICE_5DAY,
        INF_HYPERBILI_NICE_14DAY
      ), ~ .x == 1), na.rm = TRUE) > 0 ~ 1,
      
      # If none are 1, and at least one is 0 ~ Negative
      rowSums(across(c(
        INF_HYPERBILI_NICE_24HR, INF_HYPERBILI_NICE_5DAY,
        INF_HYPERBILI_NICE_14DAY
      ), ~ .x == 1), na.rm = TRUE) == 0 &
        rowSums(across(c(
          INF_HYPERBILI_NICE_24HR, INF_HYPERBILI_NICE_5DAY,
          INF_HYPERBILI_NICE_14DAY
        ), ~ .x == 0), na.rm = TRUE) > 0 ~ -1,
      
      # All NA or unknown ~ NA
      TRUE ~ NA_real_
    ),
    CROWDING_IND = round(HH_SIZE/M03_HOUSE_ROOMS_FCORRES,2),
    AGE_GROUP = transform_to_NA(AGE_GROUP),
    
    #BMI level
    BMI_LEVEL_ENROLL_underweight = ifelse(BMI_LEVEL_ENROLL == 1, 1, ifelse(!is.na(BMI_LEVEL_ENROLL), -1, NA)),
    BMI_LEVEL_ENROLL_normal = ifelse(BMI_LEVEL_ENROLL == 2, 1, ifelse(!is.na(BMI_LEVEL_ENROLL), -1, NA)),
    BMI_LEVEL_ENROLL_overweight = ifelse(BMI_LEVEL_ENROLL == 3, 1, ifelse(!is.na(BMI_LEVEL_ENROLL), -1, NA)),
    BMI_LEVEL_ENROLL_obese = ifelse(BMI_LEVEL_ENROLL == 4, 1, ifelse(!is.na(BMI_LEVEL_ENROLL), -1, NA)),
    
    # Gestional weight gain adequacy
    GWG_ADEQUACY_inadequate = ifelse(GWG_ADEQUACY == 1, 1, ifelse(!is.na(GWG_ADEQUACY), -1, NA)),
    GWG_ADEQUACY_adequate = ifelse(GWG_ADEQUACY == 2, 1, ifelse(!is.na(GWG_ADEQUACY), -1, NA)),
    GWG_ADEQUACY_excessive = ifelse(GWG_ADEQUACY == 3, 1, ifelse(!is.na(GWG_ADEQUACY), -1, NA)),
    
    MAT_AGE = transform_negative_to_NA(MAT_AGE),
    NUM_MISCARRIAGE_ind = case_when(
      NUM_MISCARRIAGE >=3 ~ 1,
      NUM_MISCARRIAGE < 3 ~ -1,
      TRUE ~ NA_real_                 
    ),
    BMI_ENROLL = transform_negative_to_NA(BMI_ENROLL),
    MUAC_ENROLL = transform_negative_to_NA(MUAC_ENROLL),
    #Number of fetus
    NUM_FETUS_1 = ifelse(NUM_FETUS == 1, 1, ifelse(!is.na(NUM_FETUS), -1, NA)),
    NUM_FETUS_2 = ifelse(NUM_FETUS == 2, 1, ifelse(!is.na(NUM_FETUS), -1, NA)),
    NUM_FETUS_3 = ifelse(NUM_FETUS == 3, 1, ifelse(!is.na(NUM_FETUS), -1, NA)),
    GESTAGEBIRTH_ANY = transform_negative_to_NA(GESTAGEBIRTH_ANY),
    BWEIGHT_ANY = transform_negative_to_NA(BWEIGHT_ANY),
    #Depression
    DEPR_EVER = case_when(
      DEPR_SITE_ANC_EVER_NUM == 1 & DEPR_ANC_EVER_DENOM == 1 ~ 1,
      DEPR_SITE_ANC_EVER_NUM == 0 & DEPR_ANC_EVER_DENOM == 1 ~ -1,
      is.na(DEPR_SITE_ANC_EVER_NUM) & DEPR_ANC_EVER_DENOM == 1 ~ -1,
      TRUE ~ NA_real_
    ),
    #Place of birth
    BIRTH_FACILITY = case_when(
      M09_MAT_LD_OHOLOC == 1 ~ 1,
      M09_MAT_LD_OHOLOC == 2 ~ -1,
      TRUE ~ NA_real_
    ),
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
    ), 
    # HIV Positive Enrollment
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
    NG_TEST_POSITIVE_ENROLL = case_when(
      NG_TEST_POSITIVE_ENROLL == 1 ~ 1,
      NG_TEST_POSITIVE_ENROLL == 0 ~ -1,
      TRUE ~ NA_real_
    ),
    
    # Chlamydia Positive Enrollment
    CT_TEST_POSITIVE_ENROLL = case_when(
      CT_TEST_POSITIVE_ENROLL == 1 ~ 1,
      CT_TEST_POSITIVE_ENROLL == 0 ~ -1,
      TRUE ~ NA_real_
    ),
    
    # Genital Ulcers Positive Enrollment
    HBV_POSITIVE_ENROLL = case_when(
      HBV_POSITIVE_ENROLL == 1 ~ 1,
      HBV_POSITIVE_ENROLL == 0 ~ -1,
      TRUE ~ NA_real_
    ),
    
    # Other Infections Positive Enrollment
    HCV_POSITIVE_ENROLL = case_when(
      HCV_POSITIVE_ENROLL == 1 ~ 1,
      HCV_POSITIVE_ENROLL == 0 ~ -1,
      TRUE ~ NA_real_
    ),
    
    STI_POSITIVE_ENROLL = case_when(
      # If any variable is 1 ~ Positive
      rowSums(across(c(
        SYPH_POSITIVE_ENROLL, NG_TEST_POSITIVE_ENROLL, CT_TEST_POSITIVE_ENROLL
      ), ~ .x == 1), na.rm = TRUE) > 0 ~ 1,
      
      # If none are 1, and at least one is 0 ~ Negative
      rowSums(across(c(
        SYPH_POSITIVE_ENROLL, NG_TEST_POSITIVE_ENROLL, CT_TEST_POSITIVE_ENROLL
      ), ~ .x == 1), na.rm = TRUE) == 0 &
        rowSums(across(c(
          SYPH_POSITIVE_ENROLL, NG_TEST_POSITIVE_ENROLL, CT_TEST_POSITIVE_ENROLL
        ), ~ .x == -1), na.rm = TRUE) > 0 ~ -1,
      
      # All NA or unknown ~ NA
      TRUE ~ NA_real_
    ),
    
    # Malaria Positive Enrollment
    MAL_POSITIVE_ENROLL = case_when(
      MAL_POSITIVE_ENROLL == 1 ~ 1,
      MAL_POSITIVE_ENROLL == 0 ~ -1,
      TRUE ~ NA_real_
    ),
    
    # Tuberculosis Symptoms Positive Enrollment
    TB_POSITIVE_ENROLL = case_when(
      TB_POSITIVE_ENROLL == 1 ~ 1,
      TB_POSITIVE_ENROLL == 0 ~ -1,
      TRUE ~ NA_real_
    ),
    HIV_POSITIVE_EVER_PREG = case_when(
      HIV_POSITIVE_EVER_PREG == 1 ~ 1,
      HIV_POSITIVE_EVER_PREG == 0 ~ -1,
      TRUE ~ NA_real_
    ),
    
    # Syphilis Positive during ANC
    SYPH_POSITIVE_EVER_PREG = case_when(
      SYPH_POSITIVE_EVER_PREG == 1 ~ 1,
      SYPH_POSITIVE_EVER_PREG == 0 ~ -1,
      TRUE ~ NA_real_
    ),
    
    # Gonorrhea Positive during ANC
    NG_TEST_POSITIVE_EVER_PREG = case_when(
      NG_TEST_POSITIVE_EVER_PREG == 1 ~ 1,
      NG_TEST_POSITIVE_EVER_PREG == 0 ~ -1,
      TRUE ~ NA_real_
    ),
    
    # Chlamydia Positive during ANC
    CT_TEST_POSITIVE_EVER_PREG = case_when(
      CT_TEST_POSITIVE_EVER_PREG == 1 ~ 1,
      CT_TEST_POSITIVE_EVER_PREG == 0 ~ -1,
      TRUE ~ NA_real_
    ),
  
    STI_POSITIVE_EVER_PREG = case_when(
      # If any variable is 1 ~ Positive
      rowSums(across(c(
        SYPH_POSITIVE_EVER_PREG, NG_TEST_POSITIVE_EVER_PREG, CT_TEST_POSITIVE_EVER_PREG
      ), ~ .x == 1), na.rm = TRUE) > 0 ~ 1,
      
      # If none are 1, and at least one is 0 ~ Negative
      rowSums(across(c(
       SYPH_POSITIVE_EVER_PREG, NG_TEST_POSITIVE_EVER_PREG, CT_TEST_POSITIVE_EVER_PREG
      ), ~ .x == 1), na.rm = TRUE) == 0 &
        rowSums(across(c(
          SYPH_POSITIVE_EVER_PREG, NG_TEST_POSITIVE_EVER_PREG, CT_TEST_POSITIVE_EVER_PREG
        ), ~ .x == -1), na.rm = TRUE) > 0 ~ -1,
      
      # All NA or unknown ~ NA
      TRUE ~ NA_real_
    ),
    
    # Malaria Positive during ANC
    MAL_POSITIVE_EVER_PREG = case_when(
      MAL_POSITIVE_EVER_PREG == 1 ~ 1,
      MAL_POSITIVE_EVER_PREG == 0 ~ -1,
      TRUE ~ NA_real_
    ),
    
    # Hepatitis B Positive during ANC
    HBV_POSITIVE_EVER_PREG = case_when(
      HBV_POSITIVE_EVER_PREG == 1 ~ 1,
      HBV_POSITIVE_EVER_PREG == 0 ~ -1,
      TRUE ~ NA_real_
    ),
    
    # Hepatitis C Positive during ANC
    HCV_POSITIVE_EVER_PREG = case_when(
      HCV_POSITIVE_EVER_PREG == 1 ~ 1,
      HCV_POSITIVE_EVER_PREG == 0 ~ -1,
      TRUE ~ NA_real_
    ),
    
    # Tuberculosis Symptoms Positive during ANC
    TB_POSITIVE_EVER_PREG = case_when(
      TB_POSITIVE_EVER_PREG == 1 ~ 1,
      TB_POSITIVE_EVER_PREG == 0 ~ -1,
      TRUE ~ NA_real_
    ),
    
    HIV_POSITIVE = case_when(
      # If any variable is 1 ~ Positive
      rowSums(across(c(
        HIV_POSITIVE_ENROLL, HIV_POSITIVE_EVER_PREG
      ), ~ .x == 1), na.rm = TRUE) > 0 ~ 1,
      
      # If none are 1, and at least one is 0 ~ Negative
      rowSums(across(c(
        HIV_POSITIVE_ENROLL, HIV_POSITIVE_EVER_PREG
      ), ~ .x == 1), na.rm = TRUE) == 0 &
        rowSums(across(c(
          HIV_POSITIVE_ENROLL, HIV_POSITIVE_EVER_PREG
        ), ~ .x == -1), na.rm = TRUE) > 0 ~ -1,
      
      # All NA or unknown ~ NA
      TRUE ~ NA_real_
    ),
    
    TB_SYMP_POSITIVE = case_when(
      # If any variable is 1 ~ Positive
      rowSums(across(c(
        TB_POSITIVE_ENROLL, TB_POSITIVE_EVER_PREG
      ), ~ .x == 1), na.rm = TRUE) > 0 ~ 1,
      
      # If none are 1, and at least one is 0 ~ Negative
      rowSums(across(c(
        TB_POSITIVE_ENROLL, TB_POSITIVE_EVER_PREG
      ), ~ .x == 1), na.rm = TRUE) == 0 &
        rowSums(across(c(
          TB_POSITIVE_ENROLL, TB_POSITIVE_EVER_PREG
        ), ~ .x == -1), na.rm = TRUE) > 0 ~ -1,
      
      # All NA or unknown ~ NA
      TRUE ~ NA_real_
    ),
    
    MAL_POSITIVE = case_when(
      # If any variable is 1 ~ Positive
      rowSums(across(c(
        MAL_POSITIVE_ENROLL, MAL_POSITIVE_EVER_PREG
      ), ~ .x == 1), na.rm = TRUE) > 0 ~ 1,
      
      # If none are 1, and at least one is 0 ~ Negative
      rowSums(across(c(
        MAL_POSITIVE_ENROLL, MAL_POSITIVE_EVER_PREG
      ), ~ .x == 1), na.rm = TRUE) == 0 &
        rowSums(across(c(
          MAL_POSITIVE_ENROLL, MAL_POSITIVE_EVER_PREG
        ), ~ .x == -1), na.rm = TRUE) > 0 ~ -1,
      
      # All NA or unknown ~ NA
      TRUE ~ NA_real_
    ),
    
    
    STI_POSITIVE = case_when(
      # If any variable is 1 ~ Positive
      rowSums(across(c(
        STI_POSITIVE_ENROLL, STI_POSITIVE_EVER_PREG
      ), ~ .x == 1), na.rm = TRUE) > 0 ~ 1,
      
      # If none are 1, and at least one is 0 ~ Negative
      rowSums(across(c(
        STI_POSITIVE_ENROLL, STI_POSITIVE_EVER_PREG
      ), ~ .x == 1), na.rm = TRUE) == 0 &
        rowSums(across(c(
          STI_POSITIVE_ENROLL, STI_POSITIVE_EVER_PREG
        ), ~ .x == -1), na.rm = TRUE) > 0 ~ -1,
      
      # All NA or unknown ~ NA
      TRUE ~ NA_real_
    ),
    
    #Hypertension
    HTN = case_when(
      HDP_GROUP == 1 ~ 1,
      HDP_GROUP %in% c(0,2,3,4,5) ~ -1,
      TRUE ~ NA_real_
    ),
    GES_HTN = case_when(
      HDP_GROUP %in% c(2,3,4) ~ 1,
      HDP_GROUP %in% c(0,1,5) ~ -1,
      TRUE ~ NA_real_
    ),
    #Diabetes
    DIAB_OVERT_ANY = case_when(
      DIAB_OVERT_ANY == 1 ~ 1,
      DIAB_OVERT_ANY == 0 ~ -1,
      TRUE ~ NA_real_
    ),
    DIAB_GEST_ANY = case_when(
      DIAB_GEST_ANY == 1 ~ 1,
      DIAB_GEST_ANY == 0 ~ -1,
      TRUE ~ NA_real_
    ),
    BREASTFED = case_when(
      M13_BRSTFEED == 1 ~ 1,
      M13_BRSTFEED == 0 ~ -1,
      TRUE ~ NA_real_
    ),
    INF_ANOMALY =case_when(
      INF_ANOMALY == 1 ~ 1,
      INF_ANOMALY == 0 ~ -1,
      TRUE ~ NA_real_
    ),
    INF_PSBI_ANY =case_when(
      INF_PSBI_ANY == 1 ~ 1,
      INF_PSBI_ANY == 0 ~ -1,
      TRUE ~ NA_real_
    ),
    #Outcome
    INF_ASPH=transform_to_NA(INF_ASPH),
    STILLBIRTH_SIGNS_LIFE=abs(transform_to_NA(STILLBIRTH_SIGNS_LIFE)-1),
    PRETERMBIRTH_LT37=transform_to_NA(PRETERMBIRTH_LT37),
    INF_PSBI_OUTCOME=transform_to_NA(INF_PSBI_OUTCOME),
    INF_JAUN_NON_SEV_ANY=transform_to_NA(INF_JAUN_NON_SEV_ANY),
    NEARMISS=transform_to_NA(NEARMISS),
    LBW2500_ANY=transform_to_NA(LBW2500_ANY),
    NEO_DTH=transform_to_NA(NEO_DTH),
    SVN = case_when(
      # If any variable is 1 ~ Positive
      rowSums(across(c(
        INF_SGA_PRETERM, INF_AGA_PRETERM, INF_SGA_TERM
      ), ~ .x == 1), na.rm = TRUE) > 0 ~ 1,
      
      # If none are 1, and at least one is 0 ~ Negative
      rowSums(across(c(
        INF_SGA_PRETERM, INF_AGA_PRETERM, INF_SGA_TERM
      ), ~ .x == 1), na.rm = TRUE) == 0 &
        rowSums(across(c(
          INF_SGA_PRETERM, INF_AGA_PRETERM, INF_SGA_TERM
        ), ~ .x == 0), na.rm = TRUE) > 0 ~ 0,
      
      # All NA or unknown ~ NA
      TRUE ~ NA_real_
    )) %>% select("SITE","INFANTID","MOMID", "PREGID",
                  "INF_ASPH","STILLBIRTH_SIGNS_LIFE","SVN","INF_PSBI_OUTCOME","PRETERMBIRTH_LT37","NEARMISS","NEO_DTH","LBW2500_ANY","INF_JAUN_NON_SEV_ANY",
                  "INF_PRES_CEPH","INF_PRES_BREECH","INF_PRES_TRANS","INF_PRES_BROW","INF_PRES_OTHER",
                  "DEPR_EVER",
                  "ASPHYXIA_IND","STILLBIRTH_IND","PRETERM_IND","POSTTERM_IND","GEST_HTN_IND","PREECLAMPSIA_IND","GEST_DIAB_IND","PREMATURE_RUPTURE_IND","OBSTR_LABOR_IND","CESARIAN_IND",
                  "NUM_MISCARRIAGE_ind", "NUM_MISCARRIAGE","PAID_WORK","GPARITY_2","GPARITY_1","GPARITY_0","WEALTH_QUINT_1","WEALTH_QUINT_2","WEALTH_QUINT_3","WEALTH_QUINT_4","WEALTH_QUINT_5","WEALTH_QUINT_55","SCHOOL_MORE10","WATER_IMPROVED","TOILET_IMPROVED","STOVE_FUEL","HH_SMOKE","SMOKE","CHEW_TOBACCO","CHEW_BETELNUT","AGE_GROUP","BMI_LEVEL_ENROLL","CROWDING_IND",
                  "BMI_LEVEL_ENROLL_underweight","BMI_LEVEL_ENROLL_normal","BMI_LEVEL_ENROLL_overweight","BMI_LEVEL_ENROLL_obese",
                  "GWG_ADEQUACY_inadequate","GWG_ADEQUACY_adequate","GWG_ADEQUACY_excessive",
                  "MAT_AGE","BMI_ENROLL","MUAC_ENROLL","NUM_FETUS",
                  "NUM_FETUS_1","NUM_FETUS_2","NUM_FETUS_3",
                  "MEM_CES","MEM_ART","MEM_SPON","HIGH_BP_SEVERE_ANY","LABOR_ANY","PRO_LABOR","OBS_LABOR","MAT_PRETERM_ANY","MAT_CES_ANY","BIRTH_FACILITY",
                  "HEM_APH","HTN","GES_HTN","DIAB_OVERT_ANY","DIAB_GEST_ANY",
                  "INF_SEX","GESTAGEBIRTH_ANY","BWEIGHT_ANY","INF_HYPERBILI_NICE","BREASTFED",
                  "INF_ANOMALY","INF_PSBI_ANY",
                  "HIV_POSITIVE_ENROLL","SYPH_POSITIVE_ENROLL","NG_TEST_POSITIVE_ENROLL","CT_TEST_POSITIVE_ENROLL","HBV_POSITIVE_ENROLL","HCV_POSITIVE_ENROLL","MAL_POSITIVE_ENROLL","TB_POSITIVE_ENROLL","STI_POSITIVE_ENROLL",
                  "HIV_POSITIVE_EVER_PREG","SYPH_POSITIVE_EVER_PREG","NG_TEST_POSITIVE_EVER_PREG","CT_TEST_POSITIVE_EVER_PREG","HBV_POSITIVE_EVER_PREG","HCV_POSITIVE_EVER_PREG","MAL_POSITIVE_EVER_PREG","TB_POSITIVE_EVER_PREG","STI_POSITIVE_EVER_PREG",
                  "HIV_POSITIVE","MAL_POSITIVE","TB_SYMP_POSITIVE","STI_POSITIVE")

subject.list <- data.static %>%
  expand_grid(TYPE_VISIT = c(1, 2, 3, 4, 5)) %>% 
  select("SITE","INFANTID","MOMID", "PREGID", "TYPE_VISIT")

data.temporal_pre <- df_supplement %>%
  full_join(merged_df08, by = c("SITE", "MOMID", "PREGID", "TYPE_VISIT","PREG_START_DATE"))
data.temporal<- subject.list %>% 
  left_join(data.temporal_pre, by = c("SITE", "MOMID", "PREGID", "TYPE_VISIT")) %>%
  left_join(df_ultrasound, by = c("SITE", "MOMID", "PREGID", "INFANTID", "TYPE_VISIT"))

data.temporal.mask <- data.temporal %>%
  mutate(across(-c(SITE, MOMID, INFANTID, PREGID, TYPE_VISIT), ~ ifelse(is.na(.), 1, 0)))

data.temporal.mean <- data.temporal %>%
  summarise(across(-c(SITE, MOMID, INFANTID ,PREGID, TYPE_VISIT, PREG_START_DATE, M04_ANC_OBSSTDAT, M08_LBSTDAT, M01_US_OHOSTDAT), ~ round(mean(.x, na.rm = TRUE), 2))) %>%
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
    M04_ANTHELMINTHIC_CMOCCUR,
    # AFI index
    AFI,
    # Placental anomalies
    PREVIA,
    # Moderate or severe anemia
    M08_ANEMIA), ~ 0))

data.temporal.GA <- data.temporal %>% group_by(TYPE_VISIT) %>%
  summarise(across(c(M04_GA_days, M08_GA_days, M01_GA_days), ~ round(mean(.x, na.rm = TRUE), 2)))

# Apply LOCF to each subject while excluding specific columns
data.temporal.impute.pre <- data.temporal %>%
  group_by(SITE, INFANTID, MOMID, PREGID) %>%
  mutate(across(-c(TYPE_VISIT, PREG_START_DATE, M04_ANC_OBSSTDAT, M08_LBSTDAT, M01_US_OHOSTDAT, M04_GA_days, M08_GA_days, M01_GA_days), 
                ~ na.locf(.x, na.rm = FALSE))) %>% ungroup()

# Join the calculated means by TYPE_VISIT to the main dataset
data.temporal.impute.pre <- data.temporal.impute.pre %>%
  left_join(data.temporal.GA, by = "TYPE_VISIT", suffix = c("", "_mean")) %>%
  mutate(
    # Impute missing values in M01_GA_days, M04_GA_days and M08_GA_days with the calculated means by TYPE_VISIT
    M01_GA_days = coalesce(M01_GA_days, M01_GA_days_mean),
    M04_GA_days = coalesce(M04_GA_days, M04_GA_days_mean),
    M08_GA_days = coalesce(M08_GA_days, M08_GA_days_mean)
  ) %>%
  select(-M04_GA_days_mean, -M08_GA_days_mean, -M01_GA_days_mean)  # Remove the mean columns after imputation

data.temporal.impute <- data.temporal.impute.pre %>%
  mutate(across(everything(), ~ coalesce(.x, data.temporal.mean[[cur_column()]])))

data.temporal.delta <- data.temporal.impute %>% 
  group_by(SITE, INFANTID , MOMID, PREGID) %>%
  mutate(
    M01_delta_time = case_when(
      TYPE_VISIT == 1 ~ -1,
      TYPE_VISIT %in% c(2,3,4,5) ~ M01_GA_days - lag(M01_GA_days),
    ),
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
      c(M01_AFI_PERES, M01_PREVIA_PERES, AFI, PREVIA),
      ~ M01_delta_time
    ),
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
        M08_AGP_LBORRES, M08_CBC_HB_LBORRES, M08_ANEMIA,
        M08_UA_PROT_ind, M08_UA_LEUK_ind, M08_UA_NITRITE_LBORRES),
      ~ M08_delta_time
    )
  )

data.static.impute <- data.static %>%
  mutate(
    across(
      -c(SITE, INFANTID, MOMID, PREGID, INF_ASPH, STILLBIRTH_SIGNS_LIFE, SVN, PRETERMBIRTH_LT37, 
         LBW2500_ANY, NEO_DTH, INF_PSBI_OUTCOME, INF_JAUN_NON_SEV_ANY, NEARMISS,
         MAT_AGE, MUAC_ENROLL, GESTAGEBIRTH_ANY, BWEIGHT_ANY, CROWDING_IND, BMI_ENROLL), 
      ~ ifelse(is.na(.x), 0, .x)
    ),
    across(
      c(MAT_AGE, MUAC_ENROLL, GESTAGEBIRTH_ANY, BWEIGHT_ANY, CROWDING_IND, BMI_ENROLL), 
      ~ ifelse(is.na(.x), mean(.x, na.rm = TRUE), .x)
    )
  )

data.static.merge <- data.static %>% 
  mutate(
    across(
      -c(SITE, INFANTID, MOMID, PREGID, INF_ASPH, STILLBIRTH_SIGNS_LIFE, SVN, PRETERMBIRTH_LT37, 
         LBW2500_ANY, NEO_DTH, INF_PSBI_OUTCOME, INF_JAUN_NON_SEV_ANY, NEARMISS,
         MAT_AGE, MUAC_ENROLL, GESTAGEBIRTH_ANY, BWEIGHT_ANY, CROWDING_IND, BMI_ENROLL), 
      ~ ifelse(is.na(.x), 0, .x)
    ),
    across(
      c(MAT_AGE, MUAC_ENROLL, GESTAGEBIRTH_ANY, BWEIGHT_ANY, CROWDING_IND, BMI_ENROLL), 
      ~ ifelse(is.na(.x), mean(.x, na.rm = TRUE), .x)
    )
  ) %>%
  slice(rep(1:n(), each = 5)) %>% select(-c(SITE ,INFANTID, MOMID, PREGID))

data.dl.continuous<-cbind(data.temporal.impute,data.static.merge)
data.dl.continuous.delta<-cbind(data.temporal.delta,data.static.merge)
data.dl.continuous.mask<-cbind(data.temporal.mask,data.static.merge)

write.csv(data.dl.continuous,"D:/Users/yipeng_wei/Documents/dl data/2025-10-31/df_dl_continuous.csv")
write.csv(data.static,"D:/Users/yipeng_wei/Documents/dl data/2025-10-31/df_dl_static.csv")

# Pivot the data to wide format
data.temporal.impute.wide  <- data.temporal.impute  %>% 
  select(-c(PREG_START_DATE, M04_ANC_OBSSTDAT, M08_LBSTDAT, M01_US_OHOSTDAT)) %>% 
  pivot_wider(
    names_from = TYPE_VISIT,  # Use TYPE_VISIT as the basis for new columns
    values_from = -c(SITE,INFANTID,MOMID,PREGID,TYPE_VISIT)  # Columns to widen
  )

data.static.merge.wide<-data.static.impute %>% 
  select(-c(SITE ,INFANTID, MOMID, PREGID))

data.dl.continuous.wide<-cbind(data.temporal.impute.wide,data.static.merge.wide)

data.temporal.wide  <- data.temporal%>% 
  select(-c(PREG_START_DATE, M04_ANC_OBSSTDAT, M08_LBSTDAT, M01_US_OHOSTDAT)) %>% 
  pivot_wider(
    names_from = TYPE_VISIT,  # Use TYPE_VISIT as the basis for new columns
    values_from = -c(SITE,INFANTID,MOMID,PREGID,TYPE_VISIT)  # Columns to widen
  )

write.csv(data.temporal.wide,"D:/Users/yipeng_wei/Documents/dl data/2025-10-31/df_dl_temporal_wide.csv")
write.csv(data.dl.continuous.wide,"D:/Users/yipeng_wei/Documents/dl data/2025-10-31/df_dl_continuous_wide.csv")

write.csv(data.dl.continuous.delta,"D:/Users/yipeng_wei/Documents/dl data/2025-10-31/df_dl_continuous_delta.csv")
write.csv(data.dl.continuous.mask,"D:/Users/yipeng_wei/Documents/dl data/2025-10-31/df_dl_continuous_mask.csv")

