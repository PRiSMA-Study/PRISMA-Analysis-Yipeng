library(TSB.NICE)
library(gt)
library(dplyr)
library(stringr)
library(tibble)
library(sigmoid)
library(lme4)
library(irr)
library(survival)
library(tidyr)

load("Z:/Alyssa_working_files/validIMCI_postnatalage_TCB_GA_norepeats_31Oct2025.Rdata")

tcb_df<-validIMCI_postnatalage_TCB_GA_norepeats
remove(validIMCI_postnatalage_TCB_GA_norepeats)

# Make sure POSTNATALAGE is numeric hours
tcb_df <- tcb_df %>%
  mutate(
    POSTNATALAGE = as.numeric(POSTNATALAGE),
    PNC_age_days  = POSTNATALAGE %/% 24,
    PNC_age_hours = POSTNATALAGE %% 24
  )

TCB_threshold_fun<-function(GA,Days,Hours){
  if (is.na(GA)|is.na(Days)|is.na(Hours)){
    result<-NA
  }
  else if (GA>=38) {
    result<-TSB_NICE("P0",">= 38 weeks",Days,Hours)
  }
  else if (GA==37) {
    result<-TSB_NICE("P0","37 weeks",Days,Hours)
  }
  else if (GA==36) {
    result<-TSB_NICE("P0","36 weeks",Days,Hours)
  }
  else if (GA==35) {
    result<-TSB_NICE("P0","35 weeks",Days,Hours)
  }
  else if (GA==34) {
    result<-TSB_NICE("P0","34 weeks",Days,Hours)
  }
  else if (GA==33) {
    result<-TSB_NICE("P0","33 weeks",Days,Hours)
  }
  else if (GA==32) {
    result<-TSB_NICE("P0","32 weeks",Days,Hours)
  }
  else if (GA==31) {
    result<-TSB_NICE("P0","31 weeks",Days,Hours)
  }
  else if (GA==30) {
    result<-TSB_NICE("P0","30 weeks",Days,Hours)
  }
  else if (GA==29) {
    result<-TSB_NICE("P0","29 weeks",Days,Hours)
  }
  else if (GA==28) {
    result<-TSB_NICE("P0","28 weeks",Days,Hours)
  }
  else if (GA==27) {
    result<-TSB_NICE("P0","27 weeks",Days,Hours)
  }
  else if (GA==26) {
    result<-TSB_NICE("P0","26 weeks",Days,Hours)
  }
  else if (GA==25) {
    result<-TSB_NICE("P0","25 weeks",Days,Hours)
  }
  else if (GA==24) {
    result<-TSB_NICE("P0","24 weeks",Days,Hours)
  }
  else if (GA==23) {
    result<-TSB_NICE("P0","23 weeks",Days,Hours)
  }
  else {
    result<-NA
  }
  return(result)
}

# NICE threshold (vectorized with mapply)
tcb_df <- tcb_df %>%
  mutate(
    TCB_threshold_NICE = mapply(
      TCB_threshold_fun,
      GA    = GESTAGEBIRTH_ANY,
      Days  = PNC_age_days,
      Hours = PNC_age_hours
    )
  )

# Referral indicators
tcb_df <- tcb_df %>%
  mutate(
    referred_NICE  = ifelse(!is.na(TCB) & !is.na(TCB_threshold_NICE),
                            as.integer(TCB >= (TCB_threshold_NICE - 3)),
                            NA_integer_),
    referred_TCB15 = ifelse(!is.na(TCB),
                            as.integer(TCB >= 15),
                            NA_integer_),
    jaundice = case_when(
      JAUNDATVISIT == 2 ~ 1,
      JAUNDATVISIT %in% c(1,0) ~ 0,
      TRUE~ NA_integer_
    )
  )


tcb_df_threshold1 <- tcb_df %>%
  filter(referred_NICE==1) %>%
  mutate(Response=ifelse(jaundice==1,1,0))

tcb_df_threshold0 <- tcb_df %>%
  filter(referred_NICE==0) %>%
  mutate(Response=ifelse(jaundice==0,1,0))

tcb_df_15_1 <- tcb_df %>%
  filter(referred_TCB15==1) %>%
  mutate(Response=ifelse(jaundice==1,1,0))

tcb_df_15_0 <- tcb_df %>%
  filter(referred_TCB15==0) %>%
  mutate(Response=ifelse(jaundice==0,1,0))

###PPA/NPA
print.PPA_NPA <- function(data1, data0) {
  sigmoid <- function(x) 1 / (1 + exp(-x))
  
  get_point_and_ci <- function(model) {
    est <- fixef(model)[1]
    se <- sqrt(vcov(model)[1, 1])
    
    point <- sigmoid(est)
    se_delta <- point * (1 - point) * se
    
    ci_low <- max(point - 1.96 * se_delta, 1e-6)
    ci_up  <- min(point + 1.96 * se_delta, 1)
    
    return(paste0(
      round(point * 100, 1), "% (",
      round(ci_low * 100, 1), "%, ",
      round(ci_up * 100, 1), "%)"
    ))
  }
  
  model1 <- glmer(Response ~ (1 | INFANTID) + (1 | SITE) + (1 | BIN_POSTNATALAGE),
                  data = data1, family = binomial)
  model0 <- glmer(Response ~ (1 | INFANTID) + (1 | SITE) + (1 | BIN_POSTNATALAGE),
                  data = data0, family = binomial)
  
  PPA <- get_point_and_ci(model1)
  NPA <- get_point_and_ci(model0)
  
  return(c(PPA,NPA))
}

print.PPA_NPA(tcb_df_threshold1,tcb_df_threshold0)
print.PPA_NPA(tcb_df_15_1,tcb_df_15_0)

###Cohen's kappa
glm_model_TCB <- lmer(TCB ~ (1 | INFANTID) + (1 | SITE) + (1 | BIN_POSTNATALAGE),
                      data = tcb_df)
glm_model_JAUNDICE <- glmer(jaundice ~ (1 | INFANTID) + (1 | SITE) + (1 | BIN_POSTNATALAGE),
                            data = tcb_df, family = binomial)

tcb_df$predicted_tcb <- predict(glm_model_TCB, newdata = tcb_df)
tcb_df$predicted_prob_jaundice <- predict(glm_model_JAUNDICE, type = "response")

jaundice_threshold<-sum(tcb_df$jaundice==1)/nrow(tcb_df)

tcb_df$predicted_tcb_threshold_ind <- ifelse(tcb_df$predicted_tcb > tcb_df$TCB_threshold_NICE,1,0) 
tcb_df$predicted_tcb15 <- ifelse(tcb_df$predicted_tcb > 15,1,0) 
tcb_df$predicted_jaundice <- ifelse(tcb_df$predicted_prob_jaundice > jaundice_threshold,1,0) 

kappa_result_threshold <- kappa2(tcb_df[, c( "predicted_tcb_threshold_ind", "predicted_jaundice")])
kappa_result_15 <- kappa2(tcb_df[, c( "predicted_tcb15", "predicted_jaundice")])

kappa_result_threshold
kappa_result_15

###conditional LR
tcb_df_threshold_method<-tcb_df%>%
  select("MOMID","PREGID","SITE","INFANTID","referred_NICE","jaundice","BIN_POSTNATALAGE") %>% 
    pivot_longer(
    cols = c(jaundice, referred_NICE),
    names_to = "method",
    values_to = "value"
  ) %>% mutate(
    strata_id = paste(SITE, INFANTID, BIN_POSTNATALAGE, sep = "_")
  )

#set jaundice method as the reference group
tcb_df_threshold_method$method <- relevel(factor(tcb_df_threshold_method$method), ref = "jaundice")

clogit(formula = value ~ method + strata(strata_id),data = tcb_df_threshold_method)

tcb_df_15_method<-tcb_df%>%
  select("MOMID","PREGID","SITE","INFANTID","referred_TCB15","jaundice","BIN_POSTNATALAGE") %>% 
  pivot_longer(
    cols = c(jaundice, referred_TCB15),
    names_to = "method",
    values_to = "value"
  ) %>% mutate(
    strata_id = paste(SITE, INFANTID, BIN_POSTNATALAGE, sep = "_")
  )

#set jaundice method as the reference group
tcb_df_15_method$method <- relevel(factor(tcb_df_15_method$method), ref = "jaundice")

clogit(formula = value ~ method + strata(strata_id),data = tcb_df_15_method)

