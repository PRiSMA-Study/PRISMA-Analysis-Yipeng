library(TSB.NICE)
library(dplyr)

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
                            NA_integer_)
  )

mcnemar_table_by_category <- function(data, category_value,
                                      category_var = "JAUNDATVISIT",
                                      tcb15_var = "referred_TCB15",
                                      nice_var  = "referred_NICE") {
  
  d <- data %>%
    filter(.data[[category_var]] == category_value) %>%
    filter(!is.na(.data[[tcb15_var]]) & !is.na(.data[[nice_var]]))
  
  tcb15 <- factor(d[[tcb15_var]], levels = c(1, 0))
  nice  <- factor(d[[nice_var]],  levels = c(1, 0))
  
  tab <- as.matrix(table(tcb15, nice))
  # ensure 2x2 even if one level absent
  if (!all(dim(tab) == c(2,2))) {
    full <- matrix(0, nrow = 2, ncol = 2,
                   dimnames = list(c("1","0"), c("1","0")))
    full[rownames(tab), colnames(tab)] <- tab
    tab <- full
  }
  
  pval <- tryCatch({
    p <- mcnemar.test(tab)$p.value
    if (p < 0.001) "<0.001" else sprintf("%.3f", p)
  }, error = function(e) "NA")
  
  total <- sum(tab)
  fmt <- function(x) paste0(x, " (", sprintf("%.2f", 100 * x / total), "%)")
  
  out <- matrix("", nrow = 2, ncol = 3)
  out[1,1] <- fmt(tab["1","1"])
  out[1,2] <- fmt(tab["1","0"])
  out[2,1] <- fmt(tab["0","1"])
  out[2,2] <- fmt(tab["0","0"])
  out[1,3] <- pval
  out[2,3] <- ""
  
  rownames(out) <- c("Referred by TCB15", "Not referred by TCB15")
  colnames(out) <- c("Referred by NICE", "Not referred by NICE", "McNemar's test (p-value)")
  out
}

# --- add these label maps ---
bin_labels <- c(
  "1" = "0-2 days",
  "2" = "2-6 days",
  "3" = "6-14 days"
)

jaundice_labels <- c(
  "0" = "No Jaundice",
  "1" = "Jaundice",
  "2" = "Severe Jaundice"
)

# make sure BIN_POSTNATALAGE and JAUNDATVISIT are numeric-coded for filtering
tcb_df <- tcb_df %>%
  mutate(
    BIN_POSTNATALAGE = as.integer(as.character(BIN_POSTNATALAGE)),
    JAUNDATVISIT     = as.integer(as.character(JAUNDATVISIT))
  )

jaundice_levels <- c(2, 1, 0)

tables_by_bin <- list()

for (b in c(1, 2, 3)) {
  df_b <- tcb_df %>% filter(BIN_POSTNATALAGE == b)
  
  for (ct in jaundice_levels) {
    key <- paste0(
      bin_labels[as.character(b)],
      " | ",
      jaundice_labels[as.character(ct)]
    )
    
    tables_by_bin[[key]] <- mcnemar_table_by_category(
      data = df_b,
      category_value = ct,
      category_var = "JAUNDATVISIT",
      tcb15_var = "referred_TCB15",
      nice_var  = "referred_NICE"
    )
  }
}

tables_by_bin
