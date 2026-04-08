##A.9.3.1 Data cleaning 
# ============================================================
# WORKERS COMP DATA CLEANING
# ============================================================

install.packages("tidyverse")
install.packages("readxl")
install.packages("janitor")

library(tidyverse)
library(readxl)
library(janitor)

# ============================================================
# 1. LOAD DATA
# ============================================================

path <- "/Users/dhrutipatel/Desktop/"

freq <- read_excel(paste0(path, "srcsc-2026-claims-workers-comp.xlsx"), sheet = "freq")
sev  <- read_excel(paste0(path, "srcsc-2026-claims-workers-comp.xlsx"), sheet = "sev")

freq <- freq %>% clean_names()
sev  <- sev  %>% clean_names()

cat("Raw freq rows:", nrow(freq), "\n")
cat("Raw sev rows:",  nrow(sev),  "\n")

# ============================================================
# 2. VALID VALUES FROM DATA DICTIONARY
# ============================================================

valid_solar_systems   <- c("Helionis Cluster", "Epsilon", "Zeta")
valid_occupations     <- c("Administrator", "Manager", "Executive", "Scientist",
                           "Engineer", "Safety Officer", "Drill Operator",
                           "Maintenance Staff", "Spacecraft Operator",
                           "Technology Officer", "Planetary Operations")
valid_employment_type <- c("Full time", "Full-time", "Contract")
valid_scores          <- 1:5
valid_hours           <- c(20, 25, 30, 35, 40)
valid_injury_types    <- c("Psychological", "Amputation", "Cut laceration",
                           "Sprain, strain", "Burns", "Stress", "Other")
valid_injury_causes   <- c("Stress/strain", "Vehicle accident", "Exposure",
                           "Overexertion", "Caught in machine", "Violence", "Other")

# ============================================================
# 3. RECOVER CORRUPTED SOLAR SYSTEM 
# ============================================================

recover_solar <- function(x) {
  case_when(
    x %in% valid_solar_systems        ~ x,
    str_detect(x, "Helionis Cluster") ~ "Helionis Cluster",
    str_detect(x, "Epsilon")          ~ "Epsilon",
    str_detect(x, "Zeta")             ~ "Zeta",
    TRUE                              ~ NA_character_
  )
}

freq <- freq %>%
  mutate(
    solar_system_original  = solar_system,
    solar_system_corrupted = !solar_system %in% valid_solar_systems & !is.na(solar_system),
    solar_system           = recover_solar(solar_system)
  )

sev <- sev %>%
  mutate(
    solar_system_original  = solar_system,
    solar_system_corrupted = !solar_system %in% valid_solar_systems & !is.na(solar_system),
    solar_system           = recover_solar(solar_system)
  )

n_freq_solar_corrupted <- sum(freq$solar_system_corrupted, na.rm = TRUE)
n_sev_solar_corrupted  <- sum(sev$solar_system_corrupted,  na.rm = TRUE)

# ============================================================
# 4. RECOVER CORRUPTED OCCUPATION 
# ============================================================

recover_occupation <- function(x) {
  case_when(
    x %in% valid_occupations              ~ x,
    str_detect(x, "Administrator")        ~ "Administrator",
    str_detect(x, "Manager")              ~ "Manager",
    str_detect(x, "Executive")            ~ "Executive",
    str_detect(x, "Scientist")            ~ "Scientist",
    str_detect(x, "Engineer")             ~ "Engineer",
    str_detect(x, "Safety Officer")       ~ "Safety Officer",
    str_detect(x, "Drill Operator")       ~ "Drill Operator",
    str_detect(x, "Maintenance Staff")    ~ "Maintenance Staff",
    str_detect(x, "Spacecraft Operator")  ~ "Spacecraft Operator",
    str_detect(x, "Technology Officer")   ~ "Technology Officer",
    str_detect(x, "Planetary Operations") ~ "Planetary Operations",
    TRUE                                  ~ NA_character_
  )
}

freq <- freq %>%
  mutate(
    occupation_original  = occupation,
    occupation_corrupted = !occupation %in% valid_occupations & !is.na(occupation),
    occupation           = recover_occupation(occupation)
  )

sev <- sev %>%
  mutate(
    occupation_original  = occupation,
    occupation_corrupted = !occupation %in% valid_occupations & !is.na(occupation),
    occupation           = recover_occupation(occupation)
  )

n_freq_occ_corrupted <- sum(freq$occupation_corrupted, na.rm = TRUE)
n_sev_occ_corrupted  <- sum(sev$occupation_corrupted,  na.rm = TRUE)

# ============================================================
# 5. REMOVE NULL SOLAR SYSTEM 
# ============================================================

freq_null_solar   <- freq %>% filter(is.na(solar_system))
sev_null_solar    <- sev  %>% filter(is.na(solar_system))
n_freq_null_solar <- nrow(freq_null_solar)
n_sev_null_solar  <- nrow(sev_null_solar)

freq <- freq %>% filter(!is.na(solar_system))
sev  <- sev  %>% filter(!is.na(solar_system))

# ============================================================
# 6. REMOVE NULL CLAIM COUNT 
# ============================================================

freq_null_claims   <- freq %>% filter(is.na(claim_count))
n_freq_null_claims <- nrow(freq_null_claims)

freq <- freq %>% filter(!is.na(claim_count))

# ============================================================
# 7. PRINT THEN REMOVE NEGATIVE CLAIM AMOUNT
# ============================================================

cat("\n--- Negative Claim Record (printed before removal) ---\n")
sev %>% filter(claim_amount < 0) %>% print()

n_sev_negative <- nrow(sev %>% filter(claim_amount < 0))

sev <- sev %>% filter(claim_amount > 0)

# ============================================================
# 8. REMOVE CLAIM AMOUNT EXTREME OUTLIERS
# ============================================================

cat("\n--- Claim amounts above Đ170,000 (printed before removal) ---\n")
sev %>% filter(claim_amount > 170000) %>%
  dplyr::select(claim_id, solar_system, occupation,
                injury_type, injury_cause,
                claim_length, claim_amount) %>%
  print()

n_sev_above_170k <- nrow(sev %>% filter(claim_amount > 170000))
sev <- sev %>% filter(claim_amount <= 170000)

# ============================================================
# 9. OUT-OF-RANGE NUMERICS 
# All ranges taken directly from Data Dictionary
# ============================================================

# --- FREQ TAB ---
freq <- freq %>%
  mutate(
    
    # exposure: 0 - 1
    exposure = ifelse(
      exposure < 0 | exposure > 1,
      NA, exposure),
    
    # experience_yrs: ~0.2 - 40
    experience_yrs = ifelse(
      experience_yrs < 0.2 | experience_yrs > 40,
      NA, experience_yrs),
    
    # psych_stress_index: {1,2,3,4,5}
    psych_stress_index = ifelse(
      !psych_stress_index %in% valid_scores,
      NA, psych_stress_index),
    
    # hours_per_week: {20,25,30,35,40}
    hours_per_week = ifelse(
      !hours_per_week %in% valid_hours,
      NA, hours_per_week),
    
    # supervision_level: 0 - 1
    supervision_level = ifelse(
      supervision_level < 0 | supervision_level > 1,
      NA, supervision_level),
    
    # gravity_level: 0.75 - 1.50
    gravity_level = ifelse(
      gravity_level < 0.75 | gravity_level > 1.50,
      NA, gravity_level),
    
    # safety_training_index: {1,2,3,4,5}
    safety_training_index = ifelse(
      !safety_training_index %in% valid_scores,
      NA, safety_training_index),
    
    # protective_gear_quality: {1,2,3,4,5}
    protective_gear_quality = ifelse(
      !protective_gear_quality %in% valid_scores,
      NA, protective_gear_quality),
    
    # base_salary: ~20K - 130K
    base_salary = ifelse(
      base_salary < 20000 | base_salary > 130000,
      NA, base_salary),
    
    # claim_count: 0 - 2
    claim_count = ifelse(
      claim_count < 0 | claim_count > 2,
      NA, claim_count),
    
    # employment_type: valid levels only
    employment_type = ifelse(
      !employment_type %in% valid_employment_type,
      NA, employment_type),
    
    # accident_history_flag: {0,1}
    accident_history_flag = ifelse(
      !accident_history_flag %in% c(0, 1),
      NA, accident_history_flag)
  )

# --- SEV TAB ---
sev <- sev %>%
  mutate(
    
    # exposure: 0 - 1
    exposure = ifelse(
      exposure < 0 | exposure > 1,
      NA, exposure),
    
    # experience_yrs: ~0.2 - 40
    experience_yrs = ifelse(
      experience_yrs < 0.2 | experience_yrs > 40,
      NA, experience_yrs),
    
    # gravity_level: 0.75 - 1.50
    gravity_level = ifelse(
      gravity_level < 0.75 | gravity_level > 1.50,
      NA, gravity_level),
    
    # base_salary: ~20K - 130K
    base_salary = ifelse(
      base_salary < 20000 | base_salary > 130000,
      NA, base_salary),
    
    # psych_stress_index: {1,2,3,4,5}
    psych_stress_index = ifelse(
      !psych_stress_index %in% valid_scores,
      NA, psych_stress_index),
    
    # safety_training_index: {1,2,3,4,5}
    safety_training_index = ifelse(
      !safety_training_index %in% valid_scores,
      NA, safety_training_index),
    
    # protective_gear_quality: {1,2,3,4,5}
    protective_gear_quality = ifelse(
      !protective_gear_quality %in% valid_scores,
      NA, protective_gear_quality),
    
    # hours_per_week: {20,25,30,35,40}
    hours_per_week = ifelse(
      !hours_per_week %in% valid_hours,
      NA, hours_per_week),
    
    # supervision_level: 0 - 1
    supervision_level = ifelse(
      supervision_level < 0 | supervision_level > 1,
      NA, supervision_level),
    
    # claim_length: 3 - 1000
    claim_length = ifelse(
      claim_length < 3 | claim_length > 1000,
      NA, claim_length),
    
    # injury_type: valid levels only
    injury_type = ifelse(
      !injury_type %in% valid_injury_types,
      NA, injury_type),
    
    # injury_cause: valid levels only
    injury_cause = ifelse(
      !injury_cause %in% valid_injury_causes,
      NA, injury_cause),
    
    # employment_type: valid levels only
    employment_type = ifelse(
      !employment_type %in% valid_employment_type,
      NA, employment_type),
    
    # accident_history_flag: {0,1}
    accident_history_flag = ifelse(
      !accident_history_flag %in% c(0, 1),
      NA, accident_history_flag),
    
    # claim_amount: Đ5 to Đ170,000
    # upper bound already enforced in step 8
    claim_amount = ifelse(claim_amount < 5, NA, claim_amount)
  )

# ============================================================
# 10. FINAL CLEAN DATASETS
# ============================================================

freq_clean <- freq
sev_clean  <- sev

# ============================================================
# 11. EXPLORE FREQUENCY
# ============================================================

cat("\n--- Overall Claim Rate ---\n")
freq_clean %>%
  summarise(
    total_workers  = n(),
    total_claims   = sum(claim_count, na.rm = TRUE),
    total_exposure = sum(exposure, na.rm = TRUE),
    claim_rate     = total_claims / total_exposure
  ) %>% print()

cat("\n--- Claim Rate by Solar System ---\n")
freq_clean %>%
  group_by(solar_system) %>%
  summarise(
    workers    = n(),
    claims     = sum(claim_count, na.rm = TRUE),
    exposure   = sum(exposure, na.rm = TRUE),
    claim_rate = claims / exposure
  ) %>%
  arrange(desc(claim_rate)) %>% print()

cat("\n--- Claim Rate by Occupation ---\n")
freq_clean %>%
  filter(!is.na(occupation)) %>%
  group_by(occupation) %>%
  summarise(
    claims     = sum(claim_count, na.rm = TRUE),
    exposure   = sum(exposure, na.rm = TRUE),
    claim_rate = claims / exposure
  ) %>%
  arrange(desc(claim_rate)) %>% print()

cat("\n--- Claim Rate by Employment Type ---\n")
freq_clean %>%
  filter(!is.na(employment_type)) %>%
  group_by(employment_type) %>%
  summarise(
    claims     = sum(claim_count, na.rm = TRUE),
    exposure   = sum(exposure, na.rm = TRUE),
    claim_rate = claims / exposure
  ) %>% print()

cat("\n--- Claim Rate by Accident History Flag ---\n")
freq_clean %>%
  group_by(accident_history_flag) %>%
  summarise(
    claims     = sum(claim_count, na.rm = TRUE),
    exposure   = sum(exposure, na.rm = TRUE),
    claim_rate = claims / exposure
  ) %>% print()

cat("\n--- Claim Rate by Psych Stress Index ---\n")
freq_clean %>%
  filter(!is.na(psych_stress_index)) %>%
  group_by(psych_stress_index) %>%
  summarise(
    claims     = sum(claim_count, na.rm = TRUE),
    exposure   = sum(exposure, na.rm = TRUE),
    claim_rate = claims / exposure
  ) %>% print()

cat("\n--- Claim Rate by Safety Training Index ---\n")
freq_clean %>%
  filter(!is.na(safety_training_index)) %>%
  group_by(safety_training_index) %>%
  summarise(
    claims     = sum(claim_count, na.rm = TRUE),
    exposure   = sum(exposure, na.rm = TRUE),
    claim_rate = claims / exposure
  ) %>% print()

cat("\n--- Claim Rate by Gravity Level (Binned) ---\n")
freq_clean %>%
  filter(!is.na(gravity_level)) %>%
  mutate(gravity_bin = cut(gravity_level,
                           breaks = c(0.75, 0.90, 1.05, 1.20, 1.50),
                           labels = c("Low (0.75-0.90)", "Medium (0.90-1.05)",
                                      "High (1.05-1.20)", "Very High (1.20-1.50)"))) %>%
  group_by(gravity_bin) %>%
  summarise(
    claims     = sum(claim_count, na.rm = TRUE),
    exposure   = sum(exposure, na.rm = TRUE),
    claim_rate = claims / exposure
  ) %>% print()

# ============================================================
# 12. EXPLORE SEVERITY
# ============================================================

cat("\n--- Overall Severity Stats ---\n")
sev_clean %>%
  summarise(
    n      = n(),
    mean   = mean(claim_amount, na.rm = TRUE),
    median = median(claim_amount, na.rm = TRUE),
    sd     = sd(claim_amount, na.rm = TRUE),
    min    = min(claim_amount, na.rm = TRUE),
    max    = max(claim_amount, na.rm = TRUE),
    p95    = quantile(claim_amount, 0.95, na.rm = TRUE),
    p99    = quantile(claim_amount, 0.99, na.rm = TRUE)
  ) %>% print()

cat("\n--- Severity by Solar System ---\n")
sev_clean %>%
  group_by(solar_system) %>%
  summarise(
    n      = n(),
    mean   = mean(claim_amount, na.rm = TRUE),
    median = median(claim_amount, na.rm = TRUE),
    sd     = sd(claim_amount, na.rm = TRUE),
    max    = max(claim_amount, na.rm = TRUE),
    p95    = quantile(claim_amount, 0.95, na.rm = TRUE),
    p99    = quantile(claim_amount, 0.99, na.rm = TRUE)
  ) %>% print()

cat("\n--- Severity by Injury Type ---\n")
sev_clean %>%
  filter(!is.na(injury_type)) %>%
  group_by(injury_type) %>%
  summarise(
    n      = n(),
    mean   = mean(claim_amount, na.rm = TRUE),
    median = median(claim_amount, na.rm = TRUE),
    max    = max(claim_amount, na.rm = TRUE)
  ) %>%
  arrange(desc(mean)) %>% print()

cat("\n--- Severity by Injury Cause ---\n")
sev_clean %>%
  filter(!is.na(injury_cause)) %>%
  group_by(injury_cause) %>%
  summarise(
    n    = n(),
    mean = mean(claim_amount, na.rm = TRUE),
    max  = max(claim_amount, na.rm = TRUE)
  ) %>%
  arrange(desc(mean)) %>% print()

# ============================================================
# 13. DATA LIMITATIONS SUMMARY
# ============================================================

freq_na_summary <- freq_clean %>%
  summarise(across(everything(), ~sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "column", values_to = "na_count") %>%
  filter(na_count > 0) %>%
  arrange(desc(na_count))

sev_na_summary <- sev_clean %>%
  summarise(across(everything(), ~sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "column", values_to = "na_count") %>%
  filter(na_count > 0) %>%
  arrange(desc(na_count))

cat("\n================================================\n")
cat("DATA LIMITATIONS SUMMARY\n")
cat("================================================\n")

cat("\nFREQ TAB:\n")
cat("  Raw rows loaded:                              ", nrow(freq) + n_freq_null_solar + n_freq_null_claims, "\n")
cat("  Corrupted solar system (recovered):           ", n_freq_solar_corrupted, "\n")
cat("  Corrupted occupation (recovered):             ", n_freq_occ_corrupted,   "\n")
cat("  Null solar system rows removed:               ", n_freq_null_solar,      "\n")
cat("  Null claim count rows removed:                ", n_freq_null_claims,     "\n")
cat("  Final clean rows:                             ", nrow(freq_clean),       "\n")

cat("\nSEV TAB:\n")
cat("  Raw rows loaded:                              ", nrow(sev) + n_sev_null_solar + n_sev_negative, "\n")
cat("  Corrupted solar system (recovered):           ", n_sev_solar_corrupted,  "\n")
cat("  Corrupted occupation (recovered):             ", n_sev_occ_corrupted,    "\n")
cat("  Null solar system rows removed:               ", n_sev_null_solar,       "\n")
cat("  Negative claim amount removed:                ", n_sev_negative,         "\n")
n_sev_below_5 <- sum(is.na(sev_clean$claim_amount))
cat("  Claim amounts above Đ170,000 removed:         ", n_sev_above_170k, "\n")
cat("  Claim amounts below Đ5 set to NA:             ", n_sev_below_5,    "\n")
cat("  Final clean rows:                             ", nrow(sev_clean),        "\n")

cat("\nOUT-OF-RANGE VALUES SET TO NA - columns affected:\n")
cat("\n  FREQ:\n")
print(freq_na_summary)
cat("\n  SEV:\n")
print(sev_na_summary)

##A.9.3.2 Modelling

library(tidyverse)
library(MASS)

# ============================================================
# 1. LOAD CLEAN DATA
# ============================================================

freq_clean <- readRDS("/Users/dhrutipatel/Desktop/freq_clean.rds")

# ============================================================
# 2. PREPARE MODELLING DATASET 
# ============================================================

freq_model <- freq_clean %>%
  mutate(
    solar_system            = factor(solar_system),
    occupation              = factor(occupation),
    employment_type         = factor(employment_type),
    accident_history_flag   = factor(accident_history_flag),
    psych_stress_index      = factor(psych_stress_index),
    safety_training_index   = factor(safety_training_index),
    protective_gear_quality = factor(protective_gear_quality)
  )

cat("Rows available for modelling:", nrow(freq_model), "\n")

# ============================================================
# 3. CHECK ZERO INFLATION
# ============================================================

cat("\n--- Zero Inflation Check ---\n")
zero_count <- sum(freq_model$claim_count == 0, na.rm = TRUE)
total      <- sum(!is.na(freq_model$claim_count))
zero_pct   <- (zero_count / total) * 100

cat("Total workers:         ", total,                    "\n")
cat("Workers with 0 claims: ", zero_count,               "\n")
cat("Workers with 1+ claims:", total - zero_count,       "\n")
cat("Zero claim percentage: ", round(zero_pct, 1), "%\n")

# ============================================================
# 4. CHECK OVERDISPERSION
# ============================================================

cat("\n--- Overdispersion Check ---\n")
claim_mean <- mean(freq_model$claim_count, na.rm = TRUE)
claim_var  <- var(freq_model$claim_count,  na.rm = TRUE)
ratio      <- claim_var / claim_mean

cat("Mean of claim_count:     ", round(claim_mean, 6), "\n")
cat("Variance of claim_count: ", round(claim_var,  6), "\n")
cat("Variance/Mean ratio:     ", round(ratio,       4), "\n")

# ============================================================
# 5. DISTRIBUTION OF CLAIM COUNTS
# ============================================================

cat("\n--- Claim Count Distribution ---\n")
freq_model %>%
  filter(!is.na(claim_count)) %>%
  count(claim_count) %>%
  mutate(percentage = round(n / sum(n) * 100, 2)) %>%
  as.data.frame() %>%
  print() 

# ============================================================
# 6. FIT POISSON GLM - FULL MODEL
# ============================================================

cat("\n--- Fitting Poisson GLM ---\n")

model_poisson <- glm(
  claim_count ~
    solar_system +
    occupation +
    employment_type +
    accident_history_flag +
    psych_stress_index +
    safety_training_index +
    protective_gear_quality +
    gravity_level +
    supervision_level +
    experience_yrs +
    hours_per_week +
    offset(log(exposure)),
  family = poisson(link = "log"),
  data   = freq_model
)

cat("Poisson AIC:", round(AIC(model_poisson), 2), "\n")
summary(model_poisson)

# ============================================================
# 7. FIT REDUCED POISSON MODEL
# Remove variables with no significant levels at all:
# Keep solar_system for Cosmic Quarry pricing
# ============================================================

model_reduced <- glm(
  claim_count ~
    solar_system +
    occupation +
    accident_history_flag +
    psych_stress_index +
    safety_training_index +
    offset(log(exposure)),
  family = poisson(link = "log"),
  data   = freq_model
)

cat("Full model AIC:    ", round(AIC(model_poisson), 2), "\n")
cat("Reduced model AIC: ", round(AIC(model_reduced), 2), "\n")
summary(model_reduced)

library(MASS)
library(car)

# ============================================================
# 1. CHECK CLAIM AMOUNT DISTRIBUTION
# ============================================================

cat("\n--- Claim Amount Summary ---\n")
sev_clean %>%
  summarise(
    n      = n(),
    mean   = round(mean(claim_amount,           na.rm = TRUE), 2),
    median = round(median(claim_amount,         na.rm = TRUE), 2),
    sd     = round(sd(claim_amount,             na.rm = TRUE), 2),
    min    = round(min(claim_amount,            na.rm = TRUE), 2),
    max    = round(max(claim_amount,            na.rm = TRUE), 2),
    p75    = round(quantile(claim_amount, 0.75, na.rm = TRUE), 2),
    p90    = round(quantile(claim_amount, 0.90, na.rm = TRUE), 2),
    p95    = round(quantile(claim_amount, 0.95, na.rm = TRUE), 2),
    p99    = round(quantile(claim_amount, 0.99, na.rm = TRUE), 2)
  ) %>%
  print()

# ============================================================
# 2. SKEWNESS CHECK
# ============================================================

cat("\n--- Skewness Check ---\n")
cat("Mean:  ", round(mean(sev_clean$claim_amount,   na.rm = TRUE), 2), "\n")
cat("Median:", round(median(sev_clean$claim_amount, na.rm = TRUE), 2), "\n")
cat("If mean >> median = right skewed = use Gamma or Lognormal\n")


# ============================================================
# STEP 3: SEVERITY - DISTRIBUTION FITTING
# ============================================================

library(MASS)

# ============================================================
# A. GET CLEAN CLAIMS AND SCALE
# ============================================================

claims <- sev_clean %>%
  filter(!is.na(claim_amount), claim_amount > 0) %>%
  pull(claim_amount)

claims_scaled <- claims / 1000

cat("Number of claims:", length(claims), "\n")

# ============================================================
# B. FIT LOGNORMAL
# ============================================================

fit_lnorm <- fitdistr(claims_scaled, "lognormal")
aic_lnorm <- -2 * fit_lnorm$loglik + 2 * 2

cat("\n--- Lognormal ---\n")
cat("meanlog:", round(fit_lnorm$estimate["meanlog"], 4), "\n")
cat("sdlog:  ", round(fit_lnorm$estimate["sdlog"],   4), "\n")
cat("AIC:    ", round(aic_lnorm, 2), "\n")

# ============================================================
# C. FIT GAMMA
# ============================================================

fit_gamma <- fitdistr(claims_scaled, "gamma")
aic_gamma <- -2 * fit_gamma$loglik + 2 * 2

cat("\n--- Gamma ---\n")
cat("shape:", round(fit_gamma$estimate["shape"], 4), "\n")
cat("rate: ", round(fit_gamma$estimate["rate"],  4), "\n")
cat("AIC:  ", round(aic_gamma, 2), "\n")

# ============================================================
# D. AIC COMPARISON
# ============================================================

cat("\n--- AIC Comparison ---\n")
cat("Lognormal AIC:", round(aic_lnorm, 2), "\n")
cat("Gamma AIC:    ", round(aic_gamma,  2), "\n")
cat("Lower AIC = better fit\n")

if (aic_lnorm < aic_gamma) {
  cat("\nSelected: LOGNORMAL\n")
} else {
  cat("\nSelected: GAMMA\n")
}

# ============================================================
# E. QQ PLOT- LOGNORMAL
# ============================================================

dev.new(width = 8, height = 6)
par(mar = c(4, 4, 2, 1))

qqnorm(log(claims),
       main = "QQ Plot Lognormal")
qqline(log(claims), col = "red", lwd = 2)

# ============================================================
# F. QQ PLOT- GAMMA
# ============================================================

dev.new(width = 8, height = 6)
par(mar = c(4, 4, 2, 1))

n       <- length(claims_scaled)
probs   <- ppoints(n)
q_gamma <- qgamma(probs,
                  shape = fit_gamma$estimate["shape"],
                  rate  = fit_gamma$estimate["rate"])

plot(q_gamma, sort(claims_scaled),
     main = "QQ Plot Gamma",
     xlab = "Theoretical Gamma Quantiles",
     ylab = "Observed Claim Amounts (scaled)")
abline(0, 1, col = "red", lwd = 2)

# ============================================================
# STEP 4: SEVERITY GLM- LOGNORMAL
# ============================================================

sev_model <- sev_clean %>%
  filter(!is.na(claim_amount), claim_amount > 0) %>%
  mutate(
    log_claim_amount        = log(claim_amount),
    solar_system            = factor(solar_system),
    occupation              = factor(occupation),
    injury_type             = factor(injury_type),
    injury_cause            = factor(injury_cause),
    employment_type         = factor(employment_type),
    accident_history_flag   = factor(accident_history_flag),
    psych_stress_index      = factor(psych_stress_index),
    safety_training_index   = factor(safety_training_index),
    protective_gear_quality = factor(protective_gear_quality)
  )

cat("Rows for severity modelling:", nrow(sev_model), "\n")

model_sev_full <- glm(
  log_claim_amount ~
    solar_system +
    occupation +
    injury_type +
    injury_cause +
    employment_type +
    accident_history_flag +
    psych_stress_index +
    safety_training_index +
    protective_gear_quality +
    gravity_level +
    experience_yrs +
    claim_length,
  family = gaussian(link = "identity"),
  data   = sev_model
)

cat("Full model AIC:", round(AIC(model_sev_full), 2), "\n")
summary(model_sev_full)

# ============================================================
# STEP 4B: REDUCED SEVERITY GLM
# ============================================================

model_sev_reduced <- glm(
  log_claim_amount ~
    solar_system +
    occupation +
    injury_cause +
    employment_type +
    psych_stress_index +
    safety_training_index +
    protective_gear_quality +
    claim_length,
  family = gaussian(link = "identity"),
  data   = sev_model
)

cat("Full model AIC:    ", round(AIC(model_sev_full),    2), "\n")
cat("Reduced model AIC: ", round(AIC(model_sev_reduced), 2), "\n")
summary(model_sev_reduced)

# ============================================================
# STEP 5: AGGREGATE LOSS - WEIGHTED PROXY MONTE CARLO
# ============================================================
#
# DATA MAPPING:
#   Historical data systems: Helionis Cluster, Epsilon, Zeta
#   Cosmic Quarry targets:   Helionis Cluster, Bayesia, Oryn Delta
#
# PROXY WEIGHTS (Workers' Comp specific - physical worker exposure):
#   Bayesia    = 0.20 × Helionis + 0.80 × Zeta
#     Rationale: High gravity + elevated radiation + binary star
#     spikes + temperature extremes → Zeta-dominant (volcanic
#     activity, solar flares, hazardous surface conditions)
#
#   Oryn Delta = 0.55 × Helionis + 0.45 × Zeta
#     Rationale: Habitable zone base operations = Helionis-like
#     stability, BUT low visibility + sporadic unpredictable flares
#     + hazardous asteroid ring expansion = significant Zeta-like
#     physical WC hazard. Intentionally different from BI weights
#     (0.9/0.1) - physical worker exposure matters more here than
#     operational continuity.
#
# ============================================================

library(tidyverse)
library(MASS)

# ============================================================
# 0. PROXY WEIGHTS
# Centralised here - change once to sensitivity-test everything
# ============================================================

w_h_b <- 0.20   # Helionis weight for Bayesia
w_z_b <- 0.80   # Zeta weight for Bayesia
w_h_o <- 0.55   # Helionis weight for Oryn Delta
w_z_o <- 0.45   # Zeta weight for Oryn Delta

cat("================================================\n")
cat("PROXY WEIGHTS\n")
cat("================================================\n")
cat("Bayesia    =", w_h_b, "x Helionis +", w_z_b, "x Zeta\n")
cat("Oryn Delta =", w_h_o, "x Helionis +", w_z_o, "x Zeta\n")

# ============================================================
# 1. PREPARE BASE FREQUENCY PRICING DATASET
# ============================================================

freq_pricing_base <- freq_clean %>%
  filter(
    !is.na(claim_count),
    !is.na(solar_system),
    !is.na(occupation),
    !is.na(accident_history_flag),
    !is.na(psych_stress_index),
    !is.na(safety_training_index),
    !is.na(exposure),
    exposure > 0
  ) %>%
  mutate(
    solar_system          = factor(solar_system),
    occupation            = factor(occupation),
    employment_type       = factor(employment_type),
    accident_history_flag = factor(accident_history_flag),
    psych_stress_index    = factor(psych_stress_index),
    safety_training_index = factor(safety_training_index),
    exposure              = 1    # per-worker-per-year rate
  )

cat("\nTotal workers in pricing base:", nrow(freq_pricing_base), "\n")
cat("By system:\n")
freq_pricing_base %>% count(solar_system) %>% print()

# ============================================================
# 2. PREPARE SEVERITY REFERENCE DATASET
# ============================================================

sev_pricing <- sev_clean %>%
  filter(
    !is.na(claim_amount), claim_amount > 0,
    !is.na(solar_system),
    !is.na(occupation),
    !is.na(injury_cause),
    !is.na(employment_type),
    !is.na(psych_stress_index),
    !is.na(safety_training_index),
    !is.na(protective_gear_quality),
    !is.na(claim_length)
  ) %>%
  mutate(
    solar_system            = factor(solar_system),
    occupation              = factor(occupation),
    injury_cause            = factor(injury_cause),
    employment_type         = factor(employment_type),
    psych_stress_index      = factor(psych_stress_index),
    safety_training_index   = factor(safety_training_index),
    protective_gear_quality = factor(protective_gear_quality),
    log_claim_amount        = log(claim_amount)
  )

sigma_sev <- sqrt(summary(model_sev_reduced)$dispersion)
cat("\nSeverity sigma (sdlog):", round(sigma_sev, 4), "\n")

# ============================================================
# 3. DATA-DRIVEN REPRESENTATIVE SEVERITY VALUES
# ============================================================

mode_injury_cause <- sev_clean %>%
  filter(!is.na(injury_cause)) %>%
  count(injury_cause) %>%
  slice_max(n, n = 1) %>%
  pull(injury_cause)

mode_gear_quality <- sev_clean %>%
  filter(!is.na(protective_gear_quality)) %>%
  count(protective_gear_quality) %>%
  slice_max(n, n = 1) %>%
  pull(protective_gear_quality)

median_claim_length <- median(sev_clean$claim_length, na.rm = TRUE)

cat("\nRepresentative severity values (data-driven):\n")
cat("  Modal injury cause:  ", mode_injury_cause,   "\n")
cat("  Modal gear quality:  ", mode_gear_quality,   "\n")
cat("  Median claim length: ", median_claim_length, "days\n")

# ============================================================
# 4. HELPER FUNCTIONS
# ============================================================

# --- Predict lambda under a specified solar system ---
predict_lambda_as <- function(df, system) {
  df_temp <- df %>%
    mutate(solar_system = factor(system,
                                 levels = levels(df$solar_system)))
  predict(model_reduced, newdata = df_temp, type = "response")
}

# --- Predict meanlog under a specified solar system ---
predict_meanlog_as <- function(df, system) {
  df_temp <- df %>%
    mutate(
      solar_system = factor(system,
                            levels = levels(sev_pricing$solar_system)),
      injury_cause = factor(mode_injury_cause,
                            levels = levels(sev_pricing$injury_cause)),
      protective_gear_quality = factor(mode_gear_quality,
                                       levels = levels(sev_pricing$protective_gear_quality)),
      claim_length = median_claim_length,
      # align all remaining factor levels to severity model
      occupation = factor(occupation,
                          levels = levels(sev_pricing$occupation)),
      employment_type = factor(employment_type,
                               levels = levels(sev_pricing$employment_type)),
      psych_stress_index = factor(psych_stress_index,
                                  levels = levels(sev_pricing$psych_stress_index)),
      safety_training_index = factor(safety_training_index,
                                     levels = levels(sev_pricing$safety_training_index))
    )
  predict(model_sev_reduced, newdata = df_temp, type = "response")
}

# --- Impute NA meanlog with system average ---
impute_meanlog <- function(df, label) {
  n_na <- sum(is.na(df$meanlog))
  avg  <- mean(df$meanlog, na.rm = TRUE)
  df   <- df %>%
    mutate(
      meanlog_imputed = is.na(meanlog),
      meanlog         = ifelse(is.na(meanlog), avg, meanlog)
    )
  cat(label, "- NAs imputed:", n_na,
      "| imputation value (system avg meanlog):",
      round(avg, 4), "\n")
  return(df)
}

# ============================================================
# 5. HELIONIS CLUSTER
# ============================================================

helionis_workers <- freq_pricing_base %>%
  filter(solar_system == "Helionis Cluster")

helionis_workers$lambda  <- predict_lambda_as(helionis_workers,
                                              "Helionis Cluster")
helionis_workers$meanlog <- predict_meanlog_as(helionis_workers,
                                               "Helionis Cluster")
helionis_workers         <- impute_meanlog(helionis_workers, "Helionis")

# ============================================================
# 6. BAYESIA SYSTEM
# Proxy: 0.20 x Helionis + 0.80 x Zeta
# Workers: Epsilon historical workers used as exposure base
# Lambda and meanlog each blended separately
# ============================================================

bayesia_workers <- freq_pricing_base %>%
  filter(solar_system == "Epsilon")

# --- Frequency blending ---
lambda_h_for_b <- predict_lambda_as(bayesia_workers, "Helionis Cluster")
lambda_z_for_b <- predict_lambda_as(bayesia_workers, "Zeta")

bayesia_workers$lambda <- w_h_b * lambda_h_for_b +
  w_z_b * lambda_z_for_b

cat("\n--- Bayesia Lambda Diagnostics ---\n")
cat("  Helionis component mean lambda: ",
    round(mean(lambda_h_for_b), 4), "\n")
cat("  Zeta component mean lambda:     ",
    round(mean(lambda_z_for_b), 4), "\n")
cat("  Blended mean lambda:            ",
    round(mean(bayesia_workers$lambda), 4), "\n")

# --- Severity blending ---
meanlog_h_for_b <- predict_meanlog_as(bayesia_workers, "Helionis Cluster")
meanlog_z_for_b <- predict_meanlog_as(bayesia_workers, "Zeta")

bayesia_workers$meanlog <- w_h_b * meanlog_h_for_b +
  w_z_b * meanlog_z_for_b

cat("\n--- Bayesia Meanlog Diagnostics ---\n")
cat("  Helionis component mean meanlog:",
    round(mean(meanlog_h_for_b, na.rm = TRUE), 4), "\n")
cat("  Zeta component mean meanlog:    ",
    round(mean(meanlog_z_for_b, na.rm = TRUE), 4), "\n")
cat("  Blended mean meanlog:           ",
    round(mean(bayesia_workers$meanlog, na.rm = TRUE), 4), "\n")

bayesia_workers <- impute_meanlog(bayesia_workers, "Bayesia")

# ============================================================
# 7. ORYN DELTA
# Proxy: 0.55 x Helionis + 0.45 x Zeta
# Workers: Zeta historical workers used as exposure base
# Different from BI weights (0.9/0.1) - sporadic flares and
# low visibility create larger physical WC hazard than BI risk
# ============================================================

oryn_workers <- freq_pricing_base %>%
  filter(solar_system == "Zeta")

# --- Frequency blending ---
lambda_h_for_o <- predict_lambda_as(oryn_workers, "Helionis Cluster")
lambda_z_for_o <- predict_lambda_as(oryn_workers, "Zeta")

oryn_workers$lambda <- w_h_o * lambda_h_for_o +
  w_z_o * lambda_z_for_o

cat("\n--- Oryn Delta Lambda Diagnostics ---\n")
cat("  Helionis component mean lambda: ",
    round(mean(lambda_h_for_o), 4), "\n")
cat("  Zeta component mean lambda:     ",
    round(mean(lambda_z_for_o), 4), "\n")
cat("  Blended mean lambda:            ",
    round(mean(oryn_workers$lambda), 4), "\n")

# --- Severity blending ---
meanlog_h_for_o <- predict_meanlog_as(oryn_workers, "Helionis Cluster")
meanlog_z_for_o <- predict_meanlog_as(oryn_workers, "Zeta")

oryn_workers$meanlog <- w_h_o * meanlog_h_for_o +
  w_z_o * meanlog_z_for_o

cat("\n--- Oryn Delta Meanlog Diagnostics ---\n")
cat("  Helionis component mean meanlog:",
    round(mean(meanlog_h_for_o, na.rm = TRUE), 4), "\n")
cat("  Zeta component mean meanlog:    ",
    round(mean(meanlog_z_for_o, na.rm = TRUE), 4), "\n")
cat("  Blended mean meanlog:           ",
    round(mean(oryn_workers$meanlog, na.rm = TRUE), 4), "\n")

oryn_workers <- impute_meanlog(oryn_workers, "Oryn Delta")

# ============================================================
# 8. WORKER SUMMARY - ALL SYSTEMS SIDE BY SIDE
# ============================================================

cat("\n================================================\n")
cat("WORKER SUMMARY - WEIGHTED PROXY MODEL\n")
cat("================================================\n")

bind_rows(
  helionis_workers %>%
    summarise(
      system       = "Helionis Cluster",
      workers      = n(),
      avg_lambda   = round(mean(lambda), 4),
      avg_meanlog  = round(mean(meanlog), 4),
      avg_exp_sev  = round(mean(exp(meanlog + 0.5 * sigma_sev^2)), 2),
      imputed      = sum(meanlog_imputed)
    ),
  bayesia_workers %>%
    summarise(
      system       = "Bayesia System",
      workers      = n(),
      avg_lambda   = round(mean(lambda), 4),
      avg_meanlog  = round(mean(meanlog), 4),
      avg_exp_sev  = round(mean(exp(meanlog + 0.5 * sigma_sev^2)), 2),
      imputed      = sum(meanlog_imputed)
    ),
  oryn_workers %>%
    summarise(
      system       = "Oryn Delta",
      workers      = n(),
      avg_lambda   = round(mean(lambda), 4),
      avg_meanlog  = round(mean(meanlog), 4),
      avg_exp_sev  = round(mean(exp(meanlog + 0.5 * sigma_sev^2)), 2),
      imputed      = sum(meanlog_imputed)
    )
) %>% print()

# ============================================================
# 9. MONTE CARLO SIMULATION FUNCTION
# Unchanged from original - logic is identical
# ============================================================

simulate_worker_losses <- function(df, sigma, n_sim = 100000) {
  lambdas      <- df$lambda
  meanlogs     <- df$meanlog
  total_lambda <- sum(lambdas, na.rm = TRUE)
  weights      <- lambdas / total_lambda
  
  sims <- numeric(n_sim)
  
  for (i in 1:n_sim) {
    n_claims <- rpois(1, total_lambda)
    
    if (n_claims > 0) {
      idx     <- sample(seq_along(meanlogs),
                        size    = n_claims,
                        replace = TRUE,
                        prob    = weights)
      sev     <- rlnorm(n_claims,
                        meanlog = meanlogs[idx],
                        sdlog   = sigma)
      sims[i] <- sum(sev)
    } else {
      sims[i] <- 0
    }
  }
  return(sims)
}

# ============================================================
# 10. RUN SIMULATIONS
# ============================================================

set.seed(123)
cat("\n================================================\n")
cat("RUNNING MONTE CARLO SIMULATIONS\n")
cat("100,000 simulations per system\n")
cat("================================================\n")

mc_helionis <- simulate_worker_losses(helionis_workers,
                                      sigma = sigma_sev,
                                      n_sim = 100000)
cat("Helionis complete\n")

mc_bayesia  <- simulate_worker_losses(bayesia_workers,
                                      sigma = sigma_sev,
                                      n_sim = 100000)
cat("Bayesia complete\n")

mc_oryn     <- simulate_worker_losses(oryn_workers,
                                      sigma = sigma_sev,
                                      n_sim = 100000)
cat("Oryn Delta complete\n")

mc_total <- mc_helionis + mc_bayesia + mc_oryn
cat("All simulations complete\n")

# ============================================================
# 11. SUMMARISE RESULTS
# ============================================================

summarise_mc <- function(losses, system_name) {
  cat("\n---", system_name, "---\n")
  cat("Expected loss:      Đ",
      format(round(mean(losses), 0),           big.mark = ","), "\n")
  cat("Standard deviation: Đ",
      format(round(sd(losses), 0),             big.mark = ","), "\n")
  cat("Median:             Đ",
      format(round(quantile(losses, 0.50), 0), big.mark = ","), "\n")
  cat("VaR 95%:            Đ",
      format(round(quantile(losses, 0.95), 0), big.mark = ","), "\n")
  cat("VaR 99%:            Đ",
      format(round(quantile(losses, 0.99), 0), big.mark = ","), "\n")
  cat("TVaR 99%:           Đ",
      format(round(mean(losses[losses >
                                 quantile(losses, 0.99)]), 0),      big.mark = ","), "\n")
}

cat("\n================================================\n")
cat("AGGREGATE LOSS RESULTS - WEIGHTED PROXY MODEL\n")
cat("Bayesia    = 0.20 x Helionis + 0.80 x Zeta\n")
cat("Oryn Delta = 0.55 x Helionis + 0.45 x Zeta\n")
cat("================================================\n")

summarise_mc(mc_helionis, "Helionis Cluster")
summarise_mc(mc_bayesia,  "Bayesia System")
summarise_mc(mc_oryn,     "Oryn Delta")
summarise_mc(mc_total,    "TOTAL- All Systems")

##A.9.3.3 Pricing 
# ============================================================
# STEP 6: PRICING - WORKERS' COMP
# SD Principle: Premium = (Cost + 0.5×SD) × (1 + expense + profit)
# Output: ST and LT | Cost, Return, Net Revenue (95% CI) | Loss Ratio
# ============================================================

library(tidyverse)
library(MASS)
library(dplyr)

# ============================================================
# 0. PARAMETERS
# ============================================================

# Proxy weights
w_h_b <- 0.20;  w_z_b <- 0.80   # Bayesia    = 20% Helionis + 80% Zeta
w_h_o <- 0.55;  w_z_o <- 0.45   # Oryn Delta = 55% Helionis + 45% Zeta

# Premium loading
loading     <- 0.50
expense_pct <- 0.10
profit_pct  <- 0.05

# Economic parameters (from interest/inflation file)
inflation <- 0.0423   # 5yr avg 2170-2174
spot_1yr  <- 0.0457   # projected 2175
spot_10yr <- 0.0376   # 5yr projected avg
inv_return <- spot_10yr

# Growth rates (from encyclopedia)
growth_hb <- (1.25)^(1/10) - 1   # 2.257% pa - Helionis & Bayesia (+25% over 10yr)
growth_o  <- (1.15)^(1/10) - 1   # 1.407% pa - Oryn Delta (+15% over 10yr)

# Cosmic Quarry 2174 revenue
cq_revenue <- 61491e6

cat("================================================\n")
cat("ECONOMIC PARAMETERS\n")
cat("================================================\n")
cat("Inflation (5yr avg):  ", round(inflation  * 100, 3), "%\n")
cat("1yr spot rate:        ", round(spot_1yr   * 100, 3), "%\n")
cat("10yr spot rate:       ", round(spot_10yr  * 100, 3), "%\n")
cat("Investment return:    ", round(inv_return * 100, 3), "%\n")
cat("Growth Helionis/Bay:  ", round(growth_hb  * 100, 3), "% pa\n")
cat("Growth Oryn Delta:    ", round(growth_o   * 100, 3), "% pa\n")

# ============================================================
# 1. PERSONNEL DATASET
# Source: srcsc-2026-cosmic-quarry-personnel.xlsx
# ============================================================

personnel_raw <- tribble(
  ~role,                       ~headcount,  ~ft,    ~salary,
  # Management
  "Executive",                         12,     12,   500000,
  "Vice President",                    54,     54,   250000,
  "Director",                         111,    111,   150000,
  # Administration
  "HR",                               426,    426,    80000,
  "IT",                               568,    426,    85000,
  "Legal",                            142,     85,   110000,
  "Finance & Accounting",             426,    398,   100000,
  # Environmental & Safety
  "Environmental Scientists",         711,    569,   120000,
  "Safety Officer",                  2132,   2132,    80000,
  "Medical Personel",                1421,   1137,   130000,
  # Exploration Operations
  "Geoligist",                        152,    152,   120000,
  "Scientist",                        211,    169,   120000,
  "Field technician",                 842,    421,    55000,
  # Extraction Operations
  "Drilling operators",              2526,   1684,    60000,
  "Maintenance",                    10316,   8253,    65000,
  "Engineers",                       2684,   2684,    95000,
  "Freight operators",               3553,   2842,    60000,
  "Robotics technician",              711,    711,    75000,
  # Spacecraft Operations
  "Navigation officers",             2842,   2842,    85000,
  "Maintenance (SC)",                1137,   1137,    65000,
  "Security personel",               2132,   1421,    55000,
  "Steward",                          568,    568,    65000,
  "Galleyhand",                      2132,      0,    40000
)

# Map to WC model occupation levels
occ_map <- c(
  "Executive"                = "Executive",
  "Vice President"           = "Manager",
  "Director"                 = "Manager",
  "HR"                       = "Administrator",
  "IT"                       = "Technology Officer",
  "Legal"                    = "Administrator",
  "Finance & Accounting"     = "Administrator",
  "Environmental Scientists" = "Scientist",
  "Safety Officer"           = "Safety Officer",
  "Medical Personel"         = "Administrator",
  "Geoligist"                = "Scientist",
  "Scientist"                = "Scientist",
  "Field technician"         = "Maintenance Staff",
  "Drilling operators"       = "Drill Operator",
  "Maintenance"              = "Maintenance Staff",
  "Engineers"                = "Engineer",
  "Freight operators"        = "Spacecraft Operator",
  "Robotics technician"      = "Engineer",
  "Navigation officers"      = "Spacecraft Operator",
  "Maintenance (SC)"         = "Maintenance Staff",
  "Security personel"        = "Safety Officer",
  "Steward"                  = "Administrator",
  "Galleyhand"               = "Planetary Operations"
)

personnel_raw <- personnel_raw %>%
  mutate(
    occupation      = occ_map[role],
    ft_pct          = ft / headcount,
    employment_type = ifelse(ft_pct >= 0.9, "Full-time", "Contract")
  )

total_workers  <- sum(personnel_raw$headcount)
total_payroll  <- sum(personnel_raw$headcount * personnel_raw$salary)
wtd_avg_salary <- total_payroll / total_workers

cat("\n================================================\n")
cat("PERSONNEL SUMMARY\n")
cat("================================================\n")
cat("Total CQ workers:    ", format(total_workers,                    big.mark = ","), "\n")
cat("Total payroll:      Đ", format(round(total_payroll,  0),         big.mark = ","), "\n")
cat("Wtd avg salary:     Đ", format(round(wtd_avg_salary, 0),         big.mark = ","), "\n")

# ============================================================
# 2. SPLIT WORKERS BY SOLAR SYSTEM
# Mine-count proportions from encyclopedia:
# 30 Helionis / 15 Bayesia / 10 Oryn Delta = 55 total
# ============================================================

prop_h <- 30/55   # 54.5% Helionis
prop_b <- 15/55   # 27.3% Bayesia
prop_o <- 10/55   # 18.2% Oryn Delta

payroll_h <- total_payroll * prop_h
payroll_b <- total_payroll * prop_b
payroll_o <- total_payroll * prop_o

cat("\n--- System Split (mine-count proportions) ---\n")
cat("Helionis:", round(prop_h*100, 1), "% =",
    round(total_workers*prop_h), "workers | Payroll Đ",
    format(round(payroll_h, 0), big.mark = ","), "\n")
cat("Bayesia: ", round(prop_b*100, 1), "% =",
    round(total_workers*prop_b), "workers | Payroll Đ",
    format(round(payroll_b, 0), big.mark = ","), "\n")
cat("Oryn:    ", round(prop_o*100, 1), "% =",
    round(total_workers*prop_o), "workers | Payroll Đ",
    format(round(payroll_o, 0), big.mark = ","), "\n")

# ============================================================
# 3. BUILD WORKER-LEVEL PRICING DATASET
# Imputed covariates from training data:
#   psych_stress_index     = 3   (median)
#   safety_training_index  = 3   (median)
#   protective_gear_quality= 5   (mode)
#   accident_history_flag  = 0   (mode)
#   injury_cause           = "Caught in machine" (mode)
#   claim_length           = 7   (median days)
#   exposure               = 1.0 (per worker per year)
# ============================================================

build_system_data <- function(personnel, system_label, proportion) {
  personnel %>%
    mutate(
      n_workers               = round(headcount * proportion),
      solar_system            = system_label,
      psych_stress_index      = factor(3,
                                       levels = levels(sev_pricing$psych_stress_index)),
      safety_training_index   = factor(3,
                                       levels = levels(sev_pricing$safety_training_index)),
      protective_gear_quality = factor(5,
                                       levels = levels(sev_pricing$protective_gear_quality)),
      accident_history_flag   = factor(0,
                                       levels = levels(freq_model$accident_history_flag)),
      injury_cause            = factor("Caught in machine",
                                       levels = levels(sev_pricing$injury_cause)),
      claim_length            = 7,
      base_salary             = salary,
      exposure                = 1
    ) %>%
    filter(n_workers > 0) %>%
    uncount(n_workers)
}

align_factors <- function(df) {
  df %>%
    mutate(
      occupation      = factor(occupation,
                               levels = levels(freq_model$occupation)),
      employment_type = factor(employment_type,
                               levels = levels(freq_model$employment_type)),
      solar_system    = factor(solar_system,
                               levels = levels(freq_model$solar_system))
    )
}

# Epsilon used as Bayesia proxy; Zeta as Oryn Delta proxy
helionis_cq <- build_system_data(personnel_raw, "Helionis Cluster", prop_h) %>%
  align_factors()
bayesia_cq  <- build_system_data(personnel_raw, "Epsilon",          prop_b) %>%
  align_factors()
oryn_cq     <- build_system_data(personnel_raw, "Zeta",             prop_o) %>%
  align_factors()

cat("\n--- Worker Rows Built ---\n")
cat("Helionis:              ", nrow(helionis_cq), "\n")
cat("Bayesia (Epsilon base):", nrow(bayesia_cq),  "\n")
cat("Oryn Delta (Zeta base):", nrow(oryn_cq),     "\n")

# ============================================================
# 4. PREDICT LAMBDA AND MEANLOG - WEIGHTED PROXY BLENDING
# ============================================================

predict_lambda_as <- function(df, system) {
  df %>%
    mutate(solar_system = factor(system,
                                 levels = levels(freq_model$solar_system))) %>%
    predict(model_reduced, newdata = ., type = "response")
}

predict_meanlog_as <- function(df, system) {
  df %>%
    mutate(
      solar_system            = factor(system,
                                       levels = levels(sev_pricing$solar_system)),
      occupation              = factor(occupation,
                                       levels = levels(sev_pricing$occupation)),
      employment_type         = factor(employment_type,
                                       levels = levels(sev_pricing$employment_type)),
      psych_stress_index      = factor(psych_stress_index,
                                       levels = levels(sev_pricing$psych_stress_index)),
      safety_training_index   = factor(safety_training_index,
                                       levels = levels(sev_pricing$safety_training_index)),
      protective_gear_quality = factor(protective_gear_quality,
                                       levels = levels(sev_pricing$protective_gear_quality))
    ) %>%
    predict(model_sev_reduced, newdata = ., type = "response")
}

impute_meanlog <- function(df, label) {
  n_na <- sum(is.na(df$meanlog))
  avg  <- mean(df$meanlog, na.rm = TRUE)
  df   <- df %>%
    mutate(
      meanlog_imputed = is.na(meanlog),
      meanlog         = ifelse(is.na(meanlog), avg, meanlog)
    )
  cat(label, "- NAs imputed:", n_na,
      "| system avg meanlog:", round(avg, 4), "\n")
  df
}

cat("\n--- Predicting Lambda and Meanlog ---\n")

# Helionis - direct, no blending
helionis_cq$lambda  <- predict_lambda_as(helionis_cq, "Helionis Cluster")
helionis_cq$meanlog <- predict_meanlog_as(helionis_cq, "Helionis Cluster")
helionis_cq         <- impute_meanlog(helionis_cq, "Helionis")

# Bayesia - 20% Helionis + 80% Zeta
lam_h_b            <- predict_lambda_as(bayesia_cq, "Helionis Cluster")
lam_z_b            <- predict_lambda_as(bayesia_cq, "Zeta")
bayesia_cq$lambda  <- w_h_b * lam_h_b + w_z_b * lam_z_b

ml_h_b             <- predict_meanlog_as(bayesia_cq, "Helionis Cluster")
ml_z_b             <- predict_meanlog_as(bayesia_cq, "Zeta")
bayesia_cq$meanlog <- w_h_b * ml_h_b + w_z_b * ml_z_b
bayesia_cq         <- impute_meanlog(bayesia_cq, "Bayesia")

# Oryn Delta - 55% Helionis + 45% Zeta
lam_h_o           <- predict_lambda_as(oryn_cq, "Helionis Cluster")
lam_z_o           <- predict_lambda_as(oryn_cq, "Zeta")
oryn_cq$lambda    <- w_h_o * lam_h_o + w_z_o * lam_z_o

ml_h_o            <- predict_meanlog_as(oryn_cq, "Helionis Cluster")
ml_z_o            <- predict_meanlog_as(oryn_cq, "Zeta")
oryn_cq$meanlog   <- w_h_o * ml_h_o + w_z_o * ml_z_o
oryn_cq           <- impute_meanlog(oryn_cq, "Oryn Delta")

cat("\n--- CQ Workforce Pricing Summary ---\n")
bind_rows(
  helionis_cq %>% summarise(system = "Helionis", workers = n(),
                            avg_lambda  = round(mean(lambda),  4),
                            avg_meanlog = round(mean(meanlog), 4),
                            avg_exp_sev = round(mean(exp(meanlog + 0.5 * sigma_sev^2)), 2)),
  bayesia_cq  %>% summarise(system = "Bayesia",  workers = n(),
                            avg_lambda  = round(mean(lambda),  4),
                            avg_meanlog = round(mean(meanlog), 4),
                            avg_exp_sev = round(mean(exp(meanlog + 0.5 * sigma_sev^2)), 2)),
  oryn_cq     %>% summarise(system = "Oryn Delta", workers = n(),
                            avg_lambda  = round(mean(lambda),  4),
                            avg_meanlog = round(mean(meanlog), 4),
                            avg_exp_sev = round(mean(exp(meanlog + 0.5 * sigma_sev^2)), 2))
) %>% print()

# ============================================================
# 5. MONTE CARLO SIMULATION
# ============================================================

simulate_worker_losses <- function(df, sigma, n_sim = 100000) {
  lambdas      <- df$lambda
  meanlogs     <- df$meanlog
  total_lambda <- sum(lambdas, na.rm = TRUE)
  weights      <- lambdas / total_lambda
  sims         <- numeric(n_sim)
  for (i in seq_len(n_sim)) {
    n_claims <- rpois(1, total_lambda)
    if (n_claims > 0) {
      idx     <- sample(seq_along(meanlogs), size = n_claims,
                        replace = TRUE, prob = weights)
      sims[i] <- sum(rlnorm(n_claims,
                            meanlog = meanlogs[idx],
                            sdlog   = sigma_sev))
    }
  }
  sims
}

set.seed(123)
cat("\n--- Running Monte Carlo (100,000 sims per system) ---\n")
mc_h <- simulate_worker_losses(helionis_cq, sigma_sev); cat("Helionis done\n")
mc_b <- simulate_worker_losses(bayesia_cq,  sigma_sev); cat("Bayesia done\n")
mc_o <- simulate_worker_losses(oryn_cq,     sigma_sev); cat("Oryn Delta done\n")
mc_t <- mc_h + mc_b + mc_o;                             cat("Total done\n")

# ============================================================
# 6. EXTRACT SIMULATION STATISTICS
# 95% CI = 2.5th to 97.5th percentile
# ============================================================

sim_stats <- function(mc, label) {
  list(
    system = label,
    E      = mean(mc),
    SD     = sd(mc),
    low    = as.numeric(quantile(mc, 0.025)),
    high   = as.numeric(quantile(mc, 0.975)),
    VaR99  = as.numeric(quantile(mc, 0.99)),
    TVaR99 = mean(mc[mc > quantile(mc, 0.99)])
  )
}

s_h <- sim_stats(mc_h, "Helionis Cluster")
s_b <- sim_stats(mc_b, "Bayesia System")
s_o <- sim_stats(mc_o, "Oryn Delta")
s_t <- sim_stats(mc_t, "TOTAL")

cat("\n--- Raw Simulation Stats ---\n")
bind_rows(lapply(list(s_h, s_b, s_o, s_t), as_tibble)) %>%
  mutate(across(where(is.numeric), ~round(., 0))) %>%
  print()

# ============================================================
# 7. COST FUNCTIONS (trend + discount)
# ST: 1-year horizon - trend by inflation × growth, discount by 1yr spot
# LT: 10-year horizon - compound inflation, growth, discount by 10yr spot
# ============================================================

trend_discount_ST <- function(loss, growth) {
  loss * (1 + inflation) * (1 + growth) / (1 + spot_1yr)
}

trend_discount_LT <- function(loss, growth) {
  loss * (1 + inflation)^10 * (1 + growth)^10 / (1 + spot_10yr)^10
}

# ============================================================
# 8. FULL PRICING FUNCTION
# Returns: Cost, Return, Net Revenue (base + 95% CI) | Loss Ratio
# ============================================================

calc_premium <- function(cost, sd, loading, exp_pct, prof_pct) {
  (cost + loading * sd) * (1 + exp_pct + prof_pct)
}

price_system <- function(stats, growth, payroll, sys_label) {
  
  E  <- stats$E
  SD <- stats$SD
  lo <- stats$low
  hi <- stats$high
  
  # --- COSTS ---
  ST_c    <- trend_discount_ST(E,  growth)
  ST_c_lo <- trend_discount_ST(lo, growth)
  ST_c_hi <- trend_discount_ST(hi, growth)
  
  LT_c    <- trend_discount_LT(E,  growth)
  LT_c_lo <- trend_discount_LT(lo, growth)
  LT_c_hi <- trend_discount_LT(hi, growth)
  
  # --- PREMIUMS (SD loading fixed; only cost changes across CI) ---
  ST_SD <- SD
  LT_SD <- SD * sqrt(10)
  
  ST_p    <- calc_premium(ST_c,    ST_SD, loading, expense_pct, profit_pct)
  ST_p_lo <- calc_premium(ST_c_lo, ST_SD, loading, expense_pct, profit_pct)
  ST_p_hi <- calc_premium(ST_c_hi, ST_SD, loading, expense_pct, profit_pct)
  
  LT_p    <- calc_premium(LT_c,    LT_SD, loading, expense_pct, profit_pct)
  LT_p_lo <- calc_premium(LT_c_lo, LT_SD, loading, expense_pct, profit_pct)
  LT_p_hi <- calc_premium(LT_c_hi, LT_SD, loading, expense_pct, profit_pct)
  
  # --- INVESTMENT INCOME (cost × inv_return) ---
  ST_inv    <- ST_c    * inv_return
  ST_inv_lo <- ST_c_lo * inv_return
  ST_inv_hi <- ST_c_hi * inv_return
  
  LT_inv    <- LT_c    * inv_return
  LT_inv_lo <- LT_c_lo * inv_return
  LT_inv_hi <- LT_c_hi * inv_return
  
  # --- RETURN = premium + investment income ---
  ST_ret    <- ST_p    + ST_inv
  ST_ret_lo <- ST_p_lo + ST_inv_lo
  ST_ret_hi <- ST_p_hi + ST_inv_hi
  
  LT_ret    <- LT_p    + LT_inv
  LT_ret_lo <- LT_p_lo + LT_inv_lo
  LT_ret_hi <- LT_p_hi + LT_inv_hi
  
  # --- NET REVENUE = return - cost ---
  ST_net    <- ST_ret    - ST_c
  ST_net_lo <- ST_ret_lo - ST_c_lo
  ST_net_hi <- ST_ret_hi - ST_c_hi
  
  LT_net    <- LT_ret    - LT_c
  LT_net_lo <- LT_ret_lo - LT_c_lo
  LT_net_hi <- LT_ret_hi - LT_c_hi
  
  # --- LOSS RATIOS ---
  ST_lr <- ST_c / ST_p * 100
  LT_lr <- LT_c / LT_p * 100
  
  # --- RATE PER Đ100 PAYROLL ---
  ST_rate <- ST_p / (payroll / 100)
  LT_rate <- LT_p / (payroll / 100)
  
  tibble(
    System        = sys_label,
    # SHORT TERM
    ST_Cost_Base  = round(ST_c,      0),
    ST_Cost_Low   = round(ST_c_lo,   0),
    ST_Cost_High  = round(ST_c_hi,   0),
    ST_Prem_Base  = round(ST_p,      0),
    ST_Prem_Low   = round(ST_p_lo,   0),
    ST_Prem_High  = round(ST_p_hi,   0),
    ST_Ret_Base   = round(ST_ret,    0),
    ST_Ret_Low    = round(ST_ret_lo, 0),
    ST_Ret_High   = round(ST_ret_hi, 0),
    ST_Net_Base   = round(ST_net,    0),
    ST_Net_Low    = round(ST_net_lo, 0),
    ST_Net_High   = round(ST_net_hi, 0),
    ST_LossRatio  = round(ST_lr,     1),
    ST_Rate_100   = round(ST_rate,   4),
    # LONG TERM
    LT_Cost_Base  = round(LT_c,      0),
    LT_Cost_Low   = round(LT_c_lo,   0),
    LT_Cost_High  = round(LT_c_hi,   0),
    LT_Prem_Base  = round(LT_p,      0),
    LT_Prem_Low   = round(LT_p_lo,   0),
    LT_Prem_High  = round(LT_p_hi,   0),
    LT_Ret_Base   = round(LT_ret,    0),
    LT_Ret_Low    = round(LT_ret_lo, 0),
    LT_Ret_High   = round(LT_ret_hi, 0),
    LT_Net_Base   = round(LT_net,    0),
    LT_Net_Low    = round(LT_net_lo, 0),
    LT_Net_High   = round(LT_net_hi, 0),
    LT_LossRatio  = round(LT_lr,     1),
    LT_Rate_100   = round(LT_rate,   4)
  )
}

results <- bind_rows(
  price_system(s_h, growth_hb, payroll_h, "Helionis Cluster"),
  price_system(s_b, growth_hb, payroll_b, "Bayesia System"),
  price_system(s_o, growth_o,  payroll_o, "Oryn Delta")
)

# ============================================================
# 9. PRINT RESULTS
# Format: Mean (2.5th - 97.5th pct)
# ============================================================

fmt <- function(base, lo, hi) {
  paste0(format(base, big.mark = ","),
         " (", format(lo, big.mark = ","),
         " - ", format(hi, big.mark = ","), ")")
}

output_table <- bind_rows(
  results %>% transmute(
    System      = System,
    Horizon     = "ST",
    Cost        = fmt(ST_Cost_Base, ST_Cost_Low,  ST_Cost_High),
    Return      = fmt(ST_Ret_Base,  ST_Ret_Low,   ST_Ret_High),
    Net_Revenue = fmt(ST_Net_Base,  ST_Net_Low,   ST_Net_High),
    Loss_Ratio  = paste0(ST_LossRatio, "%"),
    Rate_per_100 = ST_Rate_100
  ),
  results %>% transmute(
    System      = System,
    Horizon     = "LT",
    Cost        = fmt(LT_Cost_Base, LT_Cost_Low,  LT_Cost_High),
    Return      = fmt(LT_Ret_Base,  LT_Ret_Low,   LT_Ret_High),
    Net_Revenue = fmt(LT_Net_Base,  LT_Net_Low,   LT_Net_High),
    Loss_Ratio  = paste0(LT_LossRatio, "%"),
    Rate_per_100 = LT_Rate_100
  )
) %>% arrange(System, Horizon)

cat("\n================================================\n")
cat("WORKERS' COMP PRICING - 95% CONFIDENCE INTERVALS\n")
cat("Cost/Return/Net Revenue: Mean (2.5th - 97.5th pct)\n")
cat("Premium = (Cost + 0.50 x SD) x (1 + 10% exp + 5% profit)\n")
cat("================================================\n")
print(output_table, n = Inf, width = Inf)

# ============================================================
# 10. TOTALS
# ============================================================

ST_total_cost <- sum(results$ST_Cost_Base)
ST_total_ret  <- sum(results$ST_Ret_Base)
ST_total_net  <- sum(results$ST_Net_Base)
ST_total_prem <- sum(results$ST_Prem_Base)

LT_total_cost <- sum(results$LT_Cost_Base)
LT_total_ret  <- sum(results$LT_Ret_Base)
LT_total_net  <- sum(results$LT_Net_Base)
LT_total_prem <- sum(results$LT_Prem_Base)

cat("\n================================================\n")
cat("TOTALS - ALL SYSTEMS\n")
cat("================================================\n")
cat("ST | Cost:    Đ", format(round(ST_total_cost, 0), big.mark = ","), "\n")
cat("ST | Return:  Đ", format(round(ST_total_ret,  0), big.mark = ","), "\n")
cat("ST | Net Rev: Đ", format(round(ST_total_net,  0), big.mark = ","), "\n")
cat("LT | Cost:    Đ", format(round(LT_total_cost, 0), big.mark = ","), "\n")
cat("LT | Return:  Đ", format(round(LT_total_ret,  0), big.mark = ","), "\n")
cat("LT | Net Rev: Đ", format(round(LT_total_net,  0), big.mark = ","), "\n")

cat("\n================================================\n")
cat("PREMIUM AS % OF CQ REVENUE (Đ61.5bn)\n")
cat("================================================\n")
cat("ST Total Premium: Đ", format(round(ST_total_prem, 0), big.mark = ","),
    "=", round(ST_total_prem / cq_revenue * 100, 4), "%\n")
cat("LT Total Premium: Đ", format(round(LT_total_prem, 0), big.mark = ","),
    "=", round(LT_total_prem / cq_revenue * 100, 4), "%\n")

# ============================================================
# 11. KEY ASSUMPTIONS SUMMARY
# ============================================================

cat("\n================================================\n")
cat("KEY ASSUMPTIONS\n")
cat("================================================\n")
cat("Exposure base:          CQ personnel file (", total_workers, "workers)\n")
cat("System split:           Mine-count proportions (55% / 27% / 18%)\n")
cat("Proxy - Bayesia:        20% Helionis + 80% Zeta\n")
cat("Proxy - Oryn Delta:     55% Helionis + 45% Zeta\n")
cat("Imputed psych stress:   3 (training data median)\n")
cat("Imputed safety train:   3 (training data median)\n")
cat("Imputed gear quality:   5 (training data mode)\n")
cat("Imputed acc. history:   0 (training data mode)\n")
cat("Imputed injury cause:   Caught in machine (mode)\n")
cat("Imputed claim length:   7 days (median)\n")
cat("SD loading:             0.50\n")
cat("Expense ratio:          10%\n")
cat("Profit margin:          5%\n")
cat("Inflation:              4.23% (5yr avg 2170-2174)\n")
cat("1yr spot rate:          4.57% (projected 2175)\n")
cat("10yr spot rate:         3.76% (5yr projected avg)\n")
cat("Investment return:      3.76% (10yr spot proxy)\n")
cat("Growth Helionis/Bay:    2.257% pa (+25% over 10yr)\n")
cat("Growth Oryn Delta:      1.407% pa (+15% over 10yr)\n")
cat("CI:                     95% (2.5th to 97.5th pct)\n")
cat("Simulations:            100,000 per system, seed = 123\n")
cat("================================================\n")

##A.9.3.4 Stress + Scenario testing

# ============================================================
# STEP 7A: SYSTEM-SPECIFIC STRESS TESTS
# Each stress shocks ONE system - other two stay at base
#
# Multiplier mechanics:
#   lambda_stressed  = lambda * freq_mult
#   meanlog_stressed = meanlog + log(sev_mult)
# ============================================================

library(tidyverse)
library(dplyr)

# ============================================================
# 0. PULL PRICING OUTPUTS FROM STEP 5 RESULTS
# ============================================================

ST_prem_h <- results$ST_Prem_Base[results$System == "Helionis Cluster"]
ST_prem_b <- results$ST_Prem_Base[results$System == "Bayesia System"]
ST_prem_o <- results$ST_Prem_Base[results$System == "Oryn Delta"]
ST_prem_t <- sum(results$ST_Prem_Base)

LT_prem_h <- results$LT_Prem_Base[results$System == "Helionis Cluster"]
LT_prem_b <- results$LT_Prem_Base[results$System == "Bayesia System"]
LT_prem_o <- results$LT_Prem_Base[results$System == "Oryn Delta"]
LT_prem_t <- sum(results$LT_Prem_Base)

ST_cost_t <- sum(results$ST_Cost_Base)
LT_cost_t <- sum(results$LT_Cost_Base)

ST_inv_t  <- ST_cost_t * inv_return
LT_inv_t  <- LT_cost_t * inv_return

workers_h <- nrow(helionis_cq)
workers_b <- nrow(bayesia_cq)
workers_o <- nrow(oryn_cq)
workers_t <- workers_h + workers_b + workers_o

avg_growth <- (growth_hb * (workers_h + workers_b) +
                 growth_o  *  workers_o) / workers_t

# ============================================================
# 1. CORE STRESSED SIMULATION FUNCTION
# ============================================================

simulate_stressed <- function(df, freq_mult, sev_mult,
                              sigma, n_sim = 100000) {
  lambdas_s  <- df$lambda  * freq_mult
  meanlogs_s <- df$meanlog + log(sev_mult)
  total_lam  <- sum(lambdas_s, na.rm = TRUE)
  weights    <- lambdas_s / total_lam
  sims       <- numeric(n_sim)
  for (i in seq_len(n_sim)) {
    n_claims <- rpois(1, total_lam)
    if (n_claims > 0) {
      idx     <- sample(seq_along(meanlogs_s), size = n_claims,
                        replace = TRUE, prob = weights)
      sims[i] <- sum(rlnorm(n_claims,
                            meanlog = meanlogs_s[idx],
                            sdlog   = sigma_sev))
    }
  }
  list(
    mean   = mean(sims),
    sd     = sd(sims),
    var95  = as.numeric(quantile(sims, 0.95)),
    var99  = as.numeric(quantile(sims, 0.99)),
    tvar99 = mean(sims[sims > quantile(sims, 0.99)])
  )
}

# ============================================================
# 2. TREND / DISCOUNT + NET REVENUE HELPERS
# ============================================================

td_ST <- function(loss) {
  loss * (1 + inflation) * (1 + avg_growth) / (1 + spot_1yr)
}

td_LT <- function(loss) {
  loss * (1 + inflation)^10 *
    (1 + avg_growth)^10 / (1 + spot_10yr)^10
}

ST_net_rev <- function(loss) ST_prem_t + ST_inv_t - td_ST(loss)
LT_net_rev <- function(loss) LT_prem_t + LT_inv_t - td_LT(loss)

# ============================================================
# 3. SYSTEM-SPECIFIC STRESS TESTS
# ============================================================

cat("================================================\n")
cat("STEP 7A - SYSTEM-SPECIFIC STRESS TESTS\n")
cat("================================================\n")
cat("\nRunning 100,000 simulations per stress test...\n")

set.seed(42)

run_system_stress <- function(stressed_system,
                              freq_m, sev_m, label) {
  cat(" Stressing:", label, "\n")
  
  h_res <- if (stressed_system == "H")
    simulate_stressed(helionis_cq, freq_m, sev_m, sigma_sev) else
      simulate_stressed(helionis_cq, 1.00,  1.00,  sigma_sev)
  
  b_res <- if (stressed_system == "B")
    simulate_stressed(bayesia_cq,  freq_m, sev_m, sigma_sev) else
      simulate_stressed(bayesia_cq,  1.00,  1.00,  sigma_sev)
  
  o_res <- if (stressed_system == "O")
    simulate_stressed(oryn_cq,     freq_m, sev_m, sigma_sev) else
      simulate_stressed(oryn_cq,     1.00,  1.00,  sigma_sev)
  
  total <- h_res$mean + b_res$mean + o_res$mean
  
  tibble(
    Stress        = label,
    Freq_Mult     = freq_m,
    Sev_Mult      = sev_m,
    Raw_Loss      = round(total, 0),
    vs_Base_pct   = round(total / s_t$E * 100 - 100, 1),
    ST_Loss       = round(td_ST(total), 0),
    ST_NetRev     = round(ST_net_rev(total), 0),
    ST_LossRatio  = round(td_ST(total) / ST_prem_t * 100, 1),
    LT_Loss       = round(td_LT(total), 0),
    LT_NetRev     = round(LT_net_rev(total), 0),
    LT_LossRatio  = round(td_LT(total) / LT_prem_t * 100, 1),
    Profitable_ST = ifelse(ST_net_rev(total) > 0, "YES", "NO"),
    Profitable_LT = ifelse(LT_net_rev(total) > 0, "YES", "NO")
  )
}

system_stress <- bind_rows(
  run_system_stress("H", 1.25, 1.20,
                    "Helionis: asteroid fragmentation"),
  run_system_stress("B", 1.30, 1.35,
                    "Bayesia: radiation spike"),
  run_system_stress("O", 1.45, 1.30,
                    "Oryn Delta: frontier disruption")
)

# ============================================================
# 4. PRINT RESULTS
# ============================================================

cat("\n================================================\n")
cat("STRESS TEST RESULTS - SHORT TERM\n")
cat("Base premium: Đ",
    format(round(ST_prem_t, 0), big.mark = ","), "\n")
cat("================================================\n")
system_stress %>%
  dplyr::select(Stress, Freq_Mult, Sev_Mult,
                ST_Loss, ST_NetRev, ST_LossRatio, Profitable_ST) %>%
  print()

cat("\n================================================\n")
cat("STRESS TEST RESULTS - LONG TERM\n")
cat("Base premium: Đ",
    format(round(LT_prem_t, 0), big.mark = ","), "\n")
cat("================================================\n")
system_stress %>%
  dplyr::select(Stress, LT_Loss, LT_NetRev,
                LT_LossRatio, Profitable_LT) %>%
  print()

# ============================================================
# 5. STRESS MULTIPLIER REFERENCE TABLE
# ============================================================

cat("\n================================================\n")
cat("STRESS MULTIPLIER REFERENCE\n")
cat("================================================\n")
data.frame(
  System    = c("Helionis Cluster",
                "Bayesia System",
                "Oryn Delta"),
  Event     = c("Asteroid fragmentation",
                "Binary star radiation spike",
                "Frontier extraction disruption"),
  Freq_Mult = c(1.25, 1.30, 1.45),
  Sev_Mult  = c(1.20, 1.35, 1.30),
  Source    = c(
    "Erratic clusters, micro-collisions, debris clouds (Enc.)",
    "EM + particle spikes at binary orbital alignment (Enc.)",
    "Orbital shear, low-vis, remote site, inconsistent comms (Enc.)"
  )
) %>% print()
cat("================================================\n")

# ============================================================
# STEP 7B: SCENARIO ANALYSIS + CORRELATED RISK
#
# Part A - Portfolio-wide scenarios (best / moderate / worst)
#   Same multiplier applied uniformly to all three systems
#
# Part B - Correlated risk scenarios
#   Multiple systems hit simultaneously with system-specific
#   multipliers reflecting different exposure mechanisms
#
# Multiplier mechanics (same engine as stress testing):
#   lambda_stressed  = lambda * freq_mult
#   meanlog_stressed = meanlog + log(sev_mult)
# ============================================================

library(tidyverse)
library(dplyr)

# ============================================================
# 0. PULL PRICING OUTPUTS FROM STEP 5 RESULTS
# ============================================================

ST_prem_t <- sum(results$ST_Prem_Base)
LT_prem_t <- sum(results$LT_Prem_Base)
ST_cost_t <- sum(results$ST_Cost_Base)
LT_cost_t <- sum(results$LT_Cost_Base)
ST_inv_t  <- ST_cost_t * inv_return
LT_inv_t  <- LT_cost_t * inv_return

workers_h <- nrow(helionis_cq)
workers_b <- nrow(bayesia_cq)
workers_o <- nrow(oryn_cq)
workers_t <- workers_h + workers_b + workers_o

avg_growth <- (growth_hb * (workers_h + workers_b) +
                 growth_o  *  workers_o) / workers_t

# ============================================================
# 1. SIMULATION FUNCTION (same engine as Step 7A)
# ============================================================

simulate_stressed <- function(df, freq_mult, sev_mult,
                              sigma, n_sim = 100000) {
  lambdas_s  <- df$lambda  * freq_mult
  meanlogs_s <- df$meanlog + log(sev_mult)
  total_lam  <- sum(lambdas_s, na.rm = TRUE)
  weights    <- lambdas_s / total_lam
  sims       <- numeric(n_sim)
  for (i in seq_len(n_sim)) {
    n_claims <- rpois(1, total_lam)
    if (n_claims > 0) {
      idx     <- sample(seq_along(meanlogs_s), size = n_claims,
                        replace = TRUE, prob = weights)
      sims[i] <- sum(rlnorm(n_claims,
                            meanlog = meanlogs_s[idx],
                            sdlog   = sigma_sev))
    }
  }
  list(
    mean   = mean(sims),
    sd     = sd(sims),
    var95  = as.numeric(quantile(sims, 0.95)),
    var99  = as.numeric(quantile(sims, 0.99)),
    tvar99 = mean(sims[sims > quantile(sims, 0.99)])
  )
}

# ============================================================
# 2. TREND / DISCOUNT + NET REVENUE HELPERS
# ============================================================

td_ST <- function(loss) {
  loss * (1 + inflation) * (1 + avg_growth) / (1 + spot_1yr)
}

td_LT <- function(loss) {
  loss * (1 + inflation)^10 *
    (1 + avg_growth)^10 / (1 + spot_10yr)^10
}

ST_net_rev <- function(loss) ST_prem_t + ST_inv_t - td_ST(loss)
LT_net_rev <- function(loss) LT_prem_t + LT_inv_t - td_LT(loss)

# ============================================================
# PART A: PORTFOLIO-WIDE SCENARIOS
# ============================================================
#
# BEST CASE (freq x0.85, sev x0.90)
#   All systems in a quiet operational period. No asteroid
#   activity, no radiation spikes, no frontier incidents.
#   Safety compliance is high, response infrastructure fully
#   operational. Only minor routine injuries occur. Frequency
#   and severity both fall below average because conditions
#   are as controlled as they can realistically be in space
#   mining.
#
# MODERATE CASE (freq x1.05, sev x1.05)
#   One mildly elevated year driven primarily by Oryn Delta's
#   frontier expansion into the asteroid ring. Equipment
#   strain and navigational hazards produce a modest increase
#   in incident rates. Other systems perform normally. A 5%
#   rise in both metrics reflects realistic frontier
#   operational pressure without implying a structurally
#   deteriorating risk environment.
#
# WORST CASE (freq x1.40, sev x1.35)
#   A Bayesia radiation spike coincides with a communications
#   disruption across all systems. Radiation exposure claims
#   surge across Bayesia and Oryn Delta simultaneously while
#   delayed emergency response worsens outcomes everywhere.
#   Multipliers are strong but deliberately below mass-
#   casualty levels - this is a severe operational year, not
#   a single catastrophic accident, which is the realistic
#   WC tail event for this portfolio.
# ============================================================

cat("================================================\n")
cat("STEP 7B - SCENARIO ANALYSIS + CORRELATED RISK\n")
cat("================================================\n")

cat("\n================================================\n")
cat("PART A: PORTFOLIO-WIDE SCENARIOS\n")
cat("================================================\n")
cat("\nRunning 100,000 simulations per scenario...\n")

set.seed(99)

run_scenario <- function(freq_m, sev_m, label) {
  cat(" Running:", label, "\n")
  h <- simulate_stressed(helionis_cq, freq_m, sev_m, sigma_sev)
  b <- simulate_stressed(bayesia_cq,  freq_m, sev_m, sigma_sev)
  o <- simulate_stressed(oryn_cq,     freq_m, sev_m, sigma_sev)
  
  total_mean  <- h$mean  + b$mean  + o$mean
  total_var95 <- h$var95 + b$var95 + o$var95
  total_var99 <- h$var99 + b$var99 + o$var99
  
  tibble(
    Scenario      = label,
    Freq_Mult     = freq_m,
    Sev_Mult      = sev_m,
    Raw_Loss_Mean = round(total_mean, 0),
    Raw_VaR95     = round(total_var95, 0),
    Raw_VaR99     = round(total_var99, 0),
    vs_Base_pct   = round(total_mean / s_t$E * 100 - 100, 1),
    ST_Loss       = round(td_ST(total_mean), 0),
    ST_NetRev     = round(ST_net_rev(total_mean), 0),
    ST_LossRatio  = round(td_ST(total_mean) / ST_prem_t * 100, 1),
    LT_Loss       = round(td_LT(total_mean), 0),
    LT_NetRev     = round(LT_net_rev(total_mean), 0),
    LT_LossRatio  = round(td_LT(total_mean) / LT_prem_t * 100, 1),
    Profitable_ST = ifelse(ST_net_rev(total_mean) > 0, "YES", "NO"),
    Profitable_LT = ifelse(LT_net_rev(total_mean) > 0, "YES", "NO")
  )
}

scenarios <- bind_rows(
  run_scenario(0.85, 0.90,
               "BEST: Smooth operations, attritional claims only"),
  run_scenario(1.05, 1.05,
               "MODERATE: Mild frontier strain, Oryn Delta expansion"),
  run_scenario(1.40, 1.35,
               "WORST: Catastrophic correlated event")
)

# --- Print scenario results ---

cat("\n================================================\n")
cat("SCENARIO RESULTS - SHORT TERM\n")
cat("Base premium: Đ",
    format(round(ST_prem_t, 0), big.mark = ","), "\n")
cat("================================================\n")
scenarios %>%
  dplyr::select(Scenario, Freq_Mult, Sev_Mult,
                ST_Loss, ST_NetRev, ST_LossRatio, Profitable_ST) %>%
  print()

cat("\n================================================\n")
cat("SCENARIO RESULTS - LONG TERM\n")
cat("Base premium: Đ",
    format(round(LT_prem_t, 0), big.mark = ","), "\n")
cat("================================================\n")
scenarios %>%
  dplyr::select(Scenario, Freq_Mult, Sev_Mult,
                LT_Loss, LT_NetRev, LT_LossRatio, Profitable_LT) %>%
  print()

# --- Scenario breakdown per system ---

cat("\n================================================\n")
cat("SCENARIO BREAKDOWN - LOSS PER SYSTEM (SHORT TERM)\n")
cat("================================================\n")

breakdown_by_system <- function(freq_m, sev_m, label) {
  h <- simulate_stressed(helionis_cq, freq_m, sev_m, sigma_sev)
  b <- simulate_stressed(bayesia_cq,  freq_m, sev_m, sigma_sev)
  o <- simulate_stressed(oryn_cq,     freq_m, sev_m, sigma_sev)
  tibble(
    Scenario  = label,
    System    = c("Helionis", "Bayesia", "Oryn Delta"),
    ST_Loss   = round(c(td_ST(h$mean),
                        td_ST(b$mean),
                        td_ST(o$mean)), 0),
    Share_pct = round(c(h$mean, b$mean, o$mean) /
                        (h$mean + b$mean + o$mean) * 100, 1)
  )
}

bind_rows(
  breakdown_by_system(0.85, 0.90, "BEST"),
  breakdown_by_system(1.05, 1.05, "MODERATE"),
  breakdown_by_system(1.40, 1.35, "WORST")
) %>% print()

# ============================================================
# PART B: CORRELATED RISK SCENARIOS
# Multiple systems hit simultaneously with different
# multipliers reflecting each system's specific exposure
# ============================================================
#
# Radiation Event - Bayesia + Oryn Delta
#   Bayesia's binary star and Oryn Delta's dwarf star are
#   different mechanisms but both produce ionising radiation.
#   During an active solar period both can trigger together.
#   Helionis orbits a stable low-flare G2 star - unaffected.
#   Bayesia: freq x1.30, sev x1.35 (binary star EM spike,
#            thin magnetosphere, acute radiation exposure)
#   Oryn:    freq x1.25, sev x1.30 (dwarf star sporadic
#            flare, low visibility worsens response)
#   Helionis: base (stable star, no radiation risk)
#
# Debris Event - Helionis + Oryn Delta
#   Both systems have documented asteroid and debris hazards.
#   Helionis has erratic outer-system clusters; Oryn Delta
#   has the unstable asymmetric ring. A large fragmentation
#   cascade worsens conditions in both simultaneously.
#   Bayesia's asteroid belt is heavily mapped with stable
#   orbits - unaffected.
#   Helionis: freq x1.25, sev x1.20 (cluster fragmentation
#             sends debris into mining zones)
#   Oryn:     freq x1.35, sev x1.25 (ring is inherently less
#             stable and more reactive to perturbation)
#   Bayesia:  base (mapped, stable orbits)
#
# Communications Blackout - All Systems
#   The most important WC correlated event because it ONLY
#   raises severity, not frequency - a blackout does not
#   cause more accidents but makes every accident more
#   expensive. Helionis relay satellites go offline after
#   fragmentation events. Oryn Delta comms are already
#   inconsistent. Delayed triage, delayed evacuation, and
#   delayed surgery turn moderate injuries into serious
#   claims across all three systems at once.
#   All systems: freq x1.00, sev x1.20
# ============================================================

cat("\n================================================\n")
cat("PART B: CORRELATED RISK SCENARIOS\n")
cat("================================================\n")
cat("\nRunning 100,000 simulations per correlated scenario...\n")

set.seed(77)

run_correlated <- function(h_fm, h_sm, b_fm, b_sm,
                           o_fm, o_sm, label) {
  cat(" Running:", label, "\n")
  h_res  <- simulate_stressed(helionis_cq, h_fm, h_sm, sigma_sev)
  b_res  <- simulate_stressed(bayesia_cq,  b_fm, b_sm, sigma_sev)
  o_res  <- simulate_stressed(oryn_cq,     o_fm, o_sm, sigma_sev)
  total  <- h_res$mean + b_res$mean + o_res$mean
  tibble(
    Scenario      = label,
    Raw_Loss      = round(total, 0),
    vs_Base_pct   = round(total / s_t$E * 100 - 100, 1),
    ST_Loss       = round(td_ST(total), 0),
    ST_NetRev     = round(ST_net_rev(total), 0),
    ST_LossRatio  = round(td_ST(total) / ST_prem_t * 100, 1),
    LT_Loss       = round(td_LT(total), 0),
    LT_NetRev     = round(LT_net_rev(total), 0),
    LT_LossRatio  = round(td_LT(total) / LT_prem_t * 100, 1),
    Profitable_ST = ifelse(ST_net_rev(total) > 0, "YES", "NO"),
    Profitable_LT = ifelse(LT_net_rev(total) > 0, "YES", "NO")
  )
}

correlated <- bind_rows(
  run_correlated(
    h_fm = 1.00, h_sm = 1.00,
    b_fm = 1.30, b_sm = 1.35,
    o_fm = 1.25, o_sm = 1.30,
    label = "Radiation event - Bayesia + Oryn Delta"
  ),
  run_correlated(
    h_fm = 1.25, h_sm = 1.20,
    b_fm = 1.00, b_sm = 1.00,
    o_fm = 1.35, o_sm = 1.25,
    label = "Debris event - Helionis + Oryn Delta"
  ),
  run_correlated(
    h_fm = 1.00, h_sm = 1.20,
    b_fm = 1.00, b_sm = 1.20,
    o_fm = 1.00, o_sm = 1.20,
    label = "Comms blackout - all systems"
  )
)

cat("\n--- Correlated risk - Short term ---\n")
correlated %>%
  dplyr::select(Scenario, ST_Loss, ST_NetRev,
                ST_LossRatio, Profitable_ST) %>%
  print()

cat("\n--- Correlated risk - Long term ---\n")
correlated %>%
  dplyr::select(Scenario, LT_Loss, LT_NetRev,
                LT_LossRatio, Profitable_LT) %>%
  print()

# ============================================================
# REFERENCE TABLES
# ============================================================

cat("\n================================================\n")
cat("SCENARIO MULTIPLIER REFERENCE\n")
cat("================================================\n")
data.frame(
  Scenario    = c("Best case",
                  "Moderate case",
                  "Worst case"),
  Freq_Mult   = c(0.85, 1.05, 1.40),
  Sev_Mult    = c(0.90, 1.05, 1.35),
  Description = c(
    "Smooth ops, strong safety compliance, stable comms",
    "Mild frontier strain, Oryn Delta expansion activity",
    "Severe disruption, radiation event, delayed emergency response"
  ),
  Rationale   = c(
    "Fewer incidents + faster response = lower cost per claim",
    "Modest volume and cost increase from frontier operational pressure",
    "Realistic for WC - strong but not mass-casualty extreme"
  )
) %>% print()

cat("\n================================================\n")
cat("CORRELATED RISK MULTIPLIER REFERENCE\n")
cat("================================================\n")
data.frame(
  Event       = c("Radiation event",
                  "Debris event",
                  "Comms blackout"),
  Systems     = c("Bayesia + Oryn Delta",
                  "Helionis + Oryn Delta",
                  "All systems"),
  Freq_Impact = c("B x1.30, O x1.25",
                  "H x1.25, O x1.35",
                  "All x1.00"),
  Sev_Impact  = c("B x1.35, O x1.30",
                  "H x1.20, O x1.25",
                  "All x1.20"),
  Mechanism   = c(
    "Binary star flares + dwarf star flares trigger together",
    "Asteroid cluster fragmentation + asymmetric ring shear",
    "Satellite relay failure delays rescue and triage everywhere"
  )
) %>% print()
cat("================================================\n")



