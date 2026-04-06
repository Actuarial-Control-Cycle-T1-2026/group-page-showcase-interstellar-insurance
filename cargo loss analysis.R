install.packages("readxl", type = "source")
install.packages("dplyr")
install.packages("fitdistrplus")
install.packages("goftest")
install.packages("pscl")
install.packages("statmod")

library(readxl)
library(dplyr)
library(fitdistrplus)
library(goftest)
library(pscl)
library(statmod)

excel_sheets("/Users/maggie/Documents/UNSW/ACTL4001/srcsc-2026-claims-cargo.xlsx")

rawdata_freq <- read_excel("/Users/maggie/Documents/UNSW/ACTL4001/srcsc-2026-claims-cargo.xlsx", sheet = "freq")
rawdata_sev <- read_excel("/Users/maggie/Documents/UNSW/ACTL4001/srcsc-2026-claims-cargo.xlsx", sheet = "sev")

par(mfrow=c(1,1))

#######################
#### Data Cleaning ####
#######################

# Check and clean NA data
colSums(is.na(rawdata_freq))
colSums(is.na(rawdata_sev))

data_freq <- na.omit(rawdata_freq)
data_sev <- na.omit(rawdata_sev)

    nrow(data_freq)
    nrow(data_sev)
    nrow(rawdata_freq)
    nrow(rawdata_sev)

# Filter frequency data to align with data dictionary
data_freq <- data_freq %>%
  filter( between(cargo_value, 50e3, 680000e3),
          between(weight, 1500, 250e3),
          route_risk %in% c(1,2,3,4,5),
          between(distance, 1, 100), 
          between(transit_duration, 1, 60), 
          between(pilot_experience, 1, 30), 
          between(vessel_age, 1, 50), 
          between(solar_radiation, 0, 1), 
          between(debris_density, 0, 1), 
          between(exposure, 0, 1), 
          between(claim_count, 0, 5)
          )


# Filter severity data to align with data dictionary
data_sev <- data_sev %>%
  filter( between(cargo_value, 50e3, 680000e3),
          between(weight, 1500, 250e3),
          route_risk %in% c(1,2,3,4,5),
          between(distance, 1, 100), 
          between(transit_duration, 1, 60), 
          between(pilot_experience, 1, 30), 
          between(vessel_age, 1, 50), 
          between(solar_radiation, 0, 1), 
          between(debris_density, 0, 1), 
          between(exposure, 0, 1), 
          between(claim_amount, 31e3, 678000e3)
  )

min(data_sev$claim_amount)
max(data_sev$claim_amount)


# Clean up data cargo_type column
data_freq$cargo_type <- sub("_.*", "", data_freq$cargo_type)
data_sev$cargo_type <- sub("_.*", "", data_sev$cargo_type)

data_freq$container_type <- sub("_.*", "", data_freq$container_type)
data_sev$container_type <- sub("_.*", "", data_sev$container_type)


# Claim Freq Analysis
  ## FREQUENCY = claim count / exposure
data_freq$claim_freq <- data_freq$claim_count/data_freq$exposure

mean(data_freq$exposure)

min(data_freq$claim_count)
max(data_freq$claim_count)
hist(data_freq$claim_count, 
     breaks = 5, 
     main = "Distribution of Historical Claim Counts", 
     xlab = "Claim Count", 
     ylim = c(0,1),
     prob = T)

min(data_freq$claim_freq)
max(data_freq$claim_freq)
hist(data_freq$claim_freq, 
     breaks = 60,
     main = "Distribution of Historical Claim Frequency", 
     xlab = "Claim Count per Exposure", 
     ylim = c(0,1),
     prob = T)

freq_mean <- mean(data_freq$claim_freq)
freq_var <- var(data_freq$claim_freq)
freq_sd <- sd(data_freq$claim_freq)
freq_VaR95 <- quantile(data_freq$claim_freq, 0.95)
freq_ES95 <- mean(data_freq$claim_freq[data_freq$claim_freq > freq_VaR95])



### Cargo Type Analysis ###

cargo_type_analysis <- data_freq[, c("cargo_type", "cargo_value", "weight")]
cargo_type_analysis$value_per_weight <- cargo_type_analysis$cargo_value / cargo_type_analysis$weight

cargo_type_analysis %>%
  group_by(cargo_type) %>%
  summarise(
    mean_value_per_weight = mean(value_per_weight, na.rm = TRUE),
    median_value_per_weight = median(value_per_weight, na.rm = TRUE),
    sd_value_per_weight = sd(value_per_weight, na.rm = TRUE),
    n = n(), 
    percentage = n/nrow(cargo_type_analysis))
  # noting that platinum and gold have a much higher value per weight unit

cargo_type_freq_analysis <- data_freq %>%
  group_by(cargo_type) %>%
  summarise(
    mean_claims = mean(claim_count),
    mean_freq = mean(claim_freq)
  )

cargo_type_sev_analysis <- data_sev %>%
  group_by(cargo_type) %>%
  summarise(
    mean_sev = mean(claim_amount),
    n = n(), 
    percentage = n/nrow(data_sev)
  )

    
#################################
#### Frequency Data Analysis ####
#################################

# Claim Count Analysis
count_mean <- mean(data_freq$claim_count)
count_var <- var(data_freq$claim_count)
count_VaR95 <- quantile(data_freq$claim_count, 0.95)
count_ES95 <- mean(data_freq$claim_count[data_freq$claim_count > count_VaR95])

barplot(table(data_freq$claim_count),
        main = "Cargo Loss Claim Count Distribution",
        xlab = "Claim Count",
        ylab = "Frequency")

max(data_freq$claim_count)

data_freq %>%
  group_by(cargo_type) %>%
  summarise(avg_claim_count = mean(claim_count, na.rm = TRUE),
            var_claim_count = var(claim_count, na.rm = TRUE))

data_freq %>%
  group_by(container_type) %>%
  summarise(avg_claim_count = mean(claim_count, na.rm = TRUE),
            var_claim_count = var(claim_count, na.rm = TRUE))

data_freq %>%
  group_by(cargo_type) %>%
  summarise(avg_claim_count = mean(claim_freq, na.rm = TRUE),
            var_claim_count = var(claim_freq, na.rm = TRUE))

data_freq %>%
  group_by(container_type) %>%
  summarise(avg_claim_count = mean(claim_freq, na.rm = TRUE),
            var_claim_count = var(claim_freq, na.rm = TRUE))


### Container Type Analysis ###

container_type_analysis <- data_freq
container_type_analysis$claim_freq <- container_type_analysis$claim_count / container_type_analysis$exposure

container_type_analysis %>%
  group_by(container_type) %>%
  summarise(
    mean = mean(claim_freq, na.rm = TRUE),
    median = median(claim_freq, na.rm = TRUE),
    sd = sd(claim_freq, na.rm = TRUE),
    n = n(), 
    percentage = n / nrow(container_type_analysis))

            container_type_analysis2 <- data_sev
            container_type_analysis2 %>%
              group_by(container_type) %>%
              summarise(
                mean = mean(claim_amount, na.rm = TRUE),
                median = median(claim_amount, na.rm = TRUE),
                sd = sd(claim_amount, na.rm = TRUE),
                n = n(), 
                percentage = n / nrow(container_type_analysis2))



crate_data <- container_type_analysis %>%
  filter(container_type%in% c("HardSeal Transit Crate", "QuantumCrate Module"))

non_crate_data <- container_type_analysis %>%
  filter(!container_type%in% c("HardSeal Transit Crate", "QuantumCrate Module"))

hist(crate_data$claim_count,
     main = "Cargo Loss Claim Count for Crate Containers",
     xlab = "Claim Count",
     ylab = "Frequency", 
     prob = T, 
     breaks = 5, 
     ylim = c(0, 1))

hist(non_crate_data$claim_count,
     main = "Cargo Loss Claim Count for Non-Crate Containers",
     xlab = "Claim Count",
     ylab = "Frequency", 
     prob = T, 
     breaks = 5,
     ylim = c(0, 1))

hist(crate_data$claim_freq,
     main = "Cargo Loss Claim Freq for Crate Containers",
     xlab = "Claim Count",
     ylab = "Frequency", 
     prob = T, 
     breaks = 5,
     ylim = c(0, 0.1))

hist(non_crate_data$claim_freq,
     main = "Cargo Loss Claim Freq for Non-Crate Containers",
     xlab = "Claim Count",
     ylab = "Frequency", 
     prob = T, 
     breaks = 5,
     ylim = c(0, 0.1))

  # noting that despite the crates having a marginally higher mean and s.d, and significance in the GLM, there is not discernible difference in claim distribution
  # thus choosing to NOT model crate containers separately



#########################
##### Frequency GLM #####
#########################

# Initial basic GLM models
claim_count_model_poisson <- glm(claim_count ~ . - policy_id - shipment_id - claim_freq - exposure, data = data_freq, family = poisson(link="log"), offset = log(exposure))
summary(claim_count_model_poisson)

claim_count_model_nb <- glm.nb(claim_count ~ . - policy_id - shipment_id - claim_freq - exposure + offset(log(exposure)), data = data_freq)
summary(claim_count_model_nb)

# Reduced negative binomial GLM 
claim_count_model_nb_test <- glm.nb(claim_count ~ cargo_value + container_type + route_risk + pilot_experience + offset(log(exposure)), data = data_freq)
summary(claim_count_model_nb_test)

reduced_claim_count_model_nb <- glm.nb(claim_count ~ route_risk + pilot_experience + offset(log(exposure)), data = data_freq)
summary(reduced_claim_count_model_nb)
# note that AIC is marginally lower without cargo_value as an input variable



### SPLIT BY CONTAINER TYPE ### 

freq_container_data_deepspace <- data_freq %>%
  filter(container_type%in% c("DeepSpace Haulbox"))

freq_container_data_dockarc <- data_freq %>%
  filter(container_type%in% c("DockArc Freight Case"))

freq_container_data_hardseal <- data_freq %>%
  filter(container_type%in% c("HardSeal Transit Crate"))

freq_container_data_longhaul <- data_freq %>%
  filter(container_type%in% c("LongHaul Vault Canister"))

freq_container_data_quantumcrate <- data_freq %>%
  filter(container_type%in% c("QuantumCrate Module"))


data_freq %>%
  group_by(container_type) %>%
  summarise(mean = mean(claim_freq),
            var = var(claim_freq), 
            VaR_95 = quantile(claim_freq, 0.95), 
            ES_95 = mean(claim_freq[claim_freq > quantile(claim_freq, 0.95)]))

hist(freq_container_data_deepspace$claim_freq,
     main = "Claim Freq Deepspace Containers",
     xlab = "Claim Count",
     ylab = "Frequency", 
     prob = T, 
     breaks = 5,
     ylim = c(0, 0.1))

hist(freq_container_data_dockarc$claim_freq,
     main = "Claim Freq Dock Arc Containers",
     xlab = "Claim Count",
     ylab = "Frequency", 
     prob = T, 
     breaks = 5,
     ylim = c(0, 0.1))

hist(freq_container_data_hardseal$claim_freq,
     main = "Claim Freq Hard Seal Containers",
     xlab = "Claim Count",
     ylab = "Frequency", 
     prob = T, 
     breaks = 5,
     ylim = c(0, 0.1))

hist(freq_container_data_longhaul$claim_freq,
     main = "Claim Freq Long Haul Containers",
     xlab = "Claim Count",
     ylab = "Frequency", 
     prob = T, 
     breaks = 5,
     ylim = c(0, 0.1))

hist(freq_container_data_quantumcrate$claim_freq,
     main = "Claim Freq Quantum Crate Containers",
     xlab = "Claim Count",
     ylab = "Frequency", 
     prob = T, 
     breaks = 5,
     ylim = c(0, 0.1))



################################
#### Severity Data Analysis ####
################################

sev_mean <- mean(data_sev$claim_amount)
sev_var <- var(data_sev$claim_amount)
sev_VaR95 <- quantile(data_sev$claim_amount, 0.95)
sev_VaR99 <- quantile(data_sev$claim_amount, 0.99)
sev_ES95 <- mean(data_sev$claim_amount[data_sev$claim_amount > sev_VaR95])
sev_ES99 <- mean(data_sev$claim_amount[data_sev$claim_amount > sev_VaR99])


hist(data_sev$claim_amount, 
     breaks = 100, 
     xlab = "Claim Amount", 
     main = "Histogram of Claim Amounts", 
     prob = T)

max(data_sev$claim_amount)

      cor(data_sev$cargo_value, data_sev$claim_amount)
      cor(data_sev$route_risk, data_sev$claim_amount)
      cor(data_sev$exposure, data_sev$claim_amount)
      cor(data_sev$distance, data_sev$claim_amount)
      cor(data_sev$transit_duration, data_sev$claim_amount)
      cor(data_sev$pilot_experience, data_sev$claim_amount)
      cor(data_sev$vessel_age, data_sev$claim_amount)


data_sev %>%
  group_by(cargo_type) %>%
  summarise(avg_claim_amount = mean(claim_amount, na.rm = TRUE),
            var_claim_amount = var(claim_amount, na.rm = TRUE),
            VaR95_claim_amount = quantile(claim_amount, 0.95),
            VaR99_claim_amount = quantile(claim_amount, 0.99))


### Fitting Loss Distributions ###

# Log-normal distribution 
  claimamount <- data_sev$claim_amount
  log_claimamount <- log(claimamount) 
  
  LN_muhat  <- mean(log_claimamount)
  LN_sigmahat <- sd(log_claimamount)
  
  hist(claimamount,
       breaks = 100,
       probability = TRUE)
  xgrid <- seq(min(claimamount),
               max(claimamount),
               length = 1000)
  lines(xgrid,
        dlnorm(xgrid, meanlog = LN_muhat, sdlog = LN_sigmahat),
        col = "green",
        lwd = 2)


# Gamma
  gamma_pdf <- function(x, gammaalpha, gammalambda) {  
    ((gammalambda^gammaalpha)*(x^(gammaalpha-1))*(exp(-1*gammalambda*x)))/gamma(gammaalpha)}
  
  negLLF_gamma <- function(parameters, claimamount) {
    gammaalpha <- parameters[1]
    gammalambda <- parameters[2]
    -sum(log(gamma_pdf(claimamount, gammaalpha, gammalambda)))}
  
  # initial values set as method of moments estimates 
  gammalambda <- (length(claimamount)*sum(claimamount))/(length(claimamount)*sum(claimamount^2)-(sum(claimamount)^2))
  gammaalpha <- gammalambda*sum(claimamount)/length(claimamount)
  gamma_mle <- optim(par=c(gammaalpha, gammalambda), fn=negLLF_gamma, claimamount=claimamount)
  
  # extract the MLE estimates
  gamma_alphahat <- gamma_mle$par[1]
  gamma_lambdahat <- gamma_mle$par[2]
  
  gamma_parameters <- c(gamma_alphahat, gamma_lambdahat)
  gamma_parameters
  
  hist(claimamount,
       breaks = 100,
       probability = TRUE)
  xgrid<-seq(min(claimamount),max(claimamount),length=length(claimamount)) 
  lines(xgrid, gamma_pdf(xgrid, gamma_alphahat, gamma_lambdahat),
        col="blue", 
        lwd=1) 


# Pareto
  pareto_pdf <- function(x, paretoalpha, paretolambda) {  
    (paretoalpha*(paretolambda^paretoalpha))/(paretolambda+x)^(paretoalpha+1)}
  
  LLF_pareto <- function(claimamount, pareto_alphahat, pareto_lambdahat) {
    sum(log(pareto_pdf(claimamount, pareto_alphahat, pareto_lambdahat)))}
  
  negLLF_pareto <- function(parameters, claimamount) {
    paretoalpha <- parameters[1]
    paretolambda <- parameters[2]
    -sum(log(pareto_pdf(claimamount, paretoalpha, paretolambda)))}
  
  # initial values set as alpha, lambda > 0
  paretolambda <- min(claimamount)
  paretoalpha <- length(claimamount) / sum(log(claimamount / min(claimamount)))
  
  
  pareto_mle <- optim(par=c(paretoalpha, paretolambda), fn=negLLF_pareto, claimamount=claimamount)
  
  # extract the MLE estimates
  pareto_alphahat <- pareto_mle$par[1]
  pareto_lambdahat <- pareto_mle$par[2]
  
  pareto_parameters <- c(pareto_alphahat, pareto_lambdahat)
  pareto_parameters 

  
# Goodness of fit plots
hist(claimamount,
     breaks = 100,
     probability = TRUE, 
     xlab = "Claim Amount", 
     ylab = "Frequency", 
     main = "Goodness of Fit Distributions")

xgrid<-seq(min(claimamount),max(claimamount),length=length(claimamount)) 

lines(xgrid, dlnorm(xgrid, meanlog = LN_muhat, sdlog = LN_sigmahat),
      col = "green",
      lwd = 2)

lines(xgrid, gamma_pdf(xgrid, gamma_alphahat, gamma_lambdahat),
      col="blue", 
      lwd=1) 

lines(xgrid, pareto_pdf(xgrid, pareto_alphahat, pareto_lambdahat),
      col="red", 
      lwd=1) 

legend("topright", 
       legend=c("Log-normal", "Gamma", "Pareto"),
       col=c("green", "blue", "red"), 
       lwd=1, 
       cex=0.8)

  # Log-normal appears to be the most suitable distribution


###############################
### Log-normal distribution ###
###############################

claim_amount <- data_sev$claim_amount
log_claim_amount <- log(claim_amount) 
hist(log_claim_amount, 
     xlab = "Log Claim Amount", 
     ylab = "Frequency", 
     main = "Histogram of Log Claim Amounts", 
     breaks = 50, 
     prob = F)

# Two peaks are noticed, which does not follow a clear normal distribution
# Given correlation to cargo value, suggests that the large values of gold and platinum may be causing second peak

### Cargo Type Analysis ###

cargo_type_claim_analysis <- data_sev

cargo_type_claim_analysis %>%
  group_by(cargo_type) %>%
  summarise(
    mean = mean(claim_amount, na.rm = TRUE),
    median = median(claim_amount, na.rm = TRUE),
    sd = sd(claim_amount, na.rm = TRUE),
    n = n(), 
    percentage = n/nrow(cargo_type_claim_analysis))

cargo_type_claim_analysis %>%
  group_by(cargo_type, container_type) %>%
  summarise(
    mean = mean(claim_amount, na.rm = TRUE),
    median = median(claim_amount, na.rm = TRUE),
    sd = sd(claim_amount, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) %>%
  group_by(container_type) %>%
  mutate(percentage = n / sum(n)) %>%
  print(n=Inf)

gold_data <- cargo_type_claim_analysis %>%
  filter(cargo_type%in% c("gold"))

platinum_data <- cargo_type_claim_analysis %>%
  filter(cargo_type%in% c("platinum"))

non_splitcargo_data <- cargo_type_claim_analysis %>%
  filter(!cargo_type%in% c("gold", "platinum"))

gold_weight <- nrow(gold_data) / nrow(cargo_type_claim_analysis)
platinum_weight <- nrow(platinum_data) / nrow(cargo_type_claim_analysis)
other_weight <- nrow(non_splitcargo_data) / nrow(cargo_type_claim_analysis)

# Log Claim Distributions
gold_data$log_claim_amount <- log(gold_data$claim_amount) 
platinum_data$log_claim_amount <- log(platinum_data$claim_amount) 
non_splitcargo_data$log_claim_amount <- log(non_splitcargo_data$claim_amount) 

gold_mean <- mean(gold_data$log_claim_amount)
gold_sd <- sd(gold_data$log_claim_amount)
hist(gold_data$log_claim_amount,
     main = "Cargo Loss Log Claim Amounts for Gold Cargo",
     xlab = "Claim Amount",
     ylab = "Frequency", 
     prob = T, 
     breaks = 50) 
xgrid <- seq(min(gold_data$log_claim_amount), max(gold_data$log_claim_amount), length = 1000)
lines(xgrid, dnorm(xgrid, mean = gold_mean, sd = gold_sd), 
      col = "red")

platinum_mean <- mean(platinum_data$log_claim_amount)
platinum_sd <- sd(platinum_data$log_claim_amount)
hist(platinum_data$log_claim_amount,
     main = "Cargo Loss Log Claim Amounts for Platinum Cargo",
     xlab = "Claim Amount",
     ylab = "Frequency", 
     prob = T, 
     breaks = 50) 
xgrid <- seq(min(platinum_data$log_claim_amount), max(platinum_data$log_claim_amount), length = 1000)
lines(xgrid, dnorm(xgrid, mean = platinum_mean, sd = platinum_sd), 
      col = "blue")

other_mean <- mean(non_splitcargo_data$log_claim_amount)
other_sd <- sd(non_splitcargo_data$log_claim_amount)
hist(non_splitcargo_data$log_claim_amount,
     main = "Cargo Loss Log Claim Amounts for Other Cargo",
     xlab = "Claim Amount",
     ylab = "Frequency", 
     prob = T, 
     breaks = 50) 
xgrid <- seq(min(non_splitcargo_data$log_claim_amount), max(non_splitcargo_data$log_claim_amount), length = 1000)
lines(xgrid, dnorm(xgrid, mean = other_mean, sd = other_sd), 
      col = "green")


# Claim Distributions
hist(gold_data$claim_amount,
     main = "Cargo Loss Claim Amounts for Gold Cargo",
     xlab = "Claim Amount",
     ylab = "Frequency", 
     prob = T, 
     breaks = 100) 
xgrid <- seq(min(gold_data$claim_amount), max(gold_data$claim_amount), length = 1000)
lines(xgrid, dlnorm(xgrid, meanlog = gold_mean, sdlog = gold_sd),
      col = "red")

hist(platinum_data$claim_amount,
     main = "Cargo Loss Claim Amounts for Platinum Cargo",
     xlab = "Claim Amount",
     ylab = "Frequency", 
     prob = T, 
     breaks = 100) 
xgrid <- seq(min(platinum_data$claim_amount), max(platinum_data$claim_amount), length = 1000)
lines(xgrid, dlnorm(xgrid, meanlog = platinum_mean, sdlog = platinum_sd),
      col = "blue")

hist(non_splitcargo_data$claim_amount,
     main = "Cargo Loss Claim Amounts for Other Cargo",
     xlab = "Claim Amount",
     ylab = "Frequency", 
     prob = T, 
     breaks = 200) 
xgrid <- seq(min(non_splitcargo_data$claim_amount), max(non_splitcargo_data$claim_amount), length = 1000)
lines(xgrid, dlnorm(xgrid, meanlog = other_mean, sdlog = other_sd),
      col = "green")



#########################
##### Severity GLM ######
#########################
par(mfrow=c(2,2))

## GAMMA 
claim_sev_model_gamma <- glm(claim_amount ~ . -claim_id - policy_id - shipment_id - exposure, data = data_sev, family=Gamma(link="log"))
summary(claim_sev_model_gamma)

## LOG-NORMAL

claim_sev_ln_data <- data_sev
claim_sev_ln_data$log_claim <- log(claim_sev_ln_data$claim_amount)

claim_sev_model_ln <- lm(log_claim ~ . -claim_id - policy_id - shipment_id - exposure - claim_amount, data = claim_sev_ln_data)
summary(claim_sev_model_ln)
AIC(claim_sev_model_ln)

claim_sev_reduced_model_ln <- lm(log_claim ~ cargo_type + cargo_value + weight + route_risk + solar_radiation + debris_density, data = claim_sev_ln_data)
summary(claim_sev_reduced_model_ln)
AIC(claim_sev_reduced_model_ln)
plot(claim_sev_reduced_model_ln)


## SPLIT BY CARGO TYPE

# GOLD
gold_sev_full_model <- lm( log_claim_amount ~ claim_seq + cargo_value + weight + route_risk + distance + transit_duration 
                           + pilot_experience + vessel_age + container_type + solar_radiation + debris_density, data = gold_data)
summary(gold_sev_full_model)
AIC(gold_sev_full_model)

gold_sev_reduced_model <- lm(log_claim_amount ~ cargo_value + route_risk + solar_radiation + container_type, data = gold_data)
summary(gold_sev_reduced_model)
AIC(gold_sev_reduced_model)

        ### SPLIT BY CONTAINER TYPE ### 
        
        gold_container_data_deepspace <- gold_data %>%
          filter(container_type%in% c("DeepSpace Haulbox"))
        
        gold_container_data_dockarc <- gold_data %>%
          filter(container_type%in% c("DockArc Freight Case"))
        
        gold_container_data_hardseal <- gold_data %>%
          filter(container_type%in% c("HardSeal Transit Crate"))
        
        gold_container_data_longhaul <- gold_data %>%
          filter(container_type%in% c("LongHaul Vault Canister"))
        
        gold_container_data_quantumcrate <- gold_data %>%
          filter(container_type%in% c("QuantumCrate Module"))
        
        hist(gold_container_data_deepspace$claim_amount, prob = T)
        hist(gold_container_data_dockarc$claim_amount, prob = T)
        hist(gold_container_data_hardseal$claim_amount, prob = T)
        hist(gold_container_data_longhaul$claim_amount, prob = T)
        hist(gold_container_data_quantumcrate$claim_amount, prob = T)
        
        
        # gold deepspace model
        gold_deepspace_sev_full_model <- lm( log_claim_amount ~ claim_seq + cargo_value + weight + route_risk + distance + transit_duration 
                                   + pilot_experience + vessel_age + solar_radiation + debris_density, data = gold_container_data_deepspace)
        summary(gold_deepspace_sev_full_model)
        AIC(gold_deepspace_sev_full_model)
        
        
        gold_deepspace_sev_reduced_model <- lm( log_claim_amount ~ cargo_value + route_risk + solar_radiation, data = gold_container_data_deepspace)
        summary(gold_deepspace_sev_reduced_model)
        AIC(gold_deepspace_sev_reduced_model)
        plot(gold_deepspace_sev_reduced_model)
        
        # gold dockarc model
        gold_dockarc_sev_full_model <- lm( log_claim_amount ~ claim_seq + cargo_value + weight + route_risk + distance + transit_duration 
                                             + pilot_experience + vessel_age + solar_radiation + debris_density, data = gold_container_data_dockarc)
        summary(gold_dockarc_sev_full_model)
        AIC(gold_dockarc_sev_full_model)
        plot(gold_dockarc_sev_full_model)
        
        
        gold_dockarc_sev_reduced_model <- lm( log_claim_amount ~ cargo_value + route_risk, data = gold_container_data_dockarc)
        summary(gold_dockarc_sev_reduced_model)
        AIC(gold_dockarc_sev_reduced_model)
        plot(gold_dockarc_sev_reduced_model)
        
        
        # gold hardseal model
        gold_hardseal_sev_full_model <- lm( log_claim_amount ~ claim_seq + cargo_value + weight + route_risk + distance + transit_duration 
                                           + pilot_experience + vessel_age + solar_radiation + debris_density, data = gold_container_data_hardseal)
        summary(gold_hardseal_sev_full_model)
        AIC(gold_hardseal_sev_full_model)
        
        
        gold_hardseal_sev_reduced_model <- lm( log_claim_amount ~ cargo_value + route_risk + transit_duration + solar_radiation, data = gold_container_data_hardseal)
        summary(gold_hardseal_sev_reduced_model)
        AIC(gold_hardseal_sev_reduced_model)
        plot(gold_hardseal_sev_reduced_model)
        
        # gold longhaul model
        gold_longhaul_sev_full_model <- lm( log_claim_amount ~ claim_seq + cargo_value + weight + route_risk + distance + transit_duration 
                                            + pilot_experience + vessel_age + solar_radiation + debris_density, data = gold_container_data_longhaul)
        summary(gold_longhaul_sev_full_model)
        AIC(gold_longhaul_sev_full_model)
        
        
        gold_longhaul_sev_reduced_model <- lm( log_claim_amount ~ cargo_value + route_risk + weight + solar_radiation, data = gold_container_data_longhaul)
        summary(gold_longhaul_sev_reduced_model)
        AIC(gold_longhaul_sev_reduced_model)
        plot(gold_longhaul_sev_reduced_model)
        
        # gold quantumcrate model
        gold_quantumcrate_sev_full_model <- lm( log_claim_amount ~ claim_seq + cargo_value + weight + route_risk + distance + transit_duration 
                                            + pilot_experience + vessel_age + solar_radiation + debris_density, data = gold_container_data_quantumcrate)
        summary(gold_quantumcrate_sev_full_model)
        AIC(gold_quantumcrate_sev_full_model)
        
        
        gold_quantumcrate_sev_reduced_model <- lm( log_claim_amount ~ cargo_value + route_risk, data = gold_container_data_quantumcrate)
        summary(gold_quantumcrate_sev_reduced_model)
        AIC(gold_quantumcrate_sev_reduced_model)
        plot(gold_quantumcrate_sev_reduced_model)
        

# PLATINUM
platinum_sev_full_model <- lm( log_claim_amount ~ claim_seq + cargo_value + weight + route_risk + distance + transit_duration 
                           + pilot_experience + vessel_age + container_type + solar_radiation + debris_density, data = platinum_data)
summary(platinum_sev_full_model)
AIC(platinum_sev_full_model)

platinum_sev_reduced_model <- lm(log_claim_amount ~ cargo_value + route_risk + solar_radiation, data = platinum_data)
summary(platinum_sev_reduced_model)
AIC(platinum_sev_reduced_model)

          ### SPLIT BY CONTAINER TYPE ### 
          
          platinum_container_data_deepspace <- platinum_data %>%
            filter(container_type%in% c("DeepSpace Haulbox"))
          
          platinum_container_data_dockarc <- platinum_data %>%
            filter(container_type%in% c("DockArc Freight Case"))
          
          platinum_container_data_hardseal <- platinum_data %>%
            filter(container_type%in% c("HardSeal Transit Crate"))
          
          platinum_container_data_longhaul <- platinum_data %>%
            filter(container_type%in% c("LongHaul Vault Canister"))
          
          platinum_container_data_quantumcrate <- platinum_data %>%
            filter(container_type%in% c("QuantumCrate Module"))
          
          hist(platinum_container_data_deepspace$claim_amount, prob = T)
          hist(platinum_container_data_dockarc$claim_amount, prob = T)
          hist(platinum_container_data_hardseal$claim_amount, prob = T)
          hist(platinum_container_data_longhaul$claim_amount, prob = T)
          hist(platinum_container_data_quantumcrate$claim_amount, prob = T)
          
          
          # platinum deepspace model
          platinum_deepspace_sev_full_model <- lm( log_claim_amount ~ claim_seq + cargo_value + weight + route_risk + distance + transit_duration 
                                               + pilot_experience + vessel_age + solar_radiation + debris_density, data = platinum_container_data_deepspace)
          summary(platinum_deepspace_sev_full_model)
          AIC(platinum_deepspace_sev_full_model)
          
          
          platinum_deepspace_sev_reduced_model <- lm( log_claim_amount ~ claim_seq + cargo_value + route_risk + solar_radiation, data = platinum_container_data_deepspace)
          summary(platinum_deepspace_sev_reduced_model)
          AIC(platinum_deepspace_sev_reduced_model)
          plot(platinum_deepspace_sev_reduced_model)
          
          # platinum dockarc model
          platinum_dockarc_sev_full_model <- lm( log_claim_amount ~ claim_seq + cargo_value + weight + route_risk + distance + transit_duration 
                                             + pilot_experience + vessel_age + solar_radiation + debris_density, data = platinum_container_data_dockarc)
          summary(platinum_dockarc_sev_full_model)
          AIC(platinum_dockarc_sev_full_model)
          
          
          platinum_dockarc_sev_reduced_model <- lm( log_claim_amount ~ weight + route_risk, data = platinum_container_data_dockarc)
          summary(platinum_dockarc_sev_reduced_model)
          AIC(platinum_dockarc_sev_reduced_model)
          plot(platinum_dockarc_sev_reduced_model)
          
          # platinum hardseal model
          platinum_hardseal_sev_full_model <- lm( log_claim_amount ~ claim_seq + cargo_value + weight + route_risk + distance + transit_duration 
                                              + pilot_experience + vessel_age + solar_radiation + debris_density, data = platinum_container_data_hardseal)
          summary(platinum_hardseal_sev_full_model)
          AIC(platinum_hardseal_sev_full_model)
          
          
          platinum_hardseal_sev_reduced_model <- lm( log_claim_amount ~ weight + route_risk, data = platinum_container_data_hardseal)
          summary(platinum_hardseal_sev_reduced_model)
          AIC(platinum_hardseal_sev_full_model)
          plot(platinum_hardseal_sev_full_model)
          
          # platinum longhaul model
          platinum_longhaul_sev_full_model <- lm( log_claim_amount ~ claim_seq + cargo_value + weight + route_risk + distance + transit_duration 
                                              + pilot_experience + vessel_age + solar_radiation + debris_density, data = platinum_container_data_longhaul)
          summary(platinum_longhaul_sev_full_model)
          AIC(platinum_longhaul_sev_full_model)
          
          
          platinum_longhaul_sev_reduced_model <- lm( log_claim_amount ~ cargo_value + route_risk + solar_radiation, data = platinum_container_data_longhaul)
          summary(platinum_longhaul_sev_reduced_model)
          AIC(platinum_longhaul_sev_reduced_model)
          plot(platinum_longhaul_sev_reduced_model)
          
          # platinum quantumcrate model
          platinum_quantumcrate_sev_full_model <- lm( log_claim_amount ~ claim_seq + cargo_value + weight + route_risk + distance + transit_duration 
                                                  + pilot_experience + vessel_age + solar_radiation + debris_density, data = platinum_container_data_quantumcrate)
          summary(platinum_quantumcrate_sev_full_model)
          AIC(platinum_quantumcrate_sev_full_model)
          
          
          platinum_quantumcrate_sev_reduced_model <- lm( log_claim_amount ~ cargo_value + route_risk, data = platinum_container_data_quantumcrate)
          summary(platinum_quantumcrate_sev_reduced_model)
          AIC(platinum_quantumcrate_sev_reduced_model)
          plot(platinum_quantumcrate_sev_reduced_model)

          
          
# OTHER
other_sev_full_model <- lm( log_claim_amount ~ claim_seq + cargo_value + cargo_type + weight + route_risk + distance + transit_duration 
                               + pilot_experience + vessel_age + container_type + solar_radiation + debris_density, data = non_splitcargo_data)
summary(other_sev_full_model)
AIC(other_sev_full_model)

other_sev_reduced_model <- lm(log_claim_amount ~ cargo_value + cargo_type + weight + route_risk + solar_radiation + debris_density, data = non_splitcargo_data)
summary(other_sev_reduced_model)
AIC(other_sev_reduced_model)
plot(other_sev_reduced_model)


          ### SPLIT BY CONTAINER TYPE ### 
          
          other_container_data_deepspace <- non_splitcargo_data %>%
            filter(container_type%in% c("DeepSpace Haulbox"))
          
          other_container_data_dockarc <- non_splitcargo_data %>%
            filter(container_type%in% c("DockArc Freight Case"))
          
          other_container_data_hardseal <- non_splitcargo_data %>%
            filter(container_type%in% c("HardSeal Transit Crate"))
          
          other_container_data_longhaul <- non_splitcargo_data %>%
            filter(container_type%in% c("LongHaul Vault Canister"))
          
          other_container_data_quantumcrate <- non_splitcargo_data %>%
            filter(container_type%in% c("QuantumCrate Module"))
          
          hist(other_container_data_deepspace$claim_amount, prob = T)
          hist(other_container_data_dockarc$claim_amount, prob = T)
          hist(other_container_data_hardseal$claim_amount, prob = T)
          hist(other_container_data_longhaul$claim_amount, prob = T)
          hist(other_container_data_quantumcrate$claim_amount, prob = T)
          
          
          # other deepspace model
          other_deepspace_sev_full_model <- lm( log_claim_amount ~ claim_seq + cargo_type + cargo_value + weight + route_risk + distance + transit_duration 
                                                   + pilot_experience + vessel_age + solar_radiation + debris_density, data = other_container_data_deepspace)
          summary(other_deepspace_sev_full_model)
          AIC(other_deepspace_sev_full_model)
          
          
          other_deepspace_sev_reduced_model <- lm( log_claim_amount ~ cargo_type + cargo_value + weight + route_risk + solar_radiation + debris_density, data = other_container_data_deepspace)
          summary(other_deepspace_sev_reduced_model)
          AIC(other_deepspace_sev_reduced_model)
          plot(other_deepspace_sev_reduced_model)
          
          # other dockarc model
          other_dockarc_sev_full_model <- lm( log_claim_amount ~ claim_seq + cargo_type + cargo_value + weight + route_risk + distance + transit_duration 
                                                 + pilot_experience + vessel_age + solar_radiation + debris_density, data = other_container_data_dockarc)
          summary(other_dockarc_sev_full_model)
          AIC(other_dockarc_sev_full_model)
          
          
          other_dockarc_sev_reduced_model <- lm( log_claim_amount ~ cargo_type + weight + route_risk + solar_radiation + debris_density, data = other_container_data_dockarc)
          summary(other_dockarc_sev_reduced_model)
          AIC(other_dockarc_sev_reduced_model)
          plot(other_dockarc_sev_reduced_model)

          
          # other hardseal model
          other_hardseal_sev_full_model <- lm( log_claim_amount ~ claim_seq + cargo_type + cargo_value + weight + route_risk + distance + transit_duration 
                                                  + pilot_experience + vessel_age + solar_radiation + debris_density, data = other_container_data_hardseal)
          summary(other_hardseal_sev_full_model)
          AIC(other_hardseal_sev_full_model)
          
          
          other_hardseal_sev_reduced_model <- lm( log_claim_amount ~ cargo_type + weight + route_risk + solar_radiation + debris_density, data = other_container_data_hardseal)
          summary(other_hardseal_sev_reduced_model)
          AIC(other_hardseal_sev_reduced_model)
          plot(other_hardseal_sev_reduced_model)
          
          # other longhaul model
          other_longhaul_sev_full_model <- lm( log_claim_amount ~ claim_seq + cargo_type + cargo_value + weight + route_risk + distance + transit_duration 
                                                  + pilot_experience + vessel_age + solar_radiation + debris_density, data = other_container_data_longhaul)
          summary(other_longhaul_sev_full_model)
          AIC(other_longhaul_sev_full_model)
          
          
          other_longhaul_sev_reduced_model <- lm( log_claim_amount ~ cargo_type + cargo_value + weight + route_risk + solar_radiation + debris_density, data = other_container_data_longhaul)
          summary(other_longhaul_sev_reduced_model)
          AIC(other_longhaul_sev_reduced_model)
          plot(other_longhaul_sev_reduced_model)
          
          # other quantumcrate model
          other_quantumcrate_sev_full_model <- lm( log_claim_amount ~ claim_seq + cargo_type + cargo_value + weight + route_risk + distance + transit_duration 
                                                      + pilot_experience + vessel_age + solar_radiation + debris_density, data = other_container_data_quantumcrate)
          summary(other_quantumcrate_sev_full_model)
          AIC(other_quantumcrate_sev_full_model)
          
          
          other_quantumcrate_sev_reduced_model <- lm( log_claim_amount ~ cargo_type + cargo_value + weight + route_risk + distance + solar_radiation + debris_density, data = other_container_data_quantumcrate)
          summary(other_quantumcrate_sev_reduced_model)
          AIC(other_quantumcrate_sev_reduced_model)
          plot(other_quantumcrate_sev_reduced_model)
    





################################
### Expected Loss Simulation ###
################################

# Claim Frequency Parameters
    # freq_mu <- mean(predict(reduced_claim_count_model_nb, type = "response"))   # mean of predicted frequency from dataset 
    # freq_mu <- exp(coef(reduced_claim_count_model_nb)["(Intercept)"])     # alternative method
      freq_mu <- mean(data_freq$claim_freq)                                 # 1.229 --> account for NB model underestimating claims

freq_theta <- reduced_claim_count_model_nb$theta

# Adjust claim frequency parameters for risk exposure
freq_mu_sim_factor <- 1.013014545 # calc in Excel
freq_mu_sim_factor <- 1.037722217 # includes container adjustment
freq_mu_sim <- freq_mu * freq_mu_sim_factor

freq_theta_sim_factor <- 1
freq_theta_sim <- freq_theta * freq_theta_sim_factor



# Severity Models
severity_models <- list(
  gold_deepspace    = gold_deepspace_sev_reduced_model,
  gold_dockarc      = gold_dockarc_sev_reduced_model,
  gold_hardseal     = gold_hardseal_sev_reduced_model,
  gold_longhaul     = gold_longhaul_sev_reduced_model,
  gold_quantumcrate = gold_quantumcrate_sev_reduced_model,
  
  platinum_deepspace    = platinum_deepspace_sev_reduced_model,
  platinum_dockarc      = platinum_dockarc_sev_reduced_model,
  platinum_hardseal     = platinum_hardseal_sev_reduced_model,
  platinum_longhaul     = platinum_longhaul_sev_reduced_model,
  platinum_quantumcrate = platinum_quantumcrate_sev_reduced_model,
  
  other_deepspace    = other_deepspace_sev_reduced_model,
  other_dockarc      = other_dockarc_sev_reduced_model,
  other_hardseal     = other_hardseal_sev_reduced_model,
  other_longhaul     = other_longhaul_sev_reduced_model,
  other_quantumcrate = other_quantumcrate_sev_reduced_model
)

# Container weights
container_weights <- data.frame(
  container_type = c("deepspace", "dockarc", "hardseal", "longhaul", "quantumcrate"), 
  new_weight = c(0.05, 0.1, 0.5, 0.2, 0.15))

# Cargo weights
cargo_weights <- data.frame(
  cargo_type = c("gold", "platinum", "other"), 
  historical_weight = c(gold_weight, platinum_weight, other_weight))

# Create segments as per models, and attach relevant weights
segment_names <- as.vector(outer(cargo_weights$cargo_type, container_weights$container_type, paste, sep = "_"))
segment_weights <- setNames(as.vector(outer(cargo_weights$historical_weight, container_weights$new_weight, `*`)), segment_names)

# Extract severity parameters
sev_meanlogs_sim_factor <- 1.108686405
sev_sigma_sim_factor <- 1.05

extract_sev_params <- function(model, meanlogs_sim_factor, sigma_sim_factor) {
  meanlogs <- predict(model)
  meanlogs <- meanlogs + log(meanlogs_sim_factor)
  
  sigma <- summary(model)$sigma
  sigma <- sigma * sigma_sim_factor
  
  list(meanlogs = meanlogs, 
       sigma = sigma)}

sev_params_sim <- lapply(severity_models, extract_sev_params, meanlogs_sim_factor = sev_meanlogs_sim_factor, sigma_sim_factor = sev_sigma_sim_factor)

# Collective Risk Simulation

agg_loss_sim <- function(seg_weights, mu, theta, sev_params, n_sims = 100000){
  seg_names <- segment_names
  n_segs <- length(seg_names)
  
  # create output matrix for segment simulation and portfolio total
  seg_sims <- matrix(0, nrow = n_sims, ncol = n_segs)
  colnames(seg_sims) <- seg_names
  tot_sims <- numeric(n_sims)
  
  # set severity scenarios per segment
  seg_scens <- sapply(sev_params, function(n) length(n$meanlogs))
  
  
  for (i in 1:n_sims){
    # simulate claim count
    n_claims <- rnbinom(1, size = theta, mu = mu)
    if (n_claims > 0){
      # assign claim to portfolio segment
      claim_seg <- sample(n_segs, n_claims, replace = TRUE, prob = seg_weights)
      
      # segment level analysis
      for (s in 1:n_segs){
        claim_seg_count <- sum(claim_seg == s)
        
        if (claim_seg_count > 0){
          params <- sev_params[[s]]
          
          # assign severity scenario from segment 
          sev_scen <- sample(seg_scens[s], claim_seg_count, replace = TRUE)
          sev <- rlnorm(claim_seg_count, meanlog = params$meanlogs[sev_scen], sdlog = params$sigma)
          seg_sims[i, s] <- sum(sev)
        }
      }
      tot_sims[i] <- sum(seg_sims[i, ])
    }
  }
  list(segment_sims = seg_sims, tot_sims = tot_sims)
}

set.seed(101)

sim_results <- agg_loss_sim(segment_weights, freq_mu_sim, freq_theta_sim, sev_params_sim, n_sims = 100000)

# Summary statistics

sim_stats <- function(sims){
  VaR99 = unname(quantile(sims, 0.99))
  c(expected_loss = mean(sims), 
    sd = sd(sims), 
    quantile_2.5 = unname(quantile(sims, 0.025)), 
    quantile_97.5 = unname(quantile(sims, 0.975)),
    CI = 1.96 * sd(sims) / sqrt(length(sims)),
    VaR99 = VaR99, 
    ES99 = mean(sims[sims >= VaR99]))}

    # Segment-level statistics
    segment_stats <- as.data.frame(
      t(apply(sim_results$segment_sims, 2, sim_stats)))
    segment_stats$segment <- rownames(segment_stats)
    rownames(segment_stats) <- NULL
    segment_stats <- segment_stats[, c("segment", "expected_loss", "sd", "quantile_2.5", "quantile_97.5", "CI", "VaR99", "ES99")]
    
    # Aggregate to cargo level
    cargo_sims <- lapply(cargo_weights$cargo_type, function(cargo) {
      cols <- grep(paste0("^", cargo, "_"), colnames(sim_results$segment_sims), value = TRUE)
      rowSums(sim_results$segment_sims[, cols, drop = FALSE])})
    names(cargo_sims) <- cargo_weights$cargo_type
    
    cargo_stats <- as.data.frame(
      t(sapply(cargo_sims, sim_stats)))
    cargo_stats$segment <- paste0(rownames(cargo_stats), " (cargo total)")
    rownames(cargo_stats) <- NULL
    cargo_stats <- cargo_stats[, c("segment", "expected_loss", "sd", "quantile_2.5", "quantile_97.5", "CI", "VaR99", "ES99")]
    
    # Policy-level statistics
    policy_stats <- as.data.frame(t(sim_stats(sim_results$tot_sims)))
    policy_stats$segment <- "POLICY TOTAL"
    policy_stats <- policy_stats[, c("segment", "expected_loss", "sd", "quantile_2.5", "quantile_97.5", "CI", "VaR99", "ES99")]
    policy_stats
    
  sim_expectedloss <- policy_stats$expected_loss
  sim_sd <- policy_stats$sd


##################################################################################################
### DEDUCTIBLE + CAP MODELLING ###################################################################
##################################################################################################

  # Extract severity parameters
  sev_meanlogs_sim_factor <- 1.108686405
  sev_sigma_sim_factor <- 1.05
  
  extract_sev_params <- function(model, meanlogs_sim_factor, sigma_sim_factor) {
    meanlogs <- predict(model)
    meanlogs <- meanlogs + log(meanlogs_sim_factor)
    
    sigma <- summary(model)$sigma
    sigma <- sigma * sigma_sim_factor
    
    cargo_values <- model$model$cargo_value
    
    list(meanlogs = meanlogs, 
         sigma = sigma,
         cargo_values = cargo_values)}
  
  sev_params_sim <- lapply(severity_models, extract_sev_params, meanlogs_sim_factor = sev_meanlogs_sim_factor, sigma_sim_factor = sev_sigma_sim_factor)
  
  
  # Collective Risk Simulation
  
  ded_pct <- 0.05
  cap_pct <- 0.1
  
  agg_loss_sim <- function(seg_weights, mu, theta, sev_params, ded_pct, cap_pct, n_sims = 100000){
    seg_names <- segment_names
    n_segs <- length(seg_names)
    
    # create output matrix for segment simulation and portfolio total
    seg_sims <- matrix(0, nrow = n_sims, ncol = n_segs)
    colnames(seg_sims) <- seg_names
    tot_sims <- numeric(n_sims)
    
    # set severity scenarios per segment
    seg_scens <- sapply(sev_params, function(n) length(n$meanlogs))
    
    
    for (i in 1:n_sims){
      # simulate claim count
      n_claims <- rnbinom(1, size = theta, mu = mu)
      if (n_claims > 0){
        # assign claim to portfolio segment
        claim_seg <- sample(n_segs, n_claims, replace = TRUE, prob = seg_weights)
        
        # segment level analysis
        for (s in 1:n_segs){
          claim_seg_count <- sum(claim_seg == s)
          
          if (claim_seg_count > 0){
            params <- sev_params[[s]]
            
            # assign severity scenario from segment 
            sev_scen <- sample(seg_scens[s], claim_seg_count, replace = TRUE)
            sev <- rlnorm(claim_seg_count, meanlog = params$meanlogs[sev_scen], sdlog = params$sigma)
            
            # apply cap to cargo value
            cargo_val <- params$cargo_values[sev_scen]  # need to store cargo_value
            ded       <- ded_pct * cargo_val
            cap       <- cap_pct * cargo_val
            # sev       <- pmin(sev, cap)   # cap
            # sev       <- pmax(sev - ded, 0)   # deductible
            sev       <- pmin(pmax(sev - ded, 0), cap - ded)  # cap + deductible
            
            seg_sims[i, s] <- sum(sev)
          }
        }
        tot_sims[i] <- sum(seg_sims[i, ])
      }
    }
    list(segment_sims = seg_sims, tot_sims = tot_sims)
  }
  
  set.seed(101)
  
  TESTCAP_sim_results <- agg_loss_sim(segment_weights, freq_mu_sim, freq_theta_sim, sev_params_sim, ded_pct, cap_pct, n_sims = 100000)
  
  
  # Summary statistics
  
  sim_stats <- function(sims){
    VaR99 = unname(quantile(sims, 0.99))
    c(expected_loss = mean(sims), 
      sd = sd(sims), 
      quantile_2.5 = unname(quantile(sims, 0.025)), 
      quantile_97.5 = unname(quantile(sims, 0.975)),
      CI = 1.96 * sd(sims) / sqrt(length(sims)),
      VaR99 = VaR99, 
      ES99 = mean(sims[sims >= VaR99]))}
  
  # Segment-level statistics
  segment_stats <- as.data.frame(
    t(apply(TESTCAP_sim_results$segment_sims, 2, sim_stats)))
  segment_stats$segment <- rownames(segment_stats)
  rownames(segment_stats) <- NULL
  segment_stats <- segment_stats[, c("segment", "expected_loss", "sd", "quantile_2.5", "quantile_97.5", "CI", "VaR99", "ES99")]
  
  # Aggregate to cargo level
  cargo_sims <- lapply(cargo_weights$cargo_type, function(cargo) {
    cols <- grep(paste0("^", cargo, "_"), colnames(TESTCAP_sim_results$segment_sims), value = TRUE)
    rowSums(TESTCAP_sim_results$segment_sims[, cols, drop = FALSE])})
  names(cargo_sims) <- cargo_weights$cargo_type
  
  cargo_stats <- as.data.frame(
    t(sapply(cargo_sims, sim_stats)))
  cargo_stats$segment <- paste0(rownames(cargo_stats), " (cargo total)")
  rownames(cargo_stats) <- NULL
  cargo_stats <- cargo_stats[, c("segment", "expected_loss", "sd", "quantile_2.5", "quantile_97.5", "CI", "VaR99", "ES99")]
  
  # Policy-level statistics
  policy_stats <- as.data.frame(t(sim_stats(TESTCAP_sim_results$tot_sims)))
  policy_stats$segment <- "POLICY TOTAL"
  policy_stats <- policy_stats[, c("segment", "expected_loss", "sd", "quantile_2.5", "quantile_97.5", "CI", "VaR99", "ES99")]
  policy_stats
  
  sim_expectedloss <- policy_stats$expected_loss
  sim_sd <- policy_stats$sd
  sim_VaR <- policy_stats$VaR99
  sim_ES <- policy_stats$ES99
  
  
  helionis_containers <- 1160
  bayesia_containers <- 1128
  oryn_containers <- 774
  nb_containers <- sum(helionis_containers, bayesia_containers, oryn_containers)
  
  ####
  sim_expectedloss*nb_containers
  sim_sd*sqrt(nb_containers)
  nb_containers * sim_expectedloss + sqrt(nb_containers) * (sim_VaR - sim_expectedloss)
  nb_containers * sim_expectedloss + sqrt(nb_containers) * (sim_ES - sim_expectedloss)
  
  ####
  sim_expectedloss*helionis_containers
  sim_sd*sqrt(helionis_containers)
  helionis_containers * sim_expectedloss + sqrt(helionis_containers) * (sim_VaR - sim_expectedloss)
  helionis_containers * sim_expectedloss + sqrt(helionis_containers) * (sim_ES - sim_expectedloss)
  
  ####
  sim_expectedloss*bayesia_containers
  sim_sd*sqrt(bayesia_containers)
  bayesia_containers * sim_expectedloss + sqrt(bayesia_containers) * (sim_VaR - sim_expectedloss)
  bayesia_containers * sim_expectedloss + sqrt(bayesia_containers) * (sim_ES - sim_expectedloss)
  
  ####
  sim_expectedloss*oryn_containers
  sim_sd*sqrt(oryn_containers)
  oryn_containers * sim_expectedloss + sqrt(oryn_containers) * (sim_VaR - sim_expectedloss)
  oryn_containers * sim_expectedloss + sqrt(oryn_containers) * (sim_ES - sim_expectedloss)
  
  
  
#################################################################################################
  

### Projected Future Loss

# 1-year: 2175
inflation_1year <- 0.0423 # 5 year moving average
spotrate_1year <- 0.0457 # linear regression
growthrate_1year <- 0.0212 #CAGR
loss_1year <- sim_expectedloss * (1+inflation_1year)  * (1+growthrate_1year) / (1+spotrate_1year)
loss_1year_2.5 <- policy_stats$quantile_2.5 * (1+inflation_1year)  * (1+growthrate_1year) / (1+spotrate_1year)
loss_1year_97.5 <- policy_stats$quantile_97.5 * (1+inflation_1year)  * (1+growthrate_1year) / (1+spotrate_1year)

# 10-year: 2184
inflationfactor_10year <- 1.4726771 #calc in excel
spotrate_10year <-  0.0435
loss_10year <- sim_expectedloss * inflationfactor_10year  * (1+growthrate_1year)^10 / (1+spotrate_10year)^10
loss_10year_2.5 <- policy_stats$quantile_2.5 * inflationfactor_10year  * (1+growthrate_1year)^10 / (1+spotrate_10year)^10
loss_10year_97.5 <- policy_stats$quantile_97.5 * inflationfactor_10year  * (1+growthrate_1year)^10 / (1+spotrate_10year)^10

# Aggregate to solar systems
  
  # Short term
helionis_port_1yr_loss <- loss_1year * helionis_containers
bayesia_port_1yr_loss <- loss_1year * bayesia_containers
oryn_port_1yr_loss <- loss_1year * oryn_containers

helionis_port_1yr_loss_2.5 <- loss_1year_2.5 * helionis_containers
bayesia_port_1yr_loss_2.5 <- loss_1year_2.5 * bayesia_containers
oryn_port_1yr_loss_2.5 <- loss_1year_2.5 * oryn_containers

helionis_port_1yr_loss_97.5 <- loss_1year_97.5 * helionis_containers
bayesia_port_1yr_loss_97.5 <- loss_1year_97.5 * bayesia_containers
oryn_port_1yr_loss_97.5 <- loss_1year_97.5 * oryn_containers

  # Long term
helionis_port_10yr_loss <- loss_10year * helionis_containers
bayesia_port_10yr_loss <- loss_10year * bayesia_containers
oryn_port_10yr_loss <- loss_10year * oryn_containers

helionis_port_10yr_loss_2.5 <- loss_10year_2.5 * helionis_containers
bayesia_port_10yr_loss_2.5 <- loss_10year_2.5 * bayesia_containers
oryn_port_10yr_loss_2.5 <- loss_10year_2.5 * oryn_containers

helionis_port_10yr_loss_97.5 <- loss_10year_97.5 * helionis_containers
bayesia_port_10yr_loss_97.5 <- loss_10year_97.5 * bayesia_containers
oryn_port_10yr_loss_97.5 <- loss_10year_97.5 * oryn_containers


  # Portfolio
port_1yr_loss <- helionis_port_1yr_loss + bayesia_port_1yr_loss + oryn_port_1yr_loss
port_10yr_loss <- helionis_port_10yr_loss + bayesia_port_10yr_loss + oryn_port_10yr_loss

port_1yr_loss_2.5 <- helionis_port_1yr_loss_2.5 + bayesia_port_1yr_loss_2.5 + oryn_port_1yr_loss_2.5
port_1yr_loss_97.5 <- helionis_port_1yr_loss_97.5 + bayesia_port_1yr_loss_97.5 + oryn_port_1yr_loss_97.5

port_10yr_loss_2.5 <- helionis_port_10yr_loss_2.5 + bayesia_port_10yr_loss_2.5 + oryn_port_10yr_loss_2.5
port_10yr_loss_97.5 <- helionis_port_10yr_loss_97.5 + bayesia_port_10yr_loss_97.5 + oryn_port_10yr_loss_97.5


################################################################################################################################################


#########################
### Premium Modelling ###
#########################
par(mfrow=c(1,1))

risk_factors_range <- seq(0, 1, by = 0.01)
premiums <- numeric(length(risk_factors_range))
loss_ratios <- numeric(length(risk_factors_range))

# prem_model <- sim_expectedloss + risk_factors * sim_sd

for (i in seq_along(risk_factors_range)) {
  rf <- risk_factors_range[i]
  premiums[i] <- sim_expectedloss + rf * sim_sd
  loss_ratios[i] <- sim_expectedloss / premiums[i]
}

plot(risk_factors_range, loss_ratios, 
     main = 'Loss Ratio by Risk Factor', 
     xlab = 'Risk Factor', 
     ylab = 'Loss Ratio', 
     pch = 16, 
     cex = 0.7)

risk_factor_table <- data.frame(
  risk_factor = risk_factors_range,
  loss_ratio  = loss_ratios)

# select risk factor of 0.1 which derives an approximate 75% Loss Ratio
alpha <- 0.1
prem_model <- sim_expectedloss + alpha * sim_sd

prem_model_agg <- prem_model * nb_containers

prem_model_2.5 <- policy_stats$quantile_2.5 + alpha * sim_sd
prem_model_97.5 <- policy_stats$quantile_97.5 + alpha * sim_sd

### Projected Premiums
premium_1year <- prem_model * (1+inflation_1year)  * (1+growthrate_1year) / (1+spotrate_1year)
premium_10year <- prem_model * inflationfactor_10year  * (1+growthrate_1year)^10 / (1+spotrate_10year)^10

helionis_port_1yr_prem <- premium_1year * helionis_containers
bayesia_port_1yr_prem <- premium_1year * bayesia_containers
oryn_port_1yr_prem <- premium_1year * oryn_containers

helionis_port_10yr_prem <- premium_10year * helionis_containers
bayesia_port_10yr_prem <- premium_10year * bayesia_containers
oryn_port_10yr_prem <- premium_10year * oryn_containers

premium_1year_agg <- helionis_port_1yr_prem + bayesia_port_1yr_prem + oryn_port_1yr_prem 
premium_10year_agg <- helionis_port_10yr_prem + bayesia_port_10yr_prem + oryn_port_10yr_prem 

    ## Confidence Interval Range
    premium_1year_2.5 <- prem_model_2.5 * (1+inflation_1year)  * (1+growthrate_1year) / (1+spotrate_1year)
    premium_10year_2.5 <- prem_model_2.5 * inflationfactor_10year  * (1+growthrate_1year)^10 / (1+spotrate_10year)^10
   
    premium_1year_97.5 <- prem_model_97.5 * (1+inflation_1year)  * (1+growthrate_1year) / (1+spotrate_1year)
    premium_10year_97.5 <- prem_model_97.5 * inflationfactor_10year  * (1+growthrate_1year)^10 / (1+spotrate_10year)^10
    
    
    helionis_port_1yr_prem_2.5 <- premium_1year_2.5 * helionis_containers
    bayesia_port_1yr_prem_2.5 <- premium_1year_2.5 * bayesia_containers
    oryn_port_1yr_prem_2.5 <- premium_1year_2.5 * oryn_containers
    
    helionis_port_1yr_prem_97.5 <- premium_1year_97.5 * helionis_containers
    bayesia_port_1yr_prem_97.5 <- premium_1year_97.5 * bayesia_containers
    oryn_port_1yr_prem_97.5 <- premium_1year_97.5 * oryn_containers
    
    helionis_port_10yr_prem_2.5 <- premium_10year_2.5 * helionis_containers
    bayesia_port_10yr_prem_2.5 <- premium_10year_2.5 * bayesia_containers
    oryn_port_10yr_prem_2.5 <- premium_10year_2.5 * oryn_containers
    
    helionis_port_10yr_prem_97.5 <- premium_10year_97.5 * helionis_containers
    bayesia_port_10yr_prem_97.5 <- premium_10year_97.5 * bayesia_containers
    oryn_port_10yr_prem_97.5 <- premium_10year_97.5 * oryn_containers
    
    premium_1year_agg_2.5 <- helionis_port_1yr_prem_2.5 + bayesia_port_1yr_prem_2.5 + oryn_port_1yr_prem_2.5 
    premium_10year_agg_2.5 <- helionis_port_10yr_prem_2.5 + bayesia_port_10yr_prem_2.5 + oryn_port_10yr_prem_2.5 
  
    premium_1year_agg_97.5 <- helionis_port_1yr_prem_97.5 + bayesia_port_1yr_prem_97.5 + oryn_port_1yr_prem_97.5 
    premium_10year_agg_97.5 <- helionis_port_10yr_prem_97.5 + bayesia_port_10yr_prem_97.5 + oryn_port_10yr_prem_97.5 
    
  

### Revenue 
helionis_port_1yr_prem - helionis_port_1yr_loss
bayesia_port_1yr_prem - bayesia_port_1yr_loss
oryn_port_1yr_prem - oryn_port_1yr_loss

sum(helionis_port_1yr_prem - helionis_port_1yr_loss,
    bayesia_port_1yr_prem - bayesia_port_1yr_loss,
    oryn_port_1yr_prem - oryn_port_1yr_loss)

helionis_port_10yr_prem - helionis_port_10yr_loss
bayesia_port_10yr_prem - bayesia_port_10yr_loss
oryn_port_10yr_prem - oryn_port_10yr_loss

sum(helionis_port_10yr_prem - helionis_port_10yr_loss,
    bayesia_port_10yr_prem - bayesia_port_10yr_loss,
    oryn_port_10yr_prem - oryn_port_10yr_loss)

  #### PORTFOLIO SIM ####
  set.seed(101)
  PORTFOLIO_sim_results <- agg_loss_sim(segment_weights, freq_mu_sim*nb_containers, freq_theta_sim, sev_params_sim, ded_pct, cap_pct, n_sims = 10000)
  
  portfolio_stats <- as.data.frame(t(sim_stats(PORTFOLIO_sim_results$tot_sims)))
  portfolio_stats$segment <- "POLICY TOTAL"
  portfolio_stats <- portfolio_stats[, c("segment", "expected_loss", "sd", "quantile_2.5", "quantile_97.5", "CI", "VaR99", "ES99")]
  portfolio_stats
  
  portfolio_prem_sim <- portfolio_stats$expectedloss + alpha * portfolio_stats$sd
  portfolio_rev_sim <- portfolio_prem_sim - portfolio_stats$expectedloss

  
  revenue_sim <- function(alpha, n_policies, n_runs, n_sims,
                          segment_weights, freq_mu_sim, freq_theta_sim,
                          sev_params_sim, ded_pct, cap_pct) {
    
    # Store results across runs
    results <- data.frame(
      run = 1:n_runs,
      expected_loss = numeric(n_runs),
      sd = numeric(n_runs),
      VaR99 = numeric(n_runs),
      ES99 = numeric(n_runs),
      premium = numeric(n_runs),
      revenue = numeric(n_runs),
      loss_ratio = numeric(n_runs)
    )

  for (r in 1:n_runs) {
    # Run simulation
    sim <- agg_loss_sim(segment_weights, freq_mu_sim * n_policies, freq_theta_sim, sev_params_sim, ded_pct, cap_pct, n_sims = n_sims)
    
    # Compute statistics
    stats <- sim_stats(sim$tot_sims)
    
    # Premium = expected loss + risk loading
    prem <- stats["expected_loss"] + alpha * stats["sd"]
    
    # Revenue = premium - actual losses (random draw from simulation)
    realised_loss <- mean(sim$tot_sims)
    rev  <- prem - realised_loss
    
    # Store results
    results[r, "expected_loss"] <- stats["expected_loss"]
    results[r, "sd"]            <- stats["sd"]
    results[r, "premium"]       <- prem
    results[r, "revenue"]       <- rev
    results[r, "loss_ratio"]    <- realised_loss / prem
  }
  results
  }


set.seed(101)
revenue_results <- revenue_sim(alpha = alpha, n_policies = nb_containers, n_runs = 100, n_sims = 1000, segment_weights = segment_weights,
  freq_mu_sim = freq_mu_sim, freq_theta_sim = freq_theta_sim, sev_params_sim = sev_params_sim, ded_pct = ded_pct, cap_pct = cap_pct)

revenue_results_helionis <- revenue_sim(alpha = alpha, n_policies = helionis_containers, n_runs = 100, n_sims = 1000, segment_weights = segment_weights,
                               freq_mu_sim = freq_mu_sim, freq_theta_sim = freq_theta_sim, sev_params_sim = sev_params_sim, ded_pct = ded_pct, cap_pct = cap_pct)

revenue_results_bayesia <- revenue_sim(alpha = alpha, n_policies = bayesia_containers, n_runs = 100, n_sims = 1000, segment_weights = segment_weights,
                               freq_mu_sim = freq_mu_sim, freq_theta_sim = freq_theta_sim, sev_params_sim = sev_params_sim, ded_pct = ded_pct, cap_pct = cap_pct)

revenue_results_oryn <- revenue_sim(alpha = alpha, n_policies = oryn_containers, n_runs = 100, n_sims = 1000, segment_weights = segment_weights,
                               freq_mu_sim = freq_mu_sim, freq_theta_sim = freq_theta_sim, sev_params_sim = sev_params_sim, ded_pct = ded_pct, cap_pct = cap_pct)

#### LOSSES ####
# expected value ST
mean(revenue_results$expected_loss) * (1+inflation_1year)  * (1+growthrate_1year) / (1+spotrate_1year)
mean(revenue_results_helionis$expected_loss) * (1+inflation_1year)  * (1+growthrate_1year) / (1+spotrate_1year)
mean(revenue_results_bayesia$expected_loss) * (1+inflation_1year)  * (1+growthrate_1year) / (1+spotrate_1year)
mean(revenue_results_oryn$expected_loss) * (1+inflation_1year)  * (1+growthrate_1year) / (1+spotrate_1year)

# lower CI ST
quantile(revenue_results$expected_loss, 0.025) * (1+inflation_1year)  * (1+growthrate_1year) / (1+spotrate_1year)
quantile(revenue_results_helionis$expected_loss, 0.025) * (1+inflation_1year)  * (1+growthrate_1year) / (1+spotrate_1year)
quantile(revenue_results_bayesia$expected_loss, 0.025) * (1+inflation_1year)  * (1+growthrate_1year) / (1+spotrate_1year)
quantile(revenue_results_oryn$expected_loss, 0.025) * (1+inflation_1year)  * (1+growthrate_1year) / (1+spotrate_1year)

# upper CI ST
quantile(revenue_results$expected_loss, 0.975) * (1+inflation_1year)  * (1+growthrate_1year) / (1+spotrate_1year)
quantile(revenue_results_helionis$expected_loss, 0.975) * (1+inflation_1year)  * (1+growthrate_1year) / (1+spotrate_1year)
quantile(revenue_results_bayesia$expected_loss, 0.975) * (1+inflation_1year)  * (1+growthrate_1year) / (1+spotrate_1year)
quantile(revenue_results_oryn$expected_loss, 0.975) * (1+inflation_1year)  * (1+growthrate_1year) / (1+spotrate_1year)



# expected value LT
mean(revenue_results$expected_loss) *  inflationfactor_10year  * (1+growthrate_1year)^10 / (1+spotrate_10year)^10
mean(revenue_results_helionis$expected_loss) *  inflationfactor_10year  * (1+growthrate_1year)^10 / (1+spotrate_10year)^10
mean(revenue_results_bayesia$expected_loss) *  inflationfactor_10year  * (1+growthrate_1year)^10 / (1+spotrate_10year)^10
mean(revenue_results_oryn$expected_loss) *  inflationfactor_10year  * (1+growthrate_1year)^10 / (1+spotrate_10year)^10

# lower CI LT
quantile(revenue_results$expected_loss, 0.025) *  inflationfactor_10year  * (1+growthrate_1year)^10 / (1+spotrate_10year)^10
quantile(revenue_results_helionis$expected_loss, 0.025) *  inflationfactor_10year  * (1+growthrate_1year)^10 / (1+spotrate_10year)^10
quantile(revenue_results_bayesia$expected_loss, 0.025) *  inflationfactor_10year  * (1+growthrate_1year)^10 / (1+spotrate_10year)^10
quantile(revenue_results_oryn$expected_loss, 0.025) *  inflationfactor_10year  * (1+growthrate_1year)^10 / (1+spotrate_10year)^10

# upper CI LT
quantile(revenue_results$expected_loss, 0.975) *  inflationfactor_10year  * (1+growthrate_1year)^10 / (1+spotrate_10year)^10
quantile(revenue_results_helionis$expected_loss, 0.975) *  inflationfactor_10year  * (1+growthrate_1year)^10 / (1+spotrate_10year)^10
quantile(revenue_results_bayesia$expected_loss, 0.975) *  inflationfactor_10year  * (1+growthrate_1year)^10 / (1+spotrate_10year)^10
quantile(revenue_results_oryn$expected_loss, 0.975) *  inflationfactor_10year  * (1+growthrate_1year)^10 / (1+spotrate_10year)^10




#### PREMIUM ####
# expected value ST
mean(revenue_results$premium) * (1+inflation_1year)  * (1+growthrate_1year) / (1+spotrate_1year)
mean(revenue_results_helionis$premium) * (1+inflation_1year)  * (1+growthrate_1year) / (1+spotrate_1year)
mean(revenue_results_bayesia$premium) * (1+inflation_1year)  * (1+growthrate_1year) / (1+spotrate_1year)
mean(revenue_results_oryn$premium) * (1+inflation_1year)  * (1+growthrate_1year) / (1+spotrate_1year)

# lower CI ST
quantile(revenue_results$premium, 0.025) * (1+inflation_1year)  * (1+growthrate_1year) / (1+spotrate_1year)
quantile(revenue_results_helionis$premium, 0.025) * (1+inflation_1year)  * (1+growthrate_1year) / (1+spotrate_1year)
quantile(revenue_results_bayesia$premium, 0.025) * (1+inflation_1year)  * (1+growthrate_1year) / (1+spotrate_1year)
quantile(revenue_results_oryn$premium, 0.025) * (1+inflation_1year)  * (1+growthrate_1year) / (1+spotrate_1year)

# upper CI ST
quantile(revenue_results$premium, 0.975) * (1+inflation_1year)  * (1+growthrate_1year) / (1+spotrate_1year)
quantile(revenue_results_helionis$premium, 0.975) * (1+inflation_1year)  * (1+growthrate_1year) / (1+spotrate_1year)
quantile(revenue_results_bayesia$premium, 0.975) * (1+inflation_1year)  * (1+growthrate_1year) / (1+spotrate_1year)
quantile(revenue_results_oryn$premium, 0.975) * (1+inflation_1year)  * (1+growthrate_1year) / (1+spotrate_1year)



# expected value LT
mean(revenue_results$premium) *  inflationfactor_10year  * (1+growthrate_1year)^10 / (1+spotrate_10year)^10
mean(revenue_results_helionis$premium) *  inflationfactor_10year  * (1+growthrate_1year)^10 / (1+spotrate_10year)^10
mean(revenue_results_bayesia$premium) *  inflationfactor_10year  * (1+growthrate_1year)^10 / (1+spotrate_10year)^10
mean(revenue_results_oryn$premium) *  inflationfactor_10year  * (1+growthrate_1year)^10 / (1+spotrate_10year)^10

# lower CI LT
quantile(revenue_results$premium, 0.025) *  inflationfactor_10year  * (1+growthrate_1year)^10 / (1+spotrate_10year)^10
quantile(revenue_results_helionis$premium, 0.025) *  inflationfactor_10year  * (1+growthrate_1year)^10 / (1+spotrate_10year)^10
quantile(revenue_results_bayesia$premium, 0.025) *  inflationfactor_10year  * (1+growthrate_1year)^10 / (1+spotrate_10year)^10
quantile(revenue_results_oryn$premium, 0.025) *  inflationfactor_10year  * (1+growthrate_1year)^10 / (1+spotrate_10year)^10

# upper CI LT
quantile(revenue_results$premium, 0.975) *  inflationfactor_10year  * (1+growthrate_1year)^10 / (1+spotrate_10year)^10
quantile(revenue_results_helionis$premium, 0.975) *  inflationfactor_10year  * (1+growthrate_1year)^10 / (1+spotrate_10year)^10
quantile(revenue_results_bayesia$premium, 0.975) *  inflationfactor_10year  * (1+growthrate_1year)^10 / (1+spotrate_10year)^10
quantile(revenue_results_oryn$premium, 0.975) *  inflationfactor_10year  * (1+growthrate_1year)^10 / (1+spotrate_10year)^10



#### REVENUE #####
# expected value ST
mean(revenue_results$revenue) * (1+inflation_1year)  * (1+growthrate_1year) / (1+spotrate_1year)
mean(revenue_results_helionis$revenue) * (1+inflation_1year)  * (1+growthrate_1year) / (1+spotrate_1year)
mean(revenue_results_bayesia$revenue) * (1+inflation_1year)  * (1+growthrate_1year) / (1+spotrate_1year)
mean(revenue_results_oryn$revenue) * (1+inflation_1year)  * (1+growthrate_1year) / (1+spotrate_1year)

# lower CI ST
quantile(revenue_results$revenue, 0.025) * (1+inflation_1year)  * (1+growthrate_1year) / (1+spotrate_1year)
quantile(revenue_results_helionis$revenue, 0.025) * (1+inflation_1year)  * (1+growthrate_1year) / (1+spotrate_1year)
quantile(revenue_results_bayesia$revenue, 0.025) * (1+inflation_1year)  * (1+growthrate_1year) / (1+spotrate_1year)
quantile(revenue_results_oryn$revenue, 0.025) * (1+inflation_1year)  * (1+growthrate_1year) / (1+spotrate_1year)

# upper CI ST
quantile(revenue_results$revenue, 0.975) * (1+inflation_1year)  * (1+growthrate_1year) / (1+spotrate_1year)
quantile(revenue_results_helionis$revenue, 0.975) * (1+inflation_1year)  * (1+growthrate_1year) / (1+spotrate_1year)
quantile(revenue_results_bayesia$revenue, 0.975) * (1+inflation_1year)  * (1+growthrate_1year) / (1+spotrate_1year)
quantile(revenue_results_oryn$revenue, 0.975) * (1+inflation_1year)  * (1+growthrate_1year) / (1+spotrate_1year)



# expected value LT
mean(revenue_results$revenue) *  inflationfactor_10year  * (1+growthrate_1year)^10 / (1+spotrate_10year)^10
mean(revenue_results_helionis$revenue) *  inflationfactor_10year  * (1+growthrate_1year)^10 / (1+spotrate_10year)^10
mean(revenue_results_bayesia$revenue) *  inflationfactor_10year  * (1+growthrate_1year)^10 / (1+spotrate_10year)^10
mean(revenue_results_oryn$revenue) *  inflationfactor_10year  * (1+growthrate_1year)^10 / (1+spotrate_10year)^10

# lower CI LT
quantile(revenue_results$revenue, 0.025) *  inflationfactor_10year  * (1+growthrate_1year)^10 / (1+spotrate_10year)^10
quantile(revenue_results_helionis$revenue, 0.025) *  inflationfactor_10year  * (1+growthrate_1year)^10 / (1+spotrate_10year)^10
quantile(revenue_results_bayesia$revenue, 0.025) *  inflationfactor_10year  * (1+growthrate_1year)^10 / (1+spotrate_10year)^10
quantile(revenue_results_oryn$revenue, 0.025) *  inflationfactor_10year  * (1+growthrate_1year)^10 / (1+spotrate_10year)^10

# upper CI LT
quantile(revenue_results$revenue, 0.975) *  inflationfactor_10year  * (1+growthrate_1year)^10 / (1+spotrate_10year)^10
quantile(revenue_results_helionis$revenue, 0.975) *  inflationfactor_10year  * (1+growthrate_1year)^10 / (1+spotrate_10year)^10
quantile(revenue_results_bayesia$revenue, 0.975) *  inflationfactor_10year  * (1+growthrate_1year)^10 / (1+spotrate_10year)^10
quantile(revenue_results_oryn$revenue, 0.975) *  inflationfactor_10year  * (1+growthrate_1year)^10 / (1+spotrate_10year)^10


revenue_1year <- prem_model * (1+inflation_1year)  * (1+growthrate_1year) / (1+spotrate_1year)
revenue_10year <- prem_model * inflationfactor_10year  * (1+growthrate_1year)^10 / (1+spotrate_10year)^10

  

### Loss Ratios
LR_base <- sim_expectedloss / prem_model
LR_1year <- loss_1year / premium_1year
LR_10year <- loss_10year / premium_10year

LR_base
LR_1year
LR_10year




################################################################################################################################################



######################
### STRESS TESTING ###
######################

### STRESS TEST SCENARIO 1: 1.5x freq, 1.5x sev 
# Large, unpredictable stellar flare in Oryn disrupts communications and equipment

# Adjust claim frequency parameters for risk exposure
freq_mu_ST1_factor <- 1.5
freq_mu_sim <- freq_mu * freq_mu_sim_factor * freq_mu_ST1_factor

freq_theta_ST1_factor <- 1
freq_theta_sim <- freq_theta * freq_theta_sim_factor * freq_theta_ST1_factor

sev_meanlogs_ST1_factor <- 1.5
sev_sigma_ST1_factor <- 1

sev_params_ST1 <- lapply(severity_models, extract_sev_params, meanlogs_sim_factor = sev_meanlogs_sim_factor*sev_meanlogs_ST1_factor, sigma_sim_factor = sev_sigma_sim_factor*sev_sigma_ST1_factor)

# Collective Risk Simulation

ST1_results <- agg_loss_sim(segment_weights, freq_mu_sim, freq_theta_sim, sev_params_ST1, ded_pct, cap_pct, n_sims = 100000)

# Policy-level statistics
ST1_policy_stats <- as.data.frame(t(sim_stats(ST1_results$tot_sims)))
ST1_policy_stats$segment <- "POLICY TOTAL"
ST1_policy_stats <- ST1_policy_stats[, c("segment", "expected_loss", "sd", "quantile_2.5", "quantile_97.5", "CI", "VaR99", "ES99")]


ST1_policy_stats$expected_loss
ST1_policy_stats$sd

ST1_agg_mean <- ST1_policy_stats$expected_loss * nb_containers
ST1_agg_sd <- ST1_policy_stats$sd * sqrt(nb_containers)

#######


### STRESS TEST SCENARIO 2: 3x freq, 1x sev 
# Major asteroid cluster fragmentation causes debris clouds and clutter that cause longer-term communication outages for weeks

# Adjust claim frequency parameters for risk exposure
freq_mu_ST2_factor <- 3
freq_mu_sim <- freq_mu * freq_mu_sim_factor * freq_mu_ST2_factor

freq_theta_ST2_factor <- 1
freq_theta_sim <- freq_theta * freq_theta_sim_factor * freq_theta_ST2_factor

sev_meanlogs_ST2_factor <- 1
sev_sigma_ST2_factor <- 1

sev_params_ST2 <- lapply(severity_models, extract_sev_params, meanlogs_sim_factor = sev_meanlogs_sim_factor*sev_meanlogs_ST2_factor, sigma_sim_factor = sev_sigma_sim_factor*sev_sigma_ST2_factor)

# Collective Risk Simulation

ST2_results <- agg_loss_sim(segment_weights, freq_mu_sim, freq_theta_sim, sev_params_ST2, ded_pct, cap_pct, n_sims = 100000)

# Policy-level statistics
ST2_policy_stats <- as.data.frame(t(sim_stats(ST2_results$tot_sims)))
ST2_policy_stats$segment <- "POLICY TOTAL"
ST2_policy_stats <- ST2_policy_stats[, c("segment", "expected_loss", "sd", "quantile_2.5", "quantile_97.5", "CI", "VaR99", "ES99")]

ST2_policy_stats$expected_loss
ST2_policy_stats$sd

ST2_agg_mean <- ST2_policy_stats$expected_loss * nb_containers
ST2_agg_sd <- ST2_policy_stats$sd * sqrt(nb_containers)

#######


### STRESS TEST SCENARIO 3: 1x freq, 3x sev 

# Adjust claim frequency parameters for risk exposure
freq_mu_ST3_factor <- 1
freq_mu_sim <- freq_mu * freq_mu_sim_factor * freq_mu_ST3_factor

freq_theta_ST3_factor <- 1
freq_theta_sim <- freq_theta * freq_theta_sim_factor * freq_theta_ST3_factor

sev_meanlogs_ST3_factor <- 3
sev_sigma_ST3_factor <- 1

sev_params_ST3 <- lapply(severity_models, extract_sev_params, meanlogs_sim_factor = sev_meanlogs_sim_factor*sev_meanlogs_ST3_factor, sigma_sim_factor = sev_sigma_sim_factor*sev_sigma_ST3_factor)

# Collective Risk Simulation

ST3_results <- agg_loss_sim(segment_weights, freq_mu_sim, freq_theta_sim, sev_params_ST3, ded_pct, cap_pct, n_sims = 100000)

# Policy-level statistics
ST3_policy_stats <- as.data.frame(t(sim_stats(ST3_results$tot_sims)))
ST3_policy_stats$segment <- "POLICY TOTAL"
ST3_policy_stats <- ST3_policy_stats[, c("segment", "expected_loss", "sd", "quantile_2.5", "quantile_97.5", "CI", "VaR99", "ES99")]

ST3_policy_stats$expected_loss
ST3_policy_stats$sd

ST3_agg_mean <- ST3_policy_stats$expected_loss * nb_containers
ST3_agg_sd <- ST3_policy_stats$sd * sqrt(nb_containers)

#######


### STRESS TEST SCENARIO 4: 1.5x freq, 1.3x sev 
# Oryn's asymmetric asteroid ring 

# Adjust claim frequency parameters for risk exposure
freq_mu_ST4_factor <- 1.5
freq_mu_sim <- freq_mu * freq_mu_sim_factor * freq_mu_ST4_factor

freq_theta_ST4_factor <- 1
freq_theta_sim <- freq_theta * freq_theta_sim_factor * freq_theta_ST4_factor

sev_meanlogs_ST4_factor <- 1.3
sev_sigma_ST4_factor <- 1


sev_params_ST4 <- lapply(severity_models, extract_sev_params, meanlogs_sim_factor = sev_meanlogs_sim_factor*sev_meanlogs_ST4_factor, sigma_sim_factor = sev_sigma_sim_factor*sev_sigma_ST4_factor)

# Collective Risk Simulation

ST4_results <- agg_loss_sim(segment_weights, freq_mu_sim, freq_theta_sim, sev_params_ST4, ded_pct, cap_pct, n_sims = 100000)

# Policy-level statistics
ST4_policy_stats <- as.data.frame(t(sim_stats(ST4_results$tot_sims)))
ST4_policy_stats$segment <- "POLICY TOTAL"
ST4_policy_stats <- ST4_policy_stats[, c("segment", "expected_loss", "sd", "quantile_2.5", "quantile_97.5", "CI", "VaR99", "ES99")]

ST4_policy_stats$expected_loss
ST4_policy_stats$sd

ST4_agg_mean <- ST4_policy_stats$expected_loss * nb_containers
ST4_agg_sd <- ST4_policy_stats$sd * sqrt(nb_containers)


#######


### STRESS TEST SCENARIO 5: 1.25x freq, 1.75x sev 
# Binary star alignment produces extreme radiation storm, forcing mining operations to shut down and potential equipment failure. 

# Adjust claim frequency parameters for risk exposure
freq_mu_ST5_factor <- 1.5
freq_mu_sim <- freq_mu * freq_mu_sim_factor * freq_mu_ST5_factor

freq_theta_ST5_factor <- 1
freq_theta_sim <- freq_theta * freq_theta_sim_factor * freq_theta_ST5_factor

sev_meanlogs_ST5_factor <- 1.3
sev_sigma_ST5_factor <- 1


sev_params_ST5 <- lapply(severity_models, extract_sev_params, meanlogs_sim_factor = sev_meanlogs_sim_factor*sev_meanlogs_ST5_factor, sigma_sim_factor = sev_sigma_sim_factor*sev_sigma_ST5_factor)

# Collective Risk Simulation

ST5_results <- agg_loss_sim(segment_weights, freq_mu_sim, freq_theta_sim, sev_params_ST5, ded_pct, cap_pct, n_sims = 100000)

# Policy-level statistics
ST5_policy_stats <- as.data.frame(t(sim_stats(ST5_results$tot_sims)))
ST5_policy_stats$segment <- "POLICY TOTAL"
ST5_policy_stats <- ST5_policy_stats[, c("segment", "expected_loss", "sd", "quantile_2.5", "quantile_97.5", "CI", "VaR99", "ES99")]

ST5_policy_stats$expected_loss
ST5_policy_stats$sd

ST5_agg_mean <- ST5_policy_stats$expected_loss * nb_containers
ST5_agg_sd <- ST5_policy_stats$sd * sqrt(nb_containers)


#######

ST1_policy_stats$expected_loss
ST2_policy_stats$expected_loss
ST3_policy_stats$expected_loss
ST4_policy_stats$expected_loss
ST5_policy_stats$expected_loss



#########################
### SCENARIO ANALYSIS ###
#########################

### MODERATE SCENARIO : 1.25x freq, 1.25x sev 

# Adjust claim frequency parameters for risk exposure
freq_mu_MOD_factor <- 1.1
freq_mu_sim <- freq_mu * freq_mu_sim_factor * freq_mu_MOD_factor

freq_theta_MOD_factor <- 1
freq_theta_sim <- freq_theta * freq_theta_sim_factor * freq_theta_MOD_factor

sev_meanlogs_MOD_factor <- 1.05
sev_sigma_MOD_factor <- 1

sev_params_MOD <- lapply(severity_models, extract_sev_params, meanlogs_sim_factor = sev_meanlogs_sim_factor*sev_meanlogs_MOD_factor, sigma_sim_factor = sev_sigma_sim_factor*sev_sigma_MOD_factor)

# Collective Risk Simulation

MOD_results <- agg_loss_sim(segment_weights, freq_mu_sim, freq_theta_sim, sev_params_MOD, ded_pct, cap_pct, n_sims = 100000)

# Policy-level statistics
MOD_policy_stats <- as.data.frame(t(sim_stats(MOD_results$tot_sims)))
MOD_policy_stats$segment <- "POLICY TOTAL"
MOD_policy_stats <- MOD_policy_stats[, c("segment", "expected_loss", "sd", "quantile_2.5", "quantile_97.5", "CI", "VaR99", "ES99")]


MOD_policy_stats$expected_loss
MOD_policy_stats$sd

MOD_agg_mean <- MOD_policy_stats$expected_loss * nb_containers
MOD_agg_sd <- MOD_policy_stats$sd * sqrt(nb_containers)


### BEST CASE SCENARIO : 0.75x freq, 0.75x sev 

# Adjust claim frequency parameters for risk exposure
freq_mu_BEST_factor <- 0.9
freq_mu_sim <- freq_mu * freq_mu_sim_factor * freq_mu_BEST_factor

freq_theta_BEST_factor <- 1
freq_theta_sim <- freq_theta * freq_theta_sim_factor * freq_theta_BEST_factor

sev_meanlogs_BEST_factor <- 0.85
sev_sigma_BEST_factor <- 1

sev_params_BEST <- lapply(severity_models, extract_sev_params, meanlogs_sim_factor = sev_meanlogs_sim_factor*sev_meanlogs_BEST_factor, sigma_sim_factor = sev_sigma_sim_factor*sev_sigma_BEST_factor)

# Collective Risk Simulation

BEST_results <- agg_loss_sim(segment_weights, freq_mu_sim, freq_theta_sim, sev_params_BEST, ded_pct, cap_pct, n_sims = 100000)

# Policy-level statistics
BEST_policy_stats <- as.data.frame(t(sim_stats(BEST_results$tot_sims)))
BEST_policy_stats$segment <- "POLICY TOTAL"
BEST_policy_stats <- BEST_policy_stats[, c("segment", "expected_loss", "sd", "quantile_2.5", "quantile_97.5", "CI", "VaR99", "ES99")]


BEST_policy_stats$expected_loss
BEST_policy_stats$sd

BEST_agg_mean <- BEST_policy_stats$expected_loss * nb_containers
BEST_agg_sd <- BEST_policy_stats$sd * sqrt(nb_containers)




### WORST CASE SCENARIO : 3x freq, 3x sev 

# Adjust claim frequency parameters for risk exposure
freq_mu_WORST_factor <- 1.3
freq_mu_sim <- freq_mu * freq_mu_sim_factor * freq_mu_WORST_factor

freq_theta_WORST_factor <- 1
freq_theta_sim <- freq_theta * freq_theta_sim_factor * freq_theta_WORST_factor

sev_meanlogs_WORST_factor <- 1.3
sev_sigma_WORST_factor <- 1

sev_params_WORST <- lapply(severity_models, extract_sev_params, meanlogs_sim_factor = sev_meanlogs_sim_factor*sev_meanlogs_WORST_factor, sigma_sim_factor = sev_sigma_sim_factor*sev_sigma_WORST_factor)

# Collective Risk Simulation

WORST_results <- agg_loss_sim(segment_weights, freq_mu_sim, freq_theta_sim, sev_params_WORST, ded_pct, cap_pct, n_sims = 100000)

# Policy-level statistics
WORST_policy_stats <- as.data.frame(t(sim_stats(WORST_results$tot_sims)))
WORST_policy_stats$segment <- "POLICY TOTAL"
WORST_policy_stats <- WORST_policy_stats[, c("segment", "expected_loss", "sd", "quantile_2.5", "quantile_97.5", "CI", "VaR99", "ES99")]


WORST_policy_stats$expected_loss
WORST_policy_stats$sd

WORST_agg_mean <- WORST_policy_stats$expected_loss * nb_containers
WORST_agg_sd <- WORST_policy_stats$sd * sqrt(nb_containers)













