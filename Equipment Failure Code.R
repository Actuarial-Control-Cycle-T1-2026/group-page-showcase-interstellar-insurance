#Libraries
library(readxl)
library(MASS)
library(pscl)
library(tidyr)
library(dplyr)
library(stringr)
library(ggplot2)
library(fitdistrplus)
library(survival)


#reading data sheets
freq_equip <- read_excel("srcsc-2026-claims-equipment-failure.xlsx", sheet = "freq")
sev_equip <- read_excel("srcsc-2026-claims-equipment-failure.xlsx", sheet = "sev")


#Inputed the excel for Quarry Inventory through tables 
########type
inv_equipment <- tibble(
  equipment_type = c("Quantum Bores","Graviton Extractors",
                     "Fexstram Carriers","ReglAggregators",
                     "Flux Riders","Ion Pulverizers"),
  Helionis = c(300,240,150,300,1500,90),
  Bayesia = c(150,120,75,150,750,45),
  OrynDelta = c(100,80,50,100,500,30)
) %>%
  pivot_longer(cols = c(Helionis,Bayesia,OrynDelta),
               names_to = "solar_system",
               values_to = "equipment_count")

##########service years
service_mid <- c("<5"=2.5, "5-9"=7, "10-14"=12, "15-19"=17, "20+"=22)

helionis_mat <- matrix(
  c(
    30,24,15,30,150,9,
    45,36,23,45,225,14,
    180,144,89,180,900,53,
    30,24,15,30,150,9,
    15,12,8,15,75,5
  ),
  nrow=5,
  byrow=TRUE
)

colnames(helionis_mat) <- c(
  "Quantum Bores","Graviton Extractors","Fexstram Carriers",
  "ReglAggregators","Flux Riders","Ion Pulverizers"
)

bayesia_mat <- matrix(
  c(
    45,36,23,45,225,14,
    38,30,19,38,187,11,
    59,48,29,59,300,18,
    8,6,4,8,38,2,
    0,0,0,0,0,0
  ),
  nrow=5,
  byrow=TRUE
)

colnames(bayesia_mat) <- colnames(helionis_mat)

oryn_mat <- matrix(
  c(
    75,60,37,75,375,22,
    15,12,8,15,75,5,
    10,8,5,10,50,3,
    0,0,0,0,0,0,
    0,0,0,0,0,0
  ),
  nrow=5,
  byrow=TRUE
)

colnames(oryn_mat) <- colnames(helionis_mat)

###Age
compute_avg_age <- function(mat, system_name){

  counts <- colSums(mat)

  avg_age <- sapply(seq_len(ncol(mat)), function(j){
    sum(mat[,j] * service_mid) / sum(mat[,j])
  })

  tibble(
    equipment_type = colnames(mat),
    solar_system = system_name,
    avg_service_years = avg_age
  )
}

service_df <- bind_rows(
  compute_avg_age(helionis_mat,"Helionis"),
  compute_avg_age(bayesia_mat,"Bayesia"),
  compute_avg_age(oryn_mat,"OrynDelta")
)

####Maintenance
operation_df <- tibble(
  equipment_type = c("Quantum Bores","Graviton Extractors",
                     "Fexstram Carriers","ReglAggregators",
                     "Flux Riders","Ion Pulverizers"),
  Helionis_op = c(0.95,0.95,0.90,0.80,0.80,0.50),
  Helionis_maint = c(750,750,375,1500,1500,1000),
  Bayesia_op = c(0.80,0.80,0.75,0.75,0.80,0.60),
  Bayesia_maint = c(600,600,400,1000,1000,750),
  Oryn_op = c(0.75,0.75,0.70,0.70,0.75,0.50),
  Oryn_maint = c(500,500,250,300,300,500)
) %>%
  pivot_longer(cols = -equipment_type,
               names_to = c("solar_system",".value"),
               names_pattern = "(Helionis|Bayesia|Oryn)_?(op|maint)") %>%
  rename(percent_in_operation = op,
         maintenance_hours = maint)
operation_df$solar_system <- recode(
  operation_df$solar_system,
  "Oryn" = "OrynDelta"
)

###Risk Index
risk_index <- tibble(
  equipment_type = rep(c("Quantum Bores","Graviton Extractors",
                         "Fexstram Carriers","ReglAggregators",
                         "Flux Riders","Ion Pulverizers"),3),
  solar_system = rep(c("Helionis","Bayesia","OrynDelta"), each=6),
  risk_index = c(
    0.69,0.48,0.67,0.24,0.24,0.64,   # Helionis
    0.77,0.57,0.71,0.26,0.21,0.66,   # Bayesia
    0.93,0.73,0.78,0.35,0.20,0.75    # Oryn Delta
  )
)

###Creating a Predict_Data Dataset for the Quarry Inventory 
predict_data <- inv_equipment %>%
  left_join(service_df, by = c("equipment_type","solar_system"))
predict_data <- predict_data %>%
  rename(equipment_age = avg_service_years)
predict_data <- predict_data %>%
  left_join(operation_df, by = c("equipment_type","solar_system"))
predict_data <- predict_data %>%
  rename(
    usage_int = percent_in_operation,
    maintenance_int = maintenance_hours
  )
predict_data$exposure <- predict_data$equipment_count

#############################################################################DATA CLEANING

#Checking what is going on in both frequency and severity
summary(freq_equip)
summary(sev_equip)
colSums(is.na(freq_equip))
colSums(is.na(sev_equip))


#Deal with unrealistic values
quantile(freq_equip$equipment_age, probs = c(0.95, 0.99), na.rm = TRUE)
upper_cutoff_freq <- quantile(freq_equip$equipment_age, 0.99, na.rm = TRUE)
freq_equip$equipment_age[freq_equip$equipment_age < 0 | freq_equip$equipment_age > upper_cutoff_freq] <- NA #quantile
freq_equip$maintenance_int[freq_equip$maintenance_int < 100 | freq_equip$maintenance_int > 5000] <- NA
freq_equip$usage_int[freq_equip$usage_int < 0 | freq_equip$usage_int > 24] <- NA
freq_equip$exposure[freq_equip$exposure < 0 | freq_equip$exposure > 1] <- NA
freq_equip$claim_count[freq_equip$claim_count < 0 | freq_equip$claim_count > 3] <- NA

quantile(sev_equip$equipment_age, probs = c(0.95, 0.99), na.rm = TRUE)
upper_cutoff_sev <- quantile(sev_equip$equipment_age, 0.99, na.rm = TRUE)
sev_equip$claim_seq[sev_equip$claim_seq < 0 | sev_equip$claim_seq > 4] <- NA
sev_equip$equipment_age[sev_equip$equipment_age < 0 | sev_equip$equipment_age > upper_cutoff_sev] <- NA
sev_equip$maintenance_int[sev_equip$maintenance_int < 100 | sev_equip$maintenance_int > 5000] <- NA
sev_equip$usage_int[sev_equip$usage_int < 0 | sev_equip$usage_int > 24] <- NA
sev_equip$exposure[sev_equip$exposure < 0 | sev_equip$exposure > 1] <- NA
sev_equip$claim_amount[sev_equip$claim_amount < 0 | sev_equip$claim_amount > 790000] <- NA

#check nas 
colSums(is.na(freq_equip))
colMeans(is.na(freq_equip)) *100  #very small percentage are missing - safe to delete missing ids

colSums(is.na(sev_equip))
colMeans(is.na(sev_equip)) * 100 #very small percentages are missing - safe to delete missing ids

#SOLAR SYSTEM CLEANING
freq_equip$solar_system <- sub("_.*", "", freq_equip$solar_system)
sev_equip$solar_system  <- sub("_.*", "", sev_equip$solar_system)
unique(freq_equip$solar_system)
unique(sev_equip$solar_system)

#EQUIPMENT TYPE CLEANING
freq_equip$equipment_type <- sub("_.*", "", freq_equip$equipment_type)
sev_equip$equipment_type  <- sub("_.*", "", sev_equip$equipment_type)
unique(freq_equip$equipment_type)
unique(sev_equip$equipment_type)

#omit claim count/ amount (missingness is small perctanges if checked as above - wont impact integrity of data)
freq_equip <- na.omit(freq_equip)
sev_equip <- na.omit(sev_equip)

####################################################################################EDA
lognorm_fit <- fitdistr(sev_equip$claim_amount, "lognormal")
wei_fit <- fitdist(sev_equip$claim_amount, "weibull")

hist(sev_equip$claim_amount,
     probability = TRUE,
     breaks = 50,
     col = "lightgray",
     main = "Claim Severity Distribution",
     xlab = "Claim Amount")

#x values
x <- seq(min(sev_equip$claim_amount),
         max(sev_equip$claim_amount),
         length = 1000)

#lognormal curve
lines(x,
      dlnorm(x,
             meanlog = lognorm_fit$estimate["meanlog"],
             sdlog = lognorm_fit$estimate["sdlog"]),
      col = "blue",
      lwd = 2)

#weibull curve
lines(x,
      dweibull(x,
               shape = wei_fit$estimate["shape"],
               scale = wei_fit$estimate["scale"]),
      col = "green",
      lwd = 2)
legend("topright",
       legend = c("Lognormal", "Weibull"),
       col = c("blue", "green"),
       lwd = 2)

table(sev_equip$solar_system) 
table(sev_equip$equipment_type) 


mean(freq_equip$claim_count, na.rm = TRUE)
var(freq_equip$claim_count, na.rm = TRUE)

freq_tail <- freq_equip %>%
  group_by(solar_system) %>%
  summarise(
    E_X = mean(claim_count, na.rm = TRUE),
    Var_X = var(claim_count, na.rm = TRUE),
    VaR_99 = quantile(claim_count, 0.99, na.rm = TRUE),
    TVaR_99 = mean(
      claim_count[claim_count >= quantile(claim_count, 0.99, na.rm = TRUE)],
      na.rm = TRUE
    )
  )

freq_tail

sev_tail <- sev_equip %>%
  group_by(solar_system) %>%
  summarise(
    E_X = mean(claim_amount, na.rm = TRUE),
    Var_X = var(claim_amount, na.rm = TRUE),
    VaR_99 = quantile(claim_amount, 0.99, na.rm = TRUE),
    TVaR_99 = mean(
      claim_amount[claim_amount >= quantile(claim_amount, 0.99, na.rm = TRUE)],
      na.rm = TRUE
    )
  )

sev_tail

###################################################################GLMs
####map solar systems to each other 
predict_data$solar_system <- recode(predict_data$solar_system,
                                    "Bayesia" = "Zeta",
                                    "OrynDelta" = "Epsilon",
					"Helionis" = "Helionis Cluster")
predict_data <- predict_data %>%
  mutate(equipment_type = str_replace_all(equipment_type, 
                                          c("Fexstram Carriers" = "FexStram Carrier",
                                            "Flux Riders" = "Flux Rider",
                                            "Graviton Extractors" = "Graviton Extractor",
                                            "Ion Pulverizers" = "Ion Pulverizer",
                                            "Quantum Bores" = "Quantum Bore")))


#frequency modelling
#negative binomial
freq_model <- glm.nb(
  claim_count ~ equipment_age + maintenance_int +
    usage_int + solar_system + equipment_type +
    offset(log(exposure)),
  data = freq_equip
)
summary(freq_model)

predict_data$pred_frequency <- predict(
  freq_model,
  newdata = predict_data,
  type = "response"
)

#diagnostic plots 
par(mfrow = c(2,2))
plot(freq_model)

#poisson
freq_pois <- glm(
  claim_count ~ equipment_age + maintenance_int +
    usage_int + solar_system + equipment_type +
    offset(log(exposure)),
  family = poisson(link = "log"),
  data = freq_equip
)
summary(freq_pois)


#zero inflated nb
freq_zinb <- zeroinfl(
  claim_count ~ equipment_age + maintenance_int +
    usage_int + solar_system + equipment_type +
    offset(log(exposure)) |
    equipment_age + maintenance_int + usage_int,
  dist = "negbin",
  data = freq_equip
)
summary(freq_zinb)


AIC(freq_pois, freq_model, freq_zinb)

#severity modelling
#weibull 
sev_weibull <- survreg(
  Surv(claim_amount, rep(1, nrow(sev_equip))) ~ 
    equipment_age + maintenance_int + usage_int + solar_system + equipment_type,
  dist = "weibull",
  data = sev_equip
)

summary(sev_weibull)

#lognormal
sev_log <- lm(
  log(claim_amount) ~ equipment_age + maintenance_int +
    usage_int + solar_system + equipment_type,
  data = sev_equip
)
summary(sev_log)

predict_data$pred_severity <- exp(
  predict(sev_log, newdata = predict_data) 
)

#diagnostic plots
par(mfrow = c(2,2))
plot(sev_log) 

AIC(sev_weibull, sev_log)

###monte carlo - simulating for 10000 trials using neg binom
systems <- unique(predict_data$solar_system)
set.seed(123)
n_sim <- 10000
varN   <- var(freq_equip$claim_count)
varX   <- var(sev_equip$claim_amount)

lambda <- mean(freq_equip$claim_count)

#negbinomial parmater 
size <- lambda^2 / (varN - lambda)

muX    <- mean(sev_equip$claim_amount)

E_S <- lambda * muX

Var_S <- lambda * varX + varN * (muX^2)

#

results_sim <- data.frame()

meanlog <- coef(sev_log)[1]
sdlog <- summary(sev_log)$sigma

for(sys in systems){
  
  sys_freq <- sum(predict_data$pred_frequency[predict_data$solar_system == sys])
  
  agg_loss <- numeric(n_sim)
  
  for(i in 1:n_sim){
    
    N <- rnbinom(1, size = size, mu = sys_freq)
    
    if(N > 0){
      severities <- rlnorm(N, meanlog, sdlog)
      agg_loss[i] <- sum(severities)
    } else {
      agg_loss[i] <- 0
    }
  }
  
  results_sim <- rbind(results_sim, data.frame(
    solar_system = sys,
    expected_value = mean(agg_loss),
    sd = sd(agg_loss),
    VaR_99 = quantile(agg_loss, 0.99),
    TVaR_99 = mean(agg_loss[agg_loss > quantile(agg_loss, 0.99)])
  ))
}

results_sim

results_sim <- results_sim %>%
  mutate(
    lambda_est = (TVaR_99 - expected_value) / sd
  )
###risk indices 
risk_index$solar_system <- recode(risk_index$solar_system,
  "Helionis" = "Helionis Cluster",
  "Bayesia" = "Zeta",
  "OrynDelta" = "Epsilon"
)
risk_index <- risk_index %>%
  mutate(equipment_type = str_replace_all(equipment_type,
    c("Fexstram Carriers" = "FexStram Carrier",
      "Flux Riders" = "Flux Rider",
      "Graviton Extractors" = "Graviton Extractor",
      "Ion Pulverizers" = "Ion Pulverizer",
      "Quantum Bores" = "Quantum Bore")))

predict_data <- predict_data %>%
  left_join(risk_index, by = c("equipment_type","solar_system"))

#expected loss and adjusting for risk
predict_data$expected_loss <- predict_data$pred_frequency * predict_data$pred_severity
predict_data$expected_loss_risk <- predict_data$expected_loss * (1 + predict_data$risk_index)

#growth rates, inflation and spot rates
growth_rates <- c("Helionis Cluster" = 0.0226, "Zeta" = 0.0226, "Epsilon" = 0.01405)

inflation <- 0.0423
spot_1yr <- 0.0474
spot_10yr <- 0.051
predict_data$growth_rate <- growth_rates[predict_data$solar_system]

#####1 year growth rate
predict_data$short_term_cost <- predict_data$expected_loss_risk * (1 + inflation) *
  (1 + predict_data$growth_rate) /  (1 + spot_1yr)

####long term
predict_data$long_term_cost <- predict_data$expected_loss_risk *(1 + inflation)^10 *
  (1 + predict_data$growth_rate)^10 / (1 + spot_10yr)^10

cost_summary <- predict_data %>%
  group_by(solar_system) %>%
  summarise(
    short_term = sum(short_term_cost),
    long_term = sum(long_term_cost)
  )


#standard deviation premiump principle
loading_map <- setNames(results_sim$lambda_est, results_sim$solar_system)
predict_data$loading <- loading_map[predict_data$solar_system]
predict_data$loading <- pmin(pmax(predict_data$loading, 0.3), 1.5)
predict_data$variance_loss <- predict_data$pred_frequency * varX + varN * (predict_data$pred_severity^2)
predict_data$sd_loss <- sqrt(predict_data$variance_loss)


#premium cost and revenue simulation
set.seed(123)
mc_financials <- data.frame()

for(sys in systems){

  sys_data <- predict_data[predict_data$solar_system == sys, ]
  
  agg_cost_short <- numeric(n_sim)
  agg_cost_long  <- numeric(n_sim)

  agg_premium_short <- numeric(n_sim)
  agg_premium_long  <- numeric(n_sim)

  for(i in 1:n_sim){

    total_loss_short <- 0
    total_loss_long  <- 0

    for(j in 1:nrow(sys_data)){

      N <- rnbinom(1, size = size, mu = sys_data$pred_frequency[j])

      if(N > 0){
        severities <- rlnorm(N, meanlog, sdlog)
        loss <- sum(severities)
      } else {
        loss <- 0
      }

      #short term cost
      cost_short <- loss * (1 + sys_data$risk_index[j]) *
                    (1 + inflation) *
                    (1 + sys_data$growth_rate[j]) /
                    (1 + spot_1yr)

      #long term cost
      cost_long <- loss * (1 + sys_data$risk_index[j]) *
                   (1 + inflation)^10 *
                   (1 + sys_data$growth_rate[j])^10 /
                   (1 + spot_10yr)^10

      #standard deviation loading
      sd_loss <- sqrt(sys_data$variance_loss[j])

      premium_short <- cost_short + sys_data$loading[j] * sd_loss
      premium_long  <- cost_long  + sys_data$loading[j] * sd_loss * sqrt(10)

      total_loss_short <- total_loss_short + cost_short
      total_loss_long  <- total_loss_long  + cost_long

      agg_premium_short[i] <- agg_premium_short[i] + premium_short
      agg_premium_long[i]  <- agg_premium_long[i]  + premium_long
    }

    agg_cost_short[i] <- total_loss_short
    agg_cost_long[i]  <- total_loss_long
  }

  mc_financials <- rbind(mc_financials, data.frame(
    solar_system = sys,

    #costs
    cost_mean_short = mean(agg_cost_short),
    cost_mean_long  = mean(agg_cost_long),

    cost_95_low_short = quantile(agg_cost_short, 0.025),
    cost_95_high_short = quantile(agg_cost_short, 0.975),

    cost_95_low_long = quantile(agg_cost_long, 0.025),
    cost_95_high_long = quantile(agg_cost_long, 0.975),

    #premiums
    premium_mean_short = mean(agg_premium_short),
    premium_mean_long  = mean(agg_premium_long),

    premium_95_low_short = quantile(agg_premium_short, 0.025),
    premium_95_high_short = quantile(agg_premium_short, 0.975),

    premium_95_low_long = quantile(agg_premium_long, 0.025),
    premium_95_high_long = quantile(agg_premium_long, 0.975),

    #net revenue
    net_revenue_mean_short = mean(agg_premium_short - agg_cost_short),
    net_revenue_mean_long  = mean(agg_premium_long - agg_cost_long),

    net_revenue_95_low_short = quantile(agg_premium_short - agg_cost_short, 0.025),
    net_revenue_95_high_short = quantile(agg_premium_short - agg_cost_short, 0.975),

    net_revenue_95_low_long = quantile(agg_premium_long - agg_cost_long, 0.025),
    net_revenue_95_high_long = quantile(agg_premium_long - agg_cost_long, 0.975)
  ))
}

mc_financials


#stress testing
scenario_impacts <- list(
  "Helionis Cluster" = data.frame(
    scenario = c("Prolonged maintenance failure", 
                 "High utilisation surge"),
    freq_multiplier = c(1.4, 1.5),
    sev_multiplier  = c(1.3, 1.2)
  ),
  "Zeta" = data.frame(
    scenario = c("Radiation storm", 
                 "Temporary shutdown/restart cycles"),
    freq_multiplier = c(1.7, 1.5),
    sev_multiplier  = c(1.6, 1.4)
  ),
  "Epsilon" = data.frame(
    scenario = c("Orbital instability", 
                 "Extreme extraction conditions"),
    freq_multiplier = c(2.2, 1.8),
    sev_multiplier  = c(1.7, 1.5)
  )
)

predict_data <- predict_data %>%
  mutate(
    freq_scenario = case_when(
      solar_system == "Helionis Cluster" ~ pred_frequency * scenario_impacts[["Helionis Cluster"]]$freq_multiplier[1],
      solar_system == "Zeta"            ~ pred_frequency * scenario_impacts[["Zeta"]]$freq_multiplier[1],
      solar_system == "Epsilon"         ~ pred_frequency * scenario_impacts[["Epsilon"]]$freq_multiplier[1],
      TRUE                              ~ pred_frequency
    ),
    sev_scenario = case_when(
      solar_system == "Helionis Cluster" ~ pred_severity * scenario_impacts[["Helionis Cluster"]]$sev_multiplier[1],
      solar_system == "Zeta"            ~ pred_severity * scenario_impacts[["Zeta"]]$sev_multiplier[1],
      solar_system == "Epsilon"         ~ pred_severity * scenario_impacts[["Epsilon"]]$sev_multiplier[1],
      TRUE                              ~ pred_severity
    )
  )

scenario_stats <- data.frame()

for(sys in names(scenario_impacts)) {
  
  sys_data <- predict_data[predict_data$solar_system == sys, ]
  scenarios <- scenario_impacts[[sys]]
  
  for(i in 1:nrow(scenarios)) {
    
    freq_adj <- sys_data$pred_frequency * scenarios$freq_multiplier[i]
    sev_adj  <- sys_data$pred_severity  * scenarios$sev_multiplier[i]
    
    expected_loss <- sum(freq_adj * sev_adj)
    
    var_agg <- sum(freq_adj * var(sev_adj) + var(freq_adj) * (mean(sev_adj)^2))
    
    sd_loss <- sqrt(var_agg)

    scenario_stats <- rbind(scenario_stats, data.frame(
      solar_system = sys,
      scenario     = scenarios$scenario[i],
      expected_loss = expected_loss,
      sd_loss       = sd_loss
    ))
  }
}

scenario_stats


#scenario testing

#best case
best_case <- predict_data
best_case$freq <- best_case$pred_frequency * 0.75
best_case$sev  <- best_case$pred_severity * 0.75
best_case$loss <- best_case$freq * best_case$sev


#moderate case
moderate_case <- predict_data
moderate_case$freq <- moderate_case$pred_frequency * 1.1
moderate_case$sev  <- moderate_case$pred_severity * 1.1
moderate_case$loss <- moderate_case$freq * moderate_case$sev


#worst case
worst_case <- predict_data
worst_case$freq <- worst_case$pred_frequency * 2
worst_case$sev  <- worst_case$pred_severity * 1.5
worst_case$loss <- worst_case$freq * worst_case$sev



#table summary
scenario_summary <- data.frame(
  Scenario = c("Best Case", "Moderate Case", "Worst Case"),
  Total_Loss = c(
    sum(best_case$loss),
    sum(moderate_case$loss),
    sum(worst_case$loss)
  )
)

scenario_summary

scenario_by_system <- bind_rows(
  best_case %>% mutate(Scenario = "Best"),
  moderate_case %>% mutate(Scenario = "Moderate"),
  worst_case %>% mutate(Scenario = "Worst")
) %>%
  group_by(Scenario, solar_system) %>%
  summarise(total_loss = sum(loss), .groups = "drop")

scenario_by_system


