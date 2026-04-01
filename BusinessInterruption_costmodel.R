
# Install and load packages

  # install.packages("readxl", "dplyr", "MASS", "ggplot2", "evir", "evd")

  library(readxl)
  library(dplyr)
  library(MASS)
  library(ggplot2)
  library(evir)
  library(evd)


# Data Download
  # Replace file path with local device configuration

  freqdata <- read_excel("~/Desktop/UNSW/ACTL 4001/SOA_2026_Case_Study_Materials/srcsc-2026-claims-business-interruptionLIMIT_clean.xlsx"
                       , sheet = "freq clean")

  sevdata <- read_excel("~/Desktop/UNSW/ACTL 4001/SOA_2026_Case_Study_Materials/srcsc-2026-claims-business-interruptionLIMIT_clean.xlsx"
                       , sheet = "sev clean")

# Seed
  
  set.seed(333)
  
# Initial Data Exploration
  # Defining functions to measure tail risk
  
  VaR <- function(x, alpha = 0.99) {
  quantile(x, alpha, names = FALSE)
  }

  TVaR <- function(x, alpha = 0.99) {
  q <- VaR(x, alpha)
  mean(x[x > q])
  }
  
  # Resulting summary statistics
  
  summary_stats_freq <- freqdata %>%
    group_by(solar_system) %>%
    summarise(
      EX     = mean(claim_count),
      VarX   = var(claim_count),
      VaR  = VaR(claim_count, 0.95),
      TVaR = TVaR(claim_count, 0.95)
    )
  
  summary_stats_sev <- sevdata %>%
    group_by(solar_system) %>%
    summarise(
      EX     = mean(claim_amount),
      VarX   = var(claim_amount),
      VaR  = VaR(claim_amount, 0.95),
      TVaR = TVaR(claim_amount, 0.95)
    )
  
  # Data cleaning and correlation checks 
  
  vars <- c("solar_system",
            "production_load",
            "energy_backup_score",
            "supply_chain_index",
            "avg_crew_exp",
            "maintenance_freq",
            "safety_compliance",
            "exposure")
  
  vars1 <- c("production_load",
            "energy_backup_score",
            "supply_chain_index",
            "avg_crew_exp",
            "maintenance_freq",
            "safety_compliance",
            "exposure")

  freqdata$production_load<-as.numeric(freqdata$production_load)
  freqdata$energy_backup_score<-as.numeric(freqdata$energy_backup_score)
  freqdata$supply_chain_index<-as.numeric(freqdata$supply_chain_index)
  freqdata$avg_crew_exp<-as.numeric(freqdata$avg_crew_exp)
  freqdata$maintenance_freq<-as.numeric(freqdata$maintenance_freq)
  freqdata$safety_compliance<-as.numeric(freqdata$safety_compliance)
  freqdata$exposure<-as.numeric(freqdata$exposure)

  cor_matrix <- cor(freqdata[vars1], use = "complete.obs")
  
  cor_matrix # no significant linear correlation found

# Frequency Modelling (Neg-Bin GLM)
  
  # Negative Binomial GLM selected due to the distribution of summary statistics

  freqmodel <-glm.nb(claim_count ~ energy_backup_score + supply_chain_index + maintenance_freq + offset(log(exposure)), 
                     data = freqdata)
  
  summary(freqmodel)
  
  theta <- freqmodel$theta
  
  predicted_claims <- predict(freqmodel, newdata=freqdata, type = "response")
 
  
  # Aggregating results by solar system 

  freq_by_system <- freqdata %>%
    mutate(predicted_claims = predicted_claims) %>%
    group_by(solar_system) %>%
    summarise(
      total_claims = sum(predicted_claims, na.rm = TRUE),
      mean_frequency = mean(predicted_claims, na.rm =  TRUE)
    )
  
  h_expectedfreq <- freq_by_system[[2,3]]
  e_expectedfreq <- freq_by_system[[1,3]]
  z_expectedfreq <- freq_by_system[[4,3]]
  
  # Aggregating results across the portfolio 
  
  freq_agg <- freqdata %>%
    mutate(predicted_claims = predicted_claims) %>%
    summarise(
      total_claims = sum(predicted_claims, na.rm = TRUE),
      mean_frequency = mean(predicted_claims, na.rm =  TRUE)
    )
  
  expectedfreq <- freq_agg[[1,2]]
  
# Severity: Body (Log-normal GLM)
  
  # Data preparation 
  
  sevdata$production_load<-as.numeric(sevdata$production_load)
  sevdata$claim_amount<-as.numeric(sevdata$claim_amount)
  sevdata$energy_backup_score<-as.numeric(sevdata$energy_backup_score)
  sevdata$safety_compliance<-as.numeric(sevdata$safety_compliance)
  sevdata$exposure<-as.numeric(sevdata$exposure)
  
  # Defining the Body and Tail of the data
  
  threshold <- quantile(sevdata$claim_amount, probs = 0.95, na.rm=TRUE)
  
  body <- sevdata[sevdata$claim_amount <= threshold, ]
  tail <- sevdata[sevdata$claim_amount > threshold, ]

  n1 <- nrow(body)
  n2 <- nrow(tail)
  
  # Investigation into the body of the data
  
  body %>%
    filter(solar_system != "NA") %>%
    ggplot(aes(x = claim_amount)) +
    geom_histogram(bins = 30, fill = "lightblue", color = "black") +
    facet_wrap(~ solar_system, scales = "free") +
    labs(title = "Histogram of Body Claims by Solar System",
         x = "Claim Amount",
         y = "Count") +
    theme_minimal()
  
  # Log-normal statistical distribution selection 
  ## Note a GLM fitting was attempted but only one of the provided variables were found to be statistically significant
 
    bodymodel <- glm(log(claim_amount) ~ solar_system + production_load + energy_backup_score + safety_compliance,
                   data = body, gaussian()) # no exposure since it is already accounted for in freq
  
    summary(bodymodel)
    
  # Final model 
    
  fit_body <- fitdistr(body$claim_amount, "lognormal")
  
  mu <- fit_body$estimate["meanlog"]
  sigma <- fit_body$estimate["sdlog"]
  
  expectedbody_perclaim <- exp(mu + (sigma^2)/2)
  expectedbody_perclaim
  
# Severity: Tail (GPD)
  
  # Investigation into the tail distribution 
  
  tail %>%
    filter(solar_system != "NA") %>%
    ggplot(aes(x = claim_amount)) +
    geom_histogram(bins = 30, fill = "lightblue", color = "black") +
    facet_wrap(~ solar_system, scales = "free") +  # one plot per category
    labs(title = "Histogram of Tail Claims by Solar System",
         x = "Claim Amount",
         y = "Count") +
    theme_minimal()
  
  table(tail$solar_system)
  
  tail <- tail[tail$solar_system != 'NA',]
 
   
  # Final model
  ## This is done per solar system 
  
  epsilon <- sevdata[sevdata$solar_system == "Epsilon",]
  zeta <- sevdata[sevdata$solar_system == "Zeta",]
  helionis <- sevdata[sevdata$solar_system == "Helionis Cluster",]
  
  h_prob <- nrow(helionis[helionis$claim_amount <= threshold,])/nrow(helionis)
  z_prob <- nrow(zeta[zeta$claim_amount <= threshold,])/nrow(zeta)
  e_prob <- nrow(epsilon[epsilon$claim_amount <= threshold,])/nrow(epsilon)
  prob <- nrow(sevdata[sevdata$claim_amount <= threshold,])/nrow(sevdata)
  
  fitepsilon_tail <- gpd(epsilon$claim_amount, threshold = threshold)
  fitzeta_tail <- gpd(zeta$claim_amount, threshold = threshold)
  fithelionis_tail <- gpd(helionis$claim_amount, threshold = threshold)
  
  helionis_tail <- helionis$claim_amount[helionis$claim_amount > threshold]
  epsilon_tail <- epsilon$claim_amount[epsilon$claim_amount > threshold ]
  zeta_tail <- zeta$claim_amount[zeta$claim_amount > threshold ]
  
  ## Plot Epsilon and Zeta tail dist to see if tails are heavy enough (due to NA's in standard error )

  qqplot(qgpd(ppoints(length(epsilon_tail)), fitepsilon_tail$par.ests["xi"], fitepsilon_tail$par.ests["beta"]), epsilon_tail - threshold)
  qqplot(qgpd(ppoints(length(zeta_tail)), fitzeta_tail$par.ests["xi"], fitzeta_tail$par.ests["beta"]), zeta_tail - threshold)
  
  # Final Model 
  
  h_xi <- fithelionis_tail$par.ests["xi"]
  h_beta <- fithelionis_tail$par.ests["beta"]
  
  h_expectedtail_perclaim <- threshold + h_beta / (1 - h_xi)
  
  e_xi <- fitepsilon_tail$par.ests["xi"]
  e_beta <- fitepsilon_tail$par.ests["beta"]
  
  e_expectedtail_perclaim <- threshold + e_beta / (1 - e_xi)
  
  z_xi <- fitzeta_tail$par.ests["xi"]
  z_beta <- fitzeta_tail$par.ests["beta"]
  
  z_expectedtail_perclaim <- threshold + z_beta / (1 - z_xi)
  
  # Final estimates per solar system (historic)
  
  h_expectedsev <- h_expectedtail_perclaim*(1-h_prob) + expectedbody_perclaim*h_prob
  h_expectedsev
  
  e_expectedsev <- e_expectedtail_perclaim*(1-e_prob) + expectedbody_perclaim*e_prob
  e_expectedsev
  
  z_expectedsev <- z_expectedtail_perclaim*(1-z_prob) + expectedbody_perclaim*z_prob
  z_expectedsev
  
  
# Mapping Historical systems to current 
  
  # Frequency data exploration 

  freqdata %>%
  filter(solar_system != "NA") %>%
  ggplot(aes(x = energy_backup_score)) +
    geom_histogram(bins=30) +
    facet_wrap(~ solar_system) +
    theme_minimal()
  
  freqdata %>%
    filter(solar_system != "NA") %>%
  ggplot(aes(x = supply_chain_index)) +
    geom_histogram(bins=30) +
    facet_wrap(~ solar_system) +
    theme_minimal()
  
  freqdata %>%
    filter(solar_system != "NA") %>%
  ggplot(aes(x = maintenance_freq)) +
    geom_histogram(bins=30) +
    facet_wrap(~ solar_system) +
    theme_minimal()
  
  ## Severity data exploration
  
  ggplot(sevdata, aes(x = energy_backup_score)) +
    geom_histogram(bins=30) +
    facet_wrap(~ solar_system) +
    theme_minimal()
  
  ggplot(sevdata, aes(x = production_load)) +
    geom_histogram(bins=30) +
    facet_wrap(~ solar_system) +
    theme_minimal()
  
  ggplot(sevdata, aes(x = safety_compliance)) +
    geom_histogram(bins=30) +
    facet_wrap(~ solar_system) +
    theme_minimal()
  
  # Mapping Beta System and Oryn Delta parameters based on Helionis Cluster and Zeta
  
  b_xi <- 0.2*h_xi + 0.8*z_xi
  b_beta <- 0.2*h_beta + 0.8*z_beta
  
  o_xi <- 0.9*h_xi + 0.1*z_xi
  o_beta <- 0.9*h_beta + 0.1*z_beta
  
  b_expectedfreq <- 0.2*h_expectedfreq + 0.8*z_expectedfreq
  o_expectedfreq <- 0.9*h_expectedfreq + 0.1*z_expectedfreq
  
  b_expectedsev <- 0.2*h_expectedsev + 0.8*z_expectedsev
  o_expectedsev <- 0.9*h_expectedsev + 0.1*z_expectedsev
  
  b_prob <- 0.2*h_prob + 0.8*z_prob
  o_prob <- 0.9*h_prob + 0.1*z_prob
  
# Monte Carlo Simulation in order to project expected cost per exposure
  
  # Parameter setup
  
  set.seed(333)
  n_sim <- 100000
  total_losses <- numeric(n_sim)
  
  base_loading <- data.frame(
    helionis = 0.15,
    bayesia = 0.15,
    oryn = 0.2
  )
  
  L <- 1500000
  
  # Helionis Cluster

  for (i in 1:n_sim){
    N_claims <- rnbinom(1, mu = h_expectedfreq, size = theta)
    
    if(N_claims == 0){
      total_losses[i] <- 0
    } else {
      u <- runif(N_claims)
      
      body_idx <- u < h_prob
      tail_idx <- !body_idx
      
      severities <- numeric(N_claims)
      
      severities[body_idx] <- rlnorm(sum(body_idx), mu, sigma)
      
      severities[tail_idx] <- threshold + rgpd(sum(tail_idx), h_xi, h_beta)
      
      severities <- pmin(severities, L)
      
      total_losses[i] <- sum(severities)
    }
  }
  
  h_results <- data.frame(
  Mean = mean(total_losses),
  sd = sqrt(var(total_losses)),
  VaR_99 = quantile(total_losses, 0.99),
  TVaR_99 = mean(total_losses[total_losses > quantile(total_losses, 0.99)]),
  Pct_5 = quantile(total_losses, 0.025),
  Pct_95 = quantile(total_losses, 0.975)
  )
  
  h_results
  
  h_premium <- h_results$Mean + base_loading$helionis * h_results$sd
  
  h_premium_sim <- total_losses + base_loading$helionis * h_results$sd
  
  h_net_revenue <- h_premium - total_losses
  
  h_premium_results <- data.frame(
    Mean = mean(h_premium_sim),
    SD = sd(h_premium_sim),
    Pct_5 = quantile(h_premium_sim, 0.025),
    Pct_95 = quantile(h_premium_sim, 0.975)
  )
  
  h_net_results <- data.frame(
    Mean = mean(h_net_revenue),
    SD = sd(h_net_revenue),
    Pct_5 = quantile(h_net_revenue, 0.025),
    Pct_95 = quantile(h_net_revenue, 0.975)
  )
  
  h_premium_results
  h_net_results
 
  # Oryn Delta
  
  for (i in 1:n_sim){
    N_claims <- rnbinom(1, mu = o_expectedfreq, size = theta)
    
    if(N_claims == 0){
      total_losses[i] <- 0
    } else {
      u <- runif(N_claims)
      
      body_idx <- u < o_prob
      tail_idx <- !body_idx
      
      severities <- numeric(N_claims)
      
      severities[body_idx] <- rlnorm(sum(body_idx), mu, sigma)
      
      severities[tail_idx] <- threshold + rgpd(sum(tail_idx), o_xi, o_beta)
      
      severities <- pmin(severities, L)
      
      total_losses[i] <- sum(severities)
    }
  }
  
  o_results <- data.frame(
    Mean = mean(total_losses),
    sd = sqrt(var(total_losses)),
    VaR_99 = quantile(total_losses, 0.99),
    TVaR_99 = mean(total_losses[total_losses > quantile(total_losses, 0.99)]),
    Pct_5 = quantile(total_losses, 0.025),
    Pct_95 = quantile(total_losses, 0.975)
  )
  
  o_results
  
  o_premium <- o_results$Mean + base_loading$oryn * o_results$sd
  o_premium_sim <- total_losses + base_loading$oryn * o_results$sd

  o_net_revenue <- o_premium - total_losses
  
  o_net_results <- data.frame(
    Mean = mean(o_net_revenue),
    SD = sd(o_net_revenue),
    Pct_5 = quantile(o_net_revenue, 0.025),
    Pct_95 = quantile(o_net_revenue, 0.975)
  )
  
  o_premium_results <- data.frame(
    Mean = mean(o_premium_sim),
    SD = sd(o_premium_sim),
    Pct_5 = quantile(o_premium_sim, 0.025),
    Pct_95 = quantile(o_premium_sim, 0.975)
  )
  
  o_net_results
  o_premium_results
  
  # Bayesia Cluster
  
  for (i in 1:n_sim){
    N_claims <- rnbinom(1, mu = b_expectedfreq, size = theta)
    
    if(N_claims == 0){
      total_losses[i] <- 0
    } else {
      u <- runif(N_claims)
      
      body_idx <- u < b_prob
      tail_idx <- !body_idx
      
      severities <- numeric(N_claims)
      
      severities[body_idx] <- rlnorm(sum(body_idx), mu, sigma)
      
      severities[tail_idx] <- threshold + rgpd(sum(tail_idx), b_xi, b_beta)
      
      severities <- pmin(severities, L)
      
      total_losses[i] <- sum(severities)
    }
  }
  
  b_results <- data.frame(
    Mean = mean(total_losses),
    sd = sqrt(var(total_losses)),
    VaR_99 = quantile(total_losses, 0.99),
    TVaR_99 = mean(total_losses[total_losses > quantile(total_losses, 0.99)]),
    Pct_5 = quantile(total_losses, 0.025),
    Pct_95 = quantile(total_losses, 0.975)
  )
  
  b_results
  
  b_premium <- b_results$Mean + base_loading$bayesia * b_results$sd
  b_premium_sim <- total_losses + base_loading$bayesia * b_results$sd
  
  b_net_revenue <- b_premium - total_losses
  
  b_net_results <- data.frame(
    Mean = mean(b_net_revenue),
    SD = sd(b_net_revenue),
    Pct_5 = quantile(b_net_revenue, 0.025),
    Pct_95 = quantile(b_net_revenue, 0.975)
  )
  
  b_premium_results <- data.frame(
    Mean = mean(b_premium_sim),
    SD = sd(b_premium_sim),
    Pct_5 = quantile(b_premium_sim, 0.025),
    Pct_95 = quantile(b_premium_sim, 0.975)
  )
  
  b_net_results
  b_premium_results
  
# Scenario Analysis
  
  # Setup base parameters for reference 
  
  systems <- c("Helionis", "Bayesia", "Oryn Delta")

  freq_base <- c(Helionis = h_expectedfreq, Bayesia = b_expectedfreq, OrynDelta = o_expectedfreq)

  severity_base <- list(
    Helionis = list(logmean = mu, logsd = sigma, gpd_scale = h_xi, gpd_shape = h_beta),
    Bayesia  = list(logmean = mu, logsd = sigma, gpd_scale = b_xi, gpd_shape = b_beta),
    OrynDelta= list(logmean = mu,  logsd = sigma, gpd_scale = o_xi, gpd_shape = o_beta)
  )
  
  # Simulation function to simulate scenarios 
  
  simulate_losses <- function(freq_factor, sev_factor,
                              base_freq, theta,
                              mu, sigma,
                              threshold, xi, beta,
                              prob_body,
                              n_sim = 10000){
    
    freq <- base_freq * freq_factor
    mu_scaled <- mu + log(sev_factor) 
    beta_scaled <- beta * sev_factor
    
    total_losses <- numeric(n_sim)
    
    for(i in 1:n_sim){
      
      N_claims <- rnbinom(1, mu = freq, size = theta)
      
      if(N_claims == 0){
        total_losses[i] <- 0
        
      } else {
        
        u <- runif(N_claims)
        
        body_idx <- u < prob_body
        tail_idx <- !body_idx
        
        severities <- numeric(N_claims)
        
        severities[body_idx] <- rlnorm(sum(body_idx), mu_scaled, sigma)
        
        severities[tail_idx] <- threshold + rgpd(sum(tail_idx), xi, beta_scaled)
        
        severities <- pmin(severities, L)
        
        total_losses[i] <- sum(severities)
      }
    }
    
    results <- data.frame(
      Mean = mean(total_losses),
      sd = sd(total_losses),
      VaR_99 = quantile(total_losses, 0.99),
      TVaR_99 = mean(total_losses[total_losses > quantile(total_losses, 0.99)]),
      Pct_5 = quantile(total_losses, 0.025),
      Pct_95 = quantile(total_losses, 0.975)
    )
    
    return(results)
  }
  
 
  
  # Worst-case scenario for different systems 
  
  set.seed(333)
  
  worst_h <- simulate_losses(1.4, 1.3, h_expectedfreq, theta, mu, sigma, threshold, h_xi, h_beta, h_prob, n_sim = 100000)
  worst_h
  
  worst_b <- simulate_losses(1.4, 1.3, h_expectedfreq, theta, mu, sigma, threshold, b_xi, b_beta, h_prob, n_sim = 100000)
  worst_b
  
  worst_o <- simulate_losses(1.4, 1.3, o_expectedfreq, theta, mu, sigma, threshold, o_xi, o_beta, h_prob, n_sim = 100000)
  worst_o
  
  # Moderate-case scenario for different systems 
  
  set.seed(333)
  
  mod_h <- simulate_losses(1.15, 1.1, h_expectedfreq, theta, mu, sigma, threshold, h_xi, h_beta, h_prob, n_sim = 100000)
  mod_h
  
  mod_b <- simulate_losses(1.15, 1.1, h_expectedfreq, theta, mu, sigma, threshold, b_xi, b_beta, h_prob, n_sim = 100000)
  mod_b
  
  mod_o <- simulate_losses(1.15, 1.1, o_expectedfreq, theta, mu, sigma, threshold, o_xi, o_beta, h_prob, n_sim = 100000)
  mod_o
  
  # Best-case scenario for different systems
  
  set.seed(333)
  
  best_h <- simulate_losses(0.85, 0.9, h_expectedfreq, theta, mu, sigma, threshold, h_xi, h_beta, h_prob, n_sim = 100000)
  best_h
  
  best_b <- simulate_losses(0.85, 0.9, h_expectedfreq, theta, mu, sigma, threshold, b_xi, b_beta, h_prob, n_sim = 100000)
  best_b
  
  best_o <- simulate_losses(0.85, 0.9, o_expectedfreq, theta, mu, sigma, threshold, o_xi, o_beta, h_prob, n_sim = 100000)
  best_o
 
# Stress Testing
  
  # Single system events 
  
  # Setup for different potential scenarios to consider
  ## Note this process has been split by system for business interruption insurance
  
  scenarios <- list(
    H1 = list(freq_mult = 1.3, sev_mult = 1.7),
    B1 = list(freq_mult = 1.8, sev_mult = 1.5),
    B2 = list(freq_mult = 1.4, sev_mult = 2),
    B3 = list(freq_mult = 1.6, sev_mult = 1.2),
    O1 = list(freq_mult = 2.5, sev_mult = 1.7),
    O2 = list(freq_mult = 1.7, sev_mult = 1.7),
    O3 = list(freq_mult = 2, sev_mult = 1.4)
  )
  
  # Simulation function to stress test model
  
  simulate_losses_stress <- function(system = "Helionis", scenario = "baseline", n_sim = 10000){
    
    total_losses <- numeric(n_sim)
    
    if(scenario %in% names(scenarios)){
      if(system %in% names(scenarios[[scenario]])){
        freq_mult <- scenarios[[scenario]][[system]]$freq_mult
        sev_mult  <- scenarios[[scenario]][[system]]$sev_mult
      } else {
        freq_mult <- scenarios[[scenario]]$freq_mult
        sev_mult  <- scenarios[[scenario]]$sev_mult
      }
    } else {
      freq_mult <- 1
      sev_mult  <- 1
    }
    
    for(i in 1:n_sim){
      # simulate stressed number of claims
      N_claims <- rnbinom(1, mu = h_expectedfreq * freq_mult, size = theta)
      
      if(N_claims == 0){
        total_losses[i] <- 0
      } else {
        u <- runif(N_claims)
        body_idx <- u < h_prob
        tail_idx <- !body_idx
        
        severities <- numeric(N_claims)
        severities[body_idx] <- rlnorm(sum(body_idx), mu, sigma) * sev_mult
        severities[tail_idx] <- (threshold + rgpd(sum(tail_idx), h_xi, h_beta)) * sev_mult
        
        severities <- pmin(severities, L)
        
        total_losses[i] <- sum(severities)
      }
    }
    
    results <- data.frame(
      Mean = mean(total_losses),
      sd = sqrt(var(total_losses)),
      VaR_99 = quantile(total_losses, 0.99),
      TVaR_99 = mean(total_losses[total_losses > quantile(total_losses, 0.99)]),
      Pct_5 = quantile(total_losses, 0.025),
      Pct_95 = quantile(total_losses, 0.975)
      
    )
    
    results
  }
  
  # Stress test results
  
  set.seed(333)
  
  loss_H1 <- simulate_losses_stress(system = "Helionis", scenario = "H1")
  loss_H1
  loss_B1 <- simulate_losses_stress(system = "Bayesia", scenario = "B1")
  loss_B1
  loss_B2 <- simulate_losses_stress(system = "Bayesia", scenario = "B1")
  loss_B2
  loss_B3 <- simulate_losses_stress(system = "Bayesia", scenario = "B1")
  loss_B3
  loss_O1 <- simulate_losses_stress(system = "Bayesia", scenario = "B1")
  loss_O1
  loss_O2 <- simulate_losses_stress(system = "Bayesia", scenario = "B1")
  loss_O2
  loss_O3 <- simulate_losses_stress(system = "Bayesia", scenario = "B1")
  loss_O3
  

  # Multi-system events
  
  ## Note this is modelled on an aggregate level 
  
  # Model Tail on an aggregate scale
  
  fit_tail <- gpd(sevdata$claim_amount, threshold = threshold)
  
  xi <- fit_tail$par.ests["xi"]
  beta <- fit_tail$par.ests["beta"]
  
  # Simulate aggregate losses

  simulate_losses_multi <- function(scenario = "baseline", n_sim = 10000){

    systems <- c("Helionis", "Bayesia", "Oryn")
    
    scenario_mult <- list(
      baseline = list(
        Helionis = list(freq_mult = 1, sev_mult = 1),
        Bayesia  = list(freq_mult = 1, sev_mult = 1),
        Oryn     = list(freq_mult = 1, sev_mult = 1)
      ),
      C1 = list(
        Helionis = list(freq_mult = 1.4, sev_mult = 1.4),
        Bayesia  = list(freq_mult = 1.4, sev_mult = 1.4),
        Oryn     = list(freq_mult = 1.4, sev_mult = 1.4)
      ),
      C2 = list(
        Helionis = list(freq_mult = 1.5, sev_mult = 1.2),
        Bayesia  = list(freq_mult = 1.5, sev_mult = 1.2),
        Oryn     = list(freq_mult = 1.5, sev_mult = 1.2)
      ),
      C3 = list(
        Helionis = list(freq_mult = 1.3, sev_mult = 1.6),
        Bayesia  = list(freq_mult = 1.3, sev_mult = 1.6),
        Oryn     = list(freq_mult = 1.3, sev_mult = 1.6)
      )
    )
    
    rho <- matrix(c(
      1.0, 0.25, 0.6,
      0.25, 1.0, 0.4,
      0.6, 0.4, 1.0
    ), nrow = 3, byrow = TRUE)
    
    Z <- mvrnorm(n_sim, mu = c(0,0,0), Sigma = rho)
    U <- pnorm(Z)
    
    total_losses <- matrix(0, nrow = n_sim, ncol = length(systems))
    colnames(total_losses) <- systems
    
    for(i in 1:n_sim){
      for(j in 1:length(systems)){
        
        sys <- systems[j]
        
        freq_mult <- scenario_mult[[scenario]][[sys]]$freq_mult
        sev_mult  <- scenario_mult[[scenario]][[sys]]$sev_mult
        
        N_claims <- qnbinom(U[i,j], mu = expectedfreq * freq_mult, size = theta)
        
        if(N_claims == 0){
          total_losses[i,j] <- 0
        } else {
          
          u2 <- runif(N_claims)
          body_idx <- u2 < prob
          tail_idx <- !body_idx
          
          severities <- numeric(N_claims)
          
          severities[body_idx] <- rlnorm(sum(body_idx), mu, sigma) * sev_mult
          severities[tail_idx] <- (threshold + rgpd(sum(tail_idx), xi, beta)) * sev_mult
          
          severities <- pmin(severities, L)
          
          total_losses[i,j] <- sum(severities)
        }
      }
    }
    
    portfolio_loss <- rowSums(total_losses)
    
    results <- data.frame(
      Mean = mean(portfolio_loss),
      SD = sd(portfolio_loss),
      VaR_99 = quantile(portfolio_loss, 0.99),
      TVaR_99 = mean(portfolio_loss[portfolio_loss > quantile(portfolio_loss, 0.99)]),
      Pct_5 = quantile(total_losses, 0.025),
      Pct_95 = quantile(total_losses, 0.975)
    )
    
    return(results)
  }
  
  # Stress test results 
  
  set.seed(333)
  
  C1_losses <- simulate_losses_multi(scenario = "C1", n_sim = 100000)
  C1_losses
  
  C2_losses <- simulate_losses_multi(scenario = "C2", n_sim = 100000)
  C2_losses
  
  C3_losses <- simulate_losses_multi(scenario = "C3", n_sim = 100000)
  C3_losses
  
   Baseline <- simulate_losses_multi(scenario = "baseline", n_sim = 100000)
  Baseline
  