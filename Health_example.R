# Detection of health disparities
---------------------------------
# Source: [Ky_Wu](https://github.com/Ky-Wu/bayesian_spatial_health_disparities)

# Load required packages ----

if (!require("pacman")|!require("groundhog")| !require("here")) {
  
  install.packages(c("pacman","groundhog", "here"))
}


library("pacman")
library("here")
library("groundhog")

# install.packages("groundhog")
library("groundhog")
set.groundhog.folder(here("groundhog_library"))
groundhog.day = "2024-07-15"
#Dowloaded fromn https://github.com/CredibilityLab/groundhog

pkgs = c("data.table", "stringr", "magrittr", "spdep", "maps", "maptools", 
         "Rcpp", "RcppArmadillo", "ggplot2", "ggpubr", "xtable", "openxlsx")

groundhog.library(pkgs, groundhog.day)

set.seed(1969)

# Read in data and load in custom functions for BYNY2 model sampling:----
# BYM2 Model Sampling functions
source(file.path(getwd(), "src", "R", "bym2_sampling.R"))

# Helper functions to compute posterior probabilities v_ij
source(file.path(getwd(), "src", "R", "vij_computation.R"))

# Helper functions for epsilon-loss bayesian FDR control procedure
source(file.path(getwd(), "src", "R", "eps_loss_FDR.R"))

# Read in and setup lung + smoking data
source(file.path(getwd(), "src", "R", "RDA", "US_data_setup.R")) 
  #Spherical geometry (s2) switched off

X <- cbind(1, cancer_smoking_sf$total_mean_smoking)
y <- cancer_smoking_sf$mortality2014
cut_pts <- quantile(y, seq(0, 1, length = 6))

# Load in Rcpp code for sampling from posterior of BYM2 model:----
# Exact sampling and Gibbs sampling
Rcpp::sourceCpp(file.path(getwd(), "src", "rcpp", "BYM2ExactSampling.cpp"))
Rcpp::sourceCpp(file.path(getwd(), "src", "rcpp", "BYM2_flatbeta_MCMC.cpp"))

## Plotting data
plot(has_data_sf[, "has_data"]
     , main = "Lung Cancer Mortality and Smoking Data Coverage")

(lung_map <- ggplot() +
  geom_sf(data = county_sf) +
  geom_sf(data = cancer_smoking_sf[!is.na(cancer_smoking_sf$mortality2014),],
          aes(fill = cut(mortality2014, cut_pts, right = FALSE,
                         include.lowest = TRUE)), col = "gray") +
  scale_fill_viridis_d(name = "Tracheal, bronchus & lung cancer mortality",
                       drop = FALSE) +
  coord_sf(crs = st_crs(5070)) +
  theme_bw() +
  theme(legend.position = "bottom", legend.title=element_text(size=10)))

# Assessment of Spatial Autocorrelation: ----
# linear regression of lung cancer mortality rate on smoking prevalence to 
# Test for spatial autocorrelation

ols_fit <- lm(y ~ X)
e <- residuals(ols_fit)
lw <- nb2listw(county_nbs, style = "W", zero.policy = TRUE)
e_lag <- lag.listw(lw, e)

plot(e_lag ~ e, pch = 16, asp = 1)
M1 <- lm(e_lag ~ e)
abline(M1, col = "blue")

coef(M1)[2]

moran.mc(e, lw, nsim = 10000, alternative = "greater")

geary.mc(e, lw, nsim = 10000, alternative = "greater")

# Both results (highly significant (p-value = 1/10,001) suggest that spatial autocorrelation is not fully captured by 
# the linear relationship between smoking prevalence and mortality rates. 
# Therefore, we proceed with analyzing the data under a spatial model.

# Set priors and initialize sampler object: ---
# Using Gibbs sampling to sample from the posterior of the BYM2 model

# Priors
a_sigma <- 0.1
b_sigma <- 0.1

# Paper considers the class of penalized complexity (PC) priors introduced in
# Simpson et al. (2017) as a means to encourage model parsimony and construct 
# priors imparting better interpretation. They follow Riebler et al. (2016) by 
# placing a PC prior on 𝜌, denoted by PC(𝜌; 𝜆,𝑉𝜙) ∝ 𝜆 exp(−𝜆𝑑(𝜌;𝑉𝜙)) 
# for 0 ≤ 𝜌 ≤ 1, where 𝜆 > 0 is a fixed hyperparameter.
# Since PC prior on rho, Inverse Gamma (IG) prior parameters will be ignored 
# by sampler object [Why?]
a_rho <- 0.0
b_rho <- 0.0

# limits on possible values of rho
lower_rho <- 0.0
upper_rho <- 1.0

# PC prior hyperparameter selected in pc_prior_selection.Rmd [CHECK!!]
lambda_rho <- 0.0335

# INITIALIZE GIBBS SAMPLER OBJECT
gibbsSampler <- new(BYM2FlatBetaMCMC, X, y, Q_scaled)

# SET PRIORS
gibbsSampler$setPriors(  a_sigma
                       , b_sigma
                       , rho_prior_type = "pc"
                       , a_rho
                       , b_rho
                       , lower_rho
                       , upper_rho
                       , lambda_rho
                      )
# INITIALIZE chain at OLS estimates for beta, 0_n for gamma
# rho = 0.5, sigma2 = SSE / (n - 1)
gibbsSampler$initOLS()

# Burn-in sample, discarding these initial samples, you allow the chain to
# “burn in” and reach a state where it is more likely to be sampling 
# from the true distribution
gibbsSampler$burnMCMCSample(10000)

# DRAW POSTERIOR SAMPLES
n_sim <- 30000
system.time({
  samps <- gibbsSampler$MCMCSample(n_sim)
}, gcFirst = TRUE
)

# user   system  elapsed 
# 238.61     4.72 19335.39 

beta_sim <- samps$beta
gamma_sim <- samps$gamma
sigma2_sim <- samps$sigma2
rho_sim <- samps$rho
YFit_sim <- samps$YFit


# Obtaining the posterior samples of ϕ 

phi_sim <- apply(gamma_sim, MARGIN = 2, function(x) {
  x / sqrt(sigma2_sim * rho_sim)
})

# Compute posterior samples of the differences: ϕk1−ϕk2/Var(ϕk1−ϕk2|y,ρ,σ2)
# ij_list is the list of all pairs of neighboring counties,
# computed during data setup
phi_diffs <- BYM2_StdDiff(phi_sim, rho_sim, Q_scaled, X, ij_list)

# Plotting the posterior samples of ρ:
(rho_hist <- ggplot() +
  geom_histogram(aes(x = rho_sim), fill = "dodgerblue", color = "black",
                 breaks = seq(0, 1, by = .05)) +
  lims(x = c(0, 1)) +
  labs(x = paste0("rho")) +
  theme_bw()
)




