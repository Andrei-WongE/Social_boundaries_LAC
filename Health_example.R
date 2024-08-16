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

# Minimize the conditional entropy loss function: ----
# Maximize conditional entropy with respect to epsilon
loss_function <- function(V, epsilon) -ConditionalEntropy(V)

eps_optim <- optim(median(abs(phi_diffs)), function(e) {
  e_vij <- ComputeSimVij(phi_diffs, epsilon = e)
  loss_function(e_vij, epsilon = e)
}, method = "Brent", lower = 0.0001, upper = 5.0)

optim_e <- eps_optim$par #optimun difference threshold


# The resulting difference threshold ϵCE = 0.9282174, the a Monte Carlo estimates 
# of the difference probabilities is computed:
optim_e_vij <- ComputeSimVij(phi_diffs, epsilon = optim_e)

# Plotting the ϵCE difference probabilities
(optim_e_vij_hist <- ggplot() +
    geom_histogram(aes(x = optim_e_vij), fill = "dodgerblue", color = "black",
                   breaks = seq(0, 1, by = .05)) +
    lims(x = c(0, 1)) +
    labs(x = paste0("v_ij(", round(optim_e, digits = 3), ")")) +
    theme_bw()
)

# Bayesian FDR Control: ----
# select cutoff t in d(i, j) = I(v_ij > t) to control FDR and minimize FNR
optim_e_vij_order <- order(optim_e_vij, decreasing = F)
eta <- .05
t_seq_length <- 10000

t_seq <- seq(0, max(optim_e_vij) - .001, length.out = t_seq_length)
t_FDR <- vapply(t_seq, function(t) FDR_estimate(optim_e_vij, t, e = 0), numeric(1))
t_FNR <- vapply(t_seq, function(t) FNR_estimate(optim_e_vij, t, e = 0), numeric(1))

optim_t <- min(c(t_seq[t_FDR <= eta], 1))

# As result, the chosen threshold is t⋆(ϵCE) = 0.8815059

# Final reported disparities: ----
decisions <- logical(nrow(ij_list))
decisions[optim_e_vij >= optim_t] <- TRUE

# proportion of boundaries reported as spatial disparity
mean(decisions)
# [1] 0.0792676 vs # [1] 0.08313431 in PDF [CHECK!!]

# Checking stability of rankings s with respect to choice of the difference threshold: -----
# plotting the rankings of the difference probabilities under different values of 
# the difference threshold ϵ and compare to those previously computed using ϵCE.
optim_e_vij_order <- order(optim_e_vij, decreasing = F)

rej_indx <- optim_e_vij_order[optim_e_vij_order %in%
                                which(optim_e_vij >= optim_t)]

detected_borders <- ij_list[rej_indx,]

node1_all <- cancer_smoking_sf[detected_borders$i,]
node2_all <- cancer_smoking_sf[detected_borders$j,]

intersections <- lapply(seq_len(sum(decisions)), function(i) {
  node1 <- node1_all[i,]
  node2 <- node2_all[i,]
  suppressMessages(st_intersection(st_buffer(node1, 0.0003),
                                   st_buffer(node2, 0.0003)))
}) %>%
  do.call(rbind, .)

intersections$rank <- sort(seq_len(sum(decisions))
                           , decreasing = TRUE) # intersections a sf data.frame


## Now using 4 different difference threshold values
e2 <- round(optim_e / 3, digits = 3)
e3 <- round(optim_e / 1.5, digits = 3)
e4 <- round(optim_e * 1.5, digits = 3)
e5 <- round(optim_e * 3, digits = 3)

e2_vij <- ComputeSimVij(phi_diffs, epsilon = e2)
e3_vij <- ComputeSimVij(phi_diffs, epsilon = e3)
e4_vij <- ComputeSimVij(phi_diffs, epsilon = e4)
e5_vij <- ComputeSimVij(phi_diffs, epsilon = e5)

# Note: the R order function returns permutation index to sort vector, NOT vector of ranks
optim_e_vij_order <- order(optim_e_vij, decreasing = F)
e2_vij_order <- order(e2_vij[optim_e_vij_order], decreasing = F)
e3_vij_order <- order(e3_vij[optim_e_vij_order], decreasing = F)
e4_vij_order <- order(e4_vij[optim_e_vij_order], decreasing = F)
e5_vij_order <- order(e5_vij[optim_e_vij_order], decreasing = F)

rejection_path <- data.table(
  optim_e_order = seq_along(optim_e_vij),
  e2_vij_order = e2_vij_order,
  e3_vij_order = e3_vij_order,
  e4_vij_order = e4_vij_order,
  e5_vij_order = e5_vij_order,
  classified_diff = decisions[optim_e_vij_order])

rejection_path <- melt(rejection_path,
                       id.vars = c("optim_e_order", "classified_diff"),
                       variable.name = "order_type",
                       value.name = "order")

rejection_path[, order_type := fcase(
  order_type == "e2_vij_order", paste0("eps = ", e2),
  order_type == "e3_vij_order", paste0("eps = ", e3),
  order_type == "e4_vij_order", paste0("eps = ", e4),
  order_type == "e5_vij_order", paste0("eps = ", e5)
)]

# Plotting paths
(vij_order_graph <- ggplot() +
  geom_point(data = rejection_path,
             aes(x = optim_e_order, y = order, color = classified_diff,
                 shape = classified_diff),
             alpha = 0.3, size = 1) +
  geom_vline(xintercept = nrow(ij_list) - sum(optim_e_vij == 1)) +
  facet_grid(~order_type) +
  labs(x = paste0("Rank of v_ij(", round(optim_e, digits = 3), ")"),
       y = "Rank of v_ij(eps)") +
  theme_bw() +
  scale_color_manual(name = paste0("eps = ", round(optim_e, digits = 3)),
                     labels = c("Not classified difference", "Classified difference"),
                     values = c("FALSE" = "darkgrey", "TRUE" = "dodgerblue")) +
  scale_shape_manual(name = paste0("eps = ", round(optim_e, digits = 3)),
                     labels = c("Not classified difference", "Classified difference"),
                     values = c(2, 7)) +
  guides(colour = guide_legend(override.aes = list(size = 2, alpha = 1)),
         shape = guide_legend(override.aes = list(size = 2, alpha = 1)))
)

# Posterior Means and Boundaries Map: ----
yfit_pmeans <- colMeans(YFit_sim)

# Building sf database of yfit
yfit_pmeans_sf <- cbind(y_pmeans = cut(  yfit_pmeans
                                       , cut_pts
                                       , right = FALSE
                                       , include.lowest = TRUE),
                        cancer_smoking_sf)

# Plotting boundaries
(yfit_pmeans_map <- ggplot() +
  geom_sf(data = county_sf, col = "gray") +
  geom_sf(data = yfit_pmeans_sf, aes(fill = y_pmeans), col = "gray") +
  scale_fill_viridis_d(name = "Y posterior mean", drop = FALSE) +
  geom_sf(data = intersections, col = "red", alpha = 0, lwd = 0.4) +
  coord_sf(crs = st_crs(5070)) +
  theme_bw() +
  theme(legend.position = "bottom", legend.title=element_text(size=10))
)

# 95% credible intervals of all parameters except γ:----
params_sim <- cbind(samps$beta, samps$sigma2, samps$rho)

alpha <- 0.05

params_summary <- apply(params_sim, MARGIN = 2, function(x) {
  data.table(mean = mean(x), lower = quantile(x, alpha / 2),
             upper = quantile(x, 1 - alpha / 2))
}) %>%
  do.call(rbind, .)

round_digits <- 3

params_summary[, ':='(
  Parameter = c("$\\beta_0$", "$\\beta_1$", "$\\sigmaˆ2$", "$\\rho$"),
  Description = c("Intercept", "Smoking prevalence", "Total variance", "Spatial proportion of variance"),
                  'Mean' = round(mean, digits = round_digits),
                  '95\\% Credible Interval' = paste0("(", round(lower, digits = round_digits), ", ",
                                                     round(upper, digits = round_digits), ")")
  )]

params_summary <- params_summary[ , .(Parameter, Description, 'Mean', '95\\% Credible Interval')]

rownames(params_summary) <- NULL

print(xtable(  params_summary
             , type = "latex"
             , caption = "Posterior summaries of non-spatial effect parameters."
             , label = "tab:RDA_post_summaries"
             , align = c("c", "c", "c", "c", "c")
             )
             , type = "latex"
             , sanitize.text.function = function(x) {x},
                  include.rownames = FALSE,
                  file = here( "Output", "params_summary.tex")
    )


# Reported spatial disparities in lung cancer mortality between neighboring US counties. ----
rankings <- order(optim_e_vij[decisions], decreasing = TRUE)

node1_indx <- ij_list[decisions,]$i[rankings]
node2_indx <- ij_list[decisions,]$j[rankings]

(disparity_df <- data.table(
                `County 1` = cancer_smoking_sf$location[node1_indx],
                `County 2` = cancer_smoking_sf$location[node2_indx],
                `$v_k(\\epsilon_{CE})$` = optim_e_vij[decisions][rankings]
                          )
)

print(xtable(disparity_df
             , type = "latex"
             , digits = 3
             , caption = "Reported spatial disparities in lung cancer mortality between neighboring US."
             , label = "tab:RDA_disparities_table"
             , align = c("c", "c", "c", "c")
             )
       , type = "latex"
       , sanitize.text.function = function(x) {x}
       , include.rownames = TRUE
       , tabular.environment = "longtable"
      )
