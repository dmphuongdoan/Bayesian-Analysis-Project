library(brms)
library(bayesplot)
library(loo)
library(tidyverse)

set.seed(42)

df_scaled <- readRDS("/home/rstudio/project/df_scaled.rds")
#==============================================================
# SECTION 7: BAYESIAN HIERARCHICAL MODELS (WITH NON-LINEARITY)
#==============================================================
df_scaled <- df_scaled %>%
  mutate(
    year_s = scale(year)[,1]          # scale year thành mean=0, sd=1
  )

#scale variables
cat("New variables added\n")
df_scaled %>%
  dplyr::select(country,life_exp_s,log_gdp_s, health_exp_s, clean_water_s, 
                urban_pop_s, fertility_s, year_s) %>%
  head(10) %>%
  as.data.frame() %>%
  print()

#improved priors 
priors <- c(
  prior(normal(0, 1.5), class = "Intercept"),
  prior(normal(0,1), class = "b"), #for linear coefficient
  prior(exponential(1), class = "sd"), # for random effect (1|country)
  prior(exponential(1), class = "sigma")
)
#---------------------------
# Model A: Linear Baseline (for comparision)
#---------------------------
model_A_linear <- brm(
  formula = life_exp_s ~ log_gdp_s + urban_pop_s + health_exp_s + 
    clean_water_s + fertility_s + (1 | country),
  data = df_scaled,
  family = gaussian(),
  prior = priors,
  chains = 4, 
  iter = 4000, 
  warmup = 2000,
  cores = 4,
  control = list(adapt_delta = 0.95, max_treedepth = 12),
  seed = 42
)

saveRDS(model_A_linear, "/home/rstudio/project/model_A_linear.rds")
cat("Model A Linear completed \n")

#---------------------------
# Model B: Non-linear with Spline (main model)
#---------------------------
# 1. Buộc dùng đúng compiler C++
Sys.setenv(MAKEFLAGS = "CC=gcc CXX=g++")
Sys.setenv(PKG_CXXFLAGS = "-std=gnu++17")

# 2. Reinstall các package gây lỗi
remove.packages(c("rstan", "StanHeaders", "RcppEigen", "BH", "brms"))

install.packages("rstan", repos = "https://mc-stan.org/r-packages/", quiet = TRUE)
install.packages("brms", dependencies = TRUE)

# 3. Tạo file Makevars để fix lâu dài
dir.create("~/.R", showWarnings = FALSE)
cat('
CXX14 = g++ -std=gnu++17
CXX14FLAGS = -O3 -march=native -mtune=native -fPIC
', file = "~/.R/Makevars")
#----------

cat("Running Model B - Non-linear with Splines...\n")

model_B_spline <- brm(
  formula = life_exp_s ~ 
    s(log_gdp_s, bs = "cr", k = 8) + 
    s(fertility_s, bs = "cr", k = 6) + 
    health_exp_s + clean_water_s + urban_pop_s +
    (1 | country),
  
  data = df_scaled,
  family = gaussian(),
  prior = priors,
  chains = 4, 
  iter = 1500,      # giảm tạm để test nhanh (sau khi ổn thì tăng lại 4000)
  warmup = 750,
  cores = 4,
  control = list(adapt_delta = 0.98, max_treedepth = 12),
  seed = 42,
  save_pars = save_pars(all = TRUE)
)

saveRDS(model_B_spline, "/home/rstudio/project/model_B_spline.rds")
cat("Model B Non-linear completed \n")

#---------------------------
# Model C: Non-linear + Time Trend
#---------------------------
install.packages("cmdstanr", repos = c("https://stan-dev.r-universe.dev", getOption("repos")))
cmdstanr::install_cmdstan(cores = 4, quiet = TRUE)

cat("Running Model C - Non-linear + Time trend...\n")
model_C_timetrend <- brm(
  formula = life_exp_s ~ 
    s(log_gdp_s,   bs = "cr", k = 8) +
    s(fertility_s, bs = "cr", k = 6) +
    s(year_s,      bs = "cr", k = 5) +
    health_exp_s + clean_water_s + urban_pop_s +
    (1 | country),
  
  data = df_scaled,
  family = gaussian(),
  prior = priors,
  
  backend = "cmdstanr",
  chains = 4,
  iter = 2000,           # Giữ tạm 2000 để test
  warmup = 1000,
  cores = 4,
  control = list(adapt_delta = 0.98, max_treedepth = 12),
  seed = 42,
  
  # === Chỉ giữ refresh, bỏ show_messages ===
  refresh = 100,         
  silent = 1             # silent = 1 hoặc 2 để giảm output
)

saveRDS(model_C_timetrend, "/home/rstudio/project/model_C_timetrend.rds")
cat("Model C Nonlinearity and Time trend completed \n")

#==============================================================
# SECTION 8: Model comparison using LOO-CV
#==============================================================

model_A_linear <- readRDS("~/project/model_A_linear.rds")
model_B_spline <- readRDS("~/project/model_B_spline.rds")
model_C_timetrend <- readRDS("~/project/model_C_timetrend.rds")

cat("Performing LOO-CV Model Comparision\n")
loo_A_linear <- loo(model_A_linear, cores = 4)
loo_B_spline <- loo(model_B_spline, cores =4, 
                    reloo = TRUE)
loo_C_timetrend <- loo(model_C_timetrend, cores =4,
                       reloo = TRUE)

comparison_nonlinear <- loo_compare(loo_A_linear, loo_B_spline, loo_C_timetrend)
print(comparison, digits = 3)

writeLines(capture.output(print(comparison_nonlinear, digits = 3)),
           "/home/rstudio/project/loo_comparison_nonlinear.txt")
cat("\n LOO comparision saved to 'loo_comparision_nonlinear.txt'\n")
cat("Model with highest elpd_loo is the best model.\n")


# Kiểm tra Pareto-k
plot(loo_B, main = "Pareto-k diagnostics - Model B")
plot(loo_C, main = "Pareto-k diagnostics - Model C")


# ========================================================
# SECTION 9: DIAGNOSTICS MODEL A (Best model according to LOO)
# ========================================================

cat("=== DIAGNOSTICS FOR MODEL A (Linear) ===\n")

# 1. Model Summary
summary(model_A_linear)

# 2. MCMC Convergence
cat("\nRhat and ESS:\n")
print(posterior::summarise_draws(as_draws_df(model_A_linear), "rhat", "ess_bulk", "ess_tail"))

# 3. Trace plots
mcmc_plot(model_A_linear, type = "trace", facet_args = list(ncol = 3))

# 4. Posterior Predictive Check (rất quan trọng)
pp_check(model_A_linear, ndraws = 500) + 
  labs(title = "Posterior Predictive Check - Model A (Linear)")

# 5. Conditional Effects (dù là linear vẫn nên xem)
conditional_effects(model_A_linear, effects = "log_gdp_s", prob = 0.95)
conditional_effects(model_A_linear, effects = "health_exp_s", prob = 0.95)
conditional_effects(model_A_linear, effects = "fertility_s", prob = 0.95)
conditional_effects(model_A_linear, effects = "urban_pop_s", prob = 0.95)


