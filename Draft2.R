library(brms)
library(bayesplot)
library(loo)
library(tidyverse)

set.seed(42)
df_scaled <- readRDS("/home/rstudio/project/df_scaled.rds")

df_scaled <- df_scaled %>%
  mutate(
    year_s = scale(year)[,1]          # scale year thành mean=0, sd=1
  )
cat("New variables added\n")
df_scaled %>%
  dplyr::select(country,life_exp_s,log_gdp_s, health_exp_s, clean_water_s, 
                urban_pop_s, fertility_s, year_s) %>%
  head(10) %>%
  as.data.frame() %>%
  print()

priors_spline <- c(
  prior(normal(0, 1),   class = "Intercept"),
  prior(normal(0, 0.5), class = "b"),
  prior(exponential(1), class = "sd"),
  prior(exponential(1), class = "sigma"),
  prior(exponential(1), class = "sds")  # smoothing parameter cho splines
)

#-------------------------------------------------------------------------------
# 1. Buộc dùng đúng compiler C++
Sys.setenv(MAKEFLAGS = "CC=gcc CXX=g++")
Sys.setenv(PKG_CXXFLAGS = "-std=gnu++17")

# 2. Reinstall các package gây lỗi
remove.packages(c("rstan", "StanHeaders", "RcppEigen", "BH", "brms"))

install.packages("rstan", repos = "https://mc-stan.org/r-packages/", quiet = TRUE)
install.packages("brms", dependencies = TRUE)

# 3. Tạo file Makevars để fix lâu dài
# Bước 1: Ghi đè Makevars với nội dung đúng
writeLines(
  c(
    "CXX17 = g++",
    "CXX17STD = -std=gnu++17",
    "CXX17FLAGS = -O2 -fPIC"
  ),
  con = "~/.R/Makevars"
)

# Bước 2: Verify nội dung
readLines("~/.R/Makevars")

# Sau khi restart R (Session → Restart R)
install.packages("StanHeaders", repos = "https://mc-stan.org/r-packages/")
install.packages("rstan",       repos = "https://mc-stan.org/r-packages/")
install.packages("brms", dependencies = TRUE)

# Test compiler hoạt động chưa
library(rstan)
library(brms)
stan_version()
#-------------------------------------------------------------------------------
# Bước 1: Cài cmdstanr
install.packages("cmdstanr", 
                 repos = c("https://mc-stan.org/r-packages/", 
                           getOption("repos")))

# Bước 2: Cài CmdStan (backend thực sự)
library(cmdstanr)
install_cmdstan()

# Bước 3: Kiểm tra
cmdstan_version()  # phải trả về version number
#-------------------------------------------------------------------------------

cat("Running Model B.1 - Non-linear with Splines...\n")

model_B1_spline <- brm(
  formula = life_exp_s ~ 
    s(log_gdp_s, bs = "cr", k = 8) + 
    s(fertility_s, bs = "cr", k = 6) + 
    health_exp_s + clean_water_s + urban_pop_s +
    (1 | country),
  
  data = df_scaled,
  family = gaussian(),
  prior = priors_spline,
  backend = "cmdstanr",
  chains = 4, 
  iter = 4000,      # tăng từ 1500 lên 4000
  warmup = 2000,  # tăng từ 750 lên 2000
  cores = 4,
  control = list(adapt_delta = 0.98, max_treedepth = 12),
  seed = 42,
  save_pars = save_pars(all = TRUE)
)

saveRDS(model_B1_spline, "/home/rstudio/project/model_B1_spline.rds")
cat("Model B.1 Non-linear completed \n")

#========================================================
cat("Running Model C.1 - Non-linear + Time trend...\n")
model_C1_timetrend <- brm(
  formula = life_exp_s ~ 
    s(log_gdp_s,   bs = "cr", k = 8) +
    s(fertility_s, bs = "cr", k = 6) +
    s(year_s,      bs = "cr", k = 5) +
    health_exp_s + clean_water_s + urban_pop_s +
    (1 | country),
  data    = df_scaled,
  family  = gaussian(),
  prior   = priors_spline,
  # backend = "cmdstanr",  # ← comment dòng này ra
  chains  = 4,
  iter    = 4000,
  warmup  = 2000,
  cores   = 4,
  control = list(adapt_delta = 0.99, max_treedepth = 12), #(fix divergent transitions, 0.98 -> 0.99)
  seed    = 42
)
saveRDS(model_C1_timetrend, "/home/rstudio/project/model_C1_timetrend.rds")
cat("Model C.1 Nonlinearity and Time trend completed \n")


#====================================================
# LOO-COMPARISON
#====================================================
model_A_linear <- readRDS("~/project/model_A_linear.rds")
loo_A <- loo(model_A_linear)
loo_B1 <- loo(model_B1_spline, reloo = TRUE, seed = 42)
loo_C1 <- loo(model_C1_timetrend, reloo = TRUE, seed = 42)

loo_compare(loo_A, loo_B1, loo_C1)

saveRDS(loo_compare, "/home/rstudio/project/loo_comparison.rds")

# "Model C1 (spline + time trend) outperforms 
# both B1 and A linear, with LOO differences exceeding 2.9 SE, 
# confirmed by PSIS diagnostics showing all Pareto-k < 0.5."

#DIAGNOSTICS MODEL C1 (Best model according to LOO)
# 1. Posterior predictive check
pp_check(model_C1_timetrend, ndraws = 100)

# 2. Plot spline effects
conditional_effects(model_C1_timetrend)

# 3. Trace plot model
plot(model_C1_timetrend)    


# 4. Random effects by country
ranef(model_C1_timetrend)

