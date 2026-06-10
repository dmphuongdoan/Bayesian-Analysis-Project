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

library(rstan)
library(brms)
stan_version()

install.packages("cmdstanr", 
                 repos = c("https://mc-stan.org/r-packages/", 
                           getOption("repos")))

# Bước 2: Cài CmdStan (backend thực sự)
library(cmdstanr)
install_cmdstan()

# Bước 3: Kiểm tra
cmdstan_version()  # phải trả về version number

cat("Running Model B.2 - Non-linear with Splines...\n")
model_B2_spline <- brm(
  formula = life_exp_s ~ 
    log_gdp_s +
    s(clean_water_s, bs = "cr", k = 6) + 
    s(fertility_s, bs = "cr", k = 6) + 
    health_exp_s + urban_pop_s +
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

saveRDS(model_B2_spline, "/home/rstudio/project/model_B2_spline.rds")
cat("Model B.2 Non-linear completed \n")

cat("Running Model C.2 - Non-linear + Time trend...\n")
model_C1_timetrend <- brm(
  formula = life_exp_s ~ 
    log_gdp_s +                              # linear (log transform đã xử lý skew)
    s(clean_water_s, bs = "cr", k = 6) +     # spline ← ceiling effect từ Figure 6b
    s(fertility_s,   bs = "cr", k = 6) +     # spline ← diminishing effect từ Figure 6c
    s(year_s,        bs = "cr", k = 5) +     # spline ← structural shifts (COVID, MDG)
    health_exp_s + urban_pop_s +
    (1 | country),
  data    = df_scaled,
  family  = gaussian(),
  prior   = priors_spline,
  chains  = 4,
  iter    = 4000,
  warmup  = 2000,
  cores   = 4,
  control = list(adapt_delta = 0.99, max_treedepth = 12),
  seed    = 42
)
saveRDS(model_C1_timetrend, "/home/rstudio/project/model_C1_timetrend.rds")

model_A_linear <- readRDS("~/project/model_A_linear.rds")
loo_A <- loo(model_A_linear)
loo_B2 <- loo(model_B2_spline, reloo = TRUE, seed = 42)
loo_C1 <- loo(model_C1_timetrend, reloo = TRUE, seed = 42)

loo_compare(loo_A, loo_B2, loo_C1)

conditional_effects(model_C1_timetrend)
ranef(model_C1_timetrend)
pp_check(model_C1_timetrend)

#-----------RESULT
library(bayesplot)
library(ggplot2)

# 1. TRACE PLOTS — chain mixing
mcmc_trace(model_C1_timetrend, 
           pars = c("b_Intercept", "b_log_gdp_s", 
                    "b_health_exp_s", "b_urban_pop_s",
                    "sd_country__Intercept", "sigma"))

# 2. POSTERIOR DISTRIBUTIONS với 95% CI
mcmc_areas(model_C1_timetrend,
           pars = c("b_log_gdp_s", "b_health_exp_s", 
                    "b_urban_pop_s"),
           prob = 0.95)

# 3. R-HAT & ESS — từ summary
summary(model_C1_timetrend)

# 4. POSTERIOR PREDICTIVE CHECK
pp_check(model_C1_timetrend, ndraws = 100)

# 5. DIVERGENT TRANSITIONS
library(dplyr)
nuts_params(model_C1_timetrend) %>%
  filter(Parameter == "divergent__") %>%
  summarise(total_divergences = sum(Value))

# 6. CONDITIONAL EFFECTS — spline shapes
plot(conditional_effects(model_C1_timetrend, 
                         effects = "clean_water_s"))

plot(conditional_effects(model_C1_timetrend, 
                         effects = "fertility_s"))

plot(conditional_effects(model_C1_timetrend, 
                         effects = "year_s"))

# 7. RANDOM EFFECTS — country-level
ranef(model_C1_timetrend)


#===== chạy lại model mới==========================================

cat("Running Model C2 Final - clean_water linear...\n")
model_C2_final <- brm(
  formula = life_exp_s ~ 
    log_gdp_s +                          # linear
    s(fertility_s, bs = "cr", k = 6) +   # spline
    s(year_s,      bs = "cr", k = 5) +   # spline
    health_exp_s + clean_water_s + urban_pop_s +  # all linear
    (1 | country),
  data    = df_scaled,
  family  = gaussian(),
  prior   = priors_spline,
  chains  = 4,
  iter    = 4000,
  warmup  = 2000,
  cores   = 4,
  control = list(adapt_delta = 0.995, max_treedepth = 12),
  seed    = 42
)

saveRDS(model_C2_final, "/home/rstudio/project/model_C2_final.rds")
cat("Model C2 Final completed\n")

# Sau khi chạy xong, LOO comparison
loo_C2_final <- loo(model_C2_final)
loo_compare(loo_A_linear, loo_B2_spline, loo_C2_final)
#===== chạy lại model mới==========================================
