install.packages(c("brms", "bayesplot", "loo", "tidyverse"),
                 repos = "https://cloud.r-project.org",
                 dependencies = TRUE)

# Checking root packages 
library(brms)
library(bayesplot)
library(loo)
library(tidyverse)

cat("brms:", as.character(packageVersion("brms")), "\n")
cat("bayesplot:", as.character(packageVersion("bayesplot")), "\n")
cat("loo:", as.character(packageVersion("loo")), "\n")
#-----------------------------------------------------------------
#========================================================
# SECTION 7: BAYESIAN HIERACHICAL MODEL
#========================================================
# Load data
df_scaled <- readRDS("/home/rstudio/project/df_scaled.rds")
cat("Data loaded:", nrow(df_scaled), "rows\n")

# Prior
priors <- c(
  prior(normal(0, 1),   class = "Intercept"),
  prior(normal(0, 0.5), class = "b"),
  prior(exponential(1), class = "sd"),
  prior(exponential(1), class = "sigma")
)

# MODEL B: log_gdp
cat("Starting Model B...\n")
model_B <- brm(
  formula = life_exp_s ~ log_gdp_s + health_exp_s +
    clean_water_s + fertility_s +
    (1 | country),
  data    = df_scaled,
  family  = gaussian(),
  prior   = priors,
  chains  = 4, iter = 4000, warmup = 2000,
  cores   = 4, seed = 42,
  control = list(adapt_delta = 0.95)
)
saveRDS(model_B, "/home/rstudio/project/model_B.rds")
sink("/home/rstudio/project/summary_model_B.txt")
print(summary(model_B))
sink()
cat("✅ Model B done!\n")

# MODEL C: urban_pop
cat("Starting Model C...\n")
model_C <- brm(
  formula = life_exp_s ~ urban_pop_s + health_exp_s +
    clean_water_s + fertility_s +
    (1 | country),
  data    = df_scaled,
  family  = gaussian(),
  prior   = priors,
  chains  = 4, iter = 4000, warmup = 2000,
  cores   = 4, seed = 42,
  control = list(adapt_delta = 0.95)
)
saveRDS(model_C, "/home/rstudio/project/model_C.rds")
sink("/home/rstudio/project/summary_model_C.txt")
print(summary(model_C))
sink()
cat("✅ Model C done!\n")

#----------
# Load B and C 
model_B <- readRDS("/home/rstudio/project/model_B.rds")
model_C <- readRDS("/home/rstudio/project/model_C.rds")
cat("✅ Model B & C loaded!\n")

# Running Model A (full model)
model_A <- brm(
  formula = life_exp_s ~ log_gdp_s + urban_pop_s +
    health_exp_s + clean_water_s +
    fertility_s + (1 | country),
  data    = df_scaled,
  family  = gaussian(),
  prior   = priors,
  chains  = 4, iter = 4000, warmup = 2000,
  cores   = 4, seed = 42,
  control = list(adapt_delta = 0.97)
)
saveRDS(model_A, "/home/rstudio/project/model_A.rds")
capture.output(summary(model_A),
               file = "/home/rstudio/project/summary_model_A.txt")
cat("✅ Model A done!\n")
summary(model_A)
#-----------------------------------
cat("\n🎉 ALL MODELS COMPLETED!\n")
cat("Files saved:\n")
cat("- model_A.rds + summary_model_A.txt\n")
cat("- model_B.rds + summary_model_B.txt\n")
cat("- model_C.rds + summary_model_C.txt\n")

#========================================================
# SECTION 8: MCMC DIAGNOSTICS
#========================================================



#========================================================
# SECTION 9: LOO-CV COMPARISON
#========================================================

#-------------------------------------
# LOO-CV Model Comparison
cat("Computing LOO...\n")

loo_A <- loo(model_A)
loo_B <- loo(model_B)
loo_C <- loo(model_C)
 
# Comparison
comparison <- loo_compare(loo_A, loo_B, loo_C)
print(comparison)

capture.output(
  print(comparison),
  file = "/home/rstudio/project/loo_comparison.txt"
)
cat("✅ LOO comparison saved!\n")

#========================================================
# SECTION 10: TIME DEPENDENCE AND NONLINEARITY
#========================================================


df_scaled <- df_scaled %>%
  mutate(
    #time trend (scaled)
    year_s= scale(year)[,1],
    
    #quadratic terms (nonlinearity)
    fertility_s2 = fertility_s^2, #fertility quadratic
    clean_water_s2 = clean_water_s^2, #clean water quadratic
  )

#checking
cat("New variables added\n")
df_scaled %>%
  dplyr::select(country,year, year_s, fertility_s,fertility_s2,
                clean_water_s, clean_water_s2) %>%
  head(10) %>%
  as.data.frame() %>%
  print()

#------Visualize-non-linearity-before-model-----
#fertility vs life expectancy - non linearity?
ggplot(df_scaled, aes(x = fertility_s, y = life_exp_s)) +
  geom_point(alpha = 0.4, color = "steelblue") +
  geom_smooth(method = "lm",   formula = y ~ x,
              color = "red",   linetype = "dashed",
              se = FALSE, linewidth = 1) +
  geom_smooth(method = "lm",   formula = y ~ x + I(x^2),
              color = "darkgreen", se = TRUE,
              linewidth = 1) +
  labs(title    = "Fertility Rate vs Life Expectancy",
       subtitle = "Red = linear | Green = quadratic",
       x = "Fertility Rate (scaled)",
       y = "Life Expectancy (scaled)") +
  theme_minimal()
#clean water vs life expectancy - non linearity?
ggplot(df_scaled, aes(x = clean_water_s, y = life_exp_s)) +
  geom_point(alpha = 0.4, color = "coral") +
  geom_smooth(method = "lm", formula = y ~ x,
              color = "red", linetype = "dashed",
              se = FALSE, linewidth = 1) +
  geom_smooth(method = "lm", formula = y ~ x + I(x^2),
              color = "darkgreen", se = TRUE,
              linewidth = 1) +
  labs(title    = "Clean Water Access vs Life Expectancy",
       subtitle = "Red = linear | Green = quadratic",
       x = "Clean Water (scaled)",
       y = "Life Expectancy (scaled)") +
  theme_minimal()

#time trend - life expectancy increased by time?
install.packages("ggrepel")
library(ggrepel)
df_last <- df_scaled %>%
  group_by(country) %>%
  filter(year == max(year)) %>%
  ungroup()
ggplot(df_scaled, aes(x = year, y = life_exp_s,
                      group = country, color = country)) +
  geom_line(alpha = 0.6) +
  geom_smooth(aes(group = 1), method = "lm",
              color = "black", linewidth = 1.2,
              se = TRUE) +
  geom_text_repel(
    data = df_last,
    aes(label = country),
    nudge_x = 1.5,              # đẩy label sang phải
    direction = "y",
    hjust = 0,
    segment.color = "grey50",   # màu mũi tên
    segment.size = 0.5,
    box.padding = 0.3,
    max.overlaps = Inf,
    size = 3
  ) +
  labs(title    = "Life Expectancy Over Time",
       subtitle = "Black line = overall time trend",
       x = "Year", y = "Life Expectancy (scaled)") +
  theme_minimal() +
  theme(legend.position = "none")


#----------- Test non-linearity between Life expectancy ----
# vs Fertility
lm_linear_f <- lm(life_exp_s ~ fertility_s, data = df_scaled)
lm_quad_f   <- lm(life_exp_s ~ fertility_s + I(fertility_s^2), data = df_scaled)
anova(lm_linear_f, lm_quad_f)

# vs Clean Water
lm_linear_w <- lm(life_exp_s ~ clean_water_s, data = df_scaled)
lm_quad_w   <- lm(life_exp_s ~ clean_water_s + I(clean_water_s^2), data = df_scaled)
anova(lm_linear_w, lm_quad_w)

#--------
#Model D (time dependency)
cat("Starting Model D (time trend)...\n")

model_D <- brm(
  formula = life_exp_s ~ log_gdp_s + urban_pop_s +
    health_exp_s + clean_water_s +
    fertility_s + year_s +  # ← time trend
    (1 | country),          # ← country random effect
  data    = df_scaled,
  family  = gaussian(),
  prior   = priors,
  chains  = 4, iter = 4000, warmup = 2000,
  cores   = 4, seed = 42,
  control = list(adapt_delta = 0.97)
)

saveRDS(model_D, "/home/rstudio/project/model_D.rds")
capture.output(summary(model_D),
               file = "/home/rstudio/project/summary_model_D.txt")
cat("✅ Model D done!\n")
summary(model_D)

#-------


