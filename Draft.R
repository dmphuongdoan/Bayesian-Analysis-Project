#========================================================
# PROJECT: BAYESIAN ANALYSIS  
# IMPACT OF SOCIO-ECONOMIC FACTORS ON LIFE EXPECTANCY 
# REGION: EAST AND PACIFIC COUNTRIES 
# TIME DURATION: 2000-2022 (23 YEARS)
# DATASET: WDI
#========================================================

#============================================================
# SECTION 1: LOAD LIBRARIES & DEFINE COUNTRIES AND INDICATOR
#============================================================
#1. Installing packages and setting libraries

library(WDI); library(tidyverse); library(brms)
library(bayesplot); library(loo); library(mice)
library(corrplot); library(ggplot2); library(knitr)

#2. Defining indicators and countries

indicators <- c(
  life_expectancy = "SP.DYN.LE00.IN",
  health_exp_gdp = "SH.XPD.CHEX.GD.ZS",
  gdp_per_capita = "NY.GDP.PCAP.KD",
  clean_water = "SH.H2O.BASW.ZS",
  sanitation = "SH.STA.BASS.ZS",
  urban_pop = "SP.URB.TOTL.IN.ZS",
  fertility_rate = "SP.DYN.TFRT.IN"
)

countries <- c(
  # East Asia
  "CN", # China
  "JP", # Japan
  "KR", # Korea, Rep.
  #"KP", # Korea, Dem. People's Rep. (likely mostly NA)
  "MN", # Mongolia
  
  # Southeast Asia
  "ID", # Indonesia
  "TH", # Thailand
  "VN", # Vietnam
  "MY", # Malaysia
  "PH", # Philippines
  "MM", # Myanmar
  "KH", # Cambodia
  "LA", # Lao PDR
  "BN", # Brunei Darussalam
  
  # Pacific Developed
  "AU", # Australia
  "NZ", # New Zealand
  
  # Pacific Islands
  "FJ", # Fiji
  "WS", # Samoa
  
  # SAR
  "HK"  # Hong Kong SAR, China
)
#========================================================
# SECTION 2:  DOWNLOAD AND CLEAN DATA
#========================================================

#1. Downloading data
df_raw <- WDI(
  country = countries,
  indicator = indicators,
  start = 2000,
  end = 2022,
  extra = TRUE
)

#2. Checking data 
head(df_raw)
str(df_raw)
summary(df_raw)

df_clean <- df_raw |> 
  select(country, iso2c, year, income, 
         life_expectancy, health_exp_gdp, 
         gdp_per_capita,clean_water, sanitation, urban_pop, 
         fertility_rate) |>
  filter(!is.na(life_expectancy))

glimpse(df_clean)

cat("\n== General View ==\n")
cat("Rows:", nrow(df_clean), "\n")
cat("Number of countries:", n_distinct(df_clean$country), "\n")
#========================================================
# SECTION 3: MISSING DATA CHECK AND IMPUTATION
#========================================================
#1. Missing data table 
df_clean %>% 
  summarize(across(everything(), ~sum(is.na(.)))) %>%
  pivot_longer(everything(),
             names_to = "Variable",
             values_to = "N_missing") %>%
  mutate(Pct_Missing = round(N_missing/nrow(df_clean) * 100, 1)) %>%
  print()

#2. Imputation
library(mice)

cat("\n=== Rows missing health_exp_gdp ===\n")
df_clean %>% 
  filter(is.na(health_exp_gdp)) %>%
  dplyr::select(country, year, health_exp_gdp) %>%
  as.data.frame() %>%
  print()                    # ← bỏ (n = 30) đi

cat("\n=== Rows missing clean_water ===\n")
df_clean %>% 
  filter(is.na(clean_water)) %>%
  dplyr::select(country, year, clean_water) %>%
  as.data.frame() %>%
  print()

# Separing numeric var to impute
df_to_impute <- df_clean %>%    # ← sửa tên: df_to_impute (không phải df_to_imputed)
  dplyr::select(life_expectancy, gdp_per_capita, health_exp_gdp,
                clean_water, sanitation, urban_pop, fertility_rate)

# Running imputation
set.seed(42)
imputed <- mice(
  df_to_impute,
  m      = 5,
  method = "pmm",
  maxit  = 50,
  print  = FALSE
)

df_imputed_numeric <- complete(imputed, 1)

# Joining
df_final <- bind_cols(
  df_clean %>% dplyr::select(country, iso2c, year, income),
  df_imputed_numeric
)

# Checking missing
cat("=== Missing after imputation ===\n")
df_final %>%
  summarise(across(everything(), ~sum(is.na(.)))) %>%
  tidyr::pivot_longer(everything(),
                      names_to  = "Variable",
                      values_to = "N_Missing") %>%
  as.data.frame() %>%
  print()

cat("\n=== ALL VARIABLES IN df_final ===\n")
cat("Cols:", ncol(df_final), "\n")
cat("Rows:", nrow(df_final), "\n\n")
glimpse(df_final)

cat("\n=== DESCRIPTIVE STATISTICS ===\n")
df_final %>%
  dplyr::select(where(is.numeric)) %>%
  as.data.frame() %>%
  summary() %>%
  print()
  
#After checking we know that HK also have many missing data value from 2000-2022
#Decide eliminate HongKong

#========================================================
# SECTION 4: REMOVE HONG KONG
#========================================================

# Removing Hong Kong of df_final (after imputation)
df_final <- df_final %>%
  filter(iso2c != "HK")

# Confirming 
cat("Number of countries after removing:", n_distinct(df_final$country), "\n")
cat("Rows:", nrow(df_final), "\n")
# Expectations: 17 countries × 23 years = 391 rows

df_final %>% 
  distinct(country) %>% 
  as.data.frame() %>%
  print()
  
#========================================================
# SECTION 5: LOG TRANSFORM & EDA
#========================================================

df_model <- df_final %>% 
  mutate(
    #log transform GDP (right-skewed too strong)
    log_gdp = log(gdp_per_capita),
    #income group in order
    income_group = factor(income,
                          levels = c("Low income",
                                     "Lower middle income", 
                                     "Upper middle income",
                                     "High income"))
  )
#============================================================

library(gridExtra)

p1 <- ggplot(df_model, aes(x = gdp_per_capita)) +
  geom_histogram(fill = "steelblue", bins = 30, alpha = 0.8) +
  labs(title = "GDP per capita (raw)", x = "", y = "Count") +
  theme_minimal()

p2 <- ggplot(df_model, aes(x = log_gdp)) +
  geom_histogram(fill = "coral", bins = 30, alpha = 0.8) +
  labs(title = "Log(GDP per capita)", x = "", y = "Count") +
  theme_minimal()

grid.arrange(p1, p2, ncol = 2)
#-------------------------------------  
# Reset graphics device trước
#dev.off()

# Sau đó chạy lại histogram
#par(mfrow = c(1, 2), mar = c(4, 4, 2, 1))  # ← thêm mar để giảm margin
#hist(df_model$gdp_per_capita, 
#     main = "GDP per capita (raw)",
#     col  = "steelblue", xlab = "")
#hist(df_model$log_gdp, 
#     main = "Log(GDP per capita)",
#     col  = "coral", xlab = "")
#par(mfrow = c(1, 1))  # Reset về 1 plot  
#-------------------------------------  
# Reset trước
dev.off()

# EDA PLOTS (ggplot)

# Plot 1: Life Expectancy trend
install.packages("ggrepel")
library(ggplot2)
library(dplyr)
library(ggrepel)

# last point in each country
df_last <- df_model %>%
  group_by(country) %>%
  filter(year == max(year))

ggplot(df_model, aes(x = year, y = life_expectancy,
                     color = country, group = country)) +
  geom_line(linewidth = 1.0) +
  geom_point(size = 1.0) +
  
  # 👇 label + arrow
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
  
  # expanding the x-axis make room for label
  scale_x_continuous(expand = expansion(mult = c(0.02, 0.15))) +
  
  labs(
    title    = "Life Expectancy Trend in EAP (2000–2022)",
    subtitle = "17 countries across East Asia & Pacific",
    x = "Year",
    y = "Life Expectancy (years)"
  ) +
  
  theme_minimal() +
  
  # ❌ eliminate legend 
  theme(legend.position = "none")

# Plot 2: Boxplot theo Income Group
ggplot(df_model, aes(x = income_group, y = life_expectancy,
                     fill = income_group)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Life Expectancy by Income Group (2000–2022)",
       x = "", y = "Life Expectancy (years)") +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 15, hjust = 1))

# Plot 3: Log GDP vs Life Expectancy
ggplot(df_model, aes(x = log_gdp, y = life_expectancy,
                     color = income_group)) +
  geom_point(alpha = 0.5, size = 1.5) +
  geom_smooth(method = "lm", se = TRUE, color = "black") +
  labs(title = "Log GDP per Capita vs Life Expectancy",
       x = "Log(GDP per Capita)",
       y = "Life Expectancy (years)",
       color = "Income Group") +
  theme_minimal()

# Plot 4: Fertility Rate vs Life Expectancy
ggplot(df_model, aes(x = fertility_rate, y = life_expectancy,
                     color = income_group)) +
  geom_point(alpha = 0.5, size = 1.5) +
  geom_smooth(method = "lm", se = TRUE, color = "black") +
  labs(title = "Fertility Rate vs Life Expectancy",
       x = "Fertility Rate (births per woman)",
       y = "Life Expectancy (years)",
       color = "Income Group") +
  theme_minimal()

#Plot: Health expenditure vs Life expectancy
ggplot(df_model, aes(x = health_exp_gdp, y = life_expectancy,
                     color = income_group)) +
  geom_point(alpha = 0.5, size = 1.5) +
  geom_smooth(method = "lm", se = TRUE, color = "black") +
  labs(title = "Health Expenditure vs Life Expectancy",
       x = "Health Expenditure (% of GDP)",
       y = "Life Expectancy (years)",
       color = "Income Group") +
  theme_minimal()

#Plot: Clean water access vs Life expectancy
ggplot(df_model, aes(x = clean_water, y = life_expectancy,
                     color = income_group)) +
  geom_point(alpha = 0.5, size = 1.5) +
  geom_smooth(method = "lm", se = TRUE, color = "black") +
  labs(title = "Clean Water Access vs Life Expectancy",
       x = "Clean Water Access",
       y = "Life Expectancy (years)",
       color = "Income Group") +
  theme_minimal()

# Plot 5: Correlation Matrix
library(corrplot)
cor_matrix <- df_model %>%
  dplyr::select(life_expectancy, log_gdp, health_exp_gdp,
                clean_water, sanitation, urban_pop,
                fertility_rate) %>%
  cor(use = "complete.obs")

corrplot(cor_matrix,
         method      = "color",
         type        = "upper",
         addCoef.col = "black",
         number.cex  = 0.75,
         tl.cex      = 0.85,
         title       = "Correlation Matrix — EAP Dataset",
         mar         = c(0, 0, 2, 0))

#========================================================
# SECTION 6: CHECKING VIF
#========================================================

# Keep: log_gdp, health_exp_gdp, clean_water, urban_pop, fertility_rate
# Eliminate: sanitation (vì corr với clean_water = 0.87 → redundant)

# Checking VIF before running model
install.packages("car")
library(car)

# Run OLS temporary to check VIF
vif_check <- lm(life_expectancy ~ log_gdp + health_exp_gdp +
                  clean_water + urban_pop + fertility_rate,
                data = df_model)
vif(vif_check)
# VIF > 10 = multicollinearity nghiêm trọng
# VIF > 5  = cần chú ý

# Check VIF if eliminate urban_pop
vif_check2 <- lm(life_expectancy ~ log_gdp + health_exp_gdp +
                   clean_water + fertility_rate,
                 data = df_model)
vif(vif_check2)

# Check VIF if log_gdp
vif_check3 <- lm(life_expectancy ~ health_exp_gdp + clean_water +
                   urban_pop + fertility_rate,
                 data = df_model)
vif(vif_check3)
#===================================================
# SECTION 7: BAYESIAN HIERARCHICAL MODEL
#===================================================

# scale all variables
df_scaled <- df_model %>%
  mutate(
    life_exp_s = scale(life_expectancy)[,1],
    log_gdp_s = scale(log_gdp)[,1],
    health_exp_s = scale(health_exp_gdp)[,1],
    clean_water_s = scale(clean_water)[,1],
    urban_pop_s = scale(urban_pop)[,1],
    fertility_s = scale(fertility_rate)[,1]
  )

# Checking scaling
cat("== Scaling Check (mean = 0, sd = 1 ==\n")
df_scaled %>% 
  dplyr::select(ends_with("_s")) %>%
  summarise(across(everything(),
                   list(mean = ~round(mean(.),3),
                        sd = ~round(sd(.),3)))) %>%
  as.data.frame() %>%
  print()

#--------------------------------------------------------

saveRDS(df_scaled, "df_scaled.rds")
saveRDS(df_model,  "df_model.rds")
saveRDS(df_final,  "df_final.rds")

cat("All data is saved!\n")
cat("Rows:", nrow(df_scaled), "\n")
cat("Countries:", n_distinct(df_scaled$country), "\n")

file.exists("df_scaled.rds")
file.exists("df_model.rds")
file.exists("df_final.rds")
