# Bayesian Hierarchical Analysis of Socioeconomic Factors Affecting Life Expectancy in East Asian and Pacific Countries (2000–2022)

**Author:** Doan Duy My Phuong 
**Supervisor:** Prof. Rossini Luca

## Overview
Bayesian hierarchical models to examine how socioeconomic factors affect life expectancy across **17 EAP countries** (2000–2022). Panel data (391 observations) sourced from [World Development Indicators (WDI)](https://databank.worldbank.org/source/world-development-indicators). Model selection via **LOO-CV** confirms the non-linear specification outperforms the linear baseline (ΔELPD = 150.3, SE = 38.9).

## Variables

**Outcome:** `life_expectancy` — Life expectancy at birth (years)

**Predictors:**
| Variable | Description | Specification |
|---|---|---|
| `log_gdp_pc` | GDP per capita (constant 2015 USD) | Linear |
| `health_exp` | Health expenditure (% of GDP) | Linear |
| `urban_pop` | Urban population (% of total) | Linear |
| `clean_water` | Access to clean water (% of population) | Spline |
| `fertility_rate` | Total fertility rate (births per woman) | Spline |
| `year` | Time trend | Spline |

> `sanitation` excluded due to high collinearity with `clean_water` (r = 0.87). All predictors standardised prior to model fitting.

## Project Structure
```
├── data/          # Raw & processed WDI panel data
├── scripts/       # R scripts (download → clean → EDA → models → comparison)
├── outputs/       # Figures & tables
└── report/        # Final report
```

## Key Results
- **Log GDP per capita** is the strongest predictor (β = 0.78, 95% CI [0.62, 0.94]), consistent with the Preston curve.
- **Fertility rate** shows a significant non-linear negative effect — steepest at low-to-moderate levels, levelling off at high values.
- **Health expenditure** and **urbanisation** show negligible effects conditional on income.
- **Country-level heterogeneity** is substantial (sd = 0.57 vs. sigma = 0.11). Viet Nam, Japan, and Thailand are notable overperformers; Brunei Darussalam underperforms despite being the highest-income country in the dataset.
