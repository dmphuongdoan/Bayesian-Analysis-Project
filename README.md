# Bayesian Hierarchical Analysis of Life Expectancy in East Asia & Pacific (2000–2022)

**Author:** Doan Duy My Phuong · 
**Supervisor:** Prof. Rossini Luca

## Overview
Bayesian hierarchical models to examine how GDP per capita, health expenditure, clean water access, urbanisation, and fertility rate affect life expectancy across **17 EAP countries** (2000–2022). Data from [World Development Indicators (WDI)](https://databank.worldbank.org/source/world-development-indicators). Model selection via **LOO-CV** favours the non-linear specification.

## Project Structure
├── data/          # Raw & processed WDI panel data
├── scripts/       # R scripts (download → clean → EDA → models → comparison)
├── outputs/       # Figures & tables
└── report/        # Final report
