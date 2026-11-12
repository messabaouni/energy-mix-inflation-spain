# Energy Mix, Wholesale Electricity Prices and Inflation in Spain

Master's thesis (MESIO UPC-UB, Statistics and Operations Research).
Monthly analysis, 2010–2024, of how Spain's electricity generation mix
(nuclear, wind, solar, gas, coal, hydro…) relates to wholesale electricity prices
and consumer inflation (IPC) and producer prices (IPRI).

## Methods
- Data cleaning and harmonisation of monthly series (generation, prices, IPC)
- Exploratory time-series analysis and stationarity tests (ADF, KPSS)
- Correlation analysis, PCA and k-means clustering of the energy mix
- Cross-correlations between prices and inflation
- VAR / SVAR models: impulse responses (IRF) and variance decomposition (FEVD)
- Machine learning forecasting (XGBoost, rolling window) with feature importance, PDP and ICE (`iml`), and fan charts for 2024

## Structure
```
data/       monthly datasets (data.csv, cleaned and transformed versions, variable descriptions)
scripts/    01_load_clean_data.R, 02_exploratory_analysis.R, 03_correlation_pca.R
analysis/   tfm_all.Rmd – full VAR/SVAR + XGBoost analysis
report/     thesis.pdf (full thesis) and defense_slides.pdf
figures/    model figures (IRF, FEVD, fan charts, PDPs)
output/     EDA and PCA tables and plots
```

## Reading the results
The full thesis (`report/thesis.pdf`, in Spanish) explains the motivation and conclusions. The PCA and clustering in `output/` summarise how the generation mix evolves over time. The VAR/SVAR impulse responses in `figures/res/` show how a shock to one variable (for example gas generation or wholesale price) propagates to the others, including inflation. The XGBoost fan charts compare forecasts for 2024 with observed values.

## How to run
Run from the repository root in R:
```r
source("scripts/01_load_clean_data.R")
source("scripts/02_exploratory_analysis.R")
source("scripts/03_correlation_pca.R")
rmarkdown::render("analysis/tfm_all.Rmd")
```
Main packages: tidyverse, lubridate, janitor, zoo, tseries, urca, vars, FactoMineR, factoextra, xgboost, caret, iml, Metrics, patchwork.

## Data
Monthly electricity generation by technology, wholesale electricity prices and
INE consumer price indices. The exact download sources are not recorded in the repository, so check the original providers' terms of use before reusing the data. The raw hourly price files are not included because of their size.

## Author
Mohammed Essabaouni
