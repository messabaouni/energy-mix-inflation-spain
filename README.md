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
figures/    model figures (IRF, FEVD, fan charts, PDPs)
output/     EDA and PCA tables and plots
```

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
INE consumer price indices. The raw hourly price files are not included because of their size.
Check the original providers' terms of use before reusing the data.

## Author
Mohammed Essabaouni
