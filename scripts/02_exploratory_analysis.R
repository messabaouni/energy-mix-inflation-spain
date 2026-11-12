# 02_exploratory_analysis.R
# ------------------------------------------------------------
# TFM: Mix energético, precios mayoristas e impacto en IPC/IPRI
# Objetivo: análisis exploratorio de series temporales
# Entrada:   ./data/data_clean.rds  (creado en 01_load_clean_data.R)
# Salidas:   ./output/* (gráficos y tablas)
# ------------------------------------------------------------

# ==== 0) Paquetes ====
req_pkgs <- c("readr","dplyr","tidyr","stringr","janitor","lubridate",
              "ggplot2","scales","patchwork","tseries","urca","GGally")
for (p in req_pkgs) {
  if (!requireNamespace(p, quietly = TRUE)) {
    install.packages(p, repos = "https://cloud.r-project.org")
  }
}
library(readr); library(dplyr); library(tidyr); library(stringr); library(janitor); library(lubridate)
library(ggplot2); library(scales); library(patchwork); library(tseries); library(urca); library(GGally)

# ==== 1) Cargar datos limpios ====
dataset_all <- readRDS(file.path("data","data_clean.rds"))

stopifnot("fecha" %in% names(dataset_all))
dataset_all <- dataset_all %>% arrange(fecha)

# ==== 2) Definir grupos de variables por patrón ====
vars_mix <- names(dataset_all) %>%
  setdiff(c("fecha")) %>%
  keep(~ str_detect(.x, "hidraul|nuclear|carbon|fuel|ciclo|eolica|fotovoltaica|termica|renovab|cogeneracion|residuos|generacion_total"))

vars_precios <- names(dataset_all) %>%
  keep(~ str_detect(.x, "mercado|precio_total|energia_final|eur_mwh|diario|intradiario"))

vars_ipc <- names(dataset_all) %>%
  keep(~ str_detect(.x, "^ipc|alimentos|bebidas|vestido|vivienda|muebles|sanidad|transporte|comunicaciones|ocio|ensenanza|restaurantes|hoteles|otros"))

vars_ipri <- names(dataset_all) %>%
  keep(~ str_detect(.x, "ipri|bienes_consumo|bienes_equipo|bienes_intermedios|energia"))

# ==== 3) Helper: función para guardar gráficos ====
save_plot <- function(p, filename, w=10, h=5){
  outpath <- file.path("output", filename)
  ggsave(outpath, p, width=w, height=h, dpi=120)
  message("Guardado: ", outpath)
}

# ==== 4) Resumen de NAs ====
na_tbl <- dataset_all %>% summarise(across(everything(), ~ sum(is.na(.x)))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "na_count") %>%
  arrange(desc(na_count))

readr::write_csv(na_tbl, file.path("output","na_summary.csv"))

# ==== 5) Descriptivos básicos ====
desc_tbl <- dataset_all %>%
  summarise(across(-fecha, list(n=~sum(!is.na(.x)),
                                mean=~mean(.x, na.rm=TRUE),
                                sd=~sd(.x, na.rm=TRUE),
                                p25=~quantile(.x, .25, na.rm=TRUE),
                                median=~median(.x, na.rm=TRUE),
                                p75=~quantile(.x, .75, na.rm=TRUE),
                                min=~min(.x, na.rm=TRUE),
                                max=~max(.x, na.rm=TRUE))), .groups="drop")
readr::write_csv(desc_tbl, file.path("output","descriptives.csv"))

# ==== 6) Gráficos de series temporales ====
plot_ts_group <- function(df, vars, title){
  if (length(vars) == 0) return(NULL)
  long <- df %>% select(fecha, all_of(vars)) %>% pivot_longer(-fecha, names_to="variable", values_to="value")
  ggplot(long, aes(fecha, value)) +
    geom_line() +
    facet_wrap(~variable, scales="free_y", ncol = 3) +
    labs(title = title, x = NULL, y = NULL) +
    theme_minimal(base_size = 12)
}

p_mix <- plot_ts_group(dataset_all, vars_mix, "Series: Mix energético")
if (!is.null(p_mix)) save_plot(p_mix, "ts_mix_energetico.png", w=12, h=8)

p_precios <- plot_ts_group(dataset_all, vars_precios, "Series: Precios energía (€/MWh)")
if (!is.null(p_precios)) save_plot(p_precios, "ts_precios_energia.png", w=12, h=6)

p_ipc <- plot_ts_group(dataset_all, vars_ipc, "Series: IPC por componentes")
if (!is.null(p_ipc)) save_plot(p_ipc, "ts_ipc.png", w=12, h=10)

p_ipri <- plot_ts_group(dataset_all, vars_ipri, "Series: IPRI y agregados")
if (!is.null(p_ipri)) save_plot(p_ipri, "ts_ipri.png", w=12, h=8)

# ==== 7) Matrices de correlación (con rezagos implícitos no aún) ====
# Correlación contemporánea entre grupos seleccionados
# Seleccionamos una muestra compacta para no saturar la matriz
vars_for_corr <- c(head(vars_mix, 6), vars_precios, c("ipc_general", "vivienda_agua_electricidad_gas"), 
                   head(vars_ipri, 6)) %>% unique()
vars_for_corr <- intersect(vars_for_corr, names(dataset_all))
if (length(vars_for_corr) >= 2) {
  corr_df <- dataset_all %>% select(all_of(vars_for_corr)) %>% drop_na()
  if (nrow(corr_df) > 2) {
    p_corr <- GGally::ggcorr(corr_df, label = TRUE, label_size = 2.8, hjust = 0.85, layout.exp = 2)
    save_plot(p_corr, "corr_matrix_selected.png", w=12, h=10)
  }
}

# ==== 8) Estacionariedad: pruebas ADF y KPSS para variables clave ====
key_vars <- c(vars_precios, "ipc_general","vivienda_agua_electricidad_gas","ipri_total","energia")
key_vars <- intersect(key_vars, names(dataset_all))

adf_results <- list()
kpss_results <- list()

for (v in key_vars) {
  x <- dataset_all[[v]]
  x <- x[!is.na(x)]
  if (length(x) > 12) {
    # ADF (tseries::adf.test)
    adf <- tryCatch(tseries::adf.test(x, k = trunc(sqrt(length(x)))), error = function(e) NULL)
    # KPSS (urca::ur.kpss) nivel
    kpss <- tryCatch(urca::ur.kpss(x, type="mu"), error = function(e) NULL)
    adf_results[[v]] <- if (is.null(adf)) NA else adf$p.value
    kpss_results[[v]] <- if (is.null(kpss)) NA else kpss@teststat
  }
}

adf_tbl <- tibble::tibble(variable = names(adf_results), adf_p_value = unlist(adf_results))
kpss_tbl <- tibble::tibble(variable = names(kpss_results), kpss_stat = unlist(kpss_results))
stationarity_tbl <- dplyr::left_join(adf_tbl, kpss_tbl, by="variable")

readr::write_csv(stationarity_tbl, file.path("output","stationarity_tests_adf_kpss.csv"))

# ==== 9) Transformaciones sugeridas (log y diferencias) ====
# Creamos una versión transformada (sin sobreescribir el original)
dataset_tr <- dataset_all

log_candidates <- c(vars_precios, "ipc_general","vivienda_agua_electricidad_gas","ipri_total","energia")
log_candidates <- intersect(log_candidates, names(dataset_tr))

for (v in log_candidates) {
  if (all(dataset_tr[[v]] > 0, na.rm = TRUE)) {
    dataset_tr[[paste0(v,"_log")]] <- log(dataset_tr[[v]])
    dataset_tr[[paste0(v,"_dlog")]] <- c(NA, diff(dataset_tr[[v]] %>% log()))
  } else {
    dataset_tr[[paste0(v,"_diff")]] <- c(NA, diff(dataset_tr[[v]]))
  }
}
saveRDS(dataset_tr, file.path("data","data_transformed_for_modeling.rds"))
readr::write_csv(dataset_tr, file.path("data","data_transformed_for_modeling.csv"), na = "")

# Visual rápido de IPC general log-dif
if ("ipc_general_log" %in% names(dataset_tr)) {
  p_ipc_dlog <- ggplot(dataset_tr, aes(fecha, ipc_general_dlog)) + geom_line() +
    labs(title = "IPC general (Δ log)", x = NULL, y = NULL) + theme_minimal(base_size = 12)
  save_plot(p_ipc_dlog, "ipc_general_dlog.png", w=10, h=4)
}

# ==== 10) Correlaciones cruzadas (CCF) precios → IPC/IPRI ====
target_vars <- intersect(c("ipc_general","vivienda_agua_electricidad_gas","ipri_total"), names(dataset_tr))
price_vars  <- vars_precios

ccf_results <- list()
for (tvar in target_vars) {
  for (pvar in price_vars) {
    x <- dataset_tr[[pvar]]
    y <- dataset_tr[[tvar]]
    if (sum(!is.na(x))>24 && sum(!is.na(y))>24) {
      # Usamos Δlog si disponible
      y_use <- if (paste0(tvar,"_dlog") %in% names(dataset_tr)) dataset_tr[[paste0(tvar,"_dlog")]] else y
      x_use <- if (paste0(pvar,"_dlog") %in% names(dataset_tr)) dataset_tr[[paste0(pvar,"_dlog")]] else x
      ok <- complete.cases(y_use, x_use)
      if (sum(ok) > 24) {
        ccf_xy <- ccf(y_use[ok], x_use[ok], lag.max = 18, plot = FALSE)
        # Extraemos el lag con mayor correlación absoluta
        idx <- which.max(abs(ccf_xy$acf))
        best_lag <- ccf_xy$lag[idx]
        best_acf <- ccf_xy$acf[idx]
        ccf_results[[paste(tvar, pvar, sep="~")]] <- data.frame(
          target = tvar, price = pvar, best_lag = as.numeric(best_lag), best_ccf = as.numeric(best_acf)
        )
      }
    }
  }
}
if (length(ccf_results) > 0) {
  ccf_tbl <- dplyr::bind_rows(ccf_results)
  readr::write_csv(ccf_tbl, file.path("output","ccf_prices_to_inflation.csv"))
}

message("Exploratorio completado. Revisa la carpeta ./output para gráficos y tablas.")
