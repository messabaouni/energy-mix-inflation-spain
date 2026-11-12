# 01_load_clean_data.R
# ------------------------------------------------------------
# TFM: Mix energético, precios mayoristas e impacto en IPC/IPRI
# Objetivo: cargar, limpiar y estandarizar el dataset mensual unificado
# Entrada:   ./data/data.csv   (separador ';')
# Salidas:   ./data/data_clean.csv, ./data/data_clean.rds
# Objeto en memoria: dataset_all
# ------------------------------------------------------------

# ==== 0) Paquetes ====
req_pkgs <- c("readr","dplyr","tidyr","stringr","janitor","lubridate")
for (p in req_pkgs) {
  if (!requireNamespace(p, quietly = TRUE)) {
    install.packages(p, repos = "https://cloud.r-project.org")
  }
}
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(janitor)
library(lubridate)

# ==== 1) Ruta y lectura cruda ====
# Nota: el archivo original está separado por ';'
infile <- file.path("data", "data.csv")

raw <- read_delim(
  infile,
  delim = ";",
  trim_ws = TRUE,
  locale = locale(decimal_mark = ".", grouping_mark = ","),
  show_col_types = FALSE
)

# Si por algún motivo vino como UNA sola columna, volvemos a intentar detectando separador
if (ncol(raw) == 1) {
  message("Detectada una sola columna. Releyendo como texto y separando por ';'...")
  raw_txt <- readr::read_file(infile)
  con <- textConnection(raw_txt)
  raw <- read_delim(
    con,
    delim = ";",
    trim_ws = TRUE,
    locale = locale(decimal_mark = ".", grouping_mark = ","),
    show_col_types = FALSE
  )
  close(con)
}

# ==== 2) Normalizar nombres de columnas ====
raw <- raw %>% janitor::clean_names()  # snake_case, sin acentos

# Intentar detectar la columna de fecha
# Aceptamos nombres comunes: fecha, period, periodo, date, month, mes
date_col_candidates <- c("fecha","periodo","period","date","mes","month")
date_col <- intersect(names(raw), date_col_candidates)
if (length(date_col) == 0) {
  stop("No se ha encontrado una columna de fecha. Renombra tu columna de fecha (p.ej. 'fecha').")
} else {
  date_col <- date_col[1]
}

# ==== 3) Parsear fecha mensual ====
# Intentamos YYYY-MM-DD, YYYY-MM, o formatos similares
dataset_all <- raw %>%
  mutate(
    # Intento 1: ymd (YYYY-MM-DD)
    "{date_col}" := suppressWarnings(lubridate::ymd(.data[[date_col]]))
  )

if (any(is.na(dataset_all[[date_col]]))) {
  # Intento 2: tratar como YYYY-MM o YYYY/MM
  dataset_all <- dataset_all %>%
    mutate("{date_col}" := suppressWarnings(lubridate::ym(str_replace(.data[[date_col]], "[/]", "-"))))
}

# Si sigue habiendo NA en fecha, avisar
if (any(is.na(dataset_all[[date_col]]))) {
  warning("Algunas fechas no se han podido parsear. Revisa el formato de la columna de fecha.")
}

# ==== 4) Convertir columnas numéricas ====
# Excluimos la columna de fecha de la conversión
num_cols <- setdiff(names(dataset_all), date_col)

# Algunas columnas pueden venir con caracteres, % o comas: usamos parse_number
dataset_all <- dataset_all %>%
  mutate(across(all_of(num_cols), ~ readr::parse_number(as.character(.x), locale = locale(decimal_mark=".", grouping_mark=","))))

# ==== 5) Ordenar, quitar duplicados, chequear faltantes ====
dataset_all <- dataset_all %>%
  arrange(.data[[date_col]]) %>%
  distinct()

# Reporte rápido
message("Estructura:")
print(str(dataset_all))

message("\nResumen de NAs por columna:")
print(colSums(is.na(dataset_all)))

# ==== 6) Guardar datos limpios ====
out_csv <- file.path("data", "data_clean.csv")
out_rds <- file.path("data", "data_clean.rds")

readr::write_csv(dataset_all, out_csv, na = "")
saveRDS(dataset_all, out_rds)

message("\nGuardado: ", out_csv, " y ", out_rds)
message("Objeto disponible en memoria: dataset_all")
