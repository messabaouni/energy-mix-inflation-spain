# 03_correlation_pca.R
# ------------------------------------------------------------
# TFM: Mix energético, precios mayoristas e impacto en IPC/IPRI
# Objetivo: preparar el mix energético como participaciones (%),
#           realizar PCA (sobre correlaciones) y clustering de perfiles.
# Entradas: ./data/data_clean.rds (de 01) 
# Salidas : ./output/* (gráficos y tablas), ./data/data_pca_outputs.rds
# ------------------------------------------------------------

# ==== 0) Paquetes ====
req_pkgs <- c("readr","dplyr","tidyr","stringr","janitor","lubridate",
              "ggplot2","scales","FactoMineR","factoextra","psych","cluster","patchwork")
for (p in req_pkgs) {
  if (!requireNamespace(p, quietly = TRUE)) {
    install.packages(p, repos = "https://cloud.r-project.org")
  }
}
library(readr); library(dplyr); library(tidyr); library(stringr); library(janitor); library(lubridate)
library(ggplot2); library(scales); library(FactoMineR); library(factoextra); library(psych); library(cluster); library(patchwork)

# ==== 1) Cargar datos ====
dataset_all <- readRDS(file.path("data","data_clean.rds")) %>% arrange(fecha)

# ==== 2) Identificar columnas del mix ====
mix_vars <- names(dataset_all) %>%
  keep(~ str_detect(.x, "hidraul|nuclear|carbon|fuel|ciclo|eolica|fotovoltaica|termica|otras_renovab|renovab|cogeneracion|residuos|generacion_total")) %>%
  unique()

# Priorizar una única columna de total
total_candidates <- c("generacion_total","generación_total","total_generacion","total")
total_col <- intersect(names(dataset_all), total_candidates)
if (length(total_col) == 0) {
  # si no hay total, intentamos construirlo sumando categorías que no sean precios ni IPC/IPRI
  part_cols <- mix_vars[!mix_vars %in% total_candidates]
  dataset_all <- dataset_all %>% mutate(generacion_total_calc = rowSums(across(all_of(part_cols)), na.rm = TRUE))
  total_col <- "generacion_total_calc"
} else {
  total_col <- total_col[1]
}

# Excluir el total del set de variables de mix
mix_part_cols <- setdiff(mix_vars, c(total_candidates, total_col))
mix_part_cols <- intersect(mix_part_cols, names(dataset_all))

stopifnot(length(mix_part_cols) >= 4) # necesitamos varias fuentes para una PCA útil

# ==== 3) Crear participaciones (%) del mix ====
# Si los datos están en GWh/MWh por mes, convertimos a share
mix_share <- dataset_all %>%
  select(fecha, all_of(mix_part_cols), all_of(total_col)) %>%
  mutate(across(all_of(mix_part_cols), ~ .x / .data[[total_col]], .names = "{.col}_share")) %>%
  select(fecha, ends_with("_share"))

# Comprobar suma aproximada a 1 (puede no cerrar al 100% si faltan categorías)
mix_share$sum_share <- mix_share %>% select(-fecha) %>% rowSums(na.rm = TRUE)
share_summary <- mix_share %>% summarise(min_sum = min(sum_share, na.rm=TRUE),
                                         p50_sum = quantile(sum_share, 0.5, na.rm=TRUE),
                                         max_sum = max(sum_share, na.rm=TRUE))
readr::write_csv(share_summary, file.path("output","mix_share_closure_check.csv"))

# ==== 4) Matriz para PCA (sobre correlaciones) ====
X <- mix_share %>% select(-fecha, -sum_share) %>% drop_na()

# Escalado (al usar cor, el escalado es implícito; pero estandarizamos por claridad)
X_scaled <- scale(X)

# ==== 5) PCA con prcomp (correlaciones) ====
pca_fit <- prcomp(X_scaled, center = TRUE, scale. = TRUE)

# Varianza explicada
eig_vals <- pca_fit$sdev^2
var_explained <- eig_vals / sum(eig_vals)
eig_tbl <- data.frame(
  component = paste0("PC", seq_along(eig_vals)),
  eigenvalue = eig_vals,
  prop_var = var_explained,
  cum_var = cumsum(var_explained)
)
readr::write_csv(eig_tbl, file.path("output","pca_eigenvalues.csv"))

# ==== 6) Cargas (loadings) y scores ====
loadings <- pca_fit$rotation %>% as.data.frame() %>%
  tibble::rownames_to_column("variable")
scores <- pca_fit$x %>% as.data.frame() %>%
  tibble::rownames_to_column("rowid") %>%
  mutate(rowid = as.integer(rowid))

# Vincular scores con fechas
scores_with_date <- mix_share %>%
  drop_na() %>%
  mutate(rowid = dplyr::row_number()) %>%
  select(fecha, rowid) %>%
  left_join(scores, by = "rowid")

readr::write_csv(loadings, file.path("output","pca_loadings.csv"))
readr::write_csv(scores_with_date, file.path("output","pca_scores_by_date.csv"))

# ==== 7) Gráficos =====

# Scree plot
p_scree <- ggplot(eig_tbl, aes(x = factor(component, levels = component), y = prop_var)) +
  geom_col() +
  geom_line(aes(group=1, y = cum_var)) +
  geom_point(aes(y = cum_var)) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(title = "PCA del mix energético: varianza explicada",
       x = "Componentes principales", y = "Proporción de varianza") +
  theme_minimal(base_size = 12)

ggsave(file.path("output","pca_scree.png"), p_scree, width = 10, height = 5, dpi = 120)

# Biplot 2D (PC1 vs PC2)
p_biplot <- factoextra::fviz_pca_biplot(
  pca_fit, repel = TRUE, label = "var", habillage = NULL
) + ggtitle("Biplot PCA (PC1 vs PC2) – Mix energético")
ggplot2::ggsave(file.path("output","pca_biplot_pc1_pc2.png"), p_biplot, width = 10, height = 7, dpi = 120)

# Cargas ordenadas por PC1 y PC2
top_loadings <- loadings %>%
  pivot_longer(starts_with("PC"), names_to = "PC", values_to = "loading") %>%
  group_by(PC) %>%
  arrange(desc(abs(loading)), .by_group = TRUE)

readr::write_csv(top_loadings, file.path("output","pca_top_loadings_by_pc.csv"))

# Heatmap de loadings
ld_long <- loadings %>%
  pivot_longer(starts_with("PC"), names_to = "PC", values_to = "loading")
p_ld <- ggplot(ld_long, aes(PC, variable, fill = loading)) +
  geom_tile() +
  scale_fill_gradient2(low = "steelblue", mid = "white", high = "firebrick") +
  labs(title = "Matriz de cargas (loadings) de la PCA", x = NULL, y = NULL) +
  theme_minimal(base_size = 12)
ggsave(file.path("output","pca_loadings_heatmap.png"), p_ld, width = 9, height = 8, dpi = 120)

# Trayectoria temporal de PC1 y PC2
if (all(c("PC1","PC2") %in% names(scores_with_date))) {
  p_pc1 <- ggplot(scores_with_date, aes(fecha, PC1)) + geom_line() +
    labs(title = "PC1 (perfil de mix) en el tiempo", x = NULL, y = "Score") +
    theme_minimal(base_size = 12)
  p_pc2 <- ggplot(scores_with_date, aes(fecha, PC2)) + geom_line() +
    labs(title = "PC2 (perfil de mix) en el tiempo", x = NULL, y = "Score") +
    theme_minimal(base_size = 12)
  ggsave(file.path("output","pca_pc1_series.png"), p_pc1, width = 10, height = 4, dpi = 120)
  ggsave(file.path("output","pca_pc2_series.png"), p_pc2, width = 10, height = 4, dpi = 120)
}

# ==== 8) Clustering de perfiles (k-means) sobre scores ====
# Usamos los primeros PCs necesarios para explicar ~80% de varianza
k_cut <- which(cumsum(var_explained) >= 0.80)[1]
if (is.na(k_cut) || k_cut < 2) k_cut <- min(3, ncol(scores_with_date) - 1)
pc_cols <- paste0("PC", seq_len(k_cut))
pc_cols <- intersect(pc_cols, names(scores_with_date))

set.seed(123)
# Selección de k por inspección rápida (2..6) con total withinss
wss <- sapply(2:6, function(k){
  km <- kmeans(scale(scores_with_date[, pc_cols]), centers = k, nstart = 50)
  km$tot.withinss
})
wss_tbl <- data.frame(k = 2:6, tot_withinss = wss)
readr::write_csv(wss_tbl, file.path("output","kmeans_wss_by_k.csv"))

# Elegimos k=3 por defecto (ajustable según codo)
k_opt <- 3
km_fit <- kmeans(scale(scores_with_date[, pc_cols]), centers = k_opt, nstart = 100)
scores_with_date$cluster <- factor(km_fit$cluster)

# Visualización PC1-PC2 coloreado por cluster
if (all(c("PC1","PC2") %in% names(scores_with_date))) {
  p_clusters <- ggplot(scores_with_date, aes(PC1, PC2, color = cluster)) +
    geom_point(alpha = 0.8) +
    labs(title = sprintf("Clustering k-means (k=%d) sobre PC1-PC%d", k_opt, 2), x = "PC1", y = "PC2") +
    theme_minimal(base_size = 12)
  ggsave(file.path("output","kmeans_pc1_pc2.png"), p_clusters, width = 8, height = 6, dpi = 120)
}

# Series por cluster: PC1 en el tiempo
p_pc1_cl <- ggplot(scores_with_date, aes(fecha, PC1, color = cluster)) +
  geom_line() +
  labs(title = "PC1 por cluster a lo largo del tiempo", x = NULL, y = "PC1") +
  theme_minimal(base_size = 12)
ggsave(file.path("output","pc1_series_by_cluster.png"), p_pc1_cl, width = 10, height = 4, dpi = 120)

# ==== 9) Guardar objetos y datasets derivados ====
out_list <- list(
  mix_share = mix_share,
  X = X,
  X_scaled = X_scaled,
  pca_fit = pca_fit,
  eigen = eig_tbl,
  loadings = loadings,
  scores = scores_with_date,
  kmeans = list(k_opt = k_opt, fit = km_fit)
)
saveRDS(out_list, file.path("data","data_pca_outputs.rds"))
readr::write_csv(mix_share, file.path("data","mix_share_timeseries.csv"), na = "")
message("Listo: PCA y clustering generados. Revisa la carpeta ./output y ./data.")
