# library(rgee)
# library(reticulate)
# 
# reticulate::use_python("C:/Users/Andini A/.conda/envs/rgee/python.exe",
#            required = TRUE)
# reticulate::py_config()
# reticulate::conda_list()
# ee_Authenticate()
# ee_Initialize()
# 
# xmin <- 113.53203460599778
# ymin <- -2.4012625094410396
# xmax <- 114.15163037695532
# ymax <- -1.650156917002786
# 
# aoi <- ee$Geometry$BBox(xmin, ymin, xmax, ymax)
# 
# mask_s2_clouds <- function(image) {
#   scl <- image$select("SCL")
#   clear_pixels <- scl$neq(3)$And(scl$neq(8))$And(scl$neq(9))$And(scl$neq(10))
#   return(image$updateMask(clear_pixels))
# }
# 
# add_indices <- function(image) {
#   ndvi <- image$normalizedDifference(c("B8", "B4"))$rename("NDVI")
#   nbr  <- image$normalizedDifference(c("B8", "B12"))$rename("NBR")
#   return(image$addBands(c(ndvi, nbr)))
# }
# 
# s2_col <- ee$ImageCollection("COPERNICUS/S2_SR_HARMONIZED")$
#   filterBounds(aoi)$
#   map(mask_s2_clouds)$
#   map(add_indices)
# 
# juli <- s2_col$filterDate("2025-07-01", "2025-07-31")$median()$clip(aoi)
# agustus <- s2_col$filterDate("2025-08-01", "2025-08-31")$median()$clip(aoi)
# september <- s2_col$filterDate("2025-09-01", "2025-09-30")$median()$clip(aoi)
# 
# monthly_list <- list(
#   Jul = juli,
#   Aug = agustus,
#   Sep = september
# )
# 
# indices <- c("NDVI", "NBR")
# 
# for (m in names(monthly_list)) {
#   for (idx in indices) {
#     task <- ee_image_to_drive(
#       image = monthly_list[[m]]$select(idx),
#       description = paste0("S2_", idx, "_Palangkaraya_", m),
#       folder = "FINAL",
#       scale = 10,
#       region = aoi
#     )
#     task$start()
#   }
# }
# ee_monitoring()

workdir <- ("C:/an/ETS/")
setwd(workdir)
getwd() #cek

library(terra)
library(dplyr)
library(ggplot2)
library(patchwork)

shp_pky <- vect("KotaPalangkaraya/Palangka_Raya.shp")

m_ndvi <- matrix(c(
  -Inf, 0.30, 1, 
  0.30, 0.60, 2, 
  0.60, 0.78, 3, 
  0.78,  Inf, 4  
), ncol = 3, byrow = TRUE)

m_nbr <- matrix(c(
  -Inf, 0.05, 1, 
  0.05, 0.35, 2, 
  0.35, 0.60, 3, 
  0.60,  Inf, 4  
), ncol = 3, byrow = TRUE)

lbls_ndvi <- c("Non-Vegetasi", "Jarang", "Sedang", "Rapat")
lbls_nbr <- c("Terganggu Tinggi", "Terganggu Sedang", "Normal", "Sehat/Lembap")

cols_ndvi <- c(
  "Non-Vegetasi" = "#d7191c", # Merah Cerah
  "Jarang"       = "#fdae61", # Oranye-Kuning
  "Sedang"       = "#a6d96a", # Hijau Muda
  "Rapat"        = "#1a9641"  # Hijau Tua
)

cols_nbr <- c(
  "Terganggu Tinggi" = "#67000d", # Maroon Pekat (Bekeras/Terbakar Parah)
  "Terganggu Sedang" = "#f46d43", # Oranye Cerah
  "Normal"           = "#fee08b", # Kuning Lembut / Jerami
  "Sehat/Lembap"     = "#2b83ba"  # Biru Teal (Simbol Area Lembap/Air)
)

cols_all <- c(cols_ndvi, cols_nbr)

get_summary <- function(month, index_type, shp, path_folder = "Res_RGEE") {
  file_path <- sprintf("%s/S2_%s_Palangkaraya_%s_2026.tif", path_folder, index_type, month)
  r <- rast(file_path)
  r_cut <- crop(r, shp, mask = TRUE)
  is_ndvi <- index_type == "NDVI"
  m <- if (is_ndvi) m_ndvi else m_nbr
  lbls <- if (is_ndvi) lbls_ndvi else lbls_nbr
  r_class <- classify(r_cut, m, right = FALSE)
  freq_df <- as.data.frame(freq(r_class))
  freq_df %>%
    filter(!is.na(value)) %>%
    transmute(
      Bulan = factor(month, levels = c("Jul", "Aug", "Sep")),
      Indeks = index_type,
      Kategori = factor(lbls[value], levels = lbls),
      Piksel = count,
      Luas_ha = (count * 100) / 10000,
      Luas_ribu_ha = Luas_ha / 1000 # ribu Ha
    )
}

param_grid <- expand.grid(
  month = c("Jul", "Aug", "Sep"),
  index_type = c("NDVI", "NBR"),
  stringsAsFactors = FALSE
)

df_summary <- bind_rows(lapply(1:nrow(param_grid), function(i) {
  get_summary(param_grid$month[i], param_grid$index_type[i], shp_pky)
}))

plot_map_and_chart <- function(target_month, target_index, df_sum, shp, path_folder = "Res_RGEE", sample_fact = 5) {
  file_path <- sprintf("%s/S2_%s_Palangkaraya_%s_2026.tif", path_folder, target_index, target_month)
  r <- rast(file_path)
  r_cut <- crop(r, shp, mask = TRUE)
  
  is_ndvi <- target_index == "NDVI"
  m <- if (is_ndvi) m_ndvi else m_nbr
  lbls <- if (is_ndvi) lbls_ndvi else lbls_nbr
  pal <- if (is_ndvi) cols_ndvi else cols_nbr
  
  r_class <- classify(r_cut, m, right = FALSE)
  
  if (sample_fact > 1) {
    r_plot <- aggregate(r_class, fact = sample_fact, fun = "modal")
  } else {
    r_plot <- r_class
  }
  
  df_map <- as.data.frame(r_plot, xy = TRUE)
  names(df_map)[3] <- "Kelas_Val"
  
  df_map <- df_map %>%
    filter(!is.na(Kelas_Val)) %>%
    mutate(Kategori = factor(lbls[Kelas_Val], levels = lbls))
  
  sub_sum <- df_sum %>% filter(Indeks == target_index, Bulan == target_month)
  
  p_map <- ggplot(df_map, aes(x = x, y = y, fill = Kategori)) +
    geom_raster() +
    coord_equal() +
    scale_fill_manual(values = pal, drop = FALSE) +
    theme_void() +
    labs(title = paste("Peta Sebaran", target_index, "-", target_month)) +
    theme(
      legend.position = "bottom",
      plot.title = element_text(face = "bold", size = 12, hjust = 0.5)
    )
  
  p_chart <- ggplot(sub_sum, aes(x = Kategori, y = Luas_ribu_ha, fill = Kategori)) +
    geom_col(show.legend = FALSE) +
    scale_fill_manual(values = pal, drop = FALSE) +
    scale_y_continuous(labels = scales::comma) +
    theme_minimal() +
    labs(
      title = "Distribusi Luas",
      x = NULL,
      y = "Luas (Ribu Ha)"
    ) +
    theme(
      axis.text.x = element_text(angle = 30, hjust = 1),
      plot.title = element_text(face = "bold", size = 11, hjust = 0.5)
    )
  
  p_map + p_chart + plot_layout(widths = c(2, 1))
}

plot_map_and_chart("Jul", "NDVI", df_summary, shp_pky)
plot_map_and_chart("Aug", "NDVI", df_summary, shp_pky)
plot_map_and_chart("Sep", "NDVI", df_summary, shp_pky)
plot_map_and_chart("Jul", "NBR", df_summary, shp_pky)
plot_map_and_chart("Aug", "NBR", df_summary, shp_pky)
plot_map_and_chart("Sep", "NBR", df_summary, shp_pky)

build_trend_plot <- function(df_data, include_dense = TRUE) {
  
  if (include_dense) {
    df_ndvi <- df_data %>% filter(Indeks == "NDVI")
    df_nbr  <- df_data %>% filter(Indeks == "NBR")
    sub_title <- "Semua Kategori Tutupan & Kondisi Lahan"
  } else {
    df_ndvi <- df_data %>% filter(Indeks == "NDVI", Kategori != "Rapat")
    df_nbr  <- df_data %>% filter(Indeks == "NBR", Kategori != "Sehat/Lembap")
    sub_title <- "Fokus Kategori Terganggu & Non-Rapat"
  }
  
  p_ndvi <- ggplot(df_ndvi, aes(x = Bulan, y = Luas_ribu_ha, fill = Kategori)) +
    geom_col(position = "dodge", width = 0.7) +
    scale_fill_manual(values = cols_ndvi) +
    scale_y_continuous(labels = scales::comma) +
    theme_minimal() +
    labs(
      title = "NDVI",
      x = "Bulan",
      y = "Luas (Ribu Ha)",
      fill = "Kategori NDVI"
    ) +
    theme(
      plot.title = element_text(face = "bold", size = 11, hjust = 0.5),
      legend.position = "bottom",
      legend.title = element_text(face = "bold", size = 10)
    ) +
    guides(fill = guide_legend(nrow = 2))
  
  p_nbr <- ggplot(df_nbr, aes(x = Bulan, y = Luas_ribu_ha, fill = Kategori)) +
    geom_col(position = "dodge", width = 0.7) +
    scale_fill_manual(values = cols_nbr) +
    scale_y_continuous(labels = scales::comma) +
    theme_minimal() +
    labs(
      title = "NBR",
      x = "Bulan",
      y = NULL,
      fill = "Kategori NBR"
    ) +
    theme(
      plot.title = element_text(face = "bold", size = 11, hjust = 0.5),
      legend.position = "bottom",
      legend.title = element_text(face = "bold", size = 10)
    ) +
    guides(fill = guide_legend(nrow = 2))
  
  (p_ndvi + p_nbr) +
    plot_annotation(
      title = "Tren Perubahan Luasan Wilayah Kota Palangka Raya (Jul - Sep)",
      subtitle = sub_title,
      theme = theme(
        plot.title = element_text(face = "bold", size = 13),
        plot.subtitle = element_text(size = 10)
      )
    )
}

p_trend_full <- build_trend_plot(df_summary, include_dense = TRUE)
print(p_trend_full)

p_trend_filtered <- build_trend_plot(df_summary, include_dense = FALSE)
print(p_trend_filtered)
