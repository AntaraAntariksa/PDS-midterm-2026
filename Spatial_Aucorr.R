library(sf)
library(tmap)
library(terra)
library(dplyr)
library(spData)
library(spdep)
library(ggplot2)
library(patchwork)
library(exactextractr)

#atur direktori
workdir <- ("C:/an/ETS/")
setwd(workdir)
getwd() #cek

# ========================================================================
# 0. LOAD DATA DAN RAPIHIN DATA
# ========================================================================
desa <- st_read("KotaPalangkaraya/Desa_Palangkaraya.shp") |> st_transform(32749)
hotspot <- st_read("hotspot_kalteng_bersih.shp") |> st_transform(32749)
plot(st_geometry(desa))
plot(st_geometry(hotspot), add = TRUE)

gdb_path <- "KotaPalangkaraya/RBI25K_KOTA PALANGKARAYA_KUGI50.gdb"
st_layers(gdb_path)

jalan   <- st_read(dsn = gdb_path, layer = "JALAN_LN_25K") |> 
  st_transform(32749)
sungai  <- st_read(dsn = gdb_path, layer = "SUNGAI_LN_25K") |> 
  st_transform(32749)
irigasi <- st_read(dsn = gdb_path, layer = "IRIGASI_LN_25K") |> 
  st_transform(32749)

# merge sungai dan irigasi jadi drainase
sungai_clean <- st_sf(
  tipe = "Sungai", 
  geometry = st_geometry(sungai))
irigasi_clean <- st_sf(
  tipe = "Irigasi", 
  geometry = st_geometry(irigasi))
drainase <- bind_rows(sungai_clean, irigasi_clean)

# cek plot
plot(st_geometry(drainase), col = "blue", add = TRUE)
plot(st_geometry(jalan), col = "darkgreen", add = TRUE)

temp_raster  <- rast(ext(desa), resolution = 30, crs = crs(desa))
jalan_vect   <- vect(jalan)
jalan_raster <- rasterize(jalan_vect, temp_raster, field = 1)
dist_raster  <- distance(jalan_raster)
plot(dist_raster)

# ========================================================================
# 1. PILIH SATUAN ANALISIS ("desa" ATAU "grid")
# ========================================================================
pilihan_batas <- "grid" #desa atau grid 1x1 km

if (pilihan_batas == "desa") {
  input <- desa |>
    mutate(
      id_unit  = as.character(NAME_3), #NAME_3 untuk tingkat kecamatan   
      luas_km2 = as.numeric(st_area(geometry)) / 1e6)
  style <- "S" #buat input nb2listw
  
} else if (pilihan_batas == "grid") {
  grid_raw <- st_make_grid(
    desa,
    cellsize = c(1000, 1000), 
    square   = TRUE)
  input <- st_sf(geometry = grid_raw) |>
    st_filter(desa) |>                           
    mutate(
      id_unit  = paste0("GRID_", row_number()),  
      luas_km2 = 1)
  style <- "W" #buat input nb2listw
  
} else {
  stop("Pilihan tidak valid! Pilih 'desa' atau 'grid'.")
}

# ========================================================================
# 2. EKSTRAK VARIABEL KE SATUAN ANALISIS
# ========================================================================
# HOTSPOT DENSITY
input$hotspot_count <- lengths(st_intersects(input, hotspot))
input <- input |>
  mutate(hotspot_density = hotspot_count / luas_km2)

# DRAINAGE DENSITY
drainase_intersect <- st_intersection(drainase, input) |>
  mutate(panjang_kanal_km = as.numeric(st_length(geometry)) / 1000)

drainase_summary <- drainase_intersect |>
  st_drop_geometry() |>
  group_by(id_unit) |>                
  summarise(tot_panjang_kanal = sum(panjang_kanal_km, na.rm = TRUE))

input <- input |>
  left_join(drainase_summary, by = "id_unit") |>
  mutate(
    tot_panjang_kanal = ifelse(is.na(tot_panjang_kanal), 0, tot_panjang_kanal),
    drain_density     = tot_panjang_kanal / luas_km2)

# RATA-RATA JARAK TERHADAP JALAN
input$mean_dist_road_m <- exact_extract(dist_raster, input, 'mean')

# CEK OUTPUT AKHIR
head(input |> select(id_unit, hotspot_density, drain_density, mean_dist_road_m))

# ========================================================================
# 3. REGRESI LINEAR (BIVARIAT DAN BERGANDA)
# ========================================================================
clean <- input %>% st_drop_geometry()

# 3.1 Exploratory Data Analysis
eda_summary <- clean %>%
  summarise(
    across(
      c(hotspot_density, drain_density, mean_dist_road_m),
      list(
        mean   = ~mean(.x, na.rm = TRUE),
        sd     = ~sd(.x, na.rm = TRUE),
        median = ~median(.x, na.rm = TRUE),
        min    = ~min(.x, na.rm = TRUE),
        max    = ~max(.x, na.rm = TRUE)
      ),
      .names = "{.col}_{.fn}"
    )
  )
t(eda_summary)

# 3.2 Regresi Linear (Bivariat)
# Densitas Drainase -> Hotspot
reg_drain <- lm(hotspot_density ~ drain_density, data = clean)
summary(reg_drain)

# Jarak ke Jalan -> Hotspot
reg_road <- lm(hotspot_density ~ mean_dist_road_m, data = clean)
summary(reg_road)

# 3.3 Regresi Linear Berganda
ols_model <- lm(hotspot_density ~ drain_density + mean_dist_road_m, data = clean)
summary(ols_model)
###################
# 3.4 Plot grafik
###################
r2_drain  <- paste0("R² = ", round(summary(reg_drain)$r.squared, 4))
p1 <- ggplot(clean, aes(x = drain_density, y = hotspot_density)) +
  geom_point(color = "red", size = 2.5, alpha = 0.7) +
  geom_smooth(method = "lm", color = "black", se = FALSE) +
  annotate("text", x = 0.05, y = 9, label = r2_drain, hjust = 0, size = 3.8) +
  labs(
    title = "Scatterplot Kanal vs Titik Panas",
    x = "Kepadatan Drainase (km/km²)",
    y = "Kepadatan Titik Panas"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5)
  )

r2_road <- paste0("R² = ", round(summary(reg_road)$r.squared, 6))
p2 <- ggplot(clean, aes(x = mean_dist_road_m, y = hotspot_density)) +
  geom_point(color = "navy", size = 2.5, alpha = 0.7) +
  geom_smooth(method = "lm", color = "black", se = FALSE) +
  annotate("text", x = 1000, y = 9, label = r2_road, hjust = 0, size = 3.8) +
  labs(
    title = "Scatterplot Jarak Jalan vs Hotspot",
    x = "Rata-rata Jarak terhadap Jalan (m)",
    y = "Kepadatan Titik Panas"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5)
  )

p1 + p2

var_list <- c("hotspot_density", "drain_density", "mean_dist_road_m")
clean_sf <- input %>%
  mutate(
    hotspot_density  = as.numeric(hotspot_density),
    drain_density    = as.numeric(drain_density),
    mean_dist_road_m = as.numeric(mean_dist_road_m)
  )

hotspot_pr <- st_filter(hotspot, clean_sf)
m_hotspot <- tm_shape(clean_sf) +
  tm_polygons(
    fill = "hotspot_density",
    fill.scale = tm_scale(values = "-magma"),      
    fill.legend = tm_legend(title = "Kepadatan"),
    col = "grey30",                                
    lwd = 0.5
  ) +
  tm_shape(hotspot_pr) +
  tm_dots(
    fill = "red",                                 
    size = 0.5, 
    fill_alpha = 0.7                              
  ) +
  tm_grid(alpha = 0.4) +
  tm_compass(position = tm_pos_in("right", "top")) +
  tm_scalebar(position = tm_pos_in("left", "bottom")) + 
  tm_title(
    text = "Kepadatan & Sebaran Titik Panas",
    size = 1.3,
    fontface = "bold",
    position = tm_pos_out("center", "top")
  ) +
  tm_title(
    text = "Periode 1-31 Agustus 2026",
    size = 0.8,
    fontface = "plain",
    position = tm_pos_out("center", "top")
  ) +
  tm_layout(
    legend.position = tm_pos_out("right", "center"),
    component.autoscale = FALSE
  )
m_hotspot

m_drain <- tm_shape(clean_sf) +
  tm_polygons(
    fill = "drain_density",
    fill.scale = tm_scale(values = "-magma"),
    fill.legend = tm_legend(
      title = "Kepadatan (km/km²)",
      position = tm_pos_out("right", "center")
    ),
    col = "grey30",
    lwd = 0.1
  ) +
  tm_grid(alpha = 0.4) +
  tm_compass(position = tm_pos_in("right", "top")) +
  tm_scalebar(position = tm_pos_in("left", "bottom")) + 
  tm_title(
    text = "Kepadatan Drainase",
    size = 1.1,
    fontface = "bold",
    position = tm_pos_out("center", "top")
  ) +
  tm_layout(
    component.autoscale = FALSE
  )
m_drain

m_road <- tm_shape(clean_sf) +
  tm_polygons(
    fill = "mean_dist_road_m",
    fill.scale = tm_scale(values = "-magma"), 
    fill.legend = tm_legend(
      title = "Jarak (m)",
      position = tm_pos_out("right", "center")
    ),
    col = "grey30",
    lwd = 0.1
  ) +
  tm_grid(alpha = 0.4) +
  tm_compass(position = tm_pos_in("right", "top")) +
  tm_scalebar(position = tm_pos_in("left", "bottom")) + 
  tm_title(
    text = "Rata-rata Jarak terhadap Jalan",
    size = 1.1,
    fontface = "bold",
    position = tm_pos_out("center", "top")
  ) +
  tm_layout(
    component.autoscale = FALSE
  )
m_road

clean_centroids <- st_centroid(clean_sf)
make_bivariate_tmap <- function(poly_data, 
                                point_data, 
                                var_name, 
                                point_color, 
                                legend_title_point, 
                                title, 
                                grid_spacing = NULL) {
  hotspot_base <- tm_shape(poly_data) +
    tm_polygons(
      fill = "hotspot_density",
      fill.scale = tm_scale(values = "-magma"),
      fill.legend = tm_legend(
        title = "Kepadatan Titik Panas",
        position = tm_pos_out("right", "center")
      ),
      col = "grey40",
      lwd = 0.3
    )
  hotspot_biv <- hotspot_base +
    tm_shape(point_data) +
    tm_symbols(
      size = var_name,
      fill = point_color,
      col = "black",
      lwd = 0.3,
      size.legend = tm_legend(
        title = legend_title_point,
        position = tm_pos_out("right", "center")
      )
    ) +
    tm_title(
      text = title,
      size = 1.1,
      fontface = "bold",
      position = tm_pos_out("center", "top")
    ) +
    tm_grid(alpha = 0.4)+
    tm_compass(position = tm_pos_in("right", "top")) +
    tm_scalebar(position = tm_pos_in("left", "bottom")) +
    tm_layout(component.autoscale = FALSE)
  return(hotspot_biv)
}

  # 1. Peta Drainage Density
map_drain <- make_bivariate_tmap(
    poly_data          = clean_sf,
    point_data         = clean_centroids,
    var_name           = "drain_density",
    point_color        = "magenta",
    legend_title_point = "Kerapatan Drainase (km/km²)",
    title              = "Peta Perbandingan Titik Panas dan Kerapatan Drainase",
  )
map_drain
  
  # 2. Peta Distance to Road
map_road <- make_bivariate_tmap(
    poly_data          = clean_sf,
    point_data         = clean_centroids,
    var_name           = "mean_dist_road_m",
    point_color        = "purple",
    legend_title_point = "Jarak Jalan (m)",
    title              = "Peta Perbandingan Titik Panas dan Jarak ke Jalan",
  )
map_road
# ========================================================================
# 4. MATRIKS BOBOT SPASIAL - QUEEN CONTIGUITY
# ========================================================================
nb <- poly2nb(input, queen = TRUE)
lw <- nb2listw(nb, style = style, zero.policy = TRUE)
summary(nb)

# ========================================================================
# 5. FUNGSI GLOBAL MORAN'S I (+ PLOT GRAFIK)
# ========================================================================
globalMoran <- function(variabel, warna = "steelblue", main_title = NULL) {
  if (is.null(main_title)) main_title <- paste("Moran Plot - Variabel", variabel)
  x <- input[[variabel]]
  moran <- moran.test(x = x,
                      listw = lw,
                      alternative = 'greater',
                      zero.policy = TRUE)
  print(moran)
  moran.plot(
    x = x, 
    listw = lw, 
    pch = 16, 
    col = warna,
    main = main_title,
    zero.policy = TRUE,
    xlab = variabel,
    ylab = paste("spatially lagged", variabel))
  res <- data.frame(
    Variabel     = variabel,
    Moran_I      = round(as.numeric(moran$estimate[["Moran I statistic"]]), 4),
    Expectation  = round(as.numeric(moran$estimate[["Expectation"]]), 4),
    P_Value      = round(moran$p.value, 5),
    Signifikan   = ifelse(moran$p.value <= 0.05, "Ya", "Tidak"))
  return(res)
}

# ========================================================================
# 6. FUNGSI LOCAL MORAN'S I (LISA)
# ========================================================================
lisaCalc <- function(variabel, alpha = 0.05, alternative = "two.sided", title = NULL) {
  if (is.null(title)) title <- paste("Peta LISA - Variabel", variabel)
  x <- input[[variabel]]
  z_x <- as.numeric(scale(x))
  lag_z_x <- lag.listw(lw, z_x, zero.policy = TRUE)
  local_m <- localmoran(z_x, lw, alternative = alternative, zero.policy = TRUE)
  data_result <- input |>
    mutate(
      z_val     = z_x,
      lag_z_val = lag_z_x,
      p_val     = local_m[, 5],
      quadrant = case_when(
        z_val >= 0 & lag_z_val >= 0 ~ "High-High",
        z_val < 0  & lag_z_val < 0  ~ "Low-Low",
        z_val >= 0 & lag_z_val < 0  ~ "High-Low",
        z_val < 0  & lag_z_val >= 0 ~ "Low-High"),
      quadrant_sig = ifelse(p_val <= alpha, quadrant, "Not Significant"))
  peta <- tm_shape(data_result) +
    tm_polygons(
      fill = "quadrant_sig",
      fill.scale = tm_scale_categorical(
        values = c(
          "High-High"       = "#d7191c",
          "Low-Low"         = "#2b83ba",
          "High-Low"        = "#fdae61",
          "Low-High"        = "#abdda4",
          "Not Significant" = "#e0e0e0"
        ),
        levels = c("High-High", "Low-Low", "High-Low", "Low-High", "Not Significant")
      ),
      fill.legend = tm_legend(
        title = "Kategori Klaster",
        position = tm_pos_out("right", "center") 
      )
    ) +
    tm_grid(alpha = 0.2) + # Grid default
    tm_compass(position = tm_pos_in("right", "top")) + 
    tm_scalebar(position = tm_pos_in("left", "bottom")) + 
    tm_title(
      text = title,
      size = 1.1,
      fontface = "bold",
      position = tm_pos_out("center", "top") 
    ) + 
    tm_layout(component.autoscale = FALSE)
  return(list(data = data_result, plot = peta))
}

# ========================================================================
# 7. FUNGSI GETIS ORD GI*
# ========================================================================
nb_self <- include.self(nb) 
lw_gi   <- nb2listw(nb_self, style = "B", zero.policy = TRUE)

giCalc <- function(variabel) {
  x <- input[[variabel]]
  gi_results <- localG(x, lw_gi, zero.policy = TRUE)
  gi_zscore  <- as.numeric(gi_results)
  data_res <- input |>
    mutate(
      gi_zscore = gi_zscore,
      gi_category = case_when(
        gi_zscore >= 2.58  ~ "Hotspot 99%",
        gi_zscore >= 1.96  ~ "Hotspot 95%",
        gi_zscore >= 1.65  ~ "Hotspot 90%",
        gi_zscore <= -2.58 ~ "Coldspot 99%",
        gi_zscore <= -1.96 ~ "Coldspot 95%",
        gi_zscore <= -1.65 ~ "Coldspot 90%",
        TRUE               ~ "Not Significant"
      ),
      gi_category = factor(gi_category, levels = c("Hotspot 99%", "Hotspot 95%", "Hotspot 90%", 
                                                   "Not Significant", 
                                                   "Coldspot 90%", "Coldspot 95%", "Coldspot 99%"))
    )
  peta <- tm_shape(data_res) +
    tm_polygons(
      fill = "gi_category",
      fill.scale = tm_scale_categorical(
        values = c(
          "Hotspot 99%"     = "#bd0026",
          "Hotspot 95%"     = "#f03b20",
          "Hotspot 90%"     = "#feb24c",
          "Not Significant" = "#e0e0e0",
          "Coldspot 90%"    = "#bdc9e1",
          "Coldspot 95%"    = "#74a9cf",
          "Coldspot 99%"    = "#0570b0"
        ),
        levels = c(
          "Hotspot 99%", "Hotspot 95%", "Hotspot 90%",
          "Not Significant",
          "Coldspot 90%", "Coldspot 95%", "Coldspot 99%"
        )
      ),
      fill.legend = tm_legend(
        title = "Tingkat Kepercayaan",
        position = tm_pos_out("right", "center") 
      ),
      col = NA 
    ) +
    tm_grid(alpha = 0.2) + 
    tm_compass(position = tm_pos_in("right", "top")) + 
    tm_scalebar(position = tm_pos_in("left", "bottom")) + 
    tm_title(
      text = paste("Getis-Ord Gi* - Variabel", variabel),
      size = 1.1,
      fontface = "bold",
      position = tm_pos_out("center", "top") 
    ) +
    tm_layout(component.autoscale = FALSE)
  
  list(data = data_res, plot = peta)
}

# ========================================================================
# 8. RUNNING SEMUA VARIABEL
# ========================================================================
list_summary_master  <- list()
list_ranking_hotspot <- list()
list_plot_lisa       <- list()
list_plot_gi         <- list()

for (v in var_list) {
  df_moran <- globalMoran(v)
  
  res_lisa <- lisaCalc(v)
  list_plot_lisa[[v]] <- res_lisa$plot
  print(res_lisa$plot)
  
  res_gi <- giCalc(v)
  list_plot_gi[[v]] <- res_gi$plot
  print(res_gi$plot)
  
  input[[paste0("lisa_", v)]]      <- res_lisa$data$quadrant_sig
  input[[paste0("gi_zscore_", v)]] <- res_gi$data$gi_zscore
  input[[paste0("gi_cat_", v)]]    <- res_gi$data$gi_category
  
  counts_lisa <- table(factor(res_lisa$data$quadrant_sig, levels = c("High-High", "Low-Low", "High-Low", "Low-High", "Not Significant")))
  counts_gi   <- table(res_gi$data$gi_category)
  
  list_summary_master[[v]] <- df_moran |>
    mutate(
      LISA_HH     = as.numeric(counts_lisa["High-High"]),
      LISA_HL     = as.numeric(counts_lisa["High-Low"]),
      LISA_LH     = as.numeric(counts_lisa["Low-High"]),
      LISA_LL     = as.numeric(counts_lisa["Low-Low"]),
      GI_Hot_99   = as.numeric(counts_gi["Hotspot 99%"]),
      GI_Hot_95   = as.numeric(counts_gi["Hotspot 95%"]),
      GI_Hot_90   = as.numeric(counts_gi["Hotspot 90%"]),
      GI_Cold_90  = as.numeric(counts_gi["Coldspot 90%"]),
      GI_Cold_95  = as.numeric(counts_gi["Coldspot 95%"]),
      GI_Cold_99  = as.numeric(counts_gi["Coldspot 99%"]),
      Tidak_Sig   = as.numeric(counts_gi["Not Significant"])
    )
  
  list_ranking_hotspot[[v]] <- res_gi$data |>
    st_drop_geometry() |>
    select(all_of("id_unit"), nilai_asli = all_of(v), gi_zscore, gi_category) |>
    mutate(Variabel = v) |>
    arrange(desc(gi_zscore))
}

tabel_master_final <- bind_rows(list_summary_master)
tabel_ranking_hotspot <- bind_rows(list_ranking_hotspot)

####EXPORT
folder_tujuan <- "plot_grid" 

if (!dir.exists(folder_tujuan)) {
  dir.create(folder_tujuan)
}

for (v in var_list) {
  tmap_save(list_plot_lisa[[v]], file.path(folder_tujuan, paste0("LISA_", v, ".png")), 
            width = 5.5, height = 5, units = "in", dpi = 100)
  
  tmap_save(list_plot_gi[[v]], file.path(folder_tujuan, paste0("Gi_", v, ".png")), 
            width = 6.5, height = 5, units = "in", dpi = 100)
}

write.csv(tabel_master_final, file.path(folder_tujuan, "tabel_master_final.csv"), row.names = FALSE)
write.csv(tabel_ranking_hotspot, file.path(folder_tujuan, "tabel_ranking_hotspot.csv"), row.names = FALSE)
