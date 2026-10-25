workdir <- ("C:/an/ETS/")
setwd(workdir)
getwd() #cek

library(terra)
library(dplyr)
library(ggplot2)

# Load Shapefile
shp_pky <- vect("KotaPalangkaraya/Palangka_Raya.shp")

# 1. Fungsi Sampling Gabungan 3 Bulan (Memory-Safe)
get_joint_sample <- function(index_type, path_folder = "Res_RGEE", n_samples = 100000) {
  files <- sprintf("%s/S2_%s_Palangkaraya_%s_2026.tif", path_folder, index_type, c("Jul", "Aug", "Sep"))
  
  # Load & stack 3 bulan sekaligus
  r_stack <- rast(files) %>% crop(shp_pky, mask = TRUE)
  
  # Ambil 100.000 piksel sampel acak dari gabungan 3 bulan
  sample_mat <- spatSample(r_stack, size = n_samples, method = "random", na.rm = TRUE)
  
  # Ratakan matriks jadi 1 vektor tunggal
  data.frame(
    Val = as.vector(as.matrix(sample_mat)),
    Indeks = index_type
  )
}

# 2. Ambil Sampel Gabungan NDVI dan NBR
df_sample_ndvi <- get_joint_sample("NDVI")
df_sample_nbr  <- get_joint_sample("NBR")

# 3. Cek Statistik Kuantil / Persentil Gabungan
cat("--- PERSENTIL GABUNGAN NDVI (3 BULAN) ---\n")
print(quantile(df_sample_ndvi$Val, probs = c(0.05, 0.20, 0.50, 0.75, 0.90, 0.95), na.rm = TRUE))

cat("\n--- PERSENTIL GABUNGAN NBR (3 BULAN) ---\n")
print(quantile(df_sample_nbr$Val, probs = c(0.05, 0.20, 0.50, 0.75, 0.90, 0.95), na.rm = TRUE))

# 4. Plot Kurva Densitas + Garis Ambang Batas Usulan
# A. Visualisasi Evaluasi Threshold NDVI
p_check_ndvi <- ggplot(df_sample_ndvi, aes(x = Val)) +
  geom_density(fill = "#a6d96a", alpha = 0.5, color = "#1a9641", linewidth = 1) +
  # Masukkan usulan threshold tropis (misal: 0.20, 0.50, 0.72)
  geom_vline(xintercept = c(0.20, 0.50, 0.72), linetype = "dashed", color = "#d7191c", linewidth = 0.8) +
  theme_minimal() +
  labs(
    title = "Distribusi Nilai NDVI Gabungan (Juli - September)",
    subtitle = "Garis merah = Evaluasi threshold kelas (0.20, 0.50, 0.72)",
    x = "Nilai NDVI",
    y = "Densitas Piksel"
  )

# B. Visualisasi Evaluasi Threshold NBR
p_check_nbr <- ggplot(df_sample_nbr, aes(x = Val)) +
  geom_density(fill = "#fee08b", alpha = 0.5, color = "#f46d43", linewidth = 1) +
  # Masukkan usulan threshold gambut tropis (misal: -0.15, 0.05, 0.45)
  geom_vline(xintercept = c(-0.15, 0.05, 0.45), linetype = "dashed", color = "#67000d", linewidth = 0.8) +
  theme_minimal() +
  labs(
    title = "Distribusi Nilai NBR Gabungan (Juli - September)",
    subtitle = "Garis maroon = Evaluasi threshold kelas (-0.15, 0.05, 0.45)",
    x = "Nilai NBR",
    y = "Densitas Piksel"
  )

# Tampilkan Plot
print(p_check_ndvi)
print(p_check_nbr)
