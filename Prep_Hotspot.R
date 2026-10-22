library(sf)
library(dplyr)

#atur direktori
workdir <- ("C:/an/ETS/")
setwd(workdir)
getwd() #cek

# 1. BACA DATA
snpp <- st_read("fire_nrt_SV-C2_825463.shp")
n20 <- st_read("fire_nrt_J1V-C2_825461.shp")
n21 <- st_read("fire_nrt_J2V-C2_825462.shp")

# merge data jadi satu (kolom persis)
viirs_gabungan <- bind_rows(snpp, n20, n21)

# 2. TRANSFORMASI KOORDINAT
viirs_utm <- st_transform(viirs_gabungan, crs = 32749)

# 3. BUAT GRID 375 METER (sesuai resolusi VIIRS)
grid_375m <- st_make_grid(viirs_utm, cellsize = 375, square = TRUE) %>%
  st_sf(grid_id = 1:length(.)) # jadi objek sf dan beri kolom ID berurutan

# 4. SPATIAL JOIN
titik_grid <- st_join(viirs_utm, grid_375m, join = st_intersects)

# 5. AGGREGATE
titik_bersih <- titik_grid %>%
  group_by(grid_id, ACQ_DATE) %>%
  summarise(
    BRIGHTNESS = max(BRIGHTNESS, na.rm = TRUE), 
    CONFIDENCE = first(CONFIDENCE),             
    SATELLITE = paste(unique(SATELLITE), collapse = ","), 
    .groups = "drop"
  ) %>%
  st_centroid()

# 6. HITUNG TOTAL & SIMPAN
total_titik <- nrow(titik_bersih)
print(paste("Total titik api bersih (tanpa duplikat):", total_titik))

st_write(titik_bersih, "hotspot_kalteng_bersih.shp", append = FALSE)