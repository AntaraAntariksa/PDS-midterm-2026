library(sf)
library(tmap)

batas_kab <- st_read("GADM41_IDN_2.shp")

prov_kalimantan <- c(
  "Kalimantan Barat", "Kalimantan Tengah", "Kalimantan Selatan", 
  "Kalimantan Timur", "Kalimantan Utara"
)
kalimantan <- subset(batas_kab, NAME_1 %in% prov_kalimantan)
kalimantan <- st_transform(kalimantan, crs = 4326)

pky <- subset(kalimantan, grepl("Palangka Raya", NAME_2, ignore.case = TRUE))
kab_lain <- subset(kalimantan, !grepl("Palangka Raya", NAME_2, ignore.case = TRUE))

tmap_mode("view")

tm_basemap("Esri.WorldImagery") +
  tm_shape(kab_lain) +
  tm_polygons(
    fill = "white", 
    fill_alpha = 0.5, 
    col = "black", 
    lwd = 1.5
  ) +
  tm_shape(pky) +
  tm_borders(
    col = "red", 
    lwd = 3
  ) +
  tm_shape(kalimantan) +
  tm_text(
    "NAME_2", 
    col = "white", 
    size = 0.8,
    bg.color = "black",
    bg.alpha = 0.5 
  ) +
  tm_title(
    "PETA LOKASI STUDI AREA",
    position = c("center", "top")
  )