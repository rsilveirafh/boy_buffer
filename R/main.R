# Código para gerar buffer em cada cluster
# Ricardo da Silveira Filho
# Felipe Magalhães
# 2023.06.06


# 0) Setup ----------------------------------------------------------------

library(alphahull)         # hull convex
source("R/buffer.R")
library(janitor)           # clean names
library(tidyverse)         # data wrangling
library(rnaturalearth)     # world data
library(rnaturalearthdata) # world data
library(sf)                # dealing with shapefiles



# 1) Data -----------------------------------------------------------------

gen_data <- read_csv("data/Sceloporus_geographic_data_genomic.csv") |> 
	clean_names() |> 
	rename(lon = longitude,
		   lat = latitude)

gbif <- read_csv("data/Sceloporus_GBIF.csv") |> 
	clean_names() |> 
	dplyr::select(concat_id,
		   lon = longitude,
		   lat = latitude)

## Using pipe |> or %>%
select(filter(filter(gen_data, clade >= 5), latitude < 37), samples)
	
gen_data |> 
	filter(clade >=5) |> 
	filter(latitude < 37) %>%
	select(samples)
##


# selecionar clado
for (i in unique(gen_data$clade)) {
	print(i)
}

clade4 <- gen_data |> 
	filter(clade == 4)

clade4_hull <- ahull(x = clade4$lon, y = clade4$lat, alpha = 3)
plot(clade4_hull)
source("R/ah2sp.R")
c4_shp <- ah2sp(clade4_hull)
plot(c4_shp)



library(raster)

gbif_spp <- gbif |> 
	dplyr::select(lon, lat)

coordinates(gbif_spp) <- ~ lon + lat
terra::crs(gbif_spp) <- terra::crs(world)
terra::crs(c4_shp) <- terra::crs(world)

ovr <- over(gbif_spp, c4_shp)

result <- filter(ovr, !is.na(HID)) |> 
	tibble::rownames_to_column(var = "rownames")


gbif4 <- gbif |> 
	tibble::rownames_to_column(var = "rownames") |> 
	right_join(result, by = "rownames")
	




plot(tmp)

# 1.1) Buffer -------------------------------------------------------------

new_gbif4 <- gbif4 |> 
	dplyr::select(x = lon,
		   y = lat)

# variable order = long, lat
# buffer.f(data, distance, iteraction)
buffer_gbif4 <- buffer.f(new_gbif4, 0.5, 1000)


plot(gbif4$lon, gbif4$lat)

plot(buffer_gbif4$x, buffer_gbif4$y, col = "red", add = T)
dev.off()


# 2) Map ------------------------------------------------------------------

world <- ne_countries(scale = "medium", returnclass = "sf")

ggplot2::ggplot() +
	ggplot2::geom_sf(data = world) +
	ggplot2::geom_point(data = buffer_gbif4, ggplot2::aes(x = x, y = y)) +
	ggplot2::geom_point(data = clade4, ggplot2::aes(x = lon, y = lat), color = "pink") + 
	theme_bw() +
	coord_sf(xlim = c(-130, -60), ylim = c(30, 50), expand = FALSE)
















