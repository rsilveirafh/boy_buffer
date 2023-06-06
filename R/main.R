# Código para gerar buffer em cada cluster
# Ricardo da Silveira Filho
# Felipe Magalhães
# 2023.06.06


# 0) Setup ----------------------------------------------------------------


library(tidyverse)          # data wrangling
library(rnaturalearth)      # world data
library(sf)                 # dealing with shapefiles
source("R/utils.R")         # functions: hull_by_clade(), plot_hull()

# crs = 4326

# 1) Data -----------------------------------------------------------------

gen_data <- read_csv("data/Sceloporus_geographic_data_genomic.csv") |> 
	janitor::clean_names() |> 
	rename(long = longitude,
		   lat = latitude)

gbif <- read_csv("data/Sceloporus_GBIF.csv") |> 
	janitor::clean_names() |> 
	dplyr::select(concat_id,
		   long = longitude,
		   lat = latitude)


### Using pipe |> or %>% ----
select(filter(filter(gen_data, clade >= 5), latitude < 37), samples)
	
gen_data |> 
	filter(clade >=5) |> 
	filter(latitude < 37) %>%
	select(samples)
###



# 1.1) Convex of Hull by Clade --------------------------------------------

# Returns a list of sf objects
# Contain the polygons of each Clade
hull_list <- hull_by_clade(data = gen_data, long = long, lat = lat)



# 1.2) Buffering Hulls ----------------------------------------------------

# dist is in km
hull_buff_list <- hull_list |> 
	map(st_buffer, dist = 5000)

# i.e.: Function to plot the differences between Hull and Buffered Hull
# nCluster: 1 to 7
plot_hull(dataHull = hull_list, 
		  dataBuff = hull_buff_list, 
		  nCluster = 4)


# 1.3) GBIF data into sf object -------------------------------------------

# Transforming GBIF dataframe into a sf object
gbif_sf <- sf::st_as_sf(gbif, coords = c("long", "lat")) |> 
	st_set_crs(4326)


# 1.4) Filtering points inside Polygons -----------------------------------


# Filtering GBIF data of each clade using its Buffered Hull Convex
filtered_gbif_clusters <- filter_gbif(dataList = hull_buff_list,
									  gbifData = gbif_sf)



# 1.5) Buffering points inside Convexes -----------------------------------


# Preparing objects
# Data frames with ID, long and lat

dfs_pre_buffer <- prep_dfs(filtered_gbif_clusters)



# Buffer
# dist = radius of distance between points
# reps = number of iteractions

final_list <- buffer_boy(data = dfs_pre_buffer, 
						 dist = 0.1, 
						 reps = 1)






# 2) Map ------------------------------------------------------------------

world <- ne_countries(scale = "medium", returnclass = "sf")

# Function to plot separately
unique_plot <- function(nCluster) {
	ggplot2::ggplot() +
		ggplot2::geom_sf(data = world) +
		ggplot2::geom_point(data = final_list[[nCluster]], ggplot2::aes(x = long, y = lat)) +
		ggplot2::geom_point(data = dplyr::filter(gen_data, clade == nCluster), 
							ggplot2::aes(x = long, y = lat), color = "pink") + 
		theme_bw() +
		coord_sf(xlim = c(-130, -60), ylim = c(25, 50), expand = FALSE)	
}

unique_plot(nCluster = 7)


# Ploting everything together
all_data <- bind_rows(final_list, .id = "cluster")

ggplot2::ggplot() +
	ggplot2::geom_sf(data = world, fill = "#F4F4F4", lwd = .6) +
	ggplot2::geom_point(data = all_data, ggplot2::aes(x = long, y = lat, fill = cluster),
						size = 2, color = "white", shape = 21) +
	scale_fill_viridis_d() +
	ggspatial::annotation_scale(location = "br",
								text_cex = 1,
								bar_cols = c("black", "white")) +
	ggspatial::annotation_north_arrow(location = "bl", which_north = "true",
									  height = unit(0.8, "in"),
									  width = unit(0.8, "in"),
									  pad_x = unit(0.2, "in"),
									  pad_y = unit(0.2, "in"),
									  style = ggspatial::north_arrow_nautical(
									  	fill = c("black", "white"),
									  	line_col = "grey20")) +
	ggplot2::coord_sf(xlim = c(-130, -60), ylim = c(25, 50), expand = FALSE) +
	ggplot2::labs(x = "Longitude",
				  y = "Latitude") +
	ggplot2::theme_bw() +
	ggplot2::theme(panel.background = element_rect(fill = "#AAD3DF"),
				   panel.grid.major = element_blank(),
				   panel.grid.minor = element_blank(),
				   axis.text = element_text(size = 12),
				   axis.title = element_text(size = 14),
				   legend.title = element_text(size = 12),
				   legend.text = element_text(size = 8))



# Saving data
write_csv(all_data, "output/filtered_gbif.csv")
