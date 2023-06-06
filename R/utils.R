# Ricardo da Silveira Filho
# ricardodasilveira@gmail.com
# 2023.06.06


# hull_by_clade -----------------------------------------------------------

hull_by_clade <- function(data, long, lat) {
	
	require(dplyr)
	require(sf)
	
	clade_hull_list <- list()
	
	for (i in unique(sort(data$clade))) {
		
		
		suppressWarnings({
			clade_sf <- data |> 
				dplyr::filter(clade == i) |> 
				sf::st_as_sf(coords = c(deparse(substitute(long)), 
										deparse(substitute(lat)))) |> 
				sf::st_set_crs(4326) |> 
				st_union() |> 
				st_convex_hull() 
			
			clade_hull_list[[i]] <- clade_sf
			
		})
	}
	
	return(clade_hull_list)
	
}



# plot_hull ---------------------------------------------------------------

plot_hull <- function(dataHull, dataBuff, nCluster) {
	
	require(ggplot2)
	
	ggplot2::ggplot() +
		ggplot2::geom_sf(data = hull_list[[nCluster]], fill = NA) +
		ggplot2::geom_sf(data = hull_buff[[nCluster]], fill = NA) +
		ggplot2::theme_light() +
		ggplot2::coord_sf()
	
}



# filter_gbif -------------------------------------------------------------

filter_gbif <- function(dataList, gbifData) {
	
	require(sf)
	result <- list()
	
	for (i in seq_along(dataList)) {
		
		tmp <- sf::st_filter(gbifData, dataList[[i]])
		
		result[[i]] <- tmp
		
	}
	
	return(result)
	
}



# prep_dfs ----------------------------------------------------------------

prep_dfs <- function(data) {
	
	require(dplyr)
	require(purrr)
	require(sf)
	
	tmp_df <- list() 
	
	for (i in seq_along(data)) {
		
		df_clade <- data[[i]] %>% 
			dplyr::mutate(long = unlist(purrr::map(.$geometry, 1)),
						  lat = unlist(purrr::map(.$geometry, 2))) |> 
			sf::st_drop_geometry()
		
		tmp_df[[i]] <- df_clade
	}
	
	return(tmp_df)
}


# f -----------------------------------------------------------------------

f <- function(data, dist, reps) {
	suitable <- list()
	for (k in 1:reps) {
		outvec <- as.numeric(c())
		dropvec <- c()
		for (i in 1:nrow(data)) {
			if (length(dropvec) < nrow(data)) {
				if (i > 1) {
					rowsleft <- (1:nrow(data))[-c(dropvec)]
				} else {
					rowsleft <- 1:nrow(data)
				}
				outpoint <- as.numeric(sample(as.character(rowsleft), 
											  1))
				outvec[i] <- outpoint
				outcoord <- data[outpoint, c("long", "lat")]
				dropvec <- c(dropvec, which(sqrt((data$long - outcoord$long)^2 + 
												 	(data$lat - outcoord$lat)^2) < dist))
				dropvec <- dropvec[!duplicated(dropvec)]
			}
		}
		suitable[[k]] <- outvec
	}
	best <- unlist(suitable[which.max(lapply(suitable, length))])
	data[best, ]
}


# buffer_boy --------------------------------------------------------------

buffer_boy <- function(data, dist, reps) {
	
	result <- list()
	
	for (i in seq_along(tmp_list)) {
		
		df_final <- f(tmp_list[[i]], dist = dist, reps = reps)
		
		result[[i]] <- df_final
		
	}
	
	return(result)
	
}
