# bread-and-butter
library(tidyverse) # the tidyverse
library(viridis) # viridis colors
library(harrypotter) # hp colors
library(RColorBrewer) # brewer colors
library(scales) # work with number and plot scales
library(latex2exp)

# visualization
library(mapview) # interactive html maps
library(kableExtra) # tables
library(patchwork) # combine plots
library(ggnewscale) # ggnewscale

# spatial analysis
library(terra) # raster
library(sf) # simple features

# clean session
remove(list = ls())
gc()

# where is this data?
aoi_f <- "c:/Users/georg/Downloads/MogollonRimMCSite/MogollonRimMCSite.shp"
fire_sim_f <- "c:/Users/georg/Downloads/QUIK-Fire_Boundary/QUIK-Fire_Boundary.shp"

# load
aoi <- sf::st_read(aoi_f, quiet = T) %>% dplyr::rename_with(tolower)
fire_sim <- sf::st_read(fire_sim_f, quiet = T) %>% dplyr::rename_with(tolower)

# what?
aoi %>% dplyr::glimpse()
fire_sim %>% dplyr::glimpse()

# project
sf::st_crs(aoi)
identical(sf::st_crs(aoi), sf::st_crs(fire_sim))
fire_sim <- fire_sim %>% sf::st_transform(sf::st_crs(aoi))

# area
aoi %>% sf::st_area() %>% as.numeric() %>% `/`(10000) %>% round() %>% paste("ha")
fire_sim %>% sf::st_area() %>% as.numeric() %>% `/`(10000) %>% round() %>% paste("ha")

# plot
# option to put satellite imagery as base layer of mapview maps
mapview::mapviewOptions(
  homebutton = FALSE
  , basemaps = c("Esri.WorldImagery", "OpenStreetMap")
)
mapview::mapview(
  aoi
  , color = "black"
  , lwd = 1.5
  , alpha.regions = 0
  , label = FALSE
  , legend = FALSE
  , popup = FALSE
  , layer.name = "AOI"
) +
mapview::mapview(
  fire_sim
  , zcol = "unit.name"
  , col.regions = RColorBrewer::brewer.pal(n=fire_sim$unit.name %>% length(),name="Set2")
  , alpha.regions = 0.6
  , lwd = 0.2
  , label = T
  , legend = FALSE
  , layer.name = "Fire Units"
  , hide = FALSE
)

##########.....................................
# looked up aoi at usgs lidar explorer:
# under "lidar within AOI"...click on the list box icon to download a downloadlist.txt file list of point cloud tiles
# lidar project: AZ_USFS_3DEP_Processing_2019_D20
# lidar project id: 195122
# collect st: 2013/08/23
# collect end: 2014/10/11
# meta data:
#   https://prd-tnm.s3.amazonaws.com/index.html?prefix=StagedProducts/Elevation/metadata/AZ_USFS_3DEP_Processing_2019_D20/AZ_USFS_3DEP_Processing_2019
# LPC: 
#   https://rockyweb.usgs.gov/vdelivery/Datasets/Staged/Elevation/LPC/Projects/AZ_USFS_3DEP_Processing_2019_D20/AZ_USFS_3DEP_Processing_2019

# download point cloud tiles
########################
# download files
########################
dl_f <- "c:/data/usfs/lidar_phys_fire_mods/data/downloadlist.txt"
dl <- readr::read_lines(dl_f) %>% dplyr::as_tibble() %>% dplyr::rename(url=1)
dl %>% dplyr::glimpse()
# download directory
dl_dir <- "E:/lidar_phys_fire_mods/data/mogollon_rim_az_lidar"
# function to download
dl_fn <- function(x, dest = tempdir()){
  # increase the download timeout
  to_bu <- getOption("timeout")
  options(timeout = max(1111, to_bu))
  # create delivery dir if needed
  dest <- file.path(dest)
  if(!file.exists(dest)){
    dir.create(dest, showWarnings = FALSE)
  }
  # dl file name
  d <- file.path(dest, basename(x))
  # download it...if needed
  if(!file.exists(d)){
    # print
    message("downloading file: ", x, "at ", Sys.time())
    # download it
    download.file(
      url = x, destfile = d
      , quiet = F
      , mode = "wb"
    )
  }
  # reset timeout
  options(timeout = to_bu)
  return(d)
}

# map over the las files to read
dl_fn_ans <- dl$url %>% 
  unique() %>% 
  stringr::str_subset("USGS_LPC_AZ_USFS_3DEP_Processing_2019") %>% 
  stringr::str_subset("n3815.laz",negate = T) %>% # these are too far N
  purrr::map(\(x) dl_fn(x, dest = dl_dir)) %>% 
  unlist()
# dl_fn_ans
list.files(dl_dir, full.names = T) %>% 
  .[11] %>% 
  lidR::readLAS(filter = "-keep_random_fraction 0.2") %>% 
  lidR::plot(color = "Z", breaks = "quantile", bg = "white", legend = T)

las_ctg <- list.files(dl_dir, full.names = T) %>% 
  lidR::readLAScatalog()
lidR::plot(las_ctg)
# mapview
mapview::mapview(
  las_ctg@data %>% 
    dplyr::select(Number.of.point.records, filename) %>% 
    dplyr::mutate(filename = basename(filename))
  , zcol = "Number.of.point.records"
  , layer.name = "tile #pts"
) +
mapview::mapview(
  aoi
  , color = "black"
  , lwd = 1.5
  , alpha.regions = 0
  , label = FALSE
  , legend = FALSE
  , popup = FALSE
  , layer.name = "AOI"
)
# looks good
# projection??
lidR::crs(las_ctg)

get_horizontal_crs <- function(x) {
  xcrs <- sf::st_crs(x)
  if (is.na(xcrs)) stop("No CRS defined...try setting the parameter `new_crs` if known")

  wkt <- sf::st_as_text(xcrs)

  if (!grepl("COMPD_CS", wkt)) {
    # Should just be a horizontal CRS - simply return it
    xcrs
  } else {
    # Extract the horizontal component
    i <- regexpr("PROJCS\\[", wkt)
    wkt <- base::substring(wkt, i)

    # Match square brackets to discard any trailing
    # component (e.g. the vertical CRS)
    wkt_chars <- base::strsplit(wkt, "")[[1]]
    level <- 1
    k <- base::match("[", wkt_chars)
    while (level > 0) {
      k <- k + 1
      if (wkt_chars[k] == '[') {
        level <- level + 1
      } else if (wkt_chars[k] == ']') {
        level <- level - 1
      }
    }

    wkt <- base::substring(wkt, 1, k)
    # return
    return(sf::st_crs(wkt))
  }
}

# pull crs for using in write operations
# crs_list_temp = las_ctg@data$CRS
(crs_list_temp <- sf::st_crs(las_ctg)$epsg)

# handle missing epsg with user defined parameter
if(is.na(crs_list_temp)){
  # try to pull the epsg another way if still NA
  n_crs <- get_horizontal_crs(las_ctg)
  if(!is.na(n_crs)){
    las_ctg <- las_ctg %>% sf::st_set_crs(n_crs)
    crs_list_temp <- sf::st_crs(las_ctg)$epsg
  }else{
    stop("No CRS defined...try setting the parameter `new_crs` if known")
  }
}
# set crs for use in project
if(length(unique(crs_list_temp))>1){
  stop("The raw las files have multiple CRS settings. Confine las files in `folder` to files with same CRS or re-generate las files with same projection.")
}else{
  proj_crs <- paste0("EPSG:",unique(crs_list_temp))
}
crs_list_temp
proj_crs
# clean session
remove(list = ls())
gc()

cloud2trees_ans <- cloud2trees::cloud2trees(
  output_dir = "E:/lidar_phys_fire_mods/data/"
  , input_las_dir = "E:/lidar_phys_fire_mods/data/mogollon_rim_az_lidar"
  # , max_ctg_pts = 1.1e+07
  # , keep_intrmdt = T
  # , estimate_tree_competition = T
  , min_height = 1.37
  , estimate_tree_hmd = T, hmd_estimate_missing_hmd = T
  , estimate_tree_dbh = T
  , estimate_tree_type = T
  , estimate_tree_cbh = T, cbh_estimate_missing_cbh = T, cbh_tree_sample_prop = 0.08
)
# what is it?
cloud2trees_ans %>% names()
# there's a DTM
cloud2trees_ans$dtm_rast %>% terra::plot()
# there's a CHM
cloud2trees_ans$chm_rast %>% terra::plot()
# there are tree crowns
cloud2trees_ans$crowns_sf %>% dplyr::glimpse()
cloud2trees_ans$crowns_sf %>% nrow()
cloud2trees_ans$crowns_sf %>% ggplot2::ggplot() + ggplot2::geom_sf(mapping = ggplot2::aes(fill = tree_height_m))
# there are tree top points
cloud2trees_ans$treetops_sf %>% dplyr::glimpse()
cloud2trees_ans$treetops_sf %>% ggplot2::ggplot() + ggplot2::geom_sf(mapping = ggplot2::aes(color = tree_height_m))
# there are tree competition distances if we did it
cloud2trees_ans$treetops_sf %>% ggplot2::ggplot() + ggplot2::geom_sf(mapping = ggplot2::aes(color = comp_dist_to_nearest_m))
# there are tree types if we did it
cloud2trees_ans$treetops_sf %>% ggplot2::ggplot() + ggplot2::geom_sf(mapping = ggplot2::aes(color = forest_type_group))
# there is a forest types raster if we did it
cloud2trees_ans$foresttype_rast %>% terra::plot()
# there are dbh values if we did it
cloud2trees_ans$crowns_sf %>% ggplot2::ggplot() + ggplot2::geom_sf(mapping = ggplot2::aes(fill = dbh_cm))
cloud2trees_ans$crowns_sf %>%
  dplyr::arrange(is_training_data) %>%
  ggplot2::ggplot(
    mapping = ggplot2::aes(x = tree_height_m, y = dbh_cm, color=is_training_data)
  ) +
  ggplot2::geom_point()

# there are cbh values if we did it
cloud2trees_ans$treetops_sf %>%
  ggplot2::ggplot(
    mapping = ggplot2::aes(fill=tree_cbh_m, color=is_training_cbh)
  ) +
  ggplot2::geom_sf(shape = 21)
cloud2trees_ans$crowns_sf %>%
  dplyr::arrange(is_training_cbh) %>%
  ggplot2::ggplot(
    mapping = ggplot2::aes(x = tree_height_m, y = tree_cbh_m, color=is_training_cbh)
  ) +
  ggplot2::geom_point()

cloud2trees_ans$crowns_sf %>%
  sf::st_drop_geometry() %>%
  dplyr::count(is_training_cbh)

# there are hmd values if we did it
cloud2trees_ans$crowns_sf %>%
  ggplot2::ggplot(
    mapping = ggplot2::aes(fill=max_crown_diam_height_m, color=is_training_hmd)
  ) +
  ggplot2::geom_sf(shape = 21)
cloud2trees_ans$crowns_sf %>%
  dplyr::arrange(is_training_hmd) %>%
  ggplot2::ggplot(
    mapping = ggplot2::aes(x = tree_height_m, y = max_crown_diam_height_m, color=is_training_hmd)
  ) +
  ggplot2::geom_point()

cloud2trees_ans$crowns_sf %>%
  sf::st_drop_geometry() %>%
  dplyr::count(is_training_hmd)
