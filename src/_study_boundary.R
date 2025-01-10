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
  # .[1:2] %>%
  purrr::map(\(x) dl_fn(x, dest = dl_dir)) %>% 
  unlist()
# dl_fn_ans
list.files(dl_dir, full.names = T) %>% 
  .[1] %>% 
  lidR::readLAS(filter = "-keep_random_fraction 0.2") %>% 
  lidR::plot(color = "Z", breaks = "quantile", bg = "white", legend = T)
