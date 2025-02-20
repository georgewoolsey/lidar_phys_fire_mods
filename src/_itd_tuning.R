library(tidyverse)
f <- "e:/lidar_phys_fire_mods/data/mogollon_rim_az_lidar/"
list.files(f)
ctg <- lidR::readLAScatalog(f)
ctg@data %>% mapview::mapview()

itd_tuning_ans <- cloud2trees::itd_tuning(f)

itd_tuning_ans %>% names()

itd_tuning_ans$plot_samples
ggsave(filename = "e:/lidar_phys_fire_mods/data/itd_tuning1.jpg", height = 10, width = 8, dpi = "print")

ggplot() +
  geom_function(aes(color = "lin_fn"),fun=itd_tuning_ans$ws_fn_list$lin_fn, lwd=1.2) +
  geom_function(aes(color = "nonlin_fn"),fun=itd_tuning_ans$ws_fn_list$nonlin_fn, lwd=1.2) +
  geom_function(aes(color = "exp_fn"),fun=itd_tuning_ans$ws_fn_list$exp_fn, lwd=1.2) +
  ggplot2::xlim(-5,60)

itd_tuning_ans$ws_fn_list$lin_fn

custom_lin <- function (x){
  y <- dplyr::case_when(
    is.na(x) ~ 0.001
    , x < 0 ~ 0.001
    , x < 2 ~ 1.2
    , x > 30 ~ 5
    , TRUE ~ 0.9 + (x * 0.139)
  )
  return(y)
}

ggplot() +
  geom_function(aes(color = "custom_lin"),fun=custom_lin, lwd = 1.2) +
  ggplot2::xlim(-5,60)

itd_tuning_ans2 <- cloud2trees::itd_tuning(
  f
  , ws_fn_list = list(
    my_custom_lin = custom_lin
    , lin_fn = itd_tuning_ans$ws_fn_list$lin_fn
    , nonlin_fn = itd_tuning_ans$ws_fn_list$nonlin_fn
  )
  , n_samples = 4
)

itd_tuning_ans2$plot_samples

ggsave(filename = "e:/lidar_phys_fire_mods/data/itd_tuning2.jpg", height = 10, width = 8, dpi = "print")

ggplot() +
  geom_function(aes(color = "lin_fn"),fun=itd_tuning_ans2$ws_fn_list$lin_fn, lwd=1.2) +
  geom_function(aes(color = "nonlin_fn"),fun=itd_tuning_ans2$ws_fn_list$nonlin_fn, lwd=1.2) +
  geom_function(aes(color = "my_custom_lin"),fun=itd_tuning_ans2$ws_fn_list$my_custom_lin, lwd=1.2) +
  ggplot2::xlim(-5,60)

############################################
ans <- cloud2trees::cloud2trees(
  output_dir = "c:/data/usfs/lidar_phys_fire_mods/data/"
  , input_las_dir = f
  , accuracy_level = 2
  , dtm_res_m = 1
  , chm_res_m = 0.25
  , min_height = 2
  , ws = custom_lin
  , keep_intrmdt = T
  , estimate_tree_dbh = T
  , estimate_tree_type = T
  , estimate_biomass_method = c("cruz", "landfire")
  , estimate_tree_hmd = T, hmd_estimate_missing_hmd = T
  , estimate_tree_cbh = T, cbh_estimate_missing_cbh = T
  , cbh_tree_sample_n = 25000
)

