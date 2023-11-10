library(tidyverse)
library(glue)
library(lidR)
library(caret)
library(randomForest)
library(terra)
library(doParallel)
library(foreach)
library(sf)

# ==============================================================================
# ================================= User inputs ================================
# ==============================================================================

uas_las_folder <- 'data/las/uas'
zones <- 6:14
rf_file <-
  'data/ml_output/rf_spatial_cluster_model_20230419_2138.RData'

shp_gdb <- 'data/boundaries/ssu_3dforests.gdb'
shp_layer <- 'c1_uas_zones'

output_folder <- 'data/prediction_maps'

# ==============================================================================
# ================================== Data prep =================================
# ==============================================================================

# RF Models

rf <- readRDS(rf_file)

rf_biomass <- rf %>%
  keep_at(at = ~ str_detect(.x, 'biomass')) %>%
  .[[1]]

rf_cbh <- rf %>%
  keep_at(at = ~ str_detect(.x, 'cbh')) %>%
  .[[1]]

rf_cbd <- rf %>%
  keep_at(at = ~ str_detect(.x, 'cbd')) %>%
  .[[1]]

# UAS las files

uas_files <- list.files(uas_las_folder,
                        pattern = 'uas_hnrm.las$') %>%
  str_subset('ppwd_c1') %>%
  tibble(file = .) %>%
  rowwise() %>%
  mutate(id = sum(str_detect(file, glue('z{zones}_')))) %>%
  filter(id == 1) %>%
  pull(file)

shp <- read_sf(shp_gdb, shp_layer)

# ==============================================================================
# ============================= Generate predictions ===========================
# ==============================================================================

cl <- makeCluster(8)
registerDoParallel(cl)

foreach (
  uas_i = uas_files,
  .packages = c('lidR', 'tidyverse', 'terra', 'caret', 'randomForest', 'glue'),
  .export = c('shp', 'output_folder', 'rf_biomass', 'rf_cbd', 'rf_cbh', 'uas_las_folder')
) %dopar% {
  
  source('R/uas_metric_function.R')
  
  file_skeleton <- glue('{output_folder}/{uas_i}') %>%
    str_replace('hnrm.las', '{type}_prediction.tif')
  
  las <- glue('{uas_las_folder}/{uas_i}') %>%
    readLAS() 
  
  uas_metrics <- las %>%
    pixel_metrics(
      ~ uas_cld_metrics(
        z = Z,
        r = red,
        g = green,
        b = blue,
        re = re,
        nir = nir,
        ndvi = ndvi,
        ndre = ndre,
        gndvi = gndvi
      )
    )
  
  uas_metrics[is.na(uas_metrics)] <-  -9999
  
  biomass_i <- terra::predict(uas_metrics, rf_biomass)
  
  shp_i <- shp %>%
    filter(Zone == str_extract(uas_i, pattern = '(?<=_z)[:digit:]+')) %>%
    vect() %>%
    project(biomass_i)
  
  biomass_i %>%
    mask(shp_i) %>%
    terra::writeRaster(glue(file_skeleton, type = 'biomass'),
                overwrite = TRUE)
  
  terra::predict(uas_metrics, rf_cbh) %>%
    mask(shp_i) %>%
    terra::writeRaster(glue(file_skeleton, type = 'cbh'),
                overwrite = TRUE)
  
  terra::predict(uas_metrics, rf_cbd) %>%
    mask(shp_i) %>%
    terra::writeRaster(glue(file_skeleton, type = 'cbd'),
                overwrite = TRUE)
  
  uas_i
  
}
