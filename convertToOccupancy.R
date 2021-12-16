# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, rcartocolor, rgdal, rgeos, future, furrr, reproducible, RColorBrewer, 
               colorspace, ggspatial, ggpubr, gridExtra, hrbrthemes, terra, stringr, glue, 
               sf, tidyverse, RStoolbox, fs, future.apply, fst, trend, crayon)


g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)

# Load data ---------------------------------------------------------------
root <- './outputs'
dirs <- fs::dir_ls(root, type = 'directory')
spcs <- basename(dirs)

# Function -----------------------------------------------------------------
get_probOcc <- function(spc){
  
  #spc <- spcs[1] # Run and erase
  dir <- grep(spc, dirs, value = TRUE)
  fls <- fs::dir_ls(dir, regexp = '.tif$')
  fls <- fls <- grep('mean', fls, value = TRUE)
  yrs <- parse_number(basename(fls))
  yrs <- unique(yrs)
  gcm <- str_sub(basename(fls), start = 16, end = nchar(basename(fls)) - 4)
  gcm <- unique(gcm)
  
  # occuRas<- map2(.x = 1:length(gcm), .y = 1:length(yrs), .f = function(.k, .yr){
  # cat('Starting with"', gcm[k], yrs[yr], '\n')
  # fl <- grep(gcm[k], fls, value = TRUE)
  # fl <- grep(yrs[yr], fl, value = TRUE)
  # fl <- as.character(fl)
  # rst <- raster::raster(fl)
  # dps <- calc(x = rst, fun = function(pxl){1- dpois(x = 0, lambda = pxl)})
  # name <-  glue('./outputs/{spc}/occur/occu_{spc}_{gcm}_{yr}.tif')
  # dr <- dirname(name)
  # ifelse(!dir.exists(name), dir.create(dr), print('Folder already exist'))
  # cat('Writing prob of occurrence raster\n')
  # writeRaster(x = dps, filename = name, overwrite = TRUE) #guarda el raster 
  # cat('Done ', k, '\n')
  # })
  occurRas<- map(.x = 1:length(gcm), .f = function(k){
    message(crayon::green('Loading files for', gcm[k]))
    fl <- grep(gcm[k], fls, value = TRUE)
    fl <- as.character(fl)
    # stk <- raster::stack(fl)
    # dps <- calc(x = stk, fun = function(pxl){1- dpois(x = 0, lambda = pxl)})
    # ou <- glue('./outputs/{spc}/occur/occu_{spc}_{gcm}.tif')
    # dr <- dirname(name)
    # writeRaster(x = dps, filename = ou[k], overwrite = TRUE )
    # 
    proOccRas<-  map(.x = 1:length(yrs), .f = function(yr){
      message(crayon::green('Year', yrs[yr]))
      sfl <- grep(yrs[yr], fl, value = TRUE)
      rst <- raster::raster(sfl)
      dps <- calc(x = rst, fun = function(pxl){1- dpois(x = 0, lambda = pxl)})
      out <- glue('./outputs/{spc}/ocurr')
      ifelse(!dir.exists(out), dir.create(out, recursive = TRUE), print('Folder already exist'))
      writeRaster(x = dps, filename = glue('./outputs/{spc}/ocurr/occu_{gcm[k]}_{yrs[yr]}.tif'), overwrite = TRUE)
      cat('Done!\n')
    return(dps)
    })
    
  #   ou <- glue('./outputs/{spc}/occur/occu_{spc}_{gcm}_{yr}.tif')
  #   dr <- dirname(ou)
  #   #ifelse(!dir.exists(ou), dir.create(dr), print('Folder already exist'))
  #   raster::writeRaster(x = dps, filename = ou[k],
  #                       format = 'GTiff', overwrite = TRUE)
  })
  # 
  ou <- glue('./outputs/{spc}/occur/occu_{spc}_{gcm}_{yr}.tif')
  dr <- dirname(ou)
  # ifelse(!dir.exists(ou), dir.create(dr, recursive = TRUE), print('Folder already exist'))
  message(crayon::green(paste0('Writing prob of occurrence raster for:', spc, '\n',
                               gcm,'\n', yr ,'\n')))
  # writeRaster(x = dps, filename = name, overwrite = TRUE) #guarda el raster 
  # cat('Done ', k, '\n')
  
  saveRas<- lapply(proOccRas, FUN =function(x){
    raster::writeRaster(x, filename = ou[k], overwrite = TRUE)
  })
  }

# Apply the function ------------------------------------------------------
map(.x = spcs[1:3], .f = get_probOcc)
  