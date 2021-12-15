

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
spcs <- dir_ls(root, type = 'directory')


# Function -----------------------------------------------------------------
get_pssn <- function(spc){
  
  spc <- spcs[1] # Run and erase
  
  cat('Start ', spc, '\n')
  fls <- dir_ls(spc, regexp = '.tif$')
  
  # Testing CanESM2
  fle <- grep('CanESM2', fls, value = TRUE)
  fle <- grep('mean', fle, value = TRUE)
  fle <- grep('2011', fle, value = TRUE)
  fle <- as.character(fle)
  rst <- raster(fle)
  lmd <- cellStats(x = rst, stat = 'mean', na.rm = TRUE)
  dps <- calc(x = rst, fun = function(x){dpois(x, lambda = lmd)})
  
  
  
  dpois(x = 2, lambda = 1)
  
  
}

