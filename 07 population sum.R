

# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, future, furrr, reproducible, RColorBrewer, 
               colorspaces, ggspatial, ggpubr, gridExtra, terra, stringr, glue, 
               sf, tidyverse, RStoolbox, fs, future.apply, fst, trend)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)

# Load data ---------------------------------------------------------------
root <- './input/predictions'
spcs <- dir_ls(root)

# Function ----------------------------------------------------------------
get_sum_population <- function(spc){
  
  spc <- spcs[1]
  
  cat('To start\n')
  fls <- dir_ls(spcs, regexp = '.tif$')
  
  
  
  
  cat('Done\n')
  
}


