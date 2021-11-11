
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, future, furrr, reproducible, RColorBrewer, 
               colorspaces, ggspatial, ggpubr, gridExtra, terra, stringr, glue, 
               sf, tidyverse, RStoolbox, fs, future.apply, fst, trend)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)


# Load data ---------------------------------------------------------------
root <- './outputs/predictions'
spcs <- dir_ls(root) 

# Make difference ---------------------------------------------------------
make_difference <- function(spc){
  
  spc <- spcs[1]
  
  cat('Start ', spc, '\n')
  
  
}


