

# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, future, furrr, reproducible, RColorBrewer, 
               colorspaces, ggspatial, ggpubr, gridExtra, terra, stringr, glue, 
               sf, tidyverse, RStoolbox, fs, future.apply, fst, trend, crayon)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)

# Functions ---------------------------------------------------------------
source('08_functions.R')


# Load data ---------------------------------------------------------------
spcs <- dir_ls('./outputs')
prds <- c('')


# Main function -----------------------------------------------------------
make_slope <- function(spc){
  
  spc <- spcs[1] # Run and erase 
  
  message(crayon::green("Loading: ", spc))
  fls <- dir_ls(spc, regexp = '.tif$')
  fls <- grep('mean', fls, value = TRUE)
  
  
  
}



