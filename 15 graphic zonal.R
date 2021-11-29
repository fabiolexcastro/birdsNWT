

# Load libraries ----------------------------------------------------------
library(pacman)
pacman::p_load(glue, raster, rgdal, rgeos, readxl, stringr, sf, R.filesets,
               tidyverse, terra, foreach, fs, future.apply, furrr, fst, exactextractr,
               stringr, glue, compiler, hrbrthemes, gtools, ggpubr, gridExtra, hrbrthemes, colorspace)

rm(list = ls())

# Load data ---------------------------------------------------------------
fles <- dir_ls('./qs/zonal', regexp = 'table_ratio')

# Function ----------------------------------------------------------------
make_graph <- function(fle){
  
  fle <- fles[1]
  
  cat('Start\n')
  
  
}


