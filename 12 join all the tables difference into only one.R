
# Load libraries --------------------------------------------
library(pacman)

pacman::p_load(glue, raster, rgdal, rgeos, readxl, stringr, sf, R.filesets,
               tidyverse, terra, foreach, fs, future.apply, furrr, fst,
               stringr, glue, compiler, hrbrthemes, gtools, ggpubr, gridExtra, hrbrthemes, colorspace)

rm(list = ls())


# Load data ---------------------------------------------------------------
root <- '../tables/qs'
fles <- dir_ls(root, regexp = '.qs')

# Function ----------------------------------------------------------------
join_tble <- function(gcm){
  
  gcm <- gcms[1]
  
  fle <- grep(gcm, gcm, value = TRUE)
  fle <- mixedsort(fle)
  tbl <- qs::qread(file = fle)
  colnames(tbl)
  
}