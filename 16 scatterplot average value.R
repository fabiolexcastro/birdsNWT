

# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, future, furrr, reproducible, RColorBrewer, 
               colorspace, ggspatial, ggpubr, gridExtra, terra, stringr, glue, 
               sf, tidyverse, RStoolbox, fs, future.apply, fst, trend, crayon)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)

# Load data ---------------------------------------------------------------
root <- './outputs'
dirs <- fs::dir_ls(root, type = 'directory')
spcs <- basename(dirs)

# Function ----------------------------------------------------------------
get_scatterplot <- function(spc){
  
  spc <- spcs[1] # Run and erase or use #
  
  cat('Start\n')
  dir <- grep(spc, dirs, value = TRUE)
  fle <- dir_ls(dir, regexp = '.qs')
  tbl <- qs::qread(file = glue('./qs/{spc}_table_ratio.qs'))
  tbl <- dplyr::select(tbl, lon, lat, everything())
  tbl <- as_tibble(tbl)
  tbl <- dplyr::select(tbl, lon, lat, gc, avg)
  tbl <- tbl %>% spread(gc, avg)
  colnames(tbl) <- gsub('-', '_', colnames(tbl)) 
  tbl <- mutate(tbl, gid = 1:nrow(tbl))
  
  set.seed(1234)
  smp <- sample_n(tbl = tbl, size = nrow(tbl) * 0.01, replace = FALSE)
  smp$gid
  
  cat('Scatterplot 1')
  
  cat('Done!\n')
  
}


















