# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, reproducible, terra, stringr, glue, sf, tidyverse, RStoolbox, fs, fst, trend)

g <- gc(reset = TRUE)
rm(list = ls())

# Load data ---------------------------------------------------------------
root <- './outputs'
dirs <- fs::dir_ls(root, type = 'directory')
spcs <- basename(dirs)

# Function to use ---------------------------------------------------------
see_changes <- function(spc){
  
  spc <- spcs[1]
  
  cat('To start ', spc, '\n')
  dir <- grep(spc, dirs, value = TRUE)
  fle <- fs::dir_ls(dir, regexp = '.fst')
  tbl <- fst::read_fst(path = fle)
  tbl <- dplyr::select(tbl, x, y, gc, everything())
  names(tbl)[1:2] <- c('lon', 'lat')
  tbl <- mutate(tbl, avg = rowMeans(tbl[,4:9]))
  tbl <- as_tibble(tbl)
  
  cat('To see the average in a raster file\n')
  rst.avg <- tbl %>% dplyr::select(lon, lat, avg) %>% rasterFromXYZ()
  plot(rst.avg)
  
 }

# Apply the function ------------------------------------------------------


