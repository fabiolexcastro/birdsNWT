
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, future, furrr, reproducible, RColorBrewer, 
               colorspace, ggspatial, ggpubr, gridExtra, terra, stringr, glue, 
               sf, tidyverse, fs, fst, trend)

g <- gc(reset = TRUE)
rm(list = ls())


# Load data ---------------------------------------------------------------
path <- '../output'
spcs <- dir_ls(path)
fles <- dir_ls(spcs[1], regexp = '.tif$')


# Sum --------------------------------------------------------------------
flsm <- grep('sum', fles, value = TRUE)
flsm <- as.character(flsm)


