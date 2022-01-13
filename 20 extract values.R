

# Load libraries ----------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, stringr, sf, fasterize, tidyverse, fs, gtools, glue)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)

# Load data ---------------------------------------------------------------
fles <- dir_ls('./qs/sample')
tbls <- map(fles, qread)
tbls <- bind_rows(tbls)

# list files ocurrences
root <- './outputs'
dirs <- dir_ls(root, type = 'directory')
dirs <- glue('{dirs}/occur')

# Get the name of each GCM
gcms <- dir_ls(dirs[1], regexp = '.tif$') 
gcms <- gcms[1]

# To process --------------------------------------------------------------
get_values <- function(gcm){
  
  
  
}



