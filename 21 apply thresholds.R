
# Load libraries ----------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, stringr, sf, fasterize, tidyverse, fs, gtools, glue)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)

# Load data ---------------------------------
trhs <- read_csv('./inputs/prevOcc.csv')
root <- './outputs'
dirs <- dir_ls(root, type = 'directory')
dirs <- glue('{dirs}/occur')
dirs <- as.character(dirs)

# Get the name of each GCM
gcms <- dir_ls(dirs[1], regexp = '.tif$') 
gcms <- basename(gcms)
gcms <- grep('2011', gcms, value = TRUE)
gcms <- basename(gcms)
gcms <- str_sub(gcms, 16, nchar(gcms) - 4)

# List each directory -----------------------------------------------------
fles <- as.character(flatten(map(.x = dirs, .f = function(k){dir_ls(k, regexp = '.tif')})))
spcs <- str_sub(dirs, 11, 15)
