
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
