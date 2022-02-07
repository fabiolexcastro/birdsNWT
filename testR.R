

# Load libraries --------------------------------------------------------
require(pacman)

pacman::p_load(raster, rgdal, rgeos, terra, stringr, glue, sf, tidyverse, RStoolbox, fs, fst, trend)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)

# Load data ---------------------------------------------------------------
thrs <- read_csv('./inputs/prevOcc3.csv')
root <- './outputs'
dirs <- fs::dir_ls(root, type = 'directory')
spcs <- basename(dirs)
dirs <- glue('{dirs}/occur')
dirs <- as.character(dirs)