

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
glue('{dirs}/occur')

# To process --------------------------------------------------------------




