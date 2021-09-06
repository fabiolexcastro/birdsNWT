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
  dir
  
}

# Apply the function ------------------------------------------------------


