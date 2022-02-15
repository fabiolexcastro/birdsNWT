

# Load libraries ----------------------------------------------------------
library(pacman)
pacman::p_load(dplyr, fs, fst, gdata, glue, quantreg, rasterVis, reproducible,
               stringr,tidyverse, terra )


# Load data ---------------------------------------------------------------

pathPred <- 'inputs/predictions'
pathCurr <- 'inputs/current'
dirsPred <- fs::dir_ls(pathPred, type = 'directory')
dirsCurr <- fs::dir_ls(pathPred, type = 'directory')
species <- basename(dirs)
ecor <- terra::rast('./inputs/RTM_BCR6_NT1.tif')

# Velocity metric ---------------------------------------------------------
get_velocity <- function(sp){
  
  sp <- species[1]
  
  flsPrd <- grep(sp, dirsPred, value = TRUE)
  flsCur <- grep(sp, dirsCurr, value = TRUE)
  flsPrd <- dir_ls(flsPrd)
  flsCur <- dir_ls(flsCur)
  flsCur <- grep('NA_range.tif', flsCur, value = TRUE)
  
  fls <- fs::dir_ls(sp)
  yrs <- parse_number(basename(fls))
  yrs <- unique(yrs)
  yrs <- na.omit(yrs)
  gcm <- str_sub(basename(fls), start = 45, end = nchar(basename(fls)) - 17)
  gcm <- unique(gcm)
  
  
  present <- terra::rast(fl)
  
}