

# Load libraries --------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, terra, stringr, sf, tidyverse, RStoolbox, fs, trend)

g <- gc(reset = TRUE)
rm(list = ls())

# Load data ---------------------------------------------------------------
root <- './outputs'
dirs <- fs::dir_ls(root, type = 'directory')
spcs <- basename(dirs)

# See the changes  --------------------------------------------------------
see_changes <- function(spc){
  
  # Proof
  spc <- spcs[1] # Run and comment (after)
  
  cat('Start ', spc, '\n')
  dir <- grep(spc, dirs, value = TRUE)
  fls <- fs::dir_ls(dir)
  yrs <- parse_number(basename(fls))
  yrs <- unique(yrs)
  gcm <- str_sub(basename(fls), start = 16, end = nchar(basename(fls)) - 4)
  
  map(.x = 1:length(gcm), .f = function(k){
    
    cat(gcm[k], '\n')
    fl <- grep(gcm[k], fls, value = TRUE)
    fl
    
  })
  
}

