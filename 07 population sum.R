

# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, future, furrr, reproducible, RColorBrewer, 
               colorspaces, ggspatial, ggpubr, gridExtra, terra, stringr, glue, 
               sf, tidyverse, RStoolbox, fs, future.apply, fst, trend)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)

# Load data ---------------------------------------------------------------
root <- './inputs/predictions'
spcs <- dir_ls(root)

# Function ----------------------------------------------------------------
get_sum_population <- function(spc){
  
  spc <- spcs[1]
  
  cat('To start\n')
  fls <- dir_ls(spc, regexp = '.tif$')
  
  cat('To get the name of each gcm\n')
  gcm <- str_split(basename(fls), '_')
  gcm <- sapply(gcm, function(x) x[1])
  gcm <- unique(gcm)
  prd <- str_sub(string = basename(fls), start = nchar(basename(fls)) - 7, end = nchar(basename(fls)) - 4)
  prd <- unique(prd)
  
  map(.x = 1:length(gcm), .f = function(i){
    map(.x = 1:length(prd), .f = function(j){
      
      i <- j <- 1 # Run and erase
      stk <- grep(gcm[i], fls, value = TRUE) %>% 
        grep(prd[j], ., value = TRUE) %>% 
        as.character() %>% 
        stack()
      cls <- cellStats(stk, 'sum')
      
    })
  })
  
  cat('Done\n')
  
}


