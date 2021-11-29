



# Load libraries ----------------------------------------------------------
library(pacman)
pacman::p_load(glue, raster, rgdal, rgeos, readxl, stringr, sf, R.filesets,
               tidyverse, terra, foreach, fs, future.apply, furrr, fst,
               stringr, glue, compiler, hrbrthemes, gtools, ggpubr, gridExtra, hrbrthemes, colorspace)

rm(list = ls())


# Load data ---------------------------------------------------------------


fles <- dir_ls('./qs', regexp = 'table_ratio')
fles
shpf <- shapefile('inputs/ecoregions/ecoregions.shp')

# Function ----------------------------------------------------------------
get_max_min <- function(fle){
  
  fle <- fles[1]
  
  cat('Start\n')
  spc <- str_sub(basename(fle), 1, 4)
  qst <- qs::qread(fle)
  gcm <- unique(qst$gc)
  
  rsl <- map(.x = 1:length(gcm), .f = function(k){
    
    k <- 1 # Run and erase
    tbl <- filter(qst, gc == gcm)
    
    
  })
    
}

