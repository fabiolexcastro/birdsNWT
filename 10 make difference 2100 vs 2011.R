
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, future, furrr, reproducible, RColorBrewer, 
               colorspaces, ggspatial, ggpubr, gridExtra, terra, stringr, glue, 
               sf, tidyverse, RStoolbox, fs, future.apply, fst, trend)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)


# Load data ---------------------------------------------------------------
root <- './outputs/predictions'
spcs <- dir_ls(root) 

# Make difference ---------------------------------------------------------
make_difference <- function(spc){
  
  # spc <- spcs[1]
  
  cat('Start ', spc, '\n')
  fls <- dir_ls(grep(spc, spcs, value = TRUE))
  fls <- grep('mean', fls, value = TRUE)
  fls <- as.character(fls)
  fls <- grep(paste0(c('2011', '2100'), collapse = '|'), fls, value = TRUE)
  gcm <- str_split(fls, pattern = '_')
  gcm <- sapply(gcm, function(k) k[[4]])
  gcm <- gsub('.tif', '', gcm)
  gcm <- unique(gcm)
  
  lapply(1:length(gcm), function(i){
    cat('Start ', gcm[i], '\n')
    stk <- raster::stack(grep(gcm[i], fls, value = TRUE))
    dfr <- stk[[2]] - stk[[1]]
    tbl <- as_tibble(rasterToPoints(dfr))
    names(tbl) <- c('x', 'y', paste0('diff_', basename(spc), '_', gcm[i]))
    out <- glue('./tables/qs/diff_{basename(spc)}_{gcm[i]}.qs')
    qsave(x = tbl, file = out)
    cat('Done!\n')
  })
  
  cat('----- Finish ----- \n')
  
}

# Apply the function ------------------------------------------------------
lapply(spcs, make_difference)

