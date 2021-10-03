
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, future, furrr, reproducible, RColorBrewer, 
               colorspaces, ggspatial, ggpubr, gridExtra, terra, stringr, glue, 
               sf, tidyverse, RStoolbox, fs, future.apply, fst, trend)

pkg <- sort(c('raster', 'rgdal', 'rgeos', 'future', 'furrr', 'reproducible', 'RColorBrewer', 
              'colorspaces', 'ggspatial', 'ggpubr', 'gridExtra', 'terra', 'stringr', 'glue', 
              'sf', 'tidyverse', 'RStoolbox', 'fs', 'future.apply', 'fst', 'trend'))

pacman::p_load(cat(noquote(pkg)))

g <- gc(reset = TRUE)
rm(list = ls())

# Load data ---------------------------------------------------------------
root <- './inputs/predictions'
spcs <- dir_ls(root)

# Get values  -------------------------------------------------------------
get_extreme_values <- function(spc){
  
  # spc <- spcs[1]
  
  cat('Start\n', scp, '\n')
  fls <- dir_ls(spc, regexp = '.tif$')
  
  ncores <- 45
  plan(multicore, workers = ncores)
  future.apply::future_lapply(X = 1:length(fls), FUN = function(i){
    
    cat('Start ', fls[i], '\n')
    fl <- fls[i]
    rs <- raster(fl)
    vl <- getValues(rs)
    vl <- na.omit(vl)
    vl <- as.numeric(vl)
    cat('Done\n')
    return(vl)
    
  })
  future:::ClusterRegistry("stop")
  
  system.time(vls <- map(.x = 1:length(fls), .f = function(i){
    
    cat('Start ', fls[i], '\n')
    fl <- fls[i]
    rs <- raster(fl)
    vl <- getValues(rs)
    vl <- na.omit(vl)
    vl <- as.numeric(vl)
    cat('Done\n')
    return(vl)
  
  }))
  
  all <- Reduce(c, vls)
  all <- rnorm(100)
  prc <- quantile(all, seq(0, 1, 0.2))
  prc <- as.numeric(prc)
  dfm <- data.frame(specie = basename(spc), intervals = prc)
  dfm <- mutate(dfm, quantile = seq(0, 1, 0.2))
  dfm <- dplyr::select(dfm, specie, quantile, intervals)
  cat('Done\n')
  return(dfm)
  
}

# Apply all the function --------------------------------------------------
rsl <- map(.x = spcs, .f = get_extreme_values)
saveRDS(object = rsl, file = './test.rds')

# End

