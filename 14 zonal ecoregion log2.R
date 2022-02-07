



# Load libraries ----------------------------------------------------------
library(pacman)
pacman::p_load(glue, raster, rgdal, rgeos, readxl, stringr, sf, R.filesets,
               tidyverse, terra, foreach, fs, future.apply, furrr, fst, exactextractr,
               stringr, glue, compiler, hrbrthemes, gtools, ggpubr, gridExtra, hrbrthemes, colorspace)

rm(list = ls())


# Load data ---------------------------------------------------------------


fles <- dir_ls('./qs', regexp = 'table_ratio')
fles
shpf <- shapefile('inputs/ecoregions/ecoregions.shp')

# Function ----------------------------------------------------------------
get_max_min <- function(fle){
  
  # fle <- fles[1]
  
  cat('Start\n')
  spc <- str_sub(basename(fle), 1, 4)
  qst <- qs::qread(fle)
  gcm <- unique(qst$gc)
  
  rsl <- map(.x = 1:length(gcm), .f = function(k){
    
    cat(gcm[k], '\n')
    tbl <- filter(qst, gc == gcm[k])
    tbl <- dplyr::select(tbl, lon, lat, logRatio)
    rst <- rasterFromXYZ(tbl)
    # plot(rst) # Run and erase
    crs(rst) <- targetCRS
    znl.avg <- exactextractr::exact_extract(rst, ecrg_limt, 'mean')
    znl.sdt <- exactextractr::exact_extract(rst, ecrg_limt, 'stdev')
    dfm <- data.frame(region = pull(ecrg_limt, 'REGION_NAM'), average = znl.avg, sdt = znl.sdt)
    dfm <- as_tibble(dfm)
    dfm <- mutate(dfm, model = gcm[k])
    cat('Done ', gcm[k], '\n')
    return(dfm)
    
  })
  
  rsl <- bind_rows(rsl)
  qs::qsave(x = rsl, file = glue('./qs/zonal/{spc}_logZonal.qs'))
  cat('Done!\n')
  return(rsl)

    
}

# Apply the function ------------------------------------------------------
cat('Calculating zonal\n')
plan(cluster, workers = 3, gc = TRUE)
options(future.globals.maxSize= 4194304000) ## this option helps with  the error about global sizes 
znl_all <- furrr::future_map(.x = 1:length(fles), .f = function(i){
  cat('Start\n')
  znl <- get_max_min(fle = fles[i])
  cat('Done!\n')
  return(znl)
})
future:::ClusterRegistry('stop')                       
                        
                        
                        