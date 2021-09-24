
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, future, furrr, reproducible, RColorBrewer, 
               colorspaces, ggspatial, ggpubr, gridExtra, terra, stringr, glue, 
               sf, tidyverse, RStoolbox, fs, future.apply, fst, trend)

g <- gc(reset = TRUE)
rm(list = ls())

# Load data ---------------------------------------------------------------
root <- './inputs/predictions'
dirs <- fs::dir_ls(root, type = 'directory')
spcs <- basename(dirs)
limt <- sf::st_read('limiteareadeestudio.shp')
targetCRS <-  paste("+proj=lcc +lat_1=49 +lat_2=77 +lat_0=0 +lon_0=-95",
                    "+x_0=0 +y_0=0 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")
limt <- st_transform(x = limt, targetCRS)

# Function to use ---------------------------------------------------------
make_sum <- function(spc){
  
  # spc <- spcs[1] # Run and erase
  
  cat('Start ', spc, '\n')
  dir <- grep(spc, dirs, value = TRUE)
  fls <- fs::dir_ls(dir, directory = '.tif$')
  
  cat('To get the name of each gcm\n')
  gcm <- str_split(basename(fls), '_')
  gcm <- sapply(gcm, function(x) x[3])
  gcm <- unique(gcm)
  
  cat('To apply to each gcm\n')
  rsl <- map(.x = 1:length(gcm), function(k){
    
    prd <- str_sub(fls, start = nchar(fls) - 7, end = nchar(fls) - 4)
    prd <- unique(prd)
    fl <- grep(gcm[k], fls, value = TRUE)
    
    rs <- map(.x = 1:length(prd), .f = function(i){
      
      cat(gcm[k], '\n')
      fl <- grep(prd[i], fl, value = TRUE)
      rs <- terra::rast(fl)
      tb <- terra::as.points(rs)
      df <- terra::as.data.frame(x = tb)
      sm <- apply(X = df, MARGIN = 1, FUN = sum)
      sd <- apply(X = df, MARGIN = 1, FUN = sd)
      gm <- terra::geom(tb)
      df <- cbind(gm[,3:4], df)
      df <- mutate(df, sma = sm, std = sd)
      rs.sm <- dplyr::select(df, x, y, sma) %>% rasterFromXYZ()
      rs.sd <- dplyr::select(df, x, y, std) %>% rasterFromXYZ()
      dr <- glue('./outputs/{spc}')
      writeRaster(x = rs.sm, filename = glue('{dr}/sum_{spc}_{gcm[k]}_{prd[i]}.tif'), overwrite = T)
      writeRaster(x = rs.sd, filename = glue('{dr}/std_{spc}_{gcm[k]}_{prd[i]}.tif'), overwrite = T)
      cat('Done\n')
      
    })

  })
  
  cat(' --------------------- Finish --------------------\n')
  
}

# Apply the function to make the sum --------------------------------------
make_sum(spc = spcs[1])
map(.x = spcs[2:length(spcs)], .f = make_sum)

# To make the maps --------------------------------------------------------

fldr <- './outputs'
spcs <- fs::dir_ls(fldr, type = 'directory')

make_maps <- function(spc){
  
  spc <- spcs[1] # Run and comment
  
  fld <- grep(spc, spcs, value = TRUE)
  fls <- list.files(fld, full.names = TRUE, pattern = '.tif')
  fls <- grep(paste0(c('sum', 'std'), collapse = '|'), fls, value = TRUE)
  print(fls)
  gcm <- str_split(basename(fls), '_')
  gcm <- sapply(gcm, function(x) x[3])
  gcm <- unique(gcm)
  
  map(.x = 1:length(gcm), .f = function(i){
    
    i <- 1 # Run and comment
    
    fle <- grep(gcm[i], fls, value = TRUE)
    sum <- grep('sum', fle, value = TRUE) 
    std <- grep('std', fle, value = TRUE)
    sum <- raster::stack(sum)
    tsm <- terra::rast(sum)
    tsd <- terra::rast(std)
    std <- raster::stack(std)
    
    # Sum raster - Table
    tbl.sum <- rasterToPoints(sum, spatial = FALSE) %>% as_tibble()
    tbl.sum <- mutate(tbl.sum, gid = 1:nrow(tbl.sum))
    tbl.sum <- gather(tbl.sum, var, value, -gid, -x, -y)
    tbl.sum <- separate(data = tbl.sum, col = var, into = c('type', 'spc', 'gcm', 'year'))
    
    # Std raster - Table
    tbl.std <- rasterToPoints(sum, spatial = FALSE) %>% as_tibble()
    tbl.std <- mutate(tbl.std, gid = 1:nrow(tbl.std))
    tbl.std <- gather(tbl.std, var, value, -gid, -x, -y)
    tbl.std <- separate(data = tbl.std, col = var, into = c('type', 'spc', 'gcm', 'year'))
    
  })
  
  
}




