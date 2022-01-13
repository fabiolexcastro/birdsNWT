

# Load libraries ----------------------------
require(pacman)
pacman::p_load(raster, exactextractr, rgdal, rgeos, stringr, sf, fasterize, tidyverse, fs, gtools, glue)

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
dirs <- glue('{dirs}/occur')
dirs <- as.character(dirs)

# Get the name of each GCM
gcms <- dir_ls(dirs[1], regexp = '.tif$') 
gcms <- basename(gcms)
gcms <- grep('2011', gcms, value = TRUE)
gcms <- basename(gcms)
gcms <- str_sub(gcms, 16, nchar(gcms) - 4)

# To process --------------------------------------------------------------
get_values <- function(gcm){
  
  gcm <- gcms[1]
  
  cat('Start ', gcm, '\n')
  dir <- map(dirs, dir_ls)
  dir <- map(dir, as.character)
  dir <- map(1:length(dir), function(i){grep(gcm, dir[[i]], value = TRUE)})
  dir <- flatten(dir)
  dir <- unlist(dir)
  yrs <- c('2011', '2031', '2051', '2071', '2091')
  
  cat('To get the table for each period\n')
  map(1:length(yrs), function(k){
    
    k <- 1 # Run and erase
    fls <- grep(yrs[k], dir, value = TRUE)
    stk <- raster::stack(fls)
    trr <- terra::rast(fls)
    
    rsl <- map(1:10000, function(j){
      cat(j, '\n')
      sub <- filter(tbls, rep == k)
      vls <- exactextractr::exact_extract(x = stk, y = sub[,1:2])
      vls <- terra::extract(trr, sub[,1:2])
      vls <- as.data.frame(vls)
      vls <- as_tibble(vls)
      vls <- mutate(vls, rep = k)
      cat('Done!\n')
      return(vls)
    })
    
    rsl[[1]]; rsl[[2]]
    
  })
  
  
}



