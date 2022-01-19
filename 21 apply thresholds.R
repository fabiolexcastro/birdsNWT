
# Load libraries ----------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, terra, stringr, sf, fasterize, tidyverse, fs, gtools, glue)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)

# Load data ---------------------------------
thrs <- read_csv('./inputs/prevOcc.csv')
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

# List each directory -----------------------------------------------------
fles <- as.character(flatten(map(.x = dirs, .f = function(k){dir_ls(k, regexp = '.tif')})))
spcs <- str_sub(dirs, 11, 14)

# Function ----------------------------------------------------------------
my_rcl <- function(spc){
  
  spc <- spcs[1] # Run and erase
  
  cat('Start ', spc, '\n')
  fls <- grep(spc, fles, value = TRUE)
  thr <- filter(thrs, spec == spc)
  vle <- unique(thr$pOccMean)
  
  cat('To reclassify\n')
  rst <- map(.x = 1:length(fls), .f = function(i){
    cat(i, '\n')
    rs <- terra::rast(fls[i])
    rc <- rs
    rc[which(rc[] < vle)] <- 0
    return(rc)
  })
  
  cat('Raster to table\n')
  tbl <- map(.x = 1:length(rst), .f = function(j){
    return(as.data.frame(rst[[j]], xy = TRUE))
  })
  dfm <- tbl %>% purrr::reduce(inner_join)
  
  
  
  
  cat('Done!\n')
  
  
}

