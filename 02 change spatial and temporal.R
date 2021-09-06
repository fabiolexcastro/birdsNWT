

# Load libraries --------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, terra, stringr, glue, sf, tidyverse, RStoolbox, fs, fst, trend)

g <- gc(reset = TRUE)
rm(list = ls())

# Functions to use --------------------------------------------------------
raster_to_table <- function(spc){
  
  # Proof
  # spc <- spcs[1] # Run and comment (after)
  cat('Start ', spc, '\n')
  dir <- grep(spc, dirs, value = TRUE)
  fls <- fs::dir_ls(dir)
  yrs <- parse_number(basename(fls))
  yrs <- unique(yrs)
  gcm <- str_sub(basename(fls), start = 16, end = nchar(basename(fls)) - 4)
  gcm <- unique(gcm)
  
  cat('Raster to table\n')
  dfm <- map(.x = 1:length(gcm), .f = function(k){
    
    cat(gcm[k], '\n')
    fl <- grep(gcm[k], fls, value = TRUE)
    
    cat('----- Terra library functions -----\n')
    tr <- terra::rast(fl)
    tb <- terra::as.points(tr)
    df <- terra::as.data.frame(x = tb)
    names(df) <- paste0('y', yrs)
    gm <- terra::geom(tb)
    df <- cbind(gm[,3:4], df)
    df <- as_tibble(df)
    df <- mutate(df, gc = gcm[k])
    return(df)
    
  })
  
  rsl <- bind_rows(dfm)
  fst::write_fst(x = rsl, path = glue('./outputs/{spc}/tbl_yrs_{scp}.fst'))
  cat('------- Done -------\n')
  return(rsl)
  
}

# Load data ---------------------------------------------------------------
root <- './outputs'
dirs <- fs::dir_ls(root, type = 'directory')
spcs <- basename(dirs)

# Raster to table ---------------------------------------------------------
dfrm <- map(.x = spcs, .f = see_changes)
dim(dfrm)
object.size(dfrm)









