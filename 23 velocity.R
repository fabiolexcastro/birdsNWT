

# Load libraries ----------------------------------------------------------
library(pacman)
pacman::p_load(dplyr, fs, fst, gdata, glue, quantreg, rasterVis, reproducible,
               stringr,tidyverse, terra )


# Load data ---------------------------------------------------------------

pathPred <- 'inputs/predictions'
pathCurr <- 'inputs/current'
dirsPred <- fs::dir_ls(pathPred, type = 'directory')
dirsCurr <- fs::dir_ls(pathPred, type = 'directory')
species <- basename(dirs)
ecor <- terra::rast('./inputs/RTM_BCR6_NT1.tif')

# Velocity metric ---------------------------------------------------------
get_velocity <- function(sp){
  
  sp <- species[1]
  
  flsPrd <- grep(sp, dirsPred, value = TRUE)
  flsCur <- grep(sp, dirsCurr, value = TRUE)
  flsPrd <- dir_ls(flsPrd)
  flsCur <- dir_ls(flsCur)
  flsCur <- grep('NA_range.tif', flsCur, value = TRUE)
  
  map(.x = 1:length(flsPrd), .f = function(i){
    
    i <- 1 # Run and erase
    flePrd <- flsPrd[i]
    rstCur <- terra::rast(fleCur)
    rstPrd <- terra::rast(flePrd)
    
    msk <- rstCur * 0 + 1
    
    tblCur <- terra::as.data.frame(rstCur, xy = TRUE)
    colnames(tblCur)[3] <- 'prev'
    tblPrd <- terra::as.data.frame(rstPrd, xy = TRUE)
    colnames(tblPrd)[3] <- 'prev'
  
    p.xy <- mutate(tblCur, pixelID = 1:nrow(tblCur)) %>% dplyr::select(pixelID, x, y, prev) %>% as.matrix()
    f.xy <- mutate(tblPrd, pixelID = 1:nrow(tblCur)) %>% dplyr::select(pixelID, x, y, prev) %>% as.matrix()
    head(p.xy)
    head(f.xy)
    
    p.xy2 <- as.data.frame(p.xy) %>% filter(., prev > 0.1) %>% dplyr::select(1:3) %>% as.matrix()
    f.xy2 <- as.data.frame(f.xy) %>% filter(., prev > 0.1) %>% dplyr::select(1:3) %>% as.matrix()
  
    head(p.xy2)
    
    
  })
  
  fls <- fs::dir_ls(sp)
  yrs <- parse_number(basename(fls))
  yrs <- unique(yrs)
  yrs <- na.omit(yrs)
  gcm <- str_sub(basename(fls), start = 45, end = nchar(basename(fls)) - 17)
  gcm <- unique(gcm)
  
  
  present <- terra::rast(fl)
  
}

head(mtcars[, c("mpg", "cyl"), drop = FALSE])
head(mtcars[, c("mpg", "cyl")])
