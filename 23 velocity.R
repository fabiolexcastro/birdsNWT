# here
# Load libraries ----------------------------------------------------------
library(pacman)
pacman::p_load(dplyr, fs, fst, gdata, glue, quantreg, rasterVis, reproducible,
               stringr,tidyverse, terra, yaImpute )

# Functions ---------------------------------------------------------------
source('./R/fatTail.R')
# Load data ---------------------------------------------------------------

pathFut <- 'inputs/predictions'
pathPres <- 'inputs/current'
dirsFut <- fs::dir_ls(pathFut, type = 'directory')
dirsPres <- fs::dir_ls(pathPres, type = 'directory')
gcms <- c('CCSM4', 'GFDLCM3', 'INMCM4')
species <- basename(dirsFut)

# Velocity metric ---------------------------------------------------------
get_velocity <- function(sp, gcm){
  
  sp <- species[1]
  gcm <- gcms[1]
  
  cat('Start ', basename(sp), ' ', gcm, '\n')
  flsFut <- grep(sp, dirsFut, value = TRUE)
  flsFut <- dir_ls(flsFut)
  flsFut <- grep(gcm, flsFut, value = TRUE)
  flsPres <- grep(sp, dirsPres, value = TRUE)
  flsPres <- dir_ls(flsPres)
  flsPres <- grep('NA_range_masked.tif', flsPres, value = TRUE)
  
  
  rsltdo <- map(.x = 1:length(flsFut), .f = function(i){
    
    #i <- 1 # Run and erase
    cat('Start ', basename(flsFut[i]=, '\t')
    cat('Start ', sp, '\t')
    
    fleFut <- flsFut[i]
    rstPres <- terra::rast(flsPres)
    rstFut <- terra::rast(fleFut)
    emptyRas <- rstPres * 0 + 1  
    
    tblPres <- terra::as.data.frame(rstPres, xy = TRUE)
    colnames(tblPres)[3] <- 'prev'
    tblFut <- terra::as.data.frame(rstFut, xy = TRUE)
    colnames(tblFut)[3] <- 'prev'
    
    p.xy <- mutate(tblPres, pixelID = 1:nrow(tblPres)) %>% dplyr::select(pixelID, x, y, prev) 
    f.xy <- mutate(tblFut, pixelID = 1:nrow(tblFut)) %>% dplyr::select(pixelID, x, y, prev) 
    
    p.xy2 <-filter(p.xy, prev > 0.1) %>% dplyr::select(1:3) %>% as.matrix()
    f.xy2 <-filter(f.xy, prev > 0.1) %>% dplyr::select(1:3) %>% as.matrix()
    
    if(nrow(f.xy) > 0){
      d.ann <- as.data.frame(ann(
        as.matrix(p.xy2[,-1, drop = FALSE]),
        as.matrix(f.xy2[,-1, drop = FALSE]),
        k = 1, verbose = F)$knnIndexDist)
      d1b <- as.data.frame(cbind(f.xy2, round(sqrt(d.ann[,2]))))
      names(d1b) <- c("ID","X","Y","bvel")
    } else {
      print(spec[i])
    }
    
    f.xy <- as.data.frame(f.xy)
    colnames(f.xy) <- c('ID', 'X', 'Y', 'Pres')
    f.xy <- as_tibble(f.xy)
    d1b <- left_join(f.xy, d1b, by = c('ID', 'X', 'Y'))
    d1b <- mutate(d1b, fat = fattail(bvel, 8333.3335, 0.5))
    sppref <- rast(d1b[,c(2,3,6)])
    sppref[is.na(sppref)] <- 0
    e<- ext(emptyRas)
    spprefExt <- extend(sppref,e)
    refstack <- c(spprefExt, emptyRas)
    futprevstack <- c(emptyRas, rstFut)
    # reterra::crs(sppref) <- terra::crs(msk)
    # tblFut <- terra::as.data.frame(tblFut, xy = TRUE)
    # tblMsk <- terra::as.data.frame(msk,    xy = TRUE)
    # tblMsk <- rownames_to_column(tblMsk)
    # tblFut <- rownames_to_column(tblFut)
    # tblMskFut <- full_join(tblMsk, tblFut, by = c('rowname', 'x', 'y'))
    # tblMskFut <- dplyr::select(tblMskFut, 2:5)
    # futprevstack <- terra::rast(tblMskFut, type = 'xyz')
    cat('Done ', flsFut[i], '\n')
    return(list(futprevstack, emptyRas)) 
    
  }) 
    
  # Getting the Future rasters
  ftr.trr <- map(1:length(rsltdo), function(h) rsltdo[[h]][[1]])
  ftr.trr <- map(1:length(ftr.trr), function(h) ftr.trr[[h]][[2]])
  ftr.stk <- do.call(what = c, args = ftr.trr)
  ftr.avg <- terra::app(ftr.stk, fun = 'mean')
  
  
}
