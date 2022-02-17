#"# Load libraries ----------------------------------------------------------
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

species <- basename(dirsFut)
ecor <- terra::rast('./inputs/RTM_BCR6_NT1.tif')

# Velocity metric ---------------------------------------------------------
get_velocity <- function(sp){
  sp <- species[1]
  flsFut <- grep(sp, dirsFut, value = TRUE)
  flsPres <- grep(sp, dirsPres, value = TRUE)
  flsFut <- dir_ls(flsFut)
  flsPres <- dir_ls(flsPres)
  flsPres <- grep('NA_range.tif', flsPres, value = TRUE)
  
  rsltdo <- map(.x = 1:length(flsFut), .f = function(i){
    
    i <- 1 # Run and erase
    cat('Start ', flsFut[i], '\t')
    #cat('Start ', sp, '\t')
    
    fleFut <- flsFut[i]
    rstPres <- terra::rast(flsPres)
    rstFut <- terra::rast(fleFut)
    
    msk <- rstPres * 0 + 1
    
    
    tblPres <- terra::as.data.frame(rstPres, xy = TRUE)
    colnames(tblPres)[3] <- 'prev'
    tblFut <- terra::as.data.frame(rstFut, xy = TRUE)
    colnames(tblFut)[3] <- 'prev'
    
    # p.xy <- mutate(tblPres, pixelID = 1:nrow(tblPres)) %>% dplyr::select(pixelID, x, y, prev) %>% as.matrix()
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
    #rstFut <- terra::resample(rstFut, msk, method = 'near')
    terra::crs(sppref) <- terra::crs(msk)
    tblFut <- terra::as.data.frame(tblFut, xy = TRUE)
    tblMsk <- terra::as.data.frame(msk,    xy = TRUE)
    tblMsk <- rownames_to_column(tblFut)
    tblFut <- rownames_to_column(tblFut)
    tblMskFut <- full_join(tblMsk, tblFut, by = c('rowname', 'x', 'y'))
    
    futprevstack <- c(msk, rstFut)
    
    
    
    cat('Ending: ', flsFut[i], '\n')
    return(list(futprevstack, refstack))
    
  })
  
}
