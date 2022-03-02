# Load libraries ----------------------------------------------------------
library(pacman)
pacman::p_load(dplyr, fs, fst, gdata, glue, quantreg, rasterVis, reproducible,
               stringr,tidyverse, terra, yaImpute)
g <- gc(reset = TRUE)
rm(list = ls())

# Functions ---------------------------------------------------------------
source('./R/fatTail.R')
# Load data ---------------------------------------------------------------
pathFut <- 'inputs/predictions'
pathPres <- 'inputs/present'
dirsFut <- fs::dir_ls(pathFut, type = 'directory')
dirsPres <- fs::dir_ls(pathPres, type = 'directory')
gcms <- c('CCSM4', 'GFDLCM3', 'INMCM4')
species <- basename(dirsFut)
rcp <- c('45', '85')

# Velocity metric ---------------------------------------------------------
get_velocity <- function(sp){
  sp <- species[1]
  message(crayon::blue('Starting with ', sp, '\n'))
  flsFut <- grep(sp, dirsFut, value = TRUE)
  dirPres <- grep(sp, dirsPres, value = TRUE)
  flsFut <- dir_ls(flsFut)
  
  gcm <- str_sub(basename(flsFut),start = 45, end = nchar(basename(flsFut)) -17) # this will change if the file_name structure changes
  gcm <- unique(gcm)
  
  message(crayon::blue('Applying to each rcp', '\n'))
  rsltdo <- map(.x = 1:length(rcp), function(k){
    flsFut <- grep(rcp[k], flsFut, value = TRUE)
    yrs <- parse_number(basename(flsFut))
    yrs <- unique(yrs)
    
    rs <- map(.x = 1:length(yrs), .f = function(i){
      message(crayon::blue('Applying to year',i, '\n'))
      flsPres <- dir_ls(dirPres)
      flsPres <- grep('range_masked.tif', flsPres, value = TRUE)
      fleFut <- grep(yrs[i], flsFut, value = TRUE)
      
      rstPres <- terra::rast(flsPres)
      rstFut <- terra::rast(fleFut)
      emptyRas <- rstPres * 0 + 1 
      emptyRas <- trim(emptyRas) 
      e <- ext(emptyRas)
      
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
      sppref <- extend(sppref,e)
      # refstack <- c(sppref, emptyRas)
      refstack <- sppref
      rstFutExt <- extend(rstFut, e)
      # futprevstack <- c(emptyRas, rstFut)
      futprevstack <- rstFut
      cat('Done ', flsFut[i], '\n')
      return(list(futprevstack, refstack))
    })
    
    
    # Getting the Future rasters
    ftr.stk <- map(1:length(rs), function(h) rs[[h]][[1]])
    ftr.stk <- map(1:length(ftr.stk), function(h) mean(ftr.stk[[h]]))
    
    # Refstack
    ref.stk <- map(1:length(rs), function(h) rs[[h]][[2]])
    ref.stk <- c(ref.stk[[1]], ref.stk[[2]], ref.stk[[3]])
    ref.stk <- terra::app(ref.stk, fun = 'mean')
    
    # Write these rasters
    out <- glue('./outputs/velocity/{sp}')
    ifelse(!file.exists(out), dir_create(out), print('Already exists'))
    terra::writeRaster(ftr.stk, glue('{out}/ftr_mean_{rcp[k]}.tif'))
    terra::writeRaster(ref.stk, glue('{out}/ref_mean_{rcp[k]}.tif'))
    cat('Finish!\n')
    
  })
  
  
  
  
  
  
}
