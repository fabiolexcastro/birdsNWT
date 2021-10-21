

# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, future, furrr, reproducible, RColorBrewer, 
               colorspaces, ggspatial, ggpubr, gridExtra, terra, stringr, glue, 
               sf, tidyverse, RStoolbox, fs, future.apply, fst, trend, crayon)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)

# Functions ---------------------------------------------------------------
source('08_functions.R')


# Load data ---------------------------------------------------------------
spcs <- dir_ls('./outputs')
prds <- c('')


# Main function -----------------------------------------------------------
make_slope <- function(spc){
  
  spc <- spcs[1] # Run and erase 
  
  message(crayon::green("Loading: ", spc))
  fls <- dir_ls(spc, regexp = '.tif$')
  fls <- grep('mean', fls, value = TRUE)
  fls <- as.character(fls)
  
  message(crayon::green("GCMS"))
  gcm <- grep('2011', fls, value = TRUE)
  gcm <- str_sub(basename(gcm), 16, nchar(basename(gcm)) - 4)
  
  message(crayon::red('Make stack'))
  st1 <- grep(gcm[1], fls, value = TRUE)
  st1 <- st1[-grep('100', st1, value = FALSE)]
  st1 <- raster::stack(st1)
  
  st2 <- grep(gcm[2], fls, value = TRUE)
  st2 <- st2[-grep('100', st2, value = FALSE)]
  st2 <- raster::stack(st2)
  
  st3 <- grep(gcm[3], fls, value = TRUE)
  st3 <- st3[-grep('100', st3, value = FALSE)]
  st3 <- raster::stack(st3)
  
  
  dtSP <- lapply(X = st1, FUN = function(sp){
    browser()
    arrayStack <- raster::as.array(x = sp)
    times <- c(2011, 2031, 2051, 2071, 2091)
    slopeValues <- apply(X = arrayStack, MARGIN = c(1, 2), FUN = function(x){
      slpCoef <- RcppArmadillo::fastLmPure(X = cbind(1, times), y = x) # Original formula was way slower: lm(x ~ times, data = dfX, na.action = na.omit)
      coef <- slpCoef$coefficients[2]
      pVal <- 2*pt(abs(slpCoef$coefficients/slpCoef$stderr), slpCoef$df.residual, lower.tail=FALSE)[2]
      return(list(coef = coef, pVal = pVal))
    })
    slopeCoefficientVals <- matrix(unlist(lapply(slopeValues, `[[`, 1)),
                                   nrow = nrow(arrayStack),
                                   ncol = ncol(arrayStack),
                                   byrow = FALSE) # retrieves values from slope Coefficient, arranges into a corrected (inversed) matrix
    slopeSignificancyVals <- matrix(unlist(lapply(slopeValues, `[[`, 2)),
                                    nrow = nrow(arrayStack),
                                    ncol = ncol(arrayStack),
                                    byrow = FALSE) # retrieves values from slope Coefficient, arranges into a corrected (inversed) matrix
    slopeCoeff <- sp[[1]] %>%
      raster::setValues(slopeCoefficientVals)
    names(slopeCoeff) <- "slopeCoeff"
    
    slopeSignificancy <- sp[[1]] %>%
      raster::setValues(slopeSignificancyVals)
    names(slopeSignificancy) <- "slopeSignificancy"
    return(list(slopeCoefficient = slopeCoeff, slopeSignificancy = slopeSignificancy))
  })
  
  
  
}



