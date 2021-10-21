
loadMeanRas<- function(birdList, 
                       pathData,
                       climateScenario = NULL,
                       year = NULL){
  meanPath <- checkPath(file.path(pathData), create = TRUE)
  message(crayon::green(paste0("Looking for files in ", meanPath)))
  
  listDirs <- list.dirs(meanPath, recursive = FALSE)
  
  allMeans <- lapply(X = birdList, FUN = function(bird){
    ##list all files within the meanPath that match the pattern
    meanAvailable <- usefulFuns::grepMulti(x = list.files(listDirs, full.names = TRUE),
                                           patterns = c(bird, climateScenario))
    # browser()
    if(length(meanAvailable) == 0)
      stop(paste0("Predictions for" , bird,
                  "are not available. Please verify species"))
    
    message(crayon::green("Loading the following file(s) for", bird))
    message(crayon::magenta(paste0(" "), paste0(meanAvailable, sep = "\n")))
    allRas <- lapply(meanAvailable, raster)
    
    return(allRas)
  })
  
  stkRas <- lapply(allMeans, stack)
  names(stkRas) <- birdList
  return(stkRas)
  
}

dtSP <- lapply(X = rst, FUN = function(sp){
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


