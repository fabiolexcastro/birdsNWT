
# Load libraries ----------------------------------------------------------
library(pacman)
pacman::p_load(dplyr, fs, fst, gdata, glue, quantreg, rasterVis, reproducible,
               stringr,tidyverse, terra, yaImpute )


g <- gc(reset = TRUE)
rm(list = ls())

# Load data ---------------------------------------------------------------
root <- './qs/zonal'
fles <- dir_ls(root, regexp = '2.qs$')
head(fles); length(fles); tail(fles)

grps <- read_csv('./inputs/birdSpecies_traits.csv')
head(grps); dim(grps)

spcs <- str_sub(basename(fles), 1, 4)
spcs <- unique(spcs)

# Make graph function -----------------------------------------------------
make_graph <- function(spc){
  
  
  
}




