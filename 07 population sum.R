

# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, future, furrr, reproducible, RColorBrewer, 
               colorspaces, ggspatial, ggpubr, gridExtra, terra, stringr, glue, 
               sf, tidyverse, RStoolbox, fs, future.apply, fst, trend)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)

# Load data ---------------------------------------------------------------
root <- './inputs/predictions'
spcs <- dir_ls(root)

# Function ----------------------------------------------------------------
get_sum_population <- function(spc){
  
  # spc <- spcs[1]
  
  cat('To start\n')
  fls <- dir_ls(spc, regexp = '.tif$')
  
  cat('To get the name of each gcm\n')
  gcm <- str_split(basename(fls), '_')
  gcm <- sapply(gcm, function(x) x[1])
  gcm <- unique(gcm)
  prd <- str_sub(string = basename(fls), start = nchar(basename(fls)) - 7, end = nchar(basename(fls)) - 4)
  prd <- unique(prd)
  
  pop <- map(.x = 1:length(gcm), .f = function(i){
    rsl <- map(.x = 1:length(prd), .f = function(j){
      cat('Start ', gcm[i], ' ', prd[j], '\n')
      stk <- grep(gcm[i], fls, value = TRUE) %>% 
        grep(prd[j], ., value = TRUE) %>% 
        as.character() %>% 
        stack()
      cls <- cellStats(stk, 'sum')
      cls <- as.data.frame(cls)
      cls <- mutate(cls, model = gcm[i], period = prd[j], specie = basename(spc), run = 1:5)
      cls <- dplyr::select(cls, specie, model, period, run, sum_pop = cls)
      cat('Done sum pop\n')
      return(cls)
    })
    return(rsl)
  })
  
  po2 <- flatten(pop)
  po2 <- bind_rows(po2)
  rownames(po2) <- 1:nrow(po2)
  glue('./outputs/rds/sum_pop_{basename(spc)}.rds')
  saveRDS(object = po2, file = glue('./outputs/rds/sum_pop_{basename(spc)}.rds'))
  cat('Done\n')
  
}

# Apply the function ------------------------------------------------------
map(spcs, get_sum_population)


# To read the results -----------------------------------------------------
fles <- dir_ls('./outputs/rds', regexp = '.rds$')
fles <- grep('sum_pop', fles, value = TRUE)
tbls <- map(.x = fles, .f = readRDS)
tbls <- map(.x = tbls, .f = as_tibble)
tble <- bind_rows(tbls)
spcs <- unique(tble$specie)

infr <- min(tble$sum_pop)
supr <- max(tble$sum_pop)

# To make the graph -------------------------------------------------------
make_graph <- function(spc){
  
  # spc <- spcs[1] # Run and erase
  
  cat('Start ', spc, '\n')
  tbl <- filter(tble, specie == spc)
  tbl <- mutate(tbl, period = factor(period, levels = c('2011', '2031', '2051', '2071', '2091', '2100')))
  tbl <- mutate(tbl, run = factor(run, levels = c('1', '2', '3', '4', '5')))
  
  ggg <- ggplot(data = tbl, aes(x = period, y = sum_pop / 1000000, fill = run)) + 
    geom_bar(stat = 'identity', position = position_dodge()) + 
    facet_wrap(.~model, nrow = 1, ncol = 3) +
    scale_fill_manual(values = c("#D43F3A", "#EEA236", "#5CB85C", "#46B8DA", "#9632B8")) +
    ggtitle(label = glue('Specie: {spc}')) +
    scale_y_continuous(breaks = seq(0, 10, 1), limits = c(0, 10)) +
    theme(legend.position = 'bottom', 
          axis.text.y = element_text(angle = 90, vjust = 0.5, hjust = 0.5), 
          plot.title = element_text(size = 16, hjust = 0.5, face = 'bold')) +
    labs(x = '', y = 'Total population (x 1000000)', fill = 'Run')
  
  out <- glue('./graphs/figs/sum_pop/{spc}.png')
  
  ggsave(plot = ggg, filename = out, units = 'in', width = 12, height = 9, dpi = 300)
  cat('Done\n')
  
}

map(spcs, make_graph)


