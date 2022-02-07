# Load libraries --------------------------------------------
library(pacman)

pacman::p_load(glue, raster, rgdal, rgeos, readxl, stringr, sf, R.filesets,
               tidyverse, terra, foreach, fs, future.apply, furrr, fst,
               stringr, glue, compiler, hrbrthemes, gtools, ggpubr, gridExtra, hrbrthemes, colorspace)

rm(list = ls())

# Load data ---------------------------------------------------------------
root <- './outputs'
dirs <- fs::dir_ls(root, type = 'directory')
spcs <- basename(dirs)

files <- fs::dir_ls('./qs')
files <- grep('changes', files, value = TRUE)
#dat_list <- lapply(files, qs::qread)
#saveRDS(dat_list,'./tables/qs/changesTables.RDS')

# Read list of changes Tables ---------------------------------------------
dat_list <- loadRDS('./tables/qs/changesTables.RDS')
g
a <- dat_list[[1]]
a %>% summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE)))
a %>% 
  
  
  # Create summary table summing all rows for each species ------------------


sum_table <- function(specie){
  # specie <- spcs[1]
  message(crayon::blue('Starting\n',specie, '\n'))
  table <- qs::qread(file = glue('./qs/{specie}_table_changes.qs'))
  sumDF <- table %>% group_by(gc) %>% summarise(across(y2011:y2100, sum))
  sumDF <- mutate(sumDF, species = specie)
  cat('Done \n')
  return(sumDF)
}  


sumTable <- map(.x = spcs, .f = sum_table)

totalTable <- bind_rows(sumTable)  ## join all rows into a same table 

qsave(totalTable, './tables/totalTable.qs')


long <- totalTable %>%  gather(Year, Value, - c(gc, species))
long <- long %>% mutate(Year = as.factor(Year))
levels(long$Year) <- c('2011','2031','2051', '2071', '2091', '2100')
long <- long %>% 
  arrange(gc, species, Year) 

# create table for plotting rate of change --------------------------------
totalTable <- qs::qread(file = glue('./tables/totalTable.qs'))

change <- long %>% 
  group_by(gc, species) %>% 
  mutate(Total = Value * 6.25,
         Previous = lag(Total), 
         Next = lead(Total),
         Change <- (Total - Previous),
         pctChange = (Total - Previous)/Previous) %>% 
  ungroup()
#qs::qsave(change, file = glue('./tables/yr_changeTable.qs'))

change <- qs::qread('./tables/yr_changeTable.qs')

make_loliPlot <- function(sp){
 # sp <- spcs[1]
  subd <- filter(change, species == sp)
  years <- c('2011', '2031', '2051', '2071','2091','2100')

   message(crayon::green('Making lolipop plot for:', sp))  
  
  loliplot <- ggplot(data = subd, aes(x = Year, y = pctChange))+ 
    geom_point(size = 8, aes(col = gc, group = gc)) + 
    geom_segment(
      aes(x = Year, y = 0, xend = Year, yend = pctChange, group = gc, 
          colour = gc), size = 1) + 
    geom_hline(yintercept = 0, color = 'black', size = 0.5)+
    scale_y_continuous(labels = scales::percent) +
    facet_wrap(.~gc) +
    theme_bw() +
    ggtitle(label = sp) +
    theme(plot.title = element_text(size = 14, face = 'bold'),
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_blank(), 
          axis.text.x = element_text(angle = 90, hjust = 0.5),
          legend.position = 'none') +
    labs( y = '% change', x = 'Year') +
    scale_x_discrete(labels = years)
  
  ggsave(plot = loliplot, filename = glue('./graphs/figs/yrChange/lolipopPlot_{sp}.png'),
         units = 'in', width = 12, height = 9, dpi = 700)
  
  return(loliplot)
  
}
# Apply the function ------------------------------------------------------

ggs <- map(.x = spcs, .f = make_loliPlot)
saveRDS(ggs, file = '../ggs.rds')

ggs <- readRDS(file = '../rds/ggs.rds')


gal <- ggarrange(plotlist = ggs, ncol = 10, nrow = 8)

ggsave(plot = gal, filename = '../png/all_species_change.png', units = 'in', width = 30, height = 28, dpi = 600)
