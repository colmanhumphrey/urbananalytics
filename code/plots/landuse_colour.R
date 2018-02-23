data_folder = '../../data/'
code_folder = '../'
plot_folder = '../../plots/'

source(paste0(code_folder, 'load_packages.R'))

source(paste0(code_folder, 'shape_functions/PointsInShape.R'))
source(paste0(code_folder, 'shape_functions/ResolveMultiPoint.R'))
source(paste0(code_folder, 'shape_functions/CreateCircles.R'))

library(RColorBrewer)
source(paste0(code_folder, 'plots/plot_functions//AddAlpha.R'))
source(paste0(code_folder, 'plots/plot_functions/GenXYRLM.R'))
source(paste0(code_folder, 'plots/plot_functions/GenGeomGrob.R'))

load(paste0(data_folder, 'block_and_group.rdata'))
load(paste0(data_folder, 'business_frame.rdata'))
load(paste0(data_folder, 'landuse.rdata'))
## load(paste0(data_folder, 'crime.rdata'))

library(grid)
library(gridExtra)
library(scales)
library(cowplot)

##------------------------------------##

near_empty_theme = theme(
    plot.background = element_rect(fill = 'transparent')
   ,panel.background = element_rect(fill = 'transparent')
   ,panel.grid.major = element_line(color = 'transparent')
   ,axis.text = element_blank()
   ,axis.ticks = element_blank()
   ,line=element_blank()
   ,axis.title=element_blank()
)

##------------------------------------

landuse_legend <- bind_cols('use_detail' = landuse$use_detail,
                            'use_color' = landuse$use_color) %>% distinct() %>% arrange(use_detail)
landuse_legend$use_names = str_to_title(gsub('_', ' / ', landuse_legend$use_detail))
## individual change...:
landuse_legend$use_names[1] = 
    'Active Recreation'
landuse_legend$use_names[4] =
    'Commercial:\nBusiness'
landuse_legend$use_names[5] =
    'Commercial:\nConsumer'
landuse_legend$use_names[6] = 
    'Commercial / Residential\nMixed'
landuse_legend$use_names[11:13] =
    str_wrap(gsub(' / ', ': ',
                  gsub('density', ' Density',
                       landuse_legend$use_names[11:13])),
             15)

landuse_legend$text_size_rel = 1
landuse_legend$text_size_rel[6] = 0.845
landuse_legend$text_size_rel[7] = 0.96

landuse_legend$textcol = 'white'
landuse_legend$textcol[c(7, 11, 12, 13)] = 'black'

## ## rearrange:
land_order = c(12, 13, 11, ## res
               6, ## mixed
               5, 4, ## comm
               8, ## indus
               3, ## civic,               
               10, 1, 7, ## park / rec / culture
               15, ## water
               2, ## cemetary
               9, 14) ## unknown, vacant
landuse_legend = landuse_legend[land_order,]               
               
## landuse_legend %<>% arrange(use_color)

center_city_blocks <- block_data %>%
    filter(censustract < 1300) %>% ## all censustracts up to 12.xx
    pull(row_index)

## for center city plotting, we want those contained,
## and those that overlap, intersected...:

center_city_geom <- st_union(block_geom[center_city_blocks,])

landuse_in_cc <- st_within(landuse, center_city_geom, sparse = FALSE)
## really, overlaps does what I want, but I find that function
## isn't named well for my small brain
landuse_on_cc <- st_intersects(landuse, center_city_geom, sparse = FALSE)
land_inter <- st_intersection(landuse[landuse_on_cc & !landuse_in_cc,], center_city_geom)

cc_landuse <- rbind(landuse[landuse_in_cc,],
                    land_inter)

cc_landuse_grob <- ggplot(cc_landuse) + 
    geom_sf(fill = cc_landuse$use_color,
            colour = alpha('white', 0.001),
            size = 0.001) +
    near_empty_theme

landuse_full_grob <- ggplot(landuse) + 
    geom_sf(fill = landuse$use_color,
            colour = alpha('white', 0.001),
            size = 0.001) +
    near_empty_theme

## now arranging it all

pdf(file = paste0(plot_folder, 'plots_map/landuse_both.pdf'),
    width = 14, height = 10)
grid.newpage()
vp_full <- viewport(x = 0, y = 0, just = c('left', 'bottom'),
               width = .75, height = 1)
print(landuse_full_grob, vp = vp_full)

vp_cc <- viewport(x = 0.42, y = 0, just = c('left', 'bottom'),
                  width = .48, height = 0.6)
print(cc_landuse_grob, vp = vp_cc)

vp_leg <- viewport(x = 0.842, y = 0, just = c('left', 'bottom'),
                   width = .2, height = 1)
pushViewport(vp_leg)

legblock = grid.rect(x = 0.5, y = seq(0.85, 0.15, length.out = nrow(landuse_legend)),
                     width = 0.49, height = (0.85 - 0.15) / (nrow(landuse_legend) - 1),
                     gp = gpar(col = landuse_legend$use_color, fill = landuse_legend$use_color))
legtext = grid.text(label = landuse_legend$use_names,
                x = 0.5, y = seq(0.85, 0.15, length.out = nrow(landuse_legend)),
                gp = gpar(fontsize = 10 * landuse_legend$text_size_rel, col = landuse_legend$textcol))
popViewport()

## add circles
## no, manually...

dev.off()
