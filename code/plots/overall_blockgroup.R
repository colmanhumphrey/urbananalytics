data_folder = '../../data/'
code_folder = '../'
plot_folder = '../../plots/'

source(paste0(code_folder, 'load_packages.R'))

source(paste0(code_folder, 'shape_functions/PointsInShape.R'))
source(paste0(code_folder, 'shape_functions/ResolveMultiPoint.R'))
source(paste0(code_folder, 'shape_functions/CreateCircles.R'))

library(RColorBrewer)
source(paste0(code_folder, 'plots/plot_functions/AddAlpha.R'))
source(paste0(code_folder, 'plots/plot_functions/GenXYRLM.R'))
source(paste0(code_folder, 'plots/plot_functions/GenGeomGrob.R'))

load(paste0(data_folder, 'block_and_group.rdata'))
load(paste0(data_folder, 'business_frame.rdata'))
## load(paste0(data_folder, 'crime.rdata'))

library(scales)
library(cowplot)

##------------------------------------##

near_empty_theme = theme(
    plot.background = element_rect(fill = 'white')
   ,panel.background = element_rect(fill = 'white')
   ,panel.grid.major = element_line(color = 'transparent')
   ,axis.text = element_blank()
   ,axis.ticks = element_blank()
   ,line=element_blank()
   ,axis.title=element_blank()
)

##------------------------------------
## figure 1

## left:
## plain block and groups

block_plain <- GenGeomGrob(block_geom, fill_col = 'transparent',
                           border_col = 'grey87', lwd = 0.05)
blockgroup_plain <- block_plain + geom_sf(data = blockgroup_geom,
                                          fill = 'transparent',
                                          colour = 'black',
                                          lwd = 0.3)

## right:
## pop. dens

blockgroup_data %<>% mutate(pop_density = as.numeric(total / area))
blockgroup_geom$pop_density = blockgroup_data$pop_density

dens_grob <- ggplot(blockgroup_geom, aes(fill = pop_density)) +
    geom_sf(colour = alpha('white', 0.001),
            size = 0.001) +
    scale_fill_distiller(palette = 'RdPu',
                         direction = 1,
                         limits = c(0, 0.06),
                         breaks = c(0, 0.02, 0.04, 0.06),
                         labels = c(0, 0.02, 0.04, 0.06),
                         name = expression('Population Density (/' ~ m^2 *')'),
                         oob = scales::squish) +
    near_empty_theme +
    theme(legend.position = c(0.6, 0.25),
          legend.key.size = unit(0.5, 'cm'),
          legend.title=element_text(size=10),
          legend.text=element_text(size=9))

## combine
both_plot <- plot_grid(blockgroup_plain, dens_grob, nrow = 1)
save_plot(filename = paste0(plot_folder, 'plots_map/fig_map_blockgroups_philadelphia.pdf'),
          plot = both_plot,
          base_height = 8,
          base_aspect_ratio = 2)

##------------------------------------
## figure 2
blockgroup_geom$income <- blockgroup_data$income
blockgroup_geom$poverty_metric <- blockgroup_data$poverty_metric
## want them to be jointly NA
## they're nearly already that way: table(is.na(blockgroup_geom$income), is.na(blockgroup_geom$poverty_metric))
na_either <- is.na(blockgroup_geom$income) | is.na(blockgroup_geom$poverty_metric)
blockgroup_geom$income[na_either] = NA
blockgroup_geom$poverty_metric[na_either] = NA

## left:
## income

income_grob <- ggplot(blockgroup_geom, aes(fill = income)) +
    geom_sf(colour = alpha('white', 0.001),
            size = 0.001) +
    scale_fill_distiller(palette = 'RdPu',
                         direction = 1,
                         limits = c(0, 140000),
                         breaks = c(0, 35000, 70000, 105000, 140000),
                         labels = c('0', '35,000', '70,000', '105,000', '140,000'),
                         name = 'Income ($)',
                         oob = scales::squish) +
    near_empty_theme +
    theme(legend.position = c(0.7, 0.25),
          legend.key.size = unit(0.5, 'cm'),
          legend.title=element_text(size=10),
          legend.text=element_text(size=9))


## right:
## pop. dens

poverty_grob <- ggplot(blockgroup_geom, aes(fill = poverty_metric)) +
    geom_sf(colour = alpha('white', 0.001),
            size = 0.001) +
    scale_fill_distiller(palette = 'RdPu',
                         direction = 1,
                         limits = c(0, 1),
                         breaks = c(0, 0.25, 0.5, 0.75, 1),
                         labels = c(0, 0.25, 0.5, 0.75, 1),
                         name = 'Poverty Metric',
                         oob = scales::squish) +
    near_empty_theme +
    theme(legend.position = c(0.7, 0.25),
          legend.key.size = unit(0.5, 'cm'),
          legend.title=element_text(size=10),
          legend.text=element_text(size=9))

## combine
both_plot <- plot_grid(income_grob, poverty_grob, nrow = 1)
save_plot(filename = paste0(plot_folder, 'plots_map/fig_map_economic_philadelphia.pdf'),
          plot = both_plot,
          base_height = 8,
          base_aspect_ratio = 2)

##------------------------------------
## figure 4
blockgroup_geom$vacant_proportion <- blockgroup_data$vacant_proportion
blockgroup_geom$mixeduse_proportion <- blockgroup_data$mixeduse_proportion

## left:
## vacant_proportion

vacant_proportion_grob <- ggplot(blockgroup_geom, aes(fill = vacant_proportion)) +
    geom_sf(colour = alpha('white', 0.001),
            size = 0.001) +
    scale_fill_distiller(palette = 'RdPu',
                         direction = 1,
                         limits = c(0, 0.4),
                         breaks = c(0, 0.1, 0.2, 0.3, 0.4),
                         labels = c(0, 0.1, 0.2, 0.3, 0.4),
                         name = 'Vacant Proportion',
                         oob = scales::squish) +
    near_empty_theme +
    theme(legend.position = c(0.7, 0.25),
          legend.key.size = unit(0.5, 'cm'),
          legend.title=element_text(size=10),
          legend.text=element_text(size=9))


## right:
## pop. dens

mixed_proportion_grob <- ggplot(blockgroup_geom, aes(fill = mixeduse_proportion)) +
    geom_sf(colour = alpha('white', 0.001),
            size = 0.001) +
    scale_fill_distiller(palette = 'RdPu',
                         direction = 1,
                         limits = c(0, 0.25),
                         breaks = c(0, 0.05, 0.1, 0.15, 0.2, 0.25),
                         labels = c(0, 0.05, 0.1, 0.15, 0.2, 0.25),
                         name = 'Mixed Proportion',
                         oob = scales::squish) +
    near_empty_theme +
    theme(legend.position = c(0.7, 0.25),
          legend.key.size = unit(0.5, 'cm'),
          legend.title=element_text(size=10),
          legend.text=element_text(size=9))

## combine
both_plot <- plot_grid(vacant_proportion_grob, mixed_proportion_grob, nrow = 1)
save_plot(filename = paste0(plot_folder, 'plots_map/fig_map_landuse_vacantmixed.pdf'),
          plot = both_plot,
          base_height = 8,
          base_aspect_ratio = 2)

##------------------------------------
## figure 6
blockgroup_geom$crime_violent <- blockgroup_data$crime_violent
blockgroup_geom$crime_nonviolent <- blockgroup_data$crime_nonviolent

## left:
## crime_violent

crime_violent_grob <- ggplot(blockgroup_geom, aes(fill = crime_violent)) +
    geom_sf(colour = alpha('white', 0.001),
            size = 0.001) +
    scale_fill_distiller(palette = 'RdPu',
                         direction = 1,
                         limits = c(0, 4000),
                         breaks = c(0, 1000, 2000, 3000, 4000),
                         labels = c('0', '1,000', '2,000', '3,000', '4,000'),
                         name = 'Violent Crime Count',
                         oob = scales::squish) +
    near_empty_theme +
    theme(legend.position = c(0.7, 0.25),
          legend.key.size = unit(0.5, 'cm'),
          legend.title=element_text(size=10),
          legend.text=element_text(size=9))


## right:
## pop. dens

crime_nonviolent_grob <- ggplot(blockgroup_geom, aes(fill = crime_nonviolent)) +
    geom_sf(colour = alpha('white', 0.001),
            size = 0.001) +
    scale_fill_distiller(palette = 'RdPu',
                         direction = 1,
                         limits = c(0, 12000),
                         breaks = c(0, 3000, 6000, 9000, 12000),
                         labels = c('0', '3,000', '6,000', '9,000', '12,000'),
                         name = 'Non-violent Crime Count',
                         oob = scales::squish) +
    near_empty_theme +
    theme(legend.position = c(0.7, 0.25),
          legend.key.size = unit(0.5, 'cm'),
          legend.title=element_text(size=10),
          legend.text=element_text(size=9))

## combine
both_plot <- plot_grid(crime_violent_grob, crime_nonviolent_grob, nrow = 1)
save_plot(filename = paste0(plot_folder, 'plots_map/fig_map_crime_philadelphia.pdf'),
          plot = both_plot,
          base_height = 8,
          base_aspect_ratio = 2)

##------------------------------------
