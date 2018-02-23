data_folder = '../../data/'
code_folder = '../'
plot_folder = '../../plots/'

geom_sf_load = FALSE
source(paste0(code_folder, 'load_packages.R'))

source(paste0(code_folder, 'shape_functions/PointsInShape.R'))
source(paste0(code_folder, 'shape_functions/ResolveMultiPoint.R'))
source(paste0(code_folder, 'shape_functions/CreateCircles.R'))

library(RColorBrewer)
source(paste0(code_folder, 'plots/plot_functions/AddAlpha.R'))
source(paste0(code_folder, 'plots/plot_functions/GenXYRLM.R'))

load(paste0(data_folder, 'block_and_group.rdata'))
load(paste0(data_folder, 'business_frame.rdata'))
load(paste0(data_folder, 'crime.rdata'))

library(scales)
library(cowplot)

##------------------------------------##

## in our analyses, we restrict to blockgroups of at least 400

min_total_blockgroup <- 400

##------------------------------------##

## first, crime (vio and nonvio) by pop count, density

blockgroup_data %<>% mutate(pop_density = as.numeric(total / area)) %>%
    filter(total >= min_total_blockgroup)

title_size = 19
b_height = 12.5
gap = 0.09

xy_data <- blockgroup_data %>%
    select(x = total, y = crime_violent)

p1 <- GenXYRLM(xy_data,
               xlab = 'Population Count',
               ylab = 'Number of Violent Crimes',
               title = 'Violent Crime vs. Population Count',
               title_size = title_size)

xy_data <- blockgroup_data %>%
    select(x = total, y = crime_nonviolent)

p2 <- GenXYRLM(xy_data,
               xlab = 'Population Count',
               ylab = 'Number of Non-violent Crimes',
               title = 'Non-violent Crime vs. Population Count',
               title_size = title_size)

xy_data <- blockgroup_data %>%    
    select(x = pop_density, y = crime_violent)

p3 <- GenXYRLM(xy_data,
               xlab = expression('Population Density (/' ~ m^2 *')'),
               ylab = 'Number of Violent Crimes',
               title = 'Violent Crime vs. Population Density',
               title_size = title_size)

xy_data <- blockgroup_data %>%    
    select(x = pop_density, y = crime_nonviolent)

p4 <- GenXYRLM(xy_data,
               xlab = expression('Population Density (/' ~ m^2 *')'),
               ylab = 'Number of Non-violent Crimes',
               title = 'Non-violent Crime vs. Population Density',
               title_size = title_size)

## combine:

all_plot <- plot_grid(p1, p2, NULL, NULL, p3, p4,
                      nrow = 3, ncol = 2,
                      rel_heights = c(1, gap, 1))
save_plot(filename = paste0(plot_folder, 'plots_xy/fig_panel_crime_population.pdf'),
          plot = all_plot,
          base_height = b_height * (1 + gap/2),
          base_aspect_ratio = 1.1 / (1 + gap/2))

##------------------------------------##

## excess is defined on pop.

vio_rlm <- rlm(crime_violent ~ total, data = blockgroup_data)
nonvio_rlm <- rlm(crime_nonviolent ~ total, data = blockgroup_data)

blockgroup_data$excess_vio_pop = vio_rlm$residuals
blockgroup_data$excess_nonvio_pop = nonvio_rlm$residuals

xy_data <- blockgroup_data %>%
    select(x = income, y = excess_vio_pop)

p1 <- GenXYRLM(xy_data %>% filter(!is.na(x)),
               xlab = 'Per Capita Income',
               ylab = 'Excess Number of Violent Crimes',
               title = 'Excess Violent Crime vs. Income',
               title_size = title_size)

xy_data <- blockgroup_data %>%
    select(x = income, y = excess_nonvio_pop)

p2 <- GenXYRLM(xy_data %>% filter(!is.na(x)),
               xlab = 'Per Capita Income',
               ylab = 'Excess Number of Non-violent Crimes',
               title = 'Excess Non-violent Crime vs. Income',
               title_size = title_size)

xy_data <- blockgroup_data %>%
    select(x = poverty_metric, y = excess_vio_pop)

p3 <- GenXYRLM(xy_data %>% filter(!is.na(x)),
               xlab = 'Poverty Metric',
               ylab = 'Excess Number of Violent Crimes',
               title = 'Excess Violent Crime vs. Poverty', 
               title_size = title_size)

xy_data <- blockgroup_data %>%
    select(x = poverty_metric, y = excess_nonvio_pop)

p4 <- GenXYRLM(xy_data %>% filter(!is.na(x)),
               xlab = 'Poverty Metric',
               ylab = 'Excess Number of Non-violent Crimes',
               title = 'Excess Non-violent Crime vs. Poverty',
               title_size = title_size)

## combine:

all_plot <- plot_grid(p1, p2, NULL, NULL, p3, p4,
                      nrow = 3, ncol = 2,
                      rel_heights = c(1, gap, 1))
save_plot(filename = paste0(plot_folder, 'plots_xy/fig_panel_crime_economic.pdf'),
          plot = all_plot,
          base_height = b_height * (1 + gap/2),
          base_aspect_ratio = 1.1 / (1 + gap/2))


##------------------------------------##

## excess is defined on bunch of stuff

vio_rlm <- rlm(crime_violent ~ total + income + poverty_metric, data = blockgroup_data)
nonvio_rlm <- rlm(crime_nonviolent ~ total + income + poverty_metric, data = blockgroup_data)

is_na_income_pov <- is.na(blockgroup_data$income) | is.na(blockgroup_data$poverty_metric)

blockgroup_data$excess_vio_popecon <- NA
blockgroup_data$excess_vio_popecon[!is_na_income_pov] = vio_rlm$residuals
blockgroup_data$excess_nonvio_popecon <- NA
blockgroup_data$excess_nonvio_popecon[!is_na_income_pov] = nonvio_rlm$residuals

xy_data <- blockgroup_data %>%
    select(x = vacant_proportion,
           y = excess_vio_popecon)

p1 <- GenXYRLM(xy_data %>% filter(!is.na(x) & !is.na(y)),
               xlab = 'Vacant Proportion',
               ylab = 'Excess Number of Violent Crimes',
               title = 'Excess Violent Crime vs. Vacant Land',
               title_size = title_size)

xy_data <- blockgroup_data %>%
    select(x = vacant_proportion,
           y = excess_nonvio_popecon)

p2 <- GenXYRLM(xy_data %>% filter(!is.na(x) & !is.na(y)),
               xlab = 'Vacant Proportion',
               ylab = 'Excess Number of Non-violent Crimes',
               title = 'Excess Non-violent Crime vs. Vacant Land',
               title_size = title_size)


xy_data <- blockgroup_data %>%
    select(x = comres_proportion,
           y = excess_vio_popecon)

p3 <- GenXYRLM(xy_data %>% filter(!is.na(x) & !is.na(y)),
               xlab = 'Commercial Proportion',
               ylab = 'Excess Number of Violent Crimes',
               title = 'Excess Violent Crime vs. Commercial Land',
               title_size = title_size)

xy_data <- blockgroup_data %>%
    select(x = comres_proportion,
           y = excess_nonvio_popecon)

p4 <- GenXYRLM(xy_data %>% filter(!is.na(x) & !is.na(y)),
               xlab = 'Commercial Proportion',
               ylab = 'Excess Number of Non-violent Crimes',
               title = 'Excess Non-violent Crime vs. Commercial Land',
               title_size = title_size * 0.97)

xy_data <- blockgroup_data %>%
    select(x = mixeduse_proportion,
           y = excess_vio_popecon)

p5 <- GenXYRLM(xy_data %>% filter(!is.na(x) & !is.na(y)),
               xlab = 'Mixed Proportion',
               ylab = 'Excess Number of Violent Crimes',
               title = 'Excess Violent Crime vs. Mixed Land',
               title_size = title_size)

xy_data <- blockgroup_data %>%
    select(x = mixeduse_proportion,
           y = excess_nonvio_popecon)

p6 <- GenXYRLM(xy_data %>% filter(!is.na(x) & !is.na(y)),
               xlab = 'Mixed Proportion',
               ylab = 'Excess Number of Non-violent Crimes',
               title = 'Excess Non-violent Crime vs. Mixed Land',
               title_size = title_size)

## combine:

all_plot <- plot_grid(p1, p2, NULL, NULL, p3, p4, NULL, NULL, p5, p6,
                      nrow = 5, ncol = 2,
                      rel_heights = c(1, gap, 1, gap, 1))
save_plot(filename = paste0(plot_folder, 'plots_xy/fig_panel_crime_landuse.pdf'),
          plot = all_plot,
          base_height = b_height * 3  * (1 + 2 * gap/ 3) / 2,
          base_aspect_ratio = 1.1  * 2 / (3 * (1 + 2 * gap/ 3)))

##------------------------------------##
