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
load(paste0(data_folder, 'crime.rdata'))

library(scales)
library(cowplot)

##------------------------------------##

## fig 5

crime_bar <- crime %>%
    filter(crime_cat %in% c('violent', 'nonviolent'))

crime_bar$crimetype[crime_bar$crimetype == 'otherassault'] = 'assault'

all_crimes <- crime_bar %>%
    select(crimetype, ucr) %>%
    distinct() %>%
    arrange(ucr) %>%
    filter(!(crimetype == 'assault' & ucr == 800)) %>%
    filter(!(crimetype == 'sexcrime' & ucr == 1700))

crime_bar$crime_factor <- factor(crime_bar$crimetype,
                                 levels = all_crimes$crimetype,
                                 ordered = TRUE)

renaming <- str_to_title(all_crimes$crimetype)
names(renaming) = all_crimes$crimetype

renaming['murder'] = 'Homicide'
renaming['sexcrime'] = 'Sexual'
## renaming['otherassault'] = 'Other Assault'
renaming['motortheft'] = 'Motor Theft'

p_crimebars <- ggplot(data = crime_bar, aes(crime_factor)) + geom_bar(fill = 'blue') +
    ylab('Number of Crimes') + xlab('') +
    scale_x_discrete(labels = renaming) +
    scale_y_continuous(labels = comma)

save_plot(filename = paste0(plot_folder, 'plots_bar/fig_barplot_crimebytype.pdf'),
          plot = p_crimebars,
          base_height = 7,
          base_aspect_ratio = 1.8)
