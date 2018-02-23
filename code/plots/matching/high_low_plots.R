dot_back = '../../'
data_folder = paste0(dot_back, '../data/')
code_folder = dot_back
plot_folder = paste0(dot_back, '../plots/')

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
load(paste0(data_folder, 'crime.rdata'))
## load(paste0(data_folder, 'landuse.rdata'))

library(cowplot)

##------------------------------------##

load(file = paste0(data_folder, 'match_result/high_low_calcs.rdata'))

load(file = paste0(data_folder, 'match_result/high_low_counts_excess.rdata'))

load(file = paste0(data_folder, 'match_result/high_low_areas.rdata'))

time_name_vec <- c('total' = 'total_open',
                   'weekday' = 'weekday_evening_open',
                   'weekend' = 'weekend_night_open')

window_titles = c('Whole Week', 'Weekday Evenings (6pm - 12pm)', 'Weekend Nights (12pm - 4am)')
names(window_titles) = names(time_name_vec)

##------------------------------------

## first, the legend (same for both):

leg_data <- data_frame(x_text = c('Negative, Significant',
                                  'Negative, Not Significant',
                                  'Positive, Not Significant',
                                  'Positive, Significant',
                                  'No Difference',
                                  'No Valid Comparison'),
                       p_col = c('#E11515', '#FFC1C1', '#6181FF', '#1515E1',
                                 '#B0A0B0', '#FFFFFF'),
                       text_col = c('white', 'black', 'black', 'white', 'black', 'black'),
                       box_col = rep('black', 6),
                       y = 1)
                                 
leg_end <- ggplot(data = leg_data,
                  aes(x = x_text, y = y, fill = p_col)) + geom_tile(colour = leg_data$box_col, size = 0.3) +
    scale_fill_identity(guide = 'none') +
    scale_x_discrete(limits = leg_data$x_text) +
    labs(x = '', y = '') + geom_text(aes(label = x_text), colour = leg_data$text_col) +
    theme_void()

##------------------------------------
## generating t stats:

## first for the counts and excess:

source(paste0(code_folder, 'plots/plot_functions/GenTTest.R'))

counts_res <- list()

for(time_var in names(time_name_vec)){
    print(time_var)

    counts_res[[time_var]] <- list()
    
    for(crimecat in c('violent', 'nonviolent')){
        print(crimecat)

        count_high = counts_list[[time_var]][[crimecat]]$high
        count_low = counts_list[[time_var]][[crimecat]]$low
        
        diff_mat = count_low - count_high
        zero_mat = count_low == 0 & count_high == 0

        diff_mat[zero_mat] = NA

        res_mat = GenTTest(diff_mat)
        
        counts_res[[time_var]][[crimecat]] = res_mat
    }
}

## do excess:

excess_res <- list()

for(time_var in names(time_name_vec)){
    print(time_var)

    excess_res[[time_var]] <- list()
    
    for(crimecat in c('violent', 'nonviolent')){
        print(crimecat)

        means_high = excess_list[[time_var]][[crimecat]]$high_means
        counts_high = excess_list[[time_var]][[crimecat]]$high_counts
        
        means_low = excess_list[[time_var]][[crimecat]]$low_means
        counts_low = excess_list[[time_var]][[crimecat]]$low_counts

        ## counts already deletes when both are zero
        ## this allows hours vs no hours to happen:
        ## IF YOU SO CHOOSE
        ## means_high[is.na(means_high)] = 0
        ## means_low[is.na(means_low)] = 0
        
        diff_mat = means_low - means_high
        zero_mat = counts_low == 0 & counts_high == 0

        diff_mat[zero_mat] = NA

        res_mat = GenTTest(diff_mat)
        
        excess_res[[time_var]][[crimecat]] = res_mat
    }
}
##------------------------------------

## the plots

## Counts and Excess

x_labels = c('Business Count: Violent',
             'Business Count: Non-violent',
             'Average Excess: Violent',
             'Average Excess: Non-violent')
names(x_labels) = c('violent_counts',
                    'nonviolent_counts',
                    'violent_excess',
                    'nonviolent_excess')

plot_list <- list()

source(paste0(code_folder, 'plots/plot_functions/CombineTMats.R'))

for(time_var in names(time_name_vec)){
    combine_df <- bind_rows(CombineTMats(counts_res[[time_var]]) %>% mutate(xvar = 'counts'),
                            CombineTMats(excess_res[[time_var]]) %>% mutate(xvar = 'excess'))

    combine_df %<>% mutate(cx_var = paste0(crime_type, '_', xvar))

    combine_df %<>% mutate(plot_text = paste0(round(means, 2), ' (', counts, ')'))
    combine_df$plot_text[is.na(combine_df$means)] = ''

    p_t <- ggplot(data = combine_df,
                  aes(x = cx_var, y = bus_type, fill = p_col)) +
        geom_tile() + scale_fill_identity(guide = 'none') + 
        labs(x = '', y = 'Business Type') + 
        geom_vline(xintercept = 1.5, colour = 'grey', alpha = 0.5) +
        geom_vline(xintercept = 3.5, colour = 'grey', alpha = 0.5) +
        geom_vline(xintercept = 2.5, colour = 'black', alpha = 0.9)

    p_t_text = p_t + geom_text(aes(label = plot_text),
                               colour = combine_df$text_col) +
        ggtitle(window_titles[time_var])

    rel_ind = (1:10)
    
    row_text = rev(str_to_title(combine_df$bus_type[rel_ind]))

    row_lim = rev(combine_df$bus_type[rel_ind])

    p_t_text = p_t_text + theme(axis.text.x = element_text(angle = 90, hjust = 1, colour = 'black'),
                                axis.text.y = element_text(colour = 'black'),
                                plot.title = element_text(face = "bold")) +
        scale_y_discrete(limits = row_lim,
                         labels = row_text) +
        scale_x_discrete(limits = names(x_labels),                         
                         labels = x_labels)

    plot_list[[time_var]] = p_t_text
}
    
main_plot <- plot_grid(plot_list[[1]], plot_list[[2]], plot_list[[3]], nrow = 1)

## save_plot(filename = paste0(plot_folder, 'plots_matching/fig_matching_crimedens.pdf'),
##           plot = main_plot, base_height = 10, base_aspect_ratio = 1.8)

leg_prop_height = 0.1

## ... adjust legend:

empty_box <- ggplot()

## these l-r adjustments are particular to this exact plot:
leg_end_adj <- plot_grid(empty_box, leg_end, empty_box,
                         nrow = 1, rel_widths = c(0.1, 1, 0.02))

main_with_legend <- plot_grid(main_plot, leg_end_adj,
                              nrow = 2, rel_heights = c(1, leg_prop_height))

save_plot(filename = paste0(plot_folder, 'plots_matching/fig_matching_crimedens.pdf'),
          plot = main_with_legend,
          base_height = 10 * (1 + leg_prop_height),
          base_aspect_ratio = 1.8 / (1 + leg_prop_height))


##------------------------------------

## land areas:

area_res <- list()

for(time_var in names(time_name_vec)){
    area_res[[time_var]] = list()

    for(crimecat in c('violent', 'nonviolent')){

        high_area = areas_list[[time_var]][[crimecat]]$high
        low_area = areas_list[[time_var]][[crimecat]]$low

        ## get: vacant prop; comres ratio; mixed prop

        high_area %<>%
            mutate(vacant_prop = area_vacant_land / (50 * 50 * pi),
                   res_area =
                       area_residential_lowdensity +
                       area_residential_mediumdensity +
                       area_residential_highdensity,
                   com_area =
                       area_commercial_consumer + area_commercial_business,
                   comres_prop = com_area / (res_area + com_area),
                   mixed_prop = area_commercial_mixedresidential / (50 * 50 * pi)) %>%
            select(vacant_prop,
                   comres_prop,
                   mixed_prop) %>%
            mutate_all(as.numeric)

        low_area %<>%
            mutate(vacant_prop = area_vacant_land / (50 * 50 * pi),
                   res_area =
                       area_residential_lowdensity +
                       area_residential_mediumdensity +
                       area_residential_highdensity,
                   com_area =
                       area_commercial_consumer + area_commercial_business,
                   comres_prop = com_area / (res_area + com_area),
                   mixed_prop = area_commercial_mixedresidential / (50 * 50 * pi)) %>%
            select(vacant_prop,
                   comres_prop,
                   mixed_prop) %>%
            mutate_all(as.numeric)

        area_diff = low_area - high_area

        t_mat = GenTTest(area_diff)

        area_res[[time_var]][[crimecat]] = t_mat
    }
}

## and the plots:

land_labels = c('Vacant Proportion',
                'Commercial Proportion',
                'Mixed Proportion')
names(land_labels) = c('vacant_prop', 'comres_prop', 'mixed_prop')

crime_labels = c('Violent Crime', 'Non-violent Crime')
names(crime_labels) = c('violent', 'nonviolent')

for(time_var in names(time_name_vec)){
    
    combine_df <- CombineTMats(area_res[[time_var]])

    combine_df %<>% mutate(plot_text =
                               paste0(round(100 * means, 2), '% \n(', counts, ')'))
    combine_df$plot_text[is.na(combine_df$means)] = ''

    p_t <- ggplot(data = combine_df,
                  aes(x = crime_type, y = bus_type, fill = p_col)) +
        geom_tile() + scale_fill_identity(guide = 'none') + 
        labs(x = '', y = '') + 
        geom_vline(xintercept = 1.5, colour = 'grey', alpha = 0.5)
    
    p_t_text = p_t + geom_text(aes(label = plot_text),
                               colour = combine_df$text_col) +
        ggtitle(window_titles[time_var])

    p_t_text = p_t_text + theme(axis.text.x = element_text(angle = 90, hjust = 1, colour = 'black'),
                                axis.text.y = element_text(angle = 45, vjust = 0.9, hjust=0.9),
                                plot.title = element_text(face = "bold")) +
        scale_y_discrete(limits = rev(names(land_labels)),
                         labels = rev(land_labels)) +
        scale_x_discrete(limits = names(crime_labels),                         
                         labels = crime_labels)

    plot_list[[time_var]] = p_t_text
}

main_plot <- plot_grid(plot_list[[1]], plot_list[[2]], plot_list[[3]], nrow = 1)

## save_plot(filename = paste0(plot_folder, 'plots_matching/fig_matching_landuse.pdf'),
##           plot = main_plot, base_height = 8, base_aspect_ratio = 1.8)

leg_prop_height = 0.1

empty_box <- ggplot()

## these l-r adjustments are particular to this exact plot:
leg_end_adj <- plot_grid(empty_box, leg_end, empty_box,
                         nrow = 1, rel_widths = c(0.15, 1, 0.015))

main_with_legend <- plot_grid(main_plot, leg_end_adj,
                              nrow = 2, rel_heights = c(1, leg_prop_height))

save_plot(filename = paste0(plot_folder, 'plots_matching/fig_matching_landuse.pdf'),
          plot = main_with_legend,
          base_height = 8 * (1 + leg_prop_height),
          base_aspect_ratio = 1.8 / (1 + leg_prop_height))
