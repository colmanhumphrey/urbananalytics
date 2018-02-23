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
## load(paste0(data_folder, 'business_frame.rdata'))
## load(paste0(data_folder, 'crime.rdata'))

load(file = paste0(data_folder, 'match_result/crime_counts.rdata'))

require(cowplot)

##------------------------------------##

## compute t statistics and means
res_list = crime_diff

for(time_var in names(crime_diff)){
    print(time_var)
    
    for(bus_type in names(crime_diff[[time_var]])){
        print(bus_type)

        if(nrow(crime_diff[[time_var]][[bus_type]]) > 1){

            t_temp = apply(crime_diff[[time_var]][[bus_type]],
                           2, t.test)

            rel_diff = crime_diff[[time_var]][[bus_type]] /
                (crime_short[[time_var]][[bus_type]] + crime_long[[time_var]][[bus_type]])

            rel_diff <- rel_diff[!is.na(rel_diff[,1]),]

            t_temp_rel <- apply(rel_diff, 2, t.test)

            ests <- unlist(lapply(t_temp, '[[', 'estimate'))
            p_val <- unlist(lapply(t_temp, '[[', 'p.value'))
            sign_p = p_val * sign(ests)

            rel_ests = unlist(lapply(t_temp_rel, '[[', 'estimate'))
            rel_sign_p = unlist(lapply(t_temp_rel, '[[', 'p.value')) * sign(rel_ests)

            wilcox_res = apply(crime_diff[[time_var]][[bus_type]],
                               2, wilcox.test)

            wilcox_sign_p = unlist(lapply(wilcox_res, '[[', 'p.value')) * sign(ests)

            res_mat <- cbind('counts' = rep(nrow(crime_diff[[time_var]][[bus_type]]), 3),
                             'means' = ests,
                             'mean_rel' = rel_ests,
                             'signed_p' = sign_p,
                             'rel_sign_p' = rel_sign_p,
                             'wilcox_sign_p' = wilcox_sign_p)
            rownames(res_mat) = names(t_temp)
        } else {
            res_mat = matrix(NA, 3, 6)
            colnames(res_mat) = c('counts', 'means', 'mean_rel', 'signed_p', 'mean_sign_p', 'wilcox_sign_p')
            rownames(res_mat) = c('vio', 'nonvio', 'all')

            res_mat[,'counts'] = rep(nrow(crime_diff[[time_var]][[bus_type]]), 3)
        }

        res_list[[time_var]][[bus_type]] = res_mat
    }
}

## generating plots
window_titles = c('Whole Week', 'Weekday Evenings (6pm - 12pm)', 'Weekend Nights (12pm - 4am)')
names(window_titles) = names(res_list)

plot_list <- list()

for(time_var in names(res_list)){
    ## create df from what's inside
    long_df <- as_data_frame(do.call(rbind, res_list[[time_var]])) %>% select(counts, means, signed_p)
    long_df$bus_type = rep(names(res_list[[time_var]]), each = 3)
    long_df$crime_type = rep(c('vio', 'nonvio', 'all'), times = length(res_list[[time_var]]))

    pval_bonf = 0.05 / sum(!is.na(long_df$means))
        
    long_df %<>%
        mutate(p_col = as.character(cut(signed_p, breaks = c(-Inf, -pval_bonf, 0, pval_bonf, Inf),
                                        labels = c('#FFC1C1', '#E11515', '#1515E1', '#6181FF'))))

    long_df %<>%
        mutate(text_col = ifelse(abs(signed_p) < pval_bonf, 'white', 'black'))
                           
    zero_t <- long_df$means == 0
    long_df$p_col[zero_t] = '#B0A0B0'
    long_df$text_col[zero_t] = 'black'

    p_t <- ggplot(data = long_df,
                  aes(x = crime_type, y = bus_type, fill = p_col)) +
        geom_tile() + scale_fill_identity(guide = 'none') + 
        labs(x = '', y = 'Business Type') +
        geom_vline(xintercept = 1.5, colour = 'grey', alpha = 0.5) +
        geom_vline(xintercept = 2.5, colour = 'grey', alpha = 0.5)
    p_t_text = p_t + geom_text(aes(label = round(means, 1)),
                               colour = long_df$text_col) +
        ggtitle(window_titles[time_var])

    rel_ind = (1:10) * 3
    
    row_text = paste0(rev(str_to_title(long_df$bus_type[rel_ind])),
                      ' (',
                      rev(long_df$counts[rel_ind]), ')' )
    row_lim = rev(sort(unique(long_df$bus_type)))

    p_t_text = p_t_text + theme(axis.text.x = element_text(angle = 90, hjust = 1, colour = 'black'),
                                axis.text.y = element_text(colour = 'black'),
                                plot.title = element_text(face = "bold")) +
        scale_y_discrete(limits = row_lim,
                         labels = row_text) +
        scale_x_discrete(limits = c('vio', 'nonvio', 'all'),
                         labels = c('Violent', 'Non-violent', 'All Crime'))
    
    plot_list[[time_var]] = p_t_text
}

## creating legend:

leg_data <- data_frame(x_text = c('Negative, Significant',
                                  'Negative, Not Significant',
                                  'Positive, Not Significant',
                                  'Positive, Significant',
                                  'No Difference',
                                  'No Valid Comparison'),
                       p_col = c('#E11515', '#FFC1C1', '#6181FF', '#1515E1',
                                 '#B0A0B0', '#FFFFFF'),
                       text_col = c('white', 'black', 'black', 'white', 'black', 'black'),
                       y = 1)
                                 
leg_end <- ggplot(data = leg_data,
                  aes(x = x_text, y = y, fill = p_col)) + geom_tile(colour = 'black', size = 0.3) +
    scale_fill_identity(guide = 'none') +
    scale_x_discrete(limits = leg_data$x_text) +
    labs(x = '', y = '') + geom_text(aes(label = x_text), colour = leg_data$text_col) +
    theme_void()
    
main_plot <- plot_grid(plot_list[[1]], plot_list[[2]], plot_list[[3]],
                       nrow = 1)

## save_plot(filename = paste0(plot_folder, 'plots_matching/fig_matching_shortlong.pdf'),
##           plot = main_plot, base_height = 8, base_aspect_ratio = 1.8)

leg_prop_height = 0.1

empty_box <- ggplot()

## these l-r adjustments are particular to this exact plot:
leg_end_adj <- plot_grid(empty_box, leg_end, empty_box,
                         nrow = 1, rel_widths = c(0.15, 1, 0.01))

main_with_legend <- plot_grid(main_plot, leg_end_adj,
                              nrow = 2, rel_heights = c(1, leg_prop_height))

save_plot(filename = paste0(plot_folder, 'plots_matching/fig_matching_shortlong.pdf'),
          plot = main_with_legend,
          base_height = 8 * (1 + leg_prop_height),
          base_aspect_ratio = 1.8 / (1 + leg_prop_height))

        

