CombineTMats <- function(tmat_list, t_ret = FALSE){
    if(t_ret){
        long_df <- as_data_frame(do.call(rbind, tmat_list)) %>% select(counts, means, signed_p, t_vals)
    } else {
        long_df <- as_data_frame(do.call(rbind, tmat_list)) %>% select(counts, means, signed_p)
    }
    
    long_df$crime_type = rep(names(tmat_list), times = unlist(lapply(tmat_list, nrow)))
    long_df$bus_type = unlist(lapply(tmat_list, rownames))
    
    pval_bonf = 0.05 / sum(!is.na(long_df$means))
        
    long_df %<>%
        mutate(p_col = as.character(cut(signed_p, breaks = c(-Inf, -pval_bonf, 0, pval_bonf, Inf),
                                        labels = c('#FFC1C1', '#E11515', '#1515E1', '#6181FF'))))

    long_df %<>%
        mutate(text_col = ifelse(abs(signed_p) < pval_bonf, 'white', 'black'))
                           
    zero_t <- long_df$means == 0
    long_df$p_col[zero_t] = '#B0A0B0'
    long_df$text_col[zero_t] = 'black'

    return(long_df)
}
