#' function for adding alpha to colours...
#'
#' @param col
#' vector of colours, in number or name or whatever
#'
#' @param alpha
#' value of alpha in [0,1]
AddAlpha <- function(col, alpha = 1){
    if(length(col) == 0){
        return(col)
    }
    if(missing(col))
        stop("Please provide a vector of colours.")
    col_mat <- sapply(col, col2rgb)
    if(length(alpha) != length(col)){
        if(length(alpha) == 1){
            alpha = rep(alpha, length(col))
        } else {
            if(length(col) == 1){
                col_mat = do.call(cbind, rep(list(col_mat), length(alpha)))
            } else {
                stop('alpha either len 1 or len col')
            }
        }
    }
    col_mat <- rbind(col_mat, alpha)

    return(rgb(col_mat[1,],
               col_mat[2,],
               col_mat[3,],
               alpha = col_mat[4,] * 255,
               maxColorValue = 255))
    ## unlist(lapply(1:length(col), function(i){
    ##     x = col_mat[,i]
    ##     return(rgb(x[1], x[2], x[3], alpha = x[4] * 255, maxColorValue = 255)
}

#' takes in vector, returns colors...
#'
#' @param rel_vec
#' the vector of numbers you want to act as colors (can include NA)
#'
#' @param plot_cols
#' colors to use for rel_vec, leave as NULL to use 
#'
#' @param plot_spectrum
#' brewer.pal (colorbrewer2.org) name, leave as NULL to use PiYG
#'
#' @param alp_val
#' value of alpha to use, can be a vector the length of rel_vec
#'
#' @param n_col
#' how many color classes do you want?
#'
#' @param plot_divs
#' send in divisions manually, leave as NULL to generate
#'
#' @param quantile_col
#' logical, default FALSE; use quantile or just linear segment?
#'
#' @param plot_max
#' logical, default FALSE; if some are larger than plot_max, colors them according to max
#' 
#' @param col_n
#' num of brewer.pal cols to use...
#'
#' @param rev_col
#' logical, default FALSE; use to reverse color direction (useful for brewer.pal...)
#'
#' @param ret_legend
#' logical, default FALSE; use to send divs and cols, to create legend
ColGen <- function(rel_vec,
                   plot_cols = NULL,
                   plot_spectrum = NULL,
                   alp_val = 1,
                   n_col = NULL,
                   plot_divs = NULL,
                   quantile_col = FALSE,
                   plot_max = FALSE,
                   col_n = 9,
                   na_col = NULL,
                   rev_col = FALSE,
                   ret_legend = FALSE){
    if(is.null(plot_divs)){
        if(is.null(n_col)){
            n_col = 20
        }        
        if(quantile_col){
            plot_divs <- quantile(rel_vec, probs = (0:n_col) / n_col, na.rm = TRUE)
            if(length(unique(plot_divs)) < n_col){
                stop("Quantile doesn't produce unique values, set to FALSE or enter manually")
            }
        } else {
            plot_divs <- seq(from = min(rel_vec, na.rm = TRUE),
                             to = max(rel_vec, na.rm = TRUE), length.out = n_col + 1)
        }
    } else {
        if(!is.null(n_col)){
            if(length(plot_divs) != n_col){
                stop("You supplied plot_divs and n_col, but they don't match - length(plot_divs) should be n_col + 1")
            }
        } else {
            n_col = length(plot_divs) - 1
        }
    }

    if(is.null(plot_cols)){
        if(is.null(plot_spectrum)){
            plot_cols <- colorRampPalette(brewer.pal(col_n, 'PiYG'))(n_col)
        } else {
            plot_cols <- colorRampPalette(brewer.pal(col_n, plot_spectrum))(n_col)
        }
    } else {
        if(!is.null(plot_spectrum)){
            stop("Can't have colors and also spectrum")
        }
        plot_cols <- colorRampPalette(plot_cols)(n_col)
    }
    
    set_cols <- AddAlpha(plot_cols, alpha = alp_val)

    if(rev_col){
        set_cols <- set_cols[n_col:1]
    }

    if(ret_legend){
        return(list(divs = plot_divs,
                    cols = set_cols))
    } else {
        ret_col <- as.character(cut(rel_vec,
                                    breaks = plot_divs,
                                    labels = set_cols,
                                    include.lowest = TRUE))
        if(plot_max){
            ret_col[rel_vec > max(plot_divs)] = set_cols[length(set_cols)]
        }
        if(!is.null(na_col)){
            ret_col[is.na(ret_col)] = AddAlpha(na_col, alp_val)
        }
        return(ret_col)
    }
}

#'
#' names:
#' Set1, Set2, Set3, Pastel1, Pastel2, Paired, Accent, Dark2
#' (Set3, Paired go furthest...)
ColGenDiscrete <- function(var,
                           spec_name = 'Set3',
                           reorder_vec = NULL,
                           alp_val = 1,
                           ret_legend = FALSE){
    var = as.character(var)
    uni_var = unique(var)
    rel_cols = brewer.pal(length(uni_var), spec_name)
    if(!is.null(reorder_vec)){
        rel_cols = rel_cols[reorder_vec]
        uni_var = uni_var[reorder_vec] # for legend
    }

    if(ret_legend){
        return(list(levels = uni_var,
                    cols = rel_cols))
    } else{        
        ret_cols = AddAlpha(rel_cols[match(var, uni_var)], alpha = alp_val)
        return(ret_cols)
    }
}

    

#' add a legend... with plotrix...
AddLegend <- function(leg_list,
                      num_div_print = 3,
                      x_center,
                      y_center,
                      x_width,
                      y_width,
                      leg_gradient = 'x'){
    num_divs = length(leg_list$divs)
    len_seq <- seq(from = 1, to = num_divs, length.out = num_div_print)
    color.legend(xl = x_center - x_width / 2,
                 xr = x_center + x_width / 2,
                 yb = y_center - y_width / 2,
                 yt = y_center + y_width / 2,
                 legend = signif(leg_list$divs[len_seq],2),
                 rect.col = leg_list$cols,
                 gradient = leg_gradient)
}


    
#' generate opp. colors...
#'
RevCol <- function(cols){
    rgb_mat = col2rgb(cols)

    new_rgb_mat = 255 - rgb_mat

    new_cols = rgb(red = new_rgb_mat['red',],
                   green = new_rgb_mat['blue',],
                   blue = new_rgb_mat['green',],
                   maxColorValue = 255)
    return(new_cols)
}
