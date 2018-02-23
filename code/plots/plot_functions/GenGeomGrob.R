#' to create a plain enough grob using sf
#'
#' @param geom_obj
#' object to plot (sf)
#'
#' @param fill_col
#' color to fill the plob
#'
#' @param border_coll
#' if you want another col for the border
GenGeomGrob <- function(geom_obj,
                        fill_col,
                        border_col = NULL,
                        lwd = 1){
    if(is.null(border_col)){
        border_col = fill_col
    }
    dens_grob <- ggplot(geom_obj) + 
        geom_sf(fill = fill_col,
                colour = border_col,
                lwd = lwd) + 
        theme(
            plot.background = element_rect(fill = 'white')
           ,panel.background = element_rect(fill = 'white')
           ,panel.grid.major = element_line(color = 'transparent')
           ,axis.text = element_blank()
           ,axis.ticks = element_blank()
           ,line=element_blank()
        )
    return(dens_grob)
}

