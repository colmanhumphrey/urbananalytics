rlm_eqn <- function(model, cor.r){
    m = model
    a = format(coef(m)[1], digits = 2)
    maybeplus = format(ifelse(sign(coef(m)[2]) > 0, ' +', ' '))
    b = format(coef(m)[2], digits = 2)    
    eq <- substitute(italic(r)~"="~cor.val*","~~italic(t)~"="~tval, 
                     list(#coefab = paste0(a, maybeplus, b),
                         cor.val = format(cor.r, digits = 3),
                         tval = format(summary(m)$coefficients[2,3], digits = 3)))

    return(as.character(as.expression(eq)))
}

XY_pos <- function(xy_data){
    maxx = max(xy_data$x, na.rm = TRUE)
    maxy = max(xy_data$y, na.rm = TRUE)
    minx = min(xy_data$x, na.rm = TRUE)
    miny = min(xy_data$y, na.rm = TRUE)

    xpos = minx + (maxx - minx) * 0.3
    ypos = miny + (maxy - miny) * 0.8

    return(c(xpos = xpos, ypos = ypos))
}    

GenXYRLM <- function(xy_data,
                     xlab,
                     ylab,
                     jitterwidth = 0,
                     alpha = 0.4,
                     title,
                     textsize = NULL,
                     comma_fix = TRUE,
                     title_size = 18){
    p <- ggplot(xy_data,
                aes(x = x, y = y))

    p <- p + geom_point(alpha = alpha,
                        position = position_jitter(width = jitterwidth))

    posvec = XY_pos(xy_data)

    p <- p + geom_smooth(method = rlm, col = "red")

    rlm_reg <- rlm(y ~ x, data = xy_data)
    ## lm_reg <- lm(data[index, y] ~ data[index, x], weights = rlm_reg$w)
    p <- p + annotate("text",
                      x = posvec['xpos'],
                      y = posvec['ypos'],
                      label = rlm_eqn(rlm_reg,
                                      cov.wt(xy_data, wt = rlm_reg$w, cor = TRUE)$cor[1,2]),
                      colour = "black", size = 4.5, parse =TRUE)
    p <- p + labs(x = xlab) + labs(y = ylab)

    p <- p + labs(title = title)
    p <- p + theme(plot.title = element_text(face = "plain"))

    if(!is.null(textsize)){
        p <- p + theme(text = element_text(size = rel(textsize)))
    }

    if(comma_fix){
        p = p + scale_y_continuous(labels = comma) +
            scale_x_continuous(labels = comma)
    }
    
    p = p + theme(plot.title = element_text(size = title_size, 
                                            hjust = 0.5,
                                            face = 'bold'))

    return(p)
}
