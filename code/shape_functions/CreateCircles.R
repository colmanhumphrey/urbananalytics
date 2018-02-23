#' dumb ass st_buffer can't work or whatever, I dunno
#' anyway, project to mercator (or whatever the fake mercator is for gmaps etc)
#' then buffer
#' then project back
#' this isn't enough..., because who knows
#' but essentially we can get one step convergence
#' NOTE: nQuadSegs gives a ceiling to accuracy
#'
#' @param point_obj
#' buncha points
#'
#' @param radius
#' in meters
#'
#' @param nQuadSegs
#' how many points to make circle, per quarter?
#'
#' @param tol
#' how far from desired area can we be, as a proportion
CreateCircles <- function(point_obj,
                          radius,
                          nQuadSegs = 30,
                          tol = 1e-4){
    required_area <- (radius^2) * pi

    prop_diff <- tol * 2
    ## usually radius too small
    cur_radius = radius * 1.3

    iter = 1
    
    while(prop_diff > tol){
        if(iter > 3){
            print(iter)
        }
        try_circles = radius_CreateCircles(point_obj,
                                           cur_radius,
                                           nQuadSegs)
        cur_areas = st_area(try_circles)

        cur_ratio = as.numeric(cur_areas / required_area)

        cur_radius = cur_radius / sqrt(cur_ratio)

        prop_diff = max(abs(1 - cur_ratio))
        if(iter > 3){
            print(prop_diff)
        }
        iter = iter + 1

        if(iter > 10){
            stop('Not converging: likely tol is too small (some issues with transform maybe)') 
        }
    }

    return(try_circles)
}

#' @inheritParams CreateCircles
radius_CreateCircles <- function(point_obj,
                                 radius,
                                 nQuadSegs = 30){
    actual_crs <- st_crs(point_obj)

    ## transform:
    trans_points <- st_transform(point_obj, crs = 3857) ## map mercator

    ## here's the circles:
    trans_buffer <- st_buffer(trans_points, radius, nQuadSegs)

    ## back
    orig_buffer <- st_transform(trans_buffer, crs = actual_crs)
    return(orig_buffer)
}
