#' if a point is "in" multiple blocks, this resolves that in one of a
#' number of given ways
#'
#' @param point_obj
#' sf object of points
#'
#' @param shape_obj
#' sf object of shapes
#'
## ' @param res
## ' with conflicts, how to choose?
## ' options:
## ' - stupid Hausdorff distance doesn't work, so that's out
ResolveMultiPoint <- function(point_obj,
                              shape_obj
                              ## res = 'max_in_circle'
                              ){
    within_ind <- PointsInShape(shape_obj, point_obj)
    na_with <- is.na(within_ind)

    temp_dist_e = st_distance(point_obj[na_with,], shape_obj)
    zero_list <- apply(temp_dist_e, 1, function(x){which(as.numeric(x) == 0)})

    circle_50 <- CreateCircles(point_obj[na_with,], 50)

    ## choose one with most area of circle...:
    most_area <- function(bg_ind, circ_ind){
        circ = circle_50[circ_ind,]
        int_obj <- st_intersection(circ, shape_obj[bg_ind,])
        int_area <- st_area(int_obj)
        area_max <- which.max(int_area)
        if(length(area_max) == 1){
            return(bg_ind[area_max])
        } else {
            stop('more than one')
        }
    }

    biggest_ind <- mapply(most_area, zero_list, as.list(1:length(zero_list)))

    return(biggest_ind)
}


## WOW doesn't even work
## this is in the documentation:
## which: character; if equal to ‘Haussdorf’ or ‘Frechet’, Hausdorff
## resp. Frechet distances are returned
## once again.... if equal to HauSSdorF, returns HauSdorFF
## ???
## if(grepl('hausdorff', res, ignore.case = TRUE)){
##     temp_dist_res = st_distance(point_obj[na_with,], shape_obj,
##                                 which = 'Haussdorf')
## } else if(grepl('frechet', res)){
##     temp_dist_res = st_distance(point_obj[na_with,], shape_obj,
##                                 which = 'frechet')
## }
