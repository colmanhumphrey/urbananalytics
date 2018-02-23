#' return a vector with the shape index of each point
#' 
#' @param shape_obj
#' This is what you want the indicies to be from,
#' i.e. if points_obj number 58 is contained in shape_obj number 234,
#' then the returned vector should have vec[58] = 234
#'
#' @param points_obj
#' Points to locate within the shape_obj
#'
#' @param resolution
#' character; what to do if points are in more than
#' one shape? generally this can happen if the
#' shape_obj has overlapping shapes
#' default is to just return the whole list as a list obj, let
#' the user deal;
#' although I guess if you want that, just call st_within...
PointsInShape <- function(shape_obj, points_obj, resolution = 'ret_all'){
    in_list <- st_within(points_obj, shape_obj)

    len_list <- lengths(in_list)

    if(max(len_list) > 1){
        message('Some points in more than one shape')
        if(resolution == 'ret_all'){
            message('returning output directly from st_within')
            return(in_list)
        } else if(resolution == 'random'){
            message('randomly choosing a shape to put it in')
            in_list %<>% map_if(len_list > 2, function(x){sample(x, 1)})
        } else if(resolution == 'first'){
            message('choosing first shape as resolution')
            in_list %<>% map_if(len_list > 2, function(x){x[1]})
        } else if(resolution == 'last'){
            message('choosing last shape as resolution')
            in_list %<>% map_if(len_list > 2, function(x){x[length(x)]})
        }
    }

    ## this should ideally give NA if nothing found
    un_list <- map_int(in_list, function(x){x[1]})
    return(un_list)
}
