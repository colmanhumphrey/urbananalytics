#' find which are to be deleted in one frame in ref. to another
#' 
#' @param bus_ref
#' the name of the business frame to compare against, higher in the food chain
#'
#' @param bus_remove
#' this is the one we'll be deleting from
#'
#' @param dup_list
#' the list of lists (of lists!), containing potentail duplicates
#'
#' @param frame_list
#' the list of bus frames
#'
#' @param ref_keep
#' the indices you'll be keeping from the ref. frame
#' only these can force a deletion
#'
#' @param cut_off
#' the value under which a duplication is "real" 
RemoveDupAcross <- function(bus_ref,
                            bus_remove,
                            dup_list,
                            frame_list,
                            ref_keep,
                            cut_off){
    dup_ind_list <- dup_list[[bus_ref]][[bus_remove]]$close_inds
    dup_dist_list <- dup_list[[bus_ref]][[bus_remove]]$close_dists

    ## easier than self dup: drop any with sufficiently close matches
    CloseKeepDup <- function(ind_vec, dist_vec){
        temp_ind = ind_vec %in% ref_keep
        if(sum(temp_ind) > 0){
            return(min(dist_vec[temp_ind]))
        } else {
            return(Inf)
        }
    }

    min_dists <- mapply(CloseKeepDup, dup_ind_list, dup_dist_list)
    del_ind <- min_dists < cut_off

    return(del_ind)
}

