#' find self duplicates
#'
#' @param bus
#' the business to find the self dups
#' (lazy programming... but more consistent with RemoveDupAcross!)
#'
#' @param dup_list
#' the list of lists (of lists!), containing potentail duplicates
#'
#' @param frame_list
#' the list of bus frames
#'
#' @param cut_off
#' the value under which a duplication is "real"
RemoveSelfDup <- function(bus,
                          dup_list,
                          frame_list,
                          cut_off){
    dup_ind_list <- dup_list[[bus]][[bus]]$close_inds
    dup_dist_list <- dup_list[[bus]][[bus]]$close_dists

    dup_len = lengths(dup_ind_list)

    is_duplicated_ind <- which(dup_len > 1)
    
    ## CAREFUL don't double-remove

    ## ALSO: first, keep dups with hours when we can!!!
    ## do that by cycling through those with hours first (dups get removed...)

    ## to avoid order mattering... go by longest first. This will remove as many as possible.
    remove_ind <- vector(length = 0)

    dup_has_hours <- !is.na(frame_list[[bus]][is_duplicated_ind, 'tue_12'])

    ## removes (close to...) the most
    dup_ord <- order(!dup_has_hours, - dup_len[is_duplicated_ind])

    is_duplicated_ind_use <- is_duplicated_ind[dup_ord]

    for(i in 1:length(is_duplicated_ind)){
        j = is_duplicated_ind_use[i]
        
        if(!is.na(j)){
            sim_ind = dup_ind_list[[j]] ## get the ones that were close
            other_ind = sim_ind[!(sim_ind %in% j)] ## ignore self
            drop_ind = dup_dist_list[[j]][!(sim_ind %in% j)] < cut_off
            if(sum(drop_ind) > 0){
                                        # record those to be removed
                remove_ind = c(remove_ind, other_ind[drop_ind])
                                        # and don't cycle through them now
                is_duplicated_ind_use[is_duplicated_ind_use %in% other_ind[drop_ind]] = NA
            }
        }
    }

    return(unique(remove_ind))
}
 
