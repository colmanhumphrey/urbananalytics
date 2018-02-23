#' given a matrix of "differences" (with potentially NAs)
#' returns t results
GenTTest <- function(rel_mat,
                     t_ret = FALSE){
    t_tests = apply(rel_mat, 2, function(x){
        if(sum(!is.na(x)) > 1){
            return(t.test(x))
        } else {
            return(list(statistic = NA, estimate = NA, p.value = NA))
        }})

    ests <- unlist(lapply(t_tests, '[[', 'estimate'))
    p_val <- unlist(lapply(t_tests, '[[', 'p.value'))
    sign_p = p_val * sign(ests)

    if(t_ret){
        t_val = unlist(lapply(t_tests, '[[', 'statistic'))
        
        res_mat <- cbind('counts' = colSums(!is.na(rel_mat)),
                         'means' = ests,
                         'signed_p' = sign_p,
                         't_vals' = t_val)
        rownames(res_mat) = names(t_tests)
    } else {
        wilcox_res = apply(rel_mat,
                           2, function(x){
                               if(sum(!is.na(x)) > 1){
                                   return(wilcox.test(x))
                               } else {
                                   return(list(estimate = NA, p.value = NA))
                               }})

        wilcox_sign_p = unlist(lapply(wilcox_res, '[[', 'p.value')) * sign(ests)

        res_mat <- cbind('counts' = colSums(!is.na(rel_mat)),
                         'means' = ests,
                         'signed_p' = sign_p,
                         'wilcox_sign_p' = wilcox_sign_p)
        rownames(res_mat) = names(t_tests)
    }

    return(res_mat)
}

