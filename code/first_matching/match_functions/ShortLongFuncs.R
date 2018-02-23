#' gets quantiles of hours.
#' we want hours such that we can do <= and >= for short and long
#' when they're different, this is fine
#' when they're the same: if min or max, easy: short is <= 0, long is > 0, sim for max
#' when they're not min or max but the same:
ShortLongQuants <- function(df, quantile_vec = c(0.25, 0.75), type = 6, hour_max){

    hours = df %>% pull(hours)

    hours_quant = quantile(hours, probs = quantile_vec, type = type)

    if(length(unique(hours_quant)) < 2){
        if(hours_quant[1] == 0){
            ## both are zero: short can't get shorter, long must be longer
            hours_quant[2] = min(hours[hours > 0]) ## this will give -Inf when all zero
            
        } else if(hours_quant[1] == hour_max[time_name]){
            ## both are max: short less, long can't change
            hours_quant[1] = max(hours[hours < hour_max[time_name]]) ## sim as above, Inf
            
        } else {
            ## problem case: many businesses on same hour situation
            ## attempt:
            joint_val = hours_quant[1]
            hours_quant[1] = max(hours[hours < joint_val])
            hours_quant[2] = min(hours[hours > joint_val])
            num_valid <- NumValidHours(hours, hours_quant)

            ## if this gives at least 30%, you're good
            if(sum(num_valid) < (length(hours) * 0.3)){
                hours_quant = 'SET MANUALLY'
            }
        }
    }        
    temp_valid = NumValidHours(hours, hours_quant)
    return(list(quant_hours = hours_quant,
                valid_count = c(temp_valid, temp_valid / length(hours)),
                short_ind = hours <= hours_quant[1],
                long_ind = hours >= hours_quant[2]))
}

#' simple functions: returns counts that will be valid in later analysis
NumValidHours <- function(hours, hours_quant){
    c(short = sum(hours <= hours_quant[1]),
      long = sum(hours >= hours_quant[2]))
}



##------------------------------------

#' takes in a frame with already computed short long indexing
#' and bus_id etc etc
ShortLongPairs <- function(df){
    temp_summary <- df %>%
        group_by(blockgroup_id) %>%
        summarise(short_count = sum(short),
                  long_count = sum(long)) %>%
        ungroup() %>%
        mutate(min_count = pmin(short_count, long_count)) %>%
        filter(min_count > 0)

    use_blockgroup_id <- temp_summary$blockgroup_id

    shortlong_index <- matrix(NA,
                              nrow = length(use_blockgroup_id),
                              ncol = 2)                                  

    it = 1
    for(bg_id in use_blockgroup_id){
        temp_frame_short <- df %>%
            filter(blockgroup_id == bg_id & short)
        temp_frame_long <- df %>%
            filter(blockgroup_id == bg_id & long)

        dist_matrix <- matrix(NA,
                              nrow = nrow(temp_frame_short),
                              ncol = nrow(temp_frame_long))

        for(k in 1:nrow(temp_frame_long)){
            dist_matrix[,k] <- distGeo(p1 = cbind(temp_frame_short$lng, temp_frame_short$lat),
                                       p2 = c(temp_frame_long$lng[k], temp_frame_long$lat[k]))
        }

        ## pick smallest that are 140m apart, if any
        if(max(dist_matrix) >= 140){
            dist_matrix[dist_matrix < 140] = 2 * max(dist_matrix)
            bus_ind = which(dist_matrix == min(dist_matrix), arr.ind = TRUE)

            if(nrow(bus_ind) > 1){
                ## choose random, generally it's stores on top of one another etc
                bus_ind = bus_ind[sample(nrow(bus_ind), 1),,drop = FALSE]
            }

            ## unwrap this
            bus_unwrap = c(temp_frame_short$bus_id[bus_ind[1,1]],
                           temp_frame_long$bus_id[bus_ind[1,2]])

            shortlong_index[it,] = bus_unwrap
        }

        it = it + 1
    }

    shortlong_index <- shortlong_index[!is.na(shortlong_index[,1]),,drop = FALSE]
    colnames(shortlong_index) = c('short', 'long')

    return(shortlong_index)
}
    
TestSLPairs <- function(open_hours_bus,
                        pairs_mat,
                        timevar = 'total_open',
                        bus_cat,
                        hours_quant){
    open_short <- open_hours_bus[match(pairs_mat[,1], open_hours_bus$bus_id),]
    open_long <- open_hours_bus[match(pairs_mat[,2], open_hours_bus$bus_id),]

    dim_res = identical(dim(open_short), dim(open_long))

    if(!dim_res){
        stop('Dimensions not equal')
    }
    
    bus_unique = length(unique(c(open_short$bus_id,
                                 open_long$bus_id))) == 2 * nrow(pairs_mat)

    if(!bus_unique){
        stop('Businesses not unique')
    }

    blockgroup_id_equal = identical(open_short$blockgroup_id,
                                    open_long$blockgroup_id)

    if(!blockgroup_id_equal){
        stop('Businesses not in same blockgroup')
    }
    
    bus_cat_equal = identical(open_short %>% pull(bus_cat),
                              open_long %>% pull(bus_cat))

    if(!bus_cat_equal){
        stop(paste0('Businesses not all ', bus_cat))
    }

    dist_cat_140 = distGeo(p1 = cbind(open_short$lng, open_short$lat),
                           p2 = cbind(open_long$lng, open_long$lat))

    if(min(dist_cat_140) < 140){
        stop('Not all 140m apart')
    }

    hours_diff = (open_long %>% pull(timevar)) - (open_short %>% pull(timevar))

    if(min(hours_diff) <= 0){
        stop('Not all with different hours, or one with short having more than long')
    }

    return(list(dists = dist_cat_140,
                hours = hours_diff))
}
    
##------------------------------------

#' compute counts of crimes around points
GenCrimeCounts <- function(pair_mat,
                           open_hour_bus,
                           crime,
                           print_i = 0){
    open_short <- open_hours_bus[match(pair_mat[,1], open_hours_bus$bus_id),] %>%
        select(lng, lat)
    
    open_long <- open_hours_bus[match(pair_mat[,2], open_hours_bus$bus_id),] %>%
        select(lng, lat)

    crime_ll <- crime %>% select(long, lat, crime_cat)
    crime_rel <- crime_ll %>% filter(crime_cat %in% c('violent', 'nonviolent'))
    crime_mat = cbind(crime_rel$long, crime_rel$lat)

    crime_vioind <- crime_rel$crime_cat == 'violent'
    crime_nonvioind <- crime_rel$crime_cat == 'nonviolent'
    
    short_counts = matrix(NA,
                          nrow = nrow(open_short),
                          ncol = 3)
    long_counts = short_counts
    
    for(i in 1:nrow(open_short)){
        if(print_i > 0){
            if(i %% print_i == 0){
                print(paste0('iter ', i, ' from ', nrow(open_short)))
            }
        }
        short_point = c(open_short$lng[i],
                        open_short$lat[i])
        long_point = c(open_long$lng[i],
                       open_long$lat[i])

        worst_vec = CompLLWorst(short_point, dist = 75)
        
        pre_ind <- abs(short_point[1] - crime_mat[,1]) < worst_vec[1] &
            abs(short_point[2] - crime_mat[,2]) < worst_vec[2]
        if(sum(pre_ind) > 0){
            short_ind <- distGeo(p1 = short_point,
                                 p2 = crime_mat[pre_ind,]) < 70

            short_crime_ind <- which(pre_ind)[short_ind]            
        } else {
            short_crime_ind <- numeric(0)
        }

        worst_vec = CompLLWorst(long_point, dist = 75)
        
        pre_ind <- abs(long_point[1] - crime_mat[,1]) < worst_vec[1] &
            abs(long_point[2] - crime_mat[,2]) < worst_vec[2]
        if(sum(pre_ind) > 0){
            long_ind <- distGeo(p1 = long_point,
                                 p2 = crime_mat[pre_ind,]) < 70

            long_crime_ind <- which(pre_ind)[long_ind]            
        } else {
            long_crime_ind <- numeric(0)
        }

        short_counts[i,1:2] = c(sum(crime_vioind[short_crime_ind]),
                                sum(crime_nonvioind[short_crime_ind]))
        long_counts[i,1:2] = c(sum(crime_vioind[long_crime_ind]),
                               sum(crime_nonvioind[long_crime_ind]))
    }

    short_counts[,3] = short_counts[,1] + short_counts[,2]
    long_counts[,3] = long_counts[,1] + long_counts[,2]

    colnames(short_counts) = c('vio', 'nonvio', 'all')
    colnames(long_counts) = c('vio', 'nonvio', 'all')

    diff_counts = short_counts - long_counts

    return(list(
        short = short_counts,
        long = long_counts,
        diff = diff_counts))
}

#' computes LL vals to be within dist
#' i.e. give a point, returns closest
#' another point can be in terms of long lat
#' 
CompLLWorst <- function(point, dist){   
    abs_diff = 1
    left_pull = 0.1
    iters = 1
    while(abs_diff > 0.0001 & iters < 10){
        left_point <- point - c(left_pull, 0)
        dist_away = distGeo(left_point, point)
        abs_diff = abs((dist_away - dist)) / dist
        ## print(left_pull)        
        left_pull = left_pull * (dist / dist_away)        
        iters = iters + 1
    }

    if(iters == 10){
        stop('took too many iterations for left')
    }

    abs_diff = 1
    right_pull = 0.1
    iters = 1
    while(abs_diff > 0.0001 & iters < 10){
        right_point <- point + c(right_pull, 0)
        dist_away = distGeo(right_point, point)
        abs_diff = abs((dist_away - dist)) / dist
        ## print(right_pull)        
        right_pull = right_pull * (dist / dist_away)
        iters = iters + 1
    }

    if(iters == 10){
        stop('took too many iterations for right')
    }

    abs_diff = 1
    bottom_pull = 0.1
    iters = 1
    while(abs_diff > 0.0001 & iters < 10){
        bottom_point <- point - c(0, bottom_pull)
        dist_away = distGeo(bottom_point, point)
        abs_diff = abs((dist_away - dist)) / dist
        ## print(bottom_pull)        
        bottom_pull = bottom_pull * (dist / dist_away)
        iters = iters + 1
    }

    if(iters == 10){
        stop('took too many iterations for bottom')
    }

    abs_diff = 1
    top_pull = 0.1
    iters = 1
    while(abs_diff > 0.0001 & iters < 10){
        top_point <- point + c(0, top_pull)
        dist_away = distGeo(top_point, point)
        abs_diff = abs((dist_away - dist)) / dist
        ## print(top_pull)        
        top_pull = top_pull * (dist / dist_away)
        iters = iters + 1
    }

    if(iters == 10){
        stop('took too many iterations for top')
    }


    if(abs(left_pull / right_pull - 1) > 0.01){
        stop('left and right not close')
    }
    if(abs(bottom_pull / top_pull - 1) > 0.01){
        stop('top and bottom not close')
    }
    
    return(c(max(left_pull, right_pull),
             max(bottom_pull, top_pull)))
}
           
