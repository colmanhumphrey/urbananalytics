GenHighLow <- function(dens_df_highlow,                       
                       min_high = 0,
                       min_low = 0,
                       print_i = 0){
    temp_high_low <- dens_df_highlow %>%
        group_by(block) %>%
        summarise(high_crime_val = max(crime_dens),
                  high_count = sum(crime_dens == high_crime_val),
                  high_lng = lng[which.max(crime_dens)],
                  high_lat = lat[which.max(crime_dens)],
                  low_crime_val = min(crime_dens),
                  low_count = sum(crime_dens == low_crime_val),
                  low_lng = lng[which.min(crime_dens)],
                  low_lat = lat[which.min(crime_dens)]) %>%
        filter(high_crime_val >= min_high &
               low_crime_val >= min_low)
    
    ## probably will have none if you set min_high and min_low
    multiple_high = temp_high_low$high_count > 1
    multiple_low = temp_high_low$low_count > 1

    any_mult = multiple_high | multiple_low

    i = 1
    for(j in which(any_mult)){
        if(print_i > 0){
            if(i %% print_i == 0){
                print(paste0('Solving iter ', i, ' from ', sum(any_mult)))
            }
            i = i + 1
        }
        rel_block = temp_high_low$block[j]

        all_min <- dens_df_highlow %>%
            filter(block == rel_block &
                   crime_dens == temp_high_low$low_crime_val[j])

        all_max <- dens_df_highlow %>%
            filter(block == rel_block &
                   crime_dens == temp_high_low$high_crime_val[j])

        ## get distances:
        dist_mat <- matrix(NA,
                           nrow = nrow(all_min),
                           ncol = nrow(all_max))

        for(k in 1:nrow(all_max)){
            temp = distGeo(p1 = c(all_max$lng[k], all_max$lat[k]),
                           p2 = cbind(all_min$lng, all_min$lat))

            dist_mat[,k] = temp
        }

        ## choose closest within 140m

        if(max(dist_mat) >= 100){
            dist_mat[dist_mat < 140] = 2 * max(dist_mat)

            min_ind = which(dist_mat == min(dist_mat), arr.ind = TRUE)
            ## if more than one, just randomise, likely not more than one
            if(nrow(min_ind) > 1){
                min_ind = min_ind[sample(nrow(min_ind), 1),,drop = FALSE]
            }

            temp_high_low$low_lng[j] = all_min$lng[min_ind[1]]
            temp_high_low$low_lat[j] = all_min$lat[min_ind[1]]

            temp_high_low$high_lng[j] = all_max$lng[min_ind[2]]
            temp_high_low$high_lat[j] = all_max$lat[min_ind[2]]
        }
    } ## if all too close, will be deleted so who cares

    ## calc distances

    temp_high_low$dist =
        distGeo(p1 = cbind(temp_high_low$high_lng,
                           temp_high_low$high_lat),
                p2 = cbind(temp_high_low$low_lng,
                           temp_high_low$low_lat))

    temp_high_low$dens_diff = temp_high_low$high_crime_val - 
        temp_high_low$low_crime_val
    
    return(temp_high_low)
}
    

## similar to function from setup_landuse...:    
GetCircleAreas <- function(geom_areas,
                           geom_agg){
    if(!('row_index' %in% names(geom_agg))){
        geom_agg$row_index = 1:nrow(geom_agg)
    }

    ## if(!('area_index' %in% names(geom_areas))){
    ##     geom_areas$area_index = 1:nrow(geom_areas)
    ## }

    message('Getting contain inds, give it a second')
    ## contain_inds <- st_contains(geom_agg, geom_areas)

    intersects_inds <- st_intersects(geom_agg, geom_areas)

    all_inters <- st_intersection(geom_agg %>% select(row_index, geometry),
                                  geom_areas[unique(unlist(intersects_inds)),] %>%
                                  select(use_detail, geometry))

    ## checking these results gives not 100% perfect intersections, but good enough
    all_inters %<>%
        rename(agg_index = row_index) %>%
        mutate(area = st_area(geometry))
    all_inters <- RemoveGeom(all_inters)
    
    geom_agg_i <- AggVar(all_inters, geom_agg, add_name = 'area_')

    area_cols = grep('area_', names(geom_agg_i))
    area_frame = as_tibble(RemoveGeom(geom_agg_i)[,area_cols])

    return(area_frame)
}

#' aggregate rows, use_detail over areas or counts
AggVar <- function(geom_var, geom_agg,
                   fill_val = 0, area = TRUE,
                   add_name = 'area_'){
    if(area){
        agg_var <- geom_var %>%
            group_by(agg_index, use_detail) %>%
            summarise(total_area = sum(area))

        agg_spread <- agg_var %>%
            spread(key = use_detail, value = total_area)
        names(agg_spread) = paste0(add_name, names(agg_spread))
    } else {
        agg_var <- geom_var %>%
            group_by(agg_index, use_detail) %>%
            summarise(total_counts = n())

        agg_spread <- agg_var %>%
            spread(key = use_detail, value = total_counts)
        names(agg_spread) = paste0(add_name, names(agg_spread))
    }
    
    geom_agg <- geom_agg %>%
        left_join(agg_spread, by = c('row_index' = 'area_agg_index'))

    ## empty rows should have zero
    new_cols = names(agg_spread)[-1]
    na_list_0 <- as.list(rep(NA, ncol(geom_agg)))
    names(na_list_0) = colnames(geom_agg)
    na_list_0[new_cols] = fill_val
    geom_agg <- geom_agg %>%
        replace_na(na_list_0)

    return(geom_agg)
}

RemoveGeom <- function(df){
    df <- as_tibble(df)
    df$geometry <- NULL
    class(df) <- class(df)[class(df) != 'sf']
    return(df)
}

