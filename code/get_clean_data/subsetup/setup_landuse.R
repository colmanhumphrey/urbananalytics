#' due to a lack of consistency...
#' this take a df, delete geom, returns some sort of correct classes
RemoveGeom <- function(df){
    df <- as_tibble(df)
    df$geometry <- NULL
    class(df) <- class(df)[class(df) != 'sf']
    return(df)
}
    
#' takes in two colors, gives the midpoint,
#' with or without the two cols
#'
#' @param c1
#' either the first colour, or both
#'
#' @param c2
#' default NULL, either the second colour, or NULL if c1 contains both
#'
#' @param return_three
#' logical, default TRUE: do you want all three cols, or just the midpoint?
MidCol <- function(c1, c2 = NULL, return_three = TRUE){
    if(is.null(c2)){
        c2 = c1[2]
        c1 = c1[1]
    }
    if(return_three){
        ret_vec = c(1,2,3)
    } else {
        ret_vec = 2
    }
    return(colorRampPalette(c(c1, c2))(3)[ret_vec])
}

## translating codes to English
#' @param vector_C_DIG2
#' numeric vector of use codes, first two digits
LandUseDetail <- function(vector_C_DIG2){
    if(!is.numeric(vector_C_DIG2)){
        vector_C_DIG2 <- as.numeric(vector_C_DIG2)
    }

    ## first digit is a more general code, we'll use that for plotting
    vector_C_DIG1 <- vector_C_DIG2 %/% 10

    usecoding <- matrix(c(
        1, "residential",
        2, "commercial",
        3, "industrial",
        4, "civic_institution",
        5, "transportation",
        6, "culture_recreation",
        7, "park_openspace",
        8, "water",
        9, "vacant_other"), ncol = 2, byrow = TRUE)
    largeusecoding <- matrix(c(
        ## 10, "residential",
        11, "residential_lowdensity",
        12, "residential_mediumdensity",
        13, "residential_highdensity",
        ## 20, "commercial",
        21, "commercial_consumer",
        22, "commercial_business",
        23, "commercial_mixedresidential",
        ## 30, "industrial",
        31, "industrial",
        ## 40, "civic_institution",
        41, "civic_institution",
        ## 50, "transportation",
        51, "transportation",
        ## 60, "culture_recreation",
        61, "culture_amusement",
        62, "activerecreation",
        ## 70, "park_openspace",
        71, "park_openspace",
        72, "cemetery",
        ## 80, "water",
        81, "water",
        ## 90, "vacant_other",
        91, "vacant_land",
        92, "other_unknown"), ncol = 2, byrow = TRUE)
    small_frame <- tibble(dig1 = as.numeric(usecoding[,1]),
                          simpleuse = usecoding[,2])
    large_frame <- tibble(dig2 = as.numeric(largeusecoding[,1]),
                          usedetail = largeusecoding[,2])

    ## old...:
    ## small_frame$col <- c("skyblue1",
    ##                    "hotpink",
    ##                    "red",
    ##                    "orange",
    ##                    "snow2",
    ##                    "yellow",
    ##                    "green3",
    ##                    "royalblue",
    ##                    "black")

    large_frame$col <- c(## res, yellow to orange (low to high)
        MidCol('#ffff99', '#FFBF4C'),
        ## commercial, consumer/bus/mixed,
        '#e9181a', ## consumer, red
        '#B31517', ## business, dark red / brown
        '#FF3EA3', ## mixed: pinkish
        ## industrial, purple
        '#9C1b9f',
        ## civic, ~ us flag blue, bit lighter
        ## '#133ED1',
        '#0015c5',
        ## transportation (grey, diff to industrial)
        '#d9d9d9',
        ## culture rec: 
        '#00FA9A', ## amusement, mediumspringgreen lol
        '#32cd32', ## active recreation,
        ## park / openspace:
        '#3a9249', ## parks and openspace, green!
        '#8c510a', ## cemetery,
        ## water, blue (not same as civic), lightish
        '#0097FF',
        ## vacant, other
        '#1a1a1a', ## vacant, black (almost)
        '#696969') ## other/unknown, darkish grey                                                  

    LU_temp <- tibble(dig1 = vector_C_DIG1,
                      dig2 = vector_C_DIG2)
    LU_temp <- LU_temp %>%
        left_join(small_frame, by = 'dig1')
    LU_temp <- LU_temp %>%
        left_join(large_frame, by = 'dig2')

    return(LU_temp)
}

#' function to get the areas of the first object in the second,
#' with the assumption that the column is called "use_detail"
GetAreas <- function(geom_areas, geom_agg, add_counts = FALSE){
    if(!('row_index' %in% names(geom_agg))){
        geom_agg$row_index = 1:nrow(geom_agg)
    }

    message('Getting within inds, give it a second')
    within_inds <- st_within(geom_areas$geometry, geom_agg$geometry)

    message('creating sub datasets')
    geom_areas$len_inds <- lengths(within_inds)
    in_vals <- unlist(within_inds)

    geom_within <- as_tibble(geom_areas %>% filter(len_inds == 1))
    geom_within <- RemoveGeom(geom_within)
    
    geom_not <- geom_areas %>% filter(len_inds == 0)

    geom_within$agg_index = in_vals

    ##------------------------------------ contained lots
    message('aggregating over contained lots, area')
    geom_agg_w <- AggVar(geom_within, geom_agg, add_name = 'area_')
    
    if(add_counts){
        message('aggregating over contained lots, counts')        
        geom_agg_w <- AggVar(geom_within, geom_agg, area = FALSE, add_name = 'counts_')
    }

    ##------------------------------------ not contained lots
    message('generating intersections')
    all_inters <- st_intersection(geom_agg %>% select(row_index, geometry),
                                  geom_not %>% select(use_detail, geometry))

    ## checking these results gives not 100% perfect intersections, but good enough
    all_inters <- all_inters %>%
        rename(agg_index = row_index) %>%
        mutate(area = st_area(geometry))
    all_inters <- RemoveGeom(all_inters)
    
    geom_agg_i <- AggVar(all_inters, geom_agg, add_name = 'area_')

    if(add_counts){
        message('aggregating over contained lots, counts')        
        geom_agg_i <- AggVar(all_inters, geom_agg, area = FALSE, add_name = 'counts_')
    }

    area_cols = grep('area_', names(geom_agg_w))
    area_frame = as_tibble(RemoveGeom(geom_agg_w)[,area_cols] + RemoveGeom(geom_agg_i)[,area_cols])
    if(add_counts){
        counts_cols = grep('counts_', names(geom_agg_w))
        counts_frame = RemoveGeom(geom_agg_w)[,counts_cols] + RemoveGeom(geom_agg_i)[,counts_cols]

        return(list(area = area_frame,
                    counts = counts_frame))
    }

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
