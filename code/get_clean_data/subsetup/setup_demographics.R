##------------------------------------
#' Function to get the base data for your desired county
#' Don't forget to set your census key first, and load
#' library(censusapi)
#' get a key here: https://api.census.gov/data/key_signup.html
#' census_api_key(your_api_key)
#'
#' @param rel_state
#' either the two letter state code ("PA" etc), or
#' the FIPS code for the stat
#'
#' @param rel_county
#' The name or FIPS code of the county of interest
#'
#' @param zero_block
#' are there any blocks to zero out (in terms of pop.)?
#' why? prisons (don't interact with the rest of the city
#' the way we are trying to analyse)
#' Note that the census and ACS disgaree mainly
#' at prisons and college campuses
#' campuses are fine for us, those people
#' interact the way we are measuring.
#' Unfortunately the ACS estimates aren't the best (income etc)
#' but it's generall only a small part of the city
#'
#' @param zero_blockgroup
#' same idea as above, but specifically for blockgroups
#' Note that for census data, we pull the block version,
#' then aggregate, so you don't have to rezero if you already
#' did the blocks
#' (but you do have to for ACS)
#'
#' @param use_tibble
#' do you want R style data frames, or tibbles?
#'
#' @param SF1_vars
#' works fine as NULL (the default);
#' the variables you want to pull from the SF1 table - i.e.
#' the main census result.
#' Default pulls geometries: (block, blockgroup, censustract)
#' And pop. by demos:
#' - Total
#' - Single race, not hispanic:
#'   -- White, Black, American Indian, Asian, Hawaii / Pacific Islander, Other
#' - Two or more races, not hispanic [all combined]
#' - Hispanic or Latino (called Hispanic) [all combinations]
#'
#' @param ACS_vars
#' as null, pull:
#' - median income
#' - poverty totals in all levels
GetDemoDataGeom <- function(rel_state,
                            rel_county,
                            zero_block = NULL,
                            zero_blockgroup = NULL,
                            use_tibble = TRUE,
                            years = c('sf1' = 2010,
                                      'acs' = 2016),
                            SF1_vars = NULL,
                            ACS_vars = NULL){
    ## want all P5 vars
    if(is.null(SF1_vars)){
        ## if you want to see what else can be done, run this code:
        pull_all_vars = FALSE

        if(pull_all_vars){
            ## basically this is just slower than we need, especially since
            ## we know the answer and it should only change every 10 years or whatever
            all_vars <- load_variables('2010', 'sf1')    
            P5_ind <- grep('P005', all_vars$name)
            P5_vars <- all_vars$name[P5_ind]
        } else {
            P5_vars <- paste0('P00', 50000 + 1:17)
        }
        
        ## don't want them all, but do want block, blockgroup...:
        ## the names are a mess, but just change the numbers if there's an issue
        ## or maybe they'll fix the names
        SF1_vars <- c('BLOCK', 'BLKGRP', 'TRACT', P5_vars[c(1, 3, 4, 5, 6, 7, 8, 9, 10)])
        ## not including geometry
        names(SF1_vars) = c('block',
                           'blockgroup',
                           'censustract',
                           'total',
                           'white',
                           'black',
                           'us_indian',
                           'asian',
                           'pacific',
                           'other',
                           'two_or_more',
                           'hispanic')
    }

    message('pulling SF1 data (main census data, every person): counts by race')
    block_data_geom <- get_decennial(state = rel_state,
                                     county = rel_county,
                                     geography = 'block',
                                     variables = SF1_vars,
                                     year = years['sf1'],
                                     output = 'wide',
                                     geometry = TRUE,
                                     keep_geo_vars = FALSE,
                                     cb = FALSE)

    ## don't need GEOID: it just combines
    ## State(FIPS)/County(FIPS)/censustract/block
    block_data_geom$GEOID <- NULL
    ## name is literally paste0('Block', ' ', block_data_geom$block)
    block_data_geom$NAME <- NULL
    ## would want it if we didn't add block to SF1_vars

    ## for sake of... whatever, sort this
    ## note that blockgroup is the first number of block, nice
    block_data_geom <- block_data_geom %>%
        arrange(censustract, block)

    ## nice to separate, I think
    ## geom is a lot bigger than the data, and isn't really necessary
    if(use_tibble){
        block_data = as_tibble(block_data_geom)
        block_data$geometry <- NULL

        class(block_data) <- class(block_data)[!(class(block_data) == 'sf')]        
    } else {
        block_data = as.data.frame(block_data_geom)
        block_data$geometry <- NULL
    }
    block_geom = block_data_geom %>%
        select(block, blockgroup, censustract, geometry)

    ## ## and the non TIGER version
    ## block_simp_geom <- get_decennial(state = rel_state,
    ##                                  county = rel_county,
    ##                                  geography = 'block',
    ##                                  variables = P5_vars[1:3],
    ##                                  year = '2010',
    ##                                  output = 'wide',
    ##                                  geometry = TRUE,
    ##                                  keep_geo_vars = TRUE)
    ## block_simple_geom = block_simp_geom$geometry

    ## NOTE:
    ## it's hard to use all the race splits raw
    ## there are many, and they're somewhat arbitrary, and they change every census
    ## how you combine them depends on everything:
    ## what county are you looking at? (if it e.g. has no white people, don't need that col)
    ## what are you trying to accomplish?
    ## what inferences are you making?
    ## if race is not a primary focus of your inference, e.g. matching
    ## then bias-variance tradeoffs etc
    ## anyway, this made sense for our analyses:
    block_data <- block_data %>%
        mutate(other = other + us_indian + pacific + two_or_more) %>%
        select(-c(us_indian, pacific, two_or_more))

    if(!is.null(zero_block)){
        block_data[zero_block,c('total', 'white', 'black', 'asian', 'other', 'hispanic')] = 0
    }        

    message('Generating block group versions')
    blockgroup_data = BlockGroupify(block_data, funs(sum))
    blockgroup_geom = BlockGroupify(block_geom, funs(st_union))

    if(!is.null(zero_blockgroup)){
        blockgroup_data[zero_blockgroup,c('total', 'white', 'black', 'asian', 'other', 'hispanic')] = 0
    }        

    ## now ACS
    ## you can't get variables the exact same way...
    ## won't accept BLKGRP etc
    ## that's OK

    if(is.null(ACS_vars)){
        ## poverty:
        poverty_vars <- paste0('C17002_00', 1:8, 'E')
        names(poverty_vars) = c('total',
                                'count_5',
                                'count_1',
                                'count_125',
                                'count_15',
                                'count_185',
                                'count_2',
                                'count_inf')
        ## add income
        ACS_vars = c('income' = 'B19301_001E', poverty_vars)
    }

    message('Pulling ACS data: income and poverty')
    ## you should pull 2016!! by leaving it blank
    acs_data_geom <- get_acs(state = rel_state,
                             county = rel_county,
                             geography = 'block group',
                             variables = ACS_vars,
                             year = years['acs'],
                             output = 'wide',
                             geometry = TRUE,
                             keep_geo_vars = TRUE,
                             cb = FALSE)
    ## rename, drop etc...:
    acs_data_geom <- acs_data_geom %>%
        mutate(blockgroup = as.numeric(BLKGRPCE),
               censustract = as.numeric(TRACTCE)) %>%
        select(blockgroup, censustract,
               income,
               total,
               count_5, count_1, count_125, count_15,
               count_185, count_2, count_inf,
               geometry) %>%
        arrange(censustract, blockgroup)

    ## you can now plot acs_geom and see that it's not as good sometimes,
    ## but basically identical
    ## maybe differs on water?

    ## check ordering, potentially fix
    if(identical(order(blockgroup_data$censustract, blockgroup_data$blockgroup),
                 order(acs_data_geom$censustract, acs_data_geom$blockgroup))){
        message('Rearranging blockgroup')
        blockgroup_data <- blockgroup_data %>%
            arrange(censustract, blockgroup)
    }

    ## add income
    blockgroup_data$income = acs_data_geom$income

    ## get poverty metric
    acs_pov <- as_tibble(acs_data_geom %>%
                         select(total,
                                count_5, count_1, count_125, count_15,
                                count_185, count_2, count_inf))
    acs_pov$geometry <- NULL ## that's right, select doesn't remove...
    acs_pov <- acs_pov / acs_pov$total
    acs_pov <- acs_pov %>% select(-total)
    ## you can check rowSums to confirm
    ## pov metric: weighting the cats linearly from 1 to 0
    blockgroup_data$poverty_metric = as.numeric(as.matrix(acs_pov) %*% (c(6:0)/6))

    ## join back to block
    block_data <- block_data %>%
        left_join(blockgroup_data %>% select(blockgroup, censustract, income, poverty_metric),
                  by = c('censustract', 'blockgroup'))

    return(list(block_data = block_data,
                blockgroup_data = blockgroup_data,
                block_geom = block_geom,
                blockgroup_geom = blockgroup_geom))
}
    

#' This function takes counties by blocks, aggregates by blockgroup
#'
#' @param block_obj
#' object with (block), blockgroup, censustract
#'
#' @param agg_fun
#' what to do with the columns?
BlockGroupify <- function(block_obj,
                          agg_fun = funs(sum)){
    blockgroup_obj = block_obj %>%
        group_by(censustract, blockgroup) %>%
        mutate(num_blocks = n()) %>% ## this and the above work together...
        select(-block) %>% 
        group_by(censustract, blockgroup, num_blocks) %>% ## then you have to regroup
        summarise_all(agg_fun) %>% ## now summarise
        select(num_blocks, blockgroup, censustract, everything()) %>% ## put num_blocks first
        ungroup()
    return(blockgroup_obj)
}
