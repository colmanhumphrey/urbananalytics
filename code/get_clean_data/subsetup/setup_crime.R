CleanPoint <- function(shape){
    pre_point <- str_sub(shape, 8, nchar(shape) - 1)
    space_point <- str_split(pre_point, ' ')
    longlatmat <- cbind(as.numeric(lapply(space_point, '[[', 1)),
                        as.numeric(lapply(space_point, '[[', 2)))
    return(longlatmat)
}

space_to__ <- function(name){
    gsub(' ', '_', name, fixed = TRUE)
}

slash_to__ <- function(name){
    gsub('/', '_', name, fixed = TRUE)
}

CleanCrime <- function(crime){
    if(sum(is.na(crime$Shape)) > 0){
        message('Removing ', sum(is.na(crime$Shape)), ' points due to NA location')
        crime <- crime %>% filter(!is.na(crime$Shape))
    }

    message('Getting long/lat')
    ll_mat <- CleanPoint(crime$Shape)
    crime$long <- ll_mat[,1]
    crime$lat <- ll_mat[,2]

    crime <- crime %>% rename_all(space_to__) %>% rename_all(slash_to__) %>% rename(ucr = UCR_Code)

    ## kill a few
    crime <- crime %>%
        select(-c(District, PSA, Shape,
                  Police_Districts, Location_Block, DC_Number))

    ## crime type oh yes
    message('relabelling crime types')
    crimetypes <- c('murder', 'rape', 'robbery', 'assault',
                    'burglary', 'theft', 'motortheft',
                    'otherassault', 'arson', 'forgery',
                    'fraud', 'embezzlement', 'receivestolen',
                    'vandalism', 'weaponviolation',
                    'prostitution', 'sexoffense', 'drugviolation',
                    'gambling', 'familyoffense', 'dui', 'liquorlaw',
                    'publicdrunk', 'disorderly', 'vagrancy',
                    'other')

    crimeframe <- data_frame(ucr = sort(unique(crime$ucr)),
                             crimetype = crimetypes)

    crime <- crime %>%
        left_join(crimeframe, by = c('ucr'))

    ## there's a timedate col, but it's in 12h... so I won't use it
    ## because why
    ## temp_date = as.Date(crime$Dispatch_Date)
    ## temp_time = oh it's done...
    message('getting time date')
    crime <- crime %>%
        rename(date = Dispatch_Date,
               time = Dispatch_Time)

    crime <- crime %>%
        mutate(timedate = as.POSIXct(paste0(date, ' ', time), tz = 'EST'),
               weekday = weekdays(timedate))

    ## another delete / rename:
    crime <- crime %>%
        select(-c(Dispatch_Date_Time, Hour)) %>%
        rename(crime_cat = General_Crime_Category)

    return(crime)
}

#' left join by adding zeros
ZeroLJoin <- function(left_table, join_table, by_vec, fill_val = 0){
    left_table %<>%
        left_join(join_table, by = by_vec)

    new_cols = names(join_table)[!names(join_table) == by_vec]
    na_list_0 <- as.list(rep(NA, ncol(left_table)))
    names(na_list_0) = colnames(left_table)
    na_list_0[new_cols] = fill_val

    left_table %<>% replace_na(na_list_0)

    return(left_table)    
}

