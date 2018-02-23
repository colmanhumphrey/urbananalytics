dot_back = '../../'
data_folder = paste0(dot_back, '../data/')
code_folder = dot_back
plot_folder = paste0(dot_back, '../plots/')

source(paste0(code_folder, 'load_packages.R'))

source(paste0(code_folder, 'shape_functions/PointsInShape.R'))
source(paste0(code_folder, 'shape_functions/ResolveMultiPoint.R'))
source(paste0(code_folder, 'shape_functions/CreateCircles.R'))

library(RColorBrewer)
source(paste0(code_folder, 'plots/plot_functions/AddAlpha.R'))
source(paste0(code_folder, 'plots/plot_functions/GenXYRLM.R'))
source(paste0(code_folder, 'plots/plot_functions/GenGeomGrob.R'))

load(paste0(data_folder, 'block_and_group.rdata'))
load(paste0(data_folder, 'business_frame.rdata'))
load(paste0(data_folder, 'crime.rdata'))

library(geosphere)

##------------------------------------##

## just pick any hour
open_hours_bus <- business_frame %>%
    filter(!is.na(tue_12)) %>%
    select(-c(mon_0:sun_23))

open_hours_bus %<>% mutate(blockgroup_id = paste(blockgroup, censustract, sep = '_'),
                           bus_id = 1:nrow(open_hours_bus))

bus_types <- open_hours_bus %>% select(cafe:retail) %>% names()
names(bus_types) = bus_types

## get quantiles
## first, separate frames
bus_cat_dfs_total <- map(bus_types, function(bt){
    open_hours_bus[pull(open_hours_bus[,bt]),] %>%
        select(-c(cafe:retail, weekday_evening_open, weekend_night_open)) %>%
        rename(hours = total_open)})

bus_cat_dfs_weekday <- map(bus_types, function(bt){
    open_hours_bus[pull(open_hours_bus[,bt]),] %>%
        select(-c(cafe:retail, total_open, weekend_night_open)) %>%
        rename(hours = weekday_evening_open)})

bus_cat_dfs_weekend <- map(bus_types, function(bt){
    open_hours_bus[pull(open_hours_bus[,bt]),] %>%
        select(-c(cafe:retail, total_open, weekday_evening_open)) %>%
        rename(hours = weekend_night_open)})

bus_cat_ll <- list('total' = bus_cat_dfs_total,
                   'weekday' = bus_cat_dfs_weekday,
                   'weekend' = bus_cat_dfs_weekend)

source(paste0(code_folder, 'first_matching/match_functions/ShortLongFuncs.R'))

hour_max_vec <- c('total' = 24 * 7,
                  'weekday' = 5 * 6,
                  'weekend' = 2 * 4)

time_name_vec <- c('total' = 'total_open',
                   'weekday' = 'weekday_evening_open',
                   'weekend' = 'weekend_night_open')

## add short long index
## calc pairs
## check res
shortlong_pairs_list <- list()
test_res <- list()
for(time_name in names(bus_cat_ll)){
    print(time_name)
    
    shortlong_pairs_list[[time_name]] <- list()
    test_res[[time_name]] <- list()
    
    for(bt in bus_types){
        print(bt)
        
        temphour = ShortLongQuants(bus_cat_ll[[time_name]][[bt]],
                                   hour_max = hour_max_vec[time_name])
        
        bus_cat_ll[[time_name]][[bt]] %<>% mutate(short = temphour$short_ind,
                                                  long = temphour$long_ind)

        shortlong_pairs_list[[time_name]][[bt]] = ShortLongPairs(bus_cat_ll[[time_name]][[bt]])

        test_res[[time_name]][[bt]] = TestSLPairs(open_hours_bus,
                                                  shortlong_pairs_list[[time_name]][[bt]],
                                                  time_name_vec[time_name],
                                                  bus_cat = bt,
                                                  hours_quant = temphour$quant_hours)
    }
}

## check manually above if you want
## get crime diffs etc
crime_short <- list()
crime_long <- list()
crime_diff <- list()

for(time_name in names(bus_cat_ll)){
    print(time_name)

    crime_short[[time_name]] <- list()
    crime_long[[time_name]] <- list()
    crime_diff[[time_name]] <- list()
    
    for(bt in bus_types){
        print(bt)

        crime_temp = GenCrimeCounts(pair_mat = shortlong_pairs_list[[time_name]][[bt]],
                                    open_hour_bus = open_hour_bus,
                                    crime = crime,
                                    print_i = 20)

        crime_short[[time_name]][[bt]] = crime_temp$short
        crime_long[[time_name]][[bt]] = crime_temp$long
        crime_diff[[time_name]][[bt]] = crime_temp$diff
    }
}

save(crime_short, crime_long, crime_diff, file = paste0(data_folder, 'match_result/crime_counts.rdata'))

##------------------------------------
## do a couple tests:
time_var = 'total'
bus_var = 'cafe'

j_val = 20

open_hours_bus[shortlong_pairs_list[[time_var]][[bus_var]][j_val,],]

short_dist = distGeo(p1 = open_hours_bus[shortlong_pairs_list[[time_var]][[bus_var]][j_val,'short'],] %>%
                         select(lng, lat),
                    p2 = crime %>% select(long, lat)) < 70
short_crime_count <- table(crime$crime_cat[short_dist])

long_dist = distGeo(p1 = open_hours_bus[shortlong_pairs_list[[time_var]][[bus_var]][j_val,'long'],] %>%
                        select(lng, lat),
                    p2 = crime %>% select(long, lat)) < 70
long_crime_count <- table(crime$crime_cat[long_dist])

short_crime_count
crime_short[[time_var]][[bus_var]][j_val,]

long_crime_count
crime_long[[time_var]][[bus_var]][j_val,]

crime_diff[[time_var]][[bus_var]][j_val,]
##------------------------------------

