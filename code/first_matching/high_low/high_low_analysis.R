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
load(paste0(data_folder, 'landuse.rdata'))

library(geosphere)
library(KernSmooth)
library(hms)

##------------------------------------##

## for the density

kern_mult_val = 2 ## small for blocks
grid_length = 15 ## metres

County_box = st_bbox(County_geom)

vert_dist1 = distGeo(p1 = County_box[c(1,2)],
                     p2 = County_box[c(1,4)])
## should be very similar:
vert_dist2 = distGeo(p1 = County_box[c(3,2)],
                     p2 = County_box[c(3,4)])

vert_dist = mean(c(vert_dist1, vert_dist2))

horiz_dist1 = distGeo(p1 = County_box[c(1,2)],
                      p2 = County_box[c(3,2)])

## similar:
horiz_dist2 = distGeo(p1 = County_box[c(1,4)],
                      p2 = County_box[c(3,4)])

horiz_dist = mean(c(horiz_dist1, horiz_dist2))

## using grid_length, how many points do we need?
grid_size_vec <- c(ceiling(horiz_dist / grid_length),
                   ceiling(vert_dist / grid_length))

grid_len <- c((County_box[3] - County_box[1]) / grid_size_vec[1],
              (County_box[4] - County_box[2]) / grid_size_vec[2])

crime_mat <- cbind(crime$long, crime$lat)
crime_points <- st_as_sf(crime %>% select(long, lat, crimetype),
                         coords = c('long', 'lat'),
                         crs = st_crs(blockgroup_geom))

## We're going to run the density calculation six times, but
## we first need to compute 
crime_ind= crime$crime_cat == 'violent'

dens_violent = bkde2D(x = crime_mat[crime_ind,],
                      bandwidth = kern_mult_val * grid_len,
                      gridsize = grid_size_vec,
                      range.x = list(x = County_box[c(1,3)],
                                     y = County_box[c(2,4)]))

## long format
xy_grid <- expand.grid(x = dens_violent$x1, y = dens_violent$x2)
dens_df <- data_frame(
    lng = xy_grid$x,
    lat = xy_grid$y,
    crime_dens = c(dens_violent$fhat))

## including one other variable for (1) consistency (2) st_as_sf gives wonky stuff otherwise

dens_points <- st_as_sf(dens_df,
                        coords = c('lng', 'lat'),
                        crs = st_crs(blockgroup_geom))

block_ind <- PointsInShape(block_geom, dens_points)

## calcuating the above: you can re run the density estimate no problem
## and even change the bandwidth
## but if you change the gridsize or range, you'd have to recalculate
## not that it's that bad I guess

hour_ind <- list('total' = TRUE,
                     'weekday' = crime$time >= as.hms('18:00:00') &
                         crime$weekday %in% c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday'),
                     'weekend' = crime$time <= as.hms('04:00:00') &
                         crime$weekday %in% c('Saturday', 'Sunday'))

time_name_vec <- c('total' = 'total_open',
                   'weekday' = 'weekday_evening_open',
                   'weekend' = 'weekend_night_open')

dens_df_proto <- dens_df
dens_df_proto$block <- block_ind

dens_df_list <- list()
dens_mat_list <- list()

for(time_var in names(time_name_vec)){
    print(time_var)
    dens_df_list[[time_var]] = list()
    dens_mat_list[[time_var]] = list()
    
    for(crimecat in c('violent', 'nonviolent')){
        print(crimecat)
        
        crime_ind <- crime$crime_cat == crimecat &
            hour_ind[[time_var]]

        dens_res = bkde2D(x = crime_mat[crime_ind,],
                          bandwidth = kern_mult_val * grid_len,
                          gridsize = grid_size_vec,
                          range.x = list(x = County_box[c(1,3)],
                                         y = County_box[c(2,4)]))

        dens_df = dens_df_proto
        dens_df$crime_dens = c(dens_res$fhat)

        dens_df %<>% filter(!is.na(block))

        dens_df_list[[time_var]][[crimecat]] = dens_df

        dens_mat_list[[time_var]][[crimecat]] = dens_res$fhat
    }
}

## get the high and low points
high_low_frame = dens_df_list
high_low_circles = dens_df_list
high_low_crime = dens_df_list
high_low_bus = dens_df_list

source(paste0(code_folder, 'first_matching/match_functions/HighLowFuncs.R'))

bus_points <- st_as_sf(business_frame %>% select(name, lng, lat),
                       coords = c('lng', 'lat'),
                       crs = st_crs(block_geom))

for(time_var in names(time_name_vec)){
    print(time_var)
    
    for(crimecat in c('violent', 'nonviolent')){
        print(crimecat)

        temp_high_low = GenHighLow(dens_df_list[[time_var]][[crimecat]],
                                   min_high = 2,
                                   min_low = 1,
                                   print_i = 10)

        temp_high_low %<>%
            filter(dens_diff >= 2 &
                   dist >= 100)
        
        high_low_frame[[time_var]][[crimecat]] = temp_high_low

        ## generate circles:
        high_points = st_as_sf(temp_high_low %>% select(block, high_lng, high_lat),
                               coords = c('high_lng', 'high_lat'),
                               crs = st_crs(block_geom))
        low_points = st_as_sf(temp_high_low %>% select(block, low_lng, low_lat),
                               coords = c('low_lng', 'low_lat'),
                               crs = st_crs(block_geom))

        high_circles = CreateCircles(high_points, 50)
        low_circles = CreateCircles(low_points, 50)

        high_low_circles[[time_var]][[crimecat]] = list(high = high_circles,
                                                        low = low_circles)

        ## for checking:
        high_crime = st_contains(high_circles,
                                 crime_points[hour_ind[[time_var]] &
                                              crime$crime_cat == crimecat,])
        low_crime = st_contains(low_circles,
                                crime_points[hour_ind[[time_var]] &
                                             crime$crime_cat == crimecat,])

        high_bus = st_contains(high_circles,
                               bus_points)
        low_bus = st_contains(low_circles,
                              bus_points)

        high_low_crime[[time_var]][[crimecat]] = list(high = high_crime,
                                                      low = low_crime)
        high_low_bus[[time_var]][[crimecat]] = list(high = high_bus,
                                                  low = low_bus)
        
    }
}

save(high_low_frame,
     high_low_circles,
     high_low_crime,
     high_low_bus,
     file = paste0(data_folder, 'match_result/high_low_calcs.rdata'))

## next steps:
## get business counts and excess hours

business_frame$bus_index <- 1:nrow(business_frame)

open_hours_bus <- business_frame %>% filter(!is.na(tue_8))

bus_types <- open_hours_bus %>% select(cafe:retail) %>% names()

con_hours_list <- list()
con_hours_means <- list()
for(bus_type in bus_types){
    open_type = open_hours_bus %>%
        filter_(bus_type) %>%
        select(total_open, weekday_evening_open, weekend_night_open)

    consensus_hours <- open_type %>% summarise_all(median) %>% unlist()
    
    con_hours_list[[bus_type]] = consensus_hours
    con_hours_means[[bus_type]] = open_type %>% summarise_all(mean) %>% unlist()
}
    
con_median_mat = do.call(rbind, con_hours_list)
con_mean_mat = do.call(rbind, con_hours_means)

open_hours_cat <- open_hours_bus %>%
    select(bus_index, cafe:retail, total_open, weekday_evening_open, weekend_night_open)

open_hours_catlist <- lapply(bus_types, function(bus_type){
    open_hours_cat %>% filter_(bus_type) %>% select(-c(cafe:retail))})
names(open_hours_catlist) = bus_types

open_hours_catlist_median <- mapply(function(open_f, hours_vec){
    open_f$total_open = open_f$total_open - hours_vec['total_open']
    open_f$weekday_evening_open = open_f$weekday_evening_open - hours_vec['weekday_evening_open']
    open_f$weekend_night_open = open_f$weekend_night_open - hours_vec['weekend_night_open']
    return(open_f)},
    open_hours_catlist, con_hours_list, SIMPLIFY = FALSE)

open_hours_catlist_mean <- mapply(function(open_f, hours_vec){
    open_f$total_open = open_f$total_open - hours_vec['total_open']
    open_f$weekday_evening_open = open_f$weekday_evening_open - hours_vec['weekday_evening_open']
    open_f$weekend_night_open = open_f$weekend_night_open - hours_vec['weekend_night_open']
    return(open_f)},
    open_hours_catlist, con_hours_means, SIMPLIFY = FALSE)
                                                    
## for the high_lows, get the counts and average excess hours
bus_cat_mat <- as.matrix(business_frame %>% select(cafe:retail))

counts_list = high_low_bus
excess_list = high_low_bus

for(time_var in names(time_name_vec)){
    print(time_var)
    
    for(crimecat in c('violent', 'nonviolent')){
        print(crimecat)

        bus_list = high_low_bus[[time_var]][[crimecat]]

        high_counts = do.call(rbind,
                              lapply(bus_list$high,
                                     function(ind){
                                         colSums(bus_cat_mat[ind,,drop = FALSE])}))
        low_counts = do.call(rbind,
                             lapply(bus_list$low,
                                    function(ind){                                        
                                        colSums(bus_cat_mat[ind,,drop = FALSE])}))

        counts_list[[time_var]][[crimecat]] = list(high = high_counts,
                                                   low = low_counts)

        ## now hours...:
        high_excess_list = lapply(bus_types, function(bus_type){
            print(bus_type)
            temp_frame = open_hours_catlist_median[[bus_type]] %>% select_('bus_index', time_name_vec[time_var])

            excess_vecs = lapply(bus_list$high, function(ind){
                excess_vec = temp_frame %>% filter(bus_index %in% ind) %>%
                    pull(time_name_vec[time_var])
                return(c(mean(excess_vec), length(excess_vec)))})
            excess_mat = do.call(rbind, excess_vecs)
            colnames(excess_mat) = c('mean', 'count')

            return(excess_mat)})
        names(high_excess_list) = bus_types

        high_excess_means = do.call(cbind, lapply(high_excess_list, function(x){x[,1]}))
        high_excess_counts = do.call(cbind, lapply(high_excess_list, function(x){x[,2]}))
                
        low_excess_list = lapply(bus_types, function(bus_type){
            print(bus_type)
            temp_frame = open_hours_catlist_median[[bus_type]] %>% select_('bus_index', time_name_vec[time_var])

            excess_vecs = lapply(bus_list$low, function(ind){
                excess_vec = temp_frame %>% filter(bus_index %in% ind) %>%
                    pull(time_name_vec[time_var])
                return(c(mean(excess_vec), length(excess_vec)))})
            excess_mat = do.call(rbind, excess_vecs)
            colnames(excess_mat) = c('mean', 'count')

            return(excess_mat)})
        names(low_excess_list) = bus_types

        low_excess_means = do.call(cbind, lapply(low_excess_list, function(x){x[,1]}))
        low_excess_counts = do.call(cbind, lapply(low_excess_list, function(x){x[,2]}))
                
        excess_list[[time_var]][[crimecat]] = list(high_means = high_excess_means,
                                                   high_counts = high_excess_counts,
                                                   low_means = low_excess_means,
                                                   low_counts = low_excess_counts)

    }
}

save(counts_list,
     excess_list,
     file = paste0(data_folder, 'match_result/high_low_counts_excess.rdata'))

## finally:
## areas.

areas_list = high_low_bus

for(time_var in names(time_name_vec)){
    print(time_var)
    
    for(crimecat in c('violent', 'nonviolent')){
        print(crimecat)

        high_areas = GetCircleAreas(landuse, high_low_circles[[time_var]][[crimecat]]$high)
        low_areas = GetCircleAreas(landuse, high_low_circles[[time_var]][[crimecat]]$low)

        areas_list[[time_var]][[crimecat]] = list(high = high_areas,
                                                 low = low_areas)
    }
}

save(areas_list,
     file = paste0(data_folder, 'match_result/high_low_areas.rdata'))

##------------------------------------        











