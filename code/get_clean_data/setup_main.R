##------------------------------------
## assuming a relative structure, change if needed

## a bunch of (tidyverse mostly) packages
## note: not all are necessary. change if needed.
## IMPORTANT: if you want to use geom_sf, will use devtools edition of ggplot2
geom_sf_load = FALSE
source('../load_packages.R')

data_folder = '../../data'

## functions are loaded as needed from subsetup,
## EXCEPT:
source('../shape_functions/PointsInShape.R')

##------------------------------------

## first, get relevant census data

## you can download the relevant files if you don't want to use the API
## but I guess you'll have to do some wrangling

rel_state = 'PA'
rel_county = 'Philadelphia'
## if the above doesn't work in your code, check the FIPS codes.
## can get from data(fips_codes)

your_api_key = 'INSERT_YOUR_KEY'
## get a key here: https://api.census.gov/data/key_signup.html
census_api_key(your_api_key)

source('subsetup/setup_demographics.R')

demo_stuff = GetDemoDataGeom(rel_state,
                             rel_county,
                             zero_block = c(18860, 18872), ## prisons
                             years = c('sf1' = 2010, 'acs' = 2015)) ## you should use 2016, or latest

block_data = demo_stuff$block_data
block_geom = demo_stuff$block_geom
blockgroup_data = demo_stuff$blockgroup_data
blockgroup_geom = demo_stuff$blockgroup_geom

## for an example:
## ggplot(block_geom %>% filter(censustract < 300)) + geom_sf(aes(fill = as.character(blockgroup)))
## ggplot(blockgroup_geom %>% filter(censustract < 300)) + geom_sf(aes(fill = as.character(blockgroup)))

## add area:
block_geom <- block_geom %>%
    mutate(area = st_area(geometry))
blockgroup_geom <- blockgroup_geom %>%
    mutate(area = st_area(geometry))
## blockgroup loses its unit?

## create a single county shape, for purposes of containment etc
County_geom <- st_union(blockgroup_geom)

## you can adjust how you save all this data

save_location = paste0(data_folder, '/block_and_group.rdata')
save(block_data,
     blockgroup_data,
     block_geom,
     blockgroup_geom,
     County_geom,
     file = save_location)

##------------------------------------

## sadly that's the end of EZ lyfe,
## now everything is county specific

##------------------------------------

## landuse
## slow times ahead

## For Philly:
## download the latest file from here:
## https://www.opendataphilly.org/dataset/land-use
## note that ours is not "up to date" as it's updated frequently
## email us for our version if an exact "reproduction" (as opposed to a
## replication) is desired

## for everywhere else:
## download your data, ideally it will have a column
## describing the landuse
## we use 2-digit. If you have 1 or 3, edit the following 

## stick your file wherever, put the location (relative) here
landuse_file <- paste0(data_folder, '/raw/landuse/Land_Use.shp')

landuse <- st_read(landuse_file)
landuse$area <- st_area(landuse)

source('subsetup/setup_landuse.R')

## translate the C_DIG2 codes
## if you get any NA values or something undesirable,
## check the function, to see if you have two digit codes that aren't in the function
## if your landuse file doesn't have the same column names as ours, you'll have to 
use_codes_color <- LandUseDetail(landuse$C_DIG2) %>%
    select(-dig1)

## add new info, delete everything that's not really too useful
## left join is slow, and unless something went wrong, the order is already correct
## landuse_data <- landuse_data %>%
##     left_join(use_codes_color, by = c('C_DIG2' = 'dig2'))

landuse$use_simple <- use_codes_color$simpleuse
landuse$use_detail <- use_codes_color$usedetail
landuse$use_color <- use_codes_color$col

## you don't have to do this if already the same
landuse <- landuse %>%
    st_transform(crs = st_crs(block_geom))

landuse <- landuse %>%
    select(-c(C_DIG1, C_DIG1DESC, ## remove cols
              C_DIG2, C_DIG2DESC,
              C_DIG3, C_DIG3DESC,
              VACBLDG)) %>%
    select(use_simple, use_detail, use_color, area, YEAR, geometry) %>%
    rename(year = YEAR)

## transportation is a bit trash, let's drop it
## you may want to keep it of course
landuse <- landuse %>%
    filter(use_simple != 'transportation')

## and now... counting intersecting area with blocks
## the warnings correspond to FALSE... I don't follow
valid_land <- st_is_valid(landuse$geometry)
## seems to work with no visual issues
## even with the error messages
landuse$geometry[!valid_land] <- st_simplify(landuse$geometry[!valid_land])

## add block if contained
block_contain_inds <- st_within(landuse,
                                block_geom)
## just in case some are covered by overlapping blocks, which surely shouldn't happen:
one_ind <- lengths(block_contain_inds) == 1

landuse$block <- NA
landuse$block[one_ind] = unlist(block_contain_inds[one_ind])

block_overlaps <- st_overlaps(landuse[!one_ind,],
                              block_geom)
landuse$block_overlap <- NA
landuse$block_overlap[!one_ind] = block_overlaps

save(landuse, file = paste0(data_folder, '/landuse.rdata'))

## calculate the areas:
block_areas = GetAreas(landuse, block_geom)
block_areas$row_index <- 1:nrow(block_areas)
block_data$row_index <- 1:nrow(block_data)
## plot(block_geom$area, rowSums(block_areas))
block_data <- block_data %>%
    left_join(block_areas, by = 'row_index')
block_data$area <- block_geom$area

blockgroup_areas = GetAreas(landuse, blockgroup_geom)
blockgroup_areas$row_index <- 1:nrow(blockgroup_areas)
blockgroup_data$row_index <- 1:nrow(blockgroup_data)
## plot(block_geom$area, rowSums(block_areas))
blockgroup_data <- blockgroup_data %>%
    left_join(blockgroup_areas, by = 'row_index')
blockgroup_data$area <- blockgroup_geom$area

## quick comparison:
## test_bg <- BlockGroupify(block_data %>%
##                          select(block, blockgroup, censustract, area_water, area_industrial))
## basically identical

block_data <- block_data %>%
    mutate(vacant_proportion = as.numeric(area_vacant_land / area),
           mixeduse_proportion = as.numeric(area_commercial_mixedresidential / area),
           comres_proportion = as.numeric((area_commercial_consumer + area_commercial_business) /
               (area_residential_highdensity + area_residential_lowdensity +
                area_residential_mediumdensity + area_commercial_consumer + area_commercial_business)))
blockgroup_data <- blockgroup_data %>%
    mutate(vacant_proportion = as.numeric(area_vacant_land / area),
           mixeduse_proportion = as.numeric(area_commercial_mixedresidential / area),
           comres_proportion = as.numeric((area_commercial_consumer + area_commercial_business) /
               (area_residential_highdensity + area_residential_lowdensity +
                area_residential_mediumdensity + area_commercial_consumer + area_commercial_business)))

## overkill, but no harm:
save(block_data,
     blockgroup_data,
     block_geom,
     blockgroup_geom,
     County_geom,
     file = save_location)

##------------------------------------##

## crime

crime_file <- paste0(data_folder, '/raw/crime/crimes.csv')

crime <- read_csv(crime_file)
## I'm finding this awkward, but I'm sure there's a way to do it.
## regardless, it's nice to have longs and lats etc for some operations, so no harm
## crime <- st_read(crime_file, geometry_column = 'Shape')

source('subsetup/setup_crime.R')

## this file cleans data as it came from the Philly police
## at some point
## if yours is different, hopefully it's clear how it's different

crime <- CleanCrime(crime)

## now get by block

## in sf form
## keep at least one extra column for plotting etc etc
crime_points <- st_as_sf(crime %>% select(crimetype, long, lat),
                         coords = c('long', 'lat'),
                         crs = st_crs(block_geom))

## now we might as well remove points outsidet the county
in_county_ind <- st_contains(County_geom, crime_points, sparse = FALSE)
## mean(in_county_ind) # or sum(!in_county_ind)
## these points are basically right on the border, but just cleaner this way


block_indices <- PointsInShape(block_geom, crime_points)

crime$block <- block_indices

## can check: table(is.na(block_indices), in_county_ind)

crime %<>% filter(!is.na(block))

## adding blockgroup and censustract
crime$blockgroup <- block_data$blockgroup[crime$block]
crime$censustract <- block_data$censustract[crime$block]

## sort crime by timedate, then I guess ucr
crime %<>% arrange(timedate, ucr)

## FILTER ACCORDING TO YOUR OWN TIME ETC

min_time <- as.POSIXct("2006-01-01 EST")
max_time <- as.POSIXct("2016-01-01 EST")

crime %<>% filter(timedate >= min_time,
                  timedate < max_time)

## note: big adjustment!!
## federal definition of rape, sexoffense changed in 2013
crime %<>%
    mutate(crimetype = if_else(crimetype %in% c('rape', 'sexoffense'),
                               'sexcrime',
                               crimetype,
                               NULL))

## add violent, vice, nonviolen, other cats:
violent_type = c('assault', 'murder', 'otherassault', 'robbery', 'sexcrime')
vice_type = c('drugviolation', 'gambling', 'prostitution')
nonviolent_type = c('arson', 'theft', 'disorderly', 'motortheft', 'vandalism', 'burglary')
other_type = c('forgery', 'fraud', 'embezzlement', 'receivestolen',
               'weaponviolation', 'familyoffense', 'dui', 'liquorlaw',
               'publicdrunk', 'vagrancy', 'other')
crime %<>%
    mutate(crime_cat = case_when(
               crimetype %in% violent_type ~ 'violent',
               crimetype %in% nonviolent_type ~ 'nonviolent',
               crimetype %in% vice_type ~ 'vice',
               crimetype %in% other_type ~ 'other',
               TRUE ~ 'no_type'))

## redo
crime_points <- st_as_sf(crime %>% select(crimetype, long, lat),
                         coords = c('long', 'lat'),
                         crs = st_crs(block_geom))

save(crime, crime_points, file = paste0(data_folder, '/crime.rdata'))

##------------------------------------##

## last step in this file:
## crime by block, blockgroup

NAddCrime <- function(x){paste0('crime_', x)}

## we remove a bunch of categories, either because
## the category is difficult to place at a location,
## or doesn't seem associated with what we're studying,
## in the sense of applying to business hours or similar
## called "other" above

crime_blockcount <- crime %>%
    filter(crime_cat != 'other') %>% 
    group_by(block, crimetype) %>%
    summarise(count = n()) %>% 
    spread(key = crimetype, value = count) %>%
    ungroup() %>%
    rename_all(NAddCrime)
crime_blockgroupcount <- crime %>%
    filter(crime_cat != 'other') %>% 
    group_by(blockgroup, censustract, crimetype) %>%
    summarise(count = n()) %>% 
    spread(key = crimetype, value = count) %>%
    ungroup() %>%
    rename_all(NAddCrime)

crime_blockcount_cat <- crime %>%
    filter(crime_cat != 'other') %>% 
    group_by(block, crime_cat) %>%
    summarise(count = n()) %>% 
    spread(key = crime_cat, value = count) %>%
    ungroup() %>%
    rename_all(NAddCrime)
crime_blockgroupcount_cat <- crime %>%
    filter(crime_cat != 'other') %>% 
    group_by(blockgroup, censustract, crime_cat) %>%
    summarise(count = n()) %>% 
    spread(key = crime_cat, value = count) %>%
    ungroup() %>%
    rename_all(NAddCrime)

## add back to the datasets

block_data <- ZeroLJoin(block_data, crime_blockcount_cat, by_vec = c('row_index' = 'crime_block'), 0)
block_data <- ZeroLJoin(block_data, crime_blockcount, by_vec = c('row_index' = 'crime_block'), 0)

## add row_index...:
crime_blockgroupcount %<>%
    left_join(blockgroup_data %>% select(row_index, blockgroup, censustract),
              by = c('crime_blockgroup' = 'blockgroup',
                     'crime_censustract' = 'censustract'))
crime_blockgroupcount_cat %<>%
    left_join(blockgroup_data %>% select(row_index, blockgroup, censustract),
              by = c('crime_blockgroup' = 'blockgroup',
                     'crime_censustract' = 'censustract'))

blockgroup_data <-
    ZeroLJoin(blockgroup_data,
              crime_blockgroupcount_cat %>% select(-c(crime_blockgroup, crime_censustract)),
              by_vec = c('row_index' = 'row_index'), 0)
blockgroup_data <-
    ZeroLJoin(blockgroup_data,
              crime_blockgroupcount %>% select(-c(crime_blockgroup, crime_censustract)),
              by_vec = c('row_index' = 'row_index'), 0)

save(block_data,
     blockgroup_data,
     block_geom,
     blockgroup_geom,
     County_geom,
     file = save_location)

##------------------------------------
