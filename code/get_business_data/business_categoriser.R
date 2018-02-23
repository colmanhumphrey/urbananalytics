geom_sf_load = FALSE
source('../load_packages.R')

data_folder = '../../data'

## functions are loaded as needed from subsetup,
## EXCEPT:
source('../shape_functions/PointsInShape.R')
source('../shape_functions/ResolveMultiPoint.R')
source('../shape_functions/CreateCircles.R')

load(paste0(data_folder, '/block_and_group.rdata'))

library(RecordLinkage)
library(geosphere)
library(FNN)

##------------------------------------##

place_location = paste0(data_folder, '/raw_api/')

## loading the frames from each API result
load(file = paste0(place_location, 'gplaces_frame.rdata'))
load(file = paste0(place_location, 'yplaces_frame.rdata'))
load(file = paste0(place_location, 'fplaces_frame.rdata'))

## loading the categories and parents, also already created from the API
load(file = paste0(place_location, 'lists/all_yelp_cats.rdata'))
load(file = paste0(place_location, 'lists/all_yelp_parents.rdata'))

load(file = paste0(place_location, 'lists/all_fs_cats.rdata'))
load(file = paste0(place_location, 'lists/all_fs_parents.rdata'))

load(file = paste0(place_location, 'lists/google_cats_list.rdata'))
load(file = paste0(place_location, 'lists/all_google_cats.rdata'))
## google has no parents (like batman)


##------------------------------------##

## normally, just sourcing some horrible file is a terrible idea
## but this is an exception: many messy decisions in here,
## end result being 
source(file = 'new_categories.R')
## IF YOU WISH TO CHANGE IT, GO FOR IT
## gives yelp_cat, google_cat, fs_cat
## gives yelp_ind, google_ind, fs_ind

## but they need to change around a bit:
new_ind <- c(5,1,2,3,4,6,7,8,9,11,10,12)

google_ind <- google_ind[new_ind]
yelp_ind <- yelp_ind[new_ind]
fs_ind <- fs_ind[new_ind]

names(google_ind)[c(1,10)] = c('cafe', 'retail')
names(yelp_ind)[c(1,10)] = c('cafe', 'retail')
names(fs_ind)[c(1,10)] = c('cafe', 'retail')

ind_lists <- list('google' = google_ind,
                  'yelp' = yelp_ind,
                  'fs' = fs_ind)

frame_list <- list('google' = gplaces_frame,
                   'yelp' = yplaces_frame,
                   'fs' = fplaces_frame)

##------------------------------------##

## based on checks, using google for hours first

## also using google as categorising gold standard

##------------------------------------##

## WE WILL NOW DROP 'TRANSIT' AND 'IGNORE'

index_matrices <- lapply(ind_lists, function(il){
    temp = matrix(unlist(il), ncol = length(il))
    colnames(temp) = names(il)
    return(temp)})

index_no_cat <- lapply(index_matrices, function(m){
    rowSums(m) == 0})

## index_only_ignore <- lapply(index_matrices, function(m){    
##     m[,'ignore'] & rowSums(m) == 1})
## index_only_transit <- lapply(index_matrices, function(m){    
##     m[,'transit'] & rowSums(m) == 1})

index_ignore_or_transit <- lapply(index_matrices, function(m){    
    (m[,'ignore'] | m[,'transit']) & rowSums(m[,1:10]) == 0})

source('setup_business_functions/FindVarious.R')
## also get rid of ATMs with names that we don't want
index_atm <- lapply(frame_list, function(x){FindATM(x$name)})

## and to be honest, MDs got to go. It's just some office...
index_doc <- lapply(frame_list, function(x){FindDoctors(x$name)})

index_kill <- mapply(function(x1, x2, x3, x4){
    x1 | x2 | x3 | x4},
    index_no_cat, index_ignore_or_transit, index_atm, index_doc)

## fix it
## store original...:
len_vec <- c('google' = nrow(gplaces_frame),
             'yelp' = nrow(yplaces_frame),
             'fs' = nrow(fplaces_frame))

gplaces_frame %<>%
    filter(!index_kill[['google']])

yplaces_frame %<>%
    filter(!index_kill[['yelp']])

fplaces_frame %<>%
    filter(!index_kill[['fs']])

## redo:
frame_list <- list('google' = gplaces_frame,
                   'yelp' = yplaces_frame,
                   'fs' = fplaces_frame)
    
## and new mats:
index_adj <- list()
index_adj[['google']] <-
    index_matrices[['google']][!index_kill[['google']],]
index_adj[['yelp']] <-
    index_matrices[['yelp']][!index_kill[['yelp']],]
index_adj[['fs']] <-
    index_matrices[['fs']][!index_kill[['fs']],]

## get all dups, including self-dups

len_list <- list('orig' = len_vec,
                 'after_cat_remove' = unlist(lapply(frame_list, nrow)))

source('setup_business_functions/MatchBusiness.R')

## dups are not defined symmetrically, for a multitude
## of reasons, so you have to do all 9
## by default, we find dups of frame2 in frame1

## should only be a few mins with default 500
dup_list <- list()
for(bus1 in names(frame_list)){
    message('First by ', bus1)
    
    dup_list[[bus1]] <- list()
    
    for(bus2 in names(frame_list)){
        message('Second by ', bus2)
        
        dup_list[[bus1]][[bus2]] <- GetMatchList(frame_list[[bus1]],
                                                 frame_list[[bus2]])
    }
}

save(dup_list, file = paste0(data_folder, '/raw_api/dup_list.rdata'))

remove_dups <- list()

bus_source <- names(frame_list)

## careful here
## because they don't all seem to mismatch the same way...
## cut off at 0.3 for google
## 0.2 for yelp
## 0.1 for fs!!
bus_dist <- c('google' = 0.3,
              'yelp' = 0.2,
              'fs' = 0.1) ## slightly arb... but all (nonmanual) methods are.

source('setup_business_functions/RemoveSelfDup.R')

remove_dups[['google']] <- RemoveSelfDup(bus = 'google', dup_list, frame_list,
                                         cut_off = bus_dist['google'])
remove_dups[['yelp']] <- RemoveSelfDup(bus = 'yelp', dup_list, frame_list,
                                         cut_off = bus_dist['yelp'])
remove_dups[['fs']] <- RemoveSelfDup(bus = 'fs', dup_list, frame_list,
                                         cut_off = bus_dist['fs'])

save(remove_dups, file = paste0(data_folder, '/raw_api/remove_dups.rdata'))

len_list[['remove']] = lengths(remove_dups)

len_list[['total_pre_union']] = len_list[['after_cat_remove']] - len_list[['remove']]

##------------------------------------##

## open hours counts...:

num_open_f<- function(bus_f, rem_ind){
    sum(!is.na(bus_f$tue_2[-rem_ind]))}
len_list[['num_open']] = mapply(num_open_f, frame_list, remove_dups)

##------------------------------------##

## for non-self dups, we roll with some asymmetry
## - first, google is done with dups with just self dups
## then we further remove any other offending businesses
## - next, yelp: we check if the business is in google (the allowed google)
## remove self dups, google dups, and any other disallowed
## - finally fs must not self-dup, or google dup, or yelp dup
## or be disallowed any other way

## GOOGLE:
all_ind <- 1:nrow(gplaces_frame)
google_keep <- all_ind[!(all_ind %in% remove_dups[['google']])]

## YELP:
source('setup_business_functions/RemoveDupAcross.R')

## leaving the match at 0.9, not as strict as self-dups
yelp_dup <- RemoveDupAcross('google', 'yelp',
                            dup_list, frame_list,
                            ref_keep = google_keep,
                            cut_off = 0.9)

##---------------- quick test...:
j = 2201
yplaces_frame[which(yelp_dup)[j],1:3]
gplaces_frame[dup_list[['google']][['yelp']][['close_inds']][[which(yelp_dup)[j]]],1:3]

table(yelp_dup, (1:nrow(yplaces_frame)) %in% remove_dups[['yelp']])
##---------------- quick test ^^^

all_ind <- 1:nrow(yplaces_frame)
yelp_keep <- all_ind[!(all_ind %in% remove_dups[['yelp']]) & !yelp_dup]

## finally FS

fs_dup_google <- RemoveDupAcross('google', 'fs',
                                 dup_list, frame_list,
                                 ref_keep = google_keep,
                                 cut_off = 0.9)
fs_dup_yelp <- RemoveDupAcross('yelp', 'fs',
                               dup_list, frame_list,
                               ref_keep = yelp_keep,
                               cut_off = 0.9)
all_ind <- 1:nrow(fplaces_frame)
fs_keep <- all_ind[!(all_ind %in% remove_dups[['fs']]) &
                   !fs_dup_google &
                   !fs_dup_yelp]
                                 
## into a nice frame

business_frame <- bind_rows(gplaces_frame[google_keep,1:3],
                            yplaces_frame[yelp_keep,1:3],
                            fplaces_frame[fs_keep,1:3])

## just in case you have different hours!
hours_ind_list <- lapply(frame_list, function(ff){
    ## assuming it goes sun_0 to sat_23
    first_ind = grep('sun_0', names(ff))
    last_ind = grep('sat_23', names(ff))
    return(first_ind : last_ind)
})

open_hours <- bind_rows(gplaces_frame[google_keep, hours_ind_list[['google']]],
                        yplaces_frame[yelp_keep, hours_ind_list[['yelp']]],
                        fplaces_frame[fs_keep, hours_ind_list[['fs']]])

all_cats <- rbind(index_adj[['google']][google_keep, 1:10],
                  index_adj[['yelp']][yelp_keep, 1:10],
                  index_adj[['fs']][fs_keep, 1:10])

all_cats_frame <- as_data_frame(all_cats)

business_frame <- bind_cols(business_frame,
                            all_cats_frame)

## monday first; makes some weekend stuff easier etc etc
business_frame <- bind_cols(business_frame, open_hours[,c(25:168, 1:24)])

bus_points <- st_as_sf(business_frame %>% select('name', 'lng', 'lat'),
                       coords = c('lng', 'lat'),
                       crs = st_crs(block_geom))

bus_blocks <- PointsInShape(block_geom, bus_points)
## bus_blockgroups <- PointsInShape(blockgroup_geom, bus_points)

## points in multiple blocks
no_block <- is.na(bus_blocks)

area_res_ind <- ResolveMultiPoint(bus_points[no_block,], block_geom)

bus_blocks[no_block] = area_res_ind

business_frame$block = bus_blocks
business_frame$blockgroup = block_data$blockgroup[bus_blocks]
business_frame$censustract = block_data$censustract[bus_blocks]

bf_hours_col = which(names(business_frame) == 'mon_0') : which(names(business_frame) == 'sun_23')
business_frame$total_open <- rowSums(business_frame[, bf_hours_col])

## specific hours windows:
weekday_eve <- expand.grid(day = c('mon', 'tue', 'wed', 'thu', 'fri'),
                           hour = 18:23)
we_ind <- names(business_frame) %in% paste(weekday_eve$day, weekday_eve$hour, sep = '_')

business_frame$weekday_evening_open <- rowSums(business_frame[, we_ind])

weekend_night <- expand.grid(day = c('sat', 'sun'),
                             hour = 0:3)
wn_ind <- names(business_frame) %in% paste(weekend_night$day, weekend_night$hour, sep = '_')

business_frame$weekend_night_open <- rowSums(business_frame[, wn_ind])

save(business_frame, file = '../../data/business_frame.rdata')

##------------------------------------#
