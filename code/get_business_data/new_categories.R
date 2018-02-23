yelp_cat <- list()
google_cat <- list()
fs_cat <- list()

yelp_ind <- list()
google_ind <- list()
fs_ind <- list()

## functions for cats:

CatsinList <- function(cats, list){
    return(unlist(lapply(list, function(x){
                             sum(cats %in% x) > 0})))
}

SortTable <- function(list, index){
    return(sort(table(unlist(list[index])), decreasing = TRUE))
}

##------------------------------------##

## NOTES:
## the order given here is nearly alphabetical, but first
## taking into account that some cats are dependent on others

##------------------------------------##
## convenience and gas stations
yelp_cat[['convenience']] = c('convenience', 'servicestations') ## remove from food, auto
google_cat[['convenience']] = c(google_cats_list[['CONVENIENCE']]) 
fs_cat[['convenience']] = c('Convenience Store', 'Gas Station') ## remove from shop

yelp_ind[['convenience']] <- CatsinList(yelp_cat[['convenience']], all_yelp_cats)
google_ind[['convenience']] <- CatsinList(google_cat[['convenience']], all_google_cats)
fs_ind[['convenience']] <- CatsinList(fs_cat[['convenience']], all_fs_cats)

##------------------------------------##
## gyms, active
yelp_cat[['gym']] = c('climbing',
            'cyclingclasses',
            'fencing',
            'fitness',
            'bootcamps',
            'boxing',
            'cardioclasses',
            'dancestudio',
            'golflessons',
            'gyms',
            'martialarts',
            'pilates',
            'healthtrainers',
            'yoga',
            'gun_ranges',
            'gymnastics',
            'indoor_playcenter',
            'leisure_centers',
            'skatingrinks',
            'skiing',
            'sports_clubs', 
            'swimmingpools')

google_cat[['gym']] = c(google_cats_list[['GYM']])
fs_cat[['gym']] = c('Gym / Fitness Center',
          'Boxing Gym',
          'Climbing Gym',
          'Cycle Studio',
          'Gym Pool',
          'Gymnastics Gym',
          'Gym',
          'Martial Arts Dojo',
          'Pilates Studio',
          'Yoga Studio',
          'Hockey Rink',
          'Gun Range',
          'Indoor Play Area',
          'Pool',
          'Recreation Center',
          'Ski Area',
          'Ski Chairlift',
          'Ski Trail')

yelp_ind[['gym']] <- CatsinList(yelp_cat[['gym']], all_yelp_cats)
google_ind[['gym']] <- CatsinList(google_cat[['gym']], all_google_cats)
fs_ind[['gym']] <- CatsinList(fs_cat[['gym']], all_fs_cats)

##------------------------------------##
## institution; public stuff, education, arts, leisure
yelp_cat[['institution']] <- c('publicservicesgovt', 'education', 'arts', 'religiousorgs',
                               'health') ## remove pharmacy... actually no need, see note
google_cat[['institution']] <- c(google_cats_list[['INSTITUTION']],
                                 google_cats_list[['LEISURE']],
                                 google_cats_list[['WORSHIP']],
                                 google_cats_list[['SCHOOL']],
                                 google_cats_list[['HEALTH_LAW']])
fs_cat[['institution']] <- c('Arts & Entertainment', 'Event', 'College & University',
                             'Professional & Other Places')
                                        # but not:
                                        # Office
                                        # Parking
                                        # Residence

## quick note: yelp pharmacies are drugstores... so don't have to remove from health.
yelp_ind[['institution']] <- CatsinList(yelp_cat[['institution']], all_yelp_parents)
google_ind[['institution']] <- CatsinList(google_cat[['institution']], all_google_cats)

inst_notpro <- CatsinList(fs_cat[['institution']][-4], all_fs_parents)
office <- c('Advertising Agency',
            'Campaign Office',
            'Conference Room',
            'Corporate Amenity',
            'Corporate Cafeteria',
            'Corporate Coffee Shop',
            ## 'Coworking Space', ## this one is later fully ignored
            'Tech Startup')

res <- c('Assisted Living',
         'Home (private)',
         'Housing Development',
         'Residential Building (Apartment / Condo)',
         'Trailer Park')

pro_not3 <- CatsinList(fs_cat[['institution']][4], all_fs_parents) &
    !CatsinList(c(office, res, 'Parking'), all_fs_cats)
fs_ind[['institution']] <- inst_notpro | pro_not3

##------------------------------------##
## liquor: special case
yelp_cat[['liquor']] = c('beer_and_wine') ## remove from food
google_cat[['liquor']] = c(google_cats_list[['LIQUOR']])
fs_cat[['liquor']] = c('Beer Store', 'Wine Shop', 'Liquor Store') ## remove from shop

yelp_ind[['liquor']] <- CatsinList(yelp_cat[['liquor']], all_yelp_cats)
google_ind[['liquor']] <- CatsinList(google_cat[['liquor']], all_google_cats)
fs_ind[['liquor']] <- CatsinList(fs_cat[['liquor']], all_fs_cats)

##------------------------------------##
## PREREQS:
## liquor

## food, not restaurant
yelp_cat[['dayfood']] <- c('food') ## and not restaurant... and not convenience, or street stuff.
google_cat[['dayfood']] <- c(google_cats_list[['CAFE']], 'food') ## and not restaurant, or
## CONVENIENCE
fs_cat[['dayfood']] <- c('Food') ## and not restaurant ind!

### RUN liquor first...
yelp_subfood <- c('butcher',
                  'intlgrocery',
                  'organic_stores',
                  'gourmet',
                  'candy',
                  'cheese',
                  'chocolate',
                  'dagashi',
                  'frozenfood',
                  'markets',
                  'healthmarkets',
                  'herbsandspices',
                  'macarons',
                  'meats',
                  'oliveoil',
                  'pastashops',
                  'popcorn',
                  'seafoodmarkets',
                  'tofu',
                  'waterstores',
                  'wineries')

yelp_ind[['dayfood']] <- CatsinList(yelp_cat[['dayfood']], all_yelp_parents) &
    !CatsinList(c(yelp_cat[['liquor']], yelp_subfood), all_yelp_cats)

cafeind <- CatsinList(google_cats_list[['CAFE']], all_google_cats)
## foodnotrest <- CatsinList(c('food', 'meal_delivery', 'meal_takeaway'), all_google_cats) &
##     !CatsinList(c('restaurant'), all_google_cats)

google_ind[['dayfood']] <- cafeind

restaurant_ind <- unlist(lapply(all_fs_cats, function(x){
                                    sum(grepl('Restaurant', x)) > 0}))
otherRest_cats <- c('Diner', 'Bistro', 'BBQ Joint', 'Irish Pub', 'Buffet',
                    'Gastropub', 'Pizza Place', 'Wings Joint')    
otherRest_ind <- unlist(lapply(all_fs_cats, function(x){
                                sum(otherRest_cats %in% x) > 0}))
fsrestaurant_ind <- restaurant_ind | otherRest_ind

fs_ind[['dayfood']] <- CatsinList(fs_cat[['dayfood']], all_fs_parents) & !fsrestaurant_ind

##------------------------------------##
## lodging
yelp_cat[['lodging']] <- c('hotels', 'hostels', 'bedbreakfast', 'reststops',
                           'mountainhuts') ## add remaing hotelstravel to street (after transit)
google_cat[['lodging']] <- c(google_cats_list[['LODGING']])
fs_cat[['lodging']] <- c('Hotel',
                         'Bed & Breakfast',
                         'Boarding House',
                         'Hostel',
                         'Hotel Pool',
                         'Motel',
                         'Resort',
                         'Roof Deck',
                         'Vacation Rental')
## add remaing hotelstravel to street (after transit)

yelp_ind[['lodging']] <- CatsinList(yelp_cat[['lodging']], all_yelp_cats)
google_ind[['lodging']] <- CatsinList(google_cat[['lodging']], all_google_cats)
fs_ind[['lodging']] <- CatsinList(fs_cat[['lodging']], all_fs_cats)

##------------------------------------##
## nightlife (nightclubs, bars)
yelp_cat[['nightlife']] <- c('nightlife')
google_cat[['nightlife']] <- c(google_cats_list[['NIGHTCLUB']],
                                      google_cats_list[['BAR']])
fs_cat[['nightlife']] <- c('Nightlife Spot')

yelp_ind[['nightlife']] <- CatsinList(yelp_cat[['nightlife']], all_yelp_parents)
google_ind[['nightlife']] <- CatsinList(google_cat[['nightlife']], all_google_cats)
fs_ind[['nightlife']] <- CatsinList(fs_cat[['nightlife']], all_fs_parents)

##------------------------------------##
## pharmacy
yelp_cat[['pharmacy']] = c('drugstores') ## remove from shop/street
## yes, very annoying to use different terms.
google_cat[['pharmacy']] = c(google_cats_list[['PHARMACY']])
fs_cat[['pharmacy']] = c('Pharmacy', 'Drugstore') ## remove from shop

yelp_ind[['pharmacy']] <- CatsinList(yelp_cat[['pharmacy']], all_yelp_cats)
google_ind[['pharmacy']] <- CatsinList(google_cat[['pharmacy']], all_google_cats) 
fs_ind[['pharmacy']] <- CatsinList(fs_cat[['pharmacy']], all_fs_cats)

##------------------------------------##
## restaurants
yelp_cat[['restaurant']] <- c('restaurants')
google_cat[['restaurant']] <- c('restaurant', 'meal_delivery', 'meal_takeaway') ## not day food
fs_cat[['restaurant']] <- c('Food') ## well don't really need this

yelp_ind[['restaurant']] <- CatsinList(yelp_cat[['restaurant']], all_yelp_parents)
google_ind[['restaurant']] <- CatsinList(google_cat[['restaurant']], all_google_cats)
fs_ind[['restaurant']] <- fsrestaurant_ind

##------------------------------------##
## transit
yelp_cat[['transit']] <- c('trainstations', 'carrental',
                           'transport',
                           'airlines',
                           'airport_shuttles',
                           'bikesharing',
                           'busstations',
                           'buses',
                           'cablecars',
                           'dolmusstation',
                           'ferries',
                           'limos',
                           'metrostations',
                           'pedicabs',
                           'privatejetcharter',
                           'publictransport',
                           'sharedtaxis',
                           'taxis',
                           'towncarservice',
                           'trains',
                           'watertaxis')

google_cat[['transit']] <- c(google_cats_list[['TRANSPORT']])
fs_cat[['transit']] <- c('Tram Station', 'Train Station', 'Taxi',
                         'Metro Station', 'Light Rail Station', 'Bus Station',
                         'Transportation Service', 'Port',
                         'Rental Car Location')

yelp_ind[['transit']] <- CatsinList(yelp_cat[['transit']], all_yelp_cats)
google_ind[['transit']] <- CatsinList(google_cat[['transit']], all_google_cats)
fs_ind[['transit']] <- CatsinList(fs_cat[['transit']], all_fs_cats)

##------------------------------------##
## PREREQ:
## lodging
## transit
## food

## street facing 'stores' etc
yelp_cat[['street']] <- c('shopping', 'homeservices', 'localservices', 'eventservices',
                          'auto', 'beautysvc', 'bicycles', 'pets',
                          'financialservices', 'professional') ## auto, but NOT gas or parking
## shopping: not drugstores

streetpars <- CatsinList(yelp_cat[['street']][-c(1,5)], all_yelp_parents)
autonotgaspark <- CatsinList('auto', all_yelp_parents) &
    !CatsinList(c('servicestations', 'parking'), all_yelp_cats)
shoppingnotdrugorwine <- CatsinList('shopping', all_yelp_parents) &
    !CatsinList('drugstores', all_yelp_cats)

## add hotelstravel that isn't lodging or transit... run those first.
non_hotelstravel <- c(yelp_cat[['lodging']], yelp_cat[['transit']])

ht_add <- CatsinList('hotelstravel', all_yelp_parents) &
    !CatsinList(non_hotelstravel, all_yelp_cats)

foodind <- CatsinList(yelp_subfood, all_yelp_cats)

yelp_ind[['street']] <- streetpars | autonotgaspark | shoppingnotdrugorwine | ht_add | foodind

google_cat[['street']] <- c(google_cats_list[['RETAIL_STORE']],
                            google_cats_list[['RETAIL_SERVICE']])
google_ind[['street']] <- CatsinList(google_cat[['street']], all_google_cats)

fs_cat[['street']] <- c('Shop & Service') ## remove Pharmacy, Convenience and Gas Station from shop & service

all_fds <- c('Food & Drink Shop',
             'Beer Store',
             'Butcher',
             'Cheese Shop',
             'Farmers Market',
             'Fish Market',
             'Food Service',
             'Gourmet Shop',
             'Grocery Store',
             'Health Food Store',
             'Liquor Store',
             'Organic Grocery',
             'Sausage Shop',
             'Street Food Gathering',
             'Supermarket',
             'Wine Shop')

fds_street <- c('Food & Drink Shop',
                'Butcher',
                'Cheese Shop',
                'Farmers Market',
                'Fish Market',
                'Gourmet Shop',
                'Grocery Store',
                'Health Food Store',
                'Organic Grocery',
                'Sausage Shop',
                'Supermarket')
                
fds_liquor <- c('Beer Store',
                'Liquor Store',
                'Wine Shop')

avoidfs <- c(all_fds[!(all_fds %in% fds_street)], 'Pharmacy', 'Convenience Store',
             'Gas Station', 'ATM')

allstreet <- CatsinList(fs_cat[['street']], all_fs_parents)

avoid_street <- CatsinList(avoidfs, all_fs_cats)

non_hotelstravel <- c(fs_cat[['lodging']], fs_cat[['transit']])

ht_add <- CatsinList('Travel & Transport', all_fs_parents) &
    !CatsinList(non_hotelstravel, all_fs_cats)

fs_ind[['street']] <- (allstreet & !avoid_street) | ht_add


##------------------------------------##
## don't care about stuff...:
yelp_cat[['ignore']] <- c('parking', 'massmedia')
## and active but not gym
google_cat[['ignore']] <- c(google_cats_list[['LARGE']], google_cats_list[['IGNORE']])

fs_cat[['ignore']] <- c('ATM',
                        'Residential Building (Apartment / Condo)',
                        'Parking',
                        'Tech Startup',
                        'Conference Room',
                        'Advertising Agency',
                        'Assisted Living',
                        'Housing Development',
                        'Office',
                        'Building',
                        'Corporate Cafeteria',
                        'Coworking Space',
                        'Event Space',
                        'Medical Lab',
                        'Non-Profit',
                        'Residence',
                        'Voting Booth')
## and then outdoors that isn't gym!

yelp_ind[['ignore']] <- CatsinList(yelp_cat[['ignore']], all_yelp_cats) |
    CatsinList(yelp_cat[['ignore']], all_yelp_parents) |
        (CatsinList('active', all_yelp_parents) &
         !CatsinList(yelp_ind[['gym']], all_yelp_cats))
google_ind[['ignore']] <- CatsinList(google_cat[['ignore']], all_google_cats)
fs_ind[['ignore']] <- CatsinList(fs_cat[['ignore']], all_fs_cats) |
    (CatsinList('Outdoors & Recreation', all_fs_parents) & !
     CatsinList(fs_cat[['gym']], all_fs_cats))

## ##------------------------------------##

## ## if you want to check some stuff, here are sums of categories
## ## multi is expected, some businesses are in multiple cats
## ## zeros are fine in IG, to be checked (?) in non-ig...
## yelpmat <- matrix(unlist(yelp_ind), ncol = length(yelp_ind))
## table(rowSums(yelpmat))

## yelpmat_ig <- matrix(unlist(yelp_ind[1:11]), ncol = length(yelp_ind[1:11]))
## table(rowSums(yelpmat_ig))

## googlemat <- matrix(unlist(google_ind), ncol = length(yelp_ind))
## table(rowSums(googlemat))

## googlemat_ig <- matrix(unlist(google_ind[1:11]), ncol = length(yelp_ind[1:11]))
## table(rowSums(googlemat_ig))

## fsmat <- matrix(unlist(fs_ind), ncol = length(yelp_ind))
## table(rowSums(fsmat))

## fsmat_ig <- matrix(unlist(fs_ind[1:11]), ncol = length(yelp_ind[1:11]))
## table(rowSums(fsmat_ig))

## ## checking leftover:

## table(unlist(all_google_cats[rowSums(googlemat_ig) == 0]))
## table(unlist(all_yelp_cats[rowSums(yelpmat_ig) == 0]))
## table(unlist(all_fs_cats[rowSums(fsmat_ig) == 0]))


## all_count <- cbind(unlist(lapply(google_ind, sum)),
##                    unlist(lapply(yelp_ind, sum)),
##                    unlist(lapply(fs_ind, sum)))
## colnames(all_count) = c('google', 'yelp', 'fs')

## all_count <- as_data_frame(all_count)
## all_count$all = rowSums(all_count)
## print(all_count)
## ##------------------------------------##
