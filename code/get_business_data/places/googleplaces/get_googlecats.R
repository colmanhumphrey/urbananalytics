## Read in the google list

google_cats <- readLines('business_categories_google.txt')

## a space means a new category (or capitalisation)

empty_ind <- google_cats == ""
new_cat_ind <- c(1, which(empty_ind) + 1)
end_cat_ind <- c(which(empty_ind) - 1, length(google_cats))

google_cats_list <- lapply(1:length(new_cat_ind),
                           function(x){google_cats[(new_cat_ind[x] + 1):end_cat_ind[x]]})
names(google_cats_list) = google_cats[new_cat_ind]

save(google_cats_list, file = '../../../../data/raw_api/lists/google_cats_list.rdata')
