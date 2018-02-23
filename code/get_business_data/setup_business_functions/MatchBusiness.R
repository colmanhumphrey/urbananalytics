#' find duplicates by matching names and locations
#'
#' @param bus_frame1
#' a frame with lng/lat, 
#' and a name column
#'
#' @param bus_frame2
#' see above
#'
#' @param knn_out
#' how many knn approximations to check
GetMatchList <- function(bus_frame1, bus_frame2, knn_num = 500){
    ## to vastly reduce the space
    message('Getting KNN approx')
    knn_out <- KNNApprox(bus_frame1[,c('lng', 'lat')],
                         bus_frame2[,c('lng', 'lat')],
                         k = knn_num)
    
    knn_index <- knn_out[['nn.index']]

    phys_dist <- matrix(NA, nrow = nrow(knn_index), ncol = ncol(knn_index))
    ## knn_dist <- knn_out[['nn.dist']] * 100000

    message('Getting true dist for closest points')
    for(i in 1:ncol(knn_index)){
        phys_dist[,i] = distGeo(p1 = bus_frame1[knn_index[,i],c('lng', 'lat')],
                                p2 = bus_frame2[,c('lng', 'lat')])
    }

    ## name dists:
    message('Getting name dist for closest points')
    name_dist <-  matrix(NA, nrow = nrow(knn_index), ncol = ncol(knn_index))
    for(i in 1:ncol(knn_index)){
        rel_ind <- knn_index[,i]
        
        name_max <- pmax(nchar(bus_frame1$name[rel_ind]), nchar(bus_frame2$name))
        name_dist_vec <- pmin(levenshteinDist(bus_frame1$name[rel_ind], bus_frame2$name), name_max)
        name_adjdist <- name_dist_vec / name_max
                
        name_dist[,i] = name_adjdist
    }
    
    ## this is a big deal! chosen with horrible trial and improvement
    ## tweak for yourself...!!!
    combine_dist = (phys_dist / 100)^2  + (name_dist/0.4)^2

    match_inds <- lapply(1:nrow(knn_index), function(j){
        knn_index[j,which(combine_dist[j,] < 0.9)]})

    match_dist <- apply(combine_dist, 1, function(x){x[x < 0.9]})

    return(list(close_inds = match_inds,
                close_dists = match_dist))
}

#' planar approx, squishing the lng/lat axes
KNNApprox <- function(mat1, mat2, k = 500){
    meds <- apply(mat1, 2, median)
    lng_mult <- distGeo(p1 = meds - c(0.05,0),
                        p2 = meds + c(0.05,0))
    lat_mult <- distGeo(p1 = meds - c(0, 0.05),
                        p2 = meds + c(0, 0.05))
    ll_ratio <- lng_mult / lat_mult

    mat1[,1] <- mat1[,1] * ll_ratio
    mat2[,1] <- mat2[,1] * ll_ratio
    
    knn_out = get.knnx(data = mat1,
                       query = mat2,
                       k = k) ## this is an approximation
    return(knn_out)
}
