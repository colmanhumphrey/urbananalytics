## not all are necessary


library(tidycensus) ## to download census data

## if you want geom_sf, need tidyverse version
## library(devtools)
## devtools::install_github("tidyverse/ggplot2")
library(ggplot2) ## plotting stuff

library(tigris) ## extra code for tigris filetypes
library(viridis) ## call before mapview... it calls some weird version of viridisLite
library(mapview) ## for plotting quickly on a map

library(MASS)

library(pryr) ## object_size, etc, watch for conflicts
library(sf) ## simple features, better than raw shapefiles with rgeos or whatever
library(lwgeom) ## need to calculate some distances?
library(readr) ## for reading / writing csv
library(stringr) ## easier string ops
library(magrittr) ## using %<>%, a few other things
library(purrr) ## map etc, map_dlb
library(parallel) ## running parallel computations
library(dplyr) ## bunch of stuff, %>%, select, etc etc, nice
library(tibble) ## don't know what this adds over dplyr, but I think something? as_tibble?
library(tidyr) ## spread, gather
