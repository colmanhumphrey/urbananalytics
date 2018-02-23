The purpose of this (first) project is twofold. First, to outline a method for acquiring, cleaning and reshaping
data from many sources about many facets of a given city. Second,
to perform some initial analyses.

----------------

The Data:

Here, we acquire and clean:
- Demographic data:
    - census, full collection
      -- gives population by race on block level
    - data that is re-collected routinely
      -- gives e.g. income and poverty metrics at the blockgroup level
- Crime Data
    - at the very least, should give location, time and category of each crime
      -- just have to be careful with e.g. car thefts: which location is recorded, crime or recovery?
- Landuse Data
    - gives use of land by lot, generally many lots fit in a block, in some minor cases
      lots extend over multiple blocks / blockgroups
- Business Data
   - data from Google, Yelp, Foursquare
   - hard thing is to put it together (categories)
   - well, downloading with the APIs isn't so easy, but at least is "straight forward"

Not used in these analyses, but useful later:
- Street Data
    - mainly for intersections
- School Data
    - size, location, years taught
- Transit Routes / Stops
- Property Data
  - age, height, value etc, many covariates

Collecting this data is not easy, and then it needs to be cleaned. Even then its format
might be too different to ours to fully apply our analyses, but hopefully they can be adjusted
without huge changes.

----------------

The cleaning etc:

First, we run setup_main.R, within code/get_clean_data. It has its own readme,
but basically it reads in census data from the API (you just supply your key),
and landuse and crime from external files. The cleaning functions for the latter
assume certain file characteristics, but it should be clear enough what you would
need to change if yours is different.

Next, within code/get_business_data, we don't include the code relevant to the APIs
for the three services (Google, Yelp, Foursquare) and we further assume you can
adjust the raw JSON... but we do include how to combine and remove duplicates.

----------------

The analyses:

We outline our analyses and plots from "Analysis of Urban Vibrancy and Safety
in Philadelphia".

The matching is done within code/first_matching. The folders short_long
and high_low only contain one relevant file each, those create the
data necessary to do our matching comparisons.

All plots are created within code/plots.

overall_blockgroup.R creates figs 1, 2, 4, 6.

landuse_color.R creates fig 3, it's slow.

crime_bar.R creates fig 5.

excess_plots.R creates figs 7, 8, and 9.

Within the folder code/plots/matching, short_long_plots.R creates fig 10,
and high_low_plots.R creates figs 11 and 12.

----------------
