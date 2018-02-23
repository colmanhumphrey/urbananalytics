Note: we don't supply the code for getting the business data

The data is proprietary, and so after downloading, we're only supposed to store
e.g. summaries. We feel supplying the full code and data isn't in the spirit of the
APIs.

Getting the data is a matter of figuring out the APIs of Google places, Yelp, Foursquare,
and just getting all the data. That's not "easy", but it's somewhat methodical.

The hard part is getting the three datasets to play nicely with each other. We supply
the code to get this to happen in R.

##------------------------------------

Here's how you do it:

For each of Google, Yelp, Foursquare
     - download all data from the APIs
     - convert to data frames
       -- name, lng, lat (chr, dbl, dbl)
       -- 24 * 7 logicals for each hour of the week:
          - sun_0, sun_1, ..., sun_23, mon_0, ...., sat_23
       -- that's all you'll need for business_categoriser.R, but you can add anything more
     - save lists of all categories and parent categories, lists
       -- all_yelp_cats for example should have length equal to
          number of yelp businesses collected;
          each entry is a list of lists, basically straight from JSON data
       -- all_yelp_parents should be similar, but each entry is a
          characeter vector giving the outer most category of the inner
	  cats from above;
	  Basically you just recursively take each category back to its base
       -- fs is the same, but not google

We ran this in "places", due to slightly different requirements / APIs for each

NEXT:
Basically we'll use Google as our reference dataset, it seems to be the cleanest.
In places/googleplaces, you'll find the file business_categories_google.txt,
which explains how we converted Google's categories to ours (get_googlecats.R
converts this txt file to an R list).

Run the script business_categoriser.R. It converts the Yelp and FS data
to the main categories, and checks for duplicated, including self-duplicates!
Note that it sources the 'new_categories.R' file, which does the horrible
job of categorising businesses... And that requires the cats list and parents list
referenced above.

Hopefully you can read what's happening even if you do something different.
