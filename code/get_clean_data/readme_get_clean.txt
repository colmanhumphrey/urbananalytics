this is the first code to run.

again, there are many datasets:

- Demographic data:
- Crime Data
- Landuse Data
- Business Data

Used further (next time):

- Street Data
- School Data
- Transit Routes / Stops


TWO STEPS: acquiring and cleaning.

------------------------------------

In what follows, "everything" is generated from setup_main.R
But the folder code/get_clean_data/subsetup contains a file for each of the three
main groups of data we download here, which are sources by setup_main.R

Each of those files contain functions that ideally you can easily edit to your preference.

------------------------------------

Firstly, we get census data - demographics, income, poverty.

We recommend you use the R package tidycensus (what our functions use),
or censusapi, or other similar APIs. 

If not, you can also just download the relevant files from the census, but
the naming is important (or just change the code here).

Demographics come in many versions, and "race" is a bit of a tenuous term.
We don't use race categories as objects of inference, so we chose a version
of categories that gives us "good" matching characteristics, in a bias-variance
way of thinking. The way you split your race categories will depend strongly
on what you're trying to accomplish, and where you're studying.

In subsetup/setup_demographics.R, the function GetDemoDataGeom pulls what
we need. Within the function, there is a box that can be set to pull
all variables from the SF1 Census table. If you wish to change this function,
pulling those variables is a good start, and/or finding what you want from the
census site.

Similarly, the ASC estimates are pulled too, and you can change
what variables you pull using the load_variables function again.

The zero_block argument to GetDemoDataGeom kills off any blocks
that you don't want to count. For our purposes, prison population
is not what we intend to analyse.

----------------

Landuse data is loaded next.

These files need to be sources for each city / county. If the names are different
for yours etc, it shouldn't be a problem, just edit the relevant line that pulls the
shape files.

LandUseDetail takes the two-digit landuse value and converts it to its named
type, and produces a color for it.

For colors, there is a body of work behind these, although they're not exactly
consistent in different areas. The function spells out which colors go with
which landuse type for our work.

We then add the areas to the block and blockgroup data.

----------------

Finally (for this file), we load and clean crime data.

Yours may look different, so you may have to edit subsetup/setup_crime.R
until e.g. CleanCrime etc works for your data.

NOTE: the federal definition of rape and sexoffense changed in 2013,
so to keep consistency, we add them.

The end of the main file adds crime to block and blockgroup data.
