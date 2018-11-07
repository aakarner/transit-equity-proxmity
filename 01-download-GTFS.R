# This script downloads GTFS files representing public transit service provided 
# by Houston METRO both before and after their major bus system redesign 
# (often referred to as the "System Reimagining").

library(tidytransit)

# GTFS feeds obtained from transitfeeds.com
# The “before” dataset represents service from May 23, 2015 to August 15, 2015 
# and the “after” dataset represents service from August 16, 2015 to 
# January 23, 2016. 
# More information about System Reimagining is available here: 
# https://www.ridemetro.org/Pages/Reimagining.aspx

pre_sr_gtfs <- read_gtfs("data/20150517_htx.zip", 
                         local = TRUE,
                         geometry = TRUE,
                         frequency = TRUE)

post_sr_gtfs <- read_gtfs("data/20150818_htx.zip", 
                         local = TRUE,
                         geometry = TRUE,
                         frequency = TRUE)
