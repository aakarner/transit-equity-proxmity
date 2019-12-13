# This script downloads GTFS files representing public transit service provided 
# by Houston METRO both before and after their major bus system redesign 
# (often referred to as the "System Reimagining").

library(tidytransit)

# GTFS feeds obtained from transitfeeds.com
# The “before” dataset represents service from the Spring 2015 service period (weekday) - 1/25/15 thru 6/6/15 -
# and the “after” dataset represents service from the Spring 2016 service period (weekday) - 1/24/16 thru 5/28/16.
# The actual system service change took place on August 16, 2015.
# More information about System Reimagining is available here: 
# https://www.ridemetro.org/Pages/Reimagining.aspx

pre_sr_gtfs <- read_gtfs("data/20150323_htx.zip", 
                         local = TRUE,
                         geometry = TRUE,
                         frequency = TRUE)

post_sr_gtfs <- read_gtfs("data/20160321_htx.zip", 
                         local = TRUE,
                         geometry = TRUE,
                         frequency = TRUE)



