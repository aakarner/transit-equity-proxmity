#[POPULATION WEIGHTED MEAN CALCULATIONS - ADVANCED METHODS]
#[For more advanced analysis above and beyond the traditional transit proximity approach, this
#script also includes code that generates population weighted mean departures and headways for
#each demographic category before and after the service change. The simple analysis does not 
#take frequency or amount of service into consideration, it simply considers a binary indicator of 
#access to transit or not. As such, the simple analysis does not present an indication of service loss
#for someone who lives within 400 meters of a bus stop that before had an average frequency of 
#15 minutes per day and after has an average frequency of 60 minutes per day. In reality, this is a 
#huge reduction of service for these individuals and should be reflected as a disparate impact to 
#this population in an equity analysis. The more advanced scripts are optional and are shown in 
#brackets in this documentation.] 

library(sf)
library(ggplot2)
library(ggthemes)
library(ggmap)
library(maps)
library(mapdata)
library(tmaptools)

#White
#Population weighted mean metrics for entire system/population
pre_wmd_white <- weighted.mean(pre_stop_acs_int_white$departures, pre_stop_acs_int_white$pop_est_new, na.rm = FALSE)
#pre_wmd_white #population weighted mean before = 45.50 departures/day
pre_wmh_white <- weighted.mean(pre_stop_acs_int_white$headway, pre_stop_acs_int_white$pop_est_new, na.rm = FALSE)
#pre_wmh_white #population weighted mean headway before = 31.35 minutes of those within 400 m
post_wmd_white <- weighted.mean(post_stop_acs_int_white$departures, post_stop_acs_int_white$pop_est_new, na.rm = FALSE)
#post_wmd_white #population weighted mean after = 49.13 departures/day
post_wmh_white <- weighted.mean(post_stop_acs_int_white$headway, post_stop_acs_int_white$pop_est_new, na.rm = FALSE)
#post_wmh_white #population weighted mean headway before = 28.98 minutes of those within 400 m

#Black
#Population weighted mean metrics for entire system/population
pre_wmd_black <- weighted.mean(pre_stop_acs_int_black$departures, pre_stop_acs_int_black$pop_est_new, na.rm = FALSE)
#pre_wmd_black #population weighted mean before = 42.08 departures/day
pre_wmh_black <- weighted.mean(pre_stop_acs_int_black$headway, pre_stop_acs_int_black$pop_est_new, na.rm = FALSE)
#pre_wmh_black #population weighted mean headway before = 35.68 minutes of those within 400 m
post_wmd_black <- weighted.mean(post_stop_acs_int_black$departures, post_stop_acs_int_black$pop_est_new, na.rm = FALSE)
#post_wmd_black #population weighted mean after = 42.14 departures/day
post_wmh_black <- weighted.mean(post_stop_acs_int_black$headway, post_stop_acs_int_black$pop_est_new, na.rm = FALSE)
#post_wmh_black #population weighted mean headway before = 33.06 minutes of those within 400 m

#Asian
#Population weighted mean metrics for entire system/population
pre_wmd_asian <- weighted.mean(pre_stop_acs_int_asian$departures, pre_stop_acs_int_asian$pop_est_new, na.rm = FALSE)
#pre_wmd_asian #population weighted mean before = 45.07 departures/day
pre_wmh_asian <- weighted.mean(pre_stop_acs_int_asian$headway, pre_stop_acs_int_asian$pop_est_new, na.rm = FALSE)
#pre_wmh_asian #population weighted mean headway before = 31.00 minutes of those within 400 m

post_wmd_asian <- weighted.mean(post_stop_acs_int_asian$departures, post_stop_acs_int_asian$pop_est_new, na.rm = FALSE)
#post_wmd_asian #population weighted mean after = 49.48 departures/day
post_wmh_asian <- weighted.mean(post_stop_acs_int_asian$headway, post_stop_acs_int_asian$pop_est_new, na.rm = FALSE)
#post_wmh_asian #population weighted mean headway before = 27.93 minutes of those within 400 m

#Latinx
#Population weighted mean metrics for entire system/population
pre_wmd_latin <- weighted.mean(pre_stop_acs_int_latin$departures, pre_stop_acs_int_latin$pop_est_new, na.rm = FALSE)
#pre_wmd_latin #population weighted mean before = 40.76 departures/day
pre_wmh_latin <- weighted.mean(pre_stop_acs_int_latin$headway, pre_stop_acs_int_latin$pop_est_new, na.rm = FALSE)
#pre_wmh_latin #population weighted mean headway before = 33.89 minutes of those within 400 m
post_wmd_latin <- weighted.mean(post_stop_acs_int_latin$departures, post_stop_acs_int_latin$pop_est_new, na.rm = FALSE)
#post_wmd_latin #population weighted mean after = 42.42 departures/day
post_wmh_latin <- weighted.mean(post_stop_acs_int_latin$headway, post_stop_acs_int_latin$pop_est_new, na.rm = FALSE)
#post_wmh_latin #population weighted mean headway before = 32.53 minutes of those within 400 m

#PLOT RESULTS IN BAR CHARTS
data_wmh <-c(pre_wmh_white, post_wmh_white,pre_wmh_black, post_wmh_black, 
             pre_wmh_asian, post_wmh_asian, pre_wmh_latin, post_wmh_latin)
data_wmd <-c(pre_wmd_white, post_wmd_white,pre_wmd_black, post_wmd_black, 
             pre_wmd_asian, post_wmd_asian, pre_wmd_latin, post_wmd_latin)
before_after <-c('Before', 'After', 'Before', 'After', 'Before', 'After', 'Before', 'After')
race <-c('White', 'White', 'Black', 'Black', 'Asian', 'Asian', 'LatinX', 'LatinX')

final.data_bystop <-data.frame(race, before_after, data_wmh, data_wmd)
final.data_bystop

plot_wmh <- barplot(final.data_bystop$data_wmh, names.arg = final.data_bystop$race, xlab = "Race", 
                    ylab = "Weighted Mean Headway (Minutes)", 
                    col = c("red","green"), 
                    main = "Weighted Average Transit Headway By Race Before/After System Reimagining", 
                    border = "black",
                    ylim = c(0, 46))
legend("topright", c("Before", "After"), fill = c("red", "green"))
text(plot_wmh, final.data_bystop$data_wmh + 2*sign(final.data_bystop$data_wmh), labels=round(final.data_bystop$data_wmh, 2), xpd=TRUE)

plot_wmd <- barplot(final.data_bystop$data_wmd, names.arg = final.data_bystop$race, xlab = "Race", 
                    ylab = "Weighted Mean Daily Departures", 
                    col = c("orange","blue"), 
                    main = "Weighted Average Transit Departures By Race Before/After System Reimagining", 
                    border = "black",
                    ylim = c(0, 60))
legend("topright", c("Before", "After"), fill = c("orange", "blue"))
text(plot_wmd, final.data_bystop$data_wmd + 2*sign(final.data_bystop$data_wmd), labels=round(final.data_bystop$data_wmd, 2), xpd=TRUE)






#_________________________________________________________________________________________

#text(plot_wmd, final.data_popsums$pop_est_by_race + 2*sign(final.data_popsums$pop_est_by_race), 
#     labels=round(final.data_popsums$pop_est_by_race, 2), xpd=TRUE)





#remove rows with duplicate geometries
#index_white_pre <- which(duplicated(pre_stop_acs_int_white$geometry))
#white_nodup_pre <- pre_stop_acs_int_white[-index_white_pre, ]
#index_white_post <- which(duplicated(post_stop_acs_int_white$geometry))
#white_nodup_post <- post_stop_acs_int_white[-index_white_post, ]

#index_black_pre <- which(duplicated(pre_stop_acs_int_black$geometry))
#black_nodup_pre <- pre_stop_acs_int_black[-index_black_pre, ]
#index_black_post <- which(duplicated(post_stop_acs_int_black$geometry))
#black_nodup_post <- post_stop_acs_int_black[-index_black_post, ]

#index_asian_pre <- which(duplicated(pre_stop_acs_int_asian$geometry))
#asian_nodup_pre <- pre_stop_acs_int_asian[-index_asian_pre, ]
#index_asian_post <- which(duplicated(post_stop_acs_int_asian$geometry))
#asian_nodup_post <- post_stop_acs_int_asian[-index_asian_post, ]

#index_latin_pre <- which(duplicated(pre_stop_acs_int_latin$geometry))
#latin_nodup_pre <- pre_stop_acs_int_latin[-index_latin_pre, ]
#index_latin_post <- which(duplicated(post_stop_acs_int_latin$geometry))
#latin_nodup_post <- post_stop_acs_int_latin[-index_latin_post, ]

#Put population sum data in dataframe
#pop_est_by_race <-c(sum(white_nodup_pre$pop_est_new), sum(white_nodup_post$pop_est_new), 
#                    sum(black_nodup_pre$pop_est_new), sum(black_nodup_post$pop_est_new), 
#                    sum(asian_nodup_pre$pop_est_new), sum(asian_nodup_post$pop_est_new), 
#                    sum(latin_nodup_pre$pop_est_new), sum(latin_nodup_post$pop_est_new))
#before_after_1 <-c('Before', 'After', 'Before', 'After', 'Before', 'After', 'Before', 'After')
#race_1 <-c('White', 'White', 'Black', 'Black', 'Asian', 'Asian', 'LatinX', 'LatinX')

#final.data_popsums <-data.frame(race_1, before_after_1, pop_est_by_race)

#final.data_popsums

#plot population sum data
#plot_popsums <- barplot(as.numeric(final.data_popsums$pop_est_by_race), 
#                    names.arg = final.data_popsums$race_1, 
#                    xlab = "Race", 
#                    ylab = "Total Population within 400m of Bus Stop", 
#                    col = c("yellow","pink"), 
#                    main = "Population By Race w/in 400m of Bus Stop Before/After System Reimagining", 
#                    border = "black",
#                    ylim = c(0,5000000))
#legend("topleft", c("Before", "After"), fill = c("yellow", "pink"))
#text(plot_wmd, final.data_popsums$pop_est_by_race + 2*sign(final.data_popsums$pop_est_by_race), 
#     labels=round(final.data_popsums$pop_est_by_race, 2), xpd=TRUE)