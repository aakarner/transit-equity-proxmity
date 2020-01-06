library(tidytransit)
library(tidycensus)
library(sf)
library(ggplot2)
library(tidyr)
library(forcats)

# Get demographic data ---------------------------------------------------------

# Specify counties encompassing the transit system service area
metro_svc_area <- c("Harris County", "Fort Bend County")

# Table B03002 - Hispanic or Latino origin by race
hlstatusvars <- c("B03002_003", # white alone
                  "B03002_004", # Black alone
                  "B03002_006", # Asian
                  "B03002_012") # Hispanic or Latino

metro_tracts <- get_acs(geography = "tract", 
                         variables = hlstatusvars, 
                         state = "TX", 
                         year = 2015,
                         county = metro_svc_area,
                         geometry = TRUE,
                         summary_var = "B03002_001") %>%
                  st_transform(32139) %>%
                  mutate(orig_area = units::drop_units(st_area(.)),
                         variable = 
                           factor(
                             variable,
                             labels = c("white", "Black", "Asian", "Latinx")))

# Summarize demographics -------------------------------------------------------

# Service area
# Dissolve over all routes to create a single feature representing the service 
# area before and after the service change
svc_area_sf <- gtfs %>% 
  group_by(when) %>%
  summarize() %>%
  st_intersection(metro_tracts) %>%
  mutate(area = units::drop_units(st_area(.)),
         share = area / orig_area,
         pop_new = estimate * share,
         tot_new = summary_est * share)

svc_area_demogs <- svc_area_sf %>%
  group_by(variable, when) %>%
  st_set_precision(30) %>%
  summarize(grouptot = sum(pop_new),
            totpop = sum(tot_new))

# Route level 
demogs_int <- st_intersection(gtfs, metro_tracts) %>%
  mutate(area = units::drop_units(st_area(.)),
         share = area / orig_area,
         pop_new = estimate * share,
         tot_new = summary_est * share)
  
route_demogs <- demogs_int %>%
  group_by(route_id, variable, route_short_name, when) %>%
  st_set_precision(30) %>% # Needed to avoid geometry errors
  summarize(grouptot = sum(pop_new),
            totpop = sum(tot_new),
            share = grouptot / totpop,
            total_departures = first(total_departures),
            median_headway = first(median_headways),
            mean_headway = first(mean_headways), 
            stdev_headways = first(st_dev_headways))

# High-frequency routes
hi_freq_sf <- gtfs %>% 
  mutate(hi_freq = ifelse(median_headways < 16, TRUE, FALSE)) %>%
  group_by(when, hi_freq) %>%
  summarize() %>%
  st_intersection(metro_tracts) %>%
  mutate(area = units::drop_units(st_area(.)),
         share = area / orig_area,
         pop_new = estimate * share,
         tot_new = summary_est * share)

hi_freq_demogs <- hi_freq_sf %>%
  ungroup() %>% 
  group_by(variable, hi_freq, when) %>%
  summarize(grouptot = sum(pop_new),
            totpop = sum(tot_new))

# Service cuts and additions
sr_before <- filter(gtfs, when == "pre")
sr_after <- st_drop_geometry(gtfs) %>% filter(when == "post")

sr_same <- inner_join(sr_before, sr_after, by = c("route_short_name")) %>%
  mutate(change_type = "none", 
         delta_hdwy = median_headways.y - median_headways.x,
         better_route = ifelse(delta_hdwy < 0, TRUE, FALSE)) 
sr_cut <- anti_join(sr_before, sr_after, 
                    by = c("route_short_name")) %>%
          mutate(change_type = "cut")
sr_new <- anti_join(filter(gtfs, when == "post"), 
                    st_drop_geometry(sr_before), by = c("route_short_name")) %>%
          mutate(change_type = "add")

changes_sf <- rbind(sr_cut, sr_new) %>%
  # group_by(change_type) %>%
  # summarize() %>%
  st_intersection(metro_tracts) %>%
  mutate(area = units::drop_units(st_area(.)),
         share = area / orig_area,
         pop_new = estimate * share,
         tot_new = summary_est * share)

changes_demogs <- changes_sf %>%
  group_by(route_short_name, change_type, variable) %>%
  summarize(grouptot = sum(pop_new),
            totpop = sum(tot_new)) %>%
  st_drop_geometry() %>%
  spread(variable, grouptot) %>%
  mutate(white_shr = white / totpop,
         poc_shr = (totpop - white) / totpop) %>%
  arrange(desc(poc_shr))



# Visualize results ------------------------------------------------------------

# Population with proximity
proximity_any <- st_drop_geometry(svc_area_demogs)

ggplot(proximity_any) + 
  geom_col(aes(x = fct_reorder(variable, -grouptot),
               y = grouptot, 
               color = fct_rev(when), 
               fill = fct_rev(when)), 
           position = "dodge") + 
  xlab(NULL) + ylab("total population in proximity of public transit") + 
  scale_fill_brewer(palette = "Dark2") + scale_color_brewer(palette = "Dark2") +
  theme_bw() + 
  theme(legend.position = "bottom",
        legend.title = element_blank()) 

ggsave("output/proximity_any.png", height = 4, width = 6)

# Population with high-frequency proximity
proximity_hi <- st_drop_geometry(hi_freq_demogs)
  
ggplot(filter(proximity_hi, hi_freq == TRUE)) + 
  geom_col(aes(x = fct_reorder(variable, -grouptot),
               y = grouptot, 
               color = fct_rev(when), 
               fill = fct_rev(when)), 
           position = "dodge") + 
  # facet_wrap(~ hi_freq) + 
  xlab(NULL) + ylab("total population in proximity of public transit") + 
  scale_fill_brewer(palette = "Dark2") + scale_color_brewer(palette = "Dark2") +
  theme_bw() + 
  theme(legend.position = "bottom",
        legend.title = element_blank()) 

ggsave("output/proximity_hi.png", height = 4, width = 6)

# Route-level analysis
ggplot(changes_demogs) + 
  geom_point(aes(x = poc_shr, 
                 y = fct_reorder(route_short_name, poc_shr),
                 color = change_type)) + 
  scale_fill_brewer(palette = "Dark2") + scale_color_brewer(palette = "Dark2") +
  ylab("route number") + xlab("share of people of color within buffers") + 
  theme_bw() + 
  theme(legend.position = "bottom") + 
  guides(color = guide_legend(title="type of service change"))
  
ggsave("output/route_level_changes.png", height = 5, width = 6)

ggplot() + 
  geom_sf(data = sr_cut, color = "red", fill = "red", alpha = 0.5) + 
  geom_sf(data = sr_new, color = "blue", fill = "blue", alpha = 0.5)





%>%
  select(route_short_name, delta_hdwy) %>%
  arrange(delta_hdwy)

ggplot(sr_summary) + geom_sf(aes(color = better_route, fill = better_route))

# Weekend example
    
#plot population sum data

barplot(final.data_popsums$pop_numeric, 
        names.arg = final.data_popsums$race_1, 
        xlab = "Race", 
        ylab = "Total Population within 400m of Bus Stop", 
        col = c("yellow","pink"), 
        main = "Population By Race w/in 400m of Bus Stop Before/After System Reimagining", 
        border = "black",
        ylim = c(0,750000))
legend("topleft", c("Before", "After"), fill = c("yellow", "pink"))

# END OF SIMPLE SYSTEM-WIDE ACCESSIBILITY PROXIMITY ANALYSIS

