library(stringr)
library(scales)
library(broom)
library(tidyverse)
library(lubridate)
library(purrr)
library (tigris)
library(viridis)

#-------------------------------------------------------------------------------

#2020 storm data (as of 11/9/2020)
storms <- read_csv("data/StormEvents_details-ftp_v1.0_d2020_c20201017.csv.gz")

storms <- storms %>% 
  select(BEGIN_DATE_TIME, END_DATE_TIME, EPISODE_ID, EVENT_ID, STATE,
         STATE_FIPS, CZ_NAME, CZ_TYPE, CZ_FIPS, EVENT_TYPE, SOURCE,
         BEGIN_LAT, BEGIN_LON, END_LAT, END_LON) %>% 
  mutate(BEGIN_DATE_TIME = dmy_hms(BEGIN_DATE_TIME),
         END_DATE_TIME = dmy_hms(END_DATE_TIME),
         STATE = str_to_title(STATE),
         CZ_NAME = str_to_title(CZ_NAME)) %>% 
  filter(CZ_TYPE == "C") %>% 
  select(-CZ_TYPE) %>% 
  mutate(STATE_FIPS = str_pad(STATE_FIPS, 2, side = "left", pad = "0"),
         CZ_FIPS = str_pad(CZ_FIPS, 3, side = "left", pad = "0")) %>% 
  unite(fips, STATE_FIPS, CZ_FIPS, sep = "") %>% 
  rename_all(str_to_lower)

data("state")
us_state_info <- data_frame(state = state.name,
                            area = state.area,
                            region = state.region)

state_storms <- storms %>% 
  group_by(state) %>% 
  count() %>% 
  ungroup() %>% 
  right_join(us_state_info, by = "state")

storm_plot <- ggplot(state_storms, aes(x = area, y = n)) + 
  geom_point(aes(color = region)) + 
  labs(x = "Land area (square miles)", 
       y = "# of storm events in 2020") +
  scale_x_continuous(label = comma) +
  scale_y_continuous(label = comma)

storm_plot

#Making Map#--------------------------------------------------------------------

co_counties <- counties(state = "CO", cb = TRUE, class = "sf")

co_event_counts <- storms %>% 
  filter(state == "Colorado") %>% 
  filter(event_type %in% c("Tornado", "Heavy Rain", "Hail")) %>% 
  group_by(fips, event_type) %>% 
  count() %>% 
  ungroup()

co_county_events <- co_counties %>% 
  mutate(fips = paste0(STATEFP, COUNTYFP)) %>% 
  right_join(co_event_counts, by = "fips") 

ggplot() +
  geom_sf(data = co_counties, color = "lightgrey") +
  geom_sf(data = co_county_events, aes(fill = n)) +
  scale_fill_viridis(name = "Number of events\n(2020)") +
  theme(legend.position = "bottom") +
  facet_wrap(~ event_type, nrow = 1) +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())
  

