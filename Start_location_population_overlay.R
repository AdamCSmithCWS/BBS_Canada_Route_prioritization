library(sf)
library(tidyverse)
library(bbsBayes2)


# load the 2021 population census data ------------------------------------
# https://open.canada.ca/data/en/dataset/1b3653d7-a48e-4001-8046-e6964bebe286
pop <- read_csv("2021_92-151_X.csv")

pop <- pop %>%
  select(DARPLAT_ADLAT,DARPLONG_ADLONG,
         DARPLAMX_ADLAMX,DARPLAMY_ADLAMY,
         DBPOP2021_IDPOP2021,DBAREA2021_IDSUP2021,
         PRNAME_PRNOM,DAUID_ADIDU)
# check that total sum of population is approximately 36,991,981
# reflecting the total population of Canada
# https://www12.statcan.gc.ca/census-recensement/2021/dp-pd/prof/details/page.cfm?Lang=E&DGUIDList=2021A000011124&GENDERList=1,2,3&STATISTICList=1,4&HEADERList=0&SearchText=Canada
#
sum(pop$DBPOP2021_IDPOP2021,na.rm = TRUE)
#

pop <- pop %>%
  group_by(DARPLAT_ADLAT,DARPLONG_ADLONG,
           DARPLAMX_ADLAMX,DARPLAMY_ADLAMY,
           PRNAME_PRNOM,DAUID_ADIDU) %>%
  summarise(population = sum(DBPOP2021_IDPOP2021),
            area = sum(DBAREA2021_IDSUP2021))

sum(pop$population,na.rm = TRUE)

pop_map <- st_as_sf(pop,
                    coords = c("DARPLAMX_ADLAMX","DARPLAMY_ADLAMY"))
pop_map <- st_set_crs(pop_map, 3348)


maj_cent <- pop_map %>%
  filter(population >1000)
tst <- ggplot()+
  geom_sf(data = maj_cent,
          aes(colour = population),
          alpha = 0.2)
tst



# load bbs route information ----------------------------------------------

routes_wide <- bbsBayes2::load_bbs_data()[["routes"]] %>%
  filter(country == "CA") %>%
  arrange(year) %>%
  select(route_name,state_num,route,year,obs_n) %>%
  pivot_wider(names_from = year,
           values_from = obs_n,
           names_prefix = "Surveyed_by_in_")


routes5 <- bbsBayes2::load_bbs_data()[["routes"]] %>%
  filter(country == "CA",
         year > 2017) %>%
  group_by(route_name,state_num,route) %>%
  summarise(n_surveys_since_2018 = n())


routes <- bbsBayes2::load_bbs_data()[["routes"]] %>%
  filter(country == "CA") %>%
  group_by(route_name,state_num,route,
           st_abrev,country,bcr,
         latitude,longitude) %>%
  summarise(n_surveys_total = n()) %>%
  left_join(routes5) %>%
  left_join(routes_wide) %>%
  mutate(long = longitude,
         lat = latitude) %>%
  st_as_sf(coords = c("long","lat"))

routes <- st_set_crs(routes, 4269) %>%
  st_transform(crs = st_crs(pop_map))


tst <- ggplot()+
  geom_sf(data = routes,
          aes(colour = st_abrev),
          inherit.aes = FALSE)+
  geom_sf(data = maj_cent,
          alpha = 0.2)
tst

route_buffers_100km <- st_buffer(routes,
                                dist = 100000)

route_buffers_200km <- st_buffer(routes,
                                dist = 200000)

route_buffers_300km <- st_buffer(routes,
                                dist = 300000)


tst <- ggplot()+
  geom_sf(data = route_buffers_300km,
          aes(colour = st_abrev),
          fill = NA)+
  geom_sf(data = maj_cent,
          alpha = 0.2)
tst

pop_100km <- route_buffers_100km %>%
  st_join(y = pop_map,
          largest = FALSE) %>%
  st_drop_geometry() %>%
  group_by(route_name,state_num,route,
           st_abrev,country) %>%
  summarise(population_100km = sum(population,na.rm = TRUE))

  # tst <- ggplot()+
  #   geom_sf(data = pop_100km,
  #           aes(fill = population_100km),
  #           alpha = 0.2)+
  #   geom_sf(data = maj_cent,
  #           alpha = 0.2)
  # tst


pop_200km <- route_buffers_200km %>%
  st_join(y = pop_map,
          largest = FALSE) %>%
  st_drop_geometry() %>%
  group_by(route_name,state_num,route,
           st_abrev,country) %>%
  summarise(population_200km = sum(population,na.rm = TRUE))


pop_300km <- route_buffers_300km %>%
  st_join(y = pop_map,
          largest = FALSE) %>%
  st_drop_geometry() %>%
  group_by(route_name,state_num,route,
           st_abrev,country) %>%
  summarise(population_300km = sum(population,na.rm = TRUE))


pop_summaries <- pop_100km %>%
  full_join(pop_200km) %>%
  full_join(pop_300km)

q100km <- quantile(pop_summaries$population_100km,0.05)
q200km <- quantile(pop_summaries$population_200km,0.05)
q300km <- quantile(pop_summaries$population_300km,0.05)

pop_summaries <- pop_summaries %>%
  mutate(pop_5th_percentile_100km = ifelse(population_100km < q100km,
                                            TRUE,FALSE),
         pop_5th_percentile_200km = ifelse(population_200km < q200km,
                                            TRUE,FALSE),
         pop_5th_percentile_300km = ifelse(population_300km < q300km,
                                            TRUE,FALSE))


routes_wpop <- routes %>%
  full_join(pop_summaries)


tst <- ggplot()+
  geom_sf(data = routes_wpop,
          aes(colour = pop_5th_percentile_100km),
          alpha = 0.2)+
  scale_colour_viridis_d(direction = -1)
tst

tst <- ggplot()+
  geom_sf(data = routes_wpop,
          aes(colour = pop_5th_percentile_200km),
          alpha = 0.2)+
  scale_colour_viridis_d(direction = -1)
tst

routes_out <- routes %>%
  st_drop_geometry()

route_summary <- pop_summaries %>%
  left_join(routes_out) %>%
  arrange(st_abrev,population_100km)

write_excel_csv(route_summary,"routes_w_population_in_3_buffers.csv")
