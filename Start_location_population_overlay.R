library(sf)
library(tidyverse)
library(bbsBayes2)
library(readxl)

# load the 2021 population census data ------------------------------------
# https://open.canada.ca/data/en/dataset/1b3653d7-a48e-4001-8046-e6964bebe286
pop <- read_csv("2021_92-151_X.csv",
                locale = locale(encoding = "latin1"))

# check that total sum of population is approximately 36,991,981
# reflecting the total population of Canada
# https://www12.statcan.gc.ca/census-recensement/2021/dp-pd/prof/details/page.cfm?Lang=E&DGUIDList=2021A000011124&GENDERList=1,2,3&STATISTICList=1,4&HEADERList=0&SearchText=Canada
#
sum(pop$DBPOP2021_IDPOP2021,na.rm = TRUE)
#

pop1 <- pop %>%
  group_by(CSDNAME_SDRNOM,DARPLAT_ADLAT,DARPLONG_ADLONG,
           DARPLAMX_ADLAMX,DARPLAMY_ADLAMY,
           PRNAME_PRNOM,DAUID_ADIDU) %>%
  summarise(population = sum(DBPOP2021_IDPOP2021),
            area = sum(DBAREA2021_IDSUP2021))

sum(pop1$population,na.rm = TRUE)

pop2 <- pop %>%
  group_by(CSDNAME_SDRNOM,
           #DARPLONG_ADLAT,DARPLONG_ADLONG,
           #DARPLAMX_ADLAMX,DARPLAMY_ADLAMY,DAUID_ADIDU
           PRNAME_PRNOM) %>%
  summarise(population = sum(DBPOP2021_IDPOP2021),
            area = sum(DBAREA2021_IDSUP2021),
            DARPLAMY_ADLAMY = mean(DARPLAMY_ADLAMY),
            DARPLAMX_ADLAMX = mean(DARPLAMX_ADLAMX))

sum(pop2$population,na.rm = TRUE)

pop_map <- st_as_sf(pop2,
                    coords = c("DARPLAMX_ADLAMX","DARPLAMY_ADLAMY"))
pop_map <- st_set_crs(pop_map, 3348)



# Distance to major centre ------------------------------------------------

maj_cent <- pop_map %>%
  filter(population >10000)
tst <- ggplot()+
  geom_sf(data = maj_cent,
          aes(colour = population),
          alpha = 0.2)
tst

ca_map <- bbsBayes2::load_map("bbs") %>%
  filter(country == "Canada") %>%
  summarise()

degree_blocks <- bbsBayes2::load_map("latlong")

db_map_can <- degree_blocks %>%
  sf::st_join(ca_map,
              left = FALSE)



maj_cent_repro <- maj_cent %>%
  sf::st_transform(crs = st_crs(degree_blocks))


# load bbs route information ----------------------------------------------

route_paths <- sf::read_sf("data/gis/AllRoutes2024.shp") %>%
  group_by(Nbr_FullNa,route,Active,
           Province,ProvRoute) %>%
  summarise() %>%
  st_transform(crs = st_crs(degree_blocks))



re_calc <- FALSE
if(re_calc){
db_distance <- st_distance(x = db_map_can,
                           y = maj_cent_repro) %>%
  as_tibble() %>%
  rowwise() %>%
  mutate(km_to_city = min(as.numeric(c_across(V1:V429)))/1000)

db_distance1 <- db_distance %>%
  select(km_to_city)

saveRDS(db_distance,"db_distance.rds")

saveRDS(db_distance1,"db_distance1.rds")



rt_distance <- st_distance(x = route_paths,
                           y = maj_cent_repro) %>%
  as_tibble() %>%
  rowwise() %>%
  mutate(km_to_city = min(as.numeric(c_across(V1:V429)))/1000)

rt_distance1 <- rt_distance %>%
  select(km_to_city)

saveRDS(rt_distance,"rt_distance.rds")

saveRDS(rt_distance1,"rt_distance1.rds")



}else{
  db_distance <- readRDS("db_distance.rds")

  db_distance1 <- readRDS("db_distance1.rds")

  rt_distance <- readRDS("rt_distance.rds")

  rt_distance1 <- readRDS("rt_distance1.rds")
}



db_distance_map <- db_map_can %>%
    bind_cols(db_distance1) %>%
    sf::st_join(ca_map,
                left = FALSE)


rt_distance_map <- route_paths %>%
  bind_cols(rt_distance1)


distance_map <- ggplot()+
  geom_sf(data = db_distance_map,
          aes(fill = km_to_city),
          alpha = 0.3)+
  # geom_sf(data = ca_map,
  #         fill = NA,
  #         colour = grey(0.6))+
  geom_sf(data = maj_cent_repro,
          alpha = 0.2)+
  geom_sf(data = rt_distance_map,
          aes(colour = km_to_city))+
  geom_sf_text(data = route_paths,
               aes(label = ProvRoute),
               size = 2)+
  scale_fill_viridis_b(breaks = c(0,100,200,300,400,500,1000,3000),
                       direction = -1,
                       name = "km to city > 10,000",
                       aesthetics = c("fill","colour"))

pdf("Degree Block Distance to major centre.pdf",
    width = 17,
    height = 14)
print(distance_map)
dev.off()



# Population within distance of routes ------------------------------------

route_paths_buffers_100k <- route_paths %>%
  st_buffer(1e5) # 100km buffer of route path

route_paths_buffers_200k <- route_paths %>%
  st_buffer(2e5) # 100km buffer of route path

route_paths_buffers_300k <- route_paths %>%
  st_buffer(3e5) # 100km buffer of route path

large_cent <- pop_map %>%
  filter(population >50000) %>%
  sf::st_transform(crs = st_crs(route_paths))

pop_map_repro <- pop_map %>%
  sf::st_transform(crs = st_crs(route_paths))




pop_100km_l <- route_paths_buffers_100k %>%
  st_join(y = pop_map_repro,
          largest = FALSE) %>%
  st_drop_geometry() %>%
  group_by(Nbr_FullNa,route,Active,
           Province,ProvRoute) %>%
  summarise(population = sum(population,na.rm = TRUE),
            distance = "100km")

pop_200km_l <- route_paths_buffers_200k %>%
  st_join(y = pop_map_repro,
          largest = FALSE) %>%
  st_drop_geometry() %>%
  group_by(Nbr_FullNa,route,Active,
           Province,ProvRoute) %>%
  summarise(population = sum(population,na.rm = TRUE),
            distance = "200km")


pop_300km_l <- route_paths_buffers_300k %>%
  st_join(y = pop_map_repro,
          largest = FALSE) %>%
  st_drop_geometry() %>%
  group_by(Nbr_FullNa,route,Active,
           Province,ProvRoute) %>%
  summarise(population = sum(population,na.rm = TRUE),
            distance = "300km")


pop_w_in_distance_long <- pop_100km_l %>%
  bind_rows(pop_200km_l) %>%
  bind_rows(pop_300km_l)



route_paths_pop_in_dist_long <- route_paths %>%
  left_join(pop_w_in_distance_long)

pop_thresholds <- c(0,1e4,5e4,1e5,2.5e5,5e7)



pop_w_in_distance_wide <- pop_w_in_distance_long %>%
  pivot_wider(id_cols = c(Nbr_FullNa,route,Active,Province,ProvRoute),
              names_from = distance,
              names_prefix = "Population_with_in_",
              values_from = population) %>%
  inner_join( sf::st_drop_geometry(route_paths)) %>%
  mutate(Population_category_100km = cut(Population_with_in_100km,
                                         include.lowest = TRUE,breaks = pop_thresholds),
         Population_category_200km = cut(Population_with_in_200km,
                                         include.lowest = TRUE,breaks = pop_thresholds),
         Population_category_300km = cut(Population_with_in_300km,
                                         include.lowest = TRUE,breaks = pop_thresholds))


# identifying which routes have data ------------------------------------


#bbsBayes2::fetch_bbs_data(include_unacceptable = TRUE, force = TRUE)
# saveRDS(load_bbs_data(),
#         "All_bbs_data_with_unacceptable.rds")
#
# bbsBayes2::fetch_bbs_data(include_unacceptable = FALSE, force = TRUE)

surveys <- readRDS("All_bbs_data_with_unacceptable.rds")$routes %>%
  filter(country_num == 124) %>%
  group_by(route,state_num) %>%
  summarise(first_year = min(year),
            most_recent_year = max(year),
            n_years = n(),
            n_years_since_2020 = length(which(year > 2020))) %>%
  mutate(rt = ifelse(route < 10,paste0("00",route),
                     NA),
         rt = ifelse(route < 100 & route > 9,paste0("0",route),
                     rt),
         rt = ifelse(route > 99,paste0(route),
                     rt),
         ProvRoute = as.integer(paste0(state_num,rt))) %>%
  ungroup() %>%
  select(-c(route,state_num,rt))

pop_w_in_distance_wide2 <- pop_w_in_distance_wide %>%
  left_join(surveys)


write_excel_csv(pop_w_in_distance_wide2,
                "BBS routes population within buffers of route paths.csv")


#
# pop_in_dist_map <- ggplot()+
#   geom_sf(data = ca_map, fill = NA)+
#   geom_sf(data = large_cent, size = 0.6)+
#   geom_sf(data = route_paths_pop_in_dist_long,
#           aes(colour = population),
#           alpha = 0.4)+
#   scale_colour_viridis_b(name = "Population within\ndistance of\nroute path",
#                          breaks = pop_thresholds,
#                          direction = 1,
#                          option = "turbo")+
#   geom_sf_text(data = route_paths_pop_in_dist_long,
#                aes(label = ProvRoute,
#                    colour = population),
#                size = 0.6)+
#   facet_grid(rows = vars(distance))+
#   labs(subtitle = paste("Total population within",i,"of BBS route paths. \nBlack dots show locations of population centres with > 50,000 people."))+
#   theme_bw()
#
#
#
# pdf("Routes population within distance.pdf",
#     width = 14,
#     height = 20)
# print(pop_in_dist_map)
# dev.off()

provs <- load_map("prov_state") %>%
  filter(country == "Canada")

pdf("Routes population within distance by page.pdf",
    width = 17,
    height = 14)
for(i in c("100km","200km","300km")){
  tmp <- route_paths_pop_in_dist_long %>%
    filter(distance == i)


  pop_in_dist_map <- ggplot()+
    geom_sf(data = provs, fill = NA)+
    geom_sf(data = large_cent, size = 0.6)+
    geom_sf(data = tmp,
            aes(colour = population),
            alpha = 0.4)+
    scale_colour_viridis_b(name = "Population within\ndistance of\nroute path",
                           breaks = pop_thresholds,
                           direction = 1,
                           option = "turbo")+
    geom_sf_text(data = tmp,
                 aes(label = ProvRoute,
                     colour = population),
                 size = 0.6)+
    facet_grid(rows = vars(distance))+
    labs(subtitle = paste("Total population within",i,"of BBS route paths. \nBlack dots show locations of population centres with > 50,000 people."))+
    theme_bw()
print(pop_in_dist_map)
}
dev.off()

















# Old code run with caution -----------------------------------------------

#
#
# routes_wide <- bbsBayes2::load_bbs_data()[["routes"]] %>%
#   filter(country == "CA") %>%
#   arrange(year) %>%
#   select(route_name,state_num,route,year,obs_n) %>%
#   pivot_wider(names_from = year,
#               values_from = obs_n,
#               names_prefix = "Surveyed_by_in_")
#
#
# routes5 <- bbsBayes2::load_bbs_data()[["routes"]] %>%
#   filter(country == "CA",
#          year > 2017) %>%
#   group_by(route_name,state_num,route) %>%
#   summarise(n_surveys_since_2018 = n())
#
#
# routes <- bbsBayes2::load_bbs_data()[["routes"]] %>%
#   filter(country == "CA") %>%
#   group_by(route_name,state_num,route,
#            st_abrev,country,bcr,
#            latitude,longitude) %>%
#   summarise(n_surveys_total = n()) %>%
#   left_join(routes5) %>%
#   left_join(routes_wide) %>%
#   mutate(long = longitude,
#          lat = latitude) %>%
#   st_as_sf(coords = c("long","lat"))
#
# routes <- st_set_crs(routes, 4269) %>%
#   st_transform(crs = st_crs(pop_map))
#
#
# tst <- ggplot()+
#   geom_sf(data = routes,
#           aes(colour = st_abrev),
#           inherit.aes = FALSE)+
#   geom_sf(data = maj_cent,
#           alpha = 0.2)
# tst
#
# route_buffers_100km <- st_buffer(routes,
#                                  dist = 100000)
#
# route_buffers_200km <- st_buffer(routes,
#                                  dist = 200000)
#
# route_buffers_300km <- st_buffer(routes,
#                                  dist = 300000)
#
#
# tst <- ggplot()+
#   geom_sf(data = route_buffers_300km,
#           aes(colour = st_abrev),
#           fill = NA)+
#   geom_sf(data = maj_cent,
#           alpha = 0.2)
# tst
#
# pop_100km <- route_buffers_100km %>%
#   st_join(y = pop_map,
#           largest = FALSE) %>%
#   st_drop_geometry() %>%
#   group_by(route_name,state_num,route,
#            st_abrev,country) %>%
#   summarise(population_100km = sum(population,na.rm = TRUE))
#
# # tst <- ggplot()+
# #   geom_sf(data = pop_100km,
# #           aes(fill = population_100km),
# #           alpha = 0.2)+
# #   geom_sf(data = maj_cent,
# #           alpha = 0.2)
# # tst
#
#
# pop_200km <- route_buffers_200km %>%
#   st_join(y = pop_map,
#           largest = FALSE) %>%
#   st_drop_geometry() %>%
#   group_by(route_name,state_num,route,
#            st_abrev,country) %>%
#   summarise(population_200km = sum(population,na.rm = TRUE))
#
#
# pop_300km <- route_buffers_300km %>%
#   st_join(y = pop_map,
#           largest = FALSE) %>%
#   st_drop_geometry() %>%
#   group_by(route_name,state_num,route,
#            st_abrev,country) %>%
#   summarise(population_300km = sum(population,na.rm = TRUE))
#
#
# pop_summaries <- pop_100km %>%
#   full_join(pop_200km) %>%
#   full_join(pop_300km)
#
# q100km <- quantile(pop_summaries$population_100km,0.05)
# q200km <- quantile(pop_summaries$population_200km,0.05)
# q300km <- quantile(pop_summaries$population_300km,0.05)
#
# pop_summaries <- pop_summaries %>%
#   mutate(pop_5th_percentile_100km = ifelse(population_100km < q100km,
#                                            TRUE,FALSE),
#          pop_5th_percentile_200km = ifelse(population_200km < q200km,
#                                            TRUE,FALSE),
#          pop_5th_percentile_300km = ifelse(population_300km < q300km,
#                                            TRUE,FALSE))
#
#
# routes_wpop <- routes %>%
#   full_join(pop_summaries)
#
#
# tst <- ggplot()+
#   geom_sf(data = routes_wpop,
#           aes(colour = pop_5th_percentile_100km),
#           alpha = 0.2)+
#   scale_colour_viridis_d(direction = -1)
# tst
#
# tst <- ggplot()+
#   geom_sf(data = routes_wpop,
#           aes(colour = pop_5th_percentile_200km),
#           alpha = 0.2)+
#   scale_colour_viridis_d(direction = -1)
# tst
#
# routes_out <- routes %>%
#   st_drop_geometry()
#
# route_summary <- pop_summaries %>%
#   left_join(routes_out) %>%
#   arrange(st_abrev,population_100km)
#
# write_excel_csv(route_summary,"routes_w_population_in_3_buffers.csv")
