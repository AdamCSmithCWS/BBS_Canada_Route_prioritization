## mapping priority routes
##


library(sf)
library(tidyverse)
library(bbsBayes2)
library(readxl)
library(rnaturalearthhires)
library(maps)

routes <- st_read("data/gis/ALL_ROUTES2024.shp") |>
  group_by(ProvRoute,Active) |>
  summarise() |>
  st_make_valid()


priorities <- read_xlsx("data/Remote Route Funding prioritization.xlsx") |>
  rename(paid = `paid obs...14`,
         obs = `obs...15`,
         affiliation = `obs affiliation...16`,
         last_run = `last run`,
         filled = `current status`) |>
  mutate(rt = ifelse(route < 10,paste0("00",route),
                     NA),
         rt = ifelse(route < 100 & route > 9,paste0("0",route),
                     rt),
         rt = ifelse(route > 99,paste0(route),
                     rt),
         ProvRoute = as.integer(paste0(state_num,rt))) |>
  select(c(ProvRoute,
           route_name,
           state_num,
           route,
           st_abrev,
           starts_with("pop"),
           priority,
           last_run,
           paid,
           obs,
           affiliation,
           filled))


base <- load_map("prov_state") |>
  filter(country == "Canada")

route_map <- routes |>
  inner_join(priorities,
             by = "ProvRoute") |>
  st_transform(crs = st_crs(base))


priority_routes <- route_map |>
  filter(priority %in% c(1,2,3)) |>
  mutate(status = ifelse(filled == "vacant",
                         "vacant",NA),
         status = ifelse(is.na(status) &
                           paid == "y",
                         "paid + filled",
                         status),
         status = ifelse(is.na(status) &
                           paid != "y",
                         "filled",
                         status))

priority_route_centres <- priority_routes |>
  st_centroid()

# priority_routes_vacant <- priority_route_centres |>
#   filter(filled == "vacant")
#
# priority_routes_filled_paid <- priority_route_centres |>
#   filter(filled == "filled",
#          paid == "y")


cities <- maps::canada.cities |>
  filter((pop > 15000 & country.etc %in% c("NT",
                                          "YT",
                                          "NU")) |
           (pop > 15000) | capital > 0) |>
  st_as_sf(coords = c("long","lat"),
           crs = 4269) |>
  st_transform(crs = st_crs(base))

#
# mp <- ggplot()+
#   geom_sf(data = base,
#           fill = NA)+
#   geom_sf(data = cities,
#           size = 0.1)+
#   geom_sf(data = route_map,
#           colour = grey(0.8))+
#   geom_sf(data = priority_routes_filled_paid,
#           aes(colour = priority,
#               fill = priority),
#           linewidth = 1.5,
#           alpha = 0.6)+
#   geom_sf(data = priority_routes_vacant,
#           aes(colour = priority,
#               fill = priority),
#           linewidth = 1)+
#   geom_sf(data = priority_routes,
#           aes(colour = priority,
#               fill = priority))+
#   scale_colour_viridis_d(aesthetics = c("fill","colour"),
#                          end = 0.9)+
#   theme_bw()
#
# mp
#





mp <- ggplot()+
  geom_sf(data = base,
          fill = NA)+
  geom_sf(data = cities,
          size = 0.3)+
  geom_sf(data = route_map,
          colour = grey(0.8))+
  # geom_sf_text(data = priority_route_centres,
  #              aes(label = route,
  #                  colour = priority),
  #              size = 2,
  #              alpha = 0.7,
  #              position = position_nudge(x = 50000,y = 30000))+
  geom_sf(data = priority_route_centres,
          aes(colour = priority,
              fill = priority,
              shape = status),
          alpha = 0.5,
          size = 2)+
  geom_sf(data = priority_routes,
          aes(colour = priority,
              fill = priority))+
  scale_colour_viridis_d(aesthetics = c("fill","colour"),
                         end = 0.8)+
  xlab("")+
  ylab("")+
  labs(title = "Priority BBS routes in Canada",
       caption = "Coloured lines and points are priority routes. Black points are cities > 15,000 population. Grey lines are non-priority BBS route paths.")+
  theme_bw()

mp


pdf("priority_map.pdf",
    width = 11,
    height = 8.5)
print(mp)
dev.off()


