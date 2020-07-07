library(tidyverse)
library(sf)
library(ggmap)

districts <- read_sf("MO_2010_Census_Voting_Districts_shp/MO_2010_Census_Voting_Districts_shp.shp")
districts_3857 <- st_transform(districts, 3857)

toplot <- districts_3857 %>%
  filter(COUNTYFP10 == "510")
bbox_to_plot <- toplot %>%
  st_bbox %>%
  st_as_sfc %>%
  st_transform(4326) %>%
  st_bbox
names(bbox_to_plot) <- c("left", "bottom", "right", "top")

zoom_level <- (abs(bbox_to_plot[1] - bbox_to_plot[3]) +
  abs(bbox_to_plot[2] - bbox_to_plot[4])) %>% unname
# All of MO is nice at 7, STL is nice at 12
zoom_level <- scales::rescale(zoom_level, to = c(12, 7), from = c(0, 12)) %>% round

map <- get_stamenmap(bbox = bbox_to_plot, zoom = zoom_level, maptype = "toner")

# Define a function to fix the bbox to be in EPSG:3857
# FROM https://stackoverflow.com/questions/47749078/how-to-put-a-geom-sf-produced-map-on-top-of-a-ggmap-produced-raster
ggmap_bbox <- function(map) {
  if (!inherits(map, "ggmap")) stop("map must be a ggmap object")
  # Extract the bounding box (in lat/lon) from the ggmap to a numeric vector, 
  # and set the names to what sf::st_bbox expects:
  map_bbox <- setNames(unlist(attr(map, "bb")), 
                       c("ymin", "xmin", "ymax", "xmax"))
  
  # Coonvert the bbox to an sf polygon, transform it to 3857, 
  # and convert back to a bbox (convoluted, but it works)
  bbox_3857 <- st_bbox(st_transform(st_as_sfc(st_bbox(map_bbox, crs = 4326)), 3857))
  
  # Overwrite the bbox of the ggmap object with the transformed coordinates 
  attr(map, "bb")$ll.lat <- bbox_3857["ymin"]
  attr(map, "bb")$ll.lon <- bbox_3857["xmin"]
  attr(map, "bb")$ur.lat <- bbox_3857["ymax"]
  attr(map, "bb")$ur.lon <- bbox_3857["xmax"]
  map
}

map_3857 <- ggmap_bbox(map)

ggmap(map_3857) + 
  coord_sf(crs = st_crs(3857)) +
  geom_sf(data = toplot,
          aes(fill = WHITE00 / POP00),
          inherit.aes = F,
          alpha = 0.8) +
  scale_fill_distiller("% popn white (2000 census)",
                       palette = "Spectral",
                       labels = scales::percent)

# Prototype choropleth with Cori Bush
usrep1_results <- read_csv("usrep1_dem_precinct_results.csv")
toplot_wvotes <- toplot %>%
  mutate(ward_precinct = str_remove_all(NAME10, ".* ")) %>%
  left_join(usrep1_results %>%
               filter(vote_category == "Total",
                      candidate == "Cori Bush") %>%
               mutate(ward_precinct = paste(ward, precinct, sep = "-")))

ggmap(map_3857) +
  coord_sf(crs = st_crs(3857)) +
  geom_sf(data = toplot_wvotes %>% filter(!is.na(votes)),
          aes(fill = votes / total_votes,
              alpha = total_votes),
          inherit.aes = F) +
  scale_fill_distiller("% vote share to Cori Bush",
                       palette = "Spectral",
                       labels = scales::percent) +
  scale_alpha_continuous("Total votes cast", range = c(0.5, 1)) +
  theme_void()

## Let's do some stupid modeling!
tomodel <- toplot_wvotes %>%
  as_tibble %>%
  filter(!is.na(votes)) %>%
  mutate(across(WHITE90:HISP90, ~ (.x / (POP90 + 1))),
         across(WHITE00:HISP00, ~ (.x / (POP00 + 1))),
         across(WHITE10:HISP10, ~ (.x / (POP10 + 1))),
         dBLACK90 = BLACK10 - BLACK90) %>%
  select(POP90:HISP10, dBLACK90, dBLACK00, votes) %>%
  select(-starts_with("POP"))

library(randomForest)

tomodel %>%
  ggplot(aes(BLACK10, votes, color = dBLACK00)) +
  geom_point() +
  scale_color_distiller(palette = "Spectral")

rf <- randomForest(votes ~ .,
                   data = tomodel,
                   importance = T)
varImpPlot(rf)

tomodel %>%
  ggplot(aes(BLACK10, votes)) +
  geom_point()

tomodel$pred <- predict(rf, tomodel)

tomodel %>%
  ggplot(aes(pred, votes, color = dBLACK00)) +
  geom_point() +
  geom_abline()

# Pct Black in the 2010 census is our best predictor
toplot_wvotes %>%
  as_tibble %>%
  filter(!is.na(votes)) %>%
  mutate(across(WHITE10:HISP10, ~ (.x / (POP10 + 1)))) %>%
  ggplot(aes(BLACK10, votes / total_votes)) +
  geom_smooth(method = "lm", se = F) +
  geom_point(aes(size = total_votes), alpha = 0.6) +
  scale_y_continuous("Precinct % vote share to Cori Bush",
                     labels = scales::percent) +
  scale_x_continuous("Precinct % black population (2010 US Census)",
                     labels = scales::percent) +
  scale_size_continuous("Precinct votes cast") +
  theme_bw()

# dBLACK00 is second best
toplot_wvotes %>%
  as_tibble %>%
  filter(!is.na(votes)) %>%
  mutate(across(WHITE00:HISP00, ~ (.x / (POP00 + 1))),
         across(WHITE10:HISP10, ~ (.x / (POP10 + 1))),
         dBLACK00 = BLACK10 - BLACK00) %>%
  filter(dBLACK00 > -0.5) %>%
  ggplot(aes(dBLACK00, votes / total_votes, size = total_votes)) +
  geom_smooth(method = "lm", se = F) +
  geom_point(alpha = 0.6) +
  scale_y_continuous("Precinct % vote share to Cori Bush",
                     labels = scales::percent) +
  scale_x_continuous("Change in precinct\n% black population\n(2010 - 2000 US Census)",
                     labels = scales::percent) +
  scale_size_continuous("Precinct votes cast")

# interaction
toplot_wvotes %>%
  as_tibble %>%
  filter(!is.na(votes)) %>%
  mutate(across(WHITE00:HISP00, ~ (.x / (POP00 + 1))),
         across(WHITE10:HISP10, ~ (.x / (POP10 + 1))),
         dBLACK00 = BLACK10 - BLACK00) %>%
  filter(dBLACK00 > -0.5) %>%
  mutate(shrink_black = ifelse(dBLACK00 < 0, "Lower % black", "Higher % black")) %>%
  ggplot(aes(BLACK10, votes / total_votes, color = shrink_black)) +
  geom_smooth(method = "lm", se = F) +
  geom_point(aes(size = total_votes), alpha = 0.6) +
  scale_y_continuous("Precinct % vote share to Cori Bush",
                     labels = scales::percent) +
  scale_x_continuous("Precinct % black population (2010 US Census)",
                     labels = scales::percent) +
  scale_color_brewer("Change in % black population\n(2000 to 2010 US Census)",
                     type = "qual") +
  scale_size_continuous("Precinct votes cast") +
  theme_bw()

# Dots with arrows on pct black (x axis) vs share cori (y axis), sized by total votes
toplot_wvotes %>%
  as_tibble %>%
  filter(!is.na(votes)) %>%
  transmute(BLACK90 = BLACK90 / POP90,
         BLACK00 = BLACK00 / POP00,
         BLACK10 = BLACK10 / POP10,
         POP90, POP00, POP10,
         pct_cori = votes / total_votes,
         total_votes, ward, precinct) %>%
  pivot_longer(BLACK90:POP10) %>%
  mutate(yr = str_remove_all(name, "[A-Z]"),
         yr = case_when(yr == "90" ~ 1990,
                        yr == "00" ~ 2000,
                        yr == "10" ~ 2010) %>% factor,
         name = str_remove_all(name, "[0-9]")) %>%
  pivot_wider(names_from = name, values_from = value) %>%
  mutate(lab = paste("Ward", ward, "Precinct", precinct) %>%
           fct_reorder(pct_cori)) %>%
  group_by(lab) %>% filter(max(POP) > 1000) %>% ungroup %>%
  filter(yr != 1990) %>%
  ggplot(aes(BLACK, pct_cori, group = lab)) +
  geom_path(arrow = arrow(length = unit(0.1, "inches"))) +
  geom_point(alpha = 0.8, aes(color = yr, size = POP)) +
  scale_color_brewer("US Census year",
                     palette = "Reds") +
  scale_y_continuous("Precinct % vote share to Cori Bush",
                     labels = scales::percent) +
  scale_x_continuous("Precinct % population Black (US Census)",
                     labels = scales::percent) +
  scale_size_continuous("Precinct population (US Census)") +
  theme_dark()

