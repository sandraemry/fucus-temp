## Map of study sites 

pkgs <- c("sf", "tidyverse", "cowplot", "ggspatial")
lapply(pkgs, library, character.only = TRUE)
rm(pkgs)


df <- st_read("./fucus-temp/data/raw_data/map_data/CAN_US_BCalbers")

fraser <- st_read("./fucus-temp/data/raw_data/map_data/Fraser")

sites <- tribble(
  ~type, ~name, ~lat, ~long, ~region,
  "site", "shirley", 48.392672, -123.974261, "cold salty",
  "site", "nanaimo", 49.2331232, -123.9723978, "warm fresh",
  "salinity", "race rocks", 48.2996, -123.5318, "cold salty",
  "salinity", "departure bay", 49.2064, -123.9624, "warm fresh",
  "tides", "sheringham point", 48.377, 	-123.921, "cold salty",
  "tides", "nanaimo harbour", 	49.163, 	-123.924, "warm fresh",
  NA, "fraser river", 49.25, -123.5, NA
)

#Convert sites to spatial points dataframe, with the 4326 coordinates because that's what the GPS data were collected with
sites <- st_as_sf(sites, coords = c("long", "lat")) %>% 
  st_set_crs(4326)

#Specify projection, BC Albers projection is NAD83, EPSG 3005
#This document provided the below code: https://datascience.blog.wzb.eu/2019/04/30/zooming-in-on-maps-with-sf-and-ggplot2/
target_crs <- '+init=epsg:3005'
sites_transformed <- st_transform(sites, 3005)

#Set window and label positions

# Next, we specify the display window in WGS84 coordinates as longitude / latitude degrees. 
# We only specify the bottom left and top right corners (points A and B). 
# The CRS is set to WGS84 by using the EPSG code 4236.

disp_win <- st_sfc(st_point(c(-126, 48)), st_point(c(-122, 50)),
                   crs = 4326)
disp_win_trans <- st_transform(disp_win, crs = target_crs) 
disp_win_coords <- st_coordinates(disp_win_trans)

label_shirley <- st_sfc(st_point(c(-123.8, 48.5)), crs = 4326)
label_shirley_trans <-st_transform(label_shirley, crs = target_crs)
label_shirley_trans_coord <- st_coordinates(label_shirley_trans)

label_nanaimo <- st_sfc(st_point(c(-124.25, 49.15)), crs = 4326)
label_nanaimo_trans <-st_transform(label_nanaimo, crs = target_crs)
label_nanaimo_trans_coord <- st_coordinates(label_nanaimo_trans)

label_fraser <- st_sfc(st_point(c(-122.4, 49.4)), crs = 4326)
label_fraser_trans <- st_transform(label_fraser, crs = target_crs)
label_fraser_trans_coord <- st_coordinates(label_fraser_trans)


#Visualize the data ----

# with site legend
site_map <- ggplot() +
  geom_sf(data = df, color = "grey34", fill = "wheat") +
  geom_sf(data = (sites_transformed %>% filter(type == "site")), aes(color = region), size = 5) + 
  geom_sf(data = fraser, color = "#a6bddb", fill = "#a6bddb") + 
  scale_color_manual(values = c("#0570B0", "#D94801"), name = "site", labels = c("Shirley", "Nanaimo")) +
  annotation_scale(location = "bl", width_hint = 0.3) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.1, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering) +
  coord_sf(xlim = c(1000000, 1286630 ), ylim = c(331658, 561884), # this was a very hacky way of doing this, and I should figure out how to properly do it
           expand = FALSE) +
  annotate(geom = "text", x = label_fraser_trans_coord[, 'X'], y = label_fraser_trans_coord[, 'Y'],
           label = "Fraser River", fontface = "italic", color = "grey34", size = 5) + 
  annotate(geom = "text", x = label_shirley_trans_coord[,'X'], y = label_shirley_trans_coord[,'Y'], 
           label = "Shirley", fontface = "bold", color = "grey34", size = 5)  + 
  annotate(geom = "text", x = label_nanaimo_trans_coord[,'X'], y = label_nanaimo_trans_coord[,'Y'], 
           label = "Nanaimo", fontface = "bold", color = "grey34", size = 5) +
  labs(y = "", x = "") +
  theme(panel.background = element_rect(fill = "#a6bddb",
                                        color = "#a6bddb"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 8,  vjust=-0.5), axis.text.y = element_text(size = 8)) 

# without site legend
site_map_nolegend <- ggplot() +
  geom_sf(data = df, color = "grey34", fill = "wheat") +
  geom_sf(data = (sites_transformed %>% filter(type == "site")), aes(color = region), size = 5) + 
  geom_sf(data = fraser, color = "#a6bddb", fill = "#a6bddb") + 
  scale_color_manual(values = c("#0570B0", "#D94801")) +
  scale_x_continuous(expand = c(0,0)) + 
  scale_y_continuous(expand = c(0,0)) + 
  annotation_scale(location = "bl", width_hint = 0.3) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.1, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering) +
  coord_sf(xlim = c(990000, 1306630 ), ylim = c(331658, 561884), # this was a very hacky way of doing this, and I should figure out how to properly do it
           expand = FALSE) +
  annotate(geom = "text", x = label_fraser_trans_coord[, 'X'], y = label_fraser_trans_coord[, 'Y'],
           label = "Fraser River", fontface = "italic", color = "grey34", size = 5) + 
  annotate(geom = "text", x = label_shirley_trans_coord[,'X'], y = label_shirley_trans_coord[,'Y'], 
           label = "Shirley", fontface = "bold", color = "grey34", size = 5)  + 
  annotate(geom = "text", x = label_nanaimo_trans_coord[,'X'], y = label_nanaimo_trans_coord[,'Y'], 
           label = "Nanaimo", fontface = "bold", color = "grey34", size = 5) +
  labs(y = "", x = "") +
  theme(panel.background = element_rect(fill = "#a6bddb",
                                        color = "#a6bddb"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 8,  vjust=-0.5), 
        axis.text.y = element_text(size = 8),
        legend.position = "none", 
        plot.margin = margin(0,0,0,0, "in"))

ggsave("./fucus-temp/figures/study_site.png", site_map_nolegend, height = 6, width = 10, units = "in")

saveRDS(site_map, file = "./fucus-temp/figures/site_map")
saveRDS(site_map_nolegend, file = "./fucus-temp/figures/site_map_nolegend")

