# This program reads in th ship position data and creates maps of great circle 
# connections between ports for different stages of the invasion.
# 
# Author: jmuirhead
###############################################################################

library("maps")
library("ggplot2")
library("dplyr")
library("fasttime")
library("rprojroot")

root_crit <- has_dirname("epidemiology_model", subdir = "src")
root_dir <- root_crit$make_fix_file()

# Check for non-missing elements
check_nonzero_length <- function(x) {
  vapply(x, function(x) length(x) > 0, logical(1))
}



# Custom functions for Map plotting section --------------------------------------------				
fortify.SpatialLinesDataFrame <- function(model, data, ...) {
  plyr::ldply(model@lines, fortify)
}


RegroupElements <- function(df, longcol, idcol){  
  g <- rep(1, length(df[, longcol]))
  
  # check if longitude within group differs more than 300 deg, ie if element was split
  if (diff(range(df[, longcol])) > 300) {   
  # we use the mean to help us separate the extreme values
    d <- df[, longcol] > mean(range(df[, longcol]))
  # some marker for parts that stay in place (we cheat here a little, as we do not take 
  # into account concave polygons)
    g[!d] <- 1     
    g[d] <- 2  # parts that are moved
  }
  # attach to id to create unique group variable for the dataset
  g <-  paste(df[, idcol], g, sep = "_") 
  df[["group_regroup"]] <- g
  df
}

# Function to close regrouped polygons
# takes dataframe, checks if 1st and last longitude value are the same, if not, inserts]
#  first as last and reassigns order variable

ClosePolygons <- function(df, longcol, ordercol){
  if (df[1,longcol] != df[nrow(df), longcol]) {
    tmp <- df[1,]
    df <- rbind(df, tmp)
  }
  o <- c(1: nrow(df))  # rassign the order variable
  df[, ordercol] <- o
  df
}


# Read in arrivals used in analysis

position_array <- readRDS(file.path(root_dir(), "data", "Positionarray.rds"))

# Read in port data for coordinates, etc
port_data <- readRDS(file.path(root_dir(), "data", "ports_data.rds")) %>%
  arrange(PortStd)
    
bioregions_data <- readr::read_csv(file.path(root_dir(), "data",
    "bioregion_centroids.csv")) %>%
  dplyr::select(REG_LRGGEO, xcoord, ycoord)
  

port_data %>%
  filter(PortStd == "Panama Canal")

# Convert to data_frame with ship name, port visited, and date/time

ships_list <- setNames(vector(mode = "list",
  length = length(dimnames(position_array)[[1]])), dimnames(position_array)[[1]])

for(i in seq_along(ships_list)){
  temp_array <- position_array[i, , , drop = TRUE]
  temp_position <- apply(temp_array, 2, function(x) which(!is.na(x), arr.ind = TRUE))
  visits <- check_nonzero_length(temp_position)
  ports <- unlist(unname(sapply(temp_position, function(x) attr(x, "names")))[visits])
  datetime <- fastPOSIXct(unlist(attributes(temp_position)), tz = "GMT")[visits]
  ships_list[[i]] <- data.frame(port = ports, datetime = datetime,
    stringsAsFactors = FALSE)
}

# Collapse list into a data_frame.
position_df <- bind_rows(ships_list, .id = "ship")

# Convert to data.frame with from, to, weight, latitude and longitude columns
# Eliminate rows where there was no movement between ports

ships_position_df <- position_df %>%
  group_by(ship) %>%
  mutate(from = port, to = lead(port), start_time = datetime,
    endtime = lead(datetime)) %>%
  dplyr::select(-port, -datetime) %>%
  filter(from != to) %>%
  ungroup()

# Calculate number of trips between from and to ports
port_n_trips <- ships_position_df %>%
  left_join(port_data, by = c("from" = "PortStd"), suffix = c(".from",".to")) %>% 
  left_join(port_data, by = c("to" = "PortStd"), suffix = c(".from", ".to")) %>%
   group_by(REG_LRGGEO.from, from, PortLongitudeStd.from, PortLatitudeStd.from, 
       REG_LRGGEO.to, to, PortLongitudeStd.to, PortLatitudeStd.to) %>%
  filter(from != to) %>%
  summarise(n_trips = n()) %>% 
  ungroup() %>%
  arrange(from, to) %>%
  mutate(chain_id = as.character(1:nrow(.)))

port_n_trips <- port_n_trips  %>%
  	mutate(infected_primary = ifelse(REG_LRGGEO.from %in% c("NEA-II"),
  	  TRUE, FALSE))
  	  
secondary_infections <- port_n_trips %>%
  filter(infected_primary == TRUE) %>%
  dplyr::select(to) %>%
  unique() %>%
  unlist(., use.names = FALSE)
  
  
port_n_trips <- port_n_trips  %>%
  mutate(infected_secondary = ifelse(REG_LRGGEO.from %in% "NEA-II" & 
              from %in% secondary_infections, TRUE, FALSE))
  	  
# Separate from and to bioregions into separate data_frames and add coordinates
port_start <- port_n_trips %>%
  dplyr::select(PortLongitudeStd.from, PortLatitudeStd.from)
  
port_end <- port_n_trips %>%
  dplyr::select(PortLongitudeStd.to, PortLatitudeStd.to)

# Calculate points along a great circle between start and end points
port_sp <- geosphere::gcIntermediate(port_start, port_end, n = 50, 
    breakAtDateLine = TRUE, addStartEnd = TRUE, sp = TRUE, sepNA = FALSE)

# Center map longitude on Caribbean basin
map_center <- 270 # longitude from (0 - 360)

# Convert great circle points to a data_frame for use in ggplot2
gc_df <- fortify.SpatialLinesDataFrame(port_sp) %>%
  as_tibble() %>%
  mutate(long_recenter = ifelse(long < map_center - 180, long + 360, long))

gc_recentered <- gc_df %>%
  group_by(id) %>% 
  do(., RegroupElements(., "long_recenter", "id")) %>%
  ungroup()

# Combine great circle locations with number of trips
trips_df <- port_n_trips %>%
  right_join(gc_recentered, by = c("chain_id" = "id")) %>%
  mutate(n_trips_scaled = scales::rescale(n_trips, to = c(0, 1)),
    n_trips_log = log10(n_trips + 1)) %>%
  arrange(infected_primary)
  
trips_df_primary_noninfected <- trips_df %>%
  filter(infected_primary == FALSE)

trips_df_primary_infected <- trips_df %>%
  filter(infected_primary == TRUE)

trips_df_secondary_noninfected <- trips_df %>%
  filter(infected_secondary == FALSE)

trips_df_secondary_infected <- trips_df %>%
  filter(infected_secondary == TRUE)
  
# Generate background map

world_background <- map_data("world") %>% 
    mutate(long_recenter = ifelse(long < map_center - 180, long + 360, long))
    
world_recentered <- world_background %>%
  group_by(group) %>% 
  do(., RegroupElements(., "long_recenter", "group")) %>%
  ungroup()
  
world_closedpolys <- world_recentered %>%
  group_by(group_regroup) %>%
  do(., ClosePolygons(., "long_recenter", "order"))

# Plotting section ---------------------------------------------------------------------
custom_cols <- c("white" = "white", "red" = "red", "green" = "#22A884FF")

ports_of_interest <- data_frame(name = c("Panama Canal", "Atlantic", "Pacific"),
  xcoord = c(-79.8, 0.802, 120), ycoord = c(9.15, 54.8, 23),
  color = c("green", "red", "white"))

# Map of no infected regions, just the sources

caribbean_map_initial <- ggplot() + 
	coord_map(xlim = c(-270, 90), ylim = c(-60, 90)) +
  geom_polygon(data = world_closedpolys, aes(long_recenter, lat, group = group_regroup), 
    size = 0.4, fill = "gray25", colour = "gray50") + 
  geom_line(data = trips_df_primary_noninfected, aes(x = long_recenter, y = lat,
    group = group_regroup, alpha = n_trips_scaled), size = 0.3, color = "medium blue") +
  geom_line(data = trips_df_primary_infected, aes(x = long_recenter, y = lat,
    group = group_regroup, alpha = n_trips_scaled), size = 0.3, color = "medium blue") +
  geom_point(data = ports_of_interest, aes(xcoord, ycoord, color = color), size = 3) +
  scale_color_manual(values = custom_cols) +
  scale_alpha_continuous(range = c(0.2, 1), guide = "none") +
  theme(panel.background = element_rect(fill = "black", color = "gray50"),
    plot.background = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(fill = NA, color = "gray50"),
    panel.ontop = FALSE,
    legend.position = "none")
    
ggsave(file.path(root_dir(), "figures", "epimodel_transits_initial.pdf"),
  plot = caribbean_map_initial, height = 4, units = "in")

ggsave(file.path(root_dir(), "figures", "epimodel_transits_initial.jpg"),
  plot = caribbean_map_initial, height = 4, units = "in", dpi = 300)

# Map of infected trips from source bioregion (i.e. primary infections)

caribbean_map_infected <- ggplot() + 
	coord_map(xlim = c(-270, 90), ylim = c(-60, 90)) +
  geom_polygon(data = world_closedpolys, aes(long_recenter, lat, group = group_regroup), 
    size = 0.4, fill = "gray25", colour = "gray50") + 
  geom_line(data = trips_df_primary_noninfected, aes(x = long_recenter, y = lat,
    group = group_regroup, alpha = n_trips_scaled), size = 0.3, color = "medium blue") +
  geom_line(data = trips_df_primary_infected, aes(x = long_recenter, y = lat,
    group = group_regroup, alpha = n_trips_log), size = 0.5, color = "red") +
  geom_point(data = ports_of_interest, aes(xcoord, ycoord, color = color), size = 3) +
  scale_color_manual(values = custom_cols) +
  scale_alpha_continuous(range = c(0.2, 1), guide = "none") +
  theme(panel.background = element_rect(fill = "black", color = "gray50"),
    plot.background = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(fill = NA, color = "gray50"),
    panel.ontop = FALSE,
    legend.position = "none")

  
ggsave(file.path(root_dir(), "figures", "epimodel_transits_infected.pdf"),
  plot = caribbean_map_infected, height = 4, units = "in")

ggsave(file.path(root_dir(), "figures", "epimodel_transits_infected.jpg"),
  plot = caribbean_map_infected, height = 4, units = "in", dpi = 300)
  
# Map of secondary infections where the bioregions connected by the source bioregion
# are then infected 

bgmap <- map_data("world", xlim = c(-102, -44), ylim = c(1, 33))

caribbean_map_secondary_infected <- ggplot() + 
  coord_map(xlim = c(-102, -44), ylim = c(1, 33)) +
  geom_polygon(mapping = aes(long, lat, group = group), data = bgmap,  , 
    size = 0.4, fill = "gray25", colour = "gray50") +
  geom_point(data = trips_df_secondary_noninfected,  aes(PortLongitudeStd.to,
          PortLatitudeStd.to), size = 3, color = "medium blue") +
  geom_point(data = trips_df_secondary_infected, aes(PortLongitudeStd.to,
     PortLatitudeStd.to), color = "red", size = 3) +
  geom_point(data = ports_of_interest, aes(xcoord, ycoord, color = color), size = 5) +
  scale_color_manual(values = custom_cols) +
  scale_alpha_continuous(range = c(0.2, 1), guide = "none") +
  labs(x = "Longitude", y = "Latitude") +
  theme(panel.background = element_rect(fill = "black", color = "gray50"),
    plot.background = element_blank(),
    axis.text = element_text(size = 18),
    axis.title = element_text(size = 18),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(fill = NA, color = "gray50"),
    panel.ontop = FALSE,
    legend.position = "none")

ggsave(file.path(root_dir(), "figures", "epimodel_transits_secondary_infected.pdf"),
  plot = caribbean_map_secondary_infected, height = 4, units = "in")

ggsave(file.path(root_dir(), "figures", "epimodel_transits_secondary_infected.jpg"),
  plot = caribbean_map_secondary_infected, height = 4, units = "in", dpi = 300)

