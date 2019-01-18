library("futile.logger")
library("dplyr")
library("tidyr")
library("purrr")
library("rprojroot")
library("ggplot2")
library("cowplot")
library("sf")
library("tmap")

flog.logger(name = "model_progress_log", INFO, appender = appender.console())

root_crit <- has_dirname("epidemiology_model", subdir = "src")
root_dir <- root_crit$make_fix_file()

results_dir <- find_root_file("results", criterion = root_crit)
figures_dir <- find_root_file("figures", criterion = root_crit)
data_dir <- find_root_file("data", criterion = root_crit)

source(file.path(root_dir(), "src",
  "04-plot_results_functions_and_parameters.R"))

# Create a data.frame of parameter combinations --------------------------------
flog.info("Begin data processing", name = "model_progress_log")

param_list <- create_filelist_from_results(pattern = "Parameters")

# Data frame of parameters
parameters_df <-  map_dfr(param_list, process_parameters_fn) %>%
  arrange(seed_bioregions, desc(fw_reduction)) %>%
  mutate(seed_source = ifelse(seed_bioregions == "NEA-II", "Atl", "Pac"))

# Make into data.frame in join with results later on for clearer legend
parameters_df <- parameters_df %>%
  unique() %>%
  mutate(fw_reduction = 1 - as.numeric(fw_reduction),
    seed_source = factor(seed_source, levels = c("Pac", "Atl"))) %>%
  mutate(`Source; FW reduction` = factor(interaction(seed_source,
    fw_reduction, sep = "; "))) %>%
  mutate(`Source; FW reduction` = forcats::fct_reorder(`Source; FW reduction`,
    fw_reduction)) %>%
  select(parameter, seed_bioregions, `Source; FW reduction`)


# Read in port information containing bioregion, coordinates, etc
port_data <- readRDS(file.path(data_dir, "ports_data.rds")) %>%
  select(-Port_Country) %>%
  arrange(PortStd)

if (!exists("ports_temp")) {
  ports_temp_subset <- readRDS(file.path(data_dir, "ports_temp.rds"))[[3]]
}

maps_data_daily <- list_process_df_fn(ports_temp_subset) %>%
  left_join(port_data, by = c("Location" = "PortStd")) %>%
  left_join(parameters_df, by = "parameter") %>%
  mutate(date = as.Date(Time)) %>%
  group_by(bootstrap, parameter, REG_LRGGEO, Location, PortLatitudeStd,
    PortLongitudeStd, Lifestage, date) %>%
  summarise(population = mean(Population)) %>%
  as_tibble() %>%
  ungroup()

pop_breaks <- c(1, 1e3, 1e4, 1e5, 1e6, Inf)
pop_labels <- c("1 to 1000", "1001 to 1e4", "1e4 to 1e5", "1e5 to 1e6", "1e6+")

maps_data_filtered <- maps_data_daily %>%
  mutate(population_size = cut(population, pop_breaks, labels = pop_labels)) %>%
  filter(Location != "Panama Canal", population > 0, !is.na(population_size),
    !is.na(PortLatitudeStd), !is.na(PortLongitudeStd), Lifestage == "adult") %>%
  arrange(date, Location) %>%
  rename(model_date = date)

map_dates <- maps_data_filtered %>%
  select(model_date) %>%
  unique() %>%
  arrange(model_date)

mycols <- viridis::viridis(5, option = "D")

animation_directory <- file.path(figures_dir, "animated_map")

if (!dir.exists(animation_directory)) {
  dir.create(animation_directory, showWarnings = FALSE)
}

# Using terminal outside of R: ffmpeg -i Rplot%.04d.png -pix_fmt yuv420p
# Movie.mp4
# ffmpeg -y -r 60 -i Rplot%04d.png -pix_fmt yuv420p -b:v 300k invaded_ports.mp4

# EPSG 4326 datum
world_sf <- st_as_sf(maps::map('world', plot = FALSE, fill = TRUE))

# Eckhert IV datum EPSG 54012
data(World)

plot_map <- function(i, ...){
  # This function subsets the population data for each of the ports by date

  map_data_slice <- maps_data_filtered %>%
  semi_join(map_dates[i,], by = "model_date")

  map_data_sf <- st_as_sf(map_data_slice, crs = 4326,
    coords = c("PortLongitudeStd", "PortLatitudeStd"))

  current_date <- map_dates[i, ] %>%
    unlist() %>%
    as.Date(., origin = "1970-01-01")

  # Create title using year, month only
  formatted_mapdate <- strftime(current_date, "%Y-%m",
    usetz = FALSE, origin = "1970-01-01 00:00:00")

  h <- ggplot() +
    geom_sf(data = World, fill = "grey50", color = "grey50",
      inherit.aes = FALSE, lwd = 0.2) +
    geom_sf(data = map_data_sf, aes(color = population_size), size = 0.3,
      show.legend = "point") +
    coord_sf(crs = st_crs(World), datum = NA, default = TRUE, expand = TRUE) +
    scale_color_manual("Population size", values = mycols,
      limits = pop_labels) +
    labs(x = "Longitude", y = "Latitude", title = formatted_mapdate) +
    cowplot::theme_map(font_size = 8) +
    theme(legend.title = element_text(size = rel(0.8)),
      legend.text = element_text(size = rel(0.7)), legend.position = "bottom",
      legend.direction = "horizontal",
      panel.background = element_rect(fill = "gray75"),
      legend.key = element_rect(fill = "gray75"),
      panel.ontop = FALSE) +
    guides(color = guide_legend(override.aes = list(size = 0.4)))

  plotname <- sprintf("Rplot%04d.png", i)

  ggsave(file.path(animation_directory, plotname), plot = h, dpi = 300,
    width = 10, height = 10, units = "cm")

  flog.info("Frame %i of %i total frames processed", i, nrow(map_dates),
    name = "model_progress_log")

} # End of plot_map function

purrr::walk(seq(nrow(map_dates)), plot_map)

# Combine images into a movie
ffmpeg_call <- paste0("/usr/local/bin/ffmpeg -y -r 30 -i '",
 file.path(animation_directory, "Rplot%04d.png'"),
 " -pix_fmt yuv420p -b:v 300k invasion_sequence.mpeg")

system(ffmpeg_call)
flog.info("Animation complete", name = "model_progress_log")
