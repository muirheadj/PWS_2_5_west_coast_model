# This section of code processes the data necessary to create a static map of
# the conditions at the end of the model for 4 scenarios.
# 
# Author: jmuirhead
###############################################################################
library("tidyverse")
library("viridis")
library("cowplot")
library("rprojroot")

# Set up directories -----------------------------------------------------------
root_crit <- has_dirname("epidemiology_model", subdir = "src")
root_dir <- root_crit$make_fix_file()

results_dir <- find_root_file("results", criterion = root_crit)
figures_dir <- find_root_file("figures", criterion = root_crit)
data_dir <- find_root_file("data", criterion = root_crit)

# Source file for commonly used parameters and functions
source(file.path(root_dir(), "src",
  "04-plot_results_functions_and_parameters.R"))


# Create a data.frame of parameter combinations --------------------------------
param_list <- create_filelist_from_results(pattern = "Parameters")

# Data frame of parameters
parameters_df <-  map_df(param_list, process_parameters_fn) %>%
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
  select(parameter, seed_source, fw_reduction, seed_bioregions,
    `Source; FW reduction`)

if (!exists("ports_temp")) {
  ports_temp <- readRDS(file.path(data_dir, "ports_temp.rds"))
}


# Read in port information containing bioregion, coordinates, etc
port_data <- readRDS(file.path(data_dir,"ports_data.rds")) %>%
  arrange(PortStd) %>%
  select(-Port_Country)


summarize_population <- function(y){
  purrr::map_dfr(y, function(x) list_process_df_fn(ports_temp[[x]]) %>%
      filter(Date == "2014-11-15 00:00:00")) %>%
    filter(Location != "Panama Canal") %>%
    group_by(Location, parameter, bootstrap) %>%
    summarize(Population = sum(Population, na.rm = TRUE))
}


extract_last_date <- function(x) {
  x %>%
    left_join(port_data, by = c("Location" = "PortStd")) %>%
    filter(REG_LRGGEO %in% caribbean_bioregions) %>%
    mutate(invaded = if_else(Population > 0, "invaded", "noninvaded")) %>%
    select(parameter, REG_LRGGEO, location = Location, long = PortLongitudeStd,
      lat = PortLatitudeStd, population = Population, invaded) %>%
    as_tibble()
}


count_invaded <- function(x){
  x %>%
    left_join(parameters_df, by = "parameter") %>%
    group_by(seed_source, fw_reduction, seed_bioregions, `Source; FW reduction`,
      location, long, lat) %>%
    summarise(prop_invaded = sum(invaded == "invaded") / 5) %>%
    ungroup() %>%
    arrange(prop_invaded)
}


# Atlantic Source Region, No reduction (parameter 001)
ports_base_long_001 <- summarize_population(y = c(1, 11, 21, 31, 41))
ports_base_long_001 <- extract_last_date(ports_base_long_001)
ports_base_long_001 <- count_invaded(ports_base_long_001)

# Atlantic Source Region, High reduction (parameter003)

ports_base_long_003 <- summarize_population(y = c(3, 13, 23, 33, 43))
ports_base_long_003 <- extract_last_date(ports_base_long_003)
ports_base_long_003 <- count_invaded(ports_base_long_003)

# Pacific Source Region, No reduction

ports_base_long_006 <- summarize_population(y = c(6, 16, 26, 36, 46))
ports_base_long_006 <- extract_last_date(ports_base_long_006)
ports_base_long_006 <- count_invaded(ports_base_long_006)

# Pacific Source Region, High reduction

ports_base_long_008 <- summarize_population(y = c(8, 18, 28, 38, 48))
ports_base_long_008 <- extract_last_date(ports_base_long_008)
ports_base_long_008 <- count_invaded(ports_base_long_008)

combined_port_data <- bind_rows(ports_base_long_001, ports_base_long_003,
  ports_base_long_006, ports_base_long_008)


# Generate background map -------------------------------------------------------------
library("maps")
library("ggplot2")

bgmap <- map_data("world", xlim = c(-102, -44), ylim = c(1, 33))

fig4 <- ggplot(data = combined_port_data) +
  facet_wrap(~ `Source; FW reduction`, dir = "h") +
  coord_map(xlim = c(-102, -44), ylim = c(1, 33)) +
  geom_polygon(mapping = aes(long, lat, group = group), data = bgmap,
    size = 0.4, fill = "gray25", colour = "gray50", inherit.aes = FALSE) +
  geom_point(aes(long, lat, fill = prop_invaded), color = "gray60", size = 4,
    pch = 21) +
  scale_fill_gradient(low = "medium blue", high = "red",
    "Proportion of model iterations invaded") +
  labs(x = "Longitude", y = "Latitude") +
  theme_cowplot(font_size = 20) +
  theme(panel.background = element_rect(fill = "black", color = "gray50"),
        plot.background = element_blank(),
        axis.text = element_text(size = 20),
        axis.title = element_text(size = 20),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(fill = NA, color = "gray50"),
        panel.ontop = FALSE,
        legend.position = "top",
        legend.direction = "horizontal",
        legend.justification = "center",
        legend.key.width = unit(20, "mm"))


save_plot(file.path(figures_dir, "fig4.png"), plot = fig4, ncol = 1, nrow = 1,
  base_width = col_2_wide, base_height = 0.7 * col_2_wide, dpi = 400)

save_plot(file.path(figures_dir, "fig4.pdf"), plot = fig4, ncol = 1, nrow = 1,
  base_width = col_2_wide, base_height = 0.7 * col_2_wide)

save_plot(file.path(figures_dir, "fig4.jpg"), plot = fig4, ncol = 1, nrow = 1,
    base_width = col_2_wide, base_height = 0.7 * col_2_wide, dpi = 400)

save_plot(file.path(figures_dir, "fig4.tiff"), plot = fig4, ncol = 1, nrow = 1,
    base_width = col_2_wide, base_height = 0.7 * col_2_wide, dpi = 400)
