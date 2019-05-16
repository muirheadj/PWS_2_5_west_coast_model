# Main program for processing results from model simulations. This program
# sources separate files containing functions
# (05-plot_results_functions_and_parameters.R) and calculations for failed
# introductions (06-failed_introductions.R).
#
# Author: jmuirhead
###############################################################################
library("dplyr")
library("purrr")
library("tidyr")
library("scales")
library("futile.logger")
library("viridis")
library("ggalt")
library("cowplot")
library("stringi")
library("rprojroot")
library("fs")

# Set up directories -----------------------------------------------------------
root_crit <- has_dirname("PWS_2_5_west_coast_model", subdir = "src")
root_dir <- root_crit$make_fix_file()

results_dir <- find_root_file("results", criterion = root_crit)
figures_dir <- find_root_file("figures", criterion = root_crit)
data_dir <- find_root_file("data", criterion = root_crit)

process_ports_data <- FALSE
process_ships_data <- TRUE

flog.logger(name = "model_progress_log", INFO, appender = appender.console())
flog.info("Beginning program run", name = "model_progress_log")

# Source file for commonly used parameters and functions
source(file.path(
  root_dir(), "src",
  "04-plot_results_functions_and_parameters.R"
))

# Create a data.frame of parameter combinations --------------------------------
param_list <- create_filelist_from_results(pattern = "parameter")

# Data frame of parameters
parameters_df <- readRDS(path(data_dir, "parameters_df.rds"))

# Make into data.frame in join with results later on for clearer legend
parameters_df <- parameters_df %>%
  unique() %>%
  mutate(
    scenario = factor(scenario)
  )  %>%
  select(parameter_id, scenario, n_seed_ports, n_destination_ports)

# Read in port information containing scenario info, coordinates, etc
port_data <- readr::read_csv(path(data_dir, "port_data.csv")) %>%
  select(parameter, port, lon, lat, occurrance)

# Process ports from 3-D array into data.frames

flog.info("Processing ports info", name = "model_progress_log")

ports_list <- create_filelist_from_results(pattern = "ports_pop")

flog.info("Merging port info with coordinates", name = "model_progress_log")

# Create summary table of port information
#ports.tex <- xtable::xtable(port_data, booktabs = TRUE, digits = 3)

#print(ports.tex, file.path(root_dir(), "tables", "ports_info.tex"),
#  tabular.environment = "longtable", floating = FALSE, type = "latex"
#)

#ports_base_long <- list_process_df_fn(ports_array)

# This section runs all the subroutinese to process the results for each of
# the ports across all parameters

if (isTRUE(process_ports_data)) {
  flog.info("Processing ports longformat", name = "model_progress_log")

  # Function to process the ports data from each of the parameters
  purrr::walk(seq_along(ports_list), process_ports_fn)

  flog.info("Beginning port immigration", name = "model_progress_log")

  port_immigration_list <-
    create_filelist_from_results(pattern = "port_immigration_2019")

  port_immigration_temp <- purrr::map(port_immigration_list, process_array_fn,
    full_sample_datespan)

  save_object_to_data_dir(port_immigration_temp, include_date = FALSE)

  rm(port_immigration_temp)

  # Process port immigration in order to calculate failed invasions
  purrr::walk(seq(port_immigration_list), process_port_immigration_fn)
}

# Begin processing ships section -----------------------------------------------

ships_list <- create_filelist_from_results(pattern = "ships_pop", 2)

if (isTRUE(process_ships_data)) {
  # Process ships, step 1
  flog.info("Beginning ships processing", name = "model_progress_log")
  
  for (i in seq_along(ships_list)) {

    ships_temp <- process_array_fn(ships_list[[i]], full_sample_datespan)
   
    ships_chunks <- chunkr(seq(13149), chunk_size = 1000)
    
    # Due to memory limitations, split the array into chunks based on time,
    # and calculate and save the resulting summary data.frames.
    
    for (j in seq_along(ships_chunks)) {
      ships_temp_chunks <- chunk_array(ships_chunks[[j]])
     
      # Convert to long-format data frames
      ships_temp_df <- melt_ships_temp(ships_temp_chunks)
    
     # Calculate and save daily population size
  	  process_ships_fn(ships_temp_df, chunk_number = j)	  
  	}
 } 
} # End of results pre-processing


# Calculate the instant mortality rate ----------------------------------------
#
## Read in ports_instant_mortality for each parameter and summarise across
## bootstraps
#ports_instant_mortality_list <- create_filelist_from_results(
#  pattern = "ports_instant_mortality", n_return_check = 2)
#
## For each parameter, calculate the bootstrap summaries of proportion of ports
## that experience instant mortality
#
#port_bootstrap_mortality_summary_df <- map_dfr(
#  seq(parameters_df[["parameter_id"]]),
#  port_mortality_summary_fn, ports_instant_mortality_list) %>%
#  ungroup()
#
#flog.info("Saving port_bootstrap_mortality_summary_df",
#  name = "model_progress_log"
#)
#
#save_object_to_data_dir(port_bootstrap_mortality_summary_df)
#
#port_bootstrap_mortality_summary_df <- readRDS(
#  create_filelist_from_data("port_bootstrap_mortality_summary_df")
#) %>%
#  mutate(
#    seed_source = stri_extract_first(scenario,
#      regex = "[a-zA-Z]{3}"
#    ),
#    fw_reduction = as.numeric(stri_extract_first(scenario,
#      regex = "(?<=;\\s).*"
#    ))
#  )
#
#
#port_bootstrap_mortality_summary_df <- port_bootstrap_mortality_summary_df %>%
#  mutate(seed_source = factor(seed_source, levels = c("Pac", "Atl"))) %>%
#  mutate(scenario = factor(interaction(seed_source,
#    fw_reduction,
#    sep = "; "
#  ))) %>%
#  mutate(scenario = forcats::fct_reorder(
#    scenario,
#    fw_reduction
#  ))
#
#
#ports_mortality_fig <- ggplot(
#  port_bootstrap_mortality_summary_df,
#  aes(x = date, y = prop_fail_pooled)
#) +
#  facet_wrap(~scenario, ncol = 2, dir = "h") +
#  geom_errorbar(size = 0.5, aes(
#    ymin = prop_fail_pooled_lcl,
#    ymax = pmin(1.0, prop_fail_pooled_ucl)
#  ), color = "gray75", width = 0.5) +
#  geom_point(size = 3) +
#  labs(
#    x = "Time",
#    y = stri_c(
#      "Proportion of ports undergoing instant mortality ",
#      "(mean with 95% confidence limits)"
#    )
#  ) +
#  theme_cowplot(font_size = 36)
#
#save_figures("fig7_ports_instant_mortality",
#  width = col_2_wide,
#  height = fig_max_height, dpi = 600
#)

# Calculate number of instant failures for each parameter across each year.
# Note: Use the n_fail_mean as the mean number of failures across bootstraps.
#
#n_instant_failures <- port_bootstrap_mortality_summary_df %>%
#  group_by(scenario) %>%
#  summarise(instant_failure = round(sum(n_fail_mean), 0))
#
#save_object_to_data_dir(n_instant_failures)
#
## Calculate population chains and failed introduction rate ---------------------
#flog.info("Processing population chains and failed introduction rates",
#  name = "model_progress_log"
#)

source(file.path(root_dir(), "src", "05-failed_introductions.R"))

flog.info("Beginning Figure 1 data processing", name = "model_progress_log")

# Figure 1, subsample ports and dates to show variation in values

all_dates <- ports_base_long %>%
  pull(Time) %>%
  unique()

sample_date <- all_dates[full_sample_datespan]
sample_ships <- sample(dimnames(ships_temp[[3]])[[3]],
  size = 100,
  replace = FALSE
)

sample_ports <- ports_base_long %>%
  filter(
    reg_lrggeo %in% c(destination_bioregions, "NEA-II"),
    location != "Panama Canal"
  ) %>%
  select(location) %>%
  unique()


# Figure 1. Population trajectory for parameter 2
# Note: semi-joins return just the original data.frame that has a match with a
# second data.frame
ports_base_sub <- ports_base_long %>%
  semi_join(sample_ports, by = "location") %>%
  ungroup() %>%
  mutate(seed_port = factor(seed_port,
    levels = c("Destination port", "Source port")
  )) %>%
  arrange(seed_port, location, Time)

# Separate out into 2 separate data.frames in order to plot in correct order
ports_base_sub_seed <- ports_base_sub %>%
  filter(seed_port == "Source port")
ports_base_sub_caribbean <- ports_base_sub %>%
  filter(seed_port == "Destination port")

# Count how many source ports and Caribbean ports there are
ports_seed_count <- ports_base_sub %>%
  group_by(seed_port) %>%
  summarise(n = n_distinct(location))

# Example time series plot for one iteration

fig1a <- ggplot() +
  facet_wrap(~lifestage, scales = "free") +
  geom_path(data = ports_base_sub_seed, aes(
    x = Time, y = log10Population,
    group = location, color = seed_port
  ), lwd = 0.4) +
  geom_path(data = ports_base_sub_caribbean, aes(
    x = Time, y = log10Population,
    group = location, color = seed_port
  ), lwd = 0.4) +
  scale_y_continuous(label = math_format(expr = 10^.x, format = force)) +
  scale_x_datetime(expand = c(0.09, 0)) +
  scale_color_manual("Port\nstatus", values = custom_cols[c(6, 10)]) +
  labs(y = expression(log[10] * "(Population size + 1)")) +
  theme_cowplot(font_size = 14) +
  theme(legend.position = "top") +
  theme(legend.direction = "horizontal") +
  guides(color = guide_legend(
    title = "Port status",
    override.aes = list(lwd = 1)
  ))

save_figures("fig1a_ports_time_series",
  width = col_2_wide,
  height = fig_max_height, dpi = 600
)

# Supplemental calculation for failures (i.e. final population is 0)
# for each location by lifestage

ports_base_caribbean_invaded <- ports_base_sub_caribbean %>%
  group_by(lifestage, location) %>%
  arrange(lifestage, location, Time) %>%
  mutate(pop_crash = (population > 0 & lead(population) <= 1))

ports_base_caribbean_invaded_summary <- ports_base_caribbean_invaded %>%
  filter(!is.na(pop_crash)) %>%
  summarise(pop_fail = any(pop_crash), pop_all_zero = all(population <= 1)) %>%
  print(n = 200)

# Number of failed populations
ports_base_caribbean_invaded_summary %>%
  filter(!pop_all_zero) %>%
  group_by(lifestage, pop_fail) %>%
  tally()

failed_populations <- ports_base_caribbean_invaded_summary %>%
  filter(!pop_all_zero) %>%
  left_join(ports_base_sub_caribbean, by = c("Lifestage", "Location")) %>%
  arrange(lifestage, location, Time) %>%
  ggplot(aes(Time, log10Population, color = pop_fail, group = location)) +
  geom_path() +
  facet_wrap(~lifestage, scales = "free") +
  scale_y_continuous(label = formatter_standard) +
  scale_color_manual("Pop\nstatus", values = custom_cols[c(6, 10)]) +
  scale_x_datetime(expand = c(0.09, 0)) +
  labs(y = "Population size")

save_figures("fig1c_failed_populations",
  width = col_2_wide,
  height = fig_max_height, dpi = 600
)

fig1b <- ggplot() +
  facet_wrap(~lifestage, scales = "free") +
  geom_path(data = ports_base_sub_seed, aes(
    x = Time, y = population,
    group = location, color = seed_port
  ), lwd = 0.3) +
  geom_path(data = ports_base_sub_caribbean, aes(
    x = Time, y = population,
    group = location, color = seed_port
  ), lwd = 0.3) +
  theme_cowplot() +
  scale_y_continuous(labels = formatter_standard) +
  scale_color_manual("Port\nstatus", values = custom_cols[c(6, 10)]) +
  scale_x_datetime(expand = c(0.09, 0)) +
  labs(y = "Population size")

save_figures("fig1b_ports_time_series",
  width = col_2_wide,
  height = fig_max_height, dpi = 600
)

rm(ports_base_sub, ports_base_sub_seed, ports_base_sub_caribbean)

flog.info("Beginning Figure 2 data processing", name = "model_progress_log")

# Figure 2
# Read in all port data

all_ports_df <- graph_data_preprocessing("all_ports_mean")

# Read in invaded port data
# Gives 73,770, 400 rows
# variables: date, parameter, bootstrap, lifestage, port,
# port_country, REG_LRGGEO, PortLatiatudeStd, PortLongitudeStd,
# mean_population, log1-population, pop_category, seed_bioregion,
# fw_effect, fw_reduction, scenario

destination_ports_lifestage_df <-
  graph_data_preprocessing("destination_ports_daily_mean") %>%
  select(date, bootstrap, port, scenario = scenario.x, lifestage,
    mean_population) %>%
  arrange(port, date, bootstrap)

# Sum up across lifestages to create total_population
destination_ports_df <- destination_ports_lifestage_df %>%
  group_by(date, scenario, bootstrap, port) %>%
  summarise(total_daily_population = sum(mean_population)) %>%
  ungroup()

# Cumulative number of unique invaded and noninvaded ports
# Calculates mean across locations to create mean_population for each bootstrap,
# then summarises by mean again across bootstraps

destination_ports_mean_all_df <- destination_ports_df %>%
  group_by(bootstrap, date, scenario) %>%
  summarise(mean_population_across_locations = mean(total_daily_population)) %>%
  group_by(date, scenario) %>%
  summarise_at(vars(mean_population_across_locations), stats_summary_funs) %>%
  arrange(scenario, date)

# Fix for reordering of treatments
destination_ports_mean_all_df <- destination_ports_mean_all_df %>%
  mutate(scenario = as.character(scenario)) %>%
  arrange(scenario, date)


# Same as above but keeps grouping by lifestage
# First, calculate the mean across locations
# Second, calculate mean and 95% CL across bootstraps
destination_ports_mean_lifestage_df <- destination_ports_lifestage_df %>%
  mutate(scenario= as.character(scenario)) %>%
  group_by(lifestage, bootstrap, date, scenario) %>%
  summarise(mean_population_across_locations = mean(mean_population)) %>%
  arrange(lifestage, scenario, date) %>%
  group_by(lifestage, scenario, date) %>%
  summarise_at(vars(mean_population_across_locations), stats_summary_funs) %>%
  arrange(lifestage, scenario, date)

destination_ports_mean_lifestage_df <- destination_ports_mean_lifestage_df %>%
  ungroup() %>%
  mutate(scenario = as.character(scenario)) %>%
  arrange(lifestage, scenario, date)


rm(destination_ports_df, destination_ports_lifestage_df)

# Plot of total population (summed across lifestages)

fig2b <- ggplot(
  destination_ports_mean_all_df,
  aes(x = date, y = mean, color = scenario)) +
  geom_path() 
  
  +
  
  
  scale_color_manual(scenario,
    values = custom_cols,
    drop = FALSE
  ) 
  
  
  
  +
  scale_fill_manual(scenario,
    values = custom_cols,
    drop = FALSE
  ) +
  theme_cowplot(font_size = 12) +
  guides(fill = guide_legend(override.aes = list(alpha = 1))) +
  theme(
    legend.position = c(0.05, 0.7),
    axis.title.y = element_text(size = rel(0.8))
  ) +
  scale_x_date(expand = c(0.1, 0)) +
  scale_y_continuous() +
  labs(
    x = "Time",
    y = expression(atop(
      "Total population size (" %*% 10^8 * ") in",
      "destination ports (mean)"
    ))
  )

save_figures("fig2b_all_ports_mean_population_size",
  plot = fig2b,
  dpi = 600, height = col_1.5_wide, width = col_1.5_wide
)


fig2b_no_legend <- fig2b +
  theme(legend.position = "none")

# Plot of population over time, grouped by lifestage

fig2c <- ggplot(
  destination_ports_mean_lifestage_df,
  aes(
    x = date, y = mean, color = scenario,
    group = scenario
  )
) +
  facet_wrap(~lifestage, scales = "free_y") +
  geom_ribbon(aes(
    colour = NA, fill = scenario,
    ymin = pmax(0, lcl), ymax = pmax(0, ucl)
  ), alpha = 0.2) +
  geom_path() +
  scale_color_manual("Source; FW reduction",
    values = custom_cols,
    drop = FALSE
  ) +
  scale_fill_manual("Source; FW reduction",
    values = custom_cols,
    drop = FALSE
  ) +
  theme_cowplot(font_size = 24) +
  theme(legend.position = c(0.03, 0.8)) +
  guides(fill = guide_legend(override.aes = list(alpha = 1))) +
  scale_x_date(expand = c(0.1, 0)) +
  scale_y_continuous(labels = formatter_standard) +
  labs(
    x = "Time",
    y = "Population size in destination ports (mean with 95% CL)"
  )

save_figures("fig2c_all_ports_mean_population_size",
  plot = fig2c, dpi = 600, width = col_2_wide, height = col_2_wide
)

flog.info("Beginning Figure 3 data processing",
  name = "model_progress_log"
)

caribbean_ports_count_df <-
  graph_data_preprocessing("destination_ports_pooled_count") %>%
  mutate(n_carI_ports = 202) %>%
  group_by(scenario, date) %>%
  summarise_at(vars(n_invaded_ports), stats_summary_funs) %>%
  arrange(scenario, date) # Count of Caribbean ports
# with total population > 0  = 202 ports

fig3a <- ggplot(
  caribbean_ports_count_df,
  aes(
    x = date, y = mean, color = scenario,
    group = scenario
  )
) +
  geom_ribbon(aes(
    colour = NA, fill = scenario,
    ymin = pmax(0, lcl), ymax = pmax(0, ucl)
  ), alpha = 0.2) +
  geom_path() +
  scale_color_manual("Source; FW reduction",
    values = custom_cols,
    drop = FALSE
  ) +
  scale_fill_manual("Source; FW reduction",
    values = custom_cols,
    drop = FALSE
  ) +
  theme_cowplot(font_size = 24) +
  theme(
    legend.position = c(0.05, 0.7),
    legend.title = element_text(size = rel(0.7)),
    legend.text = element_text(size = rel(0.6)),
    axis.title.y = element_text(size = rel(0.8)),
    legend.key.size = unit(4, "mm")
  ) +
  guides(fill = guide_legend(override.aes = list(alpha = 1))) +
  scale_x_date(expand = c(0.1, 0)) +
  annotate("text",
    label = "n = 202 Caribbean ports",
    x = as.Date("2011-01-01"), y = 18, size = 5
  ) +
  labs(
    x = "Time",
    y = "Count of destination ports\n with total population > 0"
  )

save_figures("fig3a_count_invaded_ports",
  plot = fig3a, dpi = 600,
  height = col_1_wide, width = col_1_wide
)

caribbean_ports_proportion_df <-
  graph_data_preprocessing("destination_ports_pooled_count") %>%
  mutate(
    n_carI_ports = 202,
    scenario = as.character(scenario)
  ) %>%
  group_by(scenario, date) %>%
  summarise(
    prop_pooled = prop_mean_fn(n_invaded_ports, n_carI_ports,
      na.rm = TRUE
    ),
    prop_se = prop_se_fn(n_invaded_ports, n_carI_ports, na.rm = TRUE)
  ) %>%
  mutate(
    prop_lcl = pmax(0, prop_pooled + qt(0.025, 4) * prop_se),
    # 5 bootstraps
    prop_ucl = pmax(0, prop_pooled + qt(0.975, 4) * prop_se)
  ) %>%
  arrange(scenario, date)
# Count of destination ports = 202


fig3b <- ggplot(
  caribbean_ports_proportion_df,
  aes(x = date, y = prop_pooled, color = scenario)
) +
  geom_ribbon(aes(
    colour = NA, fill = scenario,
    ymin = pmax(0, prop_lcl), ymax = pmax(0, prop_ucl)
  ), alpha = 0.2) +
  geom_path() +
  scale_color_manual("Source; FW reduction",
    values = custom_cols,
    drop = FALSE
  ) +
  scale_fill_manual("Source; FW reduction",
    values = custom_cols,
    drop = FALSE
  ) +
  theme_cowplot(font_size = 12) +
  theme(
    legend.position = c(0.05, 0.7),
    axis.title.y = element_text(size = rel(0.8))
  ) +
  guides(color = guide_legend(override.aes = list(size = 4, alpha = 1))) +
  scale_y_continuous(limits = c(0, 0.5)) +
  scale_x_date(expand = c(0.1, 0)) +
  annotate("text",
    label = "n = X destination ports",
    x = as.Date("2011-08-01"), y = 0.40, size = 3
  ) +
  labs(x = "Time", y = stri_c(
    "Proportion of destination ports with\n",
    "total population > 0 (mean with 95% CL)"
  ))

save_figures("fig3b_proportion_invaded_ports",
  plot = fig3b,
  dpi = 600, height = col_1.5_wide * 1.1, width = col_1.5_wide
)

fig3b_no_legend <- fig3b +
  theme(legend.position = "none")

# Maps of ports invading ---------------------------------------------------------------

# HARD CODED: FIXME
partial_sample_datespan <- seq(1, 7301, by = 480)

sample_date_plot <- as.Date(dimnames(ports_temp[[1]])[[1]][partial_sample_datespan])

maps_data_filtered <- ports_base_long %>%
  left_join(port_data, by = c("location" = "portstd", "reg_lrggeo")) %>%
  filter(
    lifestage == "adult",
    reg_lrggeo %in% c("CAR-I", "CAR-II", "CAR-III", "CAR-IV", "CAR-V"),
    population > 0, date %in% sample_date_plot, !is.na(PortLatitudeStd),
    !is.na(PortLongitudeStd), location != "Panama Canal"
  ) %>%
  rename(model_date = date) %>%
  ungroup()

world_df <- map_data("world")

theme_map <- theme_dark() +
  theme(panel.grid = element_blank()) +
  theme(strip.text = element_text(size = rel(0.8))) +
  theme(axis.ticks = element_blank()) +
  theme(axis.text = element_blank()) +
  theme(axis.title.x = element_blank()) +
  theme(axis.title.y = element_blank()) +
  theme(panel.background = element_rect(fill = "gray50")) +
  theme(legend.key = element_rect(fill = "gray50"))

fig4_nea_map <- ggplot() +
  geom_map(data = world_df, map = world_df, aes(
    map_id = region,
    group = region
  ), fill = "grey75") +
  geom_point(data = maps_data_filtered, aes(
    x = PortLongitudeStd,
    y = PortLatitudeStd, color = population
  ), size = 1) +
  facet_wrap(~model_date, shrink = FALSE) +
  coord_quickmap(xlim = c(-106, -48), ylim = c(2, 35)) +
  theme_map +
  scale_color_viridis(labels = formatter_standard) +
  theme(
    legend.title = element_text(size = rel(0.8)),
    legend.text = element_text(size = rel(0.7)),
    legend.position = "right",
    legend.direction = "vertical"
  ) +
  labs(x = "Longitude", y = "Latitude")

save_figures("fig4_port_map",
  dpi = 600, height = 6, width = 8,
  units = "cm"
)

flog.info("Beginning Figure 5 data processing for ships",
  name = "model_progress_log"
)

# Ships population growth -----------------------------------------------

invaded_ships_df <- graph_data_preprocessing("invaded_ships_mean")

invaded_ships_nonzero <- invaded_ships_df %>%
  mutate(scenario = as.character(scenario)) %>%
  group_by(parameter, scenario, date) %>%
  summarise(n = sum(n), ships_total = sum(ships_mean)) %>%
  filter(ships_total > 0)

fig5a <- ggplot(invaded_ships_nonzero, aes(
  x = date, y = ships_total,
  colour = scenario
)) +
  geom_path() +
  scale_color_manual("Source; FW reduction",
    values = custom_cols,
    drop = FALSE
  ) +
  scale_fill_manual("Source; FW reduction",
    values = custom_cols,
    drop = FALSE
  ) +
  scale_x_date(expand = c(0.1, 0)) +
  scale_y_continuous(labels = formatter_standard) +
  theme_cowplot() +
  theme(legend.position = c(0.4, 0.7)) +
  guides(color = guide_legend(override.aes = list(size = 4, alpha = 1))) +
  labs(x = "Time", y = expression("Mean population size"))

save_figures("fig5a_mean_ship_population size",
  plot = fig5a, dpi = 600,
  height = 7, width = 7
)

invaded_ships_df <- graph_data_preprocessing(
  "invaded_ships_pooled_count"
) %>%
  mutate(scenario = as.character(scenario)) %>%
  group_by(scenario, date) %>%
  summarise_at(vars(n_invaded_ships), stats_summary_funs) %>%
  arrange(scenario, date) # Count of invaded ships

flog.info("Beginning Figure 6 data processing for ships",
  name = "model_progress_log"
)

fig6a <- ggplot(
  invaded_ships_df,
  aes(x = date, y = mean, colour = scenario)
) +
  geom_path() +
  guides(color = guide_legend(override.aes = list(size = 4))) +
  scale_color_manual(values = custom_cols, drop = FALSE) +
  scale_fill_manual(values = custom_cols, drop = FALSE) +
  scale_x_date(expand = c(0.1, 0)) +
  theme(legend.position = c(0.4, 0.7)) +
  annotate("text",
    label = "n = 5085 ships total",
    x = as.Date("2011-01-01"), y = 1000, size = 5
  ) +
  labs(
    x = "Time",
    y = expression("Count of ships with populations" > "0")
  )

save_figures("fig6a_count_ships",
  dpi = 600, height = 7, width = 7,
  units = "cm"
)

invaded_ships_proportion_df <-
  graph_data_preprocessing("invaded_ships_pooled_count") %>%
  mutate(
    n_ships = 5085,
    scenario = as.character(scenario)
  ) %>%
  group_by(scenario, date) %>%
  summarise(
    prop_pooled = prop_mean_fn(n_invaded_ships, n_ships,
      na.rm = TRUE
    ),
    prop_se = prop_se_fn(n_invaded_ships, n_ships, na.rm = TRUE)
  ) %>%
  mutate(
    prop_lcl = pmax(0, prop_pooled + qt(0.025, 4) * prop_se),
    prop_ucl = pmax(0, prop_pooled + qt(0.975, 4) * prop_se)
  ) %>%
  arrange(scenario, date) # Proportion of invaded ships

fig6b <- ggplot(invaded_ships_proportion_df, aes(
  x = date, y = prop_pooled,
  colour = scenario
)) +
  geom_ribbon(aes(
    colour = NA, fill = scenario,
    ymin = pmax(0, prop_lcl), ymax = pmax(0, prop_ucl)
  ), alpha = 0.2) +
  geom_path() +
  scale_color_manual("Source; FW reduction",
    values = custom_cols,
    drop = FALSE
  ) +
  theme_cowplot(font_size = 12) +
  theme(
    legend.position = c(0.05, 0.6),
    legend.title = element_text(size = rel(0.8)),
    legend.text = element_text(size = rel(0.7)),
    legend.key.size = unit(10, "pt"),
    legend.margin = margin(0, 0, 0, 0, "cm"),
    axis.title.y = element_text(size = rel(0.8))
  ) +
  guides(color = guide_legend(override.aes = list(size = 3, alpha = 1))) +
  scale_y_continuous(limits = c(0, 0.75)) +
  scale_x_date(expand = c(0.1, 0)) +
  annotate("text",
    label = "n = 5085 ships",
    x = as.Date("2013-06-01"), y = 0.60, size = 3
  ) +
  labs(
    x = "Time",
    y = stri_c(
      "Proportion of ships with total\n",
      "population > 0 (mean with 95% CL)"
    )
  )


save_figures("fig6b_proportion_ships",
  plot = fig6b, dpi = 600,
  height = col_1_wide, width = col_1_wide
)

# Combine with proportion of ports figure --------------------------------------

ports_and_ships_proportion <- plot_grid(fig6b, fig3b_no_legend,
  align = "v",
  nrow = 2, ncol = 1, labels = "AUTO", label_fontface = "plain"
)

save_plot(file.path(figures_dir, "ports_and_ships_prop.pdf"),
  plot = ports_and_ships_proportion, ncol = 1, nrow = 2,
  base_width = col_1.5_wide
)

save_plot(file.path(figures_dir, "ports_and_ships_prop.png"),
  plot = ports_and_ships_proportion, ncol = 1, nrow = 2,
  base_width = col_1.5_wide, dpi = 600
)

save_plot(file.path(figures_dir, "ports_and_ships_prop.jpg"),
  plot = ports_and_ships_proportion, ncol = 1, nrow = 2,
  base_width = col_1.5_wide, dpi = 600
)

# Create plot of ships proportion, ports total population and
# ports proportion
ports_and_ships_proportion2 <- plot_grid(fig6b, fig2b_no_legend,
  fig3b_no_legend,
  align = "v", nrow = 3, ncol = 1, labels = "AUTO",
  label_fontface = "plain", hjust = -0.5, scale = 0.95, axis = "r"
)

save_plot(file.path(figures_dir, "ports_and_ships_prop2.pdf"),
  plot = ports_and_ships_proportion2, ncol = 1, nrow = 3,
  base_width = col_1_wide, base_height = 7, units = "cm"
)

save_plot(file.path(figures_dir, "ports_and_ships_prop2.tiff"),
  plot = ports_and_ships_proportion2, ncol = 1, nrow = 3,
  base_width = col_1_wide, base_height = 7, dpi = 600, units = "cm"
)

save_plot(file.path(figures_dir, "ports_and_ships_prop2.jpg"),
  plot = ports_and_ships_proportion2, ncol = 1, nrow = 3,
  base_width = col_1_wide, base_height = 7, units = "cm", dpi = 600
)


# Port population and ports proportion combined figure -------------------------

ports_combination_figure <- plot_grid(fig2b, fig3b_no_legend,
  align = "v",
  nrow = 2, ncol = 1, labels = "AUTO", label_fontface = "bold",
  scale = 0.9, label_size = 36, hjust = -1
)

save_plot(file.path(figures_dir, "fig2_ports_combined.pdf"),
  plot = ports_combination_figure, ncol = 1, nrow = 2, base_width = col_1_wide,
  base_height = 0.9 * col_1_wide
)

save_plot(file.path(figures_dir, "fig2_ports_combined.jpg"),
  plot = ports_combination_figure, ncol = 1, nrow = 2, base_width = col_1_wide,
  base_height = 0.9 * col_1_wide, dpi = 600
)

save_plot(file.path(figures_dir, "fig2_ports_combined.png"),
  plot = ports_combination_figure, ncol = 1, nrow = 2, base_width = col_1_wide,
  base_height = 0.9 * col_1_wide, dpi = 600
)
