# Main program for processing results from model simulations. This program
# sources separate files containing functions
# (04-plot_results_functions_and_parameters.R)
# Author: jmuirhead
###############################################################################
library("dplyr")
library("purrr")
library("tidyr")
library("scales")
library("futile.logger")
library("stringi")
library("rprojroot")
library("fs")

# Set up directories -----------------------------------------------------------
root_crit <- has_dirname("PWS_2_5_west_coast_model", subdir = "src")
root_dir <- root_crit$make_fix_file()

results_dir <- find_root_file("results", criterion = root_crit)
figures_dir <- find_root_file("figures", criterion = root_crit)
data_dir <- find_root_file("data", criterion = root_crit)

process_ports_data <- TRUE
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
  filter(parameter %in% c("parameter001", "parameter002")) %>%
  select(parameter, port, lon, lat, occurrance)

# Process ports from 3-D array into data.frames

flog.info("Processing ports info", name = "model_progress_log")

ports_list <- create_filelist_from_results(pattern = "ports_pop")

flog.info("Merging port info with coordinates", name = "model_progress_log")

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
    melt_ships_temp(ships_temp)
    rm(ships_temp)
    gc()
  }
} # End of results pre-processing

# Processing ships summary, step 1

boot_seq <- seq(1, 2)
ships_longformat_list <- create_filelist_from_results(
  pattern = "ships_longformat", n_return_check = 2
)[boot_seq]

if (isTRUE(process_ships_data)) {
  # Process ships, step2
  flog.info("Beginning ships processing step 2",
    name = "model_progress_log"
  )

  for (i in seq_along(ships_longformat_list)) {
    flog.info("Reading in ships_longformat data %i", i,
      name = "model_progress_log"
    )
    ships_longformat <- readRDS(ships_longformat_list[[i]])
    process_ships_fn()
  }
}


