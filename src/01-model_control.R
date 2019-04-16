# This is the main control program for the infection model for various levels
# of transmission probability. It runs the model for each combination of
# transmission probability and then saves the results to lists at the end.
#
# Author: jmuirhead
###############################################################################

param_iter <- 2

# Pass parameters to model based on arguments supplied to Rscript
param_iter <- commandArgs(trailingOnly = TRUE)
param_iter <- as.integer((param_iter))

suppressMessages(TRUE)

library("methods")
library("utils")
library("reshape2")
library("data.table")
library("lazyeval")
library("dplyr")
library("ggplot2")
library("abind")
library("bit")
library("Rcpp")
library("RcppArmadillo")
library("iterators")
library("assertive.numbers")
library("stringi")
library("futile.logger")
library("rprojroot")

root_crit <- has_dirname("PWS_2_5_west_coast_model", subdir = "src")
root_dir <- root_crit$make_fix_file()

options(tibble.width = Inf)

# Imput model parameters from YAML file

yaml_params <- yaml::read_yaml(file.path(root_dir(), "params.yaml"))

# Set up tracer and info loggers

log_name <- function(x) file.path(
    root_dir(), "logs",
    sprintf("parameter%03d_%s", as.numeric(param_iter[1]), x)
  )
  
get_flog_level <- function(name){
  res <- logger.options()[[paste0("logger.", name)]][["threshold"]]
  res
}
  

flog.logger("ports_pop_trace", TRACE,
  appender = appender.tee(log_name("ports_pop_trace.log")))

flog.logger("ports_n_trace", TRACE,
  appender = appender.tee(log_name("ports_n_trace.log")))
  
flog.logger("model_progress", INFO,
  appender = appender.console())
  
flog.logger("juve_lag", TRACE,
  appender = appender.tee(log_name("juvenile_trace.log")))
  
flog.logger("ships_pop_trace", TRACE,
  appender = appender.tee(log_name("ships_pop_trace.log")))
  
flog.logger( "ports_instant_mortality_trace", TRACE,
  appender = appender.tee(log_name("ports_instant_mortality_trace.log")))
  
flog.logger("ships_emigration_trace", TRACE,
  appender = appender.tee(log_name("ships_emigration_trace.log")))

flog.threshold(INFO)

# Identify species
sp_name <- yaml_params[["params"]][["sp_name"]]

# Set up lifestages, incl which lifestages disperse (sp = ship to port)

ship_to_port_lifestages <- c(
  "larva" = 1, "cyprid" = 0, "juvenile" = 0,
  "adult" = 0
)
port_to_ship_lifestages <- c(
  "larva" = 0, "cyprid" = 1, "juvenile" = 0,
  "adult" = 0
)

# Population transition matrix
pop_transition <- readRDS(file.path(root_dir(), "data",
  "bal_improvisus_pop_transition.rds"))

# Reproductive and development time lag in seconds
larval_dev_lag <- yaml_params[["params"]][["larval_dev_lag"]] # 7 days until cyprids can appear
juvenile_lag <- yaml_params[["params"]][["juvenile_lag"]] # 1 day until juveniles can first appear after cyprids appear in the population
mature_lag <- yaml_params[["params"]][["mature_lag"]] # McDonald et al 2009: 8 weeks post-settlement
repro_lag <- yaml_params[["params"]][["repro_lag"]] # McDonald et al 2009: 10 weeks post-settlement

# Read in table of port information for seed ports and habitat suitability

port_data <- read.csv(file.path(root_dir(), "data", "port_data.csv"),
  stringsAsFactors = FALSE
)

# Define port area in square meters
port_area <- # 312 times larger than max wsa for ships, 1546 times larger
  # than average ship wsa

# Define carrying capacity per square meter
max_density_individuals <- yaml_params[["params"]][["max_density_individuals"]]

# Generate combination of parameters
parameter_grid <- expand.grid(
  species = yaml_params[["params"]][["sp_name"]],
  scenario = yaml_params[["params"]][["scenario"]],
  port_area = yaml_params[["params"]][["port_area"]],
  k_ports = port_area * max_density_individuals,
  fw_reduction = yaml_params[["params"]][["fw_reduction"]],
  max_density_individuals = max_density_individuals,
  larval_dev_lag = larval_dev_lag,
  juvenile_lag = juvenile_lag,
  mature_lag = mature_lag,
  repro_lag = repro_lag,
  ship_port_prob = yaml_params[["params"]][["ship_port_prob"]],
  port_compentency_prob = yaml_params[["params"]][["port_comptency_prob"]],
  stringsAsFactors = FALSE
)

habitat_threshold <- unlist(yaml_params[["params"]][["habitat_threshold"]],
    recursive = FALSE
  ) %>%
  as_tibble() %>%
  tidyr::gather(key = "species", value = "habitat_threshold")

parameter_grid <- left_join(parameter_grid, habitat_threshold, by = "species")

parameter_grid <- parameter_grid %>%
  filter(!(species == "hypothetical_sp" & scenario == "seed_ports2")) %>%
  mutate(parameter_id = sprintf("parameter%0.3d", seq(nrow(.)))) %>%
  select(parameter_id, everything())

saveRDS(parameter_grid, file = file.path(
  root_dir(), "data",
  "parameters_df.rds"
), compress = TRUE)


# Extract Ports habitat suitability as a vector --------------------------------

ports_habitat_suitability <- port_data %>%
  filter(
    species == parameter_grid[param_iter, "species"],
    scenario == parameter_grid[param_iter, "scenario"]
  ) %>%
  purrr::pluck("habitat_suitability")

names(ports_habitat_suitability) <- port_data %>%
  purrr::pluck("port") %>%
  unique()

# Identify seed ports ---------------------------------------------------------

# Extract seed ports and store in a vector
seed_ports <- port_data %>%
  filter(
    species == parameter_grid[param_iter, "species"],
    scenario == parameter_grid[param_iter, "scenario"],
    occurrance == 1
  ) %>%
  select(port)

# Re-assign date_list_ext
full_date_list <- format(seq(
  from = as.POSIXct("2010-01-01 00:00:00",
    tz = "UTC"
  ),
  to = as.POSIXct("2017-12-31 18:00:00", tz = "UTC"),
  by = "6 hours"
), format = "%Y-%m-%d %H:%M:%S")

# Cycle through bootstrap population loops
boot_iter <- yaml_params[["params"]][["boot_iter"]]

scenario <- parameter_grid[param_iter, "scenario"]

data_directory <- file.path(
  root_dir(), "data"
)

flog.info("data directory: %s", data_directory, name = "model_progress.log")

# Get ship imo info, ships_array and ports_array (population arrays)
ship_imo_tbl <- readRDS(file.path(
  data_directory, "ship_movements",
  "ship_imo_tbl.rds"
))

# Set the effective wsa for ships to be 10% of the wetted surface area
effective_wsa_scale <- yaml_params[["params"]][["effective_wsa_scale"]]

ship_imo_tbl[["effective_wsa"]] <- ship_imo_tbl[["wsa"]] * effective_wsa_scale

# Pre-allocate memory for ships and ports arrays
ships_pop_temp <- readRDS(file.path(
  data_directory, "ship_movements",
  "ships_array.rds"
))
ports_pop_temp <- readRDS(file.path(
  data_directory, "ship_movements",
  "ports_array.rds"
))

# Keep track of instant mortality in ports which has the same array dimensions
# as ports_pop_temp
ports_instant_mortality <- ports_pop_temp

# Make sure the ports_pop_temp matrix names are in the right order
dimnames(ports_pop_temp)[["port"]] <- sort(dimnames(ports_pop_temp)[["port"]])

dimnames(ports_instant_mortality)[["port"]] <-
  sort(dimnames(ports_instant_mortality)[["port"]])


# Get list of bootstrapped ship populations
boot_filelist <- list.files(
  path = data_directory,
  pattern = "chunk[0-9]{3}.rds$", full.names = FALSE, recursive = TRUE
)

# Reshape some of the arrays for the population growth model
source(file.path(root_dir(), "munge", "04-select_ship_sources_for_caribbean.R"))

# Setup invasion status (population density of organisms) of initial ports
# (ports_array)

seed_ports_fn <- function(param, seed_names, ports_pop_input, lifestages) {

  # Check if port names matches up with the ports_ppop_input
  flog.info("Port name check: %s", all(dimnames(ports_pop_input)[[3]] %in%
    port_data[["port"]]) == TRUE, name = "model_progress.log")

  n_lifestages <- length(lifestages)

  n_at_carrying_capacity <- param[, "k_ports"]

  n_at_stability <- c(
    larva = 837611569, cyprid = 92118541,
    juvenile = 11155834, adult = 31199732
  )

  seed_value <- array(
    data = rep(n_at_stability,
      each = dim(ports_pop_input)[1]
    ),
    dim = list(dim(ports_pop_input)[1], length(lifestages), nrow(seed_names)),
    dimnames = list(
      dimnames(ports_pop_input)[[1]], names(lifestages),
      seed_names$port
    )
  )

  # Set up some populations in the seed ports
  # Note: This matches the port names from
  # seed ports, all the dates and lifestages

  afill(ports_pop_input, local = TRUE) <- seed_value
  ports_pop_input
}

# Source c++ version of population growth
sourceCpp(file.path(root_dir(), "src", "popgrow.cpp"), verbose = FALSE)

# Source c++ version of stochastic matrix
sourceCpp(file.path(root_dir(), "src", "stoch_pop_growth.cpp"), verbose = FALSE)


# Add a dimension for the number of life stages in the population
ships_pop <- ships_array_add(ships_pop_temp,
  lifestages = ship_to_port_lifestages
)
ports_pop_temp <- ports_array_add(ports_pop_temp,
  lifestages = ship_to_port_lifestages
)
ports_pop <- seed_ports_fn(
  param = parameter_grid[param_iter, ],
  seed_names = seed_ports, ports_pop_temp,
  lifestages = ship_to_port_lifestages
)

source(file.path(root_dir(), "src", "02-main_model.R"))

# Run main model ---------------------------------------------------------------

sourceCpp(file.path(root_dir(), "src", "arma_cube.cpp"), verbose = FALSE)

model_run <- main_model_fn(ship_imo_tbl = ship_imo_tbl,
  param = parameter_grid[param_iter, ],
  A_mat = pop_transition,
  ports_pop = ports_pop,
  root_dir(),
  data_directory,
  param_iter,
  boot_iter,
  ship_to_port_lifestages,
  port_to_ship_lifestages,
  ports_instant_mortality,
  ports_habitat_suitability
)

flog.info("Finished model run", name = "model_progress.log")
