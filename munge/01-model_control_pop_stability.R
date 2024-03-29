# This is the main control program for the infection model for various levels
# of transmission probability. It runs the model for each combination of
# transmission probability and then saves the results to lists at the end.
#
# Author: jmuirhead
###############################################################################

# Pass parameters to model based on arguments supplied to Rscript

param_iter <- 2

suppressMessages(TRUE)

library("methods")
library("utils")
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
library("fs")

root_crit <- has_dirname("PWS_2_5_west_coast_model", subdir = "src")
root_dir <- root_crit$make_fix_file()

options(tibble.width = Inf)

# Imput model parameters from YAML file

yaml_params <- yaml::read_yaml(path(root_dir(), "params.yaml"))

# Set up tracer and info loggers

log_name <- function(x) path(
    root_dir(), "logs",
    sprintf("parameter%03d_%s", as.numeric(param_iter[1]), x)
  )

flog.logger("stab_ports_pop_trace", TRACE,
  appender = appender.tee(log_name("stab_ports_pop_trace.log"))
  )

flog.logger(
  name = "model_progress", INFO,
  appender = appender.console()
)


flog.logger(
  name = "juve_lag", TRACE,
  appender = appender.console()
)


flog.threshold(TRACE)

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
port_area <- yaml_params[["params"]][["port_area"]]# 312 times larger than max wsa for ships, 1546 times larger
  # than average ship wsa

# Define carrying capacity per square meter
max_density_individuals <- yaml_params[["params"]][["max_density_individuals"]]

pop_transition <- readRDS(file.path(root_dir(), "data",
	"bal_improvisus_pop_stab_transition.rds"))


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

data_directory <- path(
  root_dir(), "data"
)

flog.info("data directory: %s", data_directory, name = "model_progress.log")

ports_pop_temp <- readRDS(path(
  data_directory, "ship_movements",
  "ports_array", ext = "rds"))



# Get list of bootstrapped ship populations
boot_filelist <- dir_ls(
  path = data_directory,
  glob = "*chunk*", recursive = TRUE
)

# Reshape some of the arrays for the population growth model
source(path(root_dir(), "munge", "04-select_ship_sources_for_caribbean.R"))

# Setup invasion status (population density of organisms) of initial ports
# (ports_array)

seed_ports_fn <- function(param, seed_names, ports_pop_input, lifestages) {

  # Check if port names matches up with the ports_ppop_input
  flog.info("Port name check: %s", all(dimnames(ports_pop_input)[[3]] %in%
    port_data[["port"]]) == TRUE, name = "model_progress.log")

  n_lifestages <- length(lifestages)

  n_at_carrying_capacity <- param[, "k_ports"]

  n_at_stability <- c(
    larva =  8905713602, cyprid = 936133112,
    juvenile = 108947634, adult = 325860930
  )

  seed_value <- array(
    data = rep(n_at_stability,
      each = dim(ports_pop_input)[1]
    ),
    dim = list(dim(ports_pop_input)[1], length(lifestages), nrow(seed_names)),
    dimnames = list("time_idx" = dimnames(ports_pop_input)[[1]],
      "lifestage" = names(lifestages),
      "port" = seed_names$port
    )
  )

  # Set up some populations in the seed ports
  # Note: This matches the port names from
  # seed ports, all the dates and lifestages

  afill(ports_pop_input, local = TRUE) <- seed_value

  dimnames_list <- dimnames(ports_pop_input)
  names(dimnames_list) <- c("time_idx", "lifestage", "port")
  dimnames(ports_pop_input) <- dimnames_list
  ports_pop_input
}

# Source c++ version of population growth
sourceCpp(path(root_dir(), "src", "popgrow.cpp"), verbose = FALSE)

# Source c++ version of stochastic matrix
sourceCpp(path(root_dir(), "src", "stoch_pop_growth_stab.cpp"), verbose = FALSE)


# Add a dimension for the number of life stages in the population
ports_pop_temp <- ports_array_add(ports_pop_temp,
  lifestages = ship_to_port_lifestages
)

ports_pop <- seed_ports_fn(
  param = parameter_grid[param_iter, ],
  seed_names = seed_ports, ports_pop_temp,
  lifestages = ship_to_port_lifestages
)

source(path(root_dir(), "munge", "02a-pop_stability_model.R"))

# Run main model ---------------------------------------------------------------

sourceCpp(path(root_dir(), "src", "fill_cube.cpp"), verbose = FALSE)

model_run <- main_model_fn(ship_imo_tbl = ship_imo_tbl,
  param = parameter_grid[param_iter, ],
  A_mat = pop_transition,
  ports_pop = ports_pop,
  root_dir(),
  param_iter,
  boot_iter
)

flog.info("Finished model run", name = "model_progress.log")
