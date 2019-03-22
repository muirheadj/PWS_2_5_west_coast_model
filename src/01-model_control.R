# This is the main control program for the infection model for various levels
# of transmission probability. It runs the model for each combination of
# transmission probability and then saves the results to lists at the end.
#
# Author: jmuirhead
###############################################################################


# Pass parameters to model based on arguments supplied to Rscript
param_iter <- 1

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

# Helper functions
chunkr <- function(vec, chunk_size = NULL, n_chunks = NULL,
	use_bit_package = FALSE){
  if (is.null(chunk_size) && is.null(n_chunks)) {
    stop(stri_c("You must provide either the size of the chunks ",
    	" or number of desired chunks"))
  }

  if (is.null(chunk_size)) {
    if (n_chunks == 1) {
      chunk <- vec
    } else {
      chunk <- split(vec, cut(seq_along(vec), n_chunks, labels = FALSE))
    }
  }
  if (is.null(n_chunks)) chunk <- split(vec,
  	ceiling(seq_along(vec) / chunk_size))

  if (use_bit_package == TRUE) chunk <- bit::chunk(from = 1, to = length(vec),
  	by = chunk_size, length.out = n_chunks)
  chunk
}

`%nin%` <- Negate(`%in%`)

# Wrapper for table to always report missing values
table <- function(..., useNA = "always") base::table(..., useNA = useNA)

# Set up tracer and info loggers

log_name <- function(x) paste0("parameter",
  sprintf("%03d", as.numeric(param_iter[1])), "_", x)

flog.logger(name = "ports_pop_trace", TRACE,
  appender = appender.tee(log_name("ports_pop_trace.log")))
flog.logger(name = "ports_n_trace", TRACE,
  appender = appender.tee(log_name("ports_n_trace.log")))
flog.logger(name = "model_progress", INFO,
  appender = appender.console())
flog.logger(name = "juve_lag", TRACE,
  appender = appender.tee(log_name("juvenile_trace.log")))
flog.logger(name = "ships_pop_trace", TRACE,
  appender = appender.tee(log_name("ships_pop_trace.log")))
flog.logger(name = "ports_instant_mortality_trace", TRACE,
  appender = appender.tee(log_name("ports_instant_mortality_trace.log")))

flog.threshold(TRACE, name = "ports_pop_trace")
flog.threshold(WARN, name = "ports_n_trace")
flog.threshold(WARN, name = "juve_lag")
flog.threshold(WARN, name = "ships_pop_trace")
flog.threshold(WARN, name = "ports_instant_mortality_trace")

# Identify species
sp_name <- yaml_params[["params"]][["sp_name"]]


# Set up lifestages, incl which lifestages disperse (sp = ship to port)
ship_to_port_lifestages <- c("larva" = 1, "cyprid" = 0, "juvenile" = 0,
  "adult" = 0)
port_to_ship_lifestages <- c("larva" = 0, "cyprid" = 1, "juvenile" = 0,
  "adult" = 0)


 # Population transition matrix
 pop_transition <- matrix(data = c(0.7, 0, 0, 2.4, 0.12, 0.7, 0, 0, 0, 0.08,
   0.8, 0, 0, 0, 0.12, 0.975), nrow = 4, ncol = 4, byrow = TRUE)

 dimnames(pop_transition)[[1]] <- dimnames(pop_transition)[[2]] <-
   names(ship_to_port_lifestages)

# Reproductive and development time lag in seconds
larval_dev_lag <- yaml_params[["params"]][["larval_dev_lag"]] # 7 days until cyprids can appear
juvenile_lag <- yaml_params[["params"]][["juvenile_lag"]] # 1 day until juveniles can first appear after cyprids appear in the population
mature_lag <- yaml_params[["params"]][["mature_lag"]] # McDonald et al 2009: 8 weeks post-settlement
repro_lag <- yaml_params[["params"]][["repro_lag"]] # McDonald et al 2009: 10 weeks post-settlement

# Read in table of port information for seed ports and habitat suitability

port_data <- read.csv(file.path(root_dir(), "data", "port_data.csv"),
  stringsAsFactors = FALSE)


# Define port area in square meters
port_area <- yaml_params[["params"]][["port_area"]] # 312 times larger than max wsa for ships, 1546 times larger
# than average ship wsa

# Define carrying capacity per square meter
max_density_individuals <- yaml_params[["params"]][["max_density_individuals"]]

# Generate combination of parameters
parameter_grid <- expand.grid(
  species  = yaml_params[["params"]][["sp_name"]],
  seed_ports = yaml_params[["params"]][["seed_ports"]],
  port_area = port_area,
  k_ports = port_area * max_density_individuals,
  fw_reduction = yaml_params[["params"]][["fw_reduction"]],
  max_density_individuals = max_density_individuals,
  larval_dev_lag = larval_dev_lag,
  juvenile_lag = juvenile_lag,
  mature_lag = mature_lag,
  repro_lag = repro_lag,
  ship_port_prob = yaml_params[["params"]][["ship_port_prob"]],
  port_compentency_prob = yaml_params[["params"]][["port_comptency_prob"]],
  stringsAsFactors = FALSE)

parameter_grid <- parameter_grid %>%
  filter(!(species == "hypothetical_sp" & seed_ports == "seed_ports2"))

# Extract Ports habitat suitability as a vector --------------------------------

habitat_suitability_colname <-  paste(parameter_grid[param_iter, "species"],
  "habitat_suitability", sep = ".")

ports_habitat_suitability <- port_data %>%
  select_at(vars(habitat_suitability_colname)) %>%
  unlist(use.names = FALSE)

names(ports_habitat_suitability) <- port_data %>%
  purrr::pluck("port")

 # Identify seed ports ---------------------------------------------------------

seed_ports_colname <- paste(parameter_grid[param_iter, "species"],
  parameter_grid[param_iter, "seed_ports"], sep = ".")

# Extract seed ports and store in a vector
seed_port_names <- port_data %>%
  filter_at(vars(seed_ports_colname), all_vars(. == 1)) %>%
  select(port)

# Re-assign date_list_ext
full_date_list <- format(seq(from = as.POSIXct("2010-01-01 00:00:00",
      tz = "UTC"),
    to = as.POSIXct("2017-12-31 18:00:00", tz = "UTC"),
    by = '6 hours'), format = "%Y-%m-%d %H:%M:%S")

# Cycle through bootstrap population loops
boot_iter <- yaml_params[["params"]][["boot_iter"]]

scenario <- yaml_params[["params"]][["scenario"]]

data_directory <- file.path(root_dir(), "data", scenario)

flog.info("data directory: %s", data_directory,  name = "model_progress.log")

# Get ship imo info, ships_array and ports_array (population arrays)
ship_imo_tbl <- readRDS(file.path(data_directory, "ship_imo_tbl.rds"))

# Set the effective wsa for ships to be 10% of the wetted surface area
effective_wsa_scale <- yaml_params[["params"]][["effective_wsa_scale"]]

ship_imo_tbl[["effective_wsa"]] <- ship_imo_tbl[["wsa"]] * effective_wsa_scale

summary(ship_imo_tbl$effective_wsa)

ships_pop_temp <- readRDS(file.path(data_directory, "ships_array.rds"))
ports_pop_temp <- readRDS(file.path(data_directory, "ports_array.rds"))


#ships_instant_mortality <- ships_pop_temp
ports_instant_mortality <- ports_pop_temp

# Make sure the ports_pop_temp matrix names are in the right order
dimnames(ports_pop_temp)[["port"]] <- sort(dimnames(ports_pop_temp)[["port"]])

dimnames(ports_instant_mortality)[["port"]] <-
  sort(dimnames(ports_instant_mortality)[["port"]])


# Get list of bootstrapped ship populations
boot_filelist <- list.files(path = data_directory,
  pattern = "chunk[0-9]{3}.rds$", full.names = FALSE, recursive = TRUE)

# Reshape some of the arrays for the population growth model
source(file.path(root_dir(), "munge", "04-select_ship_sources_for_caribbean.R"))

# Setup invasion status (population density of organisms) of initial ports
# (ports_array)

seed_ports_fn <- function(param, seed_names, ports_pop_input, lifestages){

  # Check if port names matches up with the ports_ppop_input
  flog.info("Port name check: %s", all(dimnames(ports_pop_input)[[3]] %in%
              port_data[["port"]]) == TRUE, name = "model_progress.log")

  seed_ports_df <- port_data[port_data$port %in%
      seed_names &
      port_data$port %in% dimnames(ports_pop_input)[[3]], ] %>%
    arrange(port)

  n_lifestages <- length(lifestages)

  n_at_carrying_capacity <- param[, "k_ports"]

  n_at_stability <- c(larva = 3334269129, cyprid = 1000280739,
  	juvenile = 264593012, adult = 416783641)

  seed_value <- array(data = rep(n_at_stability,
  	  each = dim(ports_pop_input)[1]),
    dim = list(dim(ports_pop_input)[1], length(lifestages), nrow(seed_names)),
    dimnames = list(dimnames(ports_pop_input)[[1]], names(lifestages),
      seed_ports_df[["port"]]))

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
  lifestages = ship_to_port_lifestages)
ports_pop_temp <- ports_array_add(ports_pop_temp,
  lifestages = ship_to_port_lifestages)
ports_pop <- seed_ports_fn(param = parameter_grid[param_iter, ],
  seed_names = seed_port_names, ports_pop_temp, 
  lifestages = ship_to_port_lifestages)

source(file.path(root_dir(), "src", "02-main_model.R"))

# Run main model

sourceCpp(file.path(root_dir(), "src", "arma_cube.cpp"), verbose = TRUE,
          rebuild = TRUE)


model_run <- main_model_fn(ship_imo_tbl, param = parameter_grid[param_iter, ],
	pop_transition, ports_pop, root_dir(), data_directory, param_iter, boot_iter,
    ship_to_port_lifestages, port_to_ship_lifestages, ships_instant_mortality,
    ports_instant_mortality, ports_habitat_suitability)

  flog.info("Finished model run", name = "model_progress.log")
