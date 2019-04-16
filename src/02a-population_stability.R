################################################################################
# This program contains the main model and simulates infestion for given levels
# of ship to port transmission probability, port to ship transimission
# probability.
# Author: jmuirhead
################################################################################

# Helper functions

find_date_in_chunk_fn <- function(date_slice, date_list_ext_chunks) {
  # This function finds out which chunk a particular date is in.  The 'name' of
  # vector is the chunk name, and its value is the position within that chunk
  t1 <- lapply(date_list_ext_chunks, function(x) match(x, date_slice))
  t2 <- unlist(lapply(t1, function(x) which(x == 1, arr.ind = TRUE)))
  t2
}

find_date_in_position_fn <- function(date, ship_position) {

  # This function finds out the corresponding position in the ship position_array
  # a particular date is in.

  date <- as.character(date)
  t1 <- match(date, dimnames(ship_position)[[3]])
  t1
}


# This function matches the names of an 3-D array and a matrix, when
# providing a date for the row. Indices are 0-based for use with c++.
cube_match_fn <- function(global_date, A, y) {
  match_date <- match(global_date, dimnames(A)[[1]])
  idx_match2 <- match(rownames(y), dimnames(A)[[2]])
  idx_match3 <- match(colnames(y), dimnames(A)[[3]])
  c_idx <- as.matrix(expand.grid(match_date, idx_match2, idx_match3) - 1)
  c_idx
}

# This function matches the names of an matrix and and a matrix with
# a single row. Indices are 0-based for use with c++.
matrix_match_fn <- function(A, y, default_rowname = "juvenile") {
  if (is.vector(y)) {
    idx_match1 <- match(default_rowname, dimnames(A)[[1]])
    idx_match2 <- match(names(y), dimnames(A)[[2]])
  }

  if (is.matrix(y)) {
    idx_match1 <- match(rownames(y), dimnames(A)[[1]])
    idx_match2 <- match(colnames(y), dimnames(A)[[2]])
  }
  c_idx <- as.matrix(expand.grid(idx_match1, idx_match2) - 1)
  c_idx
}


# MAIN MODEL BEGINS #-----------------------------------------------------------
main_model_fn <- function(ship_imo_tbl, param_grid, A_mat, ports_pop, ...) {

  habitat_threshold <- param_grid[["habitat_threshold"]]
  port_area <- param_grid[["port_area"]]
  k_ports <- param_grid[["k_ports"]]
  max_density_individuals <- param_grid[["max_density_individuals"]]
  larval_dev_lag <- 0
  juvenile_lag <- 0
  mature_lag <- 0
  repro_lag <- 0
  fw_reduction <- param_grid[["fw_reduction"]]
  ship_port_prob <- param_grid[["ship_port_prob"]]
  port_compentency_prob <- param_grid[["port_compentency_prob"]]

  # Get sequence of datetimes in POSIXct format
  dateseq <- as.POSIXct(dimnames(ports_pop)[[1]], tz = "UTC")

  # As character
		date_list_ext <- dimnames(ports_pop)[[1]]

  # date to set life-history time lag limit

  date_pastend <- dateseq[length(dateseq)] + (24 * 3600)

  # Initialize reproductive status for ports and ships
  port_lifehistory_status <- data.frame(
    name = dimnames(ports_pop)[[3]],
    larval_time = date_pastend,
    juvenile_time = date_pastend,
    mature_time = date_pastend,
    reprod_time = date_pastend, stringsAsFactors = FALSE
  )

  # Functions that determine invasion status for ports and ships
  lifehistory_lag <- function(x) {
    res <- names(x[x > 0])
    res
  }

  for (t_global in seq_along(date_list_ext)) {
    # Get the time slice position in the chunk as well as the name in the chunk
    # Current date

    current_datetime <- dateseq[t_global]
    # date as character format for array position matching
    t_date_global <<- date_list_ext[t_global]


    # Add time lags before life-history demographic parameters take effect
    # If t_global is equal to that date, change value in A_port_stoch for that
    # port

    t_larva <- current_datetime + larval_dev_lag
    t_juvenile <- current_datetime + juvenile_lag
    t_mature <- current_datetime + mature_lag
    t_reproduce <- current_datetime + repro_lag

    # Date at position t_global - 1 # Note the double <<- for Global assignment

    t1_date_global <<- ifelse(t_global > 1, date_list_ext[t_global - 1],
      date_list_ext[t_global]
    )

    # Find out which ports and ships have been invaded, and return the names

    if (t_global == 1) {
      larval_invaded_port_names <- lifehistory_lag(ports_pop[t_global, 1, ])
      juvenile_invaded_port_names <- lifehistory_lag(ports_pop[t_global, 3, ])
      mature_invaded_port_names <- lifehistory_lag(ports_pop[t_global, 3, ])
      reprod_invaded_port_names <- lifehistory_lag(ports_pop[t_global, 4, ])

    } else {
      # look at just the larvae
      larval_invaded_port_names <-
        lifehistory_lag(ports_pop[(t_global - 1), 1, ])


      # look at cyprid population
      juvenile_invaded_port_names <-
        lifehistory_lag(ports_pop[(t_global - 1), 2, ])


      # look at just the juveniles
      mature_invaded_port_names <-
        lifehistory_lag(ports_pop[(t_global - 1), 3, ])

      # look at just the adults
      reprod_invaded_port_names <-
        lifehistory_lag(ports_pop[(t_global - 1), 4, ])

    }

    # Update development and reproduction schedule

    # Seed ports do not undergo any time delay

    if (t_global == 1) {
      # Time index one, and adult lifestages
      seed_ports <- names(which(ports_pop[1, 4, ] > 10000))

      port_lifehistory_status[
        port_lifehistory_status$name %in% seed_ports,
        "larval_time"
      ] <-
        as.POSIXct("2010-01-01 00:00:00", tz = "UTC", origin = "1970-01-01")

      port_lifehistory_status[
        port_lifehistory_status$name %in% seed_ports,
        "juvenile_time"
      ] <-
        as.POSIXct("2010-01-01 00:00:00", tz = "UTC", origin = "1970-01-01")

      port_lifehistory_status[
        port_lifehistory_status$name %in% seed_ports,
        "mature_time"
      ] <-
        as.POSIXct("2010-01-01 00:00:00", tz = "UTC", origin = "1970-01-01")

      port_lifehistory_status[
        port_lifehistory_status$name %in% seed_ports,
        "reprod_time"
      ] <-
        as.POSIXct("2010-01-01 00:00:00", tz = "UTC", origin = "1970-01-01")
    }

    # Calculate stochastic population projection matrix given the time lags
    # for each port and each ship

    seed_port_positions <- which(dimnames(ports_pop)[[3]] %in% seed_ports,
				  arr.ind = TRUE)

    port_pop_transition <- port_stoch_pop(A_mat, current_datetime,
      port_lifehistory_status
    )

    # Calculate population change in ports (ports_pop[date, lifestage, port])
    if (t_global > 1) {

      # Population size for lifestages except juveniles
      N_ports <- popgrow_dbl(port_pop_transition, ports_pop[(t_global - 1), , ])

      dimnames(N_ports) <-
        list(dimnames(ports_pop)[[2]], dimnames(ports_pop)[[3]])

      # Apply carrying capacity upper limit
      ports_logit_factor <- (k_ports - N_ports) / k_ports

      # Larva and cyprids don't have a carrying capacity
      ports_logit_factor[which(dimnames(ports_logit_factor)[[1]] %in%
        c("larva", "cyprid")), ] <- 1

      temp_ports_pop <- N_ports * ports_logit_factor

      # Adjust for error where population persists when only 2 individuals are
      # left

      temp_ports_pop[temp_ports_pop <= 2] <- 0

      # Round up to nearest integer
      temp_ports_pop <- ceiling(temp_ports_pop)

    } else {
      # In the first time slice, so no t-1 time position
      temp_ports_pop <- ports_pop[t_global, , ]
    }

    ports_pop_idx <- cube_match_fn(t_date_global, ports_pop, temp_ports_pop)

    fill_cube_dbl(ports_pop, temp_ports_pop, ports_pop_idx)


    ports_trace_pop <-
      rowMeans(temp_ports_pop[, dimnames(temp_ports_pop)[[2]] %in% seed_ports])

    flog.trace("parameter%s port_population %i %f %f %f %f", sprintf(
      "%.03d",
      param_iter
    ), t_global, ports_trace_pop[1], ports_trace_pop[2],
    ports_trace_pop[3], ports_trace_pop[4],
    name = "stab_ports_pop_trace",
    capture = FALSE
    )

  } # End of time processing loop

  # Create directories to store results
  parameter_id <- param_grid[["parameter_id"]]
  results_dir <- file.path(root_dir(), "results", "population_stability",
    parameter_id)

  alphabet_it <- iterators::iter(letters[1:26])

  # Creates file directories with additional letter to avoid overwriting old
  # files

  if (file.exists(results_dir)) {
    results_dir <- paste0(
      results_dir,
      nextElem(alphabet_it)
    )
  }
  dir.create(results_dir, recursive = TRUE)

  # Save data
  param <- list(
    parameter = parameter_id,
    species = param_grid[["species"]],
    seed_ports = param_grid[["seed_ports"]],
    habitat_threshold = habitat_threshold,
    port_area = port_area,
    k_ports = k_ports,
    fw_reduction = fw_reduction,
    max_density_individuals = max_density_individuals,
    larval_dev_lag = larval_dev_lag,
    mature_lag = mature_lag,
    repro_lag = repro_lag,
    ship_port_prob = ship_port_prob,
    port_compentency_prob = port_compentency_prob
  )

  saveRDS(param, file = file.path(results_dir, sprintf(
    "parameters_%s%s",
    Sys.Date(), ".RData"
  )))

  flog.info("Parameters saved", name = "model_progress.log")

  saveRDS(ports_pop, file = file.path(
    results_dir,
    sprintf("ports_pop_%s%s", Sys.Date(), ".RData")
  ))

  flog.info("ports_pop saved", name = "model_progress.log")

} # End of main_model_fn statement
