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

# Enter all the parameters necessary for each of the sub functions in
# ship_port_prob
ship_port_prob_fn <- function(x, max_rate, rate) {
  # Inputs: ship characteristic table (ship_imo_tbl), maximum and rate of
  # probability of ship to port transfer based on wetted surface area (i.e.
  # spprobmax, spprobrate).  Returns the associated probability
  rr <- max_rate * (1 - exp(-1 * rate * x$wsa))
  rr
}

ship_and_port_names_from_ship_position <- function(ship_position, time_index){
  # This function takes a slice of the ship_position array and returns
  # the names of the ships, and the ports where the ships are located.

  current_position <- ship_position[,, time_index, drop = FALSE]
  arr_idx_current_position <- arrayInd(which(!is.na(current_position)),
    .dim = dim(current_position),
    .dimnames = dimnames(current_position), useNames = TRUE)

  port_and_ship_names <- list(
    port_names = dimnames(current_position)[[2]][arr_idx_current_position[, 2]],
    ship_names = dimnames(current_position)[[1]][arr_idx_current_position[, 1]])
  port_and_ship_names
}

# PORT CYPRID COMPENTENCY ------------------------------------------------------
port_cyprid_compentency_fn <- function(ports_array = ports_array,
  port_competency_proportion = port_compentency_prob,
  lifestage_vec = port_to_ship_lifestages, x = t_global,
  port_emigration_input = port_cyprid_compentency,
  port_lifehistory_input = port_lifehistory_status) {


  # This function calculations the outgoing emigration of dispersing lifestages
  # from the previous time step. Note: Emigration takes place even when the
  # ships aren't there.

  n_ports <- dim(ports_array)[[3]]

  # No emigration before the second time step of the model
  if (x > 1) {
    # Probability that the cyprid is compentent
    p_emigration_competent_proportion <- rbinom(n_ports, size = 1000,
      p = port_competency_proportion) / 1000

    # Check to see if the juvenile time lag has elapsed. This returns either the
    # proportion or a 0 if the time has not elapsed.

    p_emigration_competent_proportion <- p_emigration_competent_proportion *
      ((t1_date_global >= port_lifehistory_input[["juvenile_time"]]) + 0)

    flog.trace("Juvenile time lag logic",
      table(((t1_date_global >=
        port_lifehistory_input[["juvenile_time"]]) + 0)),
      name = "juve_lag_log", capture = TRUE)

    # Number of compentent cyprids that are available to immigrate to the ships

    p_emigration_mat <- ceiling(outer(lifestage_vec,
                p_emigration_competent_proportion) *
        ports_array[(x - 1),,, drop = TRUE])

    dimnames(p_emigration_mat) <- list(dimnames(ports_array)[[2]],
      dimnames(ports_array)[[3]])

    afill(port_emigration_input, t1_date_global,,, local = TRUE) <-
      p_emigration_mat

  }
  port_emigration_input
}

# PORT IMMIGRATION -------------------------------------------------------------
port_immigration_fn <- function(ports_array = ports_array,
  lifestage_vec = ship_to_port_lifestages,
  ship_emigration = ship_emigration, ship_position = ship_position,
  t1 = t1_position_idx,
  port_immigration_input = port_immigration, x = t_global,
  ports_instant_mortality, ports_habitat_suitability) {

  # Make sure to use ship position from t_global - 1
  if (x > 1) {
    t1_global <- x - 1
    port_names <-
      ship_and_port_names_from_ship_position(ship_position, t1)[["port_names"]]

    ship_names <-
      ship_and_port_names_from_ship_position(ship_position, t1)[["ship_names"]]

    # Need to merge ship_emigration with port_information
    # Get output of emigration for each lifestage from each of the ships
    # ship = rows , lifestage = columns

    ship_emigration_t <- t(as.matrix(x = ship_emigration[t1_global, ,
      match(ship_names, dimnames(ship_emigration)[[3]]), drop = TRUE] *
        lifestage_vec, rownames.force = TRUE))

   # Combine with the port_status to get ship, port and lifestage information
   #  into a  single array

    if (length(ship_names) > 0 & length(port_names) > 0) {

    ship_port_migration <- data.frame(ship = ship_names, port = port_names,
      ship_emigration_t, stringsAsFactors = FALSE)

    ship_port_migration_tidy <- tidyr::gather(ship_port_migration,
      key = "lifestage", value = "pop", -ship, -port, factor_key = TRUE)

    port_migration_subset <- acast(data = ship_port_migration_tidy,
        ship ~ lifestage ~ port, value.var = "pop", drop = FALSE)

    port_immigration_total <- apply(port_migration_subset, c(2, 3),
      function(x) ceiling(sum(x, na.rm = TRUE)))

    # Add stochastic establishment for larva
    prob_establishment <- 1 - exp(-1e-5 * port_immigration_total[1, ])

    p_random_inst_mort <- runif(n = length(prob_establishment), min = 0,
      max = 1)

  # Add in environmental matching for each port
    names(ports_habitat_suitability) %in% colnames(port_immigration_total)

    ports_habitat_suitability_sub <-
      ports_habitat_suitability[names(ports_habitat_suitability) %in%
        colnames(port_immigration_total)]

    port_immigration_total[1, ] <- ports_habitat_suitability_sub *
       port_immigration_total[1, ]

  # Keep track of instant mortality for ports

    ports_instant_mortality[t1_global, match(names(prob_establishment),
        colnames(ports_instant_mortality))] <<-
      (p_random_inst_mort < prob_establishment)

    port_immigration_total[1, ] <- (p_random_inst_mort < prob_establishment) *
      port_immigration_total[1, ]

    ports_immigration_idx <- cube_match_fn(t1_date_global,
      A = port_immigration_input, y = port_immigration_total)

    cube_fill_row(port_immigration_input, port_immigration_total,
      ports_immigration_idx)

    }
  } # End of (if x > 1)
  port_immigration_input
}  # End of port immigration density function

# SHIP EMIGRATION
ship_emigration_fn <- function(ships_array, ships_invasion_prob,
  lifestage_vec = ship_to_port_lifestages, x = t_global,
  ship_emigration_input = ship_emigration) {

  # This function calculations the outgoing emigration of dispersing lifestages
  # from the previous time step.

  if (x > 1) {
    t1_global <- x - 1

    ship_emigration_mat <- ceiling(outer(lifestage_vec, ships_invasion_prob) *
      ships_array[t1_global, , , drop = TRUE])

    ship_emigration_idx <- cube_match_fn(t1_date_global,
      A = ship_emigration_input, y = ship_emigration_mat)

    cube_fill_row(ship_emigration_input, ship_emigration_mat,
      ship_emigration_idx)
  }
  ship_emigration_input
}

# SHIP IMMIGRATION OF JUVENILES
ship_immigration_fn <- function(ships_pop,
  lifestage_vec = port_to_ship_lifestages,
  port_emigration = port_cyprid_compentency,
  ship_position = ship_position, t1 = t1_position_idx,
  ship_immigration_input = ship_immigration, x = t_global) {


  if (x > 1) {
    t1_global <- x - 1

    port_names <-
      ship_and_port_names_from_ship_position(ship_position, t1)[["port_names"]]
    ship_names <-
      ship_and_port_names_from_ship_position(ship_position, t1)[["ship_names"]]

    # Get port emigration at time t-1

    port_emigration_temp <- data.frame(t(port_emigration[t1_global, , ] *
      lifestage_vec), stringsAsFactors = FALSE)

    names(port_emigration_temp) <- paste0(names(port_emigration_temp), "_emigr")

    port_emigration_temp[["ports"]] <- row.names(port_emigration_temp)

    if (length(port_names) > 0 & length(ship_names) > 0) {
      combined_port_ship_names <- data.frame(ships = ship_names,
        ports = port_names, stringsAsFactors = FALSE)

      port_ship_migration <- left_join(combined_port_ship_names,
        port_emigration_temp, by = "ports")

    # Allocate amount to ships within each port
    # WSA scale is a scalar of the number of cyprids available weighted by the
    # relative surface area
    # frac_transferred is the random proportion that is able to be transferred
    # to the ship

      port_to_ship_migration_proportion <- port_ship_migration %>%
        left_join(ship_imo_tbl, by = c("ships" = "lrnoimoshipno")) %>%
        group_by(ports) %>%
        mutate(
          frac_transferred = rbinom(1, size = 1000, prob = 0.04) / 1000) %>%
        mutate(wsa_scale = effective_wsa / sum(effective_wsa, port_area,
          na.rm = TRUE)) %>%
        ungroup()

    # The number of new juveniles is the available pool scaled by relative
    # surface area and the probability of setting rounded up to the nearest
    # whole number.

      port_to_ship_migration_size <- port_to_ship_migration_proportion %>%
        mutate(juvenile = ceiling(wsa_scale * frac_transferred * cyprid_emigr))


    # Add stochastic establishment for juveniles
      prob_establishment <- 1 - exp(-1e-5 *
        port_to_ship_migration_size[["juvenile"]])
      p_random <- runif(n = length(prob_establishment), min = 0, max = 1)

    # Keep track of instant mortality on ships
    #ships_instant_mortality[match(names(prob_establishment),
    #  rownames(ships_instant_mortality)),
    #(x - 1)] <<- (p_random < prob_establishment)

      port_to_ship_migration_size[["juvenile"]] <-
        (p_random < prob_establishment) *
          port_to_ship_migration_size[["juvenile"]]

   # Reshape into a matrix

      ship_immigration_size_subset <- port_to_ship_migration_size %>%
        ungroup() %>%
        select(ships, juvenile) %>%
        melt(value.name = "pop", id.vars = "ships") %>%
        acast(variable ~ ships, value.var = "pop",
          fun.aggregate = function(x) sum(x, na.rm = TRUE), drop = FALSE)

      ships_immnigration_idx <- cube_match_fn(t1_date_global,
        A = ship_immigration_input,
        y = ship_immigration_size_subset)

    # Afill into 3-d array for ship_immigration
      cube_fill_row(ship_immigration_input, ship_immigration_size_subset,
        ships_immnigration_idx)
    }
  }
  ship_immigration_input
}

# Port cyprid settlement
port_juvenile_production_fn <- function(ports_pop,
  lifestage_vec = port_to_ship_lifestages,
  port_emigration = port_cyprid_compentency, ship_position,
  t1 = t1_position_idx, x = t_global){

  if (x > 1) {
    t1_global <- x - 1

    port_names <-
      ship_and_port_names_from_ship_position(ship_position, t1)[["port_names"]]

    ship_names <-
      ship_and_port_names_from_ship_position(ship_position, t1)[["ship_names"]]

    # Get port emigration at time t-1
    port_emigration_temp <- data.frame(t(port_emigration[t1_global,, ] *
      lifestage_vec), stringsAsFactors = FALSE)

    names(port_emigration_temp) <- paste0(names(port_emigration_temp), "_emigr")
    port_emigration_temp[["ports"]] <- row.names(port_emigration_temp)

    if (length(port_names) > 0 & length(ship_names) > 0) {
      combined_port_ship_names <- data.frame(ships = ship_names,
        ports = port_names, stringsAsFactors = FALSE)

      port_port_juve_production <- right_join(combined_port_ship_names,
        port_emigration_temp, by = "ports")

    # Allocate amount to ships within each port
    # WSA scale is a scalar of the number of cyprids available weighted by the
    # relative surface area
    # frac_transferred is the random proportion that is able to be transferred
    # to the ship

      port_port_juvenile_proportion <- port_port_juve_production %>%
        left_join(ship_imo_tbl, by = c("ships" = "lrnoimoshipno")) %>%
        group_by(ports) %>%
        mutate(wsa_scale = port_area / sum(effective_wsa, port_area,
          na.rm = TRUE),
        frac_transferred = rbinom(1, size = 1000, prob = 0.04) / 1000)

    # The number of new juveniles is the available pool scaled by relative
    # surface area and the probability of setting rounded up to the nearest
    # whole number.

      port_juvenile_production_size <- port_port_juvenile_proportion %>%
        mutate(juvenile = ceiling(wsa_scale * frac_transferred * cyprid_emigr))

    # Reshape into a matrix
      port_juvenile_size_subset <- port_juvenile_production_size %>%
        ungroup() %>%
        select(ports, juvenile) %>%
        melt(value.name = "pop", id.vars = "ports") %>%
        acast(variable ~ ports, value.var = "pop",
          fun.aggregate = function(x) mean(x, na.rm = TRUE), drop = TRUE)
      }
  }

  if (x == 1) {
    port_juvenile_size_subset <- rep(0, length = dim(port_emigration)[[3]])
    names(port_juvenile_size_subset) <- dimnames(port_emigration)[[3]]
  }
  port_juvenile_size_subset
}


# Freshwater dip reduction function
fw_reduction_fn <- function(ships_pop_input, x, ship_position,
  t1 = t1_position_idx, fw = fw_reduction){
  # Get names of ships that are in the Panama Canal at time t-1

  ship_position_panama_canal <- ship_position[,
    dimnames(ship_position)[[2]] %in% "Panama Canal", t1, drop = TRUE]

  ship_names_pc <-
    names(ship_position_panama_canal[!is.na(ship_position_panama_canal)])

  if (!is.null(ship_names_pc)) {
    ships_pop_input[(x - 1),,
      dimnames(ships_pop_input)[[3]] %in% ship_names_pc] <-
      fw * ships_pop_input[(x - 1),,
      dimnames(ships_pop_input)[[3]] %in% ship_names_pc]
    }
  ships_pop_input
}


ships_underway_larval_reduction_fn <- function(ships_pop_input, x,
  ship_position, t1 = t1_position_idx){
  # Get names of ships that are in the Panama Canal at time t-1
  position_underway <- ship_position[,, t1, drop = TRUE]
  ship_names_underway <- names(position_underway[is.na(position_underway)])

  if (!is.null(ship_names_underway)) {
    ships_pop_input[(x - 1), dimnames(ships_pop_input)[[2]] %in%
      c("larva", "cyprid"),
      dimnames(ships_pop_input)[[3]] %in% ship_names_underway] <- 0
  }
  ships_pop_input
}

# Pre-allocate arrays for port and ship emigration and immigration


port_cyprid_compentency <- array(0L, dim = dim(ports_pop),
  dimnames = dimnames(ports_pop))
port_immigration <- array(0L, dim = dim(ports_pop),
  dimnames = dimnames(ports_pop))
ship_emigration <- array(0L, dim = dim(ships_pop),
  dimnames = dimnames(ships_pop))
ship_immigration <- array(0L, dim = dim(ships_pop),
  dimnames = dimnames(ships_pop))


# MAIN MODEL BEGINS #-----------------------------------------------------------
main_model_fn <- function(ship_imo_tbl, param_grid, A_mat, ports_pop, ...) {

  port_area <- param_grid[["port_area"]]
  k_ports <- param_grid[["k_ports"]]
  max_density_individuals <- param_grid[["max_density_individuals"]]
  larval_dev_lag <- param_grid[["larval_dev_lag"]]
  juvenile_lag <- param_grid[["juvenile_lag"]]
  mature_lag <- param_grid[["mature_lag"]]
  repro_lag <- param_grid[["repro_lag"]]
  fw_reduction <- param_grid[["fw_reduction"]]
  seed_bioregions <- unlist(param_grid[["seed_bioregions"]])
  ship_port_prob <- param_grid[["ship_port_prob"]]
  port_compentency_prob <- param_grid[["port_compentency_prob"]]

  # Get sequence of datetimes in POSIXct format
  dateseq <- as.POSIXct(dimnames(ports_pop)[[1]], tz = "UTC")

  # date to set life-history time lag limit

  date_pastend <- dateseq[length(dateseq)] + (24 * 3600)

  # Create list of date chunks in order to figure out which bootstrap chunks to
  # pull out

  chunk_size <- 50

  date_list_ext <- dimnames(ships_pop)[[1]]
  date_list_ext_chunks <- chunkr(date_list_ext, chunk_size = chunk_size)

# Array to store ship specific probability of infecting ports
  ship_imo_tbl[["wsa_prob"]] <- ship_port_prob
  ships_invasion_prob <- ship_imo_tbl[["wsa_prob"]]
  names(ships_invasion_prob) <- ship_imo_tbl[["lrnoimoshipno"]]


  # Calculate the effective carrying capacity for each vessel based on 10000
  # individuals per square meter (5000 juveniles + 5000 adults)

  ship_imo_tbl[["k_ships"]] <- ship_imo_tbl[["effective_wsa"]] *
    max_density_individuals

  # Set up a matrix for the carrying capacity for each vessel
  k_ships <- matrix(data = rep(ship_imo_tbl[["k_ships"]],
          each = length(ship_to_port_lifestages)), nrow = 4)


  # Generate hybrid indicies to read the ff arrays
  hi_ship <- 1:dim(k_ships)[2]  # Index for ships
  hi_port <- 1:dim(ports_pop)[3]  # Index for ports
  hi_t1 <- 1:50  # Index for time slices 1:50
  hi_t2 <- 51:100  # Index for time slices 51:100

  last_chunk <- tail(date_list_ext_chunks, 1)
  last_chunk_name <- as.numeric(names(last_chunk)) # Number for the last chunk
  last_chunk_length <- length(unlist(last_chunk)) # Size of last chunk

  # Initialize current_chunk to 0
  current_chunk <- 0

  # Initialize reproductive status for ports and ships
  port_lifehistory_status <- data.frame(name = dimnames(ports_pop)[[3]],
    larval_time = date_pastend,
    juvenile_time = date_pastend,
    mature_time = date_pastend,
    reprod_time = date_pastend, stringsAsFactors = FALSE)

  ship_lifehistory_status <- data.frame(name = dimnames(ships_pop)[[3]],
    larval_time = date_pastend,
    juvenile_time = date_pastend,
    mature_time = date_pastend,
    reprod_time = date_pastend, stringsAsFactors = FALSE)

  # Functions that determine invasion status for ports and ships
  lifehistory_lag <- function(x){
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
    date_list_ext[t_global])

  # Current slice
  t_slice <- find_date_in_chunk_fn(t_date_global, date_list_ext_chunks)

  # What chunk of data the slice is in
  t_chunk <- as.numeric(names(t_slice))

  # Find out which ports and ships have been invaded, and return the names

  if (t_global == 1) {

    larval_invaded_port_names <- lifehistory_lag(ports_pop[t_global, 1, ])
    larval_invaded_ship_names <- lifehistory_lag(ships_pop[t_global, 1, ])

    juvenile_invaded_port_names <- lifehistory_lag(ports_pop[t_global, 3, ])
    juvenile_invaded_ship_names <- lifehistory_lag(ships_pop[t_global, 3, ])

    mature_invaded_port_names <- lifehistory_lag(ports_pop[t_global, 3, ])
    mature_invaded_ship_names <- lifehistory_lag(ships_pop[t_global, 3, ])

    reprod_invaded_port_names <- lifehistory_lag(ports_pop[t_global, 4, ])
    reprod_invaded_ship_names <- lifehistory_lag(ships_pop[t_global, 4, ])

  } else {

    larval_invaded_port_names <- lifehistory_lag(ports_pop[(t_global - 1), 1, ])
    # look at just the larvae

    larval_invaded_ship_names <- lifehistory_lag(ships_pop[(t_global - 1), 1, ])

    # look at cyprid population
    juvenile_invaded_port_names <-
      lifehistory_lag(ports_pop[(t_global - 1), 2, ])

    # look at juvenile population since cyprids are swept off the ship
    juvenile_invaded_ship_names <-
      lifehistory_lag(ships_pop[(t_global - 1), 3, ])

    # look at just the juveniles
    mature_invaded_port_names <- lifehistory_lag(ports_pop[(t_global - 1), 3, ])
    mature_invaded_ship_names <- lifehistory_lag(ships_pop[(t_global - 1), 3, ])

    # look at just the adults
    reprod_invaded_port_names <- lifehistory_lag(ports_pop[(t_global - 1), 4, ])
    reprod_invaded_ship_names <- lifehistory_lag(ships_pop[(t_global - 1), 4, ])
    }

    # Update development and reproduction schedule

    # Seed ports do not undergo any time delay

    if  (t_global == 1) {
    # Time index one, and adult lifestages
      seed_ports <- names(which(ports_pop[1, 4, ] > 10000))

      port_lifehistory_status[port_lifehistory_status$name %in% seed_ports,
        "larval_time"] <-
        as.POSIXct("2010-01-01 00:00:00", tz = "UTC", origin = "1970-01-01")

      port_lifehistory_status[port_lifehistory_status$name %in% seed_ports,
        "juvenile_time"] <-
        as.POSIXct("2010-01-01 00:00:00", tz = "UTC", origin = "1970-01-01")

      port_lifehistory_status[port_lifehistory_status$name %in% seed_ports,
        "mature_time"] <-
        as.POSIXct("2010-01-01 00:00:00", tz = "UTC", origin = "1970-01-01")

      port_lifehistory_status[port_lifehistory_status$name %in% seed_ports,
        "reprod_time"] <-
        as.POSIXct("2010-01-01 00:00:00", tz = "UTC", origin = "1970-01-01")
    }

    # Update time lag for larval development time if larvae present in
    # population

    port_lifehistory_status[(port_lifehistory_status$name %in%
      larval_invaded_port_names &
      port_lifehistory_status$larval_time == date_pastend &
      port_lifehistory_status$name %nin% seed_ports), "larval_time"] <- t_larva

    ship_lifehistory_status[(ship_lifehistory_status$name %in%
      larval_invaded_ship_names  &
      ship_lifehistory_status$larval_time == date_pastend), "larval_time"] <-
    t_larva

    # If current population goes to zero, reset time lag to past end of model
    port_lifehistory_status[(port_lifehistory_status$name %nin%
      larval_invaded_port_names &
      port_lifehistory_status$name %nin% seed_ports), "larval_time"] <-
    date_pastend

    ship_lifehistory_status[(ship_lifehistory_status$ship %nin%
      larval_invaded_ship_names), "larval_time"] <- date_pastend

    # Update time lag for appearance of juveniles in the population

    port_lifehistory_status[(port_lifehistory_status$name %in%
      juvenile_invaded_port_names &
      port_lifehistory_status$juvenile_time == date_pastend &
      port_lifehistory_status$name %nin% seed_ports), "juvenile_time"] <-
    t_juvenile

    ship_lifehistory_status[(ship_lifehistory_status$name %in%
      juvenile_invaded_ship_names &
      ship_lifehistory_status$juvenile_time == date_pastend),
    "juvenile_time"] <- t_juvenile

    # If current population goes to zero, reset time lag to past end of model

    port_lifehistory_status[(port_lifehistory_status$name %nin%
      juvenile_invaded_port_names &
      port_lifehistory_status$name %nin% seed_ports), "juvenile_time"] <-
    date_pastend

    ship_lifehistory_status[(ship_lifehistory_status$ship %nin%
      juvenile_invaded_ship_names), "juvenile_time"] <- date_pastend

    #  Update time lag for adult maturity
    port_lifehistory_status[(port_lifehistory_status$name %in%
      mature_invaded_port_names &
      port_lifehistory_status$mature_time == date_pastend &
      port_lifehistory_status$name %nin% seed_ports), "mature_time"] <-
    t_mature

    ship_lifehistory_status[(ship_lifehistory_status$name %in%
      mature_invaded_ship_names &
      ship_lifehistory_status$mature_time == date_pastend), "mature_time"] <-
    t_mature

    port_lifehistory_status[(port_lifehistory_status$name %nin%
      mature_invaded_port_names &
      port_lifehistory_status$name %nin% seed_ports), "mature_time"] <-
    date_pastend

    ship_lifehistory_status[(ship_lifehistory_status$name %nin%
        mature_invaded_ship_names), "mature_time"] <- date_pastend

   # Update time lag for reproduction

    port_lifehistory_status[(port_lifehistory_status$name %in%
      reprod_invaded_port_names &
      port_lifehistory_status$reprod_time == date_pastend &
      port_lifehistory_status$name %nin% seed_ports), "reprod_time"] <-
    t_reproduce

    ship_lifehistory_status[(ship_lifehistory_status$name %in%
      reprod_invaded_ship_names &
      ship_lifehistory_status$reprod_time == date_pastend), "reprod_time"] <-
    t_reproduce

    port_lifehistory_status[(port_lifehistory_status$name %nin%
      reprod_invaded_port_names & port_lifehistory_status$name %nin%
      seed_ports), "reprod_time"] <- date_pastend

    ship_lifehistory_status[(ship_lifehistory_status$name %nin%
              reprod_invaded_ship_names), "reprod_time"] <- date_pastend

    # Calculate stochastic population projection matrix given the time lags
    # for each port and each ship

    port_pop_transition <- port_stoch_pop(A_mat, current_datetime,
      port_lifehistory_status)

    ship_pop_transition <- ship_stoch_pop(A_mat, current_datetime,
      ship_lifehistory_status)

    # Load in chunks 1 and 2 of position data to start off with
    if (t_chunk <= 2) {
      chunk_load1 <- 1
      chunk_load2 <- 2
    } else {
      chunk_load1 <- t_chunk - 1
      chunk_load2 <- t_chunk
    }

    if (t_chunk != current_chunk) {
      chunk_1 <- readRDS(file.path(root_dir(), "data", scenario,
        sprintf("position_array_chunk%0.3d%s", chunk_load1, ".rds")))
      chunk_2 <- readRDS(file.path(root_dir(), "data", scenario,
        sprintf("position_array_chunk%0.3d%s", chunk_load2, ".rds")))

      if (t_chunk == 1) {
        ship_position <- array(data = as.logical(NA),
          dim = c(dim(chunk_1)[[1]], dim(chunk_1)[[2]],
            dim(chunk_1)[[3]] + dim(chunk_2)[[3]]),
          dimnames = list(dimnames(chunk_1)[[1]],
            dimnames(chunk_1)[[2]],
            c(dimnames(chunk_1)[[3]], dimnames(chunk_2)[[3]])))
      }

      # Update the position array by having chunk 1 in the first half and
      # chunk 2 in the second half. Also have to update the dates (ie.
      # dimnames(position_array)[[3]]) when the chunks are swapped in

      ship_position[hi_ship, hi_port, hi_t1] <-
        chunk_1[hi_ship, hi_port, hi_t1]

      if (t_chunk < last_chunk_name) {
        ship_position[hi_ship, hi_port, hi_t2] <- chunk_2[hi_ship, hi_port,
          hi_t1]
        dimnames(ship_position)[[3]] <- c(dimnames(chunk_1)[[3]],
          dimnames(chunk_2)[[3]])
      }

      if (t_chunk == last_chunk_name) {
      # Have to adjust for the last chunk which only has 5 'slices'

        ship_position[hi_ship, hi_port, 51:(51 + last_chunk_length - 1 )] <-
          chunk_2[hi_ship, hi_port, last_chunk_length]

        dimnames(ship_position)[[3]][1:(51 + last_chunk_length - 1 )] <-
          c(dimnames(chunk_1)[[3]], dimnames(chunk_2)[[3]])
      }

        t_position_idx <- find_date_in_position_fn(t_date_global, ship_position)

        # Also get the time slice position for the previous time step
        t1_position_idx <- find_date_in_position_fn(t1_date_global,
          ship_position)

        # Check to make sure the current chunk is updated on changes in the
        # chunk
        current_chunk <- t_chunk
        rm(chunk_1, chunk_2)

      } else {
        # Still have to update position even when chunk is current
        t_position_idx <- find_date_in_position_fn(t_date_global, ship_position)

        # Also get the time slice position for the previous time step

        t1_position_idx <- find_date_in_position_fn(t1_date_global,
          ship_position)
      }

    # Freshwater reduction for ships in the Panama Canal
    if (t_global > 1) {
      ships_pop <- fw_reduction_fn(ships_pop, x = t_global, ship_position,
        t1_position_idx, fw_reduction)

    # Reduction of larval and cyprid stages to zero when ship is underway
      ships_pop <- ships_underway_larval_reduction_fn(ships_pop,
        x = t_global, ship_position, t1_position_idx)
    }

    # Port emigration
    port_cyprid_compentency <- port_cyprid_compentency_fn(ports_pop,
      port_competency_proportion = port_compentency_prob,
      lifestage_vec = port_to_ship_lifestages, x = t_global,
      port_emigration_input = port_cyprid_compentency,
      port_lifehistory_input = port_lifehistory_status)

     # Ship emigration
     # REACHES MEMORY LIMIT HERE
    ship_emigration <- ship_emigration_fn(ships_pop, ships_invasion_prob,
      lifestage_vec = ship_to_port_lifestages, x = t_global,
      ship_emigration_input = ship_emigration)

     # Port immigration
    port_immigration <- port_immigration_fn(ports_pop, ship_to_port_lifestages,
      ship_emigration, ship_position, t1_position_idx,
      port_immigration_input = port_immigration, x = t_global,
      ports_instant_mortality, ports_habitat_suitability)

    port_juvenile_production <- port_juvenile_production_fn(ports_pop,
      port_to_ship_lifestages, port_cyprid_compentency, ship_position,
      t1 = t1_position_idx, x = t_global)

    # Ship immigration
    ship_immigration <- ship_immigration_fn(ships_pop, port_to_ship_lifestages,
      port_cyprid_compentency, ship_position, t1_position_idx,
      ship_immigration_input = ship_immigration, x = t_global)

    # Calculate population change in ports (ports_pop[date, lifestage, port])
    if (t_global > 1) {

    # Population size for lifestages except juveniles
      N_ports <- popgrow(port_pop_transition, ports_pop[(t_global - 1), , ])

      dimnames(N_ports) <-
        list(dimnames(ports_pop)[[2]], dimnames(ports_pop)[[3]])

   # Add in juvenile recruitment to ports separately from matrix population
   # growth

      if (length(port_juvenile_production) > 0 &
        !is.null(port_juvenile_production)) {

      # Check for port name matches. Change ports_pop to N_ports
      ports_pop_juve <-
        N_ports[which(dimnames(N_ports)[[1]] == "juvenile"),
        match(dimnames(N_ports)[[2]],
          dimnames(port_juvenile_production)[[2]])] + port_juvenile_production

        afill(N_ports, local = TRUE) <- ports_pop_juve
      }

      # Apply carrying capacity upper limit
      ports_logit_factor <- (k_ports - N_ports) / k_ports

      # Larva and cyprids don't have a carrying capacity
      ports_logit_factor[which(dimnames(ports_logit_factor)[[1]] %in%
        c("larva", "cyprid")), ] <- 1

      # Prevent 'negative' populations by setting a lower limit of 0.
      temp_ports_pop <- pmax(ceiling(N_ports * ports_logit_factor +
                  port_immigration[(t_global - 1), , ]), 0, na.rm = TRUE)

      # Adjust for error where population persists when only 2 individuals are
      # left
      temp_ports_pop[temp_ports_pop[] <= 2] <- 0

      stopifnot(!is.null(dimnames(temp_ports_pop)[[2]]))

    } else {
      # In the first time slice, so no t-1 time position
      temp_ports_pop <- ports_pop[t_global, , ]
    }

    ports_pop_idx <-  cube_match_fn(t_date_global, ports_pop, temp_ports_pop)

    cube_fill_row(ports_pop, temp_ports_pop, ports_pop_idx)

    ports_trace_pop <-
      apply(temp_ports_pop[, dimnames(temp_ports_pop)[[2]] %nin% seed_ports], 1,
        mean)

    flog.trace("parameter%s port_population %i %f %f %f %f", sprintf("%.03d",
      param_iter), t_global, ports_trace_pop[1], ports_trace_pop[2],
      ports_trace_pop[3], ports_trace_pop[4], name = "ports_pop_trace",
      capture = FALSE)

    ports_trace_n <- apply(temp_ports_pop[, dimnames(temp_ports_pop)[[2]] %nin%
      seed_ports], 1, function(x) sum(x > 0L))

    flog.trace("parameter%s port_number %i %f %f %f %f", sprintf("%.03d",
      param_iter), t_global, ports_trace_n[1], ports_trace_n[2],
      ports_trace_n[3], ports_trace_n[4], name = "ports_n_trace",
      capture = FALSE)

    # Trace for ports_instant_mortality

    ports_instant_mortality_sum <-
      sum(ports_instant_mortality[(t_global - 1), ], na.rm = TRUE)

    flog.trace("parameter%s port_mortality %i %f", sprintf("%.03d", param_iter),
      t_global, ports_instant_mortality_sum,
      name = "ports_instant_mortality_trace", capture = FALSE)

    # Calculate population change on ships
    # (ships_pop_array[date, lifestage, ship])

    if (t_global > 1) {
      # Model population growth
      N_ships <- popgrow(ship_pop_transition, ships_pop[(t_global - 1), , ])

      dimnames(N_ships) <- list(dimnames(ships_pop)[[2]],
        dimnames(ships_pop)[[3]])

      ships_logit_factor <- (k_ships - N_ships) / k_ships

      ships_logit_factor[which(dimnames(ships_logit_factor)[[1]] %in%
        c("larva", "cyprid")), ] <- 1

      temp_ships_pop <- pmax(ceiling(N_ships * ships_logit_factor -
        ship_emigration[(t_global - 1), , ] +
        ship_immigration[(t_global - 1), , ]), 0, na.rm = TRUE)

    } else {
      # In the first time slice, so no t-1 time position
      temp_ships_pop <- ships_pop[t_global, , ]
    }

    ships_pop_idx <-  cube_match_fn(t_date_global, ships_pop, temp_ships_pop)

    cube_fill_row(ships_pop, temp_ships_pop, ships_pop_idx)

    temp_ships_pop[temp_ships_pop == 0] <- NA

    ships_trace_pop <- apply(temp_ships_pop, 1,
      function(x) max(x, na.rm = TRUE))

    flog.trace("parameter%s ships_population %i %f %f %f %f", sprintf("%.03d",
      param_iter), t_global, ships_trace_pop[1], ships_trace_pop[2],
      ships_trace_pop[3], ships_trace_pop[4], name = "ships_pop_trace",
      capture = FALSE)

  # Check to see if ship adult pop size is 0, but ports still get invaded

    if ((ports_trace_pop[["larva"]] > 0) & identical(ships_trace_pop[["adult"]],
       0)) stop("Something went wrong")

    flog.info("Model date: %s Parameter: %i Iteration %i completed",
      t_date_global, param_iter, t_global, name = "model_progress")

  }  # End of time processing loop

  # Create directories to store results
  parameter_dir <- sprintf("parameter%.03d", param_iter)
  boot_dir <- paste0("bootstrap_iter", sprintf("%0.3d", boot_iter), "/")
  results_dir <- paste0(basedir, "results/", boot_dir, parameter_dir)

  alphabet_it <- iterators::iter(letters[1:26])

   # Creates file directories with additional letter to avoid overwriting old
   # files

  if (file.exists(results_dir)) results_dir <- paste0(results_dir,
    nextElem(alphabet_it))
 dir.create(results_dir, recursive = TRUE)

# Save data
param <- list(parameter = sprintf("parameter%.03d", param_iter),
  port_area = port_area,
  k_ports = k_ports,
  fw_reduction = fw_reduction,
  seed_bioregions = list(seed_bioregions),
  max_density_individuals  = max_density_individuals,
  larval_dev_lag = larval_dev_lag,
  mature_lag = mature_lag,
  repro_lag = repro_lag,
  ship_port_prob = ship_port_prob,
  port_compentency_prob = port_compentency_prob
  )

saveRDS(param, file = paste0(results_dir, "/", "Parameters_",
  Sys.Date(), ".RData"))
flog.info("Parameters saved", name = "model_progress.log")
saveRDS(ports_pop, file = paste0(results_dir, "/", "ports_pop_",
  Sys.Date(), ".RData"))
flog.info("ports_pop saved", name = "model_progress.log")
saveRDS(ships_pop, file = paste0(results_dir, "/", "ships_pop_",
  Sys.Date(), ".RData"))
flog.info("ships_pop saved", name = "model_progress.log")
saveRDS(port_cyprid_compentency, file = paste0(results_dir, "/",
  "port_cyprid_compentency_", Sys.Date(), ".RData"))
flog.info("port_emigration saved", name = "model_progress.log")
saveRDS(port_immigration, file = paste0(results_dir, "/", "port_immigration_",
  Sys.Date(), ".RData"))
flog.info("port_immigration saved", name = "model_progress.log")
saveRDS(ship_emigration, file = paste0(results_dir, "/", "ship_emigration_",
  Sys.Date(), ".RData"))
flog.info("ship_emigration saved", name = "model_progress.log")
saveRDS(ship_immigration, file = paste0(results_dir, "/", "ship_immigration_",
  Sys.Date(), ".RData"))
saveRDS(ports_instant_mortality, file = paste0(results_dir, "/",
  "ports_instant_mortality_", Sys.Date(), ".RData"))
flog.info("ports_instant_mortality saved", name = "model_progress.log")

}  # End of main_model_fn statement
