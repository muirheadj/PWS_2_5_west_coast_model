# This program generates arrays for ship position through time, ship invasion
# status through time, and port invasion status through time.

# Author: jmuirhead
###############################################################################

library("reshape2")
library("data.table")
library("dplyr")
library("assertive")
library("stringr")
library("readr")
library("futile.logger")
library("rprojroot")
library("keyring")
library("mice")
library("yaml")

root_crit <- has_dirname("PWS_2_5_west_coast_model", subdir = "src")
root_dir <- root_crit$make_fix_file()

# Read in config file for the different scenarios
yaml_params <- yaml::read_yaml(file.path(root_dir(), "params.yaml"))
scenario <- "ship_movements"

# Make directories for each choice
create_missing_directories <- function(x, root) {
  if (!dir.exists(file.path(root, "data", x))) {
    dir.create(file.path(root, "data", x), showWarnings = TRUE)
  }
}

lapply(
  yaml_params[["params"]][["choices"]],
  function(x) create_missing_directories(x, root_dir())
)

options(tibble.width = Inf) # Print all columns


# Custom functions
table <- function(x) base::table(x, useNA = "always")

chunkr <-
  function(vec,
           chunk_size = NULL,
           n_chunks = NULL,
           use_bit_package = FALSE) {
    # Check if bit package is installed
    if (!requireNamespace("bit", quietly = TRUE)) {
      stop("Package bit needed for this function to work. Please install it.",
           call. = FALSE)
    }

    if (is.null(chunk_size) & is.null(n_chunks)) {
      stop("You must provide either the size of the chunks, or number of desired chunks")
    }
    if (is.null(chunk_size))
      chunk <- split(vec, cut(seq_along(vec), n_chunks, labels = FALSE))
    if (is.null(n_chunks))
      chunk <- split(vec, ceiling(seq_along(vec) / chunk_size))
    if (use_bit_package == TRUE)
      chunk <- bit::chunk(
        from = 1,
        to = length(vec),
        by = chunk_size,
        length.out = n_chunks
      )
    chunk
  }

# Set up logging
flog.appender(appender.console(), name = "info.log")
flog.threshold(INFO, name = "info.log")

flog.info("Beginning loading and munging data", name = "info.log")

# Custom "not-in" function
`%nin%` <- Negate(`%in%`)

# load data ## -----------------------------------------------------------------
# Get shipping data from Mark

ports_con <-
  DBI::dbConnect(RMySQL::MySQL(),
    host = "serc-cg01-new.si.edu",
    dbname = "NBIC_Analysis",
    user = "nbicuser",
    password = key_get("nbic_analysis_test", username = "nbicuser")
  )

# Read in from CSV file
arrivals_qry <- "SELECT
    NVMCws_Arrivals.Analysis_year_arrival,
    NVMCws_Arrivals.NVMC_ID,
    NVMCws_Arrivals.Arrival_Port,
    NVMCws_Arrivals.Arrival_Lat,
    NVMCws_Arrivals.Arrival_Lon,
    NVMCws_Arrivals.Arrival_Coast,
    NVMCws_Arrivals.Arrival_Bioregion,
    NVMCws_Arrivals.Arrival_Date,
    NVMCws_Arrivals.Departure_Date,
    NVMCws_Arrivals.Transit_Type,
    NVMCws_Arrivals.NBIC_Vessel AS imo_no,
    NVMCws_Arrivals.Type AS nbic_shiptype,
    NVMCws_Arrivals.Sub_Type
FROM
    NVMCws_Arrivals
WHERE
    (((NVMCws_Arrivals.Analysis_year_arrival) > 2009
        AND (NVMCws_Arrivals.Analysis_year_arrival) < 2018)
        AND ((NVMCws_Arrivals.Status) = 'reviewed')
        AND ((NVMCws_Arrivals.Sub_Type) NOT Like 'Recreational'
          AND (NVMCws_Arrivals.Sub_Type) NOT Like 'Unknown'
          AND (NVMCws_Arrivals.Sub_Type) NOT Like 'Military'
          AND (NVMCws_Arrivals.Sub_Type) NOT Like 'Barge%')
        AND ((NVMCws_Arrivals.Arrival_Coast) = 'west'
          OR (NVMCws_Arrivals.Arrival_Coast) = 'alaska'
          OR (NVMCws_Arrivals.Arrival_Coast) = 'ca-west'
          OR NVMCws_Arrivals.Arrival_Bioregion = 'NA-S1')
        AND ((NVMCws_Arrivals.Transit_Type) LIKE 'c%'
          OR (NVMCws_Arrivals.Transit_Type) LIKE 'o%')
        AND ((NVMCws_Arrivals.NBIC_Vessel) IS NOT NULL)
        AND NVMCws_Arrivals.Arrival_LAT IS NOT NULL)"

arrivals_full <- tbl(ports_con, sql(arrivals_qry)) %>%
  collect(n = Inf)

names(arrivals_full) <- tolower(names(arrivals_full))

# Just use the columns on arrivals data. Last port information is required
# if the last port was on the West Coast, including Canada or Alaska.

port_data <- arrivals_full %>%
  filter(arrival_port != "Beaufort Sea") %>%
  select(
    port =arrival_port, lon = arrival_lon, lat = arrival_lat,
    bioregion = arrival_bioregion, coast = arrival_coast
  ) %>%
  unique() %>%
  arrange(port)

# Port data includes last ports that are not on the West Coast or Alaska,
# but are filtered out in the next step



# Read in raster data, and create a new column for every species if
# the value is above the threshold for habitat suitability

ports_destfile <- file.path(root_dir(), "data", "port_data.csv")

if (!file.exists(ports_destfile)) {
  readr::write_csv(port_data, file.path(root_dir(), "data", "port_data.csv"))
}

if (file.exists(ports_destfile)) {
  port_data <- readr::read_csv(file.path(root_dir(), "data", "port_data.csv"))
}
saveRDS(port_data, file.path(root_dir(), "data", "port_data.rds"), version = 2)


# ship movement ---------------------------------------------------------------
# Change dates and times to POSIXct format

datetype_grep <-
  grepl("[0-9]{4}[-][0-9]{2}[-][0-9]{2}.*", arrivals_full[1:9, ],
    perl = TRUE
  )

arrivals_full[, datetype_grep] <- lapply(
  arrivals_full[, datetype_grep],
  function(x) as.POSIXct(x, tz = "UTC"))

# Filter out blacklisted ship types, ports, etc.
ship_raw_tbl <- arrivals_full %>%
  arrange(imo_no, arrival_date)

# Fix ship types for nbic_type = "Unknown" and sub_type = "Tug"
ship_raw_tbl <- ship_raw_tbl %>%
  mutate(nbic_shiptype = case_when(
      nbic_shiptype == "Other" & sub_type == "Tug" ~ "Tug",
      nbic_shiptype == "Unknown" & sub_type == "Tug" ~ "Tug",
      nbic_shiptype == "Unknown" & sub_type == "Passenger" ~ "Passenger",
      nbic_shiptype == "Unknown" & sub_type == "Bulker" ~ "Bulker",
      nbic_shiptype == "Unknown" & sub_type == "TugOilBarge" ~ "Tug",
      nbic_shiptype == "Unknown" & sub_type == "TugBarge" ~ "Tug",
      nbic_shiptype == "Unknown" & sub_type == "TugTankBarge" ~ "Tug",
      nbic_shiptype == "Unknown" & sub_type == "Other" ~ "Other",
      nbic_shiptype == "Unknown" & sub_type == "General Cargo" ~ "General Cargo",
      nbic_shiptype == "Unknown" & sub_type == "Oil Spill Recovery" ~ "Other",
      nbic_shiptype == "Unknown" & sub_type == "OSRV" ~ "Other",
      nbic_shiptype == "Unknown" & sub_type == "OSV" ~ "Offshore Supply Vessel",
      TRUE ~ nbic_shiptype))


# Calculate difference between ArrivalDateStd and SailDateStd for each port
# calling

ship_raw_tbl <- ship_raw_tbl %>%
  mutate(port_duration = as.numeric(difftime(departure_date, arrival_date,
      units = "hours"))) %>%
  arrange(imo_no, arrival_date)

# ----WettedSurfaceArea_Calculations--------------------------------------------
# Calculate wetted surface area from IHS_Fairplay table

nbic_con <-
  DBI::dbConnect(RMySQL::MySQL(),
    host = "serc4.si.edu",
    dbname = "NBIC_Analysis",
    user = "nbicuser",
    password = key_get("nbic_analysis_test")
  )

wsa_calc_qry <- "SELECT
    IMO_No,
    Beam,
    Depth,
    Draft,
    LOA,
    LBP ,
    GT,
    Dwt,
    TEU,
    Nrt,
    Main_Vessel_Type,
    Sub_Type
FROM
    IHS_Fairplay"

wsa_df <- tbl(nbic_con, sql(wsa_calc_qry)) %>%
  collect(n = Inf)

names(wsa_df) <- stringr::str_to_lower(names(wsa_df))

try(DBI::dbDisconnect(nbic_con), silent = TRUE)

# Add to existing data
ship_raw_tbl <-
  left_join(ship_raw_tbl, wsa_df, by = c("imo_no", "sub_type"))

# Check for missing subtypes table(ship_raw_tbl$Sub_Type, useNA = "always")

# Calculate wetted surface area depending on ship type
wsa_calc_fn <- function(nbic_shiptype, sub_type, nrt, lbp, draft, beam) {
  wsa <- NA_real_
  if (!is.na(nrt)) {
    if (nbic_shiptype == "Bulker") {
      wsa <- 26.487 * nrt^0.606
    }
    if (nbic_shiptype == "Tanker") {
      wsa <- 29.614 * nrt^0.601
    }
    if (nbic_shiptype == "Passenger") {
      wsa <- 21.956 * nrt^0.584
    }
    if (nbic_shiptype == "Container") {
      wsa <- 8.073 * nrt^0.645
    }
    if (nbic_shiptype == "RoRo") {
      wsa <- 39.628 * nrt^0.54
    }
    if (nbic_shiptype == "General Cargo") {
      wsa <- 26.15 * nrt^0.587
    }
    if (nbic_shiptype == "Barge" | sub_type == "Oil Barge") {
      wsa <- lbp * (2 * draft + beam)
    }
    if (nbic_shiptype == "Other" & sub_type != "Oil Barge") {
      wsa <- 34.16 * nrt^0.576
    }
    if (nbic_shiptype == "Combo") {
      wsa <- 34.16 * nrt^0.576
    }
    if (nbic_shiptype == "Reefer") {
      wsa <- 34.16 * nrt^0.576
    }
    if (nbic_shiptype == "Tug") {
      wsa <- 4000 + 34.16 * nrt^0.576
    }
    if (nbic_shiptype == "Other" & sub_type == "Tug") {
      wsa <- 4000 + 34.16 * nrt^0.576
    }
    if (nbic_shiptype == "Offshore Supply Vessel") {
      wsa <- 34.16 * nrt^0.576
    }
  }
  wsa
}

ship_raw_tbl <-
  ship_raw_tbl %>%
  mutate(wsa = purrr::pmap_dbl(list(
    nbic_shiptype, sub_type, nrt, lbp, draft,
    beam
  ), wsa_calc_fn))

# Filter out bad ships than are drive-bys (must spend at least 1/2 hour in ports)
ship_movement_raw_tbl <- ship_raw_tbl %>%
  select(
    port = arrival_port,
    reg_lrggeo = arrival_bioregion,
    latitude = arrival_lat,
    longitude = arrival_lon,
    lrnoimoshipno = imo_no,
    nbic_shiptype,
    sub_type,
    arrival_date,
    departure_date,
    wsa,
    gt,
    nrt,
    depth,
    draft,
    loa,
    dwt
  )

# End of ship movement ## ------------------------------------------------------


# Get unique measures of Gross Tonnage, Net Registered tonnage for each unique
# vessel

ship_imo_temp_tbl <-
  ship_movement_raw_tbl %>%
  group_by(lrnoimoshipno) %>%
  select(
    lrnoimoshipno, nbic_shiptype, sub_type, wsa, gt, nrt, depth, draft,
    loa, dwt
  ) %>%
  unique()

ship_imo_temp_tbl <- ship_imo_temp_tbl %>%
  ungroup() %>%
  mutate(lrnoimoshipno = paste0("IMO", as.character(lrnoimoshipno)))

#nrow(ship_imo_temp_tbl)
#length(ship_imo_temp_tbl$lrnoimoshipno)
#length(unique(ship_imo_temp_tbl$lrnoimoshipno))


flog.info("Begin multiple imputation to fill in missing WSA", name = "info.log")

# Use multiple imputation for missing wsa values

imp_sub_tbl <- ship_imo_temp_tbl
imp_sub_tbl$lrnoimoshipno <- NULL
imp <- mice(imp_sub_tbl, meth = "rf", m = 1, maxit = 10)
imp_df <- mice::complete(imp)

## Use this table below for ship information in the model
ship_imo_tbl <-
  cbind(
    lrnoimoshipno = ship_imo_temp_tbl$lrnoimoshipno,
    imp_df,
    stringsAsFactors = FALSE
  ) %>%
  select(lrnoimoshipno, wsa)

rm(ship_imo_temp_tbl)



# Join arrivals table with imputed wsa values

ship_movement_raw_tbl <- ship_movement_raw_tbl %>%
  select(-wsa, -gt, -nrt, -depth, -draft, -loa, -dwt) %>%
  mutate(lrnoimoshipno = paste0("IMO", lrnoimoshipno)) %>%
  left_join(ship_imo_tbl, by = "lrnoimoshipno", copy = FALSE)

#---End of Wetted Surface Area calculations-------------------------------------

## The datetimes vector contains the complete range of dates and
## time indices even though they may not be present in the data

datetime_df <- tibble(
  datetime =
    seq(
      from = as.POSIXct(yaml_params[["params"]][["start_date"]], tz = "UTC"),
      to = as.POSIXct(yaml_params[["params"]][["end_date"]], tz = "UTC"),
      by = "6 hours"
    )
) %>%
  mutate(datetime_idx = seq_along(datetime))

# Set size of number of rows in each chunk to process for data.frames and
# arrays
chunk_size <- yaml_params[["params"]][["chunk_size"]]

# Do the same for the indices so that we can use these indices for further
#  extraction
datetimes_idx_chunks <- chunkr(datetime_df[["datetime_idx"]],
  chunk_size = chunk_size)

datetimes_split <- purrr::map(datetimes_idx_chunks, function(x) datetime_df[x, ])

# Added a second to ArrivalDateFullStd so that it would fit cleanly in
# intervals

ship_raw_tbl <- ship_movement_raw_tbl %>%
  mutate(
    departure_date = as.POSIXct(departure_date, tz = "UTC"),
    arrival_date = as.POSIXct(arrival_date, tz = "UTC") + 1
  ) %>%
  mutate(
    arr_time_idx = findInterval(
      arrival_date,
      datetime_df[["datetime"]]
    ),
    sail_time_idx = findInterval(
      departure_date,
      datetime_df[["datetime"]]
    )
  )

ship_raw_tbl <- ship_raw_tbl %>%
  filter(arr_time_idx > 0, sail_time_idx > 0) %>%
  mutate(
    arrivaltime_slice = datetime_df[["datetime"]][arr_time_idx],
    sailtime_slice = datetime_df[["datetime"]][sail_time_idx]
  ) %>%
  unique()

# Get unique port and ship names to define array dimensions ## -----------------
port_names <- port_data %>%
  purrr::pluck("port") %>%
  unique()

ship_names <- ship_raw_tbl %>%
  purrr::pluck("lrnoimoshipno") %>%
  unique() %>%
  sort()
  
# Filter out ships from the ship_imo_tbl that are not in the ship_raw_tbl

ship_imo_tbl <- ship_imo_tbl %>%
 filter(lrnoimoshipno %in% ship_names) 


# Preallocate ships, and ports arrays ------------------------------------------

ships_array <- matrix(
  data = NA_integer_, nrow = length(ship_names),
  ncol = nrow(datetime_df), dimnames = list(
    lrnoimoshipno = ship_names,
    time_idx = as.character(datetime_df[["datetime"]])
  )
)

# Array to store port invasion status
ports_array <- matrix(
  data = NA_integer_, nrow = nrow(datetime_df),
  ncol = length(port_names), dimnames = list(
    time_idx = as.character(datetime_df[["datetime"]]),
    port = sort(port_names)
  )
)

# Save ship wetted surface area, ship arrays and port arrays

saveRDS(ship_imo_tbl, file = file.path(
  root_dir(), "data", scenario,
  "ship_imo_tbl.rds"
), compress = TRUE, version = 2)

saveRDS(ships_array, file = file.path(
  root_dir(), "data", scenario,
  "ships_array.rds"
), compress = TRUE, version = 2)

saveRDS(ports_array, file = file.path(
  root_dir(), "data", scenario,
  "ports_array.rds"
), compress = TRUE, version = 2)

rm(ship_imo_tbl, ships_array, ports_array)


# Section to population ship position array -----------------------------------
# Loop through datetime chunks

for (k in seq_along(datetimes_split)) {
  datetime_chunks <- datetimes_split[[k]]

  position_array_chunk <- array(
    data = NA,
    dim = c(
      length(ship_names),
      length(port_names), nrow(datetime_chunks)
    ),
    dimnames = list(
      lrnoimoshipno = ship_names,
      port = port_names,
      time_idx = format(datetime_chunks[["datetime"]],
        format = "%Y-%m-%d %H:%M:%S"
      )
    )
  )

  for (tt in seq_along(datetime_chunks[["datetime"]])) {
    # Subset the data.table ship_raw_tbl by time slice, and calculate the
    # duration in port. Use Mark's data on choosing which ports to use.

    ship_movement_sub_tbl <- ship_raw_tbl %>%
      filter(
        arrivaltime_slice <= datetime_chunks[["datetime"]][tt],
        sailtime_slice > datetime_chunks[["datetime"]][tt]
      ) %>%
      mutate(duration = departure_date - arrival_date) %>%
      select(
        lrnoimoshipno,
        port,
        arrivaltime_slice,
        sailtime_slice,
        duration
      ) %>%
      unique()

    # Filtering remaining multiple ports in the same timestep. This step
    # assigns a rank to idx.  If one of the ports is an important hub port, it
    #  gets a 1, otherwise, if it contains the maximum duration, it gets a 2.

    ship_movement_sub_tbl <- ship_movement_sub_tbl %>%
      group_by(lrnoimoshipno, arrivaltime_slice) %>%
      mutate(idx = ifelse(duration == max(duration), 2, NA))

    # Select only the ports that have the lowest "importance" index
    ship_movement_sub_tbl <- ship_movement_sub_tbl %>%
      group_by(lrnoimoshipno, arrivaltime_slice) %>%
      dplyr::filter(idx == min(idx)) %>%
      select(-idx)

    # If a record exists for this combination, match the record to its
    # corresponding position in the array.

    if (nrow(ship_movement_sub_tbl) > 0) {
      ship_match <- match(
        ship_movement_sub_tbl[["lrnoimoshipno"]],
        dimnames(position_array_chunk)[[1]]
      )
      port_match <- match(
        ship_movement_sub_tbl[["port"]],
        dimnames(position_array_chunk)[[2]]
      )
      time_match <- match(
        format(datetime_chunks[["datetime"]][[tt]],
          format = "%Y-%m-%d %H:%M:%S"
        ),
        dimnames(position_array_chunk)[[3]]
      )

      pos_match <-
        matrix(cbind(ship_match, port_match, rep(time_match,
          length = length(port_match)
        )), ncol = 3)

      position_array_chunk[pos_match] <- TRUE
    }


    # Check to make sure the ship can't be in 2 different ports in the same
    # time chunk. If so, randomly pick one of them
    warn <-
      assert_all_are_in_closed_range(rowSums(position_array_chunk[, , tt],
        na.rm = TRUE
      ), 0, 1, severity = "warning")

    if (any(warn > 1)) {
      # Find out which ports the ships are in:
      duplicate_arrivals <- which(warn > 1, arr.ind = TRUE)

      for (ii in length(duplicate_arrivals)) {
        duplicate_ports <- which(!is.na(
        	position_array_chunk[duplicate_arrivals[ii], , tt]))
        position_array_chunk[duplicate_arrivals[ii], , tt] <- NA
        picked_port <- sample(duplicate_ports, size = 1)
        position_array_chunk[duplicate_arrivals[ii], picked_port, tt] <- TRUE
      }
    }
  }

  # Write to external object

  saveRDS(position_array_chunk, file = file.path(
    root_dir(), "data",
    scenario,
    sprintf("position_array_chunk%0.3d.rds", k)
  ), compress = TRUE, version = 2)

  flog.info("Iteration: %i out of %i", k, length(datetimes_split),
    name = "info.log"
  )
}

flog.info("Finished loading and munging data", name = "info.log")
