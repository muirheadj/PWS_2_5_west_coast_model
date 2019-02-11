# This program generates arrays for ship position through time, ship invasion
# status through time, and port invasion status through time.

# Author: jmuirhead
###############################################################################

library("reshape2")
library("data.table")
library("dplyr")
library("assertive")
library("Hmisc")
library("stringr")
library("readr")
library("futile.logger")
library("rprojroot")
library("keyring")
library("mice")
library("rhelpers")

root_crit <- has_dirname("PWS_2_5_west_coast_model", subdir = "src")
root_dir <- root_crit$make_fix_file()

options(tibble.width = Inf) # Print all columns

# Set up logging
flog.appender(appender.console(), name = "info.log")
flog.threshold(INFO, name = "info.log")

flog.info("Beginning loading and munging data", name = "info.log")

# Custom "not-in" function
`%nin%` <- Negate(`%in%`)

# load data ## -----------------------------------------------------------------

ship_con <-
  DBI::dbConnect(RMySQL::MySQL(),
    host = "serc-cg01-new.si.edu",
    dbname = "IHS_transits",
    user = "muirheadj",
    password = key_get("ihs_transits_test")
  )

ship_data <-
  tbl(ship_con, sql("SELECT * FROM panama_caribbean_merged")) %>%
  collect(n = Inf)

ihs_port_list <- tbl(ship_con,
   sql("SELECT DISTINCT
    PortStd
FROM
    panama_caribbean_merged
WHERE
    PortStd IS NOT NULL
        AND PortStd != 'Gibraltar'
ORDER BY PortStd"
  )) %>%
  collect()

port_data_full <- tbl(
  ship_con,
  sql(
    "SELECT DISTINCT
    PortStd,
    Port_Country,
    REG_LRGGEO,
    PortLatitudeStd,
    PortLongitudeStd
FROM
    IHS_ports
WHERE
    PortStd != 'Gibraltar'
ORDER BY PortStd"
  )
) %>%
  collect(n = Inf)

port_data <- port_data_full %>%
  filter(PortStd %in% ihs_port_list[['PortStd']])

saveRDS(port_data, file.path(root_dir(), "data", "ports_data.rds"))

try(DBI::dbDisconnect(ship_con), silent = TRUE)

# ship movement ---------------------------------------------------------------
# Ports blacklist to exclude from analysis
ports_blacklist <- c(
  "Campeche Escarpment Drilling Block",
  "Gulfmex Lightering Area No 2",
  "Kattegat Strait",
  "Magellan Strait Area",
  "Ta'Kuntah FSO",
  "Torres Strait & Great Barrier Reef",
  "US Gulf Lightering Zones",
  "Panama Pacific Lightering Area (PANPAC)"
)

ships_blacklist <- c("Military", "Combo")

# Change dates and times to POSIXct format
datetype_grep <-
  grepl("[0-9]{4}[-][0-9]{2}[-][0-9]{2}.*", ship_data[1:5,],
    perl = TRUE)
ship_data[, datetype_grep] <- lapply(ship_data[, datetype_grep],
  function(x)
    as.POSIXct(x, tz = "UTC"))


# Filter out blacklisted ship types, ports, etc.
ship_raw_tbl <-
  tbl_df(ship_data[ship_data$ShipType %nin% ships_blacklist &
      ship_data$NBIC_ShipType %nin% ships_blacklist &
      ship_data$PortStd %nin% ports_blacklist,]) %>%
  arrange(LRNOIMOShipNo, ArrivalDateFull)


# Calculate difference between ArrivalDateStd and SailDateStd for each port
# calling

ship_raw_tbl <- ship_raw_tbl %>%
  select(
    LRNOIMOShipNo,
    NBIC_ShipType,
    Port,
    PortStd,
    Port_Country,
    Movementtype,
    ArrivalDateFullStd,
    SailDateFullStd,
    ArrivalID,
    sameport_id
  ) %>%
  left_join(.,
    port_data,
    by = c("PortStd", "Port_Country"),
    copy = TRUE) %>%
  mutate(port_duration = abs(as.numeric(
    difftime(SailDateFullStd,
      ArrivalDateFullStd, units = "hours")
  ))) %>%
  arrange(LRNOIMOShipNo, ArrivalDateFullStd)

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
    IMO_No AS LRNOIMOShipNo,
    Beam,
    Depth,
    Draft,
    LOA,
    LBP,
    GT,
    Dwt,
    TEU,
    Nrt,
    Speed,
    Main_Vessel_Type,
    Sub_Type
FROM
    IHS_Fairplay"

wsa_df <- tbl(nbic_con, sql(wsa_calc_qry)) %>%
  collect(n = Inf)

try(DBI::dbDisconnect(nbic_con), silent = TRUE)

# Add to existing data
ship_raw_tbl <-
  left_join(ship_raw_tbl, wsa_df, by = "LRNOIMOShipNo")
  

# Calculate wetted surface area depending on ship type
wsa_calc_fn <- function(NBIC_ShipType, Sub_Type, Nrt, LBP, Draft, Beam) {
  wsa <- NA_real_
  if (!is.na(Nrt)) {
    if (NBIC_ShipType == "Bulker")
      wsa <- 26.487 * Nrt ^ 0.606
    if (NBIC_ShipType == "Tanker")
      wsa <- 29.614 * Nrt ^ 0.601
    if (NBIC_ShipType == "Passenger")
      wsa <- 21.956 * Nrt ^ 0.584
    if (NBIC_ShipType == "Container")
      wsa <-  8.073 * Nrt ^ 0.645
    if (NBIC_ShipType == "RoRo")
      wsa <- 39.628 * Nrt ^ 0.54
    if (NBIC_ShipType == "General Cargo")
      wsa <- 26.15 * Nrt ^ 0.587
    if (NBIC_ShipType == "Barge" | Sub_Type == "Oil Barge")
      wsa <- LBP * (2 * Draft + Beam)
    if (NBIC_ShipType == "Other" & Sub_Type != "Oil Barge")
      wsa <- 34.16 * Nrt ^ 0.576
    if (NBIC_ShipType == "Combo")
      wsa <- 34.16 * Nrt ^ 0.576
    if (NBIC_ShipType == "Reefer")
      wsa <- 34.16 * Nrt ^ 0.576
    if (NBIC_ShipType == "Tug")
      wsa <- 34.16 * Nrt ^ 0.576
    if (NBIC_ShipType == "Offshore SupplNrt Vessel")
      wsa <- 34.16 * Nrt ^ 0.576
    if (NBIC_ShipType == "Recreational")
      wsa <- 15

  }
  wsa
}

# FIXME
ship_raw_tbl <-
  ship_raw_tbl %>%
  mutate(wsa = purrr::pmap_dbl(list(NBIC_ShipType, Sub_Type, Nrt, LBP, Draft, Beam), wsa_calc_fn))

# Filter out bad ships than are drive-bys (must spend at least 1/2 hour in ports)
ship_movement_raw_tbl <- dtplyr::tbl_dt(
  ship_raw_tbl %>%
    filter(port_duration > 0.5) %>%
    select(PortStd,
      REG_LRGGEO,
      Port_Country,
      Latitude = PortLatitudeStd,
      Longitude = PortLongitudeStd,
      LRNOIMOShipNo,
      ShipType = NBIC_ShipType,
      Sub_Type,
      ArrivalID,
      ArrivalDateFullStd,
      SailDateFullStd,
      sameport_id,
      wsa,
      GT,
      Nrt,
      Depth,
      Draft,
      LOA,
      Dwt
    )
)

# End of ship movement ## ------------------------------------------------------


# Get unique measures of Gross Tonnage, Net Registered tonnage for each unique
# vessel

ship_imo_temp_tbl <-
  ship_movement_raw_tbl %>%
  group_by(LRNOIMOShipNo) %>%
  select(ShipType, Sub_Type, wsa, GT, Nrt, Depth, Draft, LOA, Dwt) %>%
  unique()
  
ship_imo_temp_tbl <- ship_imo_temp_tbl %>%
  mutate(LRNOIMOShipNo = paste0("IMO", LRNOIMOShipNo))

flog.info("Begin multiple imputation to fill in missing WSA", name = "info.log")

# Use multiple imputation for missing wsa values

imp_sub_tbl <- ship_imo_temp_tbl
imp_sub_tbl$LRNOIMOShipNo <- NULL
imp <- mice(imp_sub_tbl, meth = "rf", m = 1)
imp_df <- mice::complete(imp)

## Use this table below for ship information in the model
ship_imo_tbl <-
  cbind(LRNOIMOShipNo = ship_imo_temp_tbl$LRNOIMOShipNo,
    imp_df,
    stringsAsFactors = FALSE)
    
rm(ship_imo_temp_tbl)

#---End of Wetted Surface Area calculations-------------------------------------

## The datetimes vector contains the complete range of dates and
## time indices even though they may not be present in the data

datetime_df <- tibble(datetime = 
  seq(from = as.POSIXct("2010-01-01 00:00:00", tz = "UTC"),
    to = as.POSIXct("2017-12-31 18:00:00", tz = "UTC") + (6 * 3600),
    by = '6 hours')) %>%
  mutate(datetime_idx = seq_along(datetime))

# Set size of number of rows in each chunk to process for data.frames and 
# arrays
chunk_size <- 100

# Do the same for the indices so that we can use these indices for further
#  extraction
datetimes_idx_chunks <- chunkr(datetime_df[["datetime_idx"]],
  chunk_size = chunk_size)

datetimes_split <- purrr::map(datetimes_idx_chunks, function(x) datetime_df[x, ])

# Added a second to ArrivalDateFullStd so that it would fit cleanly in 
# intervals

ship_raw_tbl <- ship_movement_raw_tbl %>%
  mutate(SailDateFullStd = as.POSIXct(ArrivalDateFullStd, tz = "UTC"),
  ArrivalDateFullStd = as.POSIXct(ArrivalDateFullStd, tz = "UTC") + 1) %>%
  mutate(arr_time_idx = findInterval(ArrivalDateFullStd,
      datetime_df[["datetime"]]),
    sail_time_idx = findInterval(SailDateFullStd,
      datetime_df[["datetime"]])) %>%
  filter(arr_time_idx > 0) %>%
  mutate(arrivaltime_slice = datetime_df[["datetime"]][arr_time_idx],
    sailtime_slice = datetime_df[["datetime"]][sail_time_idx]) %>%
  unique()

# Port invasion status ## ------------------------------------------------------

# Get port names that were visited by each ship, excluding Gibraltar
port_invasion_tbl <- ship_raw_tbl %>%
  filter(!is.na(PortStd), PortStd != "Gibraltar") %>%
  select(PortStd) %>%
  unique()

# Add in additional port information
port_portnames <- port_data %>%
  right_join(port_invasion_tbl,  by = "PortStd") %>%
  arrange(PortStd)

rm(port_invasion_tbl)

# End of port invasion status ## -----------------------------------------------

# Get names of unique vessels and ports
ship_names <- ship_imo_tbl %>%
  purrr::pluck("LRNOIMOShipNo") %>%
  unique() %>%
  sort()

port_names <- port_portnames %>%
  purrr::pluck("PortStd")

# Preallocate ships, and ports arrays ------------------------------------------

 ships_array <- matrix(data = NA_real_, nrow = length(ship_names),
   ncol = nrow(datetime_df), dimnames = list(LRNOIMOShipNo = ship_names,
   time_idx = as.character(datetime_df[["datetime"]])))

# Array to store port invasion status
 ports_array <- matrix(data = NA_real_,  nrow = nrow(datetime_df),
   ncol = length(port_names), dimnames = list(
     time_idx = as.character(datetime_df[["datetime"]]),
     PortStd = sort(port_names)))

# Save ship wetted surface area, ship arrays and port arrays

saveRDS(ship_imo_tbl, file = file.path(root_dir(), "data", "bootstrap_iter001",
  "ship_imo_tbl.rds"), compress = TRUE)

saveRDS(ships_array, file = file.path(root_dir(), "data", "bootstrap_iter001",
  "ships_array.rds"), compress = TRUE)
  
saveRDS(ports_array, file = file.path(root_dir(), "data", "bootstrap_iter001",
  "ports_array.rds"), compress = TRUE)

rm(ship_imo_tbl, ships_array, ports_array)


# Populate ship-port movement array ahead of time
# Gets ship name, position for each time step

# Read in Mark's checklist for multiple ports to remove from data set
port_checklist <- read_tsv(file.path(root_dir(), "data",
  "Multiple_stops_new.tsv"))

port_notuse <- port_checklist %>%
  mutate(arrivaltime_slice = lubridate::force_tz(arrivaltime_slice,
    tzone = "UTC")) %>%
  filter(use == 0)

# Section to population ship position array -----------------------------------
# Loop through datetime chunks

for (k in seq_along(datetimes_split)) {

  datetime_chunks <- datetimes_split[[k]]

  position_array_chunk <- array(
    data = NA,
    dim = c(length(ship_names),
      length(port_names), nrow(datetime_chunks)),
    dimnames = list(
      LRNOIMOShipNo = ship_names,
      PortStd = port_names,
      time_idx = format(datetime_chunks[["datetime"]],
        format = "%Y-%m-%d %H:%M:%S")
      )
    )

  for (tt in seq_along(datetime_chunks[["datetime"]])) {
  # Subset the data.table ship_raw_tbl by time slice, and calculate the duration
  # in port. Use Mark's data on choosing which ports to use.

    ship_movement_sub_tbl <- ship_raw_tbl %>%
      filter(
        arrivaltime_slice <= datetime_chunks[["datetime"]][tt],
        sailtime_slice > datetime_chunks[["datetime"]][tt]) %>%
      mutate(
        LRNOIMOShipNo = paste0("IMO", LRNOIMOShipNo),
        duration = SailDateFullStd - ArrivalDateFullStd
      ) %>%
      select(
        LRNOIMOShipNo,
        PortStd,
        arrivaltime_slice,
        sailtime_slice,
        duration) %>%
      filter(
        LRNOIMOShipNo %nin% port_notuse[["LRNOIMOShipNo"]],
        arrivaltime_slice %nin% port_notuse[["arrivaltime_slice"]],
        PortStd %nin% port_notuse[["PortStd"]]
      ) %>%
      unique()

    # Filtering remaining multiple ports in the same timestep. This step
    # assigns a rank to idx.  If one of the ports is an important hub port, it
    #  gets a 1, otherwise, if it contains the maximum duration, it gets a 2.

    ship_movement_sub_tbl <- ship_movement_sub_tbl %>%
      group_by(LRNOIMOShipNo, arrivaltime_slice) %>%
      mutate(idx = ifelse(
        PortStd %in% c("Panama Canal", "Houston"), 1,
        ifelse(duration == max(duration), 2, NA)
      ))

    # Select only the ports that have the lowest "importance" index
    ship_movement_sub_tbl <- ship_movement_sub_tbl %>%
      group_by(LRNOIMOShipNo, arrivaltime_slice) %>%
      filter(which.min(idx)) %>%
      select(-idx)

    # If a record exists for this combination, match the record to its
    # corresponding position in the array.

    if (nrow(ship_movement_sub_tbl) > 0) {
      ship_match <- match(ship_movement_sub_tbl[['LRNOIMOShipNo']],
        dimnames(position_array_chunk)[[1]])
      port_match <- match(ship_movement_sub_tbl[['PortStd']],
        dimnames(position_array_chunk)[[2]])
      time_match <- match(
        format(datetime_chunks[["datetime"]][[tt]],
          format = "%Y-%m-%d %H:%M:%S"),
        dimnames(position_array_chunk)[[3]]
      )

      pos_match <-
        matrix(cbind(ship_match, port_match, rep(time_match,
          length = length(port_match))), ncol = 3)

      position_array_chunk[pos_match] <- TRUE
    }


	# Check to make sure the ship can't be in 2 different ports in the same
	# time chunk. If so, randomly pick one of them
  warn <-
    assert_all_are_in_closed_range(rowSums(position_array_chunk[, , tt],
      na.rm = TRUE), 0, 1, severity = "warning")
   
   if(any(warn > 1)) {
     # Find out which ports the ships are in:
     duplicate_arrivals <- which(warn > 1, arr.ind = TRUE)
   
     for (ii in length(duplicate_arrivals)) {
       duplicate_ports <- which(!is.na(position_array_chunk[duplicate_arrivals[ii], , tt]))
   	   position_array_chunk[duplicate_arrivals[ii], , tt] <- NA
   	   picked_port <- sample(duplicate_ports, size = 1)
   	   position_array_chunk[duplicate_arrivals[ii], picked_port, tt] <- TRUE
   	 }
  }
}

# Write to external object

saveRDS(position_array_chunk, file = file.path(root_dir(), "data",
  "bootstrap_iter001",
  sprintf("position_array_chunk%0.3d.rds", k)), compress = TRUE)

flog.info("Iteration: %i out of %i", k, length(datetimes_split),
  name = "info.log")  

}

flog.info("Finished loading and munging data", name = "info.log")
