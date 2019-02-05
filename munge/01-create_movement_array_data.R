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


root_crit <- has_dirname("PWS_2_5_west_coast_model", subdir = "src")
root_dir <- root_crit$make_fix_file()

options(tibble.width = Inf) # Print all columns

# Set up logging
flog.appender(appender.console(), name = "info.log")
flog.threshold(INFO, name = "info.log")

flog.info("Beginning loading and munging data", name = "info.log")

ptm <- proc.time()  # Start program time

## Custom "not-in" function
'%nin%' <- Negate('%in%')

## Function to convert proc.time into a more readable format.
readable_time <- function(x) {
  x <- as.numeric(eval(x))
  stopifnot(is.finite(x), !is.na(x))
  days <- floor(abs(x) / 86400)
  hrs <- floor((abs(x) - days * 86400) / 3600)
  mins <- floor((abs(x) - days * 86400 - hrs * 3600) / 60)
  secs <- (abs(x) - days * 86400 - hrs * 3600 - mins * 60)
  t <- as.character(
    paste0(
      ifelse(sign(x) < 0, "-", ""),
      sprintf("%d", days),
      " days, ",
      sprintf("%d", hrs),
      " hours, ",
      sprintf("%d", mins),
      " minutes, ",
      sprintf("%4.2f", secs),
      " seconds"
    )
  )
  t
}


## load data ## --------------------------------------------------------------

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


DBI::dbDisconnect(ship_con)

## ship movement ## -----------------------------------------------------------
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

ships_blacklist <- c("Military", "Combo", "Other")

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

## ----WettedSurfaceArea_Calculations--------------------------------------
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
    GT,
    Dwt,
    TEU,
    Nrt,
    Speed,
    Main_Vessel_Type
FROM
    IHS_Fairplay"

wsa_df <- tbl(nbic_con, sql(wsa_calc_qry)) %>%
  collect(n = Inf)

DBI::dbDisconnect(nbic_con)

# Add to existing data
ship_raw_tbl <-
  left_join(ship_raw_tbl, wsa_df, by = "LRNOIMOShipNo") %>%
  rename(ShipType = NBIC_ShipType)

# Calculate wetted surface area depending on ship type
wsa_calc_fn <- function(x, y) {
  z <- NA_real_
  if (!is.na(y)) {
    if (x == "Bulker")
      z <- 26.487 * y ^ 0.606
    if (x == "Tanker")
      z <- 29.614 * y ^ 0.601
    if (x == "Passenger")
      z <- 21.956 * y ^ 0.584
    if (x == "Container")
      z <-  8.073 * y ^ 0.645
    if (x == "RoRo")
      z <- 39.628 * y ^ 0.54
    if (x == "General Cargo")
      z <- 26.15 * y ^ 0.587
    if (x == "Other")
      z <- 34.16 * y ^ 0.576
    if (x == "Combo")
      z <- 34.16 * y ^ 0.576
    if (x == "Reefer")
      z <- 34.16 * y ^ 0.576
    if (x == "Tug")
      z <- 34.16 * y ^ 0.576
    if (x == "Offshore Supply Vessel")
      z <- 34.16 * y ^ 0.576
    if (x == "Recreational")
      z <- 15
  }
  z
}


ship_raw_tbl <-
  ship_raw_tbl %>%
  mutate(wsa = unlist(mapply(wsa_calc_fn, .$ShipType, .$Nrt)))

# Filter out bad ships than are drive-bys (must spend at least 1/2 hour in ports)
ship_movement_raw_tbl <- dtplyr::tbl_dt(
  ship_raw_tbl %>%
    filter(., port_duration > 0.5) %>%
    select(
      .,
      PortStd,
      REG_LRGGEO,
      Port_Country,
      Latitude = PortLatitudeStd,
      Longitude = PortLongitudeStd,
      LRNOIMOShipNo,
      ShipType,
      ArrivalID,
      ArrivalDateFullStd,
      SailDateFullStd,
      sameport_id,
      wsa,
      GT,
      Nrt
    )
)

## End of ship movement ## ----------------------------------------------------


# Get unique measures of Gross Tonnage, Net Registered tonnage for each unique
# vessel

ship_imo_temp_tbl <-
  ship_movement_raw_tbl %>%
  group_by(LRNOIMOShipNo) %>%
  select(ShipType, wsa, GT, Nrt) %>%
  unique()
  
ship_imo_temp_tbl <- ship_imo_temp_tbl %>%
  mutate(., LRNOIMOShipNo = paste0("IMO", LRNOIMOShipNo))

flog.info("Begin multiple imputation to fill in missing WSA", name = "info.log")

# Use multiple imputation for missing wsa values
library(mice)
imp_sub_tbl <- ship_imo_temp_tbl
imp_sub_tbl$LRNOIMOShipNo <- NULL
imp <- mice(imp_sub_tbl, meth = "rf")
imp_df <- complete(imp, 1)

## Use this table below for ship information in the model
ship_imo_tbl <-
  cbind(LRNOIMOShipNo = ship_imo_temp_tbl$LRNOIMOShipNo,
    imp_df,
    stringsAsFactors = FALSE)
    
rm(ship_imo_temp_tbl)

##---End of Wetted Surface Area calculations-----------------------------------

## Get range of time indices for the data
date_min <- as.POSIXct("2010-01-01 00:00:00", tz = "UTC")

# Add a bit of a buffer in order to make sure the upper end gets captured
date_max <- as.POSIXct("2018-12-31 18:00:00", tz = "UTC")

# Create a sequence of times
time_seq <-
  seq(from = date_min,
    to = date_max + (6 * 3600),
    by = '6 hours')

# Create an time section index based on arrival date

ship_raw_tbl <- ship_movement_raw_tbl %>%
  mutate(ArrivalDateFullStd = ArrivalDateFullStd + 1) %>%
  mutate(
    arr_time_idx = findInterval(ArrivalDateFullStd, time_seq),
    sail_time_idx = findInterval(SailDateFullStd, time_seq)
  ) %>%
  filter(arr_time_idx > 0) %>%
  mutate(arrivaltime_slice = time_seq[arr_time_idx],
    sailtime_slice = time_seq[sail_time_idx]) %>%
  unique()


## The model_daterange vector contains the complete range of dates and
## time indices even though they may not be present in the data

model_daterange <- tibble(time_slice = seq(min(ship_raw_tbl[['arrivaltime_slice']]),
  max(ship_raw_tbl[['arrivaltime_slice']]), by = '6 hours'))

model_daterange[['time_idx']] <-
  seq_along(model_daterange[['time_slice']])

## ship invasion status ## ----------------------------------------------------

# select ships by unique IMO number (4120 ships, repeat 717 times) 716?
ship_shipnames <-
  data.frame(
    LRNOIMOShipNo = ship_imo_tbl[['LRNOIMOShipNo']],
    ShipType = ship_imo_tbl[['ShipType']],
    stringsAsFactors = FALSE
  )

ship_datetime <-
  data.frame(
    time_slice = model_daterange[['time_slice']],
    time_idx = model_daterange[['time_idx']],
    stringsAsFactors = FALSE
  )

# Create a data.frame with every combination of ship and time
ships_df <- reshape::expand.grid.df(ship_shipnames, ship_datetime) %>%
  tbl_df()

ships_df <- ships_df %>%
  mutate(LRNOIMOShipNo = as.character(LRNOIMOShipNo),
    ShipType = as.character(ShipType)) %>%
  arrange(LRNOIMOShipNo, time_idx)

## End of ship invasion status ## ---------------------------------------------

## Port invasion status ## ----------------------------------------------------

port_invasion_tbl <- ship_raw_tbl %>%
  filter(!is.na(PortStd), PortStd != "Gibraltar") %>%
  select(PortStd) %>%
  unique()

port_portnames <- port_data %>%
  right_join(port_invasion_tbl,  by = "PortStd") %>%
  arrange(PortStd)

ports_df <- reshape::expand.grid.df(port_portnames, ship_datetime)
ports_df <- tbl_df(ports_df) %>%
  arrange(PortStd, time_slice)

## End of port invasion status ## ---------------------------------------------

## Main program loop ## -------------------------------------------------------

# Get number of unique vessels
ship_list_full <- unique(ships_df[['LRNOIMOShipNo']])

o <- order(ship_list_full)
ship_list <- ship_list_full[o]
port_list <- port_portnames[["PortStd"]]

unique_port_list <- unique(select(ports_df, -time_slice, -time_idx))

datetime_list <- unique(ports_df[ports_df$time_idx %in%
    1:max(ports_df$time_idx), 'time_slice'])

datetime_list <- datetime_list[['time_slice']]

flog.info("Finished loading and munging data", name = "info.log")

# Generate table for ship and port invasion status by time

position_array <- array(
  data = NA,
  dim = c(length(ship_list),
    length(port_list), length(datetime_list)),
  dimnames = list(
    LRNOIMOShipNo = ship_list,
    PortStd = port_list,
    time_idx = format(datetime_list,
      format = "%Y-%m-%d %H:%M:%S")
  )
)

# Populate ship-port movement array ahead of time
# Gets ship name, position for each time step

# Read in Mark's checklist for multiple ports to remove from data set
locale(tz = "UTC")
port_checklist <- read_tsv("data/Multiple_stops_new.tsv")

port_notuse <- port_checklist %>%
  mutate(arrivaltime_slice = lubridate::force_tz(arrivaltime_slice,
    tzone = "UTC")) %>%
  filter(use == 0)

# Function to troubleshoot ships that occupy more than 1 port in each time step
ship_check_fn <- function(x, tt, save_output = FALSE) {
  suspect_array <-
    position_array[x, , tt, drop = FALSE] # Subsets position array
  suspect_ship <-
    dimnames(suspect_array)[[1]] # Extracts the ship IMO number
  suspect_ports <-
    names(na.omit(drop(suspect_array))) # Extracts the ports
  suspect_time <-
    dimnames(suspect_array)[[3]] # Extracts the time slice

  if (save_output == TRUE) {
    # Save into a file if requested
    ship_extract <-
      ship_raw_tbl[LRNOIMOShipNo == str_extract(suspect_ship,
        "[0-9]+"),]
    write.table(
      ship_extract,
      file = "~/Desktop/ship_suspects.txt",
      sep = "\t",
      row.names = FALSE,
      col.names = TRUE
    )
  }
  list(
    suspect_ship = suspect_ship,
    suspect_ports = suspect_ports,
    suspect_time = suspect_time
  )
}

for (tt in seq_along(datetime_list)) {
  # Subset the data.table ship_raw_tbl by time slice, and calculate the duration
  # in port. Use Mark's data on choosing which ports to use.

  ship_movement_sub_tbl <- ship_raw_tbl %>%
    filter(model_daterange[['time_slice']][tt] >= arrivaltime_slice,
      model_daterange[['time_slice']][tt] < sailtime_slice) %>%
    mutate(
      LRNOIMOShipNo = paste0("IMO", LRNOIMOShipNo),
      duration = SailDateFullStd - ArrivalDateFullStd
    ) %>%
    select(LRNOIMOShipNo,
      PortStd,
      arrivaltime_slice,
      sailtime_slice,
      duration) %>%
    filter(
      LRNOIMOShipNo %nin% port_notuse[['LRNOIMOShipNo']],
      arrivaltime_slice %nin% port_notuse[['arrivaltime_slice']],
      PortStd %nin% port_notuse[['PortStd']]
    ) %>%
    unique()

  # Filtering remaining multiple ports in the same timestep. This step assigns a
  # rank to idx.  If one of the ports is an important hub port, it gets a 1,
  # otherwise, if it contains the maximum duration, it gets a 2.

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
      dimnames(position_array)[[1]])
    port_match <- match(ship_movement_sub_tbl[['PortStd']],
      dimnames(position_array)[[2]])
    time_match <- match(
      format(datetime_list[[tt]],
        format = "%Y-%m-%d %H:%M:%S"),
      dimnames(position_array)[[3]]
    )

    pos_match <-
      matrix(cbind(ship_match, port_match, rep(time_match,
        length = length(port_match))), ncol = 3)

    position_array[pos_match] <- TRUE
  }

  warn <-
    assert_all_are_in_closed_range(rowSums(position_array[, , tt],
      na.rm = TRUE), 0, 1)

  flog.info("Iteration: %i out of %i", tt, length(datetime_list),
    name = "info.log")

} # End of tt loop

flog.info("Saving position array to files", name = "info.log")

save.image(file.path(root_dir(), "data", "Positionarray2.RData"))
saveRDS(position_array,
  file.path(root_dir(), "data", "Positionarray.rds"))

etime <- proc.time() - ptm
flog.info("Elapsed time: %s", readable_time(etime[3]))
