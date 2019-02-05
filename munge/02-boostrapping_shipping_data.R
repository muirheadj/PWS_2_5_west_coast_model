# This script takes the original 180 day/4000 ship position movement set
# and resamples it with and increase of 4.588974% per year based on mean
# increase between 2000 and 2009.
#
# Author: jmuirhead
###############################################################################

library("R.utils")
library("stringr")
library("Hmisc")
library("parallel")
library("futile.logger")
library("rprojroot")

root_crit <- has_dirname("PWS_2_5_west_coast_model", subdir = "src")
root_dir <- root_crit$make_fix_file()

flog.appender(appender.console(), name = "info.log")
flog.threshold(INFO, name = "info.log")

flog.info("Beginning loading and munging data", name = "info.log")

stime <- proc.time() # Record the starting time

# Chunking function
chunkr <- function(vec, chunk_size = NULL, n_chunks = NULL, use_bit_package = FALSE){
  if(is.null(chunk_size) && is.null(n_chunks)){

    stop("You must provide either the size of the chunks, or number of desired chunks")
  }
  if(is.null(chunk_size)){
    if(n_chunks == 1){
      chunk <- vec
    } else {
      chunk <- split(vec, cut(seq_along(vec), n_chunks, labels = FALSE))
    }
  }
  if(is.null(n_chunks)) chunk <- split(vec, ceiling(seq_along(vec)/chunk_size))
  if(use_bit_package == TRUE) chunk <- bit::chunk(from = 1, to = length(vec),
        by = chunk_size, length.out = n_chunks)
  chunk
}

readable_time <- function (x) {
  x <- as.numeric(eval(x))
  stopifnot(is.finite(x), !is.na(x))
  days <- floor(abs(x)/86400)
  hrs <- floor((abs(x) - days * 86400)/3600)
  mins <- floor((abs(x) - days * 86400 - hrs * 3600)/60)
  secs <- (abs(x) - days * 86400 - hrs * 3600 - mins * 60)
  t <- as.character(paste0(ifelse(sign(x) < 0, "-", ""),
    sprintf("%d", days), " days, ", sprintf("%d", hrs), " hours, ",
    sprintf("%d", mins), " minutes, ", sprintf("%4.2f", secs), " seconds"))
  t
}



# Extend the date list
## Get range of time indices for the data
date_min <- as.POSIXct("2010-01-01 00:00:00", tz = "UTC")

# Add a bit of a buffer in order to make sure the upper end gets captured
date_max <- as.POSIXct("2018-12-31 18:00:00", tz = "UTC")

# Create a sequence of times
time_seq <-
  seq(from = date_min,
    to = date_max + (6 * 3600),
    by = '6 hours')

chunk_size <- 100

# Extended date_list broken up into chunks of 100 time segments
 date_list_chunks <- chunkr(time_seq, chunk_size = chunk_size,
   use_bit_package = FALSE)

# Do the same for the length so that we can use these indices for further extraction
 date_list_length <- seq_along(time_seq)
   
 date_list_length_chunks <- chunkr(date_list_length, chunk_size = chunk_size)

# Preallocate ships, and ports arrays

 ships_array <- matrix(data = NA, nrow = length(ship_list_ext),
   ncol = length(date_list_ext), dimnames = list(LRNOIMOShipNo = ship_list_ext,
   time_idx = date_list_ext))

# Array to store port invasion status
 ports_array <- matrix(data = NA,  nrow = length(date_list_ext),
   ncol = length(port_list), dimnames = list(time_idx = date_list_ext,
     PortStd = sort(port_list)))

# Table to store ship wetted surface area and Net Registered tonnage for
# expanded ship list
 ship_imo_ext_idx <- match(ship_list_ext_numbers,
   ship_imo_tbl[['LRNOIMOShipNo']])

 ship_imo_ext <- ship_imo_tbl[ship_imo_ext_idx, ]
 
 ship_imo_ext[['LRNOIMOShipNo']] <- ship_list_ext


# Pull in saved arrays where ship movement and ship/port invasion status are
# held

flog.info("Loading PositionArray2.Rdata", name = "info.log")

load(file.path(root_dir(), "data", "Positionarray2.RData"))

# Create an position_array in chunks of 100 time steps each

for (j in seq_along(date_list_chunks)) {

  position_array_chunk <- position_array[ , , date_list_length_chunks[[j]],
    drop = FALSE]
    
  # Write to external object
  saveRDS(position_array_chunk, file = file.path(root_dir(), "data",
    stri_c("position_array_chunk", sprintf("%0.3d", j), ".rds")),
    compress = TRUE)
  
 rm(position_array_chunk)
 
}


  # Save the other objects
  l <- list(ship_imo_ext, ships_array, ports_array)
  
  saveRDS(l, file = file.path(outputdir,
      stri_c("ships_ports_array", sprintf("%0.3d", bb), ".rds")),
    compress = TRUE)

  flog.info("Bootstrap iteration: %i out of %i finished", bb, max(bb),
    name = "info.log")


foo <- as.tbl_cube(position_array_chunk)

# Need ship_list, position_array, port_list,ship_imo_tbl
keep_list <- c("x", "ship_list", "position_array",
  "port_list", "ship_imo_tbl", "chunk_list", "chunkr", "readable_time",
  "parallel_boot", "stime")

rm(list = setdiff(ls(), keep_list))

# Make sure port_list is in the right order
port_list <- sort(port_list)

port_list_names_check <- diff(order(port_list))
position_array_names_check <- diff(order(dimnames(position_array)[[3]]))


# Run the processing function
flog.info("Running the processing function", name = "info.log")


ftime <- proc.time() - stime

flog.info("Elapsed time: %s", readable_time(ftime[3]), name = "info.log")
flog.info("Average time: %s", readable_time(ftime[3] / length(unlist(chunk_list))),
  name = "info.log")

