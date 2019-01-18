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

root_crit <- has_dirname("epidemiology_model", subdir = "src")
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

# Beginning of boot function passed to mclapply
parallel_boot <- function(x){

flog.info("Processing bootstrap iterations: %s", x, name = "info.log")

boot_ship_fn <- function(...){

# Resample the ships data so that at least 5 years worth is generated
ship_boot_list <- vector(mode = "list", length = 5)
ship_boot_list[[1]] <- ship_list

  for(i in 2:5){
    ship_boot_list[[i]] <- c(ship_boot_list[[i - 1]], paste0(sample(ship_list,
              size = ceiling(0.04588974 * length(ship_boot_list[[i - 1]])),
              replace = FALSE), letters[[i]]))
  }
  ship_boot_list
}

# Pre-allocate space for extended ship position array chunks

ext_position_array_chunk_fn <- function(date_list, date_chunks){
  ext_m <- array(data = NA,
                 dim = c(length(ship_list_ext),
                       length(port_list), length(date_chunks)),
                 dimnames = list(LRNOIMOShipNo = ship_list_ext,
                                 PortStd = port_list,
                                 time_idx = date_chunks))


  ext_m[!ship_list_letter_pos, seq(port_list), ] <-
    position_array[, seq(port_list), date_list]

# Fill in the data for resampled ships that were added to the data set
  ship_imo_match <- match(str_extract(ship_list_ext,
          "\\bIMO[0-9]{7}"), ship_list)

  position_subset <- ship_list_letter_pos * ship_imo_match # Get the ships that
# are resampled and the row numbers where the originals were sampled from.
# By multiplying with a logical, this returns just the row numbers
# corresponding to the resampled ships.

# Filter out cases containing original data
  position_subset <- position_subset[position_subset > 0]

  # Add the cloned data to the extended array
  # CHECK FOR PROPER BEHAVIOUR
  ext_m[ship_list_letter_pos, seq(port_list), ] <-
    position_array[position_subset, seq(port_list), date_list]

  ext_m
}

# Create a function to blank out records before the clones are supposed to come in

clone_na_function <- function(posit_array_ext, ext_ship_list,
  date_list_annual_idx){
# Get imo numbers ending with b, c, d, e corresponding to each annual increase
# For e, there is 385 days instead of 360 days for the other time periods

  ships_resampled_b <- grep("[b]", ext_ship_list)
  ships_resampled_c <- grep("[c]", ext_ship_list)
  ships_resampled_d <- grep("[d]", ext_ship_list)
  ships_resampled_e <- grep("[e]", ext_ship_list)

# Now go 'back' in time and fill in NA for data for time slots before resampled
# ships were introduced into the system.

  posit_array_ext[ships_resampled_b, , date_list_annual_idx %in% 1] <- NA
  posit_array_ext[ships_resampled_c, , date_list_annual_idx %in% 1:2] <- NA
  posit_array_ext[ships_resampled_d, , date_list_annual_idx %in% 1:3] <- NA
  posit_array_ext[ships_resampled_e, , date_list_annual_idx %in% 1:4] <- NA

posit_array_ext
}

#boot_seq <- seq.int(from = b_start, to = b_stop)
chunk_size <- 100

for(bb in x){
  ship_list_ext <- boot_ship_fn()[[5]]

# Create output directory to store the chunks of data for each bootstrap iteration
 outputdir <- file.path(root_dir(), "data", stri_c("bootstrap_iter", sprintf("%0.3d", bb)))

dir.create(outputdir, recursive = TRUE, mode = "0777")

# Extend the date list
 ## Change to dates with time stamps of 6 hour intervals!!!!!
 date_list_ext <- format(seq(from = as.POSIXct("2009-11-16 00:00:00", tz = "UTC"),
         to = as.POSIXct("2014-11-16 00:00:00", tz = "UTC"), by = "6 hours"),
 format = "%Y-%m-%d %H:%M:%S")

# Extended date_list broken up into chunks of 100 time segments
 date_list_ext_chunks <- chunkr(date_list_ext, chunk_size = chunk_size,
   use_bit_package = FALSE)

# Do the same for the length so that we can use these indices for further extraction
 date_list_length <- rep(1:dim(position_array)[[3]],
   length.out = length(date_list_ext))
 date_list_length_chunks <- chunkr(date_list_length, chunk_size = chunk_size)

# Do the same for annual time indices
# Note: Need extra day for last one, otherwise it will leave an NA for the last day
  
 date_list_annual_breaks <- as.POSIXct(c("2009-11-16 00:00:00",
         "2010-11-16 00:00:00", "2011-11-16 00:00:00", "2012-11-16 00:00:00",
         "2013-11-16 00:00:00", "2014-11-17 00:00:00"),
     tz = "UTC")

 date_list_annual_idx <- as.numeric(Hmisc::cut2(as.POSIXct(date_list_ext,
   tz = "UTC"), date_list_annual_breaks))
 
 date_list_annual_chunks <- chunkr(date_list_annual_idx,
   chunk_size = chunk_size)

# Preallocate ships, and ports arrays

 ships_array <- matrix(data = NA, nrow = length(ship_list_ext),
   ncol = length(date_list_ext), dimnames = list(LRNOIMOShipNo = ship_list_ext,
   time_idx = date_list_ext))

# Array to store port invasion status
 ports_array <- matrix(data = NA,  nrow = length(date_list_ext),
   ncol = length(port_list), dimnames = list(time_idx = date_list_ext,
     PortStd = sort(port_list)))

# Get ship imo numbers without the letters
 ship_list_ext_numbers <- paste0("IMO", str_extract(ship_list_ext, "[0-9]{7}"))

# Match which are extended by a letter
 ship_list_letter_pos <- str_detect(ship_list_ext, "[bcde]")


# Table to store ship wetted surface area and Net Registered tonnage for
# expanded ship list
 ship_imo_ext_idx <- match(ship_list_ext_numbers,
   ship_imo_tbl[['LRNOIMOShipNo']])

 ship_imo_ext <- ship_imo_tbl[ship_imo_ext_idx, ]
 
 ship_imo_ext[['LRNOIMOShipNo']] <- ship_list_ext

# Create an extended position_array in chunks of 100 time steps each
  for (j in seq_along(date_list_ext_chunks)){
    position_array_ext_temp <- ext_position_array_chunk_fn(
        date_list_length_chunks[[j]],
        date_list_ext_chunks[[j]])

    position_array_ext <- clone_na_function(
      posit_array_ext = position_array_ext_temp,
      ext_ship_list = ship_list_ext,
      date_list_annual_idx = date_list_annual_chunks[[j]]
    )

    # Write to external object
    saveRDS(position_array_ext, file = file.path(outputdir,
        stri_c("position_array_chunk", sprintf("%0.3d", j), ".rds")),
      compress = TRUE)
  
    flog.info("Boot iter: %i, chunk iter: %i out of %i", bb, j, 
    	length(date_list_ext_chunks), name = "info.log")
  }


  # Save the other objects
  l <- list(ship_imo_ext, ships_array, ports_array)
  
  saveRDS(l, file = file.path(outputdir,
      stri_c("ships_ports_array", sprintf("%0.3d", bb), ".rds")),
    compress = TRUE)

  flog.info("Bootstrap iteration: %i out of %i finished", bb, max(bb),
    name = "info.log")
  } # End of bb loop (Beginning 148)

} # End of parallel_boot function


chunk_list <- chunkr(1:1, n_chunks = 1)


# Pull in saved arrays where ship movement and ship/port invasion status are
# held

flog.info("Loading PositionArray2.Rdata", name = "info.log")

load(file.path(root_dir(), "data", "Positionarray2.RData"))

# Need ship_list, position_array, port_list,ship_imo_tbl
keep_list <- c("x", "ship_list", "position_array",
  "port_list", "ship_imo_tbl", "chunk_list", "chunkr", "readable_time",
  "parallel_boot", "stime")

rm(list = setdiff(ls(), keep_list))

# Make sure port_list is in the right order
port_list <- sort(port_list)

#lapply(chunk_list, parallel_boot)


port_list_names_check <- diff(order(port_list))
position_array_names_check <- diff(order(dimnames(position_array)[[3]]))

outputdir <- file.path(root_dir(), "data/",
    stri_c("bootstrap_iter", sprintf("%0.3d", 1)))

# Run the processing function
flog.info("Running the processing function", name = "info.log")

mclapply(chunk_list, parallel_boot, mc.preschedule = FALSE)

ftime <- proc.time() - stime

flog.info("Elapsed time: %s", readable_time(ftime[3]), name = "info.log")
flog.info("Average time: %s", readable_time(ftime[3] / length(unlist(chunk_list))),
  name = "info.log")

