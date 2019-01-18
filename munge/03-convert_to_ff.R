# This program takes a 3d position array and converts it into ff format for 
# higher performance.
# 
# Author: jmuirhead
###############################################################################



setwd("/Volumes/My Book Duo/Caribbean_Bootstrap_Iterations/")

wd <- getwd()

boot_seq <- 1:1

library(ff)
library(R.utils)
library(stringr)
library(Hmisc)

start_time <- proc.time() # Record the starting time

readable_time <- function (x) {
  x <- as.numeric(eval(x))
  stopifnot(is.finite(x), !is.na(x))
  days <- floor(abs(x)/86400)
  hrs <- floor((abs(x) - days * 86400)/3600)
  mins <- floor((abs(x) - days * 86400 - hrs * 3600)/60)
  secs <- (abs(x) - days * 86400 - hrs * 3600 - mins * 60)
  t <- as.character(paste0(ifelse(sign(x) < 0, "-", ""), sprintf("%d",
    days), " days, ", sprintf("%d", hrs), " hours, ", sprintf("%d",
    mins), " minutes, ", sprintf("%4.2f", secs), " seconds"))
  t
}

for (b in boot_seq){
# Get list of files that have to be changed
  bootdir <- paste0("bootstrap_iter", sprintf("%0.3d", b))
  
# Get list of files within each bootstrap directory
  file_list <- list.files(path = paste0(wd,"/", bootdir), 
    pattern = "position_array_chunk[0-9]{3}.rds",
    recursive = TRUE, include.dirs = TRUE)
# Loop through each file and create a *.ff version
  for (i in seq_along(file_list)){
    position_temp <- readRDS(paste0(bootdir,"/", file_list[i]))
    assign(paste0("position_ff", sprintf("%0.3d", i)), ff(position_temp, 
                         dimnames = dimnames(position_temp),
                         dim = dim(position_temp),
                         vmode = "boolean",
      filename = sub(".rds", ".ff", paste0(bootdir,"/", file_list[i]))))
    close(get(paste0("position_ff", sprintf("%0.3d", i))))
    cat("Bootstrap:", b, "filename:", file_list[i], "finished\n")
  }

  ffsave.image(file = paste0(bootdir, "/", "ff_archive"), precheck = FALSE, 
    safe = FALSE)

  cat("Bootstrap iteration:", b, "finished\n")

  # Compress the file as *.tar.bz2
  bzip_compress <- function(dir) system(sprintf('tar -jcvf %1$s %2$s', 
            paste0(dir, ".tar.bz2"), dir))
  bzip_compress(bootdir)
  cat("Compression for bootstrap iteration:", b, "finished\n")
} # End of bootstrap loop
  
finish_time <- proc.time() - start_time

message("Elapsed time: ", readable_time(finish_time[3]))
message("Average time: ", readable_time(finish_time[3]/length(boot_seq)))

say <- function(what) system(sprintf('say "%s"', what))
say("Hey!.. Your program is done")
