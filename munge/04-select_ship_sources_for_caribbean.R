# This series of functions identifies the ports located in the source bioregions
# and reshaping the 3-D arrays based on 2-D ship and port information, as well
# as lifestage information.
# 
# Author: jmuirhead
###############################################################################
library(abind)

ships_array_add <- function(ships_pop_input, lifestages){
  # This function takes the ships_array generated at the bootstrapping stage
  # and adds another dimension the size of the number of lifestages generated
  # This function also changes the type of array from character to numeric
  
  
  array_placeholder <- vector(mode = "list", length = length(lifestages))
  array_list <- lapply(array_placeholder, function(x) x <- ships_pop_input)
  ships_temp2 <- abind(array_list, along = 3)
  dimnames(ships_temp2)[[3]] <- names(lifestages)
  ships_aperm <- aperm(ships_temp2, c(2, 3, 1)) # Have to change the dimensions
  # of the array in order to use population growth
  ships_aperm_out <- array(0.0, dim = dim(ships_aperm),
      dimnames = dimnames(ships_aperm))
  ships_aperm_out
}


ports_array_add <- function(ports_pop_input, lifestages){
  # This function takes the ports_array generated at the bootstrapping stage
  # and adds another dimension the size of the number of lifestages generated
  # This function also changes the type of array from character to numeric
  
  array_placeholder <- vector(mode = "list", length = length(lifestages))
  array_list <- lapply(array_placeholder, function(x) x <- ports_pop_input)
  ports_temp2 <- abind(array_list, along = 3)
  dimnames(ports_temp2)[[3]] <- names(lifestages)
  ports_aperm <- aperm(ports_temp2, c(1, 3, 2))
  # Change to numeric data type
  ports_aperm_out <- array(0.0, dim = dim(ports_aperm),
      dimnames = dimnames(ports_aperm))
  ports_aperm_out
}