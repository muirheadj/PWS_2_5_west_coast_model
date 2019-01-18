# Example Unit Testing Script
library("testthat")
context("Port results")

# Test for port information -----------------------------------------------------
test_that("There are 50 files of port results", {
  # Get list of files
  create_filelist_from_results <- function(pattern){
    mylist <- list.files(path = dir_list[["results_dir"]], pattern = pattern,
        full.names = TRUE, recursive = TRUE)
  }
  
  ports_list <- create_filelist_from_results(pattern = "ports_pop")
  expect_length(ports_list, n = 50)
})

test_that("Port names are in the right order", {
  # Check for ports name order
  port_name_check <- readRDS(ports_list[[1]])
      
# Check for Marcus Hook as Example
  expect_equal(which(dimnames(port_name_check)[[3]] == "Marcus Hook"), 594)
      
  port_names_check <- dimnames(port_name_check)[[3]]
      
  port_name_check_order <- port_names_check[order(port_names_check)]
  port_name_order2 <- port_names_check %>%
      sort()

})

test_that("Port names are unique and match the names in port_data", {
  # Check for unique port names
 expect_equal(all(!duplicated(port_name_check_order)), TRUE)
      
# Load standard of port data
 port_data <- readRDS(file = paste0(dir_list[["data_dir"]], "/",
     "ports_data.rds")) %>%
   arrange(PortStd)

expect_equal(all(port_name_order2 %in% port_data[["PortStd"]]), TRUE)

})

