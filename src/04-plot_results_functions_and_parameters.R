# This program contains all of the functions and stored parameters used to
# process the results
#
# Author: jmuirhead
################################################################################

# Set variables and paths -------------------------------------------------------
library("dplyr")
library("tidyr")
library("forcats")
library("ggplot2")
library("yaml")
library("rprojroot")

root_crit <- has_dirname("PWS_2_5_west_coast_model", subdir = "src")
root_dir <- root_crit$make_fix_file()

options(tibble.width = Inf)

# Set flags to run only certain part of the code
animation_flag <- FALSE

yaml_params <- yaml::read_yaml(file.path(root_dir(), "params.yaml"))

source_region <- "foo" # Since foo will not be found, run all source_regions

full_sample_datespan <- seq(13149)

# Define color palettes
custom_cols <- c("#7D0112FF", "#A14C26FF", "#C17F45FF", "#DAAE6DFF",
  "#EBD79CFF", "#440154FF", "#414487FF", "#2A788EFF", "#22A884FF", "#7AD151FF")

# Print out of color palette
cols_sample <- tibble(custom_cols, barlength = 1)

# Define max image width
col_1_wide <- 8.7 # in cm
col_1.5_wide <- 11.4
col_2_wide <- 17.8

fig_max_height <- 22 # cm


# General helper functions ------------------------------------------------------

'%nin%' <- Negate('%in%')

log10p_trans <- function(){
  # Log10(x+1) tranformation for use in scales and ggplot2
  trans <- function(x) log10(x + 1)
  inv <- function(x) (10 ^ x) - 1
  trans_new("log10p", trans, inv, domain = c(0, Inf))
}

log10p <- function(x) log10(x + 1)

scale_x_fn <- function(x, scale_x) sign(x) * (abs(x) / (10 ^ scale_x))
na_check <- function(x) is.na(x)
zero_check <- function(x) vapply(x, function(y) isTRUE(all_equal(y, 0)),
  logical(1L))

formatter_factory <- function(scale_x, digits = 3){
  function(x){
    x_scaled <- scale_x_fn(x, scale_x)
    x_scaled_zero_check <- zero_check(x_scaled)
    x_scaled_na_check <- na_check(x_scaled)

    magnitude <- rep(0, length = length(x_scaled))
    magnitude[!x_scaled_zero_check] <-
      floor(log10(abs(x_scaled[!x_scaled_zero_check])))
    magnitude[x_scaled_na_check] <- NA
    val <- format((x_scaled / (10 ^ magnitude)), nsmall = digits - 1,
      digits = digits)
    # If magnitude are all 0s, just use the values
    if (isTRUE(all_equal(magnitude, 0))) {
      res <- parse(text = val)
    } else {
      # If the value is 0, replace the scientific notation with just "0"
      val_zero_check <- zero_check(x_scaled / (10 ^ magnitude))
      val_na_check <- na_check(x_scaled / (10 ^ magnitude))
      val_zero_text <- ifelse(!val_zero_check,
        paste0(val, "%*% 10^{", magnitude, "}"), "0")
      val_text <- rep(NA, length = length(x_scaled))
      val_text[!val_na_check] <- val_zero_text[!val_na_check]
      res <- parse(text = val_text)
    }
    res
  }
}

# ggplot2 formatting labeller function
formatter_standard <- formatter_factory(scale_x = 0)

make_full_figure_names <- function(filename){
# This function takes the filename of an object, and prepends a specified
# directory in order to save it to the correct location in the graph output
# directory

  fname <- file.path(figures_dir, filename)
  fname
}

save_results_filename <- function(filename, param){
# This function takes the filename of an object, and prepends a specified
# directory in order to save it to the correct location in the base directory
  fname <- file.path(results_dir,
    stringi::stri_c(filename, param, "_", as.Date(Sys.Date()), ".rds"))
  fname
}

save_object_to_data_dir <- function(x, include_date = FALSE) {
# This function saves an object in the same directory as the data directory
# with the filename the same as an object with the date appended to it.

  xsym <- lazyeval::expr_text(x)

	if (include_date == FALSE) {
	  saveRDS(x, file = file.path(data_dir, stringi::stri_c(xsym, ".rds")))
	}

	if (include_date == TRUE) {
	  saveRDS(x, file = file.path(data_dir,
	    stringi::stri_c(xsym, "_", as.Date(Sys.Date()), ".rds")))
	}
}

save_figures <- function(filename, plot = ggplot2::last_plot(), units = "cm",  ...) {
  cowplot::ggsave2(make_full_figure_names(stri_c(filename, ".pdf")),
      plot, ...)
  cowplot::ggsave2(make_full_figure_names(stri_c(filename, ".jpg")),
      plot, ...)
  cowplot::ggsave2(make_full_figure_names(stri_c(filename, ".png")),
      plot, ...)
}


# List to calculate summary statistics

# This is used in dplyr to summarize specified variables
stats_summary_funs <- list(n = ~sum(!is.na(.)),
  mean = ~mean(., na.rm = TRUE),
  sd = ~sd(., na.rm = TRUE),
  n_se = ~sd(., na.rm = TRUE) / sqrt(sum(!is.na(.))),
  median = ~median(., na.rm = TRUE),
  min = ~min(., na.rm = TRUE),
  max = ~max(., na.rm = TRUE))

# Calculate pooled estimate and se for proportions
prop_mean_fn <- function(x, n, na.rm = TRUE){
  res <- sum(x, na.rm = na.rm) / sum(n, na.rm = na.rm)
  res
}

prop_se_fn <- function(x, n, na.rm = TRUE){
  res <- sqrt(sum(x, na.rm = na.rm) / sum(n, na.rm = na.rm) *
    (1 - sum(x, na.rm = na.rm) / sum(n, na.rm = na.rm)) *
    sum((1 / n), na.rm = na.rm))
  res
}


# Get list of files
create_filelist_from_results <- function(pattern, n_return_check = NULL){
  mylist <- list.files(path = results_dir, pattern = pattern,
    full.names = TRUE, recursive = TRUE)
  if (length(mylist) == 0) stop("Files not found")
  if (!is.null(n_return_check)) {
    assertive.properties::assert_is_of_length(mylist, n_return_check)
  }
  mylist
}

create_filelist_from_data <- function(pattern,  n_return_check = NULL){
  mylist <- list.files(path = data_dir, pattern = pattern,
    full.names = TRUE, recursive = TRUE)
  if (length(mylist) == 0) stop("Files not found")
  if (!is.null(n_return_check)) {
    assertive.properties::assert_is_of_length(mylist, n_return_check)
  }
  mylist
}

# Processing data section ------------------------------------------------------

process_array_fn <- function(x, datespan){
  temp <- readRDS(x)
  if (length(dim(temp)) == 3) {
    temp2 <- temp[datespan, , , drop = FALSE]
  }

  if (length(dim(temp)) == 2) {
    temp2 <- temp[datespan, , drop = FALSE]
  }

  attr(temp2, "parameter") <-
    unlist(stringr::str_extract(x, "parameter[0-9]{3}"))

  attr(temp2, "bootstrap") <-
    unlist(stringr::str_extract(x, "bootstrap_iter[0-9]{3}"))
  temp2
}

list_process_df_fn <- function(x){

# Read each item into a list, and add columns indicating the treatment.
# The output is a data.frame where the attributes assigned to the matrix are
# added as additional variables.

		if(is.null(names(dimnames(x)))) stop( "Dimnames of the array must be named")
		
  temp_cube <- dplyr::as.tbl_cube(x, met_name = "population")
		temp_df <- as_tibble(temp_cube)

  temp_df[["time_idx"]] <- fasttime::fastPOSIXct(temp_df[["time_idx"]], tz = "UTC")

  temp_df[["lifestage"]] <- factor(temp_df[["lifestage"]],
      levels = c("larva", "cyprid", "juvenile", "adult"))
  temp_df[["parameter"]] <- attr(x, "parameter")
  temp_df[["bootstrap"]] <- attr(x, "bootstrap")
  temp_df
}

# Store parameters from each of the results

process_parameters_fn <- function(x){
  # This function just reads in a list of files that contain the parameters for
  # every model run and saves them in a data.frame.

  temp <- readRDS(x) %>%
    as.data.frame(., stringsAsFactors = FALSE)
}

# This function processes the raw data from the ports results

# FIXME
process_ports_fn <- function(i){

  flog.info("Processing ports file %s", i, name = "model_progress_log")

  # n_destination_ports is the number of non-seed ports
  

  n_destination_ports <- readRDS(create_filelist_from_data("n_destination_ports", 1))

  flog.info("Started reading ports_temp", name = "model_progress_log")

  ports_temp_subset <- readRDS(file.path(data_dir, "ports_temp.rds"))[[i]]

  flog.info("Finished reading ports_temp", name = "model_progress_log")

  # Note: as.Date assumes that the TZ is "UTC"
  ports_longformat_with_coords <- list_process_df_fn(ports_temp_subset) %>%
    left_join(port_data, by = c("location" = "port")) %>%
    left_join(parameters_df, by = "parameter") %>%
    mutate(date = as.Date(time),
      seed_port = if_else(reg_lrggeo == seed_bioregion, "source port",
        "non-source port")) %>%
    select(bootstrap, parameter, bioregion, location,
      port_country, lat, lon, seed_port,
      lifestage, date, date_and_time,
      population) %>%
    as_tibble() %>%
    ungroup()

  param <- ports_longformat_with_coords %>%
    ungroup() %>%
    select(parameter, bootstrap) %>%
    unique() %>%
    unlist()

  param_label <- sprintf("%s_%s", param[[1]], param[[2]])

  rm(ports_temp_subset)

# Get source ports in order to exclude them from summary

# Calculate mean daily population size for seed ports. Include
# ports with 0 population.

  destination_ports_daily_mean <- ports_longformat_with_coords %>%
    filter(seed_port == "non-source port",
      bioregion %in% seed_bioregions) %>%
    group_by(date, parameter, bootstrap, lifestage, location, port_country,
      bioregion, lat, lon) %>%
    summarize(mean_population = mean(population),
      log10population = log10p(mean(population))) %>%
    ungroup()

# Save daily values for each port
  saveRDS(destination_ports_daily_mean,
    file = save_results_filename("destination_ports_daily_mean_", param_label))

# Calculate summary statistics for all ports, including those with 0 population
  all_ports_mean <- destination_ports_daily_mean %>%
    group_by(date, parameter, bootstrap, lifestage) %>%
    summarise(n = n(), ports_sum = sum(mean_population),
      ports_mean = mean(mean_population),
      ports_median = median(mean_population),
      ports_min = min(mean_population),
      ports_max = max(mean_population),
      ports_sd = sd(mean_population),
      ports_se = sd(mean_population) / n())

  saveRDS(all_ports_mean,
    file = save_results_filename("all_ports_mean_", param_label))

  flog.info("Saving all ports mean population", name = "model_progress_log")


# Calculate summary statistics for just invaded ports

  invaded_ports_mean <- destination_ports_daily_mean %>%
    filter(mean_population > 0) %>%
    group_by(date, parameter, bootstrap, lifestage) %>%
    summarise(n = n(), ports_sum = sum(mean_population),
      ports_mean = mean(mean_population),
      ports_median = median(mean_population),
      ports_min = min(mean_population),
      ports_max = max(mean_population),
      ports_sd = sd(mean_population),
      ports_se = sd(mean_population) / n()) %>%
    as_tibble() %>%
    ungroup()


  flog.info("Saving invaded ports mean population", name = "model_progress_log")

  rm(all_ports_mean, invaded_ports_mean)

# Calculate number and proportion of invaded ports

  invaded_ports_count <- destination_ports_daily_mean %>%
    filter(mean_population > 0) %>%
    group_by(date, parameter, bootstrap, lifestage) %>%
    summarize(n_destination_ports, n_invaded_ports = n_distinct(location),
      ports_proportion = n_distinct(location) / n_destination_ports) %>%
    ungroup()

  saveRDS(invaded_ports_count,
    file = save_results_filename("destination_ports_count_", param_label))

  invaded_ports_pooled_lifestages_count <- destination_ports_daily_mean %>%
    filter(mean_population > 0) %>%
    group_by(date, parameter, bootstrap) %>%
    summarize(n_destination_ports, n_invaded_ports = n_distinct(location),
      ports_proportion = n_distinct(location) / n_destination_ports) %>%
    ungroup()

  saveRDS(invaded_ports_pooled_lifestages_count,
    file =  save_results_filename("destination_ports_pooled_count_", param_label))

  flog.info("Saving number of invaded ports", name = "model_progress_log")

  rm(invaded_ports_count, invaded_ports_pooled_lifestages_count)
}

process_port_immigration_fn <- function(i) {
# This function summarises the port_immigration_temp array
# calculated in 03-process-results.R

  flog.info("Processing port immigration file %i", i,
    name = "model_progress_log")

# Fix names in port_immigration_temp so that I can use the same list_process_df
# function to process the data
    flog.info("Started reading ports_immigration_temp", name = "model_progress_log")

	port_immigration_subset <- readRDS(file.path(data_dir,
	  "port_immigration_temp.rds"))[[i]]

  flog.info("Finished reading port_immigration_temp", name = "model_progress_log")

  ports_immigration_with_coords <-
    list_process_df_fn(port_immigration_subset) %>%
    left_join(port_data, by = c("Location" = "PortStd")) %>%
    left_join(parameters_df, by = "parameter") %>%
    mutate(date = as.Date(Time), seed_port =
      if_else(bioregion == seed_bioregion,
        "Source port", "Caribbean port")) %>%
    select(bootstrap, parameter, bioregion, location = Location,
      port_country = Port_Country, lat, lon, seed_port,
      lifestage = Lifestage, date, date_and_time = Time,
      population = Population) %>%
    as_tibble() %>%
    ungroup()

  rm(port_immigration_subset)

  # Calculate population size. Note: Exclude Panama Canal from immigration
  # calculations
  port_immigration_daily_mean <- ports_immigration_with_coords %>%
    filter(seed_port == "Caribbean port", bioregion %in% seed_bioregions,
      location != "Panama Canal") %>%
    group_by(date, parameter, bootstrap, lifestage, location, port_country,
      bioregion, lat, lon) %>%
    summarize(mean_population = mean(population, na.rm = TRUE),
      log10population = log10p(mean(population, na.rm = TRUE))) %>%
    ungroup()

  param <- port_immigration_daily_mean %>%
    ungroup() %>%
    select(parameter, bootstrap) %>%
    unique() %>%
    unlist()

  param_label <- sprintf("%s_%s", param[[1]], param[[2]])

  flog.info("Saving mean daily immigration", name = "model_progress_log")

  saveRDS(port_immigration_daily_mean,
    file = save_results_filename("port_immigration_daily_mean_", param_label))

  port_immigration_mean <- port_immigration_daily_mean %>%
    group_by(date, parameter, bootstrap, lifestage) %>%
    summarise(n = n(), ports_sum = sum(mean_population),
      ports_mean = mean(mean_population),
      ports_median = median(mean_population), ports_min = min(mean_population),
      ports_max = max(mean_population), ports_sd = sd(mean_population),
      ports_se = sd(mean_population) / n())

  flog.info("Saving mean port immigration", name = "model_progress_log")

  saveRDS(port_immigration_mean,
    file = save_results_filename("port_immigration_mean_", param_label))

}

process_instant_mortality_fn <- function(mort_mat){
# This function takes in a matrix of instant mortality results, and converts it
# to a data.frame in long format, which it then saves.

  flog.info("Processing instant port mortality", name = "model_progress_log")

  port_data_check <- getAnywhere("port_data")

  if (length(port_data_check[["where"]]) == 0) {
    stop("Please ensure that port_data is available")
  }

  temp <- as_data_frame(mort_mat, stringsAsFactors = FALSE)
  temp[["datetime"]] <- fasttime::fastPOSIXct(rownames(mort_mat), tz = "UTC")
  temp[["date"]] <- as.Date(rownames(mort_mat))
  temp2 <- temp %>%
    gather(., location, success, -datetime, -date)
  temp2[["parameter"]] <- attr(mort_mat, "parameter")
  temp2[["bootstrap"]] <- attr(mort_mat, "bootstrap")
  temp3 <- temp2 %>%
  left_join(port_data, by = c("location" = "PortStd")) %>%
  left_join(parameters_df, by = "parameter") %>%
  mutate(seed_port = if_else(bioregion == seed_bioregion,
    "Source port", "Caribbean port"))

  param_label <- paste(unique(temp2[["bootstrap"]]),
    unique(temp2[["parameter"]]), sep = "_")

   saveRDS(temp3,
    file = save_results_filename("/port_instant_mortality/port_instant_mortality_",
      param_label))
}

melt_ships_temp <- function(ships_temp){
  flog.info("Processing ships parameter %i for ships_longformat", i,
      name = "model_progress_log")

  ships_longformat <- list_process_df_fn(ships_temp) %>%
    as_tibble() %>%
    mutate(date = as.Date(Time)) %>%
    select(bootstrap, parameter, location = Location,
      lifestage = Lifestage, date, date_and_time = Time,
      population = Population) %>%
    ungroup()

  param_label <- sprintf("%s_%s", attr(ships_temp, "parameter"),
    attr(ships_temp, "bootstrap"))

  saveRDS(ships_longformat,
    file = save_results_filename("ships_longformat_",
      param = param_label))
}

process_ships_fn <- function() {
# This function processes the raw results for ships. It melts the data into
# long-format, calculates summary statistics, and then saves the summary
# statistics for later use.
# Input: a number describing the sequence of parameters to be processed.
# Output: Saved data.frames of processed ship results

  flog.info("Processing ships parameter %i", i,
    name = "model_progress_log")

  invaded_ships_daily_mean <- ships_longformat %>%
    group_by(date, parameter, bootstrap, lifestage, location) %>%
    summarize(mean_population = mean(population)) %>%
    ungroup()

    rm(ships_longformat)
    gc()


  n_all_ships <- invaded_ships_daily_mean %>%
    summarise(n = n_distinct(location)) %>%
    unlist()

  all_ships_mean <- invaded_ships_daily_mean %>%
    group_by(date, parameter, bootstrap, lifestage) %>%
    summarise(n = n(), ships_sum = sum(mean_population),
      ships_mean = mean(mean_population),
      ships_median = median(mean_population),
      ships_min = min(mean_population), ships_max = max(mean_population),
      ships_sd = sd(mean_population), ships_se = sd(mean_population) / n())

  param <- all_ships_mean %>%
    ungroup() %>%
    select(parameter, bootstrap) %>%
    unique() %>%
    unlist()

  param_label <- sprintf("%s_%s", param[[1]], param[[2]])

  saveRDS(all_ships_mean,
    file = save_results_filename("all_ships_mean_", param_label))

  flog.info("Saving all ships mean population",
    name = "model_progress_log")

  rm(all_ships_mean)

  invaded_ships_mean <- invaded_ships_daily_mean %>%
    filter(mean_population > 0) %>%
    group_by(date, parameter, bootstrap, lifestage) %>%
    summarise(n = n(), ships_sum = sum(mean_population),
      ships_mean = mean(mean_population),
      ships_median = median(mean_population),
      ships_min = min(mean_population), ships_max = max(mean_population),
      ships_sd = sd(mean_population),
      ships_se = sd(mean_population) / n())

  saveRDS(invaded_ships_mean,
    file = save_results_filename("invaded_ships_mean_", param_label))

  flog.info("Saving invaded ships mean population",
    name = "model_progress_log")

  rm(invaded_ships_mean)

  invaded_ships_count <- invaded_ships_daily_mean %>%
    filter(mean_population > 0) %>%
    group_by(date, parameter, bootstrap, lifestage) %>%
    summarize(n_invaded_ships = n_distinct(location),
      ships_proportion = n_invaded_ships / n_all_ships) %>%
    ungroup()

  saveRDS(invaded_ships_count,
    file = save_results_filename("invaded_ships_count_", param_label))

  invaded_ships_lifestages_pooled_count <- invaded_ships_daily_mean %>%
    filter(mean_population > 0) %>%
    group_by(date, parameter, bootstrap) %>%
    summarize(n_invaded_ships = n_distinct(location),
      ships_proportion = n_invaded_ships / n_all_ships) %>%
    ungroup()

  saveRDS(invaded_ships_lifestages_pooled_count,
    file = save_results_filename("invaded_ships_pooled_count_",
      param_label))

  flog.info("Saving number of invaded ships",
    name = "model_progress_log")

  rm(invaded_ships_count)
}

graph_data_preprocessing <- function(file_pattern){

# This function converts a list of the summarized results and converts them to
# long-form data.frames.

  temp_list <- create_filelist_from_results(pattern = file_pattern)
  temp_df <- purrr::map_dfr(temp_list, readRDS) %>%
    left_join(parameters_df, by = "parameter") %>%
    ungroup()
  # Fix data types
  temp_df[["date"]] <- as.Date(temp_df[["date"]])
  temp_df
}


# Sources of mortality ---------------------------------------------------------

port_mortality_summary_fn <- function(i, ports_instant_mortality_list){

  # This function calculates the number and proportion of successful and failed
  # invasions for each parameter, and summarizes across the different bootstrap
  # iterations.

  # Input: A vector of 1:10 indicating the number of parameters
  # Output: A data.frame containing the summary of mortality rates for each
  #  parameter.

  flog.info("Processing instant port mortality summary parameter %i", i,
    name = "model_progress_log")

  idx <- grepl(sprintf("parameter%0.3d", i), ports_instant_mortality_list,
    fixed = TRUE)
  ports_instant_mortality_list_sub <- ports_instant_mortality_list[idx]

  ports_instant_mortality_df_sub <- map_dfr(ports_instant_mortality_list_sub,
      readRDS) %>%
    mutate(seed_source = ifelse(seed_bioregion == "NEA-II", "Atl", "Pac")) %>%
    mutate(seed_source = factor(seed_source, levels = c("Pac", "Atl")),
    `Source; FW reduction` = factor(interaction(seed_source,
      fw_reduction, sep = "; "))) %>%
    mutate(`Source; FW reduction` = forcats::fct_reorder(`Source; FW reduction`,
        fw_reduction)) %>%
    select(-seed_bioregion, -fw_effect, -`Source bioregion; FW reduction`)

  # For each bootstrap and parameter combination, get ports_zero_pop_names_df
  # Filter out Panama Canal from results

  ports_zero_pop_names_df <- graph_data_preprocessing(
      sprintf("caribbean_ports_daily_mean_parameter%0.3d", i)) %>%
    group_by(bootstrap, `Source; FW reduction`, date, location,
      bioregion) %>%
    summarise(total_population = sum(mean_population, na.rm = TRUE)) %>%
    filter(total_population == 0, bioregion %in% seed_bioregions,
      location != "Panama Canal") %>%
    select(bootstrap, `Source; FW reduction`, date, location) %>%
    unique() %>%
    ungroup()

  # Calculate number of successess and failures

  ports_instant_mortality_temp_df <- ports_instant_mortality_df_sub %>%
    # include only caribbean ports and not the Panama Canal
    filter(bioregion %in% seed_bioregions,
      seed_port == "Caribbean port", location != "Panama Canal") %>%
    # include only ports that have a zero total population on that date
    right_join(ports_zero_pop_names_df,
      by = c("bootstrap", "date", "location",
        "Source; FW reduction")) %>%
    group_by(bootstrap, `Source; FW reduction`, date) %>%
    # Count up number of times a successful immigration occurs, as well as total
    # number of ports that receive immigrants

    summarize(n_success = sum(success, na.rm = TRUE),
      n_ports_introduction = sum(!is.na(success))) %>%
    mutate(proportion_fail = 1 - n_success / n_ports_introduction,
      n_fail = n_ports_introduction - n_success) %>%
    ungroup()

# Calculate summary statistics across bootstraps
  ports_instant_mortality_summary_df <- ports_instant_mortality_temp_df %>%
    group_by(`Source; FW reduction`, date) %>%
    summarise(prop_fail_pooled = prop_mean_fn(n_fail, n_ports_introduction),
      prop_fail_pooled_se = prop_se_fn(n_fail, n_ports_introduction),
      prop_fail_pooled_lcl = prop_mean_fn(n_fail, n_ports_introduction) +
        qt(0.025, 4) * prop_se_fn(n_fail, n_ports_introduction),
      prop_fail_pooled_ucl =  prop_mean_fn(n_fail, n_ports_introduction) +
        qt(0.975, 4) * prop_se_fn(n_fail, n_ports_introduction),
      n_fail_mean = mean(n_fail), n_fail_sd = sd(n_fail),
      n_fail_min = min(n_fail), n_fail_max = max(n_fail),
      n_success_mean = mean(n_success), n_success_sd = sd(n_success),
      n_success_min = min(n_success), n_success_max = max(n_success),
      n_ports_introduction_mean = mean(n_ports_introduction),
      n_ports_introduction_sd = sd(n_ports_introduction),
      n_ports_introduction_min = min(n_ports_introduction),
      n_ports_introduction_max = max(n_ports_introduction))

  rm(ports_instant_mortality_temp_df)

  ports_instant_mortality_summary_df

}

chain_ports_fn <- function(pop, idx){

# This function fixes the chain IDs so that the step right before the
# population chain grows and the step right after the population chain where
# the population size is 0 # get the same ID.
# Inputs: Population size, and current chain_idx
# Output: Fixed chain_idx

  idx <- ifelse(pop == 0 & lead(pop) > 0, NA, idx)
  idx <- metrumrg::nocb(idx)
  idx <- ifelse(pop == 0 & lag(pop) > 0, NA, idx)
  idx <- metrumrg::forbak(idx)
  idx
}
