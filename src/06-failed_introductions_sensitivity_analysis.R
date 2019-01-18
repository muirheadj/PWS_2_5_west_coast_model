# This script calculates the number of population chains, the number of introduction
# events, and the invasion outcomes for each parameter
#
# Author: jmuirhead
###############################################################################

library("tidyverse")
library("data.table")
## Section on failed introduction rates

# Read in port info for all parameters on daily population size
# Convert to wide format for each of the lifestages

ports_daily_mean <-
  purrr::map_df(create_filelist_from_results("caribbean_ports_daily_mean"),
    readRDS) %>%
  arrange(location, bootstrap, parameter, date) %>%
  select(location, bootstrap, parameter, date, lifestage, mean_population)

# Number of rows should be 74135600
assertive.properties::assert_is_of_dimension(ports_daily_mean, c(74135600, 6))

setDT(ports_daily_mean)

ports_daily_mean_wide <- dcast.data.table(ports_daily_mean, location +
  bootstrap + parameter +
  date ~ lifestage,
  value.var = "mean_population",
  fun.aggregate = mean)

save_object_to_data_dir(ports_daily_mean_wide)
file_to_read <- create_filelist_from_data("ports_daily_mean_wide")

assertive.properties::assert_is_of_length(file_to_read, 1)
ports_daily_mean_wide <- readRDS(file_to_read)

ports_daily_mean_wide[, total_population := larva + cyprid + juvenile + adult]

flog.info("Processing total population sizes for invasion outcomes",
  name = "model_progress_log")

# Population categories


# Add up population size across lifestages
# Sum up mean_population since we're interested in whether the total population is non-zero.

ports_total_daily_mean_sens <- ports_daily_mean_wide %>%
  group_by(date, parameter, bootstrap, location) %>%
  filter(total_population == larva) %>%
  mutate(total_population_threshold = total_population,
    total_population_threshold10 = if_else(total_population < 10, 0,
      total_population),
    total_population_threshold100 = if_else(total_population < 100, 0,
      total_population),
    total_population_threshold1000 = if_else(total_population < 1000, 0,
      total_population),
    total_population_threshold10000 = if_else(total_population < 10000, 0,
      total_population)) %>%
  as_tibble() %>%
  ungroup()

save_object_to_data_dir(ports_total_daily_mean_sens)

rm(ports_daily_mean)

file_to_read <- create_filelist_from_data("ports_total_daily_mean_sens")

assertive.properties::assert_is_of_length(file_to_read, 1)

ports_total_daily_mean_sens <- readRDS(file_to_read) %>%
  as_tibble()

# Read in port immigration info for all parameters on daily population size
# Just use immigration rate for larva, the others are just 0.

flog.info("Processing immigration to ports for invasion outcomes",
    name = "model_progress_log")

ports_immigration_daily_mean <- purrr::map_df(create_filelist_from_results(
  "port_immigration_daily_mean", 50), readRDS) %>%
rename(total_immigration = mean_population) %>%
filter(lifestage == "larva") %>%
select(-lifestage, -log10population, -pop_category) %>%
arrange(location, bootstrap, parameter, date)

# Join ports population and immigration data.frames

flog.info("Joining immigration with port total population sizes",
  name = "model_progress_log")

ports_population_sens <- ports_total_daily_mean_sens %>%
left_join(ports_immigration_daily_mean,
  by = c("date","parameter","bootstrap", "location")) %>%
arrange(bootstrap, parameter, location, date)

rm(ports_total_daily_mean_sens)
# Save the file temporarily for backup purposes
save_object_to_data_dir(ports_population_sens)

# Read in saved ports_immigration data
ports_population_sens <- readRDS(create_filelist_from_data(
 "ports_population_sens", n_return_check = 1))

# Create idx column for every non-zero mean population size

# This step sets up chaining for the total population size where a single chain consists of
# a population that ends with a 0 (The 0 is included as the tail of the chain
# The idx is the chain identifier. Any idx that is less than 0 indicates a chain
# of population sizes that ends in a zero. Any idx that is 1 indicates a population
# chain that does not end in zero, but remains viable, or is completely absent.

flog.info("Calculate all the population chains", name = "model_progress_log")

# Set up chaining for different thresholds
ports_population2_sens <- ports_population_sens %>%
  group_by(bootstrap, parameter, location) %>%
  mutate(pos_idx = data.table::rleid(sign(total_population_threshold))) %>%
  mutate(pos_idx = chain_ports_fn(total_population_threshold, pos_idx)) %>%
  group_by(bootstrap, parameter, location, pos_idx) %>%
  mutate(port_chain_idx = ifelse(tail(total_population, 1L) == 0 , -pos_idx,
    pos_idx))

ports_population2_sens <- ports_population2_sens %>%
  group_by(bootstrap, parameter, location) %>%
  mutate(pos_idx10 = data.table::rleid(sign(total_population_threshold10))) %>%
  mutate(pos_idx10 = chain_ports_fn(total_population_threshold10,
    pos_idx10)) %>%
  group_by(bootstrap, parameter, location, pos_idx10) %>%
  mutate(port_chain_idx10 = ifelse(tail(total_population_threshold10, 1L) == 0,
   -pos_idx10, pos_idx10))

ports_population2_sens <- ports_population2_sens %>%
  group_by(bootstrap, parameter, location) %>%
  mutate(pos_idx100 = data.table::rleid(
    sign(total_population_threshold100))) %>%
  mutate(pos_idx100 = chain_ports_fn(total_population_threshold100,
    pos_idx100)) %>%
  group_by(bootstrap, parameter, location, pos_idx100) %>%
  mutate(port_chain_idx100 = ifelse(tail(
    total_population_threshold100, 1L) == 0, -pos_idx100, pos_idx100))

ports_population2_sens <- ports_population2_sens %>%
  group_by(bootstrap, parameter, location) %>%
  mutate(pos_idx1000 = data.table::rleid(
    sign(total_population_threshold1000))) %>%
  mutate(pos_idx1000 = chain_ports_fn(total_population_threshold1000,
    pos_idx1000)) %>%
  group_by(bootstrap, parameter, location, pos_idx1000) %>%
  mutate(port_chain_idx1000 = ifelse(
    tail(total_population_threshold1000, 1L) == 0, -pos_idx1000, pos_idx1000))

ports_population2_sens <- ports_population2_sens %>%
  group_by(bootstrap, parameter, location) %>%
  mutate(pos_idx10000 = data.table::rleid(
    sign(total_population_threshold10000))) %>%
  mutate(pos_idx10000 = chain_ports_fn(total_population_threshold10000,
    pos_idx10000)) %>%
  group_by(bootstrap, parameter, location, pos_idx10000) %>%
  mutate(port_chain_idx10000 = ifelse(
    tail(total_population_threshold10000, 1L) == 0, -pos_idx10000,
    pos_idx10000))

rm(ports_population_sens)

# Take a random sampling of ports to look at data in spreadsheet
ports_sample <- sample(unique(ports_population2_sens[["location"]]), size = 20,
  replace = FALSE)

ports_population2_sens_sample <- ports_population2_sens %>%
  filter(location %in% ports_sample, bootstrap == "bootstrap_iter001") %>%
  write_tsv(., "~/Desktop/ports_population2_sens.txt")

# Calculate number of chains per port

flog.info("Calculate number of population chains per port",
  name = "model_progress_log")

n_chains_per_port_df <- ports_population2_sens %>%
  group_by(bootstrap, parameter, location) %>%
  summarise(n_chains_per_port = n_distinct(port_chain_idx),
    n_chains_per_port10 = n_distinct(port_chain_idx10),
    n_chains_per_port100 = n_distinct(port_chain_idx100),
    n_chains_per_port1000 = n_distinct(port_chain_idx1000),
    n_chains_per_port10000 = n_distinct(port_chain_idx10000)) %>%
  arrange(-n_chains_per_port)


# Number of chains per parameter
n_chains_df <- ports_population2_sens %>%
  filter(total_immigration > 0) %>%
  group_by(bootstrap, parameter) %>%
  summarise(n_chains = n_distinct(port_chain_idx),
    n_chains10 = n_distinct(port_chain_idx10),
    n_chains100 = n_distinct(port_chain_idx100),
    n_chains1000 = n_distinct(port_chain_idx1000),
    n_chains10000 = n_distinct(port_chain_idx10000)) %>%
  group_by(parameter) %>%
  select(-bootstrap) %>%
  summarise_each(stats_summary_funs)


# Check for n_instant_failures calculated from 03-process_results.R
if (!exists("n_instant_failures")) {
  n_instant_failures <- readRDS(create_filelist_from_data("n_instant_failures",
    n_return_check = 1))
}

### NUMBER OF IMMIGRATION EVENTS

n_events_from_populations_fn <- function(df, colvar){

# For each population chain, this step counts the number of introduction events
# If the number of introduction events is = 1, it counts as outright failure or
#  success.
# If the number of introduduction events is > 1, it counts as supplemented
#  failure or success.

# This step calculates a new port-chain_idx so that each chain in the data set
# gets a unique ID instead of unique IDs just within a single port.
# The sign of port_chain_idx indicates whether the population succeeds or
#   fails, with a positive port_chain_idx when the population succeeds.

  res <- df %>% group_by_(~bootstrap, ~parameter, colvar) %>%
    filter(total_immigration > 0) %>%
    mutate_(outright_failure = f_interp(
        ~length(total_immigration[uq(colvar) < 0]) == 1),
      outright_success = f_interp(
        ~length(total_immigration[uq(colvar) >= 1]) == 1),
      supplemented_failure = f_interp(
        ~length(total_immigration[uq(colvar) < 0]) > 1),
      supplemented_success = f_interp(
        ~length(total_immigration[uq(colvar) >= 1]) > 1)) %>%
    ungroup()

  # Replaces n_introduction_events_wide_df

  res2 <- res %>%
    group_by(bootstrap, parameter) %>%
    summarise(outright_failure = sum(outright_failure, na.rm = TRUE),
      outright_success = sum(outright_success, na.rm = TRUE),
      suppl._failure = sum(supplemented_failure, na.rm = TRUE),
      suppl._success = sum(supplemented_success, na.rm = TRUE)) %>%
    right_join(parameter_df_subset, by = "parameter") %>%
    right_join(n_instant_failures, by = "Source bioregion; FW reduction") %>%
    select(bootstrap, parameter, `Source bioregion; FW reduction`,
      seed_bioregion, fw_effect, fw_reduction, instant_failure,
      outright_failure, suppl._failure, suppl._success, outright_success)
  res2

}


ports_n_events <- n_events_from_populations_fn(ports_population2_sens,
  ~port_chain_idx)
ports_n_events10 <- n_events_from_populations_fn(ports_population2_sens,
  ~port_chain_idx)
ports_n_events100 <- n_events_from_populations_fn(ports_population2_sens,
  ~port_chain_idx)
ports_n_events1000 <- n_events_from_populations_fn(ports_population2_sens,
  ~port_chain_idx)
ports_n_events10000 <- n_events_from_populations_fn(ports_population2_sens,
  ~port_chain_idx)

ports_n_events_list <- list(ports_n_events, ports_n_events10,
    ports_n_events100, ports_n_events1000, ports_n_events10000)

# Calculate number of introduction events per outcome in wide format
# Filter data to only include ports identified in the n_instant_failures data.frame

n_introduction_events_fn <- function(input_df){

  # This is the same as n_introduction_events
  res <- input_df %>%
    select(bootstrap, `Source bioregion; FW reduction`, instant_failure,
      outright_failure, outright_success, suppl._failure, suppl._success) %>%
    gather(outcome, n_events, instant_failure:suppl._success, -bootstrap,
        -`Source bioregion; FW reduction`) %>%
    mutate(outcome = gsub("_", "\n", outcome)) %>%
    mutate(outcome = factor(outcome, levels = c("instant\nfailure",
      "outright\nfailure", "suppl.\nfailure", "suppl.\nsuccess",
      "outright\nsuccess"))) %>%
    ungroup() %>%
    arrange(bootstrap, `Source bioregion; FW reduction`, outcome)

  res
}


# Create list for number of introduction events for the various thresholds
n_events_list <- purrr::map(ports_n_events_list,
    n_introduction_events_fn)


### NUMBER OF CHAINS

# Get number of introduction events per parameter (based on daily mean 
# immigration rate)
# For each parameter, count the number of ports that have x successes, etc.


n_chains_from_populations_fn <- function(df, colvar){
  res <- df %>% group_by_(~bootstrap, ~parameter, colvar) %>%
    filter(total_immigration > 0) %>%
    summarise_(outright_failure = f_interp(
        ~sum(length(total_immigration[uq(colvar) < 0]) == 1)),
      outright_success = f_interp(
        ~sum(length(total_immigration[uq(colvar) >= 1]) == 1)),
      supplemented_failure = f_interp(
        ~sum(length(total_immigration[uq(colvar) < 0]) > 1)),
      supplemented_success = f_interp(
        ~sum(length(total_immigration[uq(colvar) >= 1]) > 1))) %>%
    ungroup()

  res2 <- res %>% group_by(bootstrap, parameter) %>%
    summarise(outright_failure = sum(outright_failure),
      outright_success = sum(outright_success),
      suppl._failure = sum(supplemented_failure),
      suppl._success = sum(supplemented_success)) %>%
    ungroup() %>%
    mutate(instant_failure = rep(30, nrow(.))) %>%
    select(bootstrap, parameter, instant_failure, outright_failure,
      suppl._failure, suppl._success, outright_success)

  res2
}


ports_n_chains <- n_chains_from_populations_fn(ports_population2_sens,
  ~port_chain_idx)
ports_n_chains10 <- n_chains_from_populations_fn(ports_population2_sens,
  ~port_chain_idx10)
ports_n_chains100 <- n_chains_from_populations_fn(ports_population2_sens,
  ~port_chain_idx100)
ports_n_chains1000 <- n_chains_from_populations_fn(ports_population2_sens,
  ~port_chain_idx1000)
ports_n_chains10000 <- n_chains_from_populations_fn(ports_population2_sens,
  ~port_chain_idx10000)

ports_n_chains_list <- list(ports_n_chains, ports_n_chains10, ports_n_chains100,
  ports_n_chains1000, ports_n_chains10000)


n_chains_invasion_outcome_fn <- function(ports_population_res){

  res <- ports_population_res %>%
    gather(outcome, n_chains, -bootstrap, -parameter) %>%
    arrange(parameter, outcome) %>%
    mutate(outcome = gsub("_", "\n", outcome)) %>%
    mutate(outcome = factor(outcome, levels = c("instant\nfailure",
      "outright\nfailure", "suppl.\nfailure", "suppl.\nsuccess",
      "outright\nsuccess"))) %>%
    right_join(parameter_df_subset, by = "parameter")

}
# Create list for number of chains for the various thresholds
n_chains_list <- purrr::map(ports_n_chains_list,
    n_chains_invasion_outcome_fn)

n_introductions_per_chain_fn <- function(n_intro_events, n_chains_outcome){
  res <- n_intro_events %>%
    left_join(n_chains_outcome, by = c("bootstrap",
      "Source bioregion; FW reduction", "outcome")) %>%
    group_by(`Source bioregion; FW reduction`, outcome) %>%
    mutate(n_intro_per_chain = ifelse(n_chains > 0,
      n_events / n_chains, 0))  %>%
    summarise_at(vars(n_events, n_chains, n_intro_per_chain),
      stats_summary_funs) %>%
    mutate(n_events_lcl = pmax(0, n_events_lcl),
      n_chains_lcl = pmax(0, n_chains_lcl),
      n_intro_per_chain_lcl = pmax(0, n_intro_per_chain_lcl)) %>%
    ungroup()

  res
}


# Create data.frame combining number of events and number of chains for each threshold
threshold_categories <- data_frame(key = as.character(1:5),
  threshold = c(3, 10, 100, 1000, 10000))


n_introductions_per_chain <- purrr::map2_df(n_events_list, n_chains_list,
    n_introductions_per_chain_fn, .id = "key") %>%
  left_join(threshold_categories, by = "key")

# Create arrowheads using triangles for each instant failure
n_chains_triangles <- data.frame(
    `Source bioregion; FW reduction` = unique(
      n_introductions_per_chain[["Source bioregion; FW reduction"]]),
    outcome = factor("instant\nfailure"), x = rep(c(0.6, 1, 1.4), length = 30),
    y = rep(c(29, 35, 29), length = 30)) %>%
  rename(`Source bioregion; FW reduction` = Source.bioregion..FW.reduction)


### PLOTTING SECTION
thresholds <- as.integer(unique(n_introductions_per_chain[["threshold"]]))

for (i in seq_along(thresholds)){
  input_df <- n_introductions_per_chain %>%
    filter(outcome != "instant\nfailure", threshold == thresholds[i])

    title <- sprintf("Number of chains with population threshold: %d",
      thresholds[i])

  ggplot(data = input_df, aes(x = outcome, y = n_chains_mean)) +
    facet_wrap(~`Source bioregion; FW reduction`, ncol = 2, dir = "v") +
    geom_errorbar(aes(ymin = n_chains_lcl, ymax = n_chains_ucl), width = 0.3) +
    geom_col(width = 0.5) +
    theme_cowplot(font_size = 16) +
    geom_text(aes(x = outcome, y = max(n_chains_ucl) + 4,
      label = n_events_mean), show.legend = FALSE, parse = FALSE, size = 4) +
    guides(fill = "none") +
    theme(axis.text.x = element_text(size = 10)) +
    labs(title = title, x = "Introduction outcome",
      y = "Number of population chains")


ggsave(file.path(root_dir(), "figures", stri_c("fig8_failed_invasion_rate_",, sprintf("%0.4d", thresholds[i]), ".pdf")),
  width = 8.5, height = 11, scale = 0.90)
}

# For supplemented and outright success chains, plot the final population size of adults.
# Combine the 5 bootstraps into a single plot


# Get date, location, bootstrap iteration, and parameter iteration for the successful
# invasions (i.e. those with a positive port_chain_idx).
final_pop_sizes_successful_invasions_idx <- ports_population2_sens %>%
  ungroup() %>%
  filter(port_chain_idx > 0) %>%
  select(date, parameter, bootstrap, location, port_chain_idx)

# Read in the raw daily summaries
file_to_read <- create_filelist_from_data("ports_daily_mean_[0-9]{4}", 1)
ports_daily_mean <- readRDS(file_to_read)

# Filter out the raw daily summaries by the index in the first step, and take the last value
final_pop_sizes_successful_invasions <- ports_daily_mean %>%
  inner_join(final_pop_sizes_successful_invasions_idx,
    by = c("date", "parameter", "bootstrap", "location")) %>%
  group_by(parameter, bootstrap, port_chain_idx) %>%
  summarise(max_pop = max(mean_population))

ggplot(final_pop_sizes_successful_invasions, aes(x = parameter, y = max_pop)) +
geom_boxplot(aes(fill = parameter))  + coord_flip()

ggsave(file.path(root_dir(), "figures",
  "figure9_max_populations_for_final_timestep.pdf"), width = 11, height = 8.5,
  scale = 0.90)
