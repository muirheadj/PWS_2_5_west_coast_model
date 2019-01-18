# This script calculates the number of population chains, the number of
# introduction events, and the invasion outcomes for each parameter.
# This script should not be run by itself, but is called from
# 03-process_results.R which loads all the necessary packages andd
# supporting functions.
#
# Author: jmuirhead
################################################################################

# Section on failed introduction rates ----------------------------------------

# Read in port info for all parameters on daily population size

ports_daily_mean <- map_df(
  create_filelist_from_results("caribbean_ports_daily_mean"), readRDS) %>%
  select(-log10population, -pop_category) %>%
  arrange(location, bootstrap, parameter, date)

flog.info("Processing total population sizes for invasion outcomes",
  name = "model_progress_log")

# Population categories

# Add up population size across lifestages
# Sum up mean_population since we're interested in whether the total population 
# is non-zero.

ports_total_daily_mean <- ports_daily_mean %>%
  group_by(date, parameter, bootstrap, location) %>%
  mutate(total_population = sum(mean_population)) %>%
  spread(key = lifestage, value = mean_population) %>%
  ungroup()

rm(ports_daily_mean)
gc()


# Read in port immigration info for all parameters on daily population size
# Just use immigration rate for larva, the others are just 0.

flog.info("Processing immigration to ports for invasion outcomes",
  name = "model_progress_log")


ports_immigration_daily_mean <- map_df(
  create_filelist_from_results("port_immigration_daily_mean"), readRDS) %>%
  rename(total_immigration = mean_population) %>%
  filter(lifestage == "larva") %>%
  select(-lifestage) %>%
  arrange(location, bootstrap, parameter, date)

# Join ports population and immigration data.frames

flog.info("Joining immigration with port total population sizes",
  name = "model_progress_log")

ports_population <- ports_total_daily_mean %>%
  left_join(ports_immigration_daily_mean) %>%
  arrange(bootstrap, parameter, location, date)

rm(ports_total_daily_mean)
gc()

# Save the file temporarily for backup purposes

save_object_to_data_dir(ports_population)

# Read in saved ports_immigration data
ports_population <- readRDS(
  create_filelist_from_data("ports_population_2018-04-18.rds", n_return_check = 1))

# Create idx column for every non-zero mean population size

# This step sets up chaining for the total population size where a single chain 
# consists of a population that ends with a 0 (The 0 is included as the tail of 
# the chain. The idx is the chain identifier. Any idx that is less than 0 
# indicates a chain of population sizes that ends in a zero. Any idx that is 1
# indicates a population chain that does not end in zero, but remains viable,
# or is completely absent.

flog.info("Calculate all the population chains", name = "model_progress_log")


# Set up chaining. 
# NOTE: Exclude the Panama Canal from the analysis.

ports_population_with_idx <- ports_population %>%
  filter(location != "Panama Canal") %>%
  group_by(bootstrap, parameter, location) %>%
  mutate(pos_idx = data.table::rleid(sign(total_population))) %>%
  mutate(pos_idx = chain_ports_fn(total_population, pos_idx)) %>%
  group_by(bootstrap, parameter, location, pos_idx) %>%
  mutate(chain_idx = ifelse(tail(total_population, 1L) == 0,
    -pos_idx, pos_idx)) %>%
  ungroup() %>%
  mutate(port_chain_idx = sign(chain_idx) * data.table::rleid(
    interaction(location, chain_idx)))

rm(ports_population)

# Calculate number of outcomes for each port for cases when there is immigration


# Look at only the subset of data where there is immigration of larva

n_events_from_populations <- function(df, colvar){
  
res <- df %>%
  group_by_(~bootstrap, ~parameter, colvar) %>%
  filter(total_immigration > 0) %>%
  mutate_(outright_failure = f_interp(
            ~length(total_immigration[uq(colvar) < 0]) == 1),
      outright_success = f_interp(~length(total_immigration[uq(colvar) >= 1]) == 1),
      supplemented_failure = f_interp(~length(total_immigration[uq(colvar) < 0]) > 1),
      supplemented_success = f_interp(~length(total_immigration[uq(colvar) >= 1]) > 1)) %>%
  ungroup()
}

ports_n_events_all <- n_events_from_populations(ports_population_with_idx,
  ~port_chain_idx)

save_object_to_data_dir(ports_n_events_all)

# Join back with ports_population_with_idx which contains chaining for the
# entire time period, not just when immigration is positive, as well as the
# table containing parameter info

ports_population_with_outcome <- ports_population_with_idx %>%
    left_join(ports_n_events_all) %>%
  right_join(parameter_df_subset, by = "parameter")

# Look at supplemented failure, and see if there are any non-larval lifestages
population_rescue_check <- ports_population_with_outcome %>%
  group_by(parameter, bootstrap, location) %>%
  summarize(n_outright_failure = sum(outright_failure, na.rm = TRUE),
      n_supplemented_failure = sum(supplemented_failure, na.rm = TRUE),
    n_outright_success = sum(outright_success, na.rm = TRUE),
    n_supplemented_success = sum(supplemented_success, na.rm = TRUE),
    n_larva = sum(larva > 0, na.rm = TRUE),
    n_cyprid = sum(cyprid > 0, na.rm = TRUE),
    n_juvenile = sum(juvenile > 0, na.rm = TRUE),
    n_adult = sum(adult > 0, na.rm = TRUE))

# Look for cases where there is supplemented failure
population_rescue_check %>%
    filter(n_supplemented_failure > 0) %>%
    print(n = Inf)

# Final population sizes of successful populations

population_check <- ports_population_with_idx %>%
  filter(port_chain_idx > 0) %>%
  arrange(date) %>%
  group_by(parameter, bootstrap, location) %>%
  slice(n()) %>%
  select(parameter, location, total_population) %>%
  group_by(parameter) %>%
  summarise_at(vars(total_population), stats_summary_funs) %>%
  print(n = 49)

# Calculate number of chains per port

flog.info("Calculate number of population chains per port",
  name = "model_progress_log")

n_chains_per_port_df <- ports_population_with_idx %>%
  group_by(bootstrap, parameter, location) %>%
  summarise(n_chains_per_port = n_distinct(port_chain_idx)) %>%
  arrange(-n_chains_per_port)

# Number of chains per parameter
n_chains_df <- ports_population_with_idx %>%
  filter(total_immigration > 0) %>%
  group_by(bootstrap, parameter) %>%
  summarise(n_chains = n_distinct(port_chain_idx)) %>%
  group_by(parameter) %>%
  select(-bootstrap) %>%
  summarise_all(stats_summary_funs)


# Check for n_instant_failures calculated from 03-process_results.R
if (!exists("n_instant_failures")) {
    n_instant_failures <- readRDS(file.path(data_dir,
      "n_instant_failures_2018-04-21.rds")) %>%
      mutate(seed_source = stringi::stri_extract_first(`Source bioregion; FW reduction`,
          regex = "[a-zA-Z]{3}"),
        fw_reduction = as.numeric(stringi::stri_extract_first(`Source bioregion; FW reduction`,
          regex = "(?<=;\\s).*"))) %>%
      mutate(seed_source = ifelse(seed_source == "NEA", "Atl", "Pac"))
      

  n_instant_failures <- n_instant_failures %>%
    mutate(seed_source = factor(seed_source, levels = c("Pac", "Atl"))) %>%
    mutate(`Source; FW reduction` = factor(interaction(seed_source,
      fw_reduction, sep = "; "))) %>%
    mutate(`Source; FW reduction` = forcats::fct_reorder(`Source; FW reduction`,
        fw_reduction)) %>%
    select(-`Source bioregion; FW reduction`)

          
}

# NUMBER OF IMMIGRATION EVENTS --------------------------------------------------

n_events_from_populations_summary <- function(df, colvar){

# For each population chain, this step counts the number of introduction events
# If the number of introduction events is = 1, it counts as outright failure or
# success.
# If the number of introduduction events is > 1, it counts as supplemented
#  failure or success.
# This step calculates a new port-chain_idx so that each chain in the data set
# gets a unique ID instead of unique IDs just within a single port.
# The sign of port_chain_idx indicates whether the population succeeds or
#  fails, with  a positive port_chain_idx when the population succeeds.

  res <- df %>% group_by_(~bootstrap, ~parameter, colvar) %>%
    filter(total_immigration > 0) %>%
    mutate_(outright_failure = f_interp(
      ~length(total_immigration[uq(colvar) < 0]) == 1),
      outright_success = f_interp(~length(total_immigration[uq(colvar) >= 1]) == 1),
      supplemented_failure = f_interp(~length(total_immigration[uq(colvar) < 0]) > 1),
      supplemented_success = f_interp(~length(total_immigration[uq(colvar) >= 1]) > 1)) %>%
      ungroup()

  # Replaces n_introduction_events_wide_df

  res2 <- res %>%
    group_by(bootstrap, parameter) %>%
    summarise(outright_failure = sum(outright_failure, na.rm = TRUE),
      outright_success = sum(outright_success, na.rm = TRUE),
      suppl._failure = sum(supplemented_failure, na.rm = TRUE),
      suppl._success = sum(supplemented_success, na.rm = TRUE)) %>%
    right_join(parameter_df_subset, by = "parameter") %>%
    right_join(n_instant_failures, by = "Source; FW reduction") %>%
    select(bootstrap, parameter, `Source; FW reduction`,
      instant_failure, outright_failure, suppl._failure, suppl._success,
      outright_success)
  res2
}


ports_n_events <- n_events_from_populations_summary(ports_population_with_idx, 
    ~port_chain_idx)

ports_n_events_list <- list(ports_n_events)

# Calculate number of introduction events per outcome in wide format
# Filter data to only include ports identified in the n_instant_failures
# data.frame


n_introduction_events_fn <- function(input_df){

# This is the same as n_introduction_events
  res <- input_df %>%
    select(bootstrap, `Source; FW reduction`, instant_failure,
      outright_failure, outright_success, suppl._failure, suppl._success) %>%
    gather(outcome, n_events, instant_failure:suppl._success, -bootstrap,
          -`Source; FW reduction`) %>%
    mutate(outcome = gsub("_", "\n", outcome)) %>%
    mutate(outcome = factor(outcome, levels = c("instant\nfailure",
      "outright\nfailure","suppl.\nfailure", "suppl.\nsuccess",
      "outright\nsuccess"))) %>%
    ungroup() %>%
    arrange(bootstrap, `Source; FW reduction`, outcome)
  res
}


# Create list for number of introduction events for the various thresholds
n_events_list <- purrr::map(ports_n_events_list,
  n_introduction_events_fn)


n_events_total <- n_events_list[[1]] %>%
  filter(outcome != "instant\nfailure") %>%
  group_by(bootstrap,`Source; FW reduction`) %>%
  summarise(total_events = sum(n_events)) %>%
  print(., n = 50)

# NUMBER OF CHAINS --------------------------------------------------------------

# Get number of introduction events per parameter (based on daily mean 
# immigration rate)
# For each parameter, count the number of ports that have x successes, etc.


n_chains_from_populations_fn <- function(df, colvar){
  res <- df %>% group_by_(~bootstrap, ~parameter, colvar) %>%
    filter(total_immigration > 0) %>%
    summarise_(outright_failure = f_interp(~sum(length(
        total_immigration[uq(colvar) < 0]) == 1)),
      outright_success = f_interp(~sum(length(
        total_immigration[uq(colvar) >= 1]) == 1)),
      supplemented_failure = f_interp(~sum(length(
        total_immigration[uq(colvar) < 0]) > 1)),
      supplemented_success = f_interp(~sum(length(
        total_immigration[uq(colvar) >= 1]) > 1))) %>%
    ungroup()

  res2 <- res %>% group_by(bootstrap, parameter) %>%
    summarise(outright_failure = sum(outright_failure),
      outright_success = sum(outright_success),
      suppl._failure = sum(supplemented_failure),
      suppl._success = sum(supplemented_success)) %>%
    ungroup() %>% mutate(instant_failure = rep(30, nrow(.))) %>%
    select(bootstrap, parameter, instant_failure, outright_failure,
      suppl._failure, suppl._success, outright_success)
  res2
}


ports_n_chains <- n_chains_from_populations_fn(ports_population_with_idx,
  ~port_chain_idx)

ports_n_chains_list <- list(ports_n_chains)

n_chains_invasion_outcome_fn <- function(ports_population_res){
  res <- ports_population_res %>%
    gather(outcome, n_chains, -bootstrap, -parameter) %>%
    arrange(parameter, outcome) %>%
    mutate(outcome = gsub("_","\n", outcome)) %>%
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
      "Source; FW reduction", "outcome")) %>%
    group_by(`Source; FW reduction`, outcome) %>%
    mutate(n_intro_per_chain = ifelse(n_chains > 0,
      n_events / n_chains, 0))  %>%
    summarise_at(vars(n_events, n_chains, n_intro_per_chain),
      stats_summary_funs) %>%
    mutate(n_events_lcl = pmax(0, n_events_lcl),
      n_chains_lcl = pmax(0, n_chains_lcl),
      n_intro_per_chain_lcl = pmax(0, n_intro_per_chain_lcl)) %>%
    mutate(n_events_lcl = ifelse(n_events_lcl == 0, NA, n_events_lcl),
      n_chains_lcl = ifelse(n_chains_lcl == 0, NA, n_chains_lcl),
      n_intro_per_chain_lcl = ifelse(n_intro_per_chain_lcl == 0, NA,
        n_intro_per_chain_lcl)) %>%
    ungroup()
  res
}


n_introductions_per_chain <- purrr::map2_df(n_events_list, n_chains_list,
  n_introductions_per_chain_fn, .id = "key")

# Create arrowheads using triangles for each instant failure
n_chains_triangles <- data.frame(`Source; FW reduction` =
    unique(n_introductions_per_chain[["Source; FW reduction"]]),
    outcome = factor("instant\nfailure"), x = rep(c(0.6, 1, 1.4), length = 30),
    y = rep(c(29, 35, 29), length = 30)) %>%
  rename(., `Source; FW reduction` = "Source..FW.reduction")


### PLOTTING SECTION

ggplot(data = n_introductions_per_chain, aes(x = outcome, y = n_chains_mean)) +
  facet_wrap(~`Source; FW reduction`, ncol = 2, dir = "h") +
  geom_errorbar(aes(ymin = n_chains_lcl, ymax = n_chains_ucl), width = 0.3) +
  geom_col(width = 0.5) +
  theme_cowplot(font_size = 36) +
  geom_text(aes(x = outcome, y = 45, label = n_events_mean),
    show.legend = FALSE, parse = FALSE, size = 10) +
  geom_polygon(data = n_chains_triangles, aes(x = x, y = y), fill = "gray35") +
  guides(fill = "none") +
  scale_y_continuous(limits = c(0, 50)) +
  theme(axis.text.x = element_text(size = 26)) +
  labs(x = "Introduction outcome", y = "Number of population chains")
  
#  ggsave(make_full_figure_names("fig8_failed_invasion_rate.pdf"), width = 17.8,
#      height = 22.86, units = "cm")

  save_figures("fig8_failed_invasion_rate_test_v2", dpi = 600,
    units = "cm", width = col_2_wide, height = 22.86)
  