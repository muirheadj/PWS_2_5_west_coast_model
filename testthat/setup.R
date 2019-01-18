# TODO: Add comment
# 
# Author: jmuirhead
###############################################################################

library("rprojroot")

root_crit <- has_dirname("epidemiology_model", subdir = "src")
root_dir <- root_crit$make_fix_file()

dir_list <- list(root_dir = root_dir(),
    results_dir = find_root_file("results", criterion = root_crit),
    figures_dir = find_root_file("figures", criterion = root_crit),
    data_dir = find_root_file("data", criterion = root_crit))