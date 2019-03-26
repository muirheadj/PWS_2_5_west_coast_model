library("seegSDM")
library("rprojroot")
library("dplyr")

root_crit <- has_dirname("PWS_2_5_west_coast_model", subdir = "src")
root_dir <- root_crit$make_fix_file()


port_data <- readr::read_csv(file.path(root_dir(), "data", "port_data.csv"))
port_coords <- as.matrix(port_data %>% select(lon, lat))
dimnames(port_coords) <- list(port_data[["port"]], c("lon", "lat"))

bal_improvisus_maxent <- raster(file.path(root_dir(), "data",
  "b_improvisus_avg_exp.tif"))
crs(bal_improvisus_maxent) <- "+init=epsg:4326"

hab_suitability <- raster::extract(bal_improvisus_maxent, port_coords)
habitat_suitability_df <- data.frame(port = rownames(port_coords),
  maxent = hab_suitability, stringsAsFactors = FALSE)

port_data_merged <- left_join(port_data, habitat_suitability_df, by = "port")
readr::write_csv(port_data_merged,
  file.path(root_dir(), "data", "port_info2.csv"))
