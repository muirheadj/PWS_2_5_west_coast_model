library("seegSDM")
library("rprojroot")
library("dplyr")
library("rasterVis")

root_crit <- has_dirname("PWS_2_5_west_coast_model", subdir = "src")
root_dir <- root_crit$make_fix_file()


port_data <- readr::read_csv(file.path(root_dir(), "data", "port_data.csv"))
port_coords <- as.matrix(port_data %>% select(lon, lat))
dimnames(port_coords) <- list(port_data[["port"]], c("lon", "lat"))

bal_improvisus_maxent <- raster(file.path(root_dir(), "data",
  "b_improvisus_avg_exp.tif"))
crs(bal_improvisus_maxent) <- "+init=epsg:4326"

bal_improvisus_NA <- crop(bal_improvisus_maxent,
  c(xmin = -180, xmax = -105, ymin = 20, ymax = 75))

hab_suitability <- raster::extract(bal_improvisus_maxent, port_coords)
habitat_suitability_df <- data.frame(port = rownames(port_coords),
  maxent = hab_suitability, stringsAsFactors = FALSE)

port_data_merged <- left_join(port_data, habitat_suitability_df, by = "port")
readr::write_csv(port_data_merged,
  file.path(root_dir(), "data", "port_info2.csv"))

raster_pal <- viridisLite::viridis(500)

# Calculate palette using thresholds
# Maximum training sensitivity plus specificity Logistic threshold = 0.1534
 
create_threshold_pal <- function(raster_pal, threshold){
  # Creates a new color palette with the values below the threshold in grey
  palette_threshold <- floor(quantile(1:500,
    probs = threshold, type = 1))
  new_pal <- raster_pal
  new_pal[seq(palette_threshold)] <- "#999999"
  new_pal
  }
  

new_pal <- create_threshold_pal(raster_pal, threshold = 0.1846)
  
raster_theme <- rasterVis::rasterTheme(region = viridisLite::viridis(500),
  panel.background = list(col = "black"))

terrestrial_polys <- rgdal::readOGR(file.path(root_dir(), "data", "gis_data",
  "gadm36_0.shp"))
  
terrestrial_NA <- terrestrial_polys[terrestrial_polys@data$GID_0 %in% 
  c("CAN", "USA"), ]
terrestrial_NA <- as(terrestrial_NA, "SpatialPolygons")




raster_only <- rasterVis::levelplot(bal_improvisus_NA,
    layers = 1,
    bg = "black",
    margin = c(FALSE, FALSE),
    colorkey = list(space = "bottom", width = 1, height = 0.5, raster = TRUE,
      interpolate = TRUE),
    par.settings = raster_theme,
    at = seq(0, 1, length.out = 500),
    col.regions = new_pal, raster = TRUE, interpolate = TRUE,
    xlab = list(label = "Probability of suitable habitat", cex = 1,
      vjust = 0),
    ylab = NULL,
    pretty = TRUE,
    main = list(label = "Balanus improvisus", fontface = "italic"))  +
    latticeExtra::layer(sp.polygons(terrestrial_NA, col = "black", fill = "black"))
    
    
jpeg(file = file.path(root_dir(), "figures", "balanus_map.jpg"), width = 6, units = "in",
  res = 300)
  print(raster_only)
dev.off()

pdf(file.path(root_dir(), "figures", "balanus_map.pdf"), width = 7, height = 7)
  print(raster_only)
dev.off()

  