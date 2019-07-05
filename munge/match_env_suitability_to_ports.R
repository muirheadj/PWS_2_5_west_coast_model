library("seegSDM")
library("rprojroot")

root_crit <- is_rstudio_project
root_dir <- root_crit$make_fix_file()

pts <- read.csv(file.path(root_dir(), "data", "port_data.csv"))

pts_subset <- pts[, c("lon", "lat")]
names(pts_subset) <- c("x", "y")

env_suit <- raster(file.path(root_dir(), "data",
  "B_improvisus_current_avg.asc"))
crs(env_suit) <- CRS("+init=epsg:4326")


vals <- extract(env_suit, pts_subset)

outside_mask <- is.na(vals)
outside_pts <- pts_subset[outside_mask, ]


nearest_land <- function(points, raster, max_distance) {
  nearest <- function(lis, raster) {
    neighbours <- matrix(lis[[1]], ncol = 2)
    point <- lis[[2]]
    land <- !is.na(neighbours[, 2])
    if (!any(land)) {
      return(c(NA, NA))
    }
    else {
      coords <- xyFromCell(raster, neighbours[land, 1])
      if (nrow(coords) == 1) {
        return(coords[1, ])
      }
      dists <- sqrt((coords[, 1] - point[1])^2 + (coords[,
        2] - point[2])^2)
      return(coords[which.min(dists), ])
    }
  }
  neighbour_list <- extract(raster, points, buffer = max_distance,
    cellnumbers = TRUE)
  neighbour_list <- lapply(1:length(points), function(i) {
    list(neighbours = neighbour_list[[i]], point = as.numeric(points[i,
      ]))
  })
  return(t(sapply(neighbour_list, nearest, raster)))
}


nearshore <- nearestLand(outside_pts, env_suit, max_distance = 500000)

vals[outside_mask] <- extract(env_suit, nearshore)

pts$habitat_suitability <- vals

write.csv(pts, file.path(root_dir(), "data", "port_data2.csv"))



plot(env_suit)
points(pts_subset, pch = 16)

points(nearshore, pch = 16, col = "dark green")

