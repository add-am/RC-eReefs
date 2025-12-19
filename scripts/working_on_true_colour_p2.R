pak::pak("add-am/RcTools")

library(RcTools)
library(dplyr)

Region <- sf::st_read("data/n3_region.gpkg") |> 
  filter(Region == "Dry Tropics", Environment == "Marine")

test <- ereefs_extract(
    Region,
    StartDate = "2022-05-01",
    EndDate = "2022-05-01",
    Variable = "True Colour"
)

library(tmap)

test <- ereefs_reproject(test)

tm_shape(test) + tm_rgb()

ereefs_plot(test)
