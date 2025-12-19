library(ereefs)
library(dplyr)
library(ggplot2)
library(stars)
library(RcTools)
library(tmap)

Region <- sf::st_read("data/n3_region.gpkg") |> 
  filter(Region == "Dry Tropics", Environment == "Marine")

StartDate = as.Date("2020-01-30")
EndDate = as.Date("2020-01-30")
input_file = "https://dapds00.nci.org.au/thredds/dodsC/fx3/GBR1_H2p0_B3p2_Cfur_Dnrt.ncml"

#convert the sf object into a bounding box, then rearrange the order for how eReefs likes it
Region <- sf::st_bbox(Region)
Region <- c(Region[1], Region[3], Region[2], Region[4])

#get all grids
grids <- ereefs::get_ereefs_grids(input_file)
        
#get x and y specifically
x_grid <- grids[["x_grid"]]
y_grid <- grids[["y_grid"]]
      
#if the value is inside the bounds of each of our coords, change it to TRUE. Those outside are automatically false
true_false_array <- 
  x_grid >= Region[1] & 
  x_grid <= Region[2] & 
  y_grid >= Region[3] & 
  y_grid <= Region[4]
      
#if the value is NA, change it to false.
true_false_array[is.na(true_false_array)] <- FALSE

#return the row index for every row that contains at least one true value:
true_rows <- which(apply(true_false_array, 1, any))

#find the first row that contains a true value
first_row <- true_rows[1]

#find the number of rows that contains a true value
num_of_rows <- utils::tail(true_rows, n = 1) - first_row

#return the row index for every row that contains at least one true value:
true_cols <- which(apply(true_false_array, 2, any))

#find the first col that contains a true value
first_col <- true_cols[1]

#find the number of cols that contains a true value
num_of_cols <- utils::tail(true_cols, n = 1) - first_col

#open the nc file
nc <- ereefs::safe_nc_open(input_file)
  
#get a vector of all times
ds <- as.Date(ereefs::safe_ncvar_get(nc, "time"), origin = as.Date("1990-01-01"))
    
#get the index for the closest days
StartDateLayerIndex <- which.min(abs(StartDate - ds))
EndDateLayerIndex <- which.min(abs(EndDate - ds))
DayCount <- pmax(EndDateLayerIndex - StartDateLayerIndex, 1)

TCbright <- 10

R_470 <- stars::read_ncdf(
    input_file, 
    var = "R_470", 
    ncsub = cbind(
        start = c(first_row, first_col, StartDateLayerIndex), 
        count = c(num_of_rows, num_of_cols, DayCount)
    )
)

R_555 <- stars::read_ncdf(
    input_file, 
    var = "R_555", 
    ncsub = cbind(
        start = c(first_row, first_col, StartDateLayerIndex), 
        count = c(num_of_rows, num_of_cols, DayCount)
    )
)

R_645 <- stars::read_ncdf(
    input_file, 
    var = "R_645", 
    ncsub = cbind(
        start = c(first_row, first_col, StartDateLayerIndex), 
        count = c(num_of_rows, num_of_cols, DayCount)
    )
)

R_470[R_470 > 1] <- NA #blue
R_555[R_555 > 1] <- NA #green
R_645[R_645 > 1] <- NA #red

R_470_10 <- R_470 * TCbright
R_555_10 <- R_555 * TCbright
R_645_10 <- R_645 * TCbright

unscaledR <- c(0, 30, 60, 120, 190, 255)/255
scaledR   <- c(1, 110, 160, 210, 240, 255)/255
scalefun  <- approxfun(unscaledR, scaledR, yleft = 0, yright = 1)

apply_scalefun <- function(x) {
  vals <- x[[1]]
  vals[vals > 1] <- NA
  vals <- scalefun(vals)
  vals <- vals * 255
  vals[vals > 255] <- 255
  x[[1]] <- vals
  x
}

R_470_10 <- apply_scalefun(R_470_10)
R_555_10 <- apply_scalefun(R_555_10)
R_645_10 <- apply_scalefun(R_645_10)

R_470_10 <- R_470_10[drop = TRUE]
R_555_10 <- R_555_10[drop = TRUE]
R_645_10 <- R_645_10[drop = TRUE]

R_470_10 <- ereefs_reproject(R_470_10)
R_555_10 <- ereefs_reproject(R_555_10)
R_645_10 <- ereefs_reproject(R_645_10)

rgb_bands <- c(R_645_10, R_555_10, R_470_10)
test <- merge(rgb_bands)

tm_shape(test) + tm_rgb()


#create a scaling function
    unscaledR <- c(0, 30, 60, 120, 190, 255)/255
    scaledR   <- c(1, 110, 160, 210, 240, 255)/255
    scalefun  <- approxfun(unscaledR, scaledR, yleft = 0, yright = 1)

nc_data <- list(R_645, R_555, R_470)

    #apply this scaling function plus some other adjustments
    nc_data <- purrr::map(nc_data, \(x) {
      x[x > 1] <- NA
      x <- x * 10
      vals <- x[[1]]# * 10 #extract values and multiple by 10
      #vals[vals > 1] <- NA #change _FillValues into NA
      vals <- scalefun(vals)*255 #scale values up to rgb channels (0 to 255)
      vals[vals > 255] <- 255 #cap vals
      x[[1]] <- vals #put values back into the netCDF
      x[drop = TRUE] #drop the time values
    })

    #join files into a single stars object
    nc_data <- do.call(c, nc_data)

    #merge the product of above
    nc_data <- merge(nc_data)

nc_data <- ereefs_reproject(nc_data)

tm_shape(nc_data) + tm_rgb()

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
