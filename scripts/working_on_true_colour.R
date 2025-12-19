library(ereefs)
library(dplyr)

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

R_470 <- safe_ncvar_get(
    nc, 
    "R_470", 
    start = c(first_row, first_col, StartDateLayerIndex), 
    count = c(num_of_rows, num_of_cols, DayCount)
)

R_555 <- safe_ncvar_get(
    nc, 
    "R_555", 
    start = c(first_row, first_col, StartDateLayerIndex), 
    count = c(num_of_rows, num_of_cols, DayCount)
)

R_645 <- safe_ncvar_get(
    nc, 
    "R_645", 
    start = c(first_row, first_col, StartDateLayerIndex), 
    count = c(num_of_rows, num_of_cols, DayCount)
)

R_470 <- R_470 * TCbright
R_555 <- R_555 * TCbright
R_645 <- R_645 * TCbright

R_470[R_470 > 1] <- 1
R_555[R_555 > 1] <- 1
R_645[R_645 > 1] <- 1

unscaledR = c(0, 30, 60, 120, 190, 255)/255
scaledR = c(1, 110, 160, 210, 240, 255)/255
scalefun <- approxfun(x = unscaledR, y = scaledR, yleft = 1, yright = 255)
red <- scalefun(R_645)
green <- scalefun(R_555)
blue <- scalefun(R_470)
red[is.na(red)] <- 0
green[is.na(green)] <- 0
blue[is.na(blue)] <- 0
ems_var <- rgb(red, green, blue)
ems_var[ems_var == "#000000"] <- NA
ems_var <- array(as.character(ems_var), dim = dim(R_645))
    
ncdf4::nc_close(nc)
a <- dim(ems_var)[1]
b <- dim(ems_var)[2]
gx <- c(x_grid[1:a, 1:b], x_grid[2:(a + 1), 1:b], x_grid[2:(a + 1), 2:(b + 1)], x_grid[1:a, 2:(b + 1)])
gy <- c(y_grid[1:a, 1:b], y_grid[2:(a + 1), 1:b], y_grid[2:(a + 1), 2:(b + 1)], y_grid[1:a, 2:(b + 1)])
gx <- array(gx, dim = c(a * b, 4))
gy <- array(gy, dim = c(a * b, 4))
gx_ok <- !apply(is.na(gx), 1, any)
gy_ok <- !apply(is.na(gy), 1, any)
n <- c(ems_var)[gx_ok & gy_ok]
gx <- c(t(gx[gx_ok & gy_ok, ]))
gy <- c(t(gy[gx_ok & gy_ok, ]))
 
id <- 1:length(n)
id <- as.factor(id)
values <- data.frame(id = id, value = n)
positions <- data.frame(id = rep(id, each = 4), x = gx, y = gy)

datapoly <- merge(values, positions, by = c("id"))

p <- ggplot2::ggplot() + ggplot2::geom_polygon(ggplot2::aes(x = x, y = y, 
        fill = value, group = id), data = datapoly) + ggplot2::scale_fill_identity()

p
