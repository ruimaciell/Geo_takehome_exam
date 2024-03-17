rm(list = ls(all.names = TRUE))


#clean the environmnet
library(sf)         # For handling spatial data
library(ggplot2)    # For plotting
library(raster)     # For creating raster data
library(terra)      # For spatial data manipulation, similar to raster but more recent
library(dplyr)      # For data manipulation



# Set the working directory to the location of the Geospatial data
setwd("/Users/ruimaciel/Desktop/Barcelona/Geospatial/Geo_takehome_exam/")

# Load the data
districts_data <- read.csv('BarcelonaCiutat_Districtes.csv')
accidents_data <- read.csv('2023_accidents_gu_bcn (1).csv')

# Assuming your districts_data has a WKT (Well-Known Text) column for geometries
# and accidents_data has coordinates for accidents.
# Convert districts and accidents data to sf objects
districts_sf <- st_as_sf(districts_data, wkt = "geometria_wgs84", crs = 4326)
accidents_sf <- st_as_sf(accidents_data, coords = c("Longitud_WGS84", "Latitud_WGS84"), crs = 4326)


# Plot the data using ggplot2
ggplot() +
  geom_sf(data = districts_sf, fill = 'lightblue', color = 'black') +  # Plot districts
  geom_sf(data = accidents_sf, color = 'red', size = 0.0001, alpha = 0.5) + # Plot accidents
  theme_minimal() +
  labs(title = "Barcelona Districts and Accidents Map",
       caption = "Districts in light blue, Accidents in red")

# Assuming your accidents_sf object is already created and is an sf object


# Define the extent based on the accidents data
ext <- st_bbox(accidents_sf)
r_extent <- raster::extent(ext$xmin, ext$xmax, ext$ymin, ext$ymax)

# Define resolution
res <- 0.002 # Adjust resolution based on your dataset

# Calculate the number of rows and columns based on the resolution
num_rows <- ceiling((ext$ymax - ext$ymin) / res)
num_cols <- ceiling((ext$xmax - ext$xmin) / res)

# Create an empty raster with defined extent and resolution
r <- raster::raster(nrows=num_rows, ncols=num_cols, xmn=ext$xmin, xmx=ext$xmax, ymn=ext$ymin, ymx=ext$ymax)

# Set the resolution of the raster
res(r) <- c(res, res)

# Rasterize the accidents data
accidents_raster <- rasterize(x=accidents_sf, y=r, field=1, fun='count', background=0)

# Convert the raster to a dataframe
accidents_df <- as.data.frame(accidents_raster, xy=TRUE)

# Replace NA with 0 if any NAs are present
if(any(is.na(accidents_df$value))) {
  accidents_df$value[is.na(accidents_df$value)] <- 0
}

# Proceed to plot
# Proceed to plot the heatmap
ggplot() +
  geom_tile(data = accidents_df, aes(x = x, y = y, fill = layer)) +  # Heatmap of accidents
  geom_sf(data = districts_sf, fill = NA, color = "black", size = 0.2) +  # Boundaries of districts
  scale_fill_gradient(low = "white", high = "red", name = "Accident Count") +
  coord_sf(datum = NA) +  # Use coord_sf to align the plot with the sf data
  labs(title = "Heatmap of Accidents in Barcelona with District Boundaries",
       x = "Longitude", y = "Latitude") +
  theme_minimal()

head(accidents_df)
