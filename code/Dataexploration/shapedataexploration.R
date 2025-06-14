#Modules 
library(tidyverse)  # For data manipulation and visualization
library(sf)         # For spatial data handling
library(tidyverse)
library(terra)
library(raster)
library(sf)
library(sp)
library(dismo)
library(sf)        # core vector GIS package
library(units)     # used for precise unit conversion
library(geodata)   # Download and load functions for core datasets
library(openxlsx)  # Reading data from Excel files
library(rnaturalearth)
library(rnaturalearthdata)
library(ggplot2)
library(ncdf4)
library(viridis)
library(viridisLite)
library(chron)
library(RColorBrewer)
library(lattice)
##############################

wheat_raster <- rast("../../sandbox/SHAPEFILES/CROPGRID/22491997/CROPGRIDSv1.08_NC_maps/CROPGRIDSv1.08_wheat.nc")


# Download a 1:100m scale world map
world_map <- ne_countries(scale = "small", returnclass = "sf")

#names 
names(wheat_raster$var)
names(wheat_raster$dim)

# Print raster information
print(wheat_raster)

# Download world map
world_map <- ne_countries(scale = "small", returnclass = "sf")

# Convert to data frame for plotting
wheat_yield <- as.data.frame(wheat_raster, xy = TRUE, na.rm = TRUE)

# Rename the value column to something meaningful
# Note: column name may vary depending on the variable name in your NetCDF
colnames(wheat_yield)[3] <- "wheatyield_density"

# Threshold for top 5%
threshold <- quantile(wheat_yield$wheatyield_density, 0.95)

# Create a binary column for top 5% and others
wheat_yield$top5 <- ifelse(wheat_yield$wheatyield_density >= threshold, 1, 0)

# Check the result
head(wheat_raster)
head(wheat_yield)

# Generate top 5 percent map
map1 <- ggplot() +
  # Add the blue ocean background
  annotate("rect", xmin = -180, xmax = 180, ymin = -90, ymax = 90, 
           fill = "lightblue", alpha = 0.3) +
  
  # Add the base world map
  geom_sf(data = world_map, fill = "#D1FFBD", alpha = 0.6, color = "black", linewidth = 0.2) +
  
  # Plot normal wheat
  geom_tile(data = wheat_yield, aes(x = x, y = y), fill = "green", alpha = 0.6) +
  
  # # Plot top 5% tiles only
  # geom_tile(data = wheat_yield %>% filter(top5 == 1),
  #           aes(x = x, y = y), fill = "darkgreen", alpha = 1) +
  
  # Map settings
  ggtitle("Top 5% wheat harvested globally") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),   # Remove gridlines
    legend.position = "right",      # Legend position
    legend.key.size = unit(1, "cm")  # Legend size 
  )

map1
ggsave("../../output/wheatmap.pdf", plot = map1)



#map loading for ages and generating some goofy bs (see if you can reduce raster resolution)