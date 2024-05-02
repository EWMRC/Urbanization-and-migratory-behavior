install.packages("tidyverse", dependencies = t)
install.packages("sf", dependencies = t)
install.packages("terra", dependencies = t)
install.packages("mapview", dependencies = t)
install.packages("stars", dependencies = t)

library(tidyverse)
library(sf)
library(terra)
library(mapview)
library(stars)


## reading in spatial data
setwd("C:/Users/zoe.pavlik/OneDrive - University of Maine System/Thesis work 2")

stopover_locationsr <- st_read("StopoverDatapoints3Export.shp") %>% #this is where your stopover locations (or polygons) will be read in
  st_transform(5070) #specify the coordinate reference system you want to use. 
# I would advise EPSG 5070 (https://epsg.io/5070-1252), which is a continental projected coordinate system that is measured in meters

# take a moment to look at how R formats the spatial point data. Each row is a standalone point (much like a dataframe).
# all geometry information (coordinates for points, lists of verticies for polygons) is stored in the 'geometry' column 
View(stopover_locationsr)


ImperviousSurface<-rast("nlcd_2021_impervious_l48_20230630.img")

ClipTemplate<- st_read("BCR_RasterTemplate.shp")
ClipTemplate1<-st_transform(ClipTemplate, crs(stopover_locationsr))
rm(ClipTemplate)

ImperviousSurfaceClip <- crop(ImperviousSurface, ClipTemplate1, filename = "intermediate1.tif", overwrite=TRUE)
ImperviousSurfaceClip<-rast("intermediate1.tif")
ImperviousSurface2 <- subst(ImperviousSurfaceClip, from=127, to=0, filename="intermediate2.tif", overwrite=TRUE) #%%
ImperviousSurface3 <-terra::aggregate(ImperviousSurface2, fact=3, fun="mean", filename="Imperviousagg.tif", overwrite=TRUE) #%% #aggregates to a 90m raster
ImperviousSurface3<-rast("Imperviousagg.tif")
ImperviousSurface4 <- project(ImperviousSurface3, "epsg:5070", filename = "ImperviousProjected_aggv2.tif", overwrite=TRUE)#Changing the projection didn't work, addressed below  
# for reloading in the future, use this code
ImperviousSurface4 <- rast("ImperviousProjected_aggv2.tif")


mapview(stopover_locationsr) #mapview allows us to visualize our data over a basemap. This is helpful for ensuring our data projected correctly
mapview(imperviousSurface3)

#Changing the projection on the point layer to match the raster since I couldn't get the projection on the raster to change
stopover_locationsr <- stopover_locationsr %>% 
  st_transform(st_crs(ImperviousSurface3))


# if you've already buffered your points into polygons, skip this step
stopover_locations1859 <- st_buffer(stopover_locationsr, 1859) #this turns our points into polygons, each a circle with a radius of 1859m
mapview(stopover_locations1859)
#Ran this a second time in a second R script using 3900 m for my other buffer radius

# This next part uses a function to calculate the mean value within each polygon
# Functions are beyond the scope of what we've covered in WLE411- if you are interested in understanding how this works, I've included a link to a tutorial below
# https://www.tutorialspoint.com/r/r_functions.htm
stopover_locations1859$mean_imp_cover <- stopover_locations1859$geometry %>% # this function runs separately on each buffered point
  map(.progress = TRUE, .f = function(buffered_point){
    buffered_point_2 <- vect(buffered_point) #converting our buffered location into a format supported by terra
    crs(buffered_point_2) <- crs(stopover_locations1859)
    cropped_surface <- crop(x = ImperviousSurface3, y = buffered_point_2) #match the extent of the raster to that of the buffered point, making the following code more efficient.
    masked_surface <- mask(x = cropped_surface, mask = buffered_point_2) #mask the raster to include only the raster cells within the polygon
    
    masked_surface %>% 
      values() %>% #extract the values from the masked raster
      mean(na.rm = TRUE) #calculate the mean, disregarding NA values
  })

# The above script might take a while- if you need a faster implementation, talk to me and we can see if we can modify this or get you on a better computer

## Examine our results
View(stopover_locations1859) # the mean impervious cover is now stored in the "mean_imp_cover" column


stopover_locations1859 <- stopover_locations1859 %>% 
  mutate(mean_imp_cover = as.numeric(mean_imp_cover)) #ensuring that the data is stored as numeric values, otherwise R won't save the file properly

#plotting high, low, and median points for a figure
mean(stopover_locations1859$mean_imp_cover)
meanpoint1<- subset(stopover_locations1859, mean_imp_cover>4.3)
View(meanpoint1)
meanpoint2<-subset(meanpoint1, mean_imp_cover<4.4)
View(meanpoint2)
mapview(meanpoint2, alpha.regions=0.3, alpha=1, lwd=3, color="white")

max(stopover_locations1859$mean_imp_cover)
highimpcover<- subset(stopover_locations1859, mean_imp_cover>66)
View(highimpcover)
mapview(highimpcover, alpha.regions=0.3, alpha=1, lwd=3, color="white")

min(stopover_locations1859$mean_imp_cover)
lowimpcover<- subset(stopover_locations1859, mean_imp_cover<0.01)
View(lowimpcover)
mapview(lowimpcover, alpha.regions=0.3, alpha=1, lwd=3, color="white")
# Save as a shapefile
st_write(stopover_locations1859, "stopover_locations1859.shp", delete_layer = TRUE)

# or as a csv
#stopover_locations1859 %>% 
  #st_drop_geometry() %>% # this removes the geometry column so that we can save it like we would a normal dataframe
  #write.csv("stopover_locations1859Take2.csv")


