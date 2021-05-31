# Loading required librarys
library(raster)
library(tidyverse)
library(parallel)
library(sf)

## Starting Cluster
threads <- 3
beginCluster(n = threads)
rm(threads)

## Loading in required files
# SIF Raster
SIF_600_1115_A <- brick("Erfassung_Phänotypen2021/HyPlant data/SIF/20180629-CKA-1115-600-L3-N-FLUO_radiance_SIFO2A_rectified.bil")
# Plots & Attributes
plots <- st_read("Erfassung_Phänotypen2021/Shapefiles/Barley_1115_buffered.shp")

## Extract Values from SIF_Map within the shapefile
raster <- raster::extract(SIF_600_1115_A, plots, cellnumber = TRUE, df = TRUE, weights = TRUE, normalizeWeights = FALSE)
# Filter for weights == 1 (cells fully within)
raster <- filter(raster, weight == 1)

## Join raster data and plot data
# Maybe use left join (or full join?) to get empty rows for plotids that didn't got any cells
raster <- inner_join(raster, mutate(plots, ID = seq(1:nrow(plots)), .keep = "all"))

## Count of captured cells per plot
print(rename(data.frame(table(raster$PLOTID)), PlotID = Var1), row.names = FALSE)

## Aggregate Cell Data
test <- summarise(group_by(select(raster, starts_with(c("Georef", "PLOTID"))), PLOTID), across(starts_with("Georef"), mean = mean))


## Closing cluster
endCluster()
