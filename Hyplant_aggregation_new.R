# Loading required libraries
library(raster)
library(tidyverse)
library(parallel)
library(sf)

## Starting Cluster
threads <- 3
beginCluster(n = threads)
rm(threads)

## Loading in required files
# SIF Raster - Is this file a final SIF-Raster?! It still has multiple bands.
SIF_600_1115_A <- brick("Erfassung_Phänotypen2021/HyPlant data/SIF/20180629-CKA-1115-600-L3-N-FLUO_radiance_SIFO2A_rectified.bil")
# Plots & Attributes
plots <- st_read("Erfassung_Phänotypen2021/Shapefiles/Barley_1115_buffered.shp")
# Assign ID
plots <- mutate(plots, ID = seq(1:nrow(plots)), .keep = "all")

## Extract Values from SIF_Map within the shape file
raster_raw <- raster::extract(SIF_600_1115_A, plots, cellnumber = TRUE, df = TRUE, weights = TRUE, normalizeWeights = FALSE)
# Filter for weights == 1 (cells fully within)
raster <- filter(raster_raw, weight == 1)

## Join raster data and plot data
# Maybe use left join (or full join?) to get empty rows for plot ids that didn't got any cells
raster <- left_join(raster, plots, by = "ID")

## Count of captured cells per plot
rename(data.frame(table(raster$PLOTID)), PlotID = Var1)
print(paste0("Für folgende ID wurde kein Pixel extrahiert: ", setdiff(unique(raster$ID), seq(1:nrow(plots)))))
# Question: How are smaller pixels (caused by rectification) are handled? Do they count on there own? Or are they part of another pixel?
# Or does the Picture has unequal pixels in it? Can we extract pixel size?

## Aggregate Cell Data
bands_mean <- summarise(group_by(raster, PLOTID), across(starts_with("Georef"), mean))
raster_mean <- left_join(bands_mean, plots, by = "PLOTID")

## Closing cluster
endCluster()
