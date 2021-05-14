#-----------------------------------------------------------
#Generating individual spectral signatures
#------------------------------------------------------------
#Packages:
library(raster)
library(robustbase)
library(readxl)
library(foreign)
library(tidyverse)
library(RColorBrewer)
library(rgdal)
library(DescTools)
library(sp)
library(sf)
library(parallel)

# reset global Enviroment
# rm(list=ls())
# opening the cluster
threads <- 11
beginCluster(n = threads)

#------------------------------------------------------------

# 1. Data preparation
#------------------------------------------------------------

SIFA <- brick("Erfassung_Phänotypen2021/HyPlant data/SIF/20180629-CKA-1115-600-L3-N-FLUO_radiance_SIFO2A_rectified.bil")
#SIFB <- brick("Erfassung_Phänotypen2021/HyPlant data/SIF/20180629-CKA-1242-350-L2-E-FLUO_radiance_SIFO2B_rectified.bil") # Das hier ist eine andere Höhe
#Raster <- brick(".../20180629-CKA-1242-350-L2-E-FLUO_radiance_deconv_i1-rect.dat")  #ich konnte die .dat Datei leider nirgends finden.
Raster <- brick("Erfassung_Phänotypen2021/HyPlant data/20180629-CKA-1115-600-L3-N-FLUO_radiance_deconv_i1-subset.bil")
Plots <- sf::st_read("Erfassung_Phänotypen2021/Shapefiles/Barley_1115_buffered.shp") # read in shapefile indicating relevant polygons (plot boundaries) 11:15 used
Attributes <- read.dbf("Erfassung_Phänotypen2021/Shapefiles/Barley_1115_buffered.dbf") 

SIFA <- SIFA[[11]]
#SIFB <- SIFB[[4]]


#---------------------------------------------------------------------
# sorting data
Plots_sorted <- Plots[order(Plots$PLOTID),] # sort shapefile for PlotID for later ID assignment

# extracting data from a raster file
DATA_original <- raster::extract(Raster, Plots_sorted, method='simple', na.rm=TRUE, small=TRUE, df=TRUE, 
                        cellnumbers = T, weights = T)
DATA <- raster::extract(Raster, Plots_sorted, method='simple', na.rm=TRUE, small=TRUE, df=TRUE, 
                        cellnumbers = TRUE, weights = TRUE, buffer = 1) # extraction of spectrum and cellnumbers


#from spectral subset based on plot boundaries (shapefile) - only pixels having their centroids inside polygons
DATA <- DATA[,1:1026]
Data <- as.matrix(sapply(DATA, as.numeric)) 


#------------------------------------------------------------

# 2. Outlier detection
#------------------------------------------------------------

Testvalues <- matrix(ncol= NCOL(DATA)-2, nrow= NROW(DATA))
# creates an empty matrix with rowcount and columncount of "DATA"


for (i in 1:length(unique(DATA$ID))) { 
  teilmatrix <- Data[Data[,1]==i,c(-(1:2))]   # first column with plotIDs not accounted for
  
  Testvalues[Data[,1]==i] <- sweep ((abs (sweep (teilmatrix, 2, colMedians(teilmatrix), "-"))), 2, 
                                    (apply (teilmatrix, 2, MeanAD, median)), "/" )
  }




Testvalues[is.na(Testvalues)] <- 0 # for all plots with bands on which all pixel values are equal

Mean_Testvalues <- rowMeans(Testvalues)

# loop which generates test values for each pixel indicating the outlier potential:
# (                                  abs(xi - median)
#     seperately for each band:     -----------------  (averaged for each pixel i)    = Testvalues
#                                     mean absolute 
#                                 deviation from median                                                )

# Alternative expression for MeanAD: sum(abs(s-median(s)))/length(s) 

Mean_Testvalues[Mean_Testvalues>1.5]  <- NA    # all Testvalues that are larger than 1.7 are set to NA

#------------------------------------------------------------

# 3. Create outlier-removed point shapefile
#------------------------------------------------------------


Coordinates <- cbind(DATA[,1:2],coordinates(Rastersmall)[DATA[,2],])    # extract coordinates
DATA_2 <- cbind(Coordinates,Mean_Testvalues)                       
Outliers <- DATA_2[apply(DATA_2, 1, function(x) any(is.na(x))),]   # subset outlier points
OutliersSHP <- st_as_sf (Outliers, coords = c("x","y"), crs = 32632)  # create point shapefile

#-----------------------------------------------------------
# Wheighted Mean and SD

#---------------------------------------------

#SIFA
Testpoints <- raster::extract(SIFA, OutliersSHP, method='simple', na.rm=TRUE, small=TRUE, df=TRUE, 
                              cellnumbers= TRUE)
Cells <- Testpoints[,2]
SIFo2A <- SIFA[[11]]
SIFo2A[Cells] <- NA
writeRaster(SIFo2A,"D:/Improvements_Dez2019/HyPlant_Data/Final SIF Values/FirstApproach(Mask)/MaskedRaster_OutlierWeighted_SIFo2A_1115.bil")
WheightedMeanso2A <- raster::extract(SIFo2A, Plots_sorted, method='simple', fun= mean, na.rm=TRUE, small=TRUE, df=TRUE, 
                                     weights = TRUE, normalizeWeights = TRUE) # extraction of pixel values 
SDo2A <- raster::extract(SIFo2A, Plots_sorted, method='simple', fun= sd, na.rm=TRUE, small=TRUE, df=TRUE) # extraction of pixel values 

#SIFB
Testpoints <- raster::extract(SIFB, OutliersSHP, method='simple', na.rm=TRUE, small=TRUE, df=TRUE, 
                              cellnumbers= TRUE)
Cells <- Testpoints[,2]
SIFo2B <- SIFB[[4]]
SIFo2B[Cells] <- NA
WheightedMeanso2B <- raster::extract(SIFo2B, Plots_sorted, method='simple', fun= mean, na.rm=TRUE, small=TRUE, df=TRUE, 
                                     weights = TRUE, normalizeWeights = TRUE) # extraction of pixel values 
SDo2B <- raster::extract(SIFo2B, Plots_sorted, method='simple', fun= sd, na.rm=TRUE, small=TRUE, df=TRUE) # extraction of pixel values 
Final <- cbind(WheightedMeanso2A, SDo2A[2], WheightedMeanso2B[2],SDo2B[2])
Attributes_sorted <- Attributes[order(Attributes$PLOTID),] #sort dbf file for PLOT ID
colnames(Attributes_sorted)[3] <- "Plot ID"  #rename first column
Final <- cbind(Final, Attributes_sorted[,3:8])


write.xlsx(Final,"....xlsx") # save file

#----------------------------------------------------------

# closing cluster
endCluster()
