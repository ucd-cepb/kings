library(sf)
library(dplyr)
library(readr)
library(terra)

shapefile_fp <- "/home/aaron/Documents/WORK/CEPB/data/justice40/usa/usa.shp"

census_tracts <- read_sf(shapefile_fp)

ca_dacs <- census_tracts %>% filter(SF == 'California', SN_C == 1)
ca_dacs <- vect(ca_dacs)
centroids <- centroids(ca_dacs)
centroids$x <- crds(centroids)[,1]
centroids$y <- crds(centroids)[,2]
