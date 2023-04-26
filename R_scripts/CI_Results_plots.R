# generate results plots ####
## this script is run automatically when there is a push 

# clear environment ####
rm(list = ls())

# load libraries ####
library(here)
library(data.table)
library(dplyr)
library(rgdal)
library(curl)

# load data ####

## new cleaned census data (Generted by R_script/CI_QAQC_repots.R)
currStem <- fread("processed_data/scbi.stem4.csv")

## previous census data (reading from Github directly)
prevStem <- fread(paste0("https://raw.githubusercontent.com/SCBI-ForestGEO/SCBI-ForestGEO-Data/master/tree_main_census/data/census-csv-files/scbi.stem3.csv"))

## quadrat map
quadrats <- rgdal::readOGR(file.path(here(""),"doc/maps/20m_grid/20m_grid.shp"))

plot(quadrats)

# generate results ####
## We want a 4x3 composite of (12) plot maps arranged in columns of n stems, biomass, and n species, and rows of current total, gain, loss, and net change (the latter 3 relative to previous census). 
## We will go through each variable and calculate the value for each and merge the value into the map/s data: quadrats@data

# stems ####

## current total (n stem)

StemTotal <- table(currStem$quadrat[currStem$status_current %in% "LI"])

## gain (recruitment)
newStems_idx <- !paste(currStem$tag, currStem$StemTag) %in% paste(prevStem$tag, prevStem$StemTag) & currStem$status_current %in% "LI" # TRUE if the stem did not exist in census 3 (and is alive in census 4 - probably unnecessary), FALSE otherwise

View(currStem[newStems_idx,]) # these are new stems

StemGain <- table(currStem[newStems_idx,]$quadrat)

## loss (mortality)
unique(prevStem$status)

deadStems_idx <- paste(currStem$tag, currStem$StemTag) %in% paste(prevStem$tag, prevStem$StemTag)[prevStem$status %in% "A"] & !currStem$status_current %in% "LI" # says TRUE stem existed and was alive in census 3, and is now dead, FALSE otherwise

StemLoss <- table(currStem[deadStems_idx,]$quadrat)


## net change (∆ n stem)
StemNetDelta <- StemTotal -  table(prevStem$quadrat[prevStem$status %in% "A"])[names(StemTotal)]



# biomass ####

## current total

## gain (woody productivity)

## loss (woody mortality)

## net change (∆ biomass)

# species richness ####

## current total (n species)

## gain (n species gained)

## loss (n species lost)

## net change (∆ species richness)

