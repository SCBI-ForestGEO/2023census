# Generate reports during census ####
## this script is run automatically when there is a push 


# Clean environment ####
rm(list = ls())

# Set working directory ####
setwd(".")

# Load libraries ####
library(data.table)
library(readxl)

# load data ####
x <- setDT(read_xlsx("example_data/Field_Maps_test_14_mar.xlsx", sheet = 2))

x  
