# Clear workspace
# ------------------------------------------------------------------------------
rm(list = ls())

# Load libraries
# ------------------------------------------------------------------------------
library(tidyverse)
library(readxl)

# Define functions
# ------------------------------------------------------------------------------
#file.choose()
source(file = "R/99_project_functions.R")

# Load data
# ------------------------------------------------------------------------------
# GROCERY RETAIL SALES DATASET
#-----------------------------

filename <- "data/grocery_store_data_cleaned.csv"

grocery_sales <- filename %>% 
  read_csv()

# Write data
# ------------------------------------------------------------------------------
write_tsv(x = grocery_sales,
          path = "data/01_my_data.tsv")

