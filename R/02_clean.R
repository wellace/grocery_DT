# Clear workspace
# ------------------------------------------------------------------------------
rm(list = ls())

# Load libraries
# ------------------------------------------------------------------------------
library(tidyverse)

# Define functions
# ------------------------------------------------------------------------------
source(file = "R/99_project_functions.R")

# Load data
# ------------------------------------------------------------------------------
my_data <- read_tsv(file = "data/01_my_data.tsv")

# Wrangle data
# ------------------------------------------------------------------------------
my_data_clean <- my_data %>% 
  # update free items to losses based on TOTAL_PRICEBUY
  mutate(PROFIT = ifelse(PAYMENT == "free", TOTAL_PRICEBUY * -1, PROFIT)) %>% 
  # update free homegrown (farm) items to losses based on TOTAL_PRICESELL
  mutate(PROFIT = ifelse(PAYMENT == "free" & TOTAL_PRICEBUY == 0, TOTAL_PRICESELL * -1, PROFIT)) %>% 
  # update TOTAL_PRICESELL to zero since no sales made
  mutate(TOTAL_PRICESELL = ifelse(PAYMENT == "free", 0, TOTAL_PRICESELL))


# Write data
# ------------------------------------------------------------------------------
write_tsv(x = my_data_clean,
          path = "data/02_my_data_clean.tsv")

