# Clear workspace
# ------------------------------------------------------------------------------
rm(list = ls())

# Load libraries
# ------------------------------------------------------------------------------
library(tidyverse)
library(readxl)
library(lubridate)

# Define functions
# ------------------------------------------------------------------------------
source(file = "R/99_project_functions.R")

# Load data
# ------------------------------------------------------------------------------
my_data_clean <- read_tsv(file = "data/02_my_data_clean.tsv")

# Wrangle data
# ------------------------------------------------------------------------------
# Task 1: Trends with respect to transactions
#--------------------------------------------
transactions_data <- my_data_clean %>% 
  # remove free rows to focus on transactional beahvaiour
  filter(PAYMENT != "free") %>% 
  # remove cashrefund rows to focus on transactional beahviour, i.e. focus on cash and magcard
  filter(PAYMENT != "cashrefund") %>% 
  # remove unnecessary columns
  select(-X1, -PAYMENT, -TRANSID, -DATE) %>% 
  # group by transaction based on ticket
  group_by(TICKET) %>% 
  # add number of item for processing later
  mutate(NUM_ITEM = 1) %>% 
  # add number of categories, number of total items and aggregate price per transaction
  summarise(NUM_CATEGORY = n_distinct(CATEGORY),
            NUM_ITEM = sum(NUM_ITEM),
            TOTAL_PRICEBUY = sum(TOTAL_PRICEBUY),
            TOTAL_PRICESELL = sum(TOTAL_PRICESELL), 
            UNIT_PRICE_MARGIN = sum(UNIT_PRICE_MARGIN),
            PROFIT = sum(PROFIT)) %>% 
  ungroup()

#------------------------------------
# Task 2: Trends with respect to time
#------------------------------------
time_data <- my_data_clean %>% 
  # remove free rows to focus on transactional beahvaiour
  filter(PAYMENT != "free") %>% 
  # remove cashrefund rows to focus on transactional beahviour, i.e. focus on cash and magcard
  filter(PAYMENT != "cashrefund") %>% 
  # remove unnecessary columns
  select(-X1, -TRANSID) %>% 
  # change date format
  mutate(DATE = date(DATE)) %>% 
  # add number of item for processing later
  mutate(NUM_ITEM = 1) %>% 
  group_by(DATE) %>% 
  # add new parameters
  mutate(NUM_ITEM = sum(NUM_ITEM),
         NUM_CATEGORY = n_distinct(CATEGORY), 
         TOTAL_PRICEBUY = sum(TOTAL_PRICEBUY),
         TOTAL_PRICESELL = sum(TOTAL_PRICESELL), 
         UNIT_PRICE_MARGIN = sum(UNIT_PRICE_MARGIN),
         PROFIT = sum(PROFIT), 
         AVG_SALES = TOTAL_PRICESELL/n_distinct(TICKET), 
         TOTAL_VOLUME = n_distinct(TICKET)) %>% 
  # select unique date per row
  distinct(DATE, .keep_all = T) %>% 
  ungroup() %>% 
  # remove remaining unnecessary columns
  select(-TICKET, -UNITS, -REFERENCE, -CODE, -NAME, -UNIT_PRICEBUY, -UNIT_PRICESELL, -CATEGORY)

#------------------------------------------------
# Task 3: Trends with respect to categories
#------------------------------------------------
cat_data <- my_data_clean %>% 
  # remove free rows to focus on transactional beahvaiour
  filter(PAYMENT != "free") %>% 
  # remove cashrefund rows to focus on transactional beahviour, i.e. focus on cash and magcard
  filter(PAYMENT != "cashrefund") %>% 
  # remove unnecessary columns
  select(-X1, -TRANSID) %>% 
  # change date format
  mutate(MONTH = month(DATE)) %>% 
  # add number of item for processing later
  mutate(NUM_ITEM = 1) %>% 
  group_by(CATEGORY) %>% 
  # add new parameters
  mutate(NUM_ITEM = sum(NUM_ITEM), 
         TOTAL_PRICEBUY = sum(TOTAL_PRICEBUY),
         TOTAL_PRICESELL = sum(TOTAL_PRICESELL), 
         UNIT_PRICE_MARGIN = sum(UNIT_PRICE_MARGIN),
         PROFIT = sum(PROFIT)) %>% 
  # add column with highest category by month
  mutate(PEAK_MONTH =  names(which.max(table(MONTH)))) %>% 
  # select unique date per row
  distinct(CATEGORY, .keep_all = T) %>% 
  ungroup() %>% 
  # remove remaining unnecessary columns
  select(-TICKET, -UNITS, -REFERENCE, -CODE, -NAME, -UNIT_PRICEBUY, -UNIT_PRICESELL, -DATE, -PAYMENT, -MONTH)

# Write data
# ------------------------------------------------------------------------------
write_tsv(x = transactions_data,
          path = "data/03_transactions_data.tsv")

write_tsv(x = time_data,
          path = "data/04_time_data.tsv")

write_tsv(x = transactions_data,
          path = "data/05_cat_data.tsv")
