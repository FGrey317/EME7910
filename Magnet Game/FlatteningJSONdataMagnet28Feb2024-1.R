# Title:    Importing data from a spreadsheet
# File:     03_03_ImportSpreadsheet.R
# Project:  R_EssT_1; R Essential Training, Part 1:
#           Wrangling and Visualizing Data

# INSTALL AND LOAD PACKAGES ################################

# Install pacman ("package manager") if needed
if (!require("pacman")) install.packages("pacman")

# Load contributed packages with pacman
pacman::p_load(pacman, party, rio, tidyverse)
# pacman: for loading/unloading packages
# party: for decision trees
# rio: for importing data
# tidyverse: for so many reasons

# LOAD AND PREPARE DATA ####################################


# tidyverse already loaded
# install.packages("tidyverse")


# How To Flatten JSON commands
# Here is what JSON is: https://www.w3schools.com/whatis/whatis_json.asp

# In your Excel spreadsheet contains multiple columns, including a key column 
#named event_data, which appears to hold JSON-like string representations of 
# nested name-value pairs. 
# To achieve the result, you are looking for in R, particularly with transforming and 
# flattening nested JSON-like strings in a column into separate variables, 
# you can use a combination of packages like jsonlite for parsing JSON and dplyr 
# for data manipulation. 
# Here's a step-by-step guide on how you might approach this task in R:
# library(readxl)
# Replace the path with the actual path to your Excel file
# file_path <- "data/MAGNET_100rows_eventsDoubleQuotes.xlsx"
# file_path <- "data/MAGNET_100rows_events.xlsx"
# data <- read_excel(file_path)

# With Magnet data set combine import with data preparation
dfMagnet <- import("data/MAGNET_100rows_eventsDoubleQuotes.xlsx") %>%
  as_tibble() %>%
  select(timestamp,event_name,event_data) %>% 
  print()

# You could maybe use R to replace the single quotes with double quotes
# mutate(event_data = gsub("'", '"', event_data))
# and replace the "True" with "double quotes"true""
# mutate(event_data = gsub("True", "true", event_data))

# Use the jsonlite package to parse the JSON from each row of the 
# event_datacolumn. Because some of these JSON objects might be nested, 
# you'll flatten them into a tabular format.

library(jsonlite)
library(dplyr)

# Define a Function, safe_from_json, to safely parse JSON and catch any errors
# 1.Modify the safe_from_json function to log errors or indicate which rows fail to 
# parse.This adjustment can help diagnose parsing issues. 
# Modified safe_from_json function with error logging

safe_from_json <- function(json_str, row_number) {
  tryCatch({
    as.data.frame(fromJSON(json_str, flatten = TRUE))
  }, error = function(e) {
    message(paste("Error in row", row_number, ":", e$message))
    NULL
  })
}

# When calling this function, you'll now need to pass the row number as well for logging purposes.
# 
# 2 Inspect Before Flattening
# 
# After applying safe_from_json but before unnesting, inspect the event_data_parsed column.
#  Apply the modified safe_from_json function and inspect
#  

dfMagnet <- dfMagnet %>%
  mutate(event_data_parsed = map2(event_data, row_number(), safe_from_json)) %>%
  select(-event_data) 

  dfMagnet %>% print()
# Optionally remove the original event_data column
 
# Inspect the structure of the event_data_parsed column
str(dfMagnet$event_data_parsed)

# 3. Modify Script to Handle Empty Returns

# Add steps to filter out rows where safe_from_json returns NULL or manage them according to your needs.
# Filter out rows with NULL returns after parsing JSON

dfMagnet <- dfMagnet %>%
  filter(!map_lgl(event_data_parsed, is.null))
 
# Alternatively, you can replace NULLs with a default value or structure
# dfMagnet <- dfMagnet %>%
#   mutate(event_data_parsed = map(event_data_parsed, ~if(is.null(.x)) tibble() else .x))
 
# Proceed with unnesting after handling NULLs
flattened_data <- dfMagnet %>%
  unnest(event_data_parsed)


flattened_data %>% print()

# flattened_data %>% view()

# Write it out as a .csv file

# To do date, you need the lubricate file.
library(lubridate)

path_file <- str_c("data/flattened_MAGNET_data", now(),".csv")
(path_file)

write_csv(flattened_data, path_file)

# simple name write_csv(flattened_data, "data/flattened_MAGNET_data.csv")

# CLEAN UP #################################################

# Clear environment
rm(list = ls()) 

# Clear packages
p_unload(all)  # Remove all add-ons

# Clear plots
graphics.off()  # Clears plots, closes all graphics devices

# Clear console
cat("\014")  # ctrl+L

# Clear mind :)
