
#Check sourcing function. Seems unable to read "R/00-setup.R" including when local address used
source("R/00-setup.R")

####Copied standard set up from "R/00-setup.R" due to source() function not working

# common functions and objects for the project

# Standard set up ==============================================================

# packages ---------------------------------------------------------------------

# broad ----------
library(tidyverse)
library(lubridate)
library(scales)
library(readxl)
library(purrr)
library(glue)
library(grattantheme) # remotes::install_github("grattan/grattantheme")

# specific ------
library(ggtext)     # for chart labelling
library(tidyr)
library(dplyr)

# common objects ---------------------------------------------------------------

  #Change working directory (currently set to Documents file in my OneDrive)
  getwd()
  setwd("C:/Users/PETRIE/OneDrive - The University of Melbourne/Documents/GitHub/school-ed-2025-primary-maths/")
  getwd()

  #Define the data file with relative address, then read in and make it a tibble
  datafile2023 <- "data/QTAC-2023-admissions_RPS-8566.xlsx"
  data2023 <- read_xlsx(datafile2023)

# helper functions =============================================================

  #Turn data into a tibble
  data2023 <- as_tibble(data2023)

  #See the values and their frequencies for key variables in the data set

  DetailedFOEDesc_table <- table(data2023$DetailedFOEDesc)
  print(DetailedFOEDesc_table)

  MM_table <- table(data2023$`MATHEMATICAL METHODS`)
  print(MM_table)

  GM_table <- table(data2023$`GENERAL MATHEMATICS`)
  print(GM_table)

  SM_table <- table(data2023$`SPECIALIST MATHEMATICS`)
  print(SM_table)

  #For now/ test purposes, only count teachers classified as studying primary teaching as the class of teachers we care about.
  primary_only <- filter(data2023, DetailedFOEDesc == "Teacher Education: Primary")

  GM_table_pri_only <- table(primary_only$`GENERAL MATHEMATICS`)
  MM_table_pri_only <- table(primary_only$`MATHEMATICAL METHODS`)
  SM_table_pri_only <- table(primary_only$`SPECIALIST MATHEMATICS`)
  print(GM_table_pri_only)
  print(MM_table_pri_only)
  print(SM_table_pri_only)

  null_NR <- c("NR", 0) #This is a row not a column, might not be useful to plug missing values into in the MM and SP tables. Rethink this

