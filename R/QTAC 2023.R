
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
library(readxl)

# common objects ---------------------------------------------------------------

  #Change working directory (currently set to Documents file in my OneDrive)
  getwd()
  setwd("C:/Users/PETRIE/OneDrive - The University of Melbourne/Documents/GitHub/school-ed-2025-primary-maths/")
  getwd()

  #Define the data file with relative address, then read in and make it a tibble
  datafile2023 <- "data/QTAC-2023-admissions_RPS-8566.xlsx"
  data2023 <- read_xlsx(datafile2023)

# helper functions =============================================================

