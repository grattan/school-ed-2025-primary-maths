
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

# common objects ---------------------------------------------------------------

  #Define the data file with relative address, then read in and make it a tibble
  datafile2023 <- "data/QTAC-2023-admissions_RPS-8566.xlsx"
  data2023 <- read.csv(datafile2023)

# helper functions =============================================================
