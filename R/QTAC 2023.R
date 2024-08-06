
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

  #turn table outputs into tibbles
  GM_table_pri_only <- as_tibble(as.data.frame(GM_table_pri_only)) %>%
      rename(result = Var1, GMfreq = Freq)

  MM_table_pri_only <- as_tibble(as.data.frame(MM_table_pri_only)) %>%
    rename(result = Var1, MMfreq = Freq)

  SM_table_pri_only <- as_tibble(as.data.frame(SM_table_pri_only)) %>%
    rename(result = Var1, SMfreq = Freq)

  #add 0 observations for 'NR' result in MM and SM, which are implicit in the data only
  MM_table_pri_only <- MM_table_pri_only %>%
      add_row(result = "NR", MMfreq = 0, .before = 6)

  SM_table_pri_only <- SM_table_pri_only %>%
    add_row(result = "NR", SMfreq = 0, .before = 6)

#Combine tables

combined_table <- GM_table_pri_only %>%
    mutate(MM_table_pri_only$MMfreq, SM_table_pri_only$SMfreq) %>%
       rename(MMfreq = 'MM_table_pri_only$MMfreq', SMfreq = 'SM_table_pri_only$SMfreq')

#rearrange order and add a totals row
combined_table <- combined_table %>%
  slice(c(2:5, 1, 6:7)) %>%
    bind_rows(combined_table %>%
      summarise(across(where(is.numeric), ~sum(.x, na.rm = TRUE)) %>%
        mutate(result = "total")))

#save labels as a vector
labels <- combined_table %>%
  select(result) %>%

#remove labels and transpose the table

flip_table <- combined_table %>%
  select(-result) %>%
    t()

colnames(flip_table) <- t(labels)

#reclassify as tibble
flip_table <- as_tibble(as.data.frame(flip_table))

#add participation
flip_table <- flip_table %>%
  mutate(partn = rowSums(select(., A, B, C, D, '-', NR), na.rm = TRUE), partn_rate = partn/total*100)

#calculate middle/median score
flip_table %>%
    mutate(typicalresult = if_else(
      partn/2 <= A, "A", if_else(
        partn/2 <= A+B, "B", if_else(
          partn/2 <= A+B+C, "C", if_else(
            partn/2 <= A+D+B+D,"D","TBC")))))
