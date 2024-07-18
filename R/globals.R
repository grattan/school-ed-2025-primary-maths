# Sets up libraries and common R objects to be used across Quarto documents

# Libraries
library(tidyverse)
library(grattantheme)
library(qualtRics)
library(knitr)
library(knitr)
library(kableExtra)
library(sjlabelled)
library(readxl)
library(janitor)
library(scales)
library(strayr)
library(glue)

# Additional configuration
options(knitr.kable.NA = '')

source_caption <- "Source: Grattan Institute's 2024 survey on primary mathematics."
