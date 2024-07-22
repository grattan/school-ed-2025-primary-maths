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
library(ggdirectlabel)
library(mice)
library(survey)
library(srvyr)
library(rmarkdown)

# Additional configuration
options(knitr.kable.NA = '')

source_caption <- "Source: Grattan Institute's 2024 survey on primary mathematics."

# Useful functions
# Creates an ordered factor
fct_case_when <- function(...) {
args <- as.list(match.call())
levels <- sapply(args[-1], function(f) f[[3]])  # extract RHS of formula
levels <- levels[!is.na(levels)]
factor(dplyr::case_when(...), levels=levels)
}
