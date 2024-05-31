
# common functions and objects for the <PROJECT NAME> project

# Standard set up ==============================================================

# packages ---------------------------------------------------------------------

# broad ----------
library(tidyverse)
library(lubridate)
library(scales)
library(readxl)
library(purrr)
library(glue)
library(janitor)
library(fst)
library(readabs)
library(strayr)       # remotes::install_github("runapp-aus/strayr")
library(grattantheme) # remotes::install_github("grattan/grattantheme")

# specific ------
library(spatstat)   # for weighted quantiles
library(ggtext)     # for chart labelling
library(patchwork)  # for arranging multiple plots
library(ggrepel)    # for neater on-chart labelling
library(zoo)        # for rolling calculations (eg rollmean)
library(fst)        # for .fst file types
library(kableExtra) # for exporting to LaTeX tables


# common objects ---------------------------------------------------------------

census_caption_base <- glue(.sep = " ",
  "Source: ABS (Census, 2016)"
)



# helper functions =============================================================

`%nin%` <- Negate(`%in%`)

# Drop total rows to avoid double counting
drop_totals <- function(.data) {
  .data %>%
    filter(across(.fns = ~ . != "Total"))
}

# Re-group ages
regroup_age <- function(x, factor = TRUE) {

  x <- case_when(
    x == "20-24" ~ "20-34",
    x == "25-34" ~ "20-34",
    x == "55-64" ~ "Over 55",
    x == "Over 65" ~ "Over 55",
    # the rest should stay the same:
    TRUE ~ x
  )

  if (factor) x <- fct_inorder(x)

  return(x)

}
