##''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
##            02. ANALYSIS OF HISTORICAL NAPLAN DATA            ''
##              School Education Program, June 2024             ''
##                          Dan Petrie                          ''
##''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

source("R/00-setup.R")
library(tidyverse)
library(lubridate)
library(grattantheme)

# 1. Read in NAPLAN data ----
# Note: Dataset is here: https://www.acara.edu.au/reporting/national-report-on-schooling-in-australia/naplan-national-report-archive#dataset.
# Note: Chat GPT might be able to help.

library(readxl)
library(dplyr)

naplan_data <- read_excel("data/naplan_results_2008_to_2022.xlsx", sheet = "Data")

#There were 50 or more warnings. First 50 were all of the form "Expecting logical in AJ2709 / R2709C36: got 'Above'. Or same but with 'close to'. That is, appear to be querying the data class for the my school comparative performance variables.

naplan_data


# 2. Filter NAPLAN data for Numeracy results ----

naplan_numeracy_data <-
  naplan_data %>%
    filter(DOMAIN == "Numeracy")


# 3. Convert NAPLAN scale scores to equivalent years of learning ----
# Note: Use ACARA equation on page 128, here: https://www.nap.edu.au/docs/default-source/default-document-library/naplan-2022-technical-report.pdf


#The formula for the mean NAPLAN score for a particular year level is
# Mean score (Y) = a * ln(x) + b, where for numeracy results slope (a) = 170.861 and intercept (b) = 213.536
#That is -     Y = 170.861 ln(x) + 213.536

#For effective years of learning, formula works in reverse (to go from NAPLAN score to effective years of learning)
# x = e^((Y-b)/a)

#Define constants
slope <- 170.861
intercept <- 213.536

#Create a new variable for the effective years of learning in numeracy, which is calculated by reference to NAPLAN numeracy scores, based on the formula for effective years of learning above.

naplan_numeracy_eyl <-
  naplan_numeracy_data %>%
    mutate(numeracy_eyl = exp((MEAN-intercept)/slope))

head(naplan_numeracy_eyl$numeracy_eyl)

# 4. Chart national NAPLAN scale scores for the past decade (2012-22) for each year level ----
# Note: Filter and chart within the one chunk of code
# Note: Try make the y-axis start at EYL1 and show breaks for every two equivalent year levels
# Note: 2020 data is missing, so you may wish to note this on the chart
# Note: Figure 1.1 here could be a guide: https://grattan.edu.au/wp-content/uploads/2024/03/Spreading-success-Why-Australia-should-trial-multi-school-organisations.pdf
# Note: You could adapt lines 77 to 110 in my code here: https://github.com/grattan/school-ed-2024-improving_schools/blob/main/R/03-chronic_underperformers.R


#Filter the naplan numeracy results for year level and year

library(ggplot2)
library(scales)

names(naplan_numeracy_eyl)

filter(naplan_numeracy_eyl, STATE == "AUS", SUBGROUP == "All") %>%
    group_by(YEAR_LEVEL)



#Chart data with x = year, y = naplan EYL, line colour = year level




# 5. Create a function to chart the NAPLAN scores in the past decade (2012-22) for each jurisdiction
# Note: This chapter provides an introduction to functional programming. https://r4ds.had.co.nz/functions.html.
# Note: You might find it helpful to look at my code here (https://github.com/grattan/school-ed-2022-teaching-assistants-analysis/blob/main/teacher-assistant-analysis.R) which has a function that charts teaching assistants for each jurisdiction, and here (https://github.com/grattan/school-ed-2023-naplan-analyser/blob/master/NAPLAN_analyser/app.R) which creates an app that spits out the 2023 NAPLAN data for each state and territory.


# 6. Check the trend over time in each jurisdiction.
# Note: Run a linear regression to test if year predicts score. See if the average annual trend is at least 0.25 EYL, using the formulae on page 128 and 129 here. https://www.nap.edu.au/docs/default-source/default-document-library/naplan-2022-technical-report.pdf
# Note: You could incorporate this into the function above, and make the result a callout on the chart.
