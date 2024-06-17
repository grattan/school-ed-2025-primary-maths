##''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
##            02. ANALYSIS OF HISTORICAL NAPLAN DATA            ''
##              School Education Program, June 2024             ''
##                          Dan Petrie                          ''
##''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

source("R/00-setup.R")

# 1. Read in NAPLAN data ----
# Note: Dataset is here: https://www.acara.edu.au/reporting/national-report-on-schooling-in-australia/naplan-national-report-archive#dataset.
# Note: Chat GPT might be able to help.
naplan_raw <- read_xlsx("naplan_data.xlsx")

# 2. Filter NAPLAN data for Numeracy results ----

# 3. Convert NAPLAN scale scores to equivalent years of learning ----
# Note: Use ACARA equation on page 128, here: https://www.nap.edu.au/docs/default-source/default-document-library/naplan-2022-technical-report.pdf

# 4. Chart national NAPLAN scale scores for the past decade (2012-22) for each year level ----
# Note: Filter and chart within the one chunk of code
# Note: Try make the y-axis start at EYL1 and show breaks for every two equivalent year levels
# Note: 2020 data is missing, so you may wish to note this on the chart
# Note: Figure 1.1 here could be a guide: https://grattan.edu.au/wp-content/uploads/2024/03/Spreading-success-Why-Australia-should-trial-multi-school-organisations.pdf
# Note: You could adapt lines 77 to 110 in my code here: https://github.com/grattan/school-ed-2024-improving_schools/blob/main/R/03-chronic_underperformers.R

# 5. Create a function to chart the NAPLAN scores in the past decade (2012-22) for each jurisdiction
# Note: This chapter provides an introduction to functional programming. https://r4ds.had.co.nz/functions.html.
# Note: You might find it helpful to look at my code here (https://github.com/grattan/school-ed-2022-teaching-assistants-analysis/blob/main/teacher-assistant-analysis.R) which has a function that charts teaching assistants for each jurisdiction, and here (https://github.com/grattan/school-ed-2023-naplan-analyser/blob/master/NAPLAN_analyser/app.R) which creates an app that spits out the 2023 NAPLAN data for each state and territory.


# 6. Check the trend over time in each jurisdiction.
# Note: Run a linear regression to test if year predicts score. See if the average annual trend is at least 0.25 EYL, using the formulae on page 128 and 129 here. https://www.nap.edu.au/docs/default-source/default-document-library/naplan-2022-technical-report.pdf
# Note: You could incorporate this into the function above, and make the result a callout on the chart.
