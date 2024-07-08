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


library(ggplot2)
library(scales)
library(glue)

names(naplan_numeracy_eyl)

#Create data sets for chart line labels

label_lines <- filter(naplan_numeracy_eyl, STATE == "AUS", SUBGROUP == "All", CALENDAR_YEAR == 2022) %>% #Filter out data on sub groups by demographic characteristics and states, and then keep only 2022 data - 1 dot per line, last on RHS
    mutate(chart_label = case_when(
      YEAR_LEVEL == 3 ~ "Year 3",
      YEAR_LEVEL == 5 ~ "Year 5",
      YEAR_LEVEL == 7 ~ "Year 7",
      YEAR_LEVEL == 9 ~ "Year 9"
    ))

#Chart data with x = year, y = naplan EYL, line colour = year level

base_chart <- filter(naplan_numeracy_eyl, STATE == "AUS", SUBGROUP == "All") %>%  #Filter out data on sub groups by demographic characteristics and states
    ggplot(aes(x = CALENDAR_YEAR, y = numeracy_eyl, colour = YEAR_LEVEL, group = YEAR_LEVEL)) +  #plot using year level as the grouping for the lines and colour each distinctly
    geom_line() +  #plot a line chart
    geom_point(size = 1) + #and plot a data points, small size
    grattan_y_continuous(limits = c(1, 10), breaks = c(0, 3, 5, 7, 9)) + #TBC Adjust the scale to show chart from 0 to 9, and check marks at 3, 5, 7 and 9
    grattan_x_continuous(limits = c(2007.5, 2022.5), breaks = c(2008, 2012, 2016, 2020, 2022)) + #Adjust x-axis to start at 2008 and end at 2022
    theme_grattan(chart_type = "normal") + #adopt grattan chart design and themes
    labs(
        x = "Year",
        y = "Numeracy effective years of learning",
        colour = "Year level",
        title = "NAPLAN suggests numeracy achievement is stagnating over time",
        subtitle = "Effective years of learning in numeracy at each year level"
        ) + #Label the chart
    geom_text(data = label_lines, aes(label = chart_label), size = 4, fontface = "bold", nudge_y = 0.5) + #Label the chart lines, reduce the text size by (relatively) half
    annotate(geom = "rect", xmin = 2019.05, xmax = 2020.95, ymin = -Inf, ymax = Inf,
           fill = "white", col = NA) +
    annotate("label",
           x = 2020,
           y = 5.5,
           label = "NAPLAN cancelled due to COVID-19",
           label.padding = unit(0.9, "lines"),
           label.size = 0,
           angle = 90,
           hjust = 0.5,
           vjust = 0.5,
           size = 4,  # Adjust text size
           color = "grey"  # Change text color
    ) #Add a label for the lack of 2020 data

base_chart

grattan_save("atlas/Historical_NAPLAN_Numeracy_EYL.pdf", base_chart, "fullslide", save_data = TRUE)
grattan_save("atlas/Historical_NAPLAN_Numeracy_EYL.png", base_chart, "fullslide", save_data = TRUE)
grattan_save_pptx(base_chart, "atlas/Historical_NAPLAN_Numeracy_EYL.pptx", "fullslide")

# 5. Create a function to chart the NAPLAN scores in the past decade (2012-22) for each jurisdiction
# Note: This chapter provides an introduction to functional programming. https://r4ds.had.co.nz/functions.html.
# Note: You might find it helpful to look at my code here (https://github.com/grattan/school-ed-2022-teaching-assistants-analysis/blob/main/teacher-assistant-analysis.R) which has a function that charts teaching assistants for each jurisdiction, and here (https://github.com/grattan/school-ed-2023-naplan-analyser/blob/master/NAPLAN_analyser/app.R) which creates an app that spits out the 2023 NAPLAN data for each state and territory.


all_jurns <- tibble(
    STATE = unique(naplan_numeracy_eyl$STATE)
    )
all_jurns
all_jurns_labels <- mutate(all_jurns, jurn_label = case_when(
      STATE == "ACT" ~ "ACT",
      STATE == "AUS" ~ "Australia",
      STATE == "NSW" ~ "NSW",
      STATE == "NT" ~ "the Northern Territory",
      STATE == "QLD" ~ "Queensland",
      STATE == "SA" ~ "South Australia",
      STATE == "TAS" ~ "Tasmania",
      STATE == "VIC" ~ "Victoria",
      STATE == "WA" ~ "Western Australia",
        ), jurn_adj = case_when(
          STATE == "ACT" ~ "ACT",
          STATE == "AUS" ~ "Australian",
          STATE == "NSW" ~ "NSW",
          STATE == "NT" ~ "Northern Territory",
          STATE == "QLD" ~ "Queensland",
          STATE == "SA" ~ "South Australian",
          STATE == "TAS" ~ "Tasmanian",
          STATE == "VIC" ~ "Victorian",
          STATE == "WA" ~ "Western Australian",
        ))
all_jurns_labels

jurn_label <- function(jurn) {
  jurn_label = case_when(
    jurn == "ACT" ~ "ACT",
    jurn == "AUS" ~ "Australia",
    jurn == "NSW" ~ "NSW",
    jurn == "NT" ~ "the Northern Territory",
    jurn == "QLD" ~ "Queensland",
    jurn == "SA" ~ "South Australia",
    jurn == "TAS" ~ "Tasmania",
    jurn == "VIC" ~ "Victoria",
    jurn == "WA" ~ "Western Australia"
      )
  jurn_label
}

jurn_label("VIC")

jurn_adj <- function(jurn) {
  jurn_adj = case_when(
    jurn == "ACT" ~ "ACT",
    jurn == "AUS" ~ "Australian",
    jurn == "NSW" ~ "NSW",
    jurn == "NT" ~ "Northern Territory",
    jurn == "QLD" ~ "Queensland",
    jurn == "SA" ~ "South Australian",
    jurn == "TAS" ~ "Tasmanian",
    jurn == "VIC" ~ "Victorian",
    jurn == "WA" ~ "Western Australian"
  )
  jurn_adj
}

jurn_adj("Vic")

chart_jurn_hist_naplan_num <- function(jurn) {
    jurn_chart <- filter(naplan_numeracy_eyl, STATE == jurn, SUBGROUP == "All") %>%  #Filter out data on sub groups by demographic characteristics and states
      ggplot(aes(x = CALENDAR_YEAR, y = numeracy_eyl, colour = YEAR_LEVEL, group = YEAR_LEVEL)) +  #plot using year level as the grouping for the lines and colour each distinctly
      geom_line() +  #plot a line chart
      geom_point(size = 1) + #and plot a data points, small size
      grattan_y_continuous(limits = c(1, 10), breaks = c(0, 3, 5, 7, 9)) + #TBC Adjust the scale to show chart from 0 to 9, and check marks at 3, 5, 7 and 9
      grattan_x_continuous(limits = c(2007.5, 2022.5), breaks = c(2008, 2012, 2016, 2020, 2022)) + #Adjust x-axis to start at 2008 and end at 2022
      theme_grattan(chart_type = "normal") + #adopt grattan chart design and themes
      labs(
        x = "Year",
        y = "Numeracy effective years of learning",
        colour = "Year level",
        title = glue("NAPLAN suggests {jurn_adj(jurn)} numeracy achievement is stagnating over time"),
        subtitle = glue("Effective years of learning in numeracy at each year level in {jurn_label(jurn)}")
      ) + #Label the chart
      geom_text(data = label_lines, aes(label = chart_label), size = 4, fontface = "bold", nudge_y = 0.5) + #Label the chart lines, reduce the text size by (relatively) half
      annotate(geom = "rect", xmin = 2019.05, xmax = 2020.95, ymin = -Inf, ymax = Inf,
               fill = "white", col = NA) +
      annotate("label",
               x = 2020,
               y = 5.5,
               label = "NAPLAN cancelled due to COVID-19",
               label.padding = unit(0.9, "lines"),
               label.size = 0,
               angle = 90,
               hjust = 0.5,
               vjust = 0.5,
               size = 4,  # Adjust text size
               color = "grey"  # Change text color
      ) #Add a label for the lack of 2020 data

      jurn_chart
    }

#Could write a function to create and save charts for each jurn. Would need to work out how to use a variable to create a file path, probably using the glue() function. Can't really be bothered right now.

#Save ACT
ACT_chart <- chart_jurn_hist_naplan_num("ACT")
grattan_save("atlas/Historical_NAPLAN_numeracy_EYL_ACT.png", ACT_chart, "fullslide", save_data = TRUE)
grattan_save_pptx(ACT_chart, "atlas/Historical_NAPLAN_numeracy_EYL_ACT.pptx", "fullslide")

#Save NSW
NSW_chart <- chart_jurn_hist_naplan_num("NSW")
grattan_save("atlas/Historical_NAPLAN_numeracy_EYL_NSW.png", NSW_chart, "fullslide", save_data = TRUE)
grattan_save_pptx(NSW_chart, "atlas/Historical_NAPLAN_numeracy_EYL_NSW.pptx", "fullslide")

#Save QLD
QLD_chart <- chart_jurn_hist_naplan_num("QLD")
grattan_save("atlas/Historical_NAPLAN_numeracy_EYL_QLD.png", QLD_chart, "fullslide", save_data = TRUE)
grattan_save_pptx(QLD_chart, "atlas/Historical_NAPLAN_numeracy_EYL_QLD.pptx", "fullslide")

#Save NT
NT_chart <- chart_jurn_hist_naplan_num("NT")
grattan_save("atlas/Historical_NAPLAN_numeracy_EYL_NT.png", NT_chart, "fullslide", save_data = TRUE)
grattan_save_pptx(NT_chart, "atlas/Historical_NAPLAN_numeracy_EYL_NT.pptx", "fullslide")

#Save SA
SA_chart <- chart_jurn_hist_naplan_num("SA")
grattan_save("atlas/Historical_NAPLAN_numeracy_EYL_SA.png", SA_chart, "fullslide", save_data = TRUE)
grattan_save_pptx(SA_chart, "atlas/Historical_NAPLAN_numeracy_EYL_SA.pptx", "fullslide")

#Save VIC
VIC_chart <- chart_jurn_hist_naplan_num("VIC")
grattan_save("atlas/Historical_NAPLAN_numeracy_EYL_VIC.png", VIC_chart, "fullslide", save_data = TRUE)
grattan_save_pptx(VIC_chart, "atlas/Historical_NAPLAN_numeracy_EYL_VIC.pptx", "fullslide")

#Save TAS
TAS_chart <- chart_jurn_hist_naplan_num("TAS")
grattan_save("atlas/Historical_NAPLAN_numeracy_EYL_TAS.png", TAS_chart, "fullslide", save_data = TRUE)
grattan_save_pptx(TAS_chart, "atlas/Historical_NAPLAN_numeracy_EYL_TAS.pptx", "fullslide")

#Save WA
WA_chart <- chart_jurn_hist_naplan_num("WA")
grattan_save("atlas/Historical_NAPLAN_numeracy_EYL_WA.png", WA_chart, "fullslide", save_data = TRUE)
grattan_save_pptx(WA_chart, "atlas/Historical_NAPLAN_numeracy_EYL_WA.pptx", "fullslide")

# 6. Check the trend over time in each jurisdiction.
# Note: Run a linear regression to test if year predicts score. See if the average annual trend is at least 0.25 EYL, using the formulae on page 128 and 129 here. https://www.nap.edu.au/docs/default-source/default-document-library/naplan-2022-technical-report.pdf
# Note: You could incorporate this into the function above, and make the result a callout on the chart.
