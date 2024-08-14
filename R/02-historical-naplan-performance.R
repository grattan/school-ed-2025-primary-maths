##''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
##            02. ANALYSIS OF HISTORICAL NAPLAN DATA            ''
##              School Education Program, June 2024             ''
##                          Dan Petrie                          ''
##''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

source("R/00-setup.R")

# 1. Read in NAPLAN data ----
# Note: Dataset is here: https://www.acara.edu.au/reporting/national-report-on-schooling-in-australia/naplan-national-report-archive#dataset.
# Note: Chat GPT might be able to help.

naplan_data <- read_excel("data/naplan_results_2008_to_2022.xlsx", sheet = "Data") |>
  janitor::clean_names()

#There were 50 or more warnings. First 50 were all of the form "Expecting logical in AJ2709 / R2709C36: got 'Above'. Or same but with 'close to'. That is, appear to be querying the data class for the my school comparative performance variables.

naplan_data


# 2. Filter NAPLAN data for Numeracy results ----

naplan_numeracy_data <-
  naplan_data %>%
    filter(domain == "Numeracy")


# 3. Convert NAPLAN scale scores to equivalent years of learning ----
# Note: Use ACARA equation on page 128, here: https://www.nap.edu.au/docs/default-source/default-document-library/naplan-2022-technical-report.pdf


#The formula for the mean NAPLAN score for a particular year level is
# Mean score (Y) = a * ln(x) + b, where for numeracy results slope (a) = 170.861 and intercept (b) = 213.536
# That is -     Y = 170.861 ln(x) + 213.536

#For effective years of learning, formula works in reverse (to go from NAPLAN score to effective years of learning)
# x = e^((Y-b)/a)

#Define constants
slope <- 170.861
intercept <- 213.536

#Create a new variable for the effective years of learning in numeracy, which is calculated by reference to NAPLAN numeracy scores, based on the formula for effective years of learning above.

naplan_numeracy_eyl <-
  naplan_numeracy_data %>%
    mutate(numeracy_eyl = exp((mean - intercept)/slope),
           numeracy_eyl_lower = exp((mean - mean_ci - intercept)/slope),
           numeracy_eyl_upper = exp((mean + mean_ci - intercept)/slope))

head(naplan_numeracy_eyl$numeracy_eyl)
head(naplan_numeracy_eyl$numeracy_eyl_lower) # Some NA values for confidence. Down to too few participants
head(naplan_numeracy_eyl$numeracy_eyl_upper)

#For the project preview pack, Jordana will want to understand the magnitude of improvement at Year 5. Here is some code which does that. We'll eventually want to add little callouts to the charts which show how many months of improvement (for those states, like Qld here there has been improvement).

# 3.2. Understand magnitude of improvement

naplan_numeracy_eyl |>
  filter(state == "AUS", subgroup == "All", calendar_year %in% c(2008, 2022)) |>
  select(year_level, calendar_year, starts_with("numeracy_eyl")) |>
  group_by(year_level) |>
  summarise(change_in_mean = last(numeracy_eyl) - first(numeracy_eyl),
            change_in_mean_mths = 12 * change_in_mean,
            min_change_in_mean = last(numeracy_eyl_lower) - first(numeracy_eyl_upper),
            min_change_in_mean_mths = 12 * min_change_in_mean)

# 3.3. Does Year 5 improve if we begin the time series at 2012?

y5_last_decade <-
  naplan_numeracy_eyl |>
  filter(state == "AUS",
         subgroup == "All",
         calendar_year >= 2012,
         year_level == 5)

# Linear models
model_NAPLAN_point <- lm(formula = mean ~ calendar_year, data = y5_last_decade)
model_eyl <- lm(formula = numeracy_eyl ~ calendar_year, data = y5_last_decade)

summary(model_NAPLAN_point)
summary(model_eyl)

# Non-linear models
nlm_model_NAPLAN_point <- lm(formula = mean ~ poly(calendar_year, 2), data = y5_last_decade)
nlm_model_eyl <- lm(formula = numeracy_eyl ~ poly(calendar_year, 2), data = y5_last_decade)

summary(nlm_model_NAPLAN_point)
summary(nlm_model_eyl)

#plotting the model
ggplot(y5_last_decade, aes(calendar_year, numeracy_eyl)) +
  geom_point() +
  geom_line(aes(calendar_year, predict(nlm_model_eyl))) +
  ggtitle("Quadratic Regression") +
  scale_y_continuous_grattan(limits = c(4,6)) +
  theme_grattan() +
  scale_x_continuous_grattan(labels = round, name = NULL)

# 4. Chart national NAPLAN scale scores for the past decade (2012-22) for each year level ----
# Note: Filter and chart within the one chunk of code
# Note: Try make the y-axis start at EYL1 and show breaks for every two equivalent year levels
# Note: 2020 data is missing, so you may wish to note this on the chart
# Note: Figure 1.1 here could be a guide: https://grattan.edu.au/wp-content/uploads/2024/03/Spreading-success-Why-Australia-should-trial-multi-school-organisations.pdf
# Note: You could adapt lines 77 to 110 in my code here: https://github.com/grattan/school-ed-2024-improving_schools/blob/main/R/03-chronic_underperformers.R

names(naplan_numeracy_eyl)

#Create data sets for chart line labels

label_lines <- filter(naplan_numeracy_eyl, state == "AUS", subgroup == "All", calendar_year == 2022) %>% #Filter out data on sub groups by demographic characteristics and states, and then keep only 2022 data - 1 dot per line, last on RHS
    mutate(chart_label = case_when(
      year_level == 3 ~ "Year 3",
      year_level == 5 ~ "Year 5",
      year_level == 7 ~ "Year 7",
      year_level == 9 ~ "Year 9"
    ))

#Chart data with x = year, y = naplan EYL, line colour = year level
base_chart <- filter(naplan_numeracy_eyl, state == "AUS", subgroup == "All") %>%  #Filter out data on sub groups by demographic characteristics and states
    ggplot(aes(x = calendar_year, y = numeracy_eyl, colour = year_level, group = year_level)) +  #plot using year level as the grouping for the lines and colour each distinctly
    geom_line() +  #plot a line chart
    annotate(geom = "rect", xmin = 2019.05, xmax = 2020.95, ymin = -Inf, ymax = Inf,
           fill = "white", col = NA) +
    geom_point(size = 8/.pt) + #and plot a data points, small size
    grattan_y_continuous(limits = c(1, 10), breaks = c(0, 3, 5, 7, 9)) + #TBC Adjust the scale to show chart from 0 to 9, and check marks at 3, 5, 7 and 9
    grattan_x_continuous(limits = c(2007.5, 2022.5), breaks = c(2008, 2012, 2016, 2020, 2022)) + #Adjust x-axis to start at 2008 and end at 2022
    theme_grattan(chart_type = "normal") + #adopt grattan chart design and themes
    scale_colour_grattan() +
    labs(
        x = NULL,
        colour = "Year level",
        title = "NAPLAN suggests numeracy achievement is stagnating over time",
        subtitle = "Effective years of learning in numeracy at each year level"
        ) + #Label the chart
    geom_text(data = label_lines,
              aes(label = chart_label),
              size = 18/.pt,
              fontface = "bold",
              nudge_y = 0.5) + #Label the chart lines, reduce the text size by (relatively) half
    annotate("text",
           x = 2020,
           y = 5.5,
           label = "NAPLAN cancelled due to COVID-19",
           angle = 90,
           hjust = 0.5,
           vjust = 0.5,
           size = 18/.pt,  # Adjust text size
           color = grattan_black  # Change text color
    ) #Add a label for the lack of 2020 data

base_chart

grattan_save("atlas/Historical_NAPLAN_Numeracy_EYL.pdf", base_chart, "fullslide", save_data = TRUE)
grattan_save("atlas/Historical_NAPLAN_Numeracy_EYL.png", base_chart, "fullslide", save_data = TRUE)
grattan_save_pptx(base_chart, "atlas/Historical_NAPLAN_Numeracy_EYL.pptx", "fullslide")

# 5. Create a function to chart the NAPLAN scores in the past decade (2012-22) for each jurisdiction
# Note: This chapter provides an introduction to functional programming. https://r4ds.had.co.nz/functions.html.
# Note: You might find it helpful to look at my code here (https://github.com/grattan/school-ed-2022-teaching-assistants-analysis/blob/main/teacher-assistant-analysis.R) which has a function that charts teaching assistants for each jurisdiction, and here (https://github.com/grattan/school-ed-2023-naplan-analyser/blob/master/NAPLAN_analyser/app.R) which creates an app that spits out the 2023 NAPLAN data for each state and territory.


all_jurns <- tibble(
    state = unique(naplan_numeracy_eyl$state)
    )
all_jurns

all_jurns_labels <- strayr::clean_state(all_jurns$state, to = "state_name")

chart_jurn_hist_naplan_num <-
  function(jurn) {
    jurn_chart <-
      filter(naplan_numeracy_eyl,
             state == jurn,
             subgroup == "All") %>%  #Filter out data on sub groups by demographic characteristics and states
      ggplot(aes(x = calendar_year, y = numeracy_eyl, colour = year_level, group = year_level)) +  #plot using year level as the grouping for the lines and colour each distinctly
      geom_line() +  #plot a line chart
      annotate(geom = "rect", xmin = 2019.05, xmax = 2020.95, ymin = -Inf, ymax = Inf,
               fill = "white", col = NA) +
      geom_point(size = 8/.pt) + #and plot a data points, small size
      grattan_y_continuous(limits = c(1, 10), breaks = c(0, 3, 5, 7, 9)) + #TBC Adjust the scale to show chart from 0 to 9, and check marks at 3, 5, 7 and 9
      grattan_x_continuous(limits = c(2007.5, 2022.5), breaks = c(2008, 2012, 2016, 2020, 2022),
                           expand_right = 0.1) + #Adjust x-axis to start at 2008 and end at 2022
      theme_grattan(chart_type = "normal") + #adopt grattan chart design and themes
      labs(
        x = NULL,
        y = NULL,
        colour = "Year level",
        title = glue("NAPLAN suggests numeracy achievement in {strayr::clean_state(jurn, to = 'state_name')} is stagnating over time"),
        subtitle = glue("Effective years of learning in numeracy at each year level in {strayr::clean_state(jurn, to = 'state_name')}")
      ) + #Label the chart
      #geom_text(data = label_lines, aes(label = chart_label), size = 18/.pt, fontface = "bold", nudge_y = 0.5) + #Label the chart lines, reduce the text size by (relatively) half
      ggdirectlabel::geom_finallabel(aes(label = paste("Year", year_level)), fontface  = "bold", size = 18/.pt) +
      annotate("label",
               x = 2020,
               y = 5.5,
               label = "NAPLAN cancelled due to COVID-19",
               label.padding = unit(0.9, "lines"),
               label.size = 0,
               angle = 90,
               hjust = 0.5,
               vjust = 0.5,
               size = 18/.pt,  # Adjust text size
               color = grattan_black  # Change text color
      ) #Add a label for the lack of 2020 data

      jurn_chart
    }

#Could write a function to create and save charts for each jurn. Would need to work out how to use a variable to create a file path, probably using the glue() function. Can't really be bothered right now.

#Create a list of plots with all charts

state_charts <-
  map(pull(all_jurns), chart_jurn_hist_naplan_num)

grattan_save_pptx(state_charts, "atlas/NAPLAN_08_22_by_jurisdiction.pptx")


# 6. Check the trend over time in each jurisdiction.
# Note: Run a linear regression to test if year predicts score. See if the average annual trend is at least 0.25 EYL, using the formulae on page 128 and 129 here. https://www.nap.edu.au/docs/default-source/default-document-library/naplan-2022-technical-report.pdf
# Note: You could incorporate this into the function above, and make the result a callout on the chart.
