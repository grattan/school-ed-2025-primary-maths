# dummy charts to test survey analysis

# Mathematics confidence ==============================================================
# Likelihood of agreeing with "I feel nervous when teaching maths". Imagining results from a logistic regression.

data_frame <-
  tribble(
    ~demographic, ~odds_ratio, ~odds_ratio_se,
    "Beginning teacher", 0.1, 0.09,
    "Did no maths in Year 12", 0.45, 0.01,
    "Female", 0.05, 0.16,
    "Employed in a government school", 0.08, 0.057,
    "Upper primary teacher", 0.21, 0.09,
    "Employed in a disadvantaged school", 0.13, 0.035
  )

data_frame |>
  ggplot(aes(
    x = reorder(demographic, odds_ratio),
    ymin = (odds_ratio - 1.65 * odds_ratio_se),
    ymax = (odds_ratio + 1.65 * odds_ratio_se),
    y = odds_ratio
  )) +
  geom_linerange(linewidth = 5,
                 alpha = 0.5) +
  geom_point() +
  geom_hline(yintercept = 0,
             color = "black",
             linewidth = 0.25,
             linetype = "dashed") +
  coord_flip() +
  # theme and labels
  theme_grattan() +
  grattan_y_continuous(labels = comma,
                       limits = c(-1, 1)) +
  labs(
    title = "Some teachers are more likely than others to report avoiding teaching maths",
    subtitle = "Relative likelihood of teachers agreeing or strongly agreeing 'If I can avoid teaching maths, I do'",
    x = "", y = NULL,
    caption = "Source: 2024 Grattan Institute survey of primary mathematics teachers. Notes: Sample size included 2,314 teachers. Odds ratio results are from a logistic regression. Odds ratio shows the relative likelihood of teachers reporting a nervousness to teach maths if they have that characteristic. So an odds ratio of 0.5 means that there is a 50 per cent increase in the likelihood of agreeing or strongly agreeing with the statement, all else being constant."
  )

# Likelihood of agreeing with the statements 'I would describe myself as strong at maths'

data_frame <-
  tribble(
    ~demographic, ~category, ~result,
    "Government", "Sector", 0.42,
    "Catholic", "Sector", 0.46,
    "Independent", "Sector", 0.56,
    "Metropolitan", "Location", 0.51,
    "Regional", "Location", 0.48,
    "Remote", "Location", 0.46,
    "Mostly disadvantaged", "Advantage", 0.36,
    "Mix of advantage", "Advantage", 0.48,
    "Mostly advantaged", "Advantage", 0.61,
  )

data_frame |>
  ggplot(aes(
    x = reorder(demographic, result),
    y = result
  )) +
  geom_col() +
  coord_flip() +
  # theme and labels
  theme_grattan(flipped = T) +
  grattan_y_continuous(labels = percent,
                       limits = c(0, 1)) +
  facet_wrap(~category, ncol = 1, scales = "free_y") +
  labs(
    title = "Across many schools, about half of teachers feel they are strong at maths",
    subtitle = "Percentage of teachers agreeing or strongly agreeing with the statement 'I would describe myself as strong at maths'",
    x = "", y = NULL,
    caption = "Source: 2024 Grattan Institute survey of primary mathematics teachers. Notes: Sample size included 2,314 teachers. Chi-squared tests indicated that differences between schools were significant."
  )


