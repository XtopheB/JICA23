# =============================================================================
# 1. LOAD PACKAGES
# =============================================================================

# --- Data management ---
library(tidyverse)    # Core collection: dplyr, tidyr, readr, ggplot2, etc.
library(modelsummary) # Publication-ready model and data summary tables


# =============================================================================
# 3. LOAD DATA
# =============================================================================

# Read the household survey data from a CSV file.
# The file is expected to be in a 'Data/' subfolder relative to your working
# directory. Adjust the path if your project is structured differently.

Mydata <- read.csv("Data/Household.csv")

# Quick sanity check: inspect the first few rows to confirm the data loaded
# correctly and to get a sense of the variable names and types.
head(Mydata)

# Check the structure: number of rows/columns, variable types (chr, int, dbl…)
str(Mydata)


# =============================================================================
# 4. DATA CLEANING
# =============================================================================

# Select only the variables needed for the analysis, then engineer new ones.
# We also convert character columns to factors so that R treats them as
# categorical variables in summaries, plots, and models.


# 1 - Keep only the relevant columns
# 2-  Create an age group variable from the continuous age variable.
# Tip: 'case_when' is an alternative to if-else.

Mydata <- Mydata %>%
  select(income, age, education, sex, education_level) %>%
  mutate(
    age_group = case_when(
      age < 20              ~ "Young",
      age >= 20 & age < 40  ~ "Middle",
      age >= 40             ~ "Old"
    ),
    
    # Order the age group levels from youngest to oldest.
    # This controls the display order in plots and tables.
    age_group = factor(age_group, levels = c("Young", "Middle", "Old")),
    
    # Convert character columns to factors for modelling and plotting
    sex             = as.factor(sex),
    education_level = as.factor(education_level)
  )

# Confirm the result: check that new variables are present and correctly typed
str(Mydata)


# =============================================================================
# 5. DESCRIPTIVE STATISTICS
# =============================================================================

# --- 5a. Continuous variables ---
# datasummary_skim() from {modelsummary} gives a compact, formatted summary
# (N, mean, SD, min, median, max, histogram) for each numeric column.
datasummary_skim(Mydata,
                 type  = "numeric",
                 title = "Continuous variables")

# --- 5b. Categorical variables ---
# The same function handles factors/characters when type = "categorical",
# showing frequency counts and proportions for each level.
datasummary_skim(Mydata,
                 type  = "categorical",
                 title = "Categorical (factors)")

# --- 5c. Summary by sex ---
# Using dplyr's group_by + summarise to compute key statistics separately
# for men and women. This reveals first-order differences between groups.

Mydata_by_sex <- Mydata %>%
  group_by(sex) %>%
  summarise(
    N                    = n(),
    mean_income          = round(mean(income),    1),
    sd_income            = round(sd(income),      1),
    min_income           = round(min(income),     1),
    max_income           = round(max(income),     1),
    mean_years_education = round(mean(education), 1),
    mean_age             = round(mean(age),       1),
    .groups = "drop"
  )


# =============================================================================
# 6. VISUALISATION - UNIVARIATE & BIVARIATE
# =============================================================================

# --- 6a. Average income by sex (bar chart) ---
# We compute group means first, then pass them to ggplot.
# coord_flip() makes the bars horizontal for easier label reading.
Mydata_by_sex %>%
  ggplot(aes(sex, mean_income, fill = sex)) +
  geom_col(width = 0.5) +
  scale_fill_manual(values = sex_colors) +
  labs(
    title = "Average income by sex",
    y     = "Mean income",
    x     = NULL
  ) +
  coord_flip() +
  theme(legend.position = "none")

# --- 6b. Scatter plot: income vs education ---
# A basic scatterplot reveals the direction and spread of the relationship.
# alpha = 0.5 adds transparency so overlapping points are still visible.
Mydata %>% 
ggplot()+
  aes(education, income) +
  geom_point(color = SIAP.color, alpha = 0.5) +
  labs(
    title = "Scatter plot of Income vs Education",
    x     = "Education (Adjusted years)",
    y     = "Income"
  )

# --- 6c. Income vs age ---
# Check whether age alone shows a clear pattern with income.
ggplot(Mydata, aes(age, income)) +
  geom_point(alpha = 0.6, color = "darkgrey") +
  labs(
    title = "Income and Age",
    x     = "Age",
    y     = "Income"
  )

# --- 6d. Income vs education, faceted by age group ---
# Faceting splits the plot into panels — one per age group — so we can see
# whether the education-income relationship differs across age categories.
# position_jitter() adds a small horizontal nudge to reduce overplotting.
ggplot(Mydata, aes(education, income, color = age_group)) +
  geom_point(alpha = 0.6, position = position_jitter(width = 0.3, height = 0)) +
  facet_wrap(~ age_group) +
  scale_color_manual(values = age_colors) +
  labs(
    title    = "Income and Education",
    subtitle = "Colour = age group",
    x        = "Education (Adjusted years)",
    y        = "Income",
    color    = "Age"
  ) +
  theme(
    panel.border     = element_rect(color = "grey70", fill = NA, linewidth = 0.5),
    panel.spacing.x  = unit(2, "lines"),
    panel.spacing.y  = unit(1, "lines"),
    legend.position  = "bottom"
  )

# --- 6e. Scatterplot matrix ---
# GGally::ggpairs() shows all pairwise combinations of the selected variables:
#   - Upper triangle: correlation coefficients
#   - Lower triangle: scatter plots
#   - Diagonal:       density curves
# Coloring by sex lets us spot group-level patterns simultaneously.
GGally::ggpairs(
  Mydata,
  columns = c("income", "age", "education"),
  aes(color = sex, alpha = 0.3),
  title   = "Distribution and relationships between Income, Age, and Education\n(by sex)",
  switch  = "both",
  upper   = list(continuous = "cor"),
  lower   = list(continuous = "points"),
  diag    = list(continuous = "densityDiag")
) +
  scale_color_manual(values = sex_colors) +
  scale_fill_manual(values = sex_colors)


# =============================================================================
# 7. REGRESSION MODELS
# =============================================================================

# --- 7a. Simple (univariate) linear regression ---
# We model income as a linear function of education only.
# This gives a baseline estimate of the education effect, ignoring other variables.
modelUni <- lm(income ~ education, data = Mydata)
summary(modelUni)

# A nicely formatted table of coefficients, standard errors, and significance
modelsummary(
  modelUni,
  statistic = "std.error",
  stars     = TRUE,
  output    = "html"
)

# --- 7b. Multiple linear regression ---
# Adding age and sex as covariates allows us to estimate the effect of education
# *controlling for* the other variables — this is the key advantage over
# descriptive statistics or separate group analyses.
modelMulti <- lm(income ~ education + age + sex, data = Mydata)
summary(modelMulti)

# Formatted coefficient table
modelsummary(
  modelMulti,
  statistic = "std.error",
  stars     = TRUE,
  output    = "html"
)

# Alternative presentation with sjPlot (shows confidence intervals, p-values)
tab_model(
  modelMulti,
  title     = "Regression of Income on Education, Age and Sex",
  show.se   = TRUE,
  show.p    = TRUE,
  dv.labels = "Income"
)


# =============================================================================
# 8. REGRESSION VISUALISATION
# =============================================================================

# --- 8a. Regression line on the scatter plot ---
# geom_smooth(method = "lm") overlays the fitted OLS line on the raw data.
ggplot(Mydata, aes(education, income)) +
  geom_point(alpha = 0.4, color = SIAP.color) +
  geom_smooth(method = "lm", se = FALSE, color = "grey") +
  labs(
    title    = "Income and Education",
    subtitle = "Regression line adjusted for age and sex",
    x        = "Education (Adjusted years)",
    y        = "Income"
  )

# --- 8b. Predicted values by sex ---
# We build a prediction grid: education varies across its full range,
# sex takes both levels, and age is fixed at its mean (holding it constant).
# This isolates the sex-specific education effect from the multivariate model.
pred_data <- expand.grid(
  education = seq(min(Mydata$education), max(Mydata$education), length.out = 100),
  sex       = unique(Mydata$sex),
  age       = mean(Mydata$age)    # Hold age constant at its mean
)

pred_data$pred_income <- predict(modelMulti, newdata = pred_data)

ggplot(Mydata, aes(education, income, color = sex)) +
  geom_point(alpha = 0.3) +
  geom_line(
    data      = pred_data,
    aes(education, pred_income, color = sex),
    linewidth = 1
  ) +
  scale_color_manual(values = sex_colors) +
  labs(
    title    = "Income and Education by Sex",
    subtitle = "Predictions from the multivariate model (age held at its mean)",
    x        = "Education (Adjusted years)",
    y        = "Income",
    color    = "Sex"
  ) +
  theme_minimal()

# --- 8c. Income–education by sex (simple, without age adjustment) ---
# Fitting separate lines per group WITHOUT controlling for age.
# Compare with 8b to see how ignoring age can change the apparent sex effect.
ggplot(Mydata, aes(education, income, color = sex)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_manual(values = sex_colors) +
  labs(
    title = "Income–Education Relationship by Sex (unadjusted)",
    x     = "Education (Adjusted years)",
    y     = "Income",
    color = "Sex"
  )


# =============================================================================
# 9. RESIDUALS - UNDERSTANDING MODEL FIT
# =============================================================================

# Residuals = observed income − model's predicted income.
# Large residuals indicate observations the model fits poorly.

# Compute predicted values and residuals from the multivariate model
Mydata <- Mydata %>%
  mutate(
    predicted   = predict(modelMulti),
    residual    = income - predicted,
    abs_residual = abs(residual)
  )

# Select one representative point per education bin to illustrate residuals.
# We cut education into 9 equal-width bins and sample one row from each.
sample_points <- Mydata %>%
  mutate(edu_bin = cut(education, breaks = 9)) %>%
  group_by(edu_bin) %>%
  sample_n(1) %>%
  ungroup() %>%
  select(-edu_bin)

# --- 9a. Highlight points far from the regression line ---
ggplot(Mydata, aes(education, income)) +
  geom_point(alpha = 0.4, color = "grey") +
  geom_smooth(method = "lm", se = FALSE, color = "grey") +
  geom_point(
    data  = sample_points,
    aes(education, income),
    color = "red", size = 2, alpha = 0.8
  ) +
  labs(
    title    = "Income and Education",
    subtitle = "Points far from the regression line highlighted in red",
    x        = "Education (Adjusted years)",
    y        = "Income"
  )

# --- 9b. Show residuals as vertical lines ---
# Each red segment connects an observed point (red dot) to its predicted value
# on the regression line. The length of the segment is the residual.
ggplot(Mydata, aes(education, income)) +
  geom_point(alpha = 0.4, color = "grey") +
  geom_smooth(method = "lm", se = FALSE, color = "grey") +
  geom_segment(
    data     = sample_points,
    aes(x = education, xend = education, y = income, yend = predicted),
    color    = "red", linewidth = 1, linetype = "solid"
  ) +
  geom_point(
    data  = sample_points,
    aes(education, income),
    color = "red", size = 1.5, alpha = 0.8
  ) +
  labs(
    title    = "Income and Education",
    subtitle = "Red segments show residuals (observed − predicted)",
    x        = "Education (Adjusted years)",
    y        = "Income"
  )

# =============================================================================
# END OF SCRIPT
# =============================================================================
