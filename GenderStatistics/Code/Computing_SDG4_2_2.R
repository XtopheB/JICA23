## ----setup, include=FALSE--------------------------------------------------------------------------------------------------------------------
# We define the options for the chunks globaly here

knitr::opts_chunk$set( message = FALSE,
                       warning = FALSE,
                       results = TRUE,
                       echo = knitr::is_html_output() # Code only in html!
                       )



## ----country---------------------------------------------------------------------------------------------------------------------------------
# Since we may want to use other data set from other countries/years
# Let us define the country and year as a parameter
country <- "Lao"
DataYear <- 2023
# Years taken into account for the definition of the indicator
RefYear <-  DataYear -1 # The last school year recorded
RefAge <- 5



## ----packages--------------------------------------------------------------------------------------------------------------------------------
# We use pacman here 
# install.packages("pacman")  # <-- Run only once

library(pacman)
pacman::p_load(
  haven,
  tidyverse,
  kableExtra, 
  modelsummary
)


## ----load-data-------------------------------------------------------------------------------------------------------------------------------
rawdf <- read_sav("../Data/hl.sav")


## ----select-vars-----------------------------------------------------------------------------------------------------------------------------
df <- rawdf %>%
  select( HL4, HL5M, HL5Y, HL6, ED4, ED9, ED10A, HH6, windex5, hhweight)




## ----Summary_all-----------------------------------------------------------------------------------------------------------------------------
# We can use the package modelsummary for a nice descriptive table  
datasummary_skim(df , type = "numeric" )


## ----redefine-type---------------------------------------------------------------------------------------------------------------------------
# We use as_factor to transform variables with their right type

df <- df %>%
  mutate(HL4 = as_factor(HL4), 
         HH6 = as_factor(HH6))


## ----Summary_factor--------------------------------------------------------------------------------------------------------------------------
# We can use the package modelsummary for a nice descriptive table  
datasummary_skim(df ,type = "categorical" )


## ----cohort-filter---------------------------------------------------------------------------------------------------------------------------

# let's define the schooling period for this country/year

Year_Start <-(RefYear-RefAge)-1 
Year_End <- RefYear-RefAge

# Filter the observations to define the target population
Pop_Children <- df %>%
  filter(     
    # Remove missing/invalid birth dates
    !is.na(HL5Y),
    !is.na(HL5M),
    !(HL5M %in% c(98, 99))
    ) %>%
  filter(    
    # Selecting the target cohort
    (HL5Y == Year_Start & HL5M >= 10) |
      (HL5Y == Year_End & HL5M <= 9)
  )


## ----Population------------------------------------------------------------------------------------------------------------------------------
Pop_Children <- Pop_Children %>%
    # Attending ECE or Grade 1 in current school year 
     mutate(
    organized_learning = ifelse(ED9 == 1 & ED10A %in% c(0, 1), TRUE, FALSE)
  )


## ----InLearning-count------------------------------------------------------------------------------------------------------------------------
Nb_InLearning <-  sum(Pop_Children$organized_learning, na.rm = TRUE)



## ----national-estimate-----------------------------------------------------------------------------------------------------------------------
sdg_4_2_2 <- Pop_Children %>%
  summarise(
    Numerator   = sum(hhweight * organized_learning, na.rm = TRUE),
    Denominator = sum(hhweight, na.rm = TRUE),
    SDG_4_2_2   = 100 * Numerator / Denominator
  )

# print(sdg_4_2_2)


## ----disagg-sex------------------------------------------------------------------------------------------------------------------------------
sdg_4_2_2_sex <- Pop_Children %>%
  group_by(HL4) %>%
  summarise(
    Numerator   = sum(hhweight * organized_learning, na.rm = TRUE),
    Denominator = sum(hhweight, na.rm = TRUE),
    SDG_4_2_2   = round(100 * Numerator / Denominator, 1)
  )



## ----viz-sex---------------------------------------------------------------------------------------------------------------------------------
sdg_4_2_2_sex %>%
  ggplot(aes(x = HL4, y = SDG_4_2_2, fill = HL4)) +
  geom_col(width = 0.5, show.legend = FALSE) +
  geom_text(aes(label = paste0(SDG_4_2_2, "%")),
            vjust = -0.5, size = 4.5) +
  scale_fill_manual(values = c("#1a7abf", "#e05c2a")) +
  scale_y_continuous(limits = c(0, 110), labels = scales::percent_format(scale = 1)) +
  labs(
    title    = "SDG 4.2.2 by Sex",
    subtitle = paste(country, "- School Year", RefYear, "-", DataYear),
    x = NULL, y = "Children in organized learning (%)"
  ) +
   coord_flip() +
  theme_minimal(base_size = 13)


## ----disagg-Area-----------------------------------------------------------------------------------------------------------------------------
sdg_4_2_2__location <- Pop_Children %>%
  mutate(HH6_N = ifelse(HH6 == "URBAN", "Urban", "Rural"))%>%
  group_by(HH6_N) %>%
  summarise(
    Numerator   = sum(hhweight * organized_learning, na.rm = TRUE),
    Denominator = sum(hhweight, na.rm = TRUE),
    SDG_4_2_2   = round(100 * Numerator / Denominator, 1)
  ) 



## ----viz-location----------------------------------------------------------------------------------------------------------------------------
sdg_4_2_2__location %>%
  ggplot(aes(x =HH6_N, y = SDG_4_2_2, fill = HH6_N)) +
  geom_col(width = 0.5, show.legend = FALSE) +
  geom_text(aes(label = paste0(SDG_4_2_2, "%")),
            vjust = -0.5, size = 4.5) +
  scale_y_continuous(limits = c(0, 110), labels = scales::percent_format(scale = 1)) +
  labs(
    title    = "SDG 4.2.2 by location",
    subtitle = paste(country, "- School Year", RefYear, "-", DataYear),
    x = NULL, y = "Children in organized learning (%)"
  ) +
   coord_flip() +
  theme_minimal(base_size = 13)


## ----disagg-wealth---------------------------------------------------------------------------------------------------------------------------
sdg_4_2_2_Wealth <- Pop_Children  %>%
   mutate(windex5 = as_factor(windex5)) %>%  # windex5 should be a factor
  group_by(windex5) %>%  
  summarise(
    Numerator = sum(hhweight * organized_learning, na.rm = TRUE),
    Denominator = sum(hhweight, na.rm = TRUE),
    SDG_4_2_2  = round(100 * Numerator / Denominator, 1)
  )



## ----viz-wealth------------------------------------------------------------------------------------------------------------------------------
sdg_4_2_2_Wealth %>%
  ggplot(aes(x = windex5, y = SDG_4_2_2, fill = windex5)) +
  geom_col(width = 0.5, show.legend = FALSE) +
  geom_text(aes(label = paste0(SDG_4_2_2, "%")),
            vjust = -0.1, size = 4.5) +
  scale_y_continuous(limits = c(0, 110), labels = scales::percent_format(scale = 1)) +
  labs(
    title    = "SDG 4.2.2 by Wealth levels (quintile)",
    subtitle = paste(country, "- School Year", RefYear, "-", DataYear),
    x = NULL, y = "Children in organized learning (%)"
  ) +
   coord_flip() +
  theme_minimal(base_size = 13)


## ----export----------------------------------------------------------------------------------------------------------------------------------
write_csv(sdg_4_2_2,     "../output/sdg_4_2_2_results.csv")
write_csv(sdg_4_2_2_sex, "../output/sdg_4_2_2_results_by_sex.csv")
write_csv(sdg_4_2_2__location, "../output/sdg_4_2_2_results_by_location.csv")
write_csv(sdg_4_2_2_Wealth, "../output/sdg_4_2_2_results_by_wealth.csv")

