###############################################################################
# SDG 4.2.2 Calculation + Output Table + CSV Export
###############################################################################

library(tidyverse)
library(haven)
library(dplyr)


# Load data
rawdf <- read_sav("../Data/hl.sav")

# Quick structure check
str(rawdf)

# -----------------------------------------------------------------------------
# Explore
# -----------------------------------------------------------------------------
#select only variables of interest
#HL4 - Sex of household member
#HL5M - Date of birth (month)
#HL5Y -Date of birth (year)
#HL6 - Age of household member
#ED4 - Ever attended school or ECE
#ED9 - Attended school or ECE in current year (2022-2023)
#ED10A - Level of education in current year (ECE==0)
#HH6 - Location or Area (rural , Urban)
#windex5: Wealth quintile
#hhweight - Sample weight (household)

#Explore sex variable HL4
class(rawdf$HL4)
table(rawdf$HL4)
attr(rawdf$HL4, "labels")

#Explore age variable HL6
class(rawdf$HL6)
summary(rawdf$HL6)

#Explore variable on current status in education ED9
class(rawdf$ED9)
table(rawdf$ED9)
attr(rawdf$ED9, "labels")

#Explore variable on status in education in previous year ED15
class(rawdf$ED15)
table(rawdf$ED15)
attr(rawdf$ED15, "labels")

#Explore variable weight variable hhweight
class(rawdf$hhweight)
summary(rawdf$hhweight)

df <-rawdf %>%
  select(HL1, HL4, HL5M, HL5Y, HL6, ED4, ED9, ED10A, HH6, windex5, hhweight)

# -----------------------------------------------------------------------------
# Compute SDG 4.2.2
# -----------------------------------------------------------------------------
# SDG 4.2.2: Lao PDR
# Children who were age 5 during Sept 2022:
# born Oct 2017 to Sept 2018

children_5_sep2022 <- df %>%
  filter(
    !is.na(HL5Y),
    !is.na(HL5M),
    !(HL5M %in% c(98, 99)),
    (HL5Y == 2016 & HL5M >= 10) |
      (HL5Y == 2017 & HL5M <= 9)
  )

hl_422 <- children_5_sep2022 %>%
  #filter(HL6 == 5) %>%
  mutate(
    # Attending school or ECE in current school year 2022-2023
    organized_learning_current = ED9 == 1 & ED10A %in% c(0, 1),
  )
sum(hl_422$organized_learning)

sdg_4_2_2 <- hl_422 %>%
  summarise(
    Numerator = sum(hhweight * organized_learning_current, na.rm = TRUE),
    Denominator = sum(hhweight, na.rm = TRUE),
    SDG_4_2_2 = 100 * Numerator / Denominator
  )
  
sdg_4_2_2$Numerator
sdg_4_2_2$Denominator
sdg_4_2_2

# -----------------------------------------------------------------------------
# Disaggregate by sex
# -----------------------------------------------------------------------------

sdg_4_2_2_sex <- hl_422 %>%
  group_by(HL4) %>%   # 1 = male, 2 = female
  summarise(
    Numerator = sum(hhweight * organized_learning_current, na.rm = TRUE),
    Denominator = sum(hhweight, na.rm = TRUE),
    proportion = 100 * Numerator / Denominator
  )

sdg_4_2_2_sex

print(sdg_4_2_2_sex)
# -----------------------------------------------------------------------------
# Display nicely in console
# -----------------------------------------------------------------------------

print(sdg_4_2_2)

###  Location 
sdg_4_2_2_location <- hl_422  %>%
  mutate(HH6_N = ifelse(HH6 == 1, HH6, 2))%>%
  group_by(HH6_N) %>%   # 1 URBAN    2 RURAL WITH ROAD  3 RURAL WITHOUT ROAD 
  summarise(
    Numerator = sum(hhweight * organized_learning_current, na.rm = TRUE),
    Denominator = sum(hhweight, na.rm = TRUE),
    proportion = 100 * Numerator / Denominator
  )

print(sdg_4_2_2_location)

###  Wealth quintile
sdg_4_2_2_Wealth <- hl_422  %>%
  group_by(windex5) %>%  
  summarise(
    Numerator = sum(hhweight * organized_learning_current, na.rm = TRUE),
    Denominator = sum(hhweight, na.rm = TRUE),
    proportion = 100 * Numerator / Denominator
  )

print(sdg_4_2_2_Wealth)

# -----------------------------------------------------------------------------
# Save output to CSV
# -----------------------------------------------------------------------------

write_csv(sdg_4_2_2, "../Output/sdg_4_2_2_results.csv")

