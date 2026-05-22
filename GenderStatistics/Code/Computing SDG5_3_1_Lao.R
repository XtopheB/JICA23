##============================================================================
# Calculation of SDG 5.3.1: Child Marriage
# ============================================================================
# This script demonstrates how to compute SDG 5.3.1 (Child Marriage)
# using MICS (2023) micro data for Lao PDR.   
# To run the codes, you should copy and paste the codes to the script editor 
# window (or directly to the console) in RStudio.

# 1. Load Required Packages ---------------------------------------------------
#pacman brings efficiency by installing, if missing and loading packages
#in one step; eliminates the need to use install.packages() and library()

#install.packages("pacman")    #install and load packages

pacman::p_load(
  haven,      #read data in SPSS format
  tidyverse   #collection of packages for data manipulation
)

# 2. Set Up File Paths------------------------------------------------------
# Define where our input and output files will be stored
base_path = "C:/Users/un157/OneDrive - United Nations/Desktop/Practical/LaoPDR"
input = file.path(base_path, "Data")
output = file.path(base_path, "Output")

# 3. Import data [wm.sav] --------------------------------------------------
#Create a data frame called df which stores the MICS data
df <- read_spss(file.path(input, "wm.sav"), user_na = TRUE)


# 4.Lets import and explore the dataset------------------------------------
# This data set has 476 Variables, but we only need 4 for this computation:
#WM17 - status of completion of the questionnaire
#WB4 - Age of woman
#WAGEM - Age at first marriage
#wmweight - sampling weight for women in the survey

#Check the class of the variables
class(df$WB4)
class(df$WAGEM) 

#Check structure of and labels of ALL the variables in the dataframe
glimpse(df)
names(df)

#Check structure and labels of WM17 variable
str(df$WM17)
table(df$WM17) #status of interview variable

#Lets explore age variable and check class
class(df$WB4) # age variable 
range(df$WB4, na.rm = TRUE)
summary(df$WB4)

#Lets explore wmweight and age of first marriage variables
class(df$wmweight)
summary(df$wmweight)
summary(df$WAGEM)


# 5.Extract the subset for analysis (women 20–24, completed interview)-----

wm_20_24 <- df %>%
  filter(
    WM17 == 1,          # women who completed interview
    WB4 >= 20,          # women age 20-24 years
    WB4 < 25
  )

# 6.Compute SDG 5.3.1 (national estimate) --------------------------------

sdg_5_3_1 <- wm_20_24 %>%
  summarise(
    indicator = "SDG 5.3.1",
    denominator = sum(wmweight, na.rm = TRUE),
    numerator   = sum(wmweight[WAGEM < 18], na.rm = TRUE),
    SDG_5_3_1    = 100 * numerator / denominator
  )

sdg_5_3_1


# 7.Disaggregation by location (urban / rural)----------------------------

sdg_5_3_1_location <- wm_20_24 %>%
  group_by(HH6) %>%   # 1 = urban, 2 = rural
  summarise(
    denominator = sum(wmweight, na.rm = TRUE),
    numerator   = sum(wmweight[WAGEM < 18], na.rm = TRUE),
    proportion  = numerator / denominator
  )

sdg_5_3_1_location
#For percentages
sdg_5_3_1_location <- sdg_5_3_1_location %>%
  mutate(proportion = proportion * 100)


# 8. Save Final Output as csv -------------------------------------------
results <- sdg_5_3_1 %>%
  mutate(
    country = "Lao PDR",
    year = 2023,
    unit = "Percent"
  )
write_csv(results, file.path(output, "SDG_5_3_1_results.csv"))

