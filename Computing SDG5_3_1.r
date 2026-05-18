# Please refer to the R demonstration file for the detailed explanations of the 
# codes presented here. 
# To run the codes, you should copy and paste the codes to the script editor 
# window (or directly to the console) in RStudio.

###############################################################################
#Calculation of SDG 5.3.1: Child Marriage 
###############################################################################

# install.packages("haven")

#For this exercise we will need the haven package to read SPSS files
library(haven)

#Check and set current directory (location where you files have been saved)
getwd()

#To set the file path to the correct folder
#setwd("~/Library/CloudStorage/OneDrive-UnitedNations/3.8 TAPOS/2025/Practical")  

###############################################################################
#Lets import and explore the dataset 
###############################################################################

#Create a data frame called wm which stores the MICS data
df <- read_spss(file ="Data/wm.sav", user_na = TRUE)

#Variables needed for this exercise are
#WM17 - status of completion of the questiannaire
#WB4 - Age of woman
#WAGEM - Age at first marriage
#Hwmweight - sampling weight for women in the survey
#Check the class of the variables

#Check structure of and labels of ALL the variables in the dataframe
str(df)
names(df)

#Check structure and labels of WM17 variable
str(df$WM17)
table(df$WM17) #status of interview variable

#Lets explore age variable and check class
class(df$WB4) # age variable 
range(df$WB4, na.rm = TRUE)
summary(df$WB4)

#Lets explore wmweight and age of first narriage variables
class(df$wmweight)
summary(df$wmweight)
summary(df$WAGEM)

###############################################################################
#Computing SDG5.3.1 - Child Marriage (Nepal 2019)  
#Numerator:Estimate number of women 20 to 24 who were married before the age of 18
#Denominator:Women 20 to 24 years
#Formula = Numerator/Denominator
###############################################################################
#For the denominator 
#Extract from data frame, wm,  women 20 - 24 years who completed the interview
wmcompleted <- subset(df, (WM17==1) & WB4>=20 & WB4<25) 
num_women <- sum(wmcompleted$wmweight)

#For the numerator 
age18 <- subset(wmcompleted, WAGEM<18)
num_child_marriage <- sum(age18$wmweight)

#Proportion of women 20-24 years, who were married before the age of 18
SDG.5.3.1 <- num_child_marriage/num_women

###############################################################################
#Computing disaggregates by location (urban/ rural)  
#Numerator: Estimate of women (20-24 yrs) married before age 18 by location
#Denominator: Women 20 - 24 years
#Formula = Numerator/Denominator
###############################################################################

str(df$HH6)
#For denominator 
#Women aged 20 - 24 living in urban areas
denominatoru <- subset(df, (WB4>=20 & WB4<25) & (HH6==1))

#Women aged 20 - 24 living in rural areas
denominatorr <- subset(df, (WB4>=20 & WB4<25) & (HH6==2))

#For Numerator
#Women aged 20 - 24 years who were married before age 18 and living in urban areas
numeratoru <- subset(denominatoru, WAGEM<18)

#omen 20 - 24 years who were married before age 18 and living in in rural
numeratorr <- subset(denominatorr, WAGEM<18)

#Computing proportions by location (urban/ rural)
#Urban areas
sum(numeratoru$wmweight)/sum(denominatoru$wmweight)

#Rural areas
sum(numeratorr$wmweight)/sum(denominatorr$wmweight)

### Outputs
 write( SDG.5.3.1, file = "Outputs/SDG.txt")

## More advanced 
Myoutput <- paste("The SDG5.3.1 (complete dataset) is:", SDG.5.3.1) 
write(Myoutput, file = "Outputs/SDG.txt", append = TRUE)


