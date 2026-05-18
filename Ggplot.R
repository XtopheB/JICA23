#### ----- A first program to use ggplot  ------
# for this program to run,  you'll need some packages (libraries)
# - tidyverse
# - ggplot 
# - model summmary
# plotly
####   ---  INSTALL THESE PACKAGES BEFORE ---


## ---- some Packages useful for Data Analysis and Graphics ------------------
# Working with data

# library(readxl)
library(tidyverse)

# Graphics
library(ggplot2)
library(plotly)

# Tables 
library(modelsummary)

## --- Here I define a color  I like for my graphics ---- 

# My colors:
SIAP.color <- "#0385a8"


## ------------ Loading the data  -----------------------------------
# file was saved on SIAP's server
 Mydata <- read.table(file ="https://www.unsiap.or.jp/on_line/2023/TAPOS/Data/M4-ggplotData.csv")

## --- When checking the data , not all variable have the right  "type" ----
# Age group  is has to be a factor
Mydata <- Mydata %>%  mutate( Age = as.factor(Age))


## ------ A look at data --------------------------------------------------------------------------------------------------------------
head(Mydata)


## ------- We can also use the package  modelsummary to produce a nice  table --------------------------------------------------------------------------------------------------------------
## Table
datasummary_skim(Mydata)

# Also for categorical variables 
datasummary_skim(Mydata, type = "categorical")


## -- Step by step construction of a ggplot  ----

## ----Scatter00-------------------------------------------------------------------------------------------------------------------------
ggplot(Mydata)


## ----Scatter0--------------------------------------------------------------------------------------------------------------------------
ggplot(Mydata) +
aes(x=Poverty, y= Maternal)


##   ----Scatter---------------------------------------------------------------------------------------------------------------------------
ggplot(Mydata) +
aes(x=Poverty, y= Maternal) +
geom_point()


## ----Scatter2--------------------------------------------------------------------------------------------------------------------------
ggplot(Mydata, aes(x=Poverty, y= Maternal)) +
  geom_point(color = "blue") +
  ggtitle("Simple scatter plot")


## ----Scatter3--------------------------------------------------------------------------------------------------------------------------
ggplot(Mydata, aes(x=Poverty, y= Maternal, colour = Country)) +
  geom_point() +
  ggtitle("Simple scatter plot with colored encoding 'countries'")


## ----Scatter4--------------------------------------------------------------------------------------------------------------------------
ggplot(Mydata, aes(x=Poverty, y= Maternal, shape = Age)) +
  geom_point() +
  ggtitle("Simple scatter plot with shapes encoding 'Age'")


## ----Scatter5--------------------------------------------------------------------------------------------------------------------------
ggplot(Mydata, aes(x=Poverty, y= Maternal, size = Population)) +
  geom_point() +
  ggtitle("Simple scatter plot with size encoding 'population'")


## ----Scatter6--------------------------------------------------------------------------------------------------------------------------
ggplot(Mydata, aes(x=Poverty, y= Maternal, size = Population, colour = Country, shape = Age)) +
  geom_point() +
  ggtitle("Simple scatter plot combining the features")


## ----boxplot---------------------------------------------------------------------------------------------------------------------------
ggplot(Mydata, aes(x=Poverty)) +
  geom_boxplot() +
  ggtitle("Simple box plot")


## ----jitter----------------------------------------------------------------------------------------------------------------------------
ggplot(Mydata, aes(x=Poverty, y ="")) +
  geom_jitter() +
  ggtitle("Jitter points") 
  

## ----Boxjitter-------------------------------------------------------------------------------------------------------------------------
ggplot(Mydata, aes(x=Poverty, y ="")) +
   geom_jitter() +
   geom_boxplot() +
  ggtitle("Boxplot and jitter")


## ----Boxjitter2------------------------------------------------------------------------------------------------------------------------
ggplot(Mydata, aes(x=Poverty, y ="")) +
  geom_boxplot() +
  geom_jitter() +
  ggtitle("Boxplot and jitter")


## ----scatterfacet----------------------------------------------------------------------------------------------------------------------
ggplot(Mydata, aes(x=Poverty, y= Maternal)) +
  geom_point(alpha = 0.2) +
  facet_wrap(~Country,nrow = 2)+
  ggtitle("Faceted graphics ")+theme_minimal()


## ----aes1------------------------------------------------------------------------------------------------------------------------------

p <- Mydata %>%
ggplot(aes(x=Poverty, y= Maternal)) +
  geom_point(color =SIAP.color) +
  ggtitle(paste("Simple scatter plot with blue points (n = ",
                nrow(Mydata), ")")) +
  theme_minimal()

ggplotly(p)

## ----aes2------------------------------------------------------------------------------------------------------------------------------
 ggplot(Mydata, aes(x=Poverty, y= Maternal)) +
   geom_point(aes(color = "Blue")) +
   ggtitle("Simple scatter plot with ... red points?")


## ----aes3------------------------------------------------------------------------------------------------------------------------------

ggplot(Mydata, aes(x=Poverty, y= Maternal)) +
  geom_point(aes(color = Age)) +
  ggtitle("Simple scatter plot with color mapped with Age")




## ----Zoom1-----------------------------------------------------------------------------------------------------------------------------
ggplot(Mydata, aes(x=Poverty, y= Maternal)) +
  geom_point() +
  ggtitle("Simple scatter plot with default axis")


## ----Zoom2-----------------------------------------------------------------------------------------------------------------------------
ggplot(Mydata, aes(x=Poverty, y= Maternal)) +
  geom_point() +
  ylim(0, 600)+
  ggtitle("Simple scatter plot with customized X axis zoomed ")


## ----Zoom3-----------------------------------------------------------------------------------------------------------------------------
ggplot(Mydata, aes(x=Poverty, y= Maternal)) +
  geom_point() +
  geom_smooth(method = "lm")+
  #geom_hline(yintercept = 600, colour = "red") +
  ggtitle("Simple scatter plot with regression line ")


## ----zoom4-----------------------------------------------------------------------------------------------------------------------------
ggplot(Mydata, aes(x=Poverty, y= Maternal)) +
  geom_point() +
  geom_hline(yintercept = 600, colour = "red") +
  ylim(c(0, 600)) +
  geom_smooth(method = "lm")+
  ggtitle("Simple scatter plot with customized X axis using xlim()")


## ----zoom5-----------------------------------------------------------------------------------------------------------------------------
ggplot(Mydata, aes(x=Poverty, y= Maternal)) +
  geom_point() +
  geom_hline(yintercept = 600, colour = "red") +
  scale_y_continuous(limits =c(0, 600))+
  geom_smooth(method = "lm") +
  ggtitle("Simple scatter plot with customized X axis using scale_y_continuous()")


## ----zoom6-----------------------------------------------------------------------------------------------------------------------------
ggplot(Mydata, aes(x=Poverty, y= Maternal)) +
  geom_point() +
  geom_hline(yintercept = 600, colour = "red") +
  coord_cartesian(ylim =c(0, 600))+
  geom_smooth(method = "lm") +
  ggtitle("Simple scatter plot with customized X axis using coord_cartesian()")


## ----zoom8-----------------------------------------------------------------------------------------------------------------------------

fit1 <- lm(Maternal ~ Poverty, data = Mydata)

predicted_df <- data.frame(Maternal_pred = predict(fit1, Mydata), Poverty = Mydata$Poverty)

ggplot(Mydata, aes(x=Poverty, y= Maternal)) +
 ylim(c(0, 600)) +
  geom_point() +
  geom_smooth(method = "lm") +
  geom_line(data = predicted_df, aes(x = Poverty , y =Maternal_pred),
            colour = "darkorange2", size  = 1.5) +
  
  ggtitle("Comparing the regressions with different zooms in the plat") +
  theme_minimal()


## ----zoom7-----------------------------------------------------------------------------------------------------------------------------
ggplot(Mydata, aes(x=Poverty, y= Maternal)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~Period)+
  ggtitle("Faceted graphics with nonparametric regressions")


## ----labels1---------------------------------------------------------------------------------------------------------------------------
ggplot(Mydata, aes(x=Poverty, y= Maternal)) +
  geom_point() +
  labs( x=  "Poverty Indic. (1.1.1)", 
        y = "Maternal Mortality Indic. (3.1.1)") +
  ggtitle("Simple scatter plot with a nice theme")




## ----theme1----------------------------------------------------------------------------------------------------------------------------
ggplot(Mydata, aes(x=Poverty, y= Maternal)) +
  geom_point() +
  ggtitle("Simple scatter plot with a nice theme") +
   theme_bw()
  # theme_base()
  # theme_minimal()
  # theme_tufte()



## ----theme2----------------------------------------------------------------------------------------------------------------------------
ggplot(Mydata, aes(x=Poverty, y= Maternal)) +
  geom_point() +
  ggtitle("Simple scatter plot a nice theme") +
  theme_minimal()



## --------------------------------------------------------------------------------------------------------------------------------------

# Colour, shape and alpha-transparency are set for the point geometry 
ggplot(Mydata, aes(x=Poverty, y= Maternal, size=Population)) +
  geom_point(colour =  "blue", alpha = 0.5, shape= 5) +
  ggtitle("Scatter plot where colour, shape and alpha-transparency are set for the point geometry ") +
  theme_minimal()


## --------------------------------------------------------------------------------------------------------------------------------------
# p is my graphic

p <- ggplot(Mydata, aes(x=Poverty, y= Maternal)) +
  geom_point(aes(color = Country)) +
  ggtitle("Simple scatter plot with a nice theme") +
  theme_minimal() +
  theme(legend.position = "bottom", 
        legend.title = element_text(color = "red", size = 18))

# plotting the graphic
p


## --------------------------------------------------------------------------------------------------------------------------------------
# Interactive version of previous graphic

ggplotly(p)


## --------------------------------------------------------------------------------------------------------------------------------------
knitr::knit_exit()

