
## Another program for creating descriptive statistics 
# JOCA-TAPOS 2023



## ----libraries-------------------------------------------------------------------------------------------------------------------------
library(readxl)
library(tidyverse)
library(viridis)
library(ggthemes)
library(ggplot2)
library(forcats)
library(knitr)
library(kableExtra)

# My colors:
SIAP.color <- "#0385a8"


## ----SDGData---------------------------------------------------------------------------------------------------------------------------

# ---  SDGs Data --- 

# Data extracted from  https://unstats.un.org/SDGS/Indicators/Database/?area=TTO
# Only countries with letters A-C selected to avoid uneccessary heavy file

# Poverty index  (Goal 1.1.1)  Proportion of population below poverty line
SDGdata1<- readxl::read_xlsx("Data/SDGDataSample.xlsx", 
                             col_names = TRUE, sheet = "Goal1")

SDGdata1$Poverty <- as.numeric(SDGdata1$Value)

# Select only a portion of the data
SDGPov <- subset(SDGdata1, Poverty >5 & Sex =="BOTHSEX" )
SDGPov.caption <- "Indicator 1.1.1 (Prop. of pop. below international poverty line)"


# Child Mortality (Goal 3.2.1)
SDGdata3.2<- readxl::read_xlsx("Data/SDGDataSample.xlsx", 
                             col_names = TRUE, sheet = "Goal3-2-1")

SDGdata3.2$ChildDeath <- as.numeric(SDGdata3.2$Value)

# Select only a portion of the data
SDGChildDeath <- subset(SDGdata3.2, ChildDeath >1 & Sex =="BOTHSEX" )
SDGChildDeath.caption <- SDGdata3.2$SeriesDescription[1]


# --- Here we generate a dataset with categorical values 
#  Data set with age recorded by country
set.seed(2512)
MyDataNumCat <- data.frame(
  Country=c( rep("A",500), rep("B",500), rep("B",500), rep("C",20), rep('D', 100)  ),
  Age=c( rnorm(500, 20, 5), rnorm(500, 13, 1), rnorm(500, 18, 1), rnorm(20, 25, 4), rnorm(100, 12, 1) )
)

#  Avg by Country (category)
MyDataNumCatAvg <- MyDataNumCat %>%
  group_by(Country) %>%
  summarize(
    Age.Avg = mean(Age)
  )



## ----TableFull, echo = FALSE, results=TRUE---------------------------------------------------------------------------------------------
 t <-  kbl(SDGPov[1:9,
                     c("Indicator","Age", "Poverty" ,"GeoAreaName")],
           caption = SDGPov.caption
)  

kable_styling(t,full_width = F)

# For interactive view of recording 
# View(SDGPov[c("Indicator","Age", "Poverty" )])


## ----TableUni, echo = FALSE, results=TRUE----------------------------------------------------------------------------------------------
 t1 <-  kbl(SDGPov[1:9,
                     c("Poverty")],
           caption = "Poverty Data (Sample)"
)  

kable_styling(t1,full_width = F)


## ----summaryOne------------------------------------------------------------------------------------------------------------------------

print(summary(SDGPov$Poverty))



## ----Histogram-------------------------------------------------------------------------------------------------------------------------
ggplot(data= SDGPov) +
 aes(x = Poverty) +
 geom_histogram(bins = 39L, fill = SIAP.color, aes(y=..density..) )+
   ggtitle("Histogram")+ 
  xlab(SDGPov.caption)+
 theme_tufte()


## ----Density---------------------------------------------------------------------------------------------------------------------------
ggplot(data= SDGPov) +
 aes(x = Poverty) +
    geom_density( fill=SIAP.color,color=SIAP.color, alpha=0.2) +
   ggtitle("Density estimate")+ 
  xlab(SDGPov.caption)+
 theme_tufte()



## ----Point-----------------------------------------------------------------------------------------------------------------------------
ggplot(data= SDGPov) +
 aes(x = Poverty, y ="") +
    geom_point(color=SIAP.color, size=0.9, alpha=0.9) +
   ggtitle("Points (raw values)")+ 
  xlab(SDGPov.caption)+
 theme_tufte()



## ----Rug-------------------------------------------------------------------------------------------------------------------------------
ggplot(data= SDGPov) +
 aes(x = Poverty) +
    geom_rug(color=SIAP.color, size=0.9, alpha=0.7, 
             length = unit(0.2, "npc")) +
   ggtitle("Rug plot")+ 
  xlab(SDGPov.caption)+
 theme_tufte() 
 
  



## ----Jitter----------------------------------------------------------------------------------------------------------------------------
ggplot(data= SDGPov) +
 aes(x = Poverty, y ="") +
    geom_jitter(color=SIAP.color, size=0.9, alpha=0.9,  height = 0.2 ) +
   ggtitle("Points (jitter)")+ 
  xlab(SDGPov.caption)+
 theme_minimal()+
  theme(panel.grid.major = element_blank(),
  panel.grid.minor = element_blank())



## ----Boxplot---------------------------------------------------------------------------------------------------------------------------
ggplot(data= SDGPov, aes(x = Poverty, y= "")) +
    geom_boxplot(width=0.3 ,fill=SIAP.color,color=SIAP.color,  alpha=0.2) +
  ggtitle("Boxplot (Box-and-Whiskers)")+ 
  xlab(SDGPov.caption)+
 theme_minimal()+
  theme(panel.grid.major = element_blank(),
  panel.grid.minor = element_blank())


## ----BoxplotOutliers-------------------------------------------------------------------------------------------------------------------
ggplot(data= SDGPov, aes(x = Poverty, y="")) +
    geom_boxplot(width=0.3 , fill=SIAP.color,color=SIAP.color, alpha=0.2,
                  outlier.colour = "red", outlier.alpha = 0.3) +
  ggtitle("Boxplot (Box-and-Whiskers)")+ 
  xlab(SDGPov.caption)+
 theme_minimal()+
  theme(panel.grid.major = element_blank(),
  panel.grid.minor = element_blank())


## ----BoxplotJitter---------------------------------------------------------------------------------------------------------------------
ggplot(data= SDGPov) +
 aes(x = Poverty, y ="") +
    geom_boxplot(width=0.3 ,fill=SIAP.color,color=SIAP.color, alpha=0.2,
                 outlier.colour = "red", outlier.alpha = 0.3)  +
    geom_jitter(color=SIAP.color, size=0.9, alpha=0.9, height = 0.1 ) +
   ggtitle("Horizontal Boxplot with points (jittered)")+ 
  xlab(SDGPov.caption)+
 theme_tufte()



## ----Densityrug------------------------------------------------------------------------------------------------------------------------
ggplot(data= SDGPov) +
 aes(x = Poverty) +
    geom_density( fill=SIAP.color,color=SIAP.color, alpha=0.2) +
    geom_rug(color=SIAP.color, size=0.9, alpha=0.7) +
   ggtitle("Density estimate and rug")+ 
  xlab(SDGPov.caption)+
 theme_tufte()



## ----TableCat, echo = FALSE, results=TRUE----------------------------------------------------------------------------------------------
t2 <- kable(MyDataNumCatAvg)
kable_styling(t2, full_width = F)


## ----Barplot---------------------------------------------------------------------------------------------------------------------------
# Barplot
ggplot(MyDataNumCatAvg, aes(x=Country, y=Age.Avg)) + 
  geom_bar(stat = "identity", fill = SIAP.color, alpha = 0.8)+
  ggtitle("Barplot")+ 
  xlab("Index by country") + 
  theme_minimal()


## ----Barplotflip-----------------------------------------------------------------------------------------------------------------------
# Barplot
ggplot(MyDataNumCatAvg, aes(x=Country, y=Age.Avg)) + 
  geom_bar(stat = "identity", fill = SIAP.color, alpha = 0.8)+
  coord_flip()+
  ggtitle("Horizontal Barplot")+ 
  xlab("Index by country") + 
  theme_minimal()


## ----Barplotfliporder------------------------------------------------------------------------------------------------------------------
# Reordering the bars 
MyDataNumCatAvg2 <- MyDataNumCatAvg
MyDataNumCatAvg2$Country <- with(MyDataNumCatAvg2, reorder(Country, Age.Avg, max ))

ggplot(MyDataNumCatAvg2, aes(x=Country, y=Age.Avg)) + 
  geom_bar(stat = "identity", fill = SIAP.color, alpha = 0.8)+
  coord_flip()+
  ggtitle("Horizontal Barplot (ordered)")+ 
  xlab("Index by country") + 
  theme_minimal()

## ----CircularBar, include=FALSE, results=FALSE-----------------------------------------------------------------------------------------
ggplot(MyDataNumCatAvg2, aes(x=Country, y=Age.Avg)) + 
  geom_bar(stat = "identity", fill = SIAP.color, alpha = 0.8)+
  # Limits of the plot = very important. The negative value controls the size of the inner circle, the positive one is useful to add size over each bar
 coord_polar(start = 0) +
  ggtitle("Circular Barplot")+ 
  xlab("Index by country")+
  # Custom the theme: no axis title and no cartesian grid
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-2,4), "cm") 
  ) 


## ----Lollipop--------------------------------------------------------------------------------------------------------------------------
ggplot(MyDataNumCatAvg, aes(x=Country, y=Age.Avg)) +
  geom_point(color=SIAP.color, size= 3,  alpha=0.9) + 
  geom_segment( aes(x=Country, xend=Country, y=0, yend=Age.Avg), color=SIAP.color) +
  ggtitle("Lollipop plot")+ 
  theme_minimal()


## ----Lollipopflip----------------------------------------------------------------------------------------------------------------------
ggplot(MyDataNumCatAvg2, aes(x=Country, y=Age.Avg)) +
  geom_point(color=SIAP.color, size= 3,  alpha=0.9) + 
  geom_segment( aes(x=Country, xend=Country, y=0, yend=Age.Avg), color=SIAP.color) +
  coord_flip() +
  ggtitle("Horizontal lollipop plot")+ 
  theme_minimal()


## ----Tablesummary, echo = FALSE, results=TRUE------------------------------------------------------------------------------------------
 MySum <- MyDataNumCat %>% 
  group_by(Country) %>% 
  summarize(Q1 = quantile(Age, 0.25),
            Mean = mean(Age),
            Median = median(Age),
            Q3 = quantile(Age, 0.75))

k <-kable(MySum, digits = 1)
kable_styling(k, full_width = F)


## ----Multibox--------------------------------------------------------------------------------------------------------------------------
# Plot
MyDataNumCat %>%
  ggplot( aes(x=Country, y=Age, fill=Country)) +
  geom_boxplot() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  coord_flip() +
  theme_minimal()+
  theme(legend.position = "none")+
  ggtitle("Boxplot by category (countries)") +
  xlab("")



## ----MultiboxJitter--------------------------------------------------------------------------------------------------------------------
# Plot
MyDataNumCat %>%
  ggplot( aes(x=Country, y=Age, fill=Country)) +
  geom_boxplot() +
  geom_jitter(color= SIAP.color, size=0.6, alpha=0.9) +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  coord_flip() +
  theme_minimal()+
  theme(legend.position = "none")+
  ggtitle("Boxplot by category (countries) with jitter") +
  xlab("")



## ----Ridgehisto------------------------------------------------------------------------------------------------------------------------
library(ggridges)
# Plot
  ggplot(MyDataNumCat,  aes(x = Age, y = Country, fill = Country)) +
  geom_density_ridges(stat="binline", bins=20) +
  theme_ridges() +
  theme(legend.position = "none")+
  ggtitle("Histogram by category (countries)") +
  xlab("Age")



## ----Ridgehistojitter------------------------------------------------------------------------------------------------------------------
library(ggridges)
# Plot
  ggplot(MyDataNumCat,  aes(x = Age, y = Country, fill = Country)) +
  geom_density_ridges(stat="binline", bins=20) +
  geom_jitter(color= SIAP.color, size=0.6, alpha=0.9) +
  theme_ridges() +
  theme(legend.position = "none")+
  ggtitle("Histogram by category (countries) with jitter ") +
  xlab("Age")



## ----Ridge-----------------------------------------------------------------------------------------------------------------------------
library(ggridges)
# Plot
  ggplot(MyDataNumCat,  aes(x = Age, y = Country, fill = Country)) +
  geom_density_ridges() +
  theme_ridges() +
  theme(legend.position = "none")+
  ggtitle("Density by category (countries) ") +
  xlab("Age")



## ----Ridgejitter-----------------------------------------------------------------------------------------------------------------------
library(ggridges)
# Plot
  ggplot(MyDataNumCat,  aes(x = Age, y = Country, fill = Country)) +
  geom_density_ridges() +
  geom_jitter(color= SIAP.color, size=0.6, alpha=0.9) +
  theme_ridges() +
  theme(legend.position = "none")+
  ggtitle("Density by category (countries) with jitter ") +
  xlab("Age")



## ----Violin----------------------------------------------------------------------------------------------------------------------------

# Plot
  ggplot(MyDataNumCat,  aes(x = Age, y = Country, fill = Country)) +
  geom_violin() +
  theme_minimal() +
  theme(legend.position = "none")+
  ggtitle("Violin plot by category (countries)") +
  xlab("Age")



## ----RidgeDensity----------------------------------------------------------------------------------------------------------------------
library(ggridges)
library(ggplot2)
library(viridis)


# Plot
ggplot(lincoln_weather, aes(x = `Mean Temperature [F]`, y = `Month`, fill = ..x..)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  scale_fill_viridis(name = "Temp. [F]", option = "C") +
  labs(title = 'Temperatures in a country for a given year') +
  theme_minimal() +
    theme(
      legend.position="none",
      panel.spacing = unit(0.1, "lines"),
      strip.text.x = element_text(size = 8)
    )


## ----PropData--------------------------------------------------------------------------------------------------------------------------
# We generate a dataset suitable for proportions
MyDataProp <- data.frame(
  Country = c("A", "B", "C", "D"),
  Population = c(100, 50, 120, 180)
)
kprop <-kable(MyDataProp, digits = 1)
kable_styling(kprop, full_width = F)



## ----Piechart--------------------------------------------------------------------------------------------------------------------------
# Basic pie chart
ggplot(MyDataProp, aes(x="", y=Population, fill=Country)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)+
  ggtitle("Pie chart") +
  theme_void() 



## ----Stackedbar------------------------------------------------------------------------------------------------------------------------
# Basic Stacked bar chart
ggplot(MyDataProp, aes(x="", y=Population, fill=Country)) +
  geom_bar(stat="identity", width=0.2) +
  coord_flip()+
  ggtitle("Horizontal stackbar") +
  theme_void()   
 


## ----Stackedbarordered, eval=FALSE, include=FALSE--------------------------------------------------------------------------------------
## # We need to reorder the file
## 
## MyDataProp2 <- MyDataProp
## MyDataProp2$Country <- with(MyDataProp2, reorder(Country, Population, max ))
## 
## # Bar chart
## ggplot(MyDataProp2, aes(x="", y=Population, fill=Country)) +
##   geom_bar(stat="identity", width=0.2) +
##   coord_flip()+
##   ggtitle("Horizontal stackbar (ordered)") +
##   theme_void()
## 


## ----Treemap---------------------------------------------------------------------------------------------------------------------------
library(treemap)
 
# Create data
group <- c("group-1","group-2","group-3")
value <- c(13,5,22)
data <- data.frame(group,value)
 
# treemap
treemap(MyDataProp,
            index="Country",vSize="Population",
            type="index", 
            palette = ggplotColours(n = 4), 
            title="A Treemap"                      
            )


## ----SDGMaternal-----------------------------------------------------------------------------------------------------------------------
# ---  SDGs Data --- 
SDGdata3<- readxl::read_xlsx("Data/SDGDataSample.xlsx", col_names = TRUE,
                             sheet = "Goal3")

SDGdata3$Maternal <- as.numeric(SDGdata3$Value)

SDGMatPov <- merge(SDGdata3, SDGPov, by = c("GeoAreaName", "TimePeriod" ))

# Creating and Age variable

SDGMatPov <- SDGMatPov %>%
  mutate( Age = str_sub(Age.y, 1,2),  #Take the lower bound of the interval
          Age = as.numeric(Age)
  )


Maternal.label.full <- SDGdata3$SeriesDescription[1]
Maternal.label <- "Maternal Mortality Indic. (3.1.1)"
Poverty.label.full <- SDGPov$SeriesDescription[1]
Poverty.label <- "Poverty Indic. (1.1.1)"



## ----Tablesummary2Var, echo = FALSE, results=TRUE--------------------------------------------------------------------------------------
 MySum2 <- SDGMatPov %>% 
  summarise_at(c("Poverty", "Maternal"), funs(mean= mean(., na.rm=TRUE), med = median(., na.rm=TRUE)))



## ---- results= "asis"------------------------------------------------------------------------------------------------------------------
library(summarytools)
descr(SDGMatPov[,c("Poverty","Maternal")], stats = "fivenum", 
      style = "rmarkdown", plain.ascii =FALSE,
      transpose = TRUE, headings = FALSE)


## ----Scatter---------------------------------------------------------------------------------------------------------------------------
ggplot(SDGMatPov, aes(x=Poverty, y= Maternal)) +
  geom_point(color = SIAP.color, , alpha = 0.99) +
  ggtitle("Simple Scatter plot") +
  xlab(Poverty.label)+
  ylab(Maternal.label)+
       theme_minimal()


## ----Scatteralpha----------------------------------------------------------------------------------------------------------------------
ggplot(SDGMatPov, aes(x=Poverty, y= Maternal)) +
  geom_point(color = SIAP.color, alpha = 0.4) +
  ggtitle("Simple Scatter plot") +
  xlab(Poverty.label)+
  ylab(Maternal.label)+
       theme_minimal()


## ----Scatterrug------------------------------------------------------------------------------------------------------------------------
ggplot(SDGMatPov, aes(x=Poverty, y= Maternal)) +
  geom_point(color = SIAP.color) +
  geom_rug(color = SIAP.color)+
  ggtitle("Scatter plot with rug") +
  xlab(Poverty.label)+
  ylab(Maternal.label)+
       theme_minimal()


## ----Scatterhisto----------------------------------------------------------------------------------------------------------------------
library(ggExtra)
p <- ggplot(SDGMatPov, aes(x=Poverty, y= Maternal)) +
  geom_point(color = SIAP.color) +
  ggtitle("Scatter plot with histograms") +
  xlab(Poverty.label)+
  ylab(Maternal.label)+
       theme_minimal()

ggExtra::ggMarginal(p, type = "histogram", fill=SIAP.color)



## ----Scatterbox------------------------------------------------------------------------------------------------------------------------
library(ggExtra)
p <- ggplot(SDGMatPov, aes(x=Poverty, y= Maternal)) +
  geom_point(color = SIAP.color) +
  ggtitle("Scatter plot with boxplot") +
  xlab(Poverty.label)+
  ylab(Maternal.label)+
       theme_minimal()

ggExtra::ggMarginal(p, type = "boxplot", fill=SIAP.color, alpha =0.5,
                    outlier.colour = SIAP.color, outlier.alpha = 0.3)



## ----Scatterdensity--------------------------------------------------------------------------------------------------------------------
library(ggExtra)
p <- ggplot(SDGMatPov, aes(x=Poverty, y= Maternal)) +
  geom_point(color = SIAP.color) +
  ggtitle("Scatter plot with boxplot") +
  xlab(Poverty.label)+
  ylab(Maternal.label)+
       theme_minimal()

ggExtra::ggMarginal(p, type = "density", fill=SIAP.color, alpha =0.5)



## ----Bubble----------------------------------------------------------------------------------------------------------------------------
library(stringr)

ggplot(SDGMatPov, aes(x=Poverty, y= Maternal, size = Age)) +
  geom_point(color = SIAP.color, alpha = 0.5) +
  ggtitle("Bubble plot") +
  xlab(Poverty.label)+
  ylab(Maternal.label)+
  xlim(0,80)+
   ylim(0,800)+
  theme_minimal() +
  theme(legend.position="bottom" )



## ----Histogram2D-----------------------------------------------------------------------------------------------------------------------
ggplot(SDGMatPov, aes(x=Poverty, y= Maternal)) +
         geom_bin2d(bins = 10) +
         scale_fill_continuous(type = "viridis") +
         theme_bw()+
  ggtitle("2D- Histogram plot") +
  xlab(Poverty.label)+
  ylab(Maternal.label)+
   xlim(0,80)+
   ylim(0,800)+
  theme_minimal() +
  theme(legend.position="bottom" )



## ----Density2D-------------------------------------------------------------------------------------------------------------------------

ggplot(SDGMatPov, aes(x=Poverty, y= Maternal)) +
 stat_density_2d(aes(fill = ..level..), geom = "polygon", colour="white") +
  ggtitle("2D- Density plot") +
  xlab(Poverty.label)+
  ylab(Maternal.label)+
  xlim(0,80)+
   ylim(0,800)+
  theme_minimal()+
  theme(legend.position="bottom" )



## ---- echo=FALSE, results=TRUE---------------------------------------------------------------------------------------------------------
# for LaTeX et xtable  use {r,  results= "asis"}
SDGMatPov.Time <- SDGMatPov %>%
  filter(GeoAreaName=="Brazil") %>%
  group_by(TimePeriod) %>%
  summarize(
    MatYear = mean(Maternal),
    PovYear = mean(Poverty)
  )
# print(SDGMatPov.Time)
#library(xtable)
# xtable(SDGMatPov.Time)

t <-  kbl(SDGMatPov.Time, digits = 1)

kable_styling(t,full_width = F)



## ----Lineplot--------------------------------------------------------------------------------------------------------------------------
ggplot(SDGMatPov.Time, aes(x=TimePeriod, y= PovYear)) +
#ggplot(toto, aes(x=TimePeriod, y= Poverty)) +
  geom_line(color = SIAP.color, alpha = 0.5, size = 1.5) +
  ggtitle("Line plot ") +
  xlab("Time")+
  ylab(Poverty.label)+
   theme_minimal()


## ----Connectedline---------------------------------------------------------------------------------------------------------------------
ggplot(SDGMatPov.Time, aes(x=TimePeriod, y= PovYear)) +
  geom_line(color = SIAP.color, alpha = 0.5, size =1.2) +
  geom_point(color = SIAP.color, alpha = 0.5, size = 3)+
  ggtitle("Connected Line plot ") +
  xlab("Time")+
  ylab(Poverty.label)+
       theme_minimal()


## ----Area------------------------------------------------------------------------------------------------------------------------------
# geom_area start at 0, so we use  geom_ribbon (with ymax in the aes)
ggplot(SDGMatPov.Time, aes(x=TimePeriod, ymax= PovYear)) +
  geom_line(aes(x=TimePeriod, y= PovYear), color = SIAP.color, size =1.2) +
  geom_ribbon(aes(ymin = 5.2), fill = SIAP.color, alpha = 0.3) +  
  geom_point(aes(x=TimePeriod, y= PovYear),color = SIAP.color, 
             alpha = 0.5, size = 2)+
  ggtitle("Area plot ") +
  xlab("Time")+
#  ylim(5.5, NA)+
  ylab(Poverty.label)+
       theme_minimal()


## ----datatime--------------------------------------------------------------------------------------------------------------------------
# create data
set.seed(2512)
time <- as.numeric(rep(seq(from = 2000,to = 2006, by=1), each=6) ) # x Axis
value <- runif(42, 10, 30)               # y Axis
country <- paste("Country", rep(LETTERS[1:6],times=7))# group, one shape per group
MyDataTime <- data.frame(time, value, country)
write.table(MyDataTime, "Data/MydataTime.csv")

print(MyDataTime %>% arrange(country,time)  %>% relocate(country, time))

#library(xtable)

#xtable(MyDataTime %>% arrange(country,time)  %>% relocate(country, time))


## ---- echo=FALSE, results= TRUE--------------------------------------------------------------------------------------------------------
MyDataLarge <-  spread(MyDataTime, key = "time", value = "value", fill = 0)
t <-  kbl(MyDataLarge, digits = 1, 
          caption = " Poverty rate indicator (1.1.1) for several countries, over 6 years")

kable_styling(t,full_width = F)


## ----Spaghetti-------------------------------------------------------------------------------------------------------------------------
# Spaghetti plot
 p<- ggplot(MyDataTime, aes(x=time, y=value, group=country, color = country)) + 
    geom_line() +
    ggtitle("Spaghetti plot ") +
    labs(caption = "Generated Data") +
    xlab("Time")+
    ylim(c(0, 40)) +
    ylab("Population below poverty line (millions)")+
    theme_minimal() +
  theme(
  plot.caption = element_text(color = "grey", face = "italic"))


## ---- eval = FALSE---------------------------------------------------------------------------------------------------------------------
## library(plotly)
## ggplotly(p)


## ----Spaghetti2------------------------------------------------------------------------------------------------------------------------
# Spaghetti plot
ggplot(MyDataTime, aes(x=time, y=value, group=country, color = country)) + 
    geom_line() +
    ggtitle("Spaghetti plot ") +
    labs(caption = "Generated Data") +
    xlab("Time")+
    ylim(c(0, 40)) +
    ylab("Population below poverty line (millions)")+
    theme_minimal() +
  theme(
  plot.caption = element_text(color = "grey", face = "italic"),
  legend.position="none")


## ----SpagettiGrey----------------------------------------------------------------------------------------------------------------------

MyDataTime$country2 <- MyDataTime$country

ggplot(MyDataTime, aes(x=time, y=value, group=country)) +
    geom_line(color="grey") +
    geom_line(data = subset(MyDataTime, country == "Country A"),
              aes(color = SIAP.color)) +
    ggtitle("Spaghetti plot with one country highlighted ") +
    labs(caption = "Generated Data") +
    xlab("Time")+
    ylim(c(0, 40)) +
    ylab("Population below poverty line (millions)")+
    theme_minimal() +
  theme(
  plot.caption = element_text(color = "grey", face = "italic"), 
  legend.position="none",
  panel.grid = element_blank()
  )



## ----LineFacet-------------------------------------------------------------------------------------------------------------------------
ggplot(MyDataTime, aes(x=time, y=value, group=country, color = country)) + 
    geom_line() +
    facet_wrap(~country)+
    ggtitle("Multiple line plot ") +
    labs(caption = "Generated Data") +
    xlab("Time")+
    ylim(c(0, 50)) +
    ylab("Pop. below poverty line (millions)")+
    theme_minimal() +
  theme(
  plot.caption = element_text(color = "grey", face = "italic"), 
  legend.position="none")



## ----LineFacetGrey---------------------------------------------------------------------------------------------------------------------

MyDataTime$country2 <- MyDataTime$country

ggplot(MyDataTime, aes(x=time, y=value)) + 
    geom_line(data = MyDataTime %>% dplyr::select(-country),
              aes(group=country2), 
              color="grey", size=0.5, alpha=0.7) +
    geom_line( aes(color=name), color=SIAP.color, size=1.2 )+
    facet_wrap(~country)+
    ggtitle("Multiple line plot ") +
    labs(caption = "Generated Data") +
    xlab("Time")+
    ylim(c(0, 50)) +
    ylab("Pop. below poverty line (millions)")+
    theme_minimal() +
  theme(
  plot.caption = element_text(color = "grey", face = "italic"), 
  legend.position="none",
  panel.grid = element_blank()
  )



## ----AreaFacet-------------------------------------------------------------------------------------------------------------------------
ggplot(MyDataTime, aes(x=time, y=value, group=country, fill = country)) + 
    geom_area(alpha = 0.8) +
    facet_wrap(~country)+
    ggtitle("Multiple area plot ") +
    labs(caption = "Generated Data") +
    xlab("Time")+
    ylim(c(0, 50)) +
    ylab("Pop. below poverty line (millions)")+
    theme_minimal() +
  theme(
  plot.caption = element_text(color = "grey", face = "italic"), 
  legend.position="none")



## ----StackedArea-----------------------------------------------------------------------------------------------------------------------


# stacked area chart
ggplot(MyDataTime, aes(x=time, y=value, fill=country)) + 
    geom_area(alpha=0.6 , size=.5, colour="white") +
    ggtitle("Stacked Area plot ") +
    labs(caption = "Generated Data") +
    xlab("Time")+
    ylab("Population below poverty line ")+
    theme_minimal() +
  theme(
  plot.caption = element_text(color = "grey", face = "italic"), 
  legend.position = "bottom")

