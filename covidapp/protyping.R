# packages

library(ggplot2)
library(scales)
library(rstudioapi)
library(gridExtra)

# set working dir to local
current_path <- getActiveDocumentContext()$path 
setwd(dirname(current_path ))

# data (wrangled in python)
agedata <- read.csv("demographics_final.csv")
finalr <- read.csv("finalr.csv")

# ordering the data by highest percentage of 60 years and over
ordering <- select(finalr, country, over_sixty_perc)
ordering <- ordering[order(-ordering$over_sixty_perc),]
finalr$country <- factor(finalr$country, levels = ordering$country)
metrics <- with(finalr, finalr[order(country, est_death_percent,deaths_per_conf_perc,deaths_per_10k),])
agedata$Country_Region <- factor(agedata$Country_Region, levels = ordering$country)
dataslice <- with(agedata, agedata[order(Country_Region, Age, Values, relative_pop),])
metrics$official_vs_predict = metrics$deaths_per_conf_perc-metrics$est_death_percent

####################### inputs ############################

countrylist = c('Monaco',"Australia","United Arab Emirates",'Ireland', 'US', 'Korea, South', 'United Kingdom')
allcountry = dataslice$Country_Region

demographicsplot <- function(selection_var){
  dataslice <- dataslice[dataslice$Country_Region %in% selection_var,]
  country <- dataslice$Country_Region
  value <- dataslice$Values
  age <- dataslice$Age
  p <- ggplot(dataslice, aes(fill=forcats::fct_rev(age), x = country, y = value)) + 
    geom_bar(position="fill", stat="identity") + 
    theme(axis.text.y = element_text(angle = 90, hjust = 0.5, vjust=0)) +
    theme(axis.text.x.top = element_text(angle = 90, hjust = 0, vjust= 0.4), legend.position = 'top') +
    scale_x_discrete(position = "top")
  p
}

demographicsplot_num <- function(number_countries){
  dataslice <- head(dataslice,number_countries*7)
  country <- dataslice$Country_Region
  value <- dataslice$Values
  age <- dataslice$Age
  p <- ggplot(dataslice, aes(fill=forcats::fct_rev(age), x = country, y = value)) + 
    geom_bar(position="fill", stat="identity") + 
    theme(axis.text.y = element_text(angle = 90, hjust = 0.5, vjust=0)) +
    theme(axis.text.x.top = element_text(angle = 90, hjust = 0, vjust= 0.4), legend.position = 'top') +
    scale_x_discrete(position = "top")
  p
}

offical_pred_plot <- function(selection_var){
  metrics <- metrics[metrics$country %in% selection_var,]
  country <- metrics$country
  p <- ggplot(metrics, aes(x = country, y = official_vs_predict, fill = official_vs_predict)) + 
    geom_bar(stat="identity") + 
    scale_fill_gradientn(colours=c("green","yellow","orange", "red"), limits = c(-2, 20))+ 
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.2), legend.position = "bottom") +
    theme(axis.text.y = element_text(angle = 90, hjust = 0.5, vjust=0))
    
  p
}

ex1 <- demographicsplot(allcountry)
ex2 <- offical_pred_plot(allcountry)
grid.arrange(ex1, ex2, nrow = 2)