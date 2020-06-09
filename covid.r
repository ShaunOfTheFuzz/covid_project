# packages

library(ggplot2)
library(scales)

# data (wrangled in python)
agedata <- demographics_final

# ordering the data by highest percentage of 60 years and over
ordering <- select(finalr, country, over_sixty_perc)
ordering <- ordering[order(-ordering$over_sixty_perc),]

# applying ordering to the country_region factors
agedata$Country_Region <- factor(agedata$Country_Region, levels = ordering$country)

# reordering the dataframe and assigning to new dataframe
dataslice <- with(agedata, agedata[order(Country_Region, Age, Values, relative_pop),])

####################### inputs ############################

#  select number of countries  ########## (should be input) ###########
n=20
# country vector based on inputs from a checklist ######## (implement checklist) #########
countrylist = c('Monaco',"Australia","United Arab Emirates",'Ireland')

# A vector containing all the countries
allcountry = dataslice$Country_Region

# using n to choose how many countries to plot ######### (temp, need to select countries from input) ########
#dataslice <- head(dataslice,n*7)

####################### end inputs ########################

#dataslice <- subset(dataslice, Country_Region == "Australia")
dataslice <- dataslice[dataslice$Country_Region %in% countrylist,]

dataslice
# create a dataset
country <- dataslice$Country_Region
value <- dataslice$Values
age <- dataslice$Age

# Stacked + percent
p <- ggplot(dataslice, aes(fill=forcats::fct_rev(age), x = country, y = value)) + 
  geom_bar(position="fill", stat="identity") #+ geom_line(aes(x=country,y=relative_pop, group = 1), color = "red4",lwd = 1.25)

demoplot <- p + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.2))

demoplot