#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/

options(max.print=1000000)

# load required packages
library(magrittr)
library(dplyr)
library(maps)
library(ggplot2)
library(reshape2)
library(ggiraph)
library(RColorBrewer)
library(leaflet)
library(plotly)
library(geojsonio)
library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(shinythemes)
library(scales)
library(rstudioapi)
library(gridExtra)
library(rgdal)

# set working dir to local
current_path <- getActiveDocumentContext()$path 
setwd(dirname(current_path ))

# data (wrangled in python)
agedata <- read.csv("demographics_final.csv")
finalr <- read.csv("finalr.csv")
worldcountry = geojson_read("countries.geo.json", what = "sp")
country_geoms = read.csv("country_geoms.csv")
finalr_choropleth_match = read.csv("finalr_choropleth_match.csv")

# ordering the data by highest percentage of 60 years and over
ordering <- select(finalr, country, over_sixty_perc)
ordering <- ordering[order(-ordering$over_sixty_perc),]
finalr$country <- factor(finalr$country, levels = ordering$country)
metrics <- with(finalr, finalr[order(country, est_death_percent,deaths_per_conf_perc,deaths_per_10k),])
agedata$Country_Region <- factor(agedata$Country_Region, levels = ordering$country)
dataslice <- with(agedata, agedata[order(Country_Region, Age, Values, relative_pop),])
metrics$official_vs_predict = metrics$deaths_per_conf_perc-metrics$est_death_percent

####################### plot functions ############################

demographicsplot <- function(selection_var){
  dataslice <- dataslice[dataslice$Country_Region %in% selection_var,]
  country <- dataslice$Country_Region
  value <- dataslice$Values
  age <- dataslice$Age
  p <- ggplot(dataslice, aes(fill=forcats::fct_rev(age), x = country, y = value)) + 
    geom_bar(position="fill", stat="identity") + 
    theme(legend.text=element_text(size=12),legend.title=element_text(size=14),axis.title.y = element_text(size = 14),axis.text.y = element_text(angle = 90, hjust = 0.5, vjust=0)) +
    theme(axis.title.x = element_text(size = 20),axis.text.x.top = element_text(angle = 90, hjust = 0, vjust= 0.4), legend.position = 'top') +
    scale_x_discrete(position = "top") + labs(fill = "Age Categories") + 
    xlab(expression("Older Population   "  %<-% "     Country     " %->%  "   Younger Population")) +
    ylab("Age Demographics")
  p
}

offical_pred_plot <- function(selection_var){
  metrics <- metrics[metrics$country %in% selection_var,]
  country <- metrics$country
  p <- ggplot(metrics, aes(x = country, y = deaths_per_conf_perc, fill = deaths_per_conf_perc)) + 
    geom_bar(stat="identity") +
    scale_fill_gradientn(colours=c("green","yellow","orange", "red"), limits = c(0, 20))+ 
    theme(legend.text=element_text(size=12),legend.title=element_text(size=14),axis.title.x = element_text(size = 20),axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.2), legend.position = "bottom") +
    theme(axis.title.y = element_text(size = 14),axis.text.y = element_text(angle = 90, hjust = 0.5, vjust=0)) + labs(fill = "Deaths per reported cases (%)") +
    xlab(expression("More Vulnerable population   "  %<-% "     Country     " %->%  "   Less Vulnerable Population")) +
    ylab("Reported deaths/cases (%)") + 
    geom_errorbar(aes(ymin = est_death_percent, ymax = est_death_percent, color = "Estimated deaths per actual cases (%)"),size = 1.25) + 
    scale_color_manual(name = " ", values = "#666666") +
    guides(colour = guide_legend(override.aes = list(linetype = "solid")))
  
  p
}

############################### Choropleth ########################################

world_map <- readOGR("ne_50m_admin_0_countries.shp")
rates <- finalr_choropleth_match$deaths_per_conf_perc[match(world_map$admin, finalr_choropleth_match$country)]
bins = c(0,1,2,4,6,8,10,12,15,20)
qpal <- colorBin("Reds", domain = finalr_choropleth_match$deaths_per_conf_perc, bins = bins)

basemap = leaflet(world_map) %>% # create a blank canvas
  addTiles() %>% # add tile
  setView( lat=20, lng=5 , zoom=2) %>%
  addPolygons( # draw polygons on top of the base map (tile)
    stroke = FALSE, 
    smoothFactor = 0.2, 
    fillOpacity = 0.5,
    color = ~qpal(rates)) %>%
  addLegend("bottomleft",values = rates,pal = qpal, title = "Deaths/Confirmed Cases", na.label = "No data")


################################### shiny UI ######################################

# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("united"),

  navbarPage(collapsible = TRUE,
             "Covid Response", id="nav",
             
             tabPanel("Covid Response Map",
                      leafletOutput("mymap", height=800)
             ),
             
             tabPanel("Demographics and death rate",
                      
                      sidebarLayout(
                        sidebarPanel("width"=3,
                          
                          pickerInput('countries',"Select Countries:",   
                                      choices = as.character(finalr$country), 
                                      options = list(`actions-box` = TRUE, `none-selected-text` = "Select one or more"),
                                      selected = c('Monaco','US','China','UK','Italy','Spain','Australia','South Korea'),
                                      multiple = TRUE), 
                          "Select one or more Countries for demographics and deaths/confirmed cases"
                        ),
                        
                        mainPanel(
                          fluidRow(
                            verticalLayout(plotOutput("demographs"), plotOutput("pred_plot")))
                        )
                      )
             ),
    
              tabPanel("Data",
                       numericInput("maxrows", "Rows to show", 30),
                       verbatimTextOutput("rawtable")
              )
  )
   
)

############################### shiny server ######################################

# Define server logic required to draw a histogram
server <- function(input, output) {
   
  output$rawtable <- renderPrint({
    orig <- options(width = 1000)
    print(head(finalr %>% select(colnames(finalr)), input$maxrows), row.names = FALSE)
    options(orig)
  })
  
  output$demographs <- renderPlot(
    demographicsplot(input$countries)
  )
  
  output$pred_plot <- renderPlot(
    offical_pred_plot(input$countries)
  )
  
  output$mymap <- renderLeaflet({ 
    basemap
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

