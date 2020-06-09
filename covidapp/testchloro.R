
library(magrittr)
library(rvest)
library(readxl)
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

#m <- leaflet() %>% 
 # setView(lng = 0.0, lat = 0.0, zoom = 2) %>% 
  #addTiles()
#m %>% addProviderTiles("Stamen.Toner")
#m

worldcountry = geojson_read("countries.geo.json", what = "sp")
country_geoms = read.csv("country_geoms.csv")
finalr_choropleth_match = read.csv("finalr_choropleth_match.csv")





leaflet(finalr) %>% addTiles() %>%
  addMarkers(~long, ~lat, popup = ~as.character(round(deaths_per_conf_perc,1)))



data <- read.csv("Household-heating-by-State-2008.csv", header=T) 
names(data)[4] <- "MobileHomes"
ag <- aggregate(MobileHomes ~ States, FUN = mean, data = data)
ag$States <- tolower(ag$States)

library(maps)
mapStates <- map("state", fill = TRUE, plot = FALSE)
# find the related rate for each state
rates <- ag$MobileHomes[match(mapStates$names, ag$States)] 

library(leaflet)
cpal <- colorNumeric("Reds", rates) # prepare the color mapping
leaflet(mapStates) %>% # create a blank canvas
  addTiles() %>% # add tile
  addPolygons( # draw polygons on top of the base map (tile)
    stroke = FALSE, 
    smoothFactor = 0.2, 
    fillOpacity = 1,
    color = ~cpal(rates) # use the rate of each state to find the correct color
  ) 


library(shiny)
library(leaflet)

ui <- fluidPage(
  # create map canvas on the page
  leafletOutput("mymap"), 
  # create a button, and bind it to the recalc event
  actionButton("recalc", "New points") 
)

server <- function(input, output, session) {
  # event handle, in this case for click event
  points <- eventReactive(input$recalc, { 
    # calculate normal distribution random points around Melbourne
    cbind(rnorm(40) * 3 + 145.0431, rnorm(40) -37.8773) 
  }, ignoreNULL = FALSE)
  
  output$mymap <- renderLeaflet({ # create leaflet map
    leaflet() %>% 
      addTiles() %>%
      # use the random generated points as markers on the map
      addMarkers(data = points()) 
  })
}

shinyApp(ui, server)






# Create a color palette with handmade bins.
library(RColorBrewer)
mybins <- c(0,10,20,50,100,500,Inf)
mypalette <- colorBin( palette="YlOrBr", domain=world_spdf@data$POP2005, na.color="transparent", bins=mybins)

# Prepare the text for tooltips:
mytext <- paste(
  "Country: ", world_spdf@data$NAME,"<br/>", 
  "Area: ", world_spdf@data$AREA, "<br/>", 
  "Population: ", round(world_spdf@data$POP2005, 2), 
  sep="") %>%
  lapply(htmltools::HTML)

# Final Map
leaflet(world_spdf) %>% 
  addTiles()  %>% 
  setView( lat=10, lng=0 , zoom=2) %>%
  addPolygons( 
    fillColor = ~mypalette(POP2005), 
    stroke=TRUE, 
    fillOpacity = 0.9, 
    color="white", 
    weight=0.3,
    label = mytext,
    labelOptions = labelOptions( 
      style = list("font-weight" = "normal", padding = "3px 8px"), 
      textsize = "13px", 
      direction = "auto"
    )
  ) %>%
  addLegend( pal=mypalette, values=~POP2005, opacity=0.9, title = "Population (M)", position = "bottomleft" )

m  

# save the widget in a html file if needed.
# library(htmlwidgets)
# saveWidget(m, file=paste0( getwd(), "/HtmlWidget/choroplethLeaflet5.html"))








library(rgdal)
world_map <- readOGR("ne_50m_admin_0_countries.shp")

#rates <- gdp_data$GDP[match(world_map$admin, finalr_choropleth_match$country)]






# create plotting parameters for map
bins = c(0,1,10,50,100,500)
cv_pal <- colorBin("Oranges", domain = finalr_choropleth_match$deaths_per_conf_perc, bins = bins)
plot_map <- world_map[world_map$admin %in% finalr_choropleth_match$country, ]

# creat cv base map 
basemap = leaflet(plot_map) %>% 
  addTiles() %>% 
  addLayersControl(
    position = "bottomright",
    overlayGroups = c("2019-COVID (active)", "2019-COVID (new)", "2019-COVID (cumulative)", "2003-SARS", "2009-H1N1 (swine flu)", "2014-Ebola"),
    options = layersControlOptions(collapsed = FALSE)) %>% 
  hideGroup(c("2019-COVID (new)", "2019-COVID (cumulative)", "2003-SARS", "2009-H1N1 (swine flu)", "2014-Ebola"))  %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  fitBounds(~-100,-50,~80,80) %>%
  addLegend("bottomright", pal = cv_pal, values = ~finalr_choropleth_match$deaths_per_conf_perc,
            title = "<small>Active cases per 100,000</small>") #%>%
#fitBounds(0,-25,90,65) # alternative coordinates for closer zoom













############################### Chloropleth template ###############################

library(rgdal)
world_map <- readOGR("ne_50m_admin_0_countries.shp")
# create plotting parameters for map
bins = c(0,2,4,6,8,10,12,14,16,18,20)
cv_pal <- colorBin("Oranges", domain = finalr_choropleth_match$deaths_per_conf_perc, bins = bins)

plot_map <- finalr_choropleth_match$deaths_per_conf_perc[match(world_map$admin, finalr_choropleth_match$country)]
#plot_map <- world_map[world_map$admin %in% finalr_choropleth_match$country, ]

# creat cv base map 
leaflet(plot_map) %>% 
  addTiles() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  fitBounds(~-100,-50,~80,80) %>%
  addLegend("bottomleft", pal = cv_pal, values = ~finalr_choropleth_match$deaths_per_conf_perc,
            title = "<small>Deaths per confirmed cases</small>") #%>%
#fitBounds(0,-25,90,65) # alternative coordinates for closer zoom



################################### shiny UI ######################################

















#libraries
library(data.table)
library(sp)
library(rgdal)
library(dplyr)
library(ggpubr)
library(grid)
library(leaflet)

#read in data table
chi_dat = fread("finalr_choropleth_match.csv")

#convert to data table
chi_tab = as.data.table(chi_dat)


#make data spatial
coordinates(chi_tab) = c("google_longitude","google_latitude")
crs.geo1 = CRS("+proj=longlat")
proj4string(chi_tab) = crs.geo1

plot(chi_tab, pch = 20, col = "steelblue")

#read in shapefile of chicago
chicago = readOGR(dsn = "./communities", layer = "geo_export_84596be1-10fc-47e4-a7d0-36d6c9a9e0a3")


#leaflet map -- # of incidents
#reproject coordinates
proj4string(chicago) = crs.geo1

chi_agg = aggregate(x=chi_tab["incidents_people"],by=chicago,FUN=length)

qpal = colorBin("Reds", chi_agg$incidents_people, bins=4)

leaflet(chi_agg) %>%
  addPolygons(stroke = TRUE,opacity = 1,fillOpacity = 0.5, smoothFactor = 0.5,
              color="black",fillColor = ~qpal(incidents_people),weight = 1) %>%
  addLegend(values=~incidents_people,pal=qpal,title="Incidents")



#table missing 7 communities (all with zero incidents)
#seven NAs: OHARE (75) = 0
chi_agg@data$incidents_people[75] = as.integer(0)
chi_agg@data$incidents_people[77] = as.integer(0)
chi_agg@data$incidents_people[73] = as.integer(0)
chi_agg@data$incidents_people[36] = as.integer(0)
chi_agg@data$incidents_people[12] = as.integer(0)
chi_agg@data$incidents_people[71] = as.integer(0)
chi_agg@data$incidents_people[13] = as.integer(0)

bins <- c(0, 10, 20, 30, 40, 50, 60, 70, 80)
qpal = colorBin("Reds", chi_agg$incidents_people, bins=bins)

labels <- sprintf(
  "<strong>%s</strong>",
  chi_agg$incidents_people
) %>% lapply(htmltools::HTML)

leaflet(chi_agg) %>%
  addPolygons(stroke = TRUE,opacity = 1,fillOpacity = 0.5, smoothFactor = 0.5,
              color="black",fillColor = ~qpal(incidents_people),weight = 1, label = labels,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>%
  addLegend(values=~incidents_people,pal=qpal,title="Incidents")




































# data (wrangled in python)
agedata <- read.csv("demographics_final.csv")
finalr <- read.csv("finalr.csv")
worldcountry = geojson_read("countries.geo.json", what = "sp")
country_geoms = read.csv("country_geoms.csv")
finalr_choropleth_match = read.csv("finalr_choropleth_match.csv")

library(rgdal)
library(leaflet)

world_map <- readOGR("ne_50m_admin_0_countries.shp")
rates <- finalr_choropleth_match$deaths_per_conf_perc[match(world_map$admin, finalr_choropleth_match$country)]
bins = c(0,1,2,4,6,8,10,12,15,20)
qpal <- colorBin( "Reds", domain = finalr_choropleth_match$deaths_per_conf_perc, bins = bins)

leaflet(world_map) %>% # create a blank canvas
  addTiles() %>% # add tile
  setView( lat=20, lng=5 , zoom=2) %>%
  addPolygons( # draw polygons on top of the base map (tile)
    stroke = FALSE, 
    smoothFactor = 0.2, 
    fillOpacity = 0.5,
    color = ~qpal(rates)) %>%
  addLegend("bottomleft",values = rates,pal = qpal, title = "Deaths/Confirmed Cases", na.label = "No data")
