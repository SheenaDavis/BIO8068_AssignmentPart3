#Assignment Part 3
#Simple online decision-support system to display environmental data

#install necessary packages
#although all these packages will not be used in this script, they are relevant to the module,
#setting them up this way means I don't mistakenly install a package that is already present. 

necessary.packages<-c("devtools","behaviouR","tuneR","seewave","ggplot2","dplyr",
                      "warbleR","leaflet","lubridate","sp","sf","raster","mapview",
                      "leafem","BIRDS","xts","zoo", "stringr","vegan", "rinat")

already.installed <- necessary.packages%in%installed.packages()[,'Package'] #asks if the necessary packages are already installed
if (length(necessary.packages[!already.installed])>=1) { #if not installed download now
  install.packages(necessary.packages[!already.installed],dep=1)
}
sapply(necessary.packages, function(p){require(p,quietly = T,character.only = T)})

#This online decision support system will have four main parts. 
#Part 1 - Cumbria map showing elevation, settlements and roads.
#Part 2 - Images of three bird species
#Part 3 - three bird species 2019 distributions mapped onto a map of Cumbria
#Part 4 - three graphs showing changes in bird populations from 2016 - 2019

#### Part 1
#Cumbria map showing elevation, settlements and rivers & lakes.

options("rgdal_show_exportToProj4_warnings"="none")

# Import raster elevation data Ordnance Survey projection
elevation <- raster("www/elevation.tif")
plot(elevation)

# default colours are the wrong way round, 
# use the terrain.colors option to set low elevation to green, high to brown, 
# with 30 colour categories
plot(elevation, col=terrain.colors(30))

#convert to lat long
#project raster over map of cumbria
ll_crs <- CRS("+init=epsg:4326")  # 4326 is the code for latitude longitude
elevation_ll <- projectRaster(elevation, crs=ll_crs)
mapview(elevation_ll)

# elevation500m is a coarser defined version of the elevation map that allows the script to run faster
elevation500m <- aggregate(elevation, fact=10) # fact=10 is the number of cells aggregated together
#change projection to lat&long
elevation500m_ll <- projectRaster(elevation500m, crs = ll_crs)

#Save elevation as an RDS files
#elevation
saveRDS(elevation500m_ll, file = "elevation500.RDS")
elevation500m_ll<-readRDS("WWW/elevation500.RDS")
mapview(elevation500m_ll)

#Add DTM-derived information to site data

#Settlments
settlements <-st_read("www/cumbria_settlements.shp")
print(settlements)

plot(elevation)
plot(settlements["NAME"], add=TRUE)

settlements_ll <- st_transform(settlements, 4326)#convert to lat-long
mapview(settlements_ll)

#Save settlements as an RDS file
saveRDS(settlements_ll, file = "www/settlements.RDS")
settlements_ll<-readRDS("WWW/settlements.RDS")
mapview(settlements_ll)

#used to create popup
settlement_info <- paste("Name", settlements_ll$NAME)

#roads 
roads<-st_read("www/cumbria_roads.shp")
roads_ll <- st_transform(roads, 4326)
mapview(roads_ll)#decided against adding this to the leaflet 

#rivers
rivers<-st_read("www/spatial/cumbria_rivers.shp")
rivers_ll <- st_transform(rivers, 4326)
mapview(rivers_ll)

#Save rivers as RDS file 
saveRDS(rivers_ll, file = "www/rivers.RDS")
rivers_ll<-readRDS("WWW/rivers.RDS")
mapview(rivers_ll)

#lakes
lakes<-st_read("www/spatial/cumbria_lakes.shp")
lakes_ll <- st_transform(lakes, 4326)
mapview(lakes_ll)

#save lakes as RDS file
saveRDS(lakes_ll, file = "www/lakes.RDS")
lakes_ll<-readRDS("WWW/lakes.RDS")
mapview(lakes_ll)

#to combined with bird maps and displayed in the app
cumbria_map <- leaflet() %>% 
  addTiles(group = "OSM (default)") %>% 
  addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
  addRasterImage(elevation500m_ll,col=terrain.colors(30), group = "Elevation") %>% 
  addFeatures(settlements_ll, group = "Settlements",
              popup = settlement_info) %>%
  addFeatures(lakes_ll, group = "Lakes") %>%
  addFeatures(rivers_ll, group = "Rivers") %>%
  
  addLayersControl(
    baseGroups = c("OSM (default)", "Satellite"), 
    overlayGroups = c("Elevation", "Settlements", "Lakes", "Rivers" ),
    options = layersControlOptions(collapsed = FALSE)
  )

cumbria_map

##Part 2 
#Bird images

#Three bird species - Robin, Great tit, redwing 
#images

source("download_images.R") #source script for image download functions
gb_ll <- readRDS("gb_simple (1).RDS") #sets the bounds to Great Britain

Robin_recs <-  get_inat_obs(taxon_name  = "Erithacus rubecula",
                                bounds = gb_ll,
                                quality = "research",
                                # month=6,   # Month can be set.
                                # year=2018, # Year can be set.
                                maxresults = 10)

download_images(spp_recs = Robin_recs, spp_folder = "robin")

great_recs <-  get_inat_obs(taxon_name  = "Parus major",
                            bounds = gb_ll,
                            quality = "research",
                            # month=6,   # Month can be set.
                            # year=2018, # Year can be set.
                            maxresults = 10)

download_images(spp_recs = great_recs, spp_folder = "great_tit")

red_recs <-  get_inat_obs(taxon_name  = "Turdus iliacus",
                            bounds = gb_ll,
                            quality = "research",
                            # month=6,   # Month can be set.
                            # year=2018, # Year can be set.
                            maxresults = 10)

download_images(spp_recs = red_recs, spp_folder = "redwing")


#from the downloads three images were selected for the app and stored in the www folder.
#file paths are as follows:
#www/great_tit.jpg
#www/redwing.jpg
#www/robin.jpg
#These images could be read in and saved as follows:

greattit_image <- base64enc::dataURI(file="www/great_tit.jpg", mime="image/jpg")
redwing_image <- base64enc::dataURI(file="www/redwing.jpg", mime="image/jpg")
robin_image <- base64enc::dataURI(file="www/robin.jpg", mime="image/jpg")

#Part 3
#three species of bird counts within Cumbria in 2019
#species data downloaded from NBN Atlas 
# read in .csv files 
robin <- read.csv("www/Robin.csv")
great_tit <-read.csv("www/Great_tit.csv")
redwing <- read.csv("www/turdus19.csv")

#2019 distributions of 3 bird species in Cumbria - to be included in app
bird_dist <- leaflet() %>% 
  addTiles(group="OSM") %>%
  addProviderTiles("Esri.WorldImagery", group= "satellite") %>% 
  ## add markers
  addCircleMarkers(robin$Longitude..WGS84., robin$Latitude..WGS84.,  
                   radius = 2, fillOpacity = 0.5, opacity = 0.5, col="red", group="Robin") %>% 
  addCircleMarkers(great_tit$Longitude..WGS84., great_tit$Latitude..WGS84.,  
                   radius = 2, fillOpacity = 0.5, opacity = 0.5, col="blue", group="Great tit") %>% 
  addCircleMarkers(redwing$Longitude..WGS84., redwing$Latitude..WGS84.,  
                   radius = 2, fillOpacity = 0.5, opacity = 0.5, col="green", group="Redwing") %>% 
 
  
  addLayersControl(baseGroups = c("OSM","satellite"),
                   overlayGroups = c("Robin","Great tit","Redwing") , #change to c("Robin","Great tit")
                   options = layersControlOptions(collapsed = FALSE))
bird_dist

#Part 4 
#Graphs showing changes in bird count numbers over time 
#Read in data of the three birds showing counts per year from 2016-2019 downloaded from NBN website 

robin_records <- read.csv("www/Robin2016_2019.csv")
greattit_records <- read.csv("www/greattit_16_19.csv")
redwing_records <- read.csv("www/turdus16_19.csv")

#Robin count histogram 
ggplot(robin_records, aes(x=Start.date.year)) +
  geom_histogram()

robin_records_per_yr <- robin_records %>%
  group_by(Start.date.year) %>%
  summarise(count_per_year = n())

ggplot(robin_records_per_yr, aes(x =Start.date.year , y=count_per_year)) +
  geom_line() + xlab("Years") + ylab("Number of birds observed")

#Great tit count histogram 
ggplot(greattit_records, aes(x=Start.date.year)) +
  geom_histogram()

greattit_records_per_yr <- greattit_records %>%
  group_by(Start.date.year) %>%
  summarise(count_per_year = n())

ggplot(greattit_records_per_yr, aes(x =Start.date.year , y=count_per_year)) +
  geom_line() + xlab("Years") + ylab("Number of birds observed")

#Redwing count histogram 
ggplot(redwing_records, aes(x=Start.date.year)) +
  geom_histogram()

redwing_records_per_yr <- redwing_records %>%
  group_by(Start.date.year) %>%
  summarise(count_per_year = n())

ggplot(redwing_records_per_yr, aes(x =Start.date.year , y=count_per_year)) +
  geom_line() + xlab("Years") + ylab("Number of birds observed")

#### App Layout # 
#Part 1 and 3 are combined into one leaflet in this map 

map<- leaflet() %>% 
  addTiles(group = "OSM (default)") %>% 
  addProviderTiles(providers$Esri.WorldImagery, group = "Satellite")%>%
  addRasterImage(elevation500m_ll,col=terrain.colors(30), group = "Elevation") %>%
  #elevation_ll changed to elevation500m_ll as after running the leaflet originally it was very slow
  addCircleMarkers(robin$Longitude..WGS84., robin$Latitude..WGS84.,  
                   radius = 2, fillOpacity = 0.5, opacity = 0.5, col="red", group="Robin") %>% 
 addCircleMarkers(great_tit$Longitude..WGS84., great_tit$Latitude..WGS84.,  
                  radius = 2, fillOpacity = 0.5, opacity = 0.5, col="blue", group="Great Tit") %>% 
 addCircleMarkers(redwing$Longitude..WGS84., redwing$Latitude..WGS84.,  
                  radius = 2, fillOpacity = 0.5, opacity = 0.5, col="green", group="Redwing") %>% 

 addFeatures(settlements_ll, group = "Settlements",
             popup = settlement_info) %>%
  addFeatures(lakes_ll, group = "Lakes") %>%
  addFeatures(rivers_ll, group = "Rivers") %>%
  addLayersControl(
    baseGroups = c("OSM", "Satellite"), 
    overlayGroups = c("Elevation", "Settlements","Rivers", "Lakes", "Robin", "Great Tit","Redwing" ),
    options = layersControlOptions(collapsed = FALSE)
  )

map

# Define UI ----
ui <- fluidPage(
  
  # Application title
  titlePanel("Cumbria: The Rural Environment"),
  
  # Sidebar ----
  sidebarLayout(
    sidebarPanel(titlePanel("Garden bird species in Cumbria"),
                 p("The rural environment of Cumbria supports a rich diversity of bird species, many of which are common visitors in gardens
                       and have grown to rely on gardens as a source of food, paticularly during the winter months.
                       The following 3 birds have been recorded in gardens during the last RSPB garden birdwatch."),
                 
                 p(strong("The European Robin"),"is a common garden visitor, which is easily identified by it's orange breast.
                       It is an insectivorous bird."),
                 
                 img(src=robin_image,height="60%", width="60%"),
                 
                 p(strong("The Great Tit"),"is the largest of the UK tit species and are well adapted to gardens although they are commonly found in woodlands. 
                       They have a black head and neck and contrasting white cheeks,while their plumage is an olive colour on top and a pale yellow underneath. "),
                 
                 img(src=greattit_image,height="60%", width="60%"),
                 
                 p(strong("The Redwing."),"is the smallest true thrush in the UK, it is the least common garden visitor of these three birds
                       prefering hedgerows and grassy fields, however it is known to seek food in gardens during the winter months. It is easily identified 
                       y its orange-red patches below the wing."),
                 
                 img(src=redwing_image,height="60%", width="60%"),
                 
                 
    ),
    
    
    # Main panel ----
    mainPanel( p("The rural environment of Cumbria boasts rich diversity, both in terms of species and landscapes.
                     This interactive webpage shows a map of the elevation denoting Cumbria's hilly landscape, rivers, lakes and settlemnts.
                     Additionally the map shows the 2019 counts of three bird species; the European Robin, the Great Tit and the Redwing.
                     Finally the histograms shown on this webpage give an indication of the changes in populations of these species using count data
                     from the years 2016 to 2019"),
               leafletOutput(outputId = "cumbria_map"),
               p("Below are histograms showing European Robin, Great Tit and Redwing counts over time. 
                     These histograms were generated using a citizen science database called the National Biodiversity Network (NBN)."),
               plotOutput(outputId = "robin_plot"),
               plotOutput(outputId = "greattit_plot"),
               plotOutput(outputId = "redwing_plot"),
               p("Looking at these histograms; there have been large increases in the number of birds observed in 2019,
                     which could indicate a rise in their populations.")
    )))



# server logic ----
server <- function(input, output) {
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles(group = "OSM (default)") %>% 
      addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
      addRasterImage(elevation500m_ll,col=terrain.colors(30), group = "Elevation") %>%
      addCircleMarkers(robin$Longitude..WGS84., robin$Latitude..WGS84.,  
                       radius = 2, fillOpacity = 0.5, opacity = 0.5, col="red", group="Robin") %>% 
      addCircleMarkers(great_tit$Longitude..WGS84., great_tit$Latitude..WGS84.,  
                       radius = 2, fillOpacity = 0.5, opacity = 0.5, col="blue", group="Great Tit") %>% 
      addCircleMarkers(redwing$Longitude..WGS84., redwing$Latitude..WGS84.,  
                       radius = 2, fillOpacity = 0.5, opacity = 0.5, col="green", group="Redwing") %>% 
      
      addFeatures(settlements_ll, group = "Settlements",
                  popup = settlement_info) %>%
      addFeatures(lakes_ll, group = "Lakes") %>%
      addFeatures(rivers_ll, group = "Rivers") %>%
      addLayersControl(
        baseGroups = c("OSM", "Satellite"), 
        overlayGroups = c("Elevation", "Settlements","Rivers", "Lakes", "Robin", "Great Tit","Redwing" ),
        options = layersControlOptions(collapsed = FALSE)
      )
  }
)
      
    
    
  observeEvent(input$map, {
    click<-input$map
    text<-paste("Lattitude ", click$lat, "Longtitude ", click$lng)
    print(text)
    
  })
  
  output$robin_plot <- renderPlot( ggplot(robin_records_per_yr, aes(x = Start.date.year, y=count_per_year)) + geom_line() + xlab("Years") + ylab("Robins Observed"))
  
  output$greattit_plot <- renderPlot( ggplot(greattit_records_per_yr, aes(x = Start.date.year, y=count_per_year)) + geom_line() + xlab("Years") + ylab("Great Tits observed"))
  
  output$redwing_plot <- renderPlot( ggplot(redwing_records_per_yr, aes(x = Start.date.year, y=count_per_year)) + geom_line() + xlab("Years") + ylab("Redwings observed"))
  
}




# Run the application
shinyApp(ui = ui, server = server)
