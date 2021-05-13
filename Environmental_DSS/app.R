#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)
library(leafem)


# Define UI for application that draws a histogram
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

# Define server logic required to draw leaflet and  histogram
server <- function(input, output) {

    output$map <- renderLeaflet({
        leaflet() %>%
            addTiles(group = "OSM (default)") %>% 
            addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
            addRasterImage(elevation500m_ll,col=terrain.colors(30), group = "Elevation")%>%
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
 
    observeEvent(input$map_click, {
        click<-input$map_click
        text<-paste("Lattitude ", click$lat, "Longtitude ", click$lng)
        print(text)
        
    })
    output$robin_plot <- renderPlot( ggplot(robin_records_per_yr, aes(x = Start.date.year, y=count_per_year)) + geom_line() + xlab("Years") + ylab("Robins Observed"))
    
    output$greattit_plot <- renderPlot( ggplot(greattit_records_per_yr, aes(x = Start.date.year, y=count_per_year)) + geom_line() + xlab("Years") + ylab("Great Tits observed"))
    
    output$redwing_plot <- renderPlot( ggplot(redwing_records_per_yr, aes(x = Start.date.year, y=count_per_year)) + geom_line() + xlab("Years") + ylab("Redwings observed"))
    
}



 

# Run the application 
shinyApp(ui = ui, server = server)
