library(shiny)
library(leaflet)
library(sf)

# get out of the scripts dir
setwd("..")

polys <- st_read("data/us_counties_hud_zip.geojson")

cmap <- colorNumeric("YlOrRd", domain=polys$br0_yr2017)

ui <- fluidPage(
    leafletOutput("mymap")
)

server <- function(input, output, session) {
    output$mymap <- renderLeaflet({
        leaflet(polys, height="800px") %>%
            setView(-96, 37.8, 4) %>%
            addProviderTiles(providers$OpenStreetMap) %>%
            addPolygons(
                weight=0,
                fillOpacity=0.7,
                fillColor = ~cmap(br0_yr2017)
            ) %>%
            addLegend(pal = cmap, values = ~br0_yr2017, opacity = 0.7, 
                      title = "Studio Rent in 2017",
                      position = "bottomright")
    })
}

shinyApp(ui, server)