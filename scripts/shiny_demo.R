library(shiny)
library(leaflet)
library(sf)

# get out of the scripts dir
setwd("..")

# load polygons
polys <- st_read("data/us_counties_hud_zip.geojson")
cmap <- colorNumeric("YlOrRd", domain=polys$br0_yr2017)

ui <- fluidPage(
    titlePanel("33percent"),
    tabsetPanel(
        tabPanel("Map",
            fluidRow(
                leafletOutput("mymap")
            ),
            br(),
            fluidRow(
                column(
                    6,
                    wellPanel(
                        fluidRow(
                            column(6,
                                selectInput("aptChoice",
                                        "Apartment Type",
                                        c("Studio", "1 Bedroom", "2 Bedroom", "3 Bedroom"))
                            ),
                            column(6,
                               selectInput("yearChoice",
                                           "Year",
                                            2017:2020) 
                            )
                            
                        ),
                        fluidRow(
                            column(6,
                                   numericInput("income",
                                                "Your income:",
                                                4000, min=0)),
                            column(6,
                                   submitButton(text="Apply"))
                        )
                    )
                ),
                column(6,
                       "This data comes from HUD fair market rent, which is published annually at <link>.")
            )
        ),
        tabPanel("About",
                 "It is a long established fact that a reader will be distracted by the readable content of a page when looking at its layout. The point of using Lorem Ipsum is that it has a more-or-less normal distribution of letters, as opposed to using 'Content here, content here', making it look like readable English. Many desktop publishing packages and web page editors now use Lorem Ipsum as their default model text, and a search for 'lorem ipsum' will uncover many web sites still in their infancy. Various versions have evolved over the years, sometimes by accident, sometimes on purpose (injected humour and the like)")
    )
)

server <- function(input, output, session) {
    output$mymap <- renderLeaflet({
        leaflet(polys, height="50%") %>%
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