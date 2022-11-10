# attach packages
library(shiny)
library(here)
library(tidyverse)
library(raster)
library(tmap)
library(terra)
library(shinythemes)
library(shinyjs)

# read in the raster data
ssp1 <- raster(here("data", "50k", "ssp1_50k.tif"))
ssp2 <- raster(here("data", "50k", "ssp2_50k.tif"))
ssp3 <- raster(here("data", "50k", "ssp3_50k.tif"))
ssp4 <- raster(here("data", "50k", "ssp4_50k.tif"))
ssp5 <- raster(here("data", "50k", "ssp5_50k.tif"))
carbon <- raster(here("data", "carbon", "carbon_50k.tif"))
# bd <- raster(here("data", "50k", "bd_50k.tif"))
               
### BEGIN UI ###

ui <- fluidPage(
  navbarPage(theme = shinytheme("flatly"),
             tags$head(tags$style(HTML('.navbar-static-top {background-color: 2E86C1;}',
                                       '.navbar-default .navbar-nav>.active>a {background-color: black;}'))),
             title = "Agricultural Abandonment",
             
             ### FIRST TAB ###
             
             tabPanel("Introduction", icon = icon("align-left"),
                      titlePanel("Introduction"),
                      mainPanel(width = 10, h5(strong("Authors:"), "Michelle Geldin", "Shayan Kaveh", "Nickolas McManus", "Lucas Boyd"),
                                img(src = "barn.jpeg")
              
                      )), #END TAB 1
             
             ### SECOND TAB ###
             
             tabPanel("Abandonment", icon = icon("seedling"),
                      titlePanel("Major Trends in Projected Global Agricultural Abandonment"),
                      sidebarLayout(
                        sidebarPanel(
                          
                          # define alpha sliders for tmap 
                          radioButtons(inputId = "ssp_radio", label = h3("Abandonment by climate scenario"), 
                                      choices = c("SSP 1" = "ssp1", 
                                                  "SSP 2" = "ssp2", 
                                                  "SSP 3" = "ssp3",
                                                  "SSP 4" = "ssp4",
                                                  "SSP 5" = "ssp5")),
                          sliderInput("carbon_slide", label = h3("Carbon Sequestration Potential"), 
                                      min = 0, 
                                      max = 1, 
                                      value = 0.5),
                        ), # end sidebar panel
                        
                        # A plot of biodiversity in the main panel
                        mainPanel(strong("Directions"), # small title at the top of the main panel
                                  p("Select your Shared Socioeconomic Pathway of interest, then adjust the Carbon slider to visualize potential carbon sequestration rates."),
                                  tmapOutput(outputId = "ab_tmap"),
                                  p(strong("Figure 1:"),"Red indicates projected agricultural abandonment, with darker colors signaling more abandonment in a given pixel. The blue represents carbon sequestration potential of land for 30 years following human disturbance. For a higher resolution look at this data go to **this google earth engine repo to be created**"),
                                  p("Data Source", a(href = "https://data.globalforestwatch.org/documents/gfw::carbon-accumulation-potential-from-natural-forest-regrowth-in-forest-and-savanna-biomes/about ", "Carbon Accumulation Potential"), ""),
                        ) # end main panel tab 1
                      ) # end sidebarlayout
             ) # end tab 2
  ) # end navbarpage
) # end UI

### BEGIN SERVER ###

# Define server logic required to draw a histogram
server <- function(input, output) {
# Front page tmap
    output$ab_tmap <- renderTmap({
      tm_shape(ssp1) + # *** need to find a way to make this reactive to different rasters input$ssp_radio
        tm_raster(col = "ssp1_50k", palette = "Reds", style = "cont", alpha = 0.7) + 
        tm_shape(carbon) +
        tm_raster(col = "carbon_50k", palette = "Blues", style = "cont", alpha = input$carbon_slide)
    }) # end tmap 1
}

# Run the application 
shinyApp(ui = ui, server = server)
