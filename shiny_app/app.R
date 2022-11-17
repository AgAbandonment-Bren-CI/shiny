

# TO DO

# 1. Find and add biodiversity data
# 2. Figure out how to include borders in the carbon layer
# 3. Improve resolution on global map
# 3. Make radio buttons for SSPs reactive
# 4. Create csv for country level results, and then analyze/visualize that data
# 5. create tab(s) for objective 2 in the shiny
# Other ideas (if time)
# - abandonment by crop type
# - abandonment at different time horizons

# attach packages
library(shiny)
library(here)
library(tidyverse)
library(tmap)
library(terra)
library(shinythemes)
library(shinyjs)

# read in the raster data
ssp1 <- rast(here("data", "50k", "ssp1_50k.tif")) %>% project("epsg:4326")
ssp2 <- rast(here("data", "50k", "ssp2_50k.tif")) %>% project("epsg:4326")
ssp3 <- rast(here("data", "50k", "ssp3_50k.tif")) %>% project("epsg:4326")
ssp4 <- rast(here("data", "50k", "ssp4_50k.tif")) %>% project("epsg:4326")
ssp5 <- rast(here("data", "50k", "ssp5_50k.tif")) %>% project("epsg:4326")
carbon <- rast(here("data", "carbon", "carbon_50k.tif")) %>% project("epsg:4326") 
# bd <- raster(here("data", "50k", "bd_50k.tif")) %>% project("epsg:4326")

# reading in total abandonment CSV
abandonment_total <- read_csv(here("data", "csv", "total_abandonment.csv")) 
vec <- c("ssp1", "ssp2", "ssp3", "ssp4", "ssp5")

### BEGIN UI ###

ui <- fluidPage(
  navbarPage(theme = shinytheme("flatly"),
             tags$head(tags$style(HTML('.navbar-static-top {background-color: 2E86C1;}',
                                       '.navbar-default .navbar-nav>.active>a {background-color: black;}'))),
             title = "Agricultural Abandonment",
             
             ### FIRST TAB ###
             
             tabPanel("Introduction", icon = icon("align-left"),
                      titlePanel("Introduction"),
                      mainPanel(width = 10, h5(strong("Authors:"), "Michelle Geldin |", "Shayan Kaveh |", "Nickolas McManus |", "Max Settineri |", "Lucas Boyd"),
                                img(src = "barn.jpeg")
              
                      )), #END TAB 1
             
             ### SECOND TAB ###
             
             tabPanel("Global", icon = icon("globe"),
                      titlePanel("Major Trends in Projected Global Agricultural Abandonment"),
                      sidebarLayout(
                        sidebarPanel(
                          
                          # define alpha sliders for tmap 
                          radioButtons(inputId = "ssp_radio", label = h3("Abandonment by climate scenario"), 
                                      choices = c("SSP 1" = "ssp1", 
                                                  "SSP 2" = "ssp2", 
                                                  "SSP 3" = "ssp3",
                                                  "SSP 4" = "ssp4",
                                                  "SSP 5" = "ssp5"),
                                      selected = "ssp1"),
                          sliderInput("abandon_slide", label = h3("Abandonment"), 
                                      min = 0.01, 
                                      max = 1, 
                                      value = 0.5),
                          sliderInput("carbon_slide", label = h3("Carbon Sequestration Potential"), 
                                      min = 0.01, 
                                      max = 1, 
                                      value = 0.5),
                          sliderInput("bd_slide", label = h3("Biodiversity"), 
                                      min = 0.01, 
                                      max = 1, 
                                      value = 0.5)
                        ), # end sidebar panel
                        
                        # A plot of biodiversity in the main panel
                        mainPanel(strong("Directions"), # small title at the top of the main panel
                                  p("Select your Shared Socioeconomic Pathway of interest, then adjust the Carbon slider to visualize potential carbon sequestration rates."),
                                  tmapOutput(outputId = "ab_tmap"),
                                  p(strong("Figure 1:"),"Red indicates projected agricultural abandonment, with darker colors signaling more abandonment in a given pixel. The blue represents carbon sequestration potential of land for 30 years following human disturbance. For a higher resolution look at this data go to **this google earth engine repo to be created**"),
                                  p("Data Source", a(href = "https://data.globalforestwatch.org/documents/gfw::carbon-accumulation-potential-from-natural-forest-regrowth-in-forest-and-savanna-biomes/about ", "Carbon Accumulation Potential"), ""),
                                  plotOutput(outputId = "total_abandonment_plot"),
                                  p(strong("Figure 2:"), "Total abandoned cropland globally in 2050 (km^2) by climate scenario. Percentages indicate the proportion of total cropland that is projected to be abandoned.")
                        ) # end main panel tab 1
                      ) # end sidebarlayout
             ), # end tab 2
             tabPanel("Country-level",  icon = icon("flag"),
                      titlePanel("Abandonment by country"),
                      mainPanel(width = 10, strong("Directions")
                      ) # end main panel of tab 3
             ) # end tab 3
  ) # end navbarpage
) # end UI

### BEGIN SERVER ###

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  ssp_reactive <- reactive({ 
    req(input$ssp_radio)
  message("value of ssp_radio = ", paste(input$ssp_radio)) 
    input$ssp_radio
  }) # end reactive subset
  
# Front page tmap
  output$ab_tmap <- renderTmap({
    req(input$ssp_radio)
    message(input$ssp_radio)
    tm_shape(shp = ssp1) + # *** need to find a way to make this reactive to different rasters input$ssp_radio
      tm_raster(title = "Proportion abandoned", 
                col = "global_PFT_2015", 
                palette = "Reds", 
                style = "cont", 
                alpha = input$abandon_slide) +
      tm_shape(carbon, raster.downsample = FALSE) +
      tm_raster(title = "C seq. (mg/ha/yr)", 
                col = "sequestration_rate__mean__aboveground__full_extent__Mg_C_ha_yr", 
                palette = "Blues", 
                style = "cont", 
                alpha = input$carbon_slide) # + need to figure out what's going on with this downsampling - abandonment map comes up blank when max.raster is expanded
     #  tmap_options(max.raster = c(plot = 1e10, view = 1e10)) 
  }) # end tmap 1
  
  # total abandonment ggplot panel 1
  output$total_abandonment_plot <- renderPlot({
    ggplot(data = abandonment_total, aes(x = ssp, y = abandonment_millions_km2, fill = abandonment_millions_km2), alpha = 0.9) +
      geom_col() +
      theme_minimal(14) + 
      labs(x = element_blank(), y = "Global abandonment (millions km^2)") +
      theme(axis.text.x = element_text(
        vjust = 5, 
        size = 16), 
        axis.text.y = element_text(
          size = 16
        )) + 
      geom_text(aes(x = ssp, y = abandonment_millions_km2 + .2, label = paste(percent, "%")), color = "black", size = 7) +
      theme(legend.position = "none") +
      scale_fill_gradientn(colors = c("deepskyblue3", "deepskyblue4"))
  })
}


# Run the application 
shinyApp(ui = ui, server = server)
