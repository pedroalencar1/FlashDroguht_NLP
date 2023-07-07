"
Simple shiny app to visualize relation between time series of 
flash droughts and percieved impacts from news papers articles

Pedro Alencar

23.02.2023
"


# 0. import libraries  -------------------------------------------

library(shiny) # comment out when pushing to shinyapps.io
library(dplyr)
library(tidyr)
library(ggplot2)
library(magrittr)
library(shinythemes)
library(bslib)
library(plotly)
library(leaflet)
library(RColorBrewer)
library(fst)
library(tibble)
library(tibbletime)
library(lubridate)
library(sf)


# 1. load datasets --------------------------------------------------------

shape_nuts_3 <- sf::read_sf("./data/GIS/nuts3.shp")
shape_nuts_2 <- sf::read_sf("./data/GIS/nuts2.shp")
shape_nuts_1 <- sf::read_sf("./data/GIS/nuts1.shp")
shape_nuts_ <- shape_nuts_2

factpal_3 <- colorFactor(hcl.colors(4, palette = "Dark 3"), shape_nuts_3$NUTS_ID)
factpal_2 <- colorFactor(hcl.colors(4, palette = "Dark 3"), shape_nuts_2$NUTS_ID)
factpal_1 <- colorFactor(hcl.colors(4, palette = "Dark 3"), shape_nuts_1$NUTS_ID)
factpal_ <- shape_nuts_2

pal_1 <- brewer.pal(n = 7, name = 'YlOrRd')[c(7,4,2)]
pal_2 <- brewer.pal(n = 5, name = 'Set3')

# full_series_2 <- readRDS("./files/gleam_full_series_lvl2.RData")
# full_series_1 <- readRDS("./files/gleam_full_series_lvl1.RData") 

full_series_3 <- readRDS("./files/ufz_full_series_lvl3.RData")
full_series_2 <- readRDS("./files/ufz_full_series_lvl2.RData")
full_series_1 <- readRDS("./files/ufz_full_series_lvl1.RData") 

regions_3 <- sort(unique(shape_nuts_3$NUTS_NAME))
regions_2 <- sort(unique(shape_nuts_2$NUTS_NAME))
regions_1 <- sort(unique(shape_nuts_1$NUTS_NAME))
# 2. THE APP --------------------------------------------------------------


#__2.1 User interface -------------------------------------------------------------
ui <- fluidPage(
  
  theme = bs_theme(version = 4, bootswatch = "materia"),
  
  title = "Flash drought perception in the media",
  
  titlePanel(div(h3('Flash droughts perception in Germany', style="margin: 0;"), 
                 h4('A comparison between occurence and reporting in news media', 
                    style="margin: 0;"),br())),
  
  # inputs and simple map ----
  fluidRow(
    column(
      2,
      h4("Select data"),
      # add menus for selections ----
      radioButtons(
        "nuts_level_",
        "NUTS level",
        choices = c(1,2,3),
        selected = 2
      ),
      selectInput(
        "region_",
        "Select region",
        choices = regions_2,
        selected = "Berlin"
      ),
      sliderInput(
        "year_",
        "Choose year",
        min = 2000,
        max = 2021,
        value = c(2003, 2007)
      ),
      h4("Plot option"),
      radioButtons("plot_", " ", 
                   choices = list("Perception summary" = 1, 
                                  "Perception discretisation" = 2),
                   selected = 1),
    ),
    # show map with states and production values ----
    column(3, offset = 0, #
           leafletOutput("Map1")),
    column(7, offset = 0,
           plotlyOutput("graph1")),
    # column(1, offset = 0,
    #        textOutput("text1")),
  ),
  br(),
  br(),
  br(),
  
  
  # notes ----
  
  wellPanel(fluidRow(column(12,
                            h4("About this app"),
                            h5("Flash drought identification method: ", 
                               tags$a(href="https://www.sciencedirect.com/science/article/abs/pii/S0168192317302885?via%3Dihub", 
                                      "Ford and Labosier (2017)"),  
                               tags$br(),
                               "Source of soil moisture data: ", 
                               tags$a(href = "https://www.ufz.de/index.php?en=37937",
                                      "ufz.de"),
                               tags$br(),
                               "Impact assessment from news articles gently provided by ",
                               tags$a(href="https://www.ufz.de/index.php?en=46549", 
                                      "Dr. Mariana M. de Brito"),
                               " and ", 
                               tags$a(href="https://jsodoge.eu/", 
                                      "MSc. Jan Sodoge"),
                               tags$br(),
                               "Author:", tags$a(href="https://www.tu.berlin/oekohydro/team/pedro-alencar/", "Pedro Alencar"),
                               tags$a(href="https://orcid.org/0000-0001-6221-8580", "(0000-0001-6221-8580)"),
                               tags$br(),
                               "Berlin, 25.05.2023"
                            )
  ),
  )
  )
)


#__2.2 Server --------------------------------------------------------------
server <- function(input, output, session) {
  
  observe({ #update the list of regions
    nuts_lvl <- input$nuts_level_
    
    name <- input$Map1_shape_click$id
    text_name <- as.character(name)
    
    if (nuts_lvl == 1){
      updateSelectInput(session, "region_",
                        choices = regions_1,
                        selected = text_name)
      shape_nuts_ <- shape_nuts_1 # to use on the map
      factpal_ <- factpal_1
    } else if (nuts_lvl == 2){
      updateSelectInput(session, "region_",
                        choices = regions_2,
                        selected = text_name)
      shape_nuts_ <- shape_nuts_2# to use on the map
      factpal_ <- factpal_2
    } else {
      updateSelectInput(session, "region_",
                        choices = regions_3,
                        selected = text_name)
      shape_nuts_ <- shape_nuts_3# to use on the map
      factpal_ <- factpal_3
    }
  })
  
  # grabs input and creates map
  observe(label = "map_desing",{
    
    nuts_lvl <- input$nuts_level_
    
    if (nuts_lvl == 1){
      shape_nuts_ <- shape_nuts_1 # to use on the map
      factpal_ <- factpal_1
    } else if (nuts_lvl == 2){
      shape_nuts_ <- shape_nuts_2# to use on the map
      factpal_ <- factpal_2
    } else {
      shape_nuts_ <- shape_nuts_3# to use on the map
      factpal_ <- factpal_3
    }
    
    region <- input$region_
    
    
    # define label features for leaflet map
    output$Map1 <- renderLeaflet({
      
      labels <- sprintf(
        "<strong>%s</strong><br/>%s",
        shape_nuts_$NUTS_NAME,
        shape_nuts_$NUTS_ID
      ) %>% lapply(htmltools::HTML)
      
      leaflet() |>
        addTiles()|>
        addPolygons(
          data = filter(shape_nuts_, NUTS_NAME == region),
          weight = 3,
          opacity = 1,
          color = "black",
          fillOpacity = 1,
          dashArray = "1"
        )|>
        addPolygons(data = shape_nuts_,
                    stroke = T,
                    color = "white",
                    weight = 2,
                    opacity = 1,
                    layerId = ~NUTS_NAME,
                    fillColor  =~factpal_(NUTS_ID),
                    fillOpacity = 0.5,
                    highlightOptions = highlightOptions(
                      weight = 5,
                      color = "#666",
                      dashArray = "",
                      fillOpacity = 0.5,
                      bringToFront = TRUE
                    ),
                    dashArray = "1",
                    label = labels, # in labels, show all production fetures
                    labelOptions = labelOptions(
                      style = list("font-weight" = "normal", 
                                   padding = "3px 8px"),
                      textsize = "15px",
                      direction = "auto")
        )
      
    })
    

    
  })
  
  observe(label = "plot", {
    
    nuts_lvl <- input$nuts_level_
    
    if (nuts_lvl == 1){
      shape_nuts_ <- shape_nuts_1 # to use on the map
      full_series <- full_series_1
    } else if (nuts_lvl == 2){
      shape_nuts_ <- shape_nuts_2# to use on the map
      full_series <- full_series_2
    } else {
      shape_nuts_ <- shape_nuts_3# to use on the map
      full_series <- full_series_3
    }
    
    region <- input$region_
    range <- input$year_
    plot_type <- input$plot_
    # 
    # region = "Berlin"
    # range = c(2003, 2005)
    
    if (region %in% shape_nuts_$NUTS_NAME){
      nuts_id_select <- shape_nuts_$NUTS_ID[which(shape_nuts_$NUTS_NAME == region)]
      
      plot_series <- full_series |>
        filter(nuts_id == nuts_id_select,
               lubridate::year(date) >= range[1],
               lubridate::year(date) <= range[2])
      
      # get selection impacts
      max_fd <- ceiling(max(plot_series$fd_ratio)*100)  %>%
        divide_by(100)
      max_imp <-  ceiling(max(plot_series$imp_ratio)*100)  %>%
        divide_by(100)
      
      if (plot_type == 1) {
        output$graph1 <- renderPlotly({
          
          plot_ly(plot_series, x = ~as.factor(date)) %>%
            add_lines(
              y = ~fd_ratio,
              name = "FD prevalence",
              yaxis = "y1",
              line = list(color = pal_1[1])
            )  %>%
            add_bars(
              y = ~imp_ratio,
              name = "Perception",
              yaxis = "y2",
              marker = list(color = pal_1[2])
            )  %>%
            layout(
              title = paste0("Flash drought prevalence and perception in ", region),
              xaxis = list(
                title = "Year",
                domain = c(0, 0.95),
                # type = "date",
                tickmode = "auto",
                nticks = 20,
                dtick = "M1",
                ticks = "outside"
              ),
              yaxis = list(
                title = "Prevalence (-)",
                side = "left",
                color = "black",
                position = 0,
                anchor = "free",
                rangemode="tozero",
                scaleanchor='y', scaleratio=1, constraintoward='bottom', secondary_y=T,
                range = c(0, max_fd),
                nticks = 4
                # dticks = 0.1
              ),
              yaxis2 = list(
                title = "Perception (-)",
                side = "right",
                color = "black",
                overlaying = "y",
                anchor = "free",
                position = 0.95,
                rangemode="tozero",
                scaleanchor='y2',scaleratio=1, constraintoward='bottom', secondary_y=F,
                range = c(0, max_imp),
                nticks = 4
                # dticks = 0.01
              ),
              showlegend = T
            )
        })
      } else {
        output$graph1 <- renderPlotly({
          
          plot_ly(plot_series, x = ~as.factor(date)) %>%
            add_lines(
              y = ~fd_ratio,
              name = "FD prevalence",
              yaxis = "y1",
              line = list(color = pal_1[1])
            )  %>%
            add_lines(
              y = ~imp_ratio,
              name = "Perception",
              yaxis = "y2",
              line = list(color = "darkgrey",
                          # alpha = 0.6,
                          width = 0.2)
            )  %>%
            add_bars(y = ~agriculture*10000,
                     name = "agriculture",
                     width = 1,
                     yaxis = "y2",
                     marker = list(color = pal_2[1])) %>%
            add_bars(y = ~energy*10000,
                     name = "energy",
                     width = 1,
                     yaxis = "y2",
                     marker = list(color = pal_2[2])) %>%
            add_bars(y = ~social*10000,
                     name = "social",
                     width = 1,
                     yaxis = "y2",
                     marker = list(color = pal_2[3])) %>%
            add_bars(y = ~fire*10000,
                     name = "fire",
                     width = 1,
                     yaxis = "y2",
                     marker = list(color = pal_2[4])) %>%
            add_bars(y = ~livestock*10000,
                     name = "livestock",
                     width = 1,
                     yaxis = "y2",
                     marker = list(color = pal_2[5])) %>%
            layout(
              title = paste0("Flash drought prevalence and perception in ", region),
              xaxis = list(
                title = "Year",
                domain = c(0, 0.95),
                # type = "date",
                tickmode = "auto",
                nticks = 20,
                dtick = "M1",
                ticks = "outside"
              ),
              yaxis = list(
                title = "Prevalence (-)",
                side = "left",
                color = "black",
                position = 0,
                anchor = "free",
                rangemode="tozero",
                scaleanchor='y', scaleratio=1, constraintoward='bottom', secondary_y=T,
                range = c(0, max_fd),
                nticks = 4
                # dticks = 0.1
              ),
              yaxis2 = list(
                title = "Perception (-)",
                side = "right",
                color = "black",
                overlaying = "y",
                anchor = "free",
                position = 0.95,
                rangemode="tozero",
                scaleanchor='y2',scaleratio=1, constraintoward='bottom', secondary_y=F,
                range = c(0, max_imp),
                nticks = 4
                # dticks = 0.01
              ),
              barmode = "stack",
              showlegend = T
            )
        })
      }
    } else {
      plot_series <- NA # do nothing -> will by default go back to Brandenburg
    }
    
    
    # # Button
    # output$downloadData <- downloadHandler(
    #   filename = function() {
    #     paste(landkreis,crop,variety, '.csv', sep = "_")
    #   },
    #   content = function(file) {
    #     write.csv(prod_series, file)
    #   }
    # )
    
  })
  
}

#__2.3 Build app ------------------------------------------------------------------
shinyApp(ui, server)
