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

#impact data
# full_series_2 <- readRDS("./files/gleam_full_series_lvl2.RData")
# full_series_1 <- readRDS("./files/gleam_full_series_lvl1.RData") 

full_series_3 <- readRDS("./files/ufz_full_series_lvl3.RData")
full_series_2 <- readRDS("./files/ufz_full_series_lvl2.RData")
full_series_1 <- readRDS("./files/ufz_full_series_lvl1.RData")

full_series_gt <- readRDS("./files/gt_full_series_lvl1_2022.RData") 


# trend data
lag_1 <- readRDS("./files/plotdata_NUTS-1_lag_ufz.RData") 
lag_2 <- readRDS("./files/plotdata_NUTS-2_lag_ufz.RData") 
lag_3 <- readRDS("./files/plotdata_NUTS-3_lag_ufz.RData") 

diff_1 <- readRDS("./files/plotdata_NUTS-1_diff_ufz.RData") 
diff_2 <- readRDS("./files/plotdata_NUTS-2_diff_ufz.RData") 
diff_3 <- readRDS("./files/plotdata_NUTS-3_diff_ufz.RData") 

int_1 <- readRDS("./files/plotdata_NUTS-1_intensity_ufz.RData") 
int_2 <- readRDS("./files/plotdata_NUTS-2_intensity_ufz.RData") 
int_3 <- readRDS("./files/plotdata_NUTS-3_intensity_ufz.RData") 

# areas
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
  
  tabsetPanel(
    tabPanel("Perception analysis",
             # inputs and simple map ----
             fluidRow(
               column(
                 2,
                 h4("Select data"),
                 # add menus for selections ----
                 radioButtons(
                   "database_",
                   "Data base",
                   choices = c("News articles", "Google trends"),
                   selected = "News articles"
                 ),
                 # h4("Select data"),
                 # add menus for selections ----
                 radioButtons(
                   "nuts_level_",
                   "NUTS level",
                   choices = c(1, 2, 3),
                   selected = 1
                 ),
                 selectInput("region_",
                             "Region",
                             choices = regions_1,
                             selected = "Berlin"),
                 sliderInput(
                   "year_",
                   "Years",
                   min = 2000,
                   max = 2022,
                   value = c(2019, 2022)
                 ),
                 h4("Plot option"),
                 radioButtons(
                   "plot_",
                   " ",
                   choices = list(
                     "Perception summary" = 1,
                     "Perception discretisation" = 2
                   ),
                   selected = 1
                 ),
               ),
               # show map with states and production values ----
               column(3, offset = 0, #
                      leafletOutput("Map1"),
                      br(),
                      br(),
                      br(),
                      downloadButton("downloadData", "Download Data"),
               ),
               column(7, offset = 0,
                      plotlyOutput("graph1")),
             ),
          ),
    
    # tabPanel("Trend analysis",
    # 
    #          # inputs and simple map ----
    #          fluidRow(
    #            column(
    #              2,
    #              h4("Select data"),
    #              # add menus for selections ----
    #              radioButtons(
    #                "nuts_level_2",
    #                "NUTS level",
    #                choices = c(1, 2, 3),
    #                selected = 1
    #              ),
    #              selectInput("region_2",
    #                          "Region",
    #                          choices = regions_1,
    #                          selected = "Berlin"),
    #            ),
    #            # show map with states and production values ----
    #            column(3, offset = 0, #
    #                   leafletOutput("Map1")),
    #            column(7, offset = 0,
    #                   plotlyOutput("graph2")),
    #          ),
    # ),
), 

hr(),

fluidRow(
  column(
    12,
    h4("Flash drought characteristics"),
    actionButton("generate_btn", "Generate Plots"),  # Button to generate plot
    br(),
    br(),
    offset = 0,
    plotlyOutput("graph2", height = "250px"),
    plotlyOutput("graph3", height = "250px"),
    plotlyOutput("graph4", height = "250px"),
    ),

  ),

  # notes ----
  br(),
  br(),
  wellPanel(fluidRow(column(
    12,
    h4("About this app"),
    h5(
      "Flash drought identification method: ",
      tags$a(href = "https://www.sciencedirect.com/science/article/abs/pii/S0168192317302885?via%3Dihub",
             "Ford and Labosier (2017)"),
      tags$br(),
      "Source of soil moisture data: ",
      tags$a(href = "https://www.ufz.de/index.php?en=37937",
             "ufz.de"),
      tags$br(),
      "Impact assessment from news articles gently provided by ",
      tags$a(href = "https://www.ufz.de/index.php?en=46549",
             "Dr. Mariana M. de Brito"),
      " and ",
      tags$a(href = "https://jsodoge.eu/",
             "MSc. Jan Sodoge"),
      tags$br(),
      "Author:",
      tags$a(href = "https://www.tu.berlin/oekohydro/team/pedro-alencar/", "Pedro Alencar"),
      tags$a(href = "https://orcid.org/0000-0001-6221-8580", "(0000-0001-6221-8580)"),
      tags$br(),
      "Berlin, 25.05.2023"
    )
  )))
)


#__2.2 Server --------------------------------------------------------------
server <- function(input, output, session) {
  

# update menu options -----------------------------------------------------
  observe({#update the list of levels
    
    database <- input$database_
    
    if (database == "News articles"){
      updateRadioButtons(session, "nuts_level_",
                         choices = c(1,2,3),
                         selected = 1)
      
      updateRadioButtons(session, "plot_",
                         choices = list("Perception summary" = 1, 
                                        "Perception discretisation" = 2),
                         selected = 1)
      
      updateSliderInput(session, "year_",
                        min = 2000,
                        max = 2022,
                        value = c(2019, 2022))
    } else {
      updateRadioButtons(session, "nuts_level_",
                         choices = c(1),
                         selected = 1)
      
      updateRadioButtons(session, "plot_",
                         choices = list("Perception summary" = 1),
                         selected = 1)
      
      updateSliderInput(session, "year_",
                        min = 2004,
                        max = 2022,
                        value = c(2019, 2022))
    }
  })
  
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
  

# map design --------------------------------------------------------------

  
  # grabs input and creates map
  observe(label = "map_design",{
    
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
  

# plot design -------------------------------------------------------------

  observe(label = "plot", {
    
    database <- input$database_
    nuts_lvl <- input$nuts_level_
    
    if (database == "Google trends") {
      shape_nuts_ <- shape_nuts_1 # to use on the map
      full_series <- full_series_gt
    } else {
      
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
              showlegend = FALSE,
              name = " ",
              yaxis = "y2",
              line = list(color = "darkgrey",
                          # alpha = 0.6,
                          width = 0.2)
            )  %>%
            add_bars(y = ~agriculture*10000,
                     name = "Agriculture",
                     width = 1,
                     yaxis = "y2",
                     marker = list(color = pal_2[1])) %>%
            add_bars(y = ~energy*10000,
                     name = "Energy",
                     width = 1,
                     yaxis = "y2",
                     marker = list(color = pal_2[2])) %>%
            add_bars(y = ~social*10000,
                     name = "Social",
                     width = 1,
                     yaxis = "y2",
                     marker = list(color = pal_2[3])) %>%
            add_bars(y = ~fire*10000,
                     name = "Fire",
                     width = 1,
                     yaxis = "y2",
                     marker = list(color = pal_2[4])) %>%
            add_bars(y = ~livestock*10000,
                     name = "Livestock",
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
    
    
    # Button
    output$downloadData <- downloadHandler(
      filename = function() {
        paste(region,min(range),max(range), '.csv', sep = "_")
      },
      content = function(file) {
        write.csv(plot_series, file)
      }
    )
    
  })
  

# trend design ----------------------------------------------------------
  
  
  observeEvent(input$generate_btn, {
    
    nuts_lvl <- input$nuts_level_
    
    if (nuts_lvl == 1) {
      p_lag <- lag_1
      p_diff <- diff_1
      p_int <- int_1
    } else if (nuts_lvl == 2) {
      p_lag <- lag_2
      p_diff <- diff_2
      p_int <- int_2
    } else {
      p_lag <- lag_3
      p_diff <- diff_3
      p_int <- int_3
    }
    
  output$graph2 <- renderPlotly({

    
    plot_ly(p_lag, x = ~year, y = ~fd, color = ~NUTS_NAME,
            type = 'scatter', mode = 'lines', hoverinfo = 'text',
            line = list(opacity = ifelse(nuts_lvl == 3, 0.3, 0.7)),
            # line = list(opacity = 0.1),
            colors = viridis::viridis(length(unique(p_lag$NUTS_NAME))),
            text = ~paste("<br>Location:", NUTS_NAME, "<br>Year:", year, "<br>Onset duration:", fd)) %>%
      layout(title = "Onset duration time series",
             xaxis = list(title = "Year"),
             yaxis = list(title = "Onset duration in pentads"),
             showlegend = ifelse(nuts_lvl == 3, F, T))
  })
  
  
  output$graph3 <- renderPlotly({
    
    plot_ly(p_diff, x = ~year, y = ~fd, color = ~NUTS_NAME,
            type = 'scatter', mode = 'lines', hoverinfo = 'text',
            line = list(opacity = ifelse(nuts_lvl == 3, 0.3, 0.7)),
            # line = list(opacity = 0.1),
            colors = viridis::viridis(length(unique(p_lag$NUTS_NAME))),
            text = ~paste("<br>Location:", NUTS_NAME, "<br>Year:", year, "<br>SM-drop:", fd)) %>%
      layout(title = "Drop of soil moisture over onset phase time series",
             xaxis = list(title = "Year"),
             yaxis = list(title = "SM-drop in percentiles"),
             showlegend = ifelse(nuts_lvl == 3, F, T))
  })
  
  output$graph4 <- renderPlotly({
    
    plot_ly(p_int, x = ~year, y = ~fd, color = ~NUTS_NAME,
            type = 'scatter', mode = 'lines', hoverinfo = 'text',
            line = list(opacity = ifelse(nuts_lvl == 3, 0.3, 0.7)),
            # line = list(opacity = 0.1),
            colors = viridis::viridis(length(unique(p_lag$NUTS_NAME))),
            text = ~paste("<br>Location:", NUTS_NAME, "<br>Year:", year, "<br>Intensity:", fd)) %>%
      layout(title = "FD intensity - ratio between SM-drop and onset duratio",
             xaxis = list(title = "Year"),
             yaxis = list(title = "Intensity (percentiles/pentad)"),
             showlegend = ifelse(nuts_lvl == 3, F, T))
  })
  
  })
  
  
}





#__2.3 Build app ------------------------------------------------------------------
shinyApp(ui, server)


