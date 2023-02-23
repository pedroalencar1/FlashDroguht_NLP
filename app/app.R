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

shape_nuts_2 <- sf::read_sf("./data/GIS/nuts2.shp")

factpal <- colorFactor(hcl.colors(4, palette = "Dark 3"), shape_nuts_2$NUTS_ID)
pal_1 <- brewer.pal(n = 7, name = 'YlOrRd')[c(7,4,2)]


full_series <- data.table::fread("files/full_series.csv")

regions <- sort(unique(shape_nuts_2$NUTS_NAME))

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
      selectInput(
        "region_",
        "Select region",
        choices = regions,
        selected = "Brandenburg"
      ),
      sliderInput(
        "year_",
        "Choose year",
        min = 2000,
        max = 2021,
        value = c(2003, 2006)
      ),
    ),
    # show map with states and production values ----
    column(3, offset = 0, #
           leafletOutput("Map1")),
    column(7, offset = 0,
           plotlyOutput("graph1"))
  ),
  br(),
  br(),
  br(),

  
  # notes ----
  
  wellPanel(fluidRow(column(12,
                            h4("About this app"),
                            h5("Flash drought identificatio method: ", 
                               tags$a(href="https://www.sciencedirect.com/science/article/abs/pii/S0168192317302885?via%3Dihub", 
                                                                             "Ford and Labosier (2017)"),  
                               tags$br(),
                               "Source of soil moisture data: ", 
                               tags$a(href = "https://www.gleam.eu/",
                                      "GLEAM v3.6"),
                               tags$br(),
                               "Impact assessment from news articles gently provided by ",
                               tags$a(href="https://www.ufz.de/index.php?en=46549", 
                                      "Dr. Mariana M. de Brito"),
                               tags$br(),
                               "Author:", tags$a(href="https://www.tu.berlin/oekohydro/team/pedro-alencar/", "Pedro Alencar"),
                               tags$a(href="https://orcid.org/0000-0001-6221-8580", "(0000-0001-6221-8580)"),
                               tags$br(),
                               "Berlin, 23.02.2023"
                            )
                            ),
                     )
            )
  )


#__2.2 Server --------------------------------------------------------------
server <- function(input, output, session) {
  
  # grabs input and creates map
  observe(label = "map_desing",{
    region <- input$region_
    
    
    # define label features for leaflet map
    output$Map1 <- renderLeaflet({
      
      labels <- sprintf(
        "<strong>%s</strong><br/>%s",
        shape_nuts_2$NUTS_NAME,
        shape_nuts_2$NUTS_ID
      ) %>% lapply(htmltools::HTML)
      
      leaflet() |>
        addTiles()|>
        addPolygons(
          data = filter(shape_nuts_2, NUTS_NAME == region),
          weight = 3,
          opacity = 1,
          color = "black",
          fillOpacity = 1,
          dashArray = "1"
        )|>
        addPolygons(data = shape_nuts_2,
                    stroke = T,
                    color = "white",
                    weight = 2,
                    opacity = 1,
                    fillColor  =~factpal(NUTS_ID),
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
    
    region <- input$region_
    range <- input$year_
    # 
    # region = "Berlin"
    # range = c(2003, 2005)

    nuts_id_select <- shape_nuts_2$NUTS_ID[which(shape_nuts_2$NUTS_NAME == region)]
    
    # get selection impacts
    plot_series <- full_series |>
      filter(nuts_id == nuts_id_select,
             lubridate::year(date) >= range[1],
             lubridate::year(date) <= range[2])
    
    max_fd <- ceiling(max(plot_series$fd_ratio)*100)  %>%
      divide_by(100)
    max_imp <-  ceiling(max(plot_series$imp_ratio)*100)  %>%
      divide_by(100)
    
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
          line = list(color = pal_1[2])
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
