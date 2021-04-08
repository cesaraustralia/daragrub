library(tidyverse)
library(shiny)
library(shinythemes)
# library(maptools)
# library(terra)
# library(raster)
library(leaflet)
library(sf)
library(plotly)
library(viridis)
library(DT)
# options(shiny.usecairo = TRUE)


# data prepration ---------------------------------------------------------
# global function
source("./getBug.R")
source("./develop.fun.R")

# reading temperature data
Tmin <- terra::rast("data/mu_Tmin_for_DOY_ag10.tif")
Tmax <- terra::rast("data/mu_Tmax_for_DOY_ag10.tif")

curYear <- format(Sys.time(), "%Y")


bugs <- list.files("bugs/") %>% 
  sapply(FUN = strsplit, "[.]") %>% 
  map(pluck(1)) %>% 
  unlist() %>% 
  sort()

bugList <- list()
#build list of all bugs with data 
for(bug in bugs){
  insect <- getBug(bug)
  bugList[insect$name] <- bug
}


# read scenario data, including pest names, cops, scenarios...
scenrio_table <- read_csv("data/Impact_scenarios.csv") %>%
  mutate(Link_to_resources = paste0("<a href='", Link_to_resources, "'>", "More information!","</a>"))
head(scenrio_table)


# points in Australia
data("wrld_simpl", package = "maptools")
auss_bound <- subset(wrld_simpl, NAME == "Australia") %>% 
  sf::st_as_sf() %>% 
  sf::st_set_crs(4326) # (CRS("+init=epsg:4326"))
# return true or false
xy_in_aus <- function(long, lat){
  data.frame(x = long, y = lat) %>% 
    sf::st_as_sf(coords = 1:2, crs = 4326) %>% 
    sf::st_intersection(sf::st_geometry(auss_bound)) %>% 
    nrow() != 0
}

# ggplot theme
mytheme <- theme_bw() +
  theme(text = element_text(size = 20,
                            family = "Nirmala UI",
                            color = "black"),
        axis.text.x = element_text(color = "black", angle = 45, vjust = 1, hjust = 1),
        axis.text.y = element_text(color = "black"),
        legend.title = element_blank(),
        legend.key = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        panel.background = element_rect(fill = "transparent"), # bg of the panel
        plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
        legend.background = element_blank(),
        axis.line.x = element_line(color = "black"),
        legend.position = "none"
  )

select_option <- c(
  "When is the critical time for monitoring?",
  "I am seeing a pest in my crop and want to check impact scenarios!"
)

# ui ----------------------------------------------------------------------
ui <- shinyUI(
  navbarPage("LifeStage", selected = "Monitoring assist", theme = shinytheme("journal"), # slate
             
             
             # Lifestage panel ---------------------------------------------------------
             tabPanel("Monitoring assist",
                      
                      column(4,
                             selectInput("selection", label = h4("1. What do you want to know:"), 
                                         choices = select_option, 
                                         selected = select_option[1],
                                         width = "100%"
                             ),
                             
                             selectInput("species", label = h4("2. Species of interest:"), 
                                         choices = bugList, 
                                         selected = bugList[[1]],
                                         width = "100%"
                             ),
                             

                             selectInput("crop", label = h4("3. Select crop:"),
                                         choices = unique(scenrio_table$Crop),
                                         selected = unique(scenrio_table$Crop)[1],
                                         width = "100%"
                             ),
                             
                             # uiOutput("impactUI"),
                             
                             selectInput(inputId = "impact",
                                         label = h4("4. Crop stage of interest:"),
                                         choices = unique(scenrio_table$Impact_scenario),
                                         selected = unique(scenrio_table$Impact_scenario)[1],
                                         width = "100%"
                             ),
                             
                             
                             dateRangeInput("crop_dev", label = h4("5. Define risk periods:"),
                                            min = paste0(curYear,"-1-1"),
                                            max = paste0(curYear,"-12-31"),
                                            format = "dd-MM", startview = "month", weekstart = 0,
                                            language = "en", width = "85%"
                             ),
                             
                             
                             
                             
                             # update based on user input
                             shiny::htmlOutput("stagetitle"),
                             
                             column(6,
                                    uiOutput("stageUI")
                             ),
                             column(6,
                                    uiOutput("observeUI")
                             ),
                             
                             
                             # uiOutput("date_ui"), # add UI for the second option
                             HTML("<br/>"),
                             shiny::htmlOutput("maptitle"),
                             span(textOutput("checklatlong"), style = "color:red"),
                             # add a leaflet map
                             leafletOutput("smap", height = 300),
                             
                             # HTML("<br/>"),
                             # shiny::htmlOutput("runtitle"),
                             # actionButton("update", "Run")
                             
                      ),
                      
                      column(8,
                             
                             # HTML("<br/>"),
                             shiny::htmlOutput("runtitle"),
                             actionButton("update", "Run"),
                             HTML("<br/>"),
                             
                             # output table should come here
                             # div(style = "position:relative",
                             plotlyOutput("phenology"),
                             # uiOutput("hover_info")
                             # ),
                             
                             HTML("<br/>"),
                             HTML("<br/>"),
                             DT::dataTableOutput("outtab")
                             
                      )
             )
             # new tab -----------------------------------------------------------------
             
             
             
             
  )
)


# Server ------------------------------------------------------------------
server <- function(session, input, output){
  
  values <- reactiveValues()
  values$df <- NULL
  values$count <- 1
  values$risk_stage <- "L1" # default startStage
  values$stage_names <- NA
  values$monitor_stage <- NA
  values$crop_date <- Sys.Date()
  
  
  # update impact scenario and crop options
  observeEvent(input$species, {
    crop_list <- scenrio_table %>%
      filter(Pest == input$species) %>%
      pull(Crop) %>%
      unique()
    
    updateSelectInput(session,
                      inputId = "crop",
                      choices = crop_list,
                      selected = crop_list[1])
    
  })
  
  
  impact_filter <- reactive({
    scenrio_table %>%
      filter(Pest == input$species,
             Crop == input$crop)
  })
  
  observeEvent(impact_filter(), {
    imsc <- impact_filter()
    updateSelectInput(session,
                      inputId = "impact",
                      choices = unique(imsc$Impact_scenario),
                      selected = unique(imsc$Impact_scenario)[1],
    )
  })
  
  
  # output$impactUI <- renderUI({
  #   imsc <- impact_filter()
  #   selectInput("impact", label = h4("4. Crop stage of interest:"),
  #               choices = unique(imsc$Impact_scenario),
  #               selected = unique(imsc$Impact_scenario)[1],
  #               width = "100%")
  # 
  # })
  
  ##****************************************************
  to_listen1 <- reactive({
    list(input$crop, input$impact, input$species)
  })
  
  # update the table
  observeEvent(to_listen1(), {
    # observeEvent(input$update, {
    # if(input$crop == "Wheat")
    #   browser()
    
    values$table <- scenrio_table %>%
      filter(
        Pest == input$species,
        Crop == input$crop#,
        # Impact_scenario == input$impact
      )
  })
  
  ##****************************************************
  
  
  to_listen2 <- reactive({
    list(input$species, input$impact, input$crop)
  })
  # set up the values of life stage based on selected pest
  observeEvent(to_listen2(), {
    insect <- getBug(input$species)
    stageList <- lapply(1:length(names(insect$dev.funs)), FUN = function(x) x)
    names(stageList) <- names(insect$dev.funs)
    values$stage_names <- names(stageList)
    # get pest risk stage
    values$risk_stage <- values$table %>% 
      filter(Impact_scenario == input$impact) %>%
      pull("Pest_risk_life_stage") %>% 
      strsplit(", ") %>% 
      pluck(1) %>% # this cause problems if there is more than one row
      identity()
    values$monitor_stage <- values$table %>% 
      filter(Impact_scenario == input$impact) %>%
      pull("Pest_monitoring_life_stage") %>% 
      strsplit(", ") %>% 
      pluck(1) %>% # this cause problems if there is more than one row
      identity()
  })
  
  
  
  
  output$stagetitle <- shiny::renderUI({
    if(input$selection == select_option[2]){
      h4("6. Observed pest stage:")
    }
  })
  output$stageUI <- renderUI({
    if(input$selection == select_option[2]){
      insect<-getBug(input$species)
      stageList<-lapply(1:length(names(insect$dev.funs)), FUN = function(x) x)
      names(stageList) <- names(insect$dev.funs)
      selectInput("stage", label = "Select observed stage", 
                  choices = names(stageList), #stageList, 
                  selected = names(stageList)[2], #stageList[[2]],
                  width = "100%")  
    }
  })
  output$observeUI <- renderUI({
    if(input$selection == select_option[2]){
      dateInput("observe_date", label = "Select observed date",
                min = paste0(curYear,"-1-1"),
                max = paste0(curYear,"-12-31"),
                format = "dd-MM", startview = "month", weekstart = 0,
                language = "en", 
                width = "100%")
    }
  })
  
  
  
  # values$crop_line <- data.frame(date = c(Sys.Date(), Sys.Date() + 5), type = "2") # unnecessary
  observeEvent(input$crop_dev, {
    values$crop_line <- data.frame(date = c(input$crop_dev[1], input$crop_dev[2]), type = "2")
    values$crop_date <- input$crop_dev[1]
  })
  
  
  to_listen3 <- reactive({
    list(input$stage, input$observe_date)
  })
  observeEvent(to_listen3(), {
    if(input$selection == select_option[2]){
      values$crop_date <- input$observe_date
      values$risk_stage <- input$stage
    }
  })
  
  
  
  # change map title base on the input
  output$maptitle <- shiny::renderUI({
    if(input$selection == select_option[1]){
      h4("6. Choose location (zoom and click):")
    } else{
      h4("7. Choose location (zoom and click):")
    }
  })
  
  # change the run button title base on the input
  output$runtitle <- shiny::renderUI({
    if(input$selection == select_option[1]){
      h4("7. Run simulation:")
    } else{
      h4("8. Run simulation:")
    }
  })
  
  # set default values for click
  input_coords <- reactiveValues()
  input_coords$long <- 145.0
  input_coords$lat <- -37.7
  # update the click
  observe({
    if(!is.null(input$smap_click)){
      if(xy_in_aus(input$smap_click$lng, input$smap_click$lat)){
        input_coords$long <- round(input$smap_click$lng, 5)
        input_coords$lat <- round(input$smap_click$lat, 5)     
      }
    }
  })
  # add the small map
  output$smap <- renderLeaflet({
    isolate({
      leaflet(options = leafletOptions(zoomControlPosition = "topright")) %>%
        setView(lng = 135.51, lat = -25.98, zoom = 3) %>%
        addTiles() %>% 
        addMarkers(lng = input_coords$long, lat = input_coords$lat)
    })
  })
  # update the click and marker without changing zoom and reloading
  observeEvent(input$smap_click, {
    leafletProxy("smap") %>%
      clearMarkers() %>% 
      addMarkers(lng = input$smap_click$lng, lat = input$smap_click$lat)
  })
  # show coordinates with click
  output$checklatlong <- renderText({ 
    if(!is.null(input$smap_click)){
      if(!xy_in_aus(input$smap_click$lng, input$smap_click$lat)){
        "Selected location is not in Australia"
      } else{
        NULL
      }
    }
  })
  
  
  
  # observe({
  observeEvent(input$update, {
    
    withProgress(message = "LOADING. PLEASE WAIT...", value = 0, { # create progress bar
      # isolate({
      # browser()
      longlat <- matrix(c(input_coords$long, input_coords$lat), ncol = 2)
      TMAX <- terra::extract(Tmax, longlat)
      TMIN <- terra::extract(Tmin, longlat)
      # startStage <- ifelse(input$startStage == "", 2, as.numeric(input$startStage))
      startStage <- which(values$stage_names == values$risk_stage[1])
      # startDay <- as.numeric(format(input$crop_dev[1],"%j")) #**** this is set to start data *****  
      startDay <- as.numeric(format(values$crop_date,"%j")) #**** this is set to start data *****  
      insect <- getBug(input$species)
      data <- develop(TMAX, TMIN, startDay, startStage, insect)
      # })
    })
    
    isolate({
      df <- as.data.frame(data[1, , ])
      df$stage <- names(insect$dev.funs)
      df$life <- insect$life
      # df$location <- input$simname
      df$long <- input_coords$long
      df$lat <- input_coords$lat
      df$generation <- ceiling(1:nrow(df) / length(insect$dev.funs))
      df$species <- paste0(stringr::str_pad(isolate(values$count), 2, pad="0"), ". ", insect$name, "\n",input$simname)
      mdf <- pivot_longer(df, cols = c("Time_start", "Time_end"))
      mdf$value <- as.Date(paste0(curYear,"-01-01")) + mdf$value
      # values$df <- rbind(values$df, mdf) # this append plots together
      values$df <- mdf
      values$count <- values$count + 1
    })
    
    
    
    data <- values$df
    data$life <- factor(data$life)
    if("adult" %in% levels(data$life))
      data$life <- factor(data$life,
                          levels = c(levels(factor(data$life))[-1], levels(factor(data$life))[1]),
                          ordered = TRUE)
    # sort the factor levels
    data$stage <- fct_relevel(str_to_sentence(data$stage), str_to_sentence(values$stage_names))
    weekspan <- as.numeric(max(data$value) - min(data$value)) / 7
    
    # create the rectangle data
    # data_rect <- data.frame()
    # browser()
    rang <- c("red", "yellow", "black")
    rang2 <- c("green", "blue", "purple")
    # for(i in 1:length(values$risk_stage)){
    data_rect <- data.frame(x1 = data %>% 
                              filter(name == "Time_start",
                                     stage == values$risk_stage[1]) %>% 
                              pull(value), 
                            x2 = data %>% 
                              filter(name == "Time_end",
                                     stage == values$risk_stage[length(values$risk_stage)]) %>% 
                              pull(value), 
                            y1 = values$stage_names[1], 
                            y2 = values$stage_names[length(values$stage_names)], 
                            fill = rang[1],
                            action = "Risk stage")
    # data_rect <- rbind(data_rect, data_rect1)
    # }
    # for(i in 1:length(values$monitor_stage)){
    # browser()
    data_rect1 <- data.frame(x1 = data %>% 
                               filter(name == "Time_start",
                                      stage == values$monitor_stage[1]) %>% 
                               pull(value), 
                             x2 = data %>% 
                               filter(name == "Time_end",
                                      stage == values$monitor_stage[length(values$monitor_stage)]) %>% 
                               pull(value), 
                             y1 = values$stage_names[1], 
                             y2 = values$stage_names[length(values$stage_names)], 
                             fill = rang2[1],
                             action = "Monitoring stage")
    data_rect <- rbind(data_rect, data_rect1)
    # }
    
    data_rect <- data_rect %>% 
      mutate(y1 = str_to_sentence(y1),
             y2 = str_to_sentence(y2))
    
    values$data_rect <- data_rect
    values$data <- data
    
    # }, priority = 0)
    
    
    
    
    # observeEvent(input$update, {
    
    output$phenology <- renderPlotly({
      
      isolate({
        p <- ggplot(data = values$data) +
          geom_point(aes(x = value, y = stage, color = stage), size = 5.5) +
          geom_line(aes(x = value, y = stage, color = stage), size = 6) +
          geom_rect(aes(xmin = x1, xmax = x2, 
                        ymin = y1, ymax = y2, 
                        fill = fill, text = paste0(action)), 
                    data = values$data_rect, 
                    alpha = 0.2) +
          geom_vline(data = values$crop_line, aes(xintercept = as.numeric(date), linetype = type)) +
          scale_linetype_manual(values = unique(values$crop_line$type)) +
          ylab(NULL) +
          xlab(NULL) +
          # scale_color_viridis_d(option = "C") +
          scale_color_manual(values = gray.colors(length(values$stage_names))) +
          scale_fill_manual(values = c("blue", "red")) +
          # geom_vline(xintercept = isolate(as.numeric(input$startDate))) +
          # geom_text(aes(x = isolate(input$startDate),
          #               label = "date observed",
          #               y = values$data$species[1]), 
          #           colour = rgb(0.5, 0.5, 0.5), vjust = 2.2, hjust = .33) +
          scale_x_date(limits = c(min(values$data$value), max(values$data$value)),
                       date_breaks = paste(ifelse(weekspan > 20, 4, 1), "weeks"),
                       date_minor_breaks = "1 week",
                       date_labels = "%d %b") +
          mytheme
        
      })
      
      ggplotly(p, tooltip = c("text"))
      
    })
    
    
    
    
    # })
    
    
    # observeEvent(input$update, {
    #   values$table2 <- isolate(values$table)
    # })
    # 
    # table_reactive <- reactive({
    #   # freezeReactiveValue(values, "table")
    #   values$table2
    #   
    #   
    # })
    
    
    # observeEvent(input$update, {
    # show thw table
    output$outtab <- DT::renderDataTable({
      
      isolate({
        
        cell_risk <- values$table %>%
          filter(Impact_scenario == input$impact) %>% 
          pull(Pest_risk_life_stage)
        cell_monit <- values$table %>% # values$table %>%
          filter(Impact_scenario == input$impact) %>% 
          pull(Pest_monitoring_life_stage)
        
        values$table %>% 
          filter(Impact_scenario == input$impact) %>% 
          setNames(., gsub("_", " ", names(.))) %>% 
          DT::datatable(escape = FALSE) %>% # show URLs - don't skip the html code
          DT::formatStyle(
            c("Pest risk life stage", "Pest monitoring life stage"),
            backgroundColor = DT::styleEqual(levels = c(cell_risk, cell_monit),
                                             values = c(alpha("red", 0.2), alpha("blue", 0.2)))
          )
        
      })
      
    })
    
    
  })
  
  
  
  
}


# run the app -------------------------------------------------------------
shinyApp(ui = ui, server = server)

