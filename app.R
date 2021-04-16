library(tidyverse)
library(shiny)
library(shinythemes)
library(maptools)
library(rgeos)
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
  "When should I be monitoring?",
  "I have found a pest in my crop. What is the potential impact?"
)

pest_urls <- list(
  Common_Armyworm = "https://cesaraustralia.com/pestfacts/native-armyworms-in-crops-this-spring/",
  Native_Budworm = "https://cesaraustralia.com/pestfacts/monitor-for-native-budworm-grubs/"
)

len_tabel <- tibble::tribble(
  ~stages,    ~Native_Budworm,  ~Common_Armyworm,
    "egg",              "egg",             "egg",
     "L1",    "L1 (1 - 3 mm)",   "L1 (2 - 3 mm)",
     "L2",    "L2 (4 - 7 mm)",   "L2 (3 - 8 mm)",
     "L3",   "L3 (8 - 13 mm)",  "L3 (7 - 16 mm)",
     "L4",  "L4 (14 - 23 mm)", "L4 (14 - 22 mm)",
     "L5",  "L5 (24 - 28 mm)", "L5 (18 - 30 mm)",
     "L6", "L6 (29 - 40+ mm)", "L6 (25 - 40 mm)",
   "pupa",             "pupa",            "pupa",
  "adalt",            "adalt",           "adalt"
)





# ui ----------------------------------------------------------------------
ui <- shinyUI(
  navbarPage("PESTIMATOR", selected = "Monitoring assist", theme = shinytheme("journal"), # slate
             
             
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
                             shiny::htmlOutput("pestInfoUI"), # link to the pest facts
                             
                             selectInput("crop", label = h4("3. Select crop:"),
                                         choices = unique(scenrio_table$Crop),
                                         selected = unique(scenrio_table$Crop)[1],
                                         width = "100%"
                             ),
                             
                             # uiOutput("impactUI"),
                             
                             selectInput(inputId = "impact",
                                         label = h4("4. What crop stage are you concerned about?"),
                                         choices = unique(scenrio_table$Crop_risk_period),
                                         selected = unique(scenrio_table$Crop_risk_period)[1],
                                         width = "100%"
                             ),
                             
                             
                             shiny::htmlOutput("datetitle"),
                             dateRangeInput("crop_dev", label = NULL, #label = h4("5. Define risk periods:"),
                                            min = paste0(curYear,"-1-1"),
                                            max = paste0(curYear,"-12-31"),
                                            format = "dd-MM", startview = "month", weekstart = 0,
                                            language = "en", width = "85%"
                             ),
                             
                             
                             
                             # update based on user input
                             shiny::htmlOutput("stagetitle"),
                             
                             column(6,
                                    uiOutput("observeUI")
                             ),
                             column(6,
                                    uiOutput("stageUI")
                             ),
                             
                             
                             # uiOutput("date_ui"), # add UI for the second option
                             # HTML("<br/>"),
                             shiny::htmlOutput("maptitle"),
                             span(textOutput("checklatlong"), style = "color:red"),
                             # add a leaflet map
                             leafletOutput("smap", height = 300)
                             
                      ),
                      
                      column(8,
                             
                             # HTML("<br/>"),
                             shiny::htmlOutput("runtitle"),
                             actionButton("update", "Predict"),
                             HTML("<br/>"),
                             
                             
                             HTML("<br/>"),
                             DT::dataTableOutput("outtab"), 
                             
                             # output plot
                             HTML("<br/>"),
                             HTML("<br/>"),
                             # plotlyOutput("phenology"),
                             plotOutput("phenology"),
                             
                             shiny::htmlOutput("plotcaption")
                             
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
  values$start_stage <- "L1"
  values$stage_names <- NA
  values$monitor_stage <- NA
  values$crop_date <- Sys.Date()
  
  
  # update impact scenario and crop options
  observeEvent(input$species, {
    crop_list <- scenrio_table %>%
      filter(Pest == input$species) %>%
      pull(Crop) %>%
      unique()
    # update crop based on species
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
                      choices = unique(imsc$Crop_risk_period),
                      selected = unique(imsc$Crop_risk_period)[1],
    )
  })

  
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
      filter(Crop_risk_period == input$impact) %>%
      pull("Pest_risk_life_stage") %>% 
      strsplit(", ") %>% 
      pluck(1) %>% # this cause problems if there is more than one row
      identity()
    values$start_stage <- values$risk_stage[1]
    values$monitor_stage <- values$table %>% 
      filter(Crop_risk_period == input$impact) %>%
      pull("Pest_monitoring_life_stage") %>% 
      strsplit(", ") %>% 
      pluck(1) %>% # this cause problems if there is more than one row
      identity()
  })
  
  
  
  
  output$stagetitle <- shiny::renderUI({
    if(input$selection == select_option[2]){
      h4("6. Observed pest details:")
    }
  })
  output$stageUI <- renderUI({
    if(input$selection == select_option[2]){
      insect <- getBug(input$species)
      stageList <- lapply(1:length(names(insect$dev.funs)), FUN = function(x) x)
      names(stageList) <- names(insect$dev.funs)
      values$stage_length <- pull(len_tabel, input$species)
      selectInput("stage", label = "What size was it?", 
                  choices = values$stage_length, # names(stageList),  
                  selected = values$stage_length[2], # names(stageList)[2],
                  width = "100%")  
    }
  })
  output$observeUI <- renderUI({
    if(input$selection == select_option[2]){
      dateInput("observe_date", label = "When you did see it?",
                min = paste0(curYear,"-1-1"),
                max = paste0(curYear,"-12-31"),
                format = "dd-MM", startview = "month", weekstart = 0,
                language = "en", 
                width = "100%")
    }
  })
  
  output$pestInfoUI <- renderUI({
    pesturl <- pest_urls[[input$species]]
    HTML(paste0("<a href='", pesturl, "'>", "Click here for identification infromation on the selected pest!","</a>"))
  })
  
  # update the line of the map and the input date
  observeEvent(input$crop_dev, {
    values$crop_line <- data.frame(date = c(input$crop_dev[1], input$crop_dev[2]), type = "2")
    values$crop_date <- input$crop_dev[1]
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
      h4("7. Predict pest growth:")
    } else{
      h4("8. Predict pest growth:")
    }
  })
  
  output$datetitle <- shiny::renderUI({
    h4(sprintf("Estimate the timing of %s for %s:", tolower(input$impact), tolower(input$crop)))
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
  
  
  
  
  
  observeEvent(input$update, {
    
    # update with the change in selection
    if(input$selection == select_option[2]){
      values$crop_date <- input$observe_date
      values$start_stage <- values$stage_names[which(values$stage_length == input$stage)]
    }
    
    withProgress(message = "LOADING. PLEASE WAIT...", value = 0, { # create progress bar
      # isolate({
      longlat <- matrix(c(input_coords$long, input_coords$lat), ncol = 2)
      TMAX <- terra::extract(Tmax, longlat)
      TMIN <- terra::extract(Tmin, longlat)
      # startStage <- ifelse(input$startStage == "", 2, as.numeric(input$startStage))
      startStage <- which(values$stage_names == values$start_stage)
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
    
    # # create the rectangle data
    # rang <- c("red", "yellow", "black")
    # rang2 <- c("green", "blue", "purple")
    # data_rect <- data.frame(x1 = data %>% 
    #                           filter(name == "Time_start",
    #                                  stage == dplyr::first(values$risk_stage)) %>% 
    #                           pull(value), 
    #                         x2 = data %>% 
    #                           filter(name == "Time_end",
    #                                  stage == dplyr::last(values$risk_stage)) %>% 
    #                           pull(value), 
    #                         y1 = dplyr::first(values$stage_names), 
    #                         y2 = dplyr::last(values$stage_names), 
    #                         fill = rang[1],
    #                         action = "Risk stage")
    # data_rect1 <- data.frame(x1 = data %>% 
    #                            filter(name == "Time_start",
    #                                   stage == dplyr::first(values$monitor_stage)) %>% 
    #                            pull(value), 
    #                          x2 = data %>% 
    #                            filter(name == "Time_end",
    #                                   stage == dplyr::last(values$monitor_stage)) %>% 
    #                            pull(value), 
    #                          y1 = dplyr::first(values$stage_names), 
    #                          y2 = dplyr::last(values$stage_names), 
    #                          fill = rang2[1],
    #                          action = "Monitoring stage")
    # data_rect <- rbind(data_rect, data_rect1)
    # 
    # data_rect <- data_rect %>% 
    #   mutate(y1 = str_to_sentence(y1),
    #          y2 = str_to_sentence(y2))
    
    data_rect <- data.frame(x1 = values$crop_line$date[1], 
                            x2 = values$crop_line$date[2], 
                            y1 = str_to_sentence(dplyr::first(values$stage_names)), 
                            y2 = str_to_sentence(dplyr::last(values$stage_names)), 
                            fill = "green",
                            action = "Crop stage of concern")
    
    # change the colour of growth bars
    barcolour <- rep("gray75", length(values$stage_names))
    barcolour[which(values$stage_names %in% values$monitor_stage)] <- alpha("blue", 0.3)
    barcolour[which(values$stage_names %in% values$risk_stage)] <- alpha("red", 0.3)
    
    
    # values$data_rect <- data_rect
    # values$data <- data
    
    # data for the text on the risk scenario
    # mean(x1, x2)
    text_dt <- data.frame(x = mean(values$crop_line$date), 
                          y = dplyr::last(levels(data$stage)),
                          t = values$table %>%
                            filter(Crop_risk_period == input$impact) %>% 
                            pull(Crop_risk_period))
    


    # output$phenology <- renderPlotly({
    output$phenology <- renderPlot({
      
      isolate({
        
        # if you want to have a round line with in ggplotly
        # remove the alpha and add pale colours
        
        p <- ggplot(data = data) +
          # geom_point(aes(x = value, y = stage, color = stage), size = 5.5) +
          geom_line(aes(x = value, y = stage, color = stage), size = 8, lineend = "round") +
          geom_rect(aes(xmin = x1, xmax = x2, 
                        ymin = y1, ymax = y2, 
                        fill = fill, text = paste0(action)), 
                    data = data_rect, 
                    alpha = 0.4) +
          geom_vline(data = values$crop_line, color = "gray60",
                     aes(xintercept = as.numeric(date), linetype = type)) +
          scale_linetype_manual(values = unique(as.numeric(values$crop_line$type))) +
          geom_text(data = text_dt, aes(x = x, y = y, label = t), nudge_y = 0.3) +
          ylab(NULL) +
          xlab(NULL) +
          scale_color_manual(values = barcolour) +
          scale_fill_manual(values = c("red")) +
          scale_x_date(limits = c(min(data$value), max(data$value)),
                       date_breaks = paste(ifelse(weekspan > 20, 4, 1), "weeks"),
                       date_minor_breaks = "1 week",
                       date_labels = "%d %b") +
          mytheme
        
      })
      
      plot(p)
      # ggplotly(p, tooltip = c("text"))
      
    })
    
    # add plot caption
    output$plotcaption <- shiny::renderUI({
      h5("Plot interpretation advice: (this will be filled later)")
    })
    

    # show thw table
    output$outtab <- DT::renderDataTable({
      
      isolate({
        
        cell_risk <- values$table %>%
          filter(Crop_risk_period == input$impact) %>% 
          pull(Pest_risk_life_stage)
        cell_monit <- values$table %>% # values$table %>%
          filter(Crop_risk_period == input$impact) %>% 
          pull(Pest_monitoring_life_stage)
        
        values$table %>% 
          filter(Crop_risk_period == input$impact) %>% 
          setNames(., gsub("_", " ", names(.))) %>% 
          DT::datatable(escape = FALSE) %>% # show URLs - don't skip the html code
          DT::formatStyle(
            c("Pest risk life stage", "Pest monitoring life stage"),
            backgroundColor = DT::styleEqual(levels = c(cell_risk, cell_monit),
                                             values = c(alpha("red", 0.3), alpha("blue", 0.3)))
          )
        
      })
      
    })
    
    
  })
  
  
  
  
}


# run the app -------------------------------------------------------------
shinyApp(ui = ui, server = server)

