library(tidyverse)
library(shiny)
library(shinythemes)

library(shinyWidgets)
library(tableHTML)
library(ggtext)

library(maptools)
library(rgeos)
library(terra)
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
scenrio_table <- read_csv("data/new_pestimate_db.csv") %>%
  mutate(Link_to_resources = paste0("<a href='", Link_to_resources, "'>", "More information!","</a>"))
head(scenrio_table)

# read regional data
regional <- read_csv("data/pestimate_db_regional.csv")

# points in Australia
data("wrld_simpl", package = "maptools")
auss_bound <- subset(wrld_simpl, NAME == "Australia") %>% 
  sf::st_as_sf() %>% 
  sf::st_set_crs(4326) # (CRS("+init=epsg:4326"))
# returns true or false
xy_in_aus <- function(long, lat){
  data.frame(x = long, y = lat) %>% 
    sf::st_as_sf(coords = 1:2, crs = 4326) %>% 
    sf::st_intersection(sf::st_geometry(auss_bound)) %>% 
    nrow() != 0
}
# read Australia reseach hubs
res_hubs <- sf::st_read("data/res_hubs.gpkg", quiet = TRUE)


# ggplot theme
mytheme <- theme_bw() +
  theme(text = element_text(size = 20,
                            family = "Nirmala UI",
                            color = "black"),
        axis.text.x = element_text(color = "black", angle = 45, vjust = 1, hjust = 1),
        axis.text.y = element_text(color = "black"),
        axis.title.y = element_text(color = "black", margin = margin(t = 0, r = 20, b = 0, l = 0)),
        legend.title = element_blank(),
        legend.key = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        panel.background = element_rect(fill = "transparent"), # bg of the panel
        plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
        legend.background = element_blank(),
        axis.line.x = element_line(color = "black"),
        legend.position = "none",
        plot.caption = element_markdown(size = 14)
  )

select_option <- c(
  "When should I be monitoring?",
  "Predict observed pest development."
)

pest_urls <- list(
  Common_Armyworm = "https://cesaraustralia.com/pestnotes/caterpillars/armyworm/",
  Native_Budworm = "https://cesaraustralia.com/pestnotes/caterpillars/native-budworm/"
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



# make_css(list('.irs-bar',
#               c('border-top', 'border-bottom', 'background'),
#               rep('#FF000000', 3)),
#          list('.irs-bar-edge',
#               c('background', 'border'),
#               c('#FF000000', '0px !important')),
#          list('.irs-single',
#               'background',
#               ''),
#          file = "slider_css.css")



# ui ----------------------------------------------------------------------
ui <- shinyUI(
  navbarPage("PESTIMATOR", selected = "Monitoring assist", theme = shinytheme("journal"), # slate
             
             # Lifestage panel ---------------------------------------------------------
             tabPanel("Monitoring assist",
                      
                      column(4,
                             
                             selectInput("species", label = h4("1. Select the pest:"), 
                                         choices = bugList, 
                                         selected = bugList[[1]],
                                         width = "100%"
                             ),
                             shiny::htmlOutput("pestInfoUI"), # link to the pest facts
                             
                             selectInput("selection", label = h4("2. What do you want to know:"), 
                                         choices = select_option, 
                                         selected = select_option[2],
                                         width = "100%"
                             ),
                             
                             selectInput("crop", label = h4("3. Select crop:"),
                                         choices = unique(scenrio_table$Crop),
                                         selected = unique(scenrio_table$Crop)[1],
                                         width = "100%"
                             ),
                             
                             # uiOutput("impactUI"),
                             
                             selectInput(inputId = "impact",
                                         label = h4("4. What crop stage are you concerned about?"),
                                         choices = unique(scenrio_table$Susceptible_crop_stage_of_interest),
                                         selected = unique(scenrio_table$Susceptible_crop_stage_of_interest)[1],
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
                             
                             
                             
                             # add dynamic title for temp adjustment
                             shiny::htmlOutput("temptitle"),
                             # change the background colour for the slider
                             tags$style(make_css(list('.irs-bar',
                                                      c('border-top', 'border-bottom', 'background'),
                                                      rep('#FF000000', 3)),
                                                 list('.irs-bar-edge',
                                                      c('background', 'border'),
                                                      c('#FF000000', '0px !important')),
                                                 list('.irs-single',
                                                      'background',
                                                      ''))),
                             # tags$style("./slider_css.css"),
                             sliderTextInput(
                               inputId = "temp",
                               label = NULL,
                               choices = seq(from = -3,
                                             to = 3,
                                             by = 0.5),
                               selected = 0,
                               grid = TRUE
                             ),

                             
                             # HTML("<br/>"),
                             shiny::htmlOutput("maptitle"),
                             # show selected region
                             textOutput(outputId = "region_selected"),
                             span(textOutput("checklatlong"), style = "color:red"),
                             # add a leaflet map
                             leafletOutput("smap", height = 300)
                             
                      ),
                      
                      column(8,
                             
                             # HTML("<br/>"),
                             
                             
                             switchInput(
                               inputId = "toggle",
                               label = "New plot?",
                               value = TRUE
                             ),
                             
                             
                             shiny::htmlOutput("runtitle"),
                             actionButton("update", "Predict"),
                             HTML("<br/>"),
                             
                             
                             HTML("<br/>"),
                             # DT::dataTableOutput("outtab"), 
                             
                             shiny::htmlOutput("tablecaption"),
                             
                             # output plot
                             HTML("<br/>"),
                             # plotlyOutput("phenology"),
                             plotOutput("phenology")
                            
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
                      choices = unique(imsc$Susceptible_crop_stage_of_interest),
                      selected = unique(imsc$Susceptible_crop_stage_of_interest)[1],
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
        # Potentil_impacts == input$impact
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
      filter(Susceptible_crop_stage_of_interest == input$impact) %>%
      pull("Pest_risk_life_stage") %>% 
      strsplit(", ") %>% 
      pluck(1) %>% # this cause problems if there is more than one row
      identity()
    values$start_stage <- values$risk_stage[1]
    values$monitor_stage <- values$table %>% 
      filter(Susceptible_crop_stage_of_interest == input$impact) %>%
      pull("Pest_monitoring_life_stage") %>% 
      strsplit(", ") %>% 
      pluck(1) %>% # this cause problems if there is more than one row
      identity()
  })
  
  
  
  # title for the stage
  output$stagetitle <- shiny::renderUI({
    if(input$selection == select_option[2]){
      h4("5. Observed pest details:")
    }
  })
  output$stageUI <- renderUI({
    if(input$selection == select_option[2]){
      insect <- getBug(input$species)
      stageList <- lapply(1:length(names(insect$dev.funs)), FUN = function(x) x)
      names(stageList) <- names(insect$dev.funs)
      values$stage_length <- pull(len_tabel, input$species)
      pickerInput(
        inputId = "stage",
        label = "What stage(s) or size(s)?", 
        choices = values$stage_length,
        selected = values$stage_length[2],
        multiple = TRUE
      )
    }
  })
  output$observeUI <- renderUI({
    if(input$selection == select_option[2]){
      dateInput("observe_date", label = "When you did see this pest?",
                min = paste0(curYear,"-1-1"),
                max = paste0(curYear,"-12-31"),
                format = "dd-MM", startview = "month", weekstart = 0,
                language = "en", 
                width = "100%")
    }
  })
  
  output$pestInfoUI <- renderUI({
    pesturl <- pest_urls[[input$species]]
    HTML(paste0("<a href='", pesturl, "'>", "Click here for identification information on the selected pest!","</a>"))
  })
  
  # update the line of the map and the input date
  observeEvent(input$crop_dev, {
    values$crop_line <- data.frame(date = c(input$crop_dev[1], input$crop_dev[2]), type = "2")
    values$crop_date <- input$crop_dev[1]
  })
  
  
  ##*****************************************************
  # update the date range based on selected region
  to_listen3 <- reactive({
    list(input_coords$region, input$crop, input$impact)
  })
  observeEvent(to_listen3(), {
    date_filer <- regional %>% 
      filter(Crop == input$crop,
             Region == input_coords$region,  
             Susceptible_crop_stage_of_interest == input$impact)
    updateDateRangeInput(session = session, 
                         inputId = "crop_dev",
                         start = paste(curYear, date_filer$Start_date, sep = "-"),
                         end = paste(curYear, date_filer$End_date, sep = "-"))
    
  })
  ##*****************************************************
  
  
  
  # change map title base on the input
  output$temptitle <- shiny::renderUI({
    if(input$selection == select_option[1]){
      h4("5. Adjust the temperature (optional):", align = "left")
    } else{
      h4("6. Adjust the temperature (optional):", align = "left")
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
      h4("7. Predict pest growth:")
    } else{
      h4("8. Predict pest growth:")
    }
  })
  
  output$datetitle <- shiny::renderUI({
    # h5(sprintf("What is the approximate date for this growth stage of %s:", tolower(input$crop)))
    h5("Adjust the default crop stage for your region if needed:")
  })
  
  
 
  # set default values for click
  input_coords <- reactiveValues()
  input_coords$long <- 145.0
  input_coords$lat <- -37.7
  input_coords$region <- "VIC"
  # update the click
  observe({
    if(!is.null(input$smap_click)){
      if(xy_in_aus(input$smap_click$lng, input$smap_click$lat)){
        input_coords$long <- round(input$smap_click$lng, 5)
        input_coords$lat <- round(input$smap_click$lat, 5)     
      }
    }
    # update selected region value with each click
    input_coords$region <- sf::st_point(x = c(input_coords$long, input_coords$lat)) %>% 
      sf::st_sfc() %>% 
      sf::st_intersection(res_hubs, .) %>% 
      sf::st_drop_geometry() %>% 
      dplyr::pull(Hub)
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
  
  # show the selected region
  output$region_selected <- renderText({
    if(!is.null(input$smap_click) && !xy_in_aus(input$smap_click$lng, input$smap_click$lat)) {
      NULL
    } else{
      sprintf("Selected location is in %s region", input_coords$region[1])
    }
  })
  
  
  
  
  observeEvent(input$update, {
    
    # adjust the temperature after hitting update button
    if(input$temp != 0){
      Tmax <- Tmax + input$temp
      Tmin <- Tmin + input$temp
    }
    
    
    # to reset the plot by updating
    values$df <- NULL
    # do the for loop even if the input$stage is NULL because of input$selection
   
    if(is.null(input$stage)){
      loops <- 1
      plot_height <- 400 # default plot height
    } else{
      loops <- input$stage
      if(!input$toggle){
        plot_height <- 400 + 150 * ifelse(length(input$stage) > 2, length(input$stage) - 2, 0)
      } else{
        plot_height <- 400
      }  
    }

    
    for(i in seq_along(loops)){
      # browser()
      # update with the change in selection
      if(input$selection == select_option[2]){
        # browser()
        values$crop_date <- input$observe_date
        values$start_stage <- values$stage_names[which(values$stage_length == input$stage[i])]
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
        mdf <- mutate(mdf, type = input$stage[i]) # add type for plotting multiple start stages
        values$df <- rbind(values$df, mdf) # this append plots together
        # values$df <- rbind(df, mdf) # this append plots together
        # values$df <- mdf # this is for single plot
        values$count <- values$count + 1
      })
    }
    
    
    # data <- values$df
    
    
    
    
    # get back to the wider form - fix this to have wide form from beginning
    if(!input$toggle){
      data <- values$df %>% 
        pivot_wider(names_from = name, values_from = value) %>% 
        as.data.frame()
    } else{
      data <- values$df
    }
    
    
    
    
    # browser()
    data$life <- factor(data$life)
    if("adult" %in% levels(data$life))
      data$life <- factor(data$life,
                          levels = c(levels(factor(data$life))[-1], levels(factor(data$life))[1]),
                          ordered = TRUE)
    # sort the factor levels
    data$stage <- fct_relevel(str_to_sentence(data$stage), str_to_sentence(values$stage_names))
    weekspan <- as.numeric(max(data$value) - min(data$value)) / 7
    
    # create the rectangle data
    if(!input$toggle){
    data_rect <- data.frame(x1 = values$crop_line$date[1], 
                            x2 = values$crop_line$date[2], 
                            # y1 = str_to_sentence(dplyr::first(values$stage_names)), 
                            # y2 = str_to_sentence(dplyr::last(values$stage_names)),
                            y1 = 0, # to stretch the rectangle
                            y2 = length(unique(values$stage_names)) + 0.5,
                            # fill = "green",
                            action = "Crop stage of concern")
    } else{
      data_rect <- data.frame(x1 = values$crop_line$date[1], 
                              x2 = values$crop_line$date[2], 
                              y1 = 0, # to stretch the rectangle
                              y2 = length(unique(data$type)) + 0.5
      )
    }
    
    # change the colour of growth bars
    barcolour <- rep("gray75", length(values$stage_names))
    barcolour[which(values$stage_names %in% values$monitor_stage)] <- "blue"# alpha("blue", 0.3)
    barcolour[which(values$stage_names %in% values$risk_stage)] <- "red" # alpha("red", 0.3)
    
    

    # data for the text on the risk scenario
    text_dt <- data.frame(x = mean(values$crop_line$date), 
                          
                          
                          y = ifelse(input$toggle, last(data$type), dplyr::last(levels(data$stage))),
                          # y = dplyr::last(levels(data$stage)),
                          
                          t = values$table %>%
                            filter(Susceptible_crop_stage_of_interest == input$impact) %>% 
                            pull(Susceptible_crop_stage_of_interest))
    
    
    
    # output$phenology <- renderPlotly({
    output$phenology <- renderPlot({
      
      isolate({
        # if you want to have a round line with in ggplotly
        # remove the alpha and add pale colours
        # browser()
        
        if(!input$toggle){
          
        p <- ggplot(data = data) +
          geom_linerange(aes(y = stage,
                             xmin = Time_start,
                             xmax = Time_end,
                             colour = stage),
                         size = 5,
                         # lineend = "round",
                         position = position_dodge2(width = 0.7)) +
          geom_point(aes(x = Time_start, y = stage, colour = stage),
                     size = 4,
                     position = position_dodge2(width = 0.7)) +
          geom_point(aes(x = Time_end, y = stage, colour = stage),
                     size = 4,
                     position = position_dodge2(width = 0.7)) +
          annotate(geom = "rect", 
                   xmin = as.Date(data_rect$x1),
                   xmax = as.Date(data_rect$x2),
                   ymin = data_rect$y1, 
                   ymax = data_rect$y2,
                   fill = "red", 
                   alpha = 0.3) +
          geom_vline(data = values$crop_line, color = "gray60",
                     aes(xintercept = as.numeric(date))) +
          # scale_linetype_manual(values = unique(as.numeric(values$crop_line$type))) +
          geom_text(data = text_dt, aes(x = x, y = y, label = t), nudge_y = 0.3) +
          scale_color_manual(values = barcolour) +
          scale_fill_manual(values = c("red")) +
          scale_x_date(limits = c(min(data$Time_start, as.Date(data_rect$x1)),
                                  max(data$Time_end, as.Date(data_rect$x2))),
                       date_breaks = paste(ifelse(weekspan > 20, 4, 1), "weeks"),
                       date_minor_breaks = "1 week",
                       date_labels = "%d %b") +
          geom_hline(yintercept = seq_along(unique(data$stage)[-1]) + .5, 
                     linetype = "dashed", 
                     size = .1) +
          labs(x = NULL, 
               y = NULL,
               caption = "<br/><b style='color:blue'>Blue:</b> <b>pest monitoring life stage(s)</b>
               <b style='color:red'>Red:</b> <b>pest risk life stage(s)</b> <br/>
               <p>Consider management action if high risk scenario identified otherwise continue monitoring.</p>
               "
          ) +
          mytheme
        
        } else{
          
          pt <- data %>%
            group_by(type) %>%
            summarise(first = dplyr::first(value),
                      last = dplyr::last(value)) %>%
            pivot_longer(cols = 2:3) %>%
            mutate(stage = ifelse(name == "first", levels(data$stage)[1], levels(data$stage)[length(levels(data$stage))]))
          
          # library(ggrepel)
          lbl <- data %>% 
            group_by(type, stage) %>% 
            summarise(x = mean(value), y = type)
          
          p <- ggplot(data = data, aes(x = value, y = type, color = stage, label = stage)) +
            geom_line(size = 8, # lineend = "round",
                      position = position_dodge2(width = dodge)) +
            geom_point(data = pt, aes(x = value, y = type, color = stage), size = 7) +
            geom_text(data = lbl, aes(x = x, y = y, label = stage), color = "black") +
            annotate(geom = "rect", 
                     xmin = as.Date(data_rect$x1),
                     xmax = as.Date(data_rect$x2),
                     ymin = data_rect$y1, 
                     ymax = data_rect$y2,
                     fill = "red", 
                     alpha = 0.3) +
            geom_text(data = text_dt, aes(x = x, y = y, label = t), nudge_y = 0.3, inherit.aes = FALSE) +
            scale_color_manual(values = barcolour) +
            scale_fill_manual(values = c("red")) +
            scale_x_date(limits = c(min(data$value, as.Date(data_rect$x1)),
                                    max(data$value, as.Date(data_rect$x2))),
                         date_breaks = paste(ifelse(weekspan > 20, 4, 1), "weeks"),
                         date_minor_breaks = "1 week",
                         date_labels = "%d %b") +
            geom_hline(yintercept = seq_along(unique(data$type)[-1]) + .5,
                       linetype = "dashed",
                       size = .1) +
            labs(x = NULL, 
                 y = "Observed stages",
                 caption = "<br/><b style='color:blue'>Blue:</b> <b>pest monitoring life stage(s)</b>
               <b style='color:red'>Red:</b> <b>pest risk life stage(s)</b> <br/>
               <p>Consider management action if high risk scenario identified otherwise continue monitoring.</p>
               "
            ) +
            mytheme
          
        }
        
        
        
        # add observation line
        if(input$selection == select_option[2]){
          p <- p + geom_vline(xintercept = input$observe_date, 
                              size = 1.5,
                              alpha = 0.4,
                              linetype = 6,
                              color = "black") +
            geom_text(x = input$observe_date - 2, 
                      y = "L4",
                      angle = 90,
                      size = 4.5,
                      label = "Observed date",
                      color = "black")
        }


        
      })
      
      plot(p)
      # ggplotly(p, tooltip = c("text"))
      
    }, height = plot_height)

    
    # add table caption
    output$tablecaption <- shiny::renderUI({
      
      isolate({
        
        HTML("<h4>Potential impacts:</h4>",
             sprintf("<h5>%s</h5>", 
                     values$table %>% # values$table %>%
                       filter(Susceptible_crop_stage_of_interest == input$impact) %>%
                       pull(Potentil_impacts) %>%
                       unique()),
             # "<br/>",
             sprintf("<h5>%s</h5>", values$table %>% # values$table %>%
                       filter(Susceptible_crop_stage_of_interest == input$impact) %>%
                       pull(Management_ext_info) %>%
                       unique()),
             "<hr/>",
             sprintf("<h4>Management action (%s region):</h4>", input_coords$region[1]),
             sprintf("<h5>%s</h5>", values$table %>% # values$table %>%
                       filter(Susceptible_crop_stage_of_interest == input$impact) %>%
                       pull(Management_actions) %>%
                       unique()),
             # "<br/>"
             "<hr/>")
        
      })
    })
    
    # show the information table
    # output$outtab <- DT::renderDataTable({
    #   
    #   isolate({
    #     
    #     cell_risk <- values$table %>%
    #       filter(Susceptible_crop_stage_of_interest == input$impact) %>% 
    #       pull(Pest_risk_life_stage) %>% 
    #       unique()
    #     cell_monit <- values$table %>% # values$table %>%
    #       filter(Susceptible_crop_stage_of_interest == input$impact) %>% 
    #       pull(Pest_monitoring_life_stage) %>% 
    #       unique()
    #     
    #     values$table %>% 
    #       dplyr::select(Pest,
    #                     Species,
    #                     Crop,
    #                     Potentil_impacts,
    #                     Susceptible_crop_stage_of_interest,
    #                     Pest_monitoring_life_stage,
    #                     Pest_risk_life_stage,
    #                     Management_actions,
    #                     Link_to_resources) %>% 
    #       filter(Susceptible_crop_stage_of_interest == input$impact) %>% 
    #       setNames(., gsub("_", " ", names(.))) %>% 
    #       DT::datatable(escape = FALSE) %>% # show URLs - don't skip the html code
    #       DT::formatStyle(
    #         c("Pest risk life stage", "Pest monitoring life stage"),
    #         backgroundColor = DT::styleEqual(levels = c(cell_risk, cell_monit),
    #                                          values = c(alpha("red", 0.3), alpha("blue", 0.3)))
    #       )
    #     
    #   })
    #   
    # })
    
    
  })
  
  
  
  
}


# run the app -------------------------------------------------------------
shinyApp(ui = ui, server = server)

