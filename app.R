library(tidyverse)
library(shiny)
library(shinythemes)

library(shinyWidgets)
library(tableHTML)

library(maptools)
library(rgeos)
library(terra)
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
scenrio_table <- read_csv("data/new_pestimate_db.csv") %>%
  mutate(Link_to_resources = paste0("<a href='", Link_to_resources, "'>", "More information!","</a>"))
head(scenrio_table)


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
  # "I have found a pest in my crop. What is the potential impact?"
  "Key (rea-ltime) management information."
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
                                         selected = select_option[1],
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
                             
                             
                             # change the backgorund colour for the slider
                             tags$style(make_css(list('.irs-bar',
                                                      c('border-top', 'border-bottom', 'background'),
                                                      rep('#FF000000', 3)),
                                                 list('.irs-bar-edge',
                                                      c('background', 'border'),
                                                      c('#FF000000', '0px !important')),
                                                 list('.irs-single',
                                                      'background',
                                                      ''))),
                             sliderTextInput(
                               inputId = "temp",
                               label = "7. Adjust the temperature (optional)",
                               choices = seq(from = -3,
                                             to = 3,
                                             by = 0.5),
                               selected = 0,
                               grid = TRUE
                             ),

                             
                             
                             # uiOutput("date_ui"), # add UI for the second option
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
                             shiny::htmlOutput("runtitle"),
                             actionButton("update", "Predict"),
                             HTML("<br/>"),
                             
                             
                             HTML("<br/>"),
                             # DT::dataTableOutput("outtab"), 
                             
                             shiny::htmlOutput("tablecaption"),
                             
                             # output plot
                             HTML("<br/>"),
                             # plotlyOutput("phenology"),
                             plotOutput("phenology"),
                             
                             # HTML("<br/>"),
                             # shiny::htmlOutput("plotcaption")
                             
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
      # selectInput("stage", label = "What stage or size?", 
      #             choices = values$stage_length, # names(stageList),  
      #             selected = values$stage_length[2], # names(stageList)[2],
      #             width = "100%")  
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
    # h4(sprintf("Estimate the timing of %s for %s:", tolower(input$impact), tolower(input$crop)))
    h5(sprintf("What is the approximate date for this growth stage of %s:", tolower(input$crop)))
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
    # update selected region value with each click
    input_coords$reg <- sf::st_point(x = c(input_coords$long, input_coords$lat)) %>% 
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
      sprintf("Selected location is in %s region", input_coords$reg[1])
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
      plot_height <- 400 + 150 * ifelse(length(input$stage) > 2, length(input$stage) - 2, 0)
    }
    # loops <- ifelse(is.null(input$stage), 1, input$stage)
    
    nstage <- 0 # count the stage for separating plots
    for(i in seq_along(loops)){
      # browser()
      nstage <- nstage + 1
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
        mdf <- mutate(mdf, type = 1) # add type for plotting multiple start stages
        values$df <- rbind(values$df, mdf) # this append plots together
        # values$df <- rbind(df, mdf) # this append plots together
        # values$df <- mdf # this is for single plot
        values$count <- values$count + 1
      })
    }
    
    
    # data <- values$df
    # get back to the wider form - fix this to have wide form from beginning
    data <- values$df %>% 
      pivot_wider(names_from = name, values_from = value) %>% 
      as.data.frame()
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
    data_rect <- data.frame(x1 = values$crop_line$date[1], 
                            x2 = values$crop_line$date[2], 
                            y1 = str_to_sentence(dplyr::first(values$stage_names)), 
                            y2 = str_to_sentence(dplyr::last(values$stage_names)), 
                            fill = "green",
                            action = "Crop stage of concern")
    
    # change the colour of growth bars
    barcolour <- rep("gray75", length(values$stage_names))
    barcolour[which(values$stage_names %in% values$monitor_stage)] <- "blue"# alpha("blue", 0.3)
    barcolour[which(values$stage_names %in% values$risk_stage)] <- "red" # alpha("red", 0.3)
    
    
    # values$data_rect <- data_rect
    # values$data <- data
    
    # data for the text on the risk scenario
    # mean(x1, x2)
    text_dt <- data.frame(x = mean(values$crop_line$date), 
                          y = dplyr::last(levels(data$stage)),
                          t = values$table %>%
                            filter(Susceptible_crop_stage_of_interest == input$impact) %>% 
                            pull(Susceptible_crop_stage_of_interest))
    
    
    
    # output$phenology <- renderPlotly({
    output$phenology <- renderPlot({
      
      isolate({
        # if you want to have a round line with in ggplotly
        # remove the alpha and add pale colours
        # browser()
        
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
          geom_rect(aes(xmin = x1, xmax = x2, 
                        ymin = y1, ymax = y2, 
                        # text = paste0(action),
                        fill = fill), 
                    data = data_rect, 
                    alpha = 0.4) +
          geom_vline(data = values$crop_line, color = "gray60",
                     aes(xintercept = as.numeric(date))) +
          # scale_linetype_manual(values = unique(as.numeric(values$crop_line$type))) +
          geom_text(data = text_dt, aes(x = x, y = y, label = t), nudge_y = 0.3) +
          # ylab(NULL) +
          # xlab(NULL) +
          scale_color_manual(values = barcolour) +
          scale_fill_manual(values = c("red")) +
          scale_x_date(limits = c(min(data$Time_start),
                                  max(data$Time_end)),
                       date_breaks = paste(ifelse(weekspan > 20, 4, 1), "weeks"),
                       date_minor_breaks = "1 week",
                       date_labels = "%d %b") +
          geom_hline(yintercept = seq_along(unique(data$stage)[-1]) + .5, 
                     linetype = "dashed", 
                     size = .1) +
          labs(x = NULL, 
               y = NULL,
               caption = "\nPlot interpretation advice: \n Consider management action if high risk scenario identified otherwise continue monitoring."
          ) +
          mytheme
        
        # original plot
        # p <- ggplot(data = data) +
        #   # geom_point(aes(x = value, y = stage, color = stage), size = 5.5) +
        #   geom_line(aes(x = value, y = stage, color = stage), size = 8, lineend = "round") +
        #   geom_rect(aes(xmin = x1, xmax = x2, 
        #                 ymin = y1, ymax = y2, 
        #                 fill = fill, text = paste0(action)), 
        #             data = data_rect, 
        #             alpha = 0.4) +
        #   geom_vline(data = values$crop_line, color = "gray60",
        #              aes(xintercept = as.numeric(date), linetype = type)) +
        #   scale_linetype_manual(values = unique(as.numeric(values$crop_line$type))) +
        #   geom_text(data = text_dt, aes(x = x, y = y, label = t), nudge_y = 0.3) +
        #   ylab(NULL) +
        #   xlab(NULL) +
        #   scale_color_manual(values = barcolour) +
        #   scale_fill_manual(values = c("red")) +
        #   scale_x_date(limits = c(min(data$value), max(data$value)),
        #                date_breaks = paste(ifelse(weekspan > 20, 4, 1), "weeks"),
        #                date_minor_breaks = "1 week",
        #                date_labels = "%d %b") +
        #   mytheme
        
      })
      
      plot(p)
      # ggplotly(p, tooltip = c("text"))
      
    }, height = plot_height)
    
    # # add plot caption
    # output$plotcaption <- shiny::renderUI({
    #   h5("Plot interpretation advice: if susceptible crop stage of interest overlaps with pest risk life stage (pink bars), you should do monitoring.")
    # })
    
    
    # add table caption
    output$tablecaption <- shiny::renderUI({
      
      isolate({
        
        h5(values$table %>% # values$table %>%
             filter(Susceptible_crop_stage_of_interest == input$impact) %>% 
             pull(Management_ext_info) %>% 
             unique()
        )
        
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

