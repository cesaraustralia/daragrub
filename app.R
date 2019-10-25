library(tidyverse)
library(shiny)
library(raster)
library(leaflet)
library(shinythemes)
library(rgdal)
library(Cairo) # better antialiasing on text
options(shiny.usecairo=T)

# global
source('./getBug.R')
source('./develop.fun.R')

################## INTIALISE DATA #######################
myLabelFormat = function(...,dates=FALSE){ 
  if(dates){ 
    function(type = "numeric", cuts){ 
      format(as.Date(cuts, origin="1970-01-01"), '%b-%d')
    } 
  }else{
    labelFormat(...)
  }
}

Tmin<-brick('data/mu_Tmin_for_DOY_ag10.tif')
Tmax<-brick('data/mu_Tmax_for_DOY_ag10.tif')

curYear <- format(Sys.time(), '%Y')

bug.files<- list.files('bugs/')
bugs<-sapply(X = bug.files, FUN = strsplit, '[.]')
bugs<-unlist(bugs)[(1:length(bugs))*2-1]
bugs<-bugs[order(bugs)]
bugList<-list()

#build list of all bugs with data 
for (bug in bugs){
  insect<-getBug(bug)
  bugList[insect$name]<-bug
}

# load crop-pest susceptibility matrix
risk = read_csv("data/crop_risk_matrix.csv") %>%  
  mutate(Mcrop = str_extract(ModerateRisk, ".+?(?=\\s\\()")) %>% 
  mutate(Mpest = str_extract(ModerateRisk, "(?<=\\()(.*?)(?=\\))")) %>%
  mutate(Mpest = str_split(Mpest, ", ")) %>%
  mutate(Hcrop = str_extract(HighRisk, ".+?(?=\\s\\()")) %>% 
  mutate(Hpest = str_extract(HighRisk, "(?<=\\()(.*?)(?=\\))")) %>%
  mutate(Hpest = str_split(Hpest, ", ")) %>% 
  dplyr::select(Pest, Crop, Mcrop, Mpest, Hcrop, Hpest)

#UI 
ui <- 
  shinyUI(navbarPage("DARAGRUB",selected = 'Assess risk', theme = shinytheme("superhero"),
                     #################### LOCAL UI ####################
                     tabPanel("Assess risk",
                          fluidRow(
                            column(width = 4,offset =0, "",
                              selectInput("species", label = h4("1. Species observed:"), 
                                          choices = bugList, 
                                          selected = bugList[[1]],
                                          width = '100%'),
                              dateInput('startDate', label = h4("2. Date pest observed:"), 
                                        value = paste0(curYear,'-6-1'), 
                                        min = paste0(curYear,'-1-1'), 
                                        max = paste0(curYear,'-12-31'),
                                        format = "dd-MM", startview = "month", weekstart = 0,
                                        language = "en", width = '100%'),
                              uiOutput("startStage"),
                              selectInput("crop", label = h4('4. Crop development'), 
                                          choices = unique(risk$Crop), 
                                          selected = unique(risk$Crop)[1],
                                          width = '100%'),
                              uiOutput("modcontrols"),
                              uiOutput("highcontrols"),
                              HTML('<br/>'),
                              h4('5. Choose location'),
                              leafletOutput("map"),
                              HTML('<br/>'),
                              h4('6. Run simulation'),
                              actionButton("update", "Run"),
                              HTML('<br/>'),
                              h4('7. Download data as table'),
                              downloadButton('downloadData.csv', 'Download'),
                              HTML('<br/>')
                              
                            ),
                            
                            column(8, 
                                   fluidRow( 
                                     imageOutput("preImage", height = 300),
                                     HTML('<br/>'),HTML('<br/>'),
                                     HTML('<br/>'),HTML('<br/>'),HTML('<br/>'),
                                     HTML('<br/>'),HTML('<br/>'),HTML('<br/>'),
                                     HTML('<br/>'),HTML('<br/>'),HTML('<br/>'),
                                       # style = "position:relative",
                                       plotOutput("phenology")
                                     
                                   )
                                   ),
                            column(6, offset = 2, 
                                   # HTML(paste(rep('<br/>', 10))),
                                   HTML('<br/>'),HTML('<br/>'),
                                   HTML('<br/>'),HTML('<br/>'),HTML('<br/>'),
                                   HTML('<br/>'),HTML('<br/>'),HTML('<br/>'),
                                   HTML('<br/>'),HTML('<br/>'),HTML('<br/>'),
                                   
                                   tableOutput('table')
                                   )
                          )    
                     ),
           ################ ABOUT UI #######################
           tabPanel("About",
                    fluidRow(column(5,'',
                                    h1('DARAGRUB'),
                                    h2('Description'),
                                    h4('Effective management of insect pests in crops requires an understanding of the rate at which insects develop. For example, it might be important to know how long a damaging stage of a pest may persist in the crop, or when eggs might hatch. The DARAGRUB program provides a convenient and readily available means of predicting development times using different insect models. Gridded climatic data of daily temperatures is used in these models to generate estimates of the dates of occurrence for each stage throughout the whole life-cycle of an insect.'),
                                    h2('Climate data'),
                                    h4('The Australian gridded climatic data used to estimate developmental times is derived from 15-year averages of the max and min temperatures at each day of the year. A daily temperature profile is calculated using a simple trigonometric function, with an amplitude spanning the max and min daily temperatures over a period of 24 hours.'),
                                    h2('Insect data'),
                                    h4('The rate of growth and development of insects and other invertebrates is strongly influenced by temperature. The temperature dependence of development varies between species, thus each species has a unique temperature response. Indeed, even within a species the temperature dependence may vary between different stages. This is accounted for in the model by assigning unique developmental functions to each stage of each insect. This functional response is derived from empirical data.'),
                                    h4('The temperature - growth rate relationship for each of the pest species modelled in this platform can be viewed opposite. The species-specific variables and rate functions for each were derived from published records, and can be varied in consultation with Dr James Maino. Similarly, new insect models for different pests can be added to this platform at any time, when based on published empirical data.'),
                                    h4('For the maintenance and updating of this tool, please contact Dr James Maino (info@cesaraustralia.com).
                                       ')
                                    ),
                             column(5,'',
                                    selectInput("species3", label = h4("Select species:"), 
                                                choices = bugList, 
                                                selected = bugList[[2]]),
                                    plotOutput('tempresponse'),
                                    HTML('<br/>'),
                                    htmlOutput('source')
                                    )
                      
                    )
           )
  )
)
# SERVER 
server <- function(input, output, session){
  ######################### ASSESS RISK ###################################
  output$startStage <- renderUI({
    insect<-getBug(input$species)
    stageList<-lapply(1:length(names(insect$dev.funs)), FUN = function(x) x)
    names(stageList)<-names(insect$dev.funs)
    selectInput("startStage", label = h4("3. Life stage observed:"),
                choices = stageList,
                selected = 2, width = '100%')
  })
  output$modcontrols <- renderUI({
    isolate({pestcroprisk = values$pestcroprisk})
    pestcroprisk = risk %>% 
      filter(Pest == input$species) %>%
      filter(Crop == input$crop)
    if(nrow(pestcroprisk) < 1 || is.na(pestcroprisk$Mcrop)) {
      updateDateRangeInput(session, "modcroprisk", start = NA, end = NA)
      return(NULL)
    } else{
      cropstring = sprintf("Estimated period of %s %s", 
                           tolower(input$crop), tolower(pestcroprisk$Mcrop))
      dateRangeInput('modcroprisk', cropstring, format = "dd-MM")
    }
  })
  
  output$highcontrols <- renderUI({
    isolate({pestcroprisk = values$pestcroprisk})
    pestcroprisk = risk %>% 
      filter(Pest == input$species) %>%
      filter(Crop == input$crop)
    if(nrow(pestcroprisk) < 1 || is.na(pestcroprisk$Hcrop)) {
      updateDateRangeInput(session, "highcroprisk", start = NA, end = NA)
      return(NULL)
    } else{
      cropstring = sprintf("Estimated period of %s %s", 
                           tolower(input$crop), tolower(pestcroprisk$Hcrop))
      dateRangeInput('highcroprisk', cropstring, format = "dd-MM")
    }
  })
  
  values <- reactiveValues()
  values$df <- NULL
  values$count<-1
  # map to select location
  output$map <- renderLeaflet({
    leaflet(options = leafletOptions(minZoom = 4)) %>%
      addProviderTiles(providers$OpenStreetMap,
                       options = providerTileOptions(noWrap = TRUE)
      ) %>%
      setView(lng = 130, lat= -30, zoom = 4) %>% 
      setMaxBounds( lng1 = 113,
                    lat1 = -45,
                    lng2 = 155,
                    lat2 = -10 )
  })
  observeEvent(input$map_click,{
    click <- input$map_click
    text<-sprintf("Latitude: %1.2f; Longtitude %1.2f",click$lat, click$lng)
    proxy <- leafletProxy("map")
    proxy %>% clearPopups() %>%
      addPopups(click$lng, click$lat, text)
  })
  
  # run simulation
  newEntry <- observe({
    if(input$update>0){
      withProgress(message = "LOADING. PLEASE WAIT...", value = 0, { # create progress bar
        isolate({
          if(is.null( input$map_click)) {
            longlat = matrix(c(120, -30), ncol = 2)
          } else {
            longlat = matrix(c(input$map_click$lng, input$map_click$lat), ncol = 2)  
          } 
          startDay<-as.numeric(format(input$startDate,'%j'))
          TMAX <- extract(Tmax, longlat)
          TMIN <- extract(Tmin, longlat)
          startStage<-ifelse(is.null(input$startStage),2,as.numeric(input$startStage))
          insect<-getBug(input$species)
          crop = input$crop
          species = input$species
          # browser()
          data<-develop(TMAX,TMIN, startDay, startStage, insect)

        })
      })
      values$pestcroprisk = risk %>% 
        filter(Pest == species) %>%
        filter(Crop == crop) 

      isolate({
        pestcroprisk = values$pestcroprisk
        df<-as.data.frame(data[1,,])
        df$stage<-names(insect$dev.funs)
        Hpest = values$pestcroprisk %>% pull(Hpest) %>% unlist
        Mpest = values$pestcroprisk %>% pull(Mpest) %>% unlist
        df$risk = "Low risk"
        df$risk = ifelse(df$stage %in% Mpest, "Moderate risk", df$risk)
        df$risk = ifelse(df$stage %in% Hpest, "High risk", df$risk)
        df$risk = factor(df$risk, levels = c("Low risk","Moderate risk","High risk"))
        df$life<-insect$life
        df$location<-input$simname
        df$species< input$species
        mdf <- gather(df, variable, value, Time_start, Time_end)
        mdf$value <- as.Date(paste0(curYear,'-1-1')) + mdf$value
        df = mdf
        
        croprisk = tibble(stage="crop risk period", risk="Low risk", value = range(df$value))
        if(nrow(pestcroprisk) > 0 && !is.na(pestcroprisk$Hcrop)){
          croprisk = bind_rows(
            croprisk,
            tibble(stage="crop risk period", risk="High risk", life = "crop plant",
                   variable = c("Time_start", "Time_end"),value = input$highcroprisk),
          )
        }
        if(nrow(pestcroprisk) > 0 && !is.na(pestcroprisk$Mcrop)){
          croprisk = bind_rows(
            croprisk,
            tibble(stage="crop risk period", risk="Moderate risk", life = "crop plant", 
                   variable = c("Time_start", "Time_end"), value = input$modcroprisk),
          )
        }
        df = bind_rows(df, croprisk)
        df$stage<- factor(df$stage, levels = c(names(insect$dev.funs), 'crop risk period'))
        values$df <- df
        values$count<-values$count+1
      })
    }
  })
 
  output$phenology<- renderPlot({
    mytheme<-theme_bw()+
              theme(text = element_text(size=20, color = 'white'),
                    axis.text.x = element_text(color = 'white',angle=45, vjust=1, hjust=1),
                    axis.text.y = element_text(color = 'white'),
                    legend.title=element_blank(),
                    legend.key = element_blank(),
                    panel.grid.minor = element_blank(),
                    panel.grid.major = element_blank(),
                    panel.border = element_blank(),
                    panel.background = element_blank(),
                    legend.background = element_blank(),
                    axis.line.x = element_line(color="white"),
                    plot.background = element_blank()
                    )
    if(input$update>0){
      if(isolate(length(values$df))){
        if(TRUE){
          data<-filter(values$df, !is.na(value))
          data$risk<- factor(data$risk, levels = c("Low risk", "Moderate risk", "High risk"))
          weekspan<-as.numeric( max(data$value)-min(data$value))/7
          # p = ggplot() + geom_line(data = croprisk, aes(value, species), color = "red")
          # browser()
          p<-ggplot(data)+
            geom_point(aes(value, stage, colour = risk), size=6.2, drop=FALSE, show.legend = FALSE)+
            geom_line(aes(value, stage, colour = risk),size = 6, drop=FALSE) +
            ylab(NULL) +
            xlab(NULL) +
            # label date pest observed
            geom_text(aes(x=isolate(input$startDate), label="stage observed", 
                          y=names(insect$dev.funs)[as.integer(input$startStage)]), 
                      colour=rgb(0.5,0.5,0.5), vjust = 2.2,hjust = .33) +
            scale_color_manual(values = c("green","orange","red"), drop = FALSE) +
            scale_y_discrete(drop=FALSE) +
            scale_x_date(limits = c(min(c(data$value), na.rm=T), 
                                    max(c(data$value), na.rm=T)),
                         date_breaks = paste(ifelse(weekspan>20,4,1),"weeks"),date_minor_breaks = '1 week',
                         date_labels = "%d %b" ) +
            ggtitle(isolate(paste(tolower(sub("_"," ", input$species)), "in", tolower(input$crop)))) +
            mytheme + geom_hline(yintercept = length(levels(data$stage)) - 0.5, color = "white")
          # if("crop risk period" %in% data$stage)
          #   p = p 
          return(p)
        }
      }
    }else{return(NULL)}

  },bg="transparent", width = 1000, height = 500)
  output$downloadData.csv <- downloadHandler(
    filename = function() { paste('data_darabug.csv', sep='') },
    content = function(file) {
      if(input$update>0){
        data<-filter(isolate(values$df), stage != "crop risk period") 
        data  %>% 
          mutate(variable = sub("Time_", "Stage ", variable)) %>% 
          mutate(value = format(value, "%d/%m/%Y")) %>%
          mutate(crop = input$crop) %>%
          spread(variable, value) %>% 
          arrange(stage) %>%
          dplyr::select(Stage = stage, Crop = crop,  Risk = risk, `Stage start`, `Stage end`) %>% 
          write_csv(file)
        
      }
    }
  )
  output$table <- renderTable({
    if(input$update>0){
      data<-filter(isolate(values$df), stage != "crop risk period") 
      data  %>% 
        mutate(variable = sub("Time_", "Stage ", variable)) %>% 
        mutate(value = format(value, "%d/%m/%Y")) %>%
        mutate(crop = input$crop) %>%
        spread(variable, value) %>% 
        arrange(stage) %>%
        dplyr::select(Stage = stage, Crop = crop,  Risk = risk, `Stage start`, `Stage end`)

    }
    })
  
  output$preImage <- renderImage({
    # When input$n is 3, filename is ./images/image3.jpeg
    filename <- normalizePath(file.path(sprintf("www/%s.png", input$species)))
    
    # Return a list containing the filename and alt text
    list(src = filename,
         alt = paste("Image number", input$n))
    
  }, deleteFile = FALSE)

    ########################## INSECT PLOT PAGE ###########################
    # include about here
    output$tempresponse<- renderPlot(bg="transparent",{
      temps<- seq(0,50,length = 1000)
      insect<-getBug(input$species3)
      df <- data.frame(temp = temps)
      stages<-names(insect$dev.funs)
      for (stage in stages){
        df[,stage]<-insect$dev.funs[[stage]](temps)
      }
      dl<-reshape2::melt(df, id = 'temp', variable.name = 'stage',
                         value.name = 'dev')
      p<-ggplot() + geom_line(data = dl, aes(x = temp, y = dev, linetype = stage, color = stage)) +
        xlab('Temperature (C)')+
        ylab('Development rate (1/d)')+ theme(text = element_text(size=20, colour = 'white'), #family='Nirmala UI',
              axis.text = element_text(size=20, colour = 'white'),
              legend.title=element_blank(),
              legend.key = element_blank(),
              panel.grid.minor = element_blank(),
              panel.grid.major = element_blank(),
              panel.border = element_blank(),
              panel.background = element_blank(),
              legend.background = element_blank(),
              axis.line.x = element_line(color="white"),
              axis.line.y = element_line(color="white"),
              plot.background = element_blank(),
              axis.ticks = element_line(color = "white")
        )


      return(p)
    })
    output$source<-reactive({
      insect<-getBug(input$species3)
      return(HTML(paste('Source:<br>', insect$source)))
      })

  } # server(...


  
  # output$vals <- renderPrint({
  #   hover <- input$plot_hover 
  #   y <- nearPoints(values$df, input$plot_hover)[,c('stage','variable','value','Stage_duration')]
  #   # y <- nearPoints(data(), input$plot_hover)["wt"]
  #   y<-subset(y, variable == 'Time_start')
  #   req(nrow(y) != 0)
  #   # y is a data frame and you can freely edit content of the tooltip 
  #   # with "paste" function 
  #   
  #   print(sprintf('%s stage begins on %s \nlasting %1.1f days','egg', format(as.Date('2016-1-1'),'%d-%b'), 2.33))
  # })


shinyApp(ui = ui, server = server)
