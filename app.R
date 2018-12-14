#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
#install.packages("XLConnect")
#install.packages("XLConnectJars")
#install.packages("leaflet")
#install.packages("shinyWidgets")
#install.packages("plotly")
# install.packages("tmap")
# install.packages("tmaptools")
# install.packages("sf")
# install.packages("leaflet")
# install.packages("geojsonio")
# install.packages("devtools")
#install.packages("ggrepel")
# install.packages("forcats")
# install.packages("scales)
library(shiny)
library(XLConnect)
library(leaflet)
library(tidyr)
library(dplyr)
library(ggplot2)
library(DT)
library(forecast)
library(shinyWidgets)
library(plotly)
#library(tmap)
#library(tmaptools)
library(sf)
library(geojsonio)
library(devtools)
library(ggrepel)
library(forcats)
library(scales)

#START*******************1.0 INITIALIZED DATA*******************************START
dfcrimeOri <- readWorksheetFromFile("MalaysiaCrime.xlsx", sheet = 1)

#this is to gather the State column into rows
dfcrime <- gather(dfcrimeOri, "State", "Cases", 2:15)
#Create 2 new columns Latitude and Longitude
dfcrime$Lat <- 0
dfcrime$Long <- 0

#-------This section is to assign latitude and longitude-----
#Perlis
dfcrime <- within(dfcrime, Lat[State == 'Perlis'] <- 6.4406)
dfcrime <- within(dfcrime, Long[State == 'Perlis'] <- 100.1984)

#Kedah
dfcrime <- within(dfcrime, Lat[State == 'Kedah'] <- 6.1248)
dfcrime <- within(dfcrime, Long[State == 'Kedah'] <- 100.3678)

#Pulau Pinang
dfcrime <- within(dfcrime, Lat[State == 'Pulau.Pinang'] <- 5.4356)
dfcrime <- within(dfcrime, Long[State == 'Pulau.Pinang'] <- 100.3091)

#Perak
dfcrime <- within(dfcrime, Lat[State == 'Perak'] <- 4.5975)
dfcrime <- within(dfcrime, Long[State == 'Perak'] <- 101.0901)

#Selangor
dfcrime <- within(dfcrime, Lat[State == 'Selangor'] <- 2.9936)
dfcrime <- within(dfcrime, Long[State == 'Selangor'] <- 101.2921)

#KL
dfcrime <- within(dfcrime, Lat[State == 'Kuala.Lumpur'] <- 3.139)
dfcrime <- within(dfcrime, Long[State == 'Kuala.Lumpur'] <- 101.6869)

#Negeri Sembilan
dfcrime <- within(dfcrime, Lat[State == 'Negeri.Sembilan'] <- 2.7259)
dfcrime <- within(dfcrime, Long[State == 'Negeri.Sembilan'] <- 101.9378)

#Melaka
dfcrime <- within(dfcrime, Lat[State == 'Melaka'] <- 2.1944)
dfcrime <- within(dfcrime, Long[State == 'Melaka'] <- 102.2491)

#Johor
dfcrime <- within(dfcrime, Lat[State == 'Johor'] <- 1.4927)
dfcrime <- within(dfcrime, Long[State == 'Johor'] <- 103.7414)

#Pahang
dfcrime <- within(dfcrime, Lat[State == 'Pahang'] <- 3.7634)
dfcrime <- within(dfcrime, Long[State == 'Pahang'] <- 103.2202)

#Kelantan
dfcrime <- within(dfcrime, Lat[State == 'Kelantan'] <- 6.1248)
dfcrime <- within(dfcrime, Long[State == 'Kelantan'] <- 102.2544)

#Terengganu
dfcrime <- within(dfcrime, Lat[State == 'Terengganu'] <- 5.3296)
dfcrime <- within(dfcrime, Long[State == 'Terengganu'] <- 103.137)

#Sabah
dfcrime <- within(dfcrime, Lat[State == 'Sabah'] <- 5.9804)
dfcrime <- within(dfcrime, Long[State == 'Sabah'] <- 116.0735)

#Sarawak
dfcrime <- within(dfcrime, Lat[State == 'Sarawak'] <- 1.5535)
dfcrime <- within(dfcrime, Long[State == 'Sarawak'] <- 110.3593)

#set mid point of Malaysia map
midpointMapLong <- 109.1172
midpointMapLat <- 4.8394

#get min/max year
min_year <- min(dfcrime$Year)
max_year <- max(dfcrime$Year)

#forecasting data prep
dfcrimeplot <- dfcrime[,c(3,4,1,5,6)]

dfcrimeplot <- dfcrimeplot %>% 
  group_by(Year, Criminal.Category, State) %>% 
  summarise(Cases = sum(Cases))

#get prediction
all_states <- sort(unique(dfcrimeplot$State))

dfcrimepredict <- NULL
for(x in unique(dfcrimeplot$State)){
  for(y in unique(dfcrimeplot$Criminal.Category)){
    dfcrimepredicttemp <- NULL
    dfcrimepredicttemp <- as.data.frame(na.omit(dfcrimeplot[dfcrimeplot$State == x & dfcrimeplot$Criminal.Category == y,c('State','Criminal.Category','Year','Cases')]))
    dfcrimepredicttemp$Type <- 'History'
    
    lastyear <- max(dfcrimepredicttemp$Year)
    
    #value <- c(dfcrimepredicttemp[order(dfcrimepredicttemp$Year, decreasing= F),'Cases'])
    #crime <- ts(value,frequency=1) # consider adding a start so you get nicer labelling on your chart. 
    #fit <- auto.arima(crime)
    fit <- auto.arima(dfcrimepredicttemp[order(dfcrimepredicttemp$Year, decreasing= F),'Cases'])
    fcast <- forecast(fit,h=5)
    newrow <- data.frame(State = x, Criminal.Category = y, Year = lastyear+1, Cases = fcast$mean[1], Type = 'Forecast', stringsAsFactors = FALSE)
    dfcrimepredicttemp <- rbind(dfcrimepredicttemp, data.frame(State = x, Criminal.Category = y, Year = lastyear+1, Cases = fcast$mean[1], Type = 'Forecast', stringsAsFactors = FALSE))
    newrow <- data.frame(State = x, Criminal.Category = y, Year = lastyear+2, Cases = fcast$mean[2], Type = 'Forecast', stringsAsFactors = FALSE)
    dfcrimepredicttemp <- rbind(dfcrimepredicttemp, newrow)
    newrow <- data.frame(State = x, Criminal.Category = y, Year = lastyear+3, Cases = fcast$mean[3], Type = 'Forecast', stringsAsFactors = FALSE)
    dfcrimepredicttemp <- rbind(dfcrimepredicttemp, newrow)
    newrow <- data.frame(State = x, Criminal.Category = y, Year = lastyear+4, Cases = fcast$mean[4], Type = 'Forecast', stringsAsFactors = FALSE)
    dfcrimepredicttemp <- rbind(dfcrimepredicttemp, newrow)
    newrow <- data.frame(State = x, Criminal.Category = y, Year = lastyear+5, Cases = fcast$mean[5], Type = 'Forecast', stringsAsFactors = FALSE)
    dfcrimepredicttemp <- rbind(dfcrimepredicttemp, newrow)
    
    if (is.null(dfcrimepredict)) {
      dfcrimepredict <- dfcrimepredicttemp
    } else {
      dfcrimepredict <- rbind(dfcrimepredict,dfcrimepredicttemp)
    }
  }
}

#import police station data
policestations=readWorksheetFromFile("Msia Police Stations.xlsx",sheet=1)
police_agg <- policestations %>% 
  group_by(State) %>% 
  summarise(Stations = sum(Number.of.police.stations + Number.of.police.stands)) 
police_agg<-police_agg[!(police_agg$State=="Federal Territory of Labuan"),]
#Create 2 new columns Latitude and Longitude
police_agg$Lat <- 0
police_agg$Long <- 0
#-------assign latitude and longitude-----
#Perlis
police_agg <- within(police_agg, Lat[State == 'Perlis'] <- 6.6626)
police_agg <- within(police_agg, Long[State == 'Perlis'] <- 100.3217)

#Kedah
police_agg <- within(police_agg, Lat[State == 'Kedah'] <- 5.993)
police_agg <- within(police_agg, Long[State == 'Kedah'] <- 100.4773)

#Pulau Pinang
police_agg <- within(police_agg, Lat[State == 'Pulau Pinang'] <- 5.1958)
police_agg <- within(police_agg, Long[State == 'Pulau Pinang'] <- 100.4921)

#Perak
police_agg <- within(police_agg, Lat[State == 'Perak'] <- 4.4022)
police_agg <- within(police_agg, Long[State == 'Perak'] <- 100.7098)

#Selangor
police_agg <- within(police_agg, Lat[State == 'Selangor'] <- 2.8190)
police_agg <- within(police_agg, Long[State == 'Selangor'] <- 101.5205)

#KL
police_agg <- within(police_agg, Lat[State == 'Federal Territory of Kuala Lumpur'] <- 3.2116)
police_agg <- within(police_agg, Long[State == 'Federal Territory of Kuala Lumpur'] <- 101.5697)

#Negeri Sembilan
police_agg <- within(police_agg, Lat[State == 'Negeri Sembilan'] <- 2.5225)
police_agg <- within(police_agg, Long[State == 'Negeri Sembilan'] <- 101.7963)

#Melaka
police_agg <- within(police_agg, Lat[State == 'Melaka'] <- 2.3822)
police_agg <- within(police_agg, Long[State == 'Melaka'] <- 102.2491)

#Johor
police_agg <- within(police_agg, Lat[State == 'Johor'] <- 1.6583)
police_agg <- within(police_agg, Long[State == 'Johor'] <- 103.606)

#Pahang
police_agg <- within(police_agg, Lat[State == 'Pahang'] <- 3.5834)
police_agg <- within(police_agg, Long[State == 'Pahang'] <- 102.7791)

#Kelantan
police_agg <- within(police_agg, Lat[State == 'Kelantan'] <- 5.8362)
police_agg <- within(police_agg, Long[State == 'Kelantan'] <- 102.4077)

#Terengganu
police_agg <- within(police_agg, Lat[State == 'Terengganu'] <- 4.7645)
police_agg <- within(police_agg, Long[State == 'Terengganu'] <- 103.4183)

#Sabah
police_agg <- within(police_agg, Lat[State == 'Sabah'] <- 6.8868)
police_agg <- within(police_agg, Long[State == 'Sabah'] <- 116.8253)

#Sarawak
police_agg <- within(police_agg, Lat[State == 'Sarawak'] <- 1.167)
police_agg<- within(police_agg, Long[State == 'Sarawak'] <- 110.5665)
#-------assign latitude and longitude-----

#data preparation for choropleth map
dfchoropmap=dfcrime%>%select(State,Year,Criminal.Category,Cases)
#Summarizing data
dfchoropmap=dfchoropmap%>%group_by(State,Year,Criminal.Category)%>%summarise(Cases=sum(Cases,na.rm=TRUE))
#Renaming states to be same as map file state names
dfchoropmap$State[dfchoropmap$State=="Kuala.Lumpur"]="Federal Territory of Kuala Lumpur"
dfchoropmap$State[dfchoropmap$State=="Negeri.Sembilan"]="Negeri Sembilan"
dfchoropmap$State[dfchoropmap$State=="Pulau.Pinang"]="Penang"
totaldfchoropmap=dfchoropmap%>%group_by(State,Year)%>%summarise(Cases=sum(Cases))%>%summarise(Cases=sum(Cases))

#importing malaysia choropleth map
states <- geojsonio::geojson_read("states.geojson", what = "sp")
#drop labuan and putrajaya file from map
states = states[states$Name != "Federal Territory of Labuan",]
states = states[states$Name != "Federal Territory of Putrajaya",]

#line up the states between our data and the shapefile
is.element(totaldfchoropmap$State, states$Name)

#data preparation for crime analysis
top3=dfcrime%>%arrange(desc(Cases))%>%group_by(Year,State)%>%slice(1:3)
top3replicate=top3
top3sum=top3%>%group_by(State,Year)%>%summarise(TotalCases=sum(Cases))
top3$Area=0

#top5states=dfcrime%>%arrange(desc(Cases))%>%group_by(Year, State)
#top5states <- top5states[,c(3, 5, 2)]
#top5sum=top5states%>%group_by(Year,State)%>%summarise(TotalCases=sum(Cases))

#-------This section is to assign state area-----
#Perlis
top3 <- within(top3, Area[State == 'Perlis'] <- 832)

#Kedah
top3 <- within(top3, Area[State == 'Kedah'] <- 9427)

#Pulau Pinang
top3 <- within(top3, Area[State == 'Pulau.Pinang'] <- 1048)

#Perak
top3 <- within(top3, Area[State == 'Perak'] <- 21035)

#Selangor
top3 <- within(top3, Area[State == 'Selangor'] <- 8104)

#KL
top3 <- within(top3, Area[State == 'Kuala.Lumpur'] <- 243)

#Negeri Sembilan
top3 <- within(top3, Area[State == 'Negeri.Sembilan'] <- 6686)

#Melaka
top3 <- within(top3, Area[State == 'Melaka'] <- 277)

#Johor
top3 <- within(top3, Area[State == 'Johor'] <- 19102)

#Pahang
top3 <- within(top3, Area[State == 'Pahang'] <- 35840)

#Kelantan
top3 <- within(top3, Area[State == 'Kelantan'] <- 15099)

#Terengganu
top3 <- within(top3, Area[State == 'Terengganu'] <- 13035)

#Sabah
top3 <- within(top3, Area[State == 'Sabah'] <- 73631)

#Sarawak
top3 <- within(top3, Area[State == 'Sarawak'] <- 124451)

#END*******************1.0 INITIALIZED DATA*******************************END

# Define UI for application that draws a histogram
ui <- bootstrapPage(
   
   # Application title
   #titlePanel("Mata-Mata"),
   tags$head(tags$link(rel = "icon", type = "image/png", href = "https://image.flaticon.com/icons/png/512/99/99381.png")),
   tags$style(type="text/css", "body {padding-top: 50px;}"),
   #tags$style(type = "text/css", ".nav-bar, .nav {padding-left: 15px !important; padding-right: 15px !important; }"),
   #tags$style(type = "text/css", ".container-fluid, #CrimeMap {padding-left: 0px !important; padding-right: 0px !important; }"),
   tags$style(type = "text/css", "#CrimeMap {margin-left: -15px !important; width: 101.4% !important; "),
   #leafletOutput("policemap"),p(),actionButton("recalc", "New points"),
   
   navbarPage("Malaysia Crime", 
              position = "fixed-top", inverse = TRUE, collapsible = TRUE, windowTitle = "Mata-Mata App",
              
              tabPanel("Exploration",
                   #PLACEHOLDER FOR EXPLORATION CONTENT
                   #h3("Exploration"),
                   tabsetPanel(type = "tabs",
                               id = "explotabsetpanel",
                               tabPanel(title = "Map",
                                        #tags$style(type = "text/css", "html, body, #CrimeMap {width:100%;height:100%}"),
                                        #uiOutput("leafl"),
                                        leafletOutput("CrimeMap", height = "91.8vh")
                               ),
                               tabPanel(title = "Plot",
                                        fluidRow(
                                          column(10, offset = 2,
                                             # Show scatterplot with brushing capability
                                             plotOutput(outputId = "scatterplot", 
                                                        brush = "plot_brush"
                                                        #hover = "plot_hover"
                                             ),
                                             DT::dataTableOutput(outputId = "selectedcrimetable"),
                                             br(),    
                                             plotOutput(outputId = "scatterplot1",
                                                        brush = "plot_brush"
                                             ),
                                             br(),    
                                             plotOutput(outputId = "scatterplot3",
                                                        brush = "plot_brush"
                                             ),
                                             br(),
                                             plotOutput(outputId = "scatterplot4",
                                                        brush = "plot_brush")											 
                                             
                                          )
                                        )
                               ),
                               tabPanel(title = "Table",
                                        br(),
                                        fluidRow(
                                          column(10, offset = 2,
                                                 #show data table
                                                 DT::dataTableOutput(outputId = "crimetable")   
                                          )
                                        )  
                               ),
                               tabPanel(title = "Chart",
                                        br(),
                                        #fluidRow(
                                        #  column(10, offset = 2,
                                       h3("Top 3 Crimes"),
                                       # Sidebar with a slider input for number of bins 
                                       sidebarLayout(
                                         sidebarPanel(
                                           #wellPanel(
                                           #h3("Data Filtering"),
                                           selectInput("InputYearAnalysis", label = "Year", choices = seq(min_year, max_year), selected = min_year),
                                           #sliderInput("InputYearRangePred", "Year:",min = min_year, max = max_year,step=1,value=c(max_year-10,max_year), sep = "")
                                           # Select State
                                           selectInput("InputStateAnalysis",
                                                       "State", 
                                                       choices = all_states, 
                                                       selected = "Johor",
                                                       multiple = F)
                                           #)
                                           , width = 2),
                                         
                                         # Show a plot of the generated distribution
                                         mainPanel(
                                           plotOutput(outputId = "analysisplot2", width = "40%", height = "400px"),
                                           plotOutput(outputId = "analysisplot", width = "60%", height = "600px")
                                           #plotlyOutput(outputId = "predictionplot", width = "100%", height = "1000px")
                                           , width = 10)
                                       )
                                          #)
                                        #)  
                               )
                   ),
                   conditionalPanel(condition = "input.explotabsetpanel != 'Chart'", 
                   #PLACE HOLDER FOR EXPLORATION FILTERING
                   absolutePanel(top = 200, left = 30, width = 300,
                       draggable = TRUE,
                       
                       HTML('<button data-toggle="collapse" data-target="#demo" class="btn btn-primary">Data Filtering</button>'),
                       tags$div(id = 'demo',  class="collapse in",
                                  wellPanel(
                                    #h3("Data Filtering"),
                                    #selectInput("InputYear", "Year", seq(1980, 2016)), 
                                    sliderInput("InputYearRange", "Year:",min = min_year, max = max_year,step=1,value=c(max_year-4,max_year), sep = ""),
                                    # uncomment this line to enable Color Scheme selection
                                    # selectInput("colors", "Color Scheme",
                                    #              rownames(subset(brewer.pal.info, category %in% c("seq", "div")))),
                                    # 
                                    selectInput("InputCategory", "Crime Category", c("ALL", "JENAYAH HARTABENDA", "JENAYAH KEKERASAN")),
                                    conditionalPanel(condition = "input.explotabsetpanel == 'Map'", 
                                                     checkboxInput("legend", "Show legend", TRUE)
                                                     ),
                                    style = "opacity: 0.9"
                                  )
                                )
                   )
                   )
              ),
              tabPanel("Prediction",
                       
                       h3("5 Years Crime Projection"),
                       # Sidebar with a slider input for number of bins 
                       sidebarLayout(
                         sidebarPanel(
                           #wellPanel(
                             #h3("Data Filtering"),
                             selectInput("InputYearRangePred", label = "Start Year", choices = seq(min_year, max_year-4), selected = min_year),
                             #sliderInput("InputYearRangePred", "Year:",min = min_year, max = max_year,step=1,value=c(max_year-10,max_year), sep = "")
                             # Select State
                             pickerInput("InputStatePred",
                                         "State", 
                                         choices = all_states, 
                                         options = list(`actions-box` = TRUE),
                                         selected = all_states,
                                         multiple = T)
                           #)
                           , width = 2),
                         
                         # Show a plot of the generated distribution
                         mainPanel(
                           #plotOutput(outputId = "predictionplot", width = "100%", height = "1000px")
                           plotlyOutput(outputId = "predictionplot", width = "100%", height = "1000px")
                           , width = 10)
                       )
                    )
   )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  #Filter Data -> dsFilteredData SHOULD BE USED FOR ALL OUTPUTS
  dsFilteredData <- reactive({
    ds00 <- dfcrime[dfcrime$Year %in% seq(from=min(input$InputYearRange),to=max(input$InputYearRange),by=1),-c(2,7,8)]
    ds0 <- ds00[,c(2,3,1,4,5)]
    if (input$InputCategory == "ALL"){
      #ds1 <- dfcrime[dfcrime$Year == input$InputYear, -c(2,7,8)]
      ds1 <- ds0
    }
    else{
      #ds1 <- dfcrime[dfcrime$Year == input$InputYear & dfcrime$Criminal.Category == input$InputCategory, -c(2,7,8)]
      ds1 <- ds0[ds0$Criminal.Category == input$InputCategory,]
    }
  })
  
  #Filter Data for Map used ONLY -> YJ, PLEASE CHK THE GROUPING BY MULTIPLE YEARS
  dsFilteredMapData <- reactive({
    ds0 <- dfcrime[dfcrime$Year %in% seq(from=min(input$InputYearRange),to=max(input$InputYearRange),by=1),]
    if (input$InputCategory == "ALL"){
      #ds1 <- dfcrime[dfcrime$Year == input$InputYear, -c(1,2,4)]
      ds1 <- ds0[,-c(1,2,4)]
      ds1 %>% 
        group_by(State, Lat, Long) %>% 
        summarise(Cases = sum(Cases)) #we name the aggregated sum column as Cases as well
    }
    else{
      #ds1 <- dfcrime[dfcrime$Year == input$InputYear & dfcrime$Criminal.Category == input$InputCategory, -c(1,2)]
      ds1 <- ds0[ds0$Criminal.Category == input$InputCategory,-c(1,2)]
      ds1 %>% 
        group_by(Criminal.Category, State, Lat, Long) %>% 
        summarise(Cases = sum(Cases)) #we name the aggregated sum column as Cases as well
    }
  })
  
  #Filter Data for Line Plot - answering data science question
  dsFilteredDataforPlot <- reactive({
    ds0 <- dfcrime[dfcrime$Year %in% seq(from=min(input$InputYearRange),to=max(input$InputYearRange),by=1),]
    if (input$InputCategory == "ALL"){
      #ds1 <- dfcrime[dfcrime$Year == input$InputYear, -c(1,2,4)]
      ds1 <- ds0[,c(3,6)]
      ds1 %>% 
        group_by(Year) %>% 
        summarise(Cases = sum(Cases)) #we name the aggregated sum column as Cases as well
    }
    else{
      #ds1 <- dfcrime[dfcrime$Year == input$InputYear & dfcrime$Criminal.Category == input$InputCategory, -c(1,2)]
      ds1 <- ds0[ds0$Criminal.Category == input$InputCategory, c(3,6)]
      ds1 %>% 
        group_by(Year) %>% 
        summarise(Cases = sum(Cases)) #we name the aggregated sum column as Cases as well
    }
  })
  
  #Filter Data for Top States Plot - answering data science question
  dsFilteredDataforTopStates <- reactive({
    ds0 <- dfcrime[dfcrime$Year %in% seq(from=min(input$InputYearRange),to=max(input$InputYearRange),by=1),]
    if (input$InputCategory == "ALL"){
      #ds1 <- dfcrime[dfcrime$Year == input$InputYear, -c(1,2,4)]
      ds1 <- ds0[,c(5,6)]
      ds1 <- ds1 %>% 
        group_by(State) %>% 
        summarise(Cases = sum(Cases)) %>%
        arrange(desc(Cases))
      ds1 <- head(ds1, 5)
      ds1 <- ds1 %>% 
        mutate(per=Cases/sum(Cases)) %>%
        mutate(ymax=cumsum(per)) %>%
        mutate(ymin=c(0, head(ymax, -1))) %>%
        mutate(label=scales::percent(per))
      
    }
    else{
      #ds1 <- dfcrime[dfcrime$Year == input$InputYear & dfcrime$Criminal.Category == input$InputCategory, -c(1,2)]
      ds1 <- ds0[ds0$Criminal.Category == input$InputCategory, c(5,6)]
      ds1 <- ds1 %>% 
        group_by(State) %>% 
        summarise(Cases = sum(Cases)) %>%
        mutate(per=Cases/sum(Cases)) %>%
        arrange(desc(Cases))%>%
        mutate(label=scales::percent(per))
      ds1 <- head(ds1, 5)
      ds1 <- ds1 %>% 
        mutate(per=Cases/sum(Cases)) %>%
        mutate(ymax=cumsum(per)) %>%
        mutate(ymin=c(0, head(ymax, -1))) %>%
        mutate(label=scales::percent(per))
    }
  })
  
  #Filter Data for Prediction
  dsFilteredPredData <- reactive({
    req(input$InputStatePred)
    dfcrimepredict[dfcrimepredict$Year >= input$InputYearRangePred & dfcrimepredict$State %in% input$InputStatePred,]
  })
  
  #Filter Data for Top 3 Crimes
  top3crime <- reactive({
    top3 %>% filter(Year == input$InputYearAnalysis) %>%
      filter(State == input$InputStateAnalysis)
  })
  
  top3crimepercent<- reactive({
    top3replicate %>% filter(Year == input$InputYearAnalysis) %>%
      filter(State == input$InputStateAnalysis)%>%
      group_by(Crime.Cases,Cases)%>%
      count()%>%
      ungroup()%>%
      mutate(per=Cases/sum(Cases)) %>%
      arrange(desc(Cases))%>%
      mutate(label=scales::percent(per))
  })
  
  policeIcon <- makeIcon(
    iconUrl = "policehat.png",
    iconWidth = 20, iconHeight = 20,
    iconAnchorX = 0, iconAnchorY = 0
  )
  
  layer=reactive({states})
  #output crime map and police stations
  output$CrimeMap <- renderLeaflet({
    leaflet() %>% 
      addProviderTiles(providers$OpenStreetMap.Mapnik, group = "Default Maptile") %>%
      addProviderTiles("MapBox", options = providerTileOptions(
        id = "mapbox.light",
        accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN')))%>%
      addPolygons(data=states)%>%
      addTiles()%>%
      addMarkers(~Long, ~Lat, data=police_agg, icon = policeIcon, label = ~as.character(Stations),
                 labelOptions = labelOptions(opacity=1, noHide = T, direction = 'top', offset=c(10,-15))) %>%
      #addCircleMarkers(data=policestations,~Long,~Lat,popup = paste("<b>District:</b>",policestations$District,
      #                                                              "</br><b>Police Stations:</b>",policestations$Number.of.police.stations,
      #                                                              "</br><b>Police Booths:</b>",policestations$Number.of.police.stands,
      #                                                              '</br><a href="', policestations$Address.Link, '" target="_blank">', "Address Directory", '</a>'))%>%
      setView(midpointMapLong, midpointMapLat, zoom = 7)
  })
  
    #%>% 
    #fitBounds(~min(long), ~min(lat), ~max(long), ~max(lat)) #%>%
  
  #output crime map observe function - add circles
  # Incremental changes to the map should be performed in
  # an observer. Our guideline:
  # Each independent set of things that can change
  # should be managed in its own observer.
  observe({
    #pal <- colorpal()
    #pal <- colorNumeric("input$colors", dfcrime$Cases)
    pal <- colorNumeric("Reds", dfcrime$Cases)
    leafletProxy("CrimeMap", data = dsFilteredMapData()) %>%
      clearShapes() %>%
      addCircles(lng = ~Long, lat=~Lat, radius = ~Cases/5, weight = 1, 
                 fillColor = ~pal(Cases),
                 fillOpacity = 0.7, label = ~as.character(Cases),
                 labelOptions = labelOptions(opacity=1, noHide = T,
                                             direction = 'top', offset=c(10,-15)))
    #popup = ~paste(Cases))
  })
  
  #output crime map observe function - add Legend
  # Use another observer to recreate the legend
  observe({
    proxy <- leafletProxy("CrimeMap", data = dsFilteredMapData())
    
    # Remove any existing legend, and only if the legend is
    # enabled, create a new one.
    proxy %>% clearControls()
    if (input$legend) {
      #pal <- colorNumeric(input$colors, dfcrime$Cases)
      pal <- colorNumeric("Reds", dfcrime$Cases)
      
      proxy %>% addLegend(position = "bottomright",
                          pal = pal, values = ~Cases
      )
    }
  })
  
  #output crime table
  #print selected movie data table
  output$crimetable <- DT::renderDataTable({
    crime_table <- dsFilteredData()
    DT::datatable(data = crime_table, 
                  options = list(pageLength = 20), 
                  rownames = FALSE)
  })
  
  #output the crime scatterplot: object the plotOutput function is expecting
  output$scatterplot <- renderPlot({
   ggplot(data = dsFilteredData(), 
          aes_string(x = 'State', y = 'Cases', color = 'Crime.Cases')) +
     geom_point()
  })
  #output selected crime from plot
  #print selected crime data table
  output$selectedcrimetable <- DT::renderDataTable({
    brushedPoints(dsFilteredData(), input$plot_brush) %>%
      #nearPoints(movies, input$plot_hover, xvar = input$x, yvar = input$y) %>%
      select(Year, Criminal.Category, Crime.Cases, State, Cases)
  })

  output$scatterplot1 <- renderPlot({
    #theme_set(theme_classic())
    #dt1001 <- readWorksheetFromFile("MalaysiaCrime(1).xlsx", sheet = 3)
    ggplot(data=dsFilteredData(), aes(x = Year,y = Cases,fill = Crime.Cases))+
      geom_bar(stat ="identity",width = 0.65,position ="stack")+     
      theme(axis.text.x = element_text(angle=65, vjust=0.6)) + 
      labs(x = "Year",y = "Cases", title="Total Crimes by Year") 
  })

  #output time series plot:
  output$scatterplot3 <- renderPlot({
    theme_set(theme_bw())
    #dt1001 <- readWorksheetFromFile("MalaysiaCrime(1).xlsx", sheet = 1)
    ggplot(data = dsFilteredDataforPlot(), aes(x=Year)) +
      #geom_line(aes(y=JENAYAH.KEKERASAN, col="JENAYAH.KEKERASAN")) +
      geom_line(aes(y=Cases), color = '#f8766d') +
      labs(title="Crime Cases Trend Analysis",
           subtitle="",
           caption="", y="Crime Cases") 
  })

  output$scatterplot4 <- renderPlot({
    
      #ggplot(data=dsFilteredDataforTopStates(), aes(x = "", y = State, fct_inorder(Cases))) +
      ggplot(data=dsFilteredDataforTopStates(), aes(fill=State, ymax=ymax, ymin=ymin, xmax=4, xmin=3)) +
      geom_rect() + 
      coord_polar("y", start = 0) +
      xlim(c(0, 4)) +
      #geom_label_repel(aes(label = label), size=5, show.legend = F, nudge_x = 1) +
      #guides(fill = guide_legend(title = "Group")) + 
      theme_bw() +
      theme(panel.grid=element_blank()) +
      theme(axis.text=element_blank()) +
      theme(axis.ticks=element_blank()) + 
      labs(title="Top 5 States with Highest Crime Cases")
  })

  #output the prediction crime plot: object the plotOutput function is expecting
  #output$predictionplot <- renderPlot({
  output$predictionplot <- renderPlotly({
    crime_predictiontable <- dsFilteredPredData()
    ggplot(crime_predictiontable) +
      geom_line(data = crime_predictiontable, aes(x = Year, y = Cases, group=State, colour = State), stat="smooth",method = "gam", formula = y ~ s(x),
                size = 1.5,
                alpha = 0.3) +
      geom_point(data = crime_predictiontable[crime_predictiontable$Type=='History',], aes(x = Year, y = Cases, group=State, colour = State), size = 1) +
      geom_point(data = crime_predictiontable[crime_predictiontable$Type=='Forecast',], aes(x = Year, y = Cases, group=State, colour = Type), shape = 17, size = 2, alpha = 1) +
      facet_wrap(~ Criminal.Category, scales = 'free_y', ncol = 1) 
    
  })
  
  output$analysisplot2 <- renderPlot({
    ggplot(top3crimepercent(), aes(x = "", y = per, fill = fct_inorder(Crime.Cases))) +
      geom_bar(width = 1, stat = "identity") +
      coord_polar("y", start = 0) +
      geom_label_repel(aes(label = label), size=5, show.legend = F, nudge_x = 1) +
      guides(fill = guide_legend(title = "Group"))
  })
  
  output$analysisplot <- renderPlot({
    ggplot(top3crime(),aes(x = Crime.Cases, y = Cases, fill=Crime.Cases))+geom_bar(stat = "identity")
  })
  

    
}

# Run the application 
shinyApp(ui = ui, server = server)

