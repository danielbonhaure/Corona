library(shiny)
library(leaflet)
library(RColorBrewer)
library(rgdal)
library(RCurl)
library(plotly)
library(viridis)
library(tidyverse)
library(geojsonsf)
library(sf)

variable <-F


# names(data)
url <- "https://twitter.com/intent/tweet?url=https://danielbonhaure.shinyapps.io/covid-19"
# 

# https://www.naturalearthdata.com/downloads/50m-cultural-vectors/50m-admin-0-countries-2/

# countries <- readOGR(dsn ="ne_50m_admin_0_countries", 
#                      layer = "ne_50m_admin_0_countries", 
#                      encoding = "utf-8",use_iconv = T,
#                      verbose = FALSE)

# save(countries, file="shapeFile.RData")
load("data/shapeFile.RData")

dataCook<- function(data, countries){
  
 
  data$`Country/Region`<-as.character(data$`Country/Region`)
  data$`Country/Region`[data$`Country/Region`=="Macau"]<- "Macao"
  data$`Country/Region`[data$`Country/Region`=="Mainland China"]<- "China"
  data$`Country/Region`[data$`Country/Region`=="South Korea"]<- "South Korea"
  data$`Country/Region`[data$`Country/Region`=="North Macedonia"]<- "Macedonia"
  data$`Country/Region`[data$`Country/Region`=="Czech Republic"]<- "Czechia"
  data$`Country/Region`[data$`Country/Region`=="Dominican Republic"]<- "Dominican Rep."
  data$`Country/Region`[data$`Country/Region`=="UK"]<- "United Kingdom"
  data$`Country/Region`[data$`Country/Region`=="Gibraltar"]<- "United Kingdom"
  data$`Country/Region`[data$`Country/Region`=="US"]<- "United States"
  data$`Country/Region`[data$`Country/Region`=="Saint Barthelemy"]<- "St-Barthélemy"
  
  data$`Country/Region`[data$`Country/Region`=="Faroe Islands"]<- "Faeroe Is."
  data$`Country/Region`[data$`Country/Region`=="Bosnia and Herzegovina"]<- "Bosnia and Herz."
  data$`Country/Region`[data$`Country/Region`=="Vatican City"]<- "Vatican"
  data$`Country/Region`[data$`Country/Region`=="Korea, South"]<- "South Korea"
  data$`Country/Region`[data$`Country/Region`=="Republic of Ireland"]<- "Ireland"
  data$`Country/Region`[data$`Country/Region`=="Taiwan*"]<-"Taiwan"
  
  data$`Country/Region`[data$`Country/Region`=="Congo (Kinshasa)"]<-"Congo"
  data$`Country/Region`[data$`Country/Region`=="Cote d'Ivoire"]<-"Côte d'Ivoire"
  data$`Country/Region`[data$`Country/Region`=="Reunion"]<-"France"
  data$`Country/Region`[data$`Country/Region`=="Martinique"]<-"France"
  data$`Country/Region`[data$`Country/Region`=="French Guiana"]<-"France"
  data$`Country/Region`[data$`Country/Region`=="Holy See"]<-"Vatican"
  data$`Country/Region`[data$`Country/Region`=="Cayman Islands"]<-"Cayman Is."
  data$`Country/Region`[data$`Country/Region`=="Guadeloupe"]<-"France"
  data$`Country/Region`[data$`Country/Region`=="Antigua and Barbuda"]<-"Antigua and Barb."
  
  data$`Country/Region`[data$`Country/Region`=="Curacao"]<-"Curaçao"
  data$`Country/Region`[data$`Country/Region`=="Guadeloupe"]<-"France"
  data$`Country/Region`[data$`Country/Region`=="occupied Palestinian territory"]<-"Palestine"
  data$`Country/Region`[data$`Country/Region`=="Congo (Brazzaville)"]<-"Congo"
  data$`Country/Region`[data$`Country/Region`=="Equatorial Guinea"]<-"Guinea"
  data$`Country/Region`[data$`Country/Region`=="Central African Republic"]<-"Central African Rep."
  
  data$`Country/Region`[data$`Country/Region`=="Central African Republic"]<-"Central African Rep."
  data$`Country/Region`[data$`Country/Region`=="Eswatini"]<-"eSwatini"
  
  

  # countries$NAME<-as.character(countries$NAME)
  # countries$NAME[is.na(countries$NAME)]<-"Côte d'Ivoire"
  data$Pays<-as.character(unique(countries$NAME)[charmatch(data$`Country/Region`,unique(countries$NAME))])
  print(data$`Country/Region`[is.na(data$Pays)])
  dataPays<- data%>%dplyr::select(-`Province/State`, -Lat, -Long,-`Country/Region`)%>%group_by(Pays)%>%summarise_each(sum)
  dataPays$Pays<-as.character(dataPays$Pays)
  return(dataPays)
}


# ver: https://carto.com/developers/sql-api/reference/#operation/getSQLStatement (para entender format)
URL <- RCurl::getURL("http://geo.stp.gov.py/user/dgeec/api/v2/sql?q=SELECT%20*%20FROM%20dgeec.paraguay_2002_departamentos&format=GeoJSON")
# URL <- "data/paraguayDepartamentos.geojson"
sf_py <- geojsonsf::geojson_sf(URL) %>%
  dplyr::select(Dpto = departamen)

URL <- RCurl::getURL("https://www.dgeec.gov.py/microdatos/cuadro/b7dc2DEP01-Paraguay-Poblacion-total-por-anio-calendario-segun-sexo-y-departamento-2000-2025.csv")
populationPY <- read.csv(text = URL, stringsAsFactors = F) %>%
# URL <- "data/paraguayPoblacion.csv"
# populationPY <- read.csv(file = URL, stringsAsFactors = F) %>%
  dplyr::select(Dpto = X., Pop = X2020) %>%
  dplyr::filter(dplyr::between(dplyr::row_number(), 2, 19)) %>%
  dplyr::mutate(Dpto = toupper(Dpto), Pop = as.numeric(Pop)) %>%
  dplyr::mutate(Dpto = ifelse(Dpto == 'NEEMBUCU', 'ÑEEMBUCU', Dpto))


# URL <- RCurl::getURL("https://raw.githubusercontent.com/danielbonhaure/Corona/master/data/py_confirmed_cases.csv")
# data <- read.csv(text = URL, check.names = F)
dataCasesPY <- read.csv(file = "data/py_confirmed_cases.csv", check.names = F, stringsAsFactors = F) %>% dplyr::select(-Cod)

# URL <- RCurl::getURL("https://raw.githubusercontent.com/danielbonhaure/Corona/master/data/py_deaths.csv")
# data <- read.csv(text = URL, check.names = F)
dataDeathsPY <- read.csv(file = "data/py_deaths.csv", check.names = F, stringsAsFactors = F) %>% dplyr::select(-Cod)



jour<-names(dataCasesPY%>%select(contains( "/")))
jourDate<- as.Date(jour, "%m/%d/%y")
names(dataCasesPY)[str_detect(names(dataCasesPY), "/")]<-format.Date(jourDate, "%m/%d/%y")
names(dataDeathsPY)[str_detect(names(dataDeathsPY), "/")]<-format.Date(jourDate, "%m/%d/%y")

dataCasesPY <- sf_py %>% sf::st_drop_geometry() %>% 
  dplyr::left_join(populationPY, by = "Dpto") %>%
  dplyr::left_join(dataCasesPY, by = "Dpto")

dataDeathsPY <- sf_py %>% sf::st_drop_geometry() %>% 
  dplyr::left_join(populationPY, by = "Dpto") %>%
  dplyr::left_join(dataDeathsPY, by = "Dpto")

arrondi<- function(x) 10^(ceiling(log10(x)))

dataDeathsPY[is.na(dataDeathsPY)]<- 0
dataCasesPY[is.na(dataCasesPY)]<- 0
ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}",
             HTML(  ".panel-default {background-color: rgb(256, 256, 256,0.5);
               padding : 10px;;}
               .panel-title {background-color: rgb(256, 256, 256,0.8);
               padding : 10px;
               border-style: solid;
               border-color: grey;}
               .panel-credits {background-color: rgb(256, 256, 256,1);
               padding : 15px;
               border-style: solid;
               border-color: black;}
               ")
             
  ),
  leafletOutput("map", width = "100%", height = "93%"),
  column(6, HTML("<b><a href='https://www.linkedin.com/in/daniel-bonhaure/'>Daniel BONHAURE</a></b></br>
                 Based on work of <b><a href='https://www.linkedin.com/in/thibaut-fabacher'>Thibaut FABACHER</a></b></br>
                 For the rest of the world: <b><a href='https://thibautfabacher.shinyapps.io/covid-19'>COVID-19 outbreak</a></b>")),
  column(2, br(), actionButton("twitter_share",
                               label = "Share",
                               icon = icon("twitter"),
                               onclick = sprintf("window.open('%s')",url)) 
  ),
  column(2, br(), checkboxInput("plotEvolT", "Show Evolution",F)
  ),
  column(2, br(), checkboxInput("credits", "Credits", FALSE)
  ),
  
  
  absolutePanel(id = "input_date_control",class = "panel panel-default",bottom = 60, left = 10, draggable = F,
                selectInput("choices", "Cases or Deaths ?", choices = c("Cases","Deaths"),selected = "Cases"),
                uiOutput("Slider"),
                helpText("The detail of each departament can be obtained by clicking on it."), 
                uiOutput("selection"),
                checkboxInput("legend", "Show legend", TRUE)
                
  ),
  uiOutput("Credits"),
  uiOutput("plotEvol"),
  absolutePanel(id = "name",class = "panel panel-title",top  = 10, left  = 100, HTML("<h1>COVID-19 outbreak - Paraguay</h1>"),draggable = T)
)

server <- function(input, output, session) {
  
  
  dataDpto<- reactive({
    if(!is.null(input$choices)){
      if(input$choices == "Cases"){
        return( dataCasesPY)
      }else{
        return( dataDeathsPY)
      }}
  })
  
  maxTotal<- reactive( max(dataDpto()%>%select(-Pop)%>%select_if(is.numeric), na.rm = T)
  )
  maxTotalPrevalence<- reactive( max(dataDpto()%>%select(-Pop)%>%select_if(is.numeric)%>%mutate_all(function(x) x/dataDpto()$Pop*100000), na.rm = T)
  )
  # 
  # 
  Top5<-reactive( unique(dataDpto()$Dpto[order(dataDpto()[,dim(dataDpto())[2]]%>%unlist(),decreasing = T)][1:5])
  )
  # 
  
  #
  #
  output$map <- renderLeaflet({
    
    
    
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
    
    leaflet(data = sf_py) %>%
      
      setView(-59, -23.5, zoom = 7)
    
    
  })
  
  
  pal <- reactive({
    if(!is.null(input$choices)){
      if(input$choices == "Deaths"){
        return( colorNumeric(c("#FFFFFFFF" , rev(inferno(maxTotal()+1, begin = 0, end = 0.6))), domain = c(0,log(arrondi(maxTotal()+1)))) )
      }else{
        return( colorNumeric(c("#FFFFFFFF" , rev(inferno(maxTotal()+1, begin = 0.3, end = 0.9))), domain = c(0,log(arrondi(maxTotal()+1)))) )
      }}
  })
  pal2 <- reactive({
    if(!is.null(input$choices)){
      if(input$choices == "Deaths"){
        return( colorNumeric(c("#FFFFFFFF", rev(inferno(maxTotal()+1, begin = 0, end = 0.6))), domain = c(0,log(arrondi(maxTotalPrevalence()+1)))) )
      }else{
        return( colorNumeric(c("#FFFFFFFF", rev(inferno(maxTotal()+1, begin = 0.3, end = 0.9))), domain = c(0,log(arrondi(maxTotalPrevalence()+1)))) )
      }}
  })
  
  
  observe({
    casesDeath<- ifelse(input$choices == "Cases","Cases","Deaths")
    if (!is.null(input$day1)) {
      indicator<-format.Date(input$day1, "%m/%d/%y")
      
    }else{
      indicator = format.Date(max(jourDate), "%m/%d/%y")
    }
    
    
    if (!is.null(input$day2)) {
      indicator2<-format.Date(input$day2-c(1,0), "%m/%d/%y")
      
    }else{
      indicator2 =format.Date(c(min(jourDate)-1,max(jourDate)), "%m/%d/%y")
    }
    
    if(is.null(input$variable)){
      
    }else{
      variable<- input$variable
      
      if(variable =="Total cases/population"){
        # nCases
        countries2 <- merge(sf_py,
                            dataDpto(),
                            by.x = "Dpto",
                            by.y = "Dpto",
                            sort = FALSE)
        country_popup <- paste0("<strong>Dpto: </strong>",
                                countries2$Dpto,
                                "<br><strong>",
                                "Total cases/population :",
                                
                                
                                " </strong>",
                                round(countries2[[indicator]]/countries2$Pop*100000,2)," /100 000",
                                "<br><strong>Population : </strong>",
                                round(countries2$Pop))
        
        
        leafletProxy("map", data = countries2)%>%
          addPolygons(fillColor = pal2()(log((countries2[[indicator]]/countries2$Pop*100000)+1)),
                      layerId = ~Dpto,
                      fillOpacity = 1,
                      color = "#BDBDC3",
                      weight = 1,
                      popup = country_popup)
        
      }else if(variable =="Total cases"){
        countries2 <- merge(sf_py,
                            dataDpto(),
                            by.x = "Dpto",
                            by.y = "Dpto",
                            sort = FALSE)
        country_popup <- paste0("<strong>Country: </strong>",
                                countries2$Dpto,
                                "<br><strong>",
                                "Total ",casesDeath," :",
                                
                                
                                " </strong>",
                                round(countries2[[indicator]],2),
                                "<br><strong>Population : </strong>",
                                round(countries2$Pop))
        
        
        leafletProxy("map", data = countries2)%>%
          addPolygons(fillColor = pal()(log((countries2[[indicator]])+1)),
                      fillOpacity = 1,
                      layerId = ~Dpto,
                      color = "#BDBDC3",
                      weight = 1,
                      popup = country_popup)
        
        
      }else if(variable =="New cases over period"){
        
        dataDptoSel<-dataDpto()%>%select(Dpto, Pop)
        if(indicator2[1] == format.Date(min(jourDate)-1, "%m/%d/%y")){
          
          dataDptoSel$ncases<-dataDpto()[,indicator2[2]]
        }else{
          dataDptoSel$ncases<-dataDpto()[,indicator2[2]]-dataDpto()[,indicator2[1]]
          
        }
        
        # nCases
        countries2 <- merge(sf_py,
                            dataDptoSel,
                            by.x = "Dpto",
                            by.y = "Dpto",
                            sort = FALSE)
        country_popup <- paste0("<strong>Country: </strong>",
                                countries2$Dpto,
                                "<br><strong>",
                                "New ",casesDeath," over period :",
                                
                                
                                " </strong>",
                                countries2$ncases,
                                "<br><strong>Population : </strong>",
                                round(countries2$Pop))
        
        leafletProxy("map", data = countries2)%>%
          addPolygons(fillColor = pal()(log(countries2$ncases+1)),
                      fillOpacity = 1,
                      color = "#BDBDC3",
                      layerId = ~Dpto,
                      weight = 1,
                      popup = country_popup)
      }else{
        
        dataDptoSel<-dataDpto()%>%select(Dpto, Pop)
        if(indicator2[1] == format.Date(min(jourDate)-1, "%m/%d/%y")){
          
          dataDptoSel$ncases<-dataDpto()[,indicator2[2]]
        }else{
          dataDptoSel$ncases<-dataDpto()[,indicator2[2]]-dataDpto()[,indicator2[1]]
          
        }
        
        # nCases
        countries2 <- merge(sf_py,
                            dataDptoSel,
                            by.x = "Dpto",
                            by.y = "Dpto",
                            sort = FALSE)
        country_popup <- paste0("<strong>Country: </strong>",
                                countries2$Dpto,
                                "<br><strong>",
                                "New ",casesDeath," over period / population :",
                                
                                
                                " </strong>",
                                round(countries2$ncases/countries2$Pop*100000,2)," /100 000",
                                "<br><strong>Population : </strong>",
                                round(countries2$Pop))
        
        leafletProxy("map", data = countries2)%>%
          addPolygons(fillColor = pal2()(log(countries2$ncases/countries2$Pop*100000+1)),
                      fillOpacity = 1,
                      color = "#BDBDC3",
                      layerId = ~Dpto,
                      weight = 1,
                      popup = country_popup)
        
      }
    }
  })
  
  
  
  
  
  observe({
    
    
    if(is.null(input$variable)){
      
    }else{
      variable<- input$variable
      
      proxy <- leafletProxy("map", data = sf_py)
      
      
      # Remove any existing legend, and only if the legend is
      # enabled, create a new one.
      proxy %>% clearControls()
      if (input$legend) {
        if(variable %in% c("Total cases/population","New cases over period/population")){
          if(round(maxTotalPrevalence())>0) {
            proxy %>% addLegend(position = "bottomright",
                                pal = pal2(),opacity = 1,
                                bins = log(10^(seq(0,log10(arrondi(maxTotalPrevalence())),0.5))),
                                value = log(1:10^(log10(arrondi(maxTotalPrevalence())))),
                                data =log(1:10^(log10(arrondi(maxTotalPrevalence())))),
                                labFormat = labelFormat(transform = function(x) round(exp(x)) ,suffix = " /100 000")
            )
          } else {
            proxy %>% addLegend(position = "bottomright",
                                pal = pal2(),opacity = 1,
                                bins = 1, value = c(0,1), data = 0,
                                labFormat = labelFormat(transform = function(x) x, suffix = " /100 000")
            )
          }
          
        }else{
          if(maxTotal()>1) {
            proxy %>% addLegend(position = "bottomright",
                                pal = pal(),opacity = 1,
                                bins = log(10^(0:log10(arrondi(maxTotal())))),
                                value = log(1:10^(log10(arrondi(maxTotal())))),
                                data = log(10^(0:log10(arrondi(maxTotal())))),
                                labFormat = labelFormat(transform =  exp )
            )
          } else {
            proxy %>% addLegend(position = "bottomright",
                                pal = pal(),opacity = 1,
                                bins = 1, value = c(0,1), data = 0,
                                labFormat = labelFormat(transform = function(x) x)
            )
          }
        }
      }
    }
    
  })
  
  output$Slider<-renderUI({
    
    if(is.null(input$variable)){
      
    }else{
      if(input$variable %in% c("Total cases", "Total cases/population")){
        sliderInput("day1", "Day", min(jourDate), max(jourDate),
                    value =  c(max(jourDate)),animate = T, step = 1
        )
      }else{
        sliderInput("day2", "Day", min(jourDate), max(jourDate),
                    value =  c(max(jourDate)-7,max(jourDate)),animate = T, step = 1
        )
      }
    }
  })
  
  output$selection <- renderUI({
    if(input$choices =="Cases"){
      radioButtons("variable", choices =  c("New cases over period",
                                            "New cases over period/population","Total cases", 'Total cases/population' ),
                   label = "Indicator")
    }else{
      radioButtons("variable", choices =  list("Deaths over period"="New cases over period",
                                               "Deaths over period/population"="New cases over period/population",
                                               "Total deaths"="Total cases",
                                               'Total deaths/population'='Total cases/population' ),
                   label = "Indicator")
    }
  })
  
  output$plotEvol<-renderUI({
    if (input$plotEvolT) {
      tagList(absolutePanel(
        id = "name",
        class = "panel panel-credits",
        top = 10,width = "700px",
        right  = 10,draggable = F,
        plotlyOutput(outputId = "evol",width = "600px"),
        actionButton("reset", "Reset Graph"),
        actionButton("clear", "Clear all traces")
      ))
    }
  })
  
  output$evol <-renderPlotly({
    
    if(input$variable %in% c("Total cases/population","Total cases")){
      df_evo<- dataDpto()%>%filter(Dpto%in% trace$data)%>%pivot_longer(cols = -c(Dpto,Pop),
                                                                       values_to = "Cases",names_to = "Date")%>%
        mutate(Date= lubridate::parse_date_time(Date, orders = c("mdy")))
      
      if(input$variable=="Total cases/population"){
        
        plot_ly(data = df_evo,x = ~Date, y = ~Cases/Pop*100000, color = ~Dpto, type = "scatter",mode = "lines")%>%
          layout(yaxis = list( title = paste(input$choices,"/ 100 000")))
        
      }else{
        
        plot_ly(data = df_evo,x = ~Date, y = ~Cases, color = ~Dpto, type = "scatter",mode = "lines")%>%
          layout(yaxis = list( title = input$choices))
        
      }
    }else{
      df_evo<- dataDpto()%>%filter(Dpto%in% trace$data)
      
      
      for(i in dim( df_evo)[2]:4)  df_evo[i]<- df_evo[i]- df_evo[i-1]
      
      
      df_evo<- df_evo%>%pivot_longer(cols = -c(Dpto,Pop),
                                     values_to = "Cases",names_to = "Date")%>%
        mutate(Date= lubridate::parse_date_time(Date, orders = c("mdy")))
      
      
      
      if( input$variable=="New cases over period/population"){
        
        plot_ly(data = df_evo,x = ~Date, y = ~Cases/Pop*100000, color = ~Dpto, type = "scatter",mode = "lines")%>%
          layout(yaxis = list( title = paste(input$choices,"/ 100 000/day")))
        
      }else{
        
        plot_ly(data = df_evo,x = ~Date, y = ~Cases, color = ~Dpto, type = "scatter",mode = "lines")%>%
          layout(yaxis = list( title = paste(input$choices,"/day")))
        
      }
      
    }
    
  })
  
  trace<- reactiveValues()
  observe({trace$data<-Top5()
  })
  
  observeEvent(input$reset, {
    
    for (i in 1: length(trace$data)){
      plotlyProxy("evol", session) %>%
        plotlyProxyInvoke("deleteTraces",list(0))
      
    }
    
    
    if(input$variable %in% c("Total cases/population","Total cases")){
      
      
      df_evo<- dataDpto()%>%filter(Dpto%in% Top5())%>%pivot_longer(cols = -c(Dpto,Pop),
                                                                   values_to = "Cases",names_to = "Date")%>%
        mutate(Date= lubridate::parse_date_time(Date, orders = c("mdy")))
      
      
      if(input$variable=="Total cases/population"){
        
        for (i in Top5()){
          df_evoi<- df_evo%>%filter(Dpto == i)
          plotlyProxy("evol", session) %>%
            plotlyProxyInvoke("addTraces",
                              list(x =df_evoi$Date ,
                                   name =i ,
                                   y = df_evoi$Cases/df_evoi$Pop*100000,
                                   type = 'scatter',
                                   mode = 'lines'))
        }
        
      }else{
        
        for (i in Top5()){
          df_evoi<- df_evo%>%filter(Dpto == i)
          plotlyProxy("evol", session) %>%
            plotlyProxyInvoke("addTraces",
                              list(x =df_evoi$Date ,
                                   name =i ,
                                   y = df_evoi$Cases,
                                   type = 'scatter',
                                   mode = 'lines'))
          
        }
        
      }
      
      
    }else{
      
      
      df_evo<- dataDpto()%>%filter(Dpto%in% Top5())

      for(i in  dim(df_evo)[2]:4) df_evo[i]<-df_evo[i]-df_evo[i-1]
      
      
      df_evo<-df_evo%>%pivot_longer(cols = -c(Dpto,Pop),
                                    values_to = "Cases",names_to = "Date")%>%
        mutate(Date= lubridate::parse_date_time(Date, orders = c("mdy")))
      
      if( input$variable=="New cases over period/population"){
        
        for (i in Top5()){
          df_evoi<- df_evo%>%filter(Dpto == i)
          plotlyProxy("evol", session) %>%
            plotlyProxyInvoke("addTraces",
                              list(x =df_evoi$Date ,
                                   name =i ,
                                   y = df_evoi$Cases/df_evoi$Pop*100000,
                                   type = 'scatter',
                                   mode = 'lines'))
          
        }
        
      }else{
        for (i in Top5()){
          df_evoi<- df_evo%>%filter(Dpto == i)
          plotlyProxy("evol", session) %>%
            plotlyProxyInvoke("addTraces",
                              list(x =df_evoi$Date ,
                                   name =i ,
                                   y = df_evoi$Cases,
                                   type = 'scatter',
                                   mode = 'lines'))
          
        }
        
      }
      
    }
    
    trace$data<-Top5()
  })
  
  
  observeEvent(input$clear, {
    for (i in 1: length(trace$data)){
      plotlyProxy("evol", session) %>%
        plotlyProxyInvoke("deleteTraces",list(0))
    }
    trace$data<- NULL
  })
  observeEvent(input$map_shape_click, {
    
    
    country_Click<- input$map_shape_click$id
    if (!country_Click%in%trace$data & input$plotEvolT){
      
      trace$data<-c(trace$data,country_Click)
      
      if(input$variable %in% c("Total cases/population","Total cases")){
        df_click<- dataDpto()%>%filter(Dpto%in% country_Click)%>%pivot_longer(cols = -c(Dpto,Pop),
                                                                              values_to = "Cases",names_to = "Date")%>%
          mutate(Date= lubridate::parse_date_time(Date, orders = c("mdy")))
        
        if(input$variable=="Total cases/population"){
          plotlyProxy("evol", session) %>%
            plotlyProxyInvoke("addTraces",
                              list(x =df_click$Date ,
                                   name =country_Click ,
                                   y = df_click$Cases/df_click$Pop*100000,
                                   type = 'scatter',
                                   mode = 'lines'))
          
        }else{
          plotlyProxy("evol", session) %>%
            plotlyProxyInvoke("addTraces",
                              list(x =df_click$Date ,
                                   name =country_Click ,
                                   y = df_click$Cases,
                                   type = 'scatter',
                                   mode = 'lines'))
          
        }
      }else{
        
        df_click<- dataDpto()%>%filter(Dpto%in% country_Click)
        
        
        

        for(i in  dim( df_click)[2]:4)  df_click[i]<- df_click[i]- df_click[i-1]
        
        
        df_click<- df_click%>%pivot_longer(cols = -c(Dpto,Pop),
                                           values_to = "Cases",names_to = "Date")%>%
          mutate(Date= lubridate::parse_date_time(Date, orders = c("mdy")))
        
        
        
        if( input$variable=="New cases over period/population"){
          plotlyProxy("evol", session) %>%
            plotlyProxyInvoke("addTraces",
                              list(x =df_click$Date ,
                                   name =country_Click ,
                                   y = df_click$Cases/df_click$Pop*100000,
                                   type = 'scatter',
                                   mode = 'lines'))
          
        }else{
          plotlyProxy("evol", session) %>%
            plotlyProxyInvoke("addTraces",
                              list(x =df_click$Date ,
                                   name =country_Click ,
                                   y = df_click$Cases,
                                   type = 'scatter',
                                   mode = 'lines'))  
        }
        
      }
      
      
      
      }
  })
  output$Credits <- renderUI({
    if (input$credits) {
      tagList(
        absolutePanel(
          id = "name",
          class = "panel panel-credits",
          top = "45%",
          left  = "45%",
          HTML(
            "<h1> Data Source : </h1>
  <p> <li><a href='https://www.mspbs.gov.py/covid-19.php'>Ministerio de Salud Publica y Bienestar Social - Paraguay</a></li>
  <li>COVID-19 Cases : <a href='https://www.mspbs.gov.py/reportes-covid19.html' target='_blank'>REPORTE PARAGUAY MSPBS COVID19</a></li>
  <li>Paraguay population : <a href='https://www.dgeec.gov.py/microdatos/cuadro/b7dc2DEP01-Paraguay-Poblacion-total-por-anio-calendario-segun-sexo-y-departamento-2000-2025.csv' target='_blank'>Paraguay Population - DGEEC</a></li>
  <li>Paraguay GeoJSON : <a href='http://geo.stp.gov.py/user/dgeec/api/v2/sql?q=SELECT%20*%20FROM%20dgeec.paraguay_2002_departamentos&format=GeoJSON' target='_blank'>Paraguay Departments - DGEEC</a></li>
  <li>Paraguay Shapefile : <a href='http://geo.stp.gov.py/user/dgeec/tables/paraguay_2002_departamentos/public' target='_blank'>Paraguay Departments - DGEEC</a></li>
  <li> <a href ='https://github.com/danielbonhaure/Corona' target='_blank'>Code on Github (Paraguay version)</a></li>
  <li> <a href ='https://github.com/DrFabach/Corona' target='_blank'>Code on Github (Original version)</a></li>
  <li> <a href = 'https://www.r-project.org/'  target='_blank'>The R Project for Statistical Computing</a></li>
  <li> <a href = 'https://shiny.rstudio.com/' target='_blank'>Shiny R package</a></li>
  <li> <a href = 'https://leafletjs.com/' target='_blank'>Leaflet </a></li> </p>"
          ),
          draggable = T
        )
      )
    }
    
  })
  
}

shinyApp(ui, server)
