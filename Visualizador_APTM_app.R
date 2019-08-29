# Cargando librerias ----
library(shiny)
library(leaflet)
library(scales)
library(sf)
library(openxlsx)
library(reshape2)
library(ggplot2)
library(dplyr)
library(tidyr)
library(plotly)
library(data.table)
# Cargando datos ----
demo <- read.xlsx("GPS - COLUMNAS ESTIMATIVAS A MOSTRAR EN UBICACIÓN GEOGRÁFICA.xlsx", rows = 3:13, colNames = T)

dpto <- st_read('./datos/Deptos.gpkg', 'Deptos', stringsAsFactors = FALSE)

# Explorando datos ----
# str(demo)
# head(demo)
demo$MS1 <- as.numeric(paste(demo$M1, demo$S1, sep = ""))
demo$MS2 <- as.numeric(paste(demo$M2, demo$S2, sep = ""))
demo$lat <- as.numeric(paste(demo$G1, demo$MS1, sep = ".")) # creando variable latitud
demo$lon <- as.numeric(paste(demo$G2, demo$MS2, sep = ".")) # creando variable longitud
demo1 <- gather(demo, key = "asociacion", value = "valor", c("APTM","CAMARA","ACTIM")) # creando la variable asociación
demo1 <- demo1 %>% filter(valor == 1) # seleccionando solo valores unicos
demo1$lat <- demo1$lat*-1 # conviertiendo latitud en negativo
demo1$lon <- demo1$lon*-1 # conviertiendo longitud en negativo
demo1$MUNICIPIO <- gsub("                     ", "", demo1$MUNICIPIO)
demo1$MUNICIPIO <- gsub("                ", "", demo1$MUNICIPIO)
demo1$MUNICIPIO <- gsub("   ", "", demo1$MUNICIPIO)

asociados <- st_as_sf(demo1, coords = c("lon", "lat"), crs = 4326) # converción a un sf object

# # Maxi ----
# ## Cambiar nombre de las columnas de los Meses
# names(Demo)[10:21]<- c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic")
# 
# ## Tablero de Ventas por Mes según Cliente
# tablero <- Demo %>% select(Cliente, Ene, Feb, Mar, Abr, May, Jun, Jul, Ago, Sep, Oct, Nov, Dic)
# 
# ## Tabla ventas según Mes
# tabla_clientes <- Demo %>% gather(Mes, Ventas, 10:21) %>% select(Cliente, Mes, Ventas)
# 
# ## Cambiar a factor y ordenar los datos
# tabla_clientes$Mes <- factor(tabla_clientes$Mes, levels = c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic") )
# 
# tabla_clientes[is.na(tabla_clientes)] <- 0

# UI ----
ui <- fluidPage(
titlePanel("Visualizador APTM"),
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "Asociados",
                  label = "Elegir asociación:",
                  choices = c("Todos", unique(asociados$asociacion)),
                  selected = "Todos",
                  multiple = FALSE),
      selectInput(inputId = "Municipios",
                  label = "Elegir municipio:",
                  choices = c("Todos", unique(asociados$MUNICIPIO)),
                  selected = "Todos",
                  multiple = FALSE)
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("General", 
          leafletOutput("map", height = 500),
          plotlyOutput(outputId = "plot", height = 300),
          plotlyOutput(outputId = "plot2", height = 300)
          ),
        tabPanel("tab 2", 
                 leafletOutput("mapT2", height = 500),
                 plotlyOutput(outputId = "plotT2", height = 300),
                 dataTableOutput(outputId = 'TablaAsociados')),
        tabPanel("tab 3", "contents3")
      )
)
))


# server ----
server <- function(input, output){
  getColor2 <- function(quakes) {
    sapply(quakes$asociacion, function(mag) {
      if(mag == 'APTM') {
        "green"
      } else if(mag == "CAMARA") {
        "orange"
      } else {
        "red"
      } })
  }  
  getColor <- function(datos) {
    sapply(datos$asociacion, function(pos) {
      #if(pos == 'APTM') {"green"} 
      #ifelse( if ( pos == 'APTM'){ 'green')} else(pos == 'CAMARA', "yellow", "red")
      ifelse(pos == "APTM","green", ifelse ( pos == "CAMARA", "yeallow", "red"))
      } )
  }
  
  output$map <- renderLeaflet(
    {
      if(input$Asociados != "Todos"){
        asociados <- asociados[which(asociados$asociacion == input$Asociados),]
      }
      if(input$Municipios != "Todos"){
        print(input$Municipios)
        asociados <- asociados[which(asociados$MUNICIPIO == input$Municipios),]
      }
      icons <- awesomeIcons(
        icon = 'ios-close',
        iconColor = 'black',
        library = 'ion',
        markerColor = getColor2(asociados)
      )
      m <- NULL
      m <- leaflet() %>%
        addTiles() %>%
        addProviderTiles("OpenStreetMap.Mapnik", group = "OpenStreetMap") %>%
        addProviderTiles("Esri.WorldImagery", group = "ESRI Aerial") %>%
        #setView(asociados$lon, dir$lat, zoom = 16) %>% 
        #addCircleMarkers(data=asociados, group="asociacion",radius = 10, opacity=1, color = "black",stroke=TRUE, fillOpacity = 0.75, weight=2, fillColor = "red", clusterOptions = NULL, options = markerClusterOptions(showCoverageOnHover = TRUE, zoomToBoundsOnClick = TRUE, spiderfyOnMaxZoom = FALSE, removeOutsideVisibleBounds = TRUE, spiderLegPolylineOptions = list(weight = 1.5, color = "#222", opacity = 0.5), freezeAtZoom = FALSE), popup = paste0("ID: ", demo1$ID, "<br> NOMBRE: ", demo1$NOMBRE, "<br> COLONIA: ", demo1$COLONIA)) 
        addAwesomeMarkers(data=asociados, icon = icons, popup = paste0("ID: ", demo1$ID, "<br> NOMBRE: ", demo1$NOMBRE, "<br> COLONIA: ", demo1$COLONIA)) %>%
        addLayersControl(
          baseGroups = c("OpenStreetMap", "ESRI Aerial"),
          options = layersControlOptions(collapsed = T)) %>% 
        addPolygons(data = dpto, color = "#444444", weight = 1, smoothFactor = 0.5, opacity = 0.5, fillOpacity = 0.5, fillColor = "lightgrey", options = markerOptions(interactive = FALSE))
      
      m
    }
  )
  
  # Grafico asociados municipio----
  output$plot <- renderPlotly(
    {
      if(input$Asociados != "Todos"){
        #demo1 <- demo1[which(demo1$asociacion == input$Asociados),]
        asociados <- asociados[which(demo1$asociacion == input$Asociados),]
          #subset(demo1, asosiacion == input$Asociados)
      }
      if(input$Municipios != "Todos"){
        asociados <- asociados[which(asociados$MUNICIPIO == input$Municipios),]
      }
      grafico_barra <- asociados %>% ggplot(aes(x = asociacion, fill = MUNICIPIO)) +
        geom_bar(show.legend = F, position = 'stack', width = 0.7) +
        #scale_fill_brewer(palette = "Pastel1") +
        labs(title = "Dsitribución de asociados por Asociación",y = "Cantidad de asociados", x = "") +
        xlim("APTM", "ACTIM", "CAMARA") +
        theme_classic()
      
        #scale_y_continuous(breaks = c(0, 300000,600000), labels = dollar)
      
     
      ggplotly(grafico_barra) 
    }
  )
  
  # Grafico produccion municipio----
  output$plot2 <- renderPlotly(
    {
      if(input$Asociados != "Todos"){
        #demo1 <- demo1[which(demo1$asociacion == input$Asociados),]
        asociados <- asociados[which(demo1$asociacion == input$Asociados),]
        #subset(demo1, asosiacion == input$Asociados)
      }
      if(input$Municipios != "Todos"){
        asociados <- asociados[which(asociados$MUNICIPIO == input$Municipios),]
      }
      grafico_barra2 <- asociados %>% ggplot(aes(x = asociacion, y = KILOS, fill = MUNICIPIO)) +
        geom_bar(show.legend = F, stat = 'identity', width = 0.7) +
        #scale_fill_brewer(palette = "Pastel1") +
        labs(title = "Dsitribución de producción por Asociación",y = "Cantidad de asociados", x = "") +
        xlim("APTM", "ACTIM", "CAMARA") +
        theme_classic()
      
      #scale_y_continuous(breaks = c(0, 300000,600000), labels = dollar)
      
      
      ggplotly(grafico_barra2) 
    }
  )
  # mapa tab panel 2 ----
  output$mapT2 <- renderLeaflet(
    {
      if(input$Asociados != "Todos"){
        asociados <- asociados[which(asociados$asociacion == input$Asociados),]
      }
      if(input$Municipios != "Todos"){
        print(input$Municipios)
        asociados <- asociados[which(asociados$MUNICIPIO == input$Municipios),]
      }
      m <- NULL
      m <- leaflet() %>%
        addTiles() %>%
        addProviderTiles("OpenStreetMap.Mapnik", group = "OpenStreetMap") %>%
        addProviderTiles("Esri.WorldImagery", group = "ESRI Aerial") %>%
        #setView(asociados$lon, dir$lat, zoom = 16) %>% 
        addCircleMarkers(data=asociados, 
                         #group="asociacion", 
                         radius = 10, opacity=1, color = "black",stroke=TRUE, fillOpacity = 0.75, weight=2,
                         fillColor = "red", clusterOptions = NULL, 
                         options = markerClusterOptions(showCoverageOnHover = TRUE, zoomToBoundsOnClick = TRUE, spiderfyOnMaxZoom = FALSE, removeOutsideVisibleBounds = TRUE, spiderLegPolylineOptions = list(weight = 1.5, color = "#222", opacity = 0.5), freezeAtZoom = FALSE),
                         popup = paste0("ID: ", demo1$ID, "<br> NOMBRE: ", demo1$NOMBRE, "<br> COLONIA: ", demo1$COLONIA)) %>%
        addLayersControl(
          baseGroups = c("OpenStreetMap", "ESRI Aerial"),
          #overlayGroups = c("Hot SPrings"),
          options = layersControlOptions(collapsed = T)) %>% 
        addPolygons(data = dpto, color = "#444444", weight = 1, smoothFactor = 0.5, opacity = 0.5, fillOpacity = 0.5, fillColor = "lightgrey", options = markerOptions(interactive = FALSE))
      
      m
    }
  )
  # tabela tab panel 2 ----
  # Table asociados ----
  output$TablaAsociados <- renderDataTable(
    {
      if(input$Asociados != "Todos"){
        #demo1 <- demo1[which(demo1$asociacion == input$Asociados),]
        asociados <- asociados[which(demo1$asociacion == input$Asociados),]
        #subset(demo1, asosiacion == input$Asociados)
      }
      if(input$Municipios != "Todos"){
        asociados <- asociados[which(asociados$MUNICIPIO == input$Municipios),]
      }
      st_drop_geometry(asociados[,c(1:4)])}
  )
  # Grafico tab panel 2----
  output$plotT2 <- renderPlotly(
    {
      if(input$Asociados != "Todos"){
        #demo1 <- demo1[which(demo1$asociacion == input$Asociados),]
        asociados <- asociados[which(demo1$asociacion == input$Asociados),]
        #subset(demo1, asosiacion == input$Asociados)
      }
      if(input$Municipios != "Todos"){
        asociados <- asociados[which(asociados$MUNICIPIO == input$Municipios),]
      }
      grafico_barra <- asociados %>% ggplot(aes(x = asociacion)) +
        geom_bar(show.legend = F, fill = "lightblue", width = 0.7) +
        #scale_fill_brewer(palette = "Pastel1") +
        labs(title = "Dsitribución de asociados por Asociación",y = "Cantidad de asociados", x = "") +
        xlim("APTM", "ACTIM", "CAMARA") +
        theme_classic()
      
      #scale_y_continuous(breaks = c(0, 300000,600000), labels = dollar)
      
      
      ggplotly(grafico_barra) 
    }
  )
}

# app ----
shinyApp(ui = ui, server = server)
