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

# Cargando datos ----
demo <- read.xlsx("GPS - COLUMNAS ESTIMATIVAS A MOSTRAR EN UBICACIÓN GEOGRÁFICA.xlsx", rows = 3:13, colNames = T)

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
asociados <- st_as_sf(demo1, coords = c("lon", "lat"), crs = 4326) # converción a un sf object

# data <- melt(Demo, id.vars="Cliente", measure.vars=paste0(rep("Mes.", 12), seq(1:12)), value.name="Mes")
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
                  multiple = FALSE)
    ),
    mainPanel(
      leafletOutput("map", height = 300),
      plotlyOutput(outputId = "plot", height = 300)
      #plotlyOutput(outputId = "linePlot")
    )
  )
)


# server ----
server <- function(input, output){
  
  output$map <- renderLeaflet(
    {
      if(input$Asociados == "Todos"){
      }else{
        asociados <- asociados[which(asociados$asociacion == input$Asociados),]
      }
      
      m <- leaflet() %>%
        addTiles() %>%
        addProviderTiles("OpenStreetMap.Mapnik", group = "OpenStreetMap") %>%
        addProviderTiles("Esri.WorldImagery", group = "ESRI Aerial") %>%
        addCircleMarkers(data=asociados, group="asociacion", radius = 10, opacity=1, color = "black",stroke=TRUE, fillOpacity = 0.75, weight=2,
                         fillColor = "red", clusterOptions = TRUE, popup = paste0("Longitud: ", demo1$lon, "<br> Latitud: ", demo1$lat, "<br> Municipio: ", demo1$MUNICIPIO)) %>%
        addLayersControl(
          baseGroups = c("OpenStreetMap", "ESRI Aerial"),
          #overlayGroups = c("Hot SPrings"),
          options = layersControlOptions(collapsed = T))
      
      m
    }
  )
  # Grafico 1----
  output$plot <- renderPlotly(
    {
      if(input$Asociados == "Todos"){
      }else{
        demo1 <- demo1[which(demo1$asociacion == input$Asociados),]
          #subset(demo1, asosiacion == input$Asociados)
      }
      grafico_barra <- demo1 %>% ggplot(aes(x = asociacion)) +
        geom_bar(show.legend = F, fill = "lightblue", width = 0.7) +
        #scale_fill_brewer(palette = "Pastel1") +
        labs(title = "Dsitribución de asociados por Asociación",y = "Cantidad de asociados", x = "") +
        xlim("APTM", "ACTIM", "CAMARA") +
        theme_classic()
      
        #scale_y_continuous(breaks = c(0, 300000,600000), labels = dollar)
      
     
      ggplotly(grafico_barra) 
    }
  )
  
  # # Grafico 2 ----
  # output$linePlot <- renderPlotly(
  #   {
  #     if(input$clientes == "Todos"){
  #     }else{
  #       tabla_clientes <- subset(tabla_clientes, Cliente == input$clientes)
  #     }
  #     grafico_linha <- tabla_clientes %>% ggplot(aes(Mes, Ventas, group = Cliente, colour = Cliente)) +
  #       geom_line(stat = "identity", show.legend = F) #+
  #     #scale_y_continuous() +
  #     #scale_fill_brewer(palette = 1) +
  #     #scale_y_continuous(breaks = c(0, 300000,600000))
  #     
  #     grafico_linha + theme_minimal()
  #     ggplotly(grafico_linha) 
  #   }
  # )
}

# app ----
shinyApp(ui = ui, server = server)
