# Aplicativo do Mapa das OSCs
# Autor: Andre Vieira

# Carrega pacotes usados

library(shiny)
library(leaflet)
library(foreign)
library(maptools)
library(RColorBrewer)
library(rgdal)
library(ggplot2)
library(scales)

# Carrega e limpa bases de dados e shape do mapa

setwd("C:/Users/André/Documents/Projetos/2015/Ipea/Projetos/Mapa/Bases/RAIS/Dados")
r2013 <- read.dta("estab2013_esfl_limpo.dta")
r2013 <- subset(r2013, r2013$rais_in_osc==1 & (r2013$dup_id_estab==0 | r2013$dup_id_estab==1))
r2013 <- subset(r2013, r2013$uf == "SP")
r2013 <- r2013[c(1, 12, 17)] # Codigo do municipio, natureza juridica e subclasse da CNAE 2.0.

setwd("C:/Users/André/Documents/Projetos/2015/Ipea/Projetos/Mapa/Site/Graficos/SP/35mu2500gsd")
SP <- readShapePoly("35mu2500gsd.shp")

freq_osc <- tapply(r2013$nat_jur2009, r2013$codemun, length)
r2013freq <- data.frame(freqOSC = freq_osc,
                        freqOSCcateg = NA)

rm(freq_osc)

r2013freq$freqOSCcateg <- cut(r2013freq$freqOSC, 
                             breaks = c(0, 10, 20, 50, 100, Inf),
                             labels = c("1-10", "11-20", "21-50", "51-100", "101 e mais"))

r2013freq$codemun <- row.names(r2013freq)
row.names(r2013freq) <- NULL
r2013freq <- rbind(r2013freq, c(NA, NA, NA))
r2013freq <- rbind(r2013freq, c(NA, NA, NA))

latlongSP <- as.data.frame(coordinates(SP))
names(latlongSP)[1] <- "long"; names(latlongSP)[2] <- "lat"
latlongSP$GEOCODIG_M <- SP$GEOCODIG_M
latlongSP$GEOCODIG_M <- substr(latlongSP$GEOCODIG_M, 1, 6)

mapaSP <- attr(SP, 'data')
mapaSP$Index <- row.names(mapaSP)
mapaSP$GEOCODIG_M <- substr(mapaSP$GEOCODIG_M, 1, 6)
mapaSP <- merge(mapaSP, r2013freq, 
                by.x = "GEOCODIG_M",
                by.y = "codemun")
mapaSP <- merge(mapaSP, latlongSP, by = "GEOCODIG_M")

pal <- brewer.pal(5, "YlOrRd")

rm(latlongSP, r2013freq)

# Cria factor de natureza juridica

r2013$nat_jur2009_factor <- factor(r2013$nat_jur2009,
                           levels = c("3999", "3220", "3069", "3204", "3212", "3239"),
                           labels = c("Associação privada",
                                      "Organização religiosa",
                                      "Fundação privada",
                                      "Fundação ou associação estrangeira",
                                      "Fundação ou associação domiciliada no exterior", 
                                      "Comunidade indígena"))

mapaSPMun <- mapaSP[, c(1, 4)]
r2013Mun <- merge(r2013, mapaSPMun, by.x = "codemun", by.y = "GEOCODIG_M")

################################################################################

# Aplicativo Shiny

# ui.R

# ui <- fluidPage(titlePanel("São Paulo"),
#                leafletOutput("map"))

ui <- pageWithSidebar(
    headerPanel("São Paulo"),
    sidebarPanel(
        selectInput("listaMun", 
                    label = "Escolha o municipio", 
                    choices = c("", levels(mapaSP$Nome_Munic)), selected = ""),
        sliderInput('numOsc', 
                    label = 'Filtre pelo número de OSCs', 
                    value = c(1, 20000), min = 1, max = 20000),
        plotOutput('plot', height = 200)),
    mainPanel(leafletOutput("map"))
    )

# server.R

server <- function(input, output, session){
    
    # Cria o mapa
    output$map <- renderLeaflet({
        leaflet(SP) %>%
            addPolygons(stroke = F, fillOpacity = 0.9, smoothFactor = 0.3,
                        color = colorQuantile("YlOrRd", mapaSP$freqOSC)(mapaSP$freqOSC)) %>% 
            addLegend(position = "bottomright", colors = pal, labels = levels(mapaSP$freqOSCcateg),
                  title = "OSCs", opacity = 1)
        
    })
    
    observeEvent(input$listaMun, {
        mapaSPsubset <- subset(mapaSP, Nome_Munic == input$listaMun)
        leafletProxy(mapId = "map") %>% 
            addMarkers(lng = mapaSPsubset$long, 
                       lat = mapaSPsubset$lat,
                       layerId = "Selected")
    })
    
    mapaSPsubsetMun <- reactive({
        if(is.null(input$listaMun)) NULL
        else subset(r2013Mun, Nome_Munic == input$listaMun)
    })
    
    output$plot <- renderPlot({
        if(nrow(mapaSPsubsetMun()) == 0)
            return(NULL)
    else g1 <- ggplot(mapaSPsubsetMun(), aes(x = nat_jur2009_factor))
        g1 <- g1 + geom_bar(aes(y = (..count..)/sum(..count..)), fill = "#003399")
        g1 <- g1 + scale_y_continuous(labels = percent_format())
        g1 <- g1 + labs(x = "", y = "") + theme_bw()
        g1 <- g1 + theme(panel.grid.major = element_line(size = .5, color = "grey"), 
                         axis.line = element_line(size=.7, color = "black"))
        g1 <- g1 + scale_x_discrete(labels = c("Ass. \n privada",
                                               "Org. \n religiosa",
                                               "Fund. \n privada",
                                               "Fund. \n estrangeira",
                                               "Fund. \n exterior", 
                                               "Comun. \n indígena")) +
            theme(axis.text.x = element_text(size = 8))
        
        g1 <- g1 + theme(panel.grid.major.x = element_blank(),
                         panel.grid.major.y = element_line(size=.1, color="grey"))
        g1 <- g1 + ggtitle("Distribuição de OSCs \n no município") +
            theme(plot.title = element_text(face = "bold", size = 12))
        print(g1)
    })
    
}

shinyApp(ui, server)

##################

# Aproximacao do marker

# setView(lng = mapaSPsubset$long, 
#        lat = mapaSPsubset$lat, 
#        zoom = 18) %>%

# Desenvolvendo funcionalidades do clique

# observeEvent(input$map_marker_click, {
#    p <- input$map_marker_click
#    if(!is.null(p$id)){
#        if(is.null(input$listaMun)) updateSelectInput(session, "listaMun", selected=p$id)
#        if(!is.null(input$listaMun) && input$listaMun!=p$id) updateSelectInput(session, "listaMun", selected=p$id)
#    }
#})

# Referencias

# http://rstudio.github.io/shiny/tutorial
# https://rstudio.github.io/leaflet
# http://shiny.rstudio.com/gallery/superzip-example.html
# http://shiny.snap.uaf.edu/cc4liteFinal/
# http://shiny.rstudio.com/gallery/bus-dashboard.html
# http://stackoverflow.com/questions/28393310/how-to-prevent-leaflet-map-from-resetting-zoom-in-shiny-app
# http://stackoverflow.com/questions/26104933/how-can-i-read-marker-data-in-shiny-with-leaflet