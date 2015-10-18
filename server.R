shinyServer(function(input, output, session){
    
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
)