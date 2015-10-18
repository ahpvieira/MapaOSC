shinyUI(pageWithSidebar(
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
))