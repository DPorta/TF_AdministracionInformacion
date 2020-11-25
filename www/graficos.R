graficos<-{
  navlistPanel( 
    tabPanel("Graficos GGPLOT2",
             h4("GGPLOT2"),hr(),
             selectInput("selectgg", label = NULL, 
                                           choices = c('grafico 1'='1','grafico 2'='2','grafico 3'='3','grafico 4'='4','grafico 5'='5',
                                                       'grafico 6'='6','grafico 7'='7','grafico 8'='8','grafica 9'='9','grafica 10'='10',
                                                       'grafico 11'='11','grafico 12'='12','grafico 13'='13','grafico 14'='14','grafico 15'='15',
                                                       'grafico 16'='16','grafico 17'='17','grafico 18'='18','grafico 19'='19','grafico 20'='20')),
             hr(),
             plotOutput('plot1')
             ),
    hr(),
    tabPanel("Codigo de Graficos",
             verbatimTextOutput("consulta3")
             )
              
  )}