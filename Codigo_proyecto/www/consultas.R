consultas<-{
  navlistPanel(
    tabPanel("Consultas DPLYR",
             h4("DPLYR"), hr(),
             selectInput("selectdp", label = NULL, 
                                           choices = c('consulta 1'='1','consulta 2'='2','consulta 3'='3','consulta 4'='4','consulta 5'='5',
                                                       'consulta 6'='6','consulta 7'='7','consulta 8'='8','consulta 9'='9','consulta 10'='10',
                                                       'consulta 11'='11','consulta 12'='12','consulta 13'='13','consulta 14'='14','consulta 15'='15',
                                                       'consulta 16'='16','consulta 17'='17','consulta 18'='18','consulta 19'='19','consulta 20'='20')),
             hr(),
             dataTableOutput("tablaS3")
             ),
    hr(),
    tabPanel("Codigo de consultas exploratorias",
             verbatimTextOutput("consulta2")
             )
    )}
