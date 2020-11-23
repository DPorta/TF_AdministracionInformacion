consultas<-{
  navlistPanel(
    tabPanel("Consultas DPLYR",
             h4("DPLYR"), hr(),
             selectInput("selectdp", label = NULL, 
                                           choices = c('consulta 1'='1','consulta 2'='2','consulta 3'='3','consulta 4'='4','consulta 5'='5')),
             checkboxInput("control2", "Mostrar Codigo", FALSE),hr(),
             verbatimTextOutput("consulta2"),dataTableOutput("tablaS3"))
    , tabPanel("Consultas con SQLDF", h4("SQLDF"),hr(),
               selectInput("selectsq", label = NULL, 
                           choices = c('consulta 1'='1','consulta 2'='2','consulta 3'='3','consulta 4'='4','consulta 5'='5')),
               checkboxInput("control3", "Mostrar Codigo", FALSE),hr(),
               verbatimTextOutput("consulta3"),dataTableOutput("tablaS4")))
 
    
}
