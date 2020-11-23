graficos<-{
  navlistPanel( 
    tabPanel("Graficos GGPLOT2",h4("GGPLOT2"),hr(),
             selectInput("selectgg", label = NULL, 
                         choices = c('consulta 1'='1','consulta 2'='2','consulta 3'='3','consulta 4'='4','consulta 5'='5')),
             hr(),
             verbatimTextOutput("consulta4"),
             plotOutput('plot1'))
    ,tabPanel("Graficos PLOTY",h4("PLOTY"),hr(),
              selectInput("selectpl", label = NULL, 
                          choices = c('consulta 1'='1','consulta 2'='2','consulta 3'='3','consulta 4'='4','consulta 5'='5')),
              checkboxInput("control5", "Mostrar Codigo", FALSE),
              hr(),
              verbatimTextOutput("consulta5"),
              plotlyOutput("plot2"))
  )
  
}