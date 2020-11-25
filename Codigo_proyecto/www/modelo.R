#regresiones, clasificaciones, reduccion de dimensiones, Kmeans, arboles de clasificacion

modelo<-{
  navlistPanel( 
    tabPanel("Graficos GGPLOT2",
             h4("GGPLOT2"),hr(),
             selectInput("selectMo", label = NULL, 
                         choices = c('Regresion'='1','KNN'='2','SVM'='3','Algo'='4')),
             hr(),
             plotOutput('plot8'),
             textOutput('modeloTextG'),
             textOutput('modeloText'),
             textOutput('modeloText2'),
             hr(),
             dataTableOutput('plot9')
    ),
    hr(),
    tabPanel("Codigo de Modelado",
             verbatimTextOutput("consulta6")
    )
    
  )}