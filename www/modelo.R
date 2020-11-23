#regresiones, clasificaciones, reduccion de dimensiones, Kmeans, arboles de clasificacion

modelo<-{
  navlistPanel(  
    tabPanel("Ayuda vs Estimacion de Daño",hr(),checkboxInput("control6", "Mostrar Codigo", FALSE),
             verbatimTextOutput("consulta6"),hr(), plotOutput("plot8"))  ) 
}

