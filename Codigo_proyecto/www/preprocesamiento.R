#imputacion, anomalos, outliers
preprocesamiento<-{sidebarLayout(
  sidebarPanel(
    
    actionButton("control1", "Mostrar Codigo"),
    hr(),
    actionButton("controlDon", "Guardar Donaciones"),
    hr(),
    actionButton("controlGes", "Guardar Gestantes"),
    hr(),
    verbatimTextOutput("consulta1")
  ),
  mainPanel(
    radioButtons("selectPre",label = h3("Escoger Dataset limpio"),choices = c("Donaciones"="1","Gestantes"="2")),
    hr(),
    tableOutput("tablaS5"))
  
  )
  
}
