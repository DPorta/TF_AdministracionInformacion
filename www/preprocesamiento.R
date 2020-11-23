#imputacion, anomalos, outliers
preprocesamiento<-{navlistPanel(
  tabPanel("Limpieza e Imputacion",
           h4("Unir Dataset's Separados"),hr(),checkboxInput("control1", "Mostrar Codigo", FALSE),hr(),
           verbatimTextOutput("consulta1"), tableOutput("tablaS5")))
}