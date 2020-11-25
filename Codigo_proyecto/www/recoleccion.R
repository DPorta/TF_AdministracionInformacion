recoleccion<-{sidebarLayout (
  sidebarPanel (
    selectInput("select", label = h3("Tipo de archivo: "), 
                choices = c('text'='1','csv'='2','xlsx'='3')),
    hr(),
    fileInput("file1", "Seleccione Archivo",accept = c("text/csv","text/json",
                                                       "text/comma-separated-values,text/plain",".csv","XLSX file", 
                                                       ".json",".xlsx",".xls",".xml")),
    helpText ( " Max. Tamano de archivo: 30MB " )),
  mainPanel(
    tabsetPanel(  tabPanel("Info",tableOutput("tablaS6")),
                  tabPanel("Data",tableOutput("tablaS2"))
    )))

}