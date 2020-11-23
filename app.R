library("ggplot2")
library("shinyjs")#para la funcion toggle
library("shiny")
library("readxl")#para lectura de excel
library("dplyr")
library("lubridate")#para fechas
library("sqldf")
library("plotly")#para graficos dinamicos
library("shinythemes")


source("www/introduccion.R")
source("www/recoleccion.R")
source("www/preprocesamiento.R")
source("www/consultas.R")
source("www/graficos.R")
source("www/modelo.R")

ui <- fluidPage(
  theme = shinytheme("cyborg"),
  titlePanel( "Trabajo Final de Admin de la Informacion"),
  tabsetPanel(
              tabPanel("Presentacion",introduccion),
              tabPanel("Recoleccion",recoleccion),
              tabPanel("Preprocesamiento",preprocesamiento),
              tabPanel('Consultas Exploracion',consultas),
              tabPanel('Graficos',graficos),
              tabPanel('Modelo',modelo))
  
)




options(shiny.maxRequestSize=30*1024^2) 
server <- function(input, output) {
  getwd()
  dtFinal<- read.csv('Datasets/pcm_donaciones.csv',sep="|")
  
  output$tablaS2 <- renderTable({
    tablaS1 <- input$file1
    if (is.null(tablaS1))
    {return(NULL)}
    box<- input$select
    if(box == "1")    {
      read.csv(tablaS1$datapath)
    } else{  if(box == "2") {
      read.csv(tablaS1$datapath)
    } else { if(box == "3") {
      read_xlsx(tablaS1$datapath)
    } 
    } }
  })
  
  output$consulta1 <- renderText({ '
    #LECTURA DE DATOS
    colocar el codigo por ejemplo

    dt2018<-read.csv("WFSServer-2018.csv")
---
    
    #CORRECION DE COLUMNAS
    
    dt2016$IND_TERMIN<-NA
...
    dt2014$DES_DANO<-NULL
    
    #RENOMBRAR COLUMNAS

    names(dt2016)[24]="NUM_POSX"
.....
    
    #ORDEN DE COLUMNAS
    
    dt2017<- dt2017[,c(names(dt2018))]
...
    
    names(dt2018)
..
    
    #MODIFICACION
    
    dt2016$IND_TERMIN[is.na(dt2016$FEC_TERMIN)== "TRUE"]= 0
...
    
    
    dt2016$NUM_ORDER[dt2016$DES_FENOME == "ALUD"]= 5
....
    #UNION DE DATAFRAMES
    
    dtFinal<- rbind(dt2018,dt2017,dt2016,dt2015,dt2014)' })
  
  
  
  
  output$consulta2 <- renderText({
    
    " colocar el codigo de la consulta por ejemplo:
dplyr1<- dtFinal%>%filter(DesFenome=='INCENDIO URBANO'& grepl('DOMESTICO', DesEmerge))%>% select(CodEmerge:DesFenome,MesEmergencia)%>% arrange(MesEmergencia)
 "
    
  })
  
  output$tablaS3 <- renderDataTable({
    box<- input$selectdp
    if (is.null(box))
    {return(NULL)}
    
    if(box == "1")    {
      dplyr1<- dtFinal
      return (dplyr1)
    } else{  if(box == "2") {
      dplyr2<- dtFinal
      return (dplyr2)
    } else { if(box == "3") {
      dplyr3<- dtFinal
      return (dplyr3)
    } else { if(box == "4")
    { dplyr4 <- dtFinal
    return (dplyr4)
    } else { if(box=="5")
    { dplyr5 <- dtFinal
    return (dplyr5)
    } 
      
    } }
    } } }
  )
  
  output$consulta3 <- renderText({
    
    "colocar el codigo de la consulta por ejemplo:
sqldf1<-sqldf('SELECT DiaEmergencia, avg(AHumanitarFam) as avg_ahf FROM dtFinal  GROUP BY  DiaEmergencia order by avg_ahf DESC;')

"
    
  })
  
  output$tablaS4 <- renderDataTable({
    box<- input$selectsq
    if (is.null(box))
    {return(NULL)}
    
    if(box == "1")    {
      sqldf1<-sqldf
      return (sqldf1)
    } else{  if(box == "2") {
      sqldf2<-sqldf
      return (sqldf2)
    } else { if(box == "3") {
      sqldf1<-sqldf
      sqldf2<-sqldf
      sqldf3<-sqldf
      return (sqldf3)
    } else { if(box == "4")
    { sqldf4<- sqldf
    return (sqldf4)
    } else { if(box=="5")
    { sqldf5<- sqldf
    return (sqldf5)
    }  
    }
    }}}})
  
  
  
  
  output$consulta4 <- renderText({
    "  colocar el codigo de la consulta por ejemplo:
  gr1<- dtFinal  %>% filter(MesEmergencia<5)%>% group_by(DesGrupo)
    gp1 <- ggplot(gr1, aes(x=CodGrupo)) + geom_bar( width=0.5, colour='blue', fill='gray')+ facet_grid(MesEmergencia ~.)
    
    "
  })
  
  output$plot1 <- renderPlot({
    box<- input$selectgg
    if (is.null(box))
    {return(NULL)}
    
    if(box == "1")    {
      gr1<- dtFinal 
  
      return (gp1)
    } else{  if(box == "2") {
      gr2<- dtFinal 
      return (gp2)
    } else { if(box == "3") {
      gr3<-dtFinal 
      return (gp3)
    } else { if(box == "4") { 
      gp4<-
    return (gp4)
    } else { if(box=="5") { ç
      gr5<- 
    return (gp5)
    } 
    } }
    } } }
  )
  
  
  
  output$consulta5 <- renderText({
    "colocar el codigo de la consulta por ejemplo:
   
    "
  })
  
  
  output$consulta6 <- renderText({
    "dtModeloR2<-data.frame(dtFinal%>%group_by(DesDpto)%>%summarise(AHumanitarFam=sum(AHumanitarFam),EDanosVivienda=sum(EDanosVivienda)))
dtModeloR2.1<-dtModeloR2%>%select(AHumanitarFam,EDanosVivienda)

regresion2 <- lm(AHumanitarFam  ~  EDanosVivienda, data = dtModeloR2)
summary(regresion2)
    
ggplot(dtModeloR2, aes(x=AHumanitarFam, y=EDanosVivienda)) + geom_point() + ggtitle('Gráfica de Regresion') + xlab('Ayuda Humanitaria por Familia') + ylab('Estimacion de Daños por Vivienda') + geom_smooth(method=lm)
    "
  })
  
  
  
  
  output$tablaS6 <- renderTable({
    
    if(is.null(input$file1)){return ()}
    input$file1
    
  })
  
  
  
  
  output$plot8 <- renderPlot({
    
   
    return( )
    
  })
  
  observeEvent(input$control1, {
    if(input$control1==TRUE)
    {toggle("consulta1")} else {toggle("consulta1")}
    
  })
  
  
  
  observeEvent(input$control2, {
    
    if(input$control2==TRUE)
    {toggle("consulta2")} else {toggle("consulta2")}
    
  })
  
  observeEvent(input$control3, {
    
    if(input$control3==TRUE)
    {toggle("consulta3")} else {toggle("consulta3")}
    
  })
  
  observeEvent(input$control4, {
    
    if(input$control4==TRUE)
    {toggle("consulta4")} else {toggle("consulta4")}
    
  })
  observeEvent(input$control5, {
    
    if(input$control5==TRUE)
    {toggle("consulta5")} else {toggle("consulta5")}
    
  })
  
  
  observeEvent(input$control6, {
    
    if(input$control6==TRUE)
    {toggle("consulta6")} else {toggle("consulta6")}
    
  })
  
  
  output$tablaS5 <- renderTable({

  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
