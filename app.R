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
  donaciones<- read.csv('Datasets/pcm_donaciones.csv',sep="|")
  gestantes<-read.csv("Datasets/Gestantes_Apurimac_Estado_Nutricional_2017.csv")
  
 
  
  #RECOLECCION
  output$tablaS2 <- renderTable({
    tablaS1 <- input$file1
    if (is.null(tablaS1))
    {return(NULL)}
    box<- input$select
    if(box == "1")    {
      a <-read.csv(paste("Datasets",tablaS1$name,sep = "/"))
      head(a)
    } else if(box == "2") {
      a <-read.csv(paste("Datasets",tablaS1$name,sep = "/"))
      if(is.null(a)){a <-read.csv(paste("Datasets",tablaS1$name,sep = "/"),sep = "|")}
      head(a)
    } else if(box == "3") {
      a <-read_xlsx(paste("Datasets",tablaS1$name,sep = "/"))
      head(a)
    } 
  })
  output$tablaS6 <- renderTable({
    
    if(is.null(input$file1)){return ()}
    input$file1
    
  })

  #PREPROCESAMIENTO
  observeEvent(input$control1, {
   
    output$consulta1 <- renderText({
      'Codigo de limpieza de texto
      donaciones<-donaciones[,c(7,10,19,28,34,36,38,40,44,45,46,47)]
      donaciones<-donaciones[-c(120:133),]
      donaciones<-donaciones[-c(509:516),]
      
      gestantes<-gestantes[,c(3,5,7:12,15:18,20,22:25)]
      gestantes$Peso[gestantes$Peso == 0.0] <- mean(gestantes$Peso,na.rm = TRUE)
      gestantes$Peso[gestantes$Peso >155] <- mean(gestantes$Peso,na.rm = TRUE)
      
      gestantes$Talla[gestantes$Talla == 0.0] <- mean(gestantes$Talla,na.rm = TRUE)
      gestantes$Talla[gestantes$Talla >200] <- mean(gestantes$Talla,na.rm = TRUE)
      
      gestantes$Edad[gestantes$Edad < 13] <- mean(gestantes$Edad,na.rm = TRUE)
      gestantes$Edad_Gestacional[gestantes$Edad_Gestacional < 13] <- mean(gestantes$Edad_Gestacional,na.rm = TRUE)
      
      gestantes$Hemoglobina[gestantes$Hemoglobina == 0.0] <- mean(gestantes$Hemoglobina,na.rm = TRUE)
      gestantes$Hbc[gestantes$Hbc == 0.0] <- mean(gestantes$Hbc,na.rm = TRUE)'
    })
  })
  output$tablaS5 <- renderTable({
    
    
    box<- input$selectPre
    
    if(box == "1")    {
      donaciones<-donaciones[,c(7,10,19,28,34,36,38,40,44,45,46,47)]
      donaciones<-donaciones[-c(120:133),]
      donaciones<-donaciones[-c(509:516),]
      head(donaciones)
    } else if(box == "2") {
      gestantes<-gestantes[,c(3,5,7:12,15:18,20,22:25)]
      gestantes$Peso[gestantes$Peso == 0.0] <- mean(gestantes$Peso,na.rm = TRUE)
      gestantes$Peso[gestantes$Peso >155] <- mean(gestantes$Peso,na.rm = TRUE)
      
      gestantes$Talla[gestantes$Talla == 0.0] <- mean(gestantes$Talla,na.rm = TRUE)
      gestantes$Talla[gestantes$Talla >200] <- mean(gestantes$Talla,na.rm = TRUE)
      
      gestantes$Edad[gestantes$Edad < 13] <- mean(gestantes$Edad,na.rm = TRUE)
      gestantes$Edad_Gestacional[gestantes$Edad_Gestacional < 13] <- mean(gestantes$Edad_Gestacional,na.rm = TRUE)
      
      gestantes$Hemoglobina[gestantes$Hemoglobina == 0.0] <- mean(gestantes$Hemoglobina,na.rm = TRUE)
      gestantes$Hbc[gestantes$Hbc == 0.0] <- mean(gestantes$Hbc,na.rm = TRUE)
      head(gestantes)
    } 
    
  })
  
  #CONSULTAS
  output$tablaS3 <- renderDataTable({
    box<- input$selectdp
    if (is.null(box))
    {return(NULL)}
    
    if(box == "1")    {
      dplyr1<- gestantes%>%filter(Distrito=="TINTAY"&Edad<25)
      return (dplyr1)
    } else  if(box == "2") {
      dplyr2<- gestantes%>%filter(Dx_CLAP=="SobrePeso"&EESS=="C.S. BELLAVISTA")
      return (dplyr2)
    } else  if(box == "3") {
      dplyr3<- gestantes%>%filter(Edad < 20)
      return (dplyr3)
    } else  if(box == "4") { 
      dplyr4 <- gestantes%>%filter(Edad > 30&Dx_CLAP=="SobrePeso")
      return (dplyr4)
    } else  if(box == "5") { 
      dplyr5 <- gestantes%>%filter(Talla < 160 & Dx_Anemia=="Anemia Moderada")
      return (dplyr5)
    } else  if(box == "6") { 
      dplyr6 <- donaciones%>%filter(NOMBRE_CLASE=="BEBIDAS Y AFINES"&PLIEGO_NOMBRE=="M. DE SALUD")
      return (dplyr6)
    } else  if(box == "7") { 
      dplyr7 <- gestantes%>%filter(Provincia=="ABANCAY"&Hemoglobina > 10)
      return (dplyr7)
    } else  if(box == "8") { 
      dplyr8 <- donaciones%>%filter(CANT_ARTICULO > 100)
      return (dplyr8)
    } else  if(box == "9") { 
      dplyr9 <- gestantes%>%filter(EESS=="P.S. OCOBAMBA"&Hbc<10)
      return (dplyr9)
    } else  if(box == "10") { 
      dplyr10 <- gestantes%>%filter(Peso > 60&Distrito =="TAMBURCO"&Dx_Anemia=="Anemia Leve")
      return (dplyr10)
    } else  if(box == "11") { 
      dplyr11 <- donaciones
      return (dplyr11)
    } else  if(box == "12") { 
      dplyr12 <- donaciones
      return (dplyr12)
    } else  if(box == "13") { 
      dplyr13 <- donaciones
      return (dplyr13)
    } else  if(box == "14") { 
      dplyr14 <- donaciones
      return (dplyr14)
    } else  if(box == "15") { 
      dplyr15 <- donaciones
      return (dplyr15)
    } else  if(box == "16") { 
      dplyr16 <- donaciones
      return (dplyr16)
    } else  if(box == "17") { 
      dplyr17 <- donaciones
      return (dplyr17)
    } else  if(box == "18") { 
      dplyr18 <- donaciones
      return (dplyr18)
    } else  if(box == "19") { 
      dplyr19 <- donaciones
      return (dplyr19)
    } else  if(box == "20") { 
      dplyr20 <- donaciones
      return (dplyr20)
    } 
    
  })
  output$consulta2 <- renderText({
    
    " COPIAR CODIGO DE CONSULTAS AQUI "
    
  })
  
  #GRAFICOS
  output$plot1 <- renderPlot({
    box<- input$selectdp
    if (is.null(box))
    {return(NULL)}
    
    if(box == "1")    {
      gr1<- donaciones
      return (gr1)
    } else  if(box == "2") {
      gr2<- donaciones
      return (gr2)
    } else  if(box == "3") {
      gr3<- donaciones
      return (gr3)
    } else  if(box == "4") { 
      gr4 <- donaciones
      return (gr4)
    } else  if(box == "5") { 
      gr5 <- donaciones
      return (gr5)
    } else  if(box == "6") { 
      gr6 <- donaciones
      return (gr6)
    } else  if(box == "7") { 
      gr7 <- donaciones
      return (gr7)
    } else  if(box == "8") { 
      gr8 <- donaciones
      return (gr8)
    } else  if(box == "9") { 
      gr9 <- donaciones
      return (gr9)
    } else  if(box == "10") { 
      gr10 <- donaciones
      return (gr10)
    } else  if(box == "11") { 
      gr11 <- donaciones
      return (gr11)
    } else  if(box == "12") { 
      gr12 <- donaciones
      return (gr12)
    } else  if(box == "13") { 
      gr13 <- donaciones
      return (gr13)
    } else  if(box == "14") { 
      gr14 <- donaciones
      return (gr14)
    } else  if(box == "15") { 
      gr15 <- donaciones
      return (gr15)
    } else  if(box == "16") { 
      gr16 <- donaciones
      return (gr16)
    } else  if(box == "17") { 
      gr17 <- donaciones
      return (gr17)
    } else  if(box == "18") { 
      gr18 <- donaciones
      return (gr18)
    } else  if(box == "19") { 
      gr19 <- donaciones
      return (gr19)
    } else  if(box == "20") { 
      gr20 <- donaciones
      return (gr20)
    } 
    })
  output$consulta3 <- renderText({
    " 
    Codigo de Graficos aqui
    
    "
  })
  
  #MODELOS
  observeEvent(input$control6, {
    
    if(input$control6==TRUE)
    {toggle("consulta6")} else {toggle("consulta6")}
    
  })
  output$consulta6 <- renderText({
    "dtModeloR2<-data.frame(donaciones%>%group_by(DesDpto)%>%summarise(AHumanitarFam=sum(AHumanitarFam),EDanosVivienda=sum(EDanosVivienda)))
dtModeloR2.1<-dtModeloR2%>%select(AHumanitarFam,EDanosVivienda)

regresion2 <- lm(AHumanitarFam  ~  EDanosVivienda, data = dtModeloR2)
summary(regresion2)
    
ggplot(dtModeloR2, aes(x=AHumanitarFam, y=EDanosVivienda)) + geom_point() + ggtitle('Gráfica de Regresion') + xlab('Ayuda Humanitaria por Familia') + ylab('Estimacion de Daños por Vivienda') + geom_smooth(method=lm)
    "
  })
  output$plot8 <- renderPlot({
    
    
    return( )
    
  })

  
}

# Run the application 
shinyApp(ui = ui, server = server)
