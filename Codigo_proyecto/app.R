library(ggplot2)
library(shinyjs)#para la funcion toggle
library(shiny)
library(readxl)#para lectura de excel
library(dplyr)
library(lubridate)#para fechas
library(sqldf)
library(plotly)#para graficos dinamicos
library(shinythemes)
library(tidyverse)
library(RColorBrewer)
library(rvest)
library(DBI)
library(RMySQL)
library(DMwR)

source("www/introduccion.R")
source("www/recoleccion.R")
source("www/preprocesamiento.R")
source("www/consultas.R")
source("www/graficos.R")
source("www/modelo.R")
source("www/credenciales.R")
source("myLibrary.R")


ui <- fluidPage(
  theme = shinytheme("superhero"),
  titlePanel( "Trabajo Final de Admin de la Informacion"),
  p("Revisa nuestro ",
    a("informe",href="https://github.com/DPorta/TF_AdministracionInformacion/blob/main/Documentacion/InformeTF.md")),
  
  hr(),
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
  
  
  #CONEXION A DATABASE
  if(dbCanConnect(drv=driver,port=port,user=user,host=host, 
                  password=password,dbname=dbname)){
    conexion<-dbConnect(drv=driver,port=port,user=user,host=host,
                        password=password,
                        dbname=dbname) 
  }
  
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
   write.csv(gestantes,"gestantes.csv")
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
  observeEvent(input$controlDon, {
    write.csv(donaciones,"BackUp/donaciones.csv")
  })
  observeEvent(input$controlGes, {
    write.csv(gestantes,"BackUp/gestantes.csv")
  })
  output$tablaS5 <- renderTable({
    
    
    box<- input$selectPre
    
    if(box == "1")    {
      donaciones<-donaciones[,c(7,10,19,28,34,36,38,40,44,45,46,47)]
      donaciones<-donaciones[-c(120:133),]
      donaciones<-donaciones[-c(509:516),]
      donaciones$PLIEGO_NOMBRE[donaciones$PLIEGO_NOMBRE==" "]<-"Desconocido"
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
    #limpieza
    donaciones<-donaciones[,c(7,10,19,28,34,36,38,40,44,45,46,47)]
    donaciones<-donaciones[-c(120:133),]
    donaciones<-donaciones[-c(509:516),]
    donaciones$PLIEGO_NOMBRE[donaciones$PLIEGO_NOMBRE==" "]<-"Desconocido"
    
    gestantes<-gestantes[,c(3,5,7:12,15:18,20,22:25)]
    gestantes$Peso[gestantes$Peso == 0.0] <- mean(gestantes$Peso,na.rm = TRUE)
    gestantes$Peso[gestantes$Peso >155] <- mean(gestantes$Peso,na.rm = TRUE)
    
    gestantes$Talla[gestantes$Talla == 0.0] <- mean(gestantes$Talla,na.rm = TRUE)
    gestantes$Talla[gestantes$Talla >200] <- mean(gestantes$Talla,na.rm = TRUE)
    
    gestantes$Edad[gestantes$Edad < 13] <- mean(gestantes$Edad,na.rm = TRUE)
    gestantes$Edad_Gestacional[gestantes$Edad_Gestacional < 13] <- mean(gestantes$Edad_Gestacional,na.rm = TRUE)
    
    gestantes$Hemoglobina[gestantes$Hemoglobina == 0.0] <- mean(gestantes$Hemoglobina,na.rm = TRUE)
    gestantes$Hbc[gestantes$Hbc == 0.0] <- mean(gestantes$Hbc,na.rm = TRUE)
    
    ##
    
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
      dplyr3<- gestantes%>%filter(Edad_Gestacional < 20)
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
      dplyr11 <- donaciones%>%select(PLIEGO_NOMBRE,VALOR_TOTAL)%>%group_by(PLIEGO_NOMBRE)%>%summarise(total=n())%>%arrange(desc(total))
      return (dplyr11)
    } else  if(box == "12") { 
      dplyr12 <- gestantes%>%filter(Dx_Anemia=="Normal")
      return (dplyr12)
    } else  if(box == "13") { 
      dplyr13 <- donaciones%>%filter(str_detect(NOMBRE_PROVEEDOR,"CENARES"))
      return (dplyr13)
    } else  if(box == "14") { 
      dplyr14 <- gestantes%>%filter(str_detect(Fecha,"/06/2017"))
      return (dplyr14)
    } else  if(box == "15") { 
      dplyr15 <- donaciones%>%filter(str_detect(FECHA_REG,"/03/2020"))
      return (dplyr15)
    } else  if(box == "16") { 
      dplyr16 <- donaciones%>%filter(str_detect(NOMBRE_GRUPO,"MÉDICO")|str_detect(NOMBRE_CLASE,"MÉDICO"))
      return (dplyr16)
    } else  if(box == "17") { 
      dplyr17 <- gestantes%>%group_by(Provincia)%>%summarise(total=n())%>%arrange(desc(total))
      return (dplyr17)
    } else  if(box == "18") { 
      dplyr18 <- gestantes%>%filter(Dx_Anemia=="Anemia Leve"&Distrito=="COYLLURQUI")
      return (dplyr18)
    } else  if(box == "19") { 
      dplyr19 <- gestantes%>%filter(Dx_CLAP=="SobrePeso"&EESS=="P.S. PISONAYPATA")
      return (dplyr19)
    } else  if(box == "20") { 
      dplyr20 <- gestantes%>%filter(str_detect(Fecha,"/01/2017")&Provincia=="COTABAMBAS")
      return (dplyr20)
    } 
    
  })
  output$consulta2 <- renderText({
    
    '  CODIGO DE CONSULTAS
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
      dplyr11 <- donaciones%>%select(PLIEGO_NOMBRE,VALOR_TOTAL)%>%group_by(PLIEGO_NOMBRE)%>%summarise(total=n())%>%arrange(desc(total))
      return (dplyr11)
    } else  if(box == "12") { 
      dplyr12 <- gestantes%>%filter(Dx_Anemia=="Normal")
      return (dplyr12)
    } else  if(box == "13") { 
      dplyr13 <- donaciones%>%filter(str_detect(NOMBRE_PROVEEDOR,"CENARES"))
      return (dplyr13)
    } else  if(box == "14") { 
      dplyr14 <- gestantes%>%filter(str_detect(Fecha,"/06/2017"))
      return (dplyr14)
    } else  if(box == "15") { 
      dplyr15 <- donaciones%>%filter(str_detect(FECHA_REG,"/03/2020"))
      return (dplyr15)
    } else  if(box == "16") { 
      dplyr16 <- donaciones%>%filter(str_detect(NOMBRE_GRUPO,"MÉDICO")|str_detect(NOMBRE_CLASE,"MÉDICO"))
      return (dplyr16)
    } else  if(box == "17") { 
      dplyr17 <- gestantes%>%group_by(Provincia)%>%summarise(total=n())%>%arrange(desc(total))
      return (dplyr17)
    } else  if(box == "18") { 
      dplyr18 <- gestantes%>%filter(Dx_Anemia=="Anemia Leve"&Distrito=="COYLLURQUI")
      return (dplyr18)
    } else  if(box == "19") { 
      dplyr19 <- gestantes%>%filter(Dx_CLAP=="SobrePeso"&EESS=="P.S. PISONAYPATA")
      return (dplyr19)
    } else  if(box == "20") { 
      dplyr20 <- gestantes%>%filter(str_detect(Fecha,"/01/2017")&Provincia=="COTABAMBAS")
      return (dplyr20)
    }'
    
  })
  #GRAFICOS
  output$plot1 <- renderPlot({
    
    #limpieza
    donaciones<-donaciones[,c(7,10,19,28,34,36,38,40,44,45,46,47)]
    donaciones<-donaciones[-c(120:133),]
    donaciones<-donaciones[-c(509:516),]
    donaciones$PLIEGO_NOMBRE[donaciones$PLIEGO_NOMBRE==" "]<-"Desconocido"
    
    gestantes<-gestantes[,c(3,5,7:12,15:18,20,22:25)]
    gestantes$Peso[gestantes$Peso == 0.0] <- mean(gestantes$Peso,na.rm = TRUE)
    gestantes$Peso[gestantes$Peso >155] <- mean(gestantes$Peso,na.rm = TRUE)
    
    gestantes$Talla[gestantes$Talla == 0.0] <- mean(gestantes$Talla,na.rm = TRUE)
    gestantes$Talla[gestantes$Talla >200] <- mean(gestantes$Talla,na.rm = TRUE)
    
    gestantes$Edad[gestantes$Edad < 13] <- mean(gestantes$Edad,na.rm = TRUE)
    gestantes$Edad_Gestacional[gestantes$Edad_Gestacional < 13] <- mean(gestantes$Edad_Gestacional,na.rm = TRUE)
    
    gestantes$Hemoglobina[gestantes$Hemoglobina == 0.0] <- mean(gestantes$Hemoglobina,na.rm = TRUE)
    gestantes$Hbc[gestantes$Hbc == 0.0] <- mean(gestantes$Hbc,na.rm = TRUE)
    
    ##
    
    box<- input$selectgg
    if (is.null(box))
    {return(NULL)}
    
    if(box == "1")    {
      dplyr1_A<-gestantes%>%group_by(Distrito)%>%summarise(total=n())
      gr1<- ggplot(dplyr1_A, aes(x="",y=total, fill=Distrito))+
        geom_bar(stat="identity") +
        coord_polar("y", start=0) +
        theme_void()+labs(title="Distribucion de gestantes por distritos")
      return (gr1)
    } else  if(box == "2") {
      dplyr2_B<- gestantes%>%group_by(Dx_CLAP)%>%filter(EESS=="C.S. BELLAVISTA")
      gr2<-ggplot(dplyr2_B, aes(x=Dx_CLAP,y=NCOL(EESS),fill=Dx_CLAP))+
        geom_bar(stat="identity")+theme(axis.text.x=element_blank(),legend.title =  element_blank())+labs(title="Gesante con Transtonos alimenticios del C.S.Bellavista",y="C.S. BELLAVISTA")
      return (gr2)
    } else  if(box == "3") {
      dplyr3_A<-gestantes%>%filter(str_detect(Fecha,"/01/2017"))%>%filter(Edad < 20)
      gr3<-ggplot(dplyr3_A,aes(x =Fecha, y = Edad, fill=Fecha))+ geom_step(direction = "hv")+theme(axis.text.x=element_blank(),legend.title =  element_blank())+labs(title="Gestantes que se atendieron en el mes de enero en funcion a su edad",x="Fecha",y="Edad")
      return (gr3)
    } else  if(box == "4") { 
      dplyr4_A<-gestantes%>%filter(Edad_Gestacional>30)%>%group_by(Dx_CLAP)
      gr4 <- ggplot(dplyr4_A, aes(x=Edad_Gestacional, y = Dx_CLAP, fill=Edad_Gestacional))+geom_point(stat="Identity",position = "jitter", aes(color = Edad_Gestacional))+labs(title="Gestantes Mayores de 30 en funcion a las conclusiones de su peso",x="Edad Gestacional",y="Dx_CLAP")
      return (gr4)
    } else  if(box == "5") { 
      dplyr5_A<-gestantes%>%filter(Hemoglobina<15 & Hemoglobina>5)%>%group_by(Dx_Anemia)
      gr5 <- ggplot(dplyr5_A, aes(x=Dx_Anemia, y = Hemoglobina, fill(Hemoglobina)))+geom_violin()+geom_point(aes(color = Hemoglobina))+labs(title="Gestantes que presentan algun tipo de Anemia en base a su Talla",x="Tipo de Anemia",y="Hemoglobina")
      return (gr5)
    } else  if(box == "6") { 
      dplyr6_A<-gestantes%>%filter(Distrito == "PICHIRHUA")%>%group_by(PPG)
      gr6<-ggplot(dplyr6_A, aes(x = PPG, y = Distrito, fill =PPG))+geom_jitter(aes(color = PPG))+labs(title="Dispercion de Gestantes segun PPG en el Distrito Pichirhua",x="PPC",y="Distrito")
      return (gr6)
    } else  if(box == "7") { 
      dplyr7_A<-gestantes%>%filter(Provincia == "ABANCAY")%>%group_by(Dx_IOM)
      gr7 <- ggplot(dplyr7_A, aes(x =Provincia, y = Dx_IOM, fill = Dx_IOM))+geom_bar(stat = "identity")+coord_polar("y", start = 0)+theme(axis.text.x=element_blank(),legend.title =  element_blank())+labs(title="Tipo de Embarazo en la provincia de Abancay",y="Tipo Embarazo")
      return (gr7)
    } else  if(box == "8") { 
      dplyr8_A<-gestantes%>%filter(Edad > 16 & Edad < 25)%>%group_by(Dx_Anemia)
      gr8 <-ggplot(dplyr8_A, aes(x = Dx_Anemia, y = Edad, fill = Dx_Anemia))+geom_jitter(aes(color = Dx_Anemia))+labs(title="Anemia segun rango de edad de 16 a 25",y="Edad", x = "Tipo de Anemia")
      return (gr8)
    } else  if(box == "9") { 
      dplyr9_A<-gestantes%>%filter(Hbc<10)%>%group_by(Distrito)
      gr9 <- ggplot(dplyr9_A,aes(x = Distrito, y = Hbc, fill = Distrito))+ geom_bar(stat = "identity")+theme(axis.text.x=element_blank(),legend.title =  element_blank())+labs(title="Distritos con HBC < 10",x="Distrito",y="Hbc")
      return (gr9)
    } else  if(box == "10") { 
      dplyr_10A<-gestantes%>%filter(Hemoglobina >3)%>%filter(Peso>50 &Peso <100)
      gr10 <- ggplot(dplyr_10A, aes(x = Peso, y = Hemoglobina, fill=Peso))+geom_jitter(aes(color = Peso))
      return (gr10)
    } else  if(box == "11") { 
      gr11 <- ggplot(dplyr11, aes(x = PLIEGO_NOMBRE, y=total,fill=PLIEGO_NOMBRE))+geom_bar(stat="identity")+theme(axis.text.x=element_blank(),legend.title =  element_blank())+labs(title="Institucion que mas aporto",x="Institucion",y="Total")
      return (gr11)
    } else  if(box == "12") { 
      dplyr12_A<-gestantes%>%group_by(Dx_Anemia)%>%summarise(total=n())
      gr12 <- ggplot(dplyr12_A, aes(x="", y=total, fill=Dx_Anemia)) +
        geom_bar(stat="identity") +
        coord_polar("y", start=0) +
        theme_void()+labs(title="Distribucion de Anemia entre las Gestantes")
      return (gr12)
    } else  if(box == "13") { 
      gr13 <- ggplot(dplyr13, aes(x="", y=NOMBRE_PROVEEDOR, fill=NOMBRE_PROVEEDOR)) +
        geom_bar(stat="identity") +
        coord_polar("y", start=0) +
        theme_void()+labs(title="Distribucion de Proveedores de CENARES")
      return (gr13)
    } else  if(box == "14") { 
      dplyr14_A<-dplyr14%>%group_by(Fecha)%>%summarise(total=n())
      gr14 <- ggplot(dplyr14_A, aes(y=total, x =Fecha,fill=Fecha))+geom_jitter()+theme(axis.text.x=element_blank(),legend.title =  element_blank())+labs(title="Consultas por Fecha en Junio de 2017",x="Fecha",y="Total")
      return (gr14)
    } else  if(box == "15") { 
      gr15 <- ggplot(dplyr15, aes(x="", y=NOMBRE_GRUPO, fill=NOMBRE_GRUPO)) +
        geom_bar(stat="identity") +
        coord_polar("y", start=0) +
        theme_void()+labs(title="Distribucion de Productos donados en Marzo 2020")
      return (gr15)
    } else  if(box == "16") { 
      gr16 <-  ggplot(dplyr16, aes(x="", y=NOMBRE_CLASE, fill=NOMBRE_CLASE)) +
        geom_bar(stat="identity") +
        coord_polar("y", start=0) +
        theme_void()+labs(title="Tipo de productos medicos donados")
      return (gr16)
    } else  if(box == "17") { 
      gr17 <- ggplot(dplyr17, aes(x = Provincia, y=total,fill=Provincia))+geom_bar(stat="identity")+theme(axis.text.x=element_blank(),legend.title =  element_blank())+labs(title="Provincias con mayor cantidad de gestantes",x="Provincias",y="Cantidad")
      return (gr17)
    } else  if(box == "18") { 
      dplyr18_A<-gestantes%>%filter(Provincia=="ABANCAY")
      gr18 <- ggplot(dplyr18_A,aes(x=Distrito,y=Edad,fill=Distrito))+geom_boxplot()+theme(axis.text.x=element_blank(),legend.title =  element_blank())+labs(title="Distribucion de edades por distrito en la provincia Abancay",x="Distrito",y="Edad")
      return (gr18)
    } else  if(box == "19") { 
      dplyr19_A<-gestantes%>%filter(Dx_CLAP=="SobrePeso"&Peso>40&Provincia=="ANDAHUAYLAS")
      gr19 <- ggplot(dplyr19_A, aes(x="", y=EESS, fill=EESS)) +
        geom_bar(stat="identity") +
        coord_polar("y", start=0) +
        theme_void()+labs(title="Gestantes con sobrepeso mayores a 30 de Andahuaylas segun Centro de Atencion")
      return (gr19)
    } else  if(box == "20") { 
      gr20 <- ggplot(dplyr20,aes(x=Distrito,y=Talla,fill=Distrito))+geom_boxplot()+theme(axis.text.x=element_blank(),legend.title =  element_blank())+labs(title="Madres de Cotabamba atendidas en Enero de 2017 segun talla y Distrito",x="Distrito",y="Talla")
      return (gr20)
    } 
    })
  output$consulta3 <- renderText({
    '
    if(box == "1")    {
      dplyr1_A<-gestantes%>%group_by(Distrito)%>%summarise(total=n())
      gr1<- ggplot(dplyr1_A, aes(x="",y=total, fill=Distrito))+
        geom_bar(stat="identity") +
        coord_polar("y", start=0) +
        theme_void()+labs(title="Distribucion de gestantes por distritos")
      return (gr1)
    } else  if(box == "2") {
      dplyr2_B<- gestantes%>%group_by(Dx_CLAP)%>%filter(EESS=="C.S. BELLAVISTA")
      gr2<-ggplot(dplyr2_B, aes(x=Dx_CLAP,y=NCOL(EESS),fill=Dx_CLAP))+
        geom_bar(stat="identity")+theme(axis.text.x=element_blank(),legend.title =  element_blank())+labs(title="Gesante con Transtonos alimenticios del C.S.Bellavista",y="C.S. BELLAVISTA")
      return (gr2)
    } else  if(box == "3") {
      dplyr3_A<-gestantes%>%filter(str_detect(Fecha,"/01/2017"))%>%filter(Edad < 20)
      gr3<-ggplot(dplyr3_A,aes(x =Fecha, y = Edad, fill=Fecha))+ geom_step(direction = "hv")+theme(axis.text.x=element_blank(),legend.title =  element_blank())+labs(title="Gestantes que se atendieron en el mes de enero en funcion a su edad",x="Fecha",y="Edad")
      return (gr3)
    } else  if(box == "4") { 
      dplyr4_A<-gestantes%>%filter(Edad_Gestacional>30)%>%group_by(Dx_CLAP)
      gr4 <- ggplot(dplyr4_A, aes(x=Edad_Gestacional, y = Dx_CLAP, fill=Edad_Gestacional))+geom_point(stat="Identity",position = "jitter", aes(color = Edad_Gestacional))+labs(title="Gestantes Mayores de 30 en funcion a las conclusiones de su peso",x="Edad Gestacional",y="Dx_CLAP")
      return (gr4)
    } else  if(box == "5") { 
      dplyr5_A<-gestantes%>%filter(Hemoglobina<15 & Hemoglobina>5)%>%group_by(Dx_Anemia)
      gr5 <- ggplot(dplyr5_A, aes(x=Dx_Anemia, y = Hemoglobina, fill(Hemoglobina)))+geom_violin()+geom_point(aes(color = Hemoglobina))+labs(title="Gestantes que presentan algun tipo de Anemia en base a su Talla",x="Tipo de Anemia",y="Hemoglobina")
      return (gr5)
    } else  if(box == "6") { 
      dplyr6_A<-gestantes%>%filter(Distrito == "PICHIRHUA")%>%group_by(PPG)
      gr6<-ggplot(dplyr6_A, aes(x = PPG, y = Distrito, fill =PPG))+geom_jitter(aes(color = PPG))+labs(title="Dispercion de Gestantes segun PPG en el Distrito Pichirhua",x="PPC",y="Distrito")
      return (gr6)
    } else  if(box == "7") { 
      dplyr7_A<-gestantes%>%filter(Provincia == "ABANCAY")%>%group_by(Dx_IOM)
      gr7 <- ggplot(dplyr7_A, aes(x =Provincia, y = Dx_IOM, fill = Dx_IOM))+geom_bar(stat = "identity")+coord_polar("y", start = 0)+theme(axis.text.x=element_blank(),legend.title =  element_blank())+labs(title="Tipo de Embarazo en la provincia de Abancay",y="Tipo Embarazo")
      return (gr7)
    } else  if(box == "8") { 
      dplyr8_A<-gestantes%>%filter(Edad > 16 & Edad < 25)%>%group_by(Dx_Anemia)
      gr8 <-ggplot(dplyr8_A, aes(x = Dx_Anemia, y = Edad, fill = Dx_Anemia))+geom_jitter(aes(color = Dx_Anemia))+labs(title="Anemia segun rango de edad de 16 a 25",y="Edad", x = "Tipo de Anemia")
      return (gr8)
    } else  if(box == "9") { 
      dplyr9_A<-gestantes%>%filter(Hbc<10)%>%group_by(Distrito)
      gr9 <- ggplot(dplyr9_A,aes(x = Distrito, y = Hbc, fill = Distrito))+ geom_bar(stat = "identity")+theme(axis.text.x=element_blank(),legend.title =  element_blank())+labs(title="Distritos con HBC < 10",x="Distrito",y="Hbc")
      return (gr9)
    } else  if(box == "10") { 
      dplyr_10A<-gestantes%>%filter(Hemoglobina >3)%>%filter(Peso>50 &Peso <100)
      gr10 <- ggplot(dplyr_10A, aes(x = Peso, y = Hemoglobina, fill=Peso))+geom_jitter(aes(color = Peso))
      return (gr10)
    } else  if(box == "11") { 
      gr11 <- ggplot(dplyr11, aes(x = PLIEGO_NOMBRE, y=total,fill=PLIEGO_NOMBRE))+geom_bar(stat="identity")+theme(axis.text.x=element_blank(),legend.title =  element_blank())+labs(title="Institucion que mas aporto",x="Institucion",y="Total")
      return (gr11)
    } else  if(box == "12") { 
      dplyr12_A<-gestantes%>%group_by(Dx_Anemia)%>%summarise(total=n())
      gr12 <- ggplot(dplyr12_A, aes(x="", y=total, fill=Dx_Anemia)) +
        geom_bar(stat="identity") +
        coord_polar("y", start=0) +
        theme_void()+labs(title="Distribucion de Anemia entre las Gestantes")
      return (gr12)
    } else  if(box == "13") { 
      gr13 <- ggplot(dplyr13, aes(x="", y=NOMBRE_PROVEEDOR, fill=NOMBRE_PROVEEDOR)) +
        geom_bar(stat="identity") +
        coord_polar("y", start=0) +
        theme_void()+labs(title="Distribucion de Proveedores de CENARES")
      return (gr13)
    } else  if(box == "14") { 
      dplyr14_A<-dplyr14%>%group_by(Fecha)%>%summarise(total=n())
      gr14 <- ggplot(dplyr14_A, aes(y=total, x =Fecha,fill=Fecha))+geom_jitter()+theme(axis.text.x=element_blank(),legend.title =  element_blank())+labs(title="Consultas por Fecha en Junio de 2017",x="Fecha",y="Total")
      return (gr14)
    } else  if(box == "15") { 
      gr15 <- ggplot(dplyr15, aes(x="", y=NOMBRE_GRUPO, fill=NOMBRE_GRUPO)) +
        geom_bar(stat="identity") +
        coord_polar("y", start=0) +
        theme_void()+labs(title="Distribucion de Productos donados en Marzo 2020")
      return (gr15)
    } else  if(box == "16") { 
      gr16 <-  ggplot(dplyr16, aes(x="", y=NOMBRE_CLASE, fill=NOMBRE_CLASE)) +
        geom_bar(stat="identity") +
        coord_polar("y", start=0) +
        theme_void()+labs(title="Tipo de productos medicos donados")
      return (gr16)
    } else  if(box == "17") { 
      gr17 <- ggplot(dplyr17, aes(x = Provincia, y=total,fill=Provincia))+geom_bar(stat="identity")+theme(axis.text.x=element_blank(),legend.title =  element_blank())+labs(title="Provincias con mayor cantidad de gestantes",x="Provincias",y="Cantidad")
      return (gr17)
    } else  if(box == "18") { 
      dplyr18_A<-gestantes%>%filter(Provincia=="ABANCAY")
      gr18 <- ggplot(dplyr18_A,aes(x=Distrito,y=Edad,fill=Distrito))+geom_boxplot()+theme(axis.text.x=element_blank(),legend.title =  element_blank())+labs(title="Distribucion de edades por distrito en la provincia Abancay",x="Distrito",y="Edad")
      return (gr18)
    } else  if(box == "19") { 
      dplyr19_A<-gestantes%>%filter(Dx_CLAP=="SobrePeso"&Peso>40&Provincia=="ANDAHUAYLAS")
      gr19 <- ggplot(dplyr19_A, aes(x="", y=EESS, fill=EESS)) +
        geom_bar(stat="identity") +
        coord_polar("y", start=0) +
        theme_void()+labs(title="Gestantes con sobrepeso mayores a 30 de Andahuaylas segun Centro de Atencion")
      return (gr19)
    } else  if(box == "20") { 
      gr20 <- ggplot(dplyr20,aes(x=Distrito,y=Talla,fill=Distrito))+geom_boxplot()+theme(axis.text.x=element_blank(),legend.title =  element_blank())+labs(title="Madres de Cotabamba atendidas en Enero de 2017 segun talla y Distrito",x="Distrito",y="Talla")
      return (gr20)
    } 
    })'
  })
  
  #MODELOS
  
  output$consulta6 <- renderText({
    "SOY UN TEXTITO    "
  })
  output$plot8 <- renderPlot({
    #limpieza
    donaciones<-donaciones[,c(7,10,19,28,34,36,38,40,44,45,46,47)]
    donaciones<-donaciones[-c(120:133),]
    donaciones<-donaciones[-c(509:516),]
    donaciones$PLIEGO_NOMBRE[donaciones$PLIEGO_NOMBRE==" "]<-"Desconocido"
    
    gestantes<-gestantes[,c(3,5,7:12,15:18,20,22:25)]
    gestantes$Peso[gestantes$Peso == 0.0] <- mean(gestantes$Peso,na.rm = TRUE)
    gestantes$Peso[gestantes$Peso >155] <- mean(gestantes$Peso,na.rm = TRUE)
    
    gestantes$Talla[gestantes$Talla == 0.0] <- mean(gestantes$Talla,na.rm = TRUE)
    gestantes$Talla[gestantes$Talla >200] <- mean(gestantes$Talla,na.rm = TRUE)
    
    gestantes$Edad[gestantes$Edad < 13] <- mean(gestantes$Edad,na.rm = TRUE)
    gestantes$Edad_Gestacional[gestantes$Edad_Gestacional < 13] <- mean(gestantes$Edad_Gestacional,na.rm = TRUE)
    
    gestantes$Hemoglobina[gestantes$Hemoglobina == 0.0] <- mean(gestantes$Hemoglobina,na.rm = TRUE)
    gestantes$Hbc[gestantes$Hbc == 0.0] <- mean(gestantes$Hbc,na.rm = TRUE)
    
    ##
    
    box<- input$selectMo
    if (is.null(box))
    {return(NULL)}
    
    if(box == "1")    {
      gg1<-ggplot(gestantes, aes(x=Hemoglobina, y=Hbc)) + geom_point() + ggtitle("Grafica de Regresion: Hemoglobina vs Hbc") + xlab("Hemoglobina") + ylab("Peso") + geom_smooth(method=lm)
      return (gg1)
    } else  if(box == "2") {
      gg2<-gg1
      return (gg2)
    } else  if(box == "3") {
      gg3<-gg1
      return (gg3)
    } else  if(box == "4") { 
      gg4<-gg1
      return (gg4)
    }
    
  })
  output$plot9 <- renderDataTable({
    #limpieza
    donaciones<-donaciones[,c(7,10,19,28,34,36,38,40,44,45,46,47)]
    donaciones<-donaciones[-c(120:133),]
    donaciones<-donaciones[-c(509:516),]
    donaciones$PLIEGO_NOMBRE[donaciones$PLIEGO_NOMBRE==" "]<-"Desconocido"
    
    gestantes<-gestantes[,c(3,5,7:12,15:18,20,22:25)]
    gestantes$Peso[gestantes$Peso == 0.0] <- mean(gestantes$Peso,na.rm = TRUE)
    gestantes$Peso[gestantes$Peso >155] <- mean(gestantes$Peso,na.rm = TRUE)
    
    gestantes$Talla[gestantes$Talla == 0.0] <- mean(gestantes$Talla,na.rm = TRUE)
    gestantes$Talla[gestantes$Talla >200] <- mean(gestantes$Talla,na.rm = TRUE)
    
    gestantes$Edad[gestantes$Edad < 13] <- mean(gestantes$Edad,na.rm = TRUE)
    gestantes$Edad_Gestacional[gestantes$Edad_Gestacional < 13] <- mean(gestantes$Edad_Gestacional,na.rm = TRUE)
    
    gestantes$Hemoglobina[gestantes$Hemoglobina == 0.0] <- mean(gestantes$Hemoglobina,na.rm = TRUE)
    gestantes$Hbc[gestantes$Hbc == 0.0] <- mean(gestantes$Hbc,na.rm = TRUE)
    
    ##
    
    box<- input$selectMo
    if (is.null(box))
    {return(NULL)}
    
    if(box == "1")    {
      dT1<-regresion(gestantes$Hemoglobina,gestantes$Hbc,8)
      return (dT1)
    } else  if(box == "2") {
      dT2<-knn(df,35,70,1)
      return (dT2)
    } else  if(box == "3") {
      dT3<-dT1
      return (dT3)
    } else  if(box == "4") { 
      dT4<-dT1
      return (dT4)
    }
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
