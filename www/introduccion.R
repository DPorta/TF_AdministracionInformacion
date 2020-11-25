introduccion<-{
  fluidRow(
    column(12,align="left",h2("Universidad Peruana de Ciencias Aplicadas"),
           column(12,tags$img(src="datascience.jpg",width="40%")),
           column(6,align="left",br(),h4("Descripcion del Proyecto"),
           tags$b(h4("El proyecto consiste en el desarrollo de un producto de software relacionado a ciencia de datos,
               hemos decidido trabajar con 2 fuentes de informacion. La primera fuenta data sobre las donaciones para 
               los afectados por COVID-19. La segunda fuenta se basa en los datos de las madres gestantes de huancavelica"))),
           p(align="left","")),
    column(12,tags$video(src="clip.mp4",type="video/mp4",controls=NA,width="100%"))
                   
    
    
  )}