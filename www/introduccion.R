introduccion<-{
  fluidRow(
    column(12,align="center",h3("Universidad Peruana de Ciencias Aplicadas"),
           tags$b(h4("proyecto demostracion"))),
    column(12,tags$video(src="clip.mp4",type="video/mp4",controls=NA,width="100%")),
    div(
      column(6,align="center",br(),h4("Descripcion del Proyecto"),
             p("El proyecto consiste es............")),                                    
      column(12,tags$img(src="datascience.jpg",width="100%"))                
      
    )
    
  )}