x<-c(14,7,13,12,16,14,18,13,12,16,13)
y<-c(16,12,13,14,15,12,16,11,13,18,17)

#######
regresion<-function(x,y, nx=NA, ny=NA){
  resultado<-list()
  prediccion<-NA
  n<-NROW(x)
  promX<-mean(x)
  promY<-mean(y)
  dx<-(x-promX)
  dy<-(y-promY)
  xy<-dx*dy
  #Calculo de la covarianza
  cov<-sum(xy)/(n-1)
  cov
  dx2<-dx^2
  dy2<-dy^2

  #Calculo de las desviaciones estandar
  sdX<-sqrt(sum(dx2)/(n-1))
  sdY<-sqrt(sum(dy2)/(n-1))
  sdX
  sdY
  #Calculo del coeficiente correlacional

  r<-cov/(sdX*sdY)
  #creacion de la recta regresional x~y
  if(is.na(ny))
  prediccion<-promY+cov*(nx-promX)/(sdX^2)
  #creando la prediccion regresional y~x
  if(is.na(nx))
  prediccion<-promX+cov*(ny-promY)/(sdY^2)
  

resultado[[1]]<-data.frame(x,y,dx,dy,dx2,dy2)
resultado[[2]]<-cov
resultado[[3]]<-r
resultado[[4]]<-prediccion
return(resultado)
}
regresion(x,y,14)

plot(women)
women
regresion(women$height, women$weight,ny =139)
###########
library(corrplot)
M<-cor(mtcars)
corrplot(M)
cor(mtcars$hp,mtcars$disp)
