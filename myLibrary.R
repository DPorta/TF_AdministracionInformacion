#Creación de algoritmos a utilizar en el modelado

#########################################
###REGRESION
#################################
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
  return(resultado[[1]])
}



#############################################
#KNN
######################################
x<-sample(30:60,50, replace = T)
y<-sample(60:100, 50, replace = T)
df<-data.frame(x,y)
plot(df)

###Categorizando
etiquetar<-function(df){
  categorias<-c()
  for(i in 1:NROW(df)){
    if(df$x[i]>=30 & df$x[i]<40)
      categorias<-c(categorias, 'A')
    else if(df$x[i]>=40 & df$x[i]<50)
      categorias<-c(categorias, 'B')
    else 
      categorias<-c(categorias, 'C')
  }
  df<-cbind(df,categorias) 
  return (df)
}
df=etiquetar(df)

###Visaulizando el df
library(ggplot2)
ggplot(data = df, aes(x=df$x, y = df$y, color=df$categorias))+geom_point()+xlab("X")+ylab("Y")+ggtitle("Clasificador KNN")

###Datos para entrenamiento
ids=sample(1:nrow(df),0.85*nrow(df))
dfEnt<-df[ids,]
nrow(dfEnt)
dfTest<-df[-ids,]
nrow(dfTest)
ggplot(data =dfTest, aes(x=x, y =y, color=categorias))+geom_point()+xlab("X")+ylab("Y")+ggtitle("Clasificador KNN")

dfTemp=df
knn<-function(dfTemp,newx,newy,k){
  #To-do probar para k-vecinos, obtener las probabilidades
  #jaccard, malahanobis, euclidiana
  newx<-35#para prueba
  newy<-70
  pos<-0
  dm<-(abs(newx-dfTemp$x)+abs(newy-dfTemp$y))
  dfTemp<-cbind(dfTemp,dm)
  dfTemp
  ##forma lenta
  #minimo<-min(dm)
  #for(i in 1:nrow(dfTemp)){
  #  if(minimo==dfTemp[i,4])
  #    p<-i
  #}
  #dfTemp[p,3]
  ##Forma rapida
  return(dfTemp[dfTemp[,4]==min(dm),3])
  
}







######################################################
#SVM
#codigo aca
###################################################

###########################################
#ALGO
#codigo aca
####################################