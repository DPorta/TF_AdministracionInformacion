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
###Categorizando
etiquetar<-function(df){
  categorias<-c()
  for(i in 1:NROW(df)){
    if(df$xT[i]>=0 & df$xT[i]<18)
      categorias<-c(categorias, 'A')
    else if(df$xT[i]>=18 & df$xT[i]<30)
      categorias<-c(categorias, 'B')
    else 
      categorias<-c(categorias, 'C')
  }
  df<-cbind(df,categorias) 
  return (df)
}

knn<-function(dfTemp,newx,newy){
  dm<-(abs(newx-dfTemp$xT)+abs(newy-dfTemp$yT))
  dfTemp<-cbind(dfTemp,dm)
  holi<-dfTemp[dfTemp[,4]==min(dm),3]
  return(holi[1])
}

######################################################
#KMEANS
obtenerKpuntos<-function(df, k){
  ids<-sample(x = 1:NROW(df),k)
  return (df[ids,])
}
euclidiana<-function(pA,pB) {
  return (sqrt((pA$x-pB$x)^2+(pA$y-pB$y)^2))
}
calcularDistancias<-function(df,puntos){
  dtemp<-df
  for(i in 1:NROW(puntos))
    dtemp[,i+NCOL(df)]<-euclidiana(df,puntos[i,])
  return (dtemp) 
}
obtenerGrupos<-function(m){
  matriz<-apply(m,1,min)==m
  grupos<-rep(-1,NROW(m))
  for(i in 1:NCOL(matriz))
    grupos[matriz[,i]]=i
  return (grupos)
}
###################################################

###########################################
#REGRESION MULTIVARIABLE
#Funcion utilizada ---> lm()
####################################