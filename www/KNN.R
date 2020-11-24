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
knn(df,35,70,1)
