# TRABAJO FINAL

##### INTEGRANTES
###### Gómez Lozano, Aldo Jhair
###### Porta Montes, David Obhed


### Introducción
El presente trabajo ha tenido como objetivo la creación de una plataforma en el que se podrán desarrollar proyectos de ciencias de datos. Todo lo implementado se ha hecho en base a los conocimientos adquiridos en el curso de Administración de la Información. Asimismo, hemos utilizado el lenguaje de programación R y html, además de herramientas de software como Rstudio y Vertabello (Para el diagrama de Entidad Relación). Por otra parte, se ha usado la plataforma AWS en la que hemos conectado instancias EC2 con nuestro proyecto. El proyecto realizado consta de 5 partes. 



### 1.Recolección 

En esta sección, se han cargado las fuentes de información (Datasets), hemos escogido 2 fuentes. La primera trata sobre donaciones para afectados por Covid-19 y la segunda que es la data de la madres gestantes del departamento de Huancavelica. 

### 2.Preprocesamiento

En esta sección, se muestran los Datasets cargados anteriormente. A comparación del punto anterior, para este punto hemos limpiado los Datasets, es decir se han realizado la eliminación de valores anómalos y vacíos. Asimismo, se han eliminado algunas columnas que no eran relevantes para la realización del proyecto

### 3.Consultas de Exploración

En esta sección, se han realizado los querys en base a los datasets cargados, se ha utilizado en su mayoría la librería `dplyr`

    Ejemplo de Consulta
    dplyr1<- gestantes%>%filter(Distrito=="TINTAY"&Edad<25)

### 4.Gráficos

En esta sección, se han realizado gráficos donde se busca representar los querys realizados en el punto anterior, hemos utilizado la librería  `ggplot2`

    Ejemplo de Gráfico
      gr1<- ggplot(dplyr1_A, aes(x="",y=total, fill=Distrito))+
           geom_bar(stat="identity") + coord_polar("y", start=0) +
      theme_void()+labs(title="Distribucion de gestantes por distritos")

### 5.Modelos

En esta sección, se han realizado 4 modelos de algoritmos. El primero es el modelo de regresión lineal, el segundo es el algoritmo KNN, el tercero viene a ser el K-Means y el último el modelo de regresión polinomial

### 6.Conclusiones

En conclusión, ha sido gratificante realizar este proyecto debido a que hemos plasmado todos los conocimientos aprendidos en el curso. Por otra parte, la herramienta shiny es muy favorable al momento de realizar este tipo de trabajos, debido a lo completa que es. 