#CODIGO DE CREACION DE DATA BASE SOBRE DONACIONES
##############################################################

pliego<-donaciones[!duplicated(donaciones$PLIEGO_NOMBRE),]
pliego<-pliego%>%select(PLIEGO_NOMBRE)
dbWriteTable(conexion,name = "pliego",value = pliego)

ejecutora<-donaciones[!duplicated(donaciones$EJECUTORA_NOMBRE),]
ejecutora<-ejecutora%>%select(EJECUTORA_NOMBRE)
dbWriteTable(conexion,name = "ejecutora",value = ejecutora)

proveedor<-donaciones[!duplicated(donaciones$NOMBRE_PROVEEDOR),]
proveedor<-proveedor%>%select(NOMBRE_PROVEEDOR)
dbWriteTable(conexion,name = "proveedor",value = proveedor)

fecha<-donaciones%>%select(FECHA_REG)
dbWriteTable(conexion,name = "fecha",value = fecha)

grupo<-donaciones[!duplicated(donaciones$NOMBRE_GRUPO),]
grupo<-grupo%>%select(NOMBRE_GRUPO)
dbWriteTable(conexion,name = "grupo",value = grupo)

clase<-donaciones[!duplicated(donaciones$NOMBRE_CLASE),]
clase<-clase%>%select(NOMBRE_CLASE)
dbWriteTable(conexion,name = "clase",value = clase)

familia<-donaciones[!duplicated(donaciones$NOMBRE_FAMILIA),]
familia<-familia%>%select(NOMBRE_FAMILIA)
dbWriteTable(conexion,name = "familia",value = familia)

item<-donaciones[!duplicated(donaciones$NOMBRE_ITEM),]
item<-item%>%select(NOMBRE_ITEM)
dbWriteTable(conexion,name = "item",value = item)

cantidad<-donaciones%>%select(CANT_ARTICULO)
dbWriteTable(conexion,name = "cantidad",value = cantidad)

precio_unitario<-donaciones%>%select(PRECIO_UNIT)
dbWriteTable(conexion,name = "precio_unitario",value = precio_unitario)

total<-donaciones%>%select(VALOR_TOTAL)
dbWriteTable(conexion,name = "total",value = total)

#IMPLEMENTACION DE LAS RELACIONES
######################################################

