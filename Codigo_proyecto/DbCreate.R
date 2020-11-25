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
dbExecute(conexion, sprintf("ALTER TABLE pliego ADD PRIMARY KEY(c_pliego)"))
dbExecute(conexion, sprintf("ALTER TABLE ejecutora ADD PRIMARY KEY(c_ejecutora)"))
dbExecute(conexion, sprintf("ALTER TABLE proveedor ADD PRIMARY KEY(c_proveedor)"))
dbExecute(conexion, sprintf("ALTER TABLE grupo ADD PRIMARY KEY(c_grupo)"))
dbExecute(conexion, sprintf("ALTER TABLE clase ADD PRIMARY KEY(c_clase)"))
dbExecute(conexion, sprintf("ALTER TABLE familia ADD PRIMARY KEY(c_familia)"))
dbExecute(conexion, sprintf("ALTER TABLE item ADD PRIMARY KEY(c_item)"))
dbExecute(conexion, sprintf("ALTER TABLE cantidad ADD PRIMARY KEY(c_cantidad)"))
dbExecute(conexion, sprintf("ALTER TABLE precio_unitario ADD PRIMARY KEY(c_preciou)"))
dbExecute(conexion, sprintf("ALTER TABLE total ADD PRIMARY KEY(c_valortotal)"))
dbExecute(conexion, sprintf("ALTER TABLE ejecutora ADD FOREIGN KEY(c_pliego) REFERENCES pliego(c_pliego)"))
dbExecute(conexion, sprintf("ALTER TABLE proveedor ADD FOREIGN KEY(c_pliego) REFERENCES pliego(c_pliego)"))
dbExecute(conexion, sprintf("ALTER TABLE marca ADD FOREIGN KEY(c_grupo) REFERENCES pliego(c_grupo)"))
dbExecute(conexion, sprintf("ALTER TABLE familia ADD FOREIGN KEY(c_grupo) REFERENCES pliego(c_grupo)"))
dbExecute(conexion, sprintf("ALTER TABLE clase ADD FOREIGN KEY(c_grupo) REFERENCES pliego(c_grupo)"))
dbExecute(conexion, sprintf("ALTER TABLE item ADD FOREIGN KEY(c_grupo) REFERENCES pliego(c_grupo)"))
dbExecute(conexion, sprintf("ALTER TABLE cantidad ADD FOREIGN KEY(c_item) REFERENCES pliego(c_item)"))
dbExecute(conexion, sprintf("ALTER TABLE precio_unitario ADD FOREIGN KEY(c_cantidad) REFERENCES pliego(c_cantidad)"))
dbExecute(conexion, sprintf("ALTER TABLE total ADD FOREIGN KEY(c_precio) REFERENCES pliego(c_precio)"))