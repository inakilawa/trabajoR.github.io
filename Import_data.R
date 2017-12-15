####################PAUTAS####################################
#### Plantear 4 preguntas que se correspondan con lo aprendido
#### 6 Graficos
#### Un contraste de hipotesis
#### 2 analisis numericos
#### 1 nuevo grafico
#############Enviar un .R, un .PPT y los datos#############

rm(list=ls(all=T))

#install.packages("readxl")
#install.packages("ggplot2")
#install.packages("ggmap")
#install.packages("plotrix")
#install.packages("plot3D")
#install.packages("plyr")
#install.packages("modeest")
#install.packages("reshape2")
library(readxl)
library(ggplot2)
library(ggmap)
library(plotrix)
library(plot3D)
library(plyr)
library(modeest)
library(reshape2)

crimedata=read.csv("crimedata_london.csv", header=TRUE)
airbnb=read.csv("airbnb_london.csv", header=TRUE)
unemp=read.csv("unemployment_london2.csv", header=TRUE)


###############################################################################################
#### 1. Comprueba si la integridad de los valores de la tabla. 
#######Cuantos valores faltan en cada columna?
#######Completa de valores una de la columnas con NAs

integrity= data.frame()
for(i in 1:ncol(airbnb)){
  count=0
 for(j in 1:nrow(airbnb)){
   count=count+is.na(airbnb[j,i])
 } 
  integrity[1,i]=count
}
colnames(integrity)=colnames(airbnb)

###############Sustituimos los NA por la media#################
bath_vec=is.na(airbnb$bathrooms)
sum(bath_vec)
bath_mean=mean(airbnb[bath_vec==F,]$bathrooms)
airbnb[bath_vec==T,]$bathrooms=round(bath_mean,0)
airbnb$bathrooms



###############################################################################################
##############2. Realiza un resumen de los ficheros importados en funcion del barrio

########################### Creamos el data frame y los vectores #############################
datasummary<-data.frame(unique(airbnb$borough))
colnames(datasummary)<-c("Borough")
long<-c()
lat<-c()
totflats<-c()
avgprice<-c()
unemp11<-c()
unemp14<-c()
totcrimes<-c()
lyearcrimes<-c()
avgyearcrimes<-c()
mfcrime<-c()
avgvls<-c()
minvls<-c()
maxvls<-c()

unemp$Borough=as.character(unemp$Borough)
flats=data.frame(table(airbnb$borough))
colnames(flats)=c("borough","Freq")

getMostFrecuentCrime<-function(aux2){
  aux2=data.frame(table(aux$major_category))
  max=0
  mfc=""
  for(j in 1:nrow(aux2)){
    if(aux2[j,2]>max){
      mfc=aux2[j,1]
      max=aux2[j,2]
    }
  }
  return(mfc)
}
######################### Generamos y asignamos los vectores ##############################3
for(i in 1:nrow(datasummary)){
  long[i]<-mean(airbnb[airbnb$borough==datasummary$Borough[i],]$long)
  lat[i]<- mean(airbnb[airbnb$borough==datasummary$Borough[i],]$lat)
  totflats[i]<-flats[flats$borough==datasummary$Borough[i],]$Freq
  avgprice[i]<-round(mean(airbnb[airbnb$borough==datasummary$Borough[i],]$price),2)
  unemp11[i]<-mean(unemp[unemp$Borough==datasummary$Borough[i],]$X2011.2013)
  unemp14[i]<-mean(unemp[unemp$Borough==datasummary$Borough[i],]$X2014.2016)
  aux=crimedata[crimedata$borough==datasummary$Borough[i],]
  totcrimes[i]=nrow(aux)
  lyearcrimes[i]=nrow(aux[aux$year==2016,])
  avgyearcrimes[i]=round(nrow(aux)/length(unique(aux$year)),0)
  mfcrime[i]=getMostFrecuentCrime(aux)
  avgvls[i]=round(mean(aux$value, na.rm=TRUE),3)
  minvls[i]=min(aux$value,na.rm = TRUE)
  maxvls[i]=max(aux$value, na.rm = TRUE)
}

datasummary$long<-long
datasummary$lat<-lat
datasummary$flats<-totflats
datasummary$avgprice<-avgprice
datasummary$unemp11<-unemp11
datasummary$unemp14<-unemp14
datasummary$totalcrimes<-totcrimes
datasummary$lastyearcrimes<-lyearcrimes
datasummary$averageproyearcrimes<-avgyearcrimes
datasummary$mostfreccrime<-mfcrime
datasummary$averagevalue<-avgvls
datasummary$minvalue<-minvls
datasummary$maxvalue<-maxvls

colnames(datasummary)<-c("Borough","Longitude","Latitude","Total Flats", "Price Average", "Unemployment 2011", "Unemployment 2014",
                         "Total Crimes","Last Year Crimes", "Avg/Year", "MFCrime", "Avg Value", "Min Value", "Max Value")

head(datasummary,10)


###############################################################################################
#### 3. Hay valores atipicos en el número de habitaciones? 
###¿Qué medidas estadisticas son adecuadas en este caso?
###Calcula la media/mediana y la desviación tipica/rango intercuartilico.

table(airbnb$bedrooms)
ggplot(airbnb, aes(x=bedrooms,fill=borough)) + geom_bar(position="stack")

#No parece haber valores atipicos. Por tanto, vamos a calcular la media de habitaciones y la desviación tipica:
mean(airbnb$bedrooms,na.rm=TRUE)
median(airbnb$bedrooms,na.rm=TRUE)
sd(airbnb$bedrooms,na.rm=TRUE)


###################################################################################3
#### 4. Hay relación entre el número de habitaciones y el precio? ¿Depende el precio del barrio?

#RESPUESTAS:
#En primer lugar realizaremos ANOVA sobre las variables habitaciones y precio (ya que hay más de 
#dos subpoblaciones:
?aov
aov(airbnb$bedrooms,airbnb$price)
ggplot(airbnb, aes(x = bedrooms, y = price, color=borough)) + geom_point()

#En segundo lugar, analicemos los barrios:
#Crearemos 2 funciones para chequear la normalidad y la varianza
check_norm<- function(a){
  b<-shapiro.test(a)
  return(b$p.value>0.05)
}

check_var<- function(a,b){
  require(ggplot2)
  c<-data.frame(a,b)
  d<-bartlett.test(c)
  return(d$p.value>0.05)
} 

croydon<-airbnb[airbnb$borough=="Croydon",]
check_norm(croydon$price)

#shapiro.test(croydon$price)
#Como los precios no siguen una distribución normal tenemos que utilizar el test de Krusal-Wallis:
kruskal.test(borough~price,airbnb)
#Y nos indica que NO hay relación entre el precio y el barrio


####  5. ¿Existe relación entre el barrio y la cantidad de habitaciones? 
#¿Existe relación entre el precio medio del alquiler en un barrio y la cantidad de cr??menes en el mismo?

#RESPUESTAS:
#En primer lugar miramos qué barrios hay, para realizar el Shapiro Test individualmente. Todos deben seguir
#una distribución normal en sus habitaciones. Si no, deberemos usar Krusal-Wallis.
check_norm(croydon$bedrooms)
#shapiro.test(croydon$bedrooms)

#No sigue una distribución normal. Por tanto tenemos que utilizar el test de Kruskal-Wallis.
kruskal.test(borough~bedrooms,airbnb)

#PODEMOS ASEGURAR QUE NO EXISTE RELACIÓN ENTRE EL BARRIO Y LA CANTIDAD DE HABITACIONES

#Veamos si existe relación entre el precio medio del alquiler y la cantidad de cr??menes en los barrios:
#Asumimos que son muestras independientes de dos subpoblaciones, por tanto realizaremos el test
#de la T de Student. Justificamos la respuesta también con un gráfico:

t.test(datasummary$`Total Crimes`,datasummary$avgprice)

#Para hacer el gráfico tenemos que unir en un nuevo dataframe todos los datos
ej3_ds<-data.frame(datasummary$Borough,datasummary$`Total Crimes`,datasummary$`Avg Value`)
ggplot(ej3_ds, aes(x = `Total Crimes`, y = avgprice, color=Borough)) + geom_point()




###############################################################################
#### 6. Representa en el mapa los barrios en función del precio y el numero de pisos

lon=mean(airbnb$longitude)
lati=mean(airbnb$latitude)

mapLon = get_map(location = c(lon,lati), zoom = 11, maptype="roadmap")
mapPoints <- ggmap(mapLon) +
  geom_point(aes(x = long, y = lat, colour= avgprice, size = sqrt(totflats)) , data = datasummary)
mapPoints  


###################################################################################
#### 7. Representa en el mapa del barrio Leyton los pisos en funcion del numero de habitaciones y el precio

neigh = "Leyton"
a_neigh=airbnb[airbnb$neighborhood==neigh,]
map_b=qmap(neigh, zoom = 15)
map_b+                                        
  geom_point(aes(x = longitude, y = latitude, colour = bedrooms, size=price), data = a_neigh, na.rm = T)

map_b+                                        
  geom_point(aes(x = longitude, y = latitude, colour = price), data = a_neigh, na.rm = T) +
  facet_grid(~bedrooms)


####################################################################################
#### 8. Haz un mapa de densidades del mapa de Londres en funcion del numero de pisos disponibles

london_den <- get_map(location = "London", zoom = 12, maptype = "roadmap") 
londonMap<-ggmap(london_den)

londonMap +
  stat_density2d(aes(x = longitude, y = latitude, fill = ..level..,alpha=..level..), bins = 10, geom = "polygon", data = airbnb) +
  scale_fill_gradient(low = "black", high = "red")+
  ggtitle("Map of density in London")



################################################################################
#### 9. Haz un grafico de tartas de....

slice_data <- data.frame(table(airbnb$overall_satisfaction))
colnames(slice_data)=c("Type", "Frequency")
data_3d
pie3D(data_3d[,2],labels=row.names(data_3d),explode=0.1,
      main="Pie Chart of Room Type", fill= factor(colnames(data_3d)))


#################################################################################
#### 10. Haz un histograma 3D


data_3d<-data.frame(table(airbnb$room_type, airbnb$overall_satisfaction))
colnames(data_3d)=c("Type", "Valoration", "Frequency")
data_3d<-acast(data_3d,Valoration~Type)
#Quito la columna que no tiene tipo
data_3d<-data_3d[,c(4,3,2)]
#Junto los que son menores que 3.5
data_3d_vec=as.numeric(rownames(data_3d))<=3.5
data_3d_aux=c()

for (i in 1:ncol(data_3d)){
  data_3d_aux[i]=sum(data_3d[data_3d_vec,i])
}
#data_3d=data_3d[data_3d_vec==F,]
data_3d=rbind(data_3d_aux,data_3d[data_3d_vec==F,])
rownames(data_3d)=c(3.5,4,4.5,5)
colnames(data_3d)=c("Shared","Private","Entire")
hist3D (x = 1:nrow(data_3d), y = 1:ncol(data_3d), z = data_3d,
        bty = "g", phi = 20,  theta = -60, scale = T, 
        xlab = "Valoration", ylab = "Type", zlab = "Frequency", main = "Valoration VS Type",
        breaks=c(0,1000,2000,3000,4000,5000,6000), border = "white", shade = 0.3,
        ticktype = "detailed", space = 0.15, d = 2, cex.axis = 1e-9, opaque.top = F)

#col = jet.col(7, alpha = 0.2)

# Use text3D to label x axis
text3D(x = 1:nrow(data_3d), y = rep(0.5, nrow(data_3d)), z = rep(3, 4),
       labels = rownames(data_3d),
       add = T, adj = 0)

text3D(x = rep(1, ncol(data_3d)),   y = 1:ncol(data_3d), z = rep(0, ncol(data_3d)),
       labels  = colnames(data_3d), phi = 20, theta = 20, bty="g", width=2,  col.axis = "red",
       add = TRUE, adj = 1)


###################### CONTRASTE DE HIPOTESIS ##########################

is_relation<- function(a,b){
  if(!check_norm(a)&!check_norm(b)){
    n=TRUE
    if(check_var(a,b)){
      v=TRUE
    }
    else{
      v=TRUE
    }
  }
  else{
    n=FALSE
    v=FALSE
  }
  return(c(n,v))
}


is_hipotesis<-function(a,b,indep){
  i=is_relation(a,b)
  if (i[1]&i[2]){
    if (indep){
      d=t.test(a,b)
    }
    else{
      d=t.test(a,b)
    }
  }
  
  else{
    if (indep){
      d=t.test(a,b)
    }
    else{
      d=t.test(a,b)
    }
  }
  return(d$p.value<0.05)
}