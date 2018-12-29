### Pregunta 1 ###

rm(list = ls())

## Pregunta 1a ##                        

library(foreign)
library(cluster)
library(fpc)

departamentos=read.spss(file.choose(),
                        use.value.labels=TRUE, max.value.labels=Inf, to.data.frame=TRUE)

colnames(departamentos) <- tolower(colnames(departamentos))
nombres=departamentos[,1]
departamentos=departamentos[,-1]
rownames(departamentos)=nombres
head(departamentos)


## K means

## Determinar número de conglomerados 

# Criterio 1: Suma de cuadrados dentro de clusters

# Se crea este vector wss para ir guardando la suma de cuadrados dentro del cluster

wss<-numeric() 
for(h in 2:10){
  b<-kmeans(scale(departamentos),h,nstart = 20)
  wss[h-1]<-b$tot.withinss
}
plot(2:10,wss,type="b")


# Criterio 2: Silueta

diss.departamentos=daisy(scale(departamentos))
par(mfrow=c(3,3))
for(h in 2:10){
  res=kmeans(scale(departamentos),h)
  plot(silhouette(res$cluster,diss.departamentos))
}
par(mfrow=c(1,1))

kmeansruns(scale(departamentos),criterion="asw")

# Criterio 3: de Calinski-Harabasz

ch<-numeric()
for(h in 2:10){
  res<-kmeans(scale(departamentos),h, nstart = 20)
  ch[h-1]<-calinhara(scale(departamentos),res$cluster)
}
plot(2:10,ch,type="b",xlab="k",
     ylab="Criterio de Calinski-Harabasz")

kmeansruns(scale(departamentos),criterion="ch")


# Criterio 4 y 5: Medidas de Validación Interna (Conectividad y Dunn)

library(clValid)
clmethods <- c("kmeans") 

intern <- clValid(scale(departamentos), nClust = 2:10, 
                  clMethods = clmethods, validation = "internal", neighbSize=5)

summary(intern)
plot(intern)
optimalScores(intern)


# Gráfico

reskm=kmeans(scale(departamentos),2)
plotcluster(departamentos,reskm$cluster)

clusplot(departamentos,reskm$cluster, color = TRUE,
         shade = TRUE, labels =2,lines=0,
         main ="Gráfico de Conglomerados")


## Método PAM


# Criterio 1: Suma de cuadrados dentro de cluster

asw<-numeric()
for(h in 2:10){
  res<-pam(scale(departamentos),h)
  asw[h-1]<-res$silinfo$avg.width
}
plot(2:10,asw,type="b",xlab="k",ylab="ASW")


# Criterio 2: Silueta

par(mfrow=c(3,3))
for(h in 2:10){
  res=pam(scale(departamentos),h)
  plot(res,which.plots=2)
}

pamk(scale(departamentos),criterion="asw")


# Criterio 3: de Calinski-Harabasz

par(mfrow=c(1,1))
ch<-numeric()
for(h in 2:10){
  res<-pam(scale(departamentos),h)
  ch[h-1]<-calinhara(scale(departamentos),res$clustering)
}
plot(2:10,ch,type="b",xlab="k",
     ylab="Criterio de Calinski-Harabasz")

pamk(scale(departamentos),criterion="ch")


# Criterio 4: Medidas de Validación Interna (Conectividad y Dunn)

library(clValid)
clmethods <- c("pam") 

intern <- clValid(scale(departamentos), nClust = 2:10, 
                  clMethods = clmethods, validation = "internal", neighbSize=5)

summary(intern)
plot(intern)
optimalScores(intern)


# Gráfico

respam=pam(scale(departamentos),2)
plotcluster(departamentos,respam$clustering)
clusplot(departamentos,respam$clustering, color = TRUE,
         shade = TRUE, labels =2,lines=0,
         main ="Gráfico de Conglomerados")



## Pregunta 1b ##

#Clustering jerarquico aglomerativo usando Agnes

library(cluster)

# Usando Enlace promedio:

res=agnes(scale(departamentos),method="average")
res
plot(res)

# Usando Enlace de Ward:

res=agnes(scale(departamentos),method="ward")
res
plot(res)

# Usando Enlace Simple:

res=agnes(scale(departamentos),method="single")
res
plot(res)

# Usando Enlace Completo:

res=agnes(scale(departamentos),method="complete")
res
plot(res)

# Usando Enlace Ponderado:

res=agnes(scale(departamentos),method="weighted")
res
plot(res)

# Usando Enlace promedio generalizado:

res=agnes(scale(departamentos),method="gaverage")
res
plot(res)


# Obtener número de conglomerados

# Criterio 1: Silueta:

diss.departamentos=daisy(scale(departamentos))
res=agnes(scale(departamentos),method="ward")

par(mfrow=c(3,3))
for(h in 2:10){
  conglomerados=cutree(res,h)
  plot(silhouette(conglomerados,diss.departamentos))
}

# Criterio 2: Calinski-Harabasz

diss.departamentos=daisy(scale(departamentos))
res=agnes(scale(departamentos),method="ward")

ch<-numeric()
for(h in 2:10){
  conglomerados=cutree(res,h)
  ch<-c(ch,calinhara(diss.departamentos,conglomerados))
}
plot(2:10,ch,type="b",xlab="k",
     ylab="Criterio de Calinski-Harabasz")


# Criterio 3: Medidas de Validación Interna

library(clValid)
clmethods <- c("agnes") 

intern <- clValid(scale(departamentos), nClust = 2:10, 
                  clMethods = clmethods, validation = "internal", neighbSize=5)

summary(intern)
plot(intern)
optimalScores(intern)

# Criterio 4: Medidas de estabilidad 

stab <- clValid(scale(departamentos), nClust = 2:10, clMethods = clmethods,
                validation = "stability")
summary(stab)

# Gráfico

res_ag<-agnes(scale(departamentos),method="ward")
conglomerados_ag<-cutree(res_ag,2)
plotcluster(departamentos,conglomerados_ag)
clusplot(departamentos,conglomerados_ag, color = TRUE,  shade = TRUE, labels =2,lines=0,
         main ="Gráfico de Conglomerados AGNES")


## Pregunta 1c ##

res=diana(scale(departamentos))
res
plot(res)

# Criterio 2: Silueta

diss.departamentos=daisy(scale(departamentos))
res=diana(scale(departamentos))
par(mfrow=c(3,3))
for(h in 2:10){
  conglomerados=cutree(res,h)
  plot(silhouette(conglomerados,diss.departamentos))
}

# Criterio 3: Calinski-Harabasz

diss.departamentos=daisy(scale(departamentos))
res=diana(scale(departamentos))

ch<-numeric()
for(h in 2:10){
  conglomerados=cutree(res,h)
  ch<-c(ch,calinhara(diss.departamentos,conglomerados))
}
plot(2:10,ch,type="b",xlab="k",
     ylab="Criterio de Calinski-Harabasz")


# Criterio 4 y 5: Medidas de Validación Interna

library(clValid)
clmethods <- c("diana") 

intern <- clValid(scale(departamentos), nClust = 2:10, 
                  clMethods = clmethods, validation = "internal", neighbSize=5)

summary(intern)
plot(intern)
optimalScores(intern)

# Gráfico

res_di<-diana(scale(departamentos))
conglomerados_di<-cutree(res_di,2)
plotcluster(departamentos,conglomerados_di)
clusplot(departamentos,conglomerados_di, color = TRUE,  shade = TRUE, labels =2,lines=0,
         main ="Gráfico de Conglomerados DIANA")


## Pregunta 1d ##


library(clValid)
clmethods <- c("kmeans","pam","agnes","diana")

# Medidas de validación interna 
intern <- clValid(scale(departamentos), nClust = 2:10,
                  clMethods = clmethods, validation = "internal") 
summary(intern)
plot(intern)

# Medidas de estabilidad (q tan estable es cuando se saca 1 columna cada vez

stab <- clValid(scale(departamentos), nClust = 2:10, clMethods = clmethods,
                validation = "stability")
summary(stab)


#  Perfilado y caracterización de clusters

# Adicionar los cluster a la base de datos
departamentos.new<-cbind(departamentos,reskm$cluster)
colnames(departamentos.new)<-c(colnames(departamentos.new[,-length(departamentos.new)]), "clusterkm")
head(departamentos.new)

departamentos.new<-cbind(departamentos.new,respam$cluster)
colnames(departamentos.new)<-c(colnames(departamentos.new[,-length(departamentos.new)]), "clusterpam")
head(departamentos.new)

departamentos.new<-cbind(departamentos.new,conglomerados_di)
colnames(departamentos.new)<-c(colnames(departamentos.new[,-length(departamentos.new)]), "clusterdiana")
head(departamentos.new)

departamentos.new<-cbind(departamentos.new,conglomerados_ag)
colnames(departamentos.new)<-c(colnames(departamentos.new[,-length(departamentos.new)]), "clusteragnes")
head(departamentos.new)

departamentos.new=data.frame(departamentos.new)
class(departamentos.new)


# Tabla de medias
medkm<-aggregate(x = departamentos.new[,1:9],by = list(departamentos.new$clusterkm),FUN = mean)
medkm
medpam<-aggregate(x = departamentos.new[,1:9],by = list(departamentos.new$clusterpam),FUN = mean)
medpam
med_ag<-aggregate(x = departamentos.new[,1:9],by = list(departamentos.new$clusteragnes),FUN = mean)
med_ag
med_di<-aggregate(x = departamentos.new[,1:9],by = list(departamentos.new$clusterdiana),FUN = mean)
med_di
                

# Describir variables

par(mfrow=c(3,3))
for (i in 1:length(departamentos.new[,1:9])) { 
  boxplot(departamentos.new[,i]~departamentos.new$clusterkm, main=names(departamentos.new[i]), type="l") 
}

par(mfrow=c(1,1))
par(mfrow=c(3,3))
for (i in 1:length(departamentos.new[,1:9])) { 
  boxplot(departamentos.new[,i]~departamentos.new$clusterpam, main=names(departamentos.new[i]), type="l") 
}

par(mfrow=c(1,1))
par(mfrow=c(3,3))
for (i in 1:length(departamentos.new[,1:9])) { 
  boxplot(departamentos.new[,i]~departamentos.new$clusterag, main=names(departamentos.new[i]), type="l") 
}


par(mfrow=c(1,1))
par(mfrow=c(3,3))
for (i in 1:length(departamentos.new[,1:9])) {
  boxplot(departamentos.new[,i]~departamentos.new$clusterdiana, main=names(departamentos.new[i]), type="l") 
} 

par(mfrow=c(1,1))




               
              

