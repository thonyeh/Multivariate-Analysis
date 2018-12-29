##### --------------------------------------------------------------------
##### TRABAJO GRUPAL - PREG. 3 - ANALISIS DE DATOS
#####
##### BASE DE DATOS: CARACTERISTICAS BIOMECANICAS DE PACIENTES ORTOPEDICOS
##### --------------------------------------------------------------------

library(cluster)
library(fpc)
library(NbClust)
library(clValid)
library(dendextend)
library(circlize)
library(sparcl)

## LECTURA DE DATOS
datos <- read.csv("C:/Users/Anthony/Documents/PUCP/2017 - 2/TÉCNICAS DE ANÁLISIS MULTIVARIADO/Laboratorio/Lista 2/ortopedico.csv")
head(datos)
sapply(datos,class)

attach(datos)

## ADICION DE GRUPOS ORIGINALES
dim.class=c(summary(class))
datos$clase.grupo=c(rep(1,dim.class[1]),rep(2,dim.class[2]))

dim.sub=c(summary(sub_class))
datos$subclase.grupo=c(rep(1,dim.sub[1]),rep(3,dim.sub[3]),rep(2,dim.sub[2]))

colnames(datos) = c("incid.pelvica","incl.pelvica","angulo.lumbar","pend.sacral",
                    "rad.pelvico","grado.spond","clase","subclase",
                    colnames(datos[,9:10])) 

head(datos)

## ANALISIS EXPLOTARIO
dim(datos)
summary(datos[,1:7])

par(mfrow=c(2,3))
for (i in 1:6) {
  boxplot(datos[,i]~datos$clase.grupo, main = names(datos[i]), type="l")
}

par(mfrow=c(1,1))
plot(datos$clase.grupo,ylab= "Grupos originales",xlab = "observaciones")

## DATOS + GRUPO CLASS
datos1 <- datos[,1:6]

par(mfrow=c(1,1))
fviz_cluster(list(data = datos1,cluster=datos[,9]),
             ellipse.type="norm",ellipse.level=0.9,
             geom = "point",
             main = "Agrupamiento original al 90%")

##### ----------------------------------------------------------
##### PRIMERA METODOLOGIA: CLUSTERING
##### ----------------------------------------------------------

###----------- PASO 1. DEFINICION DE FUNCIONES (ALTERNATIVO)

# 1: kmeans, 2: PAM, 3: clara, 4: fanny
metodo <- function(datos1,h,nm){
  set.seed(2010)
  if(nm == 2){ pam(scale(datos1),h)
  }else{if(nm == 3){ clara(scale(datos1),h)
  }else{if(nm == 4){ fanny(scale(datos1),h,maxit=5000)
  }else{ kmeans(scale(datos1),h,nstart = 100)}}}
}

# SUMA DE CUADRADOS DENTRO DE CADA CLUSTER
SC.cluster <- function(datos1, nm){
  asw<-numeric()
  if(nm ==1){for(h in 2:10){asw[h-1]=metodo(datos1,h,nm)$tot.withinss}
    plot(2:10,asw,type="b")
  }else{for(h in 2:10){asw[h-1]=metodo(datos1,h,nm)$silinfo$avg.width}
    plot(2:10,asw,type="b",xlab="k",ylab="ASW")
  }
}

# SILUETA
silueta <- function(datos1, nm){
  diss.datos=daisy(scale(datos1))
  par(mfrow=c(1,3))
  if(nm ==1){for(h in 2:4){
    plot(silhouette(metodo(datos1,h,nm)$cluster,diss.datos))}
  }else{for(h in 2:4){plot(metodo(datos1,h,nm),which.plots=2)}}
}

# CRITERIO DE CALINSKI - HARABASZ
Cal.Har <- function(datos1, nm){
  ch<-numeric()
  par(mfrow=c(1,1))
  if(nm ==1){for(h in 2:10){
    ch[h-1] = calinhara(scale(datos1),metodo(datos1,h,nm)$cluster)}
  }else{for(h in 2:10){
    ch[h-1] = calinhara(scale(datos1),metodo(datos1,h,nm)$clustering)}}
  plot(2:10,ch,type="b",xlab="k", ylab="Criterio de Calinski-Harabasz")
}

# GRAFICAS CLUSTER
plot.cluster <- function(datos1,clus){
  par(mfrow=c(1,1))
  fviz_cluster(list(data = datos1,cluster=clus),
               ellipse.type="norm",ellipse.level=0.9,
               geom = "point",
               main = "Gráfico de Conglomerados al 90%")
}

###------------ PASO 2. DETERMINANDO NUMERO DE CONGLOMERADOS
res.cluster=list()

## KMEANS
SC.cluster(datos1,1)

silueta(datos1,1)
kmeansruns(scale(datos1),criterion="asw")

Cal.Har(datos1,1)
kmeansruns(scale(datos1),criterion="ch")

set.seed(2000)
res.cluster[[1]]=kmeans(scale(datos1),2)$cluster

par(mfrow=c(1,1))
plot.cluster(datos1,res.cluster[[1]])

## PAM
SC.cluster(datos1,2)

silueta(datos1,2)
pamk(scale(datos1),criterion="asw")

Cal.Har(datos1,2)
pamk(scale(datos1),criterion="ch")

set.seed(2000)
res.cluster[[2]]=pam(scale(datos1),2)$clustering

par(mfrow=c(1,1))
plot.cluster(datos1,res.cluster[[2]])

## CLARA
SC.cluster(datos1,3)

silueta(datos1,3)
pamk(scale(datos1),criterion="asw",usepam=FALSE)

Cal.Har(datos1,3)
pamk(scale(datos1),criterion="ch",usepam=FALSE)

set.seed(2000)
res.cluster[[3]]=clara(scale(datos1),2)$clustering
for (i in 1:dim(datos)[1]) {
  if(res.cluster[[3]][i] == 1){
    res.cluster[[3]][i]=2
  }else{res.cluster[[3]][i] = 1
  }
}

par(mfrow=c(1,1))
plot.cluster(datos1,res.cluster[[3]])

## FANNY
SC.cluster(datos1,4)
silueta(datos1,4)
Cal.Har(datos1,4)

set.seed(2000)
res.cluster[[4]]=fanny(scale(datos1),2)$clustering
for (i in 1:dim(datos)[1]) {
  if(res.cluster[[4]][i] == 1){
    res.cluster[[4]][i]=2
  }else{res.cluster[[4]][i] = 1
  }
}

par(mfrow=c(1,1))
plot.cluster(datos1,res.cluster[[4]])

#table(datos[,9],res$cluster)

par(mfrow=c(2,2))
plot(res.cluster[[1]],xlab = "observaciones",ylab = "kmeans")
plot(res.cluster[[2]],xlab = "observaciones",ylab = "PAM")
plot(res.cluster[[3]],xlab = "observaciones",ylab = "clara")
plot(res.cluster[[4]],xlab = "observaciones",ylab = "fanny")

## CLUSTER JERARQUICO: HIERARCHICAL
set.seed(2000)
hc <- hclust(dist(scale(datos1)),method="ward.D")
res.cluster[[5]] = cutree(hc , 2)

dend <- as.dendrogram(hc) %>% 
  color_branches(k=2) %>% 
  color_labels

par(mar = rep(0,4))
circlize_dendrogram(dend, labels_track_height = 0.3,
                    dend_track_height = .6,
                    main = "Dendongrama")

par(mfrow=c(1,1))
ColorDendrogram(hc, y = res.cluster[[5]],
                labels = names(res.cluster[[5]]),
                main = "Dendograma",
                xlab= "observaciones",
                branchlength = 0.4)

par(mfrow=c(1,1))
plot.cluster(datos1,res.cluster[[5]])

## CLUSTER JERARQUICO: AGNES
agnes.single=agnes(scale(datos1),method="single")
agnes.single$ac
par(mfrow=c(1,1))
pltree(agnes.single,cex=1,hang=-1, main = "Dendograma Agnes")

agnes.ward=agnes(scale(datos1),method="ward")
agnes.ward$ac
par(mfrow=c(1,1))
pltree(agnes.ward,cex=1,hang=-1, main = "Dendograma Agnes")

diss.datos=daisy(scale(datos1))

agnes.ward=as.hclust(agnes.ward)
par(mfrow=c(1,3))
for(h in 2:4){
  res=cutree(agnes.ward,k=h)
  plot(silhouette(res,diss.datos))
}

set.seed(2000)
res.cluster[[6]]=cutree(agnes.ward,k=2)

par(mfrow=c(1,1))
plot.cluster(datos1,res.cluster[[6]])

## DIANA
diana.metodo=diana(scale(datos1))

par(mfrow=c(1,1))
pltree(diana.metodo,cex=1,hang=-1, main = "Dendograma Diana")

diana.metodo=as.hclust(diana.metodo)
par(mfrow=c(1,3))
for(h in 2:4){
  res=cutree(diana.metodo,k=h+1)
  plot(silhouette(res,diss.datos))
}

set.seed(2000)
res.cluster[[7]]=cutree(diana.metodo,k=3)
for (i in 1:dim(datos)[1]) {
  if(res.cluster[[7]][i] == 1){
    res.cluster[[7]][i]=2
  }else{res.cluster[[7]][i] = 1
  }
}

par(mfrow=c(1,1))
plot.cluster(datos1,res.cluster[[7]])

par(mfrow=c(1,2))
plot(res.cluster[[5]],xlab = "observaciones",ylab = "Hierarchical")
plot(res.cluster[[6]],xlab = "observaciones",ylab = "Agnes: Wald")

par(mfrow=c(1,1))
plot(res.cluster[[7]],xlab = "observaciones",ylab = "Diana")

###--------- PASO 3. PERFILADO Y CARACTERIZACION DE CLUSTERS

## ADICION DE CLUSTER A LA BASE
datos.new <- datos[,c(1:6,9)]
for (i in 1:7) {
  datos.new <- cbind(datos.new,res.cluster[[i]])
}

colnames(datos.new)<-c(colnames(datos.new[,1:7]),"cluster.km","cluster.PAM",
                       "cluster.clara","cluster.fanny","cluster.h",
                       "cluster.agnes","cluster.diana")
head(datos.new)

# TABLA DE MEDIAS
med=list()
for (i in 1:7) {
  med[[i]] = aggregate(x = datos.new[,1:6],
                       by = list(datos.new[,i+7]),
                       FUN = mean)
}
med[[1]]

# ANALISIS EXPLORATIVO POR CLUSTER
for (j in 1:7) {
  par(mfrow=c(2,3))
  for (i in 1:6) {
    boxplot(datos.new[,i]~datos.new[,7+j], main=names(datos.new[i]), type="l")
  }
}

## OTRAS TECNICAS
clmethods <- c("hierarchical","kmeans","pam","agnes","diana")

# Medidas de validación interna 
intern <- clValid(scale(datos1), nClust = 2:10,
                  clMethods = clmethods, validation = "internal")
summary(intern) 

par(mfrow=c(1,1))
plot(intern)

# Medidas de estabilidad
stab <- clValid(scale(datos1), nClust = 2:10, clMethods = clmethods,
                validation = "stability")
summary(stab)

# Mostrar solo scores óptimos
optimalScores(intern)
optimalScores(stab)