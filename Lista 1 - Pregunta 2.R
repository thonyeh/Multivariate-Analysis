### Cargar base de datos: "DepartamentosPeru.sav"

library(foreign)
departamentos = read.spss("DepartamentosPeru.sav",
                use.value.labels=TRUE, max.value.labels=Inf, to.data.frame=TRUE)
colnames(departamentos) = tolower(colnames(departamentos))
nombres = departamentos[,1]
departamentos = departamentos[,-1]
rownames(departamentos) = nombres
head(departamentos)

### Matriz y gr�fico de correlaciones
library(psych)
cor.plot(cor(departamentos))
R = round(cor(departamentos),3)

### Prueba de Esfericidad de Barlett
describe(departamentos)
cortest.bartlett(R,nrow(departamentos))

### Prueba de KMO
library(rela)
descri = paf(as.matrix(departamentos))
descri$KMO

### Matriz de correlaci�n Anti-imagen
round(descri$Anti.Image.Cor,3)

### Medidas individuales de Adecuaci�n Muestral
t(round(descri$MSA,3))

### Selecci�n del n�mero de Factores
scree(departamentos)
fa.parallel(departamentos, fm="ml", fa="fa")

### Comunalidades
factanal.none = factanal(departamentos, factors=2, rotation="none")
comunal = 1 - factanal.none$uniquenesses
comunal

### Cargas Factoriales con rotaci�n Varimax
factanal.vari = factanal(departamentos, factors=2, rotatio="varimax")
factanal.none$loadings
factanal.vari$loadings

### Gr�fica de Factores
load = factanal.vari$loadings[,1:2]
plot(load, ylim=c(-0.25,1), xlim=c(-0.25,1))
abline(h=0, v=0, lty=3)
text(load, labels=colnames(departamentos), cex=1.2, pos=1)
X11()
fa.vari = fa(departamentos, nfactors=2, rotate="varimax")
fa.diagram(fa.vari, e.size=0.1)

### Gr�fico de Puntuaciones y Biplot
punt = factanal(departamentos, factors=2, rotation="varimax", scores="regression")
plot(punt$scores)
abline(h=0, v=0, lty=3)
text(punt$scores, labels=nombres, cex=0.9, pos=4, col=departamentos.new$clusteragnes)
X11()
biplot(fa.vari, labels=rownames(departamentos))
