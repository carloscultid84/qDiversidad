########Código para construir los escenarios de rarefacción###############
#####################Exploración realizada a partir de la tabla 3.1######

install.packages("wiqid")
library(wiqid)
install.packages("HDInterval")
library(HDInterval)
library(ggplot2)
install.packages("rareNMtests")
library(rareNMtests)
library("reshape2")

###Escenarios de cv y rarefacción, todos los escenarios tienen la misma abundancia total (n = 450)

esc1 <- rep(15, 30)
esc2 <- c(rep(1,21), 18,	19,	20,	21,	22,	rep(35,3), 224)
esc3 <- c(rep(1,27), 40, 40, 343)

#Datos de Muestreo

cm1 <- c(rep(1, 7), rep(2, 8), rep(3, 6), 4, 5, 5, 6, 6) 
cm2 <- c(rep(1, 5), rep(2, 3), 3, rep(4, 3), 5) 
cm3 <- c(rep(1, 4), rep(2, 2), 3, rep(4, 2), 5)

###########generación de curvas de rarefacción comunidades focales, 

resc1 <-rarefaction.individual(esc1, method = "sample-size", q = 0, powerfun = 1, 
                               log.scale = FALSE, inds = NULL)
resc2 <-rarefaction.individual(esc2, method = "sample-size", q = 0, powerfun = 1, 
                               log.scale = FALSE, inds = NULL)
resc3 <-rarefaction.individual(esc3, method = "sample-size", q = 0, powerfun = 1, 
                               log.scale = FALSE, inds = NULL)

round((cv.esc1 <- (sd(esc1)/mean(esc1))),1)
round((cv.esc2 <- (sd(esc2)/mean(esc2))),1)
round((cv.esc3 <- (sd(esc3)/mean(esc3))),1)

t.esc <- cbind(resc1, resc2$`Hill (q=0)`, resc3$`Hill (q=0)`)
colnames(t.esc) <- c("sample_size", "c1", "c2", "c3")
names(t.esc); t.esc[1:5, ]; rt.esc <- round(t.esc, 2); rt.esc[1:5, ]


###########Curvas de rarefacción de las muestras
mesc1 <-rarefaction.individual(cm1, method = "sample-size", q = 0, powerfun = 1, 
                               log.scale = FALSE, inds = NULL)
mesc2 <-rarefaction.individual(cm2, method = "sample-size", q = 0, powerfun = 1, 
                               log.scale = FALSE, inds = NULL)
mesc3 <-rarefaction.individual(cm3, method = "sample-size", q = 0, powerfun = 1, 
                               log.scale = FALSE, inds = NULL)

round(cVm1 <- (sd(cm1)/mean(cm1)), 1) 
round(cVm2 <- (sd(cm2)/mean(cm2)), 1) #
round(cVm3 <- (sd(cm3)/mean(cm3)), 1) #

###Con la siguiente función se puede cambiar el f1 y f2 con respecto a n para examinar como cambia Cn. Tener precaución de que la abundancia en f1 y f2 no sea mayor a n. 

fCn <- function(n, f1, f2){
 cn <- 1-(f1/n)*(((n-1)*f1)/((n-1)*f1+(2*f2))) 
cn
 }

n=sum(esc3)
f1=sum(esc3 == 1)
f2=sum(esc3 == 2)

round(fCn(n,f1,f2),2)

#cm_rcm1 = 0.90
#cm_rcm2 = 0.84
#cm_rcm3 = 0.84

#Construcción de tabla para crear la figura 3.1. Fuente de la función: https://stat.ethz.ch/pipermail/r-help/2004-October/059752.html

add.col<-function(df, new.col) {n.row<-dim(df)[1]
length(new.col)<-n.row
cbind(df, new.col)}

###aplicando la función
tra.1 <- add.col(rt.esc, mesc1$`Hill (q=0)`) 
tra.2 <- add.col(tra.1, mesc2$`Hill (q=0)`)
tra.3 <- add.col(tra.2, mesc3$`Hill (q=0)`) ###tabla final

colnames(tra.3) <- c("sample_size", "c1", "c2", "c3", "mC1", "mC2", "mC3") ###Colocando los nombres necesarios a las columnas.
names(tra.3) #revisar los nombres de las columnas

#p.f1 <- c(10, 20, 30, 40, 50, 60, 70, 80)
#cn.f1 <- c(0.90, 0.80, 0.70, 0.60, 0.50, 0.40, 0.30, 0.20)
#cn.f2 <- c(1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00)
#cn.f1.f2.1 <- c(0.90, 0.80, 0.70, 0.60, 0.51, 0.41)##f2 = a un tercio de singletons
#cn.f1.f2.2 <- c(0.91, 0.81, 0.71, 0.61, 0.51, 0.41)##f2 = a la mitad de singletons
#cn.f1.f2.2 <- c(0.90, 0.81, 0.71, 0.61, 0.52)###f2 = f1 - 1, en este caso, tenemos en realidad 31 f1, es el caso en el que la comunidad solo tiene f1 y f2.

table_outR <- melt(tra.3, id ="sample_size") ###Reirganizamos la tabla, esta será la tabla que se usará para construir la Fig.3.1

table_outR[1:10, ]
round(tra.3, 2)

############Figura 3.1

lt  <- ggplot(data=table_outR,
       aes(x=sample_size, y=value)) +
  geom_line()

lt + facet_grid(variable ~.)+
  theme_classic()+
  labs(title="", x="Individuos", y = "Riqueza")+
  geom_hline(yintercept = 30, lty = 2, colour = "grey")+
  #geom_hline(yintercept = 0, colour = "black")+
  #theme(legend.position="none")
  theme(text=element_text(size=10, family = "TT Times New Roman"))


