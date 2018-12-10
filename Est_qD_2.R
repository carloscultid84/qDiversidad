#####Código para estimación y comparación de la diversidad qD#########
###Se incluye una breve descripción de cada paso, de esta forma será posible replicar los resultados que se presentan en el capítulo#######

install.packages("iNEXT")
library(iNEXT)
library(ggplot2)
library(gridExtra)

data=read.csv("qData.csv", header = T, sep = ",") # se cargan los datos

sd(data$Bn)


DataInfo(data, datatype="abundance") ## Se obtiene una tabla resumen, la cual muestra, para cada sitio o hábitat el número de individuos, la riqueza observada (Sobs), la cobertura de muestreo (SC), la cantidad de singletons (f1), de doubletons (f2) y de las otras especies raras desde f3 hasta f10:

#   site n    S.obs    SC f1 f2 f3 f4 f5 f6 f7 f8 f9 f10
#1   Bn 710    25 0.9944  4  2  0  1  1  1  2  3  1   1
#2   Rs  12     5 0.7643  3  1  0  0  0  0  1  0  0   0
#3   Pa  22     6 0.8678  3  1  0  0  0  1  0  0  0   0

###Se creará un objeto "out" el cual contiene las estimaciones del paquete iNEXT ().
out <- iNEXT(data, q=c(0, 1, 2), datatype="abundance", endpoint=710, nboot=1000, knots = 120)

summary(out)
###el objeto out es una lista y contiene tres componentes:
#         Length Class      Mode
#DataInfo 14     data.frame list
#iNextEst  3     -none-     list
#AsyEst    7     data.frame list

#Para la comparación usaremos iNextEst:

out_qdata <- out$iNextEst
write.table(out_qdata, "out_qdata.txt") #### Asi se guarda la tabla de salida que contendrá las estimaciones de qD por cobertura de muestreo. Para minipular esta tabla se recomienda abrirla y guardarla como un Excel. Cuando se tenga la tabla en formato excel, seleccionar la celda de la primera columna y fila, y dar Crtl + "+", saldrá una ventana, dar clic en la opción de mover la información hacia la derecha- De esta forma los nombres de las columnas quedaran en su lugar correcto. La primera columna no debe tener nombre. 

summary(out_qdata)##Esta tabla es una lista con tres data.frame, una por cada cobertura vegetal. Cada tabla tiene nueve columnas:

#   Length Class      Mode
#Bn 9      data.frame list
#Rs 9      data.frame list
#Pa 9      data.frame list

dim(out_qdata$Bn) #360 filas x 9 columnas
dim(out_qdata$Rs) #360 filas x 9 columnas
dim(out_qdata$Pa) #360 filas x 9 columnas

names(out_qdata$Bn) # estos son los nombres de las columnas
#"m" = Número de individuos por knots (paquete de individuos donde se estima la diversidad)
#"method" = interpolate, observed and Extrapolated
#"order"  = exponente o valor q
#"qD" = valor de diversidad estimada por cada "m" y "order"     
#"qD.LCL" "qD.UCL" "SC" "SC".LCL" "SC.UCL"

##Aplicación del protocolo: Fig. 3.2
###construcción de curvas de rarefacción. Para esto usaremos la función ggiNEXT. Esto produce la figura 3.2:

ggiNEXT(out, type = 1, se = TRUE, facet.var = "order", color.var = "site", grey = FALSE)

ggiNEXT(out, type = 3, se = TRUE, facet.var = "order", color.var = "site", grey = FALSE)

############Código para construir las figuras que muestran las comparaciones bajo cuatro escenarios de cobertura de muestreo###################

escdb <- read.csv("escenarios.csv", header = T, sep=",") ###cargamos la tabla de los escenarios de coberturas de muestreo con base a la tabla "out_qdata.txt" guardada arriba.

names(escdb) #para corroborar los nombres de las columnas

#con lo siguiente crearemos cuatro tablas diferentes, una por escenario.

escdb_1 <- subset(escdb, Set == "s1"); escdb_1$Cober <- factor(escdb_1$Cober, levels = c("Bn", "Rs", "Pa"))
escdb_2 <- subset(escdb, Set == "s2"); escdb_2$Cober <- factor(escdb_2$Cober, levels = c("Bn", "Rs", "Pa"))
escdb_3 <- subset(escdb, Set == "s3"); escdb_3$Cober <- factor(escdb_3$Cober, levels = c("Bn", "Rs", "Pa"))
escdb_4 <- subset(escdb, Set == "s4"); escdb_4$Cober <- factor(escdb_4$Cober, levels = c("Bn", "Rs", "Pa"))

###Código para crear las figuras de 3.3 - 3.5, escenarios que se resumen en la tabla 3.3

#####Figuras de diversidad (q = 0,1,2) escenario 1
D0.1 <-  ggplot(escdb_1, aes(x=Cober, y=X0D))+ geom_point()+
  geom_errorbar(aes(ymin=X0D-ir0, ymax=X0D+ur0), width=.2, position=position_dodge(0.05))+
  theme(axis.line = element_line(size=0.5, colour = "black"), panel.background = element_rect(fill = "white"))+
  xlab("")+
  ylab("Riqueza (0D)")+
  theme(text=element_text(size=12, family = "TT Times New Roman"))
D1.1 <- ggplot(escdb_1, aes(x=Cober, y=X1D))+ geom_point()+
  geom_errorbar(aes(ymin=X1D-ir1, ymax=X1D+ur1), width=.2, position=position_dodge(0.05))+
  theme(axis.line = element_line(size=0.5, colour = "black"), panel.background = element_rect(fill = "white"))+
  xlab("")+ylab("No. efectivo de especies (1D)")+theme(text=element_text(size=12, family = "TT Times New Roman"))
D2.1 <- ggplot(escdb_1, aes(x=Cober, y=X2D))+ geom_point()+
  theme(axis.line = element_line(size=0.5, colour = "black"), panel.background = element_rect(fill = "white"))+
  xlab("")+ylab("No. efectivo de especies (2D)")+theme(text=element_text(size=12, family = "TT Times New Roman"))+
  geom_errorbar(aes(ymin=X2D-ir2, ymax=X2D+ur2), width=.2, position=position_dodge(0.05))

#####Figuras de diversidad (q = 0,1,2) escenario 2
D0.2 <- ggplot(escdb_2, aes(x=Cober, y=X0D))+geom_point()+
  geom_errorbar(aes(ymin=X0D-ir0, ymax=X0D+ur0), width=.2, position=position_dodge(0.05))+
  theme(axis.line = element_line(size=0.5, colour = "black"), panel.background = element_rect(fill = "white"))+
  xlab("")+ylab("")+theme(text=element_text(size=12, family = "TT Times New Roman"))
D1.2 <- ggplot(escdb_2, aes(x=Cober, y=X1D))+geom_point()+
  geom_errorbar(aes(ymin=X1D-ir1, ymax=X1D+ur1), width=.2, position=position_dodge(0.05))+
  theme(axis.line = element_line(size=0.5, colour = "black"), panel.background = element_rect(fill = "white"))+
  xlab("")+ylab("")+theme(text=element_text(size=12, family = "TT Times New Roman"))
D2.2 <- ggplot(escdb_2, aes(x=Cober, y=X2D))+geom_point()+
  geom_errorbar(aes(ymin=X2D-ir2, ymax=X2D+ur2), width=.2, position=position_dodge(0.05))+
  theme(axis.line = element_line(size=0.5, colour = "black"), panel.background = element_rect(fill = "white"))+
  xlab("")+ylab("")+theme(text=element_text(size=12, family = "TT Times New Roman"))

#####Figuras de diversidad (q = 0,1,2) escenario 3
D0.3 <- ggplot(escdb_3, aes(x=Cober, y=X0D))+geom_point()+
  geom_errorbar(aes(ymin=X0D-ir0, ymax=X0D+ur0), width=.2, position=position_dodge(0.05))+
  theme(axis.line = element_line(size=0.5, colour = "black"), panel.background = element_rect(fill = "white"))+
  xlab("Cobertura")+ylab("Riqueza (0D)")+theme(text=element_text(size=12, family = "TT Times New Roman"))
D1.3 <-  ggplot(escdb_3, aes(x=Cober, y=X1D))+geom_point()+
  geom_errorbar(aes(ymin=X1D-ir1, ymax=X1D+ur1), width=.2, position=position_dodge(0.05))+
  theme(axis.line = element_line(size=0.5, colour = "black"), panel.background = element_rect(fill = "white"))+
  xlab("Cobertura")+ylab("No. efectivo de especies (1D)")+theme(text=element_text(size=12, family = "TT Times New Roman"))
D2.3 <- ggplot(escdb_3, aes(x=Cober, y=X2D))+ geom_point()+
  geom_errorbar(aes(ymin=X2D-ir2, ymax=X2D+ur2), width=.2, position=position_dodge(0.05))+
  theme(axis.line = element_line(size=0.5, colour = "black"), panel.background = element_rect(fill = "white"))+
  xlab("Cobertura")+ylab("No. efectivo de especies (2D)")+theme(text=element_text(size=12, family = "TT Times New Roman"))

#####Figuras de diversidad (q = 0,1,2) escenario 4
D0.4 <- ggplot(escdb_4, aes(x=Cober, y=X0D))+ geom_point()+
  geom_errorbar(aes(ymin=X0D-ir0, ymax=X0D+ur0), width=.2, position=position_dodge(0.05))+
  theme(axis.line = element_line(size=0.5, colour = "black"), panel.background = element_rect(fill = "white"))+
  xlab("Cobertura")+ylab("")+theme(text=element_text(size=12, family = "TT Times New Roman"))
D1.4 <- ggplot(escdb_4, aes(x=Cober, y=X1D))+ geom_point()+
  geom_errorbar(aes(ymin=X1D-ir1, ymax=X1D+ur1), width=.2, position=position_dodge(0.05))+
  theme(axis.line = element_line(size=0.5, colour = "black"), panel.background = element_rect(fill = "white"))+
  xlab("Cobertura")+ylab("")+theme(text=element_text(size=12, family = "TT Times New Roman"))
D2.4 <- ggplot(escdb_4, aes(x=Cober, y=X2D))+ geom_point()+
  geom_errorbar(aes(ymin=X2D-ir2, ymax=X2D+ur2), width=.2, position=position_dodge(0.05))+
  theme(axis.line = element_line(size=0.5, colour = "black"), panel.background = element_rect(fill = "white"))+
  xlab("Cobertura")+ylab("")+theme(text=element_text(size=12, family = "TT Times New Roman"))

###Con el siguiente código se construirán los mosaicos que corresponde a las figuras 3.3 - 3.5

gridplotD0 <- grid.arrange(D0.1, D0.2, D0.3, D0.4,nrow = 2, ncol = 2) #Fig3.3
gridplotD1 <- grid.arrange(D1.1, D1.2, D1.3, D1.4,nrow = 2, ncol = 2) #Fig3.4
gridplotD2 <- grid.arrange(D2.1, D2.2, D2.3, D2.4,nrow = 2, ncol = 2) #Fig3.5

####################################################################################
######################Magnitude la diferencia#######################################
####################################################################################

#Cargar tabla "MD.csv" que permitirá construir la Fig. 3.6

dMD <- read.csv("MD.csv", header = T, sep=",")

##Para ordenar la información.
dMD$Diversidad <- factor(dMD$Diversidad, levels = c("2D", "1D", "0D"))
dMD$Cobertura <- factor(dMD$Cobertura, levels = c("Rs-Pa", "Bn-Rs", "Bn-Pa"))

ggplot(dMD, aes(x=Cobertura,y=X.MD,fill=Diversidad)) +
  geom_bar(position="dodge",stat="identity")+
  coord_flip()+
  scale_fill_grey(breaks = c("0D","1D", "2D"))+
  scale_y_continuous(limits = c(-100, 100), breaks = seq(-100,100,50))+
  xlab("")+ylab("%MD")+
  theme(text=element_text(size=20, family = "TT Times New Roman", color = "black"))+ 
  theme(legend.position = c(0.18, 0.15))+
  theme(panel.background = element_blank())+
  theme(panel.border = element_rect(size = 2, color = "black", linetype = "solid", fill = NA), fill = NULL)+
  geom_hline(yintercept = 0, lty = 1, colour = "black", size = 1)


