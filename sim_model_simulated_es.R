

#################################################

# Modelo SIM estimado usando el paquete "bimets" *

# Último cambio: 23 de julio de 2022 

# Nota: este código replica y estima el modelo SIM desarrollado en "Monetary Monetary"
# Economía: Un enfoque integrado del crédito, el dinero, el ingreso, la producción y la riqueza",
# por Wynne Godley y Marc Lavoie, capítulo 3. Las series de tiempo simuladas se utilizan para estimar
# coeficientes del modelo.   


# * Para obtener información sobre "bimets", consulte:
#   https://cran.r-project.org/web/packages/bimets/vignettes/bimets.pdf
#   https://rstudio-pubs-static.s3.amazonaws.com/513407_05cc338d7e1d47548ebe30c5293f5fc1.html 

#################################################
# PARTE 1
#################################################

#A) PREPARAR EL AMBIENTE

#Clear all
rm(list=ls(all=TRUE))

#Upload datos simulados de Dropbox
#Data <- read.csv( "https://www.dropbox.com/s/ggnhnxqhgfqwnmj/data_sim.csv?dl=1" ) 


#En alternativa, cargue datos desde FRED
#install.packages("fredr") #if fredr no está instalado
library(fredr)
fredr_set_key("7515440bdb26ef51d98764cfbccff0b9")
Data0 <-fredr(
  series_id = "GDP",# Producto Interno Bruto  (https://fred.stlouisfed.org/series/GDP)
  observation_start = as.Date("1960-01-01"),
  observation_end = as.Date("2010-01-01"), 
  frequency = "a", 
  aggregation_method = "avg"
)
Data1 <-fredr(
  series_id = "W068RCQ027SBEA",# Gastos totales del gobierno  (https://fred.stlouisfed.org/series/W068RCQ027SBEA)
  observation_start = as.Date("1960-01-01"),
  observation_end = as.Date("2010-01-01"), 
  frequency = "a", 
  aggregation_method = "avg"
)
Data2 <-fredr(
  series_id = "PCE",# Gastos de Consumo Personal  (https://fred.stlouisfed.org/series/PCE)
  observation_start = as.Date("1960-01-01"),
  observation_end = as.Date("2010-01-01"), 
  frequency = "a", 
  aggregation_method = "avg"
)
Data3 <-fredr(
  series_id = "BOGMBASE",# Base Monetaria; Total (https://fred.stlouisfed.org/series/BOGMBASE)
  observation_start = as.Date("1960-01-01"),
  observation_end = as.Date("2010-01-01"), 
  frequency = "a", 
  aggregation_method = "avg"
)
Data4 <-fredr(
  series_id = "IITTRLB",# Impuesto sobre la Renta Individual de los Estados Unidos: Tasas impositivas para impuestos regulares: Tramo más bajo  (https://fred.stlouisfed.org/series/IITTRLB)
  observation_start = as.Date("1960-01-01"),
  observation_end = as.Date("2010-01-01"), 
  frequency = "a", 
  aggregation_method = "avg"
)
Data5 <-fredr(
  series_id = "W006RC1Q027SBEA",# Recibos de impuestos corrientes del gobierno federal  (https://fred.stlouisfed.org/series/W006RC1Q027SBEA) 
  observation_start = as.Date("1960-01-01"),
  observation_end = as.Date("2010-01-01"), 
  frequency = "a", 
  aggregation_method = "avg"
)
Data6 <-fredr(
  series_id = "A576RC1",#  Remuneración de empleados, recibida: Desembolsos salariales  (https://fred.stlouisfed.org/series/A576RC1)
  observation_start = as.Date("1960-01-01"),
  observation_end = as.Date("2010-01-01"), 
  frequency = "a", 
  aggregation_method = "avg"
)
Data7 <-fredr(
  series_id = "PAYEMS",# Todos los empleados, Total no agrícola  (https://fred.stlouisfed.org/series/PAYEMS)
  observation_start = as.Date("1960-01-01"),
  observation_end = as.Date("2010-01-01"), 
  frequency = "a", 
  aggregation_method = "avg"
)
Data8 <-fredr(
  series_id = "DSPI",# Ingreso Personal Disponible  (https://fred.stlouisfed.org/series/DSPI)
  observation_start = as.Date("1960-01-01"),
  observation_end = as.Date("2010-01-01"), 
  frequency = "a", 
  aggregation_method = "avg"
)
names(Data0)[names(Data0) == 'value'] <- 'Y'
names(Data1)[names(Data1) == 'value'] <- 'G'
names(Data2)[names(Data2) == 'value'] <- 'CO'
names(Data3)[names(Data3) == 'value'] <- 'H'
names(Data4)[names(Data4) == 'value'] <- 'theta' #In puntos porcentuales, debe transformarse
names(Data5)[names(Data5) == 'value'] <- 'TAX'
names(Data6)[names(Data6) == 'value'] <- 'w' #In puntos porcentuales, debe transformarse
names(Data7)[names(Data7) == 'value'] <- 'N'
names(Data8)[names(Data8) == 'value'] <- 'YD'


Data0$Year<-format(Data0$date, "%Y")


Dataa <- cbind.data.frame(Data0,
                          Data1,
                          Data2,
                          Data3,
                          Data4,
                          Data5,
                          Data6,
                          Data7,
                          Data8
)

Data <- subset(Dataa, select = c(Year,Y,G,CO,H,theta,TAX,w,N,YD))

Data$theta <- Data$theta/100 
Data$w <- Data$w/Data$N  

rm("Data0", 
   "Dataa",
   "Data1",
   "Data2",
   "Data3",
   "Data4",
   "Data5",
   "Data6",
   "Data7",
   "Data8")

#Llamar paquete bimets
#install.packages("bimets") #si bimets no está instalado
library(bimets)

#################################################

#B) ESTIMACIÓN BÁSICA

#Crear y describir el modelo SIM
sim_model.txt="MODEL

COMMENT> Función de consumo
BEHAVIORAL> CO
TSRANGE 1962 1 2010 1
EQ> CO =  c1*YD + c2*TSLAG(H_H,1)  
COEFF> c1 c2

COMMENT> Producto
IDENTITY> Y
EQ> Y = CO + G

COMMENT> Empleo
IDENTITY> N
EQ> N = Y/w

COMMENT> Pagos de impuestos
BEHAVIORAL> TAX
TSRANGE 1962 1 2010 1
EQ> TAX = theta*w*N
COEFF> theta

COMMENT> Ingreso disponible
IDENTITY> YD
EQ> YD = w*N - TAX

COMMENT> Oferta de dinero
IDENTITY> H_S
EQ> H_S = TSLAG(H_S,1) + G - TAX

COMMENT> Demanda de dinero
IDENTITY> H_H
EQ> H_H = TSLAG(H_H,1) + YD - CO

END"

#Subir el modelo
sim_model=LOAD_MODEL(modelText = sim_model.txt);

#Performar algunas comprobaciones:
# - Recopilar la información sobre la ecuación de consumo
sim_model$behaviorals$CO
# - Recopilar la información sobre la identidad del producto
sim_model$identities$Y
# - Mostrar la matriz de cadenas ordenadas que contiene los nombres de las variables endógenas que deben calcularse secuencialmente durante cada iteración del algoritmo de simulación
sim_model$vsim
# - Mostrar la matriz de cadenas ordenada que contiene los nombres de las variables pre-recursivas endógenas que deben calcularse (utilizando su definición EQ>) antes de que se lleve a cabo el algoritmo iterativo de simulación;
sim_model$vpre
# - Mostrar la matriz de cadenas ordenadas que contiene los nombres de las variables de retroalimentación endógenas que se evaluarán para la convergencia durante el algoritmo iterativo de simulación;
sim_model$vfeed
# - Mostrar la matriz de cadenas ordenadas que contiene los nombres de las variables post-recursivas endógenas que deben calcularse después de la ejecución y la convergencia del algoritmo iterativo;
sim_model$post

#Inserir valores para modelar variables y coeficientes
sim_modelData=list(  
  Y  = TIMESERIES(c(Data$Y),   
                  START=c(1960,1),FREQ=1),
  G  = TIMESERIES(c(Data$G),   
                  START=c(1960,1),FREQ=1),
  CO  = TIMESERIES(c(Data$CO),   
                   START=c(1960,1),FREQ=1),
  H_H  = TIMESERIES(c(Data$H),   
                    START=c(1960,1),FREQ=1),
  H_S  = TIMESERIES(c(Data$H),   
                    START=c(1960,1),FREQ=1),
  theta  = TIMESERIES(c(Data$theta),   
                      START=c(1960,1),FREQ=1),
  TAX  = TIMESERIES(c(Data$TAX),   
                    START=c(1960,1),FREQ=1),
  w  = TIMESERIES(c(Data$w),   
                  START=c(1960,1),FREQ=1),
  N  = TIMESERIES(c(Data$N),   
                  START=c(1960,1),FREQ=1),
  YD  = TIMESERIES(c(Data$YD),   
                   START=c(1960,1),FREQ=1)
)

#Estimar los datos en el modelo
sim_model=LOAD_MODEL_DATA(sim_model,sim_modelData);

#Estimar coeficientes del modelo
sim_model=ESTIMATE(sim_model)

# Nota: use ESTIMATE(sim_model, eqList=c('CO')) para estimar
# solo coeficientes de consumo


# Nota: valores de coeficiente, residuales y estadísticas
# se almacenan en el objeto modelo

# Nota: la función de estimación incluye: a) OLS; b) Variables instrumentales; 
# c) Restricciones lineales deterministas sobre los coeficientes; d) Polinomio de Almon
# Retrasos distribuidos; e) Autocorrelación de los errores.


#################################################

#C) REALIZAR ALGUNAS COMPROBACIONES BÁSICAS

#Imprimir coeficientes de consumo estimados
sim_model$behaviorals$CO$coefficients

#Imprimir residuales del consumo
sim_model$behaviorals$CO$residuals

#Imprimir una selección de estadísticas de estimación para el consumo:
# - Grados de libertad
sim_model$behaviorals$CO$statistics$DegreesOfFreedom
# - Error estándar
sim_model$behaviorals$CO$statistics$StandardErrorRegression
# - Covarianza
sim_model$behaviorals$CO$statistics$CoeffCovariance

#análisis  de estabilidad estructural:
#performar prueba de Chow (fallo predictivo) en la función de consumo
sim_model_chow <- ESTIMATE(sim_model
                           ,eqList='CO'
                           ,TSRANGE=c(1961,1,2000,1)
                           ,forceTSRANGE = TRUE
                           ,CHOWTEST = TRUE
)


#################################################

#D) UTILIZAR EL MODELO PARA HACER PREDICCIONES

# - Extender variables exógenas (y evaluadas condicionalmente) hasta 2030
sim_model$modelData <- within(sim_model$modelData,{
  G   = TSEXTEND(G,  UPTO=c(2030,1))
  w   = TSEXTEND(w,  UPTO=c(2030,1))
})             

# - Pronóstico
sim_model <- SIMULATE(sim_model
                      ,simType='FORECAST'
                      ,TSRANGE=c(2011,1,2030,1)
                      ,simConvergence=0.00001
                      ,simIterLimit=100)

# - Imprimir pronóstico Y
TABIT(sim_model$simulation$Y)

# - Imprimir CO pronosticado
TABIT(sim_model$simulation$CO)

# - Gráfico Y real y pronosticado 
plot(sim_modelData$Y, type="l",col=1,lwd = 2,lty=1,xlim=range(1960,2030),ylim=range(0,max(sim_model$simulation$Y)+3),font.main=1,cex.main=1,main="a) Out-of-sample forecast: output",ylab='$',cex.axis=1,cex.lab=1)
lines(sim_model$simulation$Y,type="l",col="2",lwd = 2,lty=3)
legend("topleft",c("Actual","Predicted"),bty="n",cex=1,lty=c(1,3),lwd=c(2,2),col=c(1,2),box.lty=0)

# - Traza el CO real y pronosticado
plot(sim_modelData$CO, type="l",col=1,lwd = 2,lty=1,xlim=range(1960,2030),ylim=range(0,max(sim_model$simulation$CO)+3),font.main=1,cex.main=1,main="b) Out-of-sample forecast: consumption",ylab='$',cex.axis=1,cex.lab=1)
lines(sim_model$simulation$CO,type="l",col="3",lwd = 2,lty=3)
legend("topleft",c("Actual","Predicted"),bty="n",cex=1,lty=c(1,3),lwd=c(2,2),col=c(1,3),box.lty=0)

#################################################
# PARTE 2
#################################################

#E) ESTIMACIÓN AVANZADA: UTILICE FACTORES DE ADICIÓN PARA AJUSTAR LAS PREDICCIONES EN LA MUESTRA

#Definir lista de exogenización (es decir, variables que se ajustan)
exogenizeList <- list(
  CO = c(1960,1,2010,1),      
  TAX  = c(1960,1,2010,1))

# Nota: use CO = TRUE para todo TSRANGE  

#Definir lista de factores añadidos
constantAdjList <- list(
  CO = TIMESERIES(START=c(1960,1), FREQ='A'),
  TAX = TIMESERIES(START=c(1960,1), FREQ='A'),
  Y  = TIMESERIES(START=c(1960,1), FREQ='A'))

# Nota: use CO = TIMESERIES(1,2,-1 START=c(1960,1), FREQ='A') para agregar valores
# 1, 2 y -1 a CO(1960), CO(1961) y CO(1962), respectivamente 

#Simular modelo
sim_model <- SIMULATE(sim_model
                      ,simType='FORECAST'
                      ,TSRANGE=c(1961,1,2030,1)
                      ,simConvergence=0.00001
                      ,simIterLimit=100
                      ,Exogenize=exogenizeList
                      ,ConstantAdjustment=constantAdjList
                      ,quietly=TRUE)

#Note: ESTÁTICA, DINÁMICA y otros tipos de simulación se pueden utilizar. Sin embargo, STATIC
# no habilita la previsión de valores fuera de la muestra

#Imprimir Y pronosticado
TABIT(sim_model$simulation$Y)

#Imprimir CO pronosticado
TABIT(sim_model$simulation$CO)

#Gráfico Y real y pronosticado 
plot(sim_modelData$Y, type="l",col=1,lwd = 2,lty=1,xlim=range(1960,2030),ylim=range(0,max(sim_model$simulation$Y)+3),font.main=1,cex.main=1,main="c) Adjusted in-sample prediction + \n out-of-sample forecast: output",ylab='$',cex.axis=1,cex.lab=1)
lines(sim_model$simulation$Y,type="l",col=2,lwd = 2,lty=3)
legend("topleft",c("Actual","Predicted (adjusted)"),bty="n",cex=1,lty=c(1,3),lwd=c(2,2),col=c(1,2),box.lty=0)

#Gráfico CO real y pronosticado 
plot(sim_modelData$CO, type="l",col=1,lwd = 2,lty=1,xlim=range(1960,2030),ylim=range(0,max(sim_model$simulation$CO)+3),font.main=1,cex.main=1,main="d) Adjusted in-sample prediction + \n out-of-sample forecast: consumption",ylab='$',cex.axis=1,cex.lab=1)
lines(sim_model$simulation$CO,type="l",col=3,lwd = 2,lty=3)
legend("topleft",c("Actual","Predicted (adjusted)"),bty="n",cex=1,lty=c(1,3),lwd=c(2,2),col=c(1,3),box.lty=0)

#################################################
# PARTE 3
#################################################

#D) UTILICE EL MODELO PARA CALCULAR LOS MULTIPLICADORES

#Obtener matriz multiplicadora en 2001
sim_model=MULTMATRIX(sim_model,
                     TSRANGE=c(2001,1,2001,1),
                     INSTRUMENT=c('G','w'),
                     TARGET=c('CO','Y'),
                     simIterLimit = 100)

#Mostrar la matriz multiplicadora en 2001
sim_model$MultiplierMatrix

#Obtener multiplicadores intermedios multiperíodo
sim_model <- MULTMATRIX(sim_model,
                        TSRANGE=c(2000,1,2002,1),
                        INSTRUMENT=c('G','w'),
                        TARGET=c('CO','Y'))

#Mostrar la matriz de multiplicadores del producto
sim_model$MultiplierMatrix

######################################

#E) REALIZAR ENDOGENOUS TARGETING

#Create lista de valores arbitrarios para el producto en 2002-2005
sim_modelTargets = list(
  Y=TIMESERIES(15,15.5,16,16.5,17,START=c(2002,1),FREQ=1))

#Simular el nuevo modelo
sim_model=RENORM(sim_model
                 ,INSTRUMENT = c('G')     
                 ,TARGET = sim_modelTargets
                 ,TSRANGE = c(2002,1,2005,1)
                 ,simIterLimit=100)

# Nota: necesita tantos instrumentos como objetivos

#Verificar que se alcancen los objetivos
with(sim_model$simulation,
     TABIT(Y))

#Gráfico variables objetivo simuladas contra series temporales
plot(sim_modelData$Y, type="h",col="plum",lwd = 4,xlim=range(1960,2010),font.main=1,cex.main=1,main="e) Output: actual vs. target",ylab='$',cex.axis=1,cex.lab=1)
mycol <- rgb(0, 255, 255, max = 255, alpha = 50, names = "mylightblue")
rect(xleft=2002,xright=2005,ybottom=0,ytop=25,col=mycol,border=NA)
lines(sim_model$simulation$Y,type="h",col="purple4",lwd = 2)     
legend("topleft",c("Actual","Targets achieved \n in 2002-2005"),bty="n",cex=1,lwd=c(4,2),col=c("plum","purple4"),box.lty=0)

#Gráfico variables objetivo simuladas contra series temporales
plot(sim_modelData$CO, type="h",col="seagreen1",lwd = 4,xlim=range(1960,2010),font.main=1,cex.main=1,main="d) Consumption: actual vs. target",ylab='$',cex.axis=1,cex.lab=1)
mycol <- rgb(0, 255, 255, max = 255, alpha = 50, names = "mylightblue")
rect(xleft=2002,xright=2005,ybottom=0,ytop=25,col=mycol,border=NA)
lines(sim_model$simulation$CO,type="h",col="royalblue4",lwd = 2)     
legend("topleft",c("Actual","Implied levels \n in 2002-2005"),bty="n",cex=1,lwd=c(4,2),col=c("seagreen1","royalblue4"),box.lty=0)

