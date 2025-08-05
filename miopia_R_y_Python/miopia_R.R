#las siguientes lineas son para limpieza
rm(list=ls())
gc()
#install.packages("readxl")
library(readxl)
library(ggplot2)
library(gridExtra)
library(e1071)
#install.packages("caret")
datos0 <- read_excel("chicos25.xlsx")
#si el archivo no esta en la ruta debo usar toda la direccion;C/user..../H/..
#------------------------------------TIPO DE VARIABLES-----------------------------------------------------
str(datos0)
# Verifica la clase de cada columna en 'datos'
sapply(datos0, class)
# Conversion de variables categóricas y binarias a factores No es necesaria, todas son variables numericas

colSums(is.na(datos0))

datos <- na.omit(datos0)
#elimino datos faltantes y se observa que mantiene la misma cantidad de muestras---NO hay NA

attach(datos) #me paro en este df y evito usar a futuro $

#sporthr y tvhr son variables numéricas que indican la cantidad de horas dedicadas a una actividad al mes
#mio y ma son variables categoricas (si,no) ---- defionir como factores
#detach(datos) # si el atach da problemas asi que lo quito o REINICIO R Y borro TODO

datos$ma <- ifelse(datos$ma == "Si", 1, 0)
datos$ma<-as.factor(datos$ma)
str(datos) # verifico por ultima vez las variables a usar 
datos
datos$mio<-ifelse(datos$mio == "Si", 1, 0)
datos$mio<-as.factor(datos$mio)
summary(datos)
str(datos) # verifico por ultima vez las variables a usar 
datos
# en str me aparece 2 y 1  PERO en datos lo pasa correctamente a 0 y 1
#----------------------------------GRAFICOS UNIVARIADOS---------------------------------------------------
# Codigo a utilizar para ver HISTOGRAMA para cada var NUMERICAS
hist(al,
     main = "al",
     xlab = "valores", col = "lightblue", border = "red")

range(al)# rango de valores de al

h <- hist(al, plot = FALSE)  # Guarda el histograma sin graficar
h$counts  # Muestra las frecuencias (cuántas observaciones hay en cada intervalo)

hist(sporthr[sporthr <= 500],
     main = "sporthr",
     xlab = "valores",
     col = "lightblue",
     border = "red",
     xlim = c(0, 100),
     breaks = seq(0, 500, by = 10)) 

hist(acd[acd <= 5],
     main = "acd",
     xlab = "valores",
     col = "lightblue",
     border = "red",
     xlim = c(2.5, 5),
     breaks = seq(2.5, 5, by = 0.5))



# Usamos el valor máximo de las frecuencias calculadas para solucionar problema de escala en eje Y
max_freq <- max(h$counts)
# Graficamos el histograma con el límite del eje Y ajustado
hist(al,
     main = "al",
     xlab = "valores", 
     col = "lightblue", 
     border = "red",
     ylim = c(0, max_freq))  # Se deberia justar automáticamente el eje Y al valor máximo de frecuencia
# no funciona, asi que modifico los break:
hist(al,
     main = "al",
     xlab = "valores", 
     col = "lightblue", 
     border = "red", 
     breaks = 20)  # Ajusta el número de intervalos para obtener un histograma más adecuado

#proceder de misma forma con los variables restanters NUMERICAS usando breaks o usar grid en forma automatica.

# a fin de hacerlo automatico dibujo usando grid
# Selecciono solo las columnas numéricas que serian todas en mi caso
numeric_columns <- datos[sapply(datos, is.numeric)]
# Creo una lista para almacenar los gráficos
plots <- list()
# Creo un gráfico de barras para cada columna numérica
for (col in colnames(numeric_columns)) {
  # Obtener los datos de la columna
  data <- numeric_columns[[col]]
  
  # Convertir los datos en un dataframe para ggplot
  df <- data.frame(value = data)
  
  # Crear el histograma con ggplot2
  p <- ggplot(df, aes(x = value)) +
    geom_histogram(binwidth = 1, fill = "lightblue", color = "green", alpha = 0.7) +
    labs(title = col, x = col, y = "Frecuencia") +
    theme_minimal()
  
  # Mostrar el gráfico
  print(p)
}

# para variables categoricas: mio y ma--------------------------------------------

ggplot(datos, aes(x = ma)) +
  geom_bar(fill = "lightblue", color = "red") +
  labs(title = "Gráfico de Barras de 'ma'", x = "Categorías", y = "Frecuencia") +
  theme_minimal()

ggplot(datos, aes(x = mio)) +
  geom_bar(fill = "lightblue", color = "red") +
  labs(title = "Gráfico de Barras de 'mio'", x = "Categorías", y = "Frecuencia") +
  ylim(0, max(table(mio)*1.2)) +  # Ajuste del límite del eje Y al max *1,2 para mejorar escala
  theme_minimal()

#bigotes------------------------
#---bigotes
boxplot(tvhr, las=1, col="darkorchid1", #horizontal=TRUE,
        ylab="grc")
for (col in colnames(numeric_columns)) {
  # Obtener los datos de la columna
  data <- numeric_columns[[col]]
  
  # Crear el boxplot con base R
  boxplot(data, 
          las = 1,  # Rotar las etiquetas del eje Y
          col = "darkorchid1",  # Color de las cajas
          ylab = col,  # Etiqueta del eje Y
          main = col)  # Título del gráfico
  
  # Pausa de 1 segundo (opcional)
  Sys.sleep(1)
}
summary(datos)
str(datos)
datos

#VER
#si spheq da valores negativos indica miopia y si es positivo, hipermetriopia. 0 es normal.  
# El único valor que destaca como problemático es el de sporthr, donde 450 horas por mes es un valor extremadamente alto. Este es un valor que probablemente necesite revisión o corrección!!
# Encontrar el índice de la fila con el valor mínimo de 'grc'
indice_min <- which.min(datos$sporthr)
# Imprimir el índice y la fila completa
print(indice_min)

# Ordenar el dataframe por la columna 'grc' de menor a mayor, y eliminare los 1 son menores a 2
# Mantengo los índices originales antes de ordenar
datos$indice_original <- rownames(datos)

# Ordene el dataframe por la columna 'grc' de menor a mayor
df_ordenado <- datos[order(datos$sporthr), ] #uso una ", " y dejo vacio para considerar todas las columnas

# Mostrar el dataframe ordenado con la columna de índices originales
print(df_ordenado)

#borro, y considero todas las columnas con ", ".
datos <- datos[-c(618), ]

#borro columna de indice-original utilizando select de dplyr
library(dplyr) #para facilitar la manipulación y transformación de dato
colnames(datos)
#datos <- datos %>% select(-indice_original) trato de borrar ultima columna fallido
datos <- subset(datos, select = -indice_original) #borro ultima columna



# df el  sin la columna 'indice_original'
print(datos)
summary(datos)
#########################################
#ANALISIS BIVARIADO 
# Carg0 las librerías necesarias
library(corrplot)   # Para visualizar la matriz de correlación
library(tidyverse)   # Para manipulación de datos
# Verificar los nombres de las columnas
colnames(datos)


# Seleccion de variables cuantitativa
# Eliminar espacios en los nombres de las columnas
colnames(datos) <- gsub(" ", "_", colnames(datos))

# Verificar nuevamente los nombres de las columnas
colnames(datos)

# Verificar el tipo de las columnas
sapply(datos, class)
# Cargar la librería dplyr
library(dplyr)

# Seleccionar solo las variables cuantitativas
datos_quant <- select(datos, al, acd, vcd, sporthr, tvhr)

# Calcular la matriz de correlación
cor_matrix <- cor(datos_quant)

# Visualizar la matriz de correlación
corrplot(cor_matrix, method = "circle", type = "lower", tl.col = "red", 
         addCoef.col = "black", number.cex = 0.8)


#
# gráfico de dispersión entre 'al' y 'vcd'----------------------------------
plot(al, vcd, 
     xlab = "al", 
     ylab = "vcd", 
     main = "Relación entre al y vcd",
     col = "blue", 
     pch = 19)  # pch = 19 es para puntos sólidos
# genero línea de regresión al gráfico de dispersión
abline(lm(vcd ~ al, data = datos), col = "red", lwd = 2)

# Cre0 el modelo de regresión lineal
modelo <- lm(vcd ~ al, data = datos)
#  resumen del modelo
summary(modelo)
# valores coeficientes
coef(modelo)

# Creo un gráfico de dispersión entre 'al' y 'acd'--------------------------
plot(al, acd, 
     xlab = "al", 
     ylab = "acd", 
     main = "Relación entre al y acd",
     col = "blue", 
     pch = 19)  # pch = 19 es para puntos sólidos
# línea de regresión al gráfico de dispersión
abline(lm(acd ~ al, data = datos), col = "red", lwd = 2)

#  de regresión lineal
modelo <- lm(acd ~ al, data = datos)
# resumen del modelo
summary(modelo)
#  coeficientes
coef(modelo)

# Creo un gráfico de dispersión entre 'vcd' y 'acd'--------------------------
plot(acd, vcd, 
     xlab = "acd", 
     ylab = "vcd", 
     main = "Relación entre al y acd",
     col = "blue", 
     pch = 19)  # pch = 19 es para puntos sólidos
# línea de regresión al gráfico de dispersión
abline(lm(vcd ~ acd, data = datos), col = "red", lwd = 2)

#  de regresión lineal
modelo <- lm(vcd ~ acd, data = datos)
# resumen del modelo
summary(modelo)
#  coeficientes
coef(modelo)


#--------------------------------------------------PUNTO 2 ------------------------------------

str(datos) # 
summary(datos)
str(datos) # verifico por ultima vez las variables a usar 
datos
datos <- datos[, -1] #BORRO 1ER COLUMNA-ID
set.seed(666) #DEFINO SEMILLA ALEATORIA
#attach(datos)
# Cargar el paquete
library(caret)
train<- createDataPartition(mio, p=0.7, list=FALSE) #con list false resultado se devuelve como vector de índice de fila
train #muestras para entrenamiento - son 433 filas/muestras en el vector
nrow(datos) #Nro de muestras de datos
dataTrain <- datos[train, ]
dataTest <- datos[-train, ] #lo restante (30%) para prueba
datos
summary(datos)
dataTest

#--------------------------------------------------PUNTO 3 ------------------------------------
modeloCompleto <- glm(formula = mio ~., data = dataTrain, family = "binomial")
#Error, ya que variable mio contiene 0 y 1 y NO: si o no.
# Convertir 'mio' a 0 (No) y 1 (Yes) - REPETIR CON ma tambien !!!!!!!!!!!!!!!!!!
#dataTrain$mio <- ifelse(dataTrain$mio == "Yes", 1, 0)
#dataTrain$ma <- ifelse(dataTrain$ma == "Yes", 1, 0)
dataTrain
#alternativa 1
modeloCompleto <- glm(formula = mio ~., data = dataTrain, family = "binomial")
# me avisaba antes que modelo no convergia! SOLUCION:me olvide de pasar a factor la variable ma!!! ya solucionado
summary(modeloCompleto)

#alternativa 2
modeloB <- step(modeloCompleto, direction = "backward") #parto de modelo completo y elimino las var menos significativas
summary(modeloB)

#alternativa 3 - a mi eleccion (recordar que el intercepto va por defecto y No es necvesatrio definirla)
#reviso datos del modelo completo y pruebo con:
modeloC3 <- glm(formula = mio ~ spheq+acd+sporthr+ma , data = dataTrain, family = "binomial") #parto de modelo completo y elimino las var menos significativas
summary(modeloC3)
modeloC2 <- glm(formula = mio ~ spheq+acd+ma , data = dataTrain, family = "binomial") #parto de modelo completo y elimino las var menos significativas
summary(modeloC2)
modeloC <- glm(formula = mio ~ spheq+sporthr+ma , data = dataTrain, family = "binomial") #parto de modelo completo y elimino las var menos significativas
summary(modeloC)
#el AIC incluye la devianza residual junto con una penalización por la complejidad del modelo.
#decimos q es mas parsimonioso el B? es LEVEMENTE mas alto el AIC en manual pero apenas, y tiene menos variables---seguir con anova:
anova(modeloCompleto,modeloB,modeloC) #compara con devianzas RECORDAR QUE COMPARO LA 1ER VARIABLE CON LA 2DA Y LA 2DA CON LA 3RA Y ASI../SALVO el Pvalor que compara con el base(el 1ro)
# voy de modelo complejo a otros con menores variables(menos informacion, que puede empeorar el ajuste) 
#RECORDAR ANOVA:H0=medias iguales // p<0,05 (signif) indica que adopte modelo mas completo, rechazo H0//p mas alta indica que las diferencias podrian ser por azar, No rechazo H0
#
#----------------------------4-------------------
#b-hecho antes con aic y anova.
#c-
summary(modeloC)
exp(coef(modeloC)["spheq"])
exp(coef(modeloC)["ma1"])

coef(modeloC)["ma1"]
coef(modeloC)["ma"]

coef(summary(modeloC))
levels(dataTrain$ma)

#D-MULTICOLIMNEAlidad
#Correlación entre las variables
library(car)


# multicolinealidad
#MODELO MANUAL---------------------------------------------------------------
#VIF en regresión logistica
car::vif(modeloC) #la condición es como en RLM: mayor a 5 indica multicolinealidad

#dist cook para outlier
car::influenceIndexPlot(modeloC, vars = "Cook",
                        id=T, main = "Cook's distance")

#Outliers con formato
n <- nrow(dataTrain)  # Número de observaciones SEGUIR EN TRAIN !!
umbral_cook <- 4/n  #n= casos de train =4/433
#Grafico cook mas umbral con formato
cooks_d <- cooks.distance(modeloC)
plot(cooks_d, pch = 19, col = "blue", main = "Distancias de Cook")
abline(h = umbral_cook, col = "red", lty = 2)
text(x = 1:length(cooks_d), y = cooks_d, 
     labels = ifelse(cooks_d > umbral_cook, names(cooks_d), ""), pos = 3)

#distancia de cook de outliers
obs_influyentes <- which(cooks_d > umbral_cook)
cooks_d[obs_influyentes]


print(datos[obs_influyentes, ], n = Inf)

#MODELO AUTOMATICO-------------------------------------------------------------
car::vif(modeloB) #la condición es como en RLM: mayor a 5 indica multicolinealidad

#dist cook para outlier
car::influenceIndexPlot(modeloB, vars = "Cook",
                        id=T, main = "Cook's distance")

#Outliers con formato
n <- nrow(dataTrain)  # Número de observaciones SEGUIR EN TRAIN !!
umbral_cook <- 4/n  #n= casos de train =4/433
#Grafico cook mas umbral con formato
cooks_d <- cooks.distance(modeloB)
plot(cooks_d, pch = 19, col = "blue", main = "Distancias de Cook")
abline(h = umbral_cook, col = "red", lty = 2)
text(x = 1:length(cooks_d), y = cooks_d, 
     labels = ifelse(cooks_d > umbral_cook, names(cooks_d), ""), pos = 3)

#distancia de cook de outliers
obs_influyentes <- which(cooks_d > umbral_cook)
cooks_d[obs_influyentes]


print(datos[obs_influyentes, ], n = Inf) #para ver todos los valores de la tabla y no solo los primeros10

#----------------------------5-------------------------------------------------
#library(e1071) #recordar para usar laplace de naivebayes
mod1 <- naiveBayes(mio ~spheq+sporthr+ma, data = dataTrain,laplace=1)
# a fin de evitar probabilidad cero para caso sin muestras y para tener modelo mas robusto con pocos datos y que modelo siga funcionando se opta por Laplace 1
mod1
dataTest
#veamos como hace la prediccion en un caso:
dataTest[1,] #el caso 1 del conjunto de Test
predict(object = mod1, newdata=dataTest[1,], type = "raw") #probabilidades predichas
predict(object = mod1, newdata=dataTest[1,], type = "class") #clase predicha
dataTest[2,]
predict(object = mod1, newdata=dataTest[2,], type = "raw") #probabilidades predichas
dataTest[51,]
predict(object = mod1, newdata=dataTest[51,], type = "raw") #probabilidades predichas
dataTest # verifico que la prediccion da igual a lo que tengo en mi muestra de prueba -- el modelo predice ok

#----si quisiera verlo para diferente muestras AUTOMATICO:
#evaluamos la predicción en test:
#para ver las probabilidades predichas para cada caso:
proba.1<-predict(object = mod1, newdata=dataTest, type = "raw")
head(proba.1)
#si quiero la clase predicha:
predi.1<- predict(object = mod1, newdata=dataTest, type = "class")
head(predi.1)
#-------

#--------------------------6----------------------------------------------------
# COMPARO MODELOS
#--------------------bayes::
confusion.1 <- table(dataTest$mio,predi.1, dnn = c("observado","predicho"))#OJO CON EL ORDEN!! 
#LA SUMA DE LOS QUE SON MIOPES (24 EN NUESTRO "DATATEST", MIO=1,CON MI SEMILLA 666) DEBE CORRESPONDER CON LA SUMA DE LOS ELEMENTOS DE MIO=1 EN LA MATRIZ!!
library(caret)
confusionMatrix(confusion.1,positive = "1")
# Con set.seed(666) y tras usar summary se observa comopara la muestra test se tienene 81 miopes (mio=1), y en la matriz de
#confusion tengo: 
#              prediccion
#               0   1
#observ: 0     TN   FP
#        1     FN   TP
#y nosotros tenemos que FN+TP=13+11=24 ok CUMPLE!
summary(dataTest)

#CURVA ROC Y AUC 
library(ROCR)
prediccion1<-prediction(proba.1[,2],dataTest$mio)
roc_mod1 <- performance(prediccion1, measure = "tpr", x.measure = "fpr")

plot(roc_mod1, main = "Naive Bayes mod1", colorize = T)
abline(a = 0, b = 1)
AUC.mod1 <- performance(prediccion1, "auc")
#para que me de AUC:
AUC.mod1@y.values

proba.1<-predict(object = mod1, newdata=dataTest, type = "raw")
head(proba.1)
#si quiero la clase predicha:
predi.1<- predict(object = mod1, newdata=dataTest, type = "class")
head(predi.1)

#-------------- pruebo modelo binomial:::


proba.2<-predict(object = modeloC, newdata=dataTest, type = "response")
head(proba.2)
# el modelo no tiene type class, se define mediante umbral
library(ROCR)
proba.2
prediccion2<-prediction(proba.2[],dataTest$mio)# traigo todas los elementos de la columna de proba.2 (solo tenia unindice y valor) por eso uso[] vacios
roc_mod2 <- performance(prediccion2, measure = "tpr", x.measure = "fpr")

plot(roc_mod2, main = "Binomial  mod2", colorize = T)
abline(a = 0, b = 1)#recta diagonal
AUC.mod2 <- performance(prediccion2, "auc")
#para que me de AUC:
AUC.mod2@y.values

predi.2<- ifelse(proba.2>0.5,1,0) #establezco umbral para definir predi2 ya que este modelo solo informa la probabilidad de Y=1 , y NO COMO ES EL CASO DE BAYES  que informa proba de caso Y=0  y 1.
#Establezco umbnral de 0,5. si es menor defino Y=0 y si es mayor, Y=1 (sos miope)
predi.2
confusion.2 <- table(dataTest$mio, predi.2,dnn = c("observado","predicho"))
confusionMatrix(confusion.2,positive = "1")
##y nosotros tenemos que FN+TP=14+10=24 corresponde con la cantidad de muestras de "datatest" que son miopes (mio=1)!
summary(dataTest)




#-------------------------------------------7----------------------------------------
#ver punto anterior



#######################################################################################
## para encontrar un punto de corte óptimo
pc = seq(from = 0.1, to = 0.9, by = 0.1)
#Crea una secuencia de posibles umbrales de corte  entre 0.1 y 0.9.
#Es decir: se va a probar clasificar como denegado = 1 si la probabilidad es mayor que 0.1, luego 0.2, 0.3, ...0.9

#Crea 5 vectores vacíos (rellenados con 1 por ahora) que servirán para guardar los valores de:
accu = rep(1,length(pc))
sens = rep(1,length(pc))
spec = rep(1,length(pc))
preci = rep(1,length(pc))
F1 = rep(1,length(pc))
#Cada vector tendrá la misma longitud que pc (es decir, un valor por cada umbral probado).

for(i in 1:length(pc))# se recorre cada posible umbral de corte (0.1,0.2,...0.9)
  #i toma los valores del 1 al número total de puntos de corte (length(pc) es 9 si usás de 0.1 a 0.9 en pasos de 0.1).
{pred_y = ifelse(proba.2 >i/10 , 1, 0)
#Si la probabilidad es mayor que el umbral actual, se clasifica como 1 (denegado).Si no, como 0 (no denegado).
confusion = table(pred_y,dataTest$mio)
#Crea una matriz de confusión entre los valores predichos (pred_y) y los reales (dataTest$denegado).
#                   predicho = 0    predicho = 1 
#     observado = 0  VN             FP            
#     observado = 1  FN             VP            

confusion
VP=confusion[2,2] #lo defino así para que se vea igual al realizado anteriormenete
VN=confusion[1,1]
FP=confusion[1,2]
FN=confusion[2,1]
accu[i] = (VP+VN)/(VP+VN+FP+FN)
sens[i] = VP/(VP+FN)
spec[i] = VN/(VN+FP)
preci[i] = VP/(VP+FP)
F1[i] = 2*preci[i]*sens[i]/(sens[i]+preci[i])
}
#CALCULE PARA CADA CASO DE UMBRAL DE CORTE DE pc
df_mod.log = data.frame(
  Umbral = pc,
  Accuracy= accu,
  Precision = preci,
  Sensibilidad = sens,
  Especificidad = spec,
  F1 = F1
)
df_mod.log

#grafico sensitividad y especificidad
ggplot(df_mod.log, aes(x = Umbral) )+
  #defino df y variable de eje x
  geom_line(aes(y = Sensibilidad, color = "Sensibilidad")) +
  geom_line(aes(y = Especificidad, color = "Especificidad")) +
  labs(y = "Valor", color = "") +
  #eje Y - formato / y color definido en cada aes Y
  theme_minimal()

