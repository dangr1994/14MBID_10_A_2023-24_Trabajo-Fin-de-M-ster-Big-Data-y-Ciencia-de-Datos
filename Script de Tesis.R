#######################################################################################
##############-------------Paquetes utilizados en el Proceso-------------##############
#######################################################################################
library("readxl")
library("dplyr")
library("pROC")
library("car")
library('caret')
library('openxlsx')
library("FactoMineR")
library("factoextra")
library("rpart")
library("rpart.plot")
options(scipen = 999) #Para poder ver más decimales en los resultados
#######################################################################################
##############-----------------Ingresando BD al Software-----------------##############
#######################################################################################
data <- read_xlsx("C:/Users/DANIEL GARCIA/Desktop/BD TFM.xlsx")
names(data) 
attach(data)
summary(data)
str(data)

#######################################################################################
##############------------------Selección de Variables-------------------##############
#######################################################################################
#Aplicación de modelo Stepwise
data$y <- as.factor(data$y)
modelo_step <- step(glm(y ~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8, 
                          data = data, family = binomial), direction = "backward")
  
#Ver las variables seleccionadas
print(formula(modelo_step))
summary(modelo_step)

#Uso del Test de fisher
fisher.test(data$y, data$x1)
fisher.test(data$y,data$x2)
fisher.test(data$y,data$x3)
fisher.test(data$y,data$x4)
fisher.test(data$y,data$x5)
fisher.test(data$y,data$x6)
fisher.test(data$y,data$x7)
fisher.test(data$y,data$x8)

#######################################################################################
##############-------------Análisis descriptivo de Variables-------------##############
#######################################################################################
  
#Y VS X1
YX1 <- table(y,x1)
YX1_PORC <- with(data,YX1)%>%prop.table()
barp_YX1 <- barplot(YX1,main="Y vs X1",legend=c("Entrega retrasada","Entrega a tiempo"),
                      args.legend = list(x = "top", inset = c(-0.20, 0)),xlab="Demora en llegada de implementos que vienen en embarcación",
                      beside=TRUE,ylim=c(0,250),cex.names=0.5,
                      names.arg = c("1"
                                    , "2"
                                    , "3"),col=c("grey","blue"),las=2)
  text(barp_YX1, YX1 + 1, labels = YX1,pos=3) 

#Y VS X2
YX2 <- table(y,x2)
YX2_PORC <- with(data,YX2)%>%prop.table()
barp_YX2 <- barplot(YX2,main="Y vs X2",legend=c("Entrega retrasada","Entrega a tiempo"),
                      args.legend = list(x = "top", inset = c(-0.20, 0)),xlab="Cambio de características en producción",
                      beside=TRUE,ylim=c(0,250),cex.names=0.5,
                      names.arg = c("0"
                                    , "1"),col=c("grey","blue"),las=2)
  text(barp_YX2, YX2 + 1, labels = YX2,pos=3) 
  

#Y VS X3
YX3 <- table(y,x3)
YX3_PORC <- with(data,YX3)%>%prop.table()
barp_YX3 <- barplot(YX3,main="Y vs X3",legend=c("Entrega retrasada","Entrega a tiempo"),
                      args.legend = list(x = "top", inset = c(-0.20, 0)),xlab="Cantidad de pedidos en cola.",
                      beside=TRUE,ylim=c(0,250),cex.names=0.5,
                      names.arg = c("1"
                                    , "2"
                                    , "3"),col=c("grey","blue"),las=2)
  text(barp_YX3, YX3 + 1, labels = YX3,pos=3) 
  
#Y VS X4
YX4 <- table(y,x4)
YX4_PORC <- with(data,YX4)%>%prop.table()
barp_YX4 <- barplot(YX4,main="Y vs X4",legend=c("Entrega retrasada","Entrega a tiempo"),
                      args.legend = list(x = "top", inset = c(-0.20, 0)),xlab="Disponibilidad de personal",
                      beside=TRUE,ylim=c(0,250),cex.names=0.5,
                      names.arg = c("1"
                                    , "2"
                                    , "3"),col=c("grey","blue"),las=2)
  text(barp_YX4, YX4 + 1, labels = YX4,pos=3) 
  
#Y VS X5
YX5 <- table(y,x5)
YX5_PORC <- with(data,YX5)%>%prop.table()
barp_YX5 <- barplot(YX5,main="Y vs X5",legend=c("Entrega retrasada","Entrega a tiempo"),
                      args.legend = list(x = "top", inset = c(-0.20, 0)),xlab="Condiciones climáticas",
                      beside=TRUE,ylim=c(0,250),cex.names=0.5,
                      names.arg = c("1"
                                    , "2"
                                    , "3"),col=c("grey","blue"),las=2)
  text(barp_YX5, YX5 + 1, labels = YX5,pos=3) 
  
  
#Y VS X6
YX6 <- table(y,x6)
YX6_PORC <- with(data,YX6)%>%prop.table()
barp_YX6 <- barplot(YX6,main="Y vs X6",legend=c("Entrega retrasada","Entrega a tiempo"),
                      args.legend = list(x = "top", inset = c(-0.20, 0)),xlab="Prioridad del pedido",
                      beside=TRUE,ylim=c(0,250),cex.names=0.5,
                      names.arg = c("1"
                                    , "2"
                                    , "3"),col=c("grey","blue"),las=2)
  text(barp_YX6, YX6 + 1, labels = YX6,pos=3) 
 
#Y VS X7
YX7 <- table(y,x7)
YX7_PORC <- with(data,YX7)%>%prop.table()
barp_YX7 <- barplot(YX7,main="Y vs X7",legend=c("Entrega retrasada","Entrega a tiempo"),
                      args.legend = list(x = "top", inset = c(-0.20, 0)),xlab="Funcionamiento de máquinas de producción",
                      beside=TRUE,ylim=c(0,250),cex.names=0.5,
                      names.arg = c("0"
                                    , "1"),col=c("grey","blue"),las=2)
  text(barp_YX7, YX7 + 1, labels = YX7,pos=3) 
  
#Y VS X8
YX8 <- table(y,x8)
YX8_PORC <- with(data,YX8)%>%prop.table()
barp_YX8 <- barplot(YX8,main="Y vs X8",legend=c("Entrega retrasada","Entrega a tiempo"),
                      args.legend = list(x = "top", inset = c(-0.20, 0)),xlab="Historial del cliente con Atraso de Pagos",
                      beside=TRUE,ylim=c(0,400),cex.names=0.5,
                      names.arg = c("0"
                                    , "1"),col=c("grey","blue"),las=2)
  text(barp_YX8, YX8 + 1, labels = YX8,pos=3) 
  
#######################################################################################
##########---Aplicación del Modelo Lineal Generalizado logístico Binomial---###########
#######################################################################################
  
model_glm <- glm(formula=y~x1+x2+x3+x4+x5+x6+x7+x8,data=data,
               family=binomial)
summary(model_glm) 

  
#Coeficientes del Modelo
coef(model_glm)
#Odds Ratio del Modelo
exp(model_glm$coefficients)
model_glm$aic 

#Agregando la Columna de Predicción
predic_glm <- predict(object=model_glm,newdata=data, type='response')
predic_glm <-ifelse(test=predic_glm>0.5,yes=1,no=0)
data_pred <- mutate(PREDICCIONES=predic_glm,data)
data_comp <- mutate(COMPROBACION = PREDICCIONES == y,data_pred)
attach(data_comp)
fac_res <- as.factor(data_comp$y)
fac_prd <- as.factor(data_comp$PREDICCIONES) 
#Tasa de Asertividad del Modelo
mean(data_comp$COMPROBACION) 
#Matriz de Confusión
confusionMatrix(fac_prd,fac_res)
#Curva de ROC
rocgraph_glm<-roc(data$y,data_comp$PREDICCIONES)
plot(rocgraph_glm)
mean(data_comp$COMPROBACION) 
plot.roc(rocgraph_glm,print.auc = T,print.thres = "best",col = "blue",xlab = "1-Especificidad",
           ylab = "Sensibilidad") 
auc(rocgraph_glm)

#######################################################################################
###########---------Aplicación del Modelo de Componentes principales--------###########
#######################################################################################

#Ajuste del modelo PCA
pca_model <- PCA(data[, -which(names(data) == "y")], graph = FALSE)  # Excluye la variable dependiente
summary(pca_model)
independent_vars <- data[, c('x1', 'x2', 'x3', 'x4', 'x5', 'x6', 'x7', 'x8')]

#matriz de covarianza
cov_matrix <- cov(independent_vars)
print(cov_matrix)

#Gráfico de Scree Plot
fviz_screeplot(pca_model, addlabels = TRUE, ylim = c(0, 50))
#varianza explicada
eig_values <- get_eigenvalue(pca_model)
print(eig_values)
#varianza acumulada
cum_var <- cumsum(eig_values[, "eigenvalue"]) / sum(eig_values[, "eigenvalue"]) * 100
#gráfico de la varianza acumulada
plot(cum_var, type = "b", xlab = "Número de Componentes Principales", 
     ylab = "Porcentaje Acumulado de Varianza", 
     main = "Varianza Acumulada en PCA", ylim = c(0, 120))
abline(h = 90, col = "red", lty = 2)  # Línea de referencia para el 90%
text(x = 1:length(cum_var), y = cum_var, 
     labels = round(cum_var, 1), pos = 3, cex = 0.8, col = "blue") 

#Obtenicón de componentes principales
pca_data <- as.data.frame(pca_model$ind$coord[, 1:5])
y <- data$y 
final_data <- cbind(pca_data, y)

#conjunto de entrenamiento y prueba (70% entrenamiento, 30% prueba)
set.seed(42)  # Para reproducibilidad
train_index <- sample(1:nrow(final_data), 0.7 * nrow(final_data))
train_data <- final_data[train_index, ]
test_data <- final_data[-train_index, ]

#Ajustando el modelo con ayuda de regresión logística
logistic_model <- glm(y ~ ., data = train_data, family = binomial)
summary(logistic_model)

#predicciones en el conjunto de prueba
y_pred_prob <- predict(logistic_model, newdata = test_data, type = "response")
y_pred <- ifelse(y_pred_prob > 0.5, 1, 0)  # Umbral de decisión

#Matriz de confusion
conf_matrix <- table(test_data$y, y_pred)
print(conf_matrix)
#Precisión del modelo
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
print(paste("Precisión del modelo:", round(accuracy, 4)))
#AUC y Curva ROC
roc_PCA <- roc(test_data$y, y_pred_prob)
plot(roc_PCA, main = "Curva ROC - Árbol de Decisión")
plot.roc(roc_PCA,print.auc = T,print.thres = "best",col = "blue",xlab = "1-Especificidad",
         ylab = "Sensibilidad") 
auc_value <- auc(roc_PCA)
print(paste("AUC del modelo:", round(auc_value, 4)))


  
#######################################################################################
###########---------Aplicación del Modelo de árboles de decisión--------###########
#######################################################################################

#División de datos en entrenamiento (70%) y prueba (30%)
set.seed(123)  # Fijar la semilla para reproducibilidad
train_index <- createDataPartition(data$y, p = 0.7, list = FALSE)
train_data <- data[train_index, ]
test_data <- data[-train_index, ]
#Entrenamiento del Árbol de Decisión
modelo_arbol <- rpart(y ~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8, 
                      data = train_data, method = "class")
rpart.plot(modelo_arbol)

#predicciones en el conjunto de prueba
predicciones <- predict(modelo_arbol, newdata = test_data, type = "prob")[,2]

#Conversión o transformación de las probabilidades en predicciones binarias (1 o 0)
predicciones_binarias <- ifelse(predicciones > 0.5, 1, 0)

#porcentaje de exactitud y Matriz de confusión
conf_matrix <- confusionMatrix(as.factor(predicciones_binarias), test_data$y)
accuracy <- conf_matrix$overall['Accuracy']
print(paste("Exactitud del modelo Árbol de Decisión: ", round(accuracy * 100, 2), "%"))

#Curva ROC
roc_arbol <- roc(test_data$y, predicciones)
plot(roc_arbol, main = "Curva ROC - Árbol de Decisión")
plot.roc(roc_arbol,print.auc = T,print.thres = "best",col = "blue",xlab = "1-Especificidad",
         ylab = "Sensibilidad") 
#AUC
auc_value <- auc(roc_arbol)
print(paste("AUC del modelo Árbol de Decisión: ", auc_value))




