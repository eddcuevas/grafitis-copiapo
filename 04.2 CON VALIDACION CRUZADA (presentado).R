# paquetes
library(sf)
library(leaflet)

kml_file <- "D:/01. CARLOS/00. ESTUDIOS/04. INVESTIGACION/00. MIS INVESTIGACIONES/01. Patrones Espaciales Grafitis en Copiapó/Graffitis2.kml"

layers <- st_layers(kml_file)$name
print(layers)

layers_to_read <- layers[layers != "ROSARIO"]

kml_data_list <- lapply(layers_to_read, function(layer) st_read(kml_file, layer = layer))
kml_data_combined <- do.call(rbind, kml_data_list)

print(kml_data_combined)

geom_combined <- st_geometry(kml_data_combined)
geom_type <- st_geometry_type(geom_combined)
print(unique(geom_type))  # Ver los tipos de geometría en la colección

if (any(geom_type == "POINT")) {
  # Extraer coordenadas de los puntos
  coords_combined <- st_coordinates(geom_combined[geom_type == "POINT", ])
  
  coords_combined_df <- as.data.frame(coords_combined)
  
  kml_data_combined_df <- cbind(kml_data_combined[geom_type == "POINT", ], coords_combined_df)
  
  leaflet(kml_data_combined_df) %>%
    addTiles() %>%
    addMarkers(lng = ~X, lat = ~Y, popup = ~Name)
  
} else {
  print("No se encontraron geometrías de tipo POINT")
}

###################################
###################################
#DATA ADICIONAL
###################################
###################################

datos_adicionales <- read.csv("D:/01. CARLOS/00. ESTUDIOS/04. INVESTIGACION/00. MIS INVESTIGACIONES/01. Patrones Espaciales Grafitis en Copiapó/PRUEBA 8.csv")

nrow(kml_data_combined_df)  # Número de puntos en la base espacial
nrow(datos_adicionales)     # Número de filas en la base adicional

if (nrow(datos_adicionales) < nrow(kml_data_combined_df)) {
  filas_faltantes <- nrow(kml_data_combined_df) - nrow(datos_adicionales)
  # Crear un data.frame con NA y las mismas columnas
  datos_faltantes <- data.frame(matrix(NA, nrow = filas_faltantes, ncol = ncol(datos_adicionales)))
  colnames(datos_faltantes) <- colnames(datos_adicionales)  # Asegurar los mismos nombres de columnas
  datos_adicionales <- rbind(datos_adicionales, datos_faltantes)
}

kml_data_combined_df <- cbind(kml_data_combined_df, datos_adicionales)

print(head(kml_data_combined_df))

leaflet(kml_data_combined_df) %>%
  addTiles() %>%
  addMarkers(lng = ~X, lat = ~Y, popup = ~paste(Name, "<br>", Description))  # Personaliza el popup

#################################################
#################################################
### CON POP-UP ####
#################################################
#################################################

leaflet(kml_data_combined_df) %>%
  addTiles() %>%
  addMarkers(
    lng = ~X, lat = ~Y, 
    popup = ~paste(
      "Nombre: ", Name, "<br>",
      "Descripción: ", Description, "<br>",
      "Técnica: ", Técnica.utilizada, "<br>",
      "Colores dominantes: ", Colores.dominantes, "<br>",
      "Temas representados: ", Temas.representados
    )
  )

#################################################
#################################################
### 
### ANALISIS DE DATO
###
#################################################
#################################################

# Resumen general de la base de datos
cat("Número de filas:", nrow(kml_data_combined_df), "\n")
cat("Número de columnas:", ncol(kml_data_combined_df), "\n")
cat("Nombres de las columnas:\n")
print(colnames(kml_data_combined_df))

# Tipos de datos por columna
cat("\nTipos de datos por columna:\n")
print(sapply(kml_data_combined_df, class))

# Resumen estadístico general
cat("\nResumen estadístico general:\n")
print(summary(kml_data_combined_df))























#################################################
#################################################
### 
### Visualización de datos
###
#################################################
#################################################

library(leaflet)
leaflet(kml_data_combined_df) %>%
  addTiles() %>%
  addCircleMarkers(
    lng = ~X, lat = ~Y,
    color = ~Tonalidad.predominante,  # Cambia por la columna de interés
    popup = ~paste("Descripción:", Description, "<br>",
                   "Tamaño del arte:", Tamaño.del.arte, "<br>",
                   "Temas representados:", Temas.representados)
  )


library(ggplot2)
ggplot(kml_data_combined_df, aes(x = Tamaño.del.arte)) +
  geom_bar(fill = "blue", color = "black") +
  labs(title = "Distribución del Tamaño del Arte", x = "Tamaño", y = "Frecuencia")





###################################
###################################
# PRUEBA DE MUCHOS GRAFICOS
###################################
###################################



library(ggplot2)

ggplot(kml_data_combined_df, aes(x = Formato)) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Distribución por Formato", x = "Formato", y = "Frecuencia") +
  theme_minimal()


ggplot(kml_data_combined_df, aes(x = Enfoque)) +
  geom_bar(fill = "lightgreen", color = "black") +
  labs(title = "Distribución por Enfoque", x = "Enfoque", y = "Frecuencia") +
  theme_minimal()

ggplot(kml_data_combined_df, aes(x = Tipo.de.espacio)) +
  geom_bar(fill = "coral", color = "black") +
  labs(title = "Distribución por Tipo de Espacio", x = "Tipo de Espacio", y = "Frecuencia") +
  theme_minimal()


ggplot(kml_data_combined_df, aes(x = cluster, fill = Tipo.de.espacio)) +
  geom_bar(position = "dodge") +
  labs(title = "Distribución de Tipo de Espacio por Cluster", x = "Cluster", y = "Frecuencia") +
  theme_minimal()

leaflet(kml_data_combined_df) %>%
  addTiles() %>%
  addCircleMarkers(
    lng = ~X, lat = ~Y,
    color = ~Tipo.de.espacio,  # Cambia por Formato o Enfoque si deseas
    popup = ~paste(
      "Formato: ", Formato, "<br>",
      "Enfoque: ", Enfoque, "<br>",
      "Tipo de Espacio: ", Tipo.de.espacio
    )
  )


ggplot(kml_data_combined_df, aes(x = Formato, fill = Enfoque)) +
  geom_bar(position = "fill") +
  labs(title = "Proporción de Enfoque por Formato", x = "Formato", y = "Proporción") +
  theme_minimal()




frecuencias_enfoque <- as.data.frame(table(kml_data_combined_df$Enfoque))
colnames(frecuencias_enfoque) <- c("Enfoque", "Frecuencia")

ggplot(frecuencias_enfoque, aes(x = "", y = Frecuencia, fill = Enfoque)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  geom_text(aes(label = paste0(Frecuencia, " (", round(Frecuencia / sum(Frecuencia) * 100, 1), "%)")), 
            position = position_stack(vjust = 0.5), size = 4) +
  labs(title = "Distribución por Enfoque", fill = "Enfoque") +
  theme_void() +
  theme(legend.title = element_text(size = 12),
        legend.text = element_text(size = 10))


ggplot(kml_data_combined_df, aes(x = "", fill = Tipo.de.espacio)) +
  geom_bar(stat = "count", width = 1) +
  coord_polar(theta = "y") +
  labs(title = "Distribución por Tipo de Espacio") +
  theme_void() +
  theme(legend.title = element_blank())

ggplot(kml_data_combined_df, aes(x = "", fill = Tipo.de.espacio)) +
  geom_bar(stat = "count", width = 1) +
  facet_wrap(~ cluster) +
  coord_polar(theta = "y") +
  labs(title = "Distribución de Tipo de Espacio por Cluster") +
  theme_void() +
  theme(legend.title = element_blank())

ggplot(kml_data_combined_df, aes(x = "", fill = Enfoque)) +
  geom_bar(stat = "count", width = 1) +
  coord_polar(theta = "y") +
  facet_wrap(~ Formato) +
  labs(title = "Proporción de Enfoque por Formato") +
  theme_void() +
  theme(legend.title = element_blank())

leaflet(kml_data_combined_df) %>%
  addTiles() %>%
  addCircleMarkers(
    lng = ~X, lat = ~Y,
    color = ~Tipo.de.espacio,  # Cambia por Formato o Enfoque si deseas
    popup = ~paste(
      "Formato: ", Formato, "<br>",
      "Enfoque: ", Enfoque, "<br>",
      "Tipo de Espacio: ", Tipo.de.espacio
    )
  )



kml_data_combined_df$Formato_cat <- ifelse(kml_data_combined_df$Formato == 1, "Graffiti", "Mural/Otro")

frecuencias <- as.data.frame(table(kml_data_combined_df$Formato_cat))
colnames(frecuencias) <- c("Formato_cat", "Frecuencia")

ggplot(frecuencias, aes(x = "", y = Frecuencia, fill = Formato_cat)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  geom_text(aes(label = paste0(Frecuencia, " (", round(Frecuencia / sum(Frecuencia) * 100, 1), "%)")), 
            position = position_stack(vjust = 0.5), size = 4) +
  labs(title = "Distribución por Formato", fill = "Formato") +
  theme_void() +
  theme(legend.title = element_text(size = 12),
        legend.text = element_text(size = 10))


frecuencias_espacio <- as.data.frame(table(kml_data_combined_df$Tipo.de.espacio))
colnames(frecuencias_espacio) <- c("Tipo.de.espacio", "Frecuencia")

ggplot(frecuencias_espacio, aes(x = "", y = Frecuencia, fill = Tipo.de.espacio)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  geom_text(aes(label = paste0(Frecuencia, " (", round(Frecuencia / sum(Frecuencia) * 100, 1), "%)")), 
            position = position_stack(vjust = 0.5), size = 4) +
  labs(title = "Distribución por Tipo de Espacio", fill = "Tipo de Espacio") +
  theme_void() +
  theme(legend.title = element_text(size = 12),
        legend.text = element_text(size = 10))

library(tidyr)
library(dplyr)
library(ggplot2)

kml_data_combined_df <- kml_data_combined_df %>%
  separate_rows(Temas.representados, sep = ",\\s*")

temas_frecuencia <- kml_data_combined_df %>%
  filter(!is.na(Temas.representados)) %>%
  count(Temas.representados, sort = TRUE)

temas_frecuencia %>%
  ggplot(aes(x = reorder(Temas.representados, n), y = n)) +
  geom_segment(aes(xend = Temas.representados, y = 0, yend = n), color = "grey") +
  geom_point(size = 3, color = "#69b3a2") +
  coord_flip() +
  labs(
    title = "Frecuencia de Temas Representados",
    x = "Temas Representados",
    y = "Frecuencia"
  ) +
  theme_minimal()


library(ggplot2)
library(dplyr)
library(tidyr)

kml_data_combined_df <- kml_data_combined_df %>%
  separate_rows(Temas.representados, sep = ",\\s*")

temas_frecuencia <- kml_data_combined_df %>%
  filter(!is.na(Temas.representados)) %>%
  count(Temas.representados, sort = TRUE)

temas_frecuencia <- temas_frecuencia %>%
  mutate(Temas.representados = ifelse(n <= 1, "Otros", Temas.representados)) %>%
  group_by(Temas.representados) %>%
  summarise(n = sum(n)) %>%
  ungroup() %>%
  arrange(desc(n))

empty_bar <- 10  # ACÁ USE NUMEROS DE ESPACIOS VACIOS
temas_frecuencia <- temas_frecuencia %>%
  mutate(Temas.representados = factor(Temas.representados, levels = Temas.representados))

to_add <- data.frame(Temas.representados = rep(NA, empty_bar), n = rep(NA, empty_bar))
temas_frecuencia <- rbind(temas_frecuencia, to_add)
temas_frecuencia$id <- seq(1, nrow(temas_frecuencia))

label_data <- temas_frecuencia
number_of_bar <- nrow(label_data)
angle <- 90 - 360 * (label_data$id - 0.5) / number_of_bar
label_data$hjust <- ifelse(angle < -90, 1, 0)
label_data$angle <- ifelse(angle < -90, angle + 180, angle)
label_data$label <- ifelse(is.na(label_data$Temas.representados), "", paste(label_data$Temas.representados, " (", label_data$n, ")", sep = ""))

ggplot(temas_frecuencia, aes(x = as.factor(id), y = n)) +
  geom_bar(stat = "identity", fill = alpha("#69b3a2", 0.8)) +
  ylim(-max(temas_frecuencia$n, na.rm = TRUE) * 0.5, max(temas_frecuencia$n, na.rm = TRUE) * 1.2) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1, 4), "cm")
  ) +
  coord_polar(start = 0) +
  geom_text(data = label_data, aes(x = id, y = n + 1, label = label), 
            color = "black", fontface = "bold", alpha = 0.6, size = 2.5, 
            angle = label_data$angle, hjust = label_data$hjust, inherit.aes = FALSE) +
  labs(title = "Distribución de Temas Representados")


###################################
###################################
# REGRESIÓN LOGÍSTICA
###################################
###################################


kml_data_combined_df$Formato <- as.factor(kml_data_combined_df$Formato)

set.seed(123)
ind <- sample(1:nrow(kml_data_combined_df), size = round(nrow(kml_data_combined_df) * 0.8, 0))
data.train <- kml_data_combined_df[ind, ]
data.test <- kml_data_combined_df[-ind, ]

modelo <- glm(Formato ~ Visibilidad + lat + long + ,
              data = data.train, family = binomial(link = "logit"))
summary(modelo)

odds_ratios <- exp(modelo$coefficients)
print(odds_ratios)

pred <- predict(modelo, newdata = data.test, type = "response")

###################################
###PROFESOR ENVIARÁ MEJOR THRESHOLD
threshold <- 0.5  
###################################

tpred <- ifelse(pred >= threshold, "1", "0")

# Matriz de confusión
library(caret)
confusion <- confusionMatrix(factor(tpred, levels = c("0", "1")), 
                             factor(data.test$Formato, levels = c("0", "1")))
print(confusion)

# Curva ROC
library(pROC)
ROC <- roc(data.test$Formato, pred, levels = c("0", "1"))
plot(ROC, col = "blue", main = "Curva ROC - Regresión Logística")
auc_value <- auc(ROC)
cat("AUC:", auc_value, "\n")



# -------------------------------
# -------------------------------
# REGRESIÓN LOGÍSTICA 2.0 (POST PROFESOR)
# -------------------------------
# -------------------------------


library(sf)
library(leaflet)
library(openxlsx)
library(tidyverse)
library(caret)
library(pROC)

kml_data_combined_df$Formato <- as.factor(kml_data_combined_df$Formato)

set.seed(123)
ind <- sample(1:nrow(kml_data_combined_df), size = round(nrow(kml_data_combined_df) * 0.8, 0))
data.train <- kml_data_combined_df[ind, ]
data.test <- kml_data_combined_df[-ind, ]

modelo <- glm(Formato ~ Visibilidad + Frecuencia.de.visitas.al.área + X + Y + Z, 
              data = data.train, family = binomial(link = "logit"))

summary(modelo)

odds_ratios <- exp(coef(modelo))
cat("Odds Ratios:\n")
print(odds_ratios)

pred <- predict(modelo, newdata = data.test, type = "response")

ROC <- roc(data.test$Formato, pred, levels = c("0", "1"))
plot(ROC, col = "blue", main = "Curva ROC - Regresión Logística")
auc_value <- auc(ROC)
cat("AUC:", auc_value, "\n")

opt_cutoff <- coords(ROC, "best", ret = "threshold", best.method = "youden")
optimo_threshold <- as.numeric(opt_cutoff)

cat("Umbral óptimo:", optimo_threshold, "\n")

tpred.opt <- ifelse(pred >= optimo_threshold, "1", "0")

# Matriz de confusión
confusion.opt <- confusionMatrix(factor(tpred.opt, levels = c("0", "1")), 
                                 factor(data.test$Formato, levels = c("0", "1")))

print(confusion.opt)


# -------------------------------
# RANDOM FOREST
# -------------------------------
clas2 <- randomForest(Formato ~ Visibilidad  + Frecuencia.de.visitas.al.área + X + Y, 
                      data = data.train, importance = TRUE)

pred2 <- predict(clas2, data.test, type = "prob")

ROC.RF <- roc(data.test$Formato, pred2[, 2], levels = c("0", "1"))
plot(ROC.RF, col = "red", main = "Curva ROC - Random Forest")
auc_rf <- auc(ROC.RF)
cat("AUC (Random Forest):", auc_rf, "\n")

opt_cutoff_rf <- coords(ROC.RF, "best", ret = "threshold", best.method = "youden")
optimo_threshold_rf <- as.numeric(opt_cutoff_rf)
cat("Umbral óptimo (Random Forest):", optimo_threshold_rf, "\n")

tpred_rf.opt <- ifelse(pred2[, 2] >= optimo_threshold_rf, "1", "0")

confusion_rf <- confusionMatrix(factor(tpred_rf.opt, levels = c("0", "1")), 
                                factor(data.test$Formato, levels = c("0", "1")))
print(confusion_rf)

# -------------------------------
# COMPARACIÓN ENTRE MODELOS
# -------------------------------

cat("\nComparación de Modelos:\n")
cat("AUC (Regresión Logística):", auc_value, "\n")
cat("AUC (Random Forest):", auc_rf, "\n")













###################################
###################################
#
# Corrección de la clasificación binaria
# [L.Rei + D.Nascimento + P.Ferreira + F.Louzada]
# 
###################################
###################################




library(rstan)
options(mc.cores = 4)
library(loo)

source("funcoes_aplicacao.R")

####################################################################################
########### Blood Donation Dataset ##########################################################
###################################################################################
#https://archive.ics.uci.edu/ml/datasets/Blood+Transfusion+Service+Center
setwd("D:/01. CARLOS/00. ESTUDIOS/04. INVESTIGACION/00. MIS INVESTIGACIONES/01. Patrones Espaciales Grafitis en Copiapó")
BDBD <- read.csv("BDBD.csv")

summary(df)

apply(df,2,max)
apply(df,2,sd) 

library(corrplot)
corrplot(cor(as.matrix(df[,3:5])), is.corr = FALSE, method = 'color', col.lim = c(-1, 1),, col = colorRampPalette(c("blue","white","firebrick3"))(200), addCoef.col = 'grey30')

library(ggplot2)
tbl <- with(df, prop.table(table()))
ggplot(as.data.frame(tbl), aes(factor(df$Formato), Freq )) +
  scale_y_continuous(labels=scales::percent)+
  geom_col(position = 'dodge')+ theme_bw()+ylab("Porcentagem")+xlab("")

summary(df)

Y = df$Formato
X = df[,3:5]

#################################
### Double Lomax DISTRIBUTION
model_dlomax <- stan_model('dlomax.stan')
fit_dlomax <- sampling(model_dlomax, list(n = length(Y), y=Y, X = as.matrix(X), k = ncol(X)), 
                       iter = 100, chains = 1, seed =1)
log_lik_1 <- extract_log_lik(fit_dlomax)
loo(log_lik_1)$looic 
waic(log_lik_1)$waic 

postSamples <- Reduce(cbind, extract(fit_dlomax, pars=c("beta0", "beta")))
dic(model.matrix(~X), Y, loglike_dlomax, postSamples)
eaic(model.matrix(~X), Y, loglike_dlomax, postSamples)
ebic(model.matrix(~X), Y, loglike_dlomax, postSamples)

### Reverse Power Lomax
model_rplomax <- stan_model('drplomax.stan')
fit_rplomax <- sampling(model_rplomax, list(n = length(Y), y=Y, X = as.matrix(X), k = ncol(X)), iter = 5000, chains = 4, seed =1)
log_lik_1 <- extract_log_lik(fit_rplomax)
loo(log_lik_1)$looic
waic(log_lik_1)$waic  

postSamples <- Reduce(cbind, extract(fit_rplomax, pars=c("beta0", "beta", "loglambda")))
dic(model.matrix(~X), Y, loglike_dpinvlomax, postSamples)
eaic(model.matrix(~X), Y, loglike_dpinvlomax, postSamples)
ebic(model.matrix(~X), Y, loglike_dpinvlomax, postSamples)

### Power Lomax
model_plomax <- stan_model('dplomax.stan')
fit_plomax <- sampling(model_plomax, list(n = length(Y), y=Y, X = as.matrix(X), k = ncol(X)), iter = 5000, chains = 4, seed = 1)
log_lik_1 <- extract_log_lik(fit_plomax)
loo(log_lik_1)$looic
waic(log_lik_1)$waic 

postSamples <- Reduce(cbind, extract(fit_plomax, pars=c("beta0", "beta", "loglambda")))
dic(model.matrix(~X), Y, loglike_dplomax, postSamples)
eaic(model.matrix(~X), Y, loglike_dplomax, postSamples)
ebic(model.matrix(~X), Y, loglike_dplomax, postSamples)

### Logistica 
model_logistic <- stan_model('dlogistic.stan')
fit_logistic <- sampling(model_logistic, list(n = length(Y), y=Y, X = as.matrix(X), k = ncol(X)), iter = 5000, chains = 4, seed =1)
log_lik_1 <- extract_log_lik(fit_logistic)
loo(log_lik_1)$looic
waic(log_lik_1)$waic 

postSamples <- Reduce(cbind, extract(fit_logistic, pars=c("beta0", "beta")))
dic(model.matrix(~X), Y, loglike_log, postSamples)
eaic(model.matrix(~X), Y, loglike_log, postSamples)
ebic(model.matrix(~X), Y, loglike_log, postSamples)

### Cloglog
model_cloglog <- stan_model('dcloglog.stan')
fit_cloglog <- sampling(model_cloglog, list(n = length(Y), y=Y, X = as.matrix(X), k = ncol(X)), iter = 5000, chains = 4, seed =1)
log_lik_1 <- extract_log_lik(fit_cloglog)
loo(log_lik_1)$looic
waic(log_lik_1)$waic 

postSamples <- Reduce(cbind, extract(fit_cloglog, pars=c("beta0", "beta")))
dic(model.matrix(~X), Y, loglike_cloglog, postSamples)
eaic(model.matrix(~X), Y, loglike_cloglog, postSamples)
ebic(model.matrix(~X), Y, loglike_cloglog, postSamples)

### Loglog
model_loglog <- stan_model('loglog.stan')
fit_loglog <- sampling(model_loglog, list(n = length(Y), y=Y, X = as.matrix(X), k = ncol(X)), iter = 5000, chains = 4, seed =1)
log_lik_1 <- extract_log_lik(fit_loglog)
loo(log_lik_1)$looic
waic(log_lik_1)$waic 

postSamples <- Reduce(cbind, extract(fit_loglog, pars=c("beta0", "beta")))
dic(model.matrix(~X), Y, loglike_loglog, postSamples)
eaic(model.matrix(~X), Y, loglike_loglog, postSamples)
ebic(model.matrix(~X), Y, loglike_loglog, postSamples)

### Probit
model_probit <- stan_model('probit.stan')
fit_probit <- sampling(model_probit, list(n = length(Y), y=Y, X = as.matrix(X), k = ncol(X)), iter = 5000, chains = 4, seed =1)
log_lik_1 <- extract_log_lik(fit_probit)
loo(log_lik_1)$looic
waic(log_lik_1)$waic

postSamples <- Reduce(cbind, extract(fit_probit, pars=c("beta0", "beta")))
dic(model.matrix(~X), Y, loglike_probit, postSamples)
eaic(model.matrix(~X), Y, loglike_probit, postSamples)
ebic(model.matrix(~X), Y, loglike_probit, postSamples)

### Cauchit
model_cauchit <- stan_model('cauchit.stan')
fit_cauchit <- sampling(model_cauchit, list(n = length(Y), y=Y, X = as.matrix(X), k = ncol(X)), iter = 5000, chains = 4, seed =1)
log_lik_1 <- extract_log_lik(fit_cauchit)
loo(log_lik_1)$looic
waic(log_lik_1)$waic 

postSamples <- Reduce(cbind, extract(fit_cauchit, pars=c("beta0", "beta")))
dic(model.matrix(~X), Y, loglike_cauchit, postSamples)
eaic(model.matrix(~X), Y, loglike_cauchit, postSamples)
ebic(model.matrix(~X), Y, loglike_cauchit, postSamples)

## tabela
modelos = list(fit_dlomax, fit_rplomax, fit_plomax, fit_logistic, fit_cloglog, fit_loglog, fit_probit,fit_cauchit)
loglikes = list(loglike_dlomax,  loglike_dpinvlomax, loglike_dplomax, loglike_log, loglike_cloglog, loglike_loglog,loglike_probit, loglike_cauchit )
medidas = data.frame(matrix(ncol = 9,nrow = length(modelos)))
colnames(medidas) = c("dist", "rho", "dbar", "dhat", "dic", "eaic", "ebic", "loo", "waic")
#modelos = list(fit_probit, fit_plomax)
#loglikes = list(loglike_probit,loglike_dplomax)
for(i in 1:length(modelos)){
  
  log_lik_1 <- extract_log_lik(modelos[[i]])
  medidas[i,"loo"] = loo(log_lik_1)$looic
  medidas[i,"waic"] = waic(log_lik_1)$waic 
  
  if(i %in% c(2,3)){
    postSamples <- Reduce(cbind, extract(modelos[[i]], pars=c("beta0", "beta", "loglambda")))
  }else{
    postSamples <- Reduce(cbind, extract(modelos[[i]], pars=c("beta0", "beta")))
  }
  
  dicc = dic(model.matrix(~X), Y, loglikes[[i]], postSamples)
  
  medidas[i,"dbar"] = dicc$dbar
  medidas[i,"dhat"] = dicc$dhat
  medidas[i,"rho"] = dicc$rho
  medidas[i,"dic"] = dicc$dic
  medidas[i,"eaic"] = eaic(model.matrix(~X), Y, loglikes[[i]], postSamples)
  medidas[i,"ebic"] = ebic(model.matrix(~X), Y, loglikes[[i]], postSamples)
}

medidas[,-1]
xtable(medidas[,-1], digits =3)

### valor dos parametros 

modelos = list(fit_dlomax, fit_rplomax, fit_plomax, fit_logistic, fit_cloglog, fit_loglog, fit_probit,fit_cauchit)
parametros = data.frame(matrix(ncol = 7,nrow = length(modelos)))
colnames(parametros) = c("dist", "beta0", "beta1", "beta2", "beta3", "loglambda", "lambda")


for (i in 1:length(modelos)){
  parametros[i, "beta0"] = mean(extract(modelos[[i]], permuted = TRUE)$beta0)
  parametros[i, "beta1"] = mean(extract(modelos[[i]], permuted = TRUE)$beta[,1])
  parametros[i, "beta2"] = mean(extract(modelos[[i]], permuted = TRUE)$beta[,2])
  parametros[i, "beta3"] = mean(extract(modelos[[i]], permuted = TRUE)$beta[,3])
  if(i %in% c(2,3)){
    parametros[i, "loglambda"] = mean(extract(modelos[[i]], permuted = TRUE)$loglambda)
    parametros[i, "lambda"] = mean(exp(extract(modelos[[i]], permuted = TRUE)$loglambda))
  }
  print(i)
}
library(xtable)
xtable(round(parametros,3), row.names = F, digits = 3)


rplomaxx <- extract(fit_rplomax,permuted = T)

par1 <- data.frame(matrix(ncol = 5))
names(par1) <- c("par", "mean", "sd", "median", "5", "95")

par1[1,1] <- mean(rplomaxx$beta0)
par1[1,2] <-sd(rplomaxx$beta0)
par1[1,3] <-median(rplomaxx$beta0)
par1[1,4:5] <- quantile(rplomaxx$beta0, c(0.05, 0.95))

par1[2:4,1] <-apply(rplomaxx$beta, 2, mean)
par1[2:4,2] <-apply(rplomaxx$beta, 2, sd)
par1[2:4,3] <-apply(rplomaxx$beta, 2, median)
par1[2:4,4:5] <-t(apply(rplomaxx$beta, 2, quantile, c(0.05, 0.95)))


par1[5,1] <-mean(exp(rplomaxx$loglambda))
par1[5,2] <- sd(exp(rplomaxx$loglambda))
par1[5,3] <-median(exp(rplomaxx$loglambda))
par1[5,4:5] <-quantile(exp(rplomaxx$loglambda), c(0.05, 0.95))
xtable(par1, digits = 3)

#########################################################################3
#### Análise Preditiva #####################################################
######################################################################
library(pROC)
library("cutpointr")
library(caret)

loglog <- function(x){exp(-exp(-x))}
cloglog <- function(x){1-exp(-exp(x))}

med_pred = data.frame(matrix(ncol = 7,nrow = length(modelos)))
names(med_pred) = c("cp","AUC", "Acuracia", "Precisao", "F1", "SENS", "ESP")
probabilidades <- list(F_dlomax, Finvplomax, Fplomax, plogis, cloglog, loglog, pnorm, pcauchy)

for (i in 1:nrow(parametros)){
  xbeta <- model.matrix(~X) %*% as.numeric(parametros[i,2:5],nrow=1)
  
  if(i %in% c(2,3)){
    prob <- sapply(xbeta, probabilidades[[i]],  loglambda = parametros[i,6])
  }else{
    prob <- sapply(xbeta, probabilidades[[i]])
  }
  
  cp <- 0.5
  
  med_pred[i,"cp"] <- cp
  prediction <- ifelse(prob>cp, 1,0)
  
  roc_obj <- pROC::roc(Y,prediction)
  
  med_pred[i,"AUC"] = pROC::auc(roc_obj)
  
  xtab <- table(prediction, Y)
  print(xtab)
  
  cm <- confusionMatrix(xtab, mode = "everything", positive = "1")
  if(i ==1){
    confusion <- xtab
  }else{
    confusion <- rbind(confusion,xtab) 
  }
  
  med_pred[i,"Acuracia"] <- cm$overall['Accuracy']
  med_pred[i, c("Precisao", "F1", "SENS", "ESP")] <- cm$byClass[c("Precision", "F1","Sensitivity","Specificity" )]
  print(i)
}
library(xtable) 
xtable(round(med_pred,3), row.names = F, digits = 3)
xtable(confusion)
############################################################################
### Residuals ##########################################################
##########################################################################
## usei i=2
xbeta <- model.matrix(~X) %*% as.numeric(parametros[2,2:5],nrow=1)

prob <- Finvplomax(xbeta, parametros[2,6])

set.seed(1)
a <- pbinom(Y-1, 1, prob) 
b <- pbinom(Y, 1, prob)
res <- qnorm(runif(length(a),a, b))
plot(res)
qqnorm(res, pch = 1, frame = FALSE)
qqline(res, col = "steelblue", lwd = 2)
hist(res)

require(qqplotr)
require(qqplotr)
ggplot(data = data.frame(res), mapping = aes(sample = res)) +
  stat_qq_band() +
  stat_qq_line() +
  stat_qq_point() +
  labs(x = "Quantis Teóricos", y = "Resíduos Quantílicos")+theme_bw()
ggsave(path = '/home/notmyname/Desktop/mestrado/imagens', filename = "QQplot1.png", width = 6, height = 4)

gg <- ggplot(data = data.frame(res), aes(x = res)) +
  geom_histogram(colour="black", fill="white", bins =15) +
  labs(x = "Resíduos Quantílicos", y = "Frequência")+theme_bw()
gg
ggsave(path = '/home/notmyname/Desktop/mestrado/imagens', filename = "histres1.png", width = 6, height = 4)


data = data.frame("res" = res, "index" = 1:length(res))
gg <- ggplot(data = data, aes(x = index, y = res)) +
  geom_point(size = 2) +
  labs(x = "Index", y = "Resíduos Quantílicos")+theme_bw()
gg
ggsave(path = '/home/notmyname/Desktop/mestrado/imagens', filename = "quantres1.png", width = 6, height = 4)























