# paquetes
library(sf)
library(leaflet)

# Ruta al archivo KML
kml_file <- "D:/01. CARLOS/00. ESTUDIOS/04. INVESTIGACION/00. MIS INVESTIGACIONES/01. Patrones Espaciales Grafitis en Copiapó/Graffitis2.kml"

# Listar todas las capas en el archivo KML
layers <- st_layers(kml_file)$name
print(layers)

# Excluir la capa "ROSARIO"
layers_to_read <- layers[layers != "ROSARIO"]

# Leer todas las capas excepto "ROSARIO" y combinarlas
kml_data_list <- lapply(layers_to_read, function(layer) st_read(kml_file, layer = layer))
kml_data_combined <- do.call(rbind, kml_data_list)

# Verificar las observaciones combinadas
print(kml_data_combined)

# Revisar el tipo de geometría en el objeto combinado
geom_combined <- st_geometry(kml_data_combined)
geom_type <- st_geometry_type(geom_combined)
print(unique(geom_type))  # Ver los tipos de geometría en la colección

# Extraer las coordenadas basadas en el tipo de geometría
if (any(geom_type == "POINT")) {
  # Extraer coordenadas de los puntos
  coords_combined <- st_coordinates(geom_combined[geom_type == "POINT", ])
  
  # Convertir las coordenadas a un data frame
  coords_combined_df <- as.data.frame(coords_combined)
  
  # Agregar las coordenadas al data frame original
  kml_data_combined_df <- cbind(kml_data_combined[geom_type == "POINT", ], coords_combined_df)
  
  # Crear el mapa interactivo con todas las observaciones de puntos
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

# Leer la base de datos adicional
datos_adicionales <- read.csv("D:/01. CARLOS/00. ESTUDIOS/04. INVESTIGACION/00. MIS INVESTIGACIONES/01. Patrones Espaciales Grafitis en Copiapó/PRUEBA 8.csv")

# Verificar que ambas bases tienen el mismo número de filas
nrow(kml_data_combined_df)  # Número de puntos en la base espacial
nrow(datos_adicionales)     # Número de filas en la base adicional

# Si los datos adicionales son menos, completar con NA
if (nrow(datos_adicionales) < nrow(kml_data_combined_df)) {
  filas_faltantes <- nrow(kml_data_combined_df) - nrow(datos_adicionales)
  # Crear un data.frame con NA y las mismas columnas
  datos_faltantes <- data.frame(matrix(NA, nrow = filas_faltantes, ncol = ncol(datos_adicionales)))
  colnames(datos_faltantes) <- colnames(datos_adicionales)  # Asegurar los mismos nombres de columnas
  datos_adicionales <- rbind(datos_adicionales, datos_faltantes)
}

# Combinar las bases de datos por número de fila
kml_data_combined_df <- cbind(kml_data_combined_df, datos_adicionales)

# Verificar la combinación
print(head(kml_data_combined_df))

# Crear el mapa interactivo con la información combinada
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

#########################
#########################
#manejo de datos faltantes
#########################
#########################

kml_data_combined_df$Tamaño.del.arte[is.na(kml_data_combined_df$Tamaño.del.arte)] <- "Sin información"
kml_data_combined_df$Graffiti[is.na(kml_data_combined_df$Graffiti)] <- median(kml_data_combined_df$Graffiti, na.rm = TRUE)

############################
############################
#MUY BUENOO MAPA DE CALOR
############################
############################

install.packages("leaflet.extras")
library(leaflet.extras)


leaflet(kml_data_combined_df) %>%
  addTiles() %>%
  addHeatmap(
    lng = ~X, lat = ~Y, 
    intensity = ~Graffiti,  # Cambia a la columna adecuada si "Graffiti" tiene muchos NA
    blur = 20, 
    max = 0.05,
    radius = 15
  )



############################
############################
# CLUSTER
############################
############################

# Crear un data frame con las columnas numéricas necesarias
datos_cluster <- kml_data_combined_df[, c("X", "Y", "Z")]

# Verificar las clases de las columnas
sapply(datos_cluster, class)

# Convertir a numérico si alguna columna tiene problemas
datos_cluster <- data.frame(
  X = as.numeric(unlist(datos_cluster$X)),
  Y = as.numeric(unlist(datos_cluster$Y)),
  Z = as.numeric(unlist(datos_cluster$Z))
)

cat("Filas en datos_cluster después de limpiar:", nrow(datos_cluster), "\n")
cat("Filas en kml_data_combined_df:", nrow(kml_data_combined_df), "\n")

# Ejecutar K-Means con 3 clusters
kmeans_result <- kmeans(datos_cluster, centers = 3)

# Inicializar la columna cluster con NA
kml_data_combined_df$cluster <- NA

# Si los tamaños coinciden, asignar clusters
if (nrow(datos_cluster) == nrow(kml_data_combined_df)) {
  kml_data_combined_df$cluster <- as.factor(kmeans_result$cluster)
} else {
  warning("El tamaño de datos_cluster no coincide con el original. Revisa los datos.")
}

library(ggplot2)

ggplot(kml_data_combined_df, aes(x = X, y = Y, color = cluster)) +
  geom_point(size = 3) +
  labs(title = "Clusters de Grafitis", x = "Coordenada X", y = "Coordenada Y") +
  theme_minimal()

aggregate(. ~ cluster, data = kml_data_combined_df[, c("cluster", "X", "Y", "Z")], summary)

# Excluir la columna geometry y agregar estadísticas descriptivas para cada cluster
resultado <- aggregate(cbind(X, Y, Z) ~ cluster, data = kml_data_combined_df, summary)

# Mostrar el resultado
print(resultado)


resultado_ampliado <- aggregate(cbind(X, Y, Z, Tamaño.del.arte) ~ cluster, data = kml_data_combined_df, summary)
print(resultado_ampliado)

ggplot(kml_data_combined_df, aes(x = cluster, y = Z, fill = cluster)) +
  geom_boxplot() +
  labs(title = "Distribución de alturas por cluster", x = "Cluster", y = "Altura (Z)") +
  theme_minimal()

library(leaflet)
leaflet(kml_data_combined_df) %>%
  addTiles() %>%
  addCircleMarkers(
    lng = ~X, lat = ~Y, color = ~cluster,
    radius = 5, popup = ~paste("Cluster:", cluster, "<br>", "Altura (Z):", Z)
  )



############################
############################
# CLUSTER
############################
############################

ggplot(kml_data_combined_df, aes(x = cluster, fill = Colores.dominantes)) +
  geom_bar(position = "dodge") +
  labs(title = "Distribución de Colores Dominantes por Cluster", x = "Cluster", y = "Frecuencia") +
  theme_minimal()

ggplot(kml_data_combined_df, aes(x = cluster, fill = Temas.representados)) +
  geom_bar(position = "fill") +
  labs(title = "Frecuencia de Temas Representados por Cluster", x = "Cluster", y = "Proporción") +
  theme_minimal()

leaflet(kml_data_combined_df) %>%
  addTiles() %>%
  addHeatmap(
    lng = ~X, lat = ~Y,
    intensity = ~Z, blur = 15, max = 0.05, radius = 20
  )


library(sp)
distancias_cluster <- by(kml_data_combined_df, kml_data_combined_df$cluster, function(cluster_data) {
  coords <- as.matrix(cluster_data[, c("X", "Y")])
  distancias <- spDists(coords, longlat = TRUE) # Distancia geodésica
  mean(distancias) # Promedio de distancias
})
print(distancias_cluster)




leaflet(kml_data_combined_df) %>%
  addTiles() %>%
  addCircleMarkers(
    lng = ~X, lat = ~Y,
    color = ~cluster, # Cambiar color según el cluster
    radius = 5,
    popup = ~paste(
      "Cluster: ", cluster, "<br>",
      "Tamaño del arte: ", Tamaño.del.arte, "<br>",
      "Altura (Z): ", Z, "<br>",
      "Temas representados: ", Temas.representados
    )
  ) %>%
  addLegend(position = "bottomright", pal = colorFactor(rainbow(3), kml_data_combined_df$cluster), values = ~cluster)



############################
############################
# ··Reasignar Clusters
############################
############################



# Agrupar clusters 1 y 2 en un único grupo
kml_data_combined_df$cluster_agrupado <- ifelse(
  kml_data_combined_df$cluster %in% c(1, 2),
  "Grupo 1",
  "Grupo 2"
)

# Convertir a factor para asegurar consistencia en los análisis posteriores
kml_data_combined_df$cluster_agrupado <- as.factor(kml_data_combined_df$cluster_agrupado)

# Verificar la nueva agrupación
table(kml_data_combined_df$cluster, kml_data_combined_df$cluster_agrupado)

library(ggplot2)

ggplot(kml_data_combined_df, aes(x = X, y = Y, color = cluster_agrupado)) +
  geom_point(size = 3) +
  labs(title = "Grupos Agrupados de Clusters", x = "Coordenada X", y = "Coordenada Y") +
  theme_minimal()

library(caret)


datos_rose$X <- as.numeric(as.vector(datos_rose$X))
datos_rose$Y <- as.numeric(as.vector(datos_rose$Y))
datos_rose$Z <- as.numeric(as.vector(datos_rose$Z))

sapply(datos_rose, class)

library(ROSE)

# Crear el conjunto de datos balanceado
balanced_data <- ROSE(
  cluster_agrupado ~ X + Y + Z,
  data = datos_rose,
  seed = 123
)$data

# Verificar la distribución de clases
table(balanced_data$cluster_agrupado)


library(caret)

# Ajustar el modelo
modelo_balanceado <- train(
  cluster_agrupado ~ X + Y + Z,
  data = balanced_data,
  method = "glm",
  family = "binomial"
)

# Resumen del modelo
summary(modelo_balanceado)


predicciones <- predict(modelo_balanceado, newdata = datos_rose)
confusionMatrix(predicciones, datos_rose$cluster_agrupado)


ggplot(balanced_data, aes(x = X, y = Y, color = cluster_agrupado)) +
  geom_point(size = 3) +
  labs(title = "Clusters Predichos", x = "X", y = "Y") +
  theme_minimal()



ggplot(balanced_data, aes(x = cluster_agrupado, y = Z, fill = cluster_agrupado)) +
  geom_boxplot() +
  labs(title = "Distribución de Z por Cluster", x = "Cluster", y = "Z") +
  theme_minimal()



modelo_rf <- train(
  cluster_agrupado ~ X + Y + Z,
  data = balanced_data,
  method = "rf"
)
summary(modelo_rf)





# Importancia de las variables
importancia <- varImp(modelo_rf, scale = TRUE)

# Mostrar la importancia
print(importancia)

# Visualización de la importancia
plot(importancia, main = "Importancia de las Variables (Random Forest)")



predicciones_rf <- predict(modelo_rf, newdata = datos_rose)
confusionMatrix(predicciones_rf, datos_rose$cluster_agrupado)










ggplot(balanced_data, aes(x = X, y = Y, color = predict(modelo_rf))) +
  geom_point(size = 3) +
  labs(title = "Clusters Predichos por Random Forest", x = "X", y = "Y") +
  theme_minimal()









# Extraer la mejor precisión del modelo de Random Forest
mejor_precision_rf <- max(modelo_rf$results$Accuracy)


accuracies <- data.frame(
  Modelo = c("Logístico", "Random Forest"),
  Precisión = c(0.964, mejor_precision_rf)
)



library(ggplot2)

ggplot(accuracies, aes(x = Modelo, y = Precisión, fill = Modelo)) +
  geom_bar(stat = "identity") +
  labs(title = "Comparación de Precisión", x = "Modelo", y = "Precisión") +
  theme_minimal()



##########################################
##########################################
####  Validación Cruzada
##########################################
##########################################

trainControl <- trainControl(method = "cv", number = 10)
modelo_cv <- train(
  cluster_agrupado ~ X + Y + Z,
  data = balanced_data,
  method = "rf",
  trControl = trainControl
)
print(modelo_cv)

tuneGrid <- expand.grid(mtry = c(2, 3))
modelo_rf_tuned <- train(
  cluster_agrupado ~ X + Y + Z,
  data = balanced_data,
  method = "rf",
  tuneGrid = tuneGrid,
  trControl = trainControl
)
print(modelo_rf_tuned)

leaflet(kml_data_combined_df) %>%
  addTiles() %>%
  addCircleMarkers(
    lng = ~X, lat = ~Y, color = ~cluster_agrupado,
    radius = 5, popup = ~paste("Cluster:", cluster_agrupado)
  )


