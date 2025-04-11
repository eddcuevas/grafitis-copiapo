# paquetes
library(sf)
library(leaflet)

# Ruta al archivo KML
kml_file <- "D:/01. CARLOS/00. ESTUDIOS/04. INVESTIGACION/00. MIS INVESTIGACIONES/01. Patrones Espaciales Grafitis en Copiapó/Graffitis.kml"

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
datos_adicionales <- read.csv("D:/01. CARLOS/00. ESTUDIOS/04. INVESTIGACION/00. MIS INVESTIGACIONES/01. Patrones Espaciales Grafitis en Copiapó/PRUEBA 2.csv")

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
