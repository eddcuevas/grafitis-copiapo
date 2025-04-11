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

