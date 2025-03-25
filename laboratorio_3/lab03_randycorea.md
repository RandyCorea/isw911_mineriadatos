# LAB03 - Análisis Exploratorio de Datos
**Autor:** Randy Alexander Corea Gonzalez  
**Fecha:** 25/03/2025

  
```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## Carga de Librerías
```{r}
library(tidyverse)
library(ggplot2)
library(dplyr)
```

## Carga del Dataset
```{r}
dataset <- read.csv("./files/movilizados_bus_aresep.csv", stringsAsFactors = FALSE)
head(dataset)
```

## Exploración Inicial de los Datos
### Información de las Columnas y Tipos de Datos
```{r}
str(dataset)
```

### Resumen Estadístico
```{r}
summary(dataset)
```

## Limpieza y Tratamiento de Datos
### Cambio de Nombres de Columnas
```{r}
nombres_espanol <- c("Nombre_Operador", "Cedula", "Codigo_Ruta", "Descripcion_Ruta", "Codigo_Ramal", "Descripcion_Ramal", "Mes", "Año", "Pasajero_Equivalente", "Pasajeros_Total", "Pasajeros_Adulto_Mayor", "Pasajeros_Regulares", "Carreras", "Ingresos")
colnames(dataset) <- nombres_espanol
```

### Eliminación de Variables No Útiles
```{r}
dataset <- dataset %>% select(-c("Codigo_Ramal", "Descripcion_Ramal"))  # Ajustar si hay columnas irrelevantes
```

### Eliminación de Duplicados
```{r}
dataset <- dataset %>% distinct()
```

### Conversión de Variables
```{r}
dataset$Nombre_Operador <- as.factor(dataset$Nombre_Operador)
dataset$Cedula <- as.factor(dataset$Cedula)
dataset$Codigo_Ruta <- as.factor(dataset$Codigo_Ruta)
dataset$Descripcion_Ruta <- as.factor(dataset$Descripcion_Ruta)
dataset$Mes <- as.factor(dataset$Mes)
dataset$Año <- as.numeric(dataset$Año)
dataset$Pasajero_Equivalente <- as.numeric(dataset$Pasajero_Equivalente)
dataset$Pasajeros_Total <- as.numeric(dataset$Pasajeros_Total)
dataset$Pasajeros_Adulto_Mayor <- as.numeric(dataset$Pasajeros_Adulto_Mayor)
dataset$Pasajeros_Regulares <- as.numeric(dataset$Pasajeros_Regulares)
dataset$Carreras <- as.numeric(dataset$Carreras)
dataset$Ingresos <- as.numeric(dataset$Ingresos)
```

### Detección de Valores Nulos
```{r}
sapply(dataset, function(x) sum(is.na(x)))
```

### Identificación de Valores Atípicos con Boxplot
```{r}
ggplot(stack(dataset %>% select(Pasajero_Equivalente, Pasajeros_Total, Pasajeros_Adulto_Mayor, Pasajeros_Regulares, Carreras, Ingresos)), aes(x = ind, y = values)) + geom_boxplot()
```

### Histogramas y Gráficas de Dispersión
```{r}
ggplot(dataset, aes(x=Pasajeros_Total)) + geom_histogram(binwidth=100, fill="blue", alpha=0.7)
ggplot(dataset, aes(x=Carreras, y=Ingresos)) + geom_point()
```

## Visualización de Datos
```{r}
grafico_categorico <- function(columna) {
  ggplot(dataset, aes_string(x=columna)) + geom_bar(fill="blue", alpha=0.7)
}

grafico_categorico("Nombre_Operador")
```

## Análisis de Relaciones de Datos
### Manejo de Valores Nulos para la Matriz de Correlación
```{r}
# Reemplazar valores NA con la mediana de cada columna numérica
dataset_cor <- dataset %>% select_if(is.numeric) %>% 
  mutate_all(~ifelse(is.na(.), median(., na.rm = TRUE), .))

# Verificar si alguna columna es constante (sin variabilidad)
variabilidad <- sapply(dataset_cor, function(x) length(unique(x)) > 1)
dataset_cor <- dataset_cor[, variabilidad]

# Intentar calcular la matriz de correlación solo si hay columnas con variabilidad
if (ncol(dataset_cor) > 1) {
  correlaciones <- cor(dataset_cor, use="complete.obs")
  print(correlaciones)
} else {
  print("No hay suficientes datos variables para calcular la correlación.")
}
```

### Normalización de Datos
```{r}
dataset_norm <- as.data.frame(scale(dataset_cor))
```

## Conclusiones
### Observaciones del Análisis de Correlación 📊

1. **Alta correlación entre `Pasajeros_Adulto_Mayor` y `Pasajeros_Regulares` (0.896)**  
  - Esto sugiere que cuando aumenta el número de pasajeros regulares, también lo hace el número de pasajeros adultos mayores.  
- Podría indicar que las rutas con mayor cantidad de pasajeros en general también son utilizadas por adultos mayores.

2. **Alta correlación entre `Pasajeros_Regulares` y `Carreras` (0.921)**  
  - A más carreras (viajes de autobús), más pasajeros regulares se movilizan.  
- Esto puede sugerir que la demanda de transporte en las rutas analizadas está ligada a la frecuencia del servicio.

3. **Relación moderada entre `Ingresos` y `Pasajeros_Regulares` (0.756)**  
  - Aunque existe una relación positiva, los ingresos no dependen únicamente del número de pasajeros regulares.  
- Factores como tarifas, descuentos o promociones pueden estar afectando la relación.

4. **Correlación moderada entre `Carreras` e `Ingresos` (0.637)**  
  - No hay una relación lineal directa entre la cantidad de carreras y los ingresos, lo que sugiere que otros factores influyen en la rentabilidad de las rutas.  

📌 **Interpretación General:**  
  - La cantidad de pasajeros y la frecuencia de los autobuses están estrechamente relacionadas.  
- El número de pasajeros regulares es un buen predictor de la cantidad de adultos mayores transportados.  
- Los ingresos no dependen exclusivamente del número de pasajeros o carreras, por lo que es importante analizar otras variables como tarifas y costos operativos.  

Si se desea visualizar mejor estas relaciones, se pueden generar gráficos de dispersión o un heatmap de correlaciones. 🚀
