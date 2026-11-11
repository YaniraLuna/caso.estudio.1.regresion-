# Predicción de la demanda horaria de bicicletas compartidas en Seúl

**Proyecto Integrador de Regresión – Caso 1**  
**Asignatura:** Estadística Aplicada  
Universidad de Santiago de Chile – Departamento de Ingeniería Industrial  
**Profesor:** Luis Rojo González

**Integrantes:**
- Catalina Labra
- Yanira Luna
- Vicente Flores
- Juan Cisternas

## 1. Descripción del proyecto

El proyecto busca predecir la cantidad de alquileres de bicicletas compartidas en Seúl para la hora siguiente (t+1), utilizando información de la demanda actual, variables temporales y condiciones meteorológicas.

Se comparan distintos modelos de regresión y aprendizaje automático para identificar el modelo con mejor capacidad predictiva.

## 2. Contenido del repositorio

- `codigo.final.R`: código principal del proyecto.
- `SeoulBikeData.csv`: base de datos original.
- `comparacion_modelos.csv`: resultados de comparación de modelos.
- `diccionario_variables.md`: descripción de las variables utilizadas.
- `pdp_ice_demanda.png`: gráfico PDP e ICE de la demanda.
- `pdp_ice_hora.png`: gráfico PDP e ICE de la hora.
- `pdp_ice_temperatura.png`: gráfico PDP e ICE de la temperatura.

## 3. Requisitos

Para ejecutar el proyecto se necesita:

- R versión 4.6.1.
- RStudio.
- Conexión a internet durante la primera ejecución.

El script utiliza los siguientes paquetes:

- tidyverse
- lubridate
- glmnet
- mgcv
- rpart
- ranger
- gbm
- car
- pdp

Los paquetes que no estén instalados se descargan automáticamente al inicio del script.

## 4. Instrucciones de ejecución

**Paso 1. Descargar el repositorio**

Ingresar al repositorio de GitHub, seleccionar el botón `Code` y luego `Download ZIP`.

**Paso 2. Extraer los archivos**

Descomprimir el archivo descargado en una carpeta del computador.

Verificar que los archivos `codigo.final.R` y `SeoulBikeData.csv` estén dentro de la misma carpeta.

**Paso 3. Abrir el código**

Abrir RStudio y cargar el archivo `codigo.final.R`.

**Paso 4. Establecer el directorio de trabajo**

En RStudio, seleccionar:

`Session → Set Working Directory → To Source File Location`

Esto permite que el script encuentre la base de datos necesaria para ejecutar el análisis.

**Paso 5. Ejecutar el análisis**

Presionar el botón `Source` en RStudio o utilizar el atajo:

`Ctrl + Shift + Enter`

El script realiza la preparación de los datos, entrenamiento de modelos, selección y evaluación del modelo, además de los análisis complementarios.

La ejecución tarda aproximadamente cinco minutos, dependiendo del equipo utilizado.

## 5. Resultados generados

Al finalizar la ejecución, se generan los siguientes archivos:

- `comparacion_modelos.csv`: resultados de evaluación y comparación de los modelos.
- `pdp_ice_demanda.png`: análisis del efecto de la demanda.
- `pdp_ice_hora.png`: análisis del efecto de la hora.
- `pdp_ice_temperatura.png`: análisis del efecto de la temperatura.

La semilla utilizada en los componentes aleatorios es 123, para favorecer la reproducibilidad de los resultados.

## 6. Consideraciones

La selección de hiperparámetros se realiza exclusivamente con el conjunto de validación. El conjunto de prueba se mantiene independiente y se utiliza únicamente para la evaluación final.

Para reproducir los resultados, se recomienda ejecutar el script completo desde una sesión limpia de RStudio, manteniendo los archivos originales en la misma carpeta.
