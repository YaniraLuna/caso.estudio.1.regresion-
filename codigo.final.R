# ============================================================
# CASO 1: ESTADISTICA APLICADA
# Prediccion de la demanda de bicicletas en Seul
#
# INSTRUCCIONES DE EJECUCION
# 1. Dejar "SeoulBikeData.csv" en el directorio de trabajo
#    (se puede verificar con getwd()).
# 2. Ejecutar el script completo, de arriba hacia abajo
#    (en RStudio: Ctrl+Shift+Enter o el boton Source).
# 3. Los paquetes que falten se instalan solos en la seccion 1.
# Semilla: 123. El script no requiere cambios manuales.
# Tiempo de ejecucion aproximado: 5 minutos (rejilla del GBM, PDP/ICE y modelos cuantilicos)
# ============================================================


# 1. PAQUETES

paquetes <- c("tidyverse", "lubridate", "glmnet", "mgcv",
              "rpart", "ranger", "gbm", "car", "pdp")

# Instalar los paquetes que falten
faltantes <- paquetes[
  !vapply(paquetes, requireNamespace, logical(1), quietly = TRUE)
]

if (length(faltantes) > 0) {
  install.packages(faltantes)
}

# Cargar paquetes
library(tidyverse)
library(lubridate)
library(glmnet)
library(mgcv)
library(rpart)
library(ranger)
library(gbm)
library(car)
library(pdp)
# 2. CARGAR LOS DATOS

if (!file.exists("SeoulBikeData.csv")) {
  stop("Falta el archivo SeoulBikeData.csv en el directorio de trabajo")
}

bicicletas <- read_csv(
  "SeoulBikeData.csv",
  locale = locale(encoding = "Latin1"),
  show_col_types = FALSE
)

# Comprobar que se cargaron correctamente
dim(bicicletas)
names(bicicletas)
head(bicicletas)


# ============================================================
# 3. LIMPIEZA Y REVISION DE LOS DATOS
# ============================================================

# Convertir la fecha de texto a formato fecha
bicicletas <- bicicletas %>%
  mutate(
    Date = as.Date(Date, format = "%d/%m/%Y")
  )

# Revisar tipos de variables
glimpse(bicicletas)

# Contar datos faltantes por columna
colSums(is.na(bicicletas))

# Contar filas duplicadas
sum(duplicated(bicicletas))

# Revisar el periodo cubierto por los datos
range(bicicletas$Date)

# Revisar la cantidad de registros por dia
bicicletas %>%
  count(Date) %>%
  summarise(
    dias = n(),
    minimo_registros = min(n),
    maximo_registros = max(n)
  )

# Resumen estadistico de la demanda
summary(bicicletas$`Rented Bike Count`)


# ============================================================
# 4. ANALISIS EXPLORATORIO DE LOS DATOS
# ============================================================

# 4.1 Distribucion de la demanda
ggplot(
  bicicletas,
  aes(x = `Rented Bike Count`)
) +
  geom_histogram(
    bins = 40,
    fill = "steelblue",
    color = "white"
  ) +
  labs(
    title = "Distribucion de la demanda de bicicletas",
    x = "Cantidad de bicicletas alquiladas",
    y = "Frecuencia"
  ) +
  theme_minimal()

# 4.2 Demanda promedio por hora
demanda_hora <- bicicletas %>%
  group_by(Hour) %>%
  summarise(
    promedio_alquileres = mean(`Rented Bike Count`),
    .groups = "drop"
  )

print(demanda_hora)

ggplot(
  demanda_hora,
  aes(x = Hour, y = promedio_alquileres)
) +
  geom_line(linewidth = 1, color = "steelblue") +
  geom_point(size = 2) +
  scale_x_continuous(breaks = 0:23) +
  labs(
    title = "Demanda promedio segun la hora del dia",
    x = "Hora",
    y = "Promedio de alquileres"
  ) +
  theme_minimal()

# 4.3 Demanda promedio por estacion del año
demanda_estacion <- bicicletas %>%
  group_by(Seasons) %>%
  summarise(
    promedio_alquileres = mean(`Rented Bike Count`),
    .groups = "drop"
  )

print(demanda_estacion)

ggplot(
  bicicletas,
  aes(x = Seasons, y = `Rented Bike Count`)
) +
  geom_boxplot(fill = "lightblue") +
  labs(
    title = "Distribucion de alquileres por estacion",
    x = "Estacion del año",
    y = "Cantidad de alquileres"
  ) +
  theme_minimal()

# 4.4 Correlacion entre variables numericas
variables_numericas <- bicicletas %>%
  select(
    `Rented Bike Count`,
    Hour,
    `Temperature(°C)`,
    `Humidity(%)`,
    `Wind speed (m/s)`,
    `Visibility (10m)`,
    `Dew point temperature(°C)`,
    `Solar Radiation (MJ/m2)`,
    `Rainfall(mm)`,
    `Snowfall (cm)`
  )

matriz_correlacion <- cor(variables_numericas)

round(matriz_correlacion, 2)


# ============================================================
# 5. CONSTRUCCION DE LA VARIABLE OBJETIVO 
# ============================================================

# Ordenar cronologicamente los registros
bicicletas_ordenadas <- bicicletas %>%
  arrange(Date, Hour)

# Crear la demanda de la hora siguiente
bicicletas_modelo <- bicicletas_ordenadas %>%
  mutate(
    fecha_hora = as.POSIXct(Date) + Hour * 3600,
    
    fecha_hora_siguiente = lead(fecha_hora),
    
    alquileres_siguiente = lead(
      `Rented Bike Count`
    ),
    
    funcionamiento_siguiente = lead(
      `Functioning Day`
    ),
    
    hora_siguiente = hour(
      fecha_hora_siguiente
    ),
    
    # NUEVA VARIABLE: Señal binaria de horas punta
    es_hora_punta = ifelse(hora_siguiente %in% c(7, 8, 9, 17, 18, 19), 1, 0)
  ) %>%
  filter(
    `Functioning Day` == "Yes",
    funcionamiento_siguiente == "Yes",
    !is.na(alquileres_siguiente)
  )

# Crear nombres simples para las variables principales
bicicletas_modelo <- bicicletas_modelo %>%
  mutate(
    alquileres_actuales = `Rented Bike Count`,
    estacion = Seasons,
    festivo = Holiday
  ) %>%
  select(
    fecha_hora,
    fecha_hora_siguiente,
    alquileres_actuales,
    alquileres_siguiente,
    hora_siguiente,
    es_hora_punta,
    estacion,
    festivo,
    `Temperature(°C)`,
    `Humidity(%)`,
    `Wind speed (m/s)`,
    `Visibility (10m)`,
    `Dew point temperature(°C)`,
    `Solar Radiation (MJ/m2)`,
    `Rainfall(mm)`,
    `Snowfall (cm)`
  )

# Revisar el conjunto preparado
dim(bicicletas_modelo)

summary(bicicletas_modelo$alquileres_siguiente)

head(bicicletas_modelo)


# ============================================================
# 6. DIVISION TEMPORAL DE LOS DATOS
# ============================================================

# Asegurar el orden cronologico
bicicletas_modelo <- bicicletas_modelo %>%
  arrange(fecha_hora)

# Calcular los puntos de corte
n <- nrow(bicicletas_modelo)

corte_entrenamiento <- floor(n * 0.80)
corte_validacion <- floor(n * 0.90)

# Separar sin mezclar el orden temporal
entrenamiento <- bicicletas_modelo %>%
  slice(1:corte_entrenamiento)

validacion <- bicicletas_modelo %>%
  slice((corte_entrenamiento + 1):corte_validacion)

prueba <- bicicletas_modelo %>%
  slice((corte_validacion + 1):n)

# Verificaciones anti-fuga: el orden temporal debe respetarse
stopifnot(
  max(entrenamiento$fecha_hora) < min(validacion$fecha_hora),
  max(validacion$fecha_hora) < min(prueba$fecha_hora),
  nrow(entrenamiento) + nrow(validacion) + nrow(prueba) == nrow(bicicletas_modelo)
)
cat("Verificacion OK: entrenamiento < validacion < prueba, sin solapamiento\n")


# Definir niveles de las variables categoricas
niveles_estaciones <- c(
  "Spring", "Summer", "Autumn", "Winter"
)

entrenamiento <- entrenamiento %>%
  mutate(
    estacion = factor(
      estacion,
      levels = niveles_estaciones
    ),
    festivo = factor(festivo)
  )

validacion <- validacion %>%
  mutate(
    estacion = factor(
      estacion,
      levels = niveles_estaciones
    ),
    festivo = factor(
      festivo,
      levels = levels(entrenamiento$festivo)
    )
  )

prueba <- prueba %>%
  mutate(
    estacion = factor(
      estacion,
      levels = niveles_estaciones
    ),
    festivo = factor(
      festivo,
      levels = levels(entrenamiento$festivo)
    )
  )

# Revisar tamanos de los conjuntos
tibble(
  Conjunto = c(
    "Entrenamiento",
    "Validacion",
    "Prueba"
  ),
  Observaciones = c(
    nrow(entrenamiento),
    nrow(validacion),
    nrow(prueba)
  ),
  Fecha_inicial = as.character(c(
    min(entrenamiento$fecha_hora),
    min(validacion$fecha_hora),
    min(prueba$fecha_hora)
  )),
  Fecha_final = as.character(c(
    max(entrenamiento$fecha_hora),
    max(validacion$fecha_hora),
    max(prueba$fecha_hora)
  ))
)

# Revisar la distribucion por estacion
count(entrenamiento, estacion)
count(validacion, estacion)
count(prueba, estacion)

# ============================================================
# FUNCION AUXILIAR: METRICAS DE PREDICCION
# ============================================================

metricas <- function(real, pred) {
  list(
    MAE  = mean(abs(real - pred)),
    RMSE = sqrt(mean((real - pred)^2)),
    R2   = 1 - sum((real - pred)^2) / sum((real - mean(real))^2)
  )
}

# ============================================================
# 7. MODELO BASE
# ============================================================

# Predecir que la demanda siguiente sera igual a la actual
predicciones_base <- validacion$alquileres_actuales

# Valores reales de la demanda siguiente
y_validacion <- validacion$alquileres_siguiente

# Calcular MAE, RMSE y R cuadrado fuera de muestra
m_base <- metricas(y_validacion, predicciones_base)
mae_base <- m_base$MAE
rmse_base <- m_base$RMSE
r2_base <- m_base$R2

# Mostrar resultados
tibble(
  Modelo = "Base",
  MAE = mae_base,
  RMSE = rmse_base,
  R2 = r2_base
)


# ============================================================
# 8. REGRESION LINEAL OLS
# ============================================================

modelo_ols <- lm(
  alquileres_siguiente ~
    alquileres_actuales +
    hora_siguiente +
    es_hora_punta + # Incorporada
    estacion +
    festivo +
    `Temperature(°C)` +
    `Humidity(%)` +
    `Wind speed (m/s)` +
    `Visibility (10m)` +
    `Dew point temperature(°C)` +
    `Solar Radiation (MJ/m2)` +
    `Rainfall(mm)` +
    `Snowfall (cm)`,
  data = entrenamiento
)

# Revisar los coeficientes y la calidad del ajuste
summary(modelo_ols)

# Predecir sobre el conjunto de validacion
predicciones_ols <- predict(
  modelo_ols,
  newdata = validacion
)

# Calcular las metricas de validacion
m_ols <- metricas(y_validacion, predicciones_ols)
mae_ols <- m_ols$MAE
rmse_ols <- m_ols$RMSE
r2_ols <- m_ols$R2

# Mostrar resultados
tibble(
  Modelo = "OLS",
  MAE = mae_ols,
  RMSE = rmse_ols,
  R2 = r2_ols
)

# ============================================================
# 9. DIAGNOSTICO DEL MODELO DE REGRESION LINEAL (OLS)
# ============================================================

# 9.1 Graficos de diagnostico
# Permiten revisar linealidad, normalidad y varianza de residuos.

par(mfrow = c(2, 2))
plot(modelo_ols)
par(mfrow = c(1, 1))


# 9.2 Multicolinealidad mediante VIF
# Valores elevados pueden indicar que algunas variables
# predictoras estan fuertemente relacionadas entre si.

vif_ols <- car::vif(modelo_ols)

print(vif_ols)

# 9.3 Interpretacion automatica del VIF
# Para variables categoricas con varios niveles, car puede
# devolver GVIF, grados de libertad y GVIF ajustado.

if (is.matrix(vif_ols)) {
  
  vif_ajustado <- vif_ols[, "GVIF^(1/(2*Df))"]
  
  interpretacion_vif <- data.frame(
    Variable = rownames(vif_ols),
    VIF_ajustado = as.numeric(vif_ajustado),
    Interpretacion = ifelse(
      vif_ajustado > 5,
      "Posible multicolinealidad elevada",
      "Sin indicios importantes segun este criterio"
    )
  )
  
} else {
  
  interpretacion_vif <- data.frame(
    Variable = names(vif_ols),
    VIF_ajustado = as.numeric(vif_ols),
    Interpretacion = ifelse(
      vif_ols > 5,
      "Posible multicolinealidad elevada",
      "Sin indicios importantes segun este criterio"
    )
  )
  
}

print(interpretacion_vif)

# ============================================================
# 10. PREPARACION DE MATRICES PARA RIDGE, LASSO Y ELASTIC NET
# ============================================================

# 10.1 Crear matriz de predictores para entrenamiento
# Se excluyen las fechas y la variable objetivo.

x_entrenamiento <- model.matrix(
  alquileres_siguiente ~ . - fecha_hora - fecha_hora_siguiente,
  data = entrenamiento
)[, -1]

# 10.2 Crear matriz de predictores para validacion

x_validacion <- model.matrix(
  alquileres_siguiente ~ . - fecha_hora - fecha_hora_siguiente,
  data = validacion
)[, -1]

# 10.3 Crear matriz de predictores para prueba
# Se reserva para la evaluacion final del modelo seleccionado.

x_prueba <- model.matrix(
  alquileres_siguiente ~ . - fecha_hora - fecha_hora_siguiente,
  data = prueba
)[, -1]

# 10.4 Definir variables objetivo

y_entrenamiento <- entrenamiento$alquileres_siguiente
y_validacion <- validacion$alquileres_siguiente

# 10.5 Verificar dimensiones y nombres de las columnas

cat("Dimensiones de entrenamiento:", dim(x_entrenamiento), "\n")
cat("Dimensiones de validacion:", dim(x_validacion), "\n")
cat("Dimensiones de prueba:", dim(x_prueba), "\n")

cat("¿Coinciden las columnas de entrenamiento y validacion?:",
    identical(colnames(x_entrenamiento), colnames(x_validacion)), "\n")

cat("¿Coinciden las columnas de entrenamiento y prueba?:",
    identical(colnames(x_entrenamiento), colnames(x_prueba)), "\n")



# ============================================================
# 11. MODELO RIDGE
# ============================================================

# 11.1 Validacion cruzada para seleccionar lambda
# alpha = 0 corresponde a Ridge.
# La validacion cruzada se realiza solo con entrenamiento.

set.seed(123)

cv_ridge <- cv.glmnet(
  x_entrenamiento,
  y_entrenamiento,
  alpha = 0,
  nfolds = 10,
  type.measure = "mae",
  standardize = TRUE
)

# 11.2 Lambda seleccionado

lambda_ridge <- cv_ridge$lambda.min

cat("Lambda seleccionado para Ridge:", lambda_ridge, "\n")

# 11.3 Predicciones sobre validacion

predicciones_ridge <- predict(
  cv_ridge,
  newx = x_validacion,
  s = "lambda.min"
)[, 1]

# 11.4 Metricas de evaluacion

m_ridge <- metricas(y_validacion, predicciones_ridge)
mae_ridge <- m_ridge$MAE
rmse_ridge <- m_ridge$RMSE
r2_ridge <- m_ridge$R2

cat("Resultados Ridge en validacion:\n")
cat("MAE:", round(mae_ridge, 2), "\n")
cat("RMSE:", round(rmse_ridge, 2), "\n")
cat("R2:", round(r2_ridge, 4), "\n")


# ============================================================
# 12. MODELO LASSO
# ============================================================

# 12.1 Validacion cruzada para seleccionar lambda
# alpha = 1 corresponde a Lasso.

set.seed(123)

cv_lasso <- cv.glmnet(
  x_entrenamiento,
  y_entrenamiento,
  alpha = 1,
  nfolds = 10,
  type.measure = "mae",
  standardize = TRUE
)

# 12.2 Lambda seleccionado

lambda_lasso <- cv_lasso$lambda.min

cat("Lambda seleccionado para Lasso:", lambda_lasso, "\n")

# 12.3 Predicciones sobre validacion

predicciones_lasso <- predict(
  cv_lasso,
  newx = x_validacion,
  s = "lambda.min"
)[, 1]

# 12.4 Metricas de evaluacion

m_lasso <- metricas(y_validacion, predicciones_lasso)
mae_lasso <- m_lasso$MAE
rmse_lasso <- m_lasso$RMSE
r2_lasso <- m_lasso$R2

cat("Resultados Lasso en validacion:\n")
cat("MAE:", round(mae_lasso, 2), "\n")
cat("RMSE:", round(rmse_lasso, 2), "\n")
cat("R2:", round(r2_lasso, 4), "\n")


# ============================================================
# 13. MODELO ELASTIC NET
# ============================================================

# 13.1 Validacion cruzada para seleccionar lambda
# alpha = 0.5 combina las penalizaciones Ridge y Lasso.

set.seed(123)

cv_elastic <- cv.glmnet(
  x_entrenamiento,
  y_entrenamiento,
  alpha = 0.5,
  nfolds = 10,
  type.measure = "mae",
  standardize = TRUE
)

# 13.2 Lambda seleccionado

lambda_elastic <- cv_elastic$lambda.min

cat("Lambda seleccionado para Elastic Net:", lambda_elastic, "\n")

# 13.3 Predicciones sobre validacion

predicciones_elastic <- predict(
  cv_elastic,
  newx = x_validacion,
  s = "lambda.min"
)[, 1]

# 13.4 Metricas de evaluacion

m_elastic <- metricas(y_validacion, predicciones_elastic)
mae_elastic <- m_elastic$MAE
rmse_elastic <- m_elastic$RMSE
r2_elastic <- m_elastic$R2

cat("Resultados Elastic Net en validacion:\n")
cat("MAE:", round(mae_elastic, 2), "\n")
cat("RMSE:", round(rmse_elastic, 2), "\n")
cat("R2:", round(r2_elastic, 4), "\n")


# ============================================================
# 14. COMPARACION DE MODELOS LINEALES
# ============================================================

resultados_lineales <- data.frame(
  Modelo = c(
    "Base",
    "OLS",
    "Ridge",
    "Lasso",
    "Elastic Net"
  ),
  MAE = c(
    mae_base,
    mae_ols,
    mae_ridge,
    mae_lasso,
    mae_elastic
  ),
  RMSE = c(
    rmse_base,
    rmse_ols,
    rmse_ridge,
    rmse_lasso,
    rmse_elastic
  ),
  R2 = c(
    r2_base,
    r2_ols,
    r2_ridge,
    r2_lasso,
    r2_elastic
  )
)

# Mostrar resultados redondeados

resultados_lineales_redondeados <- resultados_lineales %>%
  mutate(
    MAE = round(MAE, 2),
    RMSE = round(RMSE, 2),
    R2 = round(R2, 4)
  )

print(resultados_lineales_redondeados)

# Ordenar por RMSE: menor error primero

resultados_lineales_ordenados <- resultados_lineales %>%
  arrange(RMSE) %>%
  mutate(
    MAE = round(MAE, 2),
    RMSE = round(RMSE, 2),
    R2 = round(R2, 4)
  )

cat("\nModelos ordenados por RMSE:\n")
print(resultados_lineales_ordenados)


# ============================================================
# 15. MODELO GAM
# ============================================================

# 15.1 Renombrar variables para facilitar la formula

entrenamiento_gam <- entrenamiento %>%
  rename(
    temperatura = `Temperature(°C)`,
    humedad = `Humidity(%)`,
    radiacion_solar = `Solar Radiation (MJ/m2)`,
    lluvia = `Rainfall(mm)`,
    nieve = `Snowfall (cm)`
  )

validacion_gam <- validacion %>%
  rename(
    temperatura = `Temperature(°C)`,
    humedad = `Humidity(%)`,
    radiacion_solar = `Solar Radiation (MJ/m2)`,
    lluvia = `Rainfall(mm)`,
    nieve = `Snowfall (cm)`
  )

# 15.2 Ajustar modelo GAM
# La hora se modela como una variable ciclica:
# la hora 23 queda cerca de la hora 0.

modelo_gam <- mgcv::gam(
  alquileres_siguiente ~
    alquileres_actuales +
    es_hora_punta + # Incorporada
    festivo +
    estacion +
    s(hora_siguiente, bs = "cc", k = 12) +
    s(temperatura, k = 10) +
    s(humedad, k = 10) +
    s(radiacion_solar, k = 10) +
    s(lluvia, k = 6) +
    s(nieve, k = 6),
  data = entrenamiento_gam,
  method = "REML",
  knots = list(hora_siguiente = c(0, 24))
)

# 15.3 Resumen del modelo

summary(modelo_gam)

# 15.4 Predicciones sobre validacion

predicciones_gam <- predict(
  modelo_gam,
  newdata = validacion_gam
)

# 15.5 Metricas de evaluacion

m_gam <- metricas(validacion$alquileres_siguiente, predicciones_gam)
mae_gam <- m_gam$MAE
rmse_gam <- m_gam$RMSE
r2_gam <- m_gam$R2

cat("Resultados GAM en validacion:\n")
cat("MAE:", round(mae_gam, 2), "\n")
cat("RMSE:", round(rmse_gam, 2), "\n")
cat("R2:", round(r2_gam, 4), "\n")


# ============================================================
# 16. ARBOL DE REGRESION
# ============================================================

# 16.1 Ajustar arbol usando solo entrenamiento

modelo_arbol <- rpart(
  alquileres_siguiente ~
    alquileres_actuales +
    hora_siguiente +
    es_hora_punta + # Incorporada
    estacion +
    festivo +
    `Temperature(°C)` +
    `Humidity(%)` +
    `Wind speed (m/s)` +
    `Visibility (10m)` +
    `Dew point temperature(°C)` +
    `Solar Radiation (MJ/m2)` +
    `Rainfall(mm)` +
    `Snowfall (cm)`,
  data = entrenamiento,
  method = "anova",
  control = rpart.control(
    cp = 0.001,
    maxdepth = 10,
    minsplit = 30
  )
)

# 16.2 Revisar el modelo

print(modelo_arbol)
printcp(modelo_arbol)

# 16.3 Predicciones sobre validacion

predicciones_arbol <- predict(
  modelo_arbol,
  newdata = validacion
)

# 16.4 Metricas de evaluacion

m_arbol <- metricas(validacion$alquileres_siguiente, predicciones_arbol)
mae_arbol <- m_arbol$MAE
rmse_arbol <- m_arbol$RMSE
r2_arbol <- m_arbol$R2

cat("Resultados del arbol en validacion:\n")
cat("MAE:", round(mae_arbol, 2), "\n")
cat("RMSE:", round(rmse_arbol, 2), "\n")
cat("R2:", round(r2_arbol, 4), "\n")


# ============================================================
# 17. MODELO RANDOM FOREST
# ============================================================

# 17.1 Renombrar variables para facilitar la formula

entrenamiento_rf <- entrenamiento %>%
  rename(
    temperatura = `Temperature(°C)`,
    humedad = `Humidity(%)`,
    viento = `Wind speed (m/s)`,
    visibilidad = `Visibility (10m)`,
    punto_rocio = `Dew point temperature(°C)`,
    radiacion_solar = `Solar Radiation (MJ/m2)`,
    lluvia = `Rainfall(mm)`,
    nieve = `Snowfall (cm)`
  )

validacion_rf <- validacion %>%
  rename(
    temperatura = `Temperature(°C)`,
    humedad = `Humidity(%)`,
    viento = `Wind speed (m/s)`,
    visibilidad = `Visibility (10m)`,
    punto_rocio = `Dew point temperature(°C)`,
    radiacion_solar = `Solar Radiation (MJ/m2)`,
    lluvia = `Rainfall(mm)`,
    nieve = `Snowfall (cm)`
  )

# 17.2 Ajustar Random Forest

set.seed(123)

modelo_rf <- ranger(
  alquileres_siguiente ~
    alquileres_actuales +
    hora_siguiente +
    es_hora_punta + # Incorporada
    estacion +
    festivo +
    temperatura +
    humedad +
    viento +
    visibilidad +
    punto_rocio +
    radiacion_solar +
    lluvia +
    nieve,
  data = entrenamiento_rf,
  num.trees = 500,
  mtry = 4,
  min.node.size = 10,
  importance = "permutation",
  seed = 123
)

# 17.3 Revisar el modelo

print(modelo_rf)

# 17.4 Predicciones sobre validacion

predicciones_rf <- predict(
  modelo_rf,
  data = validacion_rf
)$predictions

# 17.5 Metricas de evaluacion

m_rf <- metricas(validacion$alquileres_siguiente, predicciones_rf)
mae_rf <- m_rf$MAE
rmse_rf <- m_rf$RMSE
r2_rf <- m_rf$R2

cat("Resultados Random Forest en validacion:\n")
cat("MAE:", round(mae_rf, 2), "\n")
cat("RMSE:", round(rmse_rf, 2), "\n")
cat("R2:", round(r2_rf, 4), "\n")

# 17.6 Importancia de las variables

importancia_rf <- sort(
  modelo_rf$variable.importance,
  decreasing = TRUE
)

print(importancia_rf)
# 17.7 Seleccion de hiperparametros del GBM usando SOLO validacion
formula_gbm <- alquileres_siguiente ~ alquileres_actuales + hora_siguiente +
  es_hora_punta + estacion + festivo + temperatura + humedad + viento +
  visibilidad + punto_rocio + radiacion_solar + lluvia + nieve

rejilla <- expand.grid(n_trees = c(1500, 3500),
                       profundidad = c(4, 6),
                       tasa = c(0.03, 0.01))

rejilla$RMSE_val <- NA_real_
for (i in seq_len(nrow(rejilla))) {
  set.seed(123)
  m <- gbm(formula_gbm, data = entrenamiento_rf, distribution = "gaussian",
           n.trees = rejilla$n_trees[i], interaction.depth = rejilla$profundidad[i],
           shrinkage = rejilla$tasa[i], n.minobsinnode = 10, bag.fraction = 0.8,
           verbose = FALSE)
  p <- predict(m, newdata = validacion_rf, n.trees = rejilla$n_trees[i])
  rejilla$RMSE_val[i] <- sqrt(mean((validacion_rf$alquileres_siguiente - p)^2))
}
print(arrange(rejilla, RMSE_val))
# ============================================================
# 18. MODELO GRADIENT BOOSTING (OPTIMIZADO)
# ============================================================

# 18.1 Ajustar el modelo
set.seed(123)

modelo_gbm <- gbm(
  alquileres_siguiente ~
    alquileres_actuales +
    hora_siguiente +
    es_hora_punta + # Incorporada
    estacion +
    festivo +
    temperatura +
    humedad +
    viento +
    visibilidad +
    punto_rocio +
    radiacion_solar +
    lluvia +
    nieve,
  data = entrenamiento_rf,
  distribution = "gaussian",
  n.trees = 3500,        # Mejor combinacion de la rejilla (seccion 17.7)
  interaction.depth = 6, # Mejor combinacion de la rejilla (seccion 17.7)
  shrinkage = 0.01,      # Mejor combinacion de la rejilla (seccion 17.7)
  n.minobsinnode = 10,
  bag.fraction = 0.8,
  train.fraction = 1,
  cv.folds = 0,
  verbose = FALSE
)

# 18.2 Revisar el modelo
print(modelo_gbm)

# 18.3 Predecir sobre el conjunto de validacion
predicciones_gbm <- predict(
  modelo_gbm,
  newdata = validacion_rf,
  n.trees = 3500
)

# 18.4 Calcular metricas

m_gbm <- metricas(validacion$alquileres_siguiente, predicciones_gbm)
mae_gbm <- m_gbm$MAE
rmse_gbm <- m_gbm$RMSE
r2_gbm <- m_gbm$R2

cat("Resultados Gradient Boosting en validacion:\n")
cat("MAE:", round(mae_gbm, 2), "\n")
cat("RMSE:", round(rmse_gbm, 2), "\n")
cat("R2:", round(r2_gbm, 4), "\n")

# ============================================================
# 19. ENSAMBLE: PROMEDIO DE RANDOM FOREST Y GRADIENT BOOSTING
# ============================================================

# 19.1 Promediar las predicciones
predicciones_promedio <- (
  predicciones_rf + predicciones_gbm
) / 2

# 19.2 Calcular metricas

m_promedio <- metricas(validacion$alquileres_siguiente, predicciones_promedio)
mae_promedio <- m_promedio$MAE
rmse_promedio <- m_promedio$RMSE
r2_promedio <- m_promedio$R2

# 19.3 Mostrar resultados
cat("Resultados del ensamble en validacion:\n")
cat("MAE:", round(mae_promedio, 2), "\n")
cat("RMSE:", round(rmse_promedio, 2), "\n")
cat("R2:", round(r2_promedio, 4), "\n")

# ============================================================
# 20. COMPARACION FINAL DE MODELOS EN VALIDACION
# ============================================================

comparacion_modelos <- data.frame(
  Modelo = c(
    "Base",
    "OLS",
    "Ridge",
    "Lasso",
    "Elastic Net",
    "GAM",
    "Arbol de regresion",
    "Random Forest",
    "Gradient Boosting",
    "Promedio RF + GBM"
  ),
  
  MAE = c(
    mae_base, mae_ols, mae_ridge, mae_lasso, mae_elastic,
    mae_gam, mae_arbol, mae_rf, mae_gbm, mae_promedio
  ),
  
  RMSE = c(
    rmse_base, rmse_ols, rmse_ridge, rmse_lasso, rmse_elastic,
    rmse_gam, rmse_arbol, rmse_rf, rmse_gbm, rmse_promedio
  ),
  
  R2 = c(
    r2_base, r2_ols, r2_ridge, r2_lasso, r2_elastic,
    r2_gam, r2_arbol, r2_rf, r2_gbm, r2_promedio
  )
)

# Ordenar del menor al mayor RMSE
comparacion_modelos <- comparacion_modelos %>%
  arrange(RMSE)

# Mostrar resultados ordenados
comparacion_modelos %>%
  mutate(
    MAE = round(MAE, 2),
    RMSE = round(RMSE, 2),
    R2 = round(R2, 4)
  ) %>%
  print(row.names = FALSE)

# ============================================================
# 21. GRAFICO COMPARATIVO DE MODELOS
# ============================================================


# Comparar RMSE: mientras menor, mejor
ggplot(
  comparacion_modelos,
  aes(x = reorder(Modelo, RMSE), y = RMSE)
) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Comparacion del RMSE de los modelos",
    subtitle = "Conjunto de validacion: menor RMSE indica mejor precision",
    x = "Modelo",
    y = "RMSE (alquileres)"
  ) +
  theme_minimal()

# Comparar MAE: mientras menor, mejor
ggplot(
  comparacion_modelos,
  aes(x = reorder(Modelo, MAE), y = MAE)
) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Comparacion del MAE de los modelos",
    subtitle = "Conjunto de validacion: menor MAE indica menor error absoluto promedio",
    x = "Modelo",
    y = "MAE (alquileres)"
  ) +
  theme_minimal()

# ============================================================
# 22. IMPORTANCIA DE VARIABLES EN GRADIENT BOOSTING
# ============================================================

importancia_gbm <- summary(
  modelo_gbm,
  n.trees = 3500, 
  plotit = FALSE
)

print(importancia_gbm)

# Grafico de importancia
gbm::summary.gbm(
  modelo_gbm,
  n.trees = 3500, 
  plotit = TRUE
)

# ============================================================
# 23. COEFICIENTE E INTERVALO DE CONFIANZA DEL MODELO OLS
# ============================================================

coeficiente <- coef(modelo_ols)["alquileres_actuales"]

intervalo <- confint(
  modelo_ols,
  parm = "alquileres_actuales",
  level = 0.95
)

cat("Coeficiente estimado:", round(coeficiente, 3), "\n")
cat("Limite inferior IC 95%:", round(intervalo[1], 3), "\n")
cat("Limite superior IC 95%:", round(intervalo[2], 3), "\n")

# Preparar datos de prueba con los nombres que espera el modelo
prueba_gbm <- prueba

names(prueba_gbm)[names(prueba_gbm) == "Temperature(°C)"] <- "temperatura"
names(prueba_gbm)[names(prueba_gbm) == "Humidity(%)"] <- "humedad"
names(prueba_gbm)[names(prueba_gbm) == "Wind speed (m/s)"] <- "viento"
names(prueba_gbm)[names(prueba_gbm) == "Visibility (10m)"] <- "visibilidad"
names(prueba_gbm)[names(prueba_gbm) == "Dew point temperature(°C)"] <- "punto_rocio"
names(prueba_gbm)[names(prueba_gbm) == "Solar Radiation (MJ/m2)"] <- "radiacion_solar"
names(prueba_gbm)[names(prueba_gbm) == "Rainfall(mm)"] <- "lluvia"
names(prueba_gbm)[names(prueba_gbm) == "Snowfall (cm)"] <- "nieve"

# Calcular predicciones finales
pred_final <- predict(modelo_gbm, newdata = prueba_gbm, n.trees = 3500) 

# Calcular métricas finales
m_final <- metricas(prueba_gbm$alquileres_siguiente, pred_final)
mae_final <- m_final$MAE
rmse_final <- m_final$RMSE
r2_final <- m_final$R2
m_base_test <- metricas(prueba$alquileres_siguiente, prueba$alquileres_actuales)
cat("Baseline en prueba -> MAE:", round(m_base_test$MAE, 2),
    " RMSE:", round(m_base_test$RMSE, 2),
    " R2:", round(m_base_test$R2, 4), "\n")
# ============================================================
# 24. RESUMEN DE RESULTADOS FINALES YA CALCULADOS
# ============================================================

modelo_seleccionado <- comparacion_modelos$Modelo[1]
stopifnot(modelo_seleccionado == "Gradient Boosting")
cat("MODELO SELECCIONADO (menor RMSE en validacion):", modelo_seleccionado, "\n\n")

cat("Metricas en validacion:\n")
cat("MAE:", round(mae_gbm, 2), "\n")
cat("RMSE:", round(rmse_gbm, 2), "\n")
cat("R2:", round(r2_gbm, 4), "\n\n")

cat("Metricas finales en prueba (modelo seleccionado en validacion):\n")
cat("MAE:", round(mae_final, 2), "\n")
cat("RMSE:", round(rmse_final, 2), "\n")
cat("R2:", round(r2_final, 4), "\n")

cat("\nError por tipo de horario en prueba:\n")


# Calcular errores por hora
resultado_horario <- prueba_gbm %>%
  mutate(
    error_absoluto = abs(alquileres_siguiente - pred_final),
    error_cuadrado = (alquileres_siguiente - pred_final)^2
  ) %>%
  group_by(hora_siguiente) %>%
  summarise(
    MAE = mean(error_absoluto),
    RMSE = sqrt(mean(error_cuadrado)),
    n = n(),
    .groups = "drop"
  )

print(resultado_horario)

resultado_horario_tipo <- prueba_gbm %>%
  mutate(
    tipo_horario = ifelse(
      hora_siguiente %in% c(7, 8, 9, 17, 18, 19),
      "Punta",
      "No punta"
    ),
    error_absoluto = abs(alquileres_siguiente - pred_final),
    error_cuadrado = (alquileres_siguiente - pred_final)^2
  ) %>%
  group_by(tipo_horario) %>%
  summarise(
    MAE = mean(error_absoluto),
    RMSE = sqrt(mean(error_cuadrado)),
    n = n(),
    .groups = "drop"
  )

print(resultado_horario_tipo)

# ============================================================
# 25. EXPORTAR COMPARACION DE MODELOS
# ============================================================

tabla_exportar <- comparacion_modelos %>%
  mutate(
    MAE = round(MAE, 2),
    RMSE = round(RMSE, 2),
    R2 = round(R2, 4)
  )

write.csv(
  tabla_exportar,
  "comparacion_modelos.csv",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

cat("Tabla guardada como comparacion_modelos.csv\n")
# ============================================================
# 26. GRAFICOS PDP E ICE (200 CURVAS) - PARA LAMINA 7
# ============================================================
graficar_pdp_ice <- function(variable, etiqueta_x) {
  ice <- pdp::partial(modelo_gbm, pred.var = variable,
                      train = entrenamiento_rf, n.trees = 3500, ice = TRUE)
  pdp_medio <- ice %>%
    group_by(across(all_of(variable))) %>%
    summarise(yhat = mean(yhat), .groups = "drop")
  set.seed(123)
  ids <- sample(unique(ice$yhat.id), 200)
  ggplot() +
    geom_line(data = filter(ice, yhat.id %in% ids),
              aes(x = .data[[variable]], y = yhat, group = yhat.id),
              alpha = 0.15, color = "grey50") +
    geom_line(data = pdp_medio, aes(x = .data[[variable]], y = yhat),
              color = "blue", linewidth = 1.5) +
    labs(title = paste("PDP e ICE:", etiqueta_x),
         subtitle = "Azul = efecto promedio (PDP). Gris = 200 observaciones (ICE).",
         x = etiqueta_x, y = "Prediccion de alquileres (hora t+1)") +
    theme_minimal()
}

g_pdp_demanda <- graficar_pdp_ice("alquileres_actuales", "Alquileres actuales (hora t)")
g_pdp_hora    <- graficar_pdp_ice("hora_siguiente", "Hora siguiente")
g_pdp_temp    <- graficar_pdp_ice("temperatura", "Temperatura (°C)")

print(g_pdp_demanda); print(g_pdp_hora); print(g_pdp_temp)
ggsave("pdp_ice_demanda.png", g_pdp_demanda, width = 8, height = 5, dpi = 200)
ggsave("pdp_ice_hora.png", g_pdp_hora, width = 8, height = 5, dpi = 200)
ggsave("pdp_ice_temperatura.png", g_pdp_temp, width = 8, height = 5, dpi = 200)

# ============================================================
# 27. INTERVALOS DE PREDICCION 90 % EN TEST - PARA LAMINA 8
# ============================================================
formula_gbm <- alquileres_siguiente ~ alquileres_actuales + hora_siguiente +
  es_hora_punta + estacion + festivo + temperatura + humedad + viento +
  visibilidad + punto_rocio + radiacion_solar + lluvia + nieve

set.seed(123)
modelo_gbm_p05 <- gbm(formula_gbm, data = entrenamiento_rf,
                      distribution = list(name = "quantile", alpha = 0.05),
                      n.trees = 3500, interaction.depth = 6, shrinkage = 0.01,
                      n.minobsinnode = 10, bag.fraction = 0.8, verbose = FALSE)

set.seed(123)
modelo_gbm_p95 <- gbm(formula_gbm, data = entrenamiento_rf,
                      distribution = list(name = "quantile", alpha = 0.95),
                      n.trees = 3500, interaction.depth = 6, shrinkage = 0.01,
                      n.minobsinnode = 10, bag.fraction = 0.8, verbose = FALSE)

y_test <- prueba_gbm$alquileres_siguiente
pred_inf <- predict(modelo_gbm_p05, newdata = prueba_gbm, n.trees = 3500)
pred_sup <- predict(modelo_gbm_p95, newdata = prueba_gbm, n.trees = 3500)

# Sin calibrar
cobertura <- mean(y_test >= pred_inf & y_test <= pred_sup)
ancho     <- mean(pred_sup - pred_inf)
cruces    <- mean(pred_inf > pred_sup)

# Calibrado con la validacion (conformal cuantilico)
y_val  <- validacion_rf$alquileres_siguiente
v_inf  <- predict(modelo_gbm_p05, newdata = validacion_rf, n.trees = 3500)
v_sup  <- predict(modelo_gbm_p95, newdata = validacion_rf, n.trees = 3500)
score  <- pmax(v_inf - y_val, y_val - v_sup)
nivel  <- min(1, ceiling((length(y_val) + 1) * 0.90) / length(y_val))
q      <- quantile(score, nivel)

pred_inf_c <- pmax(0, pred_inf - q)
pred_sup_c <- pred_sup + q
cobertura_cal <- mean(y_test >= pred_inf_c & y_test <= pred_sup_c)
ancho_cal     <- mean(pred_sup_c - pred_inf_c)

# Cobertura por tipo de horario
cobertura_tipo <- prueba_gbm %>%
  mutate(dentro = y_test >= pred_inf_c & y_test <= pred_sup_c,
         ancho = pred_sup_c - pred_inf_c,
         tipo = ifelse(es_hora_punta == 1, "Punta", "No punta")) %>%
  group_by(tipo) %>%
  summarise(cobertura = mean(dentro), ancho_medio = mean(ancho), n = n(),
            .groups = "drop")

cat("Sin calibrar: cobertura", round(cobertura * 100, 1), "% | ancho medio", round(ancho, 1),
    "| cruces de cuantiles", round(cruces * 100, 2), "%\n")
cat("Calibrado:    cobertura", round(cobertura_cal * 100, 1), "% | ancho medio", round(ancho_cal, 1), "\n")
print(cobertura_tipo)

# Comprobacion final de reproducibilidad
stopifnot(abs(rmse_final - 145.12) < 0.01)
cat("Verificacion OK: el RMSE en prueba coincide con el informe (145.12)\n")