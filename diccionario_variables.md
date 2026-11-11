# Diccionario de variables

Variables usadas en el análisis y su disponibilidad en el momento de emitir la predicción (final de la hora actual, t, para predecir la hora siguiente, t+1).

| Variable | Tipo | Descripción y unidad | Ventana temporal | Disponibilidad al predecir |
|---|---|---|---|---|
| `alquileres_actuales` | Numérica | Cantidad de bicicletas alquiladas en la hora actual; número de alquileres | Hora actual | Disponible si el registro ya está actualizado |
| `alquileres_siguiente` | Numérica, respuesta | Cantidad de bicicletas alquiladas en la hora siguiente; número de alquileres | Hora siguiente | No disponible como predictor; es la variable que se busca predecir |
| `hora_siguiente` | Temporal derivada | Hora del día correspondiente al intervalo que se desea predecir; hora (0–23) | Hora siguiente | Calculable si se conoce el horario |
| `es_hora_punta` | Categórica (indicador) | Indica si la hora que se predice es de punta: 7, 8, 9, 17, 18 o 19 h | Hora siguiente | Calculable si se conoce el horario |
| `Temperature(°C)`, `Humidity(%)`, `Wind speed (m/s)`, `Visibility (10m)`, `Dew point temperature(°C)`, `Solar Radiation (MJ/m2)`, `Rainfall(mm)`, `Snowfall (cm)` | Numérica | Mediciones meteorológicas de la hora actual, en las unidades indicadas entre paréntesis | Hora actual | Disponibles si el registro meteorológico de la hora actual ya está actualizado |
| `estacion` | Categórica | Estación del año: primavera, verano, otoño o invierno | Hora actual | Conocida a partir de la fecha |
| `festivo` | Categórica | Indica si el día corresponde a un festivo | Hora actual | Consultable en el calendario |
| `fecha_hora_siguiente` | Temporal derivada | Fecha y hora del intervalo siguiente | Hora siguiente | Calculable a partir de la hora actual |

## Observaciones

- Se excluyen las horas con el sistema inactivo (`Functioning Day = No`) en t o en t+1.
- `fecha_hora_siguiente` se usa para ordenar y particionar cronológicamente; no es un predictor.
- Fuente de los datos: *Seoul Bike Sharing Demand* [Dataset] (2020). UCI Machine Learning Repository. https://doi.org/10.24432/C5F62R. Licencia CC BY 4.0.
