Proyecto de Tesis: Predicción de Entregas a Tiempo
Este repositorio contiene los archivos y el código necesarios para replicar el análisis y modelamiento estadístico empleado en mi tesis, que aborda la predicción de la puntualidad en la entrega de productos. A través de diferentes modelos de aprendizaje, tanto supervisados como no supervisados, el proyecto busca identificar patrones clave y evaluar la precisión de las predicciones en datos reales.

Contenido
- BD TFM.xlsx: Base de datos inicial empleada para el modelado y prueba de tres tipos de modelos (supervisados y no supervisados). Contiene las variables que permiten evaluar el desempeño del modelo.

- BD_SETIEMBRE.xlsx: Base de datos de septiembre sin la variable de respuesta, utilizada para generar predicciones con el modelo.

- BD_SETIEMBRE_YREAL.xlsx: Base de datos de septiembre que incluye la variable de respuesta, para cotejar y evaluar la precisión de las predicciones realizadas previamente.

- Script de Tesis.R: Script en R que contiene el código de todos los modelos empleados en el análisis, incluyendo las pruebas y métricas de evaluación para comparar los resultados de los distintos enfoques.

Descripción del Proyecto
La tesis explora métodos de modelamiento estadístico para anticipar la puntualidad en entregas. Tras comparar tres tipos de modelos (supervisado y no supervisado), se implementó un modelo logístico, cuyos resultados fueron evaluados con base en métricas de precisión, sensibilidad, especificidad, y F1 Score. El propósito es mejorar la toma de decisiones y aplicar medidas preventivas basadas en las predicciones de entrega.

Requerimientos
Para ejecutar el script y replicar los resultados, se requiere de R y librerías como readxl para la carga de archivos .xlsx, así como librerías de modelamiento como glm para regresiones logísticas.
