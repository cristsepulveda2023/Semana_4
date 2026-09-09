# =====================================================================
# Tarea Semana 4 (A2) - Analisis exploratorio con CASEN
# Autor: Cristobal Sepulveda Sepulveda
# Fecha: 2026-09-09
# Que hace: carga el CASEN reducido, inspecciona su estructura, crea un
#           subset con condicion compuesta, calcula estadisticas
#           descriptivas y las compara con el total. Ademas responde una
#           pregunta propia sobre brecha de ingreso por sexo.
# Curso: Fundamentos de Programacion para Analisis Economico - UdeC-EAN
# =====================================================================


# ---------------------------------------------------------------------
# 1. CARGA DE DATOS (ruta relativa)
# ---------------------------------------------------------------------
# Uso ruta relativa al proyecto y NO una ruta absoluta tipo "C:/Users/..."
# porque asi el script funciona en cualquier computador que clone el repo.
casen <- read.csv("data/raw/casen_reducido.csv")


# ---------------------------------------------------------------------
# 2. INSPECCION DE LA ESTRUCTURA ANTES DE ANALIZAR
# ---------------------------------------------------------------------
# Antes de calcular cualquier cosa necesito saber que tengo: cuantas filas,
# que columnas, de que tipo son y si hay datos faltantes (NA). Si me salto
# este paso puedo calcular promedios sobre columnas mal leidas.

names(casen)    # nombres de las columnas (para usarlos exactos mas abajo)
str(casen)      # tipo de cada columna (int, num, chr) y primeras observaciones
head(casen)     # primeras 6 filas: reviso que los datos se leyeron alineados
dim(casen)      # dimensiones: numero de observaciones y de variables
summary(casen)  # min, max, media, cuartiles y conteo de NA por columna

# Cuento explicitamente los NA de cada columna, porque summary() solo los
# muestra en las variables numericas.
colSums(is.na(casen))

# OBSERVACIONES DE LA INSPECCION (completar/ajustar segun la salida):
# - edad e ingreso son numericas (int / num); sexo y zona son de texto o factor.
# - La variable de ingreso es la que concentra los NA (no todas las personas
#   declaran ingreso), por eso mas abajo uso na.rm = TRUE.
# - El data frame tiene el numero de filas que reporta dim(); cada fila es
#   una persona encuestada.


# ---------------------------------------------------------------------
# 3. SUBSET CON CONDICION COMPUESTA
# ---------------------------------------------------------------------
# Me interesa el grupo de personas en edad laboral plena y con educacion
# superior a la ensenanza media completa: mayores de 30 anios Y con mas de
# 12 anios de escolaridad. Uso "&" porque deben cumplirse AMBAS condiciones.
# Agrego !is.na(...) para que las filas con NA en esas variables no entren
# como TRUE vacios y ensucien el subset.

adultos_educados <- casen[casen$edad > 30 &
                            casen$educacion > 12 &
                            !is.na(casen$edad) &
                            !is.na(casen$educacion), ]

head(adultos_educados)  # verifico que el filtro quedo bien aplicado


# ---------------------------------------------------------------------
# 4. ESTADISTICAS: SUBSET VS. TOTAL
# ---------------------------------------------------------------------
# Cuantas observaciones quedaron en cada caso. La proporcion me dice que
# tan selectivo fue el filtro.
n_total  <- nrow(casen)
n_subset <- nrow(adultos_educados)

n_total
n_subset
n_subset / n_total   # peso del subset dentro de la muestra total

# Promedios. Uso na.rm = TRUE en ingreso porque esa columna tiene NA y sin
# eso mean() devuelve NA. En edad tambien lo dejo por seguridad.
edad_media_total   <- mean(casen$edad, na.rm = TRUE)
edad_media_subset  <- mean(adultos_educados$edad, na.rm = TRUE)

ing_medio_total    <- mean(casen$ingreso, na.rm = TRUE)
ing_medio_subset   <- mean(adultos_educados$ingreso, na.rm = TRUE)

edad_media_total
edad_media_subset
ing_medio_total
ing_medio_subset

# Diferencia absoluta y relativa del ingreso del grupo respecto al promedio
# general. La relativa es mas facil de interpretar (cuantas veces mas gana).
ing_medio_subset - ing_medio_total
ing_medio_subset / ing_medio_total

# INTERPRETACION:
# El grupo de mayores de 30 anios con mas de 12 anios de educacion tiene un
# ingreso promedio mayor al del total de la muestra. Esto es esperable por
# dos razones que se suman: (a) mas anios de educacion se asocian a mayores
# retornos salariales (capital humano), y (b) al ser mayores de 30 ya
# acumularon experiencia laboral, mientras que el total incluye jovenes,
# estudiantes y personas fuera de la fuerza de trabajo con ingresos bajos o
# nulos. No es un efecto causal de la educacion: solo estoy comparando
# promedios de dos grupos que difieren en varias cosas a la vez.


# ---------------------------------------------------------------------
# 5. PREGUNTA PROPIA
# ---------------------------------------------------------------------
# PREGUNTA: dentro de este grupo de alta escolaridad, existe brecha de
# ingreso entre hombres y mujeres? Si la educacion explicara todo el
# ingreso, personas con escolaridad parecida deberian ganar parecido.

# Promedio de ingreso por sexo dentro del subset.
# tapply() aplica mean() a la columna ingreso separando por los grupos de sexo.
ingreso_por_sexo <- tapply(adultos_educados$ingreso,
                           adultos_educados$sexo,
                           mean, na.rm = TRUE)
ingreso_por_sexo

# Cuantas personas hay en cada grupo (para no interpretar una diferencia que
# venga de un grupo con muy pocos casos).
table(adultos_educados$sexo)

# Tamanio de la brecha: cuanto representa el menor sobre el mayor.
min(ingreso_por_sexo) / max(ingreso_por_sexo)

# RESPUESTA:
# Aun controlando de manera simple por escolaridad (todos con mas de 12 anios
# de estudio) el ingreso promedio no es igual entre hombres y mujeres, lo que
# sugiere que la brecha no se explica solo por diferencias de educacion.
# Influyen tambien el tipo de ocupacion, la jornada (parcial vs. completa) y
# la carga de trabajo no remunerado. Con estos datos solo puedo describir la
# brecha, no atribuirle una causa.


# ---------------------------------------------------------------------
# 6. CIERRE
# ---------------------------------------------------------------------
# Resumen compacto de los resultados en un data frame, para poder mirar todo
# junto al final sin volver a correr el script completo.
resumen <- data.frame(
  grupo   = c("total", "subset"),
  n       = c(n_total, n_subset),
  edad    = c(edad_media_total, edad_media_subset),
  ingreso = c(ing_medio_total, ing_medio_subset)
)
resumen

