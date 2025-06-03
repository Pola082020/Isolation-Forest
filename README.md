# Isolation-Forest
rm(list = ls()); gc()
{
  # 0. Librerias y memoria ----
  Sys.setlocale("LC_ALL", "es_ES.UTF-8")
  
  library(ggplot2)
  library(tidyr)
  library(dplyr)
  library(lubridate)
  library(stringr)
  library(stringi)
  library(remotes)
  library(scales)
  library(solitude) #isolation forest
  library(DBI)
  library(RJDBC)
  library(dplyr)
  library(rJava)
  "solitude" %in% (.packages())  # Debe devolver TRUE; isolation_forest esté en el paquete
  ls("package:solitude")
  
  Sys.which("make")
  options(scipen=999)
  options(java.parameters = c(
    "--add-opens=java.base/java.nio=ALL-UNNAMED",
    "-Xms512m", "-Xmx16g"
  ))
  Sys.getenv("JAVA_TOOL_OPTIONS")
  .jinit()
  runtime <- .jcall("java/lang/Runtime", "Ljava/lang/Runtime;", "getRuntime")
  max_mem <- .jcall(runtime, "J", "maxMemory")
  cat("Java Max Memory:", round(max_mem / (1024^3), 2), "GB\n")
  options(java.parameters = "-Xmx16g")  # o -Xmx12g si tienes menos RAM
}

# 1. Carga datos conectado al Data bricks ----
# Diego.Lopez escritorio ADRES
# dleol portatil
# Base Reclamaciones
{
  drv <- JDBC(driverClass = "com.databricks.client.jdbc.Driver", classPath = "C:\\Users\\janus\\Downloads\\DatabricksJDBC42-2.7.1.1004\\DatabricksJDBC42.jar",  identifier.quote="`")
  conn <- dbConnect(drv, "jdbc:databricks://adb-935665325582595.15.azuredatabricks.net:443/default;transportMode=http;ssl=1;httpPath=/sql/1.0/warehouses/eb457c7eaf3144aa;AuthMech=3;UID=token;PWD=dapi137c8292a3d39c24e7229da163c4050d-3")
  print(dbGetQuery(conn, "SELECT count(1) FROM db_reclamaciones.dbo_encabezado_reclamaciones_juridicas;"))
}

# 2. Exploración general ---- 
dbGetQuery(conn, "SELECT COUNT(*) AS total FROM db_reclamaciones.dbo_encabezado_reclamaciones_juridicas")

# Fechas mínimas y máximas de radicación
dbGetQuery(conn, "
  SELECT MIN(fecha_radicacion) AS fecha_min,
         MAX(fecha_radicacion) AS fecha_max
  FROM db_reclamaciones.dbo_encabezado_reclamaciones_juridicas
")

# Conteo por RECLAMACIONES ID
dbGetQuery(conn, "
  SELECT RECLAMACIONID, COUNT(*) AS total
  FROM db_reclamaciones.dbo_encabezado_reclamaciones_juridicas
  GROUP BY RECLAMACIONID
  ORDER BY total ASC
")

# Conteo por sexo
dbGetQuery(conn, "
  SELECT sexo, COUNT(*) AS total
  FROM db_reclamaciones.dbo_encabezado_reclamaciones_juridicas
  GROUP BY sexo
  ORDER BY total ASC
")

# Conteo por  tipo doc reclamente
dbGetQuery(conn, "
  SELECT tipo_doc_reclamante, COUNT(*) AS total
  FROM db_reclamaciones.dbo_encabezado_reclamaciones_juridicas
  GROUP BY tipo_doc_reclamante
  ORDER BY total ASC
")

# Conteo por RECLAMACIONES ID
dbGetQuery(conn, "
  SELECT tipo_formulario, COUNT(*) AS total
  FROM db_reclamaciones.dbo_encabezado_reclamaciones_juridicas
  GROUP BY tipo_formulario
  ORDER BY total ASC
")

# Grafico 1. Conteo mensual de Reclamaciones Nuevas
query_fechas_mensuales <- "
SELECT 
  fecha_radicacion, 
  RECLAMACIONID
FROM db_reclamaciones.dbo_encabezado_reclamaciones_juridicas
WHERE presentacion = 'Nueva'
  AND fecha_radicacion IS NOT NULL
"
all_dates_df <- dbGetQuery(conn, query_fechas_mensuales)
# Validar y graficar
if (nrow(all_dates_df) > 0) {
  message("Procesando fechas para conteo mensual...")
  all_dates_df <- all_dates_df %>%
    mutate(
      fecha_radicacion = as.Date(fecha_radicacion),
      anio = year(fecha_radicacion),
      mes = month(fecha_radicacion, label = TRUE, abbr = TRUE)
    ) %>%
    group_by(anio, mes) %>%
    summarise(conteo = n_distinct(RECLAMACIONID), .groups = "drop") %>%
    group_by(anio) %>%
    filter(n() > 3) %>%  # Filtrar años con pocos datos mensuales
    ungroup()
  message("Generando gráfico de barras mensuales por año...")
  # Crear el gráfico y guardarlo en un objeto
  grafico_mensual <- ggplot(all_dates_df, aes(x = mes, y = conteo)) +
    geom_col(fill = "#32B0A5", color = "black", width = 0.7) +
    facet_wrap(~ anio, scales = "free_y") +
    labs(
      title = "Conteo mensual de Reclamaciones Nuevas",
      subtitle = "Anual",
      x = "Mes",
      y = "Número de Reclamaciones"
    ) +
    theme_minimal() +
    theme(strip.text = element_text(size = 10, face = "bold"))
  
  print(grafico_mensual)
} else {
  message("No hay datos para graficar.")
}
ggsave("grafico_conteo_mensual.png", plot = grafico_mensual, dpi = 300, width = 10, height = 6)


#Grafico 2. Número Total de Reclamaciones

query_total <- "
SELECT 
  nombre_reclamante,
  COUNT(RECLAMACIONID) AS total_reclamaciones
FROM db_reclamaciones.dbo_encabezado_reclamaciones_juridicas
WHERE presentacion = 'Nueva'
  AND fecha_radicacion IS NOT NULL
  AND nombre_reclamante IS NOT NULL
GROUP BY nombre_reclamante
"

df_total <- dbGetQuery(conn, query_total) %>%
  mutate(total_reclamaciones = as.numeric(total_reclamaciones)) %>%
  filter(nombre_reclamante != "", !is.na(nombre_reclamante)) %>%
  arrange(desc(total_reclamaciones)) %>%
  slice_head(n = 10)

grafico_total <- ggplot(df_total, aes(x = total_reclamaciones, y = reorder(nombre_reclamante, total_reclamaciones))) +
  geom_col(fill = "#32B0A5") +
  geom_label(
    aes(label = paste0(round(total_reclamaciones / 1000, 1), "K")),
    hjust = -0.1, size = 3.5, fill = "grey90", label.size = 0.2
  ) +
  labs(
    title = "Top 10 IPS Reclamantes por total de Reclamaciones",
    subtitle = "2017 a 2025",
    x = "Número Total de Reclamaciones",
    y = "IPS Reclamante"
  ) +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 9), plot.margin = margin(5, 40, 5, 5)) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.15)))
grafico_total

ggsave("top10_total_reclamaciones.png", grafico_total, dpi = 300, width = 10, height = 6)


#promedio RECLAMACIONID
query_promedio <- "
SELECT 
  nombre_reclamante,
  YEAR(fecha_radicacion) AS anio,
  COUNT(RECLAMACIONID) AS total_reclamaciones
FROM db_reclamaciones.dbo_encabezado_reclamaciones_juridicas
WHERE presentacion = 'Nueva'
  AND fecha_radicacion IS NOT NULL
  AND nombre_reclamante IS NOT NULL
GROUP BY nombre_reclamante, YEAR(fecha_radicacion)
"

df_promedio <- dbGetQuery(conn, query_promedio) %>%
  mutate(total_reclamaciones = as.numeric(total_reclamaciones)) %>%
  filter(nombre_reclamante != "", !is.na(nombre_reclamante)) %>%
  group_by(nombre_reclamante) %>%
  summarise(promedio_anual = mean(total_reclamaciones), .groups = "drop") %>%
  arrange(desc(promedio_anual)) %>%
  slice_head(n = 10)

grafico_promedio <- ggplot(df_promedio, aes(x = promedio_anual, y = reorder(nombre_reclamante, promedio_anual))) +
  geom_col(fill = "#32B0A5") +
  geom_label(
    aes(label = paste0(round(promedio_anual / 1000, 1), "K")),
    hjust = -0.1, size = 3.5, fill = "grey90", label.size = 0.2
  ) +
  labs(
    title = "Top 10 IPS Reclamantes por Promedio de Reclamaciones",
    subtitle = "2017 a 2025",
    x = "Número promedio de Reclamaciones",
    y = "IPS Reclamante"
  ) +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 9), plot.margin = margin(5, 40, 5, 5)) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.15)))
grafico_promedio
ggsave("top10_promedio_anual_reclamaciones.png", grafico_promedio, dpi = 300, width = 10, height = 6)

# Consulta SQL
query_identificados <- "
SELECT 
  placa,
  fecha_radicacion,
  RECLAMACIONID
FROM db_reclamaciones.dbo_encabezado_reclamaciones_juridicas
WHERE presentacion = 'Nueva'
  AND fecha_radicacion IS NOT NULL
"

# Ejecutar consulta
df_placa <- dbGetQuery(conn, query_identificados)

# Procesar datos y calcular porcentajes
df_placa <- df_placa %>%
  mutate(
    fecha_radicacion = as.Date(fecha_radicacion),
    anio = lubridate::year(fecha_radicacion),
    estado_placa = ifelse(is.na(placa) | placa == "", "No Identificado", "Identificado")
  ) %>%
  group_by(anio, estado_placa) %>%
  summarise(n = n_distinct(RECLAMACIONID), .groups = "drop") %>%
  group_by(anio) %>%
  mutate(
    total = sum(n),
    porcentaje = n / total,
    etiqueta = paste0(round(porcentaje * 100, 1), "%")
  ) %>%
  ungroup()

# Colores personalizados
colores <- c("Identificado" = "#32B0A5", "No Identificado" = "#19B9F7")

# Gráfico

participacion1 <- ggplot(df_placa, aes(x = factor(anio), y = porcentaje, fill = estado_placa)) +
  geom_col(position = "fill", width = 0.7) +
  geom_label(
    aes(label = etiqueta),
    position = position_fill(vjust = 0.5),
    fill = "grey90",
    size = 3.5,
    label.size = 0.2
  ) +
  scale_fill_manual(values = colores) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    title = "Participación de Reclamaciones Nuevas por estado de la placa",
    x = "Año de Radicación",
    y = "Porcentaje sobre el Total Anual",
    fill = "Estado de Placa"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 9),
    legend.title = element_text(face = "bold"),
    legend.position = "bottom"
  )

ggsave("nuevas_id_noid.png", participacion1, dpi = 300, width = 10, height = 6)







rm(list = ls()); gc()
{
  # 0. Librerias y memoria ----
  Sys.setlocale("LC_ALL", "es_ES.UTF-8")
  
  library(ggplot2)
  library(tidyr)
  library(dplyr)
  library(lubridate)
  library(stringr)
  library(stringi)
  library(remotes)
  library(scales)
  library(solitude) #isolation forest
  library(DBI)
  library(RJDBC)
  library(dplyr)
  library(rJava)
  "solitude" %in% (.packages())  # Debe devolver TRUE; isolation_forest esté en el paquete
  ls("package:solitude")
  
  Sys.which("make")
  options(scipen=999)
  options(java.parameters = c(
    "--add-opens=java.base/java.nio=ALL-UNNAMED",
    "-Xms512m", "-Xmx16g"
  ))
  Sys.getenv("JAVA_TOOL_OPTIONS")
  .jinit()
  runtime <- .jcall("java/lang/Runtime", "Ljava/lang/Runtime;", "getRuntime")
  max_mem <- .jcall(runtime, "J", "maxMemory")
  cat("Java Max Memory:", round(max_mem / (1024^3), 2), "GB\n")
  options(java.parameters = "-Xmx16g")  # o -Xmx12g si tienes menos RAM
}

# 1. Carga datos conectado al Data bricks ----
# Diego.Lopez escritorio ADRES
# dleol portatil
# Base Reclamaciones
{
  drv <- JDBC(driverClass = "com.databricks.client.jdbc.Driver", classPath = "C:\\Users\\Diego.Lopez\\Downloads\\DatabricksJDBC42-2.7.1.1004\\DatabricksJDBC42.jar",  identifier.quote="`")
  conn <- dbConnect(drv, "jdbc:databricks://adb-935665325582595.15.azuredatabricks.net:443/default;transportMode=http;ssl=1;httpPath=/sql/1.0/warehouses/eb457c7eaf3144aa;AuthMech=3;UID=token;PWD=dapi137c8292a3d39c24e7229da163c4050d-3")
  print(dbGetQuery(conn, "SELECT count(1) FROM db_reclamaciones.dbo_encabezado_reclamaciones_juridicas;"))
}



# Isolation Forest ----

## Versión 1 ---- 
print("Iniciando conteo de RECLAMACIONID por Codigo_Habilitacion...")
query_conteo_habilitacion <- "
 SELECT Codigo_Habilitacion, COUNT(RECLAMACIONID) AS Conteo_Reclamaciones
FROM db_reclamaciones.dbo_encabezado_reclamaciones_juridicas
GROUP BY Codigo_Habilitacion
 ORDER BY COUNT(RECLAMACIONID) DESC -- Ordenar por conteo descendente
 "
conteo_habilitacion_df <- dbGetQuery(conn, query_conteo_habilitacion)
print(paste("Se obtuvieron conteos para", nrow(conteo_habilitacion_df), "Codigo_Habilitacion."))


query_agregado_ips <- "
SELECT 
  Codigo_Habilitacion,
  SUM(Valor_Total_Aprobado) AS total_aprobado,
  SUM(Gastos_transporte) AS total_transporte,
  SUM(gastos_medicos_quirurgicos) AS total_medico
FROM db_reclamaciones.dbo_encabezado_reclamaciones_juridicas
WHERE presentacion = 'Nueva'
GROUP BY Codigo_Habilitacion
"
base_ips <- dbGetQuery(conn, query_agregado_ips)

base_ips_clean <- base_ips %>%
  mutate(across(c(total_aprobado, total_transporte, total_medico), as.numeric)) %>%
  filter(if_all(c(total_aprobado, total_transporte, total_medico), ~ !is.na(.)))
ids <- base_ips_clean$Codigo_Habilitacion
# Escalar las variables
base_scaled <- scale(base_ips_clean %>% select(-Codigo_Habilitacion)) %>% as.data.frame()

# Entrenar Isolation Forest
iforest_model <- isolationForest$new(sample_size = nrow(base_scaled), num_trees = 100)
iforest_model$fit(base_scaled)
predicciones <- iforest_model$predict(base_scaled) # Predicciones

# Resultado final
resultados_iforest <- cbind(
  Codigo_Habilitacion = ids,
  base_ips_clean %>% select(-Codigo_Habilitacion),
  predicciones
) %>%
  mutate(predicted_is_anomaly = ifelse(anomaly_score > quantile(anomaly_score, 0.95), 1, 0))  # top 5% como outliers

# Visualización básica
if1 <- ggplot(resultados_iforest, aes(
  x = total_medico,
  y = total_aprobado,
  size = total_transporte,
  color = factor(predicted_is_anomaly))
) +
  geom_point(alpha = 0.7) +
  scale_color_manual(values = c("0" = "blue", "1" = "red"),
                     labels = c("Normal", "Outlier")) +
  labs(
    title = "Detección de anomalías en IPS",
    x = "Total gastos médicos",
    y = "Total valor aprobado",
    size = "Transporte",
    color = "Clasificación"
  ) +
  theme_minimal()
ggsave("if1.png", if1, dpi = 300, width = 10, height = 6)


outliers <- resultados_iforest %>% filter(predicted_is_anomaly == 1)
head(outliers[order(-outliers$anomaly_score), ])

library(tidyr)

## Versión 2 ---- 
query <- "
SELECT 
  Codigo_Habilitacion,
  Valor_Total_Aprobado,
  Gastos_transporte,
  gastos_medicos_quirurgicos,
  aseguramiento
FROM db_reclamaciones.dbo_encabezado_reclamaciones_juridicas
WHERE presentacion = 'Nueva'
  AND Codigo_Habilitacion IS NOT NULL
"
datos <- dbGetQuery(conn, query)

# Codificar aseguramiento binario y limpiar datos
datos <- datos %>%
  mutate(
    aseguramiento_binario = case_when(
      aseguramiento %in% c("Asegurado", "Asegurado D.2497", "Póliza falsa", "No asegurado") ~ 1,
      aseguramiento %in% c("Vehículo fantasma", "No Asegurado - Sin Placa", 
                           "No asegurado Propietario Indeterminado", "Vehículo en fuga") ~ 0,
      TRUE ~ NA_real_
    ),
    across(c(Valor_Total_Aprobado, Gastos_transporte, gastos_medicos_quirurgicos), as.numeric)
  ) %>%
  filter(!is.na(aseguramiento_binario))

agregado <- datos %>%
  group_by(Codigo_Habilitacion, aseguramiento_binario) %>%
  summarise(
    total_valor = sum(Valor_Total_Aprobado, na.rm = TRUE),
    total_transporte = sum(Gastos_transporte, na.rm = TRUE),
    total_medico = sum(gastos_medicos_quirurgicos, na.rm = TRUE),
    .groups = "drop"
  )

# Pivot a formato ancho
agregado_wide <- agregado %>%
  pivot_wider(
    names_from = aseguramiento_binario,
    values_from = c(total_valor, total_transporte, total_medico),
    values_fill = 0,
    names_sep = "_aseg_"
  )

# Calcular totales y porcentajes
agregado_final <- agregado_wide %>%
  mutate(
    total_valor = total_valor_aseg_0 + total_valor_aseg_1,
    pct_valor_0 = round(100 * total_valor_aseg_0 / total_valor, 2),
    pct_valor_1 = round(100 * total_valor_aseg_1 / total_valor, 2),
    
    total_transporte = total_transporte_aseg_0 + total_transporte_aseg_1,
    pct_transporte_0 = round(100 * total_transporte_aseg_0 / total_transporte, 2),
    pct_transporte_1 = round(100 * total_transporte_aseg_1 / total_transporte, 2),
    
    total_medico = total_medico_aseg_0 + total_medico_aseg_1,
    pct_medico_0 = round(100 * total_medico_aseg_0 / total_medico, 2),
    pct_medico_1 = round(100 * total_medico_aseg_1 / total_medico, 2)
  ) %>%
  select(
    Codigo_Habilitacion,
    total_valor, total_valor_aseg_1, total_valor_aseg_0, pct_valor_1, pct_valor_0,
    total_transporte, total_transporte_aseg_1, total_transporte_aseg_0, pct_transporte_1, pct_transporte_0,
    total_medico, total_medico_aseg_1, total_medico_aseg_0, pct_medico_1, pct_medico_0
  )

# Mostrar resultado final
head(agregado_final)

table(base_modelo$Codigo_Habilitacion)

## Versión 3 ---- 
query <- "
SELECT 
  Codigo_Habilitacion,
  Valor_Total_Aprobado,
  Gastos_transporte,
  gastos_medicos_quirurgicos,
  RECLAMACIONID,
  aseguramiento
FROM db_reclamaciones.dbo_encabezado_reclamaciones_juridicas
WHERE presentacion = 'Nueva'
  AND Codigo_Habilitacion IS NOT NULL
  AND Valor_Total_Aprobado IS NOT NULL
"
datos <- datos %>%
  mutate(
    aseguramiento_binario = case_when(
      aseguramiento %in% c("Asegurado", "Asegurado D.2497", "Póliza falsa", "No asegurado") ~ 1,
      aseguramiento %in% c("Vehículo fantasma", "No Asegurado - Sin Placa", 
                           "No asegurado Propietario Indeterminado", "Vehículo en fuga") ~ 0,
      TRUE ~ NA_real_
    ),
    across(c(Valor_Total_Aprobado, Gastos_transporte, gastos_medicos_quirurgicos), as.numeric)
  ) %>%
  filter(!is.na(aseguramiento_binario))

# Agregado por IPS y aseguramiento
base_agregada <- datos %>%
  group_by(Codigo_Habilitacion, aseguramiento_binario) %>%
  summarise(valor_total = sum(Valor_Total_Aprobado, na.rm = TRUE), .groups = "drop")

# Pivot 
base_pivot <- base_agregada %>%
  pivot_wider(
    names_from = aseguramiento_binario,
    values_from = valor_total,
    names_prefix = "valor_aseg_",
    values_fill = 0
  )

#Calcular totales  % 
base_final <- base_pivot %>%
  mutate(
    total_valor = valor_aseg_0 + valor_aseg_1,
    pct_aseguramiento_0 = round(100 * valor_aseg_0 / total_valor, 2),
    pct_aseguramiento_1 = round(100 * valor_aseg_1 / total_valor, 2)
  )

# Agregar variables por IPS (reclamaciones + gastos)
adicionales_por_ips <- datos %>%
  group_by(Codigo_Habilitacion) %>%
  summarise(
    conteo_reclamaciones = n_distinct(RECLAMACIONID),
    total_transporte = sum(Gastos_transporte, na.rm = TRUE),
    total_medico = sum(gastos_medicos_quirurgicos, na.rm = TRUE),
    .groups = "drop"
  )

# Unir
base_modelo <- base_final %>%
  inner_join(adicionales_por_ips, by = "Codigo_Habilitacion") %>%
  select(
    Codigo_Habilitacion,
    pct_aseguramiento_1,
    total_valor, total_transporte, total_medico,
    conteo_reclamaciones
  ) %>%
  drop_na()

datos_escalados <- base_modelo %>% #escalar
  select(-Codigo_Habilitacion) %>%
  scale() %>%
  as.data.frame()

# Isolation Forest
modelo_iforest <- isolationForest$new(sample_size = nrow(datos_escalados), num_trees = 100)
modelo_iforest$fit(datos_escalados)
predicciones <- modelo_iforest$predict(datos_escalados)

resultados_iforest <- base_modelo %>%
  mutate(
    anomaly_score = predicciones$anomaly_score,
    is_anomaly = ifelse(anomaly_score > quantile(anomaly_score, 0.95), 1, 0)
  )



quantile(resultados_iforest$anomaly_score, 0.95)

print("Outliers detectados por distribución de aseguramiento y gasto total:")
print(resultados_iforest %>% filter(is_anomaly == 1) %>% arrange(desc(anomaly_score)))

resultados_iforest_final <- resultados_iforest %>% filter(is_anomaly == 1)
F_1<-ecdf(resultados_iforest_final$total_valor)
F_2<-ecdf(resultados_iforest_final$conteo_reclamaciones)
F_3<-ecdf(resultados_iforest_final$total_medico)

F_1(resultados_iforest_final$total_valor)
F_2(resultados_iforest_final$conteo_reclamaciones)
F_3(resultados_iforest_final$total_medico)

# Calcular el percentil 95
umbral_95 <- quantile(resultados_iforest$anomaly_score, 0.95)

# Agregar índice para graficar
resultados_iforest <- resultados_iforest %>%
  arrange(anomaly_score) %>%
  mutate(indice = row_number())

# Graficar los anomaly_score ordenados
library(ggplot2)

ggplot(resultados_iforest, aes(x = indice, y = anomaly_score)) +
  geom_line(color = "orange") +
  geom_point(color = "darkorange") +
  geom_hline(yintercept = umbral_95, color = "red", linetype = "dashed") +
  labs(
    title = "Anomaly Scores ordenados (Isolation Forest)",
    x = "Observaciones ordenadas",
    y = "Anomaly Score"
  ) +
  annotate("text", x = max(resultados_iforest$indice) * 0.7, y = umbral_95 + 0.01,
           label = paste("Umbral 95%:", round(umbral_95, 2)), color = "red") +
  theme_minimal()


resultados_iforest <- resultados_iforest %>%
  arrange(anomaly_score) %>%
  mutate(indice = row_number())

ggplot(resultados_iforest, aes(x = indice, y = anomaly_score)) +
  geom_line(color = "blue") +
  geom_hline(yintercept = quantile(resultados_iforest$anomaly_score, 0.95), 
             color = "red", linetype = "dashed") +
  labs(
    title = "Anomaly Scores ordenados (Isolation Forest)",
    x = "Observaciones ordenadas",
    y = "Anomaly Score"
  ) +
  theme_minimal()


ggplot(resultados_iforest, aes(
  x = total_valor,
  y = conteo_reclamaciones,
  color = factor(is_anomaly))
) +
  geom_point(size = 2, alpha = 0.7) +
  scale_color_manual(values = c("0" = "blue", "1" = "red"),
                     labels = c("Normal", "Outlier")) +
  labs(
    title = "Análisis de outliers por IPS",
    x = "Valor total aprobado",
    y = "Número de reclamaciones",
    color = "Clasificación"
  ) +
  theme_minimal()

library(writexl)
write_xlsx(
  resultados_iforest %>% filter(is_anomaly == 1) %>% arrange(desc(anomaly_score)),
  path = "outliers_iforest.xlsx"
)



library(GGally)
resultados_plot <- resultados_iforest %>%
  select(pct_aseguramiento_1, total_valor, total_transporte, total_medico, conteo_reclamaciones, is_anomaly)

GGally::ggpairs(
  resultados_plot,
  aes(color = factor(is_anomaly), alpha = 0.6),
  upper = list(continuous = wrap("points", size = 1.5))
)

pca <- prcomp(datos_escalados, center = TRUE, scale. = TRUE)
pca_df <- as.data.frame(pca$x[, 1:2]) %>%
  mutate(is_anomaly = factor(resultados_iforest$is_anomaly))

ggplot(pca_df, aes(x = PC1, y = PC2, color = is_anomaly)) +
  geom_point(alpha = 0.6) +
  scale_color_manual(values = c("0" = "gray40", "1" = "red")) +
  labs(
    title = "Outliers visualizados con PCA (todas las variables)",
    x = "Componente Principal 1",
    y = "Componente Principal 2",
    color = "Outlier"
  ) +
  theme_minimal()


ggplot(resultados_iforest, aes(
  x = pct_aseguramiento_1,
  y = total_valor,
  size = total_medico,
  color = factor(is_anomaly))
) +
  geom_point(alpha = 0.7) +
  scale_color_manual(values = c("0" = "blue", "1" = "red")) +
  labs(
    title = "Outliers con múltiples variables",
    x = "% Aseguramiento identificado",
    y = "Valor total aprobado",
    size = "Gasto médico",
    color = "Outlier"
  ) +
  theme_minimal()


## Versión 4 ----

# Consulta base
query <- "
SELECT 
  Codigo_Habilitacion,
  Valor_Total_Aprobado,
  Gastos_transporte,
  gastos_medicos_quirurgicos,
  RECLAMACIONID,
  placa
FROM db_reclamaciones.dbo_encabezado_reclamaciones_juridicas
WHERE presentacion = 'Nueva'
  AND Codigo_Habilitacion IS NOT NULL
  AND Valor_Total_Aprobado IS NOT NULL
"

datos <- dbGetQuery(conn, query)

# Clasificación binaria por placa
datos <- datos %>%
  mutate(
    placa_binaria = ifelse(is.na(placa) | placa == "", 0, 1),
    across(c(Valor_Total_Aprobado, Gastos_transporte, gastos_medicos_quirurgicos), as.numeric)
  ) %>%
  filter(!is.na(placa_binaria))

table(datos$placa_binaria)

# Agregado por IPS y estado de placa
base_agregada <- datos %>%
  group_by(Codigo_Habilitacion, placa_binaria) %>%
  summarise(valor_total = sum(Valor_Total_Aprobado, na.rm = TRUE), .groups = "drop")

# Pivot para tener columnas separadas por placa_binaria
base_pivot <- base_agregada %>%
  pivot_wider(
    names_from = placa_binaria,
    values_from = valor_total,
    names_prefix = "valor_placa_",
    values_fill = 0
  )

# Calcular totales y porcentajes
base_final <- base_pivot %>%
  mutate(
    total_valor = valor_placa_0 + valor_placa_1,
    pct_identificado = round(100 * valor_placa_1 / total_valor, 2),
    pct_no_identificado = round(100 * valor_placa_0 / total_valor, 2)
  )

# Variables adicionales por IPS
adicionales_por_ips <- datos %>%
  group_by(Codigo_Habilitacion) %>%
  summarise(
    conteo_reclamaciones = n_distinct(RECLAMACIONID),
    total_transporte = sum(Gastos_transporte, na.rm = TRUE),
    total_medico = sum(gastos_medicos_quirurgicos, na.rm = TRUE),
    .groups = "drop"
  )

# Unir todo
base_modelo <- base_final %>%
  inner_join(adicionales_por_ips, by = "Codigo_Habilitacion") %>%
  select(
    Codigo_Habilitacion,
    pct_no_identificado,
    total_valor, total_transporte, total_medico,
    conteo_reclamaciones
  ) %>%
  drop_na()

# Escalamiento
datos_escalados <- base_modelo %>%
  select(-Codigo_Habilitacion) %>%
  scale() %>%
  as.data.frame()

# Isolation Forest
modelo_iforest <- isolationForest$new(sample_size = nrow(datos_escalados), num_trees = 100)
modelo_iforest$fit(datos_escalados)
predicciones <- modelo_iforest$predict(datos_escalados)

# Resultado final
resultados_iforest4 <- base_modelo %>%
  mutate(
    anomaly_score = predicciones$anomaly_score,
    is_anomaly = ifelse(anomaly_score > quantile(anomaly_score, 0.95), 1, 0)
  )
resultados_iforest4

resultados_iforest4 <- resultados_iforest4 %>%
  mutate(
    categoria_anomalia = ifelse(is_anomaly == 1, "1", "0")
  )



library(openxlsx)

# Consulta base incluyendo fecha y nombre
query <- "
SELECT 
  Codigo_Habilitacion,
  nombre_reclamante,
  Valor_Total_Aprobado,
  Gastos_transporte,
  gastos_medicos_quirurgicos,
  RECLAMACIONID,
  placa,
  fecha_radicacion
FROM db_reclamaciones.dbo_encabezado_reclamaciones_juridicas
WHERE presentacion = 'Nueva'
  AND Codigo_Habilitacion IS NOT NULL
  AND Valor_Total_Aprobado IS NOT NULL
  AND fecha_radicacion IS NOT NULL
"

# Cargar datos
datos_full <- dbGetQuery(conn, query)

# Procesamiento general
datos_full <- datos_full %>%
  mutate(
    fecha_radicacion = as.Date(substr(fecha_radicacion, 1, 10)),
    anio = year(fecha_radicacion),
    placa_binaria = ifelse(is.na(placa) | placa == "", 0, 1),
    across(c(Valor_Total_Aprobado, Gastos_transporte, gastos_medicos_quirurgicos), as.numeric)
  )

# Asociar nombre_reclamante por Codigo_Habilitacion
nombres_ips <- datos_full %>%
  filter(!is.na(nombre_reclamante), nombre_reclamante != "") %>%
  group_by(Codigo_Habilitacion) %>%
  summarise(nombre_reclamante = first(nombre_reclamante), .groups = "drop")

# Función para procesar y modelar por año
procesar_anio <- function(datos_anio) {
  base_agregada <- datos_anio %>%
    group_by(Codigo_Habilitacion, placa_binaria) %>%
    summarise(valor_total = sum(Valor_Total_Aprobado, na.rm = TRUE), .groups = "drop")
  
  base_pivot <- base_agregada %>%
    pivot_wider(
      names_from = placa_binaria,
      values_from = valor_total,
      names_prefix = "valor_placa_",
      values_fill = 0
    )
  
  base_final <- base_pivot %>%
    mutate(
      total_valor = valor_placa_0 + valor_placa_1,
      pct_identificado = round(100 * valor_placa_1 / total_valor, 2),
      pct_no_identificado = round(100 * valor_placa_0 / total_valor, 2)
    )
  
  adicionales <- datos_anio %>%
    group_by(Codigo_Habilitacion) %>%
    summarise(
      conteo_reclamaciones = n_distinct(RECLAMACIONID),
      total_transporte = sum(Gastos_transporte, na.rm = TRUE),
      total_medico = sum(gastos_medicos_quirurgicos, na.rm = TRUE),
      .groups = "drop"
    )
  
  base_modelo <- base_final %>%
    inner_join(adicionales, by = "Codigo_Habilitacion") %>%
    select(
      Codigo_Habilitacion,
      pct_no_identificado,
      total_valor, total_transporte, total_medico,
      conteo_reclamaciones
    ) %>%
    drop_na()
  
  if (nrow(base_modelo) >= 10) {
    datos_escalados <- base_modelo %>%
      select(-Codigo_Habilitacion) %>%
      scale() %>%
      as.data.frame()
    
    modelo_iforest <- isolationForest$new(sample_size = nrow(datos_escalados), num_trees = 100)
    modelo_iforest$fit(datos_escalados)
    predicciones <- modelo_iforest$predict(datos_escalados)
    
    base_modelo %>%
      mutate(
        anomaly_score = predicciones$anomaly_score,
        is_anomaly = ifelse(anomaly_score > quantile(anomaly_score, 0.95), 1, 0),
        categoria_anomalia = ifelse(is_anomaly == 1, "Anomalía", "Normal")
      ) %>%
      left_join(nombres_ips, by = "Codigo_Habilitacion") %>%
      relocate(nombre_reclamante, .after = Codigo_Habilitacion)
  } else {
    return(NULL)
  }
}

# Crear lista para almacenar resultados anómalos por año
lista_anomalias_por_anio <- list()

# Loop por año: modelar, filtrar, exportar y guardar en R
for (anio_actual in 2017:2025) {
  datos_anio <- datos_full %>% filter(anio == anio_actual)
  
  resultado_anio <- procesar_anio(datos_anio)
  
  if (!is.null(resultado_anio)) {
    resultado_filtrado <- resultado_anio %>%
      filter(is_anomaly == 1) %>%
      mutate(año = anio_actual) %>%
      relocate(año, .after = Codigo_Habilitacion)
    
    
    if (nrow(resultado_filtrado) > 0) {
      # Exportar archivo Excel
      write.xlsx(
        resultado_filtrado,
        file = paste0("anomalias_iforest_año_", anio_actual, ".xlsx")
      )
      
      # Guardar en lista por año
      lista_anomalias_por_anio[[as.character(anio_actual)]] <- resultado_filtrado
    } else {
      message("Sin anomalías detectadas para el año ", anio_actual)
    }
  } else {
    message("No hay suficientes datos para el año ", anio_actual)
  }
}

# Unir todos los años en un solo data.frame con columna "año"
anomalias_todos_los_anios <- bind_rows(lista_anomalias_por_anio, .id = "año")

lista_anomalias_por_anio <- lapply(lista_anomalias_por_anio, function(df) {
  df$año <- as.character(df$año)
  return(df)
})

anomalias_todos_los_anios <- bind_rows(lista_anomalias_por_anio)

anomalias_todos_los_anios <- as.data.frame(anomalias_todos_los_anios)
write.xlsx(anomalias_todos_los_anios, file = "Consolidado_anomalias.xlsx")

?write.xlsx
# Guardar la lista y el data frame como archivos .rds
saveRDS(lista_anomalias_por_anio, file = "lista_anomalias_iforest_por_año.rds")
saveRDS(anomalias_todos_los_anios, file = "anomalias_iforest_todos_los_años.rds")

