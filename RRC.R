
# ejemplo 1 ---------------------------------------------------------------


# Cargar librerías necesarias
library(dplyr)

# Definir los datos de las pólizas
crear_datos_polidas <- function() {
  data.frame(
    Tipo_Poliza = c("Auto", "Salud", "Hogar", "Vida", "Empresarial"),
    Primas_Emitidas = c(800000, 500000, 300000, 400000, 600000),
    Porcentaje_Devengadas = c(0.70, 0.60, 0.20, 0.40, 0.50),
    Duracion_Meses = c(12, 12, 12, 24, 12),
    Tasa_Siniestralidad = c(0.30, 0.25, 0.15, 0.10, 0.20)
  )
}

# Calcular las primas devengadas y no devengadas
calcular_primas <- function(df) {
  df %>%
    mutate(
      Primas_Devengadas = Primas_Emitidas * Porcentaje_Devengadas,
      Primas_No_Devengadas = Primas_Emitidas - Primas_Devengadas,
      Reserva_Riesgos_en_Curso = Primas_No_Devengadas * (Duracion_Meses / 12),
      Costo_Siniestral = Primas_Devengadas * Tasa_Siniestralidad,
      Reserva_Final = Reserva_Riesgos_en_Curso - Costo_Siniestral
    )
}

# Mostrar resultados
mostrar_resultados <- function(df) {
  print(df)
  reserva_total <- sum(df$Reserva_Final)
  cat("Reserva Total de Riesgos en Curso Ajustada por Siniestralidad:", reserva_total, "\n")
}

# Función principal
main <- function() {
  polizas <- crear_datos_polidas()
  polizas_calculadas <- calcular_primas(polizas)
  mostrar_resultados(polizas_calculadas)
}

# Ejecutar la función principal
main()


# ejemplo 2 ---------------------------------------------------------------



# Datos de las pólizas
polizas <- data.frame(
  Tipo_Poliza = c("Auto", "Salud", "Hogar"),
  Primas_Emitidas = c(500000, 300000, 400000),
  Porcentaje_Devengadas = c(0.50, 0.70, 0.40)
)

# Calcular las primas devengadas y no devengadas
polizas <- polizas %>%
  mutate(
    Primas_Devengadas = Primas_Emitidas * Porcentaje_Devengadas,
    Primas_No_Devengadas = Primas_Emitidas - Primas_Devengadas,
    Reserva_Riesgos_en_Curso = Primas_No_Devengadas
  )

# Imprimir resultados
print(polizas)

# Calcular la reserva total de riesgos en curso
reserva_total <- sum(polizas$Reserva_Riesgos_en_Curso)
cat("Reserva Total de Riesgos en Curso:", reserva_total, "\n")



# ejemplo 3 ---------------------------------------------------------------


# Datos de las pólizas
polizas <- data.frame(
  Tipo_Poliza = c("Auto", "Salud", "Hogar", "Vida"),
  Primas_Emitidas = c(600000, 400000, 500000, 300000),
  Porcentaje_Devengadas = c(0.60, 0.80, 0.30, 0.50),
  Duracion_Meses = c(12, 12, 12, 24)
)

# Calcular las primas devengadas y no devengadas
polizas <- polizas %>%
  mutate(
    Primas_Devengadas = Primas_Emitidas * Porcentaje_Devengadas,
    Primas_No_Devengadas = Primas_Emitidas - Primas_Devengadas,
    Reserva_Riesgos_en_Curso = Primas_No_Devengadas * (Duracion_Meses / 12)  # Ajuste según duración
  )

# Imprimir resultados
print(polizas)

# Calcular la reserva total de riesgos en curso
reserva_total <- sum(polizas$Reserva_Riesgos_en_Curso)
cat("Reserva Total de Riesgos en Curso:", reserva_total, "\n")





