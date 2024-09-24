###########################################################################
###########################################################################
############################     SONR      ################################
###########################################################################
###########################################################################


# Reserva de siniestros ocurridos y no reportados -------------------------
# en inglés "Incurred but not reported" IBNR ------------------------------


# siniestros que han ocurrido pero no han sido comunicados a la entidad 
# aseguradora durante la anualidad de su ocurrencia.


# importamos los datos 

# install.packages('pacman') 
pacman::p_load(dplyr,lubridate,readxl)

# 
# dir <- 'D:/Daira García/Documents/daños'
# 
# datae1 <- read_excel(file.path(dir,'datas_daños.xlsx'),
                     # sheet = 'E1SONR')

###########################################################################
##########################     LINK-RATION    #############################
###########################################################################

# Crear un dataframe de ejemplo
reclamaciones <- data.frame(
  Año_Ocurrencia = c(2021, 2022, 2023),
  Año_0 = c(100, 120, 130),
  Año_1 = c(150, 200, NA),
  Año_2 = c(180, NA, NA)
)


# Función para calcular los Link Ratios
calcular_link_ratios <- function(df) {
  ratios <- numeric(ncol(df) - 2)  # Sin contar las columnas de año
  for (i in 1:(ncol(df) - 2)) {
    ratios[i] <- mean(df[!is.na(df[, i + 2]), i + 2] / df[!is.na(df[, i + 1]), i + 1], na.rm = TRUE)
  }
  return(ratios)
}

link_ratios <- calcular_link_ratios(reclamaciones)
link_ratios

# Proyectar reclamaciones
proyectar_reclamaciones <- function(df, ratios) {
  df_proyectado <- df
  for (i in 1:(ncol(df) - 2)) {
    if (any(is.na(df[, i + 2]))) {
      df_proyectado[is.na(df[, i + 2]), i + 2] <- df_proyectado[is.na(df[, i + 2]), i + 1] * ratios[i]
    }
  }
  return(df_proyectado
  )
}

reclamaciones_proyectadas <- proyectar_reclamaciones(reclamaciones, link_ratios)
reclamaciones_proyectadas

# Calcular la reserva IBNR
calcular_ibnr <- function(df_proyectado) {
  ibnr <- numeric(nrow(df_proyectado))
  for (i in 1:nrow(df_proyectado)) {
    ibnr[i] <- sum(df_proyectado[i, 3:ncol(df_proyectado)], na.rm = TRUE) - sum(df_proyectado[i, 2], na.rm = TRUE)
  }
  return(ibnr)
}

reservas_ibnr <- calcular_ibnr(reclamaciones_proyectadas)
reservas_ibnr


###########################################################################
##########################     CHAIN-LADDER    ############################
###########################################################################

# Crear un dataframe de ejemplo
reclamaciones <- data.frame(
  Año_Ocurrencia = c(2021, 2022, 2023),
  Año_0 = c(100, 120, 130),
  Año_1 = c(150, 220, NA),
  Año_2 = c(180, NA, NA)
)


# Función para calcular los ratios de desarrollo
calcular_ratios_chain_ladder <- function(df) {
  n <- ncol(df) - 1  # Número de años de desarrollo
  ratios <- numeric(n - 1)
  
  for (i in 1:(n - 1)) {
    # Calcular el ratio para cada periodo
    ratios[i] <- sum(df[1:(n - i), i + 2], na.rm = TRUE) / sum(df[1:(n - i), i + 1], na.rm = TRUE)
  }
  
  return(ratios)
}

ratios_chain_ladder <- calcular_ratios_chain_ladder(reclamaciones)
print("Ratios de Desarrollo Chain-Ladder:")
print(ratios_chain_ladder)

# Proyectar reclamaciones utilizando los ratios de desarrollo
proyectar_reclamaciones_chain_ladder <- function(df, ratios) {
  df_proyectado <- df
  n <- ncol(df) - 1  # Número de años de desarrollo
  
  for (i in 1:(n - 1)) {
    for (j in 1:(n - i)) {
      if (is.na(df_proyectado[j, i + 2])) {
        df_proyectado[j, i + 2] <- df_proyectado[j, i + 1] * ratios[i]
      }
    }
  }
  
  return(df_proyectado)
}

reclamaciones_proyectadas <- proyectar_reclamaciones_chain_ladder(reclamaciones, ratios_chain_ladder)
print("Reclamaciones Proyectadas (Chain-Ladder):")
print(reclamaciones_proyectadas)


# Calcular la reserva IBNR
calcular_ibnr_chain_ladder <- function(df_proyectado) {
  ibnr <- numeric(nrow(df_proyectado))
  for (i in 1:nrow(df_proyectado)) {
    # Sumar las reclamaciones proyectadas y restar las reportadas
    ibnr[i] <- sum(df_proyectado[i, 3:ncol(df_proyectado)], na.rm = TRUE) - sum(df_proyectado[i, 2], na.rm = TRUE)
  }
  return(ibnr)
}

reservas_ibnr_chain_ladder <- calcular_ibnr_chain_ladder(reclamaciones_proyectadas)
print("Reservas IBNR (Chain-Ladder):")
print(reservas_ibnr_chain_ladder)

