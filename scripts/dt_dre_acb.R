source("scripts/helpers.R")
source("scripts/acb_dre.R")

# Crear columna combinada con HTML
tabla <- df %>%
  mutate(
    jugador_rival = paste0(
      "<b>", license_license_str15, "</b><br><small>", rival, "</small>"
    )
  )

tabla <- df %>%
  mutate(
    # Añadimos color verde/rojo según resultado
    rival_color = case_when(
      grepl("\\(W", rival) ~ gsub(
        "\\(W",
        "<span style='color:#1a7f37;'>(W",  # verde
        rival
      ),
      grepl("\\(L", rival) ~ gsub(
        "\\(L",
        "<span style='color:#d32f2f;'>(L",  # rojo
        rival
      ),
      TRUE ~ rival
    ),
    # Fusionamos nombre + rival
    jugador_rival = paste0(
      "<b>", license_license_str15, "</b><br><small>", rival_color, "</small>"
    ),
    ,
    # Columna con el logo (usa el código ABB para encontrar la imagen)
     logo_cuadrado= paste0("<img src='", logo_cuadrado, "' style='height:32px;'>"),
  )


  reactable(
   df %>%
     select( -equipo),
   columns = list(
     num_jornada = colDef(name = "Jor",    minWidth = 50),
     license_license_str15 = colDef(name = "Jug", minWidth = 180),
     points     = colDef(name = "Pts", minWidth = 50),
     asis       = colDef(name = "Ast", minWidth = 50),
     total_rebound = colDef(name = "Reb", minWidth = 50),
     val        = colDef(name = "Val", minWidth = 50),
     dre        = colDef(name = "Dre", minWidth = 50),
     rival = colDef(name = "Rival", minWidth = 180)
   ),
    pagination = TRUE,
    filterable = TRUE,
    highlight = TRUE,
   theme = reactableTheme(
     header = list(
       borderBottom = "3px solid black",
       textAlign = "center"          # centra los nombres del header
     ),
     style = list(
       fontFamily = "Oswald",
       textAlign = "center"           # centra todos los valores de las celdas
     )
   )
  )

  
  
  