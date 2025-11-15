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
     head(10) %>% 
     select(-logo_cuadrado, -equipo),
    columns = list(
      num_jornada = colDef("Jor"),
      license_license_str15 = colDef(name = "Jug"),
      points     = colDef(name = "Pts"),
      asis     = colDef(name = "Ast"),
      total_rebound     = colDef(name = "Reb"),
      val     = colDef(name = "Val"),
      dre = colDef("Dre")
    ),
    pagination = TRUE,
    filterable = TRUE,
    highlight = TRUE,
   theme = reactableTheme(
     header = list(
       borderBottom = "3px solid black"   # << línea debajo de los nombres del header
     ),
     style = list(
       fontFamily = "Oswald"   # aplicar la fuente
     )
   )
  )

  
  
  