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
        "<span style='color:#1a7f37;'>(W", # verde
        rival
      ),
      grepl("\\(L", rival) ~ gsub(
        "\\(L",
        "<span style='color:#d32f2f;'>(L", # rojo
        rival
      ),
      TRUE ~ rival
    ),
    # Fusionamos nombre + rival
    jugador_rival = paste0(
      "<b>", license_license_str15, "</b><br><small>", rival_color, "</small>"
    ), ,
    # Columna con el logo (usa el código ABB para encontrar la imagen)
    logo_cuadrado = paste0("<img src='", logo_cuadrado, "' style='height:32px;'>"),
  )


reactable(
  df %>%
    select(-equipo, -abb, -rival, -matches, -logo_cuadrado),
  columns = list(
    num_jornada = colDef(name = "Jor", minWidth = 50),
    license_license_str15 = colDef(
      name = "Jug",
      minWidth = 170,
      html = TRUE,
      cell = function(value, index) {
        htmltools::HTML(
          combine_word(
            license_license_str15 = value,
            rival = df$rival[index],
            logo_cuadrado = df$logo_cuadrado[index]
          )
        )
      }
    ),
    time_played = colDef(name = "Min", minWidth = 50),
    points = colDef(name = "Pts", minWidth = 50),
    asis = colDef(name = "Ast", minWidth = 50),
    total_rebound = colDef(name = "Reb", minWidth = 50),
    turnovers = colDef(name = "Tot", minWidth = 50),
    steals = colDef(name = "Stl", minWidth = 50),
    blocks = colDef(name = "Blk", minWidth = 50),
    x2pm_a = colDef(name = "2pt", minWidth = 50),
    x3pm_a = colDef(name = "3pt", minWidth = 50),
    x1pm_a = colDef(name = "1pt", minWidth = 50),
    val = colDef(name = "Val", minWidth = 50, align = "center"),
    dre = colDef(name = "Dre", minWidth = 50, align = "center"),
    differential = colDef(name = "+/-", minWidth = 50)
  ),
  pagination = TRUE,
  filterable = TRUE,
  highlight = TRUE,
  defaultPageSize = 25,
  theme = reactableTheme(
    header = list(
      borderBottom = "3px solid black",
      textAlign = "center" # centra los nombres del header
    ),
    style = list(
      fontFamily = "Oswald",
      textAlign = "center" # centra todos los valores de las celdas
    )
  )
)
