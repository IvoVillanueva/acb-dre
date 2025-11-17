source("scripts/acb_dre.R")


tabla <- df %>%
  mutate(dre = round(dre, 1)) %>% 
  select(-equipo, -abb, -rival, -matches, -logo_cuadrado) %>% 
reactable(
  columns = list(
    license_license_str15 = colDef(
      name = "",
      minWidth = 80,
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
    time_played = colDef(name = "Min", minWidth = 30, align = "right", vAlign  = "center"),
    ts_pct = colDef(name = "Ts%", minWidth = 30, align = "right", vAlign  = "center"),
    usg_pct = colDef(name = "Usg%", minWidth = 30, align = "right", vAlign  = "center"),
    points = colDef(name = "Pts", minWidth = 25, vAlign  = "center"),
    asis = colDef(name = "Ast", minWidth = 25, vAlign  = "center"),
    total_rebound = colDef(name = "Reb", minWidth = 25, vAlign  = "center"),
    turnovers = colDef(name = "Tot", minWidth = 25, vAlign  = "center"),
    steals = colDef(name = "Stl", minWidth = 25, vAlign  = "center"),
    blocks = colDef(name = "Blk", minWidth = 25, vAlign  = "center"),
    x2pm_a = colDef(name = "2pt", minWidth = 25, align = "right", vAlign  = "center"),
    x3pm_a = colDef(name = "3pt", minWidth = 25, align = "right", vAlign  = "center"),
    x1pm_a = colDef(name = "1pt", minWidth = 25, align = "right", vAlign  = "center"),
    val = colDef(name = "Val", minWidth = 25, align = "center", vAlign  = "center"),
    dre = colDef(name = "Dre", minWidth = 25, align = "center", vAlign  = "center"),
    differential = colDef(name = "+/-", minWidth = 25, vAlign  = "center")
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
      fontFamily = "Oswald"
    )
  )
)
