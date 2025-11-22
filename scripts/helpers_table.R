# Este script contiene funciones helper y la carga de datos.
# Las librerías requeridas (tidyverse, janitor, glue, etc.) DEBEN 
# cargarse en el archivo principal (acb_dre_table.R o tabla_dre.qmd) 
# antes de hacer 'source("scripts/helpers_table.R")'.

# Load latest results (rival form: W/L streak)
racha <- readr::read_csv(
  "https://raw.githubusercontent.com/IvoVillanueva/pbp-acb-2025-26/refs/heads/main/data/last_result.csv",
  show_col_types = FALSE
)

# Load boxscore data for DRE calculations
jornada_dre <- readr::read_csv(
  "https://raw.githubusercontent.com/IvoVillanueva/pbp-acb-2025-26/refs/heads/main/data/boxscores_2025_26.csv",
  show_col_types = FALSE
)

# Load club logos (square icons)
clubs <- read.csv(
  "https://raw.githubusercontent.com/IvoVillanueva/datos_aFAvor_eContra/refs/heads/main/2026/clubs_logosCuadrados.csv"
) %>% 
  dplyr::select(abb, logo_cuadrado) # Usamos dplyr::select para evitar dependencia de library()

# Build player name + rival + logo HTML block
combine_word <- function(license_license_str15, rival, logo_cuadrado) {
  glue::glue(
    "<div style='display: flex; align-items: center; text-align: left; line-height: 13px;'>
      <img style='
        height: 24px;
        width: auto;
        margin-right: 6px;'
        src='{logo_cuadrado}'/>
      <div style='display: flex; flex-direction: column;'>
        <span style='font-weight: 700; font-variant: small-caps; font-size: 12px;'>{license_license_str15}</span>
        <span style='font-weight: 400; color: grey; font-variant: small-caps; font-size: 9px;'>{rival}</span>
      </div>
    </div>"
  )
}

# Generator for continuous color palettes (used for DRE backgrounds)
make_color_pal <- function(colors, bias = 1) {
  get_color <- colorRamp(colors, bias = bias)
  function(x) rgb(get_color(x), maxColorValue = 255)
}

# Gradient for DRE cells (red → white → blue)
dre_rating_color <- make_color_pal(c("#ff2700", "#f8fcf8", "#00740EBF"), bias = 2)

# Black/white font selector to ensure legibility on colored backgrounds
make_font_pal <- function(index, vec_pct) {
  if (vec_pct[index] >= 94) "white" else "black"
}

# Compute team-level totals required for usage/TS% formulas
# NOTA: Usamos el operador base pipe |> para asegurar que las dependencias
# estén cargadas en el archivo principal.
totales_equipo <- jornada_dre |> 
  dplyr::filter(num_jornada == max(num_jornada) & !is.na(license_license_str15)) |> 
  dplyr::group_by(id_match, abb) |> 
  dplyr::summarise(
    team_fga     = sum(x2pt_tried + x3pt_tried, na.rm = TRUE),
    team_fta     = sum(x1pt_tried, na.rm = TRUE),
    team_tov     = sum(turnovers, na.rm = TRUE),
    team_minutes = sum(time_played, na.rm = TRUE),
    .groups = "drop"
  )

# Extract last jornada number (used in GT title)
jor_max <- max(jornada_dre$num_jornada)

logo_header <- htmltools::HTML(paste0(
  "<div style='text-align:left; font-family: Oswald;'>

    <!-- LOGO -->
    <div style='margin-bottom:6px;'>
      <img src='https://raw.githubusercontent.com/IvoVillanueva/data/refs/heads/main/elcheff_thecleanshotlogo.png'
           style='width:44px; height:44px;' />
    </div>

    <!-- TITULO -->
    <div style='font-size:28px; font-weight:600; line-height:1.1; margin-bottom:4px;'>
      Los Mejores De La Jornada ",jor_max, "
    </div>

    <!-- SUBTITULO -->
    <div style='font-size:12px; font-weight:400; color:#8C8C8C; line-height:1.2;'>
      Filtrados por el  <a href='https://fansided.com/2017/04/10/updating-dre-tweaks/'
         style='color:#000; text-decoration:none; font-weight:500;'
         target='_blank'>
         <i>RAPM Estimate (DRE)</i>
      </a> por defecto | ACB 25/26
    </div>

  </div>
")
)

# Caption block with social + credits (HTML-ready)
caption <- htmltools::HTML(
  "<b>Datos</b>:@ACBCOM • <b>Gráfico</b>: <i>Ivo Villanueva</i> •
   <span style='color:#000000;font-family:\"Font Awesome 6 Brands\"'>&#xE61A;</span>
   @elcheff • 
   <span style='color:#E1306C;font-family:\"Font Awesome 6 Brands\"'>&#xE055;</span>
   @sport_iv0 • 
   <span style='color:#000000;font-family:\"Font Awesome 6 Brands\"'>&#xF092;</span>
   IvoVillanueva"
)