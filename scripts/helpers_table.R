# Load required libraries for reactable pipeline
library(tidyverse)
library(janitor)
library(reactable)
library(htmltools)
library(glue)

# Load latest results (rival form: W/L streak)
racha <- read_csv(
  "https://raw.githubusercontent.com/IvoVillanueva/pbp-acb-2025-26/refs/heads/main/data/last_result.csv",
  show_col_types = FALSE
)

# Load boxscore data for DRE calculations
jornada_dre <- read_csv(
  "https://raw.githubusercontent.com/IvoVillanueva/pbp-acb-2025-26/refs/heads/main/data/boxscores_2025_26.csv",
  show_col_types = FALSE
)

# Load club logos (square icons)
clubs <- read.csv(
  "https://raw.githubusercontent.com/IvoVillanueva/datos_aFAvor_eContra/refs/heads/main/2026/clubs_logosCuadrados.csv"
) %>% 
  select(abb, logo_cuadrado)

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
totales_equipo <- jornada_dre %>% 
  filter(num_jornada == max(num_jornada) & !is.na(license_license_str15)) %>%
  group_by(id_match, abb) %>% 
  summarise(
    team_fga     = sum(x2pt_tried + x3pt_tried, na.rm = TRUE),
    team_fta     = sum(x1pt_tried, na.rm = TRUE),
    team_tov     = sum(turnovers, na.rm = TRUE),
    team_minutes = sum(time_played, na.rm = TRUE),
    .groups = "drop"
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

