# Load libraries required for GT table generation and PNG export
library(tidyverse)
library(janitor)
library(gt)
library(gtExtras)
library(gtUtils)

# Load last game results (used for rival streak formatting)
racha <- read_csv(
  "https://raw.githubusercontent.com/IvoVillanueva/pbp-acb-2025-26/refs/heads/main/data/last_result.csv",
  show_col_types = FALSE
)

# Load boxscore data for DRE computation
jornada_dre <- read_csv(
  "https://raw.githubusercontent.com/IvoVillanueva/pbp-acb-2025-26/refs/heads/main/data/boxscores_2025_26.csv",
  show_col_types = FALSE
)

# Load team logos (square icons)
clubs <- read.csv(
  "https://raw.githubusercontent.com/IvoVillanueva/datos_aFAvor_eContra/refs/heads/main/2026/clubs_logosCuadrados.csv"
) %>% 
  select(abb, logo_cuadrado)

# Build HTML block containing:
# - player name
# - rival W/L streak
# - club logo
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

# Credits + social caption (HTML formatted for GT footer)
ctwitter <- "<span style='color:#c8102e;font-family: \"Font Awesome 6 Brands\"'>&#xE61A;</span>"
tweetelcheff <- "<span style='font-weight:bold;color: grey;'>*@elcheff*</span>"
insta <- "<span style='color:#E1306C;font-family: \"Font Awesome 6 Brands\"'>&#xE055;</span>"
instaelcheff <- "<span style='font-weight:bold;color: grey;'>*@sport_iv0*</span>"
github <- "<span style='color:#c8102e;font-family: \"Font Awesome 6 Brands\"'>&#xF092;</span>"
githubelcheff <- "<span style='font-weight:bold;color: grey;'>*IvoVillanueva*</span>"
caption <- glue::glue("**Datos**: *@NBA* | **Gráfico**: *Ivo Villanueva* • {twitter} {tweetelcheff} • {insta} {instaelcheff} • {github} {githubelcheff}")

# Extract last jornada number (used in GT title)
jor_max <- max(jornada_dre$num_jornada)
