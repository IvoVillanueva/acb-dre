# librerias
library(tidyverse)
library(janitor)
library(gt)
library(gtExtras)
library(gtUtils)

# datos
racha <- read_csv(
  "https://raw.githubusercontent.com/IvoVillanueva/pbp-acb-2025-26/refs/heads/main/data/last_result.csv",
  show_col_types = FALSE
)

jornada_dre <- read_csv(
  "https://raw.githubusercontent.com/IvoVillanueva/pbp-acb-2025-26/refs/heads/main/data/boxscores_2025_26.csv",
  show_col_types = FALSE
) 

# Load club logos data
clubs <- read.csv("https://raw.githubusercontent.com/IvoVillanueva/datos_aFAvor_eContra/refs/heads/main/2026/clubs_logosCuadrados.csv") %>% 
  select(abb, logo_cuadrado)


combine_word <- function(license_license_str15, rival, logo_cuadrado) {
  glue::glue(
    "<div style='display: flex; align-items: center; text-align: left; line-height: 12px;'>
      <img style='
        height: 28px;
        width: auto;
        margin-right: 6px;'
        src='{logo_cuadrado}'/>
      <div style='display: flex; flex-direction: column;'>
        <span style='font-weight: 700; font-variant: small-caps; font-size: 15px;'>{license_license_str15}</span>
        <span style='font-weight: 400; color: grey; font-variant: small-caps; font-size: 10px;'>{rival}</span>
      </div>
    </div>"
  )
}


totales_equipo <-jornada_dre %>% 
  filter(num_jornada == max(num_jornada) & !is.na(license_license_str15)) %>%
  group_by(id_match, abb) %>% 
  summarise(
    team_fga     = sum(x2pt_tried + x3pt_tried, na.rm = TRUE),
    team_fta     = sum(x1pt_tried, na.rm = TRUE),
    team_tov     = sum(turnovers, na.rm = TRUE),
    team_minutes = sum(time_played, na.rm = TRUE),
    .groups = "drop"
  )



twitter <- "<span style='color:#000000;font-family: \"Font Awesome 6 Brands\"'>&#xE61A;</span>"
tweetelcheff <- "<span style='font-weight:bold;'>*@elcheff*</span>"
insta <- "<span style='color:#E1306C;font-family: \"Font Awesome 6 Brands\"'>&#xE055;</span>"
instaelcheff <- "<span style='font-weight:bold;'>*@sport_iv0*</span>"
github <- "<span style='color:#000000;font-family: \"Font Awesome 6 Brands\"'>&#xF092;</span>"
githubelcheff <- "<span style='font-weight:bold;'>*IvoVillanueva*</span>"
caption <- glue::glue("**Datos**:@ACBCOM • **Gráfico**: *Ivo Villanueva* • {twitter} {tweetelcheff} • {insta} {instaelcheff} • {github} {githubelcheff}")


jor_max <- max(jornada_dre$num_jornada)
