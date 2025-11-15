# librerias
library(tidyverse)
library(janitor)
library(reactable)

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
    "<div style='display: flex; align-items: center; text-align: left; line-height: 16px;'>
      <img style='
        height: 30px;
        width: auto;
        margin-right: 6px;'
        src='{logo_cuadrado}'/>
      <div style='display: flex; flex-direction: column;'>
        <span style='font-weight: 700; font-variant: small-caps; font-size: 18px;'>{license_license_str15}</span>
        <span style='font-weight: 400; color: grey; font-variant: small-caps; font-size: 13px;'>{rival}</span>
      </div>
    </div>"
  )
}