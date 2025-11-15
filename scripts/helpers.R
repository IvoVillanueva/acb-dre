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