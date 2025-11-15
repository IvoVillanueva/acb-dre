source("scripts/helpers.R")
source("scripts/helpers.R")

df <- jornada_dre %>%
  janitor::clean_names() %>%
  filter(num_jornada == max(num_jornada) & !is.na(license_license_str15)) %>%
  mutate(
    dre =
      0.79 * points
        - 0.72 * x2pt_tried
        - 0.55 * x3pt_tried
        - 0.16 * x1pt_tried
        + 0.13 * offensive_rebound
        + 0.40 * defensive_rebound
        + 0.54 * asis
        + 1.68 * steals
        + 0.76 * blocks
        - 1.36 * turnovers
        - 0.11 * personal_fouls,
    time_played = hms::hms(minutes = time_played),
    x2pm_a = paste0(x2pt_success, "/", x2pt_tried),
    x3pm_a = paste0(x3pt_success, "/", x3pt_tried),
    x1pm_a = paste0(x3pt_success, "/", x1pt_tried),
    equipo = ifelse(abb == local_team_team_abbrev_name,
      local_team_team_actual_name,
      visitor_team_team_actual_name
    )
  ) %>%
  select(
    fecha, num_jornada, abb, equipo, license_license_str15,
    matches, time_played, points, total_rebound, asis, turnovers,
    steals, blocks, x2pm_a, x3pm_a, x1pm_a, differential, val, dre
  ) %>%
  arrange(desc(dre)) %>%
  left_join(racha, join_by(abb)) %>%
  left_join(clubs, join_by(abb)) %>%
  mutate(
    rival = case_when(
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
    )
  )
