source("scripts/helpers_gt.R")


df <- jornada_dre %>%
  janitor::clean_names() %>%
  left_join(totales_equipo, by = c("id_match", "abb")) %>%
  filter(num_jornada == max(num_jornada) & !is.na(license_license_str15)) %>%
  mutate(
    fga = x2pt_tried + x3pt_tried,
    fta = x1pt_tried,
    pts_calc = 2 * x2pt_success + 3 * x3pt_success + x1pt_success,
    ts_pct = if_else(
      (fga + 0.44 * fta) == 0,
      0,
      pts_calc / (2 * (fga + 0.44 * fta))
    ),
    usg_pct = 100 *
      ((fga + 0.44 * fta + turnovers) * (team_minutes / 5)) /
      (time_played * (team_fga + 0.44 * team_fta + team_tov)),
    usg_pct = ifelse(is.finite(usg_pct), usg_pct, 0),
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
    time_played = lubridate::seconds_to_period(time_played),
    time_played = sprintf(
      "%02d:%02d",
      minute(time_played),
      second(time_played)
    ),
    fecha = format(fecha, "%d/%m/%Y"),
    x2pm_a = paste0(x2pt_success, "/", x2pt_tried),
    x3pm_a = paste0(x3pt_success, "/", x3pt_tried),
    x1pm_a = paste0(x3pt_success, "/", x1pt_tried),
    equipo = ifelse(abb == local_team_team_abbrev_name,
      local_team_team_actual_name,
      visitor_team_team_actual_name
    ),
    percentil_val = round(percent_rank(val),3)*100,
    percentil_dre = round(percent_rank(dre),3)*100,,
    differential = ifelse( differential > 0,paste0("+", differential), differential)
  ) %>%
  select(
    abb, equipo, license_license_str15,
    matches, time_played, ts_pct, usg_pct, points, total_rebound, asis, turnovers,
    steals, blocks, x2pm_a, x3pm_a, x1pm_a, differential, val, percentil_val, dre, percentil_dre
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
    ),
    ts_pct = paste0(round(ts_pct * 100, 1), "%"),
    usg_pct = paste0(round(usg_pct, 1), "%")
  )
range(round(df$dre,1))