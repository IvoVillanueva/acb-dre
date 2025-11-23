source("scripts/acb_dre_gt.R")

# asegurar que la carpeta data existe
if (!dir.exists("png")) dir.create("png")


df %>%
  drop_na() %>%
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
    combo = combine_word(license_license_str15, rival, logo_cuadrado),
    combo = map(combo, gt::html),
    time_played = substr(time_played, 1, 5),
    dre = round(dre,1)
  ) %>%
  select(combo,
    mp = time_played, points, rb = total_rebound,
    asis, tov = turnovers, st = steals, blk = blocks, PT2 = x2pm_a, PT3 = x3pm_a, FT = x1pm_a,
    m_n = differential, val, dre
  ) %>%
  gt_preview(
    top_n = 5,
    bottom_n = 3
  ) %>%
  cols_align(
    align = "center",
    columns = c(val, dre)
  ) %>%
  data_color(dre, palette = c("white", "#419647"),
             domain = range(round(df$dre,1)),
             na_color = "#E4E4E4") %>%
  cols_label(
    combo = "",
    mp = "MP",
    points = "PTS",
    asis = "AST",
    rb = "RBS",
    tov = "TOV",
    st = "STL",
    blk = "BLK",
    m_n = "+/-",
    val = "VAL",
    dre = "DRE"
  ) %>%
  gt_theme_savant() %>%
  tab_options(
    heading.align = "left",
    table.font.names = "Oswald",
  ) %>%
  tab_style(
    style = cell_text(size = px(10)),
    locations = cells_source_notes()
  ) |>
  tab_style(
    style = cell_text(size = px(10),
                      weight = 700,
                      font = "Oswald"),
    locations = cells_stub()
  ) %>% 
  tab_header(
    title = paste0("Los 5 Mejores y Los 3 Peores De La J",jor_max) ,
    subtitle = md("Ordenados por el **Daily RAPM**")
  ) %>%
  tab_source_note(
    source_note = md(caption)
  ) %>%
  gtsave(paste0("png/topj", jor_max,".png"), expand = 20)
