source("substack/dreappquarto/drep.R")
library(gt)
library(gtExtras)
library(gtUtils)
library(extrafont)

twitter <- "<span style='color:#000000;font-family: \"Font Awesome 6 Brands\"'>&#xE61A;</span>"
tweetelcheff <- "<span style='font-weight:bold;'>*@elcheff*</span>"
insta <- "<span style='color:#E1306C;font-family: \"Font Awesome 6 Brands\"'>&#xE055;</span>"
instaelcheff <- "<span style='font-weight:bold;'>*@sport_iv0*</span>"
github <- "<span style='color:#000000;font-family: \"Font Awesome 6 Brands\"'>&#xF092;</span>"
githubelcheff <- "<span style='font-weight:bold;'>*IvoVillanueva*</span>"
caption <- glue::glue("**Datos**:@ACBCOM • **Gráfico**: *Ivo Villanueva* • {twitter} {tweetelcheff} • {insta} {instaelcheff} • {github} {githubelcheff}")


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

stack_html <- function(valor, pct) {
  paste0(
    "<div style='line-height:1;'>",
    "<div style='font-size:12px;'>", valor, "</div>",
    "<div style='font-size:7px; font-weight:bold; color:#000;'>", pct, "</div>",
    "</div>"
  )
}
jornda_dre %>%
  left_join(clubs, join_by(abb)) %>%
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
             domain = range(jornda_dre$dre),
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
  ) %>%
  tab_header(
    title = "Mi TOP 5 De La J6",
    subtitle = md("Ordenados por el **Daily RAPM**")
  ) %>%
  tab_source_note(
    source_note = md(caption)
  ) %>%
  gtsave("substack/topj6.png", expand = 20)
