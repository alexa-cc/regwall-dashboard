# =====================================================================
# DC Regwall Dashboard (Shiny)
# Reads the dc_mart models in BigQuery: wall_reader_funnel,
# session_engagement, registration_to_membership, ga4_events.
#
# Packages:
#   install.packages(c("shiny", "bslib", "bigrquery", "gargle", "dplyr",
#                      "tidyr", "ggplot2", "scales", "DT", "systemfonts", "ragg"))
#
# Auth:
#   Local:      first run opens a browser to sign in with your Google account.
#   Cloud Run:  uses the service's attached service account automatically.
#   Elsewhere:  set BQ_KEY_PATH to a service account JSON key file.
#
# Fonts: Gibson .otf files go in www/fonts/ (copied into the container by
# the Dockerfile). Without them, the app falls back to Open Sans.
# =====================================================================

library(shiny)
library(bslib)
library(bigrquery)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(DT)

Sys.setenv(TZ = "America/New_York")   # Cloud Run runs in UTC; match GA4's property timezone

PROJECT <- "ccc-citycast-prod"
DATASET <- "dc_mart"
LAUNCH  <- as.Date("2026-10-05")

# City Cast DC palette. Coral and black, plus a steel blue so chart bars can be told apart.
RED      <- "#FF6C5E"   # DC coral: regwall series + accents
RED_SOFT <- "#FFFFFF"   # bars are white with a black outline (no tints)
BLUE     <- "#2F5D8A"   # softwall series
INK      <- "#000000"
MUTED    <- "#000000"
# Adjacent funnel steps need to be distinguishable, so each step is its own
# colour rather than one fill for the whole chart.
FUNNEL_REG <- c(
  "Saw the wall" = "#FFC7C0",
  "Clicked CTA" = "#FF6C5E",
  "Submitted email" = "#C93B32",
  "Registered" = "#2F5D8A",
  "Verified" = "#1A1A1A"
)
FUNNEL_SOFT <- c(
  "Saw the wall" = "#C5D4E6",
  "Clicked CTA" = "#7FA0C4",
  "Submitted email" = "#2F5D8A",
  "Registered" = "#FF6C5E",
  "Verified" = "#1A1A1A"
)
FONT     <- "Gibson"    # falls back to Open Sans; files live in www/fonts

# ggplot needs the font registered with the graphics device. CSS @font-face
# only covers the HTML. Paths are relative to the app directory.
gibson_regular <- "www/fonts/Gibson-Regular.otf"
gibson_bold    <- "www/fonts/Gibson-SemiBold.otf"
options(shiny.useragg = TRUE)
if (file.exists(gibson_regular) && requireNamespace("systemfonts", quietly = TRUE)) {
  systemfonts::register_font(
    "Gibson",
    plain = gibson_regular,
    bold = if (file.exists(gibson_bold)) gibson_bold else gibson_regular,
    italic = gibson_regular,
    bolditalic = if (file.exists(gibson_bold)) gibson_bold else gibson_regular
  )
  if (requireNamespace("ragg", quietly = TRUE)) options(shiny.useragg = TRUE)
}

if (nzchar(Sys.getenv("K_SERVICE"))) {
  # Cloud Run sets K_SERVICE; sign in as the service's attached service account
  bq_auth(token = gargle::credentials_gce())
} else if (nzchar(Sys.getenv("BQ_KEY_PATH"))) {
  bq_auth(path = Sys.getenv("BQ_KEY_PATH"))
} else {
  bq_auth()   # local: interactive Google sign-in
}

# ---------------------------------------------------------------------
# Data
# ---------------------------------------------------------------------
tbl <- function(name) sprintf("`%s.%s.%s`", PROJECT, DATASET, name)

run_query <- function(sql) {
  bq_table_download(bq_project_query(PROJECT, sql), bigint = "numeric")
}

load_data <- function() {
  list(
    funnel = run_query(sprintf("
      SELECT event_date, reader_id, wall_type, device, page_type, content_group,
             page_path, utm_source, variant_name, wall_views, viewed, clicked_cta,
             submitted, registered, verified, dismissed, dismissed_after_submit,
             existing_user_login, submitted_no_outcome, views_before_reg,
             seconds_to_dismiss, scrolled_90_on_wall_page,
             registrant_visit_type, days_first_visit_to_reg
      FROM %s", tbl("wall_reader_funnel"))),

    sessions = run_query(sprintf("
      SELECT event_date, membership_status, device,
             COUNT(*) AS sessions,
             SUM(pageviews) AS pageviews,
             SUM(engaged_seconds) AS engaged_seconds,
             SUM(articles_scrolled_90) AS articles_scrolled_90,
             COUNTIF(return_window_complete) AS sessions_window_complete,
             COUNTIF(return_window_complete AND returned_within_7d) AS sessions_returned
      FROM %s
      GROUP BY 1, 2, 3", tbl("session_engagement"))),

    surfaces = run_query(sprintf("
      SELECT DISTINCT event_date, IFNULL(surface, '(not set)') AS surface,
             COALESCE(user_id, reader_id) AS account_key
      FROM %s
      WHERE event_name = 'registration_success'", tbl("ga4_events"))),

    newsletters = run_query(sprintf("
      SELECT DISTINCT event_date, IFNULL(newsletter, '(not set)') AS newsletter,
             event_name, reader_id
      FROM %s
      WHERE event_name IN ('newsletter_subscribe', 'newsletter_unsubscribe')
  AND reader_id IS NOT NULL",
                                    tbl("ga4_events"))),

    membership = run_query(sprintf("
      SELECT reg_date, account_key, reg_surface, reg_wall_type,
             checkout_view_ts IS NOT NULL AS viewed_checkout,
             checkout_advanced_ts IS NOT NULL AS advanced_checkout,
             subscription_started_ts IS NOT NULL AS became_member
      FROM %s", tbl("registration_to_membership"))),

    loaded_at = Sys.time()
  )
}

# One query set for the life of the process. Sessions filter this in memory.
# On Cloud Run the container shuts down after ~15 idle minutes, so the next
# visitor after a quiet stretch triggers a fresh load.
DATA <- load_data()

choice_vals <- function(x) sort(unique(na.omit(as.character(x))))
DEVICE_CHOICES    <- choice_vals(DATA$funnel$device)
PAGE_TYPE_CHOICES <- choice_vals(DATA$funnel$page_type)
SOURCE_CHOICES    <- choice_vals(DATA$funnel$utm_source)
VARIANT_CHOICES   <- choice_vals(DATA$funnel$variant_name)

# Yesterday, so the range is never backwards on launch day.
RANGE_END   <- Sys.Date() - 1
RANGE_START <- if (RANGE_END < LAUNCH) RANGE_END - 6 else LAUNCH

# ---------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------
# distinct readers where a flag is TRUE (NA-safe)
nd <- function(id, cond) n_distinct(id[cond %in% TRUE])

safe_div <- function(a, b) ifelse(b > 0, a / b, NA_real_)

fmt_n   <- function(x) comma(x, accuracy = 1)
fmt_pct <- function(x) ifelse(is.na(x), "n/a", percent(x, accuracy = if (isTRUE(x < 0.1)) 0.1 else 1))

funnel_summary <- function(df) {
  df |>
    summarise(
      wall_views = sum(wall_views, na.rm = TRUE),
      viewed     = nd(reader_id, viewed),
      cta        = nd(reader_id, clicked_cta),
      submitted  = nd(reader_id, submitted),
      registered = nd(reader_id, registered),
      verified   = nd(reader_id, verified),
      dismissed  = nd(reader_id, dismissed & !dismissed_after_submit)
    )
}

theme_dash <- function() {
  theme_minimal(base_size = 13, base_family = FONT) +
    theme(
      panel.grid = element_blank(),
      axis.line.x = element_line(color = INK, linewidth = 0.6),
      axis.title = element_text(color = INK, size = 11),
      axis.text = element_text(color = INK),
      plot.background = element_rect(fill = "transparent", color = NA),
      panel.background = element_rect(fill = "transparent", color = NA),
      plot.margin = margin(8, 16, 8, 8)
    )
}

empty_plot <- function(msg = "No data for the selected filters") {
  ggplot() + annotate("text", x = 0, y = 0, label = msg, color = MUTED, size = 4.5) + theme_void()
}

funnel_plot <- function(s, colors, include_cta = TRUE) {
  steps <- tibble(
    step = c("Saw the wall", "Clicked CTA", "Submitted email", "Registered", "Verified"),
    n    = c(s$viewed, s$cta, s$submitted, s$registered, s$verified)
  )
  if (!include_cta) steps <- filter(steps, step != "Clicked CTA")
  if (steps$n[1] == 0) return(empty_plot())
  steps <- steps |>
    mutate(
      label = ifelse(step == "Clicked CTA" & n == 0, "Not tracked yet",
                     paste0(fmt_n(n), "  (", fmt_pct(n / steps$n[1]), " of readers)")),
      step = factor(step, levels = rev(step))
    )
  ggplot(steps, aes(n, step)) +
    geom_col(aes(fill = step), color = INK, linewidth = 0.35, width = 0.62) +
    geom_text(aes(label = label), hjust = 0, nudge_x = max(steps$n) * 0.015,
              color = INK, size = 5, family = FONT) +
    scale_fill_manual(values = colors, guide = "none") +
    scale_x_continuous(expand = expansion(mult = c(0, 0.42))) +
    labs(x = NULL, y = NULL) +
    coord_cartesian(clip = "off") +
    theme_dash() +
    theme(panel.grid = element_blank(), axis.text.x = element_blank(),
          axis.line.x = element_blank(),
          axis.text.y = element_text(color = INK, size = 15),
          plot.margin = margin(8, 28, 8, 4))
}

# KPIs render as cells in a ruled strip (see .kpi-row CSS), not floating boxes.
kpi <- function(title, value, sub = NULL, tone = c("red", "blue")) {
  tone <- match.arg(tone)
  tags$div(
    class = paste("kpi-cell", if (tone == "blue") "kpi-blue"),
    tags$div(title, class = "kpi-label"),
    tags$div(value, class = "kpi-value"),
    if (!is.null(sub)) tags$div(sub, class = "kpi-sub")
  )
}
kpi_row <- function(...) tags$div(class = "kpi-row", ...)

dt_table <- function(df, pct_cols = NULL, page_length = 10) {
  out <- datatable(df, rownames = FALSE, class = "compact stripe",
                   options = list(dom = if (nrow(df) > page_length) "tp" else "t",
                                  pageLength = page_length, ordering = TRUE))
  if (length(pct_cols)) out <- formatPercentage(out, pct_cols, digits = 1)
  out
}

segment_table <- function(df, dim, label) {
  df |>
    mutate(key = coalesce(as.character(.data[[dim]]), "(not set)")) |>
    group_by(key) |>
    summarise(Readers = nd(reader_id, viewed),
              Registered = nd(reader_id, registered), .groups = "drop") |>
    filter(Readers > 0) |>
    mutate(`Registration rate` = Registered / Readers) |>
    arrange(desc(Readers)) |>
    rename_with(~ label, .cols = "key")
}

# ---------------------------------------------------------------------
# UI
# ---------------------------------------------------------------------
# Gibson is licensed (Canada Type). For internal use, drop the .otf files in
# www/fonts/ and they load via @font-face below; otherwise Open Sans is used.
theme <- bs_theme(
  version = 5, primary = RED, fg = INK, bg = "#FFFFFF",
  "border-radius" = "3px", "border-radius-sm" = "3px", "border-radius-lg" = "3px",
  "border-color" = "#000000",
  base_font = font_collection(FONT, font_google("Open Sans", wght = c(400, 600))),
  heading_font = font_collection(FONT, font_google("Open Sans", wght = c(600)))
)

card_head <- function(title, note, aside = FALSE) {
  card_header(
    class = "bg-transparent",
    tags$div(
      class = paste("card-head-text", if (aside) "card-head-aside"),
      tags$div(title, class = "card-title"),
      tags$div(note, class = "card-note")
    )
  )
}
card_plot <- function(title, note, id, height = 300, aside = FALSE) {
  card(class = "dash-card", full_screen = TRUE, card_head(title, note, aside),
       card_body(padding = 3, plotOutput(id, height = height)))
}
card_table <- function(title, note, id) {
  card(class = "dash-card", card_head(title, note), card_body(padding = 3, DTOutput(id)))
}

# Filters sit in a bar above every tab instead of a sidebar.
filter_select <- function(id, label, choices, placeholder) {
  selectizeInput(id, label, choices, multiple = TRUE, width = "100%",
                 options = list(placeholder = placeholder))
}
filter_bar <- tags$div(
  class = "filter-bar",
  tags$div(class = "filter-wide",
           dateRangeInput("dates", "Date range", start = RANGE_START, end = RANGE_END,
                          max = Sys.Date(), format = "M dd", separator = " – ",
                          width = "100%")),
  filter_select("device", "Device", DEVICE_CHOICES, "All devices"),
  filter_select("page_type", "Page type", PAGE_TYPE_CHOICES, "All types"),
  filter_select("utm_source", "Traffic source", SOURCE_CHOICES, "All sources"),
  filter_select("variant", "Variant", VARIANT_CHOICES, "All variants")
)

# @font-face lives in the document head, not the Sass bundle. bslib compiles
# dash_css into bootstrap.min.css, and a relative url() is then requested
# from /bootstrap-*/fonts/ instead of www/fonts.
font_css <- "
  @font-face { font-family: Gibson; font-weight: 400; font-style: normal;
    src: url('/fonts/Gibson-Regular.otf') format('opentype'); }
  @font-face { font-family: Gibson; font-weight: 600; font-style: normal;
    src: url('/fonts/Gibson-SemiBold.otf') format('opentype'); }
"

dash_css <- "
  body, .navbar, .form-control, .selectize-input, .kpi-value, .kpi-label, .kpi-sub,
  .card-title, .card-note, .section-label {
    font-family: Gibson, 'Open Sans', sans-serif;
  }
  body { -webkit-font-smoothing: antialiased; }
  h1, h2, h3, h4, h5, h6, b, strong, th { font-weight: 600 !important; }

  /* Same side inset for the navbar and the page, so the mark lines up with the filters */
  .navbar { border-top: 0 !important; border-bottom: 2px solid #000 !important; padding: 0 2rem !important; min-height: 64px; }
  .tab-pane.active { display: flex; flex-direction: column; gap: 1.75rem; }
  .navbar > .container-fluid { padding-left: 0 !important; padding-right: 0 !important; }
  body.bslib-page-navbar > .container-fluid { padding: 0 2rem 2.5rem !important; }
  .navbar-brand { font-weight: 600; font-size: 1.25rem; display: flex; align-items: center; gap: 10px; }
  .navbar-brand::before { content: ''; width: 22px; height: 22px; background: #FF6C5E; border-radius: 3px; flex: 0 0 auto; }
  .navbar .navbar-nav { align-items: stretch; }
  .navbar .nav-item { display: flex; align-items: stretch; }
  .navbar .nav-link { color: #000 !important; display: flex; align-items: center;
                      padding: 0 !important; margin-right: 28px;
                      border-bottom: 5px solid transparent; margin-bottom: -2px; }
  .navbar .nav-link.active { font-weight: 600; border-bottom-color: #FF6C5E; position: relative; z-index: 1; }
  .navbar .nav-link:hover { border-bottom-color: #000; }
  .navbar-updated { font-size: 0.875rem; color: #000; }

  /* Filter bar. No rule of its own; the KPI strip draws the next line. */
  .filter-bar { display: grid; grid-template-columns: 1.15fr repeat(4, minmax(0, 1fr)); gap: 16px;
                padding: 18px 0 22px; margin-bottom: 0; }
  .filter-bar .form-group { margin-bottom: 0; }
  .filter-bar label, .kpi-label {
    font-size: 12px; font-weight: 600; letter-spacing: .08em; text-transform: uppercase; color: #000; }
  .selectize-input {
    border: 2px solid #000 !important; border-radius: 3px !important; box-shadow: none !important;
    color: #000; min-height: 40px; background: #fff !important; }
  .selectize-input.focus { box-shadow: 0 0 0 3px #FF6C5E !important; }
  .selectize-input .item { background: #FF6C5E !important; color: #000 !important; border: 0 !important; border-radius: 3px; }
  .filter-bar .selectize-input { position: relative; padding-right: 28px !important; }
  .filter-bar .selectize-input::after {
    content: ''; position: absolute; right: 12px; top: 50%; width: 8px; height: 8px;
    border-right: 2px solid #000; border-bottom: 2px solid #000;
    transform: translateY(-70%) rotate(45deg); pointer-events: none;
  }
  .shiny-date-range-input .input-group {
    flex-wrap: nowrap; align-items: center; min-height: 40px;
    border: 2px solid #000 !important; border-radius: 3px !important; background: #fff;
  }
  .shiny-date-range-input .input-group:focus-within { box-shadow: 0 0 0 3px #FF6C5E !important; }
  .shiny-date-range-input .form-control {
    border: 0 !important; box-shadow: none !important; background: transparent; min-width: 0;
    padding: 0.35rem 0.15rem; font-size: 0.95rem; text-align: center;
  }
  .shiny-date-range-input .input-group-text {
    border: 0 !important; background: transparent; padding: 0 0.1rem; color: #000;
  }
  .shiny-date-range-input .input-group::after {
    content: ''; width: 8px; height: 8px; margin: 0 12px 0 4px; flex: 0 0 auto;
    border-right: 2px solid #000; border-bottom: 2px solid #000;
    transform: translateY(-2px) rotate(45deg);
  }

  /* KPI strip: one row of ruled cells */
  .kpi-row { display: flex; border-top: 2px solid #000; border-bottom: 2px solid #000; }
  .kpi-cell { flex: 1 1 0; min-width: 0; padding: 16px 20px; border-left: 2px solid #000;
              display: flex; flex-direction: column; gap: 6px; }
  .kpi-cell:first-child { border-left: 0; }
  .kpi-value { font-size: 2.75rem; line-height: 1; font-weight: 600; font-variant-numeric: tabular-nums; }
  .kpi-sub { font-size: 0.875rem; line-height: 1.3; }

  /* Cards: 2px black rule, 3px radius, no shadow */
  .dash-card { border: 2px solid #000 !important; border-radius: 3px !important; box-shadow: none !important; }
  .dash-card > .card-header { border-bottom: 0; padding: 18px 20px 4px; }
  .card-head-text { min-width: 0; flex: 1 1 auto; }
  .card-head-aside { display: flex; justify-content: space-between; align-items: baseline; gap: 1.5rem; }
  .card-title { font-weight: 600; font-size: 1.125rem; color: #000; line-height: 1.3; }
  .card-note { color: #000; font-size: 0.875rem; font-weight: 400; margin-top: 2px; line-height: 1.35; }
  .card-head-aside .card-note { margin-top: 0; text-align: right; max-width: 52%; }
  .section-label { font-size: 1.375rem; font-weight: 600; color: #000; margin-top: 0.5rem; }

  /* Tables */
  .dataTables_wrapper { font-size: 0.9rem; }
  table.dataTable thead th { border-bottom: 2px solid #000 !important; color: #000; }
  table.dataTable.stripe tbody tr.odd > * { box-shadow: none !important; }
  table.dataTable tbody tr:hover > * { background: #FF6C5E !important; box-shadow: none !important; }
  .dataTables_paginate .paginate_button.current { background: #000 !important; color: #fff !important;
                                                  border-radius: 3px; border: 0 !important; }
"
theme <- bs_add_rules(theme, dash_css)

ui <- page_navbar(
  title = "DC Regwall Launch",
  window_title = "DC Regwall Launch",
  theme = theme,
  fillable = FALSE,
  gap = "1.75rem",
  padding = 0,
  navbar_options = navbar_options(bg = "#FFFFFF", theme = "light", underline = FALSE),
  header = tagList(tags$head(tags$style(HTML(font_css))), filter_bar),

  nav_panel(
    "Regwall overview",
    uiOutput("p1_kpis"),
    card_plot("Regwall funnel",
              "Unique readers at each step. Readers matched on anonymous reader ID; verification on account ID.",
              "p1_funnel", 300, aside = TRUE),
    layout_columns(
      col_widths = c(6, 6),
      card_plot("Performance over time", "Wall views (bars) and registrations (line) by day.", "p1_time"),
      card_plot("Registration rate by day", "Registered readers divided by readers who saw the regwall.", "p1_rate")
    ),
    tags$h5("New vs returning registrants", class = "section-label mb-0"),
    uiOutput("p1_newret")
  ),

  nav_panel(
    "Segments and content",
    layout_columns(
      col_widths = c(4, 4, 4),
      card_table("By device", "Device where the reader first saw the regwall.", "p2_device"),
      card_table("By traffic source", "UTM source on the page where the wall appeared.", "p2_source"),
      card_table("By topic", "Article topic (content group).", "p2_topic")
    ),
    card_table("Top pages behind the wall",
               "Ranked by readers who hit the regwall. Scrolled 90% means they reached 90% of that page the same day.",
               "p2_pages"),
    layout_columns(
      col_widths = c(6, 6),
      card_plot("Wall hits before registering",
                "Regwall views per registrant up to registration. 0 means no view on record.", "p2_hist"),
      card_plot("What happened after an email submit",
                "Readers who entered an email on the regwall.", "p2_outcome")
    )
  ),

  nav_panel(
    "Softwall and reader value",
    uiOutput("p3_kpis"),
    card_plot("Softwall funnel", "The dismissible newsletter modal.", "p3_funnel", 280),
    layout_columns(
      col_widths = c(6, 6),
      card_plot("Where registrations come from", "Every registration, by sign-up surface.", "p3_surface", 340),
      card_table("Newsletter subscribes",
                 "Unique readers by list. Unsubscribes appear once that event is tracked.", "p3_news")
    ),
    card_table("Registered vs anonymous readers",
               "Each session uses its highest status: member, then registered, then anonymous. Page type, source and variant don't apply here.",
               "p3_engage"),
    card_plot("Return within 7 days",
              "Share of sessions followed by another within 7 days. Only sessions at least 8 days old are included.",
              "p3_return", 260),
    tags$h5("From registration to membership", class = "section-label mb-0"),
    uiOutput("p3_member")
  ),
  nav_spacer(),
  nav_item(tags$span(format(DATA$loaded_at, "Updated %b %d, %I:%M %p"), class = "navbar-updated"))
)

# ---------------------------------------------------------------------
# Server
# ---------------------------------------------------------------------
server <- function(input, output, session) {

  in_range <- function(d) {
    dates <- input$dates
    if (is.null(dates) || length(dates) < 2 || anyNA(dates)) return(rep(TRUE, length(d)))
    if (dates[1] > dates[2]) dates <- rev(dates)
    d >= dates[1] & d <= dates[2]
  }

  funnel_f <- reactive({
    f <- DATA$funnel |> filter(in_range(event_date))
    if (length(input$device))     f <- filter(f, device %in% input$device)
    if (length(input$page_type))  f <- filter(f, page_type %in% input$page_type)
    if (length(input$utm_source)) f <- filter(f, utm_source %in% input$utm_source)
    if (length(input$variant))    f <- filter(f, variant_name %in% input$variant)
    f
  })
  reg  <- reactive(filter(funnel_f(), wall_type == "regwall"))
  soft <- reactive(filter(funnel_f(), wall_type == "softwall"))

  # ---------------- Page 1 ----------------
  output$p1_kpis <- renderUI({
    s <- funnel_summary(reg())
    kpi_row(
      kpi("Wall views", fmt_n(s$wall_views), "Regwall impressions"),
      kpi("Readers", fmt_n(s$viewed), "Unique readers who saw the wall"),
      kpi("Registrations", fmt_n(s$registered), "Completed from the regwall"),
      kpi("Registration rate", fmt_pct(safe_div(s$registered, s$viewed)), "Registered / saw the wall"),
      kpi("Verified", fmt_n(s$verified), "Verified after registering")
    )
  })

  output$p1_funnel <- renderPlot(funnel_plot(funnel_summary(reg()), FUNNEL_REG, include_cta = TRUE), res = 96)

  daily_reg <- reactive({
    reg() |>
      group_by(event_date) |>
      summarise(views = sum(wall_views, na.rm = TRUE),
                readers = nd(reader_id, viewed),
                registered = nd(reader_id, registered), .groups = "drop") |>
      mutate(rate = safe_div(registered, readers))
  })

  output$p1_time <- renderPlot({
    d <- daily_reg()
    if (!nrow(d)) return(empty_plot())
    k <- max(d$views) / max(1, max(d$registered))
    ggplot(d, aes(event_date)) +
      geom_col(aes(y = views), fill = RED_SOFT, color = INK, linewidth = 0.4) +
      geom_line(aes(y = registered * k), color = RED, linewidth = 1) +
      geom_point(aes(y = registered * k), fill = RED, color = INK, shape = 21, size = 3, stroke = 0.6) +
      scale_y_continuous("Wall views", labels = comma,
                         sec.axis = sec_axis(~ . / k, name = "Registrations")) +
      scale_x_date(NULL, date_labels = "%b %d") +
      theme_dash()
  }, res = 96)

  output$p1_rate <- renderPlot({
    d <- filter(daily_reg(), !is.na(rate))
    if (!nrow(d)) return(empty_plot())
    ggplot(d, aes(event_date, rate)) +
      geom_line(color = RED, linewidth = 1) +
      geom_point(color = RED, size = 2.5) +
      scale_y_continuous("Registration rate", labels = percent_format(accuracy = 1),
                         limits = c(0, NA)) +
      scale_x_date(NULL, date_labels = "%b %d") +
      theme_dash()
  }, res = 96)

  output$p1_newret <- renderUI({
    r <- filter(reg(), registered %in% TRUE)
    med <- suppressWarnings(median(r$days_first_visit_to_reg, na.rm = TRUE))
    kpi_row(
      kpi("First session",
          fmt_n(nd(r$reader_id, r$registrant_visit_type == "first session")),
          "Registered on their first visit"),
      kpi("Returning",
          fmt_n(nd(r$reader_id, r$registrant_visit_type == "returning")),
          "Had visited before registering"),
      kpi("Median days to register",
          if (is.finite(med)) fmt_n(med) else "n/a",
          "From first visit to registration")
    )
  })

  # ---------------- Page 2 ----------------
  output$p2_device <- renderDT(dt_table(segment_table(reg(), "device", "Device"), "Registration rate"))
  output$p2_source <- renderDT(dt_table(segment_table(reg(), "utm_source", "Source"), "Registration rate"))
  output$p2_topic  <- renderDT(dt_table(segment_table(reg(), "content_group", "Topic"), "Registration rate"))

  output$p2_pages <- renderDT({
    d <- reg() |>
      filter(!is.na(page_path)) |>
      group_by(Page = page_path) |>
      summarise(Topic = first(na.omit(content_group)),
                `Wall views` = sum(wall_views, na.rm = TRUE),
                Readers = nd(reader_id, viewed),
                Registered = nd(reader_id, registered),
                `Scrolled 90%` = safe_div(nd(reader_id, scrolled_90_on_wall_page), nd(reader_id, viewed)),
                .groups = "drop") |>
      arrange(desc(Readers))
    dt_table(d, "Scrolled 90%", page_length = 15)
  })

  output$p2_hist <- renderPlot({
    d <- reg() |>
      filter(registered %in% TRUE) |>
      mutate(bucket = ifelse(views_before_reg >= 4, "4+", as.character(views_before_reg)),
             bucket = factor(bucket, levels = c("0", "1", "2", "3", "4+"))) |>
      count(bucket, .drop = FALSE)
    if (sum(d$n) == 0) return(empty_plot("No registrations yet"))
    ggplot(d, aes(bucket, n)) +
      geom_col(fill = RED, width = 0.6) +
      geom_text(aes(label = n), vjust = -0.5, fontface = "bold") +
      scale_y_continuous(NULL, expand = expansion(mult = c(0, 0.15))) +
      labs(x = "Regwall views before registering") +
      theme_dash() + theme(axis.text.y = element_blank(), panel.grid = element_blank())
  }, res = 96)

  output$p2_outcome <- renderPlot({
    d <- reg() |>
      filter(submitted %in% TRUE) |>
      mutate(outcome = case_when(
        registered %in% TRUE ~ "New account",
        existing_user_login %in% TRUE ~ "Existing account (logged in)",
        submitted_no_outcome %in% TRUE ~ "No outcome",
        TRUE ~ "Other"
      )) |>
      group_by(outcome) |>
      summarise(n = n_distinct(reader_id), .groups = "drop")
    if (!nrow(d)) return(empty_plot("No email submits yet"))
    d <- d |> mutate(share = n / sum(n),
                     outcome = factor(outcome, levels = c("New account", "Existing account (logged in)",
                                                          "No outcome", "Other")))
    ggplot(d, aes(share, outcome)) +
      geom_col(aes(fill = outcome), color = INK, width = 0.6, show.legend = FALSE) +
      geom_text(aes(label = paste0(n, " (", percent(share, 1), ")")), hjust = -0.1) +
      scale_fill_manual(values = c("New account" = RED, "Existing account (logged in)" = "#FFFFFF",
                                   "No outcome" = "#000000", "Other" = "#FFFFFF")) +
      scale_x_continuous(NULL, labels = NULL, expand = expansion(mult = c(0, 0.3))) +
      labs(y = NULL) +
      theme_dash() + theme(panel.grid = element_blank())
  }, res = 96)

  # ---------------- Page 3 ----------------
  output$p3_kpis <- renderUI({
    s <- funnel_summary(soft())
    med <- suppressWarnings(median(soft()$seconds_to_dismiss, na.rm = TRUE))
    kpi_row(
      kpi("Softwall views", fmt_n(s$wall_views), "Newsletter modal impressions", tone = "blue"),
      kpi("Submit rate", fmt_pct(safe_div(s$submitted, s$viewed)), "Submitted email / saw the wall", tone = "blue"),
      kpi("Dismiss rate", fmt_pct(safe_div(s$dismissed, s$viewed)), "Excludes closing after a submit", tone = "blue"),
      kpi("Seconds to dismiss", if (is.finite(med)) number(med, accuracy = 0.1) else "n/a", "Median, view to dismiss", tone = "blue")
    )
  })

  output$p3_funnel <- renderPlot(funnel_plot(funnel_summary(soft()), FUNNEL_SOFT, include_cta = FALSE), res = 96)

  output$p3_surface <- renderPlot({
    d <- DATA$surfaces |>
      filter(in_range(event_date)) |>
      group_by(surface) |>
      summarise(n = n_distinct(account_key), .groups = "drop") |>
      arrange(n) |>
      mutate(surface = factor(gsub("_", " ", surface), levels = gsub("_", " ", surface)))
    if (!nrow(d)) return(empty_plot())
    known <- c(
      "regwall" = RED,
      "softwall" = BLUE,
      "newsletter archive" = "#FF6C5E",
      "newsletter block" = "#C93B32",
      "newsletter footer" = "#E09F3E",
      "nav" = "#1A1A1A",
      "checkout" = "#7FA0C4",
      "footer" = "#F2B5AE",
      "right rail" = "#5C6B7A",
      "article inline" = "#8E5A4A",
      "(not set)" = "#C8C8C8"
    )
    spare <- c("#E09F3E", "#F2B5AE", "#5C6B7A", "#C93B32", "#8E5A4A", "#7FA0C4", "#1A1A1A")
    names_s <- levels(d$surface)
    fills <- known[names_s]
    missing <- which(is.na(fills))
    if (length(missing)) {
      fills[missing] <- spare[((seq_along(missing) - 1) %% length(spare)) + 1]
    }
    names(fills) <- names_s
    ggplot(d, aes(n, surface)) +
      geom_col(aes(fill = surface), color = INK, linewidth = 0.35, width = 0.7) +
      geom_text(aes(label = n), hjust = -0.3, fontface = "bold") +
      scale_fill_manual(values = fills, guide = "none") +
      scale_x_continuous(NULL, labels = NULL, expand = expansion(mult = c(0, 0.15))) +
      labs(y = NULL) +
      theme_dash() + theme(panel.grid = element_blank())
  }, res = 96)

  output$p3_news <- renderDT({
    d <- DATA$newsletters |>
      filter(in_range(event_date)) |>
      group_by(Newsletter = newsletter) |>
      summarise(Subscribed = n_distinct(reader_id[event_name == "newsletter_subscribe"]),
                Unsubscribed = n_distinct(reader_id[event_name == "newsletter_unsubscribe"]),
                .groups = "drop") |>
      mutate(Net = Subscribed - Unsubscribed) |>
      arrange(desc(Subscribed))
    dt_table(d)
  })

  sessions_f <- reactive({
    s <- DATA$sessions |> filter(in_range(event_date))
    if (length(input$device)) s <- filter(s, device %in% input$device)
    s
  })

  output$p3_engage <- renderDT({
    d <- sessions_f() |>
      group_by(Status = membership_status) |>
      summarise(Sessions = sum(sessions),
                `Pages per session` = sum(pageviews) / sum(sessions),
                eng = sum(engaged_seconds) / sum(sessions),
                `Articles read to 90%` = sum(articles_scrolled_90) / sum(sessions),
                .groups = "drop")
    if (!nrow(d)) return(dt_table(tibble(Note = "No sessions for the selected filters")))
    base <- d$`Pages per session`[d$Status == "anonymous"]
    d <- d |>
      mutate(`Engaged time per session` = sprintf("%d:%02d", as.integer(eng %/% 60), as.integer(round(eng %% 60))),
             `Pages vs anonymous` = if (length(base)) `Pages per session` / base - 1 else NA_real_,
             Status = factor(Status, levels = c("anonymous", "registered", "member"))) |>
      arrange(Status) |>
      select(Status, Sessions, `Pages per session`, `Engaged time per session`,
             `Articles read to 90%`, `Pages vs anonymous`)
    dt_table(d, "Pages vs anonymous") |>
      formatRound(c("Pages per session", "Articles read to 90%"), digits = 2) |>
      formatRound("Sessions", digits = 0)
  })

  output$p3_return <- renderPlot({
    d <- sessions_f() |>
      group_by(membership_status) |>
      summarise(rate = safe_div(sum(sessions_returned), sum(sessions_window_complete)),
                n = sum(sessions_window_complete), .groups = "drop") |>
      filter(n > 0)
    if (!nrow(d)) return(empty_plot("Return rates appear once sessions are 8+ days old"))
    ggplot(d, aes(rate, factor(membership_status, levels = rev(c("anonymous", "registered", "member"))))) +
      geom_col(fill = BLUE, width = 0.6) +
      geom_text(aes(label = percent(rate, 1)), hjust = -0.2, fontface = "bold") +
      scale_x_continuous(NULL, labels = NULL, expand = expansion(mult = c(0, 0.2))) +
      labs(y = NULL) +
      theme_dash() + theme(panel.grid = element_blank())
  }, res = 96)

  output$p3_member <- renderUI({
    m <- DATA$membership |> filter(in_range(reg_date))
    kpi_row(
      kpi("Registered readers", fmt_n(n_distinct(m$account_key)), "Signed up in this date range"),
      kpi("Viewed checkout", fmt_n(nd(m$account_key, m$viewed_checkout)), "Opened checkout after registering"),
      kpi("Advanced in checkout", fmt_n(nd(m$account_key, m$advanced_checkout)), "Moved past the first step"),
      kpi("Became members", fmt_n(nd(m$account_key, m$became_member)), "Started a subscription")
    )
  })
}

shinyApp(ui, server)