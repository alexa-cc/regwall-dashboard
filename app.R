# =====================================================================
# DC Regwall Dashboard (Shiny)
# Reads the dc_mart models in BigQuery: wall_reader_funnel,
# session_engagement, registration_to_membership, ga4_events.
#
# Packages:
#   install.packages(c("shiny", "bslib", "bigrquery", "dplyr", "tidyr",
#                      "ggplot2", "scales", "DT"))
#
# Auth: the first run opens a browser to sign in with your Google account.
# For a deployed app (shinyapps.io / Posit Connect), use a service account
# key instead: set BQ_KEY_PATH to the JSON key file's path.
# =====================================================================

library(shiny)
library(bslib)
library(bigrquery)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(DT)

PROJECT <- "ccc-citycast-prod"
DATASET <- "dc_mart"
LAUNCH  <- as.Date("2026-10-05")

RED      <- "#B50E28"
RED_SOFT <- "#F3C9CF"
BLUE     <- "#2F5D8A"
INK      <- "#17202A"
MUTED    <- "#56606C"

if (nzchar(Sys.getenv("BQ_KEY_PATH"))) {
  bq_auth(path = Sys.getenv("BQ_KEY_PATH"))
} else {
  bq_auth()
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
      WHERE event_name IN ('newsletter_subscribe', 'newsletter_unsubscribe')",
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
  theme_minimal(base_size = 13) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      axis.title = element_text(color = MUTED, size = 11),
      axis.text = element_text(color = MUTED),
      plot.background = element_rect(fill = "transparent", color = NA),
      panel.background = element_rect(fill = "transparent", color = NA),
      plot.margin = margin(8, 16, 8, 8)
    )
}

empty_plot <- function(msg = "No data for the selected filters") {
  ggplot() + annotate("text", x = 0, y = 0, label = msg, color = MUTED, size = 4.5) + theme_void()
}

funnel_plot <- function(s, color, include_cta = TRUE) {
  steps <- tibble(
    step = c("Saw the wall", "Clicked CTA", "Submitted email", "Registered", "Verified"),
    n    = c(s$viewed, s$cta, s$submitted, s$registered, s$verified)
  )
  if (!include_cta) steps <- filter(steps, step != "Clicked CTA")
  if (steps$n[1] == 0) return(empty_plot())
  steps <- steps |>
    mutate(
      label = ifelse(step == "Clicked CTA" & n == 0,
                     "0",
                     paste0(fmt_n(n), "  (", fmt_pct(n / steps$n[1]), " of readers)")),
      step = factor(step, levels = rev(step))
    )
  ggplot(steps, aes(n, step)) +
    geom_col(fill = color, width = 0.7) +
    geom_text(aes(label = label), hjust = -0.05, color = INK, size = 4) +
    scale_x_continuous(expand = expansion(mult = c(0, 0.58))) +
    labs(x = NULL, y = NULL) +
    theme_dash() +
    theme(panel.grid = element_blank(), axis.text.x = element_blank(),
          axis.text.y = element_text(color = INK, size = 12, face = "bold"))
}

kpi <- function(title, value, sub = NULL, tone = c("red", "blue")) {
  tone <- match.arg(tone)
  value_box(
    title = title,
    value = value,
    p(sub, class = "text-muted small mb-0"),
    theme = value_box_theme(bg = "#FFFFFF", fg = INK),
    min_height = "112px",
    fill = TRUE,
    class = paste("border-0 shadow-sm kpi-box", if (tone == "blue") "kpi-blue")
  )
}

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
theme <- bs_theme(
  version = 5, primary = RED, fg = INK, bg = "#F4F5F7",
  base_font = font_google("Public Sans", wght = c(400, 500, 600, 700)),
  heading_font = font_google("Public Sans", wght = c(600, 700))
)

card_head <- function(title, note) {
  card_header(
    class = "bg-transparent",
    tags$div(
      class = "card-head-text",
      tags$div(title, class = "card-title"),
      tags$div(note, class = "card-note")
    )
  )
}

card_plot <- function(title, note, id, height = 300) {
  card(
    class = "dash-card",
    full_screen = TRUE,
    card_head(title, note),
    card_body(padding = 3, plotOutput(id, height = height))
  )
}
card_table <- function(title, note, id) {
  card(
    class = "dash-card",
    card_head(title, note),
    card_body(padding = 3, DTOutput(id))
  )
}

dash_css <- "
  .navbar { border-bottom: 1px solid rgba(23, 32, 42, 0.08); }
  .navbar-updated { font-size: 0.82rem; color: #56606C; padding-right: 0.35rem; }
  .bslib-sidebar-layout > .sidebar { border-right: 1px solid rgba(23, 32, 42, 0.08); }
  .shiny-date-range-input .input-group { flex-wrap: nowrap; }
  .shiny-date-range-input .form-control {
    min-width: 0; padding: 0.35rem 0.4rem; font-size: 0.82rem;
  }
  .shiny-date-range-input .input-group-text { padding: 0.35rem 0.45rem; font-size: 0.78rem; }
  .dash-card {
    border: 1px solid rgba(23, 32, 42, 0.08) !important;
    box-shadow: 0 1px 2px rgba(23, 32, 42, 0.04);
  }
  .dash-card > .card-header {
    align-items: flex-start;
    border-bottom: 1px solid rgba(23, 32, 42, 0.06);
    padding: 0.9rem 1rem 0.75rem;
  }
  .card-head-text { min-width: 0; flex: 1 1 auto; }
  .card-title { font-weight: 650; font-size: 1rem; color: #17202A; line-height: 1.3; }
  .card-note { color: #56606C; font-size: 0.8rem; font-weight: 400; margin-top: 0.2rem; line-height: 1.35; }
  .kpi-box .value-box-title { font-size: 0.8rem; font-weight: 600; line-height: 1.25; }
  .kpi-box .value-box-value { font-size: 1.65rem; letter-spacing: -0.02em; font-variant-numeric: tabular-nums; }
  .kpi-box { border-top: 3px solid #B50E28 !important; }
  .kpi-box.kpi-blue { border-top-color: #2F5D8A !important; }
  .dataTables_wrapper { font-size: 0.88rem; }
  table.dataTable thead th { font-weight: 600; color: #17202A; }
  .sidebar-note { color: #56606C; font-size: 0.78rem; line-height: 1.4; margin: 0.25rem 0 0; }
  .section-label { font-size: 0.95rem; font-weight: 650; color: #17202A; margin-top: 0.35rem; }
"
theme <- bs_add_rules(theme, dash_css)

ui <- page_navbar(
  title = "DC regwall launch",
  window_title = "DC regwall launch",
  theme = theme,
  fillable = FALSE,
  gap = "1.15rem",
  padding = c("1.1rem", "1.25rem", "2rem", "1.25rem"),
  navbar_options = navbar_options(bg = "#FFFFFF", theme = "light", underline = TRUE),
  sidebar = sidebar(
    width = 292,
    bg = "#FFFFFF",
    fg = INK,
    gap = "0.85rem",
    padding = "1rem",
    resizable = FALSE,
    dateRangeInput("dates", "Date range", start = RANGE_START, end = RANGE_END,
                   max = Sys.Date(), width = "100%"),
    selectizeInput("device", "Device", DEVICE_CHOICES, multiple = TRUE,
                   width = "100%", options = list(placeholder = "All devices")),
    selectizeInput("page_type", "Page type", PAGE_TYPE_CHOICES, multiple = TRUE,
                   width = "100%", options = list(placeholder = "All types")),
    selectizeInput("utm_source", "Traffic source", SOURCE_CHOICES, multiple = TRUE,
                   width = "100%", options = list(placeholder = "All sources")),
    selectizeInput("variant", "Variant", VARIANT_CHOICES, multiple = TRUE,
                   width = "100%", options = list(placeholder = "All variants")),
    p("Page type, source, and variant filter the wall charts. Session charts use the date range and device.",
      class = "sidebar-note")
  ),

  nav_panel(
    "Regwall overview",
    uiOutput("p1_kpis"),
    card_plot("Regwall funnel",
              "Unique readers at each step. Readers matched on anonymous reader ID; verification on account ID.",
              "p1_funnel", 300),
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
               "Each session uses its highest status: member, then registered, then anonymous. Filtered by date range and device.",
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
    layout_column_wrap(
      width = 210,
      kpi("Wall views", fmt_n(s$wall_views), "Regwall impressions"),
      kpi("Readers", fmt_n(s$viewed), "Unique readers who saw the wall"),
      kpi("Registrations", fmt_n(s$registered), "Completed from the regwall"),
      kpi("Registration rate", fmt_pct(safe_div(s$registered, s$viewed)), "Registered / saw the wall"),
      kpi("Verified", fmt_n(s$verified), "Verified after registering")
    )
  })
  
  output$p1_funnel <- renderPlot(funnel_plot(funnel_summary(reg()), RED, include_cta = TRUE))
  
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
      geom_col(aes(y = views), fill = RED_SOFT, color = RED, linewidth = 0.3) +
      geom_line(aes(y = registered * k), color = RED, linewidth = 1) +
      geom_point(aes(y = registered * k), color = RED, size = 2.5) +
      scale_y_continuous("Wall views", labels = comma,
                         sec.axis = sec_axis(~ . / k, name = "Registrations")) +
      scale_x_date(NULL, date_labels = "%b %d") +
      theme_dash()
  })
  
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
  })
  
  output$p1_newret <- renderUI({
    r <- filter(reg(), registered %in% TRUE)
    med <- suppressWarnings(median(r$days_first_visit_to_reg, na.rm = TRUE))
    layout_column_wrap(
      width = 220,
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
  })
  
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
      geom_col(aes(fill = outcome), width = 0.6, show.legend = FALSE) +
      geom_text(aes(label = paste0(n, " (", percent(share, 1), ")")), hjust = -0.1) +
      scale_fill_manual(values = c("New account" = RED, "Existing account (logged in)" = MUTED,
                                   "No outcome" = "#9AA3AD", "Other" = "#C9CED4")) +
      scale_x_continuous(NULL, labels = NULL, expand = expansion(mult = c(0, 0.3))) +
      labs(y = NULL) +
      theme_dash() + theme(panel.grid = element_blank())
  })
  
  # ---------------- Page 3 ----------------
  output$p3_kpis <- renderUI({
    s <- funnel_summary(soft())
    med <- suppressWarnings(median(soft()$seconds_to_dismiss, na.rm = TRUE))
    layout_column_wrap(
      width = 210,
      kpi("Softwall views", fmt_n(s$wall_views), "Newsletter modal impressions", tone = "blue"),
      kpi("Submit rate", fmt_pct(safe_div(s$submitted, s$viewed)), "Submitted email / saw the wall", tone = "blue"),
      kpi("Dismiss rate", fmt_pct(safe_div(s$dismissed, s$viewed)), "Excludes closing after a submit", tone = "blue"),
      kpi("Seconds to dismiss", if (is.finite(med)) number(med, accuracy = 0.1) else "n/a", "Median, view to dismiss", tone = "blue")
    )
  })
  
  output$p3_funnel <- renderPlot(funnel_plot(funnel_summary(soft()), BLUE, include_cta = FALSE))
  
  output$p3_surface <- renderPlot({
    d <- DATA$surfaces |>
      filter(in_range(event_date)) |>
      group_by(surface) |>
      summarise(n = n_distinct(account_key), .groups = "drop") |>
      arrange(n) |>
      mutate(surface = factor(gsub("_", " ", surface), levels = gsub("_", " ", surface)),
             fill = case_when(surface == "regwall" ~ RED, surface == "softwall" ~ BLUE, TRUE ~ MUTED))
    if (!nrow(d)) return(empty_plot())
    ggplot(d, aes(n, surface)) +
      geom_col(aes(fill = fill), width = 0.7) +
      geom_text(aes(label = n), hjust = -0.3, fontface = "bold") +
      scale_fill_identity() +
      scale_x_continuous(NULL, labels = NULL, expand = expansion(mult = c(0, 0.15))) +
      labs(y = NULL) +
      theme_dash() + theme(panel.grid = element_blank())
  })
  
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
  })
  
  output$p3_member <- renderUI({
    m <- DATA$membership |> filter(in_range(reg_date))
    layout_column_wrap(
      width = 210,
      kpi("Registered readers", fmt_n(n_distinct(m$account_key)), "Signed up in this date range"),
      kpi("Viewed checkout", fmt_n(nd(m$account_key, m$viewed_checkout)), "Opened checkout after registering"),
      kpi("Advanced in checkout", fmt_n(nd(m$account_key, m$advanced_checkout)), "Moved past the first step"),
      kpi("Became members", fmt_n(nd(m$account_key, m$became_member)), "Started a subscription")
    )
  })
}

shinyApp(ui, server)
