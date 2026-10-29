# ============================================================
# Smart Air Pollution Risk Prediction System
# File: app.R
# Run main.R first, then run this Shiny dashboard.
# ============================================================

library(shiny)
library(ggplot2)
library(plotly)
library(dplyr)
library(randomForest)

if (!file.exists("air_risk_model.rds")) {
  stop("Model file not found. Run source('main.R') first.")
}
model <- readRDS("air_risk_model.rds")

default_values <- list(
  pm25 = 25, pm10 = 50, no = 5, no2 = 15, nox = 15, nh3 = 10,
  co = 0.5, so2 = 5, o3 = 30, benzene = 1, toluene = 2, xylene = 0.5
)

css <- "
body { background:#f1f6fb; color:#183653; font-family:'Segoe UI',Arial,sans-serif; }
.topbar { background:linear-gradient(110deg,#102f50,#146c83); color:white; padding:24px 28px; border-radius:14px; margin-bottom:20px; }
.brand { font-size:28px; font-weight:750; }
.subtitle { color:#d4edf5; margin-top:6px; }
.ready { display:inline-block; background:#d8fff0; color:#087b5e; padding:8px 13px; border-radius:22px; font-weight:700; }
.card { background:white; border:1px solid #deebf5; border-radius:14px; padding:18px; margin-bottom:18px; box-shadow:0 5px 18px rgba(26,70,110,.06); }
.kpi-label { color:#6b849e; font-size:13px; text-transform:uppercase; letter-spacing:.5px; font-weight:700; }
.kpi-value { font-size:29px; font-weight:800; color:#173f60; margin-top:7px; }
.section-title { font-size:19px; font-weight:750; color:#173f60; margin-bottom:5px; }
.section-sub { color:#718ba4; font-size:13px; margin-bottom:13px; }
label { color:#385873; font-weight:650; }
.form-control { border-radius:8px; border-color:#d6e4ef; }
.btn-predict { background:linear-gradient(90deg,#168bb5,#10ad91); color:white; border:0; border-radius:8px; font-weight:750; padding:11px; width:100%; }
.btn-predict:hover { color:white; opacity:.9; }
.risk-low { background:#e4fbf3; border:1px solid #b9efdc; color:#087c5e; }
.risk-medium { background:#fff6df; border:1px solid #f8dfa0; color:#986900; }
.risk-high { background:#fff0ef; border:1px solid #f6c9c5; color:#bd3939; }
.risk-banner { border-radius:14px; padding:21px; margin-bottom:18px; }
.footer { color:#7a90a5; text-align:center; font-size:12px; padding:12px; }
"

ui <- fluidPage(
  tags$head(
    tags$style(HTML(css)),
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1")
  ),
  div(class = "topbar",
      fluidRow(
        column(9,
          div(class = "brand", HTML("&#127807; Smart Air Pollution Risk Prediction")),
          div(class = "subtitle", "Interactive machine-learning dashboard • R + Shiny")
        ),
        column(3, div(style = "text-align:right;padding-top:8px;",
                      div(class = "ready", "● MODEL READY")))
      )
  ),

  fluidRow(
    column(3, div(class = "card",
      div(class = "kpi-label", "Predicted Risk"),
      uiOutput("risk_kpi")
    )),
    column(3, div(class = "card",
      div(class = "kpi-label", "PM2.5 Reading"),
      div(class = "kpi-value", textOutput("pm25_kpi", inline = TRUE)),
      tags$small("Entered value")
    )),
    column(3, div(class = "card",
      div(class = "kpi-label", "PM10 Reading"),
      div(class = "kpi-value", textOutput("pm10_kpi", inline = TRUE)),
      tags$small("Entered value")
    )),
    column(3, div(class = "card",
      div(class = "kpi-label", "ML Algorithm"),
      div(class = "kpi-value", style = "font-size:23px;", "Random Forest"),
      tags$small("Trained model")
    ))
  ),

  fluidRow(
    column(4,
      div(class = "card",
        div(class = "section-title", "⚙ Pollutant Inputs"),
        div(class = "section-sub", "Enter readings, then click Predict Risk."),
        fluidRow(
          column(6, numericInput("pm25", "PM2.5", default_values$pm25, min = 0)),
          column(6, numericInput("pm10", "PM10", default_values$pm10, min = 0)),
          column(6, numericInput("no", "NO", default_values$no, min = 0)),
          column(6, numericInput("no2", "NO2", default_values$no2, min = 0)),
          column(6, numericInput("nox", "NOx", default_values$nox, min = 0)),
          column(6, numericInput("nh3", "NH3", default_values$nh3, min = 0)),
          column(6, numericInput("co", "CO", default_values$co, min = 0)),
          column(6, numericInput("so2", "SO2", default_values$so2, min = 0)),
          column(6, numericInput("o3", "O3", default_values$o3, min = 0)),
          column(6, numericInput("benzene", "Benzene", default_values$benzene, min = 0)),
          column(6, numericInput("toluene", "Toluene", default_values$toluene, min = 0)),
          column(6, numericInput("xylene", "Xylene", default_values$xylene, min = 0))
        ),
        actionButton("predict", "✦ Predict Risk", class = "btn-predict"),
        br(), br(),
        actionButton("reset", "↻ Reset Values", class = "btn btn-default", style = "width:100%;")
      ),
      div(class = "card",
        div(class = "section-title", "About this project"),
        p("This educational model predicts a simplified risk category from pollutant readings."),
        p("Categories are based on AQI labels in the training dataset: Low ≤ 100, Medium 101–200, High > 200.")
      )
    ),

    column(8,
      uiOutput("risk_banner"),
      fluidRow(
        column(6, div(class = "card",
          div(class = "section-title", "Historical AQI Trend"),
          div(class = "section-sub", "Average AQI by year from city_day.csv"),
          plotlyOutput("trend", height = "280px")
        )),
        column(6, div(class = "card",
          div(class = "section-title", "Entered Pollutant Levels"),
          div(class = "section-sub", "Compare current input readings"),
          plotlyOutput("pollutants", height = "280px")
        ))
      ),
      fluidRow(
        column(6, div(class = "card",
          div(class = "section-title", "Risk Distribution"),
          div(class = "section-sub", "Share of historical AQI risk labels"),
          plotlyOutput("risk_donut", height = "320px")
        )),
        column(6, div(class = "card",
          div(class = "section-title", "Pollutant Correlation Heatmap"),
          div(class = "section-sub", "Hover over a cell to inspect correlation"),
          plotlyOutput("correlation_heatmap", height = "320px")
        ))
      ),
      div(class = "card",
        div(class = "section-title", "🌿 Recommendation"),
        uiOutput("advice")
      ),
      div(class = "footer",
          "Educational model only • Not an official AQI forecast • Follow local public-health guidance")
    )
  )
)

server <- function(input, output, session) {
  state <- reactiveValues(risk = "Low", predicted = FALSE)

  observeEvent(input$reset, {
    for (nm in names(default_values)) {
      updateNumericInput(session, nm, value = default_values[[nm]])
    }
    state$risk <- "Low"
    state$predicted <- FALSE
  })

  observeEvent(input$predict, {
    vals <- list(
      input$pm25, input$pm10, input$no, input$no2, input$nox, input$nh3,
      input$co, input$so2, input$o3, input$benzene, input$toluene, input$xylene
    )
    req(all(vapply(vals, function(x) length(x) == 1 && is.finite(x) && x >= 0, logical(1))))

    new_data <- data.frame(
      "PM2.5" = input$pm25, "PM10" = input$pm10,
      "NO" = input$no, "NO2" = input$no2, "NOx" = input$nox,
      "NH3" = input$nh3, "CO" = input$co, "SO2" = input$so2,
      "O3" = input$o3, "Benzene" = input$benzene,
      "Toluene" = input$toluene, "Xylene" = input$xylene,
      check.names = FALSE
    )
    state$risk <- as.character(predict(model, newdata = new_data))[1]
    state$predicted <- TRUE
  })

  output$pm25_kpi <- renderText(input$pm25)
  output$pm10_kpi <- renderText(input$pm10)

  output$risk_kpi <- renderUI({
    colour <- switch(state$risk, Low = "#078263", Medium = "#b47b00", High = "#c23838")
    div(class = "kpi-value", style = paste0("color:", colour, ";"), toupper(state$risk))
  })

  output$risk_banner <- renderUI({
    cls <- switch(state$risk, Low = "risk-low", Medium = "risk-medium", High = "risk-high")
    icon_text <- switch(state$risk, Low = "✓", Medium = "!", High = "⚠")
    message <- switch(state$risk,
      Low = "The model classifies these readings as low risk.",
      Medium = "The model classifies these readings as medium risk.",
      High = "The model classifies these readings as high risk."
    )
    div(class = paste("risk-banner", cls),
        div(style = "font-size:25px;font-weight:800;", paste(icon_text, state$risk, "Pollution Risk")),
        div(style = "margin-top:7px;", message))
  })

  output$advice <- renderUI({
    msg <- switch(state$risk,
      Low = "The model predicts low risk. Continue checking official local air-quality information.",
      Medium = "The model predicts medium risk. Sensitive people may consider limiting strenuous outdoor activity.",
      High = "The model predicts high risk. Check official alerts and follow public-health recommendations."
    )
    div(style = "font-size:15px;line-height:1.7;", msg)
  })

  output$trend <- renderPlotly({
    validate(need(file.exists("city_day.csv"), "Place city_day.csv in this folder to show historical data."))
    aq <- read.csv("city_day.csv", stringsAsFactors = FALSE, check.names = FALSE)
    validate(need(all(c("Date", "AQI") %in% names(aq)), "Date or AQI column is missing."))
    aq$Date <- as.Date(aq$Date)
    aq$AQI <- suppressWarnings(as.numeric(aq$AQI))
    hist <- aq |>
      filter(!is.na(Date), !is.na(AQI)) |>
      mutate(Year = format(Date, "%Y")) |>
      group_by(Year) |>
      summarise(AQI = mean(AQI), .groups = "drop")
    validate(need(nrow(hist) > 0, "No historical AQI values found."))
    p <- ggplot(hist, aes(Year, AQI, group = 1)) +
      geom_line(color = "#10AD91", linewidth = 1.2) +
      geom_point(color = "#087F76", size = 2.5) +
      labs(x = NULL, y = "Average AQI") +
      theme_minimal(base_size = 12)
    ggplotly(p, tooltip = c("x", "y"))
  })

  output$pollutants <- renderPlotly({
    vals <- data.frame(
      Pollutant = c("PM2.5", "PM10", "NO2", "O3"),
      Value = c(input$pm25, input$pm10, input$no2, input$o3)
    )
    p <- ggplot(vals, aes(x = Value, y = reorder(Pollutant, Value), fill = Pollutant)) +
      geom_col(show.legend = FALSE, width = 0.65) +
      geom_text(aes(label = Value), hjust = -0.25, size = 3.8) +
      scale_fill_manual(values = c("#10AD91", "#36B7B0", "#3898EE", "#9361DC")) +
      scale_x_continuous(limits = c(0, max(1, max(vals$Value, na.rm = TRUE) * 1.25))) +
      labs(x = "Entered reading", y = NULL) +
      theme_minimal(base_size = 12) +
      theme(panel.grid.major.y = element_blank())
    ggplotly(p, tooltip = c("x", "y"))
  })

  output$risk_donut <- renderPlotly({
    validate(need(file.exists("city_day.csv"), "Place city_day.csv in this folder to show risk distribution."))
    aq <- read.csv("city_day.csv", stringsAsFactors = FALSE, check.names = FALSE)
    validate(need("AQI" %in% names(aq), "AQI column was not found."))
    aq$AQI <- suppressWarnings(as.numeric(aq$AQI))
    aq <- aq[!is.na(aq$AQI), ]
    validate(need(nrow(aq) > 0, "No valid AQI records found."))
    aq$Risk <- cut(aq$AQI, breaks = c(-Inf, 100, 200, Inf),
                   labels = c("Low", "Medium", "High"))
    counts <- as.data.frame(table(aq$Risk))
    names(counts) <- c("Risk", "Count")
    plot_ly(counts, labels = ~Risk, values = ~Count, type = "pie", hole = 0.62,
            sort = FALSE, textinfo = "label+percent",
            marker = list(colors = c("#10AD91", "#F2B544", "#E65353")),
            hovertemplate = "%{label}<br>Records: %{value}<br>Share: %{percent}<extra></extra>") |>
      layout(showlegend = TRUE,
             legend = list(orientation = "h", y = -0.12),
             margin = list(t = 10, b = 35, l = 10, r = 10))
  })

  output$correlation_heatmap <- renderPlotly({
    validate(need(file.exists("city_day.csv"), "Place city_day.csv in this folder to show correlations."))
    aq <- read.csv("city_day.csv", stringsAsFactors = FALSE, check.names = FALSE)
    pollutants <- c("PM2.5", "PM10", "NO", "NO2", "NOx", "NH3", "CO", "SO2", "O3")
    pollutants <- intersect(pollutants, names(aq))
    validate(need(length(pollutants) >= 2, "Not enough pollutant columns found."))
    aq[pollutants] <- lapply(aq[pollutants], function(x) suppressWarnings(as.numeric(as.character(x))))
    corr <- cor(aq[, pollutants], use = "pairwise.complete.obs")
    plot_ly(
      x = colnames(corr), y = rownames(corr), z = corr,
      type = "heatmap", zmin = -1, zmax = 1,
      colors = colorRamp(c("#2878B5", "#FFFFFF", "#10AD91")),
      hovertemplate = "X: %{x}<br>Y: %{y}<br>Correlation: %{z:.2f}<extra></extra>"
    ) |>
      layout(xaxis = list(title = ""), yaxis = list(title = ""),
             margin = list(l = 65, b = 50, t = 5, r = 5))
  })
}

shinyApp(ui = ui, server = server)
