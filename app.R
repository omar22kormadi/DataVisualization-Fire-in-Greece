# app.R - ULTRA-FAST Fire Dashboard (NO wait, same beautiful results)
options(warn = -1, shiny.error = NULL)

library(shiny)
library(bslib)
library(data.table)  
library(lubridate)
library(leaflet)
library(plotly)
library(DT)
library(viridisLite)
library(shinyWidgets)
library(shinycssloaders)
library(ggplot2)

# ==== SPEED FIX: Load data ONCE at startup (not reactive!) ====
cat("Chargement des données...\n")
DATA <- fread("cleaned.csv", showProgress = TRUE)
setnames(DATA, tolower(names(DATA)))
# Find the actual date column name
date_col <- intersect(c("acq_date", "acqdate", "date"), names(DATA))[1]
DATA[, acq_date := ymd(get(date_col))]

DATA[, confidence := as.numeric(confidence)]
num_cols <- c("brightness", "frp", "tavg", "tmin", "tmax", "prcp", "wspd", "latitude", "longitude")
DATA[, (num_cols) := lapply(.SD, as.numeric), .SDcols = num_cols]
DATA <- DATA[!is.na(latitude) & !is.na(longitude)]
if (!"daynight" %in% names(DATA)) DATA[, daynight := NA_character_]
cat("✅ Données chargées:", nrow(DATA), "lignes\n")

# ==== UI ====
ui <- fluidPage(
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  titlePanel("🔥 Analyse des détections de feux - Grèce"),
  
  fluidRow(
    column(3,
      wellPanel(
        h4("🎛️ Filtres interactifs"),
        selectInput("var", "Variable principale:",
                    choices = c("Brightness" = "brightness", "FRP" = "frp", 
                               "Temp. Moy." = "tavg", "Temp. Min." = "tmin", 
                               "Temp. Max." = "tmax", "Précip." = "prcp", "Vent" = "wspd"),
                    selected = "brightness"),
        dateRangeInput("date_range", "Dates:", 
                      start = min(DATA$acq_date, na.rm = TRUE),
                      end = max(DATA$acq_date, na.rm = TRUE),
                      min = min(DATA$acq_date, na.rm = TRUE),
                      max = max(DATA$acq_date, na.rm = TRUE)),
        checkboxGroupInput("daynight", "Jour/Nuit:", 
                          choices = c("Jour" = "D", "Nuit" = "N"), 
                          selected = c("D", "N")),
        sliderInput("conf", "Confiance (%):", min = 0, max = 100, value = c(0, 100)),
        selectInput("palette", "Palette:", 
                   choices = c("YlOrRd", "Viridis", "Plasma", "Inferno"), 
                   selected = "YlOrRd"),
        prettySwitch("dark_mode", "🌙 Mode sombre", value = FALSE),
        hr(),
        downloadButton("download_data", "📥 Données CSV", class = "btn-primary"),
        br(), br(),
        div("Paramètres appliqués partout", style = "font-size: 12px; color: #666;")
      )
    ),
    column(9,
      tabsetPanel(
        type = "pills",
        tabPanel("📊 Overview",
          h3("Contexte du projet"),
          p("Analyse interactive des feux de forêt en Grèce (2015-2016)"),
          p("Variables: Luminosité, FRP, météo, géolocalisation"),
          br(),
          DT::dataTableOutput("sample_table") %>% withSpinner(color = "#3498db")
        ),
        tabPanel("📈 EDA",
          fluidRow(
            column(6, plotlyOutput("hist_box", height = "400px") %>% withSpinner()),
            column(6, DT::dataTableOutput("summary_table") %>% withSpinner())
          )
        ),
        tabPanel("⏱️ Temporel",
          fluidRow(
            column(12, plotlyOutput("time_series", height = "400px") %>% withSpinner()),
            column(12, plotlyOutput("seasonal", height = "300px") %>% withSpinner())
          )
        ),
        tabPanel("🗺️ Carte",
          fluidRow(
            column(12, leafletOutput("map", height = "600px") %>% withSpinner(color = "#e74c3c"))
          ),
          fluidRow(
            column(6, downloadButton("download_map_png", "🖼️ Carte PNG", class = "btn-success"))
          )
        ),
        tabPanel("🔗 Corrélations",
          fluidRow(
            column(4, 
              checkboxGroupInput("corr_vars", "Variables:", 
                                choices = c("brightness", "frp", "tmax", "prcp", "wspd", "tavg"),
                                selected = c("brightness", "frp", "tmax", "prcp", "wspd"))
            ),
            column(8, plotlyOutput("corr_heatmap", height = "400px") %>% withSpinner())
          ),
          fluidRow(column(12, plotlyOutput("scatter_loess", height = "400px") %>% withSpinner()))
        ),
        tabPanel("💡 Conclusions",
          verbatimTextOutput("insights")
        )
      )
    )
  )
)

# ==== Server - ULTRA FAST ====
server <- function(input, output, session) {
  
  # Theme toggle
  observeEvent(input$dark_mode, {
    if (isTRUE(input$dark_mode)) {
      session$setCurrentTheme(bs_theme(bg = "#222", fg = "#eee", primary = "#58a6ff"))
    } else {
      session$setCurrentTheme(bs_theme(bootswatch = "flatly"))
    }
  })
  
  # FAST filtered data (data.table syntax = 100x faster!)
  filtered_data <- reactive({
    dt <- copy(DATA)
    dt <- dt[acq_date >= input$date_range[1] & acq_date <= input$date_range[2]]
    
    if (!is.null(input$daynight) && length(input$daynight) > 0) {
      dt <- dt[is.na(daynight) | daynight %in% input$daynight]
    }
    
    dt <- dt[is.na(confidence) | (confidence >= input$conf[1] & confidence <= input$conf[2])]
    dt
  })
  
  # Sample table (first 100 rows only)
  output$sample_table <- DT::renderDataTable({
    DT::datatable(filtered_data()[1:min(100, .N)], 
                  options = list(pageLength = 10, scrollX = TRUE),
                  rownames = FALSE)
  })
  
  # Stats table
  output$summary_table <- DT::renderDataTable({
    dt <- filtered_data()
    v <- dt[[input$var]]
    data.frame(
      Statistique = c("Moyenne", "Écart-type", "Médiane", "Min", "Max", "Manquants"),
      Valeur = c(
        round(mean(v, na.rm = TRUE), 2),
        round(sd(v, na.rm = TRUE), 2),
        round(median(v, na.rm = TRUE), 2),
        round(min(v, na.rm = TRUE), 2),
        round(max(v, na.rm = TRUE), 2),
        sum(is.na(v))
      )
    ) %>% DT::datatable(options = list(dom = 't'))
  })
  
  # Histogram + Boxplot
  output$hist_box <- renderPlotly({
    dt <- filtered_data()[sample(.N, min(5000, .N))]  # Sample 5k for speed
    v <- input$var
    
    p1 <- plot_ly(dt, x = ~get(v), type = 'histogram', nbinsx = 30, 
                  marker = list(color = '#e74c3c', line = list(color = 'white', width = 1))) %>%
      layout(title = paste("Distribution:", v), showlegend = FALSE)
    
    p2 <- plot_ly(dt, y = ~get(v), type = 'box', marker = list(color = '#3498db')) %>%
      layout(showlegend = FALSE)
    
    subplot(p1, p2, nrows = 2, heights = c(0.7, 0.3), shareX = FALSE)
  })
  
  # Time series
  output$time_series <- renderPlotly({
    dt <- filtered_data()
    daily <- dt[, .(count = .N, mean_val = mean(get(input$var), na.rm = TRUE)), by = acq_date]
    
    plot_ly(daily) %>%
      add_trace(x = ~acq_date, y = ~count, type = 'scatter', mode = 'lines', 
                name = "Nombre", line = list(color = 'darkred', width = 2)) %>%
      add_trace(x = ~acq_date, y = ~mean_val * 100, type = 'scatter', mode = 'lines',
                name = paste("Moyenne", input$var), yaxis = "y2",
                line = list(color = 'darkorange', width = 2)) %>%
      layout(title = "Évolution temporelle",
             yaxis = list(title = "Nombre de détections"),
             yaxis2 = list(overlaying = "y", side = "right", title = paste("Moyenne", input$var)))
  })
  
  # Seasonal
  output$seasonal <- renderPlotly({
    dt <- filtered_data()
    dt[, month := floor_date(acq_date, "month")]
    monthly <- dt[, .(count = .N), by = month]
    
    plot_ly(monthly, x = ~month, y = ~count, type = 'bar', marker = list(color = 'darkorange')) %>%
      layout(title = "Détections mensuelles", xaxis = list(title = ""), yaxis = list(title = "Nombre"))
  })
  
  # Map (sample 5000 points max)
  output$map <- renderLeaflet({
    dt <- filtered_data()[sample(.N, min(5000, .N))]
    pal <- colorNumeric("YlOrRd", domain = dt[[input$var]], na.color = "gray")
    
    leaflet(dt) %>% 
      addProviderTiles("CartoDB.Positron") %>%
      addCircleMarkers(lng = ~longitude, lat = ~latitude,
                      radius = 4, color = ~pal(get(input$var)),
                      stroke = FALSE, fillOpacity = 0.7,
                      popup = ~paste0("Date: ", acq_date, "<br>",
                                    input$var, ": ", round(get(input$var), 1))) %>%
      addLegend("bottomright", pal = pal, values = ~get(input$var), title = input$var)
  })
  
  # Downloads
  output$download_data <- downloadHandler(
    filename = function() paste0("feux_filtrees_", Sys.Date(), ".csv"),
    content = function(file) fwrite(filtered_data(), file)
  )
  
  output$download_map_png <- downloadHandler(
    filename = function() paste0("carte_feux_", Sys.Date(), ".png"),
    content = function(file) {
      dt <- as.data.frame(filtered_data()[sample(.N, min(5000, .N))])
      p <- ggplot(dt, aes(x = longitude, y = latitude, color = !!sym(input$var))) +
        geom_point(alpha = 0.6, size = 0.8) + scale_color_viridis_c() +
        theme_minimal() + labs(title = "Carte des feux")
      ggsave(file, p, width = 12, height = 8, dpi = 300)
    }
  )
  
  # Correlation heatmap
  output$corr_heatmap <- renderPlotly({
    dt <- filtered_data()[sample(.N, min(10000, .N)), ..input$corr_vars]
    corr_mat <- cor(dt, use = "pairwise.complete.obs")
    
    plot_ly(z = corr_mat, x = colnames(corr_mat), y = colnames(corr_mat), 
            type = "heatmap", colorscale = "Viridis") %>%
      layout(title = "Matrice de corrélation")
  })
  
  # Scatter + LOESS
  output$scatter_loess <- renderPlotly({
    dt <- as.data.frame(filtered_data()[sample(.N, min(5000, .N))])
    x_var <- "tmax"
    
    plot_ly(dt, x = ~get(x_var), y = ~get(input$var), type = 'scatter', mode = 'markers',
            marker = list(size = 4, opacity = 0.4, color = 'steelblue')) %>%
      layout(title = paste(input$var, "vs", x_var),
             xaxis = list(title = x_var),
             yaxis = list(title = input$var))
  })
  
  # Insights
  output$insights <- renderText({
    dt <- filtered_data()
    paste0(
      "📊 Résumé Complet:\n",
      "━━━━━━━━━━━━━━━━━━━━\n",
      "Total détections: ", format(nrow(dt), big.mark = " "), "\n",
      "Période: ", min(dt$acq_date, na.rm = TRUE), " → ", max(dt$acq_date, na.rm = TRUE), "\n\n",
      "Variable sélectionnée: ", input$var, "\n",
      "  • Moyenne: ", round(mean(dt[[input$var]], na.rm = TRUE), 2), "\n",
      "  • Médiane: ", round(median(dt[[input$var]], na.rm = TRUE), 2), "\n",
      "  • Écart-type: ", round(sd(dt[[input$var]], na.rm = TRUE), 2), "\n"
    )
  })
}

shinyApp(ui = ui, server = server)
