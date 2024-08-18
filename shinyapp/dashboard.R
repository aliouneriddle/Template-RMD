library(shiny)
library(bslib)
library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(ggplot2)

# Fonction pour lire et traiter les fichiers CSV avec parsing de date
read_and_process_csv <- function(file_path) {
  read_csv2(file_path, col_types = cols(
    timestampe = col_datetime(format = "%Y/%m/%d %H:%M:%S")
  ), locale = locale(decimal_mark = ",", grouping_mark = "."))
}

# Setup -------------------------------------------------------------------

# UI ----------------------------------------------------------------------

ui <- page_sidebar(
  title = "Comparaison des Prix",
  sidebar = sidebar(
    fileInput("file1", "Choisir le premier fichier CSV",
              accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
    fileInput("file2", "Choisir le deuxième fichier CSV",
              accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
    actionButton("load", "Charger les fichiers"),
    actionButton("show_summary", "Afficher le résumé")
  ),
  layout_columns(
    fill = FALSE,
    value_box(
      title = "Nombre de produits",
      value = "N/A",  # Placeholder for actual value
      showcase = icon("box"),
      theme_color = "primary"
    ),
    value_box(
      title = "Augmentations de prix",
      value = "N/A",  # Placeholder for actual value
      showcase = icon("arrow-up"),
      theme_color = "success"
    ),
    value_box(
      title = "Réductions de prix",
      value = "N/A",  # Placeholder for actual value
      showcase = icon("arrow-down"),
      theme_color = "danger"
    ),
    value_box(
      title = "Max Augmentation",
      value = "N/A",  # Placeholder for actual value
      showcase = icon("arrow-up"),
      theme_color = "warning"
    ),
    value_box(
      title = "Min Augmentation",
      value = "N/A",  # Placeholder for actual value
      showcase = icon("arrow-up"),
      theme_color = "warning"
    ),
    value_box(
      title = "Max Réduction",
      value = "N/A",  # Placeholder for actual value
      showcase = icon("arrow-down"),
      theme_color = "info"
    ),
    value_box(
      title = "Min Réduction",
      value = "N/A",  # Placeholder for actual value
      showcase = icon("arrow-down"),
      theme_color = "info"
    )
  ),
  layout_columns(
    card(
      full_screen = TRUE,
      card_header("Comparaison des Prix"),
      tableOutput("comparison_table")
    ),
    card(
      full_screen = TRUE,
      card_header("Résumé par Date"),
      tableOutput("summary_table")
    )
  )
)

# Server ------------------------------------------------------------------

server <- function(input, output, session) {
  all_data <- reactiveVal(NULL)
  
  observeEvent(input$load, {
    req(input$file1)
    req(input$file2)
    
    file_paths <- c(input$file1$datapath, input$file2$datapath)
    
    # Extraire la date du premier fichier dans file_paths
    first_file_date <- str_extract(file_paths[1], "\\d{4}-\\d{2}-\\d{2}")
    
    data <- bind_rows(lapply(file_paths, read_and_process_csv))
    
    # Ajouter une colonne pour la période basée sur les dates
    data <- data %>%
      filter(rang == 1) %>%
      mutate(
        period = if_else(as.Date(timestampe) == as.Date(first_file_date), "Period1", "Period2")
      )
    
    all_data(data)
    
    # Effectuer la comparaison des prix
    price_comparison <- data %>%
      select(nom_produit, period, prix) %>%
      pivot_wider(
        names_from = period,
        values_from = prix,
        values_fn = list(prix = max)
      ) %>%
      mutate(diff = Period2 - Period1) %>%
      filter(diff != 0) %>%
      mutate(variation = if_else(diff > 0, "AUG", "RED"))
    
    output$comparison_table <- renderTable({
      price_comparison %>%
        filter(variation == "AUG")
    })
    
    # Calcul des KPIs
    total_products <- nrow(price_comparison)
    total_increases <- nrow(filter(price_comparison, variation == "AUG"))
    total_decreases <- nrow(filter(price_comparison, variation == "RED"))
    max_increase <- max(filter(price_comparison, variation == "AUG")$diff, na.rm = TRUE)
    min_increase <- min(filter(price_comparison, variation == "AUG")$diff, na.rm = TRUE)
    max_decrease <- max(filter(price_comparison, variation == "RED")$diff, na.rm = TRUE)
    min_decrease <- min(filter(price_comparison, variation == "RED")$diff, na.rm = TRUE)
    
    updateValueBox(session, "Nombre de produits", total_products)
    updateValueBox(session, "Augmentations de prix", total_increases)
    updateValueBox(session, "Réductions de prix", total_decreases)
    updateValueBox(session, "Max Augmentation", max_increase)
    updateValueBox(session, "Min Augmentation", min_increase)
    updateValueBox(session, "Max Réduction", max_decrease)
    updateValueBox(session, "Min Réduction", min_decrease)
  })
  
  observeEvent(input$show_summary, {
    req(all_data())
    
    summary_data <- all_data() %>%
      group_by(date = as.Date(timestampe)) %>%
      summarise(n = n())
    
    output$summary_table <- renderTable({
      summary_data
    })
  })
}

# Shiny App ---------------------------------------------------------------

shinyApp(ui = ui, server = server)
