library(shiny)
library(bslib)
library(DT)
library(readr)  # Assurez-vous que le package `readr` est installé
library(bsicons)
library(dplyr)
library(stringr)
library(tidyr)
 
  

# UI ----------------------------------------------------------------------

ui <- page_sidebar(
  title = "Affichage de fichiers CSV",
  sidebar = sidebar(
    fileInput("file1", "Choisir le premier fichier CSV", accept = c(".csv")),
    fileInput("file2", "Choisir le deuxième fichier CSV", accept = c(".csv")),
    actionButton("load", "Charger les fichiers")
  ),
  
  layout_columns(
    fill = FALSE,
    value_box(
      title = "Average bill length",
      value = scales::unit_format(unit = "mm")(10),
      showcase = bsicons::bs_icon("align-bottom")
    ),
    value_box(
      title = "Average bill depth",
      value = scales::unit_format(unit = "mm")(15),
      showcase = bsicons::bs_icon("align-center"),
      theme = "primary"
    ),
    value_box(
      full_screen = TRUE,
      title = "Fichier 1 (enregistrements)",
      value = textOutput('text1'),
      showcase = bsicons::bs_icon("handbag"),
      theme = "success"
    ),
    
    uiOutput('box1')
    
  ),
  
  navset_card_underline(
    nav_panel(title = "One", 
              # textOutput('text1'),
              card(
                full_screen = TRUE,
                card_header("Affichage du fichier 1"),
                DTOutput("fileTable1")
                )
        ),
    nav_panel(title = "Two", 
              textOutput('text2'),
              card(
                full_screen = TRUE,
                card_header("Affichage du fichier 2"),
                DTOutput("fileTable2")
              )),
    nav_panel(title = "Three", 
              card(
                full_screen = TRUE,
                card_header("Comparaison des Prix"),
                DTOutput("comparison_table")
                )
              ),
    nav_spacer(),
    nav_menu(
      title = "Links",
      nav_item("link_shiny"),
      nav_item("link_posit")
    )
  )
  
)

# Server ------------------------------------------------------------------

server <- function(input, output, session) {
  
  # Fonction pour lire et traiter les fichiers CSV avec parsing de date
  read_and_process_csv <- function(file_path) {
    read_csv2(file_path, col_types = cols(
      timestampe = col_datetime(format = "%Y/%m/%d %H:%M:%S")
    ), locale = locale(decimal_mark = ",", grouping_mark = "."))
  }
  
  
  file1_data <- reactiveVal(NULL)
  file2_data <- reactiveVal(NULL)
  
  observeEvent(input$file1, {
    req(input$file1)
    file1_data(read_and_process_csv(input$file1$datapath))
  }, ignoreInit = TRUE)
  
  observeEvent(input$file2, {
    req(input$file2)
    file2_data(read_and_process_csv(input$file2$datapath))
  }, ignoreInit = TRUE)
  
  # Réagir à la sélection du premier fichier
  observeEvent(file1_data(), {
    df1 <- file1_data()
    
    # Afficher le tableau dans l'onglet
    output$fileTable1 <- renderDT({
      datatable(df1 |> 
                  filter(rang == 1) |> 
                  select(-NombreDeCas, -rang, -url_image)
      )
    })
    
    # Afficher le nombre de lignes
    # output$text1 <- renderText({
    #   paste("Nombre de lignes :", nrow(df1))
    # })
    
    output$text1 <-renderText({
      # Supposons que vous avez une valeur numérique
      valeur_numerique <- nrow(df1)
      formatted_value <- scales::unit_format(unit = "g", big.mark = ",")(valeur_numerique)
      formatted_value
    })
    
    
    output$box1 <- renderUI({
      value_box(
        title = "Average Bill Length",
        value = scales::unit_format(unit = "mm")(nrow(df1)),
        # value = nrow(df1),
        showcase = bsicons::bs_icon("wechat")
      )
    })
    
  })
  
  # Réagir à la sélection du deuxième fichier
  observeEvent(file2_data(), {
    df2 <- file2_data()
    
    # Afficher le tableau dans l'onglet
    output$fileTable2 <- renderDT({
      datatable(df2 |> 
                  filter(rang == 1) |> 
                  select(-NombreDeCas, -rang, -url_image)
      )
    })
    
    # Afficher le nombre de lignes
    output$text2 <- renderText({
      paste("Nombre de lignes :", nrow(df2))
    })
  })
  
  observeEvent(input$load, {
    req(file1_data())
    req(file2_data())
    
    # Lire et combiner les données pour la comparaison
    all_data <- bind_rows(
      mutate(file1_data(), Source = "File1"),
      mutate(file2_data(), Source = "File2")
    )
    
    # Extraire les dates des noms de fichiers
    first_file_date <- str_extract(input$file1$name, "\\d{4}-\\d{2}-\\d{2}")
    
    # Traitement des données
    all_data <- all_data %>%
      filter(rang == 1) %>%
      mutate(
        period = if_else(as.Date(timestampe) == as.Date(first_file_date), "Period1", "Period2")
      )
    
    filtered_data <- all_data %>%
      select(nom_produit,period, prix)
    
    price_comparison <- filtered_data |> 
      pivot_wider(
        names_from = period,
        values_from = prix,
        values_fn = list(prix = max)
      ) |> 
      mutate(diff = Period2-Period1) |> 
      filter(diff!=0) |> 
      mutate(variation=if_else(diff>0,"AUG","RED"))
    
    output$comparison_table <- renderDT({
      datatable(price_comparison, options = list(pageLength = 5))
    })
  })
  
  
  
  # observe({
  #   if(input$go==0){
  #     return()
  #     
  #   isolate({
  #     print(input$n)
  #   })
  #   }
  # })
  
  
  
  
}

# Shiny App ---------------------------------------------------------------

shinyApp(ui, server)
