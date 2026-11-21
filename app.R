# CEC Database Explorer - Clean Version
library(shiny)
library(bslib)
library(DT)
library(duckdb)
library(readxl)
library(dplyr)

# Global database connection
db_con <- NULL

# Data loading function
load_data <- function() {
  # Files should be in same directory as app.R
  con <- dbConnect(duckdb::duckdb(), ":memory:")
  
  chemicals <- read_excel("data/CEC_Table_Chemicals.xlsx")
  info <- read_excel("data/CEC_Table_Information.xlsx")
  measurements <- read_excel("data/CEC_Table_Measurements.xlsx")
  properties <- read_excel("data/CEC_Table_Properties.xlsx")
  
  # Register tables
  duckdb_register(con, "chemicals", chemicals)
  duckdb_register(con, "info", info)
  duckdb_register(con, "measurements", measurements)
  duckdb_register(con, "properties", properties)
  
  return(con)
}

ui <- page_navbar(
  title = "CEC Database Explorer",
  theme = bs_theme(preset = "bootstrap"),
  
  nav_panel(
    "Chemical Explorer",
    card(
      card_header("Search Chemicals"),
      textInput("search", "Search", placeholder = "Name, InChIKey, or CAS..."),
      DTOutput("chemicals_table")
    )
  ),
  
  nav_panel(
    "Chemical Profile",
    # Add scrollable container
    div(
      style = "overflow-y: auto; height: calc(100vh - 100px); padding: 10px;",
      uiOutput("profile_content")
    )
  )
)

server <- function(input, output, session) {
  
  # Initialize database on startup
  isolate({ db_con <<- load_data() })
  
  selected_chemical <- reactiveVal(NULL)
  
  # Chemicals table with search
  output$chemicals_table <- renderDT({
    req(db_con)
    
    query <- "SELECT name, inchikey, cas FROM chemicals"
    
    # Add search filter if provided
    if (!is.null(input$search) && nchar(input$search) > 0) {
      search_term <- tolower(input$search)
      query <- sprintf(
        "SELECT name, inchikey, cas FROM chemicals 
         WHERE LOWER(name) LIKE '%%%s%%' 
            OR LOWER(inchikey) LIKE '%%%s%%' 
            OR LOWER(cas) LIKE '%%%s%%' 
            OR LOWER(iupac) LIKE '%%%s%%'",
        search_term, search_term, search_term, search_term
      )
    }
    
    data <- dbGetQuery(db_con, query)
    
    datatable(
      data,
      colnames = c("Name", "InChIKey", "CAS"),
      selection = 'single',
      options = list(pageLength = 25, scrollX = TRUE),
      rownames = FALSE
    )
  })
  
  # Handle row selection
  observeEvent(input$chemicals_table_rows_selected, {
    req(input$chemicals_table_rows_selected)
    
    # Get current filtered data
    query <- "SELECT name, inchikey, cas FROM chemicals"
    if (!is.null(input$search) && nchar(input$search) > 0) {
      search_term <- tolower(input$search)
      query <- sprintf(
        "SELECT name, inchikey, cas FROM chemicals 
         WHERE LOWER(name) LIKE '%%%s%%' 
            OR LOWER(inchikey) LIKE '%%%s%%' 
            OR LOWER(cas) LIKE '%%%s%%' 
            OR LOWER(iupac) LIKE '%%%s%%'",
        search_term, search_term, search_term, search_term
      )
    }
    data <- dbGetQuery(db_con, query)
    
    selected_chemical(data$inchikey[input$chemicals_table_rows_selected])
    updateNavbarPage(session, inputId = NULL, selected = "Chemical Profile")
  })
  
  # Profile content - comprehensive view
  output$profile_content <- renderUI({
    if (is.null(selected_chemical())) {
      return(card(
        card_header("No Chemical Selected"),
        p("Please select a chemical from the Explorer tab")
      ))
    }
    
    # Get chemical details
    chem_query <- sprintf("SELECT * FROM chemicals WHERE inchikey = '%s'", selected_chemical())
    chem_data <- dbGetQuery(db_con, chem_query)
    
    if (nrow(chem_data) == 0) {
      return(card(card_header("Error"), p("Chemical not found in database")))
    }
    
    # Get additional info
    info_query <- sprintf("SELECT info_type, content FROM info WHERE inchikey = '%s'", selected_chemical())
    info_data <- dbGetQuery(db_con, info_query)
    
    # Get measurements
    meas_query <- sprintf("SELECT * FROM measurements WHERE inchikey = '%s'", selected_chemical())
    meas_data <- dbGetQuery(db_con, meas_query)
    
    # Get properties
    prop_query <- sprintf("SELECT * FROM properties WHERE inchikey = '%s'", selected_chemical())
    prop_data <- dbGetQuery(db_con, prop_query)
    
    # Build UI - reordered with measurements last
    tagList(
      # Chemical Details Card
      card(
        card_header(paste("Chemical Details:", chem_data$name)),
        layout_columns(
          col_widths = c(6, 6),
          div(
            h4("Identifiers"),
            p(strong("Name: "), chem_data$name),
            p(strong("InChIKey: "), chem_data$inchikey),
            p(strong("CAS Number: "), chem_data$cas),
            if ("cid" %in% names(chem_data)) p(strong("PubChem CID: "), chem_data$cid),
            if ("dtxsid" %in% names(chem_data)) p(strong("DTXSID: "), chem_data$dtxsid)
          ),
          div(
            h4("Structure"),
            if ("formula" %in% names(chem_data)) p(strong("Molecular Formula: "), chem_data$formula),
            if ("smiles" %in% names(chem_data)) p(strong("SMILES: "), code(chem_data$smiles)),
            if ("iupac" %in% names(chem_data)) p(strong("IUPAC Name: "), chem_data$iupac)
          )
        ),
        # Category on its own line
        if ("label" %in% names(chem_data) && !is.na(chem_data$label)) {
          div(
            hr(),
            p(strong("Category: "), span(chem_data$label, class = "badge bg-primary"))
          )
        },
        # Description in full-width section at bottom
        if ("desc" %in% names(chem_data) && !is.na(chem_data$desc)) {
          div(
            hr(),
            h4("Description"),
            p(chem_data$desc, style = "text-align: justify;")
          )
        }
      ),
      
      # Additional Information Card
      if (nrow(info_data) > 0) {
        card(
          card_header(paste("Additional Information (", nrow(info_data), " records)")),
          textInput("info_search", "Search within information", placeholder = "Filter results..."),
          div(style = "min-height: 400px;", DTOutput("info_table"))
        )
      },
      
      # Properties Card (now BEFORE measurements)
      card(
        card_header(paste("Chemical Properties (", nrow(prop_data), " records)")),
        if (nrow(prop_data) > 0) {
          tagList(
            textInput("prop_search", "Search within properties", placeholder = "Filter results..."),
            div(style = "min-height: 500px;", DTOutput("properties_table"))
          )
        } else {
          p("No property data available for this chemical")
        }
      ),
      
      # Measurements Card (now LAST)
      card(
        card_header(paste("Environmental Measurements (", nrow(meas_data), " records)")),
        if (nrow(meas_data) > 0) {
          div(style = "min-height: 500px;", DTOutput("measurements_table"))
        } else {
          p("No measurement data available for this chemical")
        }
      )
    )
  })
  
  # Info table
  output$info_table <- renderDT({
    req(selected_chemical())
    info_query <- sprintf("SELECT info_type, content FROM info WHERE inchikey = '%s'", selected_chemical())
    info_data <- dbGetQuery(db_con, info_query)
    
    # Apply search filter if provided
    if (!is.null(input$info_search) && nchar(input$info_search) > 0) {
      search_term <- tolower(input$info_search)
      info_data <- info_data %>%
        filter(
          grepl(search_term, tolower(info_type), fixed = TRUE) |
            grepl(search_term, tolower(content), fixed = TRUE)
        )
    }
    
    datatable(
      info_data,
      colnames = c("Property Type", "Value"),
      options = list(
        pageLength = 15,
        scrollX = TRUE,
        scrollY = "350px",
        dom = 'rtip'
      ),
      rownames = FALSE
    )
  })
  
  # Properties table with search filtering
  output$properties_table <- renderDT({
    req(selected_chemical())
    prop_query <- sprintf("SELECT * FROM properties WHERE inchikey = '%s'", selected_chemical())
    prop_data <- dbGetQuery(db_con, prop_query)
    
    # Apply search filter if provided
    if (!is.null(input$prop_search) && nchar(input$prop_search) > 0) {
      search_term <- tolower(input$prop_search)
      prop_data <- prop_data %>%
        filter(if_any(everything(), ~ grepl(search_term, tolower(as.character(.)), fixed = TRUE)))
    }
    
    dt <- datatable(
      prop_data,
      options = list(
        pageLength = 20,
        scrollX = TRUE,
        scrollY = "450px"
      ),
      rownames = FALSE,
      filter = 'top'
    )
    
    # Add color coding if 'type' column exists
    if ("type" %in% names(prop_data)) {
      dt <- dt %>%
        formatStyle(
          'type',
          target = 'row',
          backgroundColor = styleEqual(
            c('experimental', 'predicted'),
            c('#e8f5e9', '#fff9c4')
          )
        )
    }
    
    dt
  })
  
  
  # Cleanup
  onStop(function() {
    if (!is.null(db_con)) dbDisconnect(db_con)
  })
}

shinyApp(ui, server)
