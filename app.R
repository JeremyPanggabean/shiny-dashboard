library(shiny)
library(shinydashboard)
library(DBI)
library(RPostgres)
library(DT)
library(ggplot2)
library(dplyr)
library(plotly)

# Database connection function
koneksi_db <- function() {
  tryCatch({
    dbConnect(
      Postgres(),
      dbname = Sys.getenv("DB_NAME", "railway"),
      host = Sys.getenv("DB_HOST", "turntable.proxy.rlwy.net"),  
      port = as.integer(Sys.getenv("DB_PORT", 30325)),
      user = Sys.getenv("DB_USER", "postgres"),
      password = Sys.getenv("DB_PASSWORD", "zFsimXXGRVzLkcMiShuvxOSXLSULnfCG")
    )
  }, error = function(e) {
    message("Database connection failed: ", e$message)
    return(NULL)
  })
}

# Simplified UI
ui <- dashboardPage(
  dashboardHeader(title = "Dashboard Penjualan Online"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("tachometer-alt")),
      menuItem("Data Table", tabName = "tabel", icon = icon("table")),
      menuItem("Analytics", tabName = "analytics", icon = icon("chart-line"))
    ),
    
    # Simple filters
    hr(),
    h4("Filters", style = "color: white; margin-left: 15px;"),
    
    selectInput("kategori_filter", 
                "Product Category:",
                choices = NULL,
                multiple = TRUE),
    
    sliderInput("limit_data", 
                "Data Limit:",
                min = 100, max = 10000, 
                value = 1000,
                step = 100),
    
    actionButton("refresh_data", "Refresh Data", 
                 class = "btn-primary", 
                 style = "margin-left: 15px; margin-top: 10px;")
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side {
          background-color: #f4f4f4;
        }
      "))
    ),
    
    tabItems(
      # Dashboard tab
      tabItem(
        tabName = "dashboard",
        fluidRow(
          valueBoxOutput("total_sales", width = 4),
          valueBoxOutput("total_orders", width = 4),
          valueBoxOutput("avg_rating", width = 4)
        ),
        fluidRow(
          box(width = 6, title = "Sales by Category", status = "primary", 
              plotlyOutput("sales_by_category", height = "300px")),
          box(width = 6, title = "Rating Distribution", status = "info", 
              plotlyOutput("rating_dist", height = "300px"))
        ),
        fluidRow(
          box(width = 12, title = "Recent Orders", status = "success",
              DT::dataTableOutput("recent_orders"))
        )
      ),
      
      # Data table tab
      tabItem(
        tabName = "tabel",
        fluidRow(
          box(width = 12, title = "Sales Data", status = "primary",
              DT::dataTableOutput("main_table"))
        )
      ),
      
      # Analytics tab
      tabItem(
        tabName = "analytics",
        fluidRow(
          box(width = 6, title = "Price vs Profit", status = "warning",
              plotlyOutput("price_profit_scatter", height = "350px")),
          box(width = 6, title = "Customer Segmentation", status = "danger",
              plotlyOutput("customer_segment", height = "350px"))
        ),
        fluidRow(
          box(width = 12, title = "Product Performance", status = "info",
              plotlyOutput("product_performance", height = "400px"))
        )
      )
    )
  )
)

# Simplified Server
server <- function(input, output, session) {
  
  # Reactive data loading with error handling
  base_data <- reactive({
    con <- koneksi_db()
    if (is.null(con)) {
      # Return sample data if DB connection fails
      return(data.frame(
        kategori = c("Electronics", "Clothing", "Books"),
        total_harga = c(1000, 500, 200),
        bintang_ulasan = c(4, 5, 3),
        nama_produk = c("Sample Product 1", "Sample Product 2", "Sample Product 3"),
        harga = c(100, 50, 20),
        estimasi_keuntungan = c(20, 10, 5),
        segmentasi_pelanggan = c("Premium", "Regular", "Basic"),
        id_pesanan = c("001", "002", "003"),
        tanggal_pesanan = as.Date(c("2024-01-01", "2024-01-02", "2024-01-03"))
      ))
    }
    
    on.exit(dbDisconnect(con), add = TRUE)
    
    query <- paste0("SELECT * FROM gabungan_3_data_produk_customers_dan_reviews LIMIT ", input$limit_data)
    
    tryCatch({
      data <- dbGetQuery(con, query)
      if (nrow(data) > 0 && "tanggal_pesanan" %in% names(data)) {
        data$tanggal_pesanan <- as.Date(data$tanggal_pesanan)
      }
      return(data)
    }, error = function(e) {
      message("Query failed: ", e$message)
      return(data.frame())
    })
  })
  
  # Update category filter choices
  observe({
    data <- base_data()
    if (nrow(data) > 0 && "kategori" %in% names(data)) {
      choices <- sort(unique(data$kategori))
      updateSelectInput(session, "kategori_filter",
                        choices = choices,
                        selected = choices)
    }
  })
  
  # Filtered data
  filtered_data <- reactive({
    data <- base_data()
    if (nrow(data) == 0) return(data)
    
    if (!is.null(input$kategori_filter) && length(input$kategori_filter) > 0) {
      data <- data[data$kategori %in% input$kategori_filter, ]
    }
    
    return(data)
  })
  
  # Refresh data when button clicked
  observeEvent(input$refresh_data, {
    base_data()
  })
  
  # Value boxes
  output$total_sales <- renderValueBox({
    data <- filtered_data()
    total <- if (nrow(data) > 0 && "total_harga" %in% names(data)) {
      sum(data$total_harga, na.rm = TRUE)
    } else {
      0
    }
    
    valueBox(
      value = paste0("$", format(total, big.mark = ",")),
      subtitle = "Total Sales",
      icon = icon("dollar-sign"),
      color = "green"
    )
  })
  
  output$total_orders <- renderValueBox({
    total <- nrow(filtered_data())
    valueBox(
      value = format(total, big.mark = ","),
      subtitle = "Total Orders",
      icon = icon("shopping-cart"),
      color = "blue"
    )
  })
  
  output$avg_rating <- renderValueBox({
    data <- filtered_data()
    avg <- if (nrow(data) > 0 && "bintang_ulasan" %in% names(data)) {
      round(mean(data$bintang_ulasan, na.rm = TRUE), 2)
    } else {
      0
    }
    
    valueBox(
      value = paste0(avg, "/5"),
      subtitle = "Average Rating",
      icon = icon("star"),
      color = "yellow"
    )
  })
  
  # Charts
  output$sales_by_category <- renderPlotly({
    data <- filtered_data()
    
    if (nrow(data) == 0 || !"kategori" %in% names(data)) {
      p <- ggplot() + 
        geom_text(aes(x = 0, y = 0, label = "No data available")) +
        theme_void()
      return(ggplotly(p))
    }
    
    chart_data <- data %>%
      group_by(kategori) %>%
      summarise(total = sum(total_harga, na.rm = TRUE), .groups = 'drop') %>%
      top_n(10, total)
    
    p <- ggplot(chart_data, aes(x = reorder(kategori, total), y = total, fill = kategori)) +
      geom_col() +
      coord_flip() +
      theme_minimal() +
      theme(legend.position = "none") +
      labs(x = "Category", y = "Total Sales")
    
    ggplotly(p)
  })
  
  output$rating_dist <- renderPlotly({
    data <- filtered_data()
    
    if (nrow(data) == 0 || !"bintang_ulasan" %in% names(data)) {
      p <- ggplot() + 
        geom_text(aes(x = 0, y = 0, label = "No data available")) +
        theme_void()
      return(ggplotly(p))
    }
    
    chart_data <- data %>%
      count(bintang_ulasan) %>%
      filter(!is.na(bintang_ulasan))
    
    p <- ggplot(chart_data, aes(x = factor(bintang_ulasan), y = n, fill = factor(bintang_ulasan))) +
      geom_col() +
      theme_minimal() +
      theme(legend.position = "none") +
      labs(x = "Rating", y = "Count")
    
    ggplotly(p)
  })
  
  output$price_profit_scatter <- renderPlotly({
    data <- filtered_data()
    
    if (nrow(data) == 0 || !all(c("harga", "estimasi_keuntungan") %in% names(data))) {
      p <- ggplot() + 
        geom_text(aes(x = 0, y = 0, label = "No data available")) +
        theme_void()
      return(ggplotly(p))
    }
    
    p <- ggplot(data, aes(x = harga, y = estimasi_keuntungan)) +
      geom_point(alpha = 0.6, color = "#2c7fb8") +
      theme_minimal() +
      labs(x = "Price", y = "Estimated Profit")
    
    ggplotly(p)
  })
  
  output$customer_segment <- renderPlotly({
    data <- filtered_data()
    
    if (nrow(data) == 0 || !"segmentasi_pelanggan" %in% names(data)) {
      p <- ggplot() + 
        geom_text(aes(x = 0, y = 0, label = "No data available")) +
        theme_void()
      return(ggplotly(p))
    }
    
    chart_data <- data %>%
      count(segmentasi_pelanggan) %>%
      filter(!is.na(segmentasi_pelanggan))
    
    plot_ly(chart_data, labels = ~segmentasi_pelanggan, values = ~n, type = 'pie') %>%
      layout(showlegend = TRUE)
  })
  
  output$product_performance <- renderPlotly({
    data <- filtered_data()
    
    if (nrow(data) == 0 || !all(c("nama_produk", "total_harga") %in% names(data))) {
      p <- ggplot() + 
        geom_text(aes(x = 0, y = 0, label = "No data available")) +
        theme_void()
      return(ggplotly(p))
    }
    
    chart_data <- data %>%
      group_by(nama_produk) %>%
      summarise(total_sales = sum(total_harga, na.rm = TRUE), .groups = 'drop') %>%
      top_n(15, total_sales)
    
    p <- ggplot(chart_data, aes(x = reorder(nama_produk, total_sales), y = total_sales)) +
      geom_col(fill = "#2c7fb8") +
      coord_flip() +
      theme_minimal() +
      labs(x = "Product", y = "Total Sales")
    
    ggplotly(p)
  })
  
  # Data tables
  output$main_table <- DT::renderDataTable({
    data <- filtered_data()
    if (nrow(data) == 0) {
      return(DT::datatable(data.frame(Message = "No data available")))
    }
    
    DT::datatable(data, 
                  options = list(
                    scrollX = TRUE, 
                    pageLength = 25,
                    responsive = TRUE
                  ),
                  class = 'cell-border stripe')
  })
  
  output$recent_orders <- DT::renderDataTable({
    data <- filtered_data()
    if (nrow(data) == 0) {
      return(DT::datatable(data.frame(Message = "No data available")))
    }
    
    # Select relevant columns and limit rows
    display_cols <- intersect(c("id_pesanan", "tanggal_pesanan", "nama_produk", 
                               "kategori", "total_harga", "bintang_ulasan"), names(data))
    
    recent_data <- data[display_cols] %>%
      head(50)
    
    DT::datatable(recent_data, 
                  options = list(
                    pageLength = 10, 
                    dom = 't',
                    responsive = TRUE
                  ), 
                  rownames = FALSE,
                  class = 'cell-border stripe')
  })
}

# Run the application
shinyApp(ui = ui, server = server)
