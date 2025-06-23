library(shiny)
library(shinydashboard)
library(DBI)
library(RPostgres)
library(DT)
library(ggplot2)
library(dplyr)
library(plotly)
library(lubridate)

koneksi_db <- function() {
  dbConnect(
    Postgres(),
    dbname = Sys.getenv("DB_NAME", "railway"),
    host = Sys.getenv("DB_HOST", "turntable.proxy.rlwy.net"),  
    port = as.integer(Sys.getenv("DB_PORT", 30325)),
    user = Sys.getenv("DB_USER", "postgres"),
    password = Sys.getenv("DB_PASSWORD", "zFsimXXGRVzLkcMiShuvxOSXLSULnfCG")
  )
}

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Dashboard Gabungan Data Penjualan"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("tachometer-alt")),
      menuItem("Tabel Data", tabName = "tabel", icon = icon("table")),
      menuItem("Analisis Penjualan", tabName = "penjualan", icon = icon("chart-line")),
      menuItem("Analisis Produk", tabName = "produk", icon = icon("box")),
      menuItem("Analisis Ulasan", tabName = "ulasan", icon = icon("star"))
    ),
    
    # Panel Filter
    hr(),
    h4("Filter Data", style = "color: white; margin-left: 15px;"),
    
    # Filter Tanggal
    dateRangeInput("date_range", 
                   "Rentang Tanggal:",
                   start = "2023-01-01",
                   end = "2024-12-31",
                   format = "yyyy-mm-dd"),
    
    # Filter Kategori
    selectInput("kategori_filter", 
                "Kategori Produk:",
                choices = NULL,
                multiple = TRUE,
                selected = NULL),
    
    # Filter Segmentasi Pelanggan
    selectInput("segmentasi_pelanggan_filter", 
                "Segmentasi Pelanggan:",
                choices = NULL,
                multiple = TRUE,
                selected = NULL),
    
    # Filter Segmentasi Produk
    selectInput("segmentasi_produk_filter", 
                "Segmentasi Produk:",
                choices = NULL,
                multiple = TRUE,
                selected = NULL),
    
    # Filter Range Harga
    sliderInput("harga_range", 
                "Range Harga:",
                min = 0, max = 2000, 
                value = c(0, 2000),
                step = 50),
    
    # Filter Bintang Ulasan
    checkboxGroupInput("bintang_filter", 
                       "Bintang Ulasan:",
                       choices = 1:5,
                       selected = 1:5),
    
    # Tombol Reset
    br(),
    actionButton("reset_filter", "Reset Filter", 
                 class = "btn-warning", 
                 style = "margin-left: 15px;")
  ),
  
  dashboardBody(
    tabItems(
      # Tab Dashboard Overview
      tabItem(
        tabName = "dashboard",
        fluidRow(
          valueBoxOutput("total_penjualan"),
          valueBoxOutput("total_transaksi"),
          valueBoxOutput("avg_rating")
        ),
        fluidRow(
          box(width = 6, title = "Penjualan Bulanan", status = "primary", 
              plotlyOutput("trend_penjualan")),
          box(width = 6, title = "Top 5 Kategori", status = "primary", 
              plotlyOutput("top_kategori"))
        ),
        fluidRow(
          box(width = 12, title = "Ringkasan Data Terbaru", status = "info",
              DT::dataTableOutput("summary_table"))
        )
      ),
      
      # Tab Tabel
      tabItem(
        tabName = "tabel",
        fluidRow(
          box(width = 12, title = "Data Gabungan (Filtered)", status = "primary",
              DT::dataTableOutput("data_gabungan"))
        )
      ),
      
      # Tab Analisis Penjualan
      tabItem(
        tabName = "penjualan",
        fluidRow(
          box(width = 6, title = "Penjualan per Kategori", status = "primary",
              plotlyOutput("penjualan_kategori")),
          box(width = 6, title = "Distribusi Total Harga", status = "primary",
              plotlyOutput("histogram_total_harga"))
        ),
        fluidRow(
          box(width = 6, title = "Penjualan per Segmentasi", status = "info",
              plotlyOutput("penjualan_segmentasi")),
          box(width = 6, title = "Heatmap Penjualan Bulanan", status = "info",
              plotlyOutput("heatmap_bulanan"))
        )
      ),
      
      # Tab Analisis Produk
      tabItem(
        tabName = "produk",
        fluidRow(
          box(width = 6, title = "Harga vs Keuntungan", status = "success",
              plotlyOutput("scatter_harga_keuntungan")),
          box(width = 6, title = "Distribusi Segmentasi Produk", status = "success",
              plotlyOutput("pie_segmentasi_produk"))
        ),
        fluidRow(
          box(width = 12, title = "Perbandingan Produk", status = "warning",
              plotlyOutput("comparison_produk"))
        )
      ),
      
      # Tab Analisis Ulasan
      tabItem(
        tabName = "ulasan",
        fluidRow(
          box(width = 6, title = "Distribusi Rating", status = "danger",
              plotlyOutput("distribusi_rating")),
          box(width = 6, title = "Rating per Kategori", status = "danger",
              plotlyOutput("rating_kategori"))
        ),
        fluidRow(
          box(width = 12, title = "Tren Rating Bulanan", status = "info",
              plotlyOutput("trend_rating"))
        )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Load data awal untuk populate filter choices
  initial_data <- reactive({
    con <- koneksi_db()
    on.exit(dbDisconnect(con), add = TRUE)
    dbGetQuery(con, "SELECT DISTINCT kategori, segmentasi_pelanggan, segmentasi_produk, 
                              MIN(harga) as min_harga, MAX(harga) as max_harga 
                     FROM gabungan_3_data")
  })
  
  # Update filter choices
  observe({
    data <- initial_data()
    
    updateSelectInput(session, "kategori_filter",
                      choices = sort(unique(data$kategori)),
                      selected = sort(unique(data$kategori)))
    
    updateSelectInput(session, "segmentasi_pelanggan_filter",
                      choices = sort(unique(data$segmentasi_pelanggan)),
                      selected = sort(unique(data$segmentasi_pelanggan)))
    
    updateSelectInput(session, "segmentasi_produk_filter",
                      choices = sort(unique(data$segmentasi_produk)),
                      selected = sort(unique(data$segmentasi_produk)))
    
    updateSliderInput(session, "harga_range",
                      min = floor(min(data$min_harga, na.rm = TRUE)),
                      max = ceiling(max(data$max_harga, na.rm = TRUE)),
                      value = c(floor(min(data$min_harga, na.rm = TRUE)), 
                                ceiling(max(data$max_harga, na.rm = TRUE))))
  })
  
  # Data reactive dengan filter
  filtered_data <- reactive({
    con <- koneksi_db()
    on.exit(dbDisconnect(con), add = TRUE)
    
    # Base query
    query <- "SELECT * FROM gabungan_3_data WHERE 1=1"
    
    # Add filters
    if (!is.null(input$date_range)) {
      query <- paste0(query, " AND tanggal_pesanan BETWEEN '", input$date_range[1], "' AND '", input$date_range[2], "'")
    }
    
    if (!is.null(input$kategori_filter) && length(input$kategori_filter) > 0) {
      kategori_list <- paste0("'", input$kategori_filter, "'", collapse = ",")
      query <- paste0(query, " AND kategori IN (", kategori_list, ")")
    }
    
    if (!is.null(input$segmentasi_pelanggan_filter) && length(input$segmentasi_pelanggan_filter) > 0) {
      seg_pelanggan_list <- paste0("'", input$segmentasi_pelanggan_filter, "'", collapse = ",")
      query <- paste0(query, " AND segmentasi_pelanggan IN (", seg_pelanggan_list, ")")
    }
    
    if (!is.null(input$segmentasi_produk_filter) && length(input$segmentasi_produk_filter) > 0) {
      seg_produk_list <- paste0("'", input$segmentasi_produk_filter, "'", collapse = ",")
      query <- paste0(query, " AND segmentasi_produk IN (", seg_produk_list, ")")
    }
    
    query <- paste0(query, " AND harga BETWEEN ", input$harga_range[1], " AND ", input$harga_range[2])
    
    if (!is.null(input$bintang_filter) && length(input$bintang_filter) > 0) {
      bintang_list <- paste(input$bintang_filter, collapse = ",")
      query <- paste0(query, " AND bintang_ulasan IN (", bintang_list, ")")
    }
    
    data <- dbGetQuery(con, query)
    data$tanggal_pesanan <- as.Date(data$tanggal_pesanan)
    data$tanggal_ulasan <- as.Date(data$tanggal_ulasan)
    
    return(data)
  })
  
  # Reset filter
  observeEvent(input$reset_filter, {
    data <- initial_data()
    
    updateDateRangeInput(session, "date_range", 
                         start = "2023-01-01", end = "2024-12-31")
    updateSelectInput(session, "kategori_filter", selected = sort(unique(data$kategori)))
    updateSelectInput(session, "segmentasi_pelanggan_filter", selected = sort(unique(data$segmentasi_pelanggan)))
    updateSelectInput(session, "segmentasi_produk_filter", selected = sort(unique(data$segmentasi_produk)))
    updateSliderInput(session, "harga_range", 
                      value = c(floor(min(data$min_harga, na.rm = TRUE)), 
                                ceiling(max(data$max_harga, na.rm = TRUE))))
    updateCheckboxGroupInput(session, "bintang_filter", selected = 1:5)
  })
  
  # Value Boxes
  output$total_penjualan <- renderValueBox({
    total <- sum(filtered_data()$total_harga, na.rm = TRUE)
    valueBox(
      value = paste0("Rp ", format(total, big.mark = ",")),
      subtitle = "Total Penjualan",
      icon = icon("money-bill-wave"),
      color = "green"
    )
  })
  
  output$total_transaksi <- renderValueBox({
    total <- nrow(filtered_data())
    valueBox(
      value = format(total, big.mark = ","),
      subtitle = "Total Transaksi",
      icon = icon("shopping-cart"),
      color = "blue"
    )
  })
  
  output$avg_rating <- renderValueBox({
    avg <- round(mean(filtered_data()$bintang_ulasan, na.rm = TRUE), 2)
    valueBox(
      value = paste0(avg, "/5"),
      subtitle = "Rata-rata Rating",
      icon = icon("star"),
      color = "yellow"
    )
  })
  
  # Tabel Data
  output$data_gabungan <- DT::renderDataTable({
    DT::datatable(filtered_data(), 
                  options = list(scrollX = TRUE, pageLength = 10),
                  filter = 'top')
  })
  
  output$summary_table <- DT::renderDataTable({
    filtered_data() %>%
      arrange(desc(tanggal_pesanan)) %>%
      head(20) %>%
      select(id_pesanan, tanggal_pesanan, nama_produk, kategori, total_harga, segmentasi_pelanggan, bintang_ulasan) %>%
      DT::datatable(options = list(pageLength = 10, dom = 't'), rownames = FALSE)
  })
  
  # Visualisasi Dashboard
  output$trend_penjualan <- renderPlotly({
    p <- filtered_data() %>%
      mutate(bulan = floor_date(tanggal_pesanan, "month")) %>%
      group_by(bulan) %>%
      summarise(total = sum(total_harga)) %>%
      ggplot(aes(x = bulan, y = total)) +
      geom_line(color = "#2c7fb8", size = 1) +
      geom_point(color = "#2c7fb8", size = 2) +
      theme_minimal() +
      labs(x = "Bulan", y = "Total Penjualan") +
      scale_y_continuous(labels = scales::comma)
    
    ggplotly(p)
  })
  
  output$top_kategori <- renderPlotly({
    p <- filtered_data() %>%
      group_by(kategori) %>%
      summarise(total = sum(total_harga)) %>%
      top_n(5, total) %>%
      ggplot(aes(x = reorder(kategori, total), y = total, fill = kategori)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      theme_minimal() +
      labs(x = "Kategori", y = "Total Penjualan") +
      theme(legend.position = "none") +
      scale_y_continuous(labels = scales::comma)
    
    ggplotly(p)
  })
  
  # Visualisasi Penjualan
  output$penjualan_kategori <- renderPlotly({
    p <- filtered_data() %>%
      group_by(kategori) %>%
      summarise(total = sum(total_harga)) %>%
      ggplot(aes(x = reorder(kategori, -total), y = total, fill = kategori)) +
      geom_bar(stat = "identity") +
      theme_minimal() +
      labs(x = "Kategori", y = "Total Penjualan") +
      theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_y_continuous(labels = scales::comma)
    
    ggplotly(p)
  })
  
  output$histogram_total_harga <- renderPlotly({
    p <- ggplot(filtered_data(), aes(x = total_harga)) +
      geom_histogram(bins = 20, fill = "#2c7fb8", color = "white", alpha = 0.7) +
      theme_minimal() +
      labs(x = "Total Harga", y = "Jumlah Transaksi") +
      scale_x_continuous(labels = scales::comma)
    
    ggplotly(p)
  })
  
  output$penjualan_segmentasi <- renderPlotly({
    p <- filtered_data() %>%
      group_by(segmentasi_pelanggan) %>%
      summarise(total = sum(total_harga)) %>%
      ggplot(aes(x = reorder(segmentasi_pelanggan, -total), y = total, fill = segmentasi_pelanggan)) +
      geom_bar(stat = "identity") +
      theme_minimal() +
      labs(x = "Segmentasi Pelanggan", y = "Total Penjualan") +
      theme(legend.position = "none") +
      scale_y_continuous(labels = scales::comma)
    
    ggplotly(p)
  })
  
  output$heatmap_bulanan <- renderPlotly({
    p <- filtered_data() %>%
      mutate(bulan = month(tanggal_pesanan, label = TRUE),
             tahun = year(tanggal_pesanan)) %>%
      group_by(tahun, bulan) %>%
      summarise(total = sum(total_harga)) %>%
      ggplot(aes(x = bulan, y = factor(tahun), fill = total)) +
      geom_tile() +
      scale_fill_gradient(low = "lightblue", high = "darkblue") +
      theme_minimal() +
      labs(x = "Bulan", y = "Tahun", fill = "Total Penjualan")
    
    ggplotly(p)
  })
  
  # Visualisasi Produk
  output$scatter_harga_keuntungan <- renderPlotly({
    p <- filtered_data() %>%
      ggplot(aes(x = harga, y = estimasi_keuntungan, color = kategori, 
                 text = paste("Produk:", nama_produk))) +
      geom_point(alpha = 0.7) +
      theme_minimal() +
      labs(x = "Harga", y = "Estimasi Keuntungan", color = "Kategori") +
      scale_x_continuous(labels = scales::comma) +
      scale_y_continuous(labels = scales::comma)
    
    ggplotly(p, tooltip = c("text", "x", "y", "colour"))
  })
  
  output$pie_segmentasi_produk <- renderPlotly({
    data_pie <- filtered_data() %>%
      count(segmentasi_produk) %>%
      mutate(persen = round(n/sum(n)*100, 1))
    
    plot_ly(data_pie, labels = ~segmentasi_produk, values = ~n, type = 'pie',
            textinfo = 'label+percent', textposition = 'inside') %>%
      layout(title = "Distribusi Segmentasi Produk")
  })
  
  output$comparison_produk <- renderPlotly({
    p <- filtered_data() %>%
      group_by(nama_produk, kategori) %>%
      summarise(
        rata_harga = mean(harga),
        rata_keuntungan = mean(estimasi_keuntungan),
        total_terjual = n()
      ) %>%
      filter(total_terjual >= 5) %>%
      ggplot(aes(x = rata_harga, y = rata_keuntungan, size = total_terjual, color = kategori,
                 text = paste("Produk:", nama_produk, "<br>Total Terjual:", total_terjual))) +
      geom_point(alpha = 0.7) +
      theme_minimal() +
      labs(x = "Rata-rata Harga", y = "Rata-rata Keuntungan", 
           size = "Total Terjual", color = "Kategori") +
      scale_x_continuous(labels = scales::comma) +
      scale_y_continuous(labels = scales::comma)
    
    ggplotly(p, tooltip = c("text", "x", "y"))
  })
  
  # Visualisasi Ulasan
  output$distribusi_rating <- renderPlotly({
    p <- filtered_data() %>%
      count(bintang_ulasan) %>%
      ggplot(aes(x = factor(bintang_ulasan), y = n, fill = factor(bintang_ulasan))) +
      geom_bar(stat = "identity") +
      theme_minimal() +
      labs(x = "Bintang Ulasan", y = "Jumlah Ulasan") +
      theme(legend.position = "none") +
      scale_fill_brewer(palette = "YlOrRd")
    
    ggplotly(p)
  })
  
  output$rating_kategori <- renderPlotly({
    p <- filtered_data() %>%
      group_by(kategori) %>%
      summarise(rata_rating = mean(bintang_ulasan)) %>%
      ggplot(aes(x = reorder(kategori, -rata_rating), y = rata_rating, fill = kategori)) +
      geom_col() +
      theme_minimal() +
      labs(x = "Kategori", y = "Rata-rata Rating") +
      theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1)) +
      ylim(0, 5)
    
    ggplotly(p)
  })
  
  output$trend_rating <- renderPlotly({
    p <- filtered_data() %>%
      mutate(bulan = floor_date(tanggal_ulasan, "month")) %>%
      group_by(bulan) %>%
      summarise(rata_rating = mean(bintang_ulasan)) %>%
      ggplot(aes(x = bulan, y = rata_rating)) +
      geom_line(color = "#e74c3c", size = 1) +
      geom_point(color = "#e74c3c", size = 2) +
      theme_minimal() +
      labs(x = "Bulan", y = "Rata-rata Rating") +
      ylim(1, 5)
    
    ggplotly(p)
  })
}

# Menjalankan aplikasi
shinyApp(ui = ui, server = server)
