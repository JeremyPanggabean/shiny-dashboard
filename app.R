library(shiny)
library(shinydashboard)
library(DBI)
library(RPostgres)
library(DT)
library(ggplot2)
library(dplyr)
library(plotly)

# Fungsi koneksi database
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
      menuItem("Tabel", tabName = "tabel", icon = icon("table")),
      menuItem("Visualisasi Penjualan", tabName = "visual_penjualan", icon = icon("chart-line")),
      menuItem("Visualisasi Produk", tabName = "visual_produk", icon = icon("chart-bar")),
      menuItem("Visualisasi Ulasan", tabName = "visual_ulasan", icon = icon("star"))
    )
  ),
  dashboardBody(
    tabItems(
      # Tab Tabel
      tabItem(
        tabName = "tabel",
        fluidRow(
          box(width = 12, title = "Data Gabungan Penjualan", DTOutput("data_gabungan"))
        )
      ),
      
      # Tab Visualisasi Penjualan
      tabItem(
        tabName = "visual_penjualan",
        fluidRow(
          box(width = 6, title = "Total Penjualan per Kategori", plotOutput("penjualan_kategori")),
          box(width = 6, title = "Distribusi Total Harga", plotOutput("histogram_total_harga"))
        ),
        fluidRow(
          box(width = 6, title = "Penjualan per Segmentasi Pelanggan", plotOutput("penjualan_segmentasi")),
          box(width = 6, title = "Trend Penjualan Bulanan", plotOutput("trend_bulanan"))
        )
      ),
      
      # Tab Visualisasi Produk
      tabItem(
        tabName = "visual_produk",
        fluidRow(
          box(width = 6, title = "Harga Rata-rata per Kategori", plotOutput("avg_harga_kategori")),
          box(width = 6, title = "Segmentasi Produk", plotOutput("segmentasi_produk"))
        ),
        fluidRow(
          box(width = 6, title = "Top 10 Produk Termahal", plotOutput("top_produk")),
          box(width = 6, title = "Estimasi Keuntungan vs Harga", plotOutput("scatter_keuntungan"))
        )
      ),
      
      # Tab Visualisasi Ulasan
      tabItem(
        tabName = "visual_ulasan",
        fluidRow(
          box(width = 6, title = "Distribusi Bintang Ulasan", plotOutput("distribusi_bintang")),
          box(width = 6, title = "Kategori Ulasan", plotOutput("kategori_ulasan"))
        ),
        fluidRow(
          box(width = 12, title = "Rata-rata Bintang per Kategori Produk", plotOutput("bintang_per_kategori"))
        )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Data reactive
  data_gabungan <- reactive({
    con <- koneksi_db()
    on.exit(dbDisconnect(con), add = TRUE)
    dbGetQuery(con, "SELECT * FROM gabungan_3_data_produk_customers_dan_reviews")
  })
  
  # Output tabel
  output$data_gabungan <- renderDT({
    datatable(data_gabungan(), options = list(scrollX = TRUE))
  })
  
  # Visualisasi Penjualan
  output$penjualan_kategori <- renderPlot({
    data_gabungan() %>%
      group_by(kategori) %>%
      summarise(total_penjualan = sum(total_harga)) %>%
      ggplot(aes(x = reorder(kategori, -total_penjualan), y = total_penjualan, fill = kategori)) +
      geom_bar(stat = "identity") +
      theme_minimal() +
      labs(x = "Kategori", y = "Total Penjualan") +
      theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_y_continuous(labels = scales::comma)
  })
  
  output$histogram_total_harga <- renderPlot({
    ggplot(data_gabungan(), aes(x = total_harga)) +
      geom_histogram(bins = 20, fill = "#2c7fb8", color = "white") +
      theme_minimal() +
      labs(x = "Total Harga", y = "Jumlah Transaksi") +
      scale_x_continuous(labels = scales::comma)
  })
  
  output$penjualan_segmentasi <- renderPlot({
    data_gabungan() %>%
      group_by(segmentasi_pelanggan) %>%
      summarise(total_penjualan = sum(total_harga)) %>%
      ggplot(aes(x = reorder(segmentasi_pelanggan, -total_penjualan), y = total_penjualan, fill = segmentasi_pelanggan)) +
      geom_bar(stat = "identity") +
      theme_minimal() +
      labs(x = "Segmentasi Pelanggan", y = "Total Penjualan") +
      theme(legend.position = "none") +
      scale_y_continuous(labels = scales::comma)
  })
  
  output$trend_bulanan <- renderPlot({
    data_gabungan() %>%
      mutate(bulan = format(as.Date(tanggal_pesanan), "%Y-%m")) %>%
      group_by(bulan) %>%
      summarise(total_penjualan = sum(total_harga)) %>%
      ggplot(aes(x = bulan, y = total_penjualan, group = 1)) +
      geom_line(color = "#2c7fb8", size = 1) +
      geom_point(color = "#2c7fb8", size = 2) +
      theme_minimal() +
      labs(x = "Bulan", y = "Total Penjualan") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_y_continuous(labels = scales::comma)
  })
  
  # Visualisasi Produk
  output$avg_harga_kategori <- renderPlot({
    data_gabungan() %>%
      group_by(kategori) %>%
      summarise(rata_rata = mean(harga)) %>%
      ggplot(aes(x = reorder(kategori, -rata_rata), y = rata_rata, fill = kategori)) +
      geom_col() +
      theme_minimal() +
      labs(x = "Kategori", y = "Harga Rata-rata") +
      theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_y_continuous(labels = scales::comma)
  })
  
  output$segmentasi_produk <- renderPlot({
    data_gabungan() %>%
      count(segmentasi_produk) %>%
      ggplot(aes(x = reorder(segmentasi_produk, -n), y = n, fill = segmentasi_produk)) +
      geom_bar(stat = "identity") +
      theme_minimal() +
      labs(x = "Segmentasi Produk", y = "Jumlah Produk") +
      theme(legend.position = "none")
  })
  
  output$top_produk <- renderPlot({
    data_gabungan() %>%
      select(nama_produk, harga) %>%
      distinct() %>%
      arrange(desc(harga)) %>%
      head(10) %>%
      ggplot(aes(x = reorder(nama_produk, harga), y = harga, fill = harga)) +
      geom_col() +
      coord_flip() +
      theme_minimal() +
      labs(x = "Produk", y = "Harga") +
      scale_fill_gradient(low = "#add8e6", high = "#2c7fb8") +
      scale_y_continuous(labels = scales::comma)
  })
  
  output$scatter_keuntungan <- renderPlot({
    data_gabungan() %>%
      ggplot(aes(x = harga, y = estimasi_keuntungan, color = kategori)) +
      geom_point(alpha = 0.7) +
      theme_minimal() +
      labs(x = "Harga", y = "Estimasi Keuntungan", color = "Kategori") +
      scale_x_continuous(labels = scales::comma) +
      scale_y_continuous(labels = scales::comma)
  })
  
  # Visualisasi Ulasan
  output$distribusi_bintang <- renderPlot({
    data_gabungan() %>%
      count(bintang_ulasan) %>%
      ggplot(aes(x = factor(bintang_ulasan), y = n, fill = factor(bintang_ulasan))) +
      geom_bar(stat = "identity") +
      theme_minimal() +
      labs(x = "Bintang Ulasan", y = "Jumlah Ulasan") +
      theme(legend.position = "none") +
      scale_fill_brewer(palette = "YlOrRd")
  })
  
  output$kategori_ulasan <- renderPlot({
    data_gabungan() %>%
      count(kategori_ulasan) %>%
      ggplot(aes(x = reorder(kategori_ulasan, -n), y = n, fill = kategori_ulasan)) +
      geom_bar(stat = "identity") +
      theme_minimal() +
      labs(x = "Kategori Ulasan", y = "Jumlah") +
      theme(legend.position = "none") +
      scale_fill_manual(values = c("Very Poor" = "#d62728", "Poor" = "#ff7f0e", 
                                   "Good" = "#2ca02c", "Very Good" = "#1f77b4"))
  })
  
  output$bintang_per_kategori <- renderPlot({
    data_gabungan() %>%
      group_by(kategori) %>%
      summarise(rata_bintang = mean(bintang_ulasan)) %>%
      ggplot(aes(x = reorder(kategori, -rata_bintang), y = rata_bintang, fill = kategori)) +
      geom_col() +
      theme_minimal() +
      labs(x = "Kategori Produk", y = "Rata-rata Bintang") +
      theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1)) +
      ylim(0, 5)
  })
}

# Menjalankan aplikasi
shinyApp(ui = ui, server = server)
