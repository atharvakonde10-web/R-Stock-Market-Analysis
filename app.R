# ============================================================
# STOCK MARKET ANALYSIS DASHBOARD
# R + Shiny + ggplot2
#
# Features:
# 1. Upload different CSV stock datasets
# 2. Automatic column detection
# 3. Automatic date conversion
# 4. Date range filtering
# 5. Five independent visualizations
# 6. If one graph cannot be created, only that graph is skipped
# 7. Other valid graphs continue to display
# ============================================================


# ============================================================
# 1. LOAD REQUIRED LIBRARIES
# ============================================================

library(shiny)
library(ggplot2)
library(dplyr)


# ============================================================
# 2. HELPER FUNCTION - FIND COLUMN
# ============================================================

find_column <- function(column_names, patterns) {

  # ----------------------------------------------------------
  # First try exact matches
  # ----------------------------------------------------------

  lower_names <- tolower(
    trimws(column_names)
  )

  lower_patterns <- tolower(patterns)

  for (pattern in lower_patterns) {

    exact_match <- which(
      lower_names == pattern
    )

    if (length(exact_match) > 0) {
      return(column_names[exact_match[1]])
    }
  }


  # ----------------------------------------------------------
  # Then try partial matches
  # ----------------------------------------------------------

  for (pattern in lower_patterns) {

    partial_match <- which(
      grepl(
        pattern,
        lower_names,
        fixed = TRUE
      )
    )

    if (length(partial_match) > 0) {
      return(column_names[partial_match[1]])
    }
  }


  # ----------------------------------------------------------
  # Nothing found
  # ----------------------------------------------------------

  return(NULL)
}


# ============================================================
# 3. HELPER FUNCTION - DATE CONVERSION
# ============================================================

convert_to_date <- function(x) {

  # Already Date
  if (inherits(x, "Date")) {
    return(x)
  }


  # POSIXct / POSIXlt
  if (inherits(x, "POSIXct") ||
      inherits(x, "POSIXlt")) {

    return(
      as.Date(x)
    )
  }


  x <- trimws(
    as.character(x)
  )


  # ----------------------------------------------------------
  # Format 1
  # 2021-04-30
  # ----------------------------------------------------------

  result <- suppressWarnings(
    as.Date(
      x,
      format = "%Y-%m-%d"
    )
  )

  if (sum(!is.na(result)) > 0) {
    return(result)
  }


  # ----------------------------------------------------------
  # Format 2
  # 30-04-2021
  # ----------------------------------------------------------

  result <- suppressWarnings(
    as.Date(
      x,
      format = "%d-%m-%Y"
    )
  )

  if (sum(!is.na(result)) > 0) {
    return(result)
  }


  # ----------------------------------------------------------
  # Format 3
  # 30/04/2021
  # ----------------------------------------------------------

  result <- suppressWarnings(
    as.Date(
      x,
      format = "%d/%m/%Y"
    )
  )

  if (sum(!is.na(result)) > 0) {
    return(result)
  }


  # ----------------------------------------------------------
  # Format 4
  # 04/30/2021
  # ----------------------------------------------------------

  result <- suppressWarnings(
    as.Date(
      x,
      format = "%m/%d/%Y"
    )
  )

  if (sum(!is.na(result)) > 0) {
    return(result)
  }


  # ----------------------------------------------------------
  # Format 5
  # 30-Apr-2021
  # ----------------------------------------------------------

  result <- suppressWarnings(
    as.Date(
      x,
      format = "%d-%b-%Y"
    )
  )

  if (sum(!is.na(result)) > 0) {
    return(result)
  }


  # ----------------------------------------------------------
  # Format 6
  # 30 Apr 2021
  # ----------------------------------------------------------

  result <- suppressWarnings(
    as.Date(
      x,
      format = "%d %b %Y"
    )
  )

  if (sum(!is.na(result)) > 0) {
    return(result)
  }


  # Nothing worked
  return(
    as.Date(
      rep(NA, length(x))
    )
  )
}


# ============================================================
# 4. UI
# ============================================================

ui <- fluidPage(

  # ----------------------------------------------------------
  # SIMPLE STYLING
  # ----------------------------------------------------------

  tags$head(tags$style(HTML("
    body { background:#f5f6f8; }
    .app-header { background:#1f3a5f; color:white; padding:14px 22px;
                  margin:-15px -15px 20px -15px; font-size:22px; }
    .kpi { background:white; border:1px solid #d0d7de; border-radius:4px;
           padding:10px 14px; margin-bottom:15px; }
    .kpi .label { color:#57606a; font-size:13px; }
    .kpi .value { font-size:24px; font-weight:600; color:#1f3a5f; }
    .nav-tabs > li > a { color:#1f3a5f; }
    .tab-body { background:white; border:1px solid #d0d7de; border-top:0;
                padding:16px; }
    .note { color:#57606a; font-size:13px; margin-bottom:8px; }
    .well { background:white; }
  "))),


  # ----------------------------------------------------------
  # TITLE BAR
  # ----------------------------------------------------------

  div(
    class = "app-header",
    "Stock Market Analysis Dashboard"
  ),


  sidebarLayout(

    # ========================================================
    # LEFT SIDEBAR
    # ========================================================

    sidebarPanel(

      width = 3,

      fileInput(
        inputId = "file",
        label = "Upload stock CSV (default: stocks.csv)",
        accept = c(".csv")
      ),

      tags$hr(),

      uiOutput("date_selector")
    ),


    # ========================================================
    # MAIN PANEL
    # ========================================================

    mainPanel(

      width = 9,

      # ------------------------------------------------------
      # SUMMARY BOXES
      # ------------------------------------------------------

      fluidRow(

        column(3, div(class = "kpi",
          div(class = "label", "Average closing price"),
          div(class = "value", textOutput("avg_price")))),

        column(3, div(class = "kpi",
          div(class = "label", "Highest closing price"),
          div(class = "value", textOutput("max_price")))),

        column(3, div(class = "kpi",
          div(class = "label", "Lowest closing price"),
          div(class = "value", textOutput("min_price")))),

        column(3, div(class = "kpi",
          div(class = "label", "Trading volume"),
          div(class = "value", textOutput("total_volume"))))
      ),


      # ------------------------------------------------------
      # GRAPH TABS
      # ------------------------------------------------------

      tabsetPanel(

        tabPanel("Price Trend",
          div(class = "tab-body",
            div(class = "note",
              "Line chart: shows how the closing price changed over time."),
            uiOutput("line_section"))),

        tabPanel("Scatter Plot",
          div(class = "tab-body",
            div(class = "note",
              "Scatter plot: each dot is one trading day, comparing open and close."),
            uiOutput("scatter_section"))),

        tabPanel("Top Stocks",
          div(class = "tab-body",
            div(class = "note",
              "Bar chart: compares the stocks with the highest average closing price."),
            uiOutput("bar_section"))),

        tabPanel("Histogram",
          div(class = "tab-body",
            div(class = "note",
              "Histogram: shows how often each closing price range occurs."),
            uiOutput("histogram_section"))),

        tabPanel("Box Plot",
          div(class = "tab-body",
            div(class = "note",
              "Box plot: shows median, spread and outliers of closing price."),
            uiOutput("box_section"))),

        tabPanel("Data Info",
          div(class = "tab-body",
            h4("Dataset Status"),
            uiOutput("dataset_status"),
            tags$hr(),
            h4("Detected Columns"),
            verbatimTextOutput("detected_columns")))
      )
    )
  )
)


# ============================================================
# 5. SERVER
# ============================================================

server <- function(input, output, session) {


  # ==========================================================
  # LOAD CSV
  # ==========================================================

  data <- reactive({

    # --------------------------------------------------------
    # Read uploaded CSV or fallback to stocks.csv
    # --------------------------------------------------------

    df <- if (!is.null(input$file)) {
      tryCatch(
        read.csv(
          input$file$datapath,
          header = TRUE,
          stringsAsFactors = FALSE,
          check.names = FALSE
        ),
        error = function(e) {
          return(NULL)
        }
      )
    } else if (file.exists("stocks.csv")) {
      tryCatch(
        read.csv(
          "stocks.csv",
          header = TRUE,
          stringsAsFactors = FALSE,
          check.names = FALSE
        ),
        error = function(e) {
          return(NULL)
        }
      )
    } else {
      NULL
    }

    req(
      !is.null(df)
    )


    # --------------------------------------------------------
    # Clean column names
    # --------------------------------------------------------

    names(df) <- trimws(
      names(df)
    )


    # --------------------------------------------------------
    # Detect date column
    # --------------------------------------------------------

    date_col <- find_column(

      names(df),

      c(
        "date",
        "trade_date",
        "trading_date",
        "business_date",
        "datetime",
        "timestamp",
        "time"
      )
    )


    # --------------------------------------------------------
    # Convert date
    # --------------------------------------------------------

    if (!is.null(date_col)) {

      df[[date_col]] <- convert_to_date(
        df[[date_col]]
      )
    }


    # --------------------------------------------------------
    # Convert numeric-looking columns
    # --------------------------------------------------------

    for (col in names(df)) {

      # Don't touch Date
      if (
        inherits(
          df[[col]],
          "Date"
        )
      ) {

        next
      }


      if (
        is.character(
          df[[col]]
        )
      ) {

        cleaned <- gsub(
          ",",
          "",
          df[[col]]
        )

        numeric_values <- suppressWarnings(
          as.numeric(
            cleaned
          )
        )

        non_empty <- sum(
          trimws(
            df[[col]]
          ) != ""
        )

        numeric_count <- sum(
          !is.na(
            numeric_values
          )
        )


        # Convert only when mostly numeric
        if (
          non_empty > 0 &&
          numeric_count / non_empty >= 0.80
        ) {

          df[[col]] <- numeric_values
        }
      }
    }


    df
  })


  # ==========================================================
  # DETECT STOCK COLUMNS
  # ==========================================================

  detected <- reactive({

    df <- data()

    req(
      df
    )

    cols <- names(df)


    # --------------------------------------------------------
    # DATE
    # --------------------------------------------------------

    date_col <- find_column(

      cols,

      c(
        "date",
        "trade_date",
        "trading_date",
        "business_date",
        "datetime",
        "timestamp",
        "time"
      )
    )


    # --------------------------------------------------------
    # STOCK / SYMBOL
    # --------------------------------------------------------

    stock_col <- find_column(

      cols,

      c(
        "stock_symbol",
        "symbol",
        "ticker",
        "ticker_symbol",
        "stock",
        "company",
        "company_name"
      )
    )


    # --------------------------------------------------------
    # OPEN
    # --------------------------------------------------------

    open_col <- find_column(

      cols,

      c(
        "stock_price_open",
        "opening_price",
        "open_price",
        "open"
      )
    )


    # --------------------------------------------------------
    # HIGH
    # --------------------------------------------------------

    high_col <- find_column(

      cols,

      c(
        "stock_price_high",
        "highest_price",
        "high_price",
        "high"
      )
    )


    # --------------------------------------------------------
    # LOW
    # --------------------------------------------------------

    low_col <- find_column(

      cols,

      c(
        "stock_price_low",
        "lowest_price",
        "low_price",
        "low"
      )
    )


    # --------------------------------------------------------
    # CLOSE
    # --------------------------------------------------------

    close_col <- find_column(

      cols,

      c(
        "stock_price_close",
        "closing_price",
        "close_price",
        "close"
      )
    )


    # --------------------------------------------------------
    # ADJUSTED CLOSE
    # --------------------------------------------------------

    adjusted_close_col <- find_column(

      cols,

      c(
        "stock_price_adj_close",
        "adjusted_close",
        "adj_close",
        "adj.close"
      )
    )


    # --------------------------------------------------------
    # VOLUME
    # --------------------------------------------------------

    volume_col <- find_column(

      cols,

      c(
        "stock_volume",
        "trading_volume",
        "trade_volume",
        "volume",
        "vol"
      )
    )


    list(

      date = date_col,

      stock = stock_col,

      open = open_col,

      high = high_col,

      low = low_col,

      close = close_col,

      adjusted_close = adjusted_close_col,

      volume = volume_col
    )
  })


  # ==========================================================
  # DATASET STATUS
  # ==========================================================

  output$dataset_status <- renderUI({

    df <- data()

    cols <- detected()


    # --------------------------------------------------------
    # Required columns for core analysis
    # --------------------------------------------------------

    missing_core <- c()


    if (
      is.null(
        cols$date
      )
    ) {

      missing_core <- c(
        missing_core,
        "Date"
      )
    }


    if (
      is.null(
        cols$close
      )
    ) {

      missing_core <- c(
        missing_core,
        "Closing Price"
      )
    }


    # --------------------------------------------------------
    # If core fields are available
    # --------------------------------------------------------

    if (
      length(
        missing_core
      ) == 0
    ) {

      tagList(

        div(
          style = "color: green; font-weight: bold;",
          "✓ Stock dataset detected"
        ),

        br(),

        paste(
          "Rows:",
          nrow(df)
        ),

        br(),

        paste(
          "Columns:",
          ncol(df)
        )
      )

    } else {

      tagList(

        div(
          style = "color: #b00020; font-weight: bold;",
          "Dataset partially supported"
        ),

        br(),

        "Missing core field(s):",

        tags$ul(

          lapply(
            missing_core,

            function(x) {
              tags$li(x)
            }
          )
        ),

        "Graphs requiring these fields will be skipped."
      )
    }
  })


  # ==========================================================
  # DATE SELECTOR
  # ==========================================================

  output$date_selector <- renderUI({

    df <- data()

    cols <- detected()


    # --------------------------------------------------------
    # No date column
    # --------------------------------------------------------

    if (
      is.null(
        cols$date
      )
    ) {

      return(

        div(
          style = "color: #b00020;",

          "No valid date column detected. ",
          "Time-based graphs will be skipped."
        )
      )
    }


    dates <- df[[cols$date]]


    dates <- dates[
      !is.na(
        dates
      )
    ]


    if (
      length(dates) == 0
    ) {

      return(

        div(
          style = "color: #b00020;",

          "No valid dates found."
        )
      )
    }


    dateRangeInput(

      inputId = "date_range",

      label = "Select Date Range",

      start = min(
        dates
      ),

      end = max(
        dates
      ),

      min = min(
        dates
      ),

      max = max(
        dates
      ),

      format = "yyyy-mm-dd",

      separator = " to "
    )
  })


  # ==========================================================
  # DETECTED COLUMNS DISPLAY
  # ==========================================================

  output$detected_columns <- renderPrint({

    cols <- detected()


    cat(
      "Date              :",
      ifelse(
        is.null(cols$date),
        "NOT FOUND",
        cols$date
      ),
      "\n"
    )


    cat(
      "Stock / Symbol    :",
      ifelse(
        is.null(cols$stock),
        "NOT FOUND",
        cols$stock
      ),
      "\n"
    )


    cat(
      "Opening Price     :",
      ifelse(
        is.null(cols$open),
        "NOT FOUND",
        cols$open
      ),
      "\n"
    )


    cat(
      "High Price        :",
      ifelse(
        is.null(cols$high),
        "NOT FOUND",
        cols$high
      ),
      "\n"
    )


    cat(
      "Low Price         :",
      ifelse(
        is.null(cols$low),
        "NOT FOUND",
        cols$low
      ),
      "\n"
    )


    cat(
      "Closing Price     :",
      ifelse(
        is.null(cols$close),
        "NOT FOUND",
        cols$close
      ),
      "\n"
    )


    cat(
      "Adjusted Close    :",
      ifelse(
        is.null(cols$adjusted_close),
        "NOT FOUND",
        cols$adjusted_close
      ),
      "\n"
    )


    cat(
      "Volume            :",
      ifelse(
        is.null(cols$volume),
        "NOT FOUND",
        cols$volume
      ),
      "\n"
    )
  })


  # ==========================================================
  # FILTER DATA USING DATE
  # ==========================================================

  filtered_data <- reactive({

    df <- data()

    cols <- detected()


    # --------------------------------------------------------
    # If there is no date range, return all data
    # --------------------------------------------------------

    if (
      is.null(
        cols$date
      ) ||
      is.null(
        input$date_range
      )
    ) {

      return(df)
    }


    # --------------------------------------------------------
    # Filter selected date range
    # --------------------------------------------------------

    df %>%

      filter(

        !is.na(
          .data[[cols$date]]
        ),

        .data[[cols$date]] >=
          input$date_range[1],

        .data[[cols$date]] <=
          input$date_range[2]
      )
  })


  # ==========================================================
  # SUMMARY - AVERAGE
  # ==========================================================

  output$avg_price <- renderText({

    df <- filtered_data()

    cols <- detected()


    if (
      is.null(
        cols$close
      )
    ) {

      return(
        "Not available"
      )
    }


    value <- mean(

      df[[cols$close]],

      na.rm = TRUE
    )


    if (
      is.nan(value)
    ) {

      return(
        "Not available"
      )
    }


    round(
      value,
      2
    )
  })


  # ==========================================================
  # SUMMARY - MAXIMUM
  # ==========================================================

  output$max_price <- renderText({

    df <- filtered_data()

    cols <- detected()


    if (
      is.null(
        cols$close
      )
    ) {

      return(
        "Not available"
      )
    }


    values <- df[[cols$close]]

    values <- values[
      !is.na(values)
    ]


    if (
      length(values) == 0
    ) {

      return(
        "Not available"
      )
    }


    round(
      max(values),
      2
    )
  })


  # ==========================================================
  # SUMMARY - MINIMUM
  # ==========================================================

  output$min_price <- renderText({

    df <- filtered_data()

    cols <- detected()


    if (
      is.null(
        cols$close
      )
    ) {

      return(
        "Not available"
      )
    }


    values <- df[[cols$close]]

    values <- values[
      !is.na(values)
    ]


    if (
      length(values) == 0
    ) {

      return(
        "Not available"
      )
    }


    round(
      min(values),
      2
    )
  })


  # ==========================================================
  # SUMMARY - VOLUME
  # ==========================================================

  output$total_volume <- renderText({

    df <- filtered_data()

    cols <- detected()


    if (
      is.null(
        cols$volume
      )
    ) {

      return(
        "Not available"
      )
    }


    value <- sum(

      df[[cols$volume]],

      na.rm = TRUE
    )


    format(
      value,
      big.mark = ",",
      scientific = FALSE
    )
  })


  # ==========================================================
  # GRAPH 1 - LINE CHART
  #
  # Requires:
  # Date + Close
  # ==========================================================

  output$line_section <- renderUI({

    cols <- detected()


    if (
      is.null(cols$date) ||
      is.null(cols$close)
    ) {

      return(

        tagList(

          h3(
            "1. Closing Price Trend (Line Chart)"
          ),

          div(
            style = "color: #b00020; font-weight: bold; padding: 15px; border: 1px solid #ddd;",
            "Graph skipped: Date and Closing Price are required."
          )
        )
      )
    }


    tagList(

      h3(
        "1. Closing Price Trend (Line Chart)"
      ),

      plotOutput(
        "linechart",
        height = "500px"
      )
    )
  })


  output$linechart <- renderPlot({

    df <- filtered_data()

    cols <- detected()


    req(
      cols$date,
      cols$close
    )


    # --------------------------------------------------------
    # Multiple stocks
    # --------------------------------------------------------

    if (!is.null(cols$stock)) {

      stock_count <- length(
        unique(
          na.omit(
            df[[cols$stock]]
          )
        )
      )

    } else {

      stock_count <- 1
    }


    # --------------------------------------------------------
    # One stock
    # --------------------------------------------------------

    if (
      stock_count <= 1
    ) {

      ggplot(

        df,

        aes(
          x = .data[[cols$date]],
          y = .data[[cols$close]]
        )

      ) +

        geom_line(
          linewidth = 0.8,
          colour = "#1f77b4"
        ) +

        labs(

          title =
            "Closing Price Over Time",

          x =
            "Date",

          y =
            "Closing Price"

        ) +

        theme_minimal()


    } else {

      # ------------------------------------------------------
      # Multiple stocks
      # ------------------------------------------------------

      ggplot(

        df,

        aes(

          x = .data[[cols$date]],

          y = .data[[cols$close]],

          group =
            .data[[cols$stock]],

          color =
            .data[[cols$stock]]

        )

      ) +

        geom_line(
          linewidth = 0.7,
          alpha = 0.7
        ) +

        labs(

          title =
            "Closing Price Over Time",

          x =
            "Date",

          y =
            "Closing Price",

          color =
            "Stock"

        ) +

        theme_minimal()
    }
  })


  # ==========================================================
  # GRAPH 2 - SCATTER
  #
  # Requires:
  # Open + Close
  # ==========================================================

  output$scatter_section <- renderUI({

    cols <- detected()


    if (
      is.null(cols$open) ||
      is.null(cols$close)
    ) {

      return(

        tagList(

          h3(
            "2. Opening Price vs Closing Price (Scatter Plot)"
          ),

          div(
            style = "color: #b00020; font-weight: bold; padding: 15px; border: 1px solid #ddd;",
            "Graph skipped: Opening Price and Closing Price are required."
          )
        )
      )
    }


    tagList(

      h3(
        "2. Opening Price vs Closing Price (Scatter Plot)"
      ),

      plotOutput(
        "scatterplot",
        height = "500px"
      )
    )
  })


  output$scatterplot <- renderPlot({

    df <- filtered_data()

    cols <- detected()


    req(
      cols$open,
      cols$close
    )


    ggplot(

      df,

      aes(

        x =
          .data[[cols$open]],

        y =
          .data[[cols$close]]

      )

    ) +

      geom_point(
        alpha = 0.4,
        colour = "#1f77b4"
      ) +

      geom_smooth(
        method = "lm",
        se = FALSE,
        colour = "#e07b39"
      ) +

      labs(

        title =
          "Opening Price vs Closing Price",

        x =
          "Opening Price",

        y =
          "Closing Price"

      ) +

      theme_minimal()
  })


  # ==========================================================
  # GRAPH 3 - BAR CHART
  #
  # Requires:
  # Close
  #
  # Multiple stocks:
  # Average Close by Stock
  #
  # One stock:
  # Average Close by Year
  # ==========================================================

  output$bar_section <- renderUI({

    cols <- detected()


    if (
      is.null(
        cols$close
      )
    ) {

      return(

        tagList(

          h3(
            "3. Average Closing Price (Bar Chart)"
          ),

          div(
            style = "color: #b00020; font-weight: bold; padding: 15px; border: 1px solid #ddd;",

            "Graph skipped: Closing Price is required."
          )
        )
      )
    }


    tagList(

      h3(
        "3. Average Closing Price (Bar Chart)"
      ),

      plotOutput(
        "barchart",
        height = "500px"
      )
    )
  })


  output$barchart <- renderPlot({

    df <- filtered_data()

    cols <- detected()


    req(
      cols$close
    )


    # --------------------------------------------------------
    # Determine number of stocks
    # --------------------------------------------------------

    if (!is.null(cols$stock)) {

      stock_count <- length(

        unique(

          na.omit(

            df[[cols$stock]]

          )
        )
      )

    } else {

      stock_count <- 1
    }


    # ========================================================
    # MULTIPLE STOCKS
    # ========================================================

    if (
      stock_count > 1
    ) {

      top10 <- df %>%

        group_by(
          .data[[cols$stock]]
        ) %>%

        summarise(

          average_close =
            mean(
              .data[[cols$close]],
              na.rm = TRUE
            ),

          .groups = "drop"
        ) %>%

        arrange(
          desc(
            average_close
          )
        ) %>%

        slice_head(
          n = 10
        )


      ggplot(

        top10,

        aes(

          x =
            reorder(
              .data[[cols$stock]],
              average_close
            ),

          y =
            average_close

        )

      ) +

        geom_col(fill = "#1f77b4") +

        coord_flip() +

        labs(

          title =
            "Top Stocks by Average Closing Price",

          x =
            "Stock",

          y =
            "Average Closing Price"

        ) +

        theme_minimal()


    } else {

      # ======================================================
      # SINGLE STOCK
      # ======================================================

      if (
        is.null(
          cols$date
        )
      ) {

        # ----------------------------------------------------
        # No date
        # ----------------------------------------------------

        one_value <- data.frame(

          Category =
            "Stock",

          Average =
            mean(
              df[[cols$close]],
              na.rm = TRUE
            )
        )


        ggplot(

          one_value,

          aes(
            x = Category,
            y = Average
          )

        ) +

          geom_col(fill = "#1f77b4") +

          labs(

            title =
              "Average Closing Price",

            x =
              "",

            y =
              "Average Closing Price"

          ) +

          theme_minimal()


      } else {

        # ----------------------------------------------------
        # Average by year
        # ----------------------------------------------------

        yearly <- df %>%

          mutate(

            Year =
              format(
                .data[[cols$date]],
                "%Y"
              )
          ) %>%

          group_by(
            Year
          ) %>%

          summarise(

            average_close =
              mean(
                .data[[cols$close]],
                na.rm = TRUE
              ),

            .groups = "drop"
          )


        ggplot(

          yearly,

          aes(

            x =
              Year,

            y =
              average_close

          )

        ) +

          geom_col(fill = "#1f77b4") +

          labs(

            title =
              "Average Closing Price by Year",

            x =
              "Year",

            y =
              "Average Closing Price"

          ) +

          theme_minimal()
      }
    }
  })


  # ==========================================================
  # GRAPH 4 - HISTOGRAM
  #
  # Requires:
  # Close
  # ==========================================================

  output$histogram_section <- renderUI({

    cols <- detected()


    if (
      is.null(
        cols$close
      )
    ) {

      return(

        tagList(

          h3(
            "4. Closing Price Distribution (Histogram)"
          ),

          div(
            style = "color: #b00020; font-weight: bold; padding: 15px; border: 1px solid #ddd;",

            "Graph skipped: Closing Price is required."
          )
        )
      )
    }


    tagList(

      h3(
        "4. Closing Price Distribution (Histogram)"
      ),

      plotOutput(
        "histogram",
        height = "500px"
      )
    )
  })


  output$histogram <- renderPlot({

    df <- filtered_data()

    cols <- detected()


    req(
      cols$close
    )


    ggplot(

      df,

      aes(
        x =
          .data[[cols$close]]
      )

    ) +

      geom_histogram(
        bins = 30,
        fill = "#1f77b4",
        colour = "white"
      ) +

      labs(

        title =
          "Distribution of Closing Prices",

        x =
          "Closing Price",

        y =
          "Frequency"

      ) +

      theme_minimal()
  })


  # ==========================================================
  # GRAPH 5 - BOX PLOT
  #
  # Requires:
  # Close
  #
  # If Stock exists:
  # Box plot by Stock
  #
  # If no Stock:
  # Overall box plot
  # ==========================================================

  output$box_section <- renderUI({

    cols <- detected()


    if (
      is.null(
        cols$close
      )
    ) {

      return(

        tagList(

          h3(
            "5. Closing Price Box Plot (Box Plot)"
          ),

          div(
            style = "color: #b00020; font-weight: bold; padding: 15px; border: 1px solid #ddd;",

            "Graph skipped: Closing Price is required."
          )
        )
      )
    }


    tagList(

      h3(
        "5. Closing Price Box Plot (Box Plot)"
      ),

      plotOutput(
        "boxplot",
        height = "500px"
      )
    )
  })


  output$boxplot <- renderPlot({

    df <- filtered_data()

    cols <- detected()


    req(
      cols$close
    )


    # --------------------------------------------------------
    # If stock column exists
    # --------------------------------------------------------

    if (!is.null(cols$stock)) {

      stock_count <- length(

        unique(

          na.omit(

            df[[cols$stock]]

          )
        )
      )

    } else {

      stock_count <- 1
    }


    # --------------------------------------------------------
    # Multiple stocks
    # --------------------------------------------------------

    if (
      stock_count > 1
    ) {

      ggplot(

        df,

        aes(

          x =
            .data[[cols$stock]],

          y =
            .data[[cols$close]]

        )

      ) +

        geom_boxplot(fill = "#cfe0f1", colour = "#1f3a5f") +

        labs(

          title =
            "Closing Price Distribution by Stock",

          x =
            "Stock",

          y =
            "Closing Price"

        ) +

        theme_minimal() +

        theme(

          axis.text.x =
            element_text(
              angle = 90,
              hjust = 1
            )
        )


    } else {

      # ------------------------------------------------------
      # Single stock
      # ------------------------------------------------------

      ggplot(

        df,

        aes(

          x = "Stock",

          y =
            .data[[cols$close]]

        )

      ) +

        geom_boxplot(fill = "#cfe0f1", colour = "#1f3a5f") +

        labs(

          title =
            "Closing Price Distribution",

          x =
            "",

          y =
            "Closing Price"

        ) +

        theme_minimal()
    }
  })
}


# ============================================================
# 6. START APPLICATION
# ============================================================

shinyApp(
  ui = ui,
  server = server
)