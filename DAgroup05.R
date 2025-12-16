
#install.packages(c("shiny", "readxl", "forecast", "ggplot2", "dplyr", "tidyr", "plotly", "dbscan", "DT"))

library(shiny)
library(readxl)
library(forecast)
library(ggplot2)
library(dplyr)
library(tidyr)
library(plotly)
library(dbscan)
library(DT)

# File paths - UPDATED to use absolute path or relative if in same folder
# Assuming this file will be moved to the Group Project folder
input_file <- "OriginalFile.xlsx"

# Load and process the data
# We wrap this in a tryCatch in case the file isn't found immediately
if (!file.exists(input_file)) {
    # Try absolute path fallback if running from scratch dir
    fallback <- "/Users/tarunkumar/Group Project/OriginalFile.xlsx"
    if (file.exists(fallback)) {
        input_file <- fallback
    }
}

original <- read_excel(input_file, col_names = FALSE)

# Define the range of years
years <- 2015:2024

# Generate column names dynamically for all years
quarters <- c("Q1", "Q2", "Q3", "Q4")
months <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

# Create column names for each year
column_names <- c("Country of Citizenship")
for (year in years) {
  column_names <- c(
    column_names,
    paste0(months[1:3], " ", quarters[1], " ", year), 
    paste0("Q1 Total ", year),
    paste0(months[4:6], " ", quarters[2], " ", year),
    paste0("Q2 Total ", year),
    paste0(months[7:9], " ", quarters[3], " ", year),
    paste0("Q3 Total ", year),
    paste0(months[10:12], " ", quarters[4], " ", year),
    paste0("Q4 Total ", year),
    paste0(year, " Total")
  )
}

# Truncate column names to match dataset column count
column_names <- column_names[1:ncol(original)]

# Apply column names to the dataset
colnames(original) <- column_names

# Clean and transform the data
transformed <- original %>%
  slice(-1:-2) %>%
  mutate(across(-`Country of Citizenship`, ~ as.numeric(gsub(",", "", ifelse(. == "--", "0", .))))) %>%
  filter(!is.na(`Country of Citizenship`) & `Country of Citizenship` != "")

# Shiny App
ui <- fluidPage(
  titlePanel("Dashboard"),
  fluidRow(
    column(4,
           h3("STATISTICS"),
           selectInput("stats_country", "Select a Country", unique(transformed$`Country of Citizenship`), selected = "India"),
           selectInput("stats_year", "Select a Year", years, selected = 2023),
           tableOutput("statistics_table")
    ),
    column(4,
           h3("VISUALIZATION"),
           selectInput("chart_type", "Select Chart Type", c("Line Chart", "Bar Chart", "Pie Chart", "Scatter Plot"), selected = "Line Chart"),
           
           # Inputs for Country (Needed for Bar, Pie, Scatter)
           conditionalPanel(condition = "input.chart_type == 'Bar Chart' || input.chart_type == 'Pie Chart' || input.chart_type == 'Scatter Plot'",
                            selectInput("vis_country", "Select a Country", unique(transformed$`Country of Citizenship`), selected = "India")
           ),
           
           # Inputs for Year (Needed for Bar, Pie)
           conditionalPanel(condition = "input.chart_type == 'Bar Chart' || input.chart_type == 'Pie Chart'",
                            selectInput("vis_year", "Select a Year", years, selected = 2023)
           ),
           
           plotOutput("dynamic_chart")
    ),
    column(4,
           h3("MODELING"),
           radioButtons("modeling_method", "Select Modeling Method:",
                        choices = c("DBSCAN", "SARIMA"),
                        selected = "DBSCAN"),
           conditionalPanel(
             condition = "input.modeling_method == 'DBSCAN'",
             selectInput("dbscan_year", "Select Year for Clustering:", 
                         choices = paste0(2015:2023, " Total"), 
                         selected = "2022 Total"),
             actionButton("run_dbscan", "Run Clustering"),
             tabsetPanel(
               tabPanel("Clustered Data", DTOutput("dbscan_table")),
               tabPanel("Cluster Plot", plotlyOutput("dbscan_plot"))
             )
           ),
           conditionalPanel(
             condition = "input.modeling_method == 'SARIMA'",
             selectInput("sarima_country", "Select Country:", choices = unique(transformed$`Country of Citizenship`)),
             plotOutput("forecastPlot"),
             tableOutput("forecastTable")
           )
    )
  )
)

server <- function(input, output, session) {
  # Reactive transformed data
  data <- reactive({ transformed })
  
  # Default Statistics Table
  output$statistics_table <- renderTable({
    req(data())
    df <- data()
    stats_country <- input$stats_country  # Selected country
    stats_year <- input$stats_year        # Selected year
    
    # Filter data for the selected country and year
    stats_data <- df %>%
      filter(`Country of Citizenship` == stats_country) %>%
      select(matches(paste0("^Q\\d Total ", stats_year, "$"))) %>%
      pivot_longer(cols = everything(), values_to = "Value") %>%
      mutate(Value = as.numeric(Value)) %>%
      summarise(
        `Country of Citizenship` = stats_country,
        Year = stats_year,
        Mean = mean(Value, na.rm = TRUE),
        Median = median(Value, na.rm = TRUE),
        Max = max(Value, na.rm = TRUE)
      )
    
    stats_data
  })
  
  # Dynamic Visualization - FIXED LOGIC
  output$dynamic_chart <- renderPlot({
    req(data())
    df <- data()
    chart_type <- input$chart_type
    
    if (chart_type == "Line Chart") {
      vis_countries <- c("India", "China, People's Republic of")  # Keep default comparison for Line Chart
      yearly_totals <- df %>%
        select(`Country of Citizenship`, matches("^\\d{4} Total$")) %>%
        pivot_longer(
          cols = matches("^\\d{4} Total$"),
          names_to = "Year",
          names_pattern = "(\\d{4}) Total",
          values_to = "Total"
        ) %>%
        mutate(Year = as.numeric(Year)) %>%
        filter(`Country of Citizenship` %in% vis_countries)
      
      ggplot(yearly_totals, aes(x = Year, y = Total, color = `Country of Citizenship`, group = `Country of Citizenship`)) +
        geom_line(linewidth = 1) +
        geom_point(size = 2) +
        theme_minimal() +
        labs(title = "Yearly Totals for Selected Countries", x = "Year", y = "Total", color = "Country")
        
    } else if (chart_type == "Bar Chart") {
        req(input$vis_country, input$vis_year)
        
        # Extract quarterly totals for the selected year and country
        year_regex <- paste0("Q[1-4] Total ", input$vis_year)
        
        bar_data <- df %>%
            filter(`Country of Citizenship` == input$vis_country) %>%
            select(`Country of Citizenship`, matches(year_regex)) %>%
            pivot_longer(cols = -`Country of Citizenship`, names_to = "Period", values_to = "Value")
            
        ggplot(bar_data, aes(x = Period, y = Value, fill = Period)) +
            geom_bar(stat = "identity") +
            theme_minimal() +
            labs(title = paste("Quarterly Breakdown for", input$vis_country, "in", input$vis_year), x = "Quarter", y = "Total")
            
    } else if (chart_type == "Pie Chart") {
        req(input$vis_country, input$vis_year)
        
        # Extract quarterly totals for the selected year and country
        year_regex <- paste0("Q[1-4] Total ", input$vis_year)
        
        pie_data <- df %>%
            filter(`Country of Citizenship` == input$vis_country) %>%
            select(`Country of Citizenship`, matches(year_regex)) %>%
            pivot_longer(cols = -`Country of Citizenship`, names_to = "Period", values_to = "Value") %>%
            mutate(Period = gsub(paste0(" Total ", input$vis_year), "", Period)) # Clean label
            
        ggplot(pie_data, aes(x = "", y = Value, fill = Period)) +
            geom_bar(stat = "identity", width = 1) +
            coord_polar("y", start = 0) +
            theme_minimal() +
            theme(axis.text.x = element_blank()) +
            labs(title = paste("Quarterly Distribution for", input$vis_country, "in", input$vis_year), fill = "Quarter", x = "", y = "")
            
    } else if (chart_type == "Scatter Plot") {
        req(input$vis_country)
        
        # Yearly totals for the selected country
        yearly_totals <- df %>%
            filter(`Country of Citizenship` == input$vis_country) %>%
            select(`Country of Citizenship`, matches("^\\d{4} Total$")) %>%
            pivot_longer(
              cols = matches("^\\d{4} Total$"),
              names_to = "Year",
              names_pattern = "(\\d{4}) Total",
              values_to = "Total"
            ) %>%
            mutate(Year = as.numeric(Year))
            
        ggplot(yearly_totals, aes(x = Year, y = Total)) +
            geom_point(size = 4, color = "blue", alpha = 0.7) +
            geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "dashed") + # Add trend line
            theme_minimal() +
            labs(title = paste("Yearly Trend (Scatter) for", input$vis_country), x = "Year", y = "Total")
    }
  })
  
  # DBSCAN Clustering Logic
  dbscan_result <- reactiveVal(NULL)
  
  # Run clustering for the default year (2022) when the app starts
  observe({
    req(data())
    df <- data()
    selected_column <- "2022 Total" # Default init
    if ("2022 Total" %in% colnames(df)) {
        clustering_input <- as.numeric(df[[selected_column]])
        clustering_input[is.na(clustering_input)] <- 0
        
        # Apply DBSCAN clustering
        eps <- 1000  # Increased eps for this dataset scale
        minPts <- 3  
        clustering <- dbscan::dbscan(as.matrix(clustering_input), eps = eps, minPts = minPts)
        
        df$Cluster <- as.factor(clustering$cluster)
        result_df <- df[, c("Country of Citizenship", selected_column, "Cluster")]
        dbscan_result(result_df)
    }
  })
  
  observeEvent(input$run_dbscan, {
    df <- data()
    selected_column <- input$dbscan_year
    req(selected_column %in% colnames(df))
    
    clustering_input <- as.numeric(df[[selected_column]])
    clustering_input[is.na(clustering_input)] <- 0
    
    # Apply DBSCAN clustering
    eps <- 1000  # Adjusted parameter
    minPts <- 3  
    clustering <- dbscan::dbscan(as.matrix(clustering_input), eps = eps, minPts = minPts)
    
    df$Cluster <- as.factor(clustering$cluster)
    result_df <- df[, c("Country of Citizenship", selected_column, "Cluster")]
    dbscan_result(result_df)
  })
  
  output$dbscan_table <- renderDT({
    req(dbscan_result())
    datatable(dbscan_result(), options = list(pageLength = 10), rownames = FALSE)
  })
  
  output$dbscan_plot <- renderPlotly({
    req(dbscan_result())
    result_df <- dbscan_result()
    plot <- ggplot(result_df, aes(
      x = seq_along(result_df[[2]]), 
      y = result_df[[2]], 
      color = Cluster, 
      text = paste("Country:", `Country of Citizenship`, "<br>Value:", result_df[[2]])
    )) +
      geom_point(size = 3) +
      labs(title = paste("DBSCAN Clustering for", input$dbscan_year),
           x = "Index", y = input$dbscan_year) +
      theme_minimal()
    ggplotly(plot, tooltip = "text")
  })
  
  # SARIMA Forecasting Logic
  sarima_data <- reactive({
    req(input$sarima_country)
    df <- data()
    selected_country <- input$sarima_country
    
    country_data <- df[df$`Country of Citizenship` == selected_country, ]
    if (nrow(country_data) == 0) return(NULL)
    
    quarterly_columns <- grep("Q[1-4] Total 20(1[5-9]|2[0-3])", colnames(country_data), value = TRUE)
    if (length(quarterly_columns) == 0) return(NULL)
    
    quarterly_totals <- unlist(country_data[1, quarterly_columns], use.names = FALSE)
    quarterly_totals <- as.numeric(quarterly_totals)
    quarterly_totals[is.na(quarterly_totals)] <- mean(quarterly_totals, na.rm = TRUE)
    
    # Simple check to avoid errors on empty/constant data
    if (all(quarterly_totals == 0) | length(unique(quarterly_totals)) < 2) return(NULL)
    
    ts_data <- ts(quarterly_totals, start = c(2015, 1), frequency = 4)
    
    tryCatch({
      model <- auto.arima(ts_data, seasonal = TRUE)
      forecasted <- forecast(model, h = 20)
      list(model = model, forecast = forecasted)
    }, error = function(e) return(NULL))
  })
  
  output$forecastPlot <- renderPlot({
    forecasted_data <- sarima_data()
    if (is.null(forecasted_data)) {
        plot(1, type="n", axes=F, xlab="", ylab="")
        text(1, 1, "Insufficient data for forecasting", cex = 1.2)
        return()
    }
    
    forecast <- forecasted_data$forecast
    ts_data <- forecasted_data$model$x
    
    ggplot() +
      geom_line(aes(x = time(ts_data), y = TS_DATA_VAL <- as.numeric(ts_data)), color = "blue") +
      geom_line(aes(x = time(forecast$mean), y = FORECAST_VAL <- as.numeric(forecast$mean)), color = "red") +
      geom_ribbon(aes(
        x = time(forecast$mean),
        ymin = as.numeric(forecast$lower[, 2]),
        ymax = as.numeric(forecast$upper[, 2])
      ), fill = "pink", alpha = 0.2) +
      labs(title = paste("SARIMA Forecast for", input$sarima_country), x = "Time", y = "Values")
  })
  
  output$forecastTable <- renderTable({
    forecasted_data <- sarima_data()
    if (is.null(forecasted_data)) return()
    
    forecast <- forecasted_data$forecast
    data.frame(
      Time = as.character(time(forecast$mean)),
      Mean = forecast$mean,
      Lower_80 = forecast$lower[, 1],
      Upper_80 = forecast$upper[, 1],
      Lower_95 = forecast$lower[, 2],
      Upper_95 = forecast$upper[, 2]
    )
  })
}

shinyApp(ui = ui, server = server)
