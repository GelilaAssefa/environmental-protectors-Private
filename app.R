# Libraries
library(shiny)
library(ggplot2)
library(dplyr)
library(readxl)
library(janitor)
library(DT)
library(scales)
library(countries)
library(plotly)
library(bslib)

################################### Data Preparation
# Initial data load for time series
data_GA <- read_excel("./data/clean_emdat_data.xlsx") %>%
  rename(
    Start_Year = year,
    Total_Damage = `Total Damage ('000 US$)`
  )

# Time series data preparation
time_series_data <- data_GA %>%
  filter(Start_Year >= 2000 & Start_Year <= 2023) %>%
  group_by(Start_Year) %>%
  summarize(
    Total_Deaths = sum(`Total Deaths`, na.rm = TRUE),
    Total_Affected = sum(`Total Affected`, na.rm = TRUE),
    Total_Damage_USD = sum(Total_Damage, na.rm = TRUE)
  )

# Main data preparation
EMDAT = read_excel("data/clean_emdat_data.xlsx") %>%
  clean_names() %>%
  select(-dis_no, -location, -month, -start_date, -end_date, -reconstruction_costs_000_us) %>%
  filter(magnitude >= 0)

# Map data preparation
EMDAT_CS = read_excel("./data/clean_emdat_data.xlsx") %>%
  clean_names() %>%
  select(-dis_no, -month, -start_date, -end_date)

EMDAT_MAP_SELECTION = EMDAT_CS %>%
  select(-disaster_group, -disaster_subgroup, -disaster_type, -disaster_subtype, 
         -country, -region, -subregion, -location, -reconstruction_costs_000_us)

nat_or_tech <- c("Natural", "Technological")

################################### UI Definition
ui <- fluidPage(
  theme = bslib::bs_theme(version = 5, bootswatch = "yeti"),
  
  tags$head(
    tags$style(HTML("
      .container-fluid { padding: 0; }
      .tab-content { height: calc(100vh - 120px); }
      .card { height: 100%; }
      .card-body { overflow-y: auto; }
      .nav-pills { margin-bottom: 15px; }
      .control-label { font-weight: bold; }
      .plot-container { height: calc(100vh - 200px); }
      .main-plot { transform: scale(0.9); transform-origin: top left; }
      .lead { font-size: 1.2rem; font-weight: 300; }
      .list-group-item { border-left: none; border-right: none; }
      h1, h2, h3 { color: #2c3e50; }
      .alert-info { background-color: #f8f9fa; border-color: #e9ecef; color: #495057; }
    "))
  ),
  
  titlePanel("Impact of Natural and Technological Disasters"),
  
  div(
    class = "container-fluid",
    tabsetPanel(
      type = "pills",
      
      # Introduction Tab
      tabPanel(
        "Introduction",
        div(
          class = "row h-100 m-0",
          div(
            class = "col-md-8 mx-auto p-4",
            div(
              class = "card shadow-sm",
              div(
                class = "card-body",
                h1("Impact of Natural and Technological Disasters", 
                   class = "mb-4 text-center"),
                
                p(class = "lead text-muted mb-4",
                  "This application provides an exploratory analysis of global disaster data from EM-DAT, 
                   the International Disaster Database."),
                
                h2("About EM-DAT", class = "mb-3"),
                p("EM-DAT was created with the initial support of the World Health Organisation (WHO) 
                   and the Belgian Government. The main objective of the database is to serve the purposes 
                   of humanitarian action at national and international levels."),
                
                h3("Data Source", class = "mb-3"),
                p("The data is sourced from ", 
                  tags$a(href = "https://www.emdat.be/", 
                         "EM-DAT (The International Disaster Database)", 
                         target = "_blank"),
                  ", maintained by the Centre for Research on the Epidemiology of 
                   Disasters (CRED)."),
                
                h3("Key Features", class = "mb-3"),
                tags$ul(
                  class = "list-group list-group-flush mb-4",
                  tags$li(class = "list-group-item", 
                          tags$b("Univariate Analysis: "), 
                          "Explore individual disaster metrics and their distributions"),
                  tags$li(class = "list-group-item", 
                          tags$b("Linear Model Analysis: "), 
                          "Investigate relationships between different disaster variables"),
                  tags$li(class = "list-group-item", 
                          tags$b("Time Series Analysis: "), 
                          "Track disaster impacts over time from 2000 to 2023"),
                  tags$li(class = "list-group-item", 
                          tags$b("Geographic Analysis: "), 
                          "Visualize disaster impacts across different regions")
                ),
                
                h3("Data Coverage", class = "mb-3"),
                p("The database contains essential data on the occurrence and effects of 
                   mass disasters worldwide, from 1900 to the present day. The data includes:"),
                tags$ul(
                  class = "list-group list-group-flush mb-4",
                  tags$li(class = "list-group-item", 
                          "Natural disasters (earthquakes, floods, storms, etc.)"),
                  tags$li(class = "list-group-item", 
                          "Technological disasters (industrial accidents, transport accidents, etc.)"),
                  tags$li(class = "list-group-item", 
                          "Impact metrics (deaths, injuries, economic damage)"),
                  tags$li(class = "list-group-item", 
                          "Geographic and temporal information")
                )
              )
            )
          )
        )
      ),
      
      # Univariate Analysis Tab
      tabPanel(
        "Univariate Analysis",
        div(
          class = "row h-100 m-0",
          div(
            class = "col-md-3 p-3",
            div(
              class = "card shadow-sm",
              div(
                class = "card-body",
                varSelectInput("variable", "Select Variable", 
                               data = EMDAT[, !names(EMDAT) %in% "year"],
                               selected = "total_damage_000_us"),
                sliderInput("bins", "Number of Bins", 
                            min = 1, max = 100, value = 40),
                checkboxInput("flip_coords", "Flip Coordinates/Factors", 
                              value = TRUE),
                div(
                  style = "height: calc(100vh - 400px); overflow-y: auto;",
                  checkboxGroupInput("univariate_years", "Report Years", 
                                     choices = sort(unique(EMDAT$year)),
                                     selected = 2023)
                )
              )
            )
          ),
          div(
            class = "col-md-9 p-3",
            div(
              class = "card shadow-sm h-100",
              div(
                class = "card-body",
                div(
                  class = "main-plot",
                  conditionalPanel(
                    condition = "['total_deaths', 'no_injured', 'no_affected', 
                                 'total_affected', 'no_homeless', 'total_damage_000_us'].includes(input.variable)",
                    htmlOutput("summary_results")
                  ),
                  conditionalPanel(
                    condition = "!['total_deaths', 'no_injured', 'no_affected', 
                                 'total_affected', 'no_homeless', 'total_damage_000_us'].includes(input.variable)",
                    plotOutput("univariate_plot", height = "800px")
                  )
                )
              )
            )
          )
        )
      ),
      
      # Linear Model Analysis Tab
      tabPanel(
        "Linear Model Analysis",
        div(
          class = "row h-100 m-0",
          div(
            class = "col-md-3 p-3",
            div(
              class = "card shadow-sm",
              div(
                class = "card-body",
                sliderInput("year_range", "Year Range", 
                            min = 1900, max = 2024, 
                            value = c(2000, 2024)),
                selectInput("dependent", "Dependent Variable", 
                            choices = NULL,
                            selected = "total_damage_000_us"),
                selectizeInput("numeric_vars", "Numeric Variables", 
                               choices = NULL, 
                               selected = c("total_deaths", "total_affected", "magnitude"),
                               multiple = TRUE),
                selectizeInput("categorical_vars", "Categorical Variables", 
                               choices = NULL,
                               selected = c("disaster_type", "region"),
                               multiple = TRUE),
                checkboxInput("add_interactions", "Add Interactions", 
                              value = TRUE),
                checkboxInput("add_polynomial", "Add Polynomial Terms", 
                              value = FALSE),
                conditionalPanel(
                  condition = "input.add_polynomial == true",
                  selectizeInput("poly_vars", "Polynomial Variables",
                                 choices = NULL, multiple = TRUE),
                  sliderInput("poly_degree", "Polynomial Degree",
                              min = 2, max = 3, value = 2, step = 1)
                ),
                conditionalPanel(
                  condition = "input.add_interactions == true",
                  selectizeInput("interaction_vars", "Interaction Variables",
                                 choices = NULL, 
                                 selected = c("total_deaths", "magnitude"),
                                 multiple = TRUE)
                ),
                checkboxGroupInput("log_vars", "Log Transformation",
                                   choices = NULL),
                checkboxInput("scale_vars", "Scale Variables", 
                              value = TRUE),
                checkboxInput("remove_na", "Remove NA Values", 
                              value = TRUE),
                actionButton("build_model", "Build Model", 
                             class = "btn btn-primary w-100 mt-3")
              )
            )
          ),
          div(
            class = "col-md-9 p-3",
            div(
              class = "card shadow-sm h-100",
              div(
                class = "card-body",
                div(
                  class = "main-plot",
                  tabsetPanel(
                    tabPanel("Model Results",
                             verbatimTextOutput("model_formula"),
                             verbatimTextOutput("model_summary")
                    ),
                    tabPanel("Diagnostic Plots",
                             plotlyOutput("scatter_plot", height = "400px"),
                             plotlyOutput("residuals_plot", height = "400px")
                    )
                  )
                )
              )
            )
          )
        )
      ),
      
      # Time Series Analysis Tab
      tabPanel(
        "Time Series Analysis",
        div(
          class = "row h-100 m-0",
          div(
            class = "col-md-3 p-3",
            div(
              class = "card shadow-sm",
              div(
                class = "card-body",
                helpText("Explore disaster impact metrics over time (2000-2023)"),
                checkboxGroupInput(
                  "metrics",
                  "Select Metrics:",
                  choices = c("Total Deaths", "Total Affected", "Total Damage (USD)"),
                  selected = c("Total Deaths", "Total Affected", "Total Damage (USD)")
                )
              )
            )
          ),
          div(
            class = "col-md-9 p-3",
            div(
              class = "card shadow-sm h-100",
              div(
                class = "card-body",
                div(
                  class = "main-plot",
                  plotOutput("timeSeriesPlot", height = "600px")
                )
              )
            )
          )
        )
      ),
      
      # Geographic Analysis Tab
      tabPanel(
        "Geographic Analysis",
        div(
          class = "row h-100 m-0",
          div(
            class = "col-md-3 p-3",
            div(
              class = "card shadow-sm",
              div(
                class = "card-body",
                varSelectInput("var_map", "Variable to Explore", 
                               data = EMDAT_MAP_SELECTION, 
                               selected = "total_deaths"),
                checkboxGroupInput("tech", "Disaster Type", 
                                   choices = nat_or_tech, 
                                   selected = nat_or_tech)
              )
            )
          ),
          div(
            class = "col-md-9 p-3",
            div(
              class = "card shadow-sm h-100",
              div(
                class = "card-body",
                div(
                  class = "main-plot",
                  plotOutput("map_plot", height = "600px")
                )
              )
            )
          )
        )
      )
    )
  )
)
server <- function(input, output, session) {
  ####### Univariate Analysis Logic
  filtered = reactive({
    data_TV = EMDAT %>%
      filter(get(input$variable) != 0) %>%
      filter(year %in% input$univariate_years) %>%
      select(input$variable, year)
    
    data_TV
  })
  
  output$univariate_plot = renderPlot({
    req(filtered()) 
    plot_data = filtered()
    if(is.numeric(plot_data[[input$variable]])) {
      p = ggplot(plot_data, aes_string(input$variable)) +
        geom_histogram(bins = input$bins, fill = "#3366cc", color = "white") +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 20, face = "bold"),
          axis.title = element_text(size = 16),
          axis.text = element_text(size = 14)
        ) +
        labs(title = paste("Distribution of", gsub("_", " ", input$variable)))
      
    } else if(is.factor(plot_data[[input$variable]]) ||
              is.character(plot_data[[input$variable]])) {
      
      p = ggplot(plot_data, aes(x = .data[[input$variable]])) + 
        geom_bar(fill = "#3366cc") +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 20, face = "bold"),
          axis.title = element_text(size = 16),
          axis.text = element_text(size = 14)
        ) +
        labs(title = paste("Distribution of", gsub("_", " ", input$variable)))
      
      if(input$flip_coords) {
        p = p + coord_flip() + theme(
          axis.text.y = element_text(face = "bold", size = 14, color = "black"))
      }
    }
    
    p
  })
  
  output$summary_results = renderUI({
    data_TV = filtered()
    var_data = data_TV[[input$variable]]
    if(is.numeric(var_data)) {
      t_test = t.test(var_data)
      
      Estimate = comma(round(t_test$estimate), 2) 
      P_value = round(t_test$p.value, 3) 
      CI_lower = comma(round(t_test$conf.int[1]), 2) 
      CI_upper = comma(round(t_test$conf.int[2]), 2) 
      Total = comma(sum(var_data, na.rm = TRUE))
      Mean = comma(round(mean(var_data, na.rm = TRUE), 2))
      Median = comma(round(median(var_data, na.rm = TRUE), 2))
      SD = comma(round(sd(var_data, na.rm = TRUE), 2))
      
      HTML(paste0(
        "<div style='font-size: 18px; line-height: 1.5; padding: 20px;'>",
        "<h3 class='mb-4'>Statistical Summary</h3>",
        "<div class='card mb-3 p-3'><b>Total:</b> ", Total, "</div>",
        "<div class='card mb-3 p-3'><b>Mean:</b> ", Mean, "</div>",
        "<div class='card mb-3 p-3'><b>Median:</b> ", Median, "</div>",
        "<div class='card mb-3 p-3'><b>Standard Deviation:</b> ", SD, "</div>",
        "<div class='card mb-3 p-3'><b>T-Test Estimate:</b> ", Estimate, "</div>",
        "<div class='card mb-3 p-3'><b>Confidence Interval:</b> [", CI_lower,", ", CI_upper, "] </div>",
        "<div class='card mb-3 p-3'><b>P-Value:</b> ", P_value, "</div>",
        "</div>"))
    }
  })
  
  ####### Linear Model Logic
  data <- reactive({
    df <- read_excel("data/clean_emdat_data.xlsx") %>%
      clean_names() %>%
      mutate(across(where(is.numeric), ~ifelse(is.infinite(.), NA, .))) %>%
      mutate(across(where(is.numeric), ~ifelse(. == 0, NA, .))) %>%
      mutate(across(where(is.numeric), ~ifelse(abs(.) > 1e10, NA, .)))
    
    df <- df[df$year >= input$year_range[1] & df$year <= input$year_range[2], ]
    return(df)
  })
  
  observe({
    req(data())
    df <- data()
    
    numeric_cols <- names(select_if(df, is.numeric))
    categorical_cols <- c("disaster_group", "disaster_type", "disaster_subtype", 
                          "country", "region", "subregion")
    categorical_cols <- categorical_cols[categorical_cols %in% names(df)]
    
    numeric_cols <- setdiff(numeric_cols, "year")
    
    pretty_numeric <- tools::toTitleCase(gsub("_", " ", numeric_cols))
    names(numeric_cols) <- pretty_numeric
    
    pretty_categorical <- tools::toTitleCase(gsub("_", " ", categorical_cols))
    names(categorical_cols) <- pretty_categorical
    
    updateSelectInput(session, "dependent", 
                      choices = numeric_cols, 
                      selected = "total_damage_000_us")
    updateSelectizeInput(session, "numeric_vars", 
                         choices = numeric_cols,
                         selected = c("total_deaths", "total_affected", "magnitude"))
    updateSelectizeInput(session, "categorical_vars", 
                         choices = categorical_cols,
                         selected = c("disaster_type", "region"))
    updateCheckboxGroupInput(session, "log_vars", 
                             choices = numeric_cols,
                             selected = c("total_damage_000_us", "total_affected"))
    updateSelectizeInput(session, "poly_vars", choices = numeric_cols)
    updateSelectizeInput(session, "interaction_vars", 
                         choices = c(numeric_cols, categorical_cols),
                         selected = c("total_deaths", "magnitude"))
  })
  
  model_data <- reactive({
    req(input$dependent, input$numeric_vars)
    
    df <- data()
    
    for(var in input$categorical_vars) {
      df[[var]] <- as.factor(df[[var]])
    }
    
    for(var in input$log_vars) {
      if(all(df[[var]] > 0, na.rm = TRUE)) {
        df[[var]] <- log(df[[var]])
      } else if(all(df[[var]] >= 0, na.rm = TRUE)) {
        df[[var]] <- log1p(df[[var]])
      }
    }
    
    if(input$scale_vars) {
      numeric_vars <- c(input$dependent, input$numeric_vars)
      df[numeric_vars] <- scale(df[numeric_vars])
    }
    
    if(input$remove_na) {
      vars_to_check <- c(input$dependent, input$numeric_vars, input$categorical_vars)
      df <- df[complete.cases(df[vars_to_check]), ]
    }
    
    return(df)
  })
  
  model_formula <- reactive({
    req(input$dependent, length(c(input$numeric_vars, input$categorical_vars)) > 0)
    
    terms <- c(input$numeric_vars, input$categorical_vars)
    
    if(input$add_polynomial && length(input$poly_vars) > 0) {
      poly_terms <- sapply(input$poly_vars, function(var) {
        paste0("poly(", var, ", ", input$poly_degree, ", raw = TRUE)")
      })
      terms <- c(terms, poly_terms)
    }
    
    if(input$add_interactions && length(input$interaction_vars) >= 2) {
      interactions <- utils::combn(input$interaction_vars, 2, function(x) {
        paste(x, collapse = ":")
      })
      terms <- c(terms, interactions)
    }
    
    as.formula(paste(input$dependent, "~", paste(terms, collapse = " + ")))
  })
  
  model <- eventReactive(input$build_model, {
    req(model_data(), model_formula())
    
    tryCatch({
      mod <- lm(model_formula(), data = model_data())
      if(summary(mod)$r.squared < 0.1) {
        showNotification(
          "Warning: Model has very low R-squared value. Consider:
           1. Adding more relevant predictors
           2. Checking for non-linear relationships
           3. Transforming variables (e.g., log transformation)", 
          type = "warning",
          duration = 10
        )
      }
      mod
    }, error = function(e) {
      showNotification(paste("Error building model:", e$message), 
                       type = "error",
                       duration = 10)
      NULL
    })
  })
  
  output$model_formula <- renderPrint({
    req(model_formula())
    cat("Model Formula:\n")
    print(model_formula())
  })
  
  output$model_summary <- renderPrint({
    req(model())
    summary(model())
  })
  
  output$scatter_plot <- renderPlotly({
    req(model())
    predicted <- predict(model())
    actual <- model()$model[[1]]
    
    plot_ly() %>%
      add_trace(x = predicted,
                y = actual,
                type = "scatter",
                mode = "markers",
                marker = list(color = "#3366cc",
                              size = 8,
                              opacity = 0.7),
                name = "Data Points") %>%
      add_trace(x = range(predicted),
                y = range(predicted),
                type = "scatter",
                mode = "lines",
                line = list(color = "red",
                            width = 2),
                name = "Perfect Fit") %>%
      layout(title = list(
        text = "Actual vs Predicted Values",
        font = list(size = 16)
      ),
      xaxis = list(title = "Predicted Values"),
      yaxis = list(title = "Actual Values"),
      showlegend = TRUE,
      legend = list(x = 0.1, y = 0.9))
  })
  
  output$residuals_plot <- renderPlotly({
    req(model())
    fitted_vals <- fitted(model())
    resid_vals <- resid(model())
    
    plot_ly() %>%
      add_trace(x = fitted_vals,
                y = resid_vals,
                type = "scatter",
                mode = "markers",
                marker = list(color = "#3366cc",
                              size = 8,
                              opacity = 0.7)) %>%
      add_trace(x = range(fitted_vals),
                y = c(0, 0),
                type = "scatter",
                mode = "lines",
                line = list(color = "red",
                            width = 2,
                            dash = "dash")) %>%
      layout(title = list(
        text = "Residuals vs Fitted Values",
        font = list(size = 16)
      ),
      xaxis = list(title = "Fitted Values"),
      yaxis = list(title = "Residuals"))
  })
  
  ####### Time Series Analysis Logic
  output$timeSeriesPlot <- renderPlot({
    metrics <- input$metrics
    
    p <- ggplot(time_series_data, aes(x = Start_Year)) +
      theme_minimal() +
      labs(
        title = "Disaster Impact Over Time (2000-2023)",
        x = "Year",
        y = "Impact Scale (log)",
        color = "Metrics"
      ) +
      theme(
        plot.title = element_text(size = 16, face = "bold"),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        legend.position = "bottom"
      )
    
    if("Total Deaths" %in% metrics) {
      p <- p + geom_line(aes(y = Total_Deaths, color = "Total Deaths"), 
                         size = 1)
    }
    if("Total Affected" %in% metrics) {
      p <- p + geom_line(aes(y = Total_Affected, color = "Total Affected"), 
                         size = 1)
    }
    if("Total Damage (USD)" %in% metrics) {
      p <- p + geom_line(aes(y = Total_Damage_USD, color = "Total Damage (USD)"), 
                         size = 1)
    }
    
    p + scale_y_log10(labels = comma)
  })
  
  ####### Geographic Analysis Logic
  output$map_plot = renderPlot({
    if (length(input$tech) == 1) {
      EMDAT_CS %>%
        filter(disaster_group == input$tech) %>%
        group_by(country) %>%
        summarize(total_country = sum(!!input$var_map, na.rm = TRUE)) -> EMDAT_MAP
      
      quick_map(EMDAT_MAP, "total_country", 
                theme = 9,
                name_legend = paste(input$var_map)) -> plot
      plot
    } else if (length(input$tech) == 2) {
      EMDAT_CS %>%
        group_by(country) %>%
        summarize(total_country = sum(!!input$var_map, na.rm = TRUE)) -> EMDAT_MAP
      
      quick_map(EMDAT_MAP, "total_country", 
                theme = 9,
                name_legend = paste(input$var_map)) -> plot
      plot
    } else {
      EMDAT_CS %>%
        mutate("test" = 0) %>%
        group_by(country) %>%
        summarize(total_country = sum(test)) -> EMDAT_TEST
      quick_map(EMDAT_TEST, "total_country",
                theme = 9,
                name_legend = paste(input$var_map)) -> plot
      plot
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)