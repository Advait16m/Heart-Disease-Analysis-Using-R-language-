library(shiny)
library(ggplot2)
library(dplyr)
library(DT)
library(reshape2)

# Load the dataset
heart_data <- read.csv("C:/BDA PROJECT/heart.csv")

# Convert numeric columns to factors with simpler labels
heart_data$sex <- factor(heart_data$sex, levels = c(0, 1), labels = c("Female", "Male"))
heart_data$cp <- factor(heart_data$cp, levels = c(0, 1, 2, 3), 
                        labels = c("Stable Chest Pain", "Unstable Chest Pain", "Non-Heart-Related Chest Pain", "Silent Chest Pain"))
heart_data$target <- factor(heart_data$target, levels = c(0, 1), labels = c("No Heart Disease", "Heart Disease"))

# Create age groups
heart_data$age_group <- cut(heart_data$age, breaks = c(29, 40, 50, 60, 70, 80), 
                            labels = c("30-40", "41-50", "51-60", "61-70", "71-80"))

# Define the UI
ui <- fluidPage(
  titlePanel("Heart Disease Analysis Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      # Filters
      sliderInput("ageInput", "Age:", 
                  min = min(heart_data$age, na.rm = TRUE), 
                  max = max(heart_data$age, na.rm = TRUE), 
                  value = c(min(heart_data$age, na.rm = TRUE), max(heart_data$age, na.rm = TRUE))),
      selectInput("sexInput", "Gender:", choices = c("Both", "Male", "Female")),
      selectInput("cpInput", "Chest Pain Type:", choices = c("All", unique(heart_data$cp))),
      actionButton("reset", "Reset Filters")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Data Table", DTOutput("dataTable")),
        tabPanel("Summary", verbatimTextOutput("summary")),
        tabPanel("Plots", 
                 plotOutput("genderDist"),   # Gender distribution
                 plotOutput("cpVsHeartDisease"),  # Chest pain type vs heart disease
                 plotOutput("ageGroupVsHeartDisease"),  # Age groups vs heart disease
                 plotOutput("bpHistogram"),  # Histogram of resting blood pressure
                 plotOutput("cholesterolDist"),  # Cholesterol distribution
                 plotOutput("heatmapCorrelation"),  # Heatmap of correlations
                 plotOutput("heartDiseasePrediction")  # Heart Disease Prediction Graph
        )
      )
    )
  )
)

# Define the server logic
server <- function(input, output, session) {
  
  # Filtered Data based on inputs
  filtered_data <- reactive({
    data <- heart_data
    
    # Age filter
    data <- data %>% filter(age >= input$ageInput[1] & age <= input$ageInput[2])
    
    # Gender filter
    if (input$sexInput == "Male") {
      data <- data %>% filter(sex == "Male")
    } else if (input$sexInput == "Female") {
      data <- data %>% filter(sex == "Female")
    }
    
    # Chest pain filter
    if (input$cpInput != "All") {
      data <- data %>% filter(cp == input$cpInput)
    }
    
    return(data)
  })
  
  # Data Table
  output$dataTable <- renderDT({
    datatable(filtered_data(), options = list(pageLength = 10))
  })
  
  # Summary
  output$summary <- renderPrint({
    summary(filtered_data())
  })
  
  # Plot 1: Gender Distribution
  output$genderDist <- renderPlot({
    ggplot(heart_data, aes(x = sex)) +
      geom_bar(fill = "skyblue") +
      labs(title = "Gender Distribution", x = "Gender", y = "Count") +
      theme_minimal()
  })
  
  # Plot 2: Chest Pain Type vs Heart Disease
  output$cpVsHeartDisease <- renderPlot({
    ggplot(filtered_data(), aes(x = cp, fill = target)) +
      geom_bar(position = "fill") +
      labs(title = "Chest Pain Type vs Heart Disease", x = "Chest Pain Type", y = "Proportion") +
      scale_fill_manual(values = c("No Heart Disease" = "skyblue", "Heart Disease" = "salmon")) +
      theme_minimal()
  })
  
  # Plot 3: Age Group vs Heart Disease (Bar Plot)
  output$ageGroupVsHeartDisease <- renderPlot({
    ggplot(filtered_data(), aes(x = age_group, fill = target)) +
      geom_bar(position = "fill") +
      labs(title = "Heart Disease Proportion by Age Group", x = "Age Group", y = "Proportion") +
      scale_fill_manual(values = c("No Heart Disease" = "skyblue", "Heart Disease" = "salmon")) +
      theme_minimal()
  })
  
  # Plot 4: Histogram of Resting Blood Pressure
  output$bpHistogram <- renderPlot({
    ggplot(filtered_data(), aes(x = trestbps)) +
      geom_histogram(binwidth = 10, fill = "lightblue", color = "black") +
      labs(title = "Resting Blood Pressure Distribution", x = "Resting Blood Pressure", y = "Count") +
      theme_minimal()
  })
  
  # Plot 5: Cholesterol Distribution
  output$cholesterolDist <- renderPlot({
    ggplot(filtered_data(), aes(x = chol)) +
      geom_histogram(binwidth = 10, fill = "lightcoral", color = "black") +
      labs(title = "Cholesterol Distribution", x = "Cholesterol", y = "Count") +
      theme_minimal()
  })
  
  # Plot 6: Heatmap of Correlation
  output$heatmapCorrelation <- renderPlot({
    # Select numeric columns for correlation
    numeric_data <- heart_data %>% 
      select(age, trestbps, chol, thalach, oldpeak)  # Include relevant numeric variables
    
    # Calculate correlation matrix
    corr_matrix <- cor(numeric_data, use = "complete.obs")
    
    # Melt the correlation matrix
    melted_corr <- melt(corr_matrix)
    
    # Plot heatmap
    ggplot(melted_corr, aes(Var1, Var2, fill = value)) +
      geom_tile(color = "white") +
      scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                           midpoint = 0, limit = c(-1, 1), space = "Lab", 
                           name = "Correlation") +
      labs(title = "Correlation Heatmap of Numeric Variables") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
  })
  
  # Plot 7: Heart Disease Prediction based on Age and Maximum Heart Rate
  output$heartDiseasePrediction <- renderPlot({
    ggplot(filtered_data(), aes(x = age, y = thalach, color = target)) +
      geom_point(alpha = 0.9) +
      labs(title = "Heart Disease Prediction by Age and Maximum Heart Rate", 
           x = "Age", 
           y = "Maximum Heart Rate Achieved",
           color = "Heart Disease") +
      scale_color_manual(values = c("No Heart Disease" = "skyblue", "Heart Disease" = "salmon")) +
      theme_minimal()
  })
  
  # Reset Filters
  observeEvent(input$reset, {
    updateSliderInput(session, "ageInput", value = c(min(heart_data$age, na.rm = TRUE), max(heart_data$age, na.rm = TRUE)))
    updateSelectInput(session, "sexInput", selected = "Both")
    updateSelectInput(session, "cpInput", selected = "All")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
