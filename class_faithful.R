

library(shiny)

ui <- fluidPage(
  titlePanel("Faithful Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput(
        inputId = "bins",
        label = "Number of bins:",
        min = 5, max = 50, value = 20
      )
    ),
    
    mainPanel(
      plotOutput("hist")
    )
  )
)

server <- function(input, output) {
  output$hist <- renderPlot({
    hist(faithful$eruptions, breaks = input$bins)
  })
}

shinyApp(ui = ui, server = server)



library(shiny)
library(ggplot2)

# Load data
rpg <- read.csv("RPG.csv")

ui <- fluidPage(
  titlePanel("RPG Character Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      
      selectInput(
        inputId = "class",
        label = "Select Class:",
        choices = unique(rpg$Class),
        selected = unique(rpg$Class),
        multiple = TRUE
      ),
      
      sliderInput(
        inputId = "bins",
        label = "Histogram Bins:",
        min = 5, max = 50, value = 20
      ),
      
      selectInput(
        inputId = "xvar",
        label = "X-axis:",
        choices = names(rpg)[1:5],  # numeric columns
        selected = "Physical"
      ),
      
      selectInput(
        inputId = "yvar",
        label = "Y-axis:",
        choices = names(rpg)[1:5],
        selected = "Magic"
      )
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Histogram", plotOutput("histPlot")),
        tabPanel("Scatter Plot", plotOutput("scatterPlot"))
      )
    )
  )
)

server <- function(input, output) {
  
  # Filter data based on class selection
  filtered_data <- reactive({
    rpg[rpg$Class %in% input$class, ]
  })
  
  # Histogram
  output$histPlot <- renderPlot({
    ggplot(filtered_data(), aes(x = Level)) +
      geom_histogram(bins = input$bins, fill = "steelblue", color = "black") +
      theme_minimal() +
      labs(title = "Level Distribution")
  })
  
  # Scatter Plot
  output$scatterPlot <- renderPlot({
    ggplot(filtered_data(), aes_string(x = input$xvar, y = input$yvar, color = "Class")) +
      geom_point(size = 3) +
      theme_minimal() +
      labs(title = "Attribute Comparison")
  })
}

shinyApp(ui = ui, server = server)






library(shiny)
library(ggplot2)

# Load data
rpg <- read.csv("RPG.csv")

ui <- fluidPage(
  titlePanel("RPG Character Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      
      # Class filter
      selectInput(
        inputId = "class",
        label = "Select Class:",
        choices = unique(rpg$Class),
        selected = unique(rpg$Class),
        multiple = TRUE
      ),
      
      # Histogram bins
      sliderInput(
        inputId = "bins",
        label = "Histogram Bins:",
        min = 5, max = 50, value = 20
      ),
      
      # Variable selector for boxplot
      selectInput(
        inputId = "boxvar",
        label = "Select Variable for Box Plot:",
        choices = names(rpg)[sapply(rpg, is.numeric)],
        selected = names(rpg)[sapply(rpg, is.numeric)][1]
      )
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Histogram", plotOutput("histPlot")),
        tabPanel("Scatter Plot", plotOutput("scatterPlot")),
        tabPanel("Box Plot", plotOutput("boxPlot"))
      )
    )
  )
)

server <- function(input, output) {
  
  # Reactive filtered data
  filtered_data <- reactive({
    rpg[rpg$Class %in% input$class, ]
  })
  
  # Histogram
  output$histPlot <- renderPlot({
    ggplot(filtered_data(), aes(x = Level)) +
      geom_histogram(bins = input$bins, fill = "steelblue", color = "black") +
      theme_minimal() +
      labs(title = "Level Distribution")
  })
  
  # Scatter Plot
  output$scatterPlot <- renderPlot({
    ggplot(filtered_data(), aes(x = Physical, y = Magic, color = Class)) +
      geom_point(size = 3) +
      theme_minimal() +
      labs(title = "Physical vs Magic")
  })
  
  # Box Plot (NEW)
  output$boxPlot <- renderPlot({
    ggplot(filtered_data(), aes_string(x = "Class", y = input$boxvar, fill = "Class")) +
      geom_boxplot(alpha = 0.7) +
      theme_minimal() +
      labs(
        title = paste("Distribution of", input$boxvar, "by Class"),
        x = "Class",
        y = input$boxvar
      ) +
      theme(legend.position = "none")
  })
}

shinyApp(ui = ui, server = server)






library(shiny)
library(ggplot2)

# Load data
rpg <- read.csv("RPG.csv")

# Get numeric columns
numeric_vars <- names(rpg)[sapply(rpg, is.numeric)]

ui <- fluidPage(
  titlePanel("RPG Character Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      
      # Class filter
      selectInput(
        inputId = "class",
        label = "Select Class:",
        choices = unique(rpg$Class),
        selected = unique(rpg$Class),
        multiple = TRUE
      ),
      
      # Histogram bins
      sliderInput(
        inputId = "bins",
        label = "Histogram Bins:",
        min = 5, max = 50, value = 20
      ),
      
      hr(),
      
      # Scatter plot controls
      h4("Scatter Plot Controls"),
      selectInput("xvar", "X-axis:", choices = numeric_vars, selected = numeric_vars[1]),
      selectInput("yvar", "Y-axis:", choices = numeric_vars, selected = numeric_vars[2]),
      
      hr(),
      
      # Box plot control
      h4("Box Plot Control"),
      selectInput(
        inputId = "boxvar",
        label = "Variable:",
        choices = numeric_vars,
        selected = numeric_vars[1]
      )
    ),
    
    mainPanel(
      tabsetPanel(
        
        tabPanel("Histogram",
                 plotOutput("histPlot")
        ),
        
        tabPanel("Scatter Plot",
                 plotOutput("scatterPlot")
        ),
        
        tabPanel("Box Plot",
                 plotOutput("boxPlot")
        )
      )
    )
  )
)

server <- function(input, output) {
  
  # Filter data
  filtered_data <- reactive({
    rpg[rpg$Class %in% input$class, ]
  })
  
  # Histogram
  output$histPlot <- renderPlot({
    ggplot(filtered_data(), aes(x = Level)) +
      geom_histogram(bins = input$bins, fill = "steelblue", color = "black") +
      theme_minimal() +
      labs(title = "Level Distribution")
  })
  
  # Scatter Plot (dynamic)
  output$scatterPlot <- renderPlot({
    ggplot(
      filtered_data(),
      aes_string(x = input$xvar, y = input$yvar, color = "Class")
    ) +
      geom_point(size = 3, alpha = 0.7) +
      theme_minimal() +
      labs(
        title = paste(input$xvar, "vs", input$yvar),
        x = input$xvar,
        y = input$yvar
      )
  })
  
  # Box Plot
  output$boxPlot <- renderPlot({
    ggplot(
      filtered_data(),
      aes_string(x = "Class", y = input$boxvar, fill = "Class")
    ) +
      geom_boxplot(alpha = 0.7) +
      theme_minimal() +
      labs(
        title = paste("Distribution of", input$boxvar, "by Class"),
        x = "Class",
        y = input$boxvar
      ) +
      theme(legend.position = "none")
  })
}

shinyApp(ui = ui, server = server)





library(shiny)
library(ggplot2)

# Load data
rpg <- read.csv("RPG.csv")

# Get numeric columns
numeric_vars <- names(rpg)[sapply(rpg, is.numeric)]

ui <- fluidPage(
  titlePanel("RPG Character Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      
      # Class filter
      selectInput(
        inputId = "class",
        label = "Select Class:",
        choices = unique(rpg$Class),
        selected = unique(rpg$Class),
        multiple = TRUE
      ),
      
      # Histogram bins
      sliderInput(
        inputId = "bins",
        label = "Histogram Bins:",
        min = 5, max = 50, value = 20
      ),
      
      hr(),
      
      # Scatter plot controls
      h4("Scatter Plot Controls"),
      selectInput("xvar", "X-axis:", choices = numeric_vars, selected = numeric_vars[1]),
      selectInput("yvar", "Y-axis:", choices = numeric_vars, selected = numeric_vars[2]),
      
      hr(),
      
      # Box plot control
      h4("Box Plot Control"),
      selectInput(
        inputId = "boxvar",
        label = "Variable:",
        choices = numeric_vars,
        selected = numeric_vars[1]
      )
    ),
    
    mainPanel(
      
      # First row: Histogram + Scatter
      fluidRow(
        column(6,
               h4("Histogram"),
               plotOutput("histPlot")
        ),
        
        column(6,
               h4("Scatter Plot"),
               plotOutput("scatterPlot")
        )
      ),
      
      br(),
      
      # Second row: Box Plot full width
      fluidRow(
        column(12,
               h4("Box Plot"),
               plotOutput("boxPlot")
        )
      )
      
    )
  )
)

server <- function(input, output) {
  
  # Filter data
  filtered_data <- reactive({
    rpg[rpg$Class %in% input$class, ]
  })
  
  # Histogram
  output$histPlot <- renderPlot({
    ggplot(filtered_data(), aes(x = Level)) +
      geom_histogram(bins = input$bins, fill = "steelblue", color = "black") +
      theme_minimal() +
      labs(title = "Level Distribution")
  })
  
  # Scatter Plot
  output$scatterPlot <- renderPlot({
    ggplot(
      filtered_data(),
      aes_string(x = input$xvar, y = input$yvar, color = "Class")
    ) +
      geom_point(size = 3, alpha = 0.7) +
      theme_minimal() +
      labs(
        title = paste(input$xvar, "vs", input$yvar),
        x = input$xvar,
        y = input$yvar
      )
  })
  
  # Box Plot
  output$boxPlot <- renderPlot({
    ggplot(
      filtered_data(),
      aes_string(x = "Class", y = input$boxvar, fill = "Class")
    ) +
      geom_boxplot(alpha = 0.7) +
      theme_minimal() +
      labs(
        title = paste("Distribution of", input$boxvar, "by Class"),
        x = "Class",
        y = input$boxvar
      ) +
      theme(legend.position = "none")
  })
}

shinyApp(ui = ui, server = server)





