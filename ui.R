library(shiny)

# Define UI for random distribution application 
shinyUI(fluidPage(
  
  # Application title
  titlePanel("ClassifyR"),
  strong(p("A Shiny App created by Ajay Kumar")),
  helpText("The shiny app allows the user to see", 
           "the results of different binary classification",
           "models on the university admit data",
           "provided by the IDRE consutling group at UCLA",
           "The first tab shows the gains curve which is", 
           "a popular metric in industry to assess clasifiers",
           "The second tab shows the confusion matrix.",
           "It has a slider to control the threshold.",
           "The user can see how the confustion matrix",
           "changes at each cut-off. Please see that changing",
           "the slider in this tab does not affect results of",
           "the first and the third tab",
           "The final tab displays a table of results"),
  
  # Sidebar with controls to select the threshold
  sidebarLayout(
    sidebarPanel(
      helpText("Choose a model from the drop", 
               "down list"),
      
      # Drop down list
      selectInput("model_type", label = h3("Select a model"),
                  
                   list("Logistic Regression" = "logis",
                        "Logistic L1 Penalty" = "LASSO",
                        "Logistic L2 Penalty" = "Ridge",
                        "Probit Regression" = "Probit",
                        "Adaboost Exponential Loss" = "Adaexp",
                        "Random Forest"= "randomForest"), selected = "logis"),
      
      br(),
      helpText("Choose % of observations in training set"),
      # let user slide value of threshold
      sliderInput("t", 
                  label = h3("Threshold for Training"), 
                  value = 0.7,
                  min = 0.3, 
                  max = 0.8),
      width = 4
    ),
    
    # Show a tabset that includes a plot, summary, and table view
    # of the generated distribution
    mainPanel(
      tabsetPanel(type = "tabs", 
                  # Display of graph
                  tabPanel("Lift Curve", plotOutput("plot")), 
                  
                  # Display of confustion matrix
                  tabPanel("Confusion Matrix", 
                           fluidRow(
                             column(3, offset = 0,
                               tableOutput("summary")      
                                    ),
                             
                             column(6, offset = 3,
                                tableOutput("Metrics"))),
                          hr(),          
                          # let user slide value of threshold
                          br(),
                          helpText("Change this to choose a cut-off"),
                          sliderInput("p", 
                                       label = h3("Threshold for cutoff:"), 
                                       value = 0.3,
                                       min = 0, 
                                       max = 1)), 
                         
                  # Display Results       
                  tabPanel("Results", tableOutput("table"))
      )
    )
  )
))
