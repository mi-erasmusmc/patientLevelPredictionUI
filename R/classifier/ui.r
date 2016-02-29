shinyUI(fluidPage(

  titlePanel("Classifier Selection"),

  
  # (TODO) would be good to have option of second, third,... models
  column(4, wellPanel(
    selectInput("model", label = "Select Model", 
                choices = list("Lasso Logistic Regression" = 1, 
                               "Gradient Boosting Machine" = 2, 
                               "KNN" = 3,
                               "Random Forest" = 4,
                               "Neural Network"=5,
                               "Support Vector Machine"=6 ), 
                selected = 1)
  ),
  wellPanel(
  h5("Selected Models:"),
  verbatimTextOutput("models"),
  actionButton("runModel", "Run Model/s")
  #htmlOutput("models")
  )
  
  
  ),

  column(7,
    "Pick model/parameter settings and add model.",

    # With the conditionalPanel, the condition is a JavaScript
    # expression. In these expressions, input values like
    # input$n are accessed with dots, as in input.n
    conditionalPanel("input.model==1",
                    uiOutput("params.1", container = div)
    ), # can i use loop?
    conditionalPanel("input.model==2",
                     uiOutput("params.2", container = div)
    ),
    conditionalPanel("input.model==3",
                     uiOutput("params.3", container = div)
    ),
    conditionalPanel("input.model==4",
                     uiOutput("params.4", container = div)
    )
    ,
    conditionalPanel("input.model==5",
                     uiOutput("params.5", container = div)
    )
    ,
    conditionalPanel("input.model==6",
                     uiOutput("params.6", container = div)
    )
    
    # option to add own model:
    #fileInput(inputId, label, multiple = FALSE, accept = NULL, width = NULL)
    #installExprFunction
    
    # This might be better than the scroll for some cases:
    # numericInput(inputId, label, value, min = NA, max = NA, step = NA,width = NULL)

    
    
  )
))