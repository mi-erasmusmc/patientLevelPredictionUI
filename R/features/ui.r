shinyUI(fluidPage(
  
  titlePanel("Feature Selection"),
  
  
  # (TODO) would be good to have option of second, third,... models
  column(4, wellPanel(
    selectInput("featSel", label = "Select Method", 
                choices = list("Filter covariates" = 1, 
                               "Wrapper GA" = 2, 
                               "Lasso logistic regression" = 3,
                               "Variable Importance" = 4,
                               "Generalised Low Rank Models"=5), 
                selected = 1)
  ),
  wellPanel(
    h5("Selected Feature Selection:"),
    verbatimTextOutput("features"),
    actionButton("refresh", "Refresh")
    #htmlOutput("models")
  )
  
  
  ),
  
  column(7,
         "Pick feature selection and settings and add.",
         
         # With the conditionalPanel, the condition is a JavaScript
         # expression. In these expressions, input values like
         # input$n are accessed with dots, as in input.n
         conditionalPanel("input.featSel==1",
                          uiOutput("params.1", container = div)
         ), # can i use loop?
         conditionalPanel("input.featSel==2",
                          uiOutput("params.2", container = div)
         ),
         conditionalPanel("input.featSel==3",
                          uiOutput("params.3", container = div)
         ),
         conditionalPanel("input.featSel==4",
                          uiOutput("params.4", container = div)
         )
         ,
         conditionalPanel("input.featSel==5",
                          uiOutput("params.5", container = div)
         )
         )
         
         # option to add own model:
         #fileInput(inputId, label, multiple = FALSE, accept = NULL, width = NULL)
         #installExprFunction
         
         # This might be better than the scroll for some cases:
         # numericInput(inputId, label, value, min = NA, max = NA, step = NA,width = NULL)
         
         
         
  ))
