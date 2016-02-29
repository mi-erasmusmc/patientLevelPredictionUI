shinyServer(function(input, output) {
  
  # output for lasso lr
  output$params.1 <- 
    renderUI(
      wellPanel(
        h4("Filter Variables"),
        helpText("Method:  A filtering method that includes/excludes",
                 "a user defined set of covariate_ids, concept_ids and/or",
                 "analysis_ids."),
        textInput("cov", "Covariate IDs", "1,5,501"),
        checkboxInput('covInclude', "Include", 
                      value = TRUE, width = NULL),
        textInput("concept", "Concept IDs", "1,5,501"),
        checkboxInput('conceptInclude', "Include", 
                      value = TRUE, width = NULL),
        textInput("analysis", "Analysis IDs", "1,5,501"),
        checkboxInput('analysisInclude', "Include", 
                      value = TRUE, width = NULL),
        actionButton("addfeatsel1", "Add feature selection settings to list")
      ))
  
  
  # output for GA wrapper
  output$params.2 <- 
    renderUI(
      wellPanel(
        h4("Feature seleciton using Genetic Algorithms"),
        helpText("Method:  Implements a genetic algorithm to",
                 "find a subset of the variables that appear",
                 "to result in the best performance on a random subset",
                 "of the training data.",
                 
                 "Finds approxiamtely varSize number of variables",
                 "after iter number of interations of the genetic algorithm",
                 "using the specified classifier."),
        
        sliderInput("iter", "Number of iterations:",
                    min = 1, max = 1000, value = 100, step = 5),
        sliderInput("varSize", "Approx number of variables to return:",
                    min = 1, max = 2000, value =100, step = 10),
         
        checkboxInput('gbm', "Gradient Boosting Machine Model", 
                      value = TRUE, width = NULL),
        
        #nbins, min rows,learn rate
        
        actionButton("addfeatsel2", "Add feature selection settings to list")
        
      ))
  
  # output for lassolr
  output$params.3 <- 
    renderUI(
      wellPanel(
        h4("Select variables with non-zero coefficients uing Lasso logistic regression"),
        helpText("Method:  Applies lasso logistic regression first to ",
                 "select the variables important to that model and uses",
                 "these only."),
        sliderInput("val", "Initial Variance:",
                    min = 0, max = 2, value = 0.01, step = 0.001),
        #nbins, min rows,learn rate
        
        actionButton("addfeatsel3", "Add feature selection settings to list")
        
      ))
  
  # output for varImp
  data.test <- matrix(runif(100), ncol=10)
  output$params.4 <- 
    renderUI(
      wellPanel(
        h4("Tree based variable importance"),
        helpText("Method:  Picks a random sample of training data n times",
                 "and trains a gradient boosting machine using a random",
                 "sampSize number of features in each tree",
                 "then finds the aggregate variable importance",
                 "across all models to selec the top m variables."),
        sliderInput("n", "Number of gradient boosting machine models:",
                    min = 1, max = 100, value = 20, step = 5),
        sliderInput("m", "Number of variables returned:",
                    min = 1, max = 1000, value = 100, step = 1),
        sliderInput("sampSize", "Number of features per each tree:",
                    min = 1, max = 1000, value =50, step = 5),
  
        # nbins,min rows
        
        actionButton("addfeatsel4", "Add feature selection settings to list")
        
      ))
  
  # output for GLRM
  output$params.5 <- 
    renderUI(
      wellPanel(
        h4("Generalised Low Rank Model"),
        helpText("Method:  Combines the covariates into topics, therefor reducing the ",
                 "dimensionality of the data."),
        
        actionButton("addfeatsel5", "Add feature selection settings to list")
        
      ))

  
  # create a list of the parameters when the action is 
  # pressed and append to existing models list
  # then output the names of the models+parameters on hover?
  featAdd1 <- eventReactive(input$addfeatsel1, {
    list(model='filterCovariates', 
         param=input
    )
  })
  featAdd2 <- eventReactive(input$addfeatsel2, {
    list(model='wrapperGA', 
         param=input
    )
  })
  featAdd3 <- eventReactive(input$addfeatsel3, {
    list(model='lassolr', 
         param=input
    )
  })
  featAdd4 <- eventReactive(input$addfeatsel4, {
    list(model='varImp', 
         param=input
    )
  })
  featAdd5 <- eventReactive(input$addfeatsel5, {
    list(model='glrm', 
         param=input
    )
  })
  
  # add to all lists to use to run the models went the 'run Models' button is hit
  ##allModelSettings <- c(allModelSettings, modelAdd)
  
  output$features <-renderText({
    paste0('Model: ',featAdd1()$model,
           'covariate_id: ', featAdd1()$param$cov,
           'include:', featAdd1()$param$covInclude,
           'concept_id: ', featAdd1()$param$concept)# need to add hover over parameters and option to delete
    
  })
  #paste(lapply(modelAdd(), function(x) x$model), collapse='<p>')
  
  
})


