shinyServer(function(input, output, session) {
  
  # Example for using the lineChart
  output$mychart <- renderLineChart({
    # Return a data frame. Each column will be a series in the line chart.
    data.frame(
      Sine = sin(1:100/10 + input$sinePhase * pi/180) * input$sineAmplitude,
      Cosine = 0.5 * cos(1:100/10),
      "Sine 2" = sin(1:100/10) * 0.25 + 0.5
    )
 
  })
  
  output$image1 <- renderImage({
      return(list(
        src = "www/plato_large.png",
        contentType = "image/png",
        alt = "PLATO"
      ))
   }, deleteFile = FALSE)
  
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  
##=================================================================================
#  FEATURE STUFF
#==================================================================================
  # output for lasso lr
  output$params.1 <- 
    renderUI(
      wellPanel(
        h4("Filter Variables"),
        helpText("Method:  A filtering method that includes/excludes",
                 "a user defined set of covariate_ids, concept_ids and/or",
                 "analysis_ids."),
        selectInput("filterType", label = "Filter type", 
                    choices = list("Covariate IDs" = 1, 
                                   "Concept IDs" = 2, 
                                   "Analysis IDs" = 3), 
                    selected = 1),
        
        textInput("ids", "IDs: ", "1,5,501"),
        # textInput("concept", "Concept IDs", "1,5,501"),
        # textInput("analysis", "Analysis IDs", "1,5,501"),
        checkboxInput('include', "Include", 
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
  featList <- reactiveValues(model =NULL, param=NULL)
  
  observeEvent(input$addfeatsel1, {
    featList$model=c(featList$model,'Filter') 
    if(!is.null(featList$param))
      featList$param[[length(featList$param)+1]]=list(Type=ifelse(input$filterType==1, 'Covariate', 
                                                                  ifelse(input$filterType==2,'Concept','Analysis')),
                                                      Ids= input$ids,
                                                      include=input$include)
    if(is.null(featList$param))
      featList$param=list(list(Type=ifelse(input$filterType==1, 'Covariate', 
                                           ifelse(input$filterType==2,'Concept','Analysis')),
                               Ids= input$ids,
                               include=input$include))
  })
  
  observeEvent(input$addfeatsel2, {
    featList$model=c(featList$model,'Genetic Algorithm') 
    if(!is.null(featList$param))
      featList$param[[length(featList$param)+1]]=list(Iterations=input$iter,
                                                      varSize=input$varSize,
                                                      gbm=input$gbm)
    if(is.null(featList$param))
      featList$param=list(list(Iterations=input$iter,
                               varSize=input$varSize,
                               gbm=input$gbm))
  })
  
  observeEvent(input$addfeatsel3, {
    featList$model=c(featList$model,'Lasso Logistic Regression') 
    if(!is.null(featList$param))
      featList$param[[length(featList$param)+1]]=list(Variance=input$val)
    if(is.null(featList$param))
      featList$param=list(list(Variance=input$val))
  })
  
  observeEvent(input$addfeatsel4, {
    featList$model=c(featList$model,'Variable importance (tree)') 
    if(!is.null(featList$param))
      featList$param[[length(featList$param)+1]]=list(n=input$n, 
                                                      m=input$m,
                                                      sampSize=input$sampSize)
    if(is.null(featList$param))
      featList$param=list(list(n=input$n, 
                               m=input$m,
                               sampSize=input$sampSize))
  })
  
  observeEvent(input$addfeatsel5, {
    featList$model=c(featList$model,'GLRM') 
    if(!is.null(featList$param))
      featList$param[[length(featList$param)+1]]=list(NULL)
    if(is.null(featList$param))
      featList$param=list(list(NULL))
  })
  
  
  
  # add to all lists to use to run the models went the 'run Models' button is hit
  ##allModelSettings <- c(allModelSettings, modelAdd)
  convertToString2 <- function(featList){
    outString <- NULL
    for(i in 1:length(featList$model)){
      outString <- paste(outString, 
                         paste0('[',i,'] ',featList$model[i],'->',
                                paste(names(featList$param[[i]]),featList$param[[i]], 
                                      sep=':', collapse=', ' )
                         )
                         ,sep=' \n ')
    }
    return(outString)
  }
  
  # reset models:
  observeEvent(input$resetFeatures, {
    featList$model=NULL
    featList$param= NULL
  })
  
  output$features <-renderText({
    if(is.null(featList$model)) return()
    convertToString2(featList)
  })
  
  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  
  ##=================================================================================
  #  CLASSIFIER STUFF
  #==================================================================================
  # output for lasso lr
  output$modelparams.1 <- 
    renderUI(
      wellPanel(
        h4("Logistic Regression with lasso regularisation"),
        helpText("Method:  Logistic regression with lasso",
                 "regularisation will find the most",
                 "predictive features while the logstic regression",
                 "is trained.  Selectinf the 'with auto cross validation'",
                 "option will be slower but will search for the variance",
                 "that resulted in the best performance on the test data."),
        sliderInput("var", "Variance (starting variance in auto CV):",
                    min = 0, max = 1, value = 0.01, step = 0.001),
        checkboxInput('useCV', "With auto cross validation", 
                      value = TRUE, width = NULL),
        actionButton("addModelLR", "Add Model")
      )
    )
  
  
  # output for gbm
  output$modelparams.2 <- 
    renderUI(
      wellPanel(
        h4("Gradient Boosting Machine"),
        helpText("Method:  Gradient boosting machines",
                 "iteratively fits decision trees, but",
                 "each additional tree is selected based on",
                 "adding more weight to people who have often been'",
                 "misclassified by previous trees in the model.",
                 "The final prediction is a weighted combination of all",
                 "the individual decision trees.",
                 "The greater the number of trees, the more likely",
                 "the model is to overfit unless a suitable learn_rate",
                 "rSampRate or cSampRate are selected. Generally a",
                 "max_depth greater than 4 will not offer performance",
                 "improvements and will increase the training time."),
        sliderInput("ntrees", "Number of trees:",
                    min = 1, max = 1000, value = 100, step = 5),
        sliderInput("max_depth", "Maximum depth of tree:",
                    min = 1, max = 10, value =4, step = 1),
        sliderInput("learn_rate", "Learning rate (regularisation):",
                    min = 0, max = 1, value = 0.8, step = 0.01),
        sliderInput("rsampRate", "Fraction of test people per tree:",
                    min = 0, max = 1, value = 0.8, step = 0.01),
        sliderInput("csampRate", "Fraction of features per tree:",
                    min = 0, max = 1, value = 0.9, step = 0.01),
        
        checkboxInput('bal', "Balance classes", 
                      value = TRUE, width = NULL),
        
        #nbins, min rows,learn rate
        
        actionButton("addModelGBM", "Add Model")
        
      ))
  
  # output for knn
  output$modelparams.3 <- 
    renderUI(
      wellPanel(
        h4("KNN"),
        helpText("Method:  KNN finds the k closest",
                 "people(using euclidean distance) in the train set and assigns",
                 "the most common class in this neighbourhood.",
                 "Weighted KNN adds more weight to less recorded features ",
                 "when calculaing the distances.  A small k",
                 "is more likely to result in model overfitting"),
        sliderInput("k", "Number of neighbours:",
                    min = 1, max = 2000, value = 1000, step = 5),
        checkboxInput('weighted', "weight on feature frequency", 
                      value = TRUE, width = NULL),
        
        #nbins, min rows,learn rate
        
        actionButton("addModelKNN", "Add Model")
        
      ))
  
  # output for rf
  data.test <- matrix(runif(100), ncol=10)
  output$modelparams.4 <- 
    renderUI(
      wellPanel(
        h4("Random Forest"),
        helpText("Method:  Random forest trains",
                 "ntrees diverse decision trees and makes the prediciton",
                 "by combining all decsion tree predictions and finding the overal consensus.",
                 "Each individual decision tree is trained using mtries features and a random.",
                 "rSampRate fraction of the people.",
                 "Ensembles such as random forest are less likely to overfit"),
        sliderInput("mtries", "Number of features per tree (-1 is default):",
                    min = -1, max = ncol(data.test), value = -1, step = 1),
        sliderInput("ntrees", "Number of trees:",
                    min = 1, max = 1000, value = 100, step = 5),
        sliderInput("max_depth", "Maximum depth of tree:",
                    min = 1, max = 10, value =4, step = 1),
        sliderInput("learn_rate", "Learning rate (regularisation):",
                    min = 0, max = 1, value = 0.8, step = 0.01),
        sliderInput("rsampRate", "Fraction of test people per tree:",
                    min = 0, max = 1, value = 0.8, step = 0.01),
        
        checkboxInput('bal', "Balance classes", 
                      value = TRUE, width = NULL),
        
        # nbins,min rows
        
        actionButton("addModelRF", "Add Model")
        
      ))
  
  # output for neural network
  output$modelparams.5 <- 
    renderUI(
      wellPanel(
        h4("Neural Network"),
        helpText("Method:  Feedforward neural network",
                 "creates a network where each feature is an input node in the first layer",
                 "and connect with each of the 'size' number of hidden nodes such",
                 "that the value of the hidden nodes is a tranformation of a weighted",
                 "combination of the input values.",
                 "The output node is a weighted combination of the hidden node values.",
                 "Initially the weights are randomly chosen but the error is calculated for",
                 "the weights by passing the input values, to calculate the hidden node values ",
                 "and then the output values and comparing the squared difference",
                 "between the prediction and ground truth",
                 "the weights are then updated based on the error (using gradient descent)",
                 "until convergence."),
        
        sliderInput("size", "Size (number of hidden nodes):",
                    min = 1, max = 100, value = 5, step = 1),
        sliderInput("decay", "Decay (regularisation):",
                    min = 0, max = 0.99, value = 0.01, step = 0.01),
        checkboxInput('weights', "Weighted by classes size", 
                      value = TRUE, width = NULL),
        # nbins,min rows
        
        actionButton("addModelNnet", "Add Model")
        
        
      ))
  
  # output for support vector machine
  output$modelparams.6 <- 
    renderUI(
      wellPanel(
        h4("Support Vector Machine with Radial Kernal"),
        sliderInput("sigma", "Sigma:",
                    min = 0, max = 1, value = 0.2, step = 0.001),
        sliderInput("C", "Cost (regularisation):",
                    min = 0, max = 5, value = 0.1, step = 0.01),
        
        actionButton("addModelSVM", "Add Model")
        
      ))
  
  # create a list of the parameters when the action is 
  # pressed and append to existing models list
  # then output the names of the models+parameters on hover?
  allList <- reactiveValues(model =NULL, param=NULL)
  
  observeEvent(input$addModelLR, {
    allList$model=c(allList$model,'Logistic Regression with Lasso') 
   
    if(!is.null(allList$param))
      allList$param[[length(allList$param)+1]]=list(variance=input$var, 
                                                    useCV=input$useCV)
    if(is.null(allList$param))
      allList$param=list(list(variance=input$var, 
                              useCV=input$useCV))
  })
  
  observeEvent(input$addModelGBM, {
    allList$model=c(allList$model,'GBM') 
    if(!is.null(allList$param))
      allList$param[[length(allList$param)+1]]=list(ntrees=input$ntrees, 
                                                    max_depth=input$max_depth,
                                                    learn_rate=input$learn_rate,
                                                    rsampRate=input$rsampRate,
                                                    csampRate=input$csampRate,
                                                    bal=input$bal)
    if(is.null(allList$param))
      allList$param=list(list(ntrees=input$ntrees, 
                              max_depth=input$max_depth,
                              learn_rate=input$learn_rate,
                              rsampRate=input$rsampRate,
                              csampRate=input$csampRate,
                              bal=input$bal))
  })
  
  observeEvent(input$addModelKNN, {
    allList$model=c(allList$model,'KNN') 
    if(!is.null(allList$param))
      allList$param[[length(allList$param)+1]]=list(k=input$k, 
                                                    weighted=input$weighted)
    if(is.null(allList$param))
      allList$param=list(list(k=input$k, 
                              weighted=input$weighted))
  })
  
  observeEvent(input$addModelRF, {
    allList$model=c(allList$model,'RF') 
    if(!is.null(allList$param))
      allList$param[[length(allList$param)+1]]=list(mtries=input$mtries,
                                                    ntrees=input$ntrees, 
                                                    max_depth=input$max_depth,
                                                    learn_rate=input$learn_rate,
                                                    rsampRate=input$rsampRate,
                                                    
                                                    bal=input$bal)
    if(is.null(allList$param))
      allList$param=list(list(ntrees=input$ntrees, 
                              max_depth=input$max_depth,
                              learn_rate=input$learn_rate,
                              rsampRate=input$rsampRate,
                              csampRate=input$csampRate,
                              bal=input$bal))
  })
  
  observeEvent(input$addModelNnet, {
    allList$model=c(allList$model,'Neural network') 
    if(!is.null(allList$param))
      allList$param[[length(allList$param)+1]]=list(Size=input$size, 
                                                    decay=input$decay,
                                                    weighted=input$weights)
    if(is.null(allList$param))
      allList$param=list(list(Size=input$size, 
                              decay=input$decay,
                              weighted=input$weights))
  })
  
  observeEvent(input$addModelSVM, {
    allList$model=c(allList$model,'svmRadial plp') 
    if(!is.null(allList$param))
      allList$param[[length(allList$param)+1]]=list(C=input$C, sigma=input$sigma)
    if(is.null(allList$param))
      allList$param=list(list(C=input$C, sigma=input$sigma))
  })
  
  # add to all lists to use to run the models went the 'run Models' button is hit
  ##allModelSettings <- c(allModelSettings, modelAdd)
  convertToString <- function(modelList){
    outString <- NULL
    for(i in 1:length(modelList$model)){
      outString <- paste(outString, 
                         paste0('[',i,'] ',modelList$model[i],'->',
                                paste(names(modelList$param[[i]]),modelList$param[[i]], 
                                      sep=':', collapse=', ' )
                         )
                         ,sep=' \n ')
    }
    return(outString)
  }
  
  # reset models:
  observeEvent(input$resetModels, {
    allList$model=NULL
    allList$param= NULL
  })
  
  output$models <-renderText({
    if(is.null(allList$model)) return()
    convertToString(allList)
  })
  
  
  
})
