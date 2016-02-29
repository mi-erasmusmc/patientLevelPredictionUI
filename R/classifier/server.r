shinyServer(function(input, output) {

 # output for lasso lr
  output$params.1 <- 
    renderUI(
      wellPanel(
    h4("Logistic Regression with lasso regularisation"),
    helpText("Method:  Logistic regression with lasso",
             "regularisation will find the most",
             "predictive features while the logstic regression",
             "is trained.  Selectinf the 'with auto cross validation'",
             "option will be slower but will search for the variance",
             "that resulted in the best performance on the test data."),
  sliderInput("n", "Variance (starting variance in auto CV):",
              min = 0, max = 1, value = 0.01, step = 0.001),
  checkboxInput('useCV', "With auto cross validation", 
                value = TRUE, width = NULL),
  submitButton(text = "Add Model", icon = NULL, width = NULL))
    )
  
  
  # output for gbm
  output$params.2 <- 
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
        
        submitButton(text = "Add Model", icon = NULL, width = NULL)
        
      ))
  
  # output for knn
  output$params.3 <- 
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
        
        submitButton(text = "Add Model", icon = NULL, width = NULL)
        
      ))
  
  # output for rf
  data.test <- matrix(runif(100), ncol=10)
  output$params.4 <- 
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
        
        submitButton(text = "Add Model", icon = NULL, width = NULL)
        
      ))
  
  # output for neural network
  output$params.5 <- 
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
        
        submitButton(text = "Add Model", icon = NULL, width = NULL)

        
      ))
  
  # output for support vector machine
  output$params.6 <- 
    renderUI(
      wellPanel(
        h4("Support Vector Machine with Radial Kernal"),
        sliderInput("sigma", "Sigma:",
                    min = 0, max = 1, value = 0.2, step = 0.001),
        sliderInput("C", "Cost (regularisation):",
                    min = 0, max = 5, value = 0.1, step = 0.01),
        
        actionButton("addModel", "Add Model")
        
      ))
  
  # create a list of the parameters when the action is 
  # pressed and append to existing models list
  # then output the names of the models+parameters on hover?
  modelAdd <- eventReactive(input$addModel, {
    list(model='svmRadial plp', 
         #param=list(sigma=input$sigma, C=input$C)
         param=input
         )
    #'svmRadial plp'
  })
  
  # add to all lists to use to run the models went the 'run Models' button is hit
  ##allModelSettings <- c(allModelSettings, modelAdd)
    
  output$models <-renderText({
    paste0('Model: ',modelAdd()$model,
           'C: ', modelAdd()$param$C,
           'sigma: ', modelAdd()$param$sigma)# need to add hover over parameters and option to delete

  })
    #paste(lapply(modelAdd(), function(x) x$model), collapse='<p>')

  
})


