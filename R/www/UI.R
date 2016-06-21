customHeaderPanel <- function(title,windowTitle=title){
  tagList(
    tags$head(
      tags$title(windowTitle),
      tags$h1(title),
      tags$link(rel="stylesheet", type="text/css",
                href="app.css")
    )
  )
}

shiny::shinyUI(
    shiny::fluidPage(
        shiny::navbarPage("PLATO",
                         # customHeaderPanel("PLATO",windowTitle="PLATO"),
                         
                         tabPanel("Home",
                                  column(width=5,
                                         imageOutput("image1")        
                                  ),
                                  column(width=6,
                                         p(h3("Person-Level Assessment of Treatment Outcomes"),
                                           p("PLATO is an integrated framework to allow all users to apply the library of predictive models developed to produce individualized risk for all medical interventions and all health outcomes of interest, based on personal demographics, medical history, and health behaviors.")),  
                                         p(a(href="http://www.ohdsi.org/analytic-tools/plato-for-patient-level-prediction/", "More Information"))
                                  )
                                  
                         ),

            #==============================================
            # Check plp develop installed
            #==============================================
            # the data tab should have 3 conditional tabs - data loader/data extractor/data viewer
            shiny::tabPanel("Install",
                     shiny::mainPanel("Install study R packages:",
                                      'Before running this package installer, please shut down any other R connections.',
                                      DT::dataTableOutput("packageList"),

                                      shiny::p('Click to install PatientLevelPrediction R Package and missing dependancies',
                     shiny::actionButton('install', 'Install'))
                         )




                     ),

            
            #========================================================
            #  Data Extraction
            #========================================================
            shiny::tabPanel("Extract Data",
                            
                            shiny::tabsetPanel(id ="dataTabs",
                                               shiny::tabPanel(title = "Connection Settings", value="connection",
                                                               shiny::h4("Connection Settings"),
                                                               shiny::uiOutput("connectSet")),
                                               shiny::tabPanel(title = "Data", value="dataExtract",
                                                               shiny::h4("Connection Status"),
                                                               shiny::tableOutput("connectionStatus"),
                                                               shiny::h4("Extract Model Data"),
                                                               shiny::uiOutput("extractData")),
                                               
                                               #shiny::uiOutput("runAnalysis")
                                               shiny::tabPanel(title = "Cohorts", value="cohorts",
                                                               shiny::h4("Conhorts in database"),
                                                               DT::dataTableOutput("cohorts")))
                            
                            # will add tab for cohort description visulisations here
                            ),
                            
                            
                            
            
            #========================================================
            #  Training settings
            #========================================================
            shiny::tabPanel("Train Model",
                            shiny::uiOutput("dataSettings"),
                                               shiny::uiOutput("popSettings"),
                                               shiny::uiOutput("modelSettings")
                            
                            
                            
                            
            ),

            #========================================================
            #  Send results
            #========================================================
            shiny::tabPanel("Explorer",
                            shiny::uiOutput("visSel"),
                            shiny::tabsetPanel(id ="analysisTabs",
                                               shiny::tabPanel(title = "All Models", value="panel_allmodes",
                                                               shiny::h4("All Models"),
                                                               DT::dataTableOutput("allmodels")),
                                               shiny::tabPanel(title = "Performance", value="panel_permform",
                                                               shiny::h4("Performance Metrics"),
                                                               DT::dataTableOutput("performance")),
                                               shiny::tabPanel(title = "Options", value="panel_options",
                                                               shiny::h4("Options"),
                                                               DT::dataTableOutput("options")),
                                               shiny::tabPanel(title = "Variables", value="panel_varimp",
                                                               shiny::h4("Variable Importance"),
                                                               DT::dataTableOutput("varImp")),
                                               shiny::tabPanel(title = "Attrition", value="panel_attrition",
                                                               shiny::h4("Attrition"),
                                                               DT::dataTableOutput("attrition")),
                                               shiny:: tabPanel(title = "ROC", value="panel_roc",
                                                                shiny::h4("Test"),
                                                                shiny::plotOutput("rocPlot"),
                                                                shiny::h4("Validation"),
                                                                shiny::plotOutput("rocPlotVal"),
                                                                shiny::h4("Train"),
                                                                shiny::plotOutput("rocPlotTrain")),
                                               shiny::tabPanel(title = "Box Plot", value="panel_box",
                                                               shiny::h4("Test"),
                                                               shiny::plotOutput("boxPlot"),
                                                               shiny::h4("Train"),
                                                               shiny::plotOutput("boxPlotTrain")
                                                               ),
                                               shiny::tabPanel(title = "Calibration", value="panel_cal",
                                                               shiny::h4("Test"),
                                                               shiny::plotOutput("calPlot"),
                                                               shiny::h4("Train"),
                                                               shiny::plotOutput("calPlotTrain")),
                                               shiny::tabPanel(title = "Preference", value="panel_pref",
                                                               shiny::h4("Test"),
                                                               shiny::plotOutput("prefPlot"),
                                                               shiny::h4("Train"),
                                                               shiny::plotOutput("prefPlotTrain"))
                            )

            ),
            
            shiny::tabPanel("Apply Models",
                            "Here you can select a model and apply it to",
                                             "predict the outcome using data extracted from a",
                                             "new database (e.g. apply model trained on CDM_JMDC_V5",
                                             "to data extracted from CDM_OPTUM_V5",
                          
                            shiny::uiOutput("apply")
                              
                            
                              
                              
                            
                            
                            
            )


        )
        )
    )
