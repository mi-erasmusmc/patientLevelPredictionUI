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

shinyUI(
  navbarPage("PLATO",
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
    
    tabPanel("Data",
             p("You can either load already extracted PLP data here or generate it below",
               a(href="https://raw.githubusercontent.com/OHDSI/PatientLevelPrediction/master/inst/doc/BuildingPredictiveModels.pdf", "(more Information)")
             ),
             fileInput("plpFile", "Load Data"),
             hr(),
             fluidRow(column(width=6,
                 fileInput("selSQL", "Select SQL to run",accept = c('.sql','txt'))
                      ),
                 column(width=6,
                        actionButton("runSQL", "runSQL")     
                 )
              ),
             
             # ToDo: fill from a query to the CDM
             fluidRow(column(width=6,
                        selectInput("selCohort", label = "Select Cohort", 
                                         choices = list("Choice 1" = 1, "Choice 2" = 2, "Choice 3" = 3), 
                                         selected = 1)
             ),
             column(width=6,
                    selectInput("selOutcome", label = "Select Outcome", 
                                choices = list("Choice 1" = 1, "Choice 2" = 2, "Choice 3" = 3), 
                                selected = 1)
                  )
             ),
             fluidRow(column(width=6,
             checkboxGroupInput("covariates1", "Select covariates",
                                c("Demographics",
                                  "ConditionOccurrence",
                                  "ConditionOccurrence365d",
                                  "ConditionOccurrence30d",
                                  "ConditionOccurrenceInpt180d",
                                  "ConditionEra",
                                  "ConditionEraEver",
                                  "ConditionEraOverlap",
                                  "ConditionGroup",
                                  "DrugExposure",
                                  "DrugExposure365d",
                                  "DrugExposure30d",
                                  "DrugEra",
                                  "DrugEra365d",
                                  "DrugEra30d",
                                  "DrugEraOverlap",
                                  "DrugEraEver",
                                  "DrugGroup",
                                  "ProcedureOccurrence",
                                  "ProcedureOccurrence365d",
                                  "ProcedureOccurrence30d",
                                  "ProcedureGroup")
                                  ,inline = FALSE)),
             column(width=6,
             checkboxGroupInput("covariates2", "",
                                c("Observation",
                                  "Observation365d",
                                  "Observation30d",
                                  "ObservationCount365d",
                                  "Measurement",
                                  "Measurement365d",
                                  "Measurement30d",
                                  "MeasurementCount365d",
                                  "MeasurementBelow",
                                  "MeasurementAbove",
                                  "ConceptCounts",
                                  "RiskScores",
                                  "RiskScoresCharlson",
                                  "RiskScoresDCSI",
                                  "RiskScoresCHADS2",
                                  "RiskScoresCHADS2VASc",
                                  "InteractionYear",
                                  "InteractionMonth")
                                ,inline = FALSE))
             
                ),
              fluidRow(column(width=6,
                             actionButton("extractSata", "Extract Data")   
               
              ),
              column( width=6 
               )
               
             )
             
    ),
    tabPanel("Features"),
    tabPanel("Models"),
    tabPanel("Results",
             fluidRow(
               column(width=6,
                  p("This applicatin is developed in OHDSI",
                        a(href="http://www.ohdsi.org/analytic-tools/plato-for-patient-level-prediction/", "More Information")
                      ),
                      p(
                        a(href="http://forums.ohdsi.org/tags/c/researchers/patientprediction", "Forum"))
                      )
    ),
    fluidRow(
      column(width=9,
             lineChartOutput("mychart")
      ),
      column(width=3,
             sliderInput("sinePhase", "Sine phase", -180, 180, 0, step=10,
                         animate=animationOptions(interval=100, loop=TRUE)),
             sliderInput("sineAmplitude", "Sine amplitude", -2, 2, 1, step=0.1,
                         animate=animationOptions(interval=100, loop=TRUE))
      )         
    )
  ),
  tabPanel("Settings",
     p(h2("Connection Configuration")),
 
     # all inputs for the database connection
     selectInput("dbms", label = "DBMS", 
                 choices = list("postgresql" = 1, "Oracle" = 2), 
                 selected = 1),
     
     br(),
     
     textInput("server", label = "Server", value = "localhost/ohdsi"),
     br(),
     
     textInput("user", label = "User", value = "joe"),
     br(),
     
     textInput("password", label = "Password", value = "supersecret"),
     br(),

     textInput("cdmDatabaseSchema", label = "cdmDatabaseSchema", value = "my_cdm_data"),
     br(),
 
     textInput("cohortsDatabaseSchema", label = "cohortsDatabaseSchema", value = "my_results"),
     br(),
  
     selectInput("cdmVersion", label = "cdmVersion", 
                 choices = list("4" = 1, "5" = 2), 
                 selected = 2)
    
  )
)
)




