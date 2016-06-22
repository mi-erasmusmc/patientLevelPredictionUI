options(fftempdir = "s:/FFtemp")
#library(PatientLevelPrediction)
require(DT)

shiny::shinyServer(function(input, output, session) {
  
  shinyDir <- file.path('S:/plpUI/version1')
  if(!dir.exists(file.path(shinyDir,'Data')))
    dir.create(file.path(shinyDir,'Data'))
  if(!dir.exists(file.path(shinyDir,'Models')))
    dir.create(file.path(shinyDir,'Models'))
  # reactive value to find plpdata
  data <- shiny::reactiveValues(choices=NULL)
  
  
  # add cluster plot here:
  output$image1 <- shiny::renderImage({
    return(list(
      src = "www/plato_large.png",
      width = "100%",
      height = 300,
      contentType = "image/png",
      alt = "Prediction"
    ))
  }, deleteFile = FALSE)
  
  # trying to get install table to update after install:
  packageList <- installed.packages()[,c('Package','Version')]
  deps <- data.frame(Package=c("devtools", "OhdsiRTools","SqlRender",
                               "DatabaseConnector","Cyclops",
                               "OhdsiSharing",
                               "FeatureExtraction",
                               #"CelecoxibPredictiveModels",
                               "PatientLevelPrediction"),
                     requiredVersion = c('Any','Any','>= 1.1.3','>= 1.3.0','>= 1.2.0','Any',
                                         'Any',
                                         #'>= 0.2',
                                         '>= 1.1.1'))
  packageList <- merge(packageList, deps, all.y=T)
  install <- shiny::reactiveValues(packageList = packageList)
  
  output$packageList <- DT::renderDataTable({
    data.frame(install$packageList)
  },     escape = FALSE, selection = 'none',
  options = list(
    pageLength = 25
    #,initComplete = I("function(settings, json) {alert('Done.');}")
  ))
  # add install setiings reactive value
  shiny::observeEvent(input$install, {
    
    # UNSET JAVA_HOME AS TRHIS CAUSES ISSUES WITH RJAVA:
    jh <- Sys.getenv('JAVA_HOME')
    Sys.unsetenv('JAVA_HOME')
    
    progress <- shiny::Progress$new()
    progress$set(message = "Installing Packages...", value = 0)
    # Close the progress when this reactive exits (even if there's an error)
    on.exit(progress$close())
    
    updateProgress <- function(value = NULL, detail = NULL) {
      if (is.null(value)) {
        value <- progress$getValue()
        value <- value + (progress$getMax() - value) / 10
      }
      progress$set(value = value, detail = detail)
    }
    updateProgress(detail='Intalling required packages if missing')
    packageList <-installed.packages()
    if (ifelse(!'devtools'%in%packageList[,'Package'],
               TRUE,
               as.character(packageList[as.character(packageList[,'Package'])=="devtools",'Version'])< '0' )) { install.packages("devtools", dependencies=T) }
    library(devtools)
    pkgs <- c("OhdsiRTools","SqlRender","DatabaseConnector","Cyclops", "OhdsiSharing", "FeatureExtraction")
    version <- data.frame(pkg=pkgs,
                          min=c('0','1.1.3','1.3.0','1.2.0', '0', '0'))
    for (pkg in pkgs) {
      if (ifelse(!pkg%in%packageList[,'Package'],
                 TRUE,
                 as.character(packageList[as.character(packageList[,'Package'])==pkg,'Version'])< as.character(version[version[,'pkg']==pkg,'min'])) )
      {devtools::install_github(paste0('ohdsi/',pkg)) }
    }
    
    if (ifelse(!'PatientLevelPrediction'%in%packageList[,'Package'],
               TRUE,
               as.character(packageList[as.character(packageList[,'Package'])=="PatientLevelPrediction",'Version'])<'1.1.1' ))
    {
      updateProgress(detail='Intalling PatientLevelPrediction develop branch...')
      devtools::install_github("ohdsi/PatientLevelPrediction", ref='develop')
    }
    
    #if (ifelse(!'CelecoxibPredictiveModels'%in%packageList[,'Package'],
    #           TRUE,
    #           as.character(packageList[as.character(packageList[,'Package'])=="CelecoxibPredictiveModels",'Version'])<'0.2' ))
    #{
    #  updateProgress(detail='Intalling CelecoxibPredictiveModels new_plp branch...')
    #  devtools::install_github("ohdsi/StudyProtocols/CelecoxibPredictiveModels", ref='new_plp')
    #}
    
    # reset jave home:
    Sys.setenv(JAVA_HOME= jh)
    
    # update reactive install$packageList
    packageList <- installed.packages()[,c('Package','Version')]
    deps <- data.frame(Package=c("devtools", "OhdsiRTools","SqlRender",
                                 "DatabaseConnector","Cyclops",
                                 "OhdsiSharing",
                                 "FeatureExtraction",
                                 #"CelecoxibPredictiveModels",
                                 "PatientLevelPrediction"),
                       requiredVersion = c('Any','Any','>= 1.1.3','>= 1.3.0','>= 1.2.0','Any',
                                           'Any',
                                           #'>= 0.2',
                                           '>= 1.1.1'))
    install$packageList <- merge(packageList, deps, all.y=T)
    
  })
  #===========================================================================
  
  
  
  
  #================================================================
  #================================================================
  # The analysis
  output$connectSet <-
    shiny::renderUI(
      shiny::wellPanel(
        #shiny::h4("Run analysis"),
        shiny::helpText("Step 1: Set up connection- ",
                        "add the CDM database connection settings ",
                        "this tells the package where to extract the ",
                        'at risk cohort, outcome cohort and sets the user connection settings'),
        #dbconnection,
        shiny::fluidRow(
          shiny::column(6,
                        shiny::textInput("user", "Username:",NULL),
                        
                        shiny::selectInput("dbms", label = "dbms:",
                                           choices = list("Microsoft SQL Server" = 'sql server',
                                                          "MySQL" = 'mysql',
                                                          "Oracle" = "oracle",
                                                          "PostgreSQL" = "postgresql",
                                                          "Amazon Redshift" = "redshift",
                                                          "Microsoft Parallel Data Warehouse (PDW)" = 'pdw',
                                                          "IBM Netezza" = 'netezza' ),
                                           selected = 'pdw'),
                        
                        shiny::textInput("port", "Port:",17001),
                        shiny::selectInput("cdmVersion", label = "cdmVersion",
                                           choices = list("version 5" = '5',
                                                          "version 4" = '4' ),
                                           selected = '5')
                        
          ),
          
          shiny::column(6, 
                        shiny::passwordInput("password", "Password:",NULL),
                        shiny::textInput("server", "Server:",'JRDUSAPSCTL01'),
                        shiny::textInput("domain", "Domain:",NULL),
                        shiny::actionButton('connect', 'Connect')
                        
                        
          ))
        
      ))
  
  extractedData<- shiny::reactiveValues(cohorts =NULL, data=NULL,
                                        connectionDetails=NULL,
                                        conn=NULL, database='CDM_CPRD_V5',
                                        details=NULL)
  
  shiny::observeEvent(input$connect,{
    # when clicked  connect and extract cohort ids
    
    progress <- shiny::Progress$new()
    progress$set(message = "Connecting...", value = 0)
    # Close the progress when this reactive exits (even if there's an error)
    on.exit(progress$close())
    
    updateProgress <- function(value = NULL, detail = NULL) {
      if (is.null(value)) {
        value <- progress$getValue()
        value <- value + (progress$getMax() - value) / 2
      }
      progress$set(value = value, detail = detail)
    }
    
    updateProgress(detail = 'Using user inputs')
    user <- input$user
    pwd <- input$password
    if(is.null(input$user))
      user <- NULL
    if(input$user%in%c('','NULL'))
      user <- NULL
    if(is.null(input$password))
      pwd <- NULL
    if(input$password%in%c('','NULL'))
      pwd <- NULL
    
    connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = input$dbms,
                                                                    server = input$server,
                                                                    user = user,
                                                                    password = pwd,
                                                                    port = input$port)
    extractedData$connectionDetails <- connectionDetails
    conn <- NULL
    conn <- tryCatch(DatabaseConnector::connect(connectionDetails),
                     error = function(e) {writeLines(paste(e))})
    
    updateProgress(detail =ifelse(is.null(conn), 'Error','Done'))
    if(is.null(conn)) return(NULL)
    extractedData$conn <- conn
    extractedData$details=paste('Server:',input$server,' -Port:', input$port)
    
  }
  )
  
  shiny::observeEvent(input$selDatabase,{
    if(is.null(extractedData$conn)) return()
    
    
    progress <- shiny::Progress$new()
    progress$set(message = "Connecting to database", value = 0)
    on.exit(progress$close())
    
    updateProgress <- function(value = NULL, detail = NULL) {
      if (is.null(value)) {
        value <- progress$getValue()
        value <- value + (progress$getMax() - value) / 2
      }
      progress$set(value = value, detail = detail)
    }
    
    extractedData$database = input$database
  
    sql <- SqlRender::renderSql('use @database; select a.cohort_definition_id, 
                                b.cohort_definition_name, b.cohort_definition_description
                                from (select distinct cohort_definition_id from cohort) a left outer join cohort_definition b
                                on a.cohort_definition_id=b.cohort_definition_id;',
                                database = input$database)$sql
    #sql <- SqlRender::renderSql('use @database; select cohort_definition_id, count(*) N, min(cohort_start_date) first, min(cohort_end_date) last from cohort group by cohort_definition_id;',
    #                            database = input$database)$sql
    updateProgress(detail='running sql')
    extractedData$cohorts <- tryCatch(DatabaseConnector::querySql(extractedData$conn, sql),
                                      warning = function(w) {
                                        writeLines(paste(w))
                                      }, error = function(e) {
                                        writeLines(paste(e))
                                        extractedData$cohorts = NULL
                                      })
    updateProgress(detail='Done')
  }
  
    )
  
  output$connectionStatus <- shiny::renderTable(
    
    data.frame('Connection Status'= ifelse(is.null(extractedData$conn),'Not connected','Connected'),
               'Details'= ifelse(is.null(extractedData$conn),
                                 'Use connection settingsx tab to set up',
                                 #paste0("Use <a href='#connection'> connection settings </a> tab to set up"),
                                 extractedData$details))
    #,
    #escape = FALSE, selection = 'none',
    #options = list(
    # pageLength = 2)
  )
  
  output$extractData <-
    shiny::renderUI(
      shiny::wellPanel(
        shiny::helpText("Step 2: extract data- ",
                        "pick the at risk/outcome cohorts, ",
                        "and the covariates you want ",
                        '(next pick the population)'),
        
        shiny::inputPanel(
          shiny::selectInput("database", label = "Database",
                             choices = list('CPRD'= "CDM_CPRD_V423" ,
                                            'TRUVEN CCAE'="CDM_TRUVEN_CCAE_V382",#418",
                                            'TRUVEN MDCR'="CDM_TRUVEN_MDCR_V415" ,
                                            'TRUVEN MDCD'="CDM_TRUVEN_MDCD_V417" ,
                                            'JMDC'="CDM_JMDC_V429" ,
                                            'OPTUM'="CDM_OPTUM_V379"),
                             selected = extractedData$database),
          shiny::actionButton('selDatabase', 'Select')
        ),
        
        shiny::inputPanel(
          shiny::selectInput("cohortId", label = "At Risk Cohort:",
                             choices = extractedData$cohorts[,'COHORT_DEFINITION_ID'],
                             selected = 1),
          shiny::selectInput("outcomeId", label = "Outcome Cohort:",
                             choices = extractedData$cohorts[,'COHORT_DEFINITION_ID'],
                             selected = 2)
        )
        
        
        ,
        shiny::fluidPage(
          
          shiny::checkboxInput('all', 'Use All Default Features',value = T),
          shiny::conditionalPanel(condition = 'input.all == false',
                                  
                                  
                                  
                                  shiny::column(3,
                                                shiny::checkboxInput('demo', 'Use Demographics',value = F),
                                                shiny::conditionalPanel(condition = 'input.demo == true',
                                                                        shiny::inputPanel(
                                                                          shiny::checkboxInput('demo_age', 'Use Age groups',value = T),
                                                                          shiny::checkboxInput('demo_gender', 'Use Gender', value=T),
                                                                          shiny::checkboxInput('demo_ethnicity', 'Use Ethnicity', value=T),
                                                                          shiny::checkboxInput('demo_race', 'Use Race', value=T),
                                                                          shiny::checkboxInput('demo_month', 'Use index month', value=F),
                                                                          shiny::checkboxInput('demo_year', 'Use index year',value=F))
                                                )),
                                  shiny::column(3,
                                                shiny::checkboxInput('conditions', 'Use Conditions', value = F),
                                                shiny::conditionalPanel(condition = 'input.conditions == true',
                                                                        shiny::checkboxInput('conditions_group', 'Use Conditions Group',value = FALSE)
                                                ),
                                                shiny::conditionalPanel(condition = 'input.conditions_group == true',
                                                                        shiny::inputPanel(
                                                                          shiny::checkboxInput('conditions_group_meddra', 'Use Meddra'),
                                                                          shiny::checkboxInput('conditions_group_snomed', 'Use Snomed')
                                                                        )),
                                                shiny::conditionalPanel(condition = 'input.conditions==true',
                                                                        shiny::checkboxInput('conditions_occur', 'Use Conditions Occurrence',value = F)
                                                ),
                                                shiny::conditionalPanel(condition = 'input.conditions_occur== true',
                                                                        shiny::inputPanel(
                                                                          shiny::checkboxInput('conditions_occur_30', 'Use Conditions Occurrence 30 days', value = F),
                                                                          shiny::checkboxInput('conditions_occur_year', 'Use Conditions Occurrence 1 year', value = F))
                                                ),
                                                shiny::conditionalPanel(condition = 'input.conditions == true',
                                                                        shiny::checkboxInput('conditions_era', 'Use Conditions Era',value = FALSE)
                                                ),
                                                shiny::conditionalPanel(condition = 'input.conditions_era == true',
                                                                        shiny::inputPanel(
                                                                          shiny::checkboxInput('conditions_era_overlap', 'Use Conditions Era overlap', value = F),
                                                                          shiny::checkboxInput('conditions_era_all', 'Use Conditions Era anytime', value = F)
                                                                        ))),
                                  shiny::column(3,
                                                shiny::checkboxInput('usedrugs', 'Use Drugs', value = F),
                                                shiny::conditionalPanel(condition = 'input.usedrugs == true',
                                                                        shiny::checkboxInput('drugs_group', 'Use Drug Group',value = FALSE)
                                                ),
                                                shiny::conditionalPanel(condition = 'input.usedrugs==true',
                                                                        shiny::checkboxInput('drugs_occur', 'Use Drug Exposure',value = F)
                                                ),
                                                shiny::conditionalPanel(condition = 'input.drugs_occur== true',
                                                                        shiny::inputPanel(
                                                                          shiny::checkboxInput('drugs_occur_30', 'Use Drug Exposure 30 days', value = F),
                                                                          shiny::checkboxInput('drugs_occur_year', 'Use Drug Exposure 1 year', value = F)))
                                                ,
                                                shiny::conditionalPanel(condition = 'input.usedrugs == true',
                                                                        shiny::checkboxInput('drugs_era', 'Use Drug Era',value = FALSE)
                                                ),
                                                shiny::conditionalPanel(condition = 'input.drugs_era == true',
                                                                        shiny::inputPanel(
                                                                          shiny::checkboxInput('drugs_era_overlap', 'Use Drug Era overlap', value=F),
                                                                          shiny::checkboxInput('drugs_era_all', 'Use Drug Era anytime', value = F)
                                                                        ))
                                  ),
                                  
                                  shiny::column(3,
                                                shiny::checkboxInput('usepro', 'Use Procedures', value = F),
                                                shiny::conditionalPanel(condition = 'input.usepro== true',
                                                                        shiny::inputPanel(
                                                                          shiny::checkboxInput('pro_month', 'Use Procedure 30 days', value = F),
                                                                          shiny::checkboxInput('pro_year', 'Use Procedure 1 year', value = F),
                                                                          shiny::checkboxInput('pro_group', 'Use Procedure Group', value = F)
                                                                        )),
                                                shiny::checkboxInput('useobs', 'Use Observation', value = F),
                                                shiny::conditionalPanel(condition = 'input.useobs== true',
                                                                        shiny::inputPanel(
                                                                          shiny::checkboxInput('obs_month', 'Use Observation 30 days', value = F),
                                                                          shiny::checkboxInput('obs_year', 'Use Observation 1 year', value = F),
                                                                          shiny::checkboxInput('obs_year_count', 'Use Observation year count', value = F)
                                                                        )),
                                                shiny::checkboxInput('usemeas', 'Use Measurements', value = F),
                                                shiny::conditionalPanel(condition = 'input.usemeas== true',
                                                                        shiny::inputPanel(
                                                                          shiny::checkboxInput('meas_month', 'Use Measurement 30 days', value = F),
                                                                          shiny::checkboxInput('meas_year', 'Use Measurement 1 year', value = F),
                                                                          shiny::checkboxInput('meas_year_count', 'Use Measurement year count', value = F),
                                                                          shiny::checkboxInput('meas_above', 'Use Measurement above', value = F),
                                                                          shiny::checkboxInput('meas_below', 'Use Measurement below', value = F)
                                                                        )),
                                                
                                                shiny::checkboxInput('userisk', 'Use Risk Scores', value = F),
                                                shiny::conditionalPanel(condition = 'input.userisk== true',
                                                                        shiny::inputPanel(
                                                                          shiny::checkboxInput('risk_charlson', 'Charlson', value = F),
                                                                          shiny::checkboxInput('risk_dsci', 'DSCI', value = F),
                                                                          shiny::checkboxInput('risk_chads2', 'CHADS2', value = F),
                                                                          shiny::checkboxInput('risk_chadsvasc', 'CHADS2VASc', value = F)
                                                                        ))
                                  ))
          
        ),
        shiny::inputPanel(
          shiny::helpText("Give the reference to the data for selecting later on:"),
          shiny::textInput('ref', 'Data Reference Name:', value=NULL, width='100%'),
          shiny::actionButton("extractData", "Extract Data")
        )
        
        
        
        
        
      ))
  
  ## observer extract data and extrac the data into the data folder:
  shiny::observeEvent(input$extractData,{
    
    progress <- shiny::Progress$new()
    progress$set(message = "Extracting data...", value = 0)
    on.exit(progress$close())
    
    updateProgress <- function(value = NULL, detail = NULL) {
      if (is.null(value)) {
        value <- progress$getValue()
        value <- value + (progress$getMax() - value) / 4
      }
      progress$set(value = value, detail = detail)
    }
    
    
    # when clicked  connect and extract cohort ids
    if(input$all){
      updateProgress(detail='constructing...')
      covSettings <- FeatureExtraction::createCovariateSettings(useCovariateDemographics = T,
                                                                useCovariateDemographicsGender = T,
                                                                useCovariateDemographicsRace = T,
                                                                useCovariateDemographicsEthnicity = T,
                                                                useCovariateDemographicsAge = T, 
                                                                useCovariateDemographicsYear = T,
                                                                useCovariateDemographicsMonth = T,
                                                                
                                                                useCovariateConditionOccurrence = T,
                                                                useCovariateConditionOccurrence365d = T,
                                                                useCovariateConditionOccurrence30d = T,
                                                                useCovariateConditionOccurrenceInpt180d =T,
                                                                useCovariateConditionEra = T, 
                                                                useCovariateConditionEraEver = T,
                                                                useCovariateConditionEraOverlap = T,
                                                                useCovariateConditionGroup = T,
                                                                useCovariateConditionGroupMeddra = T,
                                                                useCovariateConditionGroupSnomed = T,
                                                                
                                                                useCovariateDrugExposure = T, 
                                                                useCovariateDrugExposure365d = T,
                                                                useCovariateDrugExposure30d = T, 
                                                                useCovariateDrugEra = T,
                                                                useCovariateDrugEra365d = T, 
                                                                useCovariateDrugEra30d = T,
                                                                useCovariateDrugEraOverlap = T, 
                                                                useCovariateDrugEraEver = T,
                                                                useCovariateDrugGroup = T, 
                                                                
                                                                useCovariateProcedureOccurrence = T,
                                                                useCovariateProcedureOccurrence365d = T,
                                                                useCovariateProcedureOccurrence30d = T,
                                                                useCovariateProcedureGroup = T, 
                                                                useCovariateObservation = T,
                                                                useCovariateObservation365d = T, 
                                                                useCovariateObservation30d = T,
                                                                useCovariateObservationCount365d = T, 
                                                                useCovariateMeasurement = T,
                                                                useCovariateMeasurement365d = T, 
                                                                useCovariateMeasurement30d = T,
                                                                useCovariateMeasurementCount365d = T,
                                                                useCovariateMeasurementBelow = T,
                                                                useCovariateMeasurementAbove = T, 
                                                                useCovariateRiskScores = T, 
                                                                useCovariateRiskScoresCharlson = T,
                                                                useCovariateRiskScoresDCSI = T, 
                                                                useCovariateRiskScoresCHADS2 = T,
                                                                useCovariateRiskScoresCHADS2VASc = T,
                                                                excludedCovariateConceptIds = c(), includedCovariateConceptIds = c(),
                                                                deleteCovariatesSmallCount = 50)
    } else {
      updateProgress(detail='constructing...')
      covSettings <- FeatureExtraction::createCovariateSettings(useCovariateDemographics = input$demo,
                                                                useCovariateDemographicsGender = input$demo_gender,
                                                                useCovariateDemographicsRace = input$demo_race,
                                                                useCovariateDemographicsEthnicity = input$demo_ethnicity,
                                                                useCovariateDemographicsAge = input$demo_age, 
                                                                useCovariateDemographicsYear = input$demo_year,
                                                                useCovariateDemographicsMonth = input$demo_month,
                                                                
                                                                useCovariateConditionOccurrence = input$conditions,
                                                                useCovariateConditionOccurrence365d = input$conditions_occur_year,
                                                                useCovariateConditionOccurrence30d = input$conditions_occur_30,
                                                                useCovariateConditionOccurrenceInpt180d = FALSE,
                                                                useCovariateConditionEra = input$conditions_era, 
                                                                useCovariateConditionEraEver = input$conditions_era_all,
                                                                useCovariateConditionEraOverlap = input$conditions_era_overlap,
                                                                useCovariateConditionGroup = input$conditions_group,
                                                                useCovariateConditionGroupMeddra = input$conditions_group_meddra,
                                                                useCovariateConditionGroupSnomed = input$conditions_group_snomed,
                                                                
                                                                useCovariateDrugExposure = input$drugs_occur, 
                                                                useCovariateDrugExposure365d = input$drugs_occur_year,
                                                                useCovariateDrugExposure30d = input$drugs_occur_30, 
                                                                useCovariateDrugEra = input$drugs_era,
                                                                useCovariateDrugEra365d = input$drugs_era_year, 
                                                                useCovariateDrugEra30d = input$drugs_era_30,
                                                                useCovariateDrugEraOverlap = input$drugs_era_overlap, 
                                                                useCovariateDrugEraEver = input$drugs_era_all,
                                                                useCovariateDrugGroup = input$drugs_group, 
                                                                
                                                                useCovariateProcedureOccurrence = input$usepro,
                                                                useCovariateProcedureOccurrence365d = input$pro_year,
                                                                useCovariateProcedureOccurrence30d = input$pro_30,
                                                                useCovariateProcedureGroup = input$pro_group, 
                                                                useCovariateObservation = input$useobs,
                                                                useCovariateObservation365d = input$obs_year, 
                                                                useCovariateObservation30d = input$obs_30,
                                                                useCovariateObservationCount365d = input$obs_year_count, 
                                                                useCovariateMeasurement = input$usemeas,
                                                                useCovariateMeasurement365d = input$meas_year, 
                                                                useCovariateMeasurement30d = input$meas_30,
                                                                useCovariateMeasurementCount365d = input$meas_year_count,
                                                                useCovariateMeasurementBelow = input$meas_below,
                                                                useCovariateMeasurementAbove = input$meas_above, 
                                                                useCovariateRiskScores = input$userisk, 
                                                                useCovariateRiskScoresCharlson = input$risk_charlson,
                                                                useCovariateRiskScoresDCSI = input$risk_dcsi, 
                                                                useCovariateRiskScoresCHADS2 = input$risk_chads2,
                                                                useCovariateRiskScoresCHADS2VASc = input$risk_chadsvasc,
                                                                excludedCovariateConceptIds = c(), includedCovariateConceptIds = c(),
                                                                deleteCovariatesSmallCount = 50)
    }
    updateProgress(detail='extracting...')
    extractedData$data <- tryCatch(PatientLevelPrediction::getDbPlpData(connectionDetails=extractedData$connectionDetails, 
                                                                        cdmDatabaseSchema=paste(input$database,'.dbo'),
                                                                        cohortId=input$cohortId, outcomeIds=input$outcomeId, 
                                                                        cohortDatabaseSchema = paste(input$database,'.dbo'),
                                                                        cohortTable = 'cohort',
                                                                        outcomeDatabaseSchema = paste(input$database,'.dbo'),
                                                                        outcomeTable = 'cohort',
                                                                        cdmVersion=5,
                                                                        washoutPeriod=0,
                                                                        covariateSettings=covSettings),
                                   warning = function(w) {writeLines(paste(w))},
                                   error = function(e) {writeLines(paste(e))}
    )
    
    if(class(extractedData$data)%in%'plpData'){
      updateProgress(detail='saving...')
      ref <- paste(input$database,input$cohortId,input$outcomeId, sep='_')
      if(!is.null(input$ref))
        ref <- input$ref
      PatientLevelPrediction::savePlpData(extractedData$data, file=file.path(shinyDir,'Data', ref)  )
    }
    
    updateProgress(detail='Done...')
    
  }
  
  )
  
  
  
  #### extracted data:
  # extracted data  output:
  output$cohorts <- DT::renderDataTable({
    if (is.null(extractedData$cohorts)) return()
    
    data.frame(extractedData$cohorts)
  },     escape = FALSE, selection = 'none',
  options = list(
    pageLength = 25
    #,initComplete = I("function(settings, json) {alert('Done.');}")
  ))
  
  
  #==========================================
  #==========================================
  
  # load data in Data folder:
  train <- reactiveValues(dataFold=list.dirs(file.path(shinyDir, 'Data'), recursive = F, full.names = F), 
                          modelFold=list.dirs(file.path(shinyDir, 'Models'), recursive = F, full.names = F))
  
  shiny::observeEvent(input$refreshFile,{
    train$dataFold <- list.dirs(file.path(shinyDir, 'Data'), recursive = F, full.names = F)
    train$modelFold <- list.dirs(file.path(shinyDir, 'Models'), recursive = F, full.names = F)
  })
  
  
  shiny::observeEvent(input$loadData_train, {
    if(is.null(input$train_data)) return(NULL)
    
    #writeLines(input$train_data)
    extractedData$data <- tryCatch(PatientLevelPrediction::loadPlpData(file.path(shinyDir, 'Data', input$train_data))
                                   , error = function(e) writeLines(e)              
    )
  })
  
  output$dataSettings <- shiny::renderUI(
    shiny::wellPanel(
      shiny::h4('Pick Training Data:'),
      shiny::selectInput("train_data", label = "Data:",
                         choices = train$dataFold,
                         selected = 1),
      shiny::actionButton('loadData_train', 'Load Data'), shiny::actionButton('refreshFile','Refresh Data',icon = icon("refresh"))
    )
  )
  
  output$popSettings <- shiny::renderUI(
    shiny::wellPanel(
      shiny::h4('Population Settings:'),
      
      #shiny::inputPanel(
      shiny::selectInput("outcomeId", label = "Outcome ID to predict:",
                         choices = extractedData$data$metaData$call$outcomeIds,
                         selected = 1)
      #)
      ,
      
      shiny::fluidRow(
        shiny::column(8, 
                      shiny::h4('Outcome filters'),
                      
                      shiny::checkboxInput("firstExposure", "Use first exposure only:", value = T),
                      shiny::checkboxInput("removePrior", "Remove people with outcome prior to exposure:", value=F),
                      shiny::conditionalPanel(condition = 'input.removePrior== true',
                                              shiny::sliderInput("priorLook", 'Min days at risk:', 1, 9999, value=365)
                      ),
                      shiny::h4('Prior observation'),
                      shiny::sliderInput("washoutPeriod", "Remove people with less than this number days observation prior to index:", min=0,max=9999, value=365)
                      
                      
        ),
        shiny::column(8, 
                      shiny::h4('Risk window'),
                      shiny::sliderInput("riskWindowStart", "Predict from this many days after index:", min=0,max=9999, value=0),
                      shiny::checkboxInput("addExposureStart", "Add exposure to start:", value=F),
                      
                      shiny::sliderInput("riskWindowEnd", "Predict up to this many days after index:", min=0,max=9999, value=365),
                      shiny::checkboxInput("addExposureEnd", "Add exposure to end:", value=F),
                      
                      
                      shiny::checkboxInput("timeAtRisk", "Require time at risk:", value=F),
                      shiny::conditionalPanel(condition = 'input.timeAtRisk== true',
                                              shiny::sliderInput("minTimeAtRisk", 'Min cohort length (days):', 1, 9999, value=30)
                      )
                      
        )
      )
    )
  )
  
  
  output$modelSettings <- shiny::renderUI(
    shiny::wellPanel(
      shiny::h4('Model Settings:'),
      
      shiny::selectInput("modelType", label = "Classifier to train:",
                         choices = c('Regularised Logistic Regression'='lr_lasso',
                                     'Gradient Boosting Machine (XGBoost)'='gbm_xgboost',
                                     #'Gradient Boosting Machine (h2o)'='gbm_plp',
                                     'Random Forest (python)'='python_rf',
                                     #'Random Forest (h2o)'='randomForest_plp',
                                     'KNN'='knn_plp',
                                     'Naive Bayes (python)'='python_nb'
                         ),
                         selected = 1),
      
      shiny::conditionalPanel("input.modelType == 'lr_lasso'",
                              shiny::inputPanel(
                                shiny::sliderInput('val','Starting variance:', min=0.001, max=5, value=0.01)
                                
                              )
      ),
      shiny::conditionalPanel("input.modelType == 'gbm_plp'",
                              shiny::inputPanel(
                                shiny::sliderInput('ntrees','Number of trees:', min=1, max=5000, value=100),
                                shiny::sliderInput('max_depth','Depth of tree:', min=1, max=20, value=4),
                                shiny::sliderInput('learn_rate','Learning rate:', min=0.1, max=0.9, value=0.1),
                                shiny::checkboxInput('bal', 'Balance Classes:', value=F),
                                shiny::sliderInput('rsampRate','Fraction of rows per tree:', min=0.001, max=1, value=0.9),
                                shiny::sliderInput('csampRate','Fraction of features per tree:', min=0.001, max=1, value=1),
                                shiny::sliderInput('nbins','Bins to split categorical data:', min=1, max=100, value=20),
                                shiny::sliderInput('min_rows','Min rows at each end node:', min=1, max=5000, value=2)
                                
                              )
      ),
      
      shiny::conditionalPanel("input.modelType == 'gbm_xgboost'",
                              shiny::inputPanel(
                                shiny::sliderInput('ntrees','Number of trees:', min=1, max=5000, value=100),
                                shiny::sliderInput('max_depth','Depth of tree:', min=1, max=20, value=4),
                                shiny::sliderInput('learn_rate','Learning rate:', min=0.1, max=0.9, value=0.1),
                                shiny::sliderInput('nthread','Number of computer threads:', min=1, max=100, value=20),
                                shiny::sliderInput('min_rows','Min rows at each end node:', min=1, max=5000, value=2)
                                
                              )
      ),
      
      shiny::conditionalPanel("input.modelType == 'randomForest_plp'",
                              shiny::inputPanel(
                                shiny::sliderInput('ntrees','Number of trees:', min=1, max=5000, value=500),
                                shiny::sliderInput('mtries','Number features per tree (-1 is square root of total):', min=-1, max=10000, value=-1),
                                
                                shiny::sliderInput('max_depth','Depth of tree:', min=1, max=40, value=17),
                                shiny::checkboxInput('bal', 'Balance Classes:', value=F),
                                shiny::sliderInput('sample_rate','Fraction of rows per tree:', min=0.001, max=1, value=0.9),
                                shiny::sliderInput('nbins','Bins to split categorical data:', min=1, max=100, value=20),
                                shiny::sliderInput('min_rows','Min rows at each end node:', min=1, max=5000, value=2)
                                
                              )
      ),
      
      
      shiny::conditionalPanel("input.modelType == 'python_rf'",
                              shiny::inputPanel(
                                shiny::sliderInput('ntrees','Number of trees:', min=1, max=5000, value=500),
                                shiny::sliderInput('mtries','Number features per tree (-1 is square root of total):', min=-1, max=10000, value=-1),
                                
                                shiny::sliderInput('max_depth','Depth of tree:', min=1, max=40, value=17),
                                shiny::checkboxInput('varImp', 'Variable importance:', value=T)
                                
                              )
      ),
      
      
      shiny::conditionalPanel("input.modelType == 'knn_plp'",
                              shiny::inputPanel(
                                shiny::sliderInput('k','Number of neighbours:', min=1, max=5000, value=1000)
                              )
      ),
      shiny::selectInput("testSplit", label = "Validation split:",
                         choices = c('Time'='time',
                                     'Person'='person'
                         ),
                         selected = 1),
      shiny::sliderInput('testFraction', 'Validation fraction', min=0.1, max=0.9, value=0.25),
      shiny::sliderInput('nfold', 'Number of cv folds', min=1, max=20, value=2),
      
      shiny::textInput('modelName','Model Reference:', value='model 1'),
      shiny::textInput('cohortName','Cohort description:', value='cohort of ...'),
      shiny::textInput('outcomeName','Outcome description:', value='outcome of ...'),
      
      shiny::actionButton('trainModel','Train Model')
    )
    
  )
  
  shiny::observeEvent(input$trainModel,{
    if(is.null(input$train_data)) return(NULL)
    
    
    # load the data:
    data <- NULL
    data <- tryCatch(PatientLevelPrediction::loadPlpData(file.path(shinyDir,'Data',input$train_data)),
                     warning = function(w) writeLines(paste(w)), 
                     error = function(e) writeLines(paste(e)) 
    )
    if(is.null(data)) return(NULL)
    
    # convert the data if needed:
    if(length(grep('python', input$modelType))>0){
      if(!file.exists(file.path(shinyDir,'Data',paste0(input$train_data,'_lsvm')))){
        data <- PatientLevelPrediction::convertToLibsvm(plpData=data, filePath = file.path(shinyDir,'Data',paste0(input$train_data,'_lsvm'),'files'))
        PatientLevelPrediction::savePlpData(data, file=file.path(shinyDir,'Data',paste0(input$train_data,'_lsvm')))
      } 
        data <- tryCatch(PatientLevelPrediction::loadPlpData(file.path(shinyDir,'Data',paste0(input$train_data,'_lsvm'))),
                         warning = function(w) writeLines(paste(w)), 
                         error = function(e) writeLines(paste(e))) 
      
    }
    
    # create the population (need parameter checks)
    population <- NULL
    
    population <- tryCatch(PatientLevelPrediction::createStudyPopulation(
      plpData=data, population = NULL, outcomeId=input$outcomeId, binary = T,
      firstExposureOnly = input$firstExposure, 
      washoutPeriod = input$washoutPeriod,
      removeSubjectsWithPriorOutcome = input$removePrior, 
      priorOutcomeLookback = input$priorLook,
      requireTimeAtRisk = input$timeAtRisk, 
      minTimeAtRisk = input$minTimeAtRisk, 
      riskWindowStart = input$riskWindowStart,
      addExposureDaysToStart = input$addExposureStart, 
      riskWindowEnd = input$riskWindowEnd,
      addExposureDaysToEnd = input$addExposureEnd
    ),
    warning = function(w) writeLines(paste(w)), 
    error = function(e) writeLines(paste(e) )
    )
    
    if(is.null(population)) return(NULL)
    
    if(input$modelType=='gbm_xgboost'){
      modelSet <- PatientLevelPrediction::GBMclassifier_xgboost(ntrees=input$ntrees,
                                                                nthread=input$nthread,
                                                                max_depth=input$max_depth,
                                                                min_rows=input$min_rows,
                                                                learn_rate=input$learn_rate)
    }
    if(input$modelType=='lr_lasso'){
      modelSet <- PatientLevelPrediction::logisticRegressionModel(variance=input$val)
    }
    
    if(input$modelType=='python_rf'){
      modelSet <- PatientLevelPrediction::RFclassifier_python(ntrees=input$ntrees,
                                                              mtries=input$mtries,
                                                              max_depth=input$max_depth,
                                                              varImp=input$varImp
                                                              
      )
    }
    
    if(input$modelType=='python_nb'){
      modelSet <- PatientLevelPrediction::NBclassifier_python()
    }
    
    if(input$modelType=='knn_plp'){
      if(!dir.exists(file.path(shinyDir, 'knn_temp')))
        dir.create(file.path(shinyDir, 'knn_temp'))
      modelSet <- PatientLevelPrediction::KNNclassifier(k=input$k,indexFolder=file.path(shinyDir, 'knn_temp'))
    }
    
    model <- PatientLevelPrediction::developModel(
      population=population, plpData=data, featureSettings = NULL, modelSettings=modelSet,
      testSplit = input$testSplit, testFraction = as.double(input$testFraction), 
      nfold = as.double(input$nfold), indexes = NULL,
      dirPath = file.path(shinyDir,'Models'), silent = F, log = NULL,
      analysisId=input$modelName
    )
    
    # now save analysis.txt file in Models
    vals <- data.frame("COHORT_DEFINITION_ID"=data$metaData$call$cohortId,
                       "N_EXPOSURE"=nrow(population),  
                       "COHORT_NAME"=input$cohortName, 
                       "OUTCOME_ID"=as.double(input$outcomeId), 
                       "N_OUTCOME"=sum(population$outcomeCount>0),
                       "OUTCOME_NAME"=input$outcomeName
    )
    
    file_exists <- file.exists(file.path(shinyDir, 'Models/analysis.txt'))
    write.table(vals,file.path(shinyDir, 'Models/analysis.txt'), append=ifelse(file_exists,T,F ), col.names=ifelse(file_exists,F,T ), row.names = F)
    
    
  })
  
  
  #==========================================
  #==========================================
  
  initsum<- NULL#PatientLevelPrediction::createAnalysisSummary(file.path(shinyDir,'Results'), save=F)
  summary <- shiny::reactiveValues(data=initsum
                                   , choices=NULL)#as.list(1:nrow(initsum)))
  
  
  output$selectData <-
    shiny::renderUI(
      shiny::wellPanel(
        shiny::h4("Load exisitng results"),
        shiny::helpText("Select the folder containing existing results ",
                        #dbconnection,
                        #shiny::textInput("dataFolder", "Folder path:",NULL),
                        shiny::actionButton('refreshFile','Refresh Data',icon = icon("refresh"))
        ))
      
    )
  
  
  shiny::observeEvent(input$refreshModels, {
    
    summary$data <- tryCatch(PatientLevelPrediction::createAnalysisSummary(file.path(shinyDir,'Models'), save=F)
                             ,
                             error=function(cond) {
                               message(paste("Chosen directory does not seem to exist:", file.path(shinyDir,'Models')))
                               message("Here's the original error message:")
                               message(cond)
                               return(NULL)
                             },
                             warning=function(cond) {
                               message(paste("Chosen directory caused a warning:", file.path(shinyDir,'Models')))
                               message("Here's the original warning message:")
                               message(cond)
                               return(NULL)
                             }  
    )
    if(is.null(summary$data)) return()
    colnames(summary$data) <- gsub('_DEFINITION','',colnames(summary$data))
    
    summary$choice <- as.list(1:nrow(summary$data))
    
    cohortIds <- paste0('Cohort: ',summary$data[,colnames(summary$data)%in%c('COHORT_ID','COHORT_DEFINITION_ID')])
    cohortNames <- paste0('(',summary$data[,colnames(summary$data)%in%c('COHORT_NAME')],')')
    
    outcomeIds <- paste0('-- Outcome: ', summary$data[,colnames(summary$data)%in%c('OUTCOME_ID','outcomeID')])
    outcomeNames <- paste0('(',summary$data[,colnames(summary$data)%in%c('OUTCOME_NAME')],')')
    
    database <- paste0('-- Training Database: ', summary$data[,colnames(summary$data)%in%c('database')])
    
    if(sum(colnames(summary$data)%in%c('OUTCOME_NAME'))>0)
      names(summary$choice) <- paste(cohortIds, cohortNames, outcomeIds, outcomeNames,database ,sep='')
    if(sum(colnames(summary$data)%in%c('OUTCOME_NAME'))==0)
      names(summary$choice) <- paste(cohortIds, cohortNames, outcomeIds,database ,sep='')
    
    
    # find plpData in directory
    plpData <- list.dirs(file.path(shinyDir,'Data'), recursive = F)
    if(length(plpData)>0){
      data$choice <- as.list(plpData)#1:length(plpData))
      names(data$choice) <- plpData
    }
    
  })
  
  
  # summary output:
  output$summary <- DT::renderDataTable({
    if (is.null(summary$data)) return()
    
    colnames(summary$data) <- gsub('_DEFINITION','',colnames(summary$data))
    data.frame(summary$data[,colnames(summary$data)%in%c('COHORT_ID', 'COHORT_NAME','OUTCOME_ID','outcomeId','OUTCOME_NAME','database','auc')])
  },     escape = FALSE, selection = 'none',
  options = list(
    pageLength = 25
    #,initComplete = I("function(settings, json) {alert('Done.');}")
  ))
  
  #==================================
  # add the summary$data cohortId and outcomeId columns into a slection form
  # on the
  
  output$visSel <- shiny::renderUI(
    shiny::wellPanel(
      shiny::selectInput("explorerIds", label = "Select model:",
                         choices = summary$choice,
                         selected = 1),
      shiny::actionButton("explore", "Select"),
      shiny::actionButton('refreshModels','Refresh Models',icon = icon("refresh"))#,
      #DT::renderDataTable("summary")
      
    )
  )
  
  #############==================================
  # ALL MODELS
  output$allmodels <- DT::renderDataTable({
    # load the text files in the results folder - only show complete results
    analysis <- read.table(file.path(shinyDir,'Models','analysis.txt'), header=T) # "COHORT_DEFINITION_ID" "N_EXPOSURE" "COHORT_NAME" "OUTCOME_ID" "N_OUTCOME" "OUTCOME_NAME"
    model <- read.table(file.path(shinyDir,'Models','modelInfo.txt'), header=T) #"modelId" "database" "cohortId" "outcomeId" "model" "splitOn" "modelLoc" "populationLoc" "parameters" "modelTime"
    
    res <- merge(analysis[,c("COHORT_DEFINITION_ID", "COHORT_NAME", "OUTCOME_ID", "OUTCOME_NAME")],
                 model[,c('modelId','database','splitOn','model','parameters', "cohortId", "outcomeId")],
                 by.x=c("COHORT_DEFINITION_ID", "OUTCOME_ID"), by.y=c("cohortId", "outcomeId"))
    
    unique(res[,c('modelId',"COHORT_NAME","OUTCOME_NAME", 'database','splitOn','model','parameters')])
    
  },     escape = FALSE, selection = 'none',
  options = list(
    pageLength = 25
    #,initComplete = I("function(settings, json) {alert('Done.');}")
  ))
  
  #############==================================
  
  
  #=========================================================
  # PLOTS
  priorResult <- list(dataFolder=file.path(shinyDir,'Models'))
  
  output$performance <- DT::renderDataTable({
    if (is.null(summary$data)) return()
    if(is.null(input$explorerIds)){
      id <- 1
    } else{
      id <- input$explorerIds
    }
    test <- summary$data[id,colnames(summary$data)%in%c('auc', 'auc_lb95ci',
                                                        'auc_lb95ci.1', 'Brier',
                                                        'BrierScaled','Xsquared',
                                                        'df',	'pvalue',
                                                        'calibrationIntercept',
                                                        'calibrationGradient',
                                                        'preference3070_0',
                                                        'preference3070_1')]
    
    train <- summary$data[id,colnames(summary$data)%in%paste0('train_',c('auc', 'auc_lb95ci',
                                                                         'auc_lb95ci.1', 'Brier',
                                                                         'BrierScaled','Xsquared',
                                                                         'df',	'pvalue',
                                                                         'calibrationIntercept',
                                                                         'calibrationGradient',
                                                                         'preference3070_0',
                                                                         'preference3070_1'))]
    colnames(train) <- gsub('train_','', colnames(train))
    #rwnames <- colnames(test)
    returnTab <- data.frame(cbind(t(test), t(train)))
    colnames(returnTab) <- c('Test', 'Train')
    
    # add external data results here
    modelId <- summary$data[id, 'modelId']
    
    folder <- list.dirs(path = file.path(priorResult$dataFolder,modelId), 
                        full.names = TRUE, recursive = TRUE)
    folder <- gsub(paste0(priorResult$dataFolder,'/',modelId,'/'),'',folder)
    val <- folder[!folder%in%c('','savedModel','train','test')]
    if(length(val)>0 && file.exists(file.path(priorResult$dataFolder, 'performanceInfoVal.txt'))){
      #find modelDetails:
      performanceVal <- read.table(file.path(priorResult$dataFolder, 'performanceInfoVal.txt'), header=T)
      validationRes <- performanceVal[performanceVal$performanceID%in%val,]
      valNames <- validationRes$database
      validationRes <- validationRes[, !colnames(validationRes)%in%c("performanceID", "database", "modelId")]
      rownames(validationRes) <- valNames
      returnTab <- cbind(returnTab, t(validationRes))
    }
    
    returnTab
    
  },     escape = FALSE, selection = 'none',
  options = list(
    pageLength = 25
    #,initComplete = I("function(settings, json) {alert('Done.');}")
  ))
  
  output$varImp <- DT::renderDataTable({
    if (is.null(summary$data)) return()
    if(is.null(input$explorerIds)){
      id <- 1
    } else{
      id <- input$explorerIds
    }
    id <- summary$data[id, colnames(summary$data)=='modelId']
    model <- PatientLevelPrediction::loadPlpModel(file.path(priorResult$dataFolder, id, 'savedModel' ))
    data.frame(model$varImp)
  },     escape = FALSE, selection = 'none',
  options = list(
    pageLength = 25
    #,initComplete = I("function(settings, json) {alert('Done.');}")
  ))
  
  output$attrition <- DT::renderDataTable({
    if (is.null(summary$data)) return()
    if(is.null(input$explorerIds)){
      id <- 1
    } else{
      id <- input$explorerIds
    }
    id <- summary$data[id, colnames(summary$data)=='modelId']
    model <- PatientLevelPrediction::loadPlpModel(file.path(priorResult$dataFolder, id, 'savedModel' ))
    data.frame(model$populationSettings$attrition)
  },     escape = FALSE, selection = 'none',
  options = list(
    pageLength = 25
    #,initComplete = I("function(settings, json) {alert('Done.');}")
  ))
  
  output$options <- DT::renderDataTable({
    if (is.null(summary$data)) return()
    if(is.null(input$explorerIds)){
      id <- 1
    } else{
      id <- input$explorerIds
    }
    id <- summary$data[id, colnames(summary$data)=='modelId']
    model <- PatientLevelPrediction::loadPlpModel(file.path(priorResult$dataFolder, id, 'savedModel' ))
    model$populationSettings$attrition <- NULL
    result <- t(as.data.frame(model$populationSettings))
    colnames(result) <- c('Setting')
    result
  },     escape = FALSE, selection = 'none',
  options = list(
    pageLength = 25
    #,initComplete = I("function(settings, json) {alert('Done.');}")
  ))
  #=============================
  
  output$rocPlot <- shiny::renderPlot({
    if (is.null(summary$data)) return()
    if(is.null(input$explorerIds)){
      id <- 1
    } else{
      id <- input$explorerIds
    }
    id <- summary$data[id, colnames(summary$data)=='modelId']
    rocData <- read.table(file.path(priorResult$dataFolder, id,'test', 'rocRawSparse.txt' ), header=T)
    sensitivity <- rocData$TP/(rocData$TP+rocData$FN)
    one_minus_specificity <- 1-rocData$TN/(rocData$TN+rocData$FP)
    data <- data.frame(sensitivity=sensitivity,
                       one_minus_specificity=one_minus_specificity)
    #plot(1-specificity, sensitivity)
    steps <- data.frame(sensitivity = sensitivity[1:(length(sensitivity) - 1)],
                        one_minus_specificity = one_minus_specificity[2:length(one_minus_specificity)] - 1e-09)
    data <- rbind(data, steps)
    data <- data[order(data$sensitivity, data$one_minus_specificity), ]
    ggplot2::ggplot(data, ggplot2::aes(x = one_minus_specificity, y = sensitivity)) +
      ggplot2::geom_abline(intercept = 0, slope = 1) +
      ggplot2::geom_area(color = rgb(0, 0, 0.8, alpha = 0.8),
                         fill = rgb(0, 0, 0.8, alpha = 0.4)) +
      ggplot2::scale_x_continuous("1 - specificity") +
      ggplot2::scale_y_continuous("Sensitivity")
  })
  output$rocPlotTrain <- shiny::renderPlot({
    if (is.null(summary$data)) return()
    if(is.null(input$explorerIds)){
      id <- 1
    } else{
      id <- input$explorerIds
    }
    id <- summary$data[id, colnames(summary$data)=='modelId']
    rocData <- read.table(file.path(priorResult$dataFolder, id,'train', 'rocRawSparse.txt' ), header=T)
    sensitivity <- rocData$TP/(rocData$TP+rocData$FN)
    one_minus_specificity <- 1-rocData$TN/(rocData$TN+rocData$FP)
    data <- data.frame(sensitivity=sensitivity,
                       one_minus_specificity=one_minus_specificity)
    #plot(1-specificity, sensitivity)
    steps <- data.frame(sensitivity = sensitivity[1:(length(sensitivity) - 1)],
                        one_minus_specificity = one_minus_specificity[2:length(one_minus_specificity)] - 1e-09)
    data <- rbind(data, steps)
    data <- data[order(data$sensitivity, data$one_minus_specificity), ]
    ggplot2::ggplot(data, ggplot2::aes(x = one_minus_specificity, y = sensitivity)) +
      ggplot2::geom_abline(intercept = 0, slope = 1) +
      ggplot2::geom_area(color = rgb(0, 0, 0.8, alpha = 0.8),
                         fill = rgb(0, 0, 0.8, alpha = 0.4)) +
      ggplot2::scale_x_continuous("1 - specificity") +
      ggplot2::scale_y_continuous("Sensitivity")
  })
  
  output$rocPlotVal <- shiny::renderPlot({
    if (is.null(summary$data)) return()
    if(is.null(input$explorerIds)){
      id <- 1
    } else{
      id <- input$explorerIds
    }
    modelId <- summary$data[id, 'modelId']
    folder <- list.dirs(path = file.path(priorResult$dataFolder,modelId), 
                        full.names = TRUE, recursive = TRUE)
    folder <- gsub(paste0(priorResult$dataFolder,'/',modelId,'/'),'',folder)
    val <- folder[!folder%in%c('','savedModel','train','test', paste0(priorResult$dataFolder,'/',modelId))]
    if(length(val)>0){
      #find modelDetails:
      performanceVal <- read.table(file.path(priorResult$dataFolder, 'performanceInfoVal.txt'), header=T)
      validationRes <- performanceVal[performanceVal$performanceID%in%val,]
      valNames <- validationRes$database
      val <- validationRes$performanceID
      
      # for each validation load the sparseROC:
      data <- c()
      for(i in 1:length(val)){
        #writeLines(paste0(i))
        rocData <- read.table(file.path(priorResult$dataFolder, modelId,val[i], 'rocRawSparse.txt' ), header=T)
        rocData$database <- rep(valNames[i], nrow(rocData))
        #writeLines(paste0(nrow(rocData)))
        
        sensitivity <- rocData$TP/(rocData$TP+rocData$FN)
        one_minus_specificity <- 1-rocData$TN/(rocData$TN+rocData$FP)
        data.temp <- data.frame(database =rocData$database,
                                sensitivity=sensitivity,
                                one_minus_specificity=one_minus_specificity)
        #plot(1-specificity, sensitivity)
        steps <- data.frame(database =rocData$database[1:(length(sensitivity)-1)],
                            sensitivity = sensitivity[1:(length(sensitivity) - 1)],
                            one_minus_specificity = one_minus_specificity[2:length(one_minus_specificity)] - 1e-09)
        data.temp <- rbind(data.temp, steps)
        
        data <- rbind(data, data.temp)
      }
      
      data <- data[order(data$sensitivity, data$one_minus_specificity), ]
      ggplot2::ggplot(data, ggplot2::aes(x = one_minus_specificity, y = sensitivity, group=database, color=database)) +
        ggplot2::geom_abline(intercept = 0, slope = 1) +
        ggplot2::geom_line()+
        #ggplot2::geom_area(color = rgb(0, 0, 0.8, alpha = 0.8),
        #                   fill = rgb(0, 0, 0.8, alpha = 0.4)) +
        ggplot2::scale_x_continuous("1 - specificity") +
        ggplot2::scale_y_continuous("Sensitivity")
    }
  })
  
  #=====================
  
  output$boxPlot <- shiny::renderPlot({
    if(is.null(input$explorerIds)){
      id <- 1
    } else{
      id <- input$explorerIds
    }
    id <- summary$data[id, colnames(summary$data)=='modelId']
    boxData <- read.table(file.path(priorResult$dataFolder, id,'test', 'quantiles.txt' ), header=T)
    
    #"Group.1" "x.0%" "x.25%" "x.50%" "x.75%" "x.100%"
    colnames(boxData) <- c('Outcome', 'y0','y10', 'y25', 'y50', 'y75','y90', 'y100')
    ggplot2::ggplot(boxData, ggplot2::aes(as.factor(Outcome))) +
      ggplot2::geom_boxplot(
        ggplot2::aes(ymin = y10, lower = y25, middle = y50, upper = y75, ymax = y90),
        stat = "identity", color=c("#D55E00", "#56B4E9")
      ) +
      ggplot2::coord_flip() +
      ggplot2::geom_point(ggplot2::aes(y = boxData[,'y0'])) +
      ggplot2::geom_point(ggplot2::aes(y = boxData[,'y100']))
    
  })
  output$boxPlotTrain <- shiny::renderPlot({
    if(is.null(input$explorerIds)){
      id <- 1
    } else{
      id <- input$explorerIds
    }
    id <- summary$data[id, colnames(summary$data)=='modelId']
    boxData <- read.table(file.path(priorResult$dataFolder, id,'train', 'quantiles.txt' ), header=T)
    
    #"Group.1" "x.0%" "x.25%" "x.50%" "x.75%" "x.100%"
    colnames(boxData) <- c('Outcome', 'y0','y10', 'y25', 'y50', 'y75','y90', 'y100')
    ggplot2::ggplot(boxData, ggplot2::aes(as.factor(Outcome))) +
      ggplot2::geom_boxplot(
        ggplot2::aes(ymin = y10, lower = y25, middle = y50, upper = y75, ymax = y90),
        stat = "identity", color=c("#D55E00", "#56B4E9")
      ) +
      ggplot2::coord_flip() +
      ggplot2::geom_point(ggplot2::aes(y = boxData[,'y0'])) +
      ggplot2::geom_point(ggplot2::aes(y = boxData[,'y100']))
    
  })
  
  #=========================
  
  output$calPlot <- shiny::renderPlot({
    if (is.null(summary$data)) return()
    if(is.null(input$explorerIds)){
      id <- 1
    } else{
      id <- input$explorerIds
    }
    id <- summary$data[id, colnames(summary$data)=='modelId']
    calData <- read.table(file.path(priorResult$dataFolder, id,'test', 'calSparse2_10.txt' ), header=T)
    
    # linear model:
    fit <- lm(obs ~ pred, data=calData)
    param <- coefficients(fit)
    
    ggplot2::ggplot(calData,
                    ggplot2::aes(x=pred, y=obs)) +
      ggplot2::geom_point() +
      ggplot2::geom_abline(slope=param[2], intercept= param[1], color='red') +
      ggplot2::geom_abline(slope=1, intercept=0,linetype="dotted", color = "black")+
      ggplot2::xlim(0,max(calData[,'pred']))
    
  })
  
  output$calPlotTrain <- shiny::renderPlot({
    if (is.null(summary$data)) return()
    if(is.null(input$explorerIds)){
      id <- 1
    } else{
      id <- input$explorerIds
    }
    id <- summary$data[id, colnames(summary$data)=='modelId']
    calData <- read.table(file.path(priorResult$dataFolder, id,'train', 'calSparse2_10.txt' ), header=T)
    
    # linear model:
    fit <- lm(obs ~ pred, data=calData)
    param <- coefficients(fit)
    
    ggplot2::ggplot(calData,
                    ggplot2::aes(x=pred, y=obs)) +
      ggplot2::geom_point() +
      ggplot2::geom_abline(slope=param[2], intercept= param[1], color='red') +
      ggplot2::geom_abline(slope=1, intercept=0,linetype="dotted", color = "black")+
      ggplot2::xlim(0,max(calData[,'pred']))
    
  })
  #====================
  
  output$prefPlot <- shiny::renderPlot({
    if (is.null(summary$data)) return()
    if(is.null(input$explorerIds)){
      id <- 1
    } else{
      id <- input$explorerIds
    }
    id <- summary$data[id, colnames(summary$data)=='modelId']
    prefData <- read.table(file.path(priorResult$dataFolder, id,'test', 'preferenceScoresSparse.txt' ), header=T)
    ggplot2::ggplot(prefData, ggplot2::aes(x=groupVal, y=density,
                                           group=as.factor(outcomeCount), col=as.factor(outcomeCount),
                                           fill=as.factor(outcomeCount))) +
      ggplot2::geom_line() + ggplot2::xlab("Preference") + ggplot2::ylab("Density") +
      ggplot2::scale_x_continuous(limits = c(0, 1)) +
      ggplot2::geom_vline(xintercept = 0.3) + ggplot2::geom_vline(xintercept = 0.7)
    
  })
  
  output$prefPlotTrain <- shiny::renderPlot({
    if (is.null(summary$data)) return()
    if(is.null(input$explorerIds)){
      id <- 1
    } else{
      id <- input$explorerIds
    }
    id <- summary$data[id, colnames(summary$data)=='modelId']
    prefData <- read.table(file.path(priorResult$dataFolder, id,'train', 'preferenceScoresSparse.txt' ), header=T)
    ggplot2::ggplot(prefData, ggplot2::aes(x=groupVal, y=density,
                                           group=as.factor(outcomeCount), col=as.factor(outcomeCount),
                                           fill=as.factor(outcomeCount))) +
      ggplot2::geom_line() + ggplot2::xlab("Preference") + ggplot2::ylab("Density") +
      ggplot2::scale_x_continuous(limits = c(0, 1)) +
      ggplot2::geom_vline(xintercept = 0.3) + ggplot2::geom_vline(xintercept = 0.7)
    
  })
  
  
  
  #============================= APPLY MODEL ===================
  evalOnNewData <- function(plpModelLoc, plpDataLoc){
    #check result already in performanceInfoVal.txt
    database <- strsplit(plpDataLoc,'/')[[1]]
    database <- database[length(database)]
    dirCheck <- strsplit(as.character(plpModelLoc),'/')[[1]]
    modelId <- dirCheck[(length(dirCheck)-1)]
    dirCheck <- paste(paste(dirCheck[1:(length(dirCheck)-2)],sep='/',collapse='/'),'performanceInfoVal.txt', sep='/',collapse='/')
    if(file.exists(dirCheck)){
      eval_models <- read.table(dirCheck, header=T)
      eval_models <- sum(eval_models$database==gsub('plpData_','',database)  & eval_models$modelId==modelId)
      if(eval_models>0) return()
    }
    
    progress <- shiny::Progress$new()
    progress$set(message = "Installing Packages...", value = 0)
    # Close the progress when this reactive exits (even if there's an error)
    on.exit(progress$close())
    
    updateProgress <- function(value = NULL, detail = NULL) {
      if (is.null(value)) {
        value <- progress$getValue()
        value <- value + (progress$getMax() - value) / 5
      }
      progress$set(value = value, detail = detail)
    }
    updateProgress(detail='Checking and loading model/data')
    plpModelLoc <- as.character(plpModelLoc)
    writeLines(plpModelLoc)
    
    dirPath <- strsplit(as.character(plpModelLoc), '/')[[1]]
    dirPath <- paste(dirPath[1:(length(dirPath)-2)], sep='',collapse='/')
    ##writeLines(dirPath)
    # code for running existing model on new data 
    start.all <- Sys.time()
    analysisId <- gsub(':','',gsub('-','',gsub(' ','',start.all)))
    
    if(dir.exists(plpModelLoc) & dir.exists(plpDataLoc) ){
      
      # load model and data
      plpModel <- PatientLevelPrediction::loadPlpModel(plpModelLoc)
      plpData <- PatientLevelPrediction::loadPlpData(plpDataLoc)
      updateProgress(detail='Model/data loaded...')
      updateProgress(detail='Creating population')
      # create population:
      settings <- plpModel$populationSettings
      settings$plpData=plpData
      population <- do.call(PatientLevelPrediction::createStudyPopulation,
                            settings) 
      #check population settings valid for plpdata else return
      if(!is.null(population)){
        
        plpDataId <- strsplit(plpDataLoc, '/')[[1]]
        plpDataId <- gsub('plpData_','',plpDataId[length(plpDataId)])
        plpModelId <- strsplit(gsub('/savedModel','',plpModelLoc), '/')[[1]]
        plpModelId <- plpModelId[length(plpModelId)]
        
        updateProgress(detail='Calculating predictions')
        
        # apply model:
        results <- PatientLevelPrediction::applyModel(population, plpData, plpModel)
        prediction <- results$prediction
        performance <- results$performance
        
        # save into performanceInfoVal.txt - with modelId so can pull trainInfo
        #"Performance_datetime","database",Model_datetime" "auc" "auc_lb95ci" "auc_lb95ci.1" "Brier" 
        #"BrierScaled" "Xsquared" "df" "pvalue" "calibrationIntercept" "calibrationGradient" 
        #"preference3070_0" "preference3070_1"
        updateProgress(detail='Saving results...')
        
        performanceInfoVal <- data.frame(performanceID =analysisId,
                                         database = plpDataId,
                                         modelId = plpModelId, 
                                         AUC = performance$auc[1],
                                         AUC_lb = performance$auc[2],
                                         AUC_ub = performance$auc[3],
                                         Brier = performance$brier,
                                         BrierScaled = performance$brierScaled,
                                         hosmerlemeshow_chi2 = performance$hosmerlemeshow[1],
                                         hosmerlemeshow_df = performance$hosmerlemeshow[2],
                                         hosmerlemeshow_pvalue = performance$hosmerlemeshow[3],
                                         calibrationIntercept = performance$calibrationIntercept10,
                                         calibrationGradient = performance$calibrationGradient10,
                                         preference3070_0 = performance$preference3070_0,
                                         preference3070_1 = performance$preference3070_1
        )
        # save this to performanceInfoVal.txt
        if(file.exists(file.path(dirPath,'performanceInfoVal.txt')))
          write.table(performanceInfoVal, file.path(dirPath,'performanceInfoVal.txt'), append=T, row.names=F, col.names=F)
        if(!file.exists(file.path(dirPath,'performanceInfoVal.txt')))
          write.table(performanceInfoVal, file.path(dirPath,'performanceInfoVal.txt'), row.names = F, col.names=T)
        
        # save files into performance_datetime folder:
        if(!dir.exists(file.path(dirPath,plpModelId ,analysisId)))
          dir.create(file.path(dirPath,plpModelId ,analysisId), recursive = T)
        
        write.table(performance$raw, file.path(dirPath,plpModelId ,analysisId , 'rocRawSparse.txt'), row.names=F)
        write.table(performance$preferenceScores, file.path(dirPath,plpModelId ,analysisId , 'preferenceScoresSparse.txt'), row.names=F)
        write.table(performance$calSparse, file.path(dirPath,plpModelId ,analysisId , 'calSparse.txt'), row.names=F)
        write.table(performance$calSparse2_10, file.path(dirPath,plpModelId ,analysisId , 'calSparse2_10.txt'), row.names=F)
        write.table(performance$calSparse2_100, file.path(dirPath,plpModelId ,analysisId , 'calSparse2_100.txt'), row.names=F)
        write.table(performance$quantiles, file.path(dirPath,plpModelId ,analysisId , 'quantiles.txt'), row.names=F)
      }
    }
    
  }
  
  #============================================================================
  #=============================================================================
  # model selection:
  
  output$apply <- shiny::renderUI(
    shiny::wellPanel(
      shiny::h4("Pick the model you want to do the prediction: "),
      shiny::selectInput("predict_model", label = "Model:",
                         choices = train$modelFold,
                         selected = 1),
      
      shiny::h4("Select the plpData you want to predict on: "),
      
      shiny::selectInput("predict_data", label = "Data:",
                         choices = train$dataFold,
                         selected = 1),
      
      shiny::actionButton('predict', 'Predict')
    )
  )
  
  # have evalOnNewData when input$predict
  shiny::observeEvent(input$predict, {
    if(!is.null(input$predict_data)){
      if(is.null(summary$data)) return()
      if(is.null(input$predict_model)) return()
      if(!dir.exists(as.character(input$predict_data))) return()
      evalOnNewData(plpModelLoc= summary$data[input$predict_model, colnames(summary$data)=='modelLoc'], 
                    plpDataLoc=as.character(input$predict_data))
    }
  })
  
  
})
