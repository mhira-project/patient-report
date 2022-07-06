#Load packages ----
library(shiny)
library(shinydashboard)
library(tidyverse)
library(RColorBrewer)
library(data.table)
library(httr)
library(jsonlite)
library(DT)
#library(tidyjson)



# APP SETTINGS ---------------------------------------------------------------- 

timeoutSeconds <- 10*60 # App closes after x seconds of inactivity

# LOAD GRAPHQL ----------------------------------------------------------------

source("graphql_functions/getPatientReport.R")

# LOAD UTILITY ----------------------------------------------------------------

source("utility_functions/simplifyPatRep.R")
source("utility_functions/calculateScales.R")
source("utility_functions/applyCutOffs.R")
source("utility_functions/severityPlot.R")
source("utility_functions/inactivity.R")
source("utility_functions/interpretTable.R")


inactivity = inactivity(timeoutSeconds)


# LOAD TRANSLATION MATRIX -----------------------------------------------------

transMatrix = data.frame(fread("www/transMatrix.csv"), row.names = "Key")
defaultLang = "en"

# USER INTERFACE ##------------------------------------------------------------

  ui <- dashboardPage(skin = "purple",
                      title="MHIRA",
     
    # HEADER ------------------------------------------------------------------ 
    dashboardHeader(
      title = tags$a(href='http://mhira-project.org',
                     tags$img(src='mhira_logo.png', height='50', width='150'),
                     'MHIRA')
      
    ),
    
    # SIDEBAR ------------------------------------------------------------------
    dashboardSidebar(
      width = 250,
      collapsed = FALSE,
      tags$script(inactivity), # For timeout
       
      tags$script(HTML( # This javascript code gets data from localStorage of the browser
         "$(document).on('shiny:connected', function() {
            const LS = window.localStorage.getItem('auth_app_token');
            Shiny.setInputValue('accessToken', LS);
            const CL = window.localStorage.getItem('currentLang');
            Shiny.setInputValue('currentLang', CL);
            });"
         )),
      
     br(),
     
     
     uiOutput("selectAss")

   
     ),
    
    # BODY --------------------------------------------------------------------
    dashboardBody(
       
      includeCSS("www/myCSS.css"),
      
      fluidRow(

            width = 12,
           # h3(transMatrix["promtTabSelection", input$currentLang], class = "select"),
          
            uiOutput("tab")
            
        ),
      
      fluidRow(

          width = 12,
       #   uiOutput("scaleFlt")
        )
      
      
      )
      

      
                    
    
    
  # CLOSE USER INTERFACE UI ---------------------------------------------------
    
    )

 ## SERVER ## ----------------------------------------------------------------- 
  
  server = function(input, output, session) {
  
  # OBSERVE INACTIVITY AND CLOSE APP ------------------------------------------  
    
    observeEvent(input$timeOut, { 
      print(paste0(transMatrix["timeoutAt", input$currentLang], " ", Sys.time()))
      showModal(modalDialog(
        title = "Timeout",
        paste(transMatrix["timeoutReason", input$currentLang],
              input$timeOut,
        ),
        footer = NULL
      ))
      session$close()
    })
    
 
    # GET patientID FROM THE URL ------------------------------------------------
    
    patientId = reactive({
      query <- parseQueryString(session$clientData$url_search) 
      
      if (length(query) > 0){
        return(as.character(query$patient_id)) # 
        
      } else {
        return(NULL)
      }
    }) 
    
    
   # STORE LOCAL STORAGE EXTRACTED TOKEN TO SHINY SESSION OBJECT  ------------
    observe({ 
      session$userData  = fromJSON(input$accessToken)$accessToken
           }) %>%  bindEvent(input$accessToken)
    

     
  # GET PATIENT DATA AND PRE-PROCESS: DATA, SCALES, ADD CUTOFF -----------------
      
  
    data = reactiveVal() # the imported data as dataframe
    scales = reactiveVal() # calclulated scales
    
      
    observe({
      req(!is_empty(patientId()))
      
      response = getPatientReport(token = session$userData, patientId = patientId())
     
  
      # Terminate session if response doesn't contain data     
      if(is_empty(response$data)){
        showNotification(
          response$errors$message,
          type = "error",
          duration = 20)
        session$close()}
      
      if(is_empty(response$data$generatePatientReport$patient)){
        showNotification(
          transMatrix["noPatient", input$currentLang],
          type = "error",
          duration = 20)
        session$close()}
      
      if(is_empty(response$data$generatePatientReport$assessments)){
        showNotification(
          transMatrix["noAssessment", input$currentLang],
          type = "error",
          duration = 20)
        session$close()}
      
      if(is_empty(response$data$generatePatientReport$answeredQuestionnaires)){
        showNotification(
          transMatrix["noQuestionnaire", input$currentLang],
          type = "error",
          duration = 20)
        session$close()}
      
      # Simplify data and remove incomplete questionnaires
      
      data = simplifyPatRep(response = response)  
      
      # Terminate session if no competed data
      
      if(is_empty(data)){
        showNotification(
          transMatrix["noQuestionnaire", input$currentLang],
          type = "error",
          duration = 20)
        session$close()
        }
      
      questionnaireScripts = response$data$generatePatientReport$questionnaireScripts  
      
      scales = calculateScales(
                  simplifiedData = data,
                  questionnaireScripts =  questionnaireScripts)
      
  
      scales = applyCutOffs(scales = scales, questionnaireScripts =  questionnaireScripts)
      
      
  
          data(data)
          scales(scales)
          print("data has been loaded")
  
            }) %>% bindEvent(input$currentLang) 
    
  # CREATE CHECKBOX FOR ASSESSMENT SELECTION (SIDEBAR)--------------------------   
    
    output$selectAss <- renderUI({
      req(scales())
      c = scales()
      choice <-  paste(c$assessmentDate,
                       " '", c$assessmentName,"'", sep = "") %>%
                  unique %>% 
                  sort(., decreasing = T)
      
        checkboxGroupInput(inputId = "selectedAss",
                         label = "",
                         choices = choice,
                         selected = choice) %>%
        menuItem( 
        text = transMatrix["selectAssessment", input$currentLang],
        icon = icon("clipboard"),
        startExpanded = TRUE)              
      
    })
    

  # FILTER ACCORDING TO SELECTED ASSESSMENTS --------------------------------
    
    scalesFlt =  reactive({
       req(input$selectedAss)
       print("filter scales based on selectedAss") 
       s = str_split(input$selectedAss, pattern = " '") 
       sD = s %>% map_chr(.f = ~ .[1]) 
       sA = sub("'", "", s %>% map_chr(.f = ~ .[2]))  
       scalesFlt = scales() %>%
         filter(assessmentName %in% sA & as.character(assessmentDate) %in% sD)

       return(scalesFlt)
       }) 
    
   
    dataFlt =  reactive({
      req(scalesFlt())
      print("filter simple data based on selectedAss")
      dataFlt = data() %>%
        right_join(scalesFlt() %>%
        select(assessmentId), by = c("assessmentId" = "assessmentId")) 
      return(dataFlt)
    }) 


  # RENDER AND COMBINE UI (PLOT, TABLE, ...) FOR EACH QUESTIONNAIRE ----------
    
   observe({
     req(scalesFlt())
 
      for (q in scalesFlt()$questionnaireShortName %>% unique){
        
        local({ #https://gist.github.com/wch/5436415/
            
          my_q = q 
          s = scalesFlt() %>% filter(questionnaireShortName == my_q)
            
    # Create plot  
          
           plots = renderPlot(
              expr = severityPlot(scales = s, TimeOnXAxis = T),
              height = "auto", 
              width = 600
            )
            
    # Create interpretation 
            
      interpret =   s %>% interpretTable(showScale = F, transMatrix =  transMatrix, lang = input$currentLang)
     
     
     # create score table 
            
     sco =    s %>%  
                arrange(desc(assessmentDateTime), questionnaireShortName, text_order) %>%
                select(assessment = assessmentName,
                       time = assessmentDateTime,
                       scale = variable,
                       score = value)
           
     colnames(sco) <- transMatrix[c("assessment","time","scale", "score"), input$currentLang] 
              
     scores =   sco %>%  DT::renderDataTable() 
      
            
            
   # bring plot, interpret and score table together in div
           
             output[[my_q]] <-  renderUI(div(class= "box2",
                                       h1(my_q),
                                       tagList(h3(transMatrix["figure", input$currentLang]),
                                       plots,
                                       br(),
                                       hr(),
                                       h3(transMatrix["evaluation", input$currentLang]),
                                       interpret,
                                       br(),
                                       hr(),
                                       h3(transMatrix["data", input$currentLang]),
                                       scores))
                                        )
        })
      }
      
   }) %>% bindEvent(scalesFlt())
      
    
  # CREATE TABS ---------------------------------------------------------------

    output$tab <- renderUI({

      print("rendering tabbox")

      panels = lapply(scalesFlt()$questionnaireShortName %>% unique,
                      function(q){tabPanel(title = q,
                                            uiOutput(q) # Output
                                                )})


      do.call(tabsetPanel, c(header = "", id = "pannelId", panels))

 })

  }

## APP ## --------------------------------------------------------------------

shinyApp(ui = ui, server = server)
