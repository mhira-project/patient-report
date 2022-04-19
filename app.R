#Load packages ----
library(shiny)
library(shinydashboard)
library(tidyverse)
library(RColorBrewer)
library(data.table)
library(httr)
library(jsonlite)
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


inactivity = inactivity(timeoutSeconds)

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
       
      tags$script(HTML( # This javascript code gets the token from localStorage
         "$(document).on('shiny:connected', function() {
            const LS = window.localStorage.getItem('auth_app_token');
            Shiny.setInputValue('accessToken', LS);
            });"
         )),
      
     # div(class = "inlay", style = "height:15px;width:100%;background-color: #ecf0f5;"),
     br(),
     
     menuItem(
        "SELECT ASSESSMENTS",
        tabName = "select assessment(s)",
        icon = icon("clipboard"),
        uiOutput("selectAss"),
        startExpanded = TRUE
      )
   
     ),
    
    # BODY --------------------------------------------------------------------
    dashboardBody(
       
      includeCSS("www/myCSS.css"),
      
      fluidRow(

            width = 12,
            h3("Select questionnaire from tabs", class = "select"),
          
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
      print(paste0("Session (", session$token, ") timed out at: ", Sys.time()))
      showModal(modalDialog(
        title = "Timeout",
        paste("Session timeout due to",
              input$timeOut,
              "inactivity -",
              Sys.time()),
        footer = NULL
      ))
      session$close()
    })
    
  # GET patientID FROM THE URL ------------------------------------------------

     patientId = reactive({
        query <- parseQueryString(session$clientData$url_search) 
        session$userData  = fromJSON(input$accessToken)$accessToken
        
                if (length(query) > 0){
                  return(as.character(query$patient_id)) # 
                 
                } else {
                 return(NULL)
        }
      }) %>% bindEvent(input$accessToken)
     
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
          "No patient found",
          type = "error",
          duration = 20)
        session$close()}
      
      if(is_empty(response$data$generatePatientReport$assessments)){
        showNotification(
          "No assessments found",
          type = "error",
          duration = 20)
        session$close()}
      
      if(is_empty(response$data$generatePatientReport$answeredQuestionnaires)){
        showNotification(
           "No answered questionnaires found",
          type = "error",
          duration = 20)
        session$close()}
      
      # Simplify data and remove incomplete questionnaires
      
      data = simplifyPatRep(response = response)  
      
      # Terminate session if no competed data
      
      if(is_empty(data)){
        showNotification(
          "There are no completed questionnaires",
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
  
            }) 
    
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
                         selected = choice)
      
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
            
            plots = renderPlot(
              expr = severityPlot(scales = s, TimeOnXAxis = T),
              height = "auto", 
              width = 600
            )
            
            # Create interpretation table and add to list 
            
            
            interpret <-  renderDataTable({
              s %>%  
                arrange(desc(assessmentDateTime), questionnaireShortName, text_order) %>%
                select(assessment = assessmentName,
                       time = assessmentDateTime,
                       scale = variable,
                       interpretation = interpretation,
                       recommendation = recommendation) 
              
            })
            
            
     # create score table and add to list
            
            
            scores <- renderDataTable({
              req(scalesFlt())
              s %>%  
                arrange(desc(assessmentDateTime), questionnaireShortName, text_order) %>%
                select(assessment = assessmentName,
                       time = assessmentDateTime,
                       scale = variable,
                       score = value)
            })
            
            
   # bring tab elements together
            output[[my_q]] <-  renderUI(div(class= "box2",
                                       h1(my_q),
                                       tagList(h3("Figure"),
                                       plots,
                                       br(),
                                       hr(),
                                       h3("Interpretation and recommendation"),
                                       interpret,
                                       br(),
                                       hr(),
                                       h3("Data"),
                                       scores))
                                        )
        })
      }
      
   })
      

      
    
    
    
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
