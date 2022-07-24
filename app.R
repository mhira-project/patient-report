#Load packages ----
library(shiny)
library(shinydashboard)
library(tidyverse)
library(RColorBrewer)
library(data.table)
library(httr)
library(jsonlite)
library(DT)
library(crosstalk)

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
source("utility_functions/checkGraphqlResponse.R")


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
           # h3(transMatrix["promtTabSelection", lang()], class = "select"),
          
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
      print(paste0(transMatrix["timeoutAt", lang()], " ", Sys.time()))
      showModal(modalDialog(
        title = "Timeout",
        paste(transMatrix["timeoutReason", lang()],
              input$timeOut,
        ),
        footer = NULL
      ))
      session$close()
    })
    
 
    # GET patientID FROM THE URL ------------------------------------------------
    
    patientId =  reactiveVal()
    
    observe({
      print("getting patient id from url")
      query <- parseQueryString(session$clientData$url_search) 
      
      if (length(query) > 0){
         query$patient_id  %>%
          as.character() %>%
          patientId()
      } 
    }) 
    
    
   # STORE LOCAL STORAGE EXTRACTED TOKEN TO SHINY SESSION OBJECT
    observe({ 
      print("writing token to session object")
      session$userData  = fromJSON(input$accessToken)$accessToken
           }) %>%  bindEvent(input$accessToken)
    
    
  # CHECK LANGUAGE AND CREATE 'LANG' REACTIVE VALUE
     
    lang = reactiveVal(defaultLang)
    
    observe({
      print("set language")
      if(input$currentLang %in% colnames(transMatrix)){
      input$currentLang %>% lang()
      }
    }) %>% bindEvent(input$currentLang)

     
  # GET PATIENT DATA AND PRE-PROCESS: DATA, SCALES, ADD CUTOFF -----------------
  
    data = reactiveVal() # the imported data as dataframe
    scales = reactiveVal() # calclulated scales
    
      
    observe({
      req(!is_empty(patientId()))
      req(lang())
      req(!is_empty(session$userData))
      print("get patient report via graphql")
      
      response = getPatientReport(token = session$userData, patientId = patientId())
     
      checkGraphqlResponse(response, session) # can close session
     
      
      # Simplify data and remove incomplete questionnaires
      
      data = simplifyPatRep(response = response)  
      
      # Terminate session if no competed data
      
      if(is_empty(data)){
        showNotification(
          transMatrix["noQuestionnaire", lang()],
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
  
            }) %>%  bindEvent(input$accessToken)
    
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
        text = transMatrix["selectAssessment", lang()],
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
     print("Rendering elements...")
 
      for (q in scalesFlt()$questionnaireShortName %>% unique){
        
        local({ #https://gist.github.com/wch/5436415/
            
          my_q = q 
          s = scalesFlt() %>% filter(questionnaireShortName == my_q)
            
    # Create plot  
        print("... plot")  
           plots = renderPlot(
              expr = severityPlot(scales = s, TimeOnXAxis = T),
              height = "auto", 
              width = 600
            )
            
    # Create interpretation 
           print("... interpretation Table")
            
      interpret =   s %>% interpretTable(showScale = F, transMatrix =  transMatrix, lang = lang())
     
     
     # create score table 
      print("... scores")
            
     sco =    s %>%  
                arrange(desc(assessmentDateTime), questionnaireShortName, text_order) %>%
                select(time = assessmentDateTime,
                       assessment = assessmentName,
                       scale = scale,
                       score = value,
                       level = level,
                       lower_cutoff = low_cut,
                       upper_cutoff = high_cut
                       )
     
     sco$assessment[duplicated(sco$assessment)] <- NA
     sco$time[duplicated(sco$time)] <- NA
           
     colnames(sco) <- transMatrix[c("time","assessment","scale", "score", "level", "low_cut", "high_cut"), lang()] 
              
     scores =   sco %>%  datatable(options = list(pageLength = 100)) 
      

   # bring plot, interpret and score table together in div
           
    output[[my_q]] <-  renderUI(
                          div(
                              h1(my_q),  
                              class= "box2",
                              h3(transMatrix["figure", lang()]),
                              if(!is_empty(plots)){plots},
                              br(),
                              hr(),
                              h3(transMatrix["data", lang()]),
                              scores,
                              br(),
                              hr(),
                              h3(transMatrix["evaluation", lang()]),
                              interpret
                              )
                          )
        })
      }
      
   }) %>% bindEvent(scalesFlt())
      
    
  # CREATE TABS ---------------------------------------------------------------

    output$tab <- renderUI({
      req(scalesFlt())
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