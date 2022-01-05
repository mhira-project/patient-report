#Load packages ----
library(shiny)
library(shinyTree)
library(shinydashboard)
library(tidyverse)
library(tidyjson)
library(RColorBrewer)

# Load graphql functions ----
source("graphql_functions/getMhiraAssessments1.R")
source("graphql_functions/getMhiraData4.R")
source("graphql_functions/getPatiensFromMHIRA.R")
source("graphql_functions/getToken.R")

# Load instruments ----
source("instruments/instrumentLopf97Ger3.R")
source("instruments/instrumentPhq9En3.R")
source("instruments/instrumentBPFSC24En.R")

# Load utility functions ----
source("utility_functions/severityPlot.R")
source("utility_functions/makeTreeStructure.R")


# User interface -----

  ui <- dashboardPage(
     
     dashboardHeader(),
     dashboardSidebar(
       includeCSS("www/myCSS.css"),
       shinyTree("tree", checkbox = TRUE, theme = "default", themeIcons = FALSE, themeDots = TRUE),
       hr(),
       actionButton(
         inputId = "submit",
         label = "Apply Changes!",
         style = "margin:40px;"
       )
       
      
     ),
     dashboardBody(
            includeCSS("www/myCSS.css"),
          #  div(tableOutput("q"),  width = "600px"),
            fluidRow(
        #     div( 
               id = "plot-container",
               uiOutput(
                 outputId = "boxes"
                )
         #    )
                    ), 
           
          
              # tableOutput("q"),
              # tableOutput("L"),
              # plotOutput( "Plot0")
        
         
       )
      
       
     )
   
  
    
 # Server ---- 
  
  server = function(input, output, session) {
    

     patientId = reactiveVal()
      
      observe({
        query <- parseQueryString(session$clientData$url_search) 
        
                if (length(query) > 0){
                  session$userData  = query$token
                  patientId(as.character(query$patient_id)) # 
                 
                } else {
                  session$userData  = NULL
                  patientId(NULL)
        }
      }) 
      
  
      dfAss = reactiveVal()
               observeEvent(req(!is.null(session$userData) &  !is.null(patientId())), {
                 print(paste("This is", patientId()))
                      mhiraAssessments = getMhiraAssessments1(session$userData, sub("XXXX", as.numeric(patientId()),'patient:{id:{eq:XXXX}}'))
                      if(is.null(mhiraAssessments) | nrow(mhiraAssessments) < 1){output$patient_error_msg = renderText("There are no completed assessments for this patient!")
                      } else  {dfAss(data.frame(mhiraAssessments))
                               removeModal()}
                                        })


     assId = reactive({dfAss()$id})

     dfFullAss =  reactive({
            print("dfFullAss")
            print(assId())
            getMhiraData4(session$userData, assId())
              })

     #output$q = renderTable(questListFiltered() %>% map("values") %>% rbindlist %>% select(assessmentName, questionnaireName, variable))



     questListData = reactive({
       questListData = list()
       if (nrow(dfFullAss()) > 0){
       questListData[[1]]  <- instrumentLopf97Ger3(dfFullAss())
       questListData[[2]]  <- instrumentPhq9En3(dfFullAss())
       questListData[[3]]  <- instrumentBPFSC24En(dfFullAss())
       questListData = questListData %>% compact() # Remove NULL Lists

       if (is.null(questListData) | length(questListData) == 0){
          showNotification("There is no compatible reporting tool for the patient's data")} else {
          questListData
          }

         }
        })

  

     treeStructure =  reactive({
       makeTreeStructure(questListData())
     })

     output$tree = renderTree({
       treeStructure = treeStructure()
       treeStructure
     })


      questListFiltered <- reactiveVal()

    treeCurrentChoice = eventReactive(input$submit, { # trigger
             print("treeCurrentChoice")
             treeSelection = get_selected(input$tree, format = "names") # input
             if (is_empty(treeSelection)){showNotification("Please select at least one item")}
             if (!is_empty(treeSelection)) {
             treeCurrentChoice = treeSelection %>% lapply(attr, "ancestry") %>%
                 map(function(x){append(x, rep(NA, times = 2- length(x))) }) %>%
                 dplyr::bind_cols() %>%
                 transpose() %>%
                 cbind(treeSelection %>% map(pluck, 1) %>% dplyr::bind_cols() %>% transpose) %>% na.omit
                 colnames(treeCurrentChoice) = c("questionnaireName", "assessmentName", "variable")

                 treeCurrentChoice}
           })

    observeEvent(treeCurrentChoice(), {
        qf = questListData() %>% map(function(x){x[["values"]] %>% inner_join(treeCurrentChoice())})
        qustLstFlt = questListData()

        for (i in 1:length(qustLstFlt)){
            qustLstFlt[[i]][["values"]] <- qf[[i]]
        }

        qustLstFlt =  discard( qustLstFlt, function(x) {nrow(x[["values"]]) < 1 })
        questListFiltered(qustLstFlt)
    })


dat <- reactive({
                q = questListFiltered()
                req(q)
                g =  map(questListFiltered(), ~  severityPlot(.x))
                for (i in 1:length(q)){
                q[[i]][["plot"]] <- g[[i]]}
                print("1")
                q # when values or plot is missing, then set to null?

              })


       observe({
          if (length(questListFiltered()) == 0 | is.null(questListFiltered())) {
                 print("Prevent this from running if questListFiltered is not there?")
               } else {

         imap(dat() , ~{
           p =  .x[["plot"]]
           output_name <- paste0("plot_", .y)


           output[[output_name]] <- renderPlot(p)

         })
         }
       })
      #
      # # use renderUI to create a dynamic number of output ui elements
       output$boxes <- renderUI({
         req(dat())
         plots_list <- imap(dat(), ~{
           div(class= "box" , tagList(
             h3(.x[["values"]]$topic[1]),
             plotOutput(outputId = paste0("plot_", .y), width = "600px"),
             br(),

             if(any(!is.na(.x[["values"]]$Warning)) ){
             div(class = "warning" ,renderTable(.x[["values"]] %>%
                           select(assessmentName, Warning) %>%
                           drop_na() %>% rename(Time = assessmentName),
                         width = "600px"))} ,

             div(renderTable(.x[["values"]] %>%
                           select(assessmentName, Assessment, Recommendation) %>%
                           drop_na() %>% rename(Time = assessmentName),
                                  width = "600px"))))
         })

         tagList(plots_list)

       })
      
      
     
        
  }

# App ---- 

shinyApp(ui = ui, server = server)
