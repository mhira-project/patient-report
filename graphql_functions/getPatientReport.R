# Load packages ----
library(tidyverse)
library(jsonlite)
#library(tidyjson)
library(data.table)


getPatientReport = function(token, patientId){
  print("getPatientsReport")
  
  # Load functions ----
  source("graphql_functions/GQL2.R")
  source("graphql_functions/generatePatientReportQuery.R")
  source("graphql_functions/getUserProfile.R")
  
  #-- Settings ----
  source("settings.R")
  
  # GraphQL request to get patient list ----
  query = generatePatientReportQuery(patientId)
  response = GQL2(query, .token = token, .url = url) %>%
    fromJSON()
  
  
  # Log - 
  if(FALSE){ #
  profile = getUserProfile(token)
  Log =  data.frame(systemTime = Sys.time(),
                    username = profile$username,
                    role = paste(as.character(unlist(profile$roles)), collapse = "/"),
                    patientId = patientId,
                    medicalRecordNo = response$data$generatePatientReport$patient$medicalRecordNo,
                    query = "getPatientReport", 
                    app_instance = getwd(),
                    token = token)
  
  if(!dir.exists("logs")){dir.create("logs")}
  
  fwrite(Log, file = "logs/patientAccessLog.csv", append = T)}
  
  
  return(response)
  }
