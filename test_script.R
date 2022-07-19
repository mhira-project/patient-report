### THIS SCRIPT ALLOWS YOU TO SIMULATE THE DATA FLOW OF THE SHINY APP
### IT IS USEFUL FOR TESTING NEW ELEMENTS


# Load functions

setwd("shiny_apps/patient-report/") # make sure you are in the app folder, else source files will not be found

source("graphql_functions/getToken.R")
source("graphql_functions/getPatientReport.R")
source("graphql_functions/getUserProfile.R")
source("utility_functions/simplifyPatRep.R")
source("utility_functions/calculateScales.R")
source("utility_functions/applyCutOffs.R")
source("utility_functions/severityPlot.R")
source("utility_functions/interpretTable.R")

#Setting
patientId = 1 # patient_id can be found from the URL when clicking a report on the patient detail view in MHIRA
token = getToken(Username = "yourUserName", Password = "yourPassword")

response = getPatientReport(token = token, patientId = patientId)

simplifiedData = simplifyPatRep(response)
data = simplifiedData

questionnaireScripts = response$data$generatePatientReport$questionnaireScripts


data = calculateScales(
  simplifiedData = simplifiedData,
  questionnaireScripts = response$data$generatePatientReport$questionnaireScripts)


scales = applyCutOffs(scales = data, questionnaireScripts = questionnaireScripts) 

