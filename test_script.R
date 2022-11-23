### THIS SCRIPT ALLOWS YOU TO SIMULATE THE DATA FLOW OF THE SHINY APP
### IT IS USEFUL FOR TESTING NEW ELEMENTS


# Load functions

setwd("/home/mhira/shiny_apps/patient-report/") # make sure you are in the app folder, else source files will not be found

source("graphql_functions/getToken.R")
source("graphql_functions/getPatientReport.R")
source("graphql_functions/getUserProfile.R")
source("utility_functions/simplifyPatRep.R")
source("utility_functions/calculateScales.R")
source("utility_functions/applyCutOffs.R")
source("utility_functions/severityPlot.R")
source("utility_functions/interpretTable.R")
source("utility_functions/extract_cutoffs.R")

#Setting
patientId = 1 # patient_id can be found from the URL when clicking a report on the patient detail view in MHIRA
token = getToken(Username = "yourUsername", Password = "yourPassword")

response = getPatientReport(token = token, patientId = patientId, url = url)

simplifiedData = simplifyPatRep(response)
data = simplifiedData

questionnaireScripts = response$data$generatePatientReport$questionnaireScripts


scales = calculateScales(
  simplifiedData = simplifiedData,
  questionnaireScripts = response$data$generatePatientReport$questionnaireScripts)


cutoffs = extract_cutoffs(questionnaireScripts = questionnaireScripts)


applyCutOffs(scales = data, cutoffs = cutoffs) 

