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
source("utility_functions/groupCutoffs.R")

#Setting

# APP SETTINGS ---------------------------------------------------------------- 

if(!file.exists("settings.R")){
  source("settings-default.R")} else {
    source("settings.R")} # To customise settings, please create settings.R

patientId = 1 # patient_id can be found from the URL when clicking a report on the patient detail view in MHIRA

# LOAD DATA -------------------------------------------------------------------


token = getToken(Username = "yourUsername", Password = "yourPassword")

response = getPatientReport(token = token, patientId = patientId, url = url)

simplifiedData = simplifyPatRep(response)
data = simplifiedData

questionnaireScripts = response$data$generatePatientReport$questionnaireScripts  

cutoffs = extract_cutoffs(questionnaireScripts = questionnaireScripts)

cutoffs = groupCutoffs(cutoffs = cutoffs)

scales = calculateScales(
  simplifiedData = data,
  questionnaireScripts =  questionnaireScripts)

scales = applyCutOffs(scales = scales, cutoffs = cutoffs) 


# Plot

cutoffs_single = cutoffs %>% filter(questionnaireVersionId == "62d7bb7f960d740024c5de7e")
scales_single = scales %>% filter(questionnaireVersionId == "62d7bb7f960d740024c5de7e")

plots = severityPlot(scales_single, cutoffs_single)

lapply(plots[2], function(p){p})

# Single items

questionnaireVersionId = "62d7bb7f960d740024c5de7e"
printItemTable(data, questVersionId = "62d7bb7f960d740024c5de7e")