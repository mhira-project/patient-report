checkGraphqlResponse = function(response, session){


  
# Terminate session if response doesn't contain data     
if(is_empty(response$data)){
  showNotification(
    response$errors$message,
    type = "error",
    duration = 20)
  session$close()}

if(is_empty(response$data$generatePatientReport$patient)){
  showNotification(
    "No paitent was found. The reporting app will shut down.",
    type = "error",
    duration = 20)
  session$close()}

if(is_empty(response$data$generatePatientReport$assessments)){
  showNotification(
    "No assessment was found. Please make sure the patient has assessments. The reporting app will shut down.",
    type = "error",
    duration = 20)
  session$close()}

if(is_empty(response$data$generatePatientReport$answeredQuestionnaires)){
  showNotification(
    "No completed questionnaire was found. The reporting app will shut down.",
    type = "error",
    duration = 20)
  session$close()}

if(is_empty(response$data$generatePatientReport$questionnaireScripts)){
  showNotification(
    "No completed questionnaire was found. The reporting app will shut down.",
    type = "error",
    duration = 20)
  session$close()}
 
 if(is_empty(response$data$generatePatientReport$questionnaireScripts %>%
             filter(name == "scales_table" | name == "scales_function"))){
    showNotification(
      "No scale_script or scales_table found.
      You need to add these evaluation routines to your questionnaires",
      type = "error",
      duration = 20)
    session$close()}

  

  
  }