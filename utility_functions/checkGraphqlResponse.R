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
    transMatrix["noPatient", lang()],
    type = "error",
    duration = 20)
  session$close()}

if(is_empty(response$data$generatePatientReport$assessments)){
  showNotification(
    transMatrix["noAssessment", lang()],
    type = "error",
    duration = 20)
  session$close()}

if(is_empty(response$data$generatePatientReport$answeredQuestionnaires)){
  showNotification(
    transMatrix["noQuestionnaire", lang()],
    type = "error",
    duration = 20)
  session$close()}}