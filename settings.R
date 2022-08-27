# Url of graphql API of MIHRA (container-name and port)
url = "mhira-backend:3000/graphql" 

# App closes after x seconds of inactivity
timeoutSeconds = 10*60  
 
# in case no language is found from local storage(browser)
defaultLang = "en"

# Plot shows time or discrete session names on x-axis when multiple assessments were made  
TimeOnXAxis = T

# Subscales in interpretation table show one scale per row or are aggregated
showScale = F

# Show this patient information. Remove those that should not be shown.
selectPatientInfo = c(
        'medicalRecordNo',
        'initials',
        #'firstName',
        #'middleName',
        #'lastName',
        #'birthDate',
        'age',
        'gender')
