#library(wesanderson)
library(RColorBrewer)

source("graphql_functions/getToken.R")
source("graphql_functions/getMhiraData4.R")
source("graphql_functions/getMhiraAssessments1.R")


 token = getToken("User", "Password")


patientFilter = 'patient:{id:{eq:1}}'
dfAss = getMhiraAssessments1(token,patientFilter )

assessmentIds = dfAss$id

data = getMhiraData4(token, assessmentIds)


source("instrumentLopf97Ger3.R")
source("instrumentPhq9En3.R")

questListData = instrumentLopf97Ger3(data)
questListData = instrumentPhq9En3(data)



newplot = function(questListData){
ggplot() +
  geom_point(data = questListData[["values"]], aes(x = assessmentName, y = value, shape = variable), size = 3.5) +
  geom_line(data = questListData[["values"]], aes(x = assessmentName, y = value, group = variable), alpha = 0.3, lwd = 2) +
  geom_rect(data = questListData[["cut_offs"]],
            aes(xmin = 0, ymin = low_cut, xmax = length(unique(questListData[["values"]]$assessmentName)) +1 , ymax = high_cut , fill = level),
            alpha = 0.3 ) +
  scale_fill_brewer(type = "seq", palette = 7) +
  ylab("value") +
  xlab("") +
  ggtitle((unique(questListData[["values"]]$questionnaireName))) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))} 



