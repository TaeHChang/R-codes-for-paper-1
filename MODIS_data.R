library(devtools)
install_github("lbusett/MODIStsp")

install.packages("MODIStsp")
library(MODIStsp)

install.packages(c("leaflet", "shiny","shinydashboard","shinyFiles",
                   "shinyalert", "rappdirs","shinyjs",
                   "leafem", "mapedit", "magrittr"))

MODIStsp()

library(doParallel)
cl<-makeCluster(2)
registerDoParallel(cl)
detectCores()
registerDoParallel(cores=30)
getDoParWorkers()

install.packages("curl")
library(curl)







