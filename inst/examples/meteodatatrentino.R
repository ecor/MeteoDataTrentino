# TODO: Add comment
# 
# Author: ecor
###############################################################################


library(MeteoDataTrentino) 

source('/home/ecor/Dropbox/R-packages/MeteoDataTrentino/R/getMeteo.R')

metadata <- getMetaDataTrentino(return.type="list")
 
nn <- names(metadata)[!(names(metadata) %in% c("T0365"))]
nn <- "T0094" ###"T0154"
#' # Please uncomment the following run to run the command
data <- getMeteoDataTrentino(station=metadata[[nn]])
#' #
#' ## T0363
