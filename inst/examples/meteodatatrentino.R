# TODO: Add comment
# 
# Author: ecor
###############################################################################


library(MeteoDataTrentino) 


metadata <- getMetaDataTrentino(return.type="list")
 
nn <- names(metadata)[!(names(metadata) %in% c("T0365"))]
 
#' # Please uncomment the following run to run the command
data <- getMeteoDataTrentino(station=metadata[nn])
#' #


