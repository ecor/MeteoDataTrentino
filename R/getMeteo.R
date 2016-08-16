
## http://dati.meteotrentino.it/service.asmx/listaStazioni
NULL
#' Get data for Trentino Weather Station 
#' 
#' @param station ID code 
#' @param url URL with data. Default is \code{"http://dati.meteotrentino.it/service.asmx/ultimiDatiStazione?codice=T0179"}. 
#' @param verbose verbose logical argument
#' @param tz time zone. Default is \code{Etc/GMT-1}. 
#' @param smet logical value. If it is \code{TRUE}, data are returned as a list of \code{\link{smet-class}} object(s). 
#' 
#' 
#' 
#' @param ... further arguments
#' 
#' @export 
#' 
#' @author Emanuele Cordano
#'
#' @details The data are licensed as Open Data and released by Provincia Autonoma di Trento (\url{www.meteotrentino.it}) through \url{http://dati.trentino.it/dataset/dati-recenti-delle-stazioni-meteo}. Please see the link for major details. 
#' 
#' @importFrom RSMET as.smet
#' 
#' @examples 
#' 
#' 
#' metadata <- getMetaDataTrentino(return.type="list")
#' 
#' nn <- names(metadata)[!(names(metadata) %in% c("T0365"))]
#' 
#' # Please uncomment the following run to run the command
#' # data <- getMeteoDataTrentino(station=metadata[nn])
#' #
#' 
#' datap <- getMeteoDataTrentino(station=metadata[["T0153"]])
#' 
#' 
#' 

######## DA TRADURRE GI HEADER!!!!!!!!!!!!!
#
#readLines(link)

getMeteoDataTrentino <- function(station=c("T0179","T0175"),url="http://dati.meteotrentino.it/service.asmx/ultimiDatiStazione?codice=TCODE",verbose=TRUE,tz="Etc/GMT-1",smet=TRUE,...) {
	
	####
	if (length(station)>1) {
		
		names(station) <- station
		
		out <- lapply(X=station,FUN=getMeteoDataTrentino,verbose=verbose,tz=tz,smet=smet,url=url,...)
		
		return(out)
		
	}
	
	
	
	url <- str_replace(url,"TCODE",station)
	if (verbose==TRUE)  {
		
		msg <- sprintf("Importing %s ...  url: %s",station,url)
		message(msg)
	}
	
	
	
	
	#####
	
	
	
	
	
	
	###print(url) print
	out <- try(readLines(url),silent=TRUE)
	####print(out)
	
	if (!isXMLString(out)) {
		
		message <- sprintf("URL:%s does not contain an XML file!",url)
		warning(message)
		
		return(out)
		
		
		
	}
	
	
	
	out <- try(xmlTreeParse(out, asText=TRUE),silent=TRUE)
	
	if (class(out)=="try-error") {
		
		return(out)
	}
	## http://dati.meteotrentino.it/service.asmx/listaStazioni
	
#	main_node <- out$doc$children[[1]]
	main_node  <-xmlRoot(out)
	
	if (class(main_node)=="try-error") {
		
		return(out)
	}
	##XMLappy(main_node,FUN=function(x){x})
	##XMLChidren(main_node)
	
	outxml <- xmlApply(main_node,FUN=xmlChildren)
	
	
	out <- lapply(X=outxml,FUN=function(x) {
				
				o <- lapply(X=x,FUN=function(x) {
						
							
							o <- xmlChildren(x)
							
							o <- lapply(X=o,FUN=xmlValue)
							attr(o,"unit") <- (xmlAttrs(x))
							
							names(o)[names(o)!="data"] <- paste(names(o)[names(o)!="data"],xmlAttrs(x),sep="___")
							
							return(o)
					})
				#attr(o,"unit") <-  xmlAttrs(x)  
				##lapply(x,xmlAttrs)			
				return(o)
			
			})
	
	
	
	
	## Each node has the same chidren !!!!
	
	##chidrenNames <- name(outxml[[1]])
	
	
	##out <- (lapply(X=outxml,FUN=function(it) {lapply(X=it,FUN=xmlValue)}))
	
	
	nl <- unlist(lapply(X=out,FUN=function(x) {
				
				o <- length(x)
			    if (o>0) {
					o <- length(x[[1]])					
					
				}
				
				o <- (o>0)
			}
	
	))
	
	out <- out[nl]
	
	if (length(out)==0) {
		
		warning("No data found, function  returns NULL!")
		out <- NULL
		return(out)
		
		
	}
	#####
	
	out <- lapply(X=out,FUN=function(x) {
				
				out <- lapply(X=x,FUN=as.data.frame,stringsAsFactors=FALSE)
				
				
				out <- do.call(rbind,out)
				
				return(out)
				
				
			})
	
	
	merged_out <- out[[1]]

	for (it in out[-1]) {
		
		merged_out <- merge(merged_out,it)
		
		
	}
	
#	###########3
#	#str(data$T0153)
#	#'data.frame':	128 obs. of  6 variables:
#			$ data            : chr  "2016-03-17T00:00:00" "2016-03-17T00:15:00" "2016-03-17T00:30:00" "2016-03-17T00:45:00" ...
#	$ temperatura___.C: chr  "4.3" "4.3" "4.2" "4.2" ...
#	$ pioggia___mm    : chr  "0" "0" "0" "0" ...
#	$ v___m.s         : chr  "0.3" "1.4" "0" "0.4" ...
#	$ d___gN          : chr  "280" "289" "46" "301" ...
#	$ rsg___W.mq      : chr  "0" "0" "0" "0" ...
#	> 
#			
#	
#	
#	#############
	
	out <- merged_out
	
	
	if (class(out)=="data.frame") { 
	
		names(out)[names(out)=="temperatura___.C"] <- "TA" ###deg Celsius
		names(out)[names(out)=="v___m.s"] <- "VW"
		names(out)[names(out)=="d___gN"] <- "DW"
		names(out)[names(out)=="rsg___W.mq"] <- "ISWR"
		names(out)[names(out)=="data"] <- "timestamp"
		names(out)[names(out)=="pioggia___mm"] <- "PINT"
		
		
		
		nn <-  c("TA","PINT","VW","DW","ISWR")    
		
		nn <- names(out)[names(out) %in% nn] 
		
		for (nt in nn) {
			
			out[,nt] <- as.numeric(out[,nt])
			
			
		}
		
		format_date <- "%Y-%m-%dT%H:%M:%S" ### "2016-03-20T00:00:00"
		out[,names(out)=="timestamp"] <- as.POSIXct(out[,names(out)=="timestamp"],tz=tz,format=format_date)
		
		dt <- c(NA,as.numeric(diff(out$timestamp),units="hours"))
		
		if ("PINT" %in% names(out)) {
			
			out[,names(out)=="PINT"] <- out[,names(out)=="PINT"]/dt
		}
		
		if ((smet==TRUE) & (0 %in% dim(out))) {
			
			out <- NULL
			
		} else if (smet==TRUE) {

					out <- out[,(names(out) %in% c(nn,"timestamp"))]
					
					
				
					header  <- as.list(attr(station,"header"))
				
					header[["station_id"]] <- as.character(station)
					header[["station_url"]] <- url
					
					
					attr(out,"header") <- header 
					mult <- array(1,ncol(out))
					offset <- array(0,ncol(out))
					
					names(mult) <- names(out)
					names(offset) <- names(out)
					
					
					offset[names(offset)=="TA"] <- 273.15
					
				
					
					out <- as.smet(out,mult=mult,offset=offset)
		}
 		
	}
	
	
	
	
	return(out)
	
}



