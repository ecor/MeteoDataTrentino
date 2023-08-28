NULL
#' Get data for Trentino Weather Station 
#' 

#' @param url URL with data. Default is \code{"https://dati.meteotrentino.it/service.asmx/tuttiUltimiRilieviNeve"}. 
#' @param url_metadata URL with data.  Default is \code{"https://dati.meteotrentino.it/service.asmx/listaCampiNeve"}. 
#' @param tz time zone. Default is \code{Etc?GMT-1}. 
#' @param smet logical value. If it is \code{TRUE}, data are returned as a list of \code{\link{smet-class}} object(s). 
#' @param header.fields header fields for the returened \code{\link{smet-class}} objects. See \code{\link{as.smet}}. It is used in case \code{smet==TRUE}.
#' @param station_id_prefix string added to the station ID as a prefix. It distinguishes \url{www.meteotrentino.it} snow stations from the ones of other weather services. 
#' 
#' 
#' 
#' @param ... further arguments
#' 
#' @export 
#' 
#' @author Emanuele Cordano
#' 
#' @details The data are licensed as Open Data and released by Provincia Autonoma di Trento (\url{www.meteotrentino.it}) through \url{https://dati.trentino.it/dataset/dati-recenti-dei-campi-neve}. Please see the link for major details.  
#' @examples 
#' 
#' 
#' ##out <- getSnowData()
#' 
#' ### LEGEND (AINEVA) : https://www.meteotrentino.it/neve-ghiacci/Husky/mod1/legenda-mod1.pdf
#' 
#' 
#' 
#' 

getSnowData <- function(url="https://dati.meteotrentino.it/service.asmx/tuttiUltimiRilieviNeve",
		url_metadata="https://dati.meteotrentino.it/service.asmx/listaCampiNeve",
		header.fields=c("station_id","station_name","station_shortname","altitude","location","longitude","latitude"),
		station_id_prefix="TRENTINO_",
		smet=TRUE,tz="Etc/GMT-1",...) {
	
	out <- readLines(url)
	
	
	if (!isXMLString(out)) {
		
		message <- sprintf("URL:%s does not contain an XML file!",url)
		warning(message)
		
		return(out)
		
		
		
	}
	
	out <- xmlTreeParse(out, asText=TRUE)
	
	out <- xmlChildren(xmlRoot(out))
	
	out <- lapply(out,FUN=xmlChildren)
	
	out <- lapply(X=out,FUN=function(t){ lapply(X=t,FUN=xmlValue)})
	
	
	out <- lapply(X=out,FUN=unlist)
	
	nn <- unique(unlist(lapply(X=out,FUN=names)))
	
	
	out <- lapply(X=out,FUN=function(t,nn){
			
				t[!(names(t) %in% nn)] <- as.character(NA)
				t <- t[nn]
				return(t)
				
			},nn=nn)
	
	
	out <- do.call(what=rbind,args=out)
	
	rownames(out) <- NULL
	out <- as.data.frame(out,stringsAsFactors=FALSE)
	
	if (smet==TRUE)  {
		
		
		names(out)[names(out)=="codStaz"] <- "station_id"
		names(out)[names(out)=="hs"] <- "HS"
		names(out)[names(out)=="ta"] <- "TA"
		names(out)[names(out)=="t10"] <- "TSS10"
		names(out)[names(out)=="t30"] <- "TSS30"
		names(out)[names(out)=="cs"] <- "CS"
		names(out)[names(out)=="b"] <- "B"
		names(out)[names(out)=="s"] <- "S"
		names(out)[names(out)=="hn"] <- "HN"
		names(out)[names(out)=="ww"] <- "WW"
		names(out)[names(out)=="n"] <- "N"
		names(out)[names(out)=="pr"] <- "PR"
		names(out)[names(out)=="v"] <- "V"
		names(out)[names(out)=="vq1"] <- "VQ1"
		names(out)[names(out)=="vq2"] <- "VQ2"
		names(out)[names(out)=="tmin"] <- "TAMIN"
		names(out)[names(out)=="tmax"] <- "TAMAX"
		names(out)[names(out)=="fi"] <- "FI"
		timestamp <- as.POSIXct(out[,names(out)=="dataMis"],tz=tz,format="%d/%m/%Y %H:%M:%S") 
		
		time_ <- sprintf("%04d",as.numeric(out[,names(out)=="oraDB"]))
		timestamp <- paste(as.character(timestamp,format="%Y-%m-%d"),time_,sep=" ")
		
		timestamp <- as.POSIXct(timestamp,tz=tz,format="%Y-%m-%d %H%M") 
		
		out$timestamp <- timestamp
		
		out <- out[,!(names(out) %in% c("dataMis","oraDB"))]
		
		fields <- unique(c("timestamp",names(out)))
		
		out <- out[,fields]
		
		mult <- array(1,length(fields))
		offset <- array(0,length(fields))	
		names(mult) <- fields
		names(offset) <- fields
		mult[c("HS","HN")] <- 0.01
		offset[c("TA","TSS10","TSS30","TSS10","TAMAX","TAMIN")] <- 273.15
		
		####out <- str_replace(out,"/","")
		for (it in fields[!(fields %in% c("station_id","timestamp"))]) {
		
			
			vvv <- str_replace_all(out[,it],"/","")
			vvv[vvv==""] <- as.character(NA)
			
			vvv <- as.numeric(vvv)
			out[,it] <- vvv
			
			
			
			
		}
		
		###ADD METADATA
		metadata <- getMetaDataTrentino(url=url_metadata,return.type="data.frame")
		
		header.fields <- unique(c("station_id",header.fields))
		metadata <- metadata[,header.fields]
		out <- merge(out,metadata,by="station_id")
		
		out$station_id <- paste(station_id_prefix,out$station_id,sep="")
		
		
		out$data_aineva_glossary <- "http://www.meteotrentino.it/neve-ghiacci/Husky/mod1/legenda-mod1.pdf"
		out$data_source_url <- url 
		out$metadata_source_url <- url_metadata
		
		header.fields <- c(header.fields,"data_aineva_glossary","data_source_url","metadata_source_url")
		
		out <- as.smet(out,header.fields=header.fields,mult=mult,offset=offset)
		
		
		
		###
		
		
		
		
		
		
	}
	
	return(out)
	
	
}