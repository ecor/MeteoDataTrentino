## http://dati.meteotrentino.it/service.asmx/listaStazioni
NULL
#' Get metedata for Trentino Weather Station 
#' 
#' 
#' @param url URL with data. Default is \code{"http://dati.meteotrentino.it/service.asmx/listaStazioni"}. 
#' @param header string vectors of station metadata, used if \code{return.type=="list"}.
#' @param return.type character string indicating the requested type for the function output. Default is \code{"data.frame"}.
#'  
#' @import stringr XML
#' @export 
#'
#' @details The data are licensed as Open Data and released by Provincia Autonoma di Trento (\url{www.meteotrentino.it}) through \url{http://dati.trentino.it/dataset/anagrafica-stazioni-meteo-stazioni-automatiche} and \url{http://dati.trentino.it/dataset/anagrafica-campi-neve}. Please see the links for major details. 
#' @author Emanuele Cordano
#' 
#' @importFrom sp 'coordinates<-'
#' @importFrom sp 'proj4string<-'
#' @importFrom sp spTransform
#' @importFrom sp CRS
#' 
#' 
#' @examples 
#' 
#' # WEATHER STATIONS
#' url <-  "http://dati.meteotrentino.it/service.asmx/listaStazioni"
#' metadata <- getMetaDataTrentino(url=url)
#' metadata_l <- getMetaDataTrentino(url=url,return.type="attributed.list")
#' 
#' # SNOW MEASUREMENT STATIONS 
#' 
#' url_snow <- "http://dati.meteotrentino.it/service.asmx/listaCampiNeve"
#' metadata_snow <- getMetaDataTrentino(url=url_snow)
#' metadata_snow_l <- getMetaDataTrentino(url=url_snow,return.type="attributed.list")
#' 
#' ### raw_out_data
#' 


getMetaDataTrentino <- function(url="http://dati.meteotrentino.it/service.asmx/listaStazioni",return.type=c("data.frame","attributed.list","list"),header=c("latitude","longitude","station_name","station_shortname","altitude")) {
	
	
	out <- readLines(url)
	
	
	if (!isXMLString(out)) {
		
		message <- sprintf("URL:%s does not contain an XML file!",url)
		warning(message)
		
		return(out)
		
		
		
	}
	
	
	out <- xmlTreeParse(out, asText=TRUE)

#	main_node <- out$doc$children[[1]]
	main_node  <-xmlRoot(out)
	##XMLappy(main_node,FUN=function(x){x})
	##XMLChidren(main_node)
	
	outxml <- xmlApply(main_node,FUN=xmlChildren)
	
	## Each node has the same chidren !!!!
	
	##chidrenNames <- name(outxml[[1]])

	
	out <- (lapply(X=outxml,FUN=function(it) {lapply(X=it,FUN=xmlValue)}))
	
###	
	 return.type <- return.type[1]
	 
     cond <- return.type %in% c("data.frame","attributed.list","list")

	 cond <- cond | str_detect(return.type,"data.frame")
	 cond <- cond | str_detect(return.type,"list")
	 
	 
	if (cond ==TRUE) {
		
		names.col <- names(out[[1]])
		
		ncols <- unlist(lapply(X=out,FUN=length))
		if  (any(ncols!=mean(ncols))) {
			
			stop("Error in transformation into data.freme: check original XML format data!")
			
		} else {
			
			ncol <- ncols[1]
		}
		
		###print(ncol)
		
	
		
		nn <- names(out[[1]])
		
		out <- lapply(X=out,FUN=function(x,nn) { 
				
				o <- x[nn]
				
				for (it in nn) {
					
					if (length(o[[it]])==0) o[[it]] <- 'none'
					
					
				}
				
				return(o)
			},nn=nn)
		
		out <- as.data.frame(matrix(unlist(out),byrow=TRUE,ncol=ncol),stringsAsFactors=FALSE)
		names(out) <- names.col
		
	
		
		names(out)[names(out)=="quota"] <- "altitude"
		names(out)[names(out)=="latitudine"] <- "latitude"
		names(out)[names(out)=="longitudine"] <- "longitude"
		names(out)[names(out)=="utmlat"] <- "x"  #### errore di OPEN DATA TRENTINO!!!!
		names(out)[names(out)=="est"] <- "x"
		names(out)[names(out)=="utmlon"] <-  "y"  #### errore di OPEN DATA TRENTINO!!!!
		names(out)[names(out)=="north"] <- "y"

		names(out)[names(out)=="codice"] <- "station_id"
		names(out)[names(out)=="nome"] <- "station_name"
		names(out)[names(out)=="nomebreve"] <- "station_shortname"
		
		
		if (c("station_name") %in% names(out)) out$location <- out$station_name
		
		numeric_field <- c("longitude","latitude","x","y","altitude")
		numeric_field <- numeric_field[numeric_field %in% names(out)]
	
		
		##outx <<- out
		
		
		###
		
		for (it in numeric_field) {
			
			
			out[,it] <- as.numeric(out[,it])
		}
	
		
		
		### CRSs
		
		crs_latlon  <- CRS("+proj=longlat +datum=WGS84")
		
		crs_utm <- CRS("+proj=utm +zone=32 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
		
		
		###
		
		cond <- all(c("x","y") %in% names(out))
		cond <- cond & !all(c("latitude","longitude") %in% names(out))
		### EC 20181102
		il <- which(abs(out$latitude)>90)
		iv <- which(abs(out$longitude)>180)
		xx <- out$longitude[iv]
		yy <- out$latitude[il] 
		xx <- xx*10^(-trunc(log(abs(xx))/log(10)))*10
		yy <- yy*10^(-trunc(log(abs(yy))/log(10)))*10
		out$longitude[il] <- xx
		out$latitude[iv] <- yy
		
		##### END EC 20181102
		
		###outx <<- out
		
		
		if (cond==TRUE) {
			
			out$y_ <- out$y
			out$x_ <- out$x
			
			coordinates(out) <- ~ x_+y_
			proj4string(out) <- crs_utm
			##
			##
			##out2 <<- out
			##
			out <- spTransform(x=out,CRSobj=crs_latlon)
			
			out <- as.data.frame(out)
		
			names(out)[names(out)=="x_"] <- "longitude"
			names(out)[names(out)=="y_"] <- "latitude"
			
			
		}
		
		
		###return(out)
		
		
		if (all(c("longitude","latitude","x","y") %in% names(out))==TRUE) {
			
			out$y_ <- out$latitude
			out$x_ <- out$longitude
			str(out)
			coordinates(out) <- ~ x_+y_
			
			
			proj4string(out) <- (crs_latlon)
			out2 <<- out
			## REMEVA ALL <<- 
			## SEE: https://stackoverflow.com/questions/50372533/changing-crs-of-a-sf-object
			out <- spTransform(x=out,CRSobj=(crs_utm))
			
			out <- as.data.frame(out)
			
			err_x <- abs(out$x-out$x_)
			err_y <- abs(out$y-out$y_)
			
			
			out$err <- (err_x^2+err_y^2)^0.5 
			emx <- (max(err_x,na.rm=TRUE))
			emy <- (max(err_y,na.rm=TRUE))
			
			prec <- 0.5
			ll <- (which(out$err>prec)) 
			####
			message(sprintf("Maximum error coordinate: %f along x and %f along y!",emx,emy))
			if (length(ll)>0)  {
				
				print(out[ll,])
				msg <- sprintf("In the above station, error exceeds %f meters! ",prec)
				warning(msg)
				
				
				
			}
			
	
		}
	  
		cond <- return.type %in% c("attributed.list","list")
		
		cond <- cond | str_detect(return.type,"list") 
		
		
		if (cond==TRUE) {
			
			
			ll <- as.list(out$station_id)
			names(ll) <- out$station_id

		
			##out <- 
			
			
			
			for (i in 1:length(ll)) {
				
				
				attr(ll[[i]],"header") <-  out[i,names(out) %in% header]
				#attr(ll[[i]],"latitude") <- out$latitude[i]
				#attr(ll[[i]],"longitude") <- out$longitude[i]
				#attr(ll[[i]],"station_name") <- out$station_name[i]
				#attr(ll[[i]],"station_shortname") <- out$station_shortname[i]
				
				
				#####
				#####
				#####
				#attr(out,"latitude") <- attr(station,"latitude")
				#attr(out,"longitude") <- attr(station,"longitude")
				#attr(out, "altitude") <- attr(station,"longitude")
				#attr(out, "station_name") <- attr(station,"station_name")
				#attr(out, "station_shortname") <- attr(station,"station_shortname")
				#attr(out, "location") <- attr(station,"station_name")
				######
				######
				
				
				
				
				
				
			}
			
			
			out <- ll
			
		}
		
		
	}
	
	return(out)
	
	
	
}





