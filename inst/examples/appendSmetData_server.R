#!/usr/bin/env Rscript
# file appendSmetData.R
#
# This file contains instructions for creating SMET files with weather data of Trentino
#
# author: Emanuele Cordano on 21-05-2015

#This program is free software: you can redistribute it and/or modify
#it under the terms of the GNU General Public License as published by
#the Free Software Foundation, either version 3 of the License, or
#(at your option) any later version.
#
#This program is distributed in the hope that it will be useful,
#but WITHOUT ANY WARRANTY; without even the implied warranty of
#MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#GNU General Public License for more details.
#
#You should have received a copy of the GNU General Public License
#along with this program.  If not, see <http://www.gnu.org/licenses/>.

###############################################################################
options(warn=1)

rm(list=ls())


library(MeteoDataTrentino)
library(RSMET)




oldsmetdir <- '/home/ecor/local/MeteoDataTrentino/inst/smet' 
###oldsmetdir <- '/home/ecor/Dropbox/R-packages/MeteoDataTrentino/inst/smet' 
appendsmet_dir <- oldsmetdir ###'/home/ecor/Dropbox/R-packages/MeteoDataBayern/inst/a_smet'

###
###ll <- getMeteoDataTrentino(station = metadata[nn[!(nn %in% c("T0473","T0404"))]]


####

metadata <- getMetaDataTrentino(return.type="list")

nn <- names(metadata)###[!(names(metadata) %in% c("T0365","T0473","T0404"))]


newsmet <- getMeteoDataTrentino(station=metadata[nn])
newsmet <-  newsmet[!sapply(X=newsmet,is.null)]
newsmet <-  newsmet[sapply(X=newsmet,FUN=function(x){class(x)=="smet"})]



oldsmetfiles <- list.files(oldsmetdir,pattern=".smet",full.name=TRUE)



if (length(oldsmetfiles)>0) {
	
#	oldsmet <- lapply(X=oldsmetfiles,FUN=function(x) {
#				print(x) 
#				as.smet(x)
#			})
	
	oldsmet <- lapply(X=oldsmetfiles,FUN=as.smet)
	names(oldsmet) <- sapply(X=oldsmet,FUN=function(x){x@header$station_id})

} else {
	
	oldsmet <- newsmet
	
	
}



names_n <- intersect(names(newsmet),names(oldsmet))
names_u <- union(names(newsmet),names(oldsmet))
names_diff <- names_u[!(names_u %in% names(oldsmet))]


oldsmet[names_diff] <- newsmet[names_diff]

names_n <- intersect(names(newsmet),names(oldsmet))

newsmet <- newsmet[names_n]
oldsmet <- oldsmet[names_n]






appendsmet <- mapply(FUN=collapse.smet,x=newsmet,y=oldsmet)
names(appendsmet) <- sapply(X=appendsmet,FUN=function(x){x@header$station_id})
# The new appended smet was created !!
# They are written in the following directory: (please modify as youor purpose)

appendsmet <- lapply(X=appendsmet,FUN=function(x,dir) {
			print(x@header$station_id)
			x@file <- sprintf("%s/%s.smet",dir,x@header$station_id)
			print(x,file="internal")
			
		},dir=appendsmet_dir)









