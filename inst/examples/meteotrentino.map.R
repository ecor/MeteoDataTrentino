#!/usr/bin/env Rscript
# file meteopiemonte.map.R
#
# This file contains illustrates a geographic map of the   weather station in  Trentino 
# 
#
#
# author: Emanuele Cordano on 25-05-2016

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




#!/usr/bin/env Rscript
# TODO: Add comment
# 
# Author: ecor
###############################################################################


##############################################################
rm(list=ls())

library(RSMET)
library(ggmap)

library(MeteoDataTrentino)




metadata <- getMetaDataTrentino(return.type="data.frame")

map <- get_map(location ="Trento", zoom = 9)

size=3


##data_latlon <- data_latlon[data_latlon$station_name=="FORMAZZA",]


gweather <- ggmap(map) +
		geom_point(data = metadata,aes(x = longitude, y = latitude),size=size,  alpha
						=1, color="blue",show.legend  = FALSE)

## Uncomment if you want to save in PDF format the otput of gsnow
## ggsave("test-map.pdf", gweather,width=10,height=10)
