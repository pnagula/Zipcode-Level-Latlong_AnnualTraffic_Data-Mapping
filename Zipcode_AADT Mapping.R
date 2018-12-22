rm(list=ls())
library(rgdal)
library(proj4)
library(DataCombine)
library(rjson)

## CREATE PROJECTION STRING for reading shape files, make sure to update proj,zone and direction in the projection string as per your shapefile
proj4string="+proj=utm +zone=18 +north +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
## Read shape file
shps=readOGR('/$PATH/AADT_2015_tdv/AADT_2015_tdv.shp')

## extract latitude,longitude from shape file and insert rows into lat,long data frame
latlon=data.frame(lat=0.0,lon=0.0)
for(i in 1:nrow(shps)){
  #length(shps[i,]@lines[[1]]@Lines[[1]]@coords)/2
  xy=data.frame(x=shps[i,]@lines[[1]]@Lines[[1]]@coords[,1],y=shps[i,]@lines[[1]]@Lines[[1]]@coords[,2])
  pj <- project(xy, proj4string, inverse = T)
  df=data.frame(lat=pj$y, lon=pj$x)
  latlon <- InsertRow(latlon,df,nrow(latlon))
}
latlon=latlon[2:nrow(latlon),]

# Create new data frame with shape file data (AADT (Annual Average Daily Traffic Data) and  Lat,long )
fds=data.frame(latlon,shps@data)

# Aggregate AADT based on lat,long to create final dataset
final=sqldf('select lat,lon,sum(aadt) as aadt from fds group by lat,lon')
final=final[2:nrow(final),]

# Read Zip_Codes and Lat,long mapping JSON file
result <- fromJSON(file = "/$PATH/zip_codes_sorted.json")
ziplatlong=as.data.frame(result[1])
for(i in 2:length(result)){
  ziplatlong=InsertRow(ziplatlong,as.data.frame(result[i]),nrow(ziplatlong))
}

# apply KNN to assign zipcodes to lat,long in final data frame which has AADT information, tune K value as per your need
library(class)
mdl=knn(ziplatlong[,2:3],final[,1:2],ziplatlong$zip_code,k=1)
final$zipcode=mdl

# Aggregate AADT based on zipcode , finally to get zipcode level AADT information
library(sqldf)
sbase=sqldf('select zipcode,sum(aadt) from final group by zipcode')

write.csv(sbase,'/$PATH/zipcode_traffic.csv',row.names=F)


