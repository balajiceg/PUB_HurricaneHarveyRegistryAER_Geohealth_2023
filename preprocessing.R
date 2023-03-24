# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
#   Script name:MergeInundSviToHhr.R
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
#   Project name: Hurricane Harvey data merging 
#   
#   Program description:
#   
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
#   Data: 
#   
#   Restrictions:
#   
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
#   Outline of program/analysis:
#:
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
#   Author:Balaji Ramesh      Date: 28-Feb-2021
#
#   Revisions (by/date):
#
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
#   Input files:Innundation Distance, Texas SVI, Flood Ratio derived from DFO, Texas Flood registry (incl paper survey)
#      Fname		Ftype		Created by
#   
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
#   Output files:
#      Fname		Ftype		Notes
#       K:\\Projects\\FY2020-018_HHR_Outcomes\\EoFloodHealth\\Data\\Draft\\ProcessedTFR\\TFRHarveyRecordsInclPaperSurWithInundNSVI.csv
#       The output contains records from the orginal dataset(TFR) that belongs to Harvey strom alone. Along with it four
#       variables have been merged:
#       - reRankSVI, reRankSVI_T1,reRankSVI_T2, reRankSVI_T3 , reRankSVI_T4 - overall SVI and SVI themes (taken from Texas SVI) reranked to tracts in the dataset
#       - DFO_R200 - Flood ratio computed using DFO harvey product for each census tract and merged using census tract ID
#       - Inund_dist - The distance of the respondent from the nearest inundated area. 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #


#library(GISTools)
library(raster)
library(rgdal)
library(dplyr)
library(tidyverse)

#read TFR data
tfr_data<-read.csv('K:\\Library\\TX\\Health\\TFR\\Restricted\\Curated\\TFRgeo_20200401_v02.csv',stringsAsFactors = F)
tfr_data$Zip<- tfr_data$Zip %>% as.character
tfr_data_paper<-read.csv('K:\\Library\\TX\\Health\\TFR\\Restricted\\Curated\\PaperGeo_20200701_v01.csv', stringsAsFactors = F)
tfr_data_paper[,c('Zip','VehicleDamaged_Num3')]<- cbind(tfr_data_paper$Zip %>% as.character,
                                                        tfr_data_paper$VehicleDamaged_Num3 %>% as.character)

#merge both
tfr_data<-bind_rows(tfr_data,tfr_data_paper)

#subset records belonging to harvey event alone - 19993 records
hhr_data<-tfr_data[tfr_data$Storm=='Harvey',]

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Merge DFO inundation product using the created distance raster of inundation and tract level flood ratio
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#subset records with non null X, Y and form points  -> 20394 records
hhr_data_subset<-hhr_data[!is.na(hhr_data$X) & !is.na(hhr_data$Y),]
points_responses<-SpatialPoints(hhr_data_subset[,c('X','Y')],proj4string = CRS("+proj=longlat +datum=WGS84"))

#read distance raster -> distance of each pixel from the pixels that indicate flooding - from DFO inundation
flood_dist<-raster('K:\\Projects\\FY2020-018_HHR_Outcomes\\EoFloodHealth\\Data\\Draft\\InundDistRasterHarvey\\distHarvInund.tif')

#reproject the points df and extract flood distance for the records in the hhr subset
points_responses<-spTransform(points_responses,CRS(proj4string(flood_dist)))
hhr_data_subset$dfoInundDist<-extract(flood_dist,points_responses)


#join the flood ratio as per census tracts
hhr_data$tractID10<-hhr_data$GeoID10%/%10000
floodRatio<-readOGR(dsn='K:\\Projects\\FY2020-018_HHR_Outcomes\\EoFloodHealth\\Data\\InputData\\InundHarveyDFO\\FloodInun_AllJoined\\FloodInund_AllJoined_v1.gpkg')
hhr_data<-merge(hhr_data,floodRatio@data[,c('GEOID','DFO_R200')],by.x='tractID10',by.y='GEOID',all.x=T)



#merge FloodScan Inundation 
dis_inund_fscan<-raster("K:\\Projects\\FY2020-018_HHR_Outcomes\\EoFloodHealth\\Data\\InputData\\floodscan_hurricane_harvey_p00\\aer_mfed_acc_dist.tif")
depth_fscan<-raster("K:\\Projects\\FY2020-018_HHR_Outcomes\\EoFloodHealth\\Data\\InputData\\floodscan_hurricane_harvey_p00\\MFED\\aer_mfed_depth_3s_20170827-20170909_v05r01.tif")
ndays_fscan<-raster("K:\\Projects\\FY2020-018_HHR_Outcomes\\EoFloodHealth\\Data\\InputData\\floodscan_hurricane_harvey_p00\\MFED\\aer_mfed_ndays_3s_20170827-20170909_v05r01.tif")

#project to NAD83(2011) / Texas Centric Albers Equal Area
ndays_fscan<-projectRaster(ndays_fscan,crs=CRS("+init=epsg:6579"),method = 'ngb')
depth_fscan<-projectRaster(depth_fscan,crs=CRS("+init=epsg:6579"))

points_responses<-spTransform(points_responses,CRS("+init=epsg:6579"))

#inundataion disctance
hhr_data_subset$fScanInundDist<-extract(dis_inund_fscan,points_responses)
hhr_data_subset$fScanDepth<-extract(depth_fscan,points_responses)
hhr_data_subset$fScanNdays<-extract(ndays_fscan,points_responses)


#join it back to hhr_data
hhr_data<-merge(hhr_data,hhr_data_subset[,c('SurveyResponseID',"dfoInundDist","fScanInundDist", "fScanDepth","fScanNdays")],by='SurveyResponseID',all.x=T)

#join the flood scan flood ratio as per census tracts
floodRatio<-readOGR(dsn='K:\\Projects\\FY2020-018_HHR_Outcomes\\EoFloodHealth\\Data\\InputData\\floodscan_hurricane_harvey_p00\\censusTractsFloodScan_MFED\\censusTractsFloodScan_MFED.gpkg')
floodRatio$WmaxFloodRatio[is.na(floodRatio$WmaxFloodRatio)]=0
hhr_data<-merge(hhr_data,floodRatio@data[,c('GEOID','WmaxFloodRatio')],by.x='tractID10',by.y='GEOID',all.x=T)
hhr_data<-rename(hhr_data,'fScanMaxFloodRatio'='WmaxFloodRatio')


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Re-rank and merge SVI
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#read texas SVI
svi_tracts<-readOGR(dsn='K:\\Projects\\FY2020-018_HHR_Outcomes\\EoFloodHealth\\Data\\InputData\\Texas_SVI',layer = "TEXAS")
#subset only tracts in our data
svi_tracts<- subset(svi_tracts,svi_tracts$FIPS %in% hhr_data$tractID10)
#import reranking functions and rerank the SVI
source("K:\\Projects\\FY2020-018_HHR_Outcomes\\EoFloodHealth\\Code\\Draft\\Codes\\ReCalcSVI_func.R")
svi_tracts@data<-recalcSVI(svi_tracts)
#merge the reranked SVI to hhr_data
hhr_data<-merge(hhr_data,svi_tracts@data[,c('FIPS','reRankSVI','reRankSVI_T1','reRankSVI_T2','reRankSVI_T3','reRankSVI_T4')],by.x='tractID10',by.y='FIPS',all.x=T)

#remove variables that where not collected for Harvey
hhr_data<-subset(hhr_data,select=-c(EmoHealth_Impact,EmoHealth_Impact_other,EmoPrevRelated,Homeless,Imd,M19,PhysHealth_Impact_other,PhysHealth_Impact,PhysPresent))
#write output
write.csv(hhr_data,file='K:\\Projects\\FY2020-018_HHR_Outcomes\\EoFloodHealth\\Data\\Draft\\ProcessedTFR\\TFRHarveyRecordsInclPaperSurWithInundNSVI.csv',row.names = F)
