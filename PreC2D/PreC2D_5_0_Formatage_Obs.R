# library(sf)
library(sfheaders)
library(raster)
library(mapview)
library(ggplot2)
library(plotly)
require(tidyverse)
require(sf)
library(doParallel)
library(foreach)
#modif R

chem_routine=dirname(rstudioapi::getActiveDocumentContext()$path)
# chem_routine=R.home(component = "Cerema")
chemsource <- file.path(chem_routine,"PreC2D_Outils.R")
source(chemsource)
ListeObs=DataObs_choix()
## Liens Secteurs

nomlayerC="MTP_Pluie.gpkg"


## Chemin vers 
dsnlayer="C:\\Cartino2D\\France"
sc_file <- "C:\\Cartino2D\\France\\_SectionsControles\\SectControl_PPRiMtpCLL_20220610.shp"

######################################################################
############# Ne plus modifier CODE
######################################################################

# récupération du crs des rasters de lame d'eau
crs_ras <- paste0("+init=epsg:",ListeObs$EPSG)

# Ouverture du contour
# CONTOURSINI <- st_read(dsn = dsnlayer,
#                        layer = nomlayerC)
CONTOURSINI <- st_read(file.path(dsnlayer,nomlayerC))
CONTOURSINI <- sf_remove_holes(CONTOURSINI)
sc_file_read <- st_read(sc_file)
contour_contains <- st_contains_properly(CONTOURSINI,sc_file_read,sparse = T,prepared = T)
sc_file_read <- sc_file_read[unique(unlist(contour_contains)),]
# st_contains_properly(sc_file_read,CONTOURSINI)
# st_contains_properly(CONTOURSINI,sc_file_read)

# CONTOURSINI[which(contour_contains==integer(0)),]
contours_sc <- sapply(contour_contains, function(y) length(y) != 0)

CONTOURSINI_sc <- CONTOURSINI[contours_sc,]
chem_sc <- file.path(dsnlayer,"_SectionsControles",ListeObs$nom_dossier)
noms_sc <- list.dirs(chem_sc,full.names = FALSE)

# for (icont in 1:length(CONTOURSINI_sc$ID)) {
#   
# 
#   
# }
# ListeObs <- DataObs_choix()
sc_shp <- st_read(ListeObs$shp)
if(!dir.exists(file.path(ListeObs$nom_post))){
  dir.create(file.path(ListeObs$nom_post))
}
if(ListeObs$Noms=="dossiers"){
  for (i_sc_name in 1:length(sc_shp$ID)) {
    
    dsn_sc <- file.path(ListeObs$nom_dossier,sc_shp$ID[i_sc_name])
    if(!dir.exists(dsn_sc)) next
    obs_sc_file <- list.files(dsn_sc,pattern =ListeObs$ext,full.names = T )
    if(ListeObs$ext=="csv"){
      obs_sc_tab <- read.csv(obs_sc_file,sep = ListeObs$Sep,skip = ListeObs$Skip,header =(ListeObs$header=="TRUE") )
      if(ListeObs$Col_Debit==ListeObs$Col_Hauteur){
        if(!is.na(ListeObs$Colonne_type)){
          if(!is.na(ListeObs$Nom_Debit)){
            obs_sc_tab_Q <- obs_sc_tab[which(obs_sc_tab[,ListeObs$Colonne_type]==ListeObs$Nom_Debit),]
            obs_sc_tab_Q <- obs_sc_tab_Q[,c(ListeObs$Date,ListeObs$Col_Debit)]
            names(obs_sc_tab_Q) <- c("Date","Q")
            # obs_sc_tab_Q$nom <- rep(paste0(sc_shp$ID[i_sc_name],"_obs"),length(obs_sc_tab_Q$Date))
            obs_sc_tab_Q$nom <- paste0(sc_shp$ID[i_sc_name],"_obs")
            obs_sc_tab_Q$Q <- obs_sc_tab_Q$Q*ListeObs$unit_debit
            obs_sc_tab_Q$Date <- strptime(x = obs_sc_tab_Q$Date,format = ListeObs$Format_date )
            obs_sc_tab_Q$Date <- as.POSIXct(obs_sc_tab_Q$Date,tz = "UTC")
            obs_sc_tab_Q$Date <- obs_sc_tab_Q$Date+as.numeric(ListeObs$Time)*3600
            write.csv(obs_sc_tab_Q,file.path(ListeObs$nom_post,paste0(sc_shp$ID[i_sc_name],"_Q.csv")),row.names = F)
          }
          if(!is.na(ListeObs$Nom_Hauteur)){
            obs_sc_tab_H <- obs_sc_tab[which(obs_sc_tab[,ListeObs$Colonne_type]==ListeObs$Nom_Hauteur),]
            obs_sc_tab_H <- obs_sc_tab_H[,c(ListeObs$Date,ListeObs$Col_Hauteur)]
            names(obs_sc_tab_H) <- c("Date","Z")
            # obs_sc_tab_H$nom <- rep(paste0(sc_shp$ID[i_sc_name],"_obs"),length(obs_sc_tab_H$Date))
            obs_sc_tab_H$nom <- paste0(sc_shp$ID[i_sc_name],"_obs")
            obs_sc_tab_H$Z <- obs_sc_tab_H$Z*ListeObs$unit_hauteur
            obs_sc_tab_H$Date <- strptime(x = obs_sc_tab_H$Date,format = ListeObs$Format_date )
            obs_sc_tab_H$Date <- as.POSIXct(obs_sc_tab_H$Date,tz = "UTC")
            obs_sc_tab_H$Date <- obs_sc_tab_H$Date+as.numeric(ListeObs$Time)*3600
            obs_sc_tab_Haut <- obs_sc_tab_H
            # colnames(obs_sc_tab_Haut)
            names(obs_sc_tab_Haut) <- c("Date","H","nom")
            write.csv(obs_sc_tab_Haut,file.path(ListeObs$nom_post,paste0(sc_shp$ID[i_sc_name],"_H.csv")),row.names = F)
            
            if(!is.na(sc_shp$Zero_C2D[i_sc_name])){
              obs_sc_tab_H$Z <- obs_sc_tab_H$Z+sc_shp$Zero_C2D[i_sc_name]
            }else{
              obs_sc_tab_H$Z <- obs_sc_tab_H$Z+sc_shp$Zero[i_sc_name]
            }
            write.csv(obs_sc_tab_H,file.path(ListeObs$nom_post,paste0(sc_shp$ID[i_sc_name],"_Z.csv")),row.names = F)
           
            
          }
          
        }
        
      }
    }
    
  }
  
  
  
}else{
  
  for (i_sc_name in 1:length(sc_shp$ID)) {
    
    dsn_sc <- file.path(ListeObs$nom_dossier,paste0(sc_shp$ID[i_sc_name],".",ListeObs$ext))
    if(!file.exists(dsn_sc)) next
    obs_sc_file <- dsn_sc
    if(ListeObs$ext=="csv"){
      obs_sc_tab <- read.csv(obs_sc_file,sep = ListeObs$Sep,skip = ListeObs$Skip,header =(ListeObs$header=="TRUE") )
      if(ListeObs$Col_Debit==ListeObs$Col_Hauteur){
        if(!is.na(ListeObs$Colonne_type)){
          if(!is.na(ListeObs$Nom_Debit)){
            obs_sc_tab_Q <- obs_sc_tab[which(obs_sc_tab[,ListeObs$Colonne_type]==ListeObs$Nom_Debit),]
            obs_sc_tab_Q <- obs_sc_tab_Q[,c(ListeObs$Date,ListeObs$Col_Debit)]
            names(obs_sc_tab_Q) <- c("Date","Q")
            # obs_sc_tab_Q$nom <- rep(paste0(sc_shp$ID[i_sc_name],"_obs"),length(obs_sc_tab_Q$Date))
            obs_sc_tab_Q$nom <- paste0(sc_shp$ID[i_sc_name],"_obs")
            obs_sc_tab_Q$Q <- obs_sc_tab_Q$Q*ListeObs$unit_debit
            obs_sc_tab_Q$Date <- strptime(x = obs_sc_tab_Q$Date,format = ListeObs$Format_date )
            obs_sc_tab_Q$Date <- as.POSIXct(obs_sc_tab_Q$Date,tz = "UTC")
            obs_sc_tab_Q$Date <- obs_sc_tab_Q$Date+as.numeric(ListeObs$Time)*3600
            write.csv(obs_sc_tab_Q,file.path(ListeObs$nom_post,paste0(sc_shp$ID[i_sc_name],"_Q.csv")),row.names = F)
          }
          if(!is.na(ListeObs$Nom_Hauteur)){
            obs_sc_tab_H <- obs_sc_tab[which(obs_sc_tab[,ListeObs$Colonne_type]==ListeObs$Nom_Hauteur),]
            obs_sc_tab_H <- obs_sc_tab_H[,c(ListeObs$Date,ListeObs$Col_Hauteur)]
            names(obs_sc_tab_H) <- c("Date","Z")
            # obs_sc_tab_H$nom <- rep(paste0(sc_shp$ID[i_sc_name],"_obs"),length(obs_sc_tab_H$Date))
            obs_sc_tab_H$nom <- paste0(sc_shp$ID[i_sc_name],"_obs")
            obs_sc_tab_H$Z <- obs_sc_tab_H$Z*ListeObs$unit_hauteur
            obs_sc_tab_H$Date <- strptime(x = obs_sc_tab_H$Date,format = ListeObs$Format_date )
            obs_sc_tab_H$Date <- as.POSIXct(obs_sc_tab_H$Date,tz = "UTC")
            obs_sc_tab_H$Date <- obs_sc_tab_H$Date+as.numeric(ListeObs$Time)*3600
            obs_sc_tab_Haut <- obs_sc_tab_H
            # colnames(obs_sc_tab_Haut)
            names(obs_sc_tab_Haut) <- c("Date","H","nom")
            write.csv(obs_sc_tab_Haut,file.path(ListeObs$nom_post,paste0(sc_shp$ID[i_sc_name],"_H.csv")),row.names = F)
            
            if(!is.na(sc_shp$Zero_C2D[i_sc_name])){
              obs_sc_tab_H$Z <- obs_sc_tab_H$Z+sc_shp$Zero_C2D[i_sc_name]
            }else{
              obs_sc_tab_H$Z <- obs_sc_tab_H$Z+sc_shp$Zero[i_sc_name]
            }
            write.csv(obs_sc_tab_H,file.path(ListeObs$nom_post,paste0(sc_shp$ID[i_sc_name],"_Z.csv")),row.names = F)
            
            
          }
          
        }
        
      }
      if(ListeObs$Col_Hauteur!=0 & ListeObs$Col_Debit==0){
        
        if(!is.na(ListeObs$Colonne_type) & ListeObs$Colonne_type!=0){
          if(!is.na(ListeObs$Nom_Debit) & ListeObs$Nom_Debit!=0){
            obs_sc_tab_Q <- obs_sc_tab[which(obs_sc_tab[,ListeObs$Colonne_type]==ListeObs$Nom_Debit),]
            obs_sc_tab_Q <- obs_sc_tab_Q[,c(ListeObs$Date,ListeObs$Col_Debit)]
            names(obs_sc_tab_Q) <- c("Date","Q")
            # obs_sc_tab_Q$nom <- rep(paste0(sc_shp$ID[i_sc_name],"_obs"),length(obs_sc_tab_Q$Date))
            obs_sc_tab_Q$nom <- paste0(sc_shp$ID[i_sc_name],"_obs")
            obs_sc_tab_Q$Q <- obs_sc_tab_Q$Q*ListeObs$unit_debit
            obs_sc_tab_Q$Date <- strptime(x = obs_sc_tab_Q$Date,format = ListeObs$Format_date )
            obs_sc_tab_Q$Date <- as.POSIXct(obs_sc_tab_Q$Date,tz = "UTC")
            obs_sc_tab_Q$Date <- obs_sc_tab_Q$Date+as.numeric(ListeObs$Time)*3600
            write.csv(obs_sc_tab_Q,file.path(ListeObs$nom_post,paste0(sc_shp$ID[i_sc_name],"_Q.csv")),row.names = F)
          }
          if(!is.na(ListeObs$Nom_Hauteur) & ListeObs$Nom_Hauteur!=0){
            obs_sc_tab_H <- obs_sc_tab[which(obs_sc_tab[,ListeObs$Colonne_type]==ListeObs$Nom_Hauteur),]
            obs_sc_tab_H <- obs_sc_tab_H[,c(ListeObs$Date,ListeObs$Col_Hauteur)]
            names(obs_sc_tab_H) <- c("Date","Z")
            # obs_sc_tab_H$nom <- rep(paste0(sc_shp$ID[i_sc_name],"_obs"),length(obs_sc_tab_H$Date))
            obs_sc_tab_H$nom <- paste0(sc_shp$ID[i_sc_name],"_obs")
            obs_sc_tab_H$Z <- obs_sc_tab_H$Z*ListeObs$unit_hauteur
            obs_sc_tab_H$Date <- strptime(x = obs_sc_tab_H$Date,format = ListeObs$Format_date )
            obs_sc_tab_H$Date <- as.POSIXct(obs_sc_tab_H$Date,tz = "UTC")
            obs_sc_tab_H$Date <- obs_sc_tab_H$Date+as.numeric(ListeObs$Time)*3600
            obs_sc_tab_Haut <- obs_sc_tab_H
            # colnames(obs_sc_tab_Haut)
            names(obs_sc_tab_Haut) <- c("Date","H","nom")
            write.csv(obs_sc_tab_Haut,file.path(ListeObs$nom_post,paste0(sc_shp$ID[i_sc_name],"_H.csv")),row.names = F)
            
            if(!is.na(sc_shp$Zero_C2D[i_sc_name])){
              obs_sc_tab_H$Z <- obs_sc_tab_H$Z+sc_shp$Zero_C2D[i_sc_name]
            }else{
              obs_sc_tab_H$Z <- obs_sc_tab_H$Z+sc_shp$Zero[i_sc_name]
            }
            write.csv(obs_sc_tab_H,file.path(ListeObs$nom_post,paste0(sc_shp$ID[i_sc_name],"_Z.csv")),row.names = F)
            
            
          }
          
        }
        
        
        
      }
    }
    
  }
  
  
  
  
  
  
}




