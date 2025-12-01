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

# chem_routine=dirname(rstudioapi::getActiveDocumentContext()$path)
# chem_routine=R.home(component = "Cerema")
chemsource <- file.path(chem_routine,"PreC2D","PreC2D_Outils.R")
source(chemsource)

######################################################################
############# Ne plus modifier CODE
######################################################################

# ouverture du xlsx conteant les infos et choix d'un evt 
ListeCas=RexPluie_CasPluie()

# récupération du crs des rasters de lame d'eau
crs_ras <- paste0("+init=epsg:",ListeCas$EPSG)

# Ouverture du contour
# CONTOURSINI <- st_read(file.path(dsnlayer,nomlayerC))

CONTOURSINI <- sf_remove_holes(CONTOURSINI)
ras_list_all <- list.files(ListeCas$Dossier_PluieRaster,pattern=paste0("\\",ListeCas$Extension,"$"),recursive = TRUE,full.names = TRUE)
ras_list_all_ <- basename(ras_list_all)
if(!is.na(ListeCas$NomType)){
  substrings <- substr(ras_list_all_, nchar(ListeCas$NomType)+1, nchar(ListeCas$NomType)+1+ListeCas$LgDate-1)
}else{
  substrings <- substr(ras_list_all_, 1, 1+ListeCas$LgDate-1)
  
}
if(is.na(ListeCas$Formatdate)){
  dates <- as.POSIXct(substrings, format = "%Y%m%d%H%M")
}else{
  dates <- as.POSIXct(substrings, format = ListeCas$Formatdate)
}


if(use_debu_fin==TRUE){
  DateDebutp <- as.POSIXct(as.character(DateDebut), format = "%Y%m%d%H%M")
  DateFinp <- as.POSIXct(as.character(DateFin), format = "%Y%m%d%H%M")
  
  indices_in_range <- which(dates >= DateDebutp & dates <= DateFinp)
  
  # Positions de la première et de la dernière date
  first_position <- min(indices_in_range)
  last_position <- max(indices_in_range)
  ras_list_all <- ras_list_all[first_position:last_position]
  ras_list_all_ <- ras_list_all_[first_position:last_position]
  dates <- dates[first_position:last_position]
}




time_diff <- diff(dates)

# Trouver les indices où l'écart dépasse 15 minutes
breaks <- which(time_diff > ListeCas$dt*60)+1

# Ajouter un index au début et à la fin du vecteur pour s'assurer que toutes les dates sont incluses dans une sous-liste
breaks <- c(1, breaks, length(dates)+1)

# Découper la liste en sous-listes
sublists <- split(ras_list_all, rep(1:(length(breaks)-1), diff(breaks)))
subliststime <- split(dates, rep(1:(length(breaks)-1), diff(breaks)))
# ouverture du contour du BV
# bassin <- st_read(file.path(dsnlayer,paste0(nomlayerC,".shp")))
for (iper in 1:length(sublists)) {
  ras_list <- sublists[[iper]]
  time_list <- subliststime[[iper]]
  if(!is.na(nchar(ListeCas$Nomplus)>0)){
    ras_list <- ras_list[grepl(ListeCas$Nomplus,ras_list)]  
    
  }
  min_ncharlist <- min(nchar(ras_list))
  ras_list <- ras_list[which(nchar(ras_list)==min_ncharlist)]
  ras_brick <- raster::stack(ras_list)
  
  if(ListeCas$EPSG!="ProjMF"){
    crs(ras_brick) <- CRS(crs_ras)
  }
  st_crs(CONTOURSINI) <- EPSG
if(nb_proc>1){
  cl <- parallel::makeCluster(nb_proc) 
  doParallel::registerDoParallel(cl)
  foreach::foreach(ibassin = 1:length(CONTOURSINI$NOM), 
                   .combine = 'c',
                   .inorder = FALSE,
                   .packages = c("sf","dplyr","utils","sfheaders","tidyverse","raster","ggplot2","plotly","htmlwidgets")) %dopar%{
                     # if(CONTOURSINI$STEP_PRE[ibassin]==-1) next 
                     
                     dsnPluie <- file.path(dsnPluie_,CONTOURSINI$NOMPOST[ibassin])
                     if(!dir.exists(dsnPluie)){
                       
                       dir.create(dsnPluie)
                     }
                     bassin <- CONTOURSINI[ibassin,]
                     # transformation de la projection à la projection des rasters
                     
                     bassin <- st_transform(bassin,raster::crs(ras_brick))
                     
                     # être sûr d'avoir le bon crs pour les rasters 
                     
                     
                     
                     
                     # Découpage du raster sur l'extent du contour + une certaine distance
                     # ras_crop <- crop(ras_brick_fin_mm,extent(bassin),snap = 'near')
                     
                     bassin_ <- bassin %>%
                       st_simplify(preserveTopology = FALSE,
                                   dTolerance = 1) %>%
                       st_segmentize(1, crs = crs)
                     
                     bassin_ <- st_buffer(st_geometry(bassin_),500)
                     # ras_crop <- mask(ras_brick_fin_mm,st_as_sf(bassin_))
                     ras_brick_fin <-  crop(ras_brick,st_as_sf(bassin_))
                     # transformer l'unité en mm 
                     ras_brick_fin_mm <- ras_brick_fin/ListeCas$Umm
                     ras_crop <- ras_brick_fin_mm
                     # transformation des rasters découpés en points 
                     ras_pts <- rasterToPoints(ras_crop)
                     
                     # création d'une couche sf points à partir des points obtenus des rasters
                     shp_crop <- st_as_sf(as.data.frame(ras_pts), coords = c("x", "y"), crs = crs(ras_brick), agr = "constant")
                     
                     # transformation de la projection vers la projection choisie (souvent 2154)
                     shp_crop_lam93 <- st_transform(shp_crop,EPSG)
                     
                     # récupération du nombre de pas de temps 
                     if(use_debu_fin==FALSE){
                       DateDebut=as.POSIXct(time_list[1]) 
                       DateFin=as.POSIXct(time_list[length(time_list)])
                       nbpastemps <- as.numeric(difftime(DateFin,DateDebut,units = "mins"))/as.numeric(ListeCas$dt)
                       # pas de temps de fin
                       fintemps <- as.numeric(difftime(DateFin,DateDebut,units = "mins"))*60
                       ListeCas$Annee <- format(DateDebut, "%Y")
                       DateDebut <- paste0(gsub("[- :]", "", format(DateDebut, "%Y-%m-%d %H:%M:%S")), collapse = "")
                       DateFin <- paste0(gsub("[- :]", "", format(DateFin, "%Y-%m-%d %H:%M:%S")), collapse = "")
                     }else{
                       
                       DateDebut=as.POSIXct(time_list[1]) 
                       DateFin=as.POSIXct(time_list[length(time_list)])
                       nbpastemps <- as.numeric(difftime(DateFin,DateDebut,units = "mins"))/as.numeric(ListeCas$dt)
                       # pas de temps de fin
                       fintemps <- as.numeric(difftime(DateFin,DateDebut,units = "mins"))*60
                       ListeCas$Annee <- format(DateDebut, "%Y")
                       DateDebut <- paste0(gsub("[- :]", "", format(DateDebut, "%Y-%m-%d %H:%M:%S")), collapse = "")
                       DateFin <- paste0(gsub("[- :]", "", format(DateFin, "%Y-%m-%d %H:%M:%S")), collapse = "")
                       
                       
                       # nbpastemps <- as.numeric((strptime(DateFin,"%Y%m%d%H%M")-strptime(DateDebut,"%Y%m%d%H%M")),"mins")/as.numeric(ListeCas$dt)
                       # # pas de temps de fin
                       # fintemps <- as.numeric((strptime(DateFin,"%Y%m%d%H%M")-strptime(DateDebut,"%Y%m%d%H%M")),"mins")*60
                       # ListeCas$Annee <- format(DateDebut, "%Y")
                     }
                     
                     
                     # nouveaux noms des colonnes en noms
                     new_names <- seq(from =0, to = fintemps,by=(as.numeric(ListeCas$dt)*60) )
                     names(shp_crop_lam93)[1:(ncol(shp_crop_lam93)-1)] <- new_names
                     
                     
                     # ecriture de la couche des points 
                     # st_write(shp_crop_lam93,file.path(dsnPluie,paste0(DateDebut,"_",bassin$NOM,".shp")),delete_dsn = TRUE)
                     
                     # calcul du cumul 
                     rs1 <- calc(ras_crop, sum,na.rm = TRUE )
                     rs1[which(values(rs1)==0)] <- NA
                     # # mapview::mapview(bassin)+mapview::mapview(rs1)+mapview::mapview(shp_crop_lam93)
                     # # mapview::mapview(bassin)+mapview::mapview(rs1) + mapview(shp_crop)
                     writeRaster(rs1,
                                 filename =file.path(dsnPluie,
                                                     paste0(bassin$NOM,"_",DateDebut,"_","cumul.tif")),
                                 overwrite=TRUE)
                     
                     #---------------------------------------creation de la pluie homogène
                     
                     bassin_homogene <- bassin %>%sfheaders::sf_remove_holes() %>% 
                       st_union() %>% 
                       st_simplify(preserveTopology = FALSE,
                                   dTolerance = 10) %>%
                       st_segmentize(20, crs = crs) %>% 
                       st_buffer(150) %>% 
                       st_as_sf()
                     
                     ras_extract <- raster::extract(ras_brick,bassin_homogene, weights = TRUE, normalizeWeights=TRUE, df= TRUE )
                     
                     ras_exrer <- ras_extract*ras_extract$weight
                     
                     ecn <- colSums(ras_exrer,na.rm = TRUE)/ListeCas$Umm
                     ecn <- ecn[2:(length(ecn)-1)]
                     
                     initialize_vec <- seq(from = 0, to=0, length.out= length(names(ras_crop)))
                     
                     bassin_homogene <- bassin %>%sfheaders::sf_remove_holes() %>% 
                       st_union() %>% 
                       st_simplify(preserveTopology = FALSE,
                                   dTolerance = 10) %>%
                       st_segmentize(20, crs = crs) %>% 
                       st_buffer(150) %>% 
                       st_as_sf()
                     
                     ras_extract <- raster::extract(ras_brick,bassin_homogene, weights = TRUE, normalizeWeights=TRUE, df= TRUE )
                     
                     ras_exrer <- ras_extract*ras_extract$weight
                     
                     ecn <- colSums(ras_exrer)/ListeCas$Umm
                     ecn <- ecn[2:(length(ecn)-1)]
                     
                     initialize_vec <- seq(from = 0, to=0, length.out= length(names(ras_crop)))
                     
                     moyenne <- data.frame(time=initialize_vec,
                                           mean = initialize_vec)
                     
                     moyenne$time <- seq(0,ListeCas$dt*(length(moyenne$time)-1),by = ListeCas$dt)*60
                     
                     
                     moyenne$mean <- as.numeric(ecn)
                     # moyenne$mean <- as.numeric(cellStats(ras_crop,stat = "mean"))
                     
                     
                     moyenne$posrain <- moyenne$mean>minmm
                     # moyenne$posrainnext <- c(moyenne$mean[2:length(moyenne$mean)]>minmm,FALSE)
                     # moyenne$posrainbefore <- c(FALSE,moyenne$mean[1:(length(moyenne$mean)-1)]>minmm)
                     moyenne$posrainnext <- c(moyenne$mean[4:length(moyenne$mean)]>minmm,rep(FALSE,3))
                     moyenne$posrainbefore <- c(rep(FALSE,3),moyenne$mean[1:(length(moyenne$mean)-3)]>minmm)
                     
                     moyenne$posraintrue <- moyenne$posrainbefore | moyenne$posrainnext | moyenne$posrain
                     # if(length(which(moyenne$posraintrue>0))<4) next
                     runs <- rle(moyenne$posraintrue)
                     myruns = which(runs$values == TRUE & runs$lengths >= (180/ListeCas$dt))
                     
                     runs.lengths.cumsum = cumsum(runs$lengths)
                     ends = runs.lengths.cumsum[myruns]
                     
                     newindex = ifelse(myruns>1, myruns-1, 0)
                     starts = runs.lengths.cumsum[newindex] + 1
                     
                     # if(length(starts)==0) next
                     if (0 %in% newindex) starts = c(1,starts)
                     if(length(starts)==0){
                       lenstarts=1
                     }else{
                       lenstarts=length(starts)
                     }
                     
                     tab_decoup_evt <- data.frame(id=1:lenstarts,
                                                  starts=starts,
                                                  ends=ends)
                     
                     
                     for (i_tab_evt in 1:length(tab_decoup_evt$id)) {
                       tab_decoup_evt$summm[i_tab_evt] <- sum(moyenne$mean[tab_decoup_evt$starts[i_tab_evt]:tab_decoup_evt$ends[i_tab_evt]])
                       
                       # tab_decoup_evt$summm[i_tab_evt] <- sum(moyenne$mean[tab_decoup_evt$starts[i_tab_evt]:tab_decoup_evt$ends[i_tab_evt]])
                     }
                     tab_decoup_evt$todo <- tab_decoup_evt$summm > cumul_min
                     tab_decoup_evt_test <- tab_decoup_evt
                     tab_decoup_evt <- tab_decoup_evt[tab_decoup_evt$todo,]
                     if(nrow(tab_decoup_evt)==0) {
                       return(NULL)
                     }
                     tab_decoup_evt$startime <- moyenne$time[tab_decoup_evt$starts]
                     tab_decoup_evt$endtime <- moyenne$time[tab_decoup_evt$ends]
                     for (idecoup in 1:length(tab_decoup_evt$id)) {
                       
                       if(tab_decoup_evt$startime[idecoup]>(nb_h_avant*60/(ListeCas$dt)*ListeCas$dt*60)){
                         debtempsnew <- tab_decoup_evt$startime[idecoup]-(nb_h_avant*60/(ListeCas$dt)*ListeCas$dt*60)
                       }else{
                         debtempsnew <- tab_decoup_evt$startime[idecoup]
                       }
                       
                       DateDebutnew <- strptime(DateDebut,"%Y%m%d%H%M")+debtempsnew
                       
                       if(tab_decoup_evt$endtime[idecoup]<(fintemps-(nb_h_apres*60/(ListeCas$dt)*ListeCas$dt*60))){
                         fintempsnew <- tab_decoup_evt$endtime[idecoup]+(nb_h_apres*60/(ListeCas$dt)*ListeCas$dt*60)
                       }else{
                         fintempsnew <- tab_decoup_evt$endtime[idecoup]
                       }
                       
                       
                       # fintempsnew <- max(moyenne$time[which(moyenne$posraintrue==1)])+ListeCas$dt*60*2
                       DataFinnew <- strptime(DateDebut,"%Y%m%d%H%M")+floor((fintempsnew/3600)+1)*3600
                       # DataFinnew <- strptime(DateDebut,"%Y%m%d%H%M")+floor((fintempsnew/3600)+1)*3600
                       
                       
                       ##AMC calcul
                       tps_start <- tab_decoup_evt$startime[idecoup]/(3600*24)
                       if(tps_start>=5){
                         tps5d <- tab_decoup_evt$startime[idecoup]-(5*3600*24)
                         
                         sum_avant <- sum(moyenne$mean[which(moyenne$time==tps5d):tab_decoup_evt$starts[idecoup]])
                         if(sum_avant<36){
                           AMC=1
                           
                         }else if(sum_avant>=36 & sum_avant<=53){
                           AMC=2
                           
                         }else{
                           AMC=3
                         }
                       }else{
                         
                         tpsav <- 0
                         
                         sum_avant <- sum(moyenne$mean[which(moyenne$time==tpsav):tab_decoup_evt$starts[idecoup]])
                         if(sum_avant<36){
                           AMC=1
                           
                         }else if(sum_avant>=36 & sum_avant<=53){
                           AMC=2
                           
                         }else{
                           AMC=3
                         }
                         
                       }
                       
                       
                       diffs <- as.numeric(difftime(DataFinnew,DateDebutnew,units = "h"))*3600
                       
                       tpsh <- diffs %/% (60 * 60)
                       
                       diffh <- diffs-tpsh*(60 * 60)
                       
                       tpsm <- diffh %/% 60
                       datedeb_format <- gsub('[^0-9]','',DateDebutnew)
                       datedeb_format <- str_trunc(datedeb_format,width = 12,side = "right",ellipsis = "")
                       hyeto_name=file.path(dsnPluie,
                                            paste0("hyeto_",
                                                   bassin$NOM,
                                                   "_Evt",
                                                   datedeb_format,#substr(row.names(tabhyeto[NSerie[1],]),1,12),
                                                   "_",
                                                   formatC(tpsh,width=2, flag="0"),
                                                   "h",
                                                   formatC(tpsm,width=2, flag="0"),
                                                   "min_AMC",
                                                   AMC,
                                                   "_",
                                                   ListeCas$EPSG,"_Evt_",
                                                   ListeCas$Localisation,"_",
                                                   ListeCas$dt,ListeCas$UnitTemps,"_",
                                                   ListeCas$Code,"_",
                                                   ListeCas$Source,"_",
                                                   ListeCas$Annee))
                       
                       path_file <- paste0(hyeto_name,'.txt')
                       file.create(path_file)
                       first_line <- "#HYETOGRAPH FILE"
                       second_line <- "#T (s) RAINFALL (mm)"
                       datedebutstrp <- strptime(DateDebut,"%Y%m%d%H%M")
                       diffsini <- as.numeric(difftime(DataFinnew,datedebutstrp,units = "h") )*3600
                       moyennenew <- moyenne[which(moyenne$time>=debtempsnew & moyenne$time<=diffsini),]
                       
                       new_time <- seq(0,ListeCas$dt*(length(moyennenew$time)-1),by = ListeCas$dt)*60
                       new_time[length(new_time)] <- 999000
                       moyennenew$time <- new_time
                       
                       time_h <- as.vector(moyennenew$time) %>%
                         as.character()
                       pluie_h <- as.vector(moyennenew$mean) %>% 
                         round(digits = 2) %>% 
                         as.character()
                       # pluie_h[1] <- 0
                       # pluie_h[length(pluie_h)] <- 0
                       tab_pluie <- paste(time_h,pluie_h,sep = " ")
                       
                       writeLines(first_line,path_file)
                       write(second_line,path_file,append = TRUE)
                       
                       write(tab_pluie,path_file,append = TRUE)
                       
                       
                       ##################################################################################
                       ###############fichier pluie brute pour pluie spatialisée####################"####
                       tab_timestep_pb <-  data.frame(Time=moyennenew$time,
                                                      Val=moyennenew$mean)
                       
                       
                       DUREE=diffs
                       
                       aire=st_area(CONTOURSINI[ibassin,])
                       
                       
                       
                       # On rajoute du temps en fonction de la taille du Bv
                       # si V=1.5m/s, en 1h sur un rectangle longuer 2 largeur 1
                       DUREE=round(12*(DUREE/3600+as.numeric(as.character(aire))/(3600*1800)))/12
                       DUREE=DUREE*3600
                       timemin <- seq(from = 60,to =DUREE,by=60 )
                       tab_min_pb <- data.frame(Time=timemin,
                                                Val=0*(1:length(timemin)))
                       tab_timestep_pb <- tab_timestep_pb[-1,]
                       tab_timestep_pb$Time[length(tab_timestep_pb$Time)]=tab_min_pb$Time[length(tab_min_pb$Time)]
                       for (i_time in 1:length(tab_timestep_pb$Time)) {
                         if(i_time==1){
                           tab_min_pb$Val[1:which(tab_min_pb$Time==tab_timestep_pb$Time[i_time])]=tab_timestep_pb$Val[i_time]
                         }else{
                           
                           # ind_which <- which(tab_min_pb$Time==tab_timestep_pb$Time[i_time])
                           
                           tab_min_pb$Val[(which(tab_min_pb$Time==tab_timestep_pb$Time[i_time-1])+1):which(tab_min_pb$Time==tab_timestep_pb$Time[i_time])]=tab_timestep_pb$Val[i_time]
                         }
                         
                         
                       }
                       Brute_name=file.path(dsnPluie,
                                            paste0("brute_",
                                                   bassin$NOM,
                                                   "_Evts",
                                                   datedeb_format,#substr(row.names(tabhyeto[NSerie[1],]),1,12),
                                                   "_",
                                                   formatC(tpsh,width=2, flag="0"),
                                                   "h",
                                                   formatC(tpsm,width=2, flag="0"),
                                                   "min_AMC",
                                                   AMC,
                                                   "_",
                                                   ListeCas$EPSG,"_Evt_",
                                                   ListeCas$Localisation,"_",
                                                   ListeCas$dt,ListeCas$UnitTemps,"_",
                                                   ListeCas$Code,"_",
                                                   ListeCas$Source,"_",
                                                   ListeCas$Annee))
                       
                       path_file <- paste0(Brute_name,'.txt')
                       file.create(path_file)
                       # first_line <- "#HYETOGRAPH FILE"
                       # second_line <- "#T (s) RAINFALL (mm)"
                       datedebutstrp <- strptime(DateDebut,"%Y%m%d%H%M")
                       diffsini <- as.numeric(difftime(DataFinnew,datedebutstrp,units = "h") )*3600
                       moyennenew <- moyenne[which(moyenne$time>=debtempsnew & moyenne$time<=diffsini),]
                       
                       new_time <- seq(0,ListeCas$dt*(length(moyennenew$time)-1),by = ListeCas$dt)*60
                       new_time[length(new_time)] <- 999000
                       moyennenew$time <- new_time
                       
                       time_h <- as.vector(moyennenew$time) %>%
                         as.character()
                       pluie_h <- as.vector(moyennenew$mean) %>% 
                         round(digits = 2) %>% 
                         as.character()
                       # pluie_h[1] <- 0
                       # pluie_h[length(pluie_h)] <- 0
                       tab_pluie <- paste(time_h,pluie_h,sep = " ")
                       
                       # writeLines(first_line,path_file)
                       # write(second_line,path_file,append = TRUE)
                       tab_min_pb$Val <- round(tab_min_pb$Val,digits = 2)
                       write.table(tab_min_pb,path_file,row.names = FALSE)
                       
                       #-----------------------------------------------------------
                       
                       # moyennenew <- moyenne[which(moyenne$time>=debtempsnew & moyenne$time<=diffsini),]
                       
                       
                       ############ écriture du fichier de pluie spatialisée pour T2D#######################
                       # tpsh <- fintemps %/% (60 * 60)
                       # 
                       # diffh <- fintemps-tpsh*(60 * 60)
                       # 
                       # tpsm <- diffh %/% 60
                       
                       spatial_name=file.path(dsnPluie,
                                              paste0("spatial_",
                                                     bassin$NOM,
                                                     "_Evts",
                                                     datedeb_format,#substr(row.names(tabhyeto[NSerie[1],]),1,12),
                                                     "_",
                                                     formatC(tpsh,width=2, flag="0"),
                                                     "h",
                                                     formatC(tpsm,width=2, flag="0"),
                                                     "min_AMC",
                                                     AMC,
                                                     "_",
                                                     ListeCas$EPSG,"_Evt_",
                                                     ListeCas$Localisation,"_",
                                                     ListeCas$dt,ListeCas$UnitTemps,"_",
                                                     ListeCas$Code,"_",
                                                     ListeCas$Source,"_",
                                                     ListeCas$Annee))
                       
                       # writeRaster(rs1,paste0(spatial_name,"cumul.tif"),overwrite=TRUE)
                       
                       name_file <- nomlayerC
                       path_file <- paste0(spatial_name,'.txt')
                       file.create(path_file)
                       first_line <- "#Pluie spatialisee"
                       coords_pluie <- st_coordinates(shp_crop_lam93)
                       
                       X_pluie <- as.vector(coords_pluie[,1]) %>% 
                         round(digits = 2) %>% 
                         as.character()
                       Y_pluie <- as.vector(coords_pluie[,2]) %>% 
                         round(digits = 2) %>% 
                         as.character()
                       XY_pluie <- paste(X_pluie,Y_pluie,sep = ",")
                       
                       writeLines(first_line,path_file)
                       
                       nbs <- paste(as.character(length(XY_pluie)),as.character(nbpastemps),sep = ",")
                       write(nbs,path_file,append = TRUE)
                       
                       
                       write(XY_pluie,path_file,append = TRUE)
                       pluie_df <- st_drop_geometry(shp_crop_lam93)
                       pluie_df_updt <- pluie_df[,which(as.numeric(colnames(pluie_df))>=debtempsnew & as.numeric(colnames(pluie_df))<=diffsini)]
                       
                       colnames(pluie_df_updt) <- as.character(moyennenew$time)
                       
                       
                       fn_pluie <- function(x){
                         to_write <- paste(colnames(pluie_df_updt)[which(colnames(pluie_df_updt)==x)], paste(as.character(round(pluie_df_updt[,which(colnames(pluie_df_updt)==x)],digits = 2)),collapse = ", "),sep = ", ")
                         write(to_write,path_file,append = TRUE)
                       }
                       
                       sapply(colnames(pluie_df_updt), fn_pluie,simplify = TRUE,USE.NAMES = FALSE)
                       
                       
                       
                       #####################################################################################
                       
                       
                       ####SI on veut faire des stats######################
                       
                     }
                     
                     
                     
                     ################Cumuls et statistiques#############################
                     # if(calculcumul){
                     #   cumul_homogene(moyenne,ncumul,bassin)
                     # }
                     # if(calculcumulsp){
                     #   cumul_spatial(moyenne,ncumul,bassin,ras_crop)
                     # }
                     # if(do_stat){
                     #   pluie_stats(ras_crop,nbpastemps,DateDebut,DateFin)
                     # }
                     
                     if(do_stat){
                       # cum_bassin <- raster::extract(x = ras_brick_fin_mm,y = bassin,df = TRUE,fun = "sum",weights = TRUE, normalizeWeights = TRUE)
                       # cum_bassin_ <- raster::extract(x = ras_brick_fin_mm,y = bassin,df = TRUE,fun = "sum")
                       ################SCS-CN#########################################
                       # CN <- c(30,40,50,60,70,80,90,100)
                       
                       # S = 25.4*(1000/(CN-10))
                       
                       # R <- (P-0.2*S)^2/(P+0.8*S)
                       
                       names(ras_crop) <- as.character(new_names)
                       seq.POSIXt(strptime(DateDebut,"%Y%m%d%H%M"),strptime(DateFin,"%Y%m%d%H%M"),by = paste(ListeCas$dt,ListeCas$UnitTemps))
                       
                       ras_crop_cumul <- ras_crop
                       for (t in 2:length(names(ras_crop))) {
                         
                         ras_crop_cumul[[t]] <- sum(ras_crop[[t:1]])
                         
                         
                         
                       }
                       names(ras_crop_cumul) <- as.character(new_names)
                       cum_pas_detemps <- sum(ras_crop_cumul[[1]])
                       date_evt <- seq.POSIXt(strptime(DateDebut,"%Y%m%d%H%M"),strptime(DateFin,"%Y%m%d%H%M"),by = paste(ListeCas$dt,ListeCas$UnitTemps))
                       fun_stat <- c("max","mean","median","min","sd")
                       
                       #####################stats lame d'eau#######################
                       initialize_vec <- seq(from = 0, to=0, length.out= length(names(ras_crop)))
                       stats_lame_brute <- data.frame(date=date_evt,
                                                      mean = initialize_vec,
                                                      median = initialize_vec,
                                                      min = initialize_vec,
                                                      max = initialize_vec,
                                                      sd = initialize_vec,
                                                      var = initialize_vec,
                                                      sum = initialize_vec )
                       
                       
                       for (i in 1:length(fun_stat)) {
                         
                         stats_lame_brute[,i+1] <- as.numeric(cellStats(ras_crop,stat = fun_stat[i]))
                         
                       }
                       
                       stats_lame_brute <- data.frame(date=date_evt,
                                                      stat_name = as.character(initialize_vec),
                                                      stat_value = initialize_vec)
                       stats_lame_brute_tmp <- data.frame(date=date_evt,
                                                          stat_name = as.character(initialize_vec),
                                                          stat_value = initialize_vec)
                       
                       for (i in 1:length(fun_stat)) {
                         
                         stats_lame_brute_tmp$stat_name <- fun_stat[i]
                         stats_lame_brute_tmp$stat_value <- as.numeric(cellStats(ras_crop,stat = fun_stat[i]))
                         
                         if(i==1){stats_lame_brute <- stats_lame_brute_tmp }else{
                           stats_lame_brute <- rbind.data.frame(stats_lame_brute,stats_lame_brute_tmp )
                         }
                         
                         
                       }
                       
                       stats_lame_brute$stat_name <- factor(stats_lame_brute$stat_name, levels =fun_stat )
                       
                       
                       # stats_pluie_brute <- ggplot(stats_lame_brute)+
                       #   geom_line(aes(x=date,y=stat_value,color = stat_name))
                       stats_pluie_brute <- ggplot(stats_lame_brute,aes(x=date,y=stat_value,color=stat_name,fill=stat_name))+
                         geom_col(position = 'identity')
                       
                       # stats_pluie_brute <- ggplot(stats_lame_brute)+
                       #   geom_line(aes(x=date,y=stat_value,color = stat_name))
                       couleurs <- c("red","black","purple","orange","tomato","cyan","red",'black',"purple",
                                     "orange","tomato","cyan","red","black","purple","orange",
                                     "tomato","cyan","red",'black',"purple","orange","tomato","cyan")
                       for (ievtnew in 1:length(tab_decoup_evt$id)) {
                         vline_val_deb <- strptime(DateDebut,"%Y%m%d%H%M")+tab_decoup_evt$startime[ievtnew]-(nb_h_avant*60/(ListeCas$dt)*ListeCas$dt*60)
                         vline_val_fin <- strptime(DateDebut,"%Y%m%d%H%M")+tab_decoup_evt$endtime[ievtnew]+(nb_h_apres*60/(ListeCas$dt)*ListeCas$dt*60)
                         stats_pluie_brute <- stats_pluie_brute  +
                           geom_vline(xintercept = as.POSIXct(vline_val_deb),
                                      colour=couleurs[ievtnew],linetype= 2,size=0.8)+
                           geom_vline(xintercept = as.POSIXct(vline_val_fin),
                                      colour=couleurs[ievtnew],linetype= 2,size=0.8)
                         
                         
                       }
                       dir.create(file.path(dsnPluie,CONTOURSINI$NOM[ibassin]))
                       
                       stats_pluie_brute_p <- plotly::ggplotly(stats_pluie_brute,dynamicTicks = TRUE)
                       htmlwidgets::saveWidget(partial_bundle(stats_pluie_brute_p),
                                               file = file.path(dsnPluie,CONTOURSINI$NOM[ibassin],paste0(DateDebut,"_",DateFin,".html")),
                                               libdir = "lib")
                       
                       
                       #####################stats cumul############################
                       
                       stats_lame_cumul <- data.frame(date=date_evt,
                                                      mean = initialize_vec,
                                                      median = initialize_vec,
                                                      min = initialize_vec,
                                                      max = initialize_vec,
                                                      sd = initialize_vec,
                                                      var = initialize_vec,
                                                      sum = initialize_vec)
                       
                       for (i in 1:length(fun_stat)) {
                         
                         stats_lame_cumul[,i+1] <- as.numeric(cellStats(ras_crop_cumul,stat = fun_stat[i]))
                         
                       }
                       
                       
                       
                       
                       stats_lame_cumul <- data.frame(date=date_evt,
                                                      stat_name = as.character(initialize_vec),
                                                      stat_value = initialize_vec)
                       stats_lame_cumul_tmp <- data.frame(date=date_evt,
                                                          stat_name = as.character(initialize_vec),
                                                          stat_value = initialize_vec)
                       
                       for (i in 1:length(fun_stat)) {
                         
                         stats_lame_cumul_tmp$stat_name <- fun_stat[i]
                         stats_lame_cumul_tmp$stat_value <- as.numeric(cellStats(ras_crop_cumul,stat = fun_stat[i]))
                         
                         if(i==1){stats_lame_cumul <- stats_lame_cumul_tmp}else{
                           stats_lame_cumul <- rbind.data.frame(stats_lame_cumul,stats_lame_cumul_tmp)
                         }
                         
                         
                       }
                       
                       stats_lame_cumul$stat_name <- factor(stats_lame_cumul$stat_name, levels =fun_stat )
                       
                       
                       stats_cumul <- ggplot(stats_lame_cumul)+
                         geom_line(aes(x=date,y=stat_value,color = stat_name))
                       
                       
                       
                       stats_cumul_p <- plotly::ggplotly(stats_cumul,dynamicTicks = TRUE)
                       # config(stats_cumul_p, scrollZoom = TRUE)
                       htmlwidgets::saveWidget(partial_bundle(stats_cumul_p) ,
                                               file = file.path(dsnPluie,CONTOURSINI$NOM[ibassin],paste0(DateDebut,"_",DateFin,"_cumul.html")),
                                               libdir = "lib")
                       
                       for (icum in 1:length(nduree)) {
                         
                         nvalsum <- nduree[icum]
                         
                         
                       }
                       
                     }
                     if(calculcumul){
                       cumul_homogene(moyenne,ncumul,bassin)
                     }
                     if(calculcumulsp){
                       cumul_spatial(moyenne,ncumul,bassin,ras_crop)
                     }
                   }
  
  parallel::stopCluster(cl)
}else{
  for (ibassin in 1:length(CONTOURSINI$NOM)) {
    if(CONTOURSINI$STEP_PRE[ibassin]==-1) next 
    
    dsnPluie <- file.path(dsnPluie_,CONTOURSINI$NOMPOST[ibassin])
    if(!dir.exists(dsnPluie)){
      
      dir.create(dsnPluie)
    }
    bassin <- CONTOURSINI[ibassin,]
    # transformation de la projection à la projection des rasters
    
    bassin <- st_transform(bassin,raster::crs(ras_brick))
    
    # être sûr d'avoir le bon crs pour les rasters 
    
    
    
    
    # Découpage du raster sur l'extent du contour + une certaine distance
    # ras_crop <- crop(ras_brick_fin_mm,extent(bassin),snap = 'near')
    
    bassin_ <- bassin %>%
      st_simplify(preserveTopology = FALSE,
                  dTolerance = 1) %>%
      st_segmentize(1, crs = crs)
    
    bassin_ <- st_buffer(st_geometry(bassin_),500)
    # ras_crop <- mask(ras_brick_fin_mm,st_as_sf(bassin_))
    ras_brick_fin <-  crop(ras_brick,st_as_sf(bassin_))
    # transformer l'unité en mm 
    ras_brick_fin_mm <- ras_brick_fin/ListeCas$Umm
    ras_crop <- ras_brick_fin_mm
    # transformation des rasters découpés en points 
    ras_pts <- rasterToPoints(ras_crop)
    
    # création d'une couche sf points à partir des points obtenus des rasters
    shp_crop <- st_as_sf(as.data.frame(ras_pts), coords = c("x", "y"), crs = crs(ras_brick), agr = "constant")
    
    # transformation de la projection vers la projection choisie (souvent 2154)
    shp_crop_lam93 <- st_transform(shp_crop,EPSG)
    
    # récupération du nombre de pas de temps 
    if(use_debu_fin==FALSE){
      DateDebut=as.POSIXct(time_list[1]) 
      DateFin=as.POSIXct(time_list[length(time_list)])
      nbpastemps <- as.numeric(difftime(DateFin,DateDebut,units = "mins"))/as.numeric(ListeCas$dt)
      # pas de temps de fin
      fintemps <- as.numeric(difftime(DateFin,DateDebut,units = "mins"))*60
      ListeCas$Annee <- format(DateDebut, "%Y")
      DateDebut <- paste0(gsub("[- :]", "", format(DateDebut, "%Y-%m-%d %H:%M:%S")), collapse = "")
      DateFin <- paste0(gsub("[- :]", "", format(DateFin, "%Y-%m-%d %H:%M:%S")), collapse = "")
    }else{
      
      DateDebut=as.POSIXct(time_list[1]) 
      DateFin=as.POSIXct(time_list[length(time_list)])
      nbpastemps <- as.numeric(difftime(DateFin,DateDebut,units = "mins"))/as.numeric(ListeCas$dt)
      # pas de temps de fin
      fintemps <- as.numeric(difftime(DateFin,DateDebut,units = "mins"))*60
      ListeCas$Annee <- format(DateDebut, "%Y")
      DateDebut <- paste0(gsub("[- :]", "", format(DateDebut, "%Y-%m-%d %H:%M:%S")), collapse = "")
      DateFin <- paste0(gsub("[- :]", "", format(DateFin, "%Y-%m-%d %H:%M:%S")), collapse = "")
      
      
      # nbpastemps <- as.numeric((strptime(DateFin,"%Y%m%d%H%M")-strptime(DateDebut,"%Y%m%d%H%M")),"mins")/as.numeric(ListeCas$dt)
      # # pas de temps de fin
      # fintemps <- as.numeric((strptime(DateFin,"%Y%m%d%H%M")-strptime(DateDebut,"%Y%m%d%H%M")),"mins")*60
      # ListeCas$Annee <- format(DateDebut, "%Y")
    }
    
    
    # nouveaux noms des colonnes en noms
    new_names <- seq(from =0, to = fintemps,by=(as.numeric(ListeCas$dt)*60) )
    names(shp_crop_lam93)[1:(ncol(shp_crop_lam93)-1)] <- new_names
    
    
    # ecriture de la couche des points 
    # st_write(shp_crop_lam93,file.path(dsnPluie,paste0(DateDebut,"_",bassin$NOM,".shp")),delete_dsn = TRUE)
    
    # calcul du cumul 
    rs1 <- calc(ras_crop, sum,na.rm = TRUE )
    rs1[which(values(rs1)==0)] <- NA
    # # mapview::mapview(bassin)+mapview::mapview(rs1)+mapview::mapview(shp_crop_lam93)
    # # mapview::mapview(bassin)+mapview::mapview(rs1) + mapview(shp_crop)
    writeRaster(rs1,
                filename =file.path(dsnPluie,
                                    paste0(bassin$NOM,"_",DateDebut,"_","cumul.tif")),
                overwrite=TRUE)
    
    #---------------------------------------creation de la pluie homogène
    
    bassin_homogene <- bassin %>%sfheaders::sf_remove_holes() %>% 
      st_union() %>% 
      st_simplify(preserveTopology = FALSE,
                  dTolerance = 10) %>%
      st_segmentize(20, crs = crs) %>% 
      st_buffer(150) %>% 
      st_as_sf()
    
    ras_extract <- raster::extract(ras_brick,bassin_homogene, weights = TRUE, normalizeWeights=TRUE, df= TRUE )
    
    ras_exrer <- ras_extract*ras_extract$weight
    
    ecn <- colSums(ras_exrer,na.rm = TRUE)/ListeCas$Umm
    ecn <- ecn[2:(length(ecn)-1)]
    
    initialize_vec <- seq(from = 0, to=0, length.out= length(names(ras_crop)))
    
    bassin_homogene <- bassin %>%sfheaders::sf_remove_holes() %>% 
      st_union() %>% 
      st_simplify(preserveTopology = FALSE,
                  dTolerance = 10) %>%
      st_segmentize(20, crs = crs) %>% 
      st_buffer(150) %>% 
      st_as_sf()
    
    ras_extract <- raster::extract(ras_brick,bassin_homogene, weights = TRUE, normalizeWeights=TRUE, df= TRUE )
    
    ras_exrer <- ras_extract*ras_extract$weight
    
    ecn <- colSums(ras_exrer)/ListeCas$Umm
    ecn <- ecn[2:(length(ecn)-1)]
    
    initialize_vec <- seq(from = 0, to=0, length.out= length(names(ras_crop)))
    
    moyenne <- data.frame(time=initialize_vec,
                          mean = initialize_vec)
    
    moyenne$time <- seq(0,ListeCas$dt*(length(moyenne$time)-1),by = ListeCas$dt)*60
    
    
    moyenne$mean <- as.numeric(ecn)
    # moyenne$mean <- as.numeric(cellStats(ras_crop,stat = "mean"))
    
    
    moyenne$posrain <- moyenne$mean>minmm
    # moyenne$posrainnext <- c(moyenne$mean[2:length(moyenne$mean)]>minmm,FALSE)
    # moyenne$posrainbefore <- c(FALSE,moyenne$mean[1:(length(moyenne$mean)-1)]>minmm)
    moyenne$posrainnext <- c(moyenne$mean[4:length(moyenne$mean)]>minmm,rep(FALSE,3))
    moyenne$posrainbefore <- c(rep(FALSE,3),moyenne$mean[1:(length(moyenne$mean)-3)]>minmm)
    
    moyenne$posraintrue <- moyenne$posrainbefore | moyenne$posrainnext | moyenne$posrain
    # if(length(which(moyenne$posraintrue>0))<4) next
    if(force_date==TRUE){
      starts=1
      ends=length(moyenne$time)
      
    }else{
      runs <- rle(moyenne$posraintrue)
      myruns = which(runs$values == TRUE & runs$lengths >= (180/ListeCas$dt))
      
      runs.lengths.cumsum = cumsum(runs$lengths)
      ends = runs.lengths.cumsum[myruns]
      
      newindex = ifelse(myruns>1, myruns-1, 0)
      starts = runs.lengths.cumsum[newindex] + 1
      
    }

    
    # if(length(starts)==0) next
    if (0 %in% newindex) starts = c(1,starts)
    if(length(starts)==0) next 
    if(length(starts)==0){
      lenstarts=length(starts)
      tab_decoup_evt <- data.frame(id=starts,
                                   starts=starts,
                                   ends=ends)
    }else{
      lenstarts=length(starts)
      tab_decoup_evt <- data.frame(id=1:lenstarts,
                                   starts=starts,
                                   ends=ends)
    }

    
    
    for (i_tab_evt in 1:length(tab_decoup_evt$id)) {
      tab_decoup_evt$summm[i_tab_evt] <- sum(moyenne$mean[tab_decoup_evt$starts[i_tab_evt]:tab_decoup_evt$ends[i_tab_evt]])
      
      # tab_decoup_evt$summm[i_tab_evt] <- sum(moyenne$mean[tab_decoup_evt$starts[i_tab_evt]:tab_decoup_evt$ends[i_tab_evt]])
    }
    tab_decoup_evt$todo <- tab_decoup_evt$summm > cumul_min
    tab_decoup_evt_test <- tab_decoup_evt
    tab_decoup_evt <- tab_decoup_evt[tab_decoup_evt$todo,]
    if(nrow(tab_decoup_evt)==0) next
    tab_decoup_evt$startime <- moyenne$time[tab_decoup_evt$starts]
    tab_decoup_evt$endtime <- moyenne$time[tab_decoup_evt$ends]
    for (idecoup in 1:length(tab_decoup_evt$id)) {
      
      if(tab_decoup_evt$startime[idecoup]>(nb_h_avant*60/(ListeCas$dt)*ListeCas$dt*60)){
        debtempsnew <- tab_decoup_evt$startime[idecoup]-(nb_h_avant*60/(ListeCas$dt)*ListeCas$dt*60)
      }else{
        debtempsnew <- tab_decoup_evt$startime[idecoup]
      }
      
      DateDebutnew <- strptime(DateDebut,"%Y%m%d%H%M")+debtempsnew
      
      if(tab_decoup_evt$endtime[idecoup]<(fintemps-(nb_h_apres*60/(ListeCas$dt)*ListeCas$dt*60))){
        fintempsnew <- tab_decoup_evt$endtime[idecoup]+(nb_h_apres*60/(ListeCas$dt)*ListeCas$dt*60)
      }else{
        fintempsnew <- tab_decoup_evt$endtime[idecoup]
      }
      
      
      # fintempsnew <- max(moyenne$time[which(moyenne$posraintrue==1)])+ListeCas$dt*60*2
      DataFinnew <- strptime(DateDebut,"%Y%m%d%H%M")+floor((fintempsnew/3600)+1)*3600
      # DataFinnew <- strptime(DateDebut,"%Y%m%d%H%M")+floor((fintempsnew/3600)+1)*3600
      
      
      ##AMC calcul
      tps_start <- tab_decoup_evt$startime[idecoup]/(3600*24)
      if(tps_start>=5){
        tps5d <- tab_decoup_evt$startime[idecoup]-(5*3600*24)
        
        sum_avant <- sum(moyenne$mean[which(moyenne$time==tps5d):tab_decoup_evt$starts[idecoup]])
        if(sum_avant<36){
          AMC=1
          
        }else if(sum_avant>=36 & sum_avant<=53){
          AMC=2
          
        }else{
          AMC=3
        }
      }else{
        
        tpsav <- 0
        
        sum_avant <- sum(moyenne$mean[which(moyenne$time==tpsav):tab_decoup_evt$starts[idecoup]])
        if(sum_avant<36){
          AMC=1
          
        }else if(sum_avant>=36 & sum_avant<=53){
          AMC=2
          
        }else{
          AMC=3
        }
        
      }
      
      
      diffs <- as.numeric(difftime(DataFinnew,DateDebutnew,units = "h"))*3600
      
      tpsh <- diffs %/% (60 * 60)
      
      diffh <- diffs-tpsh*(60 * 60)
      
      tpsm <- diffh %/% 60
      datedeb_format <- gsub('[^0-9]','',DateDebutnew)
      datedeb_format <- str_trunc(datedeb_format,width = 12,side = "right",ellipsis = "")
      hyeto_name=file.path(dsnPluie,
                           paste0("hyeto_",
                                  bassin$NOM,
                                  "_Evt",
                                  datedeb_format,#substr(row.names(tabhyeto[NSerie[1],]),1,12),
                                  "_",
                                  formatC(tpsh,width=2, flag="0"),
                                  "h",
                                  formatC(tpsm,width=2, flag="0"),
                                  "min_AMC",
                                  AMC,
                                  "_",
                                  ListeCas$EPSG,"_Evt_",
                                  ListeCas$Localisation,"_",
                                  ListeCas$dt,ListeCas$UnitTemps,"_",
                                  ListeCas$Code,"_",
                                  ListeCas$Source,"_",
                                  ListeCas$Annee))
      
      path_file <- paste0(hyeto_name,'.txt')
      file.create(path_file)
      first_line <- "#HYETOGRAPH FILE"
      second_line <- "#T (s) RAINFALL (mm)"
      datedebutstrp <- strptime(DateDebut,"%Y%m%d%H%M")
      diffsini <- as.numeric(difftime(DataFinnew,datedebutstrp,units = "h") )*3600
      moyennenew <- moyenne[which(moyenne$time>=debtempsnew & moyenne$time<=diffsini),]
      
      new_time <- seq(0,ListeCas$dt*(length(moyennenew$time)-1),by = ListeCas$dt)*60
      new_time[length(new_time)] <- 999000
      moyennenew$time <- new_time
      
      time_h <- as.vector(moyennenew$time) %>%
        as.character()
      pluie_h <- as.vector(moyennenew$mean) %>% 
        round(digits = 2) %>% 
        as.character()
      # pluie_h[1] <- 0
      # pluie_h[length(pluie_h)] <- 0
      tab_pluie <- paste(time_h,pluie_h,sep = " ")
      
      writeLines(first_line,path_file)
      write(second_line,path_file,append = TRUE)
      
      write(tab_pluie,path_file,append = TRUE)
      
      
      ##################################################################################
      ###############fichier pluie brute pour pluie spatialisée####################"####
      tab_timestep_pb <-  data.frame(Time=moyennenew$time,
                                     Val=moyennenew$mean)
      
      
      DUREE=diffs
      
      aire=st_area(CONTOURSINI[ibassin,])
      
      
      
      # On rajoute du temps en fonction de la taille du Bv
      # si V=1.5m/s, en 1h sur un rectangle longuer 2 largeur 1
      DUREE=round(12*(DUREE/3600+as.numeric(as.character(aire))/(3600*1800)))/12
      DUREE=DUREE*3600
      timemin <- seq(from = 60,to =DUREE,by=60 )
      tab_min_pb <- data.frame(Time=timemin,
                               Val=0*(1:length(timemin)))
      tab_timestep_pb <- tab_timestep_pb[-1,]
      tab_timestep_pb$Time[length(tab_timestep_pb$Time)]=tab_min_pb$Time[length(tab_min_pb$Time)]
      for (i_time in 1:length(tab_timestep_pb$Time)) {
        if(i_time==1){
          tab_min_pb$Val[1:which(tab_min_pb$Time==tab_timestep_pb$Time[i_time])]=tab_timestep_pb$Val[i_time]
        }else{
          
          # ind_which <- which(tab_min_pb$Time==tab_timestep_pb$Time[i_time])
          
          tab_min_pb$Val[(which(tab_min_pb$Time==tab_timestep_pb$Time[i_time-1])+1):which(tab_min_pb$Time==tab_timestep_pb$Time[i_time])]=tab_timestep_pb$Val[i_time]
        }
        
        
      }
      Brute_name=file.path(dsnPluie,
                           paste0("brute_",
                                  bassin$NOM,
                                  "_Evts",
                                  datedeb_format,#substr(row.names(tabhyeto[NSerie[1],]),1,12),
                                  "_",
                                  formatC(tpsh,width=2, flag="0"),
                                  "h",
                                  formatC(tpsm,width=2, flag="0"),
                                  "min_AMC",
                                  AMC,
                                  "_",
                                  ListeCas$EPSG,"_Evt_",
                                  ListeCas$Localisation,"_",
                                  ListeCas$dt,ListeCas$UnitTemps,"_",
                                  ListeCas$Code,"_",
                                  ListeCas$Source,"_",
                                  ListeCas$Annee))
      
      path_file <- paste0(Brute_name,'.txt')
      file.create(path_file)
      # first_line <- "#HYETOGRAPH FILE"
      # second_line <- "#T (s) RAINFALL (mm)"
      datedebutstrp <- strptime(DateDebut,"%Y%m%d%H%M")
      diffsini <- as.numeric(difftime(DataFinnew,datedebutstrp,units = "h") )*3600
      moyennenew <- moyenne[which(moyenne$time>=debtempsnew & moyenne$time<=diffsini),]
      
      new_time <- seq(0,ListeCas$dt*(length(moyennenew$time)-1),by = ListeCas$dt)*60
      new_time[length(new_time)] <- 999000
      moyennenew$time <- new_time
      
      time_h <- as.vector(moyennenew$time) %>%
        as.character()
      pluie_h <- as.vector(moyennenew$mean) %>% 
        round(digits = 2) %>% 
        as.character()
      # pluie_h[1] <- 0
      # pluie_h[length(pluie_h)] <- 0
      tab_pluie <- paste(time_h,pluie_h,sep = " ")
      
      # writeLines(first_line,path_file)
      # write(second_line,path_file,append = TRUE)
      tab_min_pb$Val <- round(tab_min_pb$Val,digits = 2)
      write.table(tab_min_pb,path_file,row.names = FALSE)
      
      #-----------------------------------------------------------
      
      # moyennenew <- moyenne[which(moyenne$time>=debtempsnew & moyenne$time<=diffsini),]
      
      
      ############ écriture du fichier de pluie spatialisée pour T2D#######################
      # tpsh <- fintemps %/% (60 * 60)
      # 
      # diffh <- fintemps-tpsh*(60 * 60)
      # 
      # tpsm <- diffh %/% 60
      
      spatial_name=file.path(dsnPluie,
                             paste0("spatial_",
                                    bassin$NOM,
                                    "_Evts",
                                    datedeb_format,#substr(row.names(tabhyeto[NSerie[1],]),1,12),
                                    "_",
                                    formatC(tpsh,width=2, flag="0"),
                                    "h",
                                    formatC(tpsm,width=2, flag="0"),
                                    "min_AMC",
                                    AMC,
                                    "_",
                                    ListeCas$EPSG,"_Evt_",
                                    ListeCas$Localisation,"_",
                                    ListeCas$dt,ListeCas$UnitTemps,"_",
                                    ListeCas$Code,"_",
                                    ListeCas$Source,"_",
                                    ListeCas$Annee))
      
      # writeRaster(rs1,paste0(spatial_name,"cumul.tif"),overwrite=TRUE)
      
      name_file <- nomlayerC
      path_file <- paste0(spatial_name,'.txt')
      file.create(path_file)
      first_line <- "#Pluie spatialisee"
      coords_pluie <- st_coordinates(shp_crop_lam93)
      
      X_pluie <- as.vector(coords_pluie[,1]) %>% 
        round(digits = 2) %>% 
        as.character()
      Y_pluie <- as.vector(coords_pluie[,2]) %>% 
        round(digits = 2) %>% 
        as.character()
      XY_pluie <- paste(X_pluie,Y_pluie,sep = ",")
      
      writeLines(first_line,path_file)
      
      nbs <- paste(as.character(length(XY_pluie)),as.character(nbpastemps),sep = ",")
      write(nbs,path_file,append = TRUE)
      
      
      write(XY_pluie,path_file,append = TRUE)
      pluie_df <- st_drop_geometry(shp_crop_lam93)
      pluie_df_updt <- pluie_df[,which(as.numeric(colnames(pluie_df))>=debtempsnew & as.numeric(colnames(pluie_df))<=diffsini)]
      
      colnames(pluie_df_updt) <- as.character(moyennenew$time)
      
      
      fn_pluie <- function(x){
        to_write <- paste(colnames(pluie_df_updt)[which(colnames(pluie_df_updt)==x)], paste(as.character(round(pluie_df_updt[,which(colnames(pluie_df_updt)==x)],digits = 2)),collapse = ", "),sep = ", ")
        write(to_write,path_file,append = TRUE)
      }
      
      sapply(colnames(pluie_df_updt), fn_pluie,simplify = TRUE,USE.NAMES = FALSE)
      
      
      
      #####################################################################################
      
      
      ####SI on veut faire des stats######################
      
    }
    
    
    
    ################Cumuls et statistiques#############################
    # if(calculcumul){
    #   cumul_homogene(moyenne,ncumul,bassin)
    # }
    # if(calculcumulsp){
    #   cumul_spatial(moyenne,ncumul,bassin,ras_crop)
    # }
    # if(do_stat){
    #   pluie_stats(ras_crop,nbpastemps,DateDebut,DateFin)
    # }
    
    if(do_stat){
      # cum_bassin <- raster::extract(x = ras_brick_fin_mm,y = bassin,df = TRUE,fun = "sum",weights = TRUE, normalizeWeights = TRUE)
      # cum_bassin_ <- raster::extract(x = ras_brick_fin_mm,y = bassin,df = TRUE,fun = "sum")
      ################SCS-CN#########################################
      # CN <- c(30,40,50,60,70,80,90,100)
      
      # S = 25.4*(1000/(CN-10))
      
      # R <- (P-0.2*S)^2/(P+0.8*S)
      
      names(ras_crop) <- as.character(new_names)
      seq.POSIXt(strptime(DateDebut,"%Y%m%d%H%M"),strptime(DateFin,"%Y%m%d%H%M"),by = paste(ListeCas$dt,ListeCas$UnitTemps))
      
      ras_crop_cumul <- ras_crop
      for (t in 2:length(names(ras_crop))) {
        
        ras_crop_cumul[[t]] <- sum(ras_crop[[t:1]])
        
        
        
      }
      names(ras_crop_cumul) <- as.character(new_names)
      cum_pas_detemps <- sum(ras_crop_cumul[[1]])
      date_evt <- seq.POSIXt(strptime(DateDebut,"%Y%m%d%H%M"),strptime(DateFin,"%Y%m%d%H%M"),by = paste(ListeCas$dt,ListeCas$UnitTemps))
      fun_stat <- c("max","mean","median","min","sd")
      
      #####################stats lame d'eau#######################
      initialize_vec <- seq(from = 0, to=0, length.out= length(names(ras_crop)))
      stats_lame_brute <- data.frame(date=date_evt,
                                     mean = initialize_vec,
                                     median = initialize_vec,
                                     min = initialize_vec,
                                     max = initialize_vec,
                                     sd = initialize_vec,
                                     var = initialize_vec,
                                     sum = initialize_vec )
      
      
      for (i in 1:length(fun_stat)) {
        
        stats_lame_brute[,i+1] <- as.numeric(cellStats(ras_crop,stat = fun_stat[i]))
        
      }
      
      stats_lame_brute <- data.frame(date=date_evt,
                                     stat_name = as.character(initialize_vec),
                                     stat_value = initialize_vec)
      stats_lame_brute_tmp <- data.frame(date=date_evt,
                                         stat_name = as.character(initialize_vec),
                                         stat_value = initialize_vec)
      
      for (i in 1:length(fun_stat)) {
        
        stats_lame_brute_tmp$stat_name <- fun_stat[i]
        stats_lame_brute_tmp$stat_value <- as.numeric(cellStats(ras_crop,stat = fun_stat[i]))
        
        if(i==1){stats_lame_brute <- stats_lame_brute_tmp }else{
          stats_lame_brute <- rbind.data.frame(stats_lame_brute,stats_lame_brute_tmp )
        }
        
        
      }
      
      stats_lame_brute$stat_name <- factor(stats_lame_brute$stat_name, levels =fun_stat )
      
      
      # stats_pluie_brute <- ggplot(stats_lame_brute)+
      #   geom_line(aes(x=date,y=stat_value,color = stat_name))
      stats_pluie_brute <- ggplot(stats_lame_brute,aes(x=date,y=stat_value,color=stat_name,fill=stat_name))+
        geom_col(position = 'identity')
      
      # stats_pluie_brute <- ggplot(stats_lame_brute)+
      #   geom_line(aes(x=date,y=stat_value,color = stat_name))
      couleurs <- c("red","black","purple","orange","tomato","cyan","red",'black',"purple",
                    "orange","tomato","cyan","red","black","purple","orange",
                    "tomato","cyan","red",'black',"purple","orange","tomato","cyan")
      for (ievtnew in 1:length(tab_decoup_evt$id)) {
        vline_val_deb <- strptime(DateDebut,"%Y%m%d%H%M")+tab_decoup_evt$startime[ievtnew]-(nb_h_avant*60/(ListeCas$dt)*ListeCas$dt*60)
        vline_val_fin <- strptime(DateDebut,"%Y%m%d%H%M")+tab_decoup_evt$endtime[ievtnew]+(nb_h_apres*60/(ListeCas$dt)*ListeCas$dt*60)
        stats_pluie_brute <- stats_pluie_brute  +
          geom_vline(xintercept = as.POSIXct(vline_val_deb),
                     colour=couleurs[ievtnew],linetype= 2,size=0.8)+
          geom_vline(xintercept = as.POSIXct(vline_val_fin),
                     colour=couleurs[ievtnew],linetype= 2,size=0.8)
        
        
      }
      dir.create(file.path(dsnPluie,CONTOURSINI$NOM[ibassin]))
      
      stats_pluie_brute_p <- plotly::ggplotly(stats_pluie_brute,dynamicTicks = TRUE)
      htmlwidgets::saveWidget(partial_bundle(stats_pluie_brute_p),
                              file = file.path(dsnPluie,CONTOURSINI$NOM[ibassin],paste0(DateDebut,"_",DateFin,".html")),
                              libdir = "lib")
      
      
      #####################stats cumul############################
      
      stats_lame_cumul <- data.frame(date=date_evt,
                                     mean = initialize_vec,
                                     median = initialize_vec,
                                     min = initialize_vec,
                                     max = initialize_vec,
                                     sd = initialize_vec,
                                     var = initialize_vec,
                                     sum = initialize_vec)
      
      for (i in 1:length(fun_stat)) {
        
        stats_lame_cumul[,i+1] <- as.numeric(cellStats(ras_crop_cumul,stat = fun_stat[i]))
        
      }
      
      
      
      
      stats_lame_cumul <- data.frame(date=date_evt,
                                     stat_name = as.character(initialize_vec),
                                     stat_value = initialize_vec)
      stats_lame_cumul_tmp <- data.frame(date=date_evt,
                                         stat_name = as.character(initialize_vec),
                                         stat_value = initialize_vec)
      
      for (i in 1:length(fun_stat)) {
        
        stats_lame_cumul_tmp$stat_name <- fun_stat[i]
        stats_lame_cumul_tmp$stat_value <- as.numeric(cellStats(ras_crop_cumul,stat = fun_stat[i]))
        
        if(i==1){stats_lame_cumul <- stats_lame_cumul_tmp}else{
          stats_lame_cumul <- rbind.data.frame(stats_lame_cumul,stats_lame_cumul_tmp)
        }
        
        
      }
      
      stats_lame_cumul$stat_name <- factor(stats_lame_cumul$stat_name, levels =fun_stat )
      
      
      stats_cumul <- ggplot(stats_lame_cumul)+
        geom_line(aes(x=date,y=stat_value,color = stat_name))
      
      
      
      stats_cumul_p <- plotly::ggplotly(stats_cumul,dynamicTicks = TRUE)
      # config(stats_cumul_p, scrollZoom = TRUE)
      htmlwidgets::saveWidget(partial_bundle(stats_cumul_p) ,
                              file = file.path(dsnPluie,CONTOURSINI$NOM[ibassin],paste0(DateDebut,"_",DateFin,"_cumul.html")),
                              libdir = "lib")
      
      for (icum in 1:length(nduree)) {
        
        nvalsum <- nduree[icum]
        
        
      }
      
    }
    if(calculcumul){
      cumul_homogene(moyenne,ncumul,bassin)
    }
    if(calculcumulsp){
      cumul_spatial(moyenne,ncumul,bassin,ras_crop)
    }
  }
}
}