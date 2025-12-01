BufPlusMoins=function(nominput,seuilSup,BufPlus,BufMoins,typbord,dsnlayer,ResModif,Qgis,SecteurGRASS)
{
  nomtmp_Buf1="BufPlus"
  cmd=paste0("r.buffer --quiet --overwrite input=",nominput," output=",nomtmp_Buf1," distance=",BufPlus)
  print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
  
  cmd=paste0("r.mask -i --quiet --overwrite raster=",nomtmp_Buf1)
  print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
  
  nomtmp_Buf2="BufPlusInv"
  cmd=paste0("r.resample --quiet --overwrite input=","MASK"," output=",nomtmp_Buf2)
  print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
  
  cmd=paste0("r.mask -r")
  print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
  
  nomtmp_Buf3="BufPlusInvBufMoins"
  cmd=paste0("r.buffer --quiet --overwrite input=",nomtmp_Buf2," output=",nomtmp_Buf3," distance=",BufMoins)
  print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
  
  cmd=paste0("r.mask -i --quiet --overwrite raster=",nomtmp_Buf3)
  print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
  
  nomtmp_Buf4=paste0(nominput,"BufPlusInvBufMoinsInv")
  cmd=paste0("r.resample --quiet --overwrite input=","MASK"," output=",nomtmp_Buf4)
  print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
  
  cmd=paste0("r.mask -r")
  print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
  
  # Possible modification de la résolution
  if (ResModif>0)
  {
    cmd=paste0("g.region --overwrite --quiet -a raster=",nomtmp_Buf4," res=",as.character(ResModif))
    print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
  }
  
  nomvectsortie=file.path(dsnlayer,paste0(raci,"_",nominput,".gpkg"))

  # Méthode a priori à privilégier (plus rapide)
  if (Qgis==1)
  {
    unlink(nomvectsortie)
    DossBuf=paste0("BufPlus",BufPlus,"_Moins",BufMoins)
    if (file.exists(file.path(dsnlayer,DossBuf))==F){dir.create(file.path(dsnlayer,DossBuf))}
    nomsortie=file.path(dsnlayer,DossBuf,paste0(raci,"_",nominput,"_Rast.tif"))
    cmd=paste0("r.out.gdal --quiet --overwrite -c -f input=",nomtmp_Buf4," output=",nomsortie," format=GTiff")
    print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd)) 

    # qgis_process run gdal:polygonize --distance_units=meters --area_units=m2 --ellipsoid=EPSG:7019 --INPUT='C:/Cartino2D/France/MAMP2024_05m/_FUSION/MAMP2024_05mtSceMaxMedContFrance__ZI_Rast.tif' 
    # --BAND=1 --FIELD=DN --EIGHT_CONNECTEDNESS=false --EXTRA= --OUTPUT='C:/Cartino2D/France/MAMP2024_05m/_FUSION/testrastvect_tout2.gpkg'
    # Polygoniser
    cmd <- paste0(qgis_process, " run gdal:polygonize",
                  " --distance_units=meters --area_units=m2 --ellipsoid=EPSG:7019",
                  " --INPUT=", shQuote(nomsortie),
                  " --BAND=1 --FIELD=DN --EIGHT_CONNECTEDNESS=false --EXTRA=",
                  " --OUTPUT=", shQuote(nomvectsortie))
    print(cmd); system(cmd)
    
    # qgis_process run gdal:polygonize
    # --distance_units=meters --area_units=m2 --ellipsoid=EPSG:7019
    # --INPUT='C:/AFFAIRES/MAMP/ALEAS/ALEAS/BufPlus2_Moins2/ALEA_MAMP_P100_SEPIA_BufPlus2BufMoins2_ZI101_Rast.tif'
    # --BAND=1 --FIELD=DN --EIGHT_CONNECTEDNESS=false --EXTRA= 
    #   --OUTPUT='C:/AFFAIRES/MAMP/ALEAS/ALEAS/ALEA_MAMP_P100_SEPIA_BufPlus2BufMoins2_ZI101mano.gpkg'
    
  }else{
    # ancienne méthode
    cmd=paste0("r.to.vect ",typbord," --quiet --overwrite input=",nomtmp_Buf4," output=",nomtmp_Buf4," type=area")
    
    
    print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
    nomtmp_Buf5="FinBricolage"
    cmd=paste0("v.clean --quiet --overwrite input=",nomtmp_Buf4," output=",nomtmp_Buf5," type=area tool=rmarea threshold=",seuilSup)
    print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
    
    cmd=paste0("v.out.ogr --quiet --overwrite input=",nomtmp_Buf5," output=",nomvectsortie," format=GPKG")
    print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))

    
  }
  cat("Fichier vecteur:",nomvectsortie,"\n")
  return(nomtmp_Buf4)
}

#-----------------------------------------------------------------------
#----------------RexPluie_CasPluie----------------------------
#-----------------------------------------------------------------------
# Gestion des differents formats de pluie

RexPluie_CasPluie=function()
{
  library(readxl)
  chem_routine=R.home(component = "Cerema")
  # paste0(chem_routine,"/PreC2D")
  Link_Data = "Data_PluieRaster.xlsx"
  if (file.exists(file.path(chem_routine,"PreC2D",Link_Data))==F){cat("le fichier",Link_Data,"n'existe pas\n");BOOM=Data_PluieRasterxlsxamettredans}
  Link_DataXLS = read_excel(file.path(chem_routine,"PreC2D",Link_Data))
  Link_DataXLS=Link_DataXLS[order(Link_DataXLS$NomCas),]
  nchoix = select.list(Link_DataXLS$NomCas,title = "Choix", multiple = T, graphics = T)
  Search = which(Link_DataXLS$NomCas %in% nchoix)
  ListeCas = as.data.frame(Link_DataXLS[Search,])
  iShyreg=which(Link_DataXLS$TypPluie=="Stat" & Link_DataXLS$EPSG==ListeCas$EPSG)
  if(length(iShyreg)>0){
    Link_DataXLS$Dossier_PluieRaster[iShyreg]
    ListeCas$Dossier_SHYREG=Link_DataXLS$Dossier_PluieRaster[iShyreg]
  }
  return(ListeCas)
}

#-----------------------------------------------------------------------
#----------------RexPluie_RecupListeRaster----------------------------
#-----------------------------------------------------------------------
# Liste des fichiers dispos
RexPluie_RecupListeRasterEvt=function(DateDebut,DateFin,ListeCas)
{
  nrast=as.numeric((strptime(DateFin,"%Y%m%d%H%M")-strptime(DateDebut,"%Y%m%d%H%M")),"mins")/as.numeric(ListeCas$dt)
  print(nrast)
  inc=0
  list_Periode = list()
  periode=1
  ok=0
  for (i in 0:nrast)
  {
    dateencourstemp=strptime(DateDebut,"%Y%m%d%H%M")+i*as.numeric(ListeCas$dt)*60
    dateencours=strftime(dateencourstemp,"%Y%m%d%H%M")
    ##################################################
    incd=1
    if (ListeCas$Nbredate>0)
    {
      for (id in 1:ListeCas$Nbredate)
      {
        if (incd==1)
        {
          dateText= substr(dateencours,1,ListeCas$LgDate)
        }else{
          dateText= paste0(dateText= substr(dateencours,1,ListeCas$LgDate),"_",substr(dateencours,1,ListeCas$LgDate))
        }
      }
    }
    NomType=ifelse(is.na(ListeCas$NomType)==F,ListeCas$NomType,"")
    Nomplus=ifelse(is.na(ListeCas$Nomplus)==F,ListeCas$Nomplus,"")
    
    nomtemp=paste0(NomType,dateText,Nomplus,ListeCas$Extension)
    
    #################################
    if(i==1){print(file.path(ListeCas$Dossier_PluieRaster,nomtemp))}
    if (file.exists(file.path(ListeCas$Dossier_PluieRaster,nomtemp))=="TRUE")
    {
      if (inc==0)
      {
        print(paste("Periode",periode))
        print(nomtemp)
        NomRast=nomtemp
        inc=1
      } else{
        NomRast=cbind(NomRast,nomtemp)
        ok=1
      }
    }else{
      if (inc==1)
      {
        print(nomtemp)
        list_Periode[[periode]]=NomRast
        periode=periode+1
        inc=0
        ok=0
      }
    }
    if(ok==1)
    {
      list_Periode[[periode]]=NomRast
    }
  }
  return(list_Periode)
}

#-----------------------------------------------------------------------
#----------------RexPluie_RecupListeRaster----------------------------
#-----------------------------------------------------------------------
# Liste des fichiers dispos
RexPluie_RecupListeRasterStat=function(nduree,NPerRet,ListeCas)
{
  list_Periode = list()
  periode=1
  inc=0
  for (iduree in nduree)
  {
    for (iretour in NPerRet)
    {
      nomtemp=paste0("PM",formatC(iduree ,width=2, flag="0"),"_",formatC(iretour ,width=4, flag="0"),".asc")
      if (file.exists(file.path(ListeCas$Dossier_PluieRaster,nomtemp))=="TRUE")
      {
        if (inc==0)
        {
          NomRast=nomtemp
          inc=1
        } else{
          NomRast=cbind(NomRast,nomtemp)
        }
      }
    }
  }
  list_Periode[[periode]]=NomRast
  return(list_Periode)
}




cumul_homogene <- function(moyenne,ncumul,bassin){
  
  ncumul_select <- ncumul[which(ncumul<=floor(max(moyenne$time)/3600))]
  
  pasdetemps <- moyenne$time[2]-moyenne$time[1]
  ncumul_select <- ncumul_select[which(ncumul_select*3600> pasdetemps)]
  ncumulnoms_select <- ncumulnoms[1:length(ncumul_select)]
  noms_fichiers <- function(x){
    
    paste0(bassin$NOM,"_cumul_",x)
  }
  
  ncumulnoms_select <- sapply(ncumulnoms_select,noms_fichiers ,simplify = TRUE,USE.NAMES = FALSE)
  cumuls_list <- list_along(ncumulnoms_select)
  names(cumuls_list) <- ncumulnoms_select
  dir.create(file.path(dsnPluie,bassin$NOM))
  
  for(icumul in 1:length(ncumul_select)){
    
    # ncml <- ncumul_select[icumul]
    
    debutcalcul <- ncumul_select[icumul]*3600/pasdetemps+1
    
    vecttemps <- moyenne$time[debutcalcul:length(moyenne$time)]
    vectcumul <- rep(NA,length(debutcalcul:length(moyenne$time)))
    
    df_cumul <- data.frame(Temps = vecttemps,
                           cumul = vectcumul )
    
    cumulpluie <- function(x){
      sum(moyenne$mean[which(moyenne$time==x):(which(moyenne$time==x)-debutcalcul+1)])
    }
    
    df_cumul$cumul <-  sapply(X =df_cumul$Temps,FUN = cumulpluie,simplify = TRUE,USE.NAMES = FALSE )
    df_cumul$Date <- df_cumul$Temps+ strptime(DateDebut,"%Y%m%d%H%M")
    
    cumuls_list[[icumul]] <- df_cumul
    
    
    
    
    
  }
  cumul_file <- file.path(dsnPluie,bassin$NOM,paste0(DateDebut,"_cumuls.rds"))
  
  saveRDS(cumuls_list, file = cumul_file)
}


cumul_spatial <- function(moyenne,ncumul,bassin,ras_crop){
  
  ncumul_select <- ncumul[which(ncumul<=floor(max(moyenne$time)/3600))]
  
  pasdetemps <- moyenne$time[2]-moyenne$time[1]
  ncumul_select <- ncumul_select[which(ncumul_select*3600> pasdetemps)]
  ncumulnoms_select <- ncumulnoms[1:length(ncumul_select)]
  noms_fichiers <- function(x){
    
    paste0(bassin$NOM,"_",DateDebut,"_cumul_",x)
  }
  
  ncumulnoms_select <- sapply(ncumulnoms_select,noms_fichiers ,simplify = TRUE,USE.NAMES = FALSE)
  cumuls_list_sp <- list_along(ncumulnoms_select)
  names(cumuls_list_sp) <- ncumulnoms_select
  # ras_crop_new <- ras_crop
  # names(ras_crop_new) <- moyenne$time
  
  empty_ras <- raster(ext=raster::extent(ras_crop),
                      crs=raster::crs(ras_crop),
                      resolution = raster::res(ras_crop))
  values(empty_ras) <- NA
  
  for (icumulsp in 1:length(ncumul_select)) {
    
    
    debutcalcul <- ncumul_select[icumulsp]*3600/pasdetemps+1
    
    vecttemps <- moyenne$time[debutcalcul:length(moyenne$time)]
    vectcumul <- rep(NA,length(debutcalcul:length(moyenne$time)))
    
    df_cumul <- data.frame(Temps = vecttemps,
                           cumul = vectcumul )
    
    # rowSums(values(ras_crop[[which(moyenne$time==x):(which(moyenne$time==x)-debutcalcul+1)]]),na.rm = TRUE)
    cumulpluiesp <- function(x){
      rowSums(values(ras_crop[[which(moyenne$time==x):(which(moyenne$time==x)-debutcalcul+1)]]),na.rm = TRUE)
    }
    vals_cumul <- lapply(X =df_cumul$Temps,FUN = cumulpluiesp)
    ras_stackcum <- list(empty_ras)[rep(1,length(vals_cumul))]
    ras_stackcum <- do.call(raster::stack,ras_stackcum)
    # for (icum in 1:vals_cumul) {
    #   
    #   
    #   
    # }
    
    for (iras in 1:length(vals_cumul)) {
      
      values(ras_stackcum[[iras]]) <- vals_cumul[[iras]]
    }
    
    cumuls_list_sp[[icumulsp]] <- ras_stackcum
    
    
    
    
  }
  cumulsp_file <- file.path(dsnPluie,bassin$NOM,paste0(DateDebut,"_cumuls_sp.rds"))
  
  saveRDS(cumuls_list_sp, file = cumulsp_file)
  # save(cumuls_list_sp, file = cumulsp_file)
  # 
  # load(cumulsp_file)
  
  
  
}



pluie_stats <- function(ras_crop,nbpastemps,DateDebut,DateFin){
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
  fun_stat <- c("mean","median","min","max","sd")
  
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
  
  
  # stats_pluie_brute <- ggplot(stats_lame_brute,aes(x=date))+
  #   geom_line(aes(y=stat_value,color = stat_name))
  
  stats_pluie_brute <- ggplot(stats_lame_brute,aes(x=date,y=stat_value,color=stat_name,fill=stat_name))+
    geom_bar(stat="identity",position='dodge')
  
  
  stats_pluie_brute_p <- plotly::ggplotly(stats_pluie_brute,dynamicTicks = TRUE)
  htmlwidgets::saveWidget(partial_bundle(stats_pluie_brute_p),
                          file = file.path(dsnPluie,bassin$NOM,paste0(DateDebut,"_",DateFin,".html")),
                          libdir = "lib")
  
  # saveRDS(stats_pluie_brute_p,file =file.path(dsnPluie,bassin$NOM,paste0(DateDebut,"_",DateFin,".rds")) )
  
  
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
                          file = file.path(dsnPluie,bassin$NOM,paste0(DateDebut,"_",DateFin,"_cumul.html")),
                          libdir = "lib")
  
  
  # for (icum in 1:length(nduree)) {
  #   
  #   nvalsum <- nduree[icum]
  #   
  #   
  # }
  
}


#-----------------------------------------------------------------------
#----------------DataObs_choix----------------------------
#-----------------------------------------------------------------------
# Gestion des differents formats de pluie

DataObs_choix=function()
{
  library(readxl)
  chem_routine=R.home(component = "Cerema")
  paste0(chem_routine,"/PreC2D")
  Link_Data = "Data_obs.xlsx"
  Link_DataXLS = read_excel(file.path(chem_routine,"PreC2D",Link_Data))
  Link_DataXLS=Link_DataXLS[order(Link_DataXLS$Evenements),]
  nchoix = select.list(Link_DataXLS$Evenements,title = "Choix", multiple = T, graphics = T)
  Search = which(Link_DataXLS$Evenements %in% nchoix)
  ListeObs = as.data.frame(Link_DataXLS[Search,])
  # iShyreg=which(Link_DataXLS$TypPluie=="Stat" & Link_DataXLS$EPSG==ListeCas$EPSG)
  # Link_DataXLS$Dossier_PluieRaster[iShyreg]
  # ListeCas$Dossier_SHYREG=Link_DataXLS$Dossier_PluieRaster[iShyreg]
  return(ListeObs)
}

