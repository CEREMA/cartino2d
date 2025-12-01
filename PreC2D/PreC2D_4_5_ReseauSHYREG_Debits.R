#vPeut-être faire un masque avec le buffer autour des zones de débordements de cours d'eau
# peut-être regrouper les points très proches, un peut intutile d'injecter 90 et d'enlever 50 à 10m d'intervalles...
# mais je ne sais pas si c'est une si bonne idée pour montre en débit...
# peut-être garder les 2 points homogène avec le + et le -
# voir si un réseau est discontinu => problème endoréisme => alerte utilisateur
# repositionneent a tendance à décaler vers l'aval
# voir si on calure pas une distance au réseau bdtopo, si trop loin, avertissement utilisateur
# voir si c'est dans ce code que l'on essayer de positionner des sections de controle automatique

# ajouter les cuvettes, voir pq j'avais écrit cela...
# SecteurGRASS="C:/GRASSDATA/Test/Temp"
SecteurGRASS=SecteurGRASS=paste0(dirname(SecteurGRASS_),format(Sys.time(),format="%Y%m%d_%H%M%S"),"_","","/",basename(SecteurGRASS_))
unlink(dirname(SecteurGRASS),recursive=TRUE)
system(paste0(BatGRASS," -c EPSG:",EPSG," ",dirname(SecteurGRASS)," --text"))
system(paste0(BatGRASS," -c ",SecteurGRASS," --text"))

# On pourrait copier le débit dans un dossier cd2/debit
dsnlayerTravail=file.path(dsnlayerQ,'CalculDebits')

if (file.exists(dsnlayerTravail)==F){
  dir.create(dsnlayerTravail)
} else {
  unlink(dsnlayerTravail, recursive=TRUE)
  dir.create(dsnlayerTravail)
}

dsnLidar=dirname(TA_Lidar)
nomReseauQ=paste0("ras_QP_", Periode_retour_debit, "_france")

Type=c(1) # 1 Calcul complet sur un bv pour cartographie
# 2 Calcul Hydrométrie
# N'en garder qu'un, c'est mieux pour la vérif

##################### Lecture du raster de surface drainée ou shyreg
ras_Q = raster::raster(file.path(dsnlayerQ,paste0(nomReseauQ,".tif")))

# Lecture secteurs débits et buffer dessus ####################################################################
# ZoneCEB=st_read(nomZoneCEB)
ZoneCEB=CONTOURSINI
# nomZoneCEBBuf=file.path(dsnlayerTravail, paste0(substr(basename(nomZoneCEB),1,nchar(basename(nomZoneCEB))-5),"_Buf",distBufS,".gpkg"))
nomZoneCEBBuf=file.path(dsnlayerTravail, paste0(substr(basename(nomlayerC),1,nchar(basename(nomlayerC))-5),"_Buf",distBufS,".gpkg"))
ZoneCEB_Buf=st_buffer(ZoneCEB, distBufS)
st_write(obj = ZoneCEB_Buf, dsn = nomZoneCEBBuf, delete_layer = TRUE, quiet = TRUE)
id_deb_cum = 0
id_deb_cum_dec = 0

for (i_sect_deb in 1:dim(ZoneCEB_Buf)[1]){
  
  # Lidar #######################################################################################################
  Lidar=st_read(ZoneCEB$MNT[i_sect_deb])
  
  # id_deb <- ZoneCEB_Buf$Identifiant[i_sect_deb]
  id_deb <- ZoneCEB_Buf$NOM[i_sect_deb]
  
  id_splitted <- strsplit(id_deb, split="_|km")[[1]]
  id_deb_cum = id_deb_cum + as.integer(id_splitted[1])
  id_deb_cum_dec = id_deb_cum_dec + as.integer(id_splitted[2])
  
  ################################################################################################################
  # Calcul du réseau vecteur très classique
  nomReseauQ_thin_build=ReseauRasterVecteur(nomZone,nomZoneCEBBuf,dsnlayerTravail,dsnlayerQ,nomReseauQ,RValMin,id_deb)
  ReseauVect=st_read(nomReseauQ_thin_build)
  
  ################################################################################################################
  # section du réseau shyreg dans les zones Cours eau C2D grossier
  ReseauVect=st_cast(st_intersection(ReseauVect,ZoneCEB_Buf),"LINESTRING")
  ReseauVect$cat=1:dim(ReseauVect)[1]
  st_write(ReseauVect,file.path(dsnlayerTravail,paste0("ReseauVectcoupe.gpkg")), delete_layer=T, quiet=T)
  ################################################################################################################
  # on agglomere les vecteurs pour boucler sur chaque bassin versant
  Buff=st_sf(geometry=st_cast(st_union(st_buffer(ReseauVect,1)),"POLYGON"))
  Buff$Aire=st_area(Buff)
  Buff=Buff[order(Buff$Aire,decreasing = TRUE),]
  Buff=Buff[1,]
  st_write(Buff,file.path(dsnlayerTravail,paste0(nomReseauQ,"_Buff.gpkg")), delete_layer=T, quiet=T)
  
  ################################################################################################################
  # Modification pour repositionnement des sources
  # distBufS=distBufS+10
  distBufS=distBufS+20
  
  ################################################################################################################
  
  nb=st_intersects(ReseauVect,Buff)
  n_int = which(sapply(nb, length)>0)
  ExzSegm=ReseauVect[n_int,]
  
  ################################################################################################################
  # Classement du réseau
  Gagne=ReseauVecteurOrdo(ExzSegm,ras_Q)
  st_write(Gagne,file.path(dsnlayerTravail,paste0(nomReseauQ,"_",id_deb,"_Reseau.gpkg")), delete_layer=T, quiet=T)
  
  ################################################################################################################
  # Débit ponctuels avec + et -
  
  # temporaire
  Zone_ok_F=Buff  
  # browser()
  # Boucle inutile SHYREG mais si on veut faire plusieurs débits spécifiques, c'est déjà fait!
  Qps=0
  
  for (iQps in 1:length(Qps))
  {
    nom_base=paste0(nomReseauQ,"_",id_deb)
    nomQps=paste0(nom_base,"_Reseau_Pt_Ini")
    
    ###################################################################################################
    # Calcu des débits aux points sources
    # PourcAugm=1+10/100 # valeur pour savoir à partir de quelle augmentation on incrémente 10% par exemple
    PourcAugm=1+5/100 # valeur pour savoir à partir de quelle augmentation on incrémente 10% par exemple
    PointsC=CalculQps(Gagne,dsegm,Zone_ok_F,Qps[iQps],ras_Q,PourcAugm,intervPts)
    st_write(PointsC,file.path(dsnlayerTravail,paste0(nomQps,".gpkg")), delete_layer=T, quiet=T)
    DebMaxVerif=Qps[iQps]*max(PointsC$Qps)^0.8
    
    ###################################################################################################
    # récupération des dalles Lidar touchant des points sources
    nb = st_intersects(Lidar, st_buffer(PointsC,distBufS))
    n_int = which(sapply(nb, length) > 0)
    LidarC = Lidar[n_int,]
    dirname(ZoneCEB$MNT[i_sect_deb])
    # vrtfile=Creat_VRT(dsnLidar,LidarC,dsnlayerTravail)
    
    vrtfile=Creat_VRT(dirname(ZoneCEB$MNT[i_sect_deb]),LidarC,dsnlayerTravail)
    
    # zonedeb=st_union(st_intersection(st_buffer(Zone_ok_F,-5),st_buffer(PointsC,distBufS)))
    zonedeb=st_union(st_intersection(ZoneCEB,st_buffer(PointsC,distBufS)))
    # zonedeb=st_buffer(PointsC,distBufS)
    nomzonedeb=file.path(dirname(vrtfile),"zonedeb.gpkg")
    st_write(zonedeb,nomzonedeb, delete_layer=T, quiet=T)
    SecteurGRASS=SecteurGRASS=paste0(dirname(SecteurGRASS_),format(Sys.time(),format="%Y%m%d_%H%M%S"),"_","","/",basename(SecteurGRASS_))
    unlink(dirname(SecteurGRASS),recursive=TRUE)
    system(paste0(BatGRASS," -c EPSG:",EPSG," ",dirname(SecteurGRASS)," --text"))
    system(paste0(BatGRASS," -c ",SecteurGRASS," --text"))
    # browser()
    tabs=CalculsPosSources(PointsC,nomzonedeb,vrtfile,dsnlayerTravail)
    # tabs=cbind(st_coordinates(PointsC),0)
    
    ###################################################################################################
    # Positionnement et réalisation fichiers Telemac Complet
    for (iType in Type)
    {
      SourcesDebits=CalculsSourcesComplet(PointsC,tabs,dsnlayerTravail,iType)
      
      st_write(SourcesDebits,file.path(dsnlayerTravail,paste0(nomQps,"_SourcesDebits",iType,".gpkg")), delete_layer=T, quiet=T)
      nomqml=file.path(dsnlayerTravail,paste0(nomQps,"_SourcesDebits",iType,".qml"))
      file.copy(file.path(chem_routine,"Qml","SourcesDebits.qml"),nomqml)
      cat("#############################################################################\n")
      cat(file.path(dsnlayerTravail,paste0(nomQps,"_SourcesDebits",iType,".gpkg"))," - Verif cumul - Max raster ",max(PointsC$QpsMax)," - Sommes points ",sum(SourcesDebits$DeltaQps),"\n")
      cat("#############################################################################\n")
      # browser() # enlever le browser( si cela embete)
    }
    
    # ###################################################################################################
    print(paste(round(DebMaxVerif^0.8),sum(PointsC$DeltaQps)))
    write.table(paste(round(DebMaxVerif^0.8),sum(PointsC$DeltaQps)),file.path(dsnlayerTravail,paste0(nomQps,"_verif.txt")))
  }
  ###################################################################################################
  st_write(Zone_ok_F,file.path(dsnlayerTravail,paste0(nom_base,"_Zone_ok_F.gpkg")), delete_layer=T, quiet=T)
  
}

dsnSourceDeb <- file.path(dsnlayer, "_SourcesDebits")

# Zone d'étude
zone <- st_read(file.path(dsnlayer, nomlayerC))

if (zone$NOMPOST[1] != ''){
  if (dir.exists(file.path(dsnSourceDeb, zone$NOMPOST[1])) == FALSE & zone$NOMPOST[1] != ''){
    dir.create(file.path(dsnSourceDeb, zone$NOMPOST[1]))
  }
  dsnSourceDeb <- file.path(dsnSourceDeb, zone$NOMPOST[1])
} else {
  id_global <- paste0("DEB", id_deb_cum, "_", id_deb_cum_dec)
  dir.create(file.path(dsnSourceDeb, id_global))
  dsnSourceDeb <- file.path(dsnSourceDeb, id_global)
}

dsnSourceTxt <- file.path(dsnlayerTravail,"_SourcesDebits")

if (dir.exists(file.path(dsnSourceDeb, "_SourcesDebits")) == TRUE){
  unlink(file.path(dsnSourceDeb, "_SourcesDebits"), recursive = TRUE)
}

fs::dir_copy(dsnSourceTxt, file.path(dsnSourceDeb, "_SourcesDebits"), overwrite = TRUE)

sources_files <- list.files(path = dsnlayerTravail, pattern = "*SourcesDebits*", full.names = TRUE)
sources_files <- sources_files[which(sources_files!=file.path(dsnlayerTravail,"_SourcesDebits"))]
file.copy(from = sources_files, dsnSourceDeb, overwrite = TRUE)

################################################################################################################
################################################################################################################
################################################################################################################