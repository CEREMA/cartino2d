#################################################################################################
################   Création VRT
#################################################################################################
Creat_VRT=function(dsnLidar,LidarC,dsnlayerTravail)
{
  # Création de la liste des dalles concernées
  listeASC=paste0(dsnLidar,'/',LidarC$DOSSIERASC,'/',LidarC$NOM_ASC)
  
  nom_ascvrt=file.path(dsnlayerTravail,"listepourvrt.txt")
  file.create(nom_ascvrt)
  write(listeASC, file=nom_ascvrt, append=T)
  vrtfile = paste0(dsnlayerTravail,"/",nom_base,".vrt") ##chemin du vrt à créer
  cmd = paste(shQuote(OSGeo4W_path), "gdalbuildvrt",vrtfile,"-input_file_list",nom_ascvrt) ## commande pour exécuter gdalbuildvrt
  system(cmd) 
  return(vrtfile)
  # cmd = paste(shQuote(OSGeo4W_path), "gdalbuildvrt",vrtfile,"-input_file_list",nom_ascvrt) 
}

# #################################################################################################
# ################   Ré-échantillonage
# #################################################################################################
# ReechantillonageRaster=function(nomok,Reso,dsnlayer)
# {
#   execGRASS("g.region",flags=c("quiet","overwrite"),parameters=list(raster=nomok))
#   
#   # Mieux travailler la région
#   execGRASS("g.region",flags=c("a","quiet","overwrite"),parameters=list(raster=nomok,res=as.character(Reso)))
#   
#   nomfinal=paste0(nomok,"_",formatC(Reso,width = 3,flag = "0"),"m")
#   execGRASS("r.resamp.stats",flags=c("quiet","overwrite"),parameters=list(input=nomok,output=nomfinal))
#   
#   execGRASS("g.region",flags=c("quiet","overwrite"),parameters=list(zoom=nomfinal))
#   
#   dsnlayerSortie=file.path(dsnlayer,paste0("MNT",formatC(Reso,width = 3,flag = "0"),"m"))
#   if (dir.exists(dsnlayerSortie)==F) {dir.create(dsnlayerSortie)}
#   execGRASS("r.out.gdal",flags=c("quiet","overwrite"),parameters=list(input=nomfinal,output=file.path(dsnlayerSortie,paste0(nomok,"_",formatC(Reso,width = 3,flag = "0"),"m.asc")),format="AAIGrid",nodata=-9999, createopt="DECIMAL_PRECISION=2"))
# }

#################################################################################################
################   Conevrtit un réseau hydrologique vecteur en raster
#################################################################################################
# ReseauRasterVecteur=function(masque,dsnExzeco,nomExzeco,surfMin)
ReseauRasterVecteur=function(nomZone,nomZoneCEBBuf,dsnlayerTravail,dsnlayerQ,nomReseauQ,RValMin,id_deb)
{
  
  # requete = paste0("Identifiant=", shQuote(id_deb))
  requete = paste0("NOM=", shQuote(id_deb))
  cmd=paste0("v.in.ogr --quiet --overwrite input=",nomZoneCEBBuf," output=zone where=",shQuote(requete))
  print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
  
  cmd=paste0("g.region --quiet --overwrite vector=zone")
  print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
  
  # r.in.gdal -o input=D:\MAMP\SectorisationFP\SHYREG\QP1000_MARSEILLE.asc output=SHYREG
  cmd=paste0("r.in.gdal -o -r --quiet --overwrite input=",file.path(dsnlayerQ,paste0(nomReseauQ,".tif"))," output=",nomReseauQ)
  print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
  
  cmd=paste0("g.region --quiet --overwrite raster=",nomReseauQ)
  print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
  
  cmd=paste0("r.mask --quiet --overwrite vector=zone")
  print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
  
  # r.mapcalc expression=SHYREG_1 = if(SHYREG>0,1,null())
  nomReseauQ_1=paste0(nomReseauQ,"_1")  
  cmd=paste0("r.mapcalc --overwrite ",shQuote(paste0(nomReseauQ_1," =if(",nomReseauQ,">",RValMin,",1,null())")))
  print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
  
  cmd=paste0("r.mask -i --quiet --overwrite raster=", nomReseauQ_1)
  print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
  
  cmd=paste0("r.resample input=MASK output=masque")
  print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
  
  cmd=paste0("r.mask -r")
  system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
  
  cmd=paste0("r.to.vect --quiet --overwrite input=masque output=verif_boucle type=area")
  print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
  
  masqueinv_verif_nom <- file.path(dsnlayerTravail,paste0("masqueinv_verif.gpkg"))
  cmd=paste0("v.out.ogr --quiet --overwrite input=verif_boucle output=", masqueinv_verif_nom," format=GPKG")
  print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
  
  masqueinv_verif <- st_read(masqueinv_verif_nom)
  if(dim(masqueinv_verif)[1]>1){
    masqueinv_verif$Aire <- st_area(masqueinv_verif)
    masqueinv_verif=masqueinv_verif[order(masqueinv_verif$Aire,decreasing = TRUE),]
    masqueinv_verif=masqueinv_verif[-1,]
    units(masqueinv_verif$Aire)=NULL
    ici=which(masqueinv_verif$Aire>10*50*50)
    if (length(ici)>0){masqueinv_verif=masqueinv_verif[-ici,]}
    pixels_isoles <- file.path(dsnlayerTravail,paste0("pixels_isoles.gpkg"))
    st_write(masqueinv_verif, pixels_isoles, delete_layer=TRUE, quiet=TRUE)
    
    cmd=paste0("v.in.ogr --quiet --overwrite input=",pixels_isoles," output=pixels_isoles")
    print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
    
    cmd=paste0("v.to.rast --quiet --overwrite input=pixels_isoles output=pixels_isoles_rast use=val val=1")
    print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
    
    nomReseauQ_1_pixIso=paste0(nomReseauQ,"_1_pixIso")  
    cmd=paste0("r.series --quiet --overwrite input=", nomReseauQ_1, ",pixels_isoles_rast output=", nomReseauQ_1_pixIso," method=maximum")
    print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
    nomReseauQ_1=nomReseauQ_1_pixIso
  }
  
  # r.thin input=SHYREG_1@Temp output=SHYREG_thin
  nomReseauQ_thin=paste0(nomReseauQ_1,"_thin")  
  cmd=paste0("r.thin  --quiet --overwrite input=",nomReseauQ_1," output=",nomReseauQ_thin)
  print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
  
  # r.to.vect input=SHYREG_thin@Temp output=SHYREG_vect type=line
  cmd=paste0("r.to.vect --quiet --overwrite input=",nomReseauQ_thin," output=",nomReseauQ_thin," type=line")
  print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
  # browser()
  afairealamainsithinmauvais=0
  if (afairealamainsithinmauvais==1)
  {
    nomReseauQ_1=paste0(nomReseauQ,"_1")  
    # r.thin input=SHYREG_1@Temp output=SHYREG_thin
    nomReseauQ_thin=paste0(nomReseauQ_1,"_thin")  
    cmd=paste0("r.thin  --quiet --overwrite input=",nomReseauQ_1," output=",nomReseauQ_thin)
    print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
    
    # r.to.vect input=SHYREG_thin@Temp output=SHYREG_vect type=line
    cmd=paste0("r.to.vect --quiet --overwrite input=",nomReseauQ_thin," output=",nomReseauQ_thin," type=line")
    print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
  }
  
  # v.build.polylines input=SHYREG_vect@Temp output=SHYREG_vect_build
  nomReseauQ_thin_build=paste0(nomReseauQ,"_vect") 
  cmd=paste0("v.build.polylines --quiet --overwrite input=",nomReseauQ_thin," output=",nomReseauQ_thin_build," type=line")
  print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
  
  #v.out.ogr input=SHYREG_vect_build@Temp output=D:\MAMP\SectorisationFP\SHYREG\SHYREG_vect_build.gpkg format=GPKG
  # v.out.ogr -c input=SHYREG_vect_build@Temp output=D:\MAMP\SectorisationFP\SHYREG\testvect.gpkg format=GPKG
  nom_reseau=file.path(dsnlayerTravail,paste0(nomReseauQ_thin_build,".gpkg"))
  cmd=paste0("v.out.ogr -c --quiet --overwrite input=",nomReseauQ_thin_build," output=",nom_reseau," format=GPKG")
  print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
  
  return(nom_reseau)
}

#################################################################################################
################   Ordonne un réseau hydrologique vecteur
#################################################################################################
ReseauVecteurOrdo=function(ExzSegm,ras_Q)
{
  # ExzSegm = Lignes composant le vecteur ; pour le test il y a 104 lignes
  PtsSegm=st_coordinates(ExzSegm) # PtsSegm : X, Y et n°Ligne
  Indi=1:dim(PtsSegm)[1] # Indi : Nombre de points composant les différentes lignes ; il y en a 327 pour le test
  LINKNO=-1
  DSLINKNO=-1
  USLINKNO=-1
  USLINKNO2=-1
  PtsSegm=cbind(PtsSegm,Indi,LINKNO,DSLINKNO,USLINKNO,USLINKNO2)
  
  # # Convertir en objet sf (simple feature)
  # sf_points <- st_as_sf(PtsSegm, coords = c("X", "Y"), crs = 2154) # EPSG:4326 pour WGS84
  # 
  # # Exporter vers un fichier shapefile
  # st_write(sf_points, "D:\\Shyreg_Debit\\CalculDebits\\points_petit.shp")
  
  SdCell = raster::extract(ras_Q, (PtsSegm[,1:2])) # Extraction des valeurs de débit de pointe au niveau du réseau délimité
  
  # Rajout pour pouvoir modifier des valeurs à la main
  data=tibble(
    X=PtsSegm[,1],
    Y=PtsSegm[,2],
    Z=SdCell
  )
  
  
  # Créer un objet sf à partir des coordonnées X et Y, en conservant Z comme attribut
  sf_points <- data %>%
    st_as_sf(coords = c("X", "Y"))
  
  # Vérifier que Z est bien un attribut
  print(sf_points)
  
  # Enregistrer l'objet sf en tant que fichier GeoPackage
  st_write(sf_points, file.path(dsnlayerTravail,"Pts_SHYREG.gpkg"), delete_dsn=T,delete_layer=TRUE, quiet=TRUE)
  RetourUser=st_read(file.path(dsnlayerTravail,"Pts_SHYREG.gpkg"))
  SdCell=RetourUser$Z
  
  IndPtAval=which(SdCell==max(SdCell)) # Points en aval où le débit de pointe est donc maximal
  nL_PtAval = unique(PtsSegm[IndPtAval,3])
  
  inverse=TRUE
  
  if (length(nL_PtAval)>1){
    cat("Attention : le nombre de lignes avec valeurs maximales est supérieur à 1, modif mais pas sur que ça marche tout le temps")
    ici=which(sapply(1:length(IndPtAval), function(x) {length(which(PtsSegm[IndPtAval,1]==PtsSegm[IndPtAval[x],1] & PtsSegm[IndPtAval,2]==PtsSegm[IndPtAval[x],2]))})>1)
    SdCell[IndPtAval[ici]]=SdCell[IndPtAval[ici]]-0.1
    IndPtAval=which(SdCell==max(SdCell)) # Points en aval où le débit de pointe est donc maximal
    
    nL_PtAval = unique(PtsSegm[IndPtAval,3])
    if (length(nL_PtAval)>1){
      cat("Erreur : le nombre de lignes avec valeurs maximales est encore supérieur à 1")
      # BADABOOM
    }
  }
  # else {
  if(length(unique(SdCell[which(PtsSegm[,3]==nL_PtAval)]))==1){
    PtsSegm=PtsSegm[-which(PtsSegm[,3]==nL_PtAval),]
    SdCell=SdCell[-which(PtsSegm[,3]==nL_PtAval)]
    IndPtAval=which(SdCell==max(SdCell))
    nL_PtAval = unique(PtsSegm[IndPtAval,3])
    IndPtAval=PtsSegm[which(PtsSegm[,3]==nL_PtAval),][1,4][[1]]
  } else {
    ln = dim(PtsSegm[which(PtsSegm[,3]==nL_PtAval),])[1]
    IndPtAval=PtsSegm[which(PtsSegm[,3]==nL_PtAval),][1,4][[1]]
    IndPtAval_f=PtsSegm[which(PtsSegm[,3]==nL_PtAval),][ln,4][[1]]
    if (SdCell[IndPtAval] < SdCell[IndPtAval_f]){
      # Si le point aval a un débit plus faible que le point amont c'est que ce n'est pas normal, il ne faut pas réaliser l'inversion par la suite
      inverse=FALSE
    }
    # }
  }
  
  inc=1
  Reseaulist=list()
  while (inc<=max(PtsSegm[,3]))
  {
    # Recherche du traonçons mont associé au point bas
    IndTronAmont=which(PtsSegm[,3]==PtsSegm[IndPtAval[inc],3]) # Récupération des points composant la ligne avec le débit de pointe le plus élevé
    
    print(paste(inc,PtsSegm[IndPtAval[inc],3]))
    if (IndTronAmont[1]==IndPtAval[inc] & inverse)
    {
      PtsSegm[IndTronAmont,]=PtsSegm[IndTronAmont[length(IndTronAmont):1],] #On inverse l'ordre des indices du tronçon amont 
      # plot(PtsSegm[IndTronAmont,1],PtsSegm[IndTronAmont,2])
    }
    
    # On cherche les indices du point amont de telle sorte que les coordonnées du premier point du tronçon amont soit égaux mais que le numéro de ligne soit différent. ON va donc chercher l'intersection avec d'autres lignes
    IndPtAmont=which((PtsSegm[,1]-PtsSegm[IndTronAmont[1],1]==0) & 
                       (PtsSegm[,2]-PtsSegm[IndTronAmont[1],2]==0) &
                       PtsSegm[,3]!=PtsSegm[IndTronAmont[1],3])
    
    ### boucler car 0, 1 ou 2 et ajouter à ceux du dessus celui en dessous
    PtsSegm[IndTronAmont,"LINKNO"]=PtsSegm[IndTronAmont[1],3] # On ajoute le numéro de ligne aux point du tronçon amont
    if (length(IndPtAmont)>0) { # On regarde s'il y a des intersections
      PtsSegm[IndTronAmont,"USLINKNO"]=PtsSegm[IndPtAmont[1],3] # Dans ce cas on ajoute le numéro de ligne de la première intersection à USLINKNO pour chaque point du TronconAmont
      PtsSegm[which(PtsSegm[,3]==PtsSegm[IndPtAmont[1],3]),"DSLINKNO"]=PtsSegm[IndTronAmont[1],3] # Et on ajoute le numéro de ligne des points amont à DSLINKNO pour chaque point de la première intersection
      IndPtAval=c(IndPtAval,IndPtAmont) # On ajoute les points des intersections aux points aval
      if (length(IndPtAmont)>1) { # S'il y a plus d'une intersection on fait la même opération avec USLINKNO2
        PtsSegm[IndTronAmont,"USLINKNO2"]=PtsSegm[IndPtAmont[2],3]  # On ajoute le numéro de ligne de la seconde intersection à USLINKNO2 pour chaque point du TronconAmont
        PtsSegm[which(PtsSegm[,3]==PtsSegm[IndPtAmont[2],3]),"DSLINKNO"]=PtsSegm[IndTronAmont[2],3] # Et on ajoute le numéro de ligne des points amont à DSLINKNO pour chaque point de la seconde intersection
      }
    }
    # IndPtAval=IndPtAval[-1]
    # print(IndPtAval)
    
    # Formation d'une ligne comportant les points du Troncon Amont et ses données attributaires
    Reseaulist[[inc]]=st_sf(data.frame(LINKNO=PtsSegm[IndTronAmont[1],"LINKNO"],
                                       DSLINKNO=PtsSegm[IndTronAmont[1],"DSLINKNO"],
                                       USLINKNO=PtsSegm[IndTronAmont[1],"USLINKNO"],
                                       USLINKNO2=PtsSegm[IndTronAmont[1],"USLINKNO2"],
                                       "geometry" =st_sfc(st_linestring(PtsSegm[IndTronAmont[length(IndTronAmont):1],1:2],dim(XY)))),
                            crs=2154)
    
    inc=inc+1
    
    inverse=TRUE
  }
  Gagne = do.call(rbind, Reseaulist) 
  return(Gagne)
}


#################################################################################################
################   Ordonne un réseau hydrologique vecteur
#################################################################################################
CalculQps=function(Gagne,dsegm,Zone_ok_F,Qps,ras_Q,PourcAugm,intervPts)
{
  # Récupération réseau dans EAIP et découpage
  # nbr=st_intersects(Reseau,Zone_ok_F)
  # n_intr = which(sapply(nbr, length)>0)
  # ReseauC=Reseau[n_intr,]
  # 
  # ReseauC=st_segmentize(ReseauC,10)
  
  ReseauC=st_segmentize(Gagne,dsegm)
  ReseauC$longueur=st_length(ReseauC)
  units(ReseauC$longueur)=NULL
  
  for (isegmcourt in which(ReseauC$longueur<100))
  {
    st_geometry(ReseauC[isegmcourt,])=st_segmentize(st_geometry(ReseauC[isegmcourt,]),intervPts)
  }
  
  PointsC=st_cast(ReseauC,"POINT")
  st_write(PointsC, file.path(dsnlayerTravail,"POINT.gpkg"),delete_layer=TRUE, quiet=TRUE)
  file.path(dsnlayerTravail,"POINT.gpkg")
  nsegcourt=which(sapply(sort(unique(PointsC$LINKNO)), function(x) {length(which(PointsC$LINKNO==x))})==2)
  
  PointsC$OrdrePt=1:dim(PointsC)[1]
  
  # # test pour gérer les zones endoréiques et ne garder que les points à l'intérieur des zones de cours eau débit
  # nb=st_intersects(PointsC,ZoneCEB)
  # n_int = which(sapply(nb, length)>0)
  # PointsC=PointsC[n_int,]
  
  # st_write(PointsC,file.path(dsnlayerTravail,paste0(nom_MNT,"_1.shp")), delete_layer=T, quiet=T)
  
  # Suppresion des XXX derniers points de chaque tronçons
  # faut savoir amont aval ok ifsttar
  PointsC$Supp=0
  nptavalsupp=3
  for (iseg in unique(PointsC$LINKNO))
  {
    nbiseg=which(PointsC$LINKNO==iseg)
    PointsC[nbiseg[1:min(nptavalsupp,length(nbiseg)-2)],"Supp"]=1
    PointsC[nbiseg[length(nbiseg)],"Supp"]=1
  }
  PointsC=PointsC[which(PointsC$Supp==0),]
  # st_write(PointsC,file.path(dsnlayerTravail,paste0(nom_MNT,"_2.shp")), delete_layer=T, quiet=T)
  
  # Récupération Point dans eaip
  nbr=st_intersects(PointsC,Zone_ok_F)
  n_intr = which(sapply(nbr, length)>0)
  PointsC=PointsC[n_intr,]
  # plot(PointsC)
  PointsC$SdCell = raster::extract(ras_Q, (as_Spatial(PointsC)))
  if (Qps==0)
  {
    PointsC$Qps=round(PointsC$SdCell)
  }else{
    PointsC$Qps=round(Qps*PointsC$SdCell^0.8)
  }
  PointsC=PointsC[which(PointsC$Qps>0),]
  
  # st_write(PointsC,file.path(dsnlayerTravail,paste0(nom_MNT,"_3.shp")), delete_layer=T, quiet=T)
  
  ##################################################################################
  # Calcul par tronçons du qpsmax et élimination des débits qui ne montent pas    
  PointsC$QpsMax=0
  PointsC$Supp=0
  # Boucle sur les troncons
  for (iseg in unique(PointsC$LINKNO))
  {
    nbiseg=which(PointsC$LINKNO==iseg)
    PointsC[nbiseg,"QpsMax"]=max(PointsC[nbiseg,]$"Qps") 
    # Suppression quand cela ne monte
    PointsC$Supp=0
  } 
  
  PointsC$QpsAm=0
  PointsC$QpsAmPr=0
  
  # # Boucle sur les troncons qui ont une partie aval
  for (iseg in unique(PointsC$DSLINKNO))
  {
    nbisegAm=which(PointsC$DSLINKNO==iseg)
    nbiseg=which(PointsC$LINKNO==iseg)
    PointsC[nbiseg,"QpsAm"]=sum(unique(PointsC[nbisegAm,]$"QpsMax"))
    PointsC[nbiseg,"QpsAmPr"]=max(unique(PointsC[nbisegAm,]$"QpsMax"))
  }    
  
  PointsC=PointsC[which(PointsC$Qps>=PointsC$QpsAmPr),]
  # st_write(PointsC,file.path(dsnlayerTravail,paste0(nom_MNT,"_3b.shp")), delete_layer=T, quiet=T)
  
  PointsC$CumulAug=0
  # CumulAug=matrix(0,1,dim(PointsC)[1])
  # Boucle sur les troncons
  
  for (iseg in unique(PointsC$LINKNO))
  {
    nbiseg=which(PointsC$LINKNO==iseg)
    PointsC[nbiseg,"Qps"]=sapply(1:length(nbiseg),function(x) {max(PointsC[nbiseg[x],]$"Qps",PointsC[nbiseg[x],]$"QpsAmPr")})
    
    inc=1:length(nbiseg)
    ordr=length(nbiseg)-inc+1
    
    PointsC[nbiseg[length(nbiseg)],]$CumulAug=max(PointsC[nbiseg[length(nbiseg)],]$Qps,
                                                  PointsC[nbiseg[length(nbiseg)],]$QpsAmPr)
    if (length(ordr)>1)
    {
      for (ipt in 2:length(ordr))
      {
        PointsC[nbiseg[ordr[ipt]],]$CumulAug=
          ifelse(PointsC[nbiseg[ordr[ipt]],]$Qps>PourcAugm*max(max(PointsC[nbiseg[ordr[1:(ipt-1)]],]$CumulAug),PointsC[nbiseg[ordr[1]],]$QpsAmPr),
                 PointsC[nbiseg[ordr[ipt]],]$Qps,0)
      }  
    }
  } 
  # 
  # st_write(PointsC,file.path(dsnlayerTravail,paste0(nom_MNT,"_ici.shp")), delete_layer=T, quiet=T)
  PointsC=PointsC[which(PointsC$CumulAug>0),]
  # st_write(PointsC,file.path(dsnlayerTravail,paste0(nom_MNT,"_ici2.shp")), delete_layer=T, quiet=T)
  
  ####mise à jour après nettoyage %
  PointsC$QpsMax2=0
  for (iseg in unique(PointsC$LINKNO))
  {
    nbiseg=which(PointsC$LINKNO==iseg)
    PointsC[nbiseg,"QpsMax2"]=max(PointsC[nbiseg,]$"Qps") 
  } 
  
  # # Boucle sur les troncons qui ont une partie aval
  PointsC$QpsAm2=0
  for (iseg in unique(PointsC$DSLINKNO))
  {
    nbisegAm=which(PointsC$DSLINKNO==iseg)
    nbiseg=which(PointsC$LINKNO==iseg)
    PointsC[nbiseg,"QpsAm2"]=sum(unique(PointsC[nbisegAm,]$"QpsMax2"))
  } 
  #### fin mise à jour après nettoyage %
  
  # Calcul des deltas Qps
  # # PointsC$DeltaQpsF=0
  # PointsC$DeltaQps=PointsC$Qps-PointsC$QpsAmPr
  # 
  # Boucle sur les troncons
  
  for (iseg in unique(PointsC$LINKNO))
  {
    nbiseg=which(PointsC$LINKNO==iseg)
    # ce qu'il faudrait
    # PointsC[nbiseg,"DeltaQps"]=PointsC[nbiseg,]$"Qps"-PointsC[nbiseg,]$"QpsAm"
    PointsC[nbiseg,"DeltaQps"]=PointsC[nbiseg,]$"Qps"-PointsC[nbiseg,]$"QpsAm2"
    if (length(nbiseg)>1)
    {
      PointsC[nbiseg[(length(nbiseg)-1):1],"DeltaQps"]=PointsC[nbiseg[(length(nbiseg)-1):1],]$"DeltaQps"-PointsC[nbiseg[length(nbiseg):2],]$"DeltaQps"
    }
  }
  # st_write(PointsC,file.path(dsnlayerTravail,paste0(nom_MNT,"_4.shp")), delete_layer=T, quiet=T)
  PointsC=PointsC[which(PointsC$DeltaQps!=0 ),]
  return(PointsC)
}


#################################################################################################
################  Positionne sur les points bas du MNT les points sources
#################################################################################################
CalculsPosSources=function(PointsC,nomzonedeb,vrt_file,dsnlayerTravail)
{
  # Buffer autour des sources limité aux zones d'intérêt
  # browser()
  # Import du raster
  nomTopo="MNT"
  cmd=paste0("r.in.gdal -o --quiet --overwrite input=",vrt_file," output=",nomTopo)
  print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
  
  nom_source="SourceBuf"
  # writeVECT(nomzonedeb,nom_source, v.in.ogr_flags=c("overwrite","quiet","o"), driver="ESRI Shapefile")
  cmd=paste0("v.in.ogr -o --quiet --overwrite input=",nomzonedeb," output=",nom_source," min_area=0.000000001")
  system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
  
  #################### Tous les exports
  # execGRASS("g.region",flags=c("quiet","overwrite"),parameters=list(raster=nomTopo))
  cmd=paste0("g.region --quiet --overwrite raster=",nomTopo)
  print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
  
  # Suppression du masque (s'il existe)
  # execGRASS("r.mask",flags=c("r"))
  cmd=paste0("r.mask -r")
  system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
  
  # Création d'un masque
  # execGRASS("r.mask",parameters=list(vector=nom_source))
  cmd=paste0("r.mask --quiet --overwrite vector=",nom_source)
  system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
  
  nomoutput=file.path(dsnlayerTravail,paste0("source.xyz"))
  # r.out.xyz input=ROYAaval_1m@Temp output=D:\Tempete_Alex_2020\Moi\telemaccomplet\MNT1m\kdfqk.xyz
  # execGRASS("r.out.xyz",flags=c("quiet","overwrite"),
  #           parameters=list(input=vrt_file,
  #                           output=nomoutput,separator="comma"))
  cmd=paste0("r.out.xyz --quiet --overwrite input=",nomTopo," output=",nomoutput," separator=comma")
  system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
  
  tabs=read.csv(nomoutput,header = F)
  return(tabs)
}

#################################################################################################
################  Positionne sur les points bas du MNT les points sources
#################################################################################################
CalculsSourcesComplet=function(PointsC,tabs,dsnlayerTravail,Type)
{
  #-----
  if (file.exists(file.path(dsnlayerTravail,"_SourcesDebits"))=="FALSE"){dir.create(file.path(dsnlayerTravail,"_SourcesDebits"))}
  
  # On ne garde que les plus pour l'hydrométrie
  # et gestion forme hydrogramme
  if (Type==1) 
  {
    HydroPos=rbind(cbind(0,0),cbind(1800,1),cbind(3600,1),cbind(9999999,1))
    HydroNeg=rbind(cbind(0,0),cbind(1800,0),cbind(3600,1),cbind(9999999,1))
  }else{
    PointsC=PointsC[which(PointsC$DeltaQps>0),]
    HydroPos=rbind(cbind(0,0),cbind(3600,1),cbind(10800,0.1),cbind(9999999,0.1))
  }
  
  Res=list()
  for (isour in 1:dim(PointsC))
  {
    # cat(isour," - ",dim(PointsC))
    DeltaQps=PointsC[isour,]$DeltaQps
    if (Type==1)
    {SOURCEFILE=paste0(ifelse(DeltaQps>0,
                              paste0("POS_",formatC(DeltaQps,width = 5,flag = "0")),
                              paste0("NEG_",formatC(-DeltaQps,width = 5,flag = "0"))),".txt")
    }else{
      SOURCEFILE=paste0("HYD_",formatC(DeltaQps,width = 5,flag = "0"),".txt")
    }
    zz_source=file.path(dsnlayerTravail,"_SourcesDebits",SOURCEFILE)
    file.create(zz_source)
    write("Temps Débit", file = zz_source, append=T)
    
    if (DeltaQps>0)
    {
      # tabl=rbind(cbind(0,1),cbind(1800,DeltaQps),cbind(3600,DeltaQps),cbind(99999,DeltaQps))
      tabl=HydroPos
      tabl[,2]=DeltaQps* tabl[,2]
      tabl[1,2]=1
    }else{
      # tabl=rbind(cbind(0,0),cbind(3600,0),cbind(7200,DeltaQps),cbind(99999,DeltaQps))
      # tabl=rbind(cbind(0,1),cbind(1800,0),cbind(3600,DeltaQps),cbind(99999,DeltaQps))
      tabl=HydroNeg
      tabl[,2]=DeltaQps* tabl[,2]
    }
    write(paste(tabl[,1],tabl[,2]), file = zz_source, append=T)
    
    #Recherche point le + bas dans un rayon de distBufS
    # browser()
    PtSourIni=st_coordinates(PointsC[isour,])
    ns=which((tabs[,1]-PtSourIni[1])^2<distBufS^2 & (tabs[,2]-PtSourIni[2])^2<distBufS^2)
    
    cestla=which(tabs[ns,3]==min(tabs[ns,3]))[1]
    Res[[isour]]=st_sf(data.frame( Q=PointsC$Qps[isour],
                                   DeltaQps=DeltaQps,
                                   SOURCEFILE=SOURCEFILE),
                       "geometry" =st_sfc(st_point(c(tabs[ns[cestla],1],tabs[ns[cestla],2]),dim="XY")))
  }
  # cat("\n")
  SourcesDebits = do.call(rbind, Res) 
  # print("fin do.call")
  return(SourcesDebits)
}