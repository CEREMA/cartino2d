################################################################################
### Fonction pour faire un miniMNT autour des semipoints 3D #########################
################################################################################
SemiPts3D_miniMNT=function(nomSemiPts3D,nomBief,NBief,DistBufSemPts3D,AireInterieurMin,EPSG)
{ 
  cat("####   Pts3D_Tampon\n")
  IcSep=strsplit(nomSemiPts3D, "\\.")
  decal=nchar(IcSep[[1]][length(IcSep[[1]])])+1
  raci=paste0(substr(nomSemiPts3D,1,nchar(nomSemiPts3D)-decal))
  nomSemiPts3D_Buf_P     =paste0(raci,"_TamponP.gpkg")
  nomSemiPts3D_Buf_P_V   =paste0(raci,"_TamponP_V.gpkg")
  nomSemiPts3D_Buf_P_V_N =paste0(raci,"_TamponP_V_N.gpkg")
  
  
  # Buffer Positif
  cmd <- paste0(qgis_process, " run native:buffer",
                " --INPUT=", shQuote(nomSemiPts3D),
                " --DISTANCE=",DistBufSemPts3D,
                " --SEGMENTS=5 --END_CAP_STYLE=0 --JOIN_STYLE=0 --MITER_LIMIT=2 --DISSOLVE=True",
                " --OUTPUT=", shQuote(nomSemiPts3D_Buf_P))
  print(cmd); system(cmd)
  
  # Enlever les petits trous
  cmd <- paste0(qgis_process, " run native:deleteholes",
                " --INPUT=", shQuote(nomSemiPts3D_Buf_P),
                " --MIN_AREA=", AireInterieurMin,
                " --OUTPUT=", shQuote(nomSemiPts3D_Buf_P_V))
  print(cmd); system(cmd)
  
  # Buffer Négatif
  cmd <- paste0(qgis_process, " run native:buffer",
                " --INPUT=", shQuote(nomSemiPts3D_Buf_P_V),
                # " --DISTANCE=-",min(0.95*DistBufSemPts3D,DistBufSemPts3D-0.5),
                " --DISTANCE=-",0,
                " --SEGMENTS=5 --END_CAP_STYLE=0 --JOIN_STYLE=0 --MITER_LIMIT=2 --DISSOLVE=True",
                " --OUTPUT=", shQuote(nomSemiPts3D_Buf_P_V_N))
  print(cmd); system(cmd)
  
  
  Tampon=st_cast(st_read(nomSemiPts3D_Buf_P_V_N),"POLYGON")
  SemiPts3D=st_read(nomSemiPts3D)
  
  doss=file.path(dirname(nomSemiPts3D),"SemiMNT")
  if (file.exists(doss)==F)
  {dir.create(doss)}
  
  
  NomGPKG=file.path(doss,"SemiMNT.GPKG")
  for (itamp in 1:nrow(Tampon))
  {
    nb=st_intersects(SemiPts3D,Tampon[itamp,])
    n_int = which(sapply(nb, length)>0)
    if (length(n_int)>0)
    {
      SemiPts3D_tmp=SemiPts3D[n_int,]
      
      
      raci_=file.path(doss,paste0("MiniMNT","_",formatC(itamp,width=3,flag="0")))
      
      nomcsv=paste0(raci_,".csv")
      nomjson=paste0(raci_,".json")
      NomTIF=paste0(raci_,".TIF")
      
      
      coords=st_coordinates(SemiPts3D_tmp)
      
      write.csv(coords,nomcsv,
                row.names=FALSE)
      
      write("[",nomjson)
      write("    {",nomjson,append=T)
      write(paste0("       ",shQuote("type"),":",shQuote("readers.text"),","),nomjson,append=T)
      write(paste0("       ",shQuote("filename"),":",shQuote(nomcsv),","),nomjson,append=T)
      write(paste0("       ",shQuote("override_srs"),":",shQuote(paste0("EPSG:",EPSG))),nomjson,append=T)
      write("    },",nomjson,append=T)
      
      ############### Filtre delaunay
      write("    {",nomjson,append=T)
      write(paste0("       ",shQuote("type"),":",shQuote("filters.delaunay")),nomjson,append=T)
      write("    },",nomjson,append=T)
      ############## Interpolation
      write("    {",nomjson,append=T)
      write(paste0("       ",shQuote("type"),":",shQuote("filters.faceraster"),","),nomjson,append=T)
      write(paste0("       ",shQuote("resolution"),":",reso,","),nomjson,append=T) 
      
      # Définir la bbox du masque avec des valeurs entières pour les limites min/max
      bbox <- st_bbox(Tampon[itamp,])
      xmin <- floor(bbox["xmin"])
      xmax <- ceiling(bbox["xmax"])
      ymin <- floor(bbox["ymin"])
      ymax <- ceiling(bbox["ymax"])
      
      write(paste0("       ",shQuote("width") ,":",(xmax-xmin)/reso,","),nomjson,append=T) 
      write(paste0("       ",shQuote("height"),":",(ymax-ymin)/reso,","),nomjson,append=T) 
      write(paste0("       ",shQuote("origin_x"),":",xmin,","),nomjson,append=T) 
      write(paste0("       ",shQuote("origin_y"),":",ymin),nomjson,append=T) 
      write("    },",nomjson,append=T)
      
      ################# Export
      write("    {",nomjson,append=T)
      write(paste0("       ",shQuote("type"),":",shQuote("writers.raster"),","),nomjson,append=T)
      write(paste0("       ",shQuote("gdaldriver"),":",shQuote("GTiff"),","),nomjson,append=T)
      write(paste0("       ",shQuote("data_type"),":",shQuote("float32"),","),nomjson,append=T)  
      write(paste0("       ",shQuote("filename"),":",shQuote(NomTIF)),nomjson,append=T)
      write("    }",nomjson,append=T)
      write("]",nomjson,append=T)
      cmd=paste(pdal_exe,"pipeline",nomjson)
      print(cmd);system(cmd)
      
      unlink(nomcsv)
      unlink(nomjson)
    }
  }
  
  listeASC=file.path(doss,list.files(doss,pattern=".TIF"))
  nom_ascvrt=file.path(doss,"listepourvrt.txt")
  file.create(nom_ascvrt)
  write(listeASC, file=nom_ascvrt, append=T)
  vrtfile = paste0(doss,"/","MinisMNT.vrt") ##chemin du vrt à créer
  cmd = paste(shQuote(OSGeo4W_path), "gdalbuildvrt",vrtfile,"-input_file_list",nom_ascvrt) ## commande pour exécuter gdalbuildvrt
  system(cmd) 
  
  SecteurGRASS=paste0(dirname(SecteurGRASS_),'_',format(Sys.time(),format="%Y%m%d_%H%M%S"),"/",basename(SecteurGRASS_))
  #Creation d'un monde GRASS
  unlink(dirname(SecteurGRASS),recursive=TRUE)
  system(paste0(BatGRASS," -c EPSG:",EPSG," ",dirname(SecteurGRASS)," --text"))
  system(paste0(BatGRASS," -c ",SecteurGRASS," --text"))
  
  nomMNTg="MiniMNT"
  cmd <- paste0("r.in.gdal -o --quiet --overwrite input=", vrtfile, " output=",nomMNTg)
  print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
  
  cmd <- paste0("g.region --quiet --overwrite raster=",nomMNTg)
  print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
  
  nom_tamp="Tamp"
  cmd=paste0("v.in.ogr -o --quiet --overwrite input=",nomSemiPts3D_Buf_P_V_N," output=",nom_tamp)
  print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
  
  cmd=paste0("r.mask --quiet --overwrite vector=",nom_tamp)
  print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
  
  cmd=paste0("r.out.gdal --quiet --overwrite -c -f input=",nomMNTg," output=",NomGPKG," type=Float32 format=GPKG nodata=-9999")
  print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
  
}



################################################################################
### Fonction pour faire un tampon autour des points 3D #########################
################################################################################
# Il peut être nécessaire de faire une reprise du tampon pour 
#------ dissocier des profils proches (autour d'ouvrages)
#------ agrandir le tampon si le buffer n'est pas suffisant
Pts3D_Tampon=function(nomPts3D,nomBief,NBief,DistBuf,EPSG)
{ 
  cat("####   Pts3D_Tampon\n")
  # Lecture des points
  Pts3D=st_read(nomPts3D)
  
  ##### AFAIRE NBief avec un champ de nomTampon et pq pas DistBuf
  if (nchar(nomBief)>1)
  {
    PolygBief=st_read(nomBief)
    if (is.null(PolygBief$NBief)){PolygBief$NBief=NBief-1;PolygBief$NBief+1:nrow(PolygBief)}
    if (is.null(PolygBief$DistBuf)){PolygBief$DistBuf=DistBuf}
    nici=which(is.na(PolygBief$DistBuf))
    if (length(nici)>0) {PolygBief$DistBuf[nici]=DistBuf}
  }else{
    PolygBief=st_sf(data.frame(NBief=NBief,DistBuf=DistBuf),geometry=st_buffer(st_as_sfc(st_bbox(Pts3D)),10),crs=EPSG)
  }
  
  Tampon_l=list()
  for (iBief in 1:nrow(PolygBief))
  {
    nb=st_intersects(Pts3D,PolygBief[iBief,])
    n_int = which(sapply(nb, length)>0)
    if (length(n_int)>0)
    {
      Pts3D_tmp=Pts3D[n_int,]
      ####### travail pour combiner les points et faire une ligne 3D
      Tampon_l[[iBief]]=st_sf(NBief=PolygBief$NBief[iBief],geometry=st_cast(st_union(st_buffer(Pts3D_tmp[,1],PolygBief$DistBuf[iBief])),"POLYGON"),crs=EPSG)
      # plot(Tampon)
    }
  }
  Tampon=do.call(rbind, Tampon_l)
  
  # Nouveau Nom Export
  IcSep=strsplit(nomPts3D, "\\.")
  decal=nchar(IcSep[[1]][length(IcSep[[1]])])+1
  nomTampon=paste0(substr(nomPts3D,1,nchar(nomPts3D)-decal),"_Tampon.gpkg")
  
  st_write(Tampon,dsn=nomTampon, delete_layer=T, quiet=T)
  cat("####\n",nomTampon,"\n","####\n")
  
  return(nomTampon)
}

################################################################################
### Fonction pour convertir les points 3D en lignes 3D #########################
################################################################################
Pts3D_Lignes3D=function(nomPts3D,nomTampon,nomBief,NBief)
{
  cat("####   Pts3D_Lignes3D\n")
  # Lecture des points et du tampon
  Pts3D =st_read(nomPts3D)
  Tampon=st_read(nomTampon)
  if (is.null(Tampon$NBief)){Tampon$NBief=NBief}
  
  if (nchar(nomBief)>1)
  {
    PolygBief=st_read(nomBief)
    nb=st_intersects(Pts3D,PolygBief)
    n_int = which(sapply(nb, length)>0)
    if (length(n_int)>0)
    {
      Pts3D=Pts3D[n_int,]
    }
    
    Tamp_list=list()
    ilist=1
    
    for (ibief in unique(PolygBief$NBief))
    {
      nbief=which(PolygBief$NBief==ibief)
      
      nb=st_intersects(Tampon,PolygBief[nbief,])
      n_int = which(sapply(nb, length)>0)
      if (length(n_int)>0)
      {
        Tamp_=Tampon[n_int,]
        Tamp_$NBief=PolygBief$NBief[nbief]
        Tamp_list[[ilist]]=Tamp_
        ilist=ilist+1
      }
    }
    if (ilist>1){Tampon=do.call(rbind,Tamp_list)}
  }
  
  # Nouveau Nom Export
  IcSep=strsplit(nomPts3D, "\\.")
  decal=nchar(IcSep[[1]][length(IcSep[[1]])])+1
  nomLignes3D=paste0(substr(nomPts3D,1,nchar(nomPts3D)-decal),"_Ligne_3D.gpkg")
  
  line_sfl=list()
  for (itamp in 1:nrow(Tampon))
  {
    cat(itamp," ",itamp," ")
    
    nb=st_intersects(Pts3D,Tampon[itamp,])
    n_int = which(sapply(nb, length)>0)
    if (length(n_int)>0)
    {
      # réduction sur les dalles electionnées
      Pts3D_=Pts3D[n_int,]
      
      # Définir les coordonnées des points
      coords=st_coordinates(Pts3D_)
      
      # Calcul des distances entre points
      distance=st_distance(Pts3D_,Pts3D_)
      units(distance)=NULL
      
      # Calcul du 1er point
      # Il faut prendre un des points qui a la plus grande distance
      inc=1
      while(length(which(distance[inc,]==max(distance)))==0)
      {inc=inc+1}
      
      ici=which(distance==0)
      distance[ici]=9999
      PosiPts=matrix(inc,nrow=nrow(distance),ncol=1)
      cat("iPts")
      for (iPts in 2:nrow(distance))
      {
        # if (itamp==54){browser()}
        cat(" ",iPts)
        inc_=which(distance[inc,]==min(distance[inc,]))
        distance[inc,]=9999
        distance[,inc]=9999
        inc=inc_
        PosiPts[iPts]=inc
      }
      
      # Créer une polyligne à partir des points triés
      line_sfl[[itamp]] <- st_sf(data.frame(Type=2,
                                            NBief=Tampon$NBief[itamp],
                                            ZsiType1=-99,
                                            NSection=itamp),
                                 geometry=st_cast(st_combine(st_geometry(Pts3D_[PosiPts,])), "LINESTRING"))
    }
    cat("\n")
  }
  # Convertir la liste des lignes
  line_sf <- do.call(rbind, line_sfl)
  #############################################################################
  # Export
  st_write(line_sf,dsn=nomLignes3D, delete_layer=T, quiet=T)
  cat("####\n",nomLignes3D,"\n","####\n")
  return(nomLignes3D)
}

################################################################################
### Fonction pour ordonner les lignes 3D (on ne sait si amont aval ou inverse ##
################################################################################
Lignes3D_Ord=function(nomLignes3D,nomBief,NBief)
{
  cat("####   Lignes3D_Ord\n")
  line_sf=st_cast(st_read(nomLignes3D),"LINESTRING")
  
  line_sf=Lignes3D_Attributs(line_sf,nomBief,NBief)
  
  #### Nouveau Nom Export
  IcSep=strsplit(nomLignes3D, "\\.")
  decal=nchar(IcSep[[1]][length(IcSep[[1]])])+1
  nomLignes3D=paste0(substr(nomLignes3D,1,nchar(nomLignes3D)-decal),"_Ord.gpkg")
  
  line_sfl=list()
  listeBief=sort(unique(line_sf$NBief))
  for (iBief in 1:length(listeBief))
  {
    line_sf_=line_sf[which(line_sf$NBief==listeBief[iBief]),]
    
    #############################################################################
    ##### Travail pour ordonner les sections
    line_sf_Cent=st_centroid(line_sf_)
    # Calcul des distances entre points
    distance=st_distance(line_sf_Cent,line_sf_Cent)
    units(distance)=NULL
    
    ########## Calcul du 1er point
    # Il faut prendre un des points qui a la plus grande distance
    inc=1
    while(length(which(distance[inc,]==max(distance)))==0)
    {inc=inc+1}
    ici=which(distance==0)
    distance[ici]=9999
    PosiCentr=matrix(inc,nrow=nrow(distance),ncol=1)
    
    for (iCentr in 2:nrow(distance))
    {
      inc_=which(distance[inc,]==min(distance[inc,]))
      if (length(inc_)>1){browser()}
      distance[inc,]=9999
      distance[,inc]=9999
      inc=inc_
      PosiCentr[iCentr]=inc
    }
    
    line_sf_=line_sf_[PosiCentr,]
    line_sf_$NSection=1:nrow(line_sf_)
    line_sfl[[iBief]]=line_sf_
  }
  # Convertir la liste des lignes
  line_sf <- do.call(rbind, line_sfl)
  #############################################################################
  # Export
  st_write(line_sf,dsn=nomLignes3D, delete_layer=T, quiet=T)
  cat("####\n",nomLignes3D,"\n","####\n")
  return(nomLignes3D)
}

################################################################################
### Fonction pour mettre dans même sens lignes 3D (on ne sait si RG/RD ou inverse
################################################################################
Lignes3D_Sens=function(nomLignes3D,nomBief,NBief)
{
  
  cat("####   Lignes3D_Sens\n")
  line_sf=st_read(nomLignes3D)
  
  line_sf=Lignes3D_Attributs(line_sf,nomBief,NBief)
  
  #### Nouveau Nom Export
  IcSep=strsplit(nomLignes3D, "\\.")
  decal=nchar(IcSep[[1]][length(IcSep[[1]])])+1
  nomLignes3D=paste0(substr(nomLignes3D,1,nchar(nomLignes3D)-decal),"_Sens.gpkg")
  
  for (iBief in unique(line_sf$NBief))
  {
    nb=which(line_sf$NBief==iBief)
    for (il in 2:nrow(line_sf[nb,]))
    {
      L1=st_coordinates(line_sf[nb[il-1],])
      L2=st_coordinates(line_sf[nb[il],])
      
      L1A2A=st_sfc(st_linestring(rbind(L1[1       ,1:2],L2[1       ,1:2]),dim="XY"),crs=EPSG)
      L1B2B=st_sfc(st_linestring(rbind(L1[nrow(L1),1:2],L2[nrow(L2),1:2]),dim="XY"),crs=EPSG)
      
      ntouch=st_intersects(L1A2A,L1B2B)
      n_intouch = which(sapply(ntouch, length)>0)
      
      # browser()
      
      if (length(n_intouch)>0)
      {
        if (max(st_coordinates(line_sf[nb[il],])[,3]-st_coordinates(st_zm(line_sf[nb[il],]))[,3])==0)
        {dime="XY";var=1:2}else{dime="XYZ";var=1:3}
        
        st_geometry(line_sf[nb[il],]) <- st_sfc(st_linestring(L2[nrow(L2):1,var],dim=),crs=EPSG)
      }
    }
  }
  #############################################################################
  # Export
  st_write(line_sf,dsn=nomLignes3D, delete_layer=T, quiet=T)
  cat("####\n",nomLignes3D,"\n","####\n")
  return(nomLignes3D)
}

################################################################################
### Fonction pour ajouter les attributs à la ligne 3D au besoin
################################################################################
Lignes3D_Attributs=function(line_sf,nomBief,NBief)
{
  if (is.null(line_sf$Type)){line_sf$Type=2}
  if (is.null(line_sf$NBief))
  {
    if (nchar(nomBief)>0)
    {
      PolygBief=st_read(nomBief)
      if (is.null(PolygBief$NBief)){PolygBief$NBief=NBief-1;PolygBief$NBief+1:nrow(PolygBief)}
      if (is.null(PolygBief$DistBuf)){PolygBief$DistBuf=DistBuf}
      nici=which(is.na(PolygBief$DistBuf))
      if (length(nici)>0) {PolygBief$DistBuf[nici]=DistBuf}
      
      
      line_sfl=list()
      for (iBief in 1:nrow(PolygBief))
      {
        nb=st_intersects(line_sf,PolygBief[iBief,])
        n_int = which(sapply(nb, length)>0)
        if (length(n_int)>0)
        {
          ####### travail pour combiner les points et faire une ligne 3D
          line_sf_=line_sf[n_int,]
          line_sf_$NBief=PolygBief$NBief[iBief]
          line_sfl[[iBief]]=line_sf_
        }
      }
      line_sf=do.call(rbind, line_sfl) 
    }else{
      line_sf$NBief=NBief
    }
  }
  if (is.null(line_sf$ZsiType1)){line_sf$ZsiType1=-99}
  return(line_sf) 
}
