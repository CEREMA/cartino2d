cat("\014") # Nettoyage de la console

SecteurGRASS=SecteurGRASS=paste0(dirname(SecteurGRASS_),format(Sys.time(),format="%Y%m%d_%H%M%S"),"_","","/",basename(SecteurGRASS_))
unlink(dirname(SecteurGRASS),recursive=TRUE)
system(paste0(BatGRASS," -c EPSG:",EPSG," ",dirname(SecteurGRASS)," --text"))
system(paste0(BatGRASS," -c ",SecteurGRASS," --text"))

ExtensDEBIT=rbind(cbind("HSF","HWH","HWM"),
                  cbind("_m2_s","_m","_m"))

BufAnalyse=100
ntourmax=25
nligmin=3

# Ouverture de HWH, HSF et HWT
for (i in 1:3)
{
  # Import du raster
  if (file.exists(file.path(dsnDPE,paste0(raci,ExtensDEBIT[1,i],ExtensDEBIT[2,i],".gpkg")))==F)
  {cat(file.path(dsnDPE,paste0(raci,ExtensDEBIT[1,i],ExtensDEBIT[2,i],".gpkg"))," n'existe pas","\n");boom=modifdsnDPEetraci}
  cmd=paste0("r.in.gdal -o --quiet --overwrite input=",file.path(dsnDPE,paste0(raci,ExtensDEBIT[1,i],ExtensDEBIT[2,i],".gpkg"))," output=",ExtensDEBIT[1,i])
  print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
}

# Limitation de la région de travail
cmd=paste0("g.region --quiet --overwrite raster=",ExtensDEBIT[1,1],",",ExtensDEBIT[1,2])
print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))

dejafait=1
if (dejafait==0)
{
  #-------------------- ZI
  nomInond1="ZI1"
  exp=paste0(nomInond1," =if(",ExtensDEBIT[1,1],">",0.01,",1,null())")
  cmd=paste0("r.mapcalc --overwrite ",shQuote(exp))
  print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
  
  nomInond2="ZI2"
  exp=paste0(nomInond2," =if(",ExtensDEBIT[1,2],">",0.01,",1,null())")
  cmd=paste0("r.mapcalc --overwrite ",shQuote(exp))
  print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
  
  nomInond="ZI"
  cmd=paste0("r.series --overwrite input=",nomInond1,",",nomInond2," output=",nomInond," method=maximum")
  print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
  
  # Emprise pour Carte Socle
  BufPlus=2*resolution
  BufMoins=2*resolution
  BufPlusMoins(nomInond,seuilSup0,BufPlus,BufMoins,"-s",dsnDPE,-99,1)
  nomvectsortie=file.path(dsnDPE,paste0(raci,"_",nomInond,".gpkg"))
  
  source(file.path(chem_routine,"PreC2D","PreC2D_6_2_Decrenelage_job.R"))
  AireMin_Inter=630
  AireMin_Exter=25*25*2.01
  nomContour=Decrenelage(nomvectsortie,AireMin_Inter,AireMin_Exter,EPSG,resolution)
  
  
  # Garder le + grand
  # qgis_process run native:keepnbiggestparts --distance_units=meters --area_units=m2 --ellipsoid=EPSG:7019 --POLYGONS='C:/Cartino2D/France/EAIMQ/C0088_642819km_X319171Y6737743/Post/hyeto_C0088_642819km_X319171Y6737743_PQN_00h_00m_240h_00m_SOURCEFILE__ZI_Qgis_Final_Aint630_Aext1256.25.gpkg|layername=hyeto_C0088_642819km_X319171Y6737743_PQN_00h_00m_240h_00m_SOURCEFILE__ZI_Qgis_Final_Aint630_Aext1256.25' --PARTS=1 --OUTPUT=TEMPORARY_OUTPUT
  # Garder la plus grande partie
  nomContourGrand=file.path(dsnDPE,"ContourRes_GrandePartie.gpkg")
  ContourPropre=st_cast(st_read(nomContour),"POLYGON")
  ContourPropre$AREA=st_area(ContourPropre)
  ContourPropre=ContourPropre[which(ContourPropre$AREA==max(ContourPropre$AREA)),]
  st_write(ContourPropre,nomContourGrand, delete_dsn = T,delete_layer = T, quiet = T)
  # cmd <- paste0(qgis_process, " run native:keepnbiggestparts",
  #               " --distance_units=meters --area_units=m2 --ellipsoid=EPSG:7019",
  #               " --POLYGONS=",nomContour,
  #               " --PARTS=1",
  #               " --OUTPUT=",nomContourGrand)
  # print(cmd); system(cmd)
  
  # Buffer de 12.5m
  # qgis_process run native:buffer --distance_units=meters --area_units=m2 --ellipsoid=EPSG:7019 --INPUT='memory://Polygon?crs=EPSG:2154&field=fid:long(0,0)&field=DN:integer(0,0)&field=layer:string(0,0)&field=path:string(0,0)&field=Aire:double(0,0)&uid={0cafa039-ae39-4b14-bc65-9619e3bd93f3}' --DISTANCE=12.5 --SEGMENTS=5 --END_CAP_STYLE=0 --JOIN_STYLE=0 --MITER_LIMIT=2 --DISSOLVE=false --SEPARATE_DISJOINT=false --OUTPUT=TEMPORARY_OUTPUT
  nomContourGrand_Buf=file.path(dsnDPE,"ContourRes_GrandePartie_Buf.gpkg")
  cmd=paste0(qgis_process, " run native:buffer",
             " --distance_units=meters --area_units=m2 --ellipsoid=EPSG:7019",
             " --INPUT=",nomContourGrand,
             " --DISTANCE=",resolution,
             " --SEGMENTS=5 --END_CAP_STYLE=0 --JOIN_STYLE=0 --MITER_LIMIT=2 --DISSOLVE=True",
             " --OUTPUT=",nomContourGrand_Buf)
  print(cmd);system(cmd)
  
}else{
  nomContourGrand_Buf=file.path(dsnDPE,"ContourRes_GrandePartie_Buf.gpkg")
}


# Frontière Ligne svers polygones
# qgis_process run qgis:linestopolygons --distance_units=meters --area_units=m2 --ellipsoid=EPSG:7019 --INPUT='C:/Cartino2D/France/EAIMQ/C0088_642819km_X319171Y6737743/Frontiere.shp' --OUTPUT=TEMPORARY_OUTPUT
nomFrontiere=file.path(dsnDPE,"Frontiere_Surf.gpkg")
cmd=paste0(qgis_process, " run qgis:linestopolygons",
           " --distance_units=meters --area_units=m2 --ellipsoid=EPSG:7019",
           " --INPUT=",file.path(dirname(dsnDPE),"Frontiere.shp"),
           " --OUTPUT=",nomFrontiere)
print(cmd);system(cmd)

# Partie commune zone de validité frontière
nomCommun=file.path(dsnDPE,"Min_Front_ValidCalcul.gpkg")
cmd <- paste0(qgis_process, " run native:intersection",
              " --distance_units=meters --area_units=m2 --ellipsoid=EPSG:7019",
              " --INPUT=",nomFrontiere,
              " --OVERLAY=",file.path(dirname(dsnDPE),"Step_Cartino2d_2_Valid_Calcul.shp"),
              " --OVERLAY_FIELDS_PREFIX=",
              " --OUTPUT=",nomCommun)
print(cmd); system(cmd)

# Croisement entre results et valid
nomManque=file.path(dsnDPE,"Tour0001_a_Manques.gpkg")
cmd <- paste0(qgis_process, " run native:difference",
              " --INPUT=",nomContourGrand_Buf,
              " --OVERLAY=",nomCommun,
              " --OUTPUT=",nomManque,
              " --GRID_SIZE=None")
print(cmd);system(cmd)

# boucle sur les morceaux qui dépassent
tour=1
nlig=9999
while (tour<=ntourmax & nlig>=nligmin)
{
  texTour=paste0("tour",formatC(tour,width=3,flag="0"))
  
  nomManque_Buf=file.path(dsnDPE,paste0(texTour,"_b_Manques_Buf.gpkg"))
  cmd=paste0(qgis_process, " run native:buffer",
             " --distance_units=meters --area_units=m2 --ellipsoid=EPSG:7019",
             " --INPUT=",nomManque,
             " --DISTANCE=",BufAnalyse,
             " --SEGMENTS=5 --END_CAP_STYLE=0 --JOIN_STYLE=0 --MITER_LIMIT=2 --DISSOLVE=True",
             " --OUTPUT=",nomManque_Buf)
  print(cmd);system(cmd)
  
  nomInondHWHZi_a_agrandir1=paste0(texTour,"_Zi_a_agrandir") 
  nomHWMg=paste0(texTour,"_Zi_a_agrandir","_ga_HWMg")
  if (tour==1)
  {
    #-Faire un resample de la cote sur la zone ZI que de HWH>0.5m
    cmd=paste0("r.mapcalc --overwrite ",shQuote(paste0(nomInondHWHZi_a_agrandir1," = if( ",ExtensDEBIT[1,2]," >",0.5,",1,null())")))
    print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
    
    cmd=paste0("r.mask --quiet --overwrite raster=",nomInondHWHZi_a_agrandir1)
    print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd)) 
    
    cmd=paste0("r.resample --quiet --overwrite input=","HWM"," output=",nomHWMg)
    print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
  }else{
    nommasqHWM
    cmd=paste0("v.in.ogr -o --quiet --overwrite input=",nommasqHWM," output=",nomInondHWHZi_a_agrandir1)
    print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
    
    cmd=paste0("r.mask --quiet --overwrite vector=",nomInondHWHZi_a_agrandir1)
    print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
    
    cmd=paste0("r.resample --quiet --overwrite input=",nomneigh," output=",nomHWMg)
    print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
  }
  
  # Masques sur les manques
  nomMasqueBufg=paste0(texTour,"_gb_Masqbuf")
  cmd=paste0("v.in.ogr -o --quiet --overwrite input=",nomManque_Buf," output=",nomMasqueBufg)
  print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
  
  cmd=paste0("r.mask --quiet --overwrite vector=",nomMasqueBufg)
  print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
  
  #faire un neighbor sur la cote dans zi que dans le manque buf
  nomneigh=paste0(texTour,"_gc_HWM_Nei",8)
  cmd=paste0("r.neighbors --quiet --overwrite input=",nomHWMg," output=",nomneigh," size=",2*round(BufAnalyse/resolution)+1," method=maximum")
  print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
  
  # Faire un vrt sur interscetion manque buf et table raster
  Lidar=st_read(contours$MNT[icontour])
  
  # récupération des dalles concernées
  nb = st_intersects(Lidar, st_read(nomManque_Buf))
  n_int = which(sapply(nb, length) > 0)
  LidarC = Lidar[n_int,]
  
  # Création de la liste des dalles concernées
  if(length(which(is.na(LidarC$DOSSIERASC)))==0){
    listeASC = paste0(dirname(contours$MNT[icontour]), '/', LidarC$DOSSIERASC, '/', LidarC$NOM_ASC)
  }else{
    listeASC = paste0(dirname(contours$MNT[icontour]), '/', LidarC$NOM_ASC)
    
  }
  
  nom_MNT=paste0(texTour,"_gd_MNTg")
  
  # Creation du fichier virtuel
  nom_ascvrt = file.path(dsnDPE, "listepourvrt.txt")
  file.create(nom_ascvrt)
  write(listeASC, file = nom_ascvrt, append = T)
  vrtfile = paste0(dsnDPE, "\\", nom_MNT, ".vrt") ##chemin du vrt à créer
  cmd = paste(shQuote(OSGeo4W_path),"gdalbuildvrt",vrtfile,"-input_file_list",nom_ascvrt)
  print(cmd);system(cmd)
  unlink(nom_ascvrt)
  
  # Importation raster dans Grass
  
  cmd=paste0("r.in.gdal -o --quiet --overwrite input=",vrtfile," output=",nom_MNT)
  print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
  
  # faire une difference neighbor cote et mnt
  # recupérer les positifs
  nomDepasse=paste0(texTour,"_c_Depasse")
  exp=paste0(nomDepasse," =if(",nom_MNT,"<=",nomneigh,",1,null())")
  cmd=paste0("r.mapcalc --overwrite ",shQuote(exp))
  print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
  
  # convertir les positifs en polygones
  # r.to.vect -s --overwrite input=toto@Temp output=toto type=area
  cmd=paste0("r.to.vect --quiet --overwrite input=",nomDepasse," output=",nomDepasse," type=area")
  print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
  
  cmd=paste0("v.out.ogr --quiet --overwrite input=",nomDepasse," output=",file.path(dsnDPE,paste0(nomDepasse,".gpkg"))," format=GPKG")
  print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
  
  
  
  
  nomContourGrand_Buf2=file.path(dsnDPE,paste0(texTour,"_d_ContourRes_GrandePartie_Buf2.gpkg"))
  cmd=paste0(qgis_process, " run native:buffer",
             " --distance_units=meters --area_units=m2 --ellipsoid=EPSG:7019",
             " --INPUT=",nomContourGrand_Buf,
             " --DISTANCE=",resolution,
             " --SEGMENTS=5 --END_CAP_STYLE=0 --JOIN_STYLE=0 --MITER_LIMIT=2 --DISSOLVE=True",
             " --OUTPUT=",nomContourGrand_Buf2)
  print(cmd);system(cmd)
  
  nomDepasse1=file.path(dsnDPE,paste0(texTour,"_e_Depasse1.gpkg"))
  cmd=paste0(qgis_process, " run native:buffer",
             " --distance_units=meters --area_units=m2 --ellipsoid=EPSG:7019",
             " --INPUT=",file.path(dsnDPE,paste0(nomDepasse,".gpkg")),
             " --DISTANCE=",0,
             " --SEGMENTS=5 --END_CAP_STYLE=0 --JOIN_STYLE=0 --MITER_LIMIT=2 --DISSOLVE=True",
             " --OUTPUT=",nomDepasse1)
  print(cmd);system(cmd)
  
  # Croisement entre results et valid
  nomDepasse2=file.path(dsnDPE,paste0(texTour,"_f_Depasse2.gpkg"))
  cmd <- paste0(qgis_process, " run native:difference",
                " --INPUT=",nomDepasse1,
                " --OVERLAY=",nomContourGrand_Buf2,
                " --OUTPUT=",nomDepasse2,
                " --GRID_SIZE=None")
  print(cmd);system(cmd)
  
  nomDepasse3=file.path(dsnDPE,paste0(texTour,"_g_Depasse3.gpkg"))
  cmd=paste0(qgis_process, " run native:buffer",
             " --distance_units=meters --area_units=m2 --ellipsoid=EPSG:7019",
             " --INPUT=",nomDepasse2,
             " --DISTANCE=",resolution,
             " --SEGMENTS=5 --END_CAP_STYLE=0 --JOIN_STYLE=0 --MITER_LIMIT=2 --DISSOLVE=True",
             " --OUTPUT=",nomDepasse3)
  print(cmd);system(cmd)
  
  #
  nomDepasse4=file.path(dsnDPE,paste0(texTour,"_h_Depasse4.gpkg"))
  cmd <- paste0(qgis_process, " run native:difference",
                " --INPUT=",nomDepasse3,
                " --OVERLAY=",nomManque_Buf,
                " --OUTPUT=",nomDepasse4,
                " --GRID_SIZE=None")
  print(cmd);system(cmd)
  nommasqHWM=nomDepasse4
  
  nomDepasse5=file.path(dsnDPE,paste0(texTour,"_h_Depasse5.gpkg"))
  cmd <- paste0(qgis_process, " run native:multiparttosingleparts",
                " --INPUT=",nomDepasse4,
                " --OUTPUT=",nomDepasse5)
  print(cmd);system(cmd)
  
  
  
  newContourQ_=file.path(dsnDPE,paste0(texTour,"_i_NewContourQ_.gpkg"))
  cmd=paste0(qgis_process, " run native:union",
             " --distance_units=meters --area_units=m2 --ellipsoid=EPSG:7019",
             " --INPUT=",nomContourGrand_Buf,
             " --OVERLAY=",nomDepasse1,
             " --OVERLAY_FIELDS_PREFIX=",
             " --OUTPUT=",newContourQ_)
  print(cmd);system(cmd)
  
  
  newContourQ__=file.path(dsnDPE,paste0(texTour,"_j_NewContourQ__.gpkg"))
  cmd=paste0(qgis_process, " run native:buffer",
             " --distance_units=meters --area_units=m2 --ellipsoid=EPSG:7019",
             " --INPUT=",newContourQ_,
             " --DISTANCE=",0,
             " --SEGMENTS=5 --END_CAP_STYLE=0 --JOIN_STYLE=0 --MITER_LIMIT=2 --DISSOLVE=True",
             " --OUTPUT=",newContourQ__)
  print(cmd);system(cmd)
  
  newContourQ=file.path(dsnDPE,paste0(texTour,"_k_NewContourQ.gpkg"))
  cmd <- paste0(qgis_process, " run native:multiparttosingleparts",
                " --INPUT=",newContourQ__,
                " --OUTPUT=",newContourQ)
  print(cmd);system(cmd)
  
  Depasse=st_read(nomDepasse5)
  nlig=nrow(Depasse)
  print(nlig)
  tour=tour+1
  
  # Pour relancer la boucle
  nomManque=nomDepasse4
  nomContourGrand_Buf=newContourQ
}

# Enlever les petits trous
nomContourFinal=file.path(dsnDPE,paste0("NewContourQ.gpkg"))
cmd <- paste0(qgis_process, " run native:deleteholes",
              " --INPUT=", newContourQ,
              " --MIN_AREA=", 500*500,
              " --OUTPUT=", nomContourFinal)
print(cmd); system(cmd)

nomContourManqueFinal=file.path(dsnDPE,"NewContourQ_ManquesRestants.gpkg")
cmd <- paste0(qgis_process, " run native:multiparttosingleparts",
              " --INPUT=",nomDepasse5,
              " --OUTPUT=",nomContourManqueFinal)
print(cmd);system(cmd)


cat("\n")
cat("\n")
cat("\n")
cat("\n")
cat("###################################################################################\n")
cat("######################### C2D A LIRE SVP ###########################################\n")
cat("----------- Etape 2_7-------------------------------------------------------------\n")
cat("Vous \n")
cat("La\n")
cat("Il :\n")
cat("","\n")
cat("Modifier \n")
cat("\n")
cat("######################### Fin C2D A LIRE ###########################################\n")

