cat("\014")

source(file.path(chem_routine,"PreC2D","PreC2D_3_0_StricklerCN_job.R"))
# source(file.path(chem_routine,"PreC2D","PreC2D_3_0_StricklerCN_job_ETRANGER.R"))

# paramètres d'exports des résultats
ExportGPKG=cbind(1,0)
# 1er pour export complet sur une zone
# 2ème pour export dalles 1km²

cat("#################################################################################\n")
cat("Travail sur les zones pour préparer les dalles de calcul des paramètres (CN/Strickler)\n")
cat("--------------------------------------------------------------------------------\n")
largdalleParam=5000
# Gestion des limites
bbox=st_bbox(contours)
# Calcul des limites des dalles
XPmin=(floor(bbox$xmin/largdalleParam)+0.5)*largdalleParam
XPmax=(floor(bbox$xmax/largdalleParam)+0.5)*largdalleParam
YPmin=(floor(bbox$ymin/largdalleParam)+0.5)*largdalleParam
YPmax=(floor(bbox$ymax/largdalleParam)+0.5)*largdalleParam

# Calcul des points centraux des dalles
Coords=matrix(0,ncol=2,nrow=(((XPmax-XPmin)/largdalleParam)+1)*(((YPmax-YPmin)/largdalleParam)+1))
incC=0
for (ix in seq(XPmin,XPmax,largdalleParam))
{
  for (iy in seq(YPmin,YPmax,largdalleParam))
  {
    incC=incC+1
    Coords[incC,1:2]=cbind(ix,iy)
  }
}

# # Créer des points à partir des coordonnées et les écrire dans un fichier GeoPackage
Pts <- st_cast(st_sfc(st_multipoint(x = as.matrix(Coords[, 1:2]), dim = "XY"),crs=EPSG), "POINT")
# # Créer des carrés à partir des points et les écrire dans un fichier GeoPackage
Zones <- st_sf(data.frame(X=(Coords[,1]-0.5*largdalleParam)/1000,Y=(Coords[,2]-0.5*largdalleParam)/1000,
                          "geometry" =st_buffer(Pts, endCapStyle = "SQUARE", dist = largdalleParam/2)))

nb=st_intersects(Zones,contours)
n_int = which(sapply(nb, length)>0)
Zones=Zones[n_int,]

# verification à supprimer
nomcarre=file.path(dsnlayer,"carre.gpkg")
st_write(Zones, nomcarre, delete_dsn = T, quiet = T)

Zones$CHEMIN="CHEMIN"
Zones$DOSSIERASC="DOSSIERASC"
Zones$NOM_ASC="NOM_ASC"
Zones$ZONE=paste0(
  formatC(Zones$X, width = 4, flag = "0"),
  "_",
  formatC(Zones$Y, width = 4, flag = "0")
)

# Lecture du fchier de correspondance cn strickler bdd
data = readxl::read_excel(nomXLS)

# lecture des départements
Departement=st_read(nomDpt)

# parametre pour gérer si l'utilisatuer a imposé un dossier de sortie
dsnlayerStriCNMano=0
if (basename(dsnlayerStriCN)==dsnlayerStriCN)
{
  SousDossUser=dsnlayerStriCN
  dsnlayerStriCNMano=1
}
# browser()
# Parallélisme à faire
# Boucle sur les zones
if (nb_proc_preC2D<=1)
{
  for (iZone in 1:dim(Zones)[1])
  {
    StricklerCN(Zones,iZone,Departement)
  }
}else{
  nb_proc=nb_proc_preC2D
  cat("------ ",nb_proc            ," CALCULS MODE PARALLELE -------------\n")
  require(foreach)
  cl <- parallel::makeCluster(nb_proc)
  registerDoParallel(cl)
  foreach(iZone = 1:dim(Zones)[1],
          .inorder = FALSE,
          .packages = c("sf","dplyr")
  ) %dopar% 
    {
      StricklerCN(Zones,iZone,Departement)
    }
  stopCluster(cl)
}

# boucle sur les paramètres pour la table d'assemblage
for (iparam in 1:length(nom_Param))
{
  nom_Param_=nom_Param[iparam]
  if (nchar(dsnlayerStriCN)==0 | dsnlayerStriCNMano==1)
  {
    sous_doss=ifelse(substr(nom_Param_,1,2)=="CN","_CN",paste0("_",nom_Param_))
    
    dsnlayerStriCN_=file.path(dsnlayer,sous_doss,SousDossUser,nom_Param_)
  }else{
    dsnlayerStriCN_=dsnlayerStriCN
  }
  for (iResoStrCN in ResoStrCN)
  {
    tReso=formatC(iResoStrCN, width = 3, flag = "0")
    Zones$DOSSIERASC=paste0("_Reso",tReso)
    Zones$NOM_ASC=paste0(nom_Param_,"_",Zones$ZONE,"_Res",tReso,"m.gpkg")
    Zones$CHEMIN=file.path(Zones$DOSSIERASC,Zones$NOM_ASC)
    
    nZok=which(file.exists(file.path(dsnlayerStriCN_,Zones$DOSSIERASC,Zones$NOM_ASC))==T)
    
    nomTAZones=file.path(dsnlayerStriCN_,paste0("TA_",nom_Param_,"_Reso",tReso,".gpkg"))
    st_write(Zones[nZok,],nomTAZones,delete_dsn=T,delete_layer=T, quiet=T)
    
    file.copy( file.path(chem_routine,"C2D",Doss_Qml,"TA_Raster.qml"),
               file.path(dirname(nomTAZones),paste0(substr(basename(nomTAZones),1,nchar(basename(nomTAZones))-5),".qml")),
               overwrite = T)
    
    nomTAexp=nomTAZones
    debraciTA=substr(nomTAexp,1,nchar(nomTAexp)-1-nchar(strsplit(nomTAexp,"\\.")[[1]][length(strsplit(nomTAexp,"\\.")[[1]])]))
    nomTAexpbuf=paste0(debraciTA,"buf.gpkg")
    nomTAexprempli=paste0(debraciTA,"rempli.gpkg")
    nomTAexpvide=paste0(debraciTA,"vide.gpkg")
    
    cmd=paste0(qgis_process, " run native:buffer",
               " --INPUT=",nomTAexp,
               " --DISTANCE=1 --SEGMENTS=5 --END_CAP_STYLE=0 --JOIN_STYLE=0 --MITER_LIMIT=2 --DISSOLVE=True",
               " --OUTPUT=",nomTAexpbuf)
    print(cmd);system(cmd)
    
    # Enlever les petits trous
    cmd <- paste0(qgis_process, " run native:deleteholes",
                  " --INPUT=", nomTAexpbuf,
                  " --MIN_AREA=", 0,
                  " --OUTPUT=", nomTAexprempli)
    print(cmd); system(cmd)
    
    cmd <- paste0(qgis_process, " run native:difference",
                  " --INPUT=",nomTAexprempli,
                  " --OVERLAY=",nomTAexpbuf,
                  " --OUTPUT=",nomTAexpvide,
                  " --GRID_SIZE=None")
    print(cmd);system(cmd)
    
  }
}

cat("\n")
cat("\n")
cat("\n")
cat("\n")
cat("###################################################################################\n")
cat("######################### C2D A LIRE SVP ###########################################\n")
cat("------------------------- Etape PreC2D 3_0-------------------------------------------\n")
cat("Vous avez fini cette étape.\n")
cat("Vos traitements sur le dernier paramètre choisi et la dernière résolution choisie parmi\n")
cat(nom_Param,"\n")
cat("sont par exemple sur le dossier:\n")
cat(nomTAZones,"\n")
cat("######################### Fin C2D A LIRE ###########################################\n")
