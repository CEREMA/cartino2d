# https://www.reperesdecrues.developpement-durable.gouv.fr/services/reperes?sit_departement_code=13&full

cat("\014")

ChoixPC2D1_1 = cbind(
  "1. BDTopo",
  "2. RGEAlti",
  "3. OCS_GE",
  "4. BDNRC Repères de crue - PHE"
)
nChoixPC2D1_1 =FILINO_BDD("Prétraitement par secteur ou Fusion de secteurs (NOMPOST)",NULL,ChoixPC2D1_1 )

ListeDpt_=ListeDpt
ici1=which(nchar(ListeDpt)==1);if(length(ici1)>0){ListeDpt[ici1]=paste0("00",ListeDpt[ici1])}
ici2=which(nchar(ListeDpt)==2);if(length(ici2)>0){ListeDpt[ici2]=paste0("0",ListeDpt[ici2])}

##################################################################################
#----------------- Fonctions communes --------------------------------------------
##################################################################################
ChargeURL=function(dsn,url,destfile)
{
  if (nchar(destfile)==0)
  {destfile=file.path(dsn,basename(url))}
  cat("#--------------------------------------------------------------------\n")
  cat("#--------- Chargement de ",basename(url),"\n")
  cat(url,"\n")
  if (file.exists(destfile)==F)
  {
    try(download.file(url, destfile, method="curl", quite =TRUE))
    test=file.info(destfile)
    if (is.na(test$size)==T)
    {
      {unlink(destfile)}
    }else{
      if (test$size<1000)
      {unlink(destfile)}  
    } 
  }else{
    cat(" Présent\n")
  }
}

Lien_html=function(url)
{
  # Lire la page web
  webpage <- read_html(url)
  
  # Extraire tous les liens
  links <- webpage %>%
    html_nodes("a") %>%
    html_attr("href")
  
  links=links[grep(links,pattern="_D")]
  return(links)
}
##################################################################################
#----------------- BDTopo -------------------------------------------------------
##################################################################################

if (nChoixPC2D1_1[1]==1)
{
  destfileBDTopo=""
  links=Lien_html(urlBDTopo)
  # Afficher les liens
  links=links[-grep(links,pattern="SHP")]
  
  for (idpt in ListeDpt)
  {
    numero=idpt
    nliendpt=grep(links,pattern=paste0("_D",numero))
    
    print(links[nliendpt])
    
    url=links[nliendpt][1]
    
    ChargeURL(dsnDptBDTopo,url,"")
    destfile=file.path(dsnDptBDTopo,basename(url))
    if (dir.exists(substr(destfile,1,nchar(destfile)-3))==F)
    {
      # Exemple pour Windows
      system2(chemin7z, args = c("x", destfile, paste0("-o", dsnDptBDTopo)))
    }
    destfileBDTopo=destfile
  }
}

##################################################################################
#----------------- RGEAlti -------------------------------------------------------
##################################################################################
if (nChoixPC2D1_1[2]==1)
{
  destfileRGEAlti=""
  # URL de la page web
  
  links=Lien_html(urlRGEAlti)
  
  # Afficher les liens
  links=links[-grep(links,pattern="0_5M_ASC_")]
  
  for (idpt in ListeDpt)
  {
    if (idpt>=970){dsnRGEAlti=dirname(dsnRGEAlti)}
    numero=idpt
    nliendpt=grep(links,pattern=paste0("_D",numero))
    if (length(nliendpt)>0)
    {
      print(links[nliendpt])
      for (ilien in nliendpt)
      {
        url=links[ilien]
        ChargeURL(dsnRGEAlti,url,"")
      }
      destfile=file.path(dsnRGEAlti,basename(strsplit(links[nliendpt][1],".7z")[[1]][1]))
      if (dir.exists(destfile)==F)
      {
        # Exemple pour Windows
        Fich7z=file.path(dsnRGEAlti,basename(links[nliendpt][1]))
        system2(chemin7z, args = c("x", Fich7z, paste0("-o", dsnRGEAlti)))
      }
      destfileRGEAlti=destfile
    }
  }
  
  # Table asemblage classique
  cat(format(Sys.time(),format="%Y%m%d_%H%M%S")," Table assemblage","\n")
  largdalle=1000
  nEPSG=EPSG
  TypeTIN="RGEAlti"
  NomDirSIGBase ="00_SIGBase"
  cat("Création de la table d'assemblage\n")
  source(file.path(chem_routine,"/FILINO/filino/RFILES/FILINO_02_00c_TablesAssemblagesLazIGN.R"))
  
  colnames(paramRGEAlti)=cbind("0","0","Xdeb","Xfin","Ydeb","Yfin")
  paramRGEAlti=as.data.frame(paramRGEAlti)
  FILINO_00c_TA(dsnRGEAlti,nomTARGEAlti,".asc$",paramRGEAlti,qgis_process)
  
  cat(format(Sys.time(),format="%Y%m%d_%H%M%S")," Relecture Table assemblage","\n")
  TARGEAlti=st_read(file.path(dsnRGEAlti,nomTARGEAlti))
  cat(format(Sys.time(),format="%Y%m%d_%H%M%S")," Gestion des reso Table assemblage","\n")
  ici=grep(TARGEAlti$CHEMIN,pattern="Reso")
  if (length(ici)>0){TARGEAlti=TARGEAlti[-ici,]}
  TARGEAlti$Date=substr(basename(TARGEAlti$DOSSIERASC),nchar(basename(TARGEAlti$DOSSIERASC))-7,nchar(basename(TARGEAlti$DOSSIERASC)))
  TARGEAlti=TARGEAlti[order(TARGEAlti$NOM_ASC,TARGEAlti$Date,decreasing = T),]
  
  cat(format(Sys.time(),format="%Y%m%d_%H%M%S")," Gestion des dernières à garderTable assemblage","\n")
  # 
  # numAgarder=sapply(unique(TARGEAlti$NOM_ASC), function(x) {which(TARGEAlti$NOM_ASC==x)[1]})
  # TARGEAlti=TARGEAlti[numAgarder,]
  
  ndoublons=which(TARGEAlti$NOM_ASC[-1]==TARGEAlti$NOM_ASC[-nrow(TARGEAlti)])
  # st_write(TARGEAlti[sort(c(ndoublons,ndoublons+1)),],file.path(dsnRGEAlti,"TARGEAlti_doublons.gpkg"), delete_dsn = T,delete_layer = T, quiet = T)
  # st_write(TARGEAlti[-(ndoublons+1),],file.path(dsnRGEAlti,"TARGEAlti_sansdoublons.gpkg"), delete_dsn = T,delete_layer = T, quiet = T)
  TARGEAlti=TARGEAlti[-(ndoublons+1),]
  cat(format(Sys.time(),format="%Y%m%d_%H%M%S")," Ecriture Table assemblage","\n")
  st_write(TARGEAlti,file.path(dsnRGEAlti,nomTARGEAlti), delete_dsn = T,delete_layer = T, quiet = T)
  
  ###############################################################################
  cat(" On fusionne les dalles.shp de chaque dossier pour connaitre le nombre de points lidar\n")
  # Table assemblage des tables IGN
  cat(format(Sys.time(),format="%Y%m%d_%H%M%S")," - Recherche parfois longue des fichiers ","\n")
  listeTA_SHPIGN=list.files(dsnRGEAlti,"dalles.shp",recursive = T)
  cat(format(Sys.time(),format="%Y%m%d_%H%M%S")," - Fin Recherche des fichiers ","\n")
  
  cat(format(Sys.time(),format="%Y%m%d_%H%M%S")," Ecriture Table assemblage","\n")
  dir_tmp=file.path(dsnRGEAlti,"temp")
  dir.create(dir_tmp)
  wd=getwd()
  setwd(dir_tmp)
  cmd=paste0(qgis_process, " run native:mergevectorlayers")
  for (iPHE in listeTA_SHPIGN)
  {
    idpt_=str_split(iPHE,"_D")[[1]][3]
    idpt=substr(idpt_,1,3)
    idate=substr(idpt_,7,12)
    tab=st_read(file.path(dsnRGEAlti,iPHE))
    # nomdalle_tmp=file.path(dir_tmp,paste0("dalles",idpt,"_",idate,".gpkg"))
    nomdalle_tmp=paste0("d",idpt,"_",idate,".gpkg")
    st_write(tab,nomdalle_tmp, delete_dsn = T,delete_layer = T, quiet = T)
    cmd=paste0(cmd," --LAYERS=",shQuote(nomdalle_tmp))
  }
  cmd=paste0(cmd,
             paste0(" --CRS=QgsCoordinateReferenceSystem('EPSG:",EPSG,"') "),
             " --OUTPUT=",shQuote(file.path(dsnRGEAlti,"TA_SHPIGN.gpkg")))
  
  print(cmd);system(cmd)
  setwd(wd)
  
  TA_SHPIGN=st_read(file.path(dsnRGEAlti,"TA_SHPIGN.gpkg"))
  
  TA_SHPIGN$Date=substr(TA_SHPIGN$layer,6,11)
  
  nFXX=which(as.numeric(substr(TA_SHPIGN$NOM_DALLE,13,16))>0)
  
  TA_SHPIGN$X[nFXX ]=substr(TA_SHPIGN$NOM_DALLE,13,16)[ nFXX]
  TA_SHPIGN$Y[nFXX ]=substr(TA_SHPIGN$NOM_DALLE,18,21)[ nFXX]
  TA_SHPIGN$X[-nFXX]=substr(TA_SHPIGN$NOM_DALLE,21,24)[-nFXX]
  TA_SHPIGN$Y[-nFXX]=substr(TA_SHPIGN$NOM_DALLE,26,29)[-nFXX]
  
  TA_SHPIGN=TA_SHPIGN[order(TA_SHPIGN$X,TA_SHPIGN$Y,TA_SHPIGN$Date,decreasing = T),]
  st_write(TA_SHPIGN,file.path(dsnRGEAlti,"TA_SHPIGN_avec_doublons.gpkg"), delete_dsn = T,delete_layer = T, quiet = T)
  
  ndoublons=which(TA_SHPIGN$X[-1]==TA_SHPIGN$X[-nrow(TA_SHPIGN)] & TA_SHPIGN$Y[-1]==TA_SHPIGN$Y[-nrow(TA_SHPIGN)])
  st_write(TA_SHPIGN[sort(c(ndoublons,ndoublons+1)),],file.path(dsnRGEAlti,"TA_SHPIGN_doublons.gpkg"), delete_dsn = T,delete_layer = T, quiet = T)
  
  st_write(TA_SHPIGN[-(ndoublons+1),],file.path(dsnRGEAlti,"TA_SHPIGN_sansdoublons.gpkg"), delete_dsn = T,delete_layer = T, quiet = T)
}
##################################################################################
#----------------- OCS_GE -------------------------------------------------------
##################################################################################
if (nChoixPC2D1_1[3]==1)
{
  destOCSGE=""
  links=Lien_html(urlOCSGE)
  
  for (idpt in ListeDpt)
  {
    numero=idpt
    nliendpt=grep(links,pattern=paste0("_D",numero))
    if (length(nliendpt)>0)
    {
      print(links[nliendpt])
      # browser()
      nliendpt=nliendpt[1]
      for (ilien in nliendpt)
      {
        url=links[ilien]
        ChargeURL(dsnDptOCS_GE,url,"")
      }
      
      destfile=file.path(dsnDptOCS_GE,basename(strsplit(links[nliendpt][1],".7z")[[1]][1]))
      if (dir.exists(destfile)==F)
      {
        if (file.exists(dirname(destfile))==F){dir.create(dirname(destfile),recursive=T)}
        # Exemple pour Windows
        Fich7z=file.path(dsnDptOCS_GE,basename(links[nliendpt][1]))
        system2(chemin7z, args = c("x", Fich7z, paste0("-o", dsnDptOCS_GE)))
      }
      destOCSGE=destfile
    }
  }
}

##################################################################################
#----------------- Repères de crues -------------------------------------------------------
##################################################################################
if (nChoixPC2D1_1[4]==1)
{
  if (file.exists(dsnPHE)==F){dir.create(dsnPHE)}
  
  ici1=which(nchar(ListeDpt_)==1);if(length(ici1)>0){ListeDpt_[ici1]=paste0("0",ListeDpt_[ici1])}
  
  for (idpt in ListeDpt_)
  {
    url=paste0("https://www.reperesdecrues.developpement-durable.gouv.fr/services/reperes?sit_departement_code=",idpt,"&full")
    
    destfile=file.path(dsnPHE,paste0("reperes",idpt,".json"))
    if (file.exists(destfile)==F)
    {
      ChargeURL(dsnPHE,url,destfile)
      cat("Pause de 30 secondes pour ne pas trop fatiguer le serveur du schapi\n")
      Sys.sleep(30)
    }
  }
  
  cat(format(Sys.time(),format="%Y%m%d_%H%M%S")," Fusion des couches Reperes","\n")
  listePHE=list.files(dsnPHE,"reperes")
  
  cmd=paste0(qgis_process, " run native:mergevectorlayers")
  for (iPHE in listePHE)
  {
    cmd=paste0(cmd," --LAYERS=",shQuote(file.path(dsnPHE,iPHE)))
  }
  cmd=paste0(cmd,
             paste0(" --CRS=QgsCoordinateReferenceSystem('EPSG:",4326,"') "),
             " --OUTPUT=",shQuote(file.path(dsnPHE,nomPHE)))
  print(cmd);system(cmd)
  cat(format(Sys.time(),format="%Y%m%d_%H%M%S")," fin de Fusion des couches Reperes","\n")
  cat("Fichier créé: ",file.path(dsnPHE,nomPHE),"\n")
}

# Bonjour Frédéric,
# 
# Il commence à y avoir beaucoup de repères dans le département 34 (plus de 7000 en base: https://www.reperesdecrues.developpement-durable.gouv.fr/services/reperes?sit_departement_code=34&die=nodes)
# Difficile de faire la requête en 1 seule fois (quel que soit l'appareil utilisé pour cela), pour arriver à tous les récupérer tu peux couper la requête en paquets:
# Les 5000 premiers:
# https://www.reperesdecrues.developpement-durable.gouv.fr/services/reperes?sit_departement_code=34&full&limit=5000&page=1
# Les suivants:
# https://www.reperesdecrues.developpement-durable.gouv.fr/services/reperes?sit_departement_code=34&full&limit=5000&page=2
# Tu peux également utiliser le plugin repères de crues qui utilise ce découpe en paquets.
# 
# Bonne journée,


cat("\n")
cat("\n")
cat("\n")
cat("\n")
cat("###################################################################################\n")
cat("######################### C2D A LIRE SVP ###########################################\n")
if (exists("destfileBDTopo"))
{
  if (nChoixPC2D1_1[1]==1 & nchar(destfileBDTopo)>0)
  {
    cat("Vous aviez ou vous venez de charger des données de la BDTopo.\n")
    cat("Par exemple, le fichier: ",destfileBDTopo)
  }else{
    cat("Aucun chargement BDTopo\n")
  }
}

cat("\n")
if (exists("destfileRGEAlti"))
{
  if (nChoixPC2D1_1[2]==1 & nchar(destfileRGEAlti)>0)
  {
    cat("Vous venez de charger des données du RGE Alti.\n")
    cat("Par exemple, le fichier: ",destfileRGEAlti)
  }else{
    cat("Aucun chargement RGEAlti\n")
  }
}

cat("\n")

if (nChoixPC2D1_1[2]==1)
{
  cat("La table d'assemblage ",file.path(dsnRGEAlti,nomTARGEAlti)," a été mis à jour\n")
}
cat("\n")

if (exists("destOCSGE"))
{
  if (nChoixPC2D1_1[3]==1 & nchar(destOCSGE)>0)
  {
    cat("Vous aviez ou vous venez de charger des données OCS_GE.\n")
    cat("Par exemple, le fichier: ",destOCSGE)
  }else{
    cat("Aucun chargement OCSGE\n")
  }
}
cat("\n")
cat("######################### Fin C2D A LIRE ###########################################\n")
