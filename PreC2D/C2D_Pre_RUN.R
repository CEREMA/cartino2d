# Cest_Contour_pas_Zone=0

cat("\014") # Nettoyage de la console

source(file.path(chem_routine,"FILINO","filino","RFILES","FILINO_Utils.R"))
source(file.path(chem_routine,"PreC2D","PreC2D_Outils.R"))
source(file.path(chem_routine,"PreC2D","PreC2D_00_Confidentiel.R"))

################################################################################
############## Menu principal ##################################################
################################################################################

nPreC2D=matrix(1,7,1)
source(file.path(chem_routine,"PreC2D","PreC2D_Texte.R"))

ChoixPreC2D = cbind(
  "1. Chargement des bases de données, Tables d'assemblage",
  "2. Sectorisation (souvent après un 1er calcul C2D préliminaire Ex:25m): Redécoupage en plus petits secteurs de calculs (sous-bassins Pluie/Débit)",
  "3. Paramétrages (Strickler et CN spatialisés)",
  "4. Forçages (Pluies ou Débits)",
  "5. EN CONSTRUCTION References (Bascule de données externes en format utilisables dans C2D)",
  "6. VersionBeta Emprise ZI pour maillage fin / Alea (souvent après 2ème calcul C2D intermédiaire Ex:5m) - simplification, décrénelage...",
  "7. EN CONSTRUCTION Création sections de controle (Croisement Couche Points/Emprise ZI) - A controler MANUELLEMENT - OH flash",
  "8. EN CONSTRUCTION REMODELAGE SECTEURS PLUIE"
)
nPreC2D=FILINO_BDD("Prétraitement de C2D - 1er choix retenu",NULL,ChoixPreC2D)
nbrprc2d=which(nPreC2D==1)
if (length(nbrprc2d)>1)
{
  nPreC2D=nPreC2D[nbrprc2d[1]]
  cat("-------------------------------------------------------------------\n")
  cat("-------------- Seule votre 1er choix sera retenu-------------------\n")
  cat("-----------------------Pause de 5 secondes------------------------\n")
  Sys.sleep(5)
}

cat("\014") # Nettoyage de la console
cat("###################################################################################\n")
cat("######################### C2D A LIRE SVP ###########################################\n")
source(file.path(chem_routine,"PreC2D","PreC2D_Texte.R"))
cat("######################### Fin C2D A LIRE ###########################################\n")

################################################################################
############## Choix de travailler par 
#----------------  Secteur cartino2D -------------------------------------------
#----------------  Fusion de Secteurs cartino2D --------------------------------
################################################################################
################################################################################
#-------------------------------------------------------------------------------
#---------- Choix des contours
ChoixContours=function(dsnlayer,nomlayerC)
{
  #----------------  Ouverture du contour des secteurs ---------------------------
  # contours d'étude
  contours <- st_read(file.path(dsnlayer, nomlayerC))
  contours=contours[order(contours$STEP_PRE,contours$NOM),]
  # Choix des secteurs à traiter
  if (dim(contours)[1]<500)
  {
    
    # par choix en boite de dialogue
    # FILINO_BDD("Choisir les secteurs à traiter ( boite si moins de 150 pré-choix)",contours$NOM[which(contours$STEP_PRE==0)],contours$NOM)
    nchoixS = select.list(contours$NOM,preselect = contours$NOM[which(contours$STEP_PRE==0)],
                          title = "Choisir les secteurs à traiter ( boite si moins de 150 pré-choix",multiple = T,graphics = T)
    nlalaS = which(contours$NOM %in% nchoixS)
    if (length(nlalaS)==0){VOUSAVEZPASCHOISI=BADABOOM}
    # On focalise sur le champ ATRAITER==1
    contours=contours[nlalaS,]
    contours$STEP_PRE=0
  }else{
    contours=contours[which(contours$STEP_PRE==0),]
  }
  return(contours)
}

#-------------------------------------------------------------------------------
#---------- Travail au niveau des contour ou des post
ChoixContours_Sect_ou_Fus=function(contours)
{
  ChoixSF = cbind(
    "1. Travail sur chacun des secteurs",
    "2. Travail sur la fusion"
  )
  nChoixSF=FILINO_BDD("Prétraitement par secteur ou Fusion de secteurs (NOMPOST)",NULL,ChoixSF)
  
  # Modification du contour pour fusionner au niveau de NOMPOST
  if (nChoixSF[2]==1)
  {
    cat("Fusion parfois longue\n")
    contours_l=lapply(unique(contours$NOMPOST), function(x) {
      st_sf(NOMPOST=x,geometry=st_union(contours[which(contours$NOMPOST==x)]),crs=EPSG)})
    contours=do.call(rbind,contours_l)
  }
  return(contours)
}


# contours=ChoixContours(dsnlayer,nomlayerC)
# contours=ChoixContours_Sect_ou_Fus(contours)
NomDirSIGBase = file.path(chem_routine,"C2D",Doss_Qml,"TA_Raster.qml")
################################################################################
############## 1. BDD Vecteurs ################################################
################################################################################
if (nPreC2D[1]==1)
{
  ChoixPC2D1 = cbind(
    "1. Croisement avec départements (France hexagonale)",
    "2. Liste manuelle de n° de département (plus pour Corse et Outres-Mers)",
    "3. Table d'assemblage"
  )
  nChoixPC2D1=FILINO_BDD("Prétraitement par secteur ou Fusion de secteurs (NOMPOST)",NULL,ChoixPC2D1)
  
  if (nChoixPC2D1[3]==0)
  {
    if (nChoixPC2D1[1]==1)
    {
      contours=ChoixContours(dsnlayer,nomlayerC)
      # Contour des Départements
      Departement=st_read(nomDpt)  
      #5000 en dur...
      nb=st_intersects(Departement,st_buffer(contours,5000))
      n_int = which(sapply(nb, length)>0)
      ListeDpt <- Departement$INSEE_DEP[n_int]
    }else{
      demander_nombre <- function() {
        input <- readline(prompt = "Veuillez entrer un nombre : ")
        return(input)
      }
      nombre <- demander_nombre()
      cat("Vous avez entré le nombre :", nombre, "\n")
      composants <- strsplit(nombre, ",")[[1]]
      # Convertir les composants en nombres
      ListeDpt <- composants
    }
    
    if (nChoixPC2D1[1]==1 |nChoixPC2D1[2]==1)
    {
      source(file.path(chem_routine,"PreC2D","PreC2D_1_1_ChargeBDDVecteurs.R"))
    }
    # }else{
    #   
    #   
    #   source(file.path(chem_routine,"/FILINO/filino/RFILES/FILINO_02_00c_TablesAssemblagesLazIGN.R"))
    #   nomTAmano="TA_RGEALTI_FR.shp"
    #   paramTAmano=cbind(0,0,13,16,18,21)
    #   colnames(paramTAmano)=cbind("0","0","Xdeb","Xfin","Ydeb","Yfin")
    #   paramTAmano=as.data.frame(paramTAmano)
    #   FILINO_00c_TA(dsnRGEAlti,nomTAmano,extTAmano,paramTAmano,qgis_process)
  }
  if (nChoixPC2D1[3]==1)
  {
    largdalle=1000
    nEPSG=EPSG
    TypeTIN="Mano"
    colnames(paramTAmano)=cbind("0","0","Xdeb","Xfin","Ydeb","Yfin")
    paramTAmano=as.data.frame(paramTAmano)
    source(file.path(chem_routine,"/FILINO/filino/RFILES/FILINO_02_00c_TablesAssemblagesLazIGN.R"))
    FILINO_00c_TA(dsnTAmano,nomTAmano,extTAmano,paramTAmano,qgis_process,"","")
  }
}

GestNomMasq=0
################################################################################
############## 2. Sectorisation ################################################
################################################################################
if (nPreC2D[2]==1)
{
  nPreC2D[-1]=0
  ChoixPreC2D_Sect = cbind(
    "2.1. Zone Débit, Pluie et Endoréique (Post C2D 25m Déluge) et Sectorisation Debit",
    "2.2. Zones Potentielles de Ponts (ZPP)",
    "2.3. Modifs MNT trous points bas endorésimes et/ou creusement ZPP",
    "2.4. Calcul des bassins versants",
    "2.5. Sectorisation Pluie",
    "2.6. Analyse des PHE - sans doute mal positionné dans les menus...",
    "2.7. Remodelage Secteurs DEBITS"
  )
  nPreC2D_Sect=FILINO_BDD("PreC2d Sectorisation",NULL,ChoixPreC2D_Sect)
  
  contours=ChoixContours(dsnlayer,nomlayerC)
  contours=ChoixContours_Sect_ou_Fus(contours)

  # for (icontour in 1:nrow(contours))
  for (icontour in nrow(contours):1)
  {
    dsnDPE=
      # ifelse(nChoixSF[1]==1,
      ifelse(dim(contours)[2]>3,
             file.path(dsnlayer,contours$NOMPOST[icontour],contours$NOM[icontour],"POST"),
             file.path(dsnlayer,contours$NOMPOST[icontour],"_Fusion")
      )
    
    # Gestion du nom masque vide ou pas
    if (nchar(nommasque)==0)
    {
      # nommasque=file.path(dsnDPE,"masque_tmp.gpkg")
      # st_write(contours[icontour,],nommasque, delete_dsn = T,delete_layer = T, quiet = T)
      nommasque=file.path(dirname(dsnDPE),"Step_Cartino2d_2_Valid_Calcul.shp")
      
      GestNomMasq=1
    }
    
    listraci=list.files(dsnDPE,"HWH_m.gpkg$")
    if (nPreC2D_Sect[1]==1)
    {
      source(file.path(chem_routine,"PreC2D","PreC2D_2_1_Zone_DPE.R"))
    }
    #------------- PreC2D_2_2_DetectPONTS_OH.R
    if (nPreC2D_Sect[2]==1)
    {
      # Cest_Contour_pas_Zone=1
      Zones=st_buffer(contours[icontour,],1000)
      Zones$ZONE=contours$NOM[icontour]
      dsnexport=dsnDPE
      if (dir.exists(dsnexport)==F){dir.create(dsnexport)}
      source(file.path(chem_routine,"PreC2D","PreC2D_2_2_DetectPONTS_OH.R"))
    }
    
    #--------------- PreC2D_2_3_MNT_Endoreismes_ZPP.R
    if (nPreC2D_Sect[3]==1)
    {
      
      for (raci in substr(listraci,1,nchar(listraci)-10))
      { print(raci)
        # Cest_Contour_pas_Zone=1
        ZONE=st_buffer(contours[icontour,],1000)
        ZONE$ZONE=contours$NOM[icontour]
        nomZPP=file.path(dsnDPE,list.files(dsnDPE,"_ZPP_Final.gpkg")[1])
        nomInond_Endo=list.files(dsnDPE,"_Endo_Manuel.gpkg")
        source(file.path(chem_routine,"PreC2D","PreC2D_2_3_MNT_Endoreismes_ZPP.R"))
      }
    }
    
    #--------------- PreC2D_2_4_TauDEM.R
    if (nPreC2D_Sect[4]==1)
    {
      for (raci in substr(listraci,1,nchar(listraci)-10))
      {
        PreC2D_03_TauDEM=1
        chemin=dirname(dsnDPE)
        setwd(dir=chemin)
        ## NE PAS MODIFIER ------------------------------------------------------------
        dir  = basename(dsnDPE)
        dir_tau_riv = basename(dsnDPE)
        riv_name=paste0(raci,"_MNTPourEndo")
        format_MNT=".tif"
        src_thres=rbind(c(10,5,2,1,0.5)*1e6,matrix(ResoMNTpourTaudem,1,5))
        nb_riv=1
        dirProjQgz=file.path(chem_routine,"C2D",Doss_Qml)
        
        source(file.path(chem_routine,"PreC2D","PreC2D_2_4_TauDEM.R"))
      }
    }
    
    #--------------- PreC2D_2_5_Secto_Pluie.R
    if (nPreC2D_Sect[5]==1)
    { 
      for (raci in substr(listraci,1,nchar(listraci)-10))
      {
        watershed=file.path(dsnDPE,paste0(raci,"_MNTpourEndo"),"500000","watershed.shp")
        Bords=file.path(dsnDPE,paste0(raci,"_MNTpourEndo"),"500000","Bords.shp") # Attention 0.5km²
        NomReseau=file.path(dsnDPE,paste0(raci,"_MNTpourEndo"),"5000000","streamnet.shp")# attention 5km2
        source(file.path(chem_routine,"PreC2D","PreC2D_2_5_Secto_Pluie.R"))
      }
    }
    
    #--------------- PreC2D_2_6_PHE
    
    if (nPreC2D_Sect[6]==1)
    {
      listraci=list.files(dsnDPE,"Aext")
      PHE=st_read(file.path(dsnPHE,nomPHE))
      for (raci in listraci)
      {
        source(file.path(chem_routine,"PreC2D","PreC2D_2_6_PHE.R"))
      }
      
      # on pourrait gérer avec une gestion de scénario comme étape C2D10
    }
    
    #--------------- PreC2D_2_7_RemodelageDEBIT
    listraci=list.files(dsnDPE,"HWH_m.gpkg$")
    if (nPreC2D_Sect[7]==1)
    {
      for (raci in substr(listraci,1,nchar(listraci)-10))
      {
        source(file.path(chem_routine,"PreC2D","PreC2D_2_7_RemodelageDEBIT.R"))
      }
    }
    
    
    if (GestNomMasq==1)
    {
      unlink(nommasque)
      GestNomMasq=0
    }
  }
  
  if (exists("raci")==T)
  {
    listraci=list.files(dsnDPE,"HWH_m.gpkg$")
    for (raci in substr(listraci,1,nchar(listraci)-10))
    {
      nomQgsDPE=file.path(dsnDPE,paste0("Projet_Sectorisation",raci,".qgs"))
      file.copy(file.path(chem_routine,"C2D",Doss_Qml,"Projet_Sectorisation.qgs"),
                nomQgsDPE)
      # Lire le fichier texte
      lines <- readLines(nomQgsDPE, encoding = "UTF-8")
      
      # Remplacer "sectorisation" par le contenu de la variable raci
      lines_modified <- gsub("Sectorisation", raci, lines)
      
      unlink(nomQgsDPE)
      # Écrire le résultat dans un nouveau fichier texte
      nomQgsDPE=file.path(dsnDPE,paste0("Projet_",raci,".qgs"))
      writeLines(lines_modified,nomQgsDPE)
    }
    cat("###################################################################################\n")
    cat("######################### C2D A LIRE SVP ###########################################\n")
    cat("------------------------- Etape commeune PreC2D 2--------------------------------\n")
    cat("Merci d'ouvrir pour comprendre à chaque étape le fichier:\n")
    cat(nomQgsDPE,"\n")
    cat("et le refermer avant l'étape suivante\n")
    cat("Attention, il est recréé à chaque étape, si vous le modifiez à l'étape x, pensez à le sauvegarder sous un autre nom.\n")
    cat("\n")
    cat("######################### Fin C2D A LIRE ###########################################\n")
  }
}

################################################################################
############## 3. Paramétrages #################################################
################################################################################
if (nPreC2D[3]==1)
{
  contours=ChoixContours(dsnlayer,nomlayerC)
  source(file.path(chem_routine,"PreC2D","PreC2D_3_0_StricklerCN_pilotage.R"))
}

################################################################################
############## 4. Forçages #####################################################
################################################################################
if (nPreC2D[4]==1)
{
  # nPreC2D[-3]=0
  ChoixPreC2D_Forc = cbind(
    "4.1. PLUIE mesurée RADAR",
    "4.2. EN CONSTRUCTION PLUIE AFAIRE mesurée PLUVIO",
    "4.3. PLUIE statistique (SHYREG)",
    "4.4. AFAIRE DEBITS mesurés",
    "4.5. EN CONSTRUCTION DEBITS statistiques (SHYREG)"
  )
  nPreC2D_Forcages=FILINO_BDD("PreC2D Fichiers de Forcages",NULL,ChoixPreC2D_Forc)
  
  if (nPreC2D_Forcages[1]==1)
  {
    CONTOURSINI=ChoixContours(dsnlayer,nomlayerC)
    source(file.path(chem_routine,"PreC2D","PreC2D_4_1_RexPluie_radar.R"))
  }
  if (nPreC2D_Forcages[2]==1)
  {
    source(file.path(chem_routine,"PreC2D",""))
  }
  if (nPreC2D_Forcages[3]==1)
  {
    contours=ChoixContours(dsnlayer,nomlayerC)
    source(file.path(chem_routine,"PreC2D","PreC2D_4_3_SHYREG_Pluie.R"))
  }
  if (nPreC2D_Forcages[4]==1)
  {
    source(file.path(chem_routine,"PreC2D",""))
  }
  if (nPreC2D_Forcages[5]==1)
  {
    CONTOURSINI=ChoixContours(dsnlayer,nomlayerC)
    source(file.path(chem_routine,"PreC2D","PreC2D_4_5_ReseauSHYREG_Debits_Utilitaires.R"))
    source(file.path(chem_routine,"PreC2D","PreC2D_4_5_ReseauSHYREG_Debits.R"))
  }
}

################################################################################
############## 5. References ###################################################
################################################################################
if (nPreC2D[5]==1)
{
  nPreC2D[-4]=0
  source(file.path(chem_routine,"PreC2D","PreC2D_5_0_Formatage_Obs.R"))
}

################################################################################
############## 5. References ###################################################
################################################################################
if (nPreC2D[6]==1)
{
  contours=ChoixContours(dsnlayer,nomlayerC)
  contours=ChoixContours_Sect_ou_Fus(contours)
  
  dsnDPE=
    # ifelse(nChoixSF[1]==1,
    ifelse(dim(contours)[2]>3,
           file.path(dsnlayer,contours$NOMPOST,contours$NOM,"POST"),
           file.path(dsnlayer,contours$NOMPOST,"_Fusion")
    )
  
  cat("#########################################################################\n")
  cat("------------------------------ EN CONSTRUCTION --------------------------\n")
  cat("---------- Tout n'est pas clair pour optimiser cette étape---------------\n")
  cat("------------------------------ EN CONSTRUCTION --------------------------\n")
  cat("OPTION 1 - VOUS DEVEZ MODIFIER A LA MAIN LES PARAMETRES de:\n")
  cat(file.path(chem_routine,"PreC2D","PreC2D_6_1_PostZIpour maillage_ou_ALEA.R"),"\n")
  cat("Copier dsnDPE=","    sans l'extension\n")
  print(dsnDPE)
  cat("Choisir la racine du fichier: raci=\n")
  cat("Modifier au besoin NomMasque, Reso_Ini, buf, BufPlus=3*Reso_Ini, BufMoins=2*Reso_Ini, seuilSup0=100\n")
  cat("OPTION 2 - VOUS DEVEZ MODIFIER A LA MAIN LES PARAMETRES de:\n")
  cat(file.path(chem_routine,"PreC2D","PreC2D_6_2_Decrenelage_pilotage.R"),"\n")
  cat("Modifier au besoin Reso,nomrtovect,AireInterieureSupp,AireExterieureSupp\n")
  cat("nomrtovect a été fourni à la fin de l'étape 1\n")
  
  ChoixPreC2D_6 = cbind(
    "6.1. PostZIpour maillage_ou_ALEA",
    "6.2. Decrenelage"
  )
  nPreC2D_6=FILINO_BDD("Emprise ZI pour maillage fin / Alea (souvent après 2ème calcul C2D intermédiaire Ex:5m) - simplification, décrénelage...",NULL,ChoixPreC2D_Sect)
  if (nPreC2D_6[1]==1)
  {
    source(file.path(chem_routine,"PreC2D","PreC2D_6_1_PostZIpour maillage_ou_ALEA.R"))
    cat("le nom du fichier est à entré dans nomrtovect de l'étape suivante\n")
  }
  if (nPreC2D_6[2]==1)
  {
    source(file.path(chem_routine,"PreC2D","PreC2D_6_2_Decrenelage_pilotage.R"))
  }
}

################################################################################
############## 7. Positionnement des sections de contrôle ######################
################################################################################
if (nPreC2D[7]==1)
{
  contours=ChoixContours(dsnlayer,nomlayerC)
  contours=ChoixContours_Sect_ou_Fus(contours)
  
  # lecture de spoints où on veut mettre des sections de controle
  SH=st_read(nomSH)

  for (icontour in 1:nrow(contours))
  {
    dsnDPE=    ifelse(is.na(contours$NOM[icontour])==F,
                      file.path(dsnlayer,contours$NOMPOST[icontour],contours$NOM[icontour],"POST"),
                      file.path(dsnlayer,contours$NOMPOST[icontour],"_Fusion")
    )
    
    for (idsnDPE in dsnDPE)
    {
      # for (nomCNIR in file.path(list.files(idsnDPE,"HWH_m_ZI")))
      for (nomCNIR in file.path(dsnDPE,list.files(idsnDPE,"ZI_Qgis_Final_Aint630_Aext1256.25")))
        
      {
        # nomCNIR="C:\\Cartino2D\\France\\CNIR_RGE_PluieNette\\_FUSION\\CNIR_RGE_PluieNettetT0100_D12_PIC08HWH_m_ZI0.1.gpkg"
        source(file.path(chem_routine,"PreC2D","PreC2D_7_0_PositionSectCont_Sandre.R"))
      }
    }
  }
}

################################################################################
############## 8. EN CONSTRUCTION REMODELAGE SECTEURS PLUIE ######################
################################################################################
if (nPreC2D[8]==1)
{
  
  nomCE="C:\\AFFAIRES\\EAIM\\EAIM_DEBITS.gpkg"
  Buf8_CE=50
  Buf8Cont_ic=2500
  Buf8Cont_ic_neg=-500
  
  contours=ChoixContours(dsnlayer,nomlayerC)
  
  contours=contours[order(contours$NOM),]
  dsnDPE=file.path(dsnlayer,contours$NOMPOST,contours$NOM)
  nomMNTprec=""
  
  # Buffer sur les cours d'eau
  nomCEBuf=file.path(dsnlayer,contours$NOMPOST[1],"CoursEau_Buf.gpkg")
  cmd=paste0(qgis_process, " run native:buffer",
             " --distance_units=meters --area_units=m2 --ellipsoid=EPSG:7019",
             " --INPUT=",nomCE,
             " --DISTANCE=",Buf8_CE,
             " --SEGMENTS=5 --END_CAP_STYLE=0 --JOIN_STYLE=0 --MITER_LIMIT=2 --DISSOLVE=True",
             " --OUTPUT=",nomCEBuf)
  print(cmd);system(cmd)
  faisle=0
  chemin=dirname(dsnDPE[1])
  if (faisle==1)
  {
    for (ic in 1:nrow(contours))
    {
      
      repPetBv=file.path(contours$NOM[ic],paste0(contours$NOM[ic],"_MNT2PourContour"),"500000")
      nomnewsecteur=file.path(chemin,repPetBv,"NewSecteur.gpkg")
      if (file.exists(nomnewsecteur)==F)
      {
        # browser()
        dir.create(dsnDPE[ic],recursive = T)
        contouric=contours[ic,]
        nomcontouric=file.path(dsnDPE[ic],"contour1.gpkg")
        st_write(contours[ic,],nomcontouric, delete_dsn = T,delete_layer = T, quiet = T)
        
        nomcontouricbuf=file.path(dsnDPE[ic],"contour2buf.gpkg")
        cmd=paste0(qgis_process, " run native:buffer",
                   " --distance_units=meters --area_units=m2 --ellipsoid=EPSG:7019",
                   " --INPUT=",nomcontouric,
                   " --DISTANCE=",Buf8Cont_ic,
                   " --SEGMENTS=5 --END_CAP_STYLE=0 --JOIN_STYLE=0 --MITER_LIMIT=2 --DISSOLVE=True",
                   " --OUTPUT=",nomcontouricbuf)
        print(cmd);system(cmd)
        
        nomcontouricbufcoupCE=file.path(dsnDPE[ic],"contour3bufcoupCE.gpkg")
        cmd <- paste0(qgis_process, " run native:difference",
                      " --INPUT=",nomcontouricbuf,
                      " --OVERLAY=",nomCEBuf,
                      " --OUTPUT=",nomcontouricbufcoupCE,
                      " --GRID_SIZE=None")
        print(cmd);system(cmd)
        contour_=st_cast(st_read(nomcontouricbufcoupCE),"POLYGON")
        contour_$AIRE=st_area(contour_)
        contour_=contour_[contour_$AIRE==max(contour_$AIRE),]
        
        nomcontouricbufcoupCEgrand=file.path(dsnDPE[ic],"contour4bufcoupCEgrand.gpkg")
        st_write(contour_,nomcontouricbufcoupCEgrand, delete_dsn = T,delete_layer = T, quiet = T)
        
        # Ouverture Table Assemblage Lidar
        if (contour_$MNT!=nomMNTprec)
        {
          dsnLidar = dirname(contour_$MNT)
          Lidar <- st_read(contour_$MNT)
          st_crs(Lidar) = st_crs(EPSG)
          nomMNTprec=contour_$MNT
        }
        
        # récupération des dalles concernées
        nb = st_intersects(Lidar, contour_)
        n_int = which(sapply(nb, length) > 0)
        LidarC = Lidar[n_int,]
        
        # Création de la liste des dalles concernées
        if(length(which(is.na(LidarC$DOSSIERASC)))==0){
          listeASC = paste0(dsnLidar, '/', LidarC$DOSSIERASC, '/', LidarC$NOM_ASC)
        }else{
          listeASC = paste0(dsnLidar, '/', LidarC$NOM_ASC)
        }
        
        
        # Creation du fichier virtuel
        nom_ascvrt = file.path(dsnLidar, "listepourvrt.txt")
        file.create(nom_ascvrt)
        write(listeASC, file = nom_ascvrt, append = T)
        vrtfile = paste0(dsnLidar, "\\", contour_$NOM, ".vrt") ##chemin du vrt à créer
        cmd = paste(shQuote(OSGeo4W_path),"gdalbuildvrt",vrtfile,"-input_file_list",nom_ascvrt)
        print(cmd);system(cmd)
        unlink(nom_ascvrt)
        
        # 'E:/RGEAlti1m/France/_Reso025//C0161_187038km_X762384Y7024169.vrt'  --COPY_SUBDATASETS=false --OPTIONS= --EXTRA= --DATA_TYPE=0 --OUTPUT='C:/Cartino2D/France/EAIM/C0161_187038km_X762384Y7024169/ccc.tiff'
        riv_name_brut=file.path(dsnDPE[ic],paste0(contour_$NOM,"_MNT1PourContourbrut.tif"))
        cmd <- paste0(qgis_process, " run gdal:translate",
                      " --distance_units=meters --area_units=m2 --ellipsoid=EPSG:7019 ",
                      " --INPUT=",vrtfile,
                      " --TARGET_CRS=",'EPSG:2154',
                      " --COPY_SUBDATASETS=false --OPTIONS=",
                      " --EXTRA=",
                      " --DATA_TYPE=0",
                      " --OUTPUT=",riv_name_brut)
        print(cmd); system(cmd)
        
        # --ALPHA_BAND=false --CROP_TO_CUTLINE=true --KEEP_RESOLUTION=false --SET_RESOLUTION=false --MULTITHREADING=false --OPTIONS= --DATA_TYPE=0 --EXTRA= --OUTPUT=TEMPORARY_OUTPUT
        riv_name=file.path(dsnDPE[ic],paste0(contour_$NOM,"_MNT2PourContour.tif"))
        cmd <- paste0(qgis_process, " run gdal:cliprasterbymasklayer",
                      " --distance_units=meters --area_units=m2 --ellipsoid=EPSG:7019 ",
                      " --INPUT=",riv_name_brut,
                      " --MASK=",nomcontouricbufcoupCEgrand,
                      " --ALPHA_BAND=false --CROP_TO_CUTLINE=true --KEEP_RESOLUTION=TRUE",
                      " --SET_RESOLUTION=false",
                      " --MULTITHREADING=false --OPTIONS= --DATA_TYPE=0 --EXTRA= ",
                      " --OUTPUT=",riv_name)
        print(cmd); system(cmd)
        
        nomMNTinfo=file.path(dsnDPE[ic],paste0(contour_$NOM,"_MNTPourContourinfo.txt"))
        #### gestion pour voir si le raster est bien aligner sur des 1000/2000/3000...
        # standard IGN génant
        cmd <- paste0(qgis_process, " run gdal:gdalinfo",
                      " --distance_units=meters --area_units=m2 --ellipsoid=EPSG:7019 ",
                      " --INPUT=",riv_name,
                      " --MIN_MAX=false --STATS=true --NOGCP=false --NO_METADATA=false --EXTRA=",
                      " --OUTPUT=",nomMNTinfo)
        print(cmd); system(cmd)         
        
        # Lire le contenu du fichier
        file_content <- readLines(nomMNTinfo)
        # Extraire les valeurs des coins
        UL     <- grep("Upper Left", file_content, value = TRUE)
        LR     <- grep("Lower Right", file_content, value = TRUE)
        Taille <- grep("Size ", file_content, value = TRUE)
        
        ULLR=function(text)
          #Mistral
        {
          # Trouver les positions des parenthèses
          paren_positions <- regexpr("\\(", text)
          # Extraire les coordonnées des coins
          coords <- regmatches(text, gregexpr("\\d+\\.\\d+", text))
          # Convertir les valeurs en numérique
          coords <- as.numeric(unlist(coords))
          # Afficher les coordonnées des coins
          return(coords[1:2])
        }  
        
        LimXY=c(ULLR(UL),ULLR(LR))
        if (max(LimXY-25*round(LimXY/25))==0)
        {
          
          PreC2D_03_TauDEM=1
          
          setwd(dir=chemin)
          ## NE PAS MODIFIER ------------------------------------------------------------
          ResoMNTpourTaudem=25
          dir  = basename(dsnDPE[ic])
          dir_tau_riv = basename(dsnDPE[ic])
          riv_name=paste0(contour_$NOM,"_MNT2PourContour")
          format_MNT=".tif"
          src_thres=rbind(c(10,5,2,1,0.5)*1e6,matrix(ResoMNTpourTaudem,1,5))
          src_thres=rbind(c(10,0.5)*1e6,matrix(ResoMNTpourTaudem,1,2))
          nb_riv=1
          dirProjQgz=file.path(chem_routine,"C2D",Doss_Qml)
          
          source(file.path(chem_routine,"PreC2D","PreC2D_2_4_TauDEM.R"))
          
          nomstreamnet=file.path(chemin,repPetBv,"streamnet.shp")
          nomstreamnet2=file.path(dsnDPE[ic],"streamnet2.gpkg")
          cmd <- paste0(qgis_process, " run native:difference",
                        "  --distance_units=meters --area_units=m2 --ellipsoid=EPSG:7019",
                        " --INPUT=",nomstreamnet,
                        " --OVERLAY=",nomCEBuf,
                        " --OUTPUT=",nomstreamnet2,
                        " --GRID_SIZE=None")
          print(cmd);system(cmd)
          
          nombuf=file.path(dsnDPE[ic],"streamnet3Buf.gpkg")
          cmd=paste0(qgis_process, " run native:buffer",
                     " --distance_units=meters --area_units=m2 --ellipsoid=EPSG:7019",
                     " --INPUT=",nomstreamnet2,
                     " --DISTANCE=1 --SEGMENTS=5 --END_CAP_STYLE=0 --JOIN_STYLE=0 --MITER_LIMIT=2 --DISSOLVE=True",
                     " --OUTPUT=",shQuote(nombuf))
          print(cmd);system(cmd)
          
          nombufMP=file.path(dsnDPE[ic],"streamnet4BufMP.gpkg")
          cmd=paste0(qgis_process, " run native:multiparttosingleparts",
                     " --INPUT=",shQuote(nombuf),
                     " --OUTPUT=",shQuote(nombufMP))
          print(cmd);system(cmd)
          
          nomdedans=file.path(dsnDPE[ic],"Interieur.gpkg")
          nomdedans=file.path(dsnDPE[ic],"contour5Interieur.gpkg")
          cmd=paste0(qgis_process, " run native:buffer",
                     " --distance_units=meters --area_units=m2 --ellipsoid=EPSG:7019",
                     " --INPUT=",nomcontouric,
                     " --DISTANCE=",Buf8Cont_ic_neg,
                     " --SEGMENTS=5 --END_CAP_STYLE=0 --JOIN_STYLE=0 --MITER_LIMIT=2 --DISSOLVE=True",
                     " --OUTPUT=",nomdedans)
          print(cmd);system(cmd)
          
          nombufMP_aretenir=file.path(chemin,repPetBv,"streamnet5dedans.gpkg")
          cmd <- paste0(qgis_process, " run native:joinattributesbylocation",
                        " --distance_units=meters --area_units=m2 --ellipsoid=EPSG:7019",
                        " --INPUT=",nombufMP,
                        " --PREDICATE=0,5",
                        " --JOIN=",nomdedans,
                        " --METHOD=0 --DISCARD_NONMATCHING=true --PREFIX=",
                        " --OUTPUT=",nombufMP_aretenir
          )
          print(cmd);system(cmd)
          
          nomwatershed=file.path(chemin,repPetBv,"watershed.shp")
          nomBuf_pour_mer= "H:/FILINO_Travail/00_SIGBase/DEPARTEMENT_Buf_pour_mer.shp"
          nomwatershdcontinent=file.path(chemin,repPetBv,"watershed_cont.shp")
          
          cmd=paste0(shQuote(qgis_process),
                     " run native:clip", 
                     " --distance_units=meters",
                     "--area_units=m2",
                     " INPUT=", nomwatershed,
                     " OVERLAY=", nomBuf_pour_mer,
                     " OUTPUT=", nomwatershdcontinent)
          print(cmd); system(cmd)
          ####
          nomwatershed2=file.path(chemin,repPetBv,"watershed2.shp")
          cmd <- paste0(qgis_process, " run native:difference",
                        "  --distance_units=meters --area_units=m2 --ellipsoid=EPSG:7019",
                        " --INPUT=",nomwatershdcontinent,
                        " --OVERLAY=",nomCEBuf,
                        " --OUTPUT=",nomwatershed2,
                        " --GRID_SIZE=None")
          print(cmd);system(cmd)
          
          nombv_aretenir=file.path(chemin,repPetBv,"bv_dedans.gpkg")
          cmd <- paste0(qgis_process, " run native:joinattributesbylocation",
                        " --distance_units=meters --area_units=m2 --ellipsoid=EPSG:7019",
                        " --INPUT=",nomwatershed2,
                        " --PREDICATE=0,5",
                        " --JOIN=",nombufMP_aretenir,
                        " --METHOD=0 --DISCARD_NONMATCHING=true --PREFIX=",
                        " --OUTPUT=",nombv_aretenir)
          print(cmd);system(cmd)
          
          nombv_buf=file.path(chemin,repPetBv,"bv_dedans_buf.gpkg")
          cmd=paste0(qgis_process, " run native:buffer",
                     " --distance_units=meters --area_units=m2 --ellipsoid=EPSG:7019",
                     " --INPUT=",nombv_aretenir,
                     " --DISTANCE=0 --SEGMENTS=5 --END_CAP_STYLE=0 --JOIN_STYLE=0 --MITER_LIMIT=2 --DISSOLVE=True",
                     " --OUTPUT=",shQuote(nombv_buf))
          print(cmd);system(cmd)
          
          # Enlever les petits trous
          nombv_buf_plein=file.path(chemin,repPetBv,"NouveauContour.gpkg")
          cmd <- paste0(qgis_process, " run native:deleteholes",
                        " --INPUT=", shQuote(nombv_buf),
                        " --MIN_AREA=", 1000000000,
                        " --OUTPUT=", shQuote(nombv_buf_plein))
          print(cmd); system(cmd)
          
          ###################################################################
          #---- Essai de récupération des manques autour du nouveau contour et dans le contour initial
          # Buffer du contour initial pour croiser avec les bassins 
          trou_Buf_ContIni=250
          nomcontourictroubuf=file.path(dsnDPE[ic],"contour2buftrou.gpkg")
          cmd=paste0(qgis_process, " run native:buffer",
                     " --distance_units=meters --area_units=m2 --ellipsoid=EPSG:7019",
                     " --INPUT=",nomcontouric,
                     " --DISTANCE=",trou_Buf_ContIni,
                     " --SEGMENTS=5 --END_CAP_STYLE=0 --JOIN_STYLE=0 --MITER_LIMIT=2 --DISSOLVE=True",
                     " --OUTPUT=",nomcontourictroubuf)
          print(cmd);system(cmd)
          
          nomwatershed_join1=file.path(chemin,repPetBv,"JointureWS_ContIni.gpkg")
          cmd <- paste0(qgis_process, " run native:joinattributesbylocation",
                        " --distance_units=meters --area_units=m2 --ellipsoid=EPSG:7019",
                        " --INPUT=",      nomwatershed2,
                        " --PREDICATE=5",
                        " --JOIN=",nomcontourictroubuf,
                        " --METHOD=0 --DISCARD_NONMATCHING=true --PREFIX=",
                        " --OUTPUT=",nomwatershed_join1)
          print(cmd);system(cmd)
          
          nomwatershed_join2=file.path(chemin,repPetBv,"JointureWS_ContIni_Buf.gpkg")
          cmd=paste0(qgis_process, " run native:buffer",
                     " --distance_units=meters --area_units=m2 --ellipsoid=EPSG:7019",
                     " --INPUT=",nomwatershed_join1,
                     " --DISTANCE=0 --SEGMENTS=5 --END_CAP_STYLE=0 --JOIN_STYLE=0 --MITER_LIMIT=2 --DISSOLVE=True",
                     " --OUTPUT=",nomwatershed_join2)
          print(cmd);system(cmd)
          
          # Fusionner les polygones décalés avec les losanges à l'aide de QGIS
          nomnewsecteurt=file.path(chemin,repPetBv,"Secteurs_avec_trous.gpkg")
          cmd <- paste0(qgis_process, " run native:mergevectorlayers",
                        " --LAYERS=", nombv_buf,
                        " --LAYERS=", nomwatershed_join2,
                        " --CRS=QgsCoordinateReferenceSystem('EPSG:", EPSG, "') ",
                        " --OUTPUT=", nomnewsecteurt)
          print(cmd); system(cmd)
          
          nomnewsecteurtbuf=file.path(chemin,repPetBv,"Secteurs_avec_trous_buf.gpkg")
          cmd=paste0(qgis_process, " run native:buffer",
                     " --distance_units=meters --area_units=m2 --ellipsoid=EPSG:7019",
                     " --INPUT=",nomnewsecteurt,
                     " --DISTANCE=0 --SEGMENTS=5 --END_CAP_STYLE=0 --JOIN_STYLE=0 --MITER_LIMIT=2 --DISSOLVE=True",
                     " --OUTPUT=",nomnewsecteurtbuf)
          print(cmd);system(cmd)
          
          # Enlever les petits trous
          nomnewsecteurtbufsstrou=file.path(chemin,repPetBv,"Secteurs_sans_trous_buf.gpkg")
          cmd <- paste0(qgis_process, " run native:deleteholes",
                        " --INPUT=", nomnewsecteurtbuf,
                        " --MIN_AREA=", 1000000000,
                        " --OUTPUT=", nomnewsecteurtbufsstrou)
          print(cmd); system(cmd)
          
          # Garder la plus grande partie
          nomnewsecteur=file.path(chemin,repPetBv,"NewSecteur.gpkg")
          cmd <- paste0(qgis_process, " run native:keepnbiggestparts",
                        " --distance_units=meters --area_units=m2 --ellipsoid=EPSG:7019",
                        " --POLYGONS=",nomnewsecteurtbufsstrou,
                        " --PARTS=1",
                        " --OUTPUT=",nomnewsecteur)
          print(cmd); system(cmd)
          
        }else{
          # browser()
        }
      }
    }
  }
  browser()
  listNewSect=file.path(dsnDPE,paste0(contours$NOM,"_MNT2PourContour"),"500000","NewSecteur.gpkg")
  print(length(listNewSect))
  listNewSect=listNewSect[which(file.exists(listNewSect)==T)]
  print(length(listNewSect))
  # list.files(file.path(dsnlayer,contours$NOMPOST),"NewSecteur.gpkg",recursive=T)
  
  increment=40
  for (it in seq(1,length(listNewSect),increment))
  {
    NomNS_Fi=file.path(dirname(dsnDPE)[1],paste0("SecteurC2D_Modif",it,".gpkg"))
    cmd=paste0(qgis_process, " run native:mergevectorlayers")
    for (iNS in listNewSect[it:min(it+40-1,length(listNewSect))])
    {
      {
        cmd=paste0(cmd," --LAYERS=",shQuote(iNS))
      }
    }
    cmd=paste0(cmd,
               paste0(" --CRS=QgsCoordinateReferenceSystem('EPSG:",EPSG,"') "),
               " --OUTPUT=",shQuote(NomNS_Fi))
    print(cmd);system(cmd)
  }
  
  # Buffer sur les cours d'eau
  Buf8_CE2=75
  nomCEBuf2=file.path(dsnlayer,contours$NOMPOST[1],"CoursEau_Buf2.gpkg")
  cmd=paste0(qgis_process, " run native:buffer",
             " --distance_units=meters --area_units=m2 --ellipsoid=EPSG:7019",
             " --INPUT=",nomCEBuf,
             " --DISTANCE=",Buf8_CE2,
             " --SEGMENTS=5 --END_CAP_STYLE=0 --JOIN_STYLE=0 --MITER_LIMIT=2 --DISSOLVE=True",
             " --OUTPUT=",nomCEBuf2)
  print(cmd);system(cmd)
  
  
  cmd=paste0(qgis_process, " run native:mergevectorlayers")
  for (it in seq(1,length(listNewSect),increment))
  {
    NomNS_Fi=file.path(dirname(dsnDPE)[1],paste0("SecteurC2D_Modif",it,".gpkg"))
    cmd=paste0(cmd," --LAYERS=",shQuote(NomNS_Fi))
  }
  NomNS_F=file.path(dirname(dsnDPE)[1],"SecteurC2D_Modif_Fusion.gpkg")
  cmd=paste0(cmd,
             paste0(" --CRS=QgsCoordinateReferenceSystem('EPSG:",EPSG,"') "),
             " --OUTPUT=",shQuote(NomNS_F))
  print(cmd);system(cmd)
  
  cmd=paste0(qgis_process, " run native:mergevectorlayers")
  for (it in seq(1,length(listNewSect),increment))
  {
    NomNS_Fi=file.path(dirname(dsnDPE)[1],paste0("SecteurC2D_Modif",it,".gpkg"))
    cmd=paste0(cmd," --LAYERS=",shQuote(NomNS_Fi))
  }
  cmd=paste0(cmd," --LAYERS=",shQuote(nomCEBuf2))
  NomNS_F_et_CE=file.path(dirname(dsnDPE)[1],"SecteurC2D_Modif_Fusion_PlusCE.gpkg")
  cmd=paste0(cmd,
             paste0(" --CRS=QgsCoordinateReferenceSystem('EPSG:",EPSG,"') "),
             " --OUTPUT=",shQuote(NomNS_F_et_CE))
  print(cmd);system(cmd)
  
  NomNS_F_Buf=file.path(dirname(dsnDPE)[1],"SecteurC2D_Modif_Buffer.gpkg")
  cmd=paste0(qgis_process, " run native:buffer",
             " --distance_units=meters --area_units=m2 --ellipsoid=EPSG:7019",
             " --INPUT=",NomNS_F_et_CE,
             " --DISTANCE=0 --SEGMENTS=5 --END_CAP_STYLE=0 --JOIN_STYLE=0 --MITER_LIMIT=2 --DISSOLVE=True",
             " --OUTPUT=",NomNS_F_Buf)
  print(cmd);system(cmd)
  
  # Enlever les petits trous
  NomNS_F_Buf_Plein=file.path(dirname(dsnDPE)[1],"SecteurC2D_Modif_Buffer_Plein.gpkg")
  cmd <- paste0(qgis_process, " run native:deleteholes",
                " --INPUT=", NomNS_F_Buf,
                " --MIN_AREA=", 100000000000000,
                " --OUTPUT=", NomNS_F_Buf_Plein)
  print(cmd); system(cmd)
  
  NomNS_Vides=file.path(dirname(dsnDPE)[1],"SecteurC2D_Modif_Vides.gpkg")
  cmd <- paste0(qgis_process, " run native:difference",
                " --INPUT=",NomNS_F_Buf_Plein,
                " --OVERLAY=",NomNS_F_Buf,
                " --OUTPUT=",NomNS_Vides,
                " --GRID_SIZE=None")
  print(cmd);system(cmd)
  
  
  
  SecteurGRASS=paste0(dirname(SecteurGRASS_),format(Sys.time(),format="%Y%m%d_%H%M%S"),"/",basename(SecteurGRASS_))
  unlink(dirname(SecteurGRASS),recursive=TRUE)
  toto=system(paste0(BatGRASS," -c EPSG:",EPSG," ",dirname(SecteurGRASS)," --text"))
  if (toto!=0){cat("Vous avez un problème dans ",BatGRASS,"\n");BUGDEGRASS=BOOM}
  system(paste0(BatGRASS," -c ",SecteurGRASS," --text"))
  
  # importation vecteur dans grass grand contour
  nomg1="C2DSecteur"
  cmd=paste0("v.in.ogr -o --quiet --overwrite input=",NomNS_F," output=",nomg1)
  print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
  
  cmd=paste0("g.region --overwrite --quiet -a vector=",nomg1)
  print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
  
  NomNS_Fd=file.path(dirname(dsnDPE)[1],"SecteurC2D_Modif_Fusion_Doublons.gpkg")
  cmd=paste0("v.out.ogr --quiet --overwrite input=",nomg1," layer=2"," output=",NomNS_Fd," format=GPKG")
  print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
  
  # Manque par rapport aux contours initiaux
  NomS_ini=file.path(dirname(dsnDPE)[1],"SecteurIniFus.gpkg")
  st_write(contours,NomS_ini, delete_dsn = T,delete_layer = T, quiet = T)
  
  NomS_ini_Buf=file.path(dirname(dsnDPE)[1],"SecteurIniFus.gpkg_Buffer.gpkg")
  cmd=paste0(qgis_process, " run native:buffer",
             " --distance_units=meters --area_units=m2 --ellipsoid=EPSG:7019",
             " --INPUT=",NomS_ini,
             " --DISTANCE=0 --SEGMENTS=5 --END_CAP_STYLE=0 --JOIN_STYLE=0 --MITER_LIMIT=2 --DISSOLVE=True",
             " --OUTPUT=",NomS_ini_Buf)
  print(cmd);system(cmd)
  
  NomNS_Manques=file.path(dirname(dsnDPE)[1],"SecteurC2D_Modif_Manques.gpkg")
  cmd <- paste0(qgis_process, " run native:difference",
                " --INPUT=",NomS_ini_Buf,
                " --OVERLAY=",NomNS_F_Buf_Plein,
                " --OUTPUT=",NomNS_Manques,
                " --GRID_SIZE=None")
  print(cmd);system(cmd)         
  
  
  
  BufPM=100
  
  for (NomATraiter in cbind(NomNS_Vides,NomNS_Fd,NomNS_Manques))
  {
    nomBufNeg=paste0(substr(NomATraiter,1,nchar(NomATraiter)-5),"BufNeg",'.gpkg')
    cmd=paste0(qgis_process, " run native:buffer",
               " --distance_units=meters --area_units=m2 --ellipsoid=EPSG:7019",
               " --INPUT=",NomATraiter,
               " --DISTANCE=",-BufPM,
               " --SEGMENTS=5 --END_CAP_STYLE=0 --JOIN_STYLE=0 --MITER_LIMIT=2 --DISSOLVE=True",
               " --OUTPUT=",nomBufNeg)
    print(cmd);system(cmd)
    
    nomBufNegPos=paste0(substr(NomATraiter,1,nchar(NomATraiter)-5),"BufNegPos",'.gpkg')
    cmd=paste0(qgis_process, " run native:buffer",
               " --distance_units=meters --area_units=m2 --ellipsoid=EPSG:7019",
               " --INPUT=",nomBufNeg,
               " --DISTANCE=",BufPM*1.2,
               " --SEGMENTS=5 --END_CAP_STYLE=0 --JOIN_STYLE=0 --MITER_LIMIT=2 --DISSOLVE=True",
               " --OUTPUT=",nomBufNegPos)
    print(cmd);system(cmd)
    
    nomBufNegPosInt=paste0(substr(NomATraiter,1,nchar(NomATraiter)-5),"BufNegPosInter",'.gpkg')
    cmd <- paste0(qgis_process, " run native:intersection",
                  " --distance_units=meters --area_units=m2 --ellipsoid=EPSG:7019",
                  " --INPUT=",NomATraiter,
                  " --OVERLAY=",nomBufNegPos,
                  "  --OVERLAY_FIELDS_PREFIX=",
                  " --OUTPUT=",nomBufNegPosInt)
    print(cmd); system(cmd)
    
    nomBufNegPosIntFin=paste0(substr(NomATraiter,1,nchar(NomATraiter)-5),"BufNegPosInter_Final",'.gpkg')
    cmd <- paste0(qgis_process, " run native:multiparttosingleparts",
                  " --INPUT=",nomBufNegPosInt,
                  " --OUTPUT=",nomBufNegPosIntFin)
    print(cmd);system(cmd)
  }
  
  # Fusionner les polygones décalés avec les losanges à l'aide de QGIS
  
  nomFMV=file.path(dirname(dsnDPE)[1],"Fusion_Manques_Vides.gpkg")
  cmd <- paste0(qgis_process, " run native:mergevectorlayers",
                " --LAYERS=",  paste0(substr(NomNS_Vides,1,nchar(NomNS_Vides)-5),"BufNegPosInter_Final",'.gpkg'),
                " --LAYERS=",  paste0(substr(NomNS_Manques,1,nchar(NomNS_Manques)-5),"BufNegPosInter_Final",'.gpkg'),
                " --LAYERS=", NomNS_F,
                " --CRS=QgsCoordinateReferenceSystem('EPSG:", EPSG, "') ",
                " --OUTPUT=", nomFMV)
  print(cmd); system(cmd)
  
  # importation vecteur dans grass grand contour
  nomg2="C2DSecteur_M_V"
  cmd=paste0("v.in.ogr -o --quiet --overwrite input=",nomFMV," output=",nomg2)
  print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
  
  nomg3="C2DSecteur_M_V_rmarea"
  seuilMV=10000000
  cmd=paste0("v.clean --quiet --overwrite input=",nomg2," output=",nomg3," type=area tool=rmarea threshold=",seuilMV)
  print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
  
  NomNS_Fd=file.path(dirname(dsnDPE)[1],"SecteurC2D_Modif_Fusion_Rempli.gpkg")
  cmd=paste0("v.out.ogr -m --quiet --overwrite input=",nomg3," output=",NomNS_Fd," format=GPKG")
  print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
  
  NomNS_FdBuf=file.path(dirname(dsnDPE)[1],"SecteurC2D_Modif_Fusion_Rempli_Buf.gpkg")
  cmd=paste0(qgis_process, " run native:buffer",
             " --distance_units=meters --area_units=m2 --ellipsoid=EPSG:7019",
             " --INPUT=",NomNS_Fd,
             " --DISTANCE=",0,
             " --SEGMENTS=5 --END_CAP_STYLE=0 --JOIN_STYLE=0 --MITER_LIMIT=2 --DISSOLVE=false",
             " --OUTPUT=",NomNS_FdBuf)
  print(cmd);system(cmd)
  
  
  # Enlever les petits trous
  NomNS_FdBufsstrou=file.path(dirname(dsnDPE)[1],"SecteurC2D_Modif_Fusion_Rempli_Buf_sstrou.gpkg")
  cmd <- paste0(qgis_process, " run native:deleteholes",
                " --INPUT=", shQuote(NomNS_FdBuf),
                " --MIN_AREA=", 1000000000,
                " --OUTPUT=", shQuote(NomNS_FdBufsstrou))
  print(cmd); system(cmd)
}
