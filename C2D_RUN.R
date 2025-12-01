# Ce temps de pose peut permettre de lancer des calculs Telemac en espérant qu'ils finissent dans 4h,
# d'ouvrir un autre Rstudio et de lancer avec un décalage de 4h les étapes suivantes
# Efficace ☻ le Week-end où on ne veut pas ouvrir son PC...

tpsPause=3600*0 # TRES TRES EXPERT
Sys.unsetenv("PROJ_LIB") # Suppression uniquement dans R d'une variable environnement associée à PostGre qui posait des problème de projection dans R
library(doParallel)
library(dplyr)
library(foreach)
library(ggplot2)
library(htmlwidgets)
library(mapview)
library(plotly)
library(raster)
library(RCurl)
library(readxl)
library(rvest)
library(sf) # si vous avez des problème avec st_crs et PROJ.LIB, merci de réinstaller le package sf après avoir relancer la 1ère ligne
library(sfheaders)
library(ssh)
library(tidyverse)
library(utils)
library(jsonlite)

cat("\014") # Nettoyage de la console

chem_routine=dirname(rstudioapi::getActiveDocumentContext()$path)

cat("##################### C2D A LIRE SVP ##############################\n")
cat("SI VOUS AVEZ CETTE ERREUR AU DESSOUS\n")
cat("Erreur dans file(filename, r, encoding = encoding) : \n")
cat("impossible d'ouvrir la connexion\n")
cat("De plus : Message davis :\n")
cat("Dans file(filename, r, encoding = encoding) :\n")
cat("impossible d ouvrir le fichier /C2D_ParamUser/C2D_LienOutilsPC.R : No such file or directory\n")
cat("\n")
cat("===> RELANCER             'Source'           de RStudio\n")
cat("##################### C2D Fin ##############################\n")
source(file.path(chem_routine,"C2D_ParamUser","C2D_LienOutilsPC.R"), encoding="utf-8")
cat("\014") # Nettoyage de la console
source(file.path(chem_routine,"C2D_ParamUser","C2D_CodeCalcuDistant.R"), encoding="utf-8")


fun_check_exist =                                                                           
  function(dir,stopoupas) if(file.exists(dir)==F) 
  {
    cat("###################################################################################\n")
    cat("######################### C2D A LIRE SVP ###########################################\n")
    cat("REPERTOIRE OU FICHIER ",dir, " INTROUVABLE\n")
    cat("Pour savoir où ce trouve ce fichier ou répertoire, dans R 'Ctrl Shift F' et copier le nom qui n'est pas trouvé\n")
    cat("######################### Fin C2D A LIRE ###########################################\n")
    if (stopoupas==1){break}
  }

fun_check_exist(Bug_GMSH,1)
fun_check_exist(OSGeo4W_path,1)
fun_check_exist(qgis_process,1)
fun_check_exist(chemin_pputils,1)
fun_check_exist(telemac_folder,1)
fun_check_exist(BatGRASS,1)
fun_check_exist(pscp_path,0)

ChoixETAPE = cbind(
  "00 Creation des secteurs Cartino2D",
  "__ NEW PreC2D, Prépa Données entrées avant/pendant ou après C2D",
  "01 Traitements MNT et autre pour maillage",
  "02 Maillage destructuré/reglé et création Selafin",
  "03 Conditions limites",
  "04 Fichier cas T2D/ hyetogrammes/Sources liquides",
  "05 Calculs Telemac local/distant et transferts",
  "06 (ancien 6 et 8) Post-traitement graphiques des résultats T2D: Sections Controle / OH / Sorties",
  # "07 OLD Suivi de l avancement du traitement geo, slf, res (peu utilisé)",
  # "08 A METTRE DANS ETAPE 6 comme 14 et 15",
  "07 (ancien 09) Postraitement SIG avec Qgis (option pputils en dur au besoin)",
  "08 'ancien 10) Postraitement SIG ASC=>GPKG zone valide", #on pourrait fusionner étape 9 et 10
  # "11 OLD Verif fichier cas / fichier res (peu utilisé)",
  "09 (ancien 12) Fusion générale des résultats SIG de chaque secteurs par scénarios",
  "10 EN CONSTRUCTION Evaluation (CSI, Stat, VIDEOS)"
  
  # "13 OLD Zip des ASC (peu utilisé)",
  # "14 OLD Post des sections de Controle (sans routine PC)",
  # "15 OLD Post des sections de Controle (avec routine PC module debit/niveau uniquement Telemac standard)",
  # "16 OLD éééé associé é 15é",
  # "17 OLD A intégrer proprement Copie des Fichiers nécessaires é Telemac pour Centre calcul (attention é enlever un .f",
  # "18 OLD Script GRETEL pour fusionner les résultats Centre calcul"
)

cat("\n")
cat("\n")
cat("########################################################################################################\n")
cat("######################### C2D A LIRE SVP ###############################################################\n")
cat("---------------- PREALABLE -----------------------------------------------------------------------------\n")
cat("Tout fonctionne en système WINDOWS\n")
cat("Vous devez installer tous les outils qui sont listés dans le fichier\n")
cat(file.path(chem_routine,"C2D_ParamUser","C2D_LienOutilsPC.R"),"\n")
cat("Il est fortement conseillé de les installer dans des emplacements identiques,\n")
cat("et SURTOUT d'éviter tout caractères spéciaux et espaces dans les noms des dossiers\n")
cat("Pour ceux ne disposant pas des droits administrateurs,\n")
cat("vous pouvez essayer de mettre votre dossier codes 'Cerema' sur un autre disque\n")
cat("---------------- DEMARRAGE -----------------------------------------------------------------------------\n")
cat("Pour lancer l'étape 0 de Cartino2D, vous devez disposer de:\n")
cat("1/ Un fichier vecteur SIG avec des géométries de secteurs de calculs\n")
cat("1.a/ De type bassins versants pour des forçages Pluie\n")
cat("1.b/ De type fonds de vallée pour des forçages Débit\n")
cat("2/ Vous devez aussi renseigner un 1er fichier de paramètres dans le dossier\n")
cat(file.path(chem_routine,"C2D_ParamUser"),"avec un début de nom de fichier commençant par 'C2D_00_Secteur_Xxxx.R'\n")
cat("A vous de changer Xxxx par le nom de votre choix\n")
cat("Les paramètres à intégrer dans ce fichier sont expliqués, merci de prendre exemple sur un fichier existant\n")
cat("Un nouveau fichier SIG d'entrée pour les étapes suivantes de Cartino ou des besoins de PréTraitement sont ainsi créés\n")
cat("OBJECTIF DE CETTE ETAPE 0: créer un fichier SIG avec une table attributaire qui sera la base de pilotage des étapes ultérieures\n")
cat("-------------\n")
cat("Nous ne sommes pas informaticiens, si vous annulez, ça va planter pour sortir, touche ECHAP-ESC de votre clavier et-ou Clavier Entrée dans la console R\n")
cat("Bon courage, PATIENCE et 'Stay Focus'\n")
cat("Il est conseillé d'enlever les notifications et données mobiles de son portable\n")
cat("######################### Fin C2D A LIRE ###############################################################\n")

Choixcalcul = cbind(
  "a. Calcul PC (création fichier batch pour chaque calcul)",
  "b. Calcul PC parallèle (bien choisir le nombre de processeurs)",
  "c. Calcul DTecRem",
  "d. Téléchargement résultats DTecRem",
  # "e. Inutile",
  "f. Calcul IFREMER",
  "g. Téléchargement résultats IFREMER",
  "h. Calcul GENCI",
  "i. Téléchargement résultats GENCI"
)

Choixnbprocess = cbind("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","24","30","40","48","72","96","108","120","140","196")

ETAPE = cbind(0,0,1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0) # 1

ETPCALCUL = c(0,0,1,0,0,0,0,0)

NBPROCESS = c(0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
par(cex =2)
nchoix = select.list(
  ChoixETAPE,
  preselect = ChoixETAPE[which(ETAPE == 1)],
  title = "Choisir les étapes à effectuer",
  multiple = T,
  graphics = T
)
ETAPE = 0 * ETAPE
nlala = which(ChoixETAPE %in% nchoix)
ETAPE[nlala] = 1

if (ETAPE[1]==1)
{
  #Travail particulier pour la création des secteurs
  source(file.path(chem_routine,"C2D","Cartino2D_Etape00_CreationSecteur.R"), encoding="utf-8")
  cat("###################################################################################\n")
  cat("######################### C2D A LIRE SVP ###########################################\n")
  cat("ETAPE 0 se lance seule, les autres étapes sont ignorées\n")
  cat("######################### Fin C2D A LIRE ###########################################\n")
}else{
  
  listSect=list.files(file.path(chem_routine,"C2D_ParamUser"), pattern="C2D_ParamUtilisateur")
  if (length(listSect)>1)
  {
    nchoixZS = select.list(
      listSect,
      title = "Choix de la zone des secteurs à traiter",
      multiple = F,
      graphics = T
    )
    nlal = which(listSect %in% nchoixZS)
    listSect=listSect[nlal]
  }
  source(file.path(chem_routine,"C2D_ParamUser",listSect), encoding="utf-8")
  if (file.exists(SecteurGRASS_)==F){dir.create(dirname(dirname(SecteurGRASS_)))}
  #Vérification de la présence des fichiers ou répertoires
  fun_check_exist(file.path(dsnlayer),1)
  fun_check_exist(file.path(chem_routine,"C2D",Doss_Base),1)
  fun_check_exist(file.path(chem_routine,"C2D",Doss_Qml),1)
  fun_check_exist(dsnPluie,0)
  
  if ( ETAPE[2]==1)
  {
    # PreC2D
    source(file.path(chem_routine,"PreC2D","C2D_Pre_RUN.R"), encoding="utf-8")
  }else{
    
    ETAPE=ETAPE[-c(1,2)]
    
    ETAPE=as.matrix(c(ETAPE[1:6],0,ETAPE[6],ETAPE[7:8],0,ETAPE[9],ETAPE[10]))

    fun_check_exist(file.path(dsnlayer,nomlayerC))
    if (ETAPE[5]==1){
      nchoixcalcul = select.list(
        Choixcalcul,
        preselect = Choixcalcul[which(ETPCALCUL == 1)],
        title = "Choix du mode de calcul",
        multiple = T,
        graphics = T
      )
      ETPCALCUL = 0 * ETPCALCUL
      nlala = which(Choixcalcul %in% nchoixcalcul)
      ETPCALCUL[nlala] = 1
      
      ETPCALCUL=c(ETPCALCUL[1:4],0,ETPCALCUL[5:8])

    }else{
      ETPCALCUL = c(0,0,0,0,0,0,0,0,0)
    }
    
    if(ETPCALCUL[2]==1){
      nChoixnbprocess = select.list(
        Choixnbprocess,
        preselect = Choixnbprocess[which(NBPROCESS  == 1)],
        title = "Choix du nombre de processeurs",
        multiple = F,
        graphics = T
      )
      NBPROCESS  = 0 * NBPROCESS
      nb_process = Choixnbprocess[which(Choixnbprocess %in% nChoixnbprocess)]
      cat("NBprocess=",nb_process,"\n")
    }
    
    # ---------------------------------
    #   v     variable        unit
    # ---------------------------------
    # 0 --> VELOCITY U       [M/S]
    # 1 --> VELOCITY V       [M/S]
    # 2 --> WATER DEPTH      [M]
    # 3 --> BOTTOM           [M]
    # 4 --> FROUDE NUMBER    []
    # 5 --> SCALAR FLOWRATE  [M2/S]
    # 6 --> SCALAR VELOCITY  [M/S]
    # 7 --> ACC. RUNOFF      [M]
    # 8 --> HIGH WATER MARK  [M]
    # 9 --> HIGH WATER TIME  [S]
    # 10 --> HIGHEST VELOCIT  [M/S]
    # 11 --> MAX SCA FLOWRAT  [M2/S]
    
    Rel_Param_ext=rbind(
      cbind("Bottom_m","BOTTOM          ","BOTTOM          "),
      cbind("HWM_m"   ,"HIGH WATER MARK ","HIGH WATER MARK "),
      cbind("HWT_s"   ,"HIGH WATER TIME ","HIGH WATER TIME "),
      cbind("HV_m_s"  ,"HIGHEST VELOCIT ","HIGHEST VELOCITY"),
      cbind("HSF_m2_s","MAX SCA FLOWRAT ","MAX SCA FLOWRATE"),
      cbind("HWH_m"   ,"WATER DEPTH     ","WATER DEPTH     "),
      cbind("Fr"      ,"FROUDE NUMBER   ","FROUDE NUMBER   "),
      cbind("HSF_m2_s","SCALAR FLOWRATE ","SCALAR FLOWRATE "),
      cbind("HV_m_s"  ,"SCALAR VELOCITY ","SCALAR VELOCITY ")
    )
    
    # Rel_Param_ext=rbind(
    #         cbind("HWT25m_s"   ,"HIGH WATER TIME ","HIGH WATER TIME ")
    # )
    
    ##### Lancement du calcul
    source(file.path(chem_routine,"C2D\\Cartino2D__Pilotage.R"))
    
    setwd(dsnlayer)
    # Ce serait bien d'avoir dsnlayer cheminchem Doss... avec une même logique mais le mieux est l'ennemi du bien... pour l'instant
  }
}

