# Dossier de travail
dsnlayer = "C:\\Cartino2D_Github\\France"  ## dossier où se trouve le fichier secteurs
# Nom des secteurs à traiter
nomlayerC= "Cartino2D_Secteur_20251121_163806.gpkg"
# code EPSG du projet
EPSG = 2154
# Nom du secteur GRASS
SecteurGRASS_="C:/GRASSDATA/C2D/Temp"
NProcGrass=1 # Laisser à 1
# Option pour faire un cas avec un CN 100
CN_100 = FALSE # Ancienne option, ne pas toucher
newmethodsc=TRUE # méthode de calculs des débits aux sections de controle
# Dossier d'accès aux données de pluie
dsnPluie = "C:\\Cartino2D_Github\\France\\_Pluie"
# Parallelisation
nb_proc_pre    <- 1 # C2D pré-traitement
nb_proc_post   <- 1 # C2D post-traitement
nb_proc_preC2D <- 1 # PreC2D

################################################################################
#---- "07 (ancien 09) Postraitement SIG avec Qgis (option pputils en dur au besoin)",
################################################################################
# Option qgis plus rapide mais parfois mauvais
# quand des résultats 2014-10-07 02:59:59 au lieu de 2014-10-07 00:00:00
choixPPutilsQgis= 1 # pputils
# choixPPutilsQgis= 2 # qgis

################################################################################
# 09 (ancien 12) Fusion générale des résultats SIG de chaque secteurs par scénarios ###
################################################################################
Scena = cbind("SceMaxMedContFrance_","SceMaxMedEextContFrance_",
              "Sce20140929_Frej_","Sce20140929Emoy_Frej_",
              "Sce20141006_"  ,"Sce20141006Emoy_",
              "PR20_D12",
              "PR100_D12",
              "Evts20140929 AMC1","Evts20140929 AMC2",
              "Evts20141006 AMC1","Evts20141006 AMC2",
              "Evts20150823 AMC1","Evts20150823 AMC2",
              "P100_SEPIA_MAMP_AMC2","P030_SEPIA_MAMP_AMC2",
              "P010_SEPIA_MAMP_AMC2","P010_SEPIA_MAMP_AMC3",
              "P100_MILLES_MAMP_AMC2","P100_AIX_MAMP_AMC2",
              "SceMaxMedContFrance_AMC2",
              "T0100_D12_PIC06"
)

###############################################################################
##### EXPERT => Loi d'obturation des ouvrages hydrauliques en fonction du scénario
# Relation entre le scénario et le type d'évènement à prendre en compte pour la modification des capacités des ouvrages
RelSce_OHObstrue=rbind(
  cbind("SceMaxMed","Eext"),
  cbind("Sce20141006","Emoy"),
  cbind("Sce20140929","Emoy")
)

# Paramètre pour modifier les capacités des ouvrages
nomxlsGrilleOHobstrue=file.path(chem_routine,"C2D","GrilleOHobstrue.xlsx")


################################################################################
############## PreC2D_general ##################################################
################################################################################
chemin7z="C:/Program Files/7-Zip/7z.exe" # installer le logiciel 7z (si vous voulez charger des données)

SecteurGRASS_="C:/GRASSDATA/PreC2D/Temp"

chem_filino=file.path(chem_routine,"FILINO\\filino\\RFILES")

# Couche des départements français (hexagone)
nomDpt="H:\\FILINO_Travail\\00_SIGBase\\DEPARTEMENT.shp"

# Il est possible de rentrer un masque pour ne traiter qu'une portion de nos calculs
nommasque="" # si vide, pas d'option, quelques erreurs écrites sans problème
# Utile pour nettoyer des bords de mer et ne pas fusionner tous les mini-bv.

# Résolution du MNT pour Taudem
ResoMNTpourTaudem=5

################################################################################
############## PreC2D_1_1_ChargeBDDVecteurs.R ###########################################
################################################################################
# BDtopo
urlBDTopo <- "https://geoservices.ign.fr/bdtopo"
dsnDptBDTopo="G:\\BDTopo"

#RGEAlti
urlRGEAlti <- "https://geoservices.ign.fr/rgealti"
dsnRGEAlti="E:\\RGEAlti1m\\France"
nomTARGEAlti="TA_RGEALTI_FR.shp"
paramRGEAlti=cbind(0,0,13,16,18,21)

#OCS_GE
dsnDptOCS_GE="G:\\OCS_GE" # Repertoire de l'OCS_GE
urlOCSGE <- "https://geoservices.ign.fr/ocsge"

#BDRC PHE
dsnPHE="G:\\BDRC_PHE"
nomPHE="BNDRC_PHE.gpkg"
nomPHE="export_activite_inondation.gpkg"

# Table d'assemblage
dsnTAmano="C:\\Cartino2D_Github\\France\\_MNT\\MTP_CLL_2024"
nomTAmano="TA_MTP_CLL.gpkg"
paramTAmano=cbind(0,0,13,16,18,21)
extTAmano=".gpkg$"

################################################################################
############## PreC2D_2_1_Zone_DPE.R ###########################################
################################################################################

# paramétrage des seuils pour hauter/débit linéique/temps
HSF_mini=0.05 #0.1
HWH_mini=0.1 #0.2
TpicPluie_h=6 # en heure
DecalPicH_min=45 #25 # en minutes A AJUSTER EN FONCTION DE LA PUISSANCE DE VOTRE PLUIE
DeltaPicH_PluieDebit=5 # en minutes
Extens=rbind(cbind("HSF","HWH","HWT"),
             cbind("_m2_s","_m","_s"),
             cbind(HSF_mini,HWH_mini,(TpicPluie_h+DecalPicH_min/60)*3600))
# Résolution, moitité resolution de base
resolution=1
buf=1.1*resolution/2
# suppression des bassins versants mini
seuilSup0=100000

################################################################################
############## PreC2D_2_2_DetectPONTS_OH.R #####################################
################################################################################
# --------------- OH locaux ----------------------------------------------------
nomOHlocaux="C:\\Cartino2D\\France\\_OuvragesHydrauliques\\OH_MTPCLL20220316.shp"
BufOHlocaux=5
# --------------- Ponts Isidor ---------------------------------------------------
nomPontRRNc ="G:\\BDD_Vecteurs\\PONTS\\IsidorV3\\PONTS_RRNc\\a616a8d7-2485-48f6-859a-28d49769aa65_pontsconcedes_2023-01-01.shp"
nomPontRRNnc="G:\\BDD_Vecteurs\\PONTS\\IsidorV3\\PONTS_RRNnc\\01a1d223-9239-4f3c-9cdc-f737d9d505c9_ponts_2019-01-01.shp"
FactMultIsidor=1.75
# --------------- Ponts Programme National Cerema ------------------------------
nomPontPNP ="G:\\BDD_Vecteurs\\PONTS\\PNP\\PNP-donnees-publiques-20231001.gpkg"
FactMultPNP=1.75
# --------------- BDTopo -------------------------------------------------------
# 1 croisement des troncons linéaire dessus dessous et création d'une surface
FactMultiBDTopo=3 # agrandissement des dimensions dans les bases BDTopo
# 2 buffer autour des constructions linéaire
BufConstLi=15 # Buffer de 10m autour des constructions linéaires
# 3 buffer autour des constructions surfacique
BufConstSurf=10 # Buffer de 10m autour des constructions linéaires
# chemin d'export
dsnexport="C:\\AFFAIRES\\GT_LidarIGN\\Ponts_et_RetabliHydrau"

################################################################################
############## PreC2D_2_3_MNT_Endoreismes_ZPP ##################################
################################################################################
#------------ Table d'assemblage du MNT
dsnLidar="H:\\FILINO_Travail\\06_MNTTIN_FILINO" 
nomTA="TA_TIN_Filino.shp"
# Suppression des points à une altitude supérieure au point bas + DeltadessusMin
DeltadessusMin=1

################################################################################
############## PreC2D_2_4_TauDEM.R ##################################
################################################################################
nproc = 12
DINF = FALSE
seuil1 = 1000

################################################################################
############## PreC2D_2_5_Secto_Pluie.R ##################################
################################################################################
buf=1.1*resolution/2
# suppression des morceaux de écoulement directement sur la couche écoulement
seuilSup1=10000;units(seuilSup1)="m^2"
# suppression des mini-morceaux de écoulement qui coupe les bv
seuilSup2=10000;units(seuilSup2)="m^2"
# suppression des mini-bassins versants
seuilSup3=10000;units(seuilSup3)="m^2"
Bufferflash=20
BufferDepasse=50
# Bassin versant d'au moins 2km²
SeuilBVMin_pourC2D=2;units(SeuilBVMin_pourC2D)="m^2"

################################################################################
############## PreC2D_2_6_Secto_Pluie.R ##################################
################################################################################
SeuilDistPHE=c(0.1,7,14,25,50,100,10000)
  
################################################################################
############## PreC2D_3_0_StricklerCN ##################################
################################################################################
ResoStrCN=c(1,5,25)
# Dossier de travail
dsnlayerStriCN = "MTP20241015" #Si ce n'est pas un chemin complet (recommandé),
# il mettra les données par défaut dans un répertoire de dsnlayer
# Vous pouvez aussi imposer une sortie comme C:\\Cartino2D\\France\\_Strickler\\P4UFOCS"
# fichier de relation CN/Strickler/BDD (voir exemples)
nomXLS = file.path(chem_routine,"PreC2D","Relation_CN_Str_20241015_MTP.xlsx")
# Nom des colonnes à exporter dans le fichier xls
nom_Param=cbind("Strickler")#,"CN_B","CN_C")

################################################################################
############## PreC2D_4_1_RexPluie_radar.R #####################################
################################################################################
nb_h_avant <- -1
nb_h_apres <- 124
cumul_min <- 0 ### en mm
do_stat = TRUE
calculcumul=TRUE
nb_proc = 1
calculcumulsp <- TRUE
### Paramètres pour SHYREG, inutile si que TypPluie Stat
nduree=cbind(5,15,30,60,2*60,3*60,4*60,6*60,12*60,24*60,48*60,72*60)
ncumul <- c(1/4,1/2,1,2,3,4,6,12,24,48,72)
ncumulnoms <- c("15min","30min","1h","2h","3h","4h","6h","12h","24h","48h","72h")
minmm <- 0 ## provisoire : pour vérifier si nous avons une valeur de pluie dépassant une 
##              certaine valeur choisie 
NPerRet=cbind(2,5,10,20,50,100,500,1000)
DateDebut=201410050015  # 2021 Durée exacte
DateFin=  201410080000
use_debu_fin=FALSE
AMC = 2

################################################################################
######################### PreC2D_4_3_SHYREG_Pluie ##############################
################################################################################

duree <- 12  # Durée totale en heures
nomdpt=cbind("11")#,"13","30","34","66","74","76")
tpic <- 6 # Heure du pic de pluie
Periode_retour <- c(20,100) #c(100,500) Période de retour ==> 1000, 500, 100, 50, 20, 10, 5, 2
# Données pluie shyreg brute en dessous
ras_min <- "H:/Pluie/SHYREG/SHYREGPluieBrute/ras_P15-60mm_SHYREG_2018_France_L93_deterministe.RData" # Fichier .RData contenant les données Shyreg infra-horaire (15 à 60 min)
ras_h <- "H:/Pluie/SHYREG/SHYREGPluieBrute/ras_Ph_SHYREG_2018_France_L93_deterministe.Rdata" # Fichier .RData contenant les données Shyreg horaires (1 à 72 h)
# Données pluie shyreg nette en dessous (réservé Cerema Méditerranée)
ras_Pn_DIR="H:/Pluie/SHYREG/SHYREGPluieNette"
FournitureINRAEDptouSecteur=0 #
ContourPN_INRAE="D:\\SHYREG_Pluie_Nette\\Contour_zones_BNBV.Rdata"


################################################################################
###################### PreC2D_3_4_ReseauSHYREG_Debits ##########################
################################################################################

dsnlayerQ="D:\\Shyreg_Debit" # Lien vers le dossier contenant les données SHYREG de débits de pointe sur la France
nomZoneCEB="C:\\Cartino2D\\France\\SecteurCoursEau_Debit.gpkg" # LIen vers les Secteurs débits formés à l'étape PreC2D_1_1_Zone_DPE
TA_Lidar="D:\\RGEAltiIGN\\TA_RGEAlti.shp" # Lien vers la table d'assemblage du RGEAlti
Periode_retour_debit <- 100 # Période de retour ==> 100, 500, 1000
RValMin=20 # Seuil pour garder des valeurs superieurs à une surface drainée ou débit shyreg - Débit de pointe minimum
distBufS=90 # Distance en m sur laquelle est recherchée le point le plus bas où seront positionnés les points sources - 50 avec Exzeco
intervPts=12 # Intervalle auquel un point est défini pour les petits segments (segments < 2*resolution). Fixé à 12 pour une résolution de 50m
dsegm=80 # Fixé à 80 pour une résolution de 50m

################################################################################
###################### PreC2D_5_0_Formatage_Obs ################################
################################################################################
#En construction

################################################################################
################## PreC2D_7_0_PositionSectCont_Sandre ##########################
################################################################################
RayonSC=2500 #10km # Largeur des rayons
ElargissementSection=25 # Après avoir toruvé la forme "optimale", on élargit de cette valeur
nomSH="G:\\BDD_Vecteurs\\Sandre\\sastationhydro_fxx.gpkg"

################################################################################
#----------------------- valeur par défaut de C2D-------------------------------
################################################################################
###############################################################################
# A NE PAS CHANGER SAUF SI VOUS EN AVEZ VRAIMENT ENVIE!
Doss_Base = "_Cartino2D_PreRequis"
Doss_Qml  = "_Cartino2D_Qml_et_autre"
###############################################################################
###### A NE SURTOUT PAS CHANGER SAUF SI VOUS EN AVEZ VRAIMENT VRAIMENT ENVIE!
#--------------------------------------------------------
# Nom du projet telemac de base dans le dossier Prerequis
nom_Cas = "Cartino2D.cas"
#--------------------------------------------------------
# Nom des fichiers exports de cartino2D
nom_Telemac = "Cartino2D"
nom_STEP1_Contour = "Step_Cartino2d_1_Contour"
nom_STEP2_Zone = "Step_Cartino2d_2_Zones"
nom_STEP2_Points = "Step_Cartino2d_2_Points"
nom_STEP2_Zone_Valid_Calcul = "Step_Cartino2d_2_Valid_Calcul"
nom_nodes = "GMSH_node.csv"
nom_friction = "friction"
nom_CN = "CN"
nom_maillage = "Maillage"
nom_MNT = "MNT"


