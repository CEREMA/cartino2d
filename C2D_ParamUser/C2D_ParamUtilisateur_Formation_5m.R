# ====================================================================================================================
# CARTINO2D - PARAMÉTRAGE UTILISATEUR : Formation_5m (Cas école standard 5m)
# ====================================================================================================================

####################################################################################################################
#### 1. CONFIGURATION GÉNÉRALE & CHEMINS DU PROJET #################################################################
####################################################################################################################

dsnlayer        <- "C:/Cartino2D/Formation"
nomlayerC       <- "Formation_5m.gpkg"
EPSG            <- 2154
SecteurGRASS_   <- "C:/GRASSDATA/C2D/Temp"
NProcGrass      <- 1
dsnPluie        <- "C:/Cartino2D/Formation/_Pluie"
dsnPluie_       <- "C:/Cartino2D/Formation/_Pluie"

####################################################################################################################
#### 2. PILOTAGE, PARALLÉLISATION & SENS DE PARCOURS DES CALCULS ###################################################
####################################################################################################################

nb_proc_pre     <- 3
nb_proc_post    <- 1
nb_proc_preC2D  <- 1
SensCalcul      <- -1
newmethodsc     <- TRUE
CN_100          <- FALSE

####################################################################################################################
#### 3. OUVRAGES HYDRAULIQUES, BUSES & LOIS D'OBSTRUCTION (Étape 04 & Abaques) #####################################
####################################################################################################################

abaques         <- TRUE
OH_verif        <- TRUE
num_cores_sim_abaques <- 4
ncore_buse_abaques    <- 2
liste_EXT       <- c("005", "010", "015", "020", "025", "030", "040", "050", "060", "070", "080", "090", "100")

RelSce_OHObstrue <- rbind(
  cbind("SceP100",  "Emoy")
)

nomxlsGrilleOHobstrue <- file.path(chem_routine, "C2D", "GrilleOHobstrue.xlsx")

####################################################################################################################
#### 4. POST-TRAITEMENT, SCÉNARIOS & ARCHIVAGE (Étapes 09, 11, 13) ##################################################
####################################################################################################################

Scena           <- cbind(
  "SceP100_SEPIA_MAMP_AMC2",
  "SceP100_SEPIA_MAMP_AMC1"
)

choixPPutilsQgis <- 1
ExportGPKG      <- TRUE

####################################################################################################################
#### 5. PREC2D GÉNÉRAL & PARAMÉTRAGE GRASS / FILINO ################################################################
####################################################################################################################

chemin7z        <- "C:/Program Files/7-Zip/7z.exe"
chem_filino     <- file.path(chem_routine, "FILINO/filino/RFILES")
nomDpt          <- "D:/FILINO_Travail/00_SIGBase/DEPARTEMENT.shp"
nommasque       <- ""
ResoMNTpourTaudem <- 5

####################################################################################################################
#### 6. PREC2D 1.1 - TÉLÉCHARGEMENT & CHARGEMENT DES BDD VECTEURS ###################################################
####################################################################################################################

urlBDTopo       <- "https://data.geopf.fr/telechargement/resource/BDTOPO"
dsnDptBDTopo    <- "C:/BDD/BDTopo"

urlRGEAlti      <- "https://data.geopf.fr/telechargement/resource/RGEALTI"
dsnRGEAlti      <- "C:/BDD/RGEAlti"
nomTARGEAlti    <- "TA_RGEALTI_FR.shp"
paramRGEAlti    <- cbind(0, 0, 13, 16, 18, 21)

urlOCSGE        <- "https://data.geopf.fr/telechargement/resource/OCSGE"
dsnDptOCS_GE    <- "C:/BDD/OCS_GE"

dsnPHE          <- "C:/BDD/PHE"
nomPHE          <- "export_activite_inondation.gpkg"

dsnTAmano       <- "C:/Cartino2D/Formation/_MNT"
nomTAmano       <- "TA_TestFP.gpkg"
paramTAmano     <- cbind(0, 0, 13, 16, 18, 21)
extTAmano       <- ".gpkg$"

####################################################################################################################
#### 7. PREC2D 2.1 - DÉTERMINATION DES ZONES DPE ###################################################################
####################################################################################################################

HSF_mini        <- 0.05
HWH_mini        <- 0.1
TpicPluie_h     <- 6
DecalPicH_min   <- 30
DeltaPicH_PluieDebit <- 5

Extens          <- rbind(
  cbind("HSF", "HWH", "HWT"),
  cbind("_m2_s", "_m", "_s"),
  cbind(HSF_mini, HWH_mini, (TpicPluie_h + DecalPicH_min / 60) * 3600)
)

resolution      <- 5
buf             <- 1.1 * resolution / 2
seuilSup0       <- 50000

####################################################################################################################
#### 8. PREC2D 2.2 - DÉTECTION AUTOMATIQUE DES PONTS & OUVRAGES HYDRAULIQUES (OH) ##################################
####################################################################################################################

nomOHlocaux     <- ""
BufOHlocaux     <- 5

nomPontRRNc     <- ""
nomPontRRNnc    <- ""
FactMultIsidor  <- 1.75

nomPontPNP      <- ""
FactMultPNP     <- 1.75

FactMultiBDTopo <- 3
BufConstLi      <- 15
BufConstSurf    <- 10

####################################################################################################################
#### 9. PREC2D 2.3 - MNT, ZONES PERDUES & ENDORÉISMES (ZPP) ########################################################
####################################################################################################################

dsnLidar        <- "C:/Cartino2D/Formation/_MNT"
nomTA           <- "TA_MNT_Formation.shp"
DeltadessusMin  <- 1
nomZPP          <- ""

####################################################################################################################
#### 10. PREC2D 2.4 - HYDROLOGIE TAUDEM ############################################################################
####################################################################################################################

nproc           <- 4
DINF            <- FALSE
seuil1          <- 500

####################################################################################################################
#### 11. PREC2D 2.5 - DÉCOUPAGE & SECTORISATION PLUIE ###############################################################
####################################################################################################################

seuilSup1       <- 5000; units(seuilSup1) <- "m^2"
seuilSup2       <- 5000; units(seuilSup2) <- "m^2"
seuilSup3       <- 5000; units(seuilSup3) <- "m^2"
Bufferflash     <- 20
BufferDepasse   <- 50
SeuilBVMin_pourC2D <- 1; units(SeuilBVMin_pourC2D) <- "m^2"

####################################################################################################################
#### 12. PREC2D 2.6 - CALAGE & INTÉGRATION DES PLUS HAUTES EAUX (PHE) ##############################################
####################################################################################################################

SeuilDistPHE    <- c(0.1, 7, 14, 25, 50, 100, 1000)

####################################################################################################################
#### 13. PREC2D 2.7 - REMODELAGE ET DÉBITS #########################################################################
####################################################################################################################

dsnexport       <- "C:/Cartino2D/Formation/Export"

####################################################################################################################
#### 14. PREC2D 3.0 - CARTOGRAPHIE DU FROTTEMENT STRICKLER & INFILTRATION CURVE NUMBER #############################
####################################################################################################################

ResoStrCN       <- c(1, 5)
dsnlayerStriCN  <- "Formation"
nomXLS          <- file.path(chem_routine, "PreC2D", "Relation_CN_Str_Formation.xlsx")
nom_Param       <- cbind("CN_C", "Strickler")

####################################################################################################################
#### 15. PREC2D 4.1 - PLUIES RADAR & STATISTIQUES D'ÉVÉNEMENTS HISTORIQUES #########################################
####################################################################################################################

nb_h_avant      <- -1
nb_h_apres      <- 24
cumul_min       <- 0
do_stat         <- TRUE
calculcumul     <- TRUE
nb_proc         <- 1
calculcumulsp   <- TRUE

nduree          <- cbind(5, 15, 30, 60, 2 * 60, 3 * 60, 4 * 60, 6 * 60, 12 * 60)
ncumul          <- c(1 / 4, 1 / 2, 1, 2, 3, 4, 6, 12)
ncumulnoms      <- c("15min", "30min", "1h", "2h", "3h", "4h", "6h", "12h")
minmm           <- 0
NPerRet         <- cbind(2, 5, 10, 20, 50, 100)
DateDebut       <- 202109140000
DateFin         <- 202109150000
use_debu_fin    <- TRUE
AMC             <- 2
force_date      <- FALSE
rents           <- c(2, 5, 10, 20, 50, 100)

####################################################################################################################
#### 16. PREC2D 4.3 - PLUIES SYNTHÉTIQUES SHYREG ###################################################################
####################################################################################################################

duree           <- 12
nomdpt          <- cbind("30")
tpic            <- 6
Periode_retour  <- c(10, 50, 100)

ras_min         <- "C:/Cartino2D/SHYREG/ras_P15-60mm_SHYREG_2018_France_L93_deterministe.RData"
ras_h           <- "C:/Cartino2D/SHYREG/ras_Ph_SHYREG_2018_France_L93_deterministe.Rdata"
ras_Pn_DIR      <- "C:/Cartino2D/SHYREG/SHYREGPluieNette"
FournitureINRAEDptouSecteur <- 0
ContourPN_INRAE <- ""

####################################################################################################################
#### 17. PREC2D 4.5 - DÉBITS DE POINTE SYNTHÉTIQUES SHYREG & RÉSEAU HYDROGRAPHIQUE #################################
####################################################################################################################

dsnlayerQ       <- "C:/Cartino2D/Shyreg_Debit"
nomZoneCEB      <- ""
TA_Lidar        <- "C:/Cartino2D/Formation/_MNT/TA_MNT_Formation.shp"
Periode_retour_debit <- 100
RValMin         <- 10
distBufS        <- 50
intervPts       <- 12
dsegm           <- 50

####################################################################################################################
#### 18. PREC2D 7.0 - POSITIONNEMENT AUTOMATIQUE DES SECTIONS DE CONTRÔLE HYDRO (SANDRE) ###########################
####################################################################################################################

RayonSC         <- 1000
ElargissementSection <- 10
nomSH           <- ""

####################################################################################################################
#### 19. NOMS INTERNES ET STRUCTURE FIXE C2D (À NE PAS MODIFIER) ###################################################
####################################################################################################################

Doss_Base       <- "_Cartino2D_PreRequis"
Doss_Qml        <- "_Cartino2D_Qml_et_autre"
nom_Cas         <- "Cartino2D.cas"
nom_Telemac     <- "Cartino2D"
nom_STEP1_Contour <- "Step_Cartino2d_1_Contour"
nom_STEP2_Zone  <- "Step_Cartino2d_2_Zones"
nom_STEP2_Points <- "Step_Cartino2d_2_Points"
nom_STEP2_Zone_Valid_Calcul <- "Step_Cartino2d_2_Valid_Calcul"
nom_nodes       <- "GMSH_node.csv"
nom_friction    <- "friction"
nom_CN          <- "CN"
nom_maillage    <- "Maillage"
nom_MNT         <- "MNT"
