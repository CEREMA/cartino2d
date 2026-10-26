# ====================================================================================================================
# CARTINO2D - TEMPLATE EXHAUSTIF : C2D_ParamUtilisateur_xxxxxx.R
# Objet : Fichier complet de paramétrage utilisateur pour Cartino2D (C2D) et ses Prétraitements (PreC2D)
# ====================================================================================================================
# Instructions :
# 1. Copier ce fichier dans "C2D_ParamUser/" sous le nom "C2D_ParamUtilisateur_<NomDuProjet>.R"
# 2. Renseigner les chemins vers vos données SIG, pluies, MNT et vos seuils hydrologiques.
# 3. Tous les paramètres utilisés dans l'ensemble des versions Cartino2D sont regroupés ci-dessous
#    avec des explications sur leur utilité, leurs unités et leurs valeurs par défaut.
# 4. Ce fichier sera sélectionné via la boîte de dialogue graphique au lancement de "C2D_RUN.R".
# ====================================================================================================================


####################################################################################################################
#### 1. CONFIGURATION GÉNÉRALE & CHEMINS DU PROJET #################################################################
####################################################################################################################

# Répertoire de travail racine du projet (où se trouve le fichier des secteurs)
dsnlayer        <- "C:/Cartino2D/MonProjet"

# Nom du fichier vecteur des secteurs d'étude à traiter (produit à l'Étape 00, format GeoPackage .gpkg recommandé)
nomlayerC       <- "Secteurs_MonProjet_5m.gpkg"

# Système de coordonnées géographiques (SCR / EPSG)
# Exemples : 2154 (Lambert-93 France métropolitaine), 5490 (Guadeloupe/Martinique), 2975 (La Réunion), 4471 (Mayotte)
EPSG            <- 2154

# Répertoire de stockage de la base de données GRASS GIS locale (créé automatiquement si inexistant)
SecteurGRASS_   <- "C:/GRASSDATA/C2D/Temp"
NProcGrass      <- 1   # Nombre de sessions GRASS concurrentes (laisser à 1 pour éviter les verrous de base)

# Dossier d'accès aux données de pluie (fichiers hyétogrammes ou rasters SHYREG/Radar)
dsnPluie        <- "C:/Cartino2D/MonProjet/_Pluie"

# Dossier racine ou alternatif des données de pluie brute (utilisé par certaines routines radar/SHYREG)
dsnPluie_       <- "C:/Cartino2D/MonProjet/_Pluie"


####################################################################################################################
#### 2. PILOTAGE, PARALLÉLISATION & SENS DE PARCOURS DES CALCULS ###################################################
####################################################################################################################

# Nombre de cœurs CPU alloués pour la parallélisation multi-threads R (package doParallel / foreach) :
nb_proc_pre     <- 3   # Nombre de cœurs pour le pré-traitement C2D (Étapes 01 à 04)
nb_proc_post    <- 1   # Nombre de cœurs pour le post-traitement C2D (Étapes 06 à 09)
nb_proc_preC2D  <- 1   # Nombre de cœurs pour les prétraitements PreC2D

# Sens de tri et de parcours des secteurs d'étude dans les boucles de calcul :
#  1 = Ordre croissant (par numéro ou nom de secteur)
# -1 = Ordre décroissant (très utile pour inverser l'ordre des secteurs ou traiter de l'aval vers l'amont)
SensCalcul      <- -1

# Méthode de calcul des débits aux sections de contrôle :
# TRUE = Nouvelle méthode de calcul vectorisée et robuste (recommandée)
# FALSE = Ancienne méthode séquentielle
newmethodsc     <- TRUE

# Option historique CN 100 : forcer le Curve Number à 100 (ruissellement total sans infiltration, laisser FALSE)
CN_100          <- FALSE


####################################################################################################################
#### 3. OUVRAGES HYDRAULIQUES, BUSES & LOIS D'OBSTRUCTION (Étape 04 & Abaques) #####################################
####################################################################################################################

# Calcul hydraulique des buses par abaques :
# TRUE  = Active le couplage avec les courbes de tarage pré-calculées par abaques (recommandé pour précision hydraulique)
# FALSE = Utilise la formulation analytique simplifiée interne de TELEMAC
abaques         <- TRUE

# Diagnostic et vérification préalable des ouvrages :
# TRUE  = Active la vérification approfondie des géométries de buses (cotes radier, diamètres, pentes) avant calcul
# FALSE = Pas de vérification bloquante
OH_verif        <- TRUE

# Parallélisation dédiée au calcul des abaques de buses :
num_cores_sim_abaques <- 40   # Nombre de cœurs CPU pour la génération des courbes de tarage par simulation
ncore_buse_abaques    <- 2    # Nombre de cœurs pour paralléliser la boucle sur l'ensemble des buses (1 = séquentiel, >=2 = parallèle)

# Loi d'obstruction des ouvrages hydrauliques selon le scénario de crue (prise en compte des embâcles / débris) :
# Associe chaque nom de scénario à une sévérité d'obstruction ("Emoy" = obstruction moyenne, "Eext" = obstruction extrême)
RelSce_OHObstrue <- rbind(
  cbind("SceMaxMedzzz",  "Eext"),
  cbind("Sce20141006zzz", "Emoy"),
  cbind("Sce20140929zzz", "Emoy")
)

# Chemin vers la grille Excel définissant les pourcentages de réduction de débit des ouvrages en fonction du niveau d'obstruction
nomxlsGrilleOHobstrue <- file.path(chem_routine, "C2D", "GrilleOHobstrue.xlsx")


####################################################################################################################
#### 4. POST-TRAITEMENT, SCÉNARIOS & ARCHIVAGE (Étapes 09, 11, 13) ##################################################
####################################################################################################################

# Liste des scénarios à fusionner sous forme de matrice/vecteur de chaînes (Étape 09) :
# Exemples : cbind("SceP100_SEPIA_MAMP_AMC2", "SceP100_SEPIA_MAMP_AMC1", "T0100_D24_PIC08")
Scena           <- cbind(
  "Scena_P100",
  "Scena_P50",
  "Scena_P10"
)

# Liste des extensions et motifs de fichiers gérés par les étapes de contrôle (Étape 11),
# de nettoyage ou d'archivage compressé en zip (Étape 13) :
liste_EXT       <- cbind(
  "\\.cas",
  "\\.res",
  "_HWH_m.gpkg",
  "_ZI_HSF_0_1_HWH_0_2_Qgis_Final_Aint630_Aext1256.25.gpkg",
  "_ZI_HSF_0_08_HWH_0_15_Qgis_Final_Aint630_Aext1256.25.gpkg",
  "_ZI_HSF_0_05_HWH_0_1_Qgis_Final_Aint630_Aext1256.25.gpkg",
  "_ZI_HSF_0_03_HWH_0_05_Qgis_Final_Aint630_Aext1256.25.gpkg",
  "_ZI_HSF_0_HWH_0_Qgis_Final_Aint630_Aext1256.25.gpkg"
)


####################################################################################################################
#### 5. PRÉTRAITEMENTS PreC2D - PARAMÈTRES GÉNÉRAUX & OUTILS AUXILIAIRES ############################################
####################################################################################################################

# Chemin vers l'exécutable 7-Zip (pour l'extraction automatique des archives de données IGN)
chemin7z        <- "C:/Program Files/7-Zip/7z.exe"

# Chemin vers les scripts et fonctions utilitaires de la bibliothèque FILINO
chem_filino     <- file.path(chem_routine, "FILINO", "filino", "RFILES")

# Couche vectorielle des départements français (utilisée pour les intersections et découpages territoriaux)
nomDpt          <- "D:/FILINO_Travail/00_SIGBase/DEPARTEMENT.shp"

# Masque spatial optionnel pour restreindre les prétraitements (utile en bordure littorale pour exclure la mer)
# Laisser vide "" si aucun masque n'est requis
nommasque       <- ""

# Emprise vectorielle des dalles LiDAR .laz disponibles à traiter (ou "")
nomZone         <- ""

# Résolution spatiale du MNT pour les algorithmes d'analyse de relief sous TauDEM (mètres)
ResoMNTpourTaudem <- 5


####################################################################################################################
#### 6. PreC2D - ÉTAPE 1.1 : CHARGEMENT DES BASES DE DONNÉES IGN & MNT #############################################
####################################################################################################################

# BD TOPO IGN (Hydrographie, Bâti, Réseau Routier et Ferré)
urlBDTopo       <- "https://data.geopf.fr/telechargement/resource/BDTOPO"
dsnDptBDTopo    <- "C:/BDD/BDTopo"

# RGE Alti IGN (1m ou 5m)
urlRGEAlti      <- "https://geoservices.ign.fr/rgealti"
dsnRGEAlti      <- "E:/RGEAlti1m/France"
nomTARGEAlti    <- "TA_RGEALTI_FR.shp"
paramRGEAlti    <- cbind(0, 0, 13, 16, 18, 21)

# OCS-GE (Occupation du Sol à Grande Échelle)
dsnDptOCS_GE    <- "G:/OCS_GE"
urlOCSGE        <- "https://geoservices.ign.fr/ocsge"

# BDRC Plus Hautes Eaux (PHE)
dsnPHE          <- "G:/BDRC_PHE"
nomPHE          <- "BNDRC_PHE.gpkg"

# Table d'assemblage du MNT local / projet (MNT TIN ou raster)
dsnTAmano       <- "C:/Cartino2D/MonProjet/_MNT/TA_Local"
nomTAmano       <- "TA_MNT_Projet.shp"
paramTAmano     <- cbind(0, 0, 13, 16, 18, 21)
extTAmano       <- ".shp$"


####################################################################################################################
#### 7. PreC2D - ÉTAPE 2.1 : DÉTERMINATION DES ZONES DPE & REDÉCOUPAGE HYDROLOGIQUE ################################
####################################################################################################################

# Seuils hydrauliques de coupure pour la détection des zones inondables (DPE) :
HSF_mini        <- 0.05   # Hauteur d'eau minimale en submersion fine (m)
HWH_mini        <- 0.05   # Hauteur d'eau minimale en zone d'écoulement préférentiel (m)

# Paramètres temporels de l'onde de submersion :
TpicPluie_h     <- 2      # Heure d'occurrence du pic de pluie (heures)
DecalPicH_min   <- 8      # Décalage estimé entre pic de pluie et pic de crue (minutes, à adapter selon taille du bassin)
DeltaPicH_PluieDebit <- 5 # Fenêtre temporelle d'analyse (minutes)

Extens          <- rbind(
  cbind("HSF", "HWH", "HWT"),
  cbind("_m2_s", "_m", "_s"),
  cbind(HSF_mini, HWH_mini, (TpicPluie_h + DecalPicH_min / 60) * 3600)
)

# Résolution spatiale des calculs DPE (m) et taille de tampon associé
resolution      <- 2.5
buf             <- 1.1 * resolution / 2

# Seuil de surface minimale pour éliminer les micro-zones de stagnation isolées (m²)
seuilSup0       <- 100000


####################################################################################################################
#### 8. PreC2D - ÉTAPE 2.2 : DÉTECTION DES PONTS, RETABLISSEMENTS & OUVRAGES #######################################
####################################################################################################################

# Couche locale des ouvrages hydrauliques du gestionnaire (ou "")
nomOHlocaux     <- "C:/Cartino2D/MonProjet/_OuvragesHydrauliques/OH_Locaux.gpkg"
BufOHlocaux     <- 5      # Zone tampon (m) autour des ouvrages hydrauliques

# Ponts issus de la base ISIDOR (Réseau Routier National concédé et non concédé)
nomPontRRNc     <- ""
nomPontRRNnc    <- ""
FactMultIsidor  <- 1.75   # Facteur d'agrandissement de l'emprise des ponts Isidor

# Ponts issus du Programme National Ponts (PNP) du Cerema
nomPontPNP      <- "C:/BDD_Vecteurs/PONTS/PNP/PNP-donnees-publiques-20231001.gpkg"
FactMultPNP     <- 1.75   # Facteur d'agrandissement de l'emprise des ponts PNP

# Traitement BD TOPO IGN (croisement tronçons franchissant l'eau / surfaces) :
FactMultiBDTopo <- 3      # Facteur multiplicateur pour compenser la sous-estimation de largeur des ponts
BufConstLi      <- 15     # Buffer (m) autour des constructions linéaires
BufConstSurf    <- 10     # Buffer (m) autour des constructions surfaciques

# Répertoire de sortie pour l'export des emprises de ponts et rétablissements hydrauliques
dsnexport       <- "C:/Cartino2D/MonProjet/Exports_Ponts"

# Emplacement de la couche des zones pré-ponts (ZPP)
nomZPP          <- ""


####################################################################################################################
#### 9. PreC2D - ÉTAPE 2.3 : MNT, ALTIDEPASSE & ANALYSE DES ENDORÉISMES ############################################
####################################################################################################################

# Dossier et nom de la table d'assemblage MNT Lidar pour détection de cuvettes
dsnLidar        <- "C:/Cartino2D/MonProjet/_MNT"
nomTA           <- "TA_MNT_Projet.shp"

# Hauteur de relèvement altimétrique (m) au-dessus du point bas pour vidanger les cuvettes endoréiques parasites
DeltadessusMin  <- 1


####################################################################################################################
#### 10. PreC2D - ÉTAPE 2.4 : CALCULS TAUDEM (HYDROGRAPHIE DE SURFACE) #############################################
####################################################################################################################

nproc           <- 8      # Nombre de cœurs CPU alloués aux routines MPI de TauDEM
DINF            <- FALSE  # Algorithme d'écoulement : FALSE = D8 standard (mono-direction), TRUE = D-Infinity (multi-direction)
seuil1          <- 1000   # Seuil de surface d'accumulation (nombre de mailles) pour initier un thalweg


####################################################################################################################
#### 11. PreC2D - ÉTAPE 2.5 : SECTORISATION PLUIE & BASSINS VERSANTS ###############################################
####################################################################################################################

# Filtres surfaciques pour l'épuration topologique des sous-bassins :
seuilSup1       <- 10000; units(seuilSup1) <- "m^2"  # Suppression des tronçons parasites sur la couche d'écoulement
seuilSup2       <- 10000; units(seuilSup2) <- "m^2"  # Suppression des petits segments qui découpent inutilement un bassin
seuilSup3       <- 10000; units(seuilSup3) <- "m^2"  # Suppression des micro-bassins versants

Bufferflash     <- 20     # Tampon d'évasement (m) au niveau des exutoires
BufferDepasse   <- 50     # Marge de dépassement (m) pour assurer la continuité hydrodynamique entre secteurs contigus

# Surface minimale d'un sous-bassin pour être retenu comme secteur de calcul Cartino2D (en km²)
SeuilBVMin_pourC2D <- 2; units(SeuilBVMin_pourC2D) <- "m^2"


####################################################################################################################
#### 12. PreC2D - ÉTAPE 2.6 : PLUS HAUTES EAUX (PHE) ###############################################################
####################################################################################################################

# Seuils de distance (mètres) utilisés pour les zones tampons concentriques et l'interpolation autour des laisses PHE
SeuilDistPHE    <- c(0.1, 7, 14, 25, 50, 100, 10000)


####################################################################################################################
#### 13. PreC2D - ÉTAPE 3.0 : STRICKLER ET CURVE NUMBER SPATIALISÉS ################################################
####################################################################################################################

# Résolutions spatiales (mètres) des grilles GeoTIFF de rugosité et d'infiltration à produire
ResoStrCN       <- c(1, 5, 25)

# Dossier de destination des rasters (si chemin relatif, créé sous dsnlayer)
dsnlayerStriCN  <- "Strickler_CN"

# Fichier Excel contenant la table de décision liant [Type de sol + Occupation du sol + Pente] aux coefficients
nomXLS          <- file.path(chem_routine, "PreC2D", "Relation_CN_Str_Formation.xlsx")

# Colonnes du fichier Excel à spatialiser :
# Exemples : cbind("Strickler"), cbind("CN_C", "Strickler"), cbind("Strickler", "CN_B", "CN_C")
nom_Param       <- cbind("CN_C", "Strickler")

# Options d'export au format GeoPackage additionnel : cbind(Export_Strickler, Export_CN)
ExportGPKG      <- cbind(1, 1)


####################################################################################################################
#### 14. PreC2D - ÉTAPE 4.1 : FORÇAGES PAR PLUIES RADAR (RExP / LAME D'EAU) ########################################
####################################################################################################################

nb_h_avant      <- -1     # Nombre d'heures à intégrer avant le début déclaré de l'événement
nb_h_apres      <- 24     # Nombre d'heures à intégrer après la fin déclarée de l'événement
cumul_min       <- 0      # Seuil minimal de cumul précipité (mm) pour traiter une cellule
do_stat         <- TRUE   # Calculer les statistiques globales de pluie
calculcumul     <- TRUE   # Calculer les rasters de cumul temporel
calculcumulsp   <- TRUE   # Calculer les cumuls spatiaux par bassin
nb_proc         <- 1      # Cœurs de calcul alloués au prétraitement pluie

# Pas de cumul et périodes de retour analysés :
nduree          <- cbind(5, 15, 30, 60, 2*60, 3*60, 4*60, 6*60, 12*60, 24*60)
ncumul          <- c(1/4, 1/2, 1, 2, 3, 4, 6, 12, 24)
ncumulnoms      <- c("15min", "30min", "1h", "2h", "3h", "4h", "6h", "12h", "24h")
NPerRet         <- cbind(2, 5, 10, 20, 50, 100)

# Plage temporelle exacte de l'épisode pluvieux (Format AAAAMMJJHHMM) :
DateDebut       <- 202410170000
DateFin         <- 202410181200
use_debu_fin    <- TRUE

# Forçage strict de la plage de dates (TRUE = force exactement DateDebut/DateFin sans extension automatique)
force_date      <- TRUE

# Seuil minimal de pluie sur la zone pour déclencher le calcul (0 par défaut)
minmm           <- 0

# Pas de discrétisation temporelle de l'intensité de pluie (minutes, ex: 5, 15 ou 60)
raints          <- 15

# Condition d'Humidité Antécédente du sol (AMC - Antecedent Moisture Condition) :
# 1 = Sol sec, 2 = Sol moyen/normal, 3 = Sol saturé
AMC             <- 2

# Fichiers catalogues Excel de métadonnées des rasters radar (laisser vide "" ou nom de fichier) :
Link_Data       <- "Data_PluieRaster_formation.xlsx"
Link_DataRADAR  <- "Data_PluieRaster_COMEPHORE.xlsx"


####################################################################################################################
#### 15. PreC2D - ÉTAPE 4.3 : FORÇAGES PAR PLUIES SYNTHÉTIQUES SHYREG ##############################################
####################################################################################################################

duree           <- 24     # Durée totale de la pluie de projet en heures (ex: 12h ou 24h)
tpic            <- 8      # Heure du pic d'intensité (heures)
time_of_peak    <- 8      # Synonyme horaire de pic de pluie
time_step_sim   <- 180    # Pas de temps de discrétisation du hyétogramme synthétique (secondes, ex: 180s = 3min)

# Liste des départements d'étude à extraire des rasters SHYREG nationaux
nomdpt          <- cbind("13")

# Périodes de retour ciblées (ans) : ex c(10, 20, 50, 100, 500, 1000)
Periode_retour  <- c(100)

# Rasters nationaux SHYREG déterministes bruts (fournis par INRAE / Cerema) :
ras_min         <- "H:/Pluie/SHYREG/SHYREGPluieBrute/ras_P15-60mm_SHYREG_2018_France_L93_deterministe.RData"
ras_h           <- "H:/Pluie/SHYREG/SHYREGPluieBrute/ras_Ph_SHYREG_2018_France_L93_deterministe.Rdata"

# Répertoire des rasters SHYREG pluie nette (réservé Cerema) :
ras_Pn_DIR      <- "H:/Pluie/SHYREG/SHYREGPluieNette"
FournitureINRAEDptouSecteur <- 0

# Contour vectoriel des zones hydrographiques INRAE de pluie nette (ou "")
ContourPN_INRAE <- "D:/SHYREG_Pluie_Nette/Contour_zones_BNBV.Rdata"


####################################################################################################################
#### 16. PreC2D - ÉTAPES 4.4 & 4.5 : FORÇAGES PAR DÉBITS SHYREG & SECTEURS DE DÉBIT ###############################
####################################################################################################################

# Répertoire source contenant la base nationale SHYREG Débits de pointe
dsnlayerQ       <- "H:/Debits/Shyreg_Debit"

# Couche vectorielle des secteurs de débits (générée à l'étape PreC2D_2_1_Zone_DPE) servant de base pour l'injection
nomZoneCEB      <- "C:/Cartino2D/MonProjet/Secteurs_Debits.gpkg"

# Table d'assemblage IGN Lidar HD (format .gpkg) pour extraire précisément les cotes de fond de rivière aux injections
TA_Lidar        <- "F:/LidarHD_MNTIGN_V_0_1/LidarHD_MNTIGN_V_0_1.gpkg"

# Période de retour du débit de pointe injecté (ans) : ex 20, 50, 100, 500, 1000
Periode_retour_debit <- 100

# Seuil de surface drainée minimale (km²) ou débit de pointe pour retenir une source d'injection
RValMin         <- 20

# Distance de recherche transversale (m) pour localiser le point le plus bas du lit mineur
distBufS        <- 90

# Pas d'échantillonnage de points sur les biefs courts (< 2*resolution) (mètres)
intervPts       <- 12

# Pas de discrétisation le long des cours d'eau principaux (mètres)
dsegm           <- 80


####################################################################################################################
#### 17. PreC2D - ÉTAPE 7.0 : SECTIONS DE CONTRÔLE HYDROLOGIQUES SANDRE ############################################
####################################################################################################################

# Rayon de recherche (mètres) autour des stations hydrométriques pour implanter les sections de contrôle
RayonSC         <- 2500

# Élargissement transversal (mètres) appliqué de part et d'autre de la vallée après calage de la forme optimale
ElargissementSection <- 25

# Couche vectorielle officielle des stations hydrométriques (Banque Hydro / SANDRE)
nomSH           <- "G:/BDD_Vecteurs/Sandre/sastationhydro_fxx.gpkg"


####################################################################################################################
#### 18. VALEURS SYSTÈME CONSTANTES (À NE PAS MODIFIER SAUF BESOIN TRÈS SPÉCIFIQUE) ################################
####################################################################################################################

Doss_Base                     <- "_Cartino2D_PreRequis"
Doss_Qml                      <- "_Cartino2D_Qml_et_autre"

nom_Cas                       <- "Cartino2D.cas"
nom_Telemac                   <- "Cartino2D"
nom_STEP1_Contour             <- "Step_Cartino2d_1_Contour"
nom_STEP2_Zone                <- "Step_Cartino2d_2_Zones"
nom_STEP2_Points              <- "Step_Cartino2d_2_Points"
nom_STEP2_Zone_Valid_Calcul   <- "Step_Cartino2d_2_Valid_Calcul"
nom_nodes                     <- "GMSH_node.csv"
nom_friction                  <- "friction"
nom_CN                        <- "CN"
nom_maillage                  <- "Maillage"
nom_MNT                       <- "MNT"
