# ====================================================================================================================
# CARTINO2D - EXEMPLE SECTEUR : Grabels (Bassin de la Mosson / Montpellier)
# Objet : Paramètres de base pour l'Étape 00 (Création du fichier SIG des secteurs d'étude)
# ====================================================================================================================

####################################################################################################################
#### 1. LIEN VERS LES DOSSIERS & RÉFÉRENTIEL GÉOGRAPHIQUE ###########################################################
####################################################################################################################

# Répertoire racine du projet
dsnlayer  <- "C:/Cartino2D_Github/France"

# Nom du fichier vecteur des secteurs d'étude (Étape 00)
nomlayerC <- "Cartino2D_Secteur_Grabels.gpkg"

# Dossier des styles QML et symbologie (laisser par défaut)
Doss_Qml  <- "_Cartino2D_Qml_et_autre"

# Système de coordonnées de référence (SCR / EPSG) : 2154 = RGF93 / Lambert-93
EPSG      <- 2154


####################################################################################################################
#### 2. MAILLAGE & ZONES D'INTÉRÊT #################################################################################
####################################################################################################################

# Couche d'emprise Exzeco pour maillage déstructuré à densité variable (fin dans la zone inondable, lâche en périphérie)
Exzeco    <- "C:/Cartino2D_Github/France/_Exzeco/MTPCLL2024_GMSHtSceMaxMedContFrance_ZI_5met1m.gpkg"

# Découpage et marges autour du secteur :
Cont1_Buf <- 150     # Distance tampon extérieure (m) => frontières de calcul (150 pour pluie)
Cont1_Dx  <- 25      # Pas de discrétisation des sommets de frontière extérieure (m)
Cont2_Buf <- -100    # Distance tampon négative (m) pour délimiter la zone centrale à préserver
Ratio_Dx  <- 2 / 3   # Ratio de filtrage des sommets trop rapprochés

# Paliers de transition du maillage déstructuré autour de l'emprise Exzeco :
Exz1_Buf  <- 0       # 1er paquet : le long de la zone inondable
Exz1_Dx   <- 3       # 1er paquet : pas de maille très fin (3 m)
Exz2_Buf  <- 20      # 2ème paquet : tampon intermédiaire (20 m)
Exz2_Dx   <- 10      # 2ème paquet : pas de maille intermédiaire (10 m)
Exz3_Buf  <- 40      # 3ème paquet : transition (40 m)
Exz3_Dx   <- 20      # 3ème paquet : pas de maille de transition vers 25 m


####################################################################################################################
#### 3. DONNÉES ENTRÉES & FORÇAGES HYDROLOGIQUES ###################################################################
####################################################################################################################

# Nom d'identification pour la fusion et le post-traitement final (Étape 09)
NOMPOST   <- "Grabels"

# Lien vers la Table d'Assemblage du MNT (MTP CLL 2024)
MNT       <- "C:/Cartino2D_Github/France/_MNT/MTP_CLL_2024/TA_MTP_CLL_2024.gpkg"

# Fichier de points/sections de sources de débit individualisées (vide si pluie)
SourceDeb <- ""

# Frottement (Strickler) : lien vers la Table d'Assemblage spatialisée
Friction  <- "C:/Cartino2D_Github/France/_Strickler/TA_Strickler_MTP20240705.gpkg"

# Curve Number (CN) : lien vers la Table d'Assemblage spatialisée
CN        <- "C:/Cartino2D_Github/France/_CN/TA_CN_C_MTP20240705.gpkg"

# Prise en compte du CN : 0 = calculé dans TELEMAC-2D directement
CN_HYETO  <- 0


####################################################################################################################
#### 4. OUVRAGES & CONTRAINTES GÉOMÉTRIQUES ########################################################################
####################################################################################################################

# Couche des Ouvrages Hydrauliques / Buses / Ponts (vide si aucun)
OH        <- ""

# Couche des Lignes de contraintes topographiques (vide si aucune)
LigContr  <- ""

# Couche des Sections de Contrôle pour le suivi des débits
SectCont  <- "C:/Cartino2D_Github/France/_SectionsControles/SectControl_MTPCLL2024.gpkg"


####################################################################################################################
#### 5. PARAMÈTRES HYDRAULIQUES & NUMÉRIQUES TELEMAC-2D ###########################################################
####################################################################################################################

# Creusement altimétrique du MNT sur les frontières ouvertes (m)
DecAltiMNT <- -25

# Nombre de points voisins pour interpolation topographique
NINTERPMNT <- 1

# Cote aval minimale imposée (m NGF)
COTEAVALM  <- 0.5

# Rehausse altimétrique minimale
COTEAVALAJ <- 0.1

# Temps de début de calcul (heures)
TEMPS_DEBH <- 0

# Durée totale de la simulation (heures)
DURATION   <- 14

# Pas de temps (secondes) : 0.5 s adapté pour maillage très fin localement (1-3 m)
TIME_STEP  <- 0.5

# Période d'export (-1 = enveloppe max)
PRINTPERIO <- -1

# Flag de suivi des étapes pré-traitement
STEP_PRE   <- 0
