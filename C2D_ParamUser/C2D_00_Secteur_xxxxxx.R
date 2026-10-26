# ====================================================================================================================
# CARTINO2D - TEMPLATE : C2D_00_Secteur_xxxxxx.R
# Objet : Paramètres de base pour l'Étape 00 (Création du fichier SIG des secteurs d'étude)
# ====================================================================================================================
# Instructions :
# 1. Copier ce fichier dans "C2D_ParamUser/" sous le nom "C2D_00_Secteur_<NomDuProjet>.R"
# 2. Remplir les chemins et paramètres ci-dessous.
# 3. Les paramètres définis ici alimenteront la table attributaire du fichier GeoPackage/Shapefile créé.
#    Vous pourrez ensuite ajuster ces valeurs secteur par secteur directement sous QGIS.
# ====================================================================================================================

####################################################################################################################
#### 1. LIEN VERS LES DOSSIERS & RÉFÉRENTIEL GÉOGRAPHIQUE ###########################################################
####################################################################################################################

# Répertoire racine du projet (où seront stockés les sous-dossiers de secteurs et résultats)
dsnlayer  <- "C:/Cartino2D/MonProjet"

# Nom du fichier vecteur des secteurs qui sera généré par l'Étape 00 (GeoPackage recommandé .gpkg ou Shapefile .shp)
nomlayerC <- "Secteurs_MonProjet_5m.gpkg"

# Dossier des styles QML et symbologie (laisser par défaut)
Doss_Qml  <- "_Cartino2D_Qml_et_autre"

# Système de coordonnées de référence (SCR / EPSG)
# Exemples : 2154 (France Lambert-93), 5490 (Guadeloupe/Martinique RGAF09), 2975 (Réunion RGR92), 4471 (Mayotte)
EPSG      <- 2154


####################################################################################################################
#### 2. MAILLAGE & ZONES D'INTÉRÊT #################################################################################
####################################################################################################################

# Taille de maille (m) OU chemin complet vers une couche vecteur d'emprise/zone inondable d'intérêt (Exzeco / ZI)
# - Si nombre entre guillemets (ex: "5" ou "25") : maillage régulier homogène à cette résolution.
# - Si chemin complet vers un .gpkg/.shp : maillage déstructuré à densité variable (fin dans la ZI, lâche autour).
Exzeco    <- "5"

# Découpage et marges autour du secteur :
Cont1_Buf <- 250     # Distance tampon (m) extérieure sur tout le secteur => délimite les frontières de calcul (150 pour Pluie, 250 pour Débit)
Cont1_Dx  <- 25      # Pas de discrétisation des sommets de frontière extérieure (m)
Cont2_Buf <- -150    # Distance tampon négative (m) pour délimiter la zone centrale à préserver et desserrer la maille en périphérie
Ratio_Dx  <- 2 / 3   # Ratio de filtrage pour éliminer les sommets topologiquement trop rapprochés (défaut : 2/3)

# Paliers de transition du maillage déstructuré (utilisés si Exzeco pointe vers une couche d'emprise) :
Exz1_Buf  <- 0       # 1er paquet : distance tampon (m) le long de la zone inondable
Exz1_Dx   <- 3       # 1er paquet : pas de maille le plus fin (m) au cœur de la zone inondable
Exz2_Buf  <- 25      # 2ème paquet : distance tampon intermédiaire (m)
Exz2_Dx   <- 10      # 2ème paquet : pas de maille intermédiaire (m)
Exz3_Buf  <- 50      # 3ème paquet : distance tampon de transition (m)
Exz3_Dx   <- 20      # 3ème paquet : pas de maille de transition vers la frontière extérieure (m)


####################################################################################################################
#### 3. DONNÉES ENTRÉES & FORÇAGES HYDROLOGIQUES ###################################################################
####################################################################################################################

# Nom d'identification du scénario pour la fusion et le post-traitement final (Étape 09)
NOMPOST   <- "Scena_P100"

# Lien obligatoire vers la Table d'Assemblage du MNT (.shp ou .gpkg)
MNT       <- "C:/Cartino2D/MonProjet/_MNT/TA_MNT.shp"

# Fichier de points/sections de sources de débit individualisées (laisser vide "" si forçage Pluie)
SourceDeb <- ""

# Frottement (Strickler) :
# - Valeur numérique sous forme de chaîne (ex: "15" pour K=15 m^(1/3)/s partout)
# - OU chemin complet vers la Table d'Assemblage spatialisée (.gpkg ou .shp)
Friction  <- "15"

# Coefficient d'infiltration Curve Number (CN) :
# - Valeur numérique sous forme de chaîne (ex: "75" ou "100" pour pas d'infiltration)
# - OU chemin complet vers la Table d'Assemblage spatialisée (.gpkg ou .shp)
CN        <- "75"

# Prise en compte du CN :
# 0 = Infiltration calculée directement dans TELEMAC-2D (recommandé)
# 1 = Infiltration prétraitée dans le fichier hyétogramme de pluie
CN_HYETO  <- 0


####################################################################################################################
#### 4. OUVRAGES & CONTRAINTES GÉOMÉTRIQUES ########################################################################
####################################################################################################################

# Couche des Ouvrages Hydrauliques / Buses / Ponts (laisser "" si aucun)
OH        <- ""

# Couche des Lignes de contraintes topographiques / digues / merlons (laisser "" si aucune)
LigContr  <- ""

# Couche des Sections de Contrôle pour le suivi des hydrogrammes de débit (laisser "" si aucune)
SectCont  <- ""


####################################################################################################################
#### 5. PARAMÈTRES HYDRAULIQUES & NUMÉRIQUES TELEMAC-2D ###########################################################
####################################################################################################################

# Creusement altimétrique artificiel (m) sur les nœuds de bordure aval pour favoriser la vidange libre
DecAltiMNT <- -25

# Nombre de points voisins pour l'interpolation du MNT sur les nœuds du maillage (1 par défaut)
NINTERPMNT <- 1

# Cote aval imposée (m NGF) si la frontière a une altitude négative ou si présence de la mer
COTEAVALM  <- 0.5

# Rehausse de calage (m) ajoutée au point bas de la frontière aval dans le domaine valide
COTEAVALAJ <- 0.1

# Heure de début de la simulation (h)
TEMPS_DEBH <- 0

# Durée totale de la simulation (h)
DURATION   <- 12

# Pas de temps numérique TELEMAC-2D (secondes) (typiquement 1s ou 0.5s selon le pas de maille et stabilité CFL)
TIME_STEP  <- 1

# Périodicité d'enregistrement des résultats dans le fichier Selafin :
#  0 = Variables à la fin de la simulation uniquement
# -1 = Variables enveloppes MAXIMALES (recommandé pour Cartino2D standard)
#  N (ex: 5, 10, 60) = Pas d'export en minutes tout au long du calcul
PRINTPERIO <- -1

# Indicateur de pilotage des étapes (laisser à 0 à la création)
STEP_PRE   <- 0
