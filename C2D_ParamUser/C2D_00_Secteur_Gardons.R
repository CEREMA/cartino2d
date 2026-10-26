# ====================================================================================================================
# CARTINO2D - EXEMPLE SECTEUR : Gardons (Bassin des Gardons / Alès / Cévennes)
# Objet : Paramètres de base pour l'Étape 00 (Création du fichier SIG des secteurs d'étude)
# ====================================================================================================================

####################################################################################################################
#### 1. LIEN VERS LES DOSSIERS & RÉFÉRENTIEL GÉOGRAPHIQUE ###########################################################
####################################################################################################################

# Répertoire racine du projet (où seront stockés les sous-dossiers de secteurs et résultats)
dsnlayer  <- "C:/Cartino2D_Github/France"

# Nom du fichier vecteur des secteurs d'étude (Étape 00)
nomlayerC <- "Cartino2D_Secteur_Gardons.gpkg"

# Dossier des styles QML et symbologie (laisser par défaut)
Doss_Qml  <- "_Cartino2D_Qml_et_autre"

# Système de coordonnées de référence (SCR / EPSG) : 2154 = RGF93 / Lambert-93
EPSG      <- 2154


####################################################################################################################
#### 2. MAILLAGE & ZONES D'INTÉRÊT #################################################################################
####################################################################################################################

# Taille de maille (m) : ici maillage régulier homogène à 25 m
Exzeco    <- "25"

# Découpage et marges autour du secteur :
Cont1_Buf <- 400     # Distance tampon extérieure (m) => frontières de calcul
Cont1_Dx  <- 25      # Pas de discrétisation des sommets de frontière extérieure (m)
Cont2_Buf <- -250    # Distance tampon négative (m) pour délimiter la zone centrale
Ratio_Dx  <- 2 / 3   # Ratio de filtrage des sommets trop rapprochés

# Paliers de transition du maillage (si maille variable / ZI) :
Exz1_Buf  <- 0       # 1er paquet tampon (m)
Exz1_Dx   <- 3       # 1er paquet pas de discrétisation (m)
Exz2_Buf  <- 25      # 2ème paquet tampon (m)
Exz2_Dx   <- 10      # 2ème paquet pas de discrétisation (m)
Exz3_Buf  <- 50      # 3ème paquet tampon (m)
Exz3_Dx   <- 20      # 3ème paquet pas de discrétisation (m)


####################################################################################################################
#### 3. DONNÉES ENTRÉES & FORÇAGES HYDROLOGIQUES ###################################################################
####################################################################################################################

# Nom d'identification pour la fusion et le post-traitement final (Étape 09)
NOMPOST   <- "EAIM"

# Lien vers la Table d'Assemblage du MNT (Lidar / RGE Alti)
MNT       <- "C:/Cartino2D_Github/France/_MNT/EAIM/TA_1_2_3_4_5_mosaique.gpkg"

# Fichier de points/sections de sources de débit individualisées (vide si pluie)
SourceDeb <- ""

# Frottement (Strickler) : lien vers la Table d'Assemblage spatialisée
Friction  <- "C:/Cartino2D_Github/France/_Strickler/EAIM/Strickler/TA_Strickler_Reso025.gpkg"

# Curve Number (CN) : lien vers la Table d'Assemblage spatialisée
CN        <- "C:/Cartino2D_Github/France/_CN/EAIM/CN_C/TA_CN_C_Reso025.gpkg"

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
SectCont  <- "C:/Cartino2D_Github/France/_SectionsControles/EAIM/SectControlEAIM.shp"


####################################################################################################################
#### 5. PARAMÈTRES HYDRAULIQUES & NUMÉRIQUES TELEMAC-2D ###########################################################
####################################################################################################################

# Creusement altimétrique du MNT sur les frontières ouvertes (m) pour favoriser l'évacuation aval
DecAltiMNT <- -25

# Nombre de points voisins les plus proches pour l'interpolation altimétrique (1 = plus proche voisin)
NINTERPMNT <- 1

# Cote aval imposée ou niveau marin minimal si frontière négative (m NGF)
COTEAVALM  <- 0.5

# Rehausse altimétrique appliquée au domaine valide (m)
COTEAVALAJ <- 0.1

# Temps de début de la simulation (heures)
TEMPS_DEBH <- 0

# Durée totale de la simulation (heures)
DURATION   <- 84

# Pas de temps de calcul (secondes)
TIME_STEP  <- 5

# Période d'export des résultats graphiques :
# -1 = Valeurs maximales (enveloppe Hmax, Vmax, etc.)
#  0 = Uniquement dernier pas de temps
# >0 = Intervalle d'archivage régulier en minutes (ex: 5, 10, 15, 60)
PRINTPERIO <- -1

# Flag de suivi des étapes pré-traitement (0 = initialisation)
STEP_PRE   <- 0
