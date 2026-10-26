# ====================================================================================================================
# CARTINO2D - TEMPLATE : C2D_LienOutilsPC.R
# Objet : Déclaration des chemins vers les logiciels et binaires installés sur le PC local
# ====================================================================================================================
# Instructions :
# 1. Copier ce fichier dans "C2D_ParamUser/C2D_LienOutilsPC.R"
# 2. Ajuster les chemins selon les versions logicielles et répertoires d'installation de votre ordinateur.
# 3. Ce fichier est sourcé en tout début d'exécution par "C2D_RUN.R" (ligne 43).
#
# Remarques sur la syntaxe des chemins sous Windows :
# - Utiliser des barres obliques simples "/" ou des doubles barres inverses "\\".
# - Grâce à la version unifiée de C2D_RUN.R, les chemins avec espaces (ex: "C:/Program Files/...")
#   sont supportés de manière fiable pour QGIS, GRASS et OSGeo4W (protégés automatiquement par shQuote).
# ====================================================================================================================

# --------------------------------------------------------------------------------------------------------------------
# 1. PYTHON
# --------------------------------------------------------------------------------------------------------------------
# Nom de commande (si présent dans le PATH Windows) ou chemin absolu vers python.exe
Bug_python     <- "python"


# --------------------------------------------------------------------------------------------------------------------
# 2. GMSH (Mailleur d'éléments finis)
# --------------------------------------------------------------------------------------------------------------------
# Répertoire contenant le binaire gmsh.exe (recommandé sans espace, ex: sous C:\Cartino2D\)
Bug_GMSH       <- "C:/Cartino2D/gmsh-4.15.2-Windows64"


# --------------------------------------------------------------------------------------------------------------------
# 3. QGIS & OSGeo4W (Traitements géographiques et algorithmes QGIS Process)
# --------------------------------------------------------------------------------------------------------------------
# Chemin complet vers le script de configuration environnement OSGeo4W
OSGeo4W_path   <- "C:/Program Files/QGIS 3.44.9/OSGeo4W.bat"

# Chemin complet vers l'exécutable qgis_process (mode console/batch de QGIS)
# Astuce : sous QGIS autonome, il s'agit généralement de "qgis_process-qgis-ltr.bat" ou "qgis_process-qgis.bat"
qgis_process   <- "C:/Program Files/QGIS 3.44.9/bin/qgis_process-qgis-ltr.bat"


# --------------------------------------------------------------------------------------------------------------------
# 4. PPUTILS (Routines de post-traitement et manipulation des fichiers Selafin Telemac)
# --------------------------------------------------------------------------------------------------------------------
# Répertoire racine des routines python PPutils adaptées par le Cerema
chemin_pputils <- "C:/Cartino2D/pputils"


# --------------------------------------------------------------------------------------------------------------------
# 5. TELEMAC-2D (Moteur hydrodynamique éléments finis)
# --------------------------------------------------------------------------------------------------------------------
# Répertoire d'installation de TELEMAC (ATTENTION : TELEMAC requiert un chemin SANS espace)
telemac_folder <- "C:/TELEMAC/V8P4"


# --------------------------------------------------------------------------------------------------------------------
# 6. GRASS GIS (SIG raster haute performance pour calculs topographiques et MNT)
# --------------------------------------------------------------------------------------------------------------------
# Chemin complet vers le batch de lancement de GRASS (généralement inclus dans QGIS)
# Exemples : "grass84.bat" (GRASS 8.4), "grass83.bat" (GRASS 8.3), "grass82.bat" (GRASS 8.2)
BatGRASS       <- "C:/Program Files/QGIS 3.44.9/bin/grass84.bat"


# --------------------------------------------------------------------------------------------------------------------
# 7. PSCP (Client SCP en ligne de commande PuTTY pour transferts vers clusters distants)
# --------------------------------------------------------------------------------------------------------------------
# Chemin complet vers pscp.exe (facultatif si uniquement calculs en local sur PC)
pscp_path      <- "C:/Cartino2D/pscp.exe"
