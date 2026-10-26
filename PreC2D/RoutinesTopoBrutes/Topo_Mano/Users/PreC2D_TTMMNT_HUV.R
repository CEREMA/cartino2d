# ATTENTION AUX / et pas //
# Multi-Polygones avec le champ "NBief" et si besoin "NOM"
nomBief=""
# MutliPoints3D
nomPts3D=""
# Multi-Polygones avec possibilité du champ "NBief" et "DistBuf"
nomTampon=""
# Nom Mini-semi-Points (ex:Bathy hydromètres Aude)
nomSemiPts3D=""
# Mutli-Lignes3D avec les champs "Type"     "NBief"    "ZsiTYpe1" "NSection"
nomLignes3D="C:/Cartino2D/PreC2D/PT/HUV/sectionsPRC_HUV_20260422.gpkg"
##### Nom du fichier de rives/ligne de contrainte
##### Structure à définir
nomRives="C:/Cartino2D/PreC2D/PT/HUV/berges_huv_20260420.gpkg"
# Table d'assemblage du Lidar sur lequel sera pris la donné MNT (si Type =0)
nomTA_LidarBase="J:/MAMP/06b_MNTTIN_FILINO_HUV_HEC/TA_TIN_FILINO_HECRAS.shp"
# Table d'assemblage utilisée pour modifier le MNT
### DANGEREUX si mêmes données que précédement
nomTA_LidarC2D="J:/MAMP/06b_MNTTIN_FILINO_HUV_HEC_PT/TA_TIN_FILINO_HECRAS.shp"
# Racine des dalles MNT
raci_exp=""
# Système de projection
EPSG=2154
# Buffer pour le tampon si non integré dans un champ "DistBuf" de nomTampon
DistBuf=5
# valeur de début du NBief si non integré dans un champ "nomBief" de nomTampon
NBief=1 # si non integré dans champ nom Bief, permet de mettre un n° si on traite plusieurs jeux de données
# Nom du secteur GRASS
SecteurGRASS_="C:/GRASSDATA/PontRivCanaux/PRC"
# Resolution
reso=0.5