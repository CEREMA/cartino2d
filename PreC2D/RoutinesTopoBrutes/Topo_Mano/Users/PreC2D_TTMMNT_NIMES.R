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
# nomLignes3D="D:/Nabil/Nimes/_MNT2/merged_z2.gpkg"
nomLignes3D="D:/Nabil/Nimes/Final/MNT_modif_2bon.shp"
##### Nom du fichier de rives/ligne de contrainte
##### Structure à définir
# nomRives="D:/Nabil/Nimes/_MNT2/berges_arches.gpkg"
nomRives=""
# Table d'assemblage du Lidar sur lequel sera pris la donné MNT (si Type =0)
nomTA_LidarBase="D:/06_MNTTIN_FILINO/TA_TIN_Filino.shp"
# Table d'assemblage utilisée pour modifier le MNT
### DANGEREUX si mêmes données que précédement
nomTA_LidarC2D="D:/MNT_Nimes_Modif/TA_TIN_Filino.shp"#"C:/Cartino2D/France/_MNT/MTP_CLL_2024/TA_MTP_CLL_2024.gpkg"
# Racine des dalles MNT
raci_exp="MNT_Nimes"#
# Système de projection
EPSG=2154
# Buffer pour le tampon si non integré dans un champ "DistBuf" de nomTampon
DistBuf=6
# Buffer pour le tampon des Semi Points3D
DistBufSemPts3D=50
AireInterieurMin=10000
# valeur de début du NBief si non integré dans un champ "nomBief" de nomTampon
NBief=1 # si non integré dans champ nom Bief, permet de mettre un n° si on traite plusieurs jeux de données
# Nom du secteur GRASS
SecteurGRASS_="C:/GRASSDATA/PontRivCanaux/PRC"
# Resolution
reso=0.5