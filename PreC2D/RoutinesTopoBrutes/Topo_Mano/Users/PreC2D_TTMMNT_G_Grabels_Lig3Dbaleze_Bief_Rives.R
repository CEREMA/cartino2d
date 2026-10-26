# ATTENTION AUX / et pas //
# Multi-Polygones avec le champ "NBief" et si besoin "NOM"
nomBief="C:/AFFAIRES/Topo_Mano_Examples/NBiefs.shp"
# MutliPoints3D
nomPts3D=""
# Multi-Polygones avec possibilité du champ "NBief" et "DistBuf"
nomTampon=""
# Nom Mini-semi-Points (ex:Bathy hydromètres Aude)
nomSemiPts3D="C:/AFFAIRES/Topo_Mano_Examples/H_Aude_SemiPts3D/Points_bathy_1ere_campagne.shp"
# Mutli-Lignes3D avec les champs "Type"     "NBief"    "ZsiTYpe1" "NSection"
nomLignes3D="C:/AFFAIRES/Topo_Mano_Examples/G_Grabels_Lig3Dbaleze_Bief_Rives/Lignes3D_et_2D_melange.gpkg"
##### Nom du fichier de rives/ligne de contrainte
##### Structure à définir
nomRives="C:/AFFAIRES/Topo_Mano_Examples/Rives/Rives.gpkg"
# Table d'assemblage du Lidar sur lequel sera pris la donné MNT (si Type =0)
nomTA_LidarBase="C:/Cartino2D/France/_MNT/MTP_CLL_2024/Base/TA_MTP_CLL_2024.gpkg"
# Table d'assemblage utilisée pour modifier le MNT
### DANGEREUX si mêmes données que précédement
nomTA_LidarC2D=""#"C:/Cartino2D/France/_MNT/MTP_CLL_2024/TA_MTP_CLL_2024.gpkg"
# Racine des dalles MNT
raci_exp="MNT_Cerema_Sections_Grabels"#
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