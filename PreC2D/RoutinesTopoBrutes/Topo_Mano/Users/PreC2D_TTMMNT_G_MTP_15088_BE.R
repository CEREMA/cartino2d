# ATTENTION AUX / et pas //
# Multi-Polygones avec le champ "NBief" et si besoin "NOM"
nomBief="C:/AFFAIRES/Topo_Mano_Examples/NBiefs2.gpkg"
# MutliPoints3D
nomPts3D="C:/AFFAIRES/Montpellier/Topographie2024/Mosson/20240920/15088_BE/15088_BE_Pts3D.gpkg"
# Multi-Polygones avec possibilité du champ "NBief" et "DistBuf"
nomTampon="C:/AFFAIRES/Montpellier/Topographie2024/Mosson/20240920/15088_BE/15088_BE_Tampon.gpkg"
# Nom Mini-semi-Points (ex:Bathy hydromètres Aude)
nomSemiPts3D=""
# Mutli-Lignes3D avec les champs "Type"     "NBief"    "ZsiTYpe1" "NSection"
nomLignes3D=""
##### Nom du fichier de rives/ligne de contrainte
##### Structure à définir
nomRives="C:/AFFAIRES/Montpellier/Topographie2024/Mosson/20240920/Rives_Mosson.gpkg"
# Table d'assemblage du Lidar sur lequel sera pris la donné MNT (si Type =0)
nomTA_LidarBase=""
# Table d'assemblage utilisée pour modifier le MNT
### DANGEREUX si mêmes données que précédement
nomTA_LidarC2D=""#"C:/Cartino2D/France/_MNT/MTP_CLL_2024/TA_MTP_CLL_2024.gpkg"
# Racine des dalles MNT
raci_exp=""#
# Système de projection
EPSG=2154
# Buffer pour le tampon si non integré dans un champ "DistBuf" de nomTampon
DistBuf=5
# Buffer pour le tampon des Semi Points3D
DistBufSemPts3D=5
AireInterieurMin=500
# valeur de début du NBief si non integré dans un champ "nomBief" de nomTampon
NBief=1 # si non integré dans champ nom Bief, permet de mettre un n° si on traite plusieurs jeux de données
# Nom du secteur GRASS
SecteurGRASS_="C:/GRASSDATA/PontRivCanaux/PRC"
# Resolution
reso=0.5