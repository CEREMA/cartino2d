library(sf)
cat("\014")
# code EPSG du projet
EPSG = 2154

OSGeo4W_path="C://QGIS//OSGeo4W.bat"
# Nom du secteur GRASS # il pourrait être automatique
SecteurGRASS="C:/GRASSDATA/MTP/CLL2014"
# Lien vers le logiciel GRASS
BatGRASS="C:\\QGIS\\bin\\grass84.bat"


nomProjet3M="C:\\AFFAIRES\\Montpellier\\Topographie2024\\03_Topo\\Topo_3M_janvier_2024\\Projet3M_2024_Raster.vrt"
nomMNTC2D="C:\\Cartino2D\\France\\_MNT\\MTP_CLL\\MTP_CLL_202204_StRoch.gpkg"
nomRGE2022="C:\\AFFAIRES\\Montpellier\\Topographie2024\\RGE_Alti_202406.vrt"
nomMNTHD="C:\\AFFAIRES\\FILINO_Travail\\06_MNTTIN_FILINO\\D34_LEZ_MOSSON_COMPLET_TIN_Filino.gpkg"

Zones_Min     ="C:\\AFFAIRES\\Montpellier\\Topographie2024\\Minimum.gpkg"
Zones_Max     ="C:\\AFFAIRES\\Montpellier\\Topographie2024\\Maximum.gpkg"
ZoneRGE2022   ="C:\\AFFAIRES\\Montpellier\\Topographie2024\\Zones_RGE2024_V2.gpkg"
ZoneMNTC2D2022="C:\\AFFAIRES\\Montpellier\\Topographie2024\\Zones_MNTC2D.gpkg"

nomSecteurs   ="C:\\AFFAIRES\\Montpellier\\Topographie2024\\LimiteMTPCLL2024.gpkg"

nomMNTC2D_2024="C:\\Cartino2D\\France\\_MNT\\MTP_CLL\\MTP_CLL_C2D_2024.gpkg"

nproc=8 # taudem
NomTIF    =paste0(substr(nomMNTC2D_2024,1,nchar(nomMNTC2D_2024)-4),"tif")
NomMNTFill=paste0(substr(nomMNTC2D_2024,1,nchar(nomMNTC2D_2024)-4),"_fill.tif")
NomMNTCuv =paste0(substr(nomMNTC2D_2024,1,nchar(nomMNTC2D_2024)-4),"_cuvettes.gpkg")
# ----------------- Ordre de priorité
#1 Projet 3M                      nomProjet3M
# 2 Maximum (digues, barrages)    max(nomMNTC2D nomRGE2022 nomMNTHD)
# 3 Minimum                       min(nomMNTC2D nomRGE2022 nomMNTHD)
# 4 RGE2022                       nomRGE2022
# 5 MNTC2D des 1ers alées en 2022 nomMNTC2D
# 6 LidarHD Filino                nomMNTHD

#Creation d'un monde GRASS
unlink(dirname(SecteurGRASS),recursive=TRUE)
system(paste0(BatGRASS," -c EPSG:",EPSG," ",dirname(SecteurGRASS)," --text"))
system(paste0(BatGRASS," -c ",SecteurGRASS," --text"))

# Importation raster dans Grass
nomProjet3M_g="nomProjet3M"
nomMNTC2D_g="nomMNTC2D"
nomRGE2022_g="nomRGE2022"
nomMNTHD_g="nomMNTHD"

cmd=paste0("r.in.gdal -o --quiet --overwrite input=",nomProjet3M," output=",nomProjet3M_g)
print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
cmd=paste0("r.in.gdal -o --quiet --overwrite input=",nomMNTC2D," output=",nomMNTC2D_g)
print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
cmd=paste0("r.in.gdal -o --quiet --overwrite input=",nomRGE2022," output=",nomRGE2022_g)
print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
cmd=paste0("r.in.gdal -o --quiet --overwrite input=",nomMNTHD," output=",nomMNTHD_g)
print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))

# g.region 
cmd=paste0("g.region -a --overwrite --quiet"," raster=",nomMNTHD_g)
print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))

# importation des zones
Zones_Min_g     ="Zones_Min"
Zones_Max_g     ="Zones_Max"
ZoneRGE2022_g   ="ZoneRGE2022"
ZoneMNTC2D2022_g="ZoneMNTC2D2022"
Secteurs_g="Secteurs"

cmd=paste0("v.in.ogr -o --quiet --overwrite input=",Zones_Min," output=",Zones_Min_g)
print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
cmd=paste0("v.in.ogr -o --quiet --overwrite input=",Zones_Max," output=",Zones_Max_g)
print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
cmd=paste0("v.in.ogr -o --quiet --overwrite input=",ZoneRGE2022," output=",ZoneRGE2022_g)
print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
cmd=paste0("v.in.ogr -o --quiet --overwrite input=",ZoneMNTC2D2022," output=",ZoneMNTC2D2022_g)
print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))

cmd=paste0("v.in.ogr -o --quiet --overwrite input=",nomSecteurs," output=",Secteurs_g)
print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))

# calcul du # 2 Maximum (digues, barrages)    max(nomMNTC2D nomRGE2022 nomMNTHD)
# g.region 
Secteurs_g
cmd=paste0("g.region -a --overwrite --quiet"," vector=",Secteurs_g)
print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
# r.mask
cmd=paste0("r.mask --quiet --overwrite vector=",Zones_Max_g)
print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
# g.region 
cmd=paste0("g.region -a --overwrite --quiet"," raster=","MASK")
print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
Maxim="Maxim"
cmd=paste0("r.series --overwrite input=",nomMNTC2D_g,",",nomRGE2022_g,",",nomMNTHD_g," output=",Maxim," method=maximum")
print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
cmd=paste0("r.mask -r --quiet --overwrite")
print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))

# calcul du # 3 Minimum                       min(nomMNTC2D nomRGE2022 nomMNTHD)
# g.region 
cmd=paste0("g.region -a --overwrite --quiet"," vector=",Secteurs_g)
print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
cmd=paste0("r.mask --quiet --overwrite vector=",Zones_Min_g)
print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
# g.region 
cmd=paste0("g.region -a --overwrite --quiet"," raster=","MASK")
print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
Minim="Minim"
cmd=paste0("r.series --overwrite input=",nomMNTC2D_g,",",nomRGE2022_g,",",nomMNTHD_g," output=",Minim," method=minimum")
print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
cmd=paste0("r.mask -r --quiet --overwrite")
print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))

# ZoneRGE2022
# g.region 
cmd=paste0("g.region -a --overwrite --quiet"," vector=",Secteurs_g)
print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
cmd=paste0("r.mask --quiet --overwrite vector=",ZoneRGE2022_g)
print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
RGE2022_Final="RGE2022_Final"
cmd=paste0("r.resample --quiet --overwrite input=",nomRGE2022_g," output=",RGE2022_Final)
print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
cmd=paste0("r.mask -r --quiet --overwrite")
print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))

# ZoneMNTC2D2022
# g.region 
cmd=paste0("g.region -a --overwrite --quiet"," vector=",Secteurs_g)
print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
cmd=paste0("r.mask --quiet --overwrite vector=",ZoneMNTC2D2022_g)
print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
C2D2022_Final="C2D2022_Final"
cmd=paste0("r.resample --quiet --overwrite input=",nomMNTC2D_g," output=",C2D2022_Final)
print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
cmd=paste0("r.mask -r --quiet --overwrite")
print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))

# Patch 
cmd=paste0("g.region -a --overwrite --quiet"," vector=",Secteurs_g)
print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
cmd=paste0("r.mask --quiet --overwrite vector=",Secteurs_g)
print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
nom_MNTC2D2024_g = "MNTC2D2024"
cmd=paste0("r.patch --quiet --overwrite ","input=",nomProjet3M_g,",",Maxim,",",Minim,",",RGE2022_Final,",",C2D2022_Final,",",nomMNTHD_g," output=",nom_MNTC2D2024_g)
print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))

#---- Export
cmd=paste0("r.out.gdal --quiet --overwrite -c -f input=",nom_MNTC2D2024_g," output=",nomMNTC2D_2024," type=Float32 format=GPKG nodata=-9999")
print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd)) 
cmd = paste0(shQuote(OSGeo4W_path)," gdaladdo ","--config OGR_SQLITE_SYNCHRONOUS OFF ", "-r AVERAGE ",nomMNTC2D_2024," 2 4 8 16 32 64 128 256")
print(cmd);system(cmd)

cmd=paste0("r.out.gdal --quiet --overwrite -c -f input=",nom_MNTC2D2024_g," output=",NomTIF," format=GTiff nodata=-9999")
print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd)) 
cmd_str = paste("mpiexec -n", nproc, "PitRemove", "-z", NomTIF, "-fel", NomMNTFill)
system(cmd_str)

nomFill="MNTFill"
cmd=paste0("r.in.gdal -o --overwrite input=",NomMNTFill," output=",nomFill)
print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd)) 

nomCuv="Cuv"
cmd=paste0("r.mapcalc --overwrite ",shQuote(paste0(nomCuv," =",nomFill,"-",nom_MNTC2D2024_g)))
print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))

cmd=paste0("r.out.gdal --quiet --overwrite -c -f input=",nomCuv," output=",NomMNTCuv," type=Float32 format=GPKG nodata=-9999")
print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))

cmd = paste0(shQuote(OSGeo4W_path)," gdaladdo ","--config OGR_SQLITE_SYNCHRONOUS OFF ", "-r AVERAGE ",NomMNTCuv," 2 4 8 16 32 64 128 256")
system(cmd)

unlink(dirname(SecteurGRASS),recursive=TRUE)