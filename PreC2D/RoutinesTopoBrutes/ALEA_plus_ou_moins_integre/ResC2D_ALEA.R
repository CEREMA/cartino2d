
cat("\014") # Nettoyage de la console
chem_routine=R.home(component = "Cerema")
source(file.path(chem_routine,"PreC2D","PreC2D_Outils.R"))
source(file.path(chem_routine,"FILINO","filino","RFILES","FILINO_Utils.R"))
################################################################################
############## PreC2D_general ##################################################
################################################################################
qgis_process <- "C:/QGIS/bin/qgis_process-qgis-ltr.bat"
OSGeo4W_path="C://QGIS//OSGeo4W.bat"
# Lien vers le logiciel GRASS
BatGRASS="C:\\QGIS\\bin\\grass84.bat"
EPSG=2154
SecteurGRASS_="C:/GRASSDATA/PreC2D/Temp"
Doss_Qml  = file.path("C2D","_Cartino2D_Qml_et_autre")

dsnDPE="C:\\Cartino2D\\France\\_Results"
raci="MTPCLL2024_GMSHtSceMaxMedContFrance"
# dsnDPE="C:\\AFFAIRES\\Montpellier\\14_ALEA20240715\\Res20240715_1120\\ALEA"
dsnDPE="C:\\AFFAIRES\\Montpellier\\14_ALEA20240715\\Res20240808_MaJ\\ALEA"
raci="A_4_PPRi";ValRaster=1:3
raci="A_4_Resi";ValRaster=1
dsnDPE="C:\\Cartino2D\\France\\MTPCLL2024_MOSSON\\C03_240638km_X765849Y6278594\\Post\\ALEA"
raci="A_0_hyeto_C03_240638km_X765849Y6278594_PQN_00h_00m_28h_00m_Q_PPRi_";ValRaster=1:3
raci="A_0_hyeto_C03_240638km_X765849Y6278594_PQN_00h_00m_28h_00m_Q_Resi_";ValRaster=1

NomMasque="C:\\Cartino2D\\France\\MTPCLL2024_MOSSON\\C03_240638km_X765849Y6278594\\Post\\Commune.gpkg"
# NomMasque="C:\\AFFAIRES\\Montpellier\\14_ALEA20240715\\CLL.gpkg"
# NomMasque="C:\\Cartino2D\\France\\ajeter.gpkg"
# mettre NomMasque="" si on ne veut pas de masques
Reso_Ini=1

buf=1.1*Reso_Ini/2
seuilSup0=100
BufPlus=3*Reso_Ini
BufMoins=2*Reso_Ini

###############################################################################
SecteurGRASS=paste0(dirname(SecteurGRASS_),format(Sys.time(),format="%Y%m%d_%H%M%S"),'/',format(Sys.time(),format="%Y%m%d_%H%M%S"),"/",basename(SecteurGRASS_))
unlink(dirname(SecteurGRASS),recursive=TRUE)
system(paste0(BatGRASS," -c EPSG:",EPSG," ",dirname(SecteurGRASS)," --text"))
system(paste0(BatGRASS," -c ",SecteurGRASS," --text"))

# Ouverture de HWH, HSF et HWT
nomALEAg="ALEA"
cmd=paste0("r.in.gdal -o --quiet --overwrite input=",file.path(dsnDPE,paste0(raci,".gpkg"))," output=",nomALEAg)
print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))

# Limitation de la région de travail
cmd=paste0("g.region --quiet --overwrite raster=",nomALEAg)
print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))

if (nchar(NomMasque)>0)
{
  nommasqueG="MasqueG"
  cmd=paste0("v.in.ogr  -o --quiet --overwrite input=",NomMasque," output=",nommasqueG)
  print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
}

for (ialea in ValRaster)
{
  if (nchar(NomMasque)>0)
  {
    cmd=paste0("g.region --quiet --overwrite vector=",nommasqueG," align=",nomALEAg)
    print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
    
    cmd=paste0("r.mask --quiet --overwrite vector=",nommasqueG)
    print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
  }
  
  #-------------------- ZI
  nomInond=paste0("ZI",ialea)
  exp=paste0(nomInond," =if( ",nomALEAg," >=",ialea,",1,null())")
  cmd=paste0("r.mapcalc --overwrite ",shQuote(exp))
  print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
  
  #----- Enleve le masque avec buffer le masque
  cmd=paste0("r.mask -r")
  print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
  
  BufPlusMoins(nomInond,seuilSup0,BufPlus,BufMoins,"",dsnDPE,0,1)
}
