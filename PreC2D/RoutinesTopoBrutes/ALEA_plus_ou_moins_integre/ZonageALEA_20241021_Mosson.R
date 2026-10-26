library(sf)
OSGeo4W_path="C://QGIS//OSGeo4W.bat"
EPSG=2154
chem_routine=dirname(rstudioapi::getActiveDocumentContext()$path)
# Lien vers le logiciel GRASS
BatGRASS="C:\\QGIS\\bin\\grass84.bat"
####
SecteurGRASS_="C:/GRASSDATA/ALEA/Temp"

### Routine pour ALEA MTPCLL
dsnlayer_="C:\\Cartino2D\\France\\MTPCLL2024_MOSSON\\C03_240638km_X765849Y6278594\\Post"
liste=list.files(dsnlayer_,pattern="HWH_m.gpkg$")

nomG=cbind("HWH","HSF","HV")

dsnlayer=file.path(dsnlayer_,"ALEA")
if (file.exists(dsnlayer)==F){dir.create(dsnlayer)}

SecteurGRASS=paste0(dirname(SecteurGRASS_),'/',format(Sys.time(),format="%Y%m%d_%H%M%S"),"/",basename(SecteurGRASS_))
#Creation d'un monde GRASS
unlink(dirname(SecteurGRASS),recursive=TRUE)
system(paste0(BatGRASS," -c EPSG:",EPSG," ",dirname(SecteurGRASS)," --text"))
system(paste0(BatGRASS," -c ",SecteurGRASS," --text"))

for (iliste in liste)
{
  iscena=file.path(dsnlayer_,strsplit(iliste,"HWH_m")[[1]][1])
  nomRes=paste0(iscena,cbind("HWH_m.gpkg","HSF_m2_s.gpkg","HV_m_s.gpkg"))
  
  ######################################################################
  # Importer les couches hauteurs (HWH), d?bits lin?iques (HSF) et vitesse (HV)
  for (i in 1:3)
  {
    cmd=paste0("r.in.gdal -o --quiet --overwrite input=",nomRes[i]," output=",nomG[i])
    print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
  }
  # execGRASS("g.region",flags=c("quiet","overwrite"),parameters=list(raster=paste0(nomG[1],",",nomG[2])))	
  cmd=paste0("g.region -a --overwrite --quiet"," raster=",paste0(nomG[1],",",nomG[2]))
  print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
  
  ############################################################################################################################################
  # Calcule aléa grille DDTM34 partout sans se poser de question de seuil
  nomALEA0=paste0("A_0_",basename(iscena)) #"ALEAGrilleDDTM34"
  exp=paste0(nomALEA0,"=if( ",nomG[1]," < ",0.3,",if( ",nomG[3]," < ",0.5,",",1,",",2,"),",
             "if( ",nomG[1]," > ",1,",",3,",",2,"))")
  cmd=paste0("r.mapcalc --overwrite ",shQuote(exp))
  print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
  
  nomsortie=file.path(dsnlayer,paste0(nomALEA0,".gpkg"))
  cmd=paste0("r.out.gdal --quiet --overwrite -c -f input=",nomALEA0," output=",nomsortie," type=Float32 format=GPKG nodata=-9999")
  print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
  
  cmd = paste0(shQuote(OSGeo4W_path)," gdaladdo ","--config OGR_SQLITE_SYNCHRONOUS OFF ", "-r AVERAGE ",nomsortie," 2 4 8 16 32 64 128 256")
  print(cmd);system(cmd)
}
