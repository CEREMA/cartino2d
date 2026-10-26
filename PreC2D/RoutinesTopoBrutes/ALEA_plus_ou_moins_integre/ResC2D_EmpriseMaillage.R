
cat("\014") # Nettoyage de la console
chem_routine=dirname(rstudioapi::getActiveDocumentContext()$path)
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

# dsnDPE="C:\\Cartino2D\\France\\_Results"
# raci="MTPCLL2024_GMSHtSceMaxMedContFrance"

NomMasque="C:\\Cartino2D\\France\\Cartino2D_Secteur_MAMP_20240725_1526FUS.gpkg"

Parametre=rbind(
  cbind("C:\\Cartino2D\\France\\MAMP2024_05m\\_FUSION" ,"MAMP2024_05mtSceMaxMedContFrance_" ,5),
  cbind("C:\\Cartino2D\\France\\MAMP2024_GMSH_v0\\_FUSION","MAMP2024_GMSHtSceMaxMedContFrance_",1)
)
nomtmp_Buf4=""

NomTIF="C:\\Cartino2D\\France\\_Exzeco\\EmpriseMAMP_05_et_GMSH.tif"

###############################################################################
SecteurGRASS=paste0(dirname(SecteurGRASS_),format(Sys.time(),format="%Y%m%d_%H%M%S"),'/',format(Sys.time(),format="%Y%m%d_%H%M%S"),"/",basename(SecteurGRASS_))
unlink(dirname(SecteurGRASS),recursive=TRUE)
system(paste0(BatGRASS," -c EPSG:",EPSG," ",dirname(SecteurGRASS)," --text"))
system(paste0(BatGRASS," -c ",SecteurGRASS," --text"))

if (nchar(NomMasque)>0)
{
  nommasqueG="MasqueG"
  cmd=paste0("v.in.ogr  -o --quiet --overwrite input=",NomMasque," output=",nommasqueG)
  print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
}
  Reso_Final=5
for (ip in 1:nrow(Parametre))
{
  dsnDPE=Parametre[ip,1]
  raci=Parametre[ip,2]
  Reso_Ini=as.numeric(Parametre[ip,3])# 5 si calcul 5m, 1 si calcul destructuré 3m GMSH
  

  ################## Paramétrage pour calcul 5m
  if (Reso_Ini==1)
  {
    # paramétrage des seuils pour hauter/débit linéique/temps
    HSF_mini=0.05
    HWH_mini=0.1
    TpicPluie_h=8 # en heure
    DecalPicH_min=25 # en minutes
    DeltaPicH_PluieDebit=5 # en minutes
    Extens=rbind(cbind("HSF","HWH","HWT"),
                 cbind("_m2_s","_m","_s"),
                 cbind(HSF_mini,HWH_mini,(TpicPluie_h+DecalPicH_min/60)*3600))
    
    buf=1.1*Reso_Ini/2
    seuilSup0=200
    BufPlus=5*Reso_Ini
    BufMoins=3*Reso_Ini
  }
  if (Reso_Ini==5)
  {
    ################## Paramétrage pour calcul 5m
    # paramétrage des seuils pour hauter/débit linéique/temps
    HSF_mini=0.05
    HWH_mini=0.1
    TpicPluie_h=8 # en heure
    DecalPicH_min=25 # en minutes
    DeltaPicH_PluieDebit=5 # en minutes
    Extens=rbind(cbind("HSF","HWH","HWT"),
                 cbind("_m2_s","_m","_s"),
                 cbind(HSF_mini,HWH_mini,(TpicPluie_h+DecalPicH_min/60)*3600))
    # Résolution, moitité résolution de base
    Reso_Ini=1
    # NE PAS TOUCHER LES NOMS DESSOUS
    buf=1.1*Reso_Ini/2
    # suppression des bassins versants mini
    seuilSup0=200
    BufPlus=3*Reso_Ini
    BufMoins=2*Reso_Ini
  }
  
  
  
  # Ouverture de HWH, HSF et HWT
  for (i in 1:2)
  {
    # Import du raster
    if (file.exists(file.path(dsnDPE,paste0(raci,Extens[1,i],Extens[2,i],".gpkg")))==F)
    {cat(file.path(dsnDPE,paste0(raci,Extens[1,i],Extens[2,i],".gpkg"))," n'existe pas","\n");boom=modifdsnDPEetraci}
    cmd=paste0("r.in.gdal -o --quiet --overwrite input=",file.path(dsnDPE,paste0(raci,Extens[1,i],Extens[2,i],".gpkg"))," output=",Extens[1,i])
    print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
  }
  
  # Limitation de la région de travail
  cmd=paste0("g.region --quiet --overwrite raster=",Extens[1,1],",",Extens[1,2])
  print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
  
  if (nchar(NomMasque)>0)
  {
    cmd=paste0("g.region --quiet --overwrite vector=",nommasqueG," align=",Extens[1,1])
    print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
    
    cmd=paste0("r.mask --quiet --overwrite vector=",nommasqueG)
    print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
  }
  #-------------------- ZI
  nomInond=paste0("ZI",ip)
  exp=paste0(nomInond," =if(",Extens[1,1],">",Extens[3,1]," || ",
             Extens[1,2],">",Extens[3,2],",1,null())")
  cmd=paste0("r.mapcalc --overwrite ",shQuote(exp))
  print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
  
  #----- Enleve le masque avec buffer le masque
  cmd=paste0("r.mask -r")
  print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
  
  nomtmp_Buf4=BufPlusMoins(nomInond,seuilSup0,BufPlus,BufMoins,"",dsnDPE,5,1)
  
  listeRg=ifelse(ip==1,nomtmp_Buf4,paste0(listeRg,",",nomtmp_Buf4))
}

if (nrow(Parametre)>1)
{
  cmd=paste0("g.region --overwrite --quiet -a --overwrite raster=",listeRg," res=",as.character(Reso_Final))
  print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
  
  nomR_fus="Fusion"
  cmd=paste0("r.series --overwrite input=",listeRg," output=",nomR_fus," method=maximum")
  print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
  
  cmd=paste0("r.out.gdal --quiet --overwrite -c -f input=",nomR_fus," output=",NomTIF," format=GTiff")
  print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd)) 
  
  # Polygoniser
  nomvectsortie=paste0(substr(NomTIF,1,nchar(NomTIF)-4),"_brut.gpkg")
  cmd <- paste0(qgis_process, " run gdal:polygonize",
                " --distance_units=meters --area_units=m2 --ellipsoid=EPSG:7019",
                " --INPUT=", shQuote(NomTIF),
                " --BAND=1 --FIELD=DN --EIGHT_CONNECTEDNESS=false --EXTRA=",
                " --OUTPUT=", shQuote(nomvectsortie))
  print(cmd); system(cmd)
}