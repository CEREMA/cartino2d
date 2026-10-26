library(sf)
cat("\014")
# code EPSG du projet
EPSG = 2154
# Nom du secteur GRASS # il pourrait être automatique
SecteurGRASS="C:/GRASSDATA/MTP/CLL2014"
# Lien vers le logiciel GRASS
BatGRASS="C:\\QGIS\\bin\\grass84.bat"


nomMNTC2D="C:\\Cartino2D\\France\\_MNT\\MTP_CLL\\MTP_CLL_202204_StRoch.gpkg"
nomMNTC2D="C:\\AFFAIRES\\Montpellier\\Topographie2024\\RGE_Alti_202406.vrt"
nomMNTHD="C:\\AFFAIRES\\FILINO_Travail\\06_MNTTIN_FILINO\\D34_LEZ_MOSSON_COMPLET_TIN_Filino.gpkg"


ValSeuil=0.25 #0
distbuf=2
seuilAire=3
resol=0.5
nfiltre=5

#Creation d'un monde GRASS
unlink(dirname(SecteurGRASS),recursive=TRUE)
system(paste0(BatGRASS," -c EPSG:",EPSG," ",dirname(SecteurGRASS)," --text"))
system(paste0(BatGRASS," -c ",SecteurGRASS," --text"))


# Importation raster dans Grass
nomC2Dg="MNTC2D"
cmd=paste0("r.in.gdal -o --quiet --overwrite input=",nomMNTC2D," output=",nomC2Dg)
print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))

# g.region 
cmd=paste0("g.region -a --overwrite --quiet"," raster=",nomC2Dg)
print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))

# Importation raster dans Grass
nomHDg="MNTHD"
cmd=paste0("r.in.gdal -o --quiet --overwrite -r input=",nomMNTHD," output=",nomHDg)
print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))

# g.region 
cmd=paste0("g.region -a --overwrite --quiet"," raster=",nomHDg)
print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))

#---- Filtre
nomHDmaximum=paste0("MNTHD_maximum",nfiltre)
# r.neighbors -c input=MNTHD@CLL2014 output=MNTHDMax7 size=7 method=maximum       
cmd=paste0("r.neighbors --quiet --overwrite -c input=",nomHDg," output=",nomHDmaximum," size=",nfiltre," method=maximum")
print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))

nomHDminimum=paste0("MNTHD_minimum",nfiltre)
cmd=paste0("r.neighbors --quiet --overwrite -c input=",nomHDg," output=",nomHDminimum," size=",nfiltre," method=minimum")
print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))

nomECART="ECART3"
cmd=paste0("r.mapcalc --overwrite ",shQuote(paste0(nomECART,"=if(",nomHDmaximum," + ",ValSeuil," < ",nomC2Dg," ,1,if(",nomHDminimum," - ",ValSeuil," > ",nomC2Dg,",1,null()))")))
print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))

# r.buffer
nomECARTBUF="ECART3BUF"
cmd=paste0("r.buffer --quiet --overwrite input=",nomECART," output=",nomECARTBUF," distance=",((nfiltre+1)/2)*resol)
print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))

nomZONE="ECART4"
cmd=paste0("r.mapcalc --overwrite ",shQuote(paste0(nomZONE,"=if(",nomECARTBUF," >= ",0," ,1,null())")))
print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))

#r.to.vect
cmd=paste0("r.to.vect --quiet --overwrite input=",nomZONE," output=",nomZONE," type=area")
print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))


nomZONEclean=paste0("ECART4clean100_f",nfiltre)
cmd=paste0("v.clean --quiet --overwrite input=",nomZONE," output=",nomZONEclean," type=area tool=rmarea threshold=",100)
print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))

#export
cmd=paste0("v.out.ogr --quiet --overwrite input=",nomZONEclean," output=",file.path(dirname(nomMNTC2D),paste0(nomZONEclean,".gpkg"))," format=GPKG")
print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))


a=ez
raci=basename(nomDIFF)
raci=substr(raci,1,nchar(raci)-4)

nomDIFFg="DIFF"
# Importation raster dans Grass
cmd=paste0("r.in.gdal -o --quiet --overwrite input=",nomDIFF," output=",nomDIFFg)
print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))

# g.region 
cmd=paste0("g.region -a --overwrite --quiet"," raster=",nomDIFFg)
print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))

PlusMoins=rbind(cbind(">","Plus",""),
                cbind("<","Moins","-"),
                cbind("!=","DIFF",""))
for (isens in 1:dim(PlusMoins)[1]) 
{
  nomsens=paste0(raci,PlusMoins[isens,2],ValSeuil)
  # r.mapcalc
  cmd=paste0("r.mapcalc --overwrite ",shQuote(paste0(nomsens,"=if(",nomDIFFg," ",PlusMoins[isens,1]," ",PlusMoins[isens,3],ValSeuil,",1,null())")))
  print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
  
  
  ####### Rajouter un filtre
  # a=ez
  # r.neighbors input=Diff_MNT2022stroch_moins_LidarHDFilinoPlus0@CLL2014 output=compte size=5 method=sum
  # r.mapcalc expression=compte_20 = if( compte@CLL2014 >20
  # r.buffer
  nombuf=paste0(raci,PlusMoins[isens,2],ValSeuil,'_BUF')
  cmd=paste0("r.buffer --quiet --overwrite input=",nomsens," output=",nombuf," distance=",distbuf)
  print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
  
  # r.mask
  cmd=paste0("r.mask -i --quiet --overwrite raster=",nombuf)
  print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
  
  # r.resample
  nominv=paste0(raci,PlusMoins[isens,2],ValSeuil,'_INV')
  cmd=paste0("r.resample --quiet --overwrite input=","MASK"," output=",nominv)
  print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
  
  # Suppression du masque
  cmd=paste0("r.mask -r")
  print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
  
  # r.buffer
  nominvbuf=paste0(raci,PlusMoins[isens,2],ValSeuil,'_INVBUF')
  cmd=paste0("r.buffer --quiet --overwrite input=",nominv," output=",nominvbuf," distance=",distbuf)
  print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
  
  # r.mask
  cmd=paste0("r.mask -i --quiet --overwrite raster=",nominvbuf)
  print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
  
  #r.to.vect
  cmd=paste0("r.to.vect --quiet --overwrite input=","MASK"," output=",nomsens," type=area")
  print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
  
  # Suppression du masque
  cmd=paste0("r.mask -r")
  print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
  
  #export
  cmd=paste0("v.out.ogr --quiet --overwrite input=",nomsens," output=",file.path(dirname(nomDIFF),paste0(nomsens,".gpkg"))," format=GPKG")
  print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
  
  Vect=st_read(file.path(dirname(nomDIFF),paste0(nomsens,".gpkg")))
  Vect$AIRE=st_area(Vect)
  units(Vect$AIRE)=NULL
  
  Vect=Vect[which(Vect$AIRE>seuilAire),]
  st_write(Vect,file.path(dirname(nomDIFF),paste0(nomsens,".gpkg")),delete_layer = T, quiet = T)
}