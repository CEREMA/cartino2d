library(sf)
OSGeo4W_path="C://QGIS//OSGeo4W.bat"
EPSG=2154
chem_routine=dirname(rstudioapi::getActiveDocumentContext()$path)
# Lien vers le logiciel GRASS
BatGRASS="C:\\QGIS\\bin\\grass84.bat"
####
SecteurGRASS_="C:/GRASSDATA/ALEA/Temp"


### Routine pour ALEA MTPCLL
dsnlayer_="C:\\AFFAIRES\\Montpellier\\Res20240715_1120"
liste=list.files(dsnlayer_,pattern="HWH_m.gpkg$")

Scena=file.path(dsnlayer_,substr(liste,1,nchar(liste)-10))
print(Scena)

nomG=cbind("HWH","HSF","HV")


dsnlayer=file.path(dsnlayer_,"ALEA")
######################################################################
# Param?tres
BorneAlea=cbind(0.1, # HWH >
                0.05)  # HSF >
# BorneAlea=BorneAlea/3
BorneDCE=cbind(0.25, # HWH >
               0.1)  # HSF >
# BorneDCE=BorneDCE/3
distBuf=5

AireMin=10000
# AireMin=50
units(AireMin)="m^2"
######################################################################

if (file.exists(dsnlayer)==F){dir.create(dsnlayer)}


SecteurGRASS=paste0(dirname(SecteurGRASS_),'/',format(Sys.time(),format="%Y%m%d_%H%M%S"),"/",basename(SecteurGRASS_))
#Creation d'un monde GRASS
unlink(dirname(SecteurGRASS),recursive=TRUE)
system(paste0(BatGRASS," -c EPSG:",EPSG," ",dirname(SecteurGRASS)," --text"))
system(paste0(BatGRASS," -c ",SecteurGRASS," --text"))

for (iscena in Scena)
{
  nomRes=paste0(iscena,cbind("HWH_m.gpkg","HSF_m2_s.gpkg","HV_m_s.gpkg"))
  
  ######################################################################
  # Importer les couches hauteurs (HWH), d?bits lin?iques (HSF) et vitesse (HV)
  for (i in 1:3)
  {
    # execGRASS("r.in.gdal",flags=c("o","quiet","overwrite"),parameters=list(input=,output=nomG[i]))
    # Importation raster dans Grass
    cmd=paste0("r.in.gdal -o --quiet --overwrite input=",nomRes[i]," output=",nomG[i])
    print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
  }
  # execGRASS("g.region",flags=c("quiet","overwrite"),parameters=list(raster=paste0(nomG[1],",",nomG[2])))	
  cmd=paste0("g.region -a --overwrite --quiet"," raster=",paste0(nomG[1],",",nomG[2]))
  print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
  
  
  ############################################################################################################################################
  ######################################################################
  # Calcule aléa grille DDTM34 partout sans se poser de question de seuil
  nomALEA0=paste0("A_0_",basename(iscena)) #"ALEAGrilleDDTM34"
  exp=paste0(nomALEA0,"=if( ",nomG[1]," < ",0.3,",if( ",nomG[3]," < ",0.5,",",1,",",2,"),",
             "if( ",nomG[1]," > ",1,",",3,",",2,"))")
  # execGRASS("r.mapcalc",flags=c("quiet","overwrite"),parameters=list(expression=exp))
  cmd=paste0("r.mapcalc --overwrite ",shQuote(exp))
  print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
  
  
  NomUnivar=file.path(dsnlayer,paste0(nomALEA0,".txt"))
  # execGRASS("r.univar",flags=c("quiet","overwrite"),parameters=list(map=nomALEA0, output=nomsortie))
  # NomUnivar = file.path(dsnlayerC, "univar.txt")
  cmd=paste0("r.univar --quiet --overwrite map=",nomALEA0," output=",NomUnivar)
  print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
  
  ######################################################################
  # Export grille al?e DDTM34 sur tout le territoire (pas de diff?rence DCE/ruissellement)
  nomsortie=file.path(dsnlayer,paste0(nomALEA0,".gpkg"))
  # execGRASS("r.out.gdal",flags=c("quiet","overwrite","c","f"),
  #           parameters=list(input=nomALEA0,output=nomsortie,type="Float32",format="GPKG",nodata=-9999))
  cmd=paste0("r.out.gdal --quiet --overwrite -c -f input=",nomALEA0," output=",nomsortie," type=Float32 format=GPKG nodata=-9999")
  print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
  # cmd=paste0(shQuote("C:\\OSGeo4W64\\bin\\gdaladdo")," --config OGR_SQLITE_SYNCHRONOUS OFF -r AVERAGE ",shQuote(nomsortie),"  2 4 8 16 32 64 128 256")
  # system(cmd)
  cmd = paste0(shQuote(OSGeo4W_path)," gdaladdo ","--config OGR_SQLITE_SYNCHRONOUS OFF ", "-r AVERAGE ",nomsortie," 2 4 8 16 32 64 128 256")
  print(cmd);system(cmd)
  
  ############################################################################################################################################
  ######################################################################
  # Calcule al?a grille DDTM34 avec mini-seuil sans distinction D?bordement de cours d'eau/ruissellement
  ######################################################################
  # La mod?le Telemac forc? par la pluie met de l'eau partout, il est n?cessaire de filtrer l'al?a sur des valeurs basses
  # Pour filter l'al?a, on choisit une hauteur (HWH) > Hmin et un d?bit lin?ique (HSF)>Qlmin
  # Par exemple, si plus de 10 cm d'eau ou un d?bit lin?ique de plus de 0.05m3/s/ml 
  nomALEA1=paste0("A_1_",basename(iscena),"_Sup","_HWH",gsub("[[:punct:]]", "_",BorneAlea[1]),"m_HSF",gsub("[[:punct:]]", "_",BorneAlea[2]),"m")
  exp=paste0(nomALEA1,"=if( ",nomG[1]," > ",BorneAlea[1]," , ",nomALEA0," ,if( ",nomG[2]," > ",BorneAlea[2]," , ",nomALEA0," ,null()),null())")
  # execGRASS("r.mapcalc",flags=c("quiet","overwrite"),parameters=list(expression=exp))
  cmd=paste0("r.mapcalc --overwrite ",shQuote(exp))
  print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
  
  nomsortie=file.path(dsnlayer,paste0(nomALEA1,".gpkg"))
  # execGRASS("r.out.gdal",flags=c("quiet","overwrite","c"),
  #           parameters=list(input=nomALEA1,output=nomsortie,type="Float32",format="GPKG",nodata=-9999))
  cmd=paste0("r.out.gdal --quiet --overwrite -c -f input=",nomALEA1," output=",nomsortie," type=Float32 format=GPKG nodata=-9999")
  print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
  
  cmd = paste0(shQuote(OSGeo4W_path)," gdaladdo ","--config OGR_SQLITE_SYNCHRONOUS OFF ", "-r AVERAGE ",nomsortie," 2 4 8 16 32 64 128 256")
  print(cmd);system(cmd)
  
  ######################################################################
  #----- Cr?ation dpour ne garder que Fort et TF de l'ALEA1
  nomALEA1FTF=paste0(nomALEA1,"FTF")
  exp=paste0(nomALEA1FTF,"=if( ",nomALEA1," > ",1,", ",nomALEA1," ,null())")
  # execGRASS("r.mapcalc",flags=c("quiet","overwrite"),parameters=list(expression=exp))
  cmd=paste0("r.mapcalc --overwrite ",shQuote(exp))
  print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
  
  nomsortie=file.path(dsnlayer,paste0(nomALEA1FTF,".gpkg"))
  # execGRASS("r.out.gdal",flags=c("quiet","overwrite","c"),
  #           parameters=list(input=nomALEA1FTF,output=nomsortie,type="Float32",format="GPKG",nodata=-9999))
  cmd=paste0("r.out.gdal --quiet --overwrite -c -f input=",nomALEA1FTF," output=",nomsortie," type=Float32 format=GPKG nodata=-9999")
  print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
  cmd = paste0(shQuote(OSGeo4W_path)," gdaladdo ","--config OGR_SQLITE_SYNCHRONOUS OFF ", "-r AVERAGE ",nomsortie," 2 4 8 16 32 64 128 256")
  print(cmd);system(cmd)
  
  # 
  ############################################################################################################################################
  ######################################################################
  # Recherche de distinction d?bordement/ruissellemnt
  # Calcule al?a grille DDTM34 avec seuil D?bordement de cours d'eau/ruissellement
  ######################################################################
  ######################################################################
  # Cr?er une couche avec secteur HWH > Hmin et HSF>Qlmin,
  # pour discerner d?bordement et ruissellement
  # le raisonnement choisi, en gros prendre le graph DDE84 avec les courbe de personne assez proche du d?bit lin?ique
  # on prend un d?bit lin?ique faible et une hauteur d'eau pour les vitesses tr?s basses
  # Par exemple hauteur d'eau (HWH)>0.25m et d?bit lin?ique HSF)>0.1
  # cela correspond ? 50cm d'eau * 20 cm/s ou 20 cm eau ? 50 cm/s
  # cela correspond aussi ? 10 cm d'eau avec 1m/s
  
  nomALEA2=paste0("A_2_",basename(iscena),"_HWH",gsub("[[:punct:]]", "_",BorneDCE[1]),"m_HSF",gsub("[[:punct:]]", "_",BorneDCE[2]),"m")
  exp=paste0(nomALEA2,"=if( ",nomG[1]," > ",BorneDCE[1],", ",nomALEA0,", if( ",nomG[2]," > ",BorneDCE[2]," , ",nomALEA0,",null()),null())")
  # execGRASS("r.mapcalc",flags=c("quiet","overwrite"),parameters=list(expression=exp))
  cmd=paste0("r.mapcalc --overwrite ",shQuote(exp))
  print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
  
  nomsortie=file.path(dsnlayer,paste0(nomALEA2,".gpkg"))
  # execGRASS("r.out.gdal",flags=c("quiet","overwrite","c"),
  #           parameters=list(input=nomALEA2,output=nomsortie,type="Float32",format="GPKG",nodata=-9999))
  cmd=paste0("r.out.gdal --quiet --overwrite -c -f input=",nomALEA2," output=",nomsortie," type=Float32 format=GPKG nodata=-9999")
  print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
  cmd = paste0(shQuote(OSGeo4W_path)," gdaladdo ","--config OGR_SQLITE_SYNCHRONOUS OFF ", "-r AVERAGE ",nomsortie," 2 4 8 16 32 64 128 256")
  print(cmd);system(cmd)
  
  ######
  # Max ALEA1FTF et ALEA2
  nomALEA3=paste0("A_3_",basename(iscena),"_1FTF_2")
  # execGRASS("r.series",c("quiet","overwrite"),input=paste0(nomALEA1FTF,",",nomALEA2),output=nomALEA3,method=c("maximum"))
  cmd=paste0("r.series --overwrite input=",paste0(nomALEA1FTF,",",nomALEA2)," output=",nomALEA3," method=maximum")
  print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
  
  
  nomsortie=file.path(dsnlayer,paste0(nomALEA3,".gpkg"))
  # execGRASS("r.out.gdal",flags=c("quiet","overwrite","c"),
  #           parameters=list(input=nomALEA3,output=nomsortie,type="Float32",format="GPKG",nodata=-9999))
  cmd=paste0("r.out.gdal --quiet --overwrite -c -f input=",nomALEA3," output=",nomsortie," type=Float32 format=GPKG nodata=-9999")
  print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
  cmd = paste0(shQuote(OSGeo4W_path)," gdaladdo ","--config OGR_SQLITE_SYNCHRONOUS OFF ", "-r AVERAGE ",nomsortie," 2 4 8 16 32 64 128 256")
  print(cmd);system(cmd)
  
  #### Buffer autour de la zone d'int?r?t
  ######################################################################
  # Faire un buffer sur la zone consider?e comme du d?bordement de cours d'eau zone
  # pour assurer des continuit?s lors de micro-coupures
  # Exemple buffer de 5m
  nomALEA3Buf=paste0(nomALEA3,"_Buffer",distBuf,"m")
  # execGRASS("r.buffer",flags=c("quiet","overwrite"),
  #           parameters=list(input=nomALEA3,output=nomALEA3Buf,distances=distBuf))
  cmd=paste0("r.buffer --quiet --overwrite input=",nomALEA3," output=",nomALEA3Buf," distance=",distBuf)
  print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
  
  nomsortie=file.path(dsnlayer,paste0(nomALEA3Buf,".gpkg"))
  # execGRASS("r.out.gdal",flags=c("quiet","overwrite","c"),
  #           parameters=list(input=nomALEA3Buf,output=nomsortie,type="Float32",format="GPKG",nodata=-9999))
  cmd=paste0("r.out.gdal --quiet --overwrite -c -f input=",nomALEA3Buf," output=",nomsortie," type=Float32 format=GPKG nodata=-9999")
  print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
  cmd = paste0(shQuote(OSGeo4W_path)," gdaladdo ","--config OGR_SQLITE_SYNCHRONOUS OFF ", "-r AVERAGE ",nomsortie," 2 4 8 16 32 64 128 256")
  print(cmd);system(cmd)
  
  ############################################################################################################################################
  nomALEA3BufSecteur=paste0(nomALEA3Buf,"Secteur")
  exp=paste0(nomALEA3BufSecteur,"=if( ",nomALEA3Buf," > 0,1,null())")
  # execGRASS("r.mapcalc",flags=c("quiet","overwrite"),parameters=list(expression=exp))
  cmd=paste0("r.mapcalc --overwrite ",shQuote(exp))
  print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
  
  nomsortie=file.path(dsnlayer,paste0(nomALEA3BufSecteur,".tif"))
  # execGRASS("r.out.gdal",flags=c("quiet","overwrite","c"),
  #           parameters=list(input=nomALEA3BufSecteur,output=nomsortie,format="GTiff"))
  cmd=paste0("r.out.gdal --quiet --overwrite -c -f input=",nomALEA3BufSecteur," output=",nomsortie," format=GTiff nodata=-9999")
  print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd)) 
  
  # cmd=paste0(shQuote("C:\\OSGeo4W64\\bin\\gdaladdo")," --config OGR_SQLITE_SYNCHRONOUS OFF -r AVERAGE ",shQuote(nomsortie),"  2 4 8 16 32 64 128 256")
  # system(cmd)
  
  # On recup?re les donn?es raster en vecteur
  # execGRASS("r.to.vect",flags=c("quiet","overwrite","s"),
  # execGRASS("r.to.vect",flags=c("quiet","overwrite"),
  #           parameters=list(input=nomALEA3BufSecteur,output=nomALEA3BufSecteur,type="area"))
  cmd=paste0("r.to.vect -s --quiet --overwrite input=",nomALEA3BufSecteur," output=",nomALEA3BufSecteur," type=area")
  print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
  
  nomsortie=file.path(dsnlayer,paste0(nomALEA3BufSecteur,".gpkg"))
  cmd=paste0("v.out.ogr --quiet --overwrite input=",nomALEA3BufSecteur," output=",nomsortie," format=GPKG")
  print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
  
  # et on ajoute un champ pour regarder la taille des zones r?cup?r?es
  # L'objectif sera de ne garder que des "grandes" zones, plus de 10000m? par exemple
  # DCE=readVECT(nomALEA3BufSecteur)
  DCE=st_read(nomsortie)
  # print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
  DCE$AIRE=st_area(DCE)
  
  # nomsortie=file.path(dsnlayer,paste0(nomALEA3BufSecteur,".shp"))
  # st_write(DCE,nomsortie, delete_layer=T, quiet=T)
  
  #----- Enleve le masque avec buffer le masque
  cmd=paste0("r.mask -r")
  print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
  
  ######################################################################
  # Export grille al?a DDTM34 sur les secteurs DCE pr?c?dement d?finis
  # sur des zones avec des aires d?j? importantes
  # execGRASS("r.mask",flags=c("r"))
  nomDCEg=paste0("AireSup",AireMin,"m2")
  # writeVECT(DCE[which(DCE$AIRE>AireMin),],nomDCEg, v.in.ogr_flags=c("overwrite","quiet","o"), driver="ESRI Shapefile")
  nomsortie=file.path(dsnlayer,paste0(nomALEA3BufSecteur,nomDCEg,".gpkg"))
  st_write(DCE[which(DCE$AIRE>AireMin),],nomsortie, delete_layer=T, quiet=T)
  
  # importation vecteur dans grass grand contour
  cmd=paste0("v.in.ogr -o --quiet --overwrite input=",nomsortie," output=",nomDCEg)
  print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
  
  # execGRASS("r.mask",flags=c("quiet","overwrite"),parameters=list(vector=nomDCEg))
  cmd=paste0("r.mask --quiet --overwrite vector=",nomDCEg)
  print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
  
  nomALEA4=paste0("A_4_",basename(iscena),"_",nomDCEg,"Final")
  # Export de nomALEA1 ou nomALEA
  nomsortie=file.path(dsnlayer,paste0(nomALEA4,".gpkg"))
  # execGRASS("r.out.gdal",flags=c("quiet","overwrite","c"),
  #           parameters=list(input=nomALEA1,output=nomsortie,type="Float32",format="GPKG",nodata=-9999))
  cmd=paste0("r.out.gdal --quiet --overwrite -c -f input=",nomALEA1," output=",nomsortie," type=Float32 format=GPKG nodata=-9999")
  print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
  cmd = paste0(shQuote(OSGeo4W_path)," gdaladdo ","--config OGR_SQLITE_SYNCHRONOUS OFF ", "-r AVERAGE ",nomsortie," 2 4 8 16 32 64 128 256")
  print(cmd);system(cmd)
}
a=ez
##### ICICICICICICICIC
rm(listeALEA)
for (iscena in Scena[-grep(Scena,pattern="SceMaxMed")])
{
  nomALEA4=paste0("A_4_",basename(iscena),"_",nomDCEg,"Final")
  
  # execGRASS("r.in.gdal",flags=c("o","quiet","overwrite"),parameters=list(input=file.path(dsnlayer,paste0(nomALEA4,".gpkg")),output=nomALEA4))
  # cmd=paste0("r.in.gdal -o --quiet --overwrite input=",file.path(dsnlayer,paste0(nomALEA4,".gpkg"))," output=",nomALEA4)
  # print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
  
  if (exists("listeALEA")==F){listeALEA=nomALEA4}else{listeALEA=paste(listeALEA,nomALEA4,sep=",")}
}

# execGRASS("r.mask",flags=c("r"))
cmd=paste0("r.mask -r")
print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))

nomALEA4PPRi=paste0("A_4_PPRi")
# execGRASS("r.series",c("quiet","overwrite"),input=paste0("A_4_",basename(Scena[1:length(Scena)]),"_",nomDCEg,"Final"),output=nomALEA4PPRi,method="maximum")
cmd=paste0("r.series --overwrite input=",listeALEA," output=",nomALEA4PPRi," method=maximum")
print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))

nomsortie=file.path(dsnlayer,paste0(nomALEA4PPRi,".gpkg"))
# execGRASS("r.out.gdal",flags=c("quiet","overwrite","c"),
#           parameters=list(input=nomALEA4PPRi,output=nomsortie,type="Float32",format="GPKG",nodata=-9999))
cmd=paste0("r.out.gdal --quiet --overwrite -c -f input=",nomALEA4PPRi," output=",nomsortie," type=Float32 format=GPKG nodata=-9999")
print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
cmd = paste0(shQuote(OSGeo4W_path)," gdaladdo ","--config OGR_SQLITE_SYNCHRONOUS OFF ", "-r AVERAGE ",nomsortie," 2 4 8 16 32 64 128 256")
print(cmd);system(cmd)

nomALEA4Range=paste0("A_4_Range")
cmd=paste0("r.series --overwrite input=",listeALEA," output=",nomALEA4PPRi," method=range")
print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
nomsortie=file.path(dsnlayer,paste0(nomALEA4Range,".gpkg"))
cmd=paste0("r.out.gdal --quiet --overwrite -c -f input=",nomALEA4PPRi," output=",nomsortie," type=Float32 format=GPKG nodata=-9999")
print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
cmd = paste0(shQuote(OSGeo4W_path)," gdaladdo ","--config OGR_SQLITE_SYNCHRONOUS OFF ", "-r AVERAGE ",nomsortie," 2 4 8 16 32 64 128 256")
print(cmd);system(cmd)

# nomALEA4Resi=paste0("A_4_Resi")
# execGRASS("r.series",c("quiet","overwrite"),input=paste0("A_4_",basename(Scena[5:6]),"_",nomDCEg,"Final"),output=nomALEA4Resi,method="maximum")
# nomsortie=file.path(dsnlayer,paste0(nomALEA4Resi,".gpkg"))
# execGRASS("r.out.gdal",flags=c("quiet","overwrite","c"),
#           parameters=list(input=nomALEA4Resi,output=nomsortie,type="Float32",format="GPKG",nodata=-9999))
# cmd=paste0(shQuote("C:\\OSGeo4W64\\bin\\gdaladdo")," --config OGR_SQLITE_SYNCHRONOUS OFF -r AVERAGE ",shQuote(nomsortie),"  2 4 8 16 32 64 128 256")
# system(cmd)
