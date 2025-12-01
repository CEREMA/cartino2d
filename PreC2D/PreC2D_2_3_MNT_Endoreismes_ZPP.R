library(sf)

#Creation d'un monde GRASS
SecteurGRASS=SecteurGRASS_
unlink(dirname(SecteurGRASS),recursive=TRUE)
system(paste0(BatGRASS," -c EPSG:",EPSG," ",dirname(SecteurGRASS)," --text"))
system(paste0(BatGRASS," -c ",SecteurGRASS," --text"))

nomInond_Endo=file.path(dsnDPE,paste0(raci,"_","Endo","_Manuel.gpkg"))

# Lecture de la table d'assemblage
TA=st_read(file.path(dsnLidar,nomTA))
st_crs(TA)=EPSG

# if (Cest_Contour_pas_Zone==0)
# {
#   # Lecture de la zone d'étude
#   ZONE=st_read(nommasque)
#   st_crs(ZONE)=EPSG
# }

# Limitation de la table d'assemblage aux zones à traiter
nb=st_intersects(TA,ZONE)
n_int = which(sapply(nb, length)>0)
if (length(n_int)>0)
{
  # réduction sur les dalles electionnées
  TA=TA[n_int,]
  
  # Création de la liste des dalles concernées
  listeASC=paste0(dsnLidar,'/',TA$DOSSIERASC,'/',TA$NOM_ASC)
  
  nom_ascvrt=file.path(dsnDPE,"listepourvrt.txt")
  file.create(nom_ascvrt)
  write(listeASC, file=nom_ascvrt, append=T)
  vrtfile = paste0(dsnDPE,"/","MNT",".vrt") ##chemin du vrt à créer
  print(vrtfile)
  cmd = paste(shQuote(OSGeo4W_path), "gdalbuildvrt",vrtfile,"-input_file_list",nom_ascvrt) ## commande pour exécuter gdalbuildvrt
  system(cmd) 
  
  # Importation du raster dans GRASS
  nom_MNT_="MNT_"
  cmd=paste0("r.in.gdal -o --quiet --overwrite input=",vrtfile," output=",nom_MNT_)
  print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
  
  cmd=paste0("g.region --overwrite --quiet"," raster=",nom_MNT_)
  print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
  
  #### Récéhntillonage au pas de 
  cmd=paste0("g.region --overwrite --quiet"," raster=",nom_MNT_," res=",as.character(ResoMNTpourTaudem))
  print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
  
  # import de la zone d'étude
  nomZone="ZONE"
  cmd=paste0("v.in.ogr -o -r --quiet --overwrite input=",nommasque," output=",nomZone)
  print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))

  # On limite le calcul à la zone d'étude
  cmd=paste0("r.mask --quiet --overwrite vector=",nomZone)
  print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
  
  nom_MNT="MNT"
  cmd=paste0("r.resample --quiet --overwrite input=",nom_MNT_," output=",nom_MNT)
  print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
  
  # Lecture du vecteur endoresime
  Endo=st_read(nomInond_Endo)
  # Selection du champ Verif=1
  Endo=Endo[which(Endo$Verif==1),]
  nomEndo_Valid=file.path(dsnDPE,paste0(raci,"_","Endo","_Valid.gpkg"))
  st_write(Endo,nomEndo_Valid,delete_layer = T, quiet = T)
  
  nbe=st_intersects(Endo,ZONE)
  n_inte = which(sapply(nbe, length)>0)
  if (length(n_inte)>0)
  {
    # réduction sur les dalles electionnées
    Endo=Endo[n_inte,]
    
    nomInond_Endog="Endo"
    cmd=paste0("v.in.ogr -o --quiet --overwrite input=",nomEndo_Valid," output=",nomInond_Endog)
    print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
    
    # On limite le calcul à la zone d'étude
    cmd=paste0("r.mask --quiet --overwrite vector=",nomInond_Endog)
    print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
    
    #3 Statistique Minimum
    cmd=paste0("v.rast.stats map=",nomInond_Endog," raster=",nom_MNT," column_prefix=MNT method=minimum")
    print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
    
    # conversion en reaster
    nomBasEndo="BasEndo"
    cmd=paste0("v.to.rast --quiet --overwrite input=",nomInond_Endog," output=",nomBasEndo," use=attr attribute_column=MNT_minimum")
    print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
    
    nomTrou="Trou"
    cmd=paste0("r.mapcalc --overwrite ",shQuote(paste0(nomTrou,"= if(",nom_MNT,"<",nomBasEndo,"+",DeltadessusMin,",1,null())")))
    print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
    
    # Buffer autour de ces points
    nomTrouBuf="TrouBuf"
    cmd=paste0("r.buffer --quiet --overwrite input=",nomTrou," output=",nomTrouBuf," distance=",ResoMNTpourTaudem)
    print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
    
    cmd=paste0("r.mask -i --quiet --overwrite raster=",nomTrouBuf)
    print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
    
    
    # }else{
    #   nom_MNT=nom_MNT_
  }  
  
  if (file.exists(nomZPP)==T)
  { 
    Descend=25
    ##########################################################################
    #### Ajout des ponts
    # import de la zone d'étude
    nomZPPg="ZONE"
    cmd=paste0("v.in.ogr -o --quiet --overwrite input=",nomZPP," output=",nomZPPg)
    print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
    
    cmd=paste0("v.to.rast --quiet --overwrite input=",nomZPPg," output=",nomZPPg," use=val value=",Descend)
    print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
    
    nomMNTPont="MNT_Baisse"
    
    cmd=paste0("r.mapcalc --quiet --overwrite ",shQuote(paste0(nomMNTPont,"=",nom_MNT,"-",nomZPPg)))
    print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
    
    nomMNT2="MNT_Baisse2"
    
    cmd=paste0("r.patch --quiet --overwrite ","input=",nomMNTPont,",",nom_MNT," output=",nomMNT2)
    print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
    
    nom_MNT=nomMNT2
  }
  # Export du nouvel MNT pour aller dans taudem (TIF)
  NomTIF=file.path(dsnDPE,paste0(raci,"_","MNTPourEndo.tif"))
  # NASSIM ," nodata=-9999"
  cmd=paste0("r.out.gdal --quiet --overwrite -c input=",nom_MNT," output=",NomTIF," format=GTiff"," nodata=-9999")
  system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
}

unlink(dirname(SecteurGRASS),recursive=TRUE)



cat("\n")
cat("\n")
cat("\n")
cat("\n")
cat("###################################################################################\n")
cat("######################### C2D A LIRE SVP ###########################################\n")
cat("------------------------- Etape PreC2D 2_3-------------------------------------------\n")
cat("le fichier MNT modifié",NomTIF,"a été créé.R\n")
cat("Vous pouvez vérifier les calculs\n")
cat("######################### Fin C2D A LIRE ###########################################\n")