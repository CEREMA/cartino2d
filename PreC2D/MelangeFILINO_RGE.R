library(sf)
cheminRGE="J:\\MAMP\\RGE_MAMP"
# nomRGE="TA_RGE_ZD.gpkg"
nomRGEvrt="RGE_MAMP_ZD.vrt"

CheminHD="J:\\MAMP\\06b_MNTTIN_FILINO_2026_PRC"
nomHD="TA_TIN_Filino_2026_PRC.shp"
# TA_RGE=st_read(nomRGE)

nomZD="J:\\MAMP\\RGE_MAMP\\ZD.gpkg"

chemin_export="J:\\MAMP\\RGE_MAMP\\FUSION"

# Nom du secteur GRASS
SecteurGRASS_="C:/GRASSDATA/RGE_HD/MAMP"
# Lien vers le logiciel GRASS
BatGRASS="C:\\QGIS\\bin\\grass84.bat"
BatGRASS=shQuote(BatGRASS)
EPSG=2154


TA_HD=st_read(file.path(CheminHD,nomHD))
ZD=st_read(nomZD)

# nb=st_intersects(TA_RGE,st_buffer(ZD,10))
# n_int <- which(sapply(nb, length) >= 1 )
# TA_RGE=TA_RGE[n_int,]



SecteurGRASS=paste0(dirname(SecteurGRASS_),format(Sys.time(),format="%Y%m%d_%H%M%S"),"/",basename(SecteurGRASS_))
# SecteurGRASS=paste0(dirname(SecteurGRASS_),format(Sys.time(),format="%Y%m%d_%H%M%S"),"_",basename(dsnlayerC),"_",ic,"/",basename(SecteurGRASS_))
unlink(dirname(SecteurGRASS),recursive=TRUE)
toto=system(paste0(BatGRASS," -c EPSG:",EPSG," ",dirname(SecteurGRASS)," --text"))
# if (toto!=0){cat("Vous avez un problème dans ",BatGRASS,"\n");BUGDEGRASS=BOOM}
system(paste0(BatGRASS," -c ",SecteurGRASS," --text"))

# Importation raster vrt dans Grass
nom_RGE="RGE"
cmd=paste0("r.in.gdal -o --quiet --overwrite input=",file.path(cheminRGE,nomRGEvrt)," output=",nom_RGE)
print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))

# importation vecteur dans grass grand contour
nom_ZD="ZD"
cmd=paste0("v.in.ogr -o --quiet --overwrite input=",nomZD," output=",nom_ZD)
print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))

# faire une table qui va bien...
nb=st_intersects(TA_HD,st_buffer(ZD,10))
n_int <- which(sapply(nb, length) >= 1 )
TA_HD=TA_HD[n_int,]
contour=sfheaders::sf_remove_holes(st_union(TA_HD))
coords=st_coordinates(contour)
GrillePt <- as.matrix(expand.grid(
  X = seq(min(coords[,1]) + 500, max(coords[,1]), 1000),
  Y = seq(min(coords[,2]) + 500, max(coords[,2]), 1000)
))

GrillePt_sf=st_sf(data.frame(X=GrillePt[,1]/1000-0.5,Y=GrillePt[,2]/1000+0.5),
                  geometry=st_cast(st_sfc(st_multipoint(GrillePt, dim = "XY")),"POINT"),crs=EPSG)

nb=st_intersects(GrillePt_sf,contour)
n_int <- which(sapply(nb, length) >= 1 )
GrillePt_sf=GrillePt_sf[n_int,]
plot(GrillePt_sf)

Grille_sf=st_buffer(GrillePt_sf,500,,endCapStyle = "SQUARE")
plot(Grille_sf)

Grille_sf$ID=NA_integer_
Grille_sf$CHEMIN=NA
Grille_sf$DOSSIERASC=TA_HD$DOSSIERASC[1]
Grille_sf$NOM_ASC=paste0("LHD_FXX_",formatC(Grille_sf$X, width = 4, flag = "0"),"_",formatC(Grille_sf$Y, width = 4, flag = "0"),"_PTS_C_LAMB93_IGN69_copc_TIN_Filino.gpkg")

TAaj=list()
iaj=1
for (iTA in 1:nrow(Grille_sf))
{
  # nom=paste0("LHD_FXX_",formatC(Grille_sf$X[iTA], width = 4, flag = "0"),"_",formatC(Grille_sf$Y[iTA], width = 4, flag = "0"),"_PTS_C_LAMB93_IGN69_copc_TIN_Filino.gpkg")
  nom=Grille_sf$NOM_ASC[iTA]
  
  nomexp=file.path(chemin_export,TA_HD$DOSSIERASC[1],nom)
  
  if (file.exists(nomexp)==F)
  {
    if (file.exists(file.path(CheminHD,TA_HD$DOSSIERASC[1],nom))==T)
    {
      # Importation raster vrt dans Grass
      nom_HD=paste0("HD",iTA)
      nom_HDm=paste0("HD",iTA,"mask")
      nom_patch=paste0("HD",iTA,"patch")
      cmd=paste0("r.in.gdal -o --quiet --overwrite input=",file.path(CheminHD,Grille_sf$DOSSIERASC,Grille_sf$NOM_ASC)[iTA]," output=",nom_HD)
      print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
      
      cmd=paste0("g.region --overwrite --quiet -a raster=",nom_HD)
      print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
      
      cmd=paste0("r.mask -i --quiet --overwrite vector=",nom_ZD)
      print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
      
      cmd=paste0("r.resample --quiet --overwrite input=",nom_HD," output=",nom_HDm)
      print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
      
      cmd=paste0("r.mask --quiet --overwrite -r")
      print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
      
      cmd=paste0("r.patch --quiet --overwrite ","input=",nom_HDm,",",nom_RGE," output=",nom_patch)
      print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
      nom_expg=nom_patch
    }else{
      Reso=0.5
      cmd=paste0("g.region --quiet --overwrite n=",1000*Grille_sf$Y[iTA]," s=",1000*Grille_sf$Y[iTA]-1000," e=",1000*Grille_sf$X[iTA]+1000," w=",1000*Grille_sf$X[iTA]," res=",Reso)
      print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
      
      nom_expg=nom_RGE
      TAaj[[iaj]]=Grille_sf[iTA,cbind("ID","CHEMIN","DOSSIERASC","NOM_ASC")]
      
      # st_write(TAaj[[iaj]], file.path(chemin_export,"pourvoir.gpkg"), driver = "GPKG", delete_dsn = TRUE, quiet = TRUE)
      iaj=iaj+1
    }
    
    #---- Export
    if (dir.exists(dirname(nomexp))==F){dir.create(dirname(nomexp),recursive = TRUE)}
    cmd=paste0("r.out.gdal --quiet --overwrite -c -f input=",nom_expg," output=",nomexp," type=Float32 format=GPKG nodata=-9999")
    print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd)) 
  }
}


TA_Plus=do.call(rbind,TAaj)

TA_HD_=rbind(TA_HD,TA_Plus)
st_write(TA_HD_, file.path(chemin_export,"TA_TIN_Filino_2026_PRC_RGE.gpkg"), driver = "GPKG", delete_dsn = TRUE, quiet = TRUE)
