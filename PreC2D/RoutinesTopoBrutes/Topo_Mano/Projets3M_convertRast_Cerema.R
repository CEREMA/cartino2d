# Chargement des bibliothèques nécessaires
library(sf)
library(raster)
# library(gstat)


cat("\014")
# Lien pour utiliser Pdal
pdal_exe="C:/QGIS/bin/pdal.exe"
# Lien pour utiliser des fonction GRASS
BatGRASS="C:\\QGIS\\bin\\grass84.bat"
SecteurGRASS_="C:/GRASSDATA/Topo3M/Temp"
nEPSG=2154
reso=0.5
dsnlayer="C:/AFFAIRES/Montpellier/Topographie2024/03_Topo/Topo_3M_janvier_2024/shapes"

liste=basename(list.dirs(dsnlayer,recursive=F))

deb=1
fin=length(liste)

for (raci in liste[deb:fin])
{
  cat("\014")
  NomGPKG=file.path(dsnlayer,raci,paste0(raci,"_Z.GPKG"))
  
  if (file.exists(NomGPKG)==F)
  {
    nomZ=file.path(dsnlayer,raci,paste0(raci,"_Z.shp"))
    nomContour=file.path(dsnlayer,raci,paste0(raci,"_CONTOUR.shp"))
    # Chargez les fichiers shapefile en entrée et le masque dans R :
    if (file.exists(nomZ)==T){ShpPts <- st_read(nomZ)}else{shppts=boom}
    if (file.exists(nomContour)==T){ShpMasq <- st_read(nomContour)}else{ShpMasq=boom}
    
    nomPolygon=file.path(dsnlayer,raci,paste0(raci,"_CONTOUR_Polygon.gpkg"))
    nomBUG=file.path(dsnlayer,raci,paste0(raci,"_BUG.gpkg"))
    
    VectPoly=st_cast(ShpMasq,"POLYGON")
    print(VectPoly)
    plot(VectPoly)
        VectPoly=VectPoly[,-(which(colnames(VectPoly)=="fid"))]
    st_write(VectPoly,nomPolygon, delete_layer=T, quiet=T)
    
    nomcsv=file.path(dsnlayer,raci,paste0(raci,"_Z.csv"))
    coords=st_coordinates(ShpPts)
    nici=which(coords[,3]<0.01 | is.na(coords[,3])==T)
    if (length(nici)>0){coords=coords[-nici,];print(nomZ);st_write(ShpPts[nici,],nomBUG, delete_layer=T, quiet=T)}
    write.csv(coords,nomcsv,
              row.names=FALSE)
    
    nomjson=file.path(dsnlayer,raci,paste0(raci,"_Z.json"))
    nomLaz=file.path(dsnlayer,raci,paste0(raci,"_Z.copc.laz"))
    NomTIF=file.path(dsnlayer,raci,paste0(raci,"_Z.TIF"))
    
    
    
    write("[",nomjson)
    write("    {",nomjson,append=T)
    write(paste0("       ",shQuote("type"),":",shQuote("readers.text"),","),nomjson,append=T)
    write(paste0("       ",shQuote("filename"),":",shQuote(nomcsv),","),nomjson,append=T)
    write(paste0("       ",shQuote("override_srs"),":",shQuote(paste0("EPSG:",nEPSG))),nomjson,append=T)
    write("    },",nomjson,append=T)
    
    ############### Filtre delaunay
    write("    {",nomjson,append=T)
    write(paste0("       ",shQuote("type"),":",shQuote("filters.delaunay")),nomjson,append=T)
    write("    },",nomjson,append=T)
    ############## Interpolation
    write("    {",nomjson,append=T)
    write(paste0("       ",shQuote("type"),":",shQuote("filters.faceraster"),","),nomjson,append=T)
    write(paste0("       ",shQuote("resolution"),":",reso,","),nomjson,append=T) 
    
    # Définir la bbox du masque avec des valeurs entières pour les limites min/max
    bbox <- st_bbox(ShpMasq)
    xmin <- floor(bbox["xmin"])
    xmax <- ceiling(bbox["xmax"])
    ymin <- floor(bbox["ymin"])
    ymax <- ceiling(bbox["ymax"])
    
    write(paste0("       ",shQuote("width") ,":",(xmax-xmin)/reso,","),nomjson,append=T) 
    write(paste0("       ",shQuote("height"),":",(ymax-ymin)/reso,","),nomjson,append=T) 
    write(paste0("       ",shQuote("origin_x"),":",xmin,","),nomjson,append=T) 
    write(paste0("       ",shQuote("origin_y"),":",ymin),nomjson,append=T) 
    write("    },",nomjson,append=T)
    
    ################# Export
    write("    {",nomjson,append=T)
    write(paste0("       ",shQuote("type"),":",shQuote("writers.raster"),","),nomjson,append=T)
    write(paste0("       ",shQuote("gdaldriver"),":",shQuote("GTiff"),","),nomjson,append=T)
    write(paste0("       ",shQuote("data_type"),":",shQuote("float32"),","),nomjson,append=T)  
    write(paste0("       ",shQuote("filename"),":",shQuote(NomTIF)),nomjson,append=T)
    write("    }",nomjson,append=T)
    write("]",nomjson,append=T)
    cmd=paste(pdal_exe,"pipeline",nomjson)
    print(cmd);system(cmd)
    
    
    SecteurGRASS=paste0(dirname(SecteurGRASS_),"_",format(Sys.time(),format="%Y%m%d_%H%M%S"),"/",basename(SecteurGRASS_))
    system(paste0(BatGRASS," -c EPSG:",nEPSG," ",dirname(SecteurGRASS)," --text"))
    system(paste0(BatGRASS," -c ",SecteurGRASS," --text"))
    
    # Récupération du raster
    nomMNT="MNT"
    cmd=paste0("r.in.gdal -o --overwrite input=",NomTIF," output=",nomMNT)
    print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
    
    # Limitation de la région de travail et gestion de la résolution
    cmd=paste0("g.region --quiet --overwrite raster=",nomMNT)
    print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
    
    Masque="Masque1Poly"
    cmd=paste0("v.in.ogr  -o --quiet --overwrite input=",nomPolygon," output=",Masque," -r")
    print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
    
    cmd=paste0("r.mask -r")
    print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
    
    cmd=paste0("r.mask --quiet --overwrite vector=",Masque)
    print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
    
    cmd=paste0("r.out.gdal --quiet --overwrite -c -f input=",nomMNT," output=",NomGPKG," type=Float32 format=GPKG nodata=-9999")
    print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
    
    unlink(dirname(SecteurGRASS),recursive=TRUE)
  }
}