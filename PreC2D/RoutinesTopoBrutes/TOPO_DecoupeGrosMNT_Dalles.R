library(sf)
library(dplyr)
cat("\014")
# code EPSG du projet
EPSG = 2154
# Nom du secteur GRASS # il pourrait être automatique
SecteurGRASS="C:/GRASSDATA/Topo/Temp"
# Lien vers le logiciel GRASS
BatGRASS="C:\\QGIS\\bin\\grass84.bat"



dsnlayer="J:\\MAMP\\06b_MNTTIN_FILINO_L3D"
nomRast="J:\\MAMP\\06_MNTTIN_FILINO\\MNT_MAMP_HD_Filino_L3D_202510.gpkg"
nomRastProv="MNT_FILINO_L3D_MAMP2025"
Reso=0.5

#Creation d'un monde GRASS
unlink(dirname(SecteurGRASS),recursive=TRUE)
system(paste0(BatGRASS," -c EPSG:",EPSG," ",dirname(SecteurGRASS)," --text"))
system(paste0(BatGRASS," -c ",SecteurGRASS," --text"))

setwd(dsnlayer)
#execGRASS("r.in.gdal",flags=c("o","quiet","overwrite"),parameters=list(input=file.path(dsnRast,nomRast),output=nomRastProv))
cmd=paste0("r.in.gdal -o --quiet --overwrite input=",nomRast," output=",nomRastProv)
print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
# system(cmd)
nomregion="maregion.txt"
cmd=paste0("g.region --quiet --overwrite raster=",nomRastProv," save=",nomregion)
print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))

nomfichier=file.path(SecteurGRASS,"windows",nomregion)

fid <- file(nomfichier, "r")
region=readLines(fid, n=6)
close(fid)

region2=strsplit(region,":")
NSEO=as.numeric(as.matrix(do.call (rbind,lapply(3:6,function(x) {region2[[x]][2]}))))

NSEO=floor(NSEO/1000)
inc=0
deltares=0
for (iy in NSEO[2]:NSEO[1])
{
  for (ix in NSEO[4]:NSEO[3])
  {
    inc=inc+1
    avan=paste0(round(inc/((NSEO[3]-NSEO[4]+1)*(NSEO[1]-NSEO[2]+1))*100,0),"%")
    Lim=as.character(c(1000*iy+deltares,1000*(iy+1)+deltares,1000*ix-deltares,1000*(ix+1)-deltares))
    
    cmd=paste0("g.region --quiet --overwrite n=",Lim[2]," s=",Lim[1]," e=",Lim[4]," w=",Lim[3]," res=",Reso)
    print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
    dir.create(file.path(dsnlayer,nomRastProv))
    NomUnivar=file.path(dsnlayer,nomRastProv,paste("Univar",1000*ix,1000*(ix+1),1000*iy,1000*(iy+1),".txt",sep="_"))
    cmd=paste0("r.univar --quiet --overwrite map=",nomRastProv," output=",NomUnivar)
    print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
    nvaleur=as.numeric(scan(file=NomUnivar,NomUnivar,sep=":",skip=5,nlines=1,dec="."))[2]
    print(nvaleur)
    unlink(NomUnivar)
    
    if (nvaleur>0)
    {
      nomsortie=file.path(dsnlayer,nomRastProv,
                          paste0(nomRastProv,"_",
                                 formatC((ix),3,flag="0",format="d"),
                                 "_",
                                 formatC((iy+1),3,flag="0",format="d"),
                                 ".gpkg"))
      cat(nomsortie,"\n")
      cmd=paste0("r.out.gdal --quiet --overwrite -c -f input=",nomRastProv," output=",nomsortie," type=Float32 format=GPKG nodata=-9999")
      print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
    }
  }
}
unlink(dirname(SecteurGRASS),recursive=TRUE)
