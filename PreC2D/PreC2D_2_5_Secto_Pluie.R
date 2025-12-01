#Creation d'un monde GRASS
SecteurGRASS=SecteurGRASS_
unlink(dirname(SecteurGRASS),recursive=TRUE)
system(paste0(BatGRASS," -c EPSG:",EPSG," ",dirname(SecteurGRASS)," --text"))
system(paste0(BatGRASS," -c ",SecteurGRASS," --text"))

##############################################################################################
#### Travail sur les bassins versants
##############################################################################################
# FAIRE EN QGIS
# browser()
# BasVers1=st_read(watershed);st_crs(BasVers1)=EPSG
# BasVers2=st_read(Bords);st_crs(BasVers2)=EPSG

# MasqueReduc=st_read(nommasque)
# 
# # MasqueReduit=st_buffer(st_read(nommasque),-10);st_crs(MasqueReduit)=EPSG
# MasqueReduit=st_buffer(st_intersection(st_union(st_buffer(BasVers1,2)),MasqueReduit),-10);st_crs(MasqueReduit)=EPSG
# st_write(MasqueReduit,file.path(dsnDPE,"MasqueReduit.gpkg"), delete_layer=T, quiet=T)

# colnames(BasVers2)[1]="BV_ID"
# BasVers2$AREA_m2=0
# BasVers2$AREA_km2=0
# BasVers=rbind(BasVers1,BasVers2)
# BasVers=BasVers1
nomgBasVers="BasVers"
# nomBasVers="BasVersINIetBord"
# print(paste0("Export des Bv initiaux ",nomBasVers))
# st_crs(BasVers)=EPSG
nom_Buffer         =file.path(dsnDPE,"BasVers_watershedBuffer.gpkg")
nom_Bufsanstrou    =file.path(dsnDPE,"BasVers_watershedBuffer_sanstrous.gpkg")
nom_BufSatr_Reduit =file.path(dsnDPE,"BasVers_watershedBuffer_sanstrous_reduit.gpkg")
NomBV_GPKG         =watershed#file.path(dsnDPE,"BasVers.gpkg")
nomBV_GPKG_Coup    =file.path(dsnDPE,"BasVersINIetBordCoup.gpkg")
nomBV_GPKG_Coup2   =file.path(dsnDPE,"BasVersINIetBordCoup2.gpkg")
nomBV_GPKG_Coup2Uni=file.path(dsnDPE,"BasVersINIetBordCoup2Uni.gpkg")
NomReseauBuf       =file.path(dsnDPE,"ReseauBuf.gpkg")
NomReseauBufUni    =file.path(dsnDPE,"ReseauBufUni.gpkg")

# st_write(BasVers,NomBV_GPKG, delete_layer=T, quiet=T)

cmd <- paste0(qgis_process, " run native:buffer",
              " --INPUT=", shQuote(watershed),
              " --DISTANCE=0 --SEGMENTS=5 --END_CAP_STYLE=0 --JOIN_STYLE=0 --MITER_LIMIT=2 --DISSOLVE=True",
              " --OUTPUT=", shQuote(nom_Buffer))
print(cmd); system(cmd)

# Enlever les petits trous
cmd <- paste0(qgis_process, " run native:deleteholes",
              " --INPUT=", shQuote(nom_Buffer),
              " --MIN_AREA=", 1000000,
              " --OUTPUT=", shQuote(nom_Bufsanstrou))
print(cmd); system(cmd)

cmd <- paste0(qgis_process, " run native:intersection",
              " --INPUT=",shQuote(nom_Bufsanstrou),
              " --OVERLAY=",nommasque,
              " --OUTPUT=",shQuote(nom_BufSatr_Reduit),
              " --GRID_SIZE=None")
print(cmd); system(cmd)

cmd <- paste0(qgis_process, " run native:intersection",
              " --INPUT=",watershed,
              " --OVERLAY=",nom_BufSatr_Reduit,
              " --OUTPUT=",nomBV_GPKG_Coup,
              " --GRID_SIZE=None")
print(cmd); system(cmd)

Reseau=st_read(NomReseau)
st_crs(Reseau)=EPSG
st_write(st_buffer(Reseau,1),NomReseauBuf,delete_layer=T, quiet=T)

cmd <- paste0(qgis_process, " run native:multiparttosingleparts",
              " --INPUT=",shQuote(NomReseauBuf),
              " --OUTPUT=",shQuote(NomReseauBufUni))
system(cmd)

cmd <- paste0(qgis_process, " run native:difference",
              " --INPUT=",shQuote(nomBV_GPKG_Coup),
              " --OVERLAY=",shQuote(NomReseauBufUni),
              " --OUTPUT=",shQuote(nomBV_GPKG_Coup2),
              " --GRID_SIZE=None")
system(cmd)

cmd <- paste0(qgis_process, " run native:multiparttosingleparts",
              " --INPUT=",shQuote(nomBV_GPKG_Coup2),
              " --OUTPUT=",shQuote(nomBV_GPKG_Coup2Uni))
system(cmd)

#########################################################################################################
# NETTOYAGE DES MINI MORCEAUX DE BV QUI GENENT ESNUITE
print(paste0("Ouverture des bassins versants ",nomBV_GPKG_Coup2Uni))
cmd=paste0("v.in.ogr -o --quiet --overwrite input=",nomBV_GPKG_Coup2Uni," output=",nomgBasVers)
print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))

nomgBasVersRMarea="BasVersRMarea"
cmd=paste0("v.clean --quiet --overwrite input=",nomgBasVers," output=",nomgBasVersRMarea," type=area tool=rmarea threshold=",seuilSup0)
print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))

cmd=paste0("v.out.ogr --quiet --overwrite input=",nomgBasVersRMarea," output=",file.path(dsnDPE,paste0(nomgBasVersRMarea,".gpkg"))," format=GPKG")
print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))

print(paste0("Lecture R du ",nomgBasVersRMarea))
BasVers2 <- st_read(file.path(dsnDPE,paste0(nomgBasVersRMarea,".gpkg")))
BasVers2=BasVers2[,c(1,2,3,4)]

BasVers=BasVers2

# cat("Nbre Bv initial",dim(BasVers),"\n")
BasVers=BasVers[which(st_area(BasVers)>seuilSup3),]
BasVers$BV_ID=1:dim(BasVers)[1]
cat("Nbre Bv après suppression des mini-morceaux",dim(BasVers),"\n")
#########################################################################################################

nomInond_flash=file.path(dsnDPE,paste0(raci,"_ZI_flash",".gpkg"))
#

cmd <- paste0(qgis_process, " run native:intersection",
              " --INPUT=",nomInond_flash,
              " --OVERLAY=",nom_BufSatr_Reduit,
              " --OUTPUT=",file.path(dsnDPE,paste0(raci,"_ZI_flash2",".gpkg")),
              " --GRID_SIZE=None")
print(cmd); system(cmd)

ZoneInond_flash=st_read(file.path(dsnDPE,paste0(raci,"_ZI_flash2",".gpkg")))
st_geometry(ZoneInond_flash)="geometry"

# ZoneInond_flash <- st_read(nomInond_flash)
#
print(paste0("Buffer ",nomInond_flash))
ZoneInond_flashbuf=st_union(st_buffer(ZoneInond_flash,buf))
ZoneInond_flashbuf=st_sf(geometry=st_cast(ZoneInond_flashbuf,"POLYGON"))

# Calcul de la surface et suppression des zones de écoulement pas trop grande
Surf_km2 = st_area(ZoneInond_flashbuf)
ZoneInond_flashbuf$Aire=Surf_km2
ZoneInond_flashbuf=ZoneInond_flashbuf[which(st_area(ZoneInond_flashbuf)>seuilSup1),]
ZoneInond_flashbuf$IdZflash=1:dim(ZoneInond_flashbuf)[1]
st_write(ZoneInond_flashbuf,file.path(dsnDPE,"ZoneInond_flashbuf.gpkg"), delete_layer=T, quiet=T)

# Morceau unique à multiple
# suppression avec des aires inférieures à ...   10000m²
# Filtre de 500 m² pour supprimer des mini-chevauchements et les garder


st_crs(ZoneInond_flashbuf)=EPSG
ZIflashMini=st_intersection(ZoneInond_flashbuf,BasVers)
# Calcul de la surface et suppression des intersctions de écoulement/Bv pas trop grande
#Filtre de seuilSup2 m² pour supprimer des mini-chevauchements 
# ? pour les garder et les ré-intégrer ensuite
Surf_km2 = st_area(ZIflashMini)
ZIflashMini$Aire=Surf_km2
ZIflashMini_exclus=ZIflashMini[-which(st_area(ZIflashMini)>seuilSup2),]
ZIflashMini=ZIflashMini[which(st_area(ZIflashMini)>seuilSup2),]

st_write(ZIflashMini,file.path(dsnDPE,"ZIflashMini.gpkg"), delete_layer=T, quiet=T)
st_write(ZIflashMini_exclus,file.path(dsnDPE,"ZIflashMini_exclus.gpkg"), delete_layer=T, quiet=T)
# Si des objets ne sont que dans 1 bassin et que aucun autre objet n'est à la fois dans ce bassin et un autre, alors c'est des bassins simples et il faut que tous ces obejst écoulement représente l'ensemble du gros objet écoulement 
nasupp=-99
ZIflashMini2=ZIflashMini
while (length(nasupp)>0)
{
  # Calcul du nombre de fois où un écoulement a été coupé (présent dans combien de bv)
  ZIflashMini2$Nbre_IdZflash=sapply(1:dim(ZIflashMini2)[1], function(x) {length(which(ZIflashMini2$IdZflash==ZIflashMini2$IdZflash[x]))})
  # Calcul du nombre de zone de écoulement différents dans chaque Bv
  ZIflashMini2$Nbre_BV_ID=sapply(1:dim(ZIflashMini2)[1], function(x) {length(which(ZIflashMini2$BV_ID==ZIflashMini2$BV_ID[x]))})
  # si ZIflashMini2$Nbre_IdZflash=1 & ZIflashMini2$Nbre_BV_ID>1 => alors ce bv sera déjà traité, on supprime la ligne! on peut faire un tant que!
  
  # Si des objets ne sont que dans 1 bassin et que aucun autre objet n'est à la fois dans ce bassin et un autre, alors c'est des bassins simples et il faut que tous ces obejst écoulement représente l'ensemble du gros objet écoulement 
  nasupp=which(ZIflashMini2$Nbre_IdZflash==1 & ZIflashMini2$Nbre_BV_ID>1)
  print(nasupp)
  if (length(nasupp)>0)
  {ZIflashMini2=ZIflashMini2[-nasupp,]}
}
st_write(ZIflashMini2,file.path(dsnDPE,"ZIflashMini_v2.gpkg"), delete_layer=T, quiet=T)

#
Secteurs=lapply(unique(ZIflashMini2$IdZflash), function(x)
{st_sf(data.frame(IdZflash=x),geometry=st_cast(st_union(BasVers[ZIflashMini2[which(ZIflashMini2$IdZflash==x),]$BV_ID,]),"POLYGON"))})
Secteurs_PourC2D=do.call(rbind,Secteurs)
st_write(Secteurs_PourC2D,file.path(dsnDPE,"Secteurs_PourC2D_v0.gpkg"), delete_layer=T, quiet=T)

# rajouter les mini-chevauchements
# buffer sur les tous les résultats
ZoneInond_flashbuf_ajBUF=st_buffer(ZoneInond_flashbuf[Secteurs_PourC2D$IdZflash,],Bufferflash)
st_write(ZoneInond_flashbuf_ajBUF,file.path(dsnDPE,"ZoneInond_flashbuf_ajBUF.gpkg"), delete_layer=T, quiet=T)

# cmd <- paste0(qgis_process, " run native:intersection",
#               " --INPUT=",file.path(dsnDPE,"ZoneInond_flashbuf_ajBUF.gpkg"),
#               " --OVERLAY=",nom_BufSatr_Reduit,
#               " --OUTPUT=",file.path(dsnDPE,"ZoneInond_flashbuf_ajBUF2.gpkg"),
#               " --GRID_SIZE=None")
# print(cmd); system(cmd)
# 
# ZIflashMini_exclus_ajBUF=st_read(file.path(dsnDPE,"ZoneInond_flashbuf_ajBUF2.gpkg"))
# st_geometry(ZIflashMini_exclus_ajBUF)="geometry"

# buffer plus important sur les minimorceaux qui dépasetn sur un autre bassin
ZIflashMini_exclus_ajBUF=st_buffer(ZIflashMini_exclus,BufferDepasse)

# Ajout 22/08/2023 Ne pas garder ceux qui touchent le réseau
# Reseau=st_read(NomReseauBufUni)
# nb=st_intersects(ZIflashMini_exclus_ajBUF,Reseau)
# n_int = which(sapply(nb, length)>0)
# # plot(ZIflashMini_exclus_ajBUF[n_int,"IdZflash"])
# ZIflashMini_exclus_ajBUF=ZIflashMini_exclus_ajBUF[-n_int,]

st_write(ZIflashMini_exclus_ajBUF,file.path(dsnDPE,"ZIflashMini_exclus_ajBUF.gpkg"), delete_layer=T, quiet=T)

# cmd <- paste0(qgis_process, " run native:intersection",
#               " --INPUT=",file.path(dsnDPE,"ZIflashMini_exclus_ajBUF.gpkg"),
#               " --OVERLAY=",nommasque,
#               " --OUTPUT=",file.path(dsnDPE,"ZIflashMini_exclus_ajBUF2.gpkg"),
#               " --GRID_SIZE=None")
# print(cmd); system(cmd)
# 
# ZIflashMini_exclus_ajBUF=st_read(file.path(dsnDPE,"ZIflashMini_exclus_ajBUF2.gpkg"))
# st_geometry(ZIflashMini_exclus_ajBUF)="geometry"

#### BUGGG
Secteurs_PourC2D$Type="Secteurs_PourC2D"
ZoneInond_flashbuf_ajBUF$Type="ZoneInond_flashbuf_ajBUF"
ZIflashMini_exclus_ajBUF$Type="ZIflashMini_exclus_ajBUF"

# Un peu de travail pour agglomérer les petits morceaux sauf ceux associés à des zones de écoulements qui ont disparu avec la procédure au dessus...
Secteurs_PourC2D_2=rbind(Secteurs_PourC2D[,c("IdZflash","Type")],
                         ZoneInond_flashbuf_ajBUF[,c("IdZflash","Type")],
                         ZIflashMini_exclus_ajBUF[,c("IdZflash","Type")])
Indic=unique(Secteurs_PourC2D_2$IdZflash)
IndicBug=Indic[which(sapply(Indic, function(x)
{length(which(Secteurs_PourC2D_2$IdZflash==x))==length(which(Secteurs_PourC2D_2[which(Secteurs_PourC2D_2$IdZflash==x),]$Type=="ZIflashMini_exclus_ajBUF"))}))]

# Export pendant la mise au point méthode, inutile
st_write(Secteurs_PourC2D_2,file.path(dsnDPE,"Secteurs_PourC2D_v2.gpkg"), delete_layer=T, quiet=T)

Secteurs_PourC2D_3=lapply(unique(Secteurs_PourC2D_2$IdZflash), function(x)
{st_sf(data.frame(IdZflash=x),geometry=st_cast(st_union(Secteurs_PourC2D_2[which(Secteurs_PourC2D_2$IdZflash==x),]),"POLYGON"))})

Secteurs_PourC2D_4=do.call(rbind,Secteurs_PourC2D_3)
Secteurs_PourC2D_4$ajeter=0
for (itr in 1:dim(Secteurs_PourC2D_4)[1])
{
  if (length(which(IndicBug==Secteurs_PourC2D_4[itr,]$IdZflash))>0){Secteurs_PourC2D_4[itr,]$ajeter=1}
}

st_write(Secteurs_PourC2D_4,file.path(dsnDPE,"Secteurs_PourC2D_v4.gpkg"), delete_layer=T, quiet=T)

Secteurs_PourC2D_4$BV_ID   =-99
Secteurs_PourC2D_4$Secteur="FusionAuto"
MiniBassinsPerdus=BasVers[-unique(ZIflashMini2$BV_ID),"BV_ID"]
colnames(MiniBassinsPerdus)[2]="geometry"
st_geometry(MiniBassinsPerdus) <- "geometry"
MiniBassinsPerdus$IdZflash=-99
MiniBassinsPerdus$Secteur="BassinsPetitsPerdus"
# Secteurs_PourC2D_4=Secteurs_PourC2D_4[,cbind("Idzflash","BV_ID","Secteur")]
Secteurs_PourC2D_5=rbind(Secteurs_PourC2D_4[which(Secteurs_PourC2D_4$ajeter==0),c("IdZflash","BV_ID","Secteur")],MiniBassinsPerdus)
Secteurs_PourC2D_5$Airekm2=st_area(Secteurs_PourC2D_5)/1000000
st_write(Secteurs_PourC2D_5,file.path(dsnDPE,"Secteurs_PourC2D_v5.gpkg"), delete_layer=T, quiet=T)


# Suppression de bv en doublons! Je ne comprend pas pq il y en a
# Calcul de taille position, si les même on en garde qu'un
Secteurs_PourC2D_6=Secteurs_PourC2D_5
Identifiant=FILINO_NomMasque(Secteurs_PourC2D_6)
cat(length(unique(Identifiant))," -", length(Identifiant),"\n")
Secteurs_PourC2D_6$Identifiant=Identifiant
Secteurs_PourC2D_6=Secteurs_PourC2D_6[order(Identifiant,decreasing = TRUE),]


doublons=which(Secteurs_PourC2D_6$Identifiant[-1]==Secteurs_PourC2D_6$Identifiant[-dim(Secteurs_PourC2D_6)[1]])
if (length(doublons)>0){Secteurs_PourC2D_6=Secteurs_PourC2D_6[-doublons,]}
st_write(Secteurs_PourC2D_6,file.path(dsnDPE,"Secteurs_PourC2D_v6.gpkg"), delete_layer=T, quiet=T)

# travail MANO

cmd <- paste0(qgis_process, " run native:difference",
              " --INPUT=",shQuote(file.path(dsnDPE,"Secteurs_PourC2D_v6.gpkg")),
              " --OVERLAY=",shQuote(NomReseauBufUni),
              # " --INPUT_FIELDS=[] --OVERLAY_FIELDS=[] --OVERLAY_FIELDS_PREFIX=",
              " --OUTPUT=",shQuote(file.path(dsnDPE,"Secteurs_flash_Pluie_Coup.gpkg")),
              " --GRID_SIZE=None")
system(cmd)

cmd <- paste0(qgis_process, " run native:multiparttosingleparts",
              " --INPUT=",shQuote(file.path(dsnDPE,"Secteurs_flash_Pluie_Coup.gpkg")),
              " --OUTPUT=",shQuote(file.path(dsnDPE,"Secteurs_flash_Pluie_Coup2.gpkg")))
system(cmd)



Secteurs_PourC2D_7=st_read(file.path(dsnDPE,"Secteurs_flash_Pluie_Coup2.gpkg"))
Secteurs_PourC2D_7$Airekm2=st_area(Secteurs_PourC2D_7)/1000000
Secteurs_PourC2D_7$Identifiant=FILINO_NomMasque(Secteurs_PourC2D_7)
Secteurs_PourC2D_7=Secteurs_PourC2D_7[order(Secteurs_PourC2D_7$Identifiant,decreasing = TRUE),]
Secteurs_PourC2D_7$Id=1:dim(Secteurs_PourC2D_7)[1]
st_write(Secteurs_PourC2D_7,file.path(dsnDPE,"Secteurs_PourC2D_v7a.gpkg"), delete_layer=T, quiet=T)
Orphelin=list()
incOrph=1


while (min(Secteurs_PourC2D_7$Airekm2)<SeuilBVMin_pourC2D)
{
  ndernier=dim(Secteurs_PourC2D_7)[1]
  # plot(Secteurs_PourC2D_7[ndernier,1])
  cat(" Nombre: ",ndernier," - Aire petit km2:",Secteurs_PourC2D_7[ndernier,]$Airekm2)
  nb=st_intersects(Secteurs_PourC2D_7[ndernier,1],Secteurs_PourC2D_7[-ndernier,1])
  n_int = which(sapply(nb, length)>0)
  nvoisins=nb[[1]]
  if (length(nvoisins)>0)
  {
    # plot(Secteurs_PourC2D_7[nvoisins,1])
    Coupe=st_intersection(Secteurs_PourC2D_7[ndernier,"Id"],st_buffer(Secteurs_PourC2D_7[nvoisins,"Id"],buf))
    Coupe$Aire=st_area(Coupe)
    nvoisins=nvoisins[which(Coupe$Aire==max(Coupe$Aire))]
    # plot(Secteurs_PourC2D_7[nvoisins,1])
    nouveau=st_union(Secteurs_PourC2D_7[nvoisins,"Id"],Secteurs_PourC2D_7[ndernier,"Id"])[,1]
    nouveau$IdZflash=-99
    nouveau$BV_ID=-99
    nouveau$Secteur="Regroupement"
    nouveau$Airekm2=st_area(nouveau)/1000000
    nouveau$Identifiant=FILINO_NomMasque(nouveau)
    # plot(nouveau[,1])
    Secteurs_PourC2D_7=rbind(Secteurs_PourC2D_7[-c(nvoisins,ndernier),],nouveau)
    Secteurs_PourC2D_7=Secteurs_PourC2D_7[order(Secteurs_PourC2D_7$Identifiant,decreasing = TRUE),]
  }else{
    Orphelin[[incOrph]]=Secteurs_PourC2D_7[ndernier,]
    incOrph=incOrph+1
    Secteurs_PourC2D_7=Secteurs_PourC2D_7[-ndernier,]
  }
}

if (incOrph>1)
{
  Orphelin=do.call(rbind,Orphelin)
  st_write(Orphelin,file.path(dsnDPE,"Secteurs_Orphelin.gpkg"), delete_layer=T, quiet=T)
}

cat("\n")
st_write(Secteurs_PourC2D_7,file.path(dsnDPE,"Secteurs_PourC2D_v7b.gpkg"), delete_layer=T, quiet=T)

cmd=paste0("v.in.ogr -o --quiet --overwrite input=",file.path(dsnDPE,"Secteurs_PourC2D_v7b.gpkg")," output=","Secteurs_flash_Pluie_Regroup_DOUBLONS")
print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))

cmd=paste0("v.out.ogr --quiet --overwrite input=","Secteurs_flash_Pluie_Regroup_DOUBLONS"," layer=2"," output=",file.path(dsnDPE,"Secteurs_PourC2D_v7b_DOUBLONS.gpkg")," format=GPKG")
print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))

cat("\n")
cat("\n")
cat("\n")
cat("\n")
cat("###################################################################################\n")
cat("######################### C2D A LIRE SVP ###########################################\n")
cat("------------------------- Etape PreC2D 2_5-------------------------------------------\n")
cat("Vous avez fini cette étape, BRAVO, vous devez analyser la sectorisation automatique réalisée.\n")
cat("et la rependre au besoin.\n")
cat("Fichier:",file.path(dsnDPE,"Secteurs_PourC2D_v7b.gpkg"),"\n")
cat("######################### Fin C2D A LIRE ###########################################\n")
