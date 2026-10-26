library(sf)

OH=st_read("C:/Cartino2D/France/_OuvragesHydrauliques/OH_MTPCLL20220316.shp")

Xdeb=764000

Ydeb=6255000

SautX=5000

Bord=200
Larg=10
surlarg=100
Long=250
ds=1
PosiPont=120
LargPont=20
HPont=2
HTablier=3
DistQ=20
PentePourc=c(0,0.01,0.05,1,0.5,1,2,5,10)

ListPoints=list()
ListeOH1=list();ListeOH2=list();ListeOH3=list();ListeOH4=list()
TA_list=list()

Debit_list=list()

for (inc in 1:length(PentePourc))
{
  # Abscisse du début
  Xdeb_=Xdeb+(inc-1)*SautX
  # ordonnées et Z des points
  Ord=rbind(Ydeb+200,
            Ydeb+surlarg,
            Ydeb-PosiPont,
            Ydeb-PosiPont-ds,
            Ydeb-PosiPont-LargPont,
            Ydeb-PosiPont-LargPont-ds,
            Ydeb-Long,
            Ydeb-Long-200)
  
  Z=(Ord-Ydeb)*PentePourc[inc]/100+max((Ydeb-Ord)*PentePourc[inc]/100)
  Z[1]=50
  Z[4:5]=Z[4:5]+HTablier
  # ListPoints[[inc]]=rbind(cbind(Xdeb_-Bord,Ord,50),cbind(Xdeb_-0.05,Ord,50),cbind(Xdeb_,Ord,Z),cbind(Xdeb_+Larg,Ord,Z),cbind(Xdeb_+Larg+0.05,Ord,50),cbind(Xdeb_+Larg+Bord,Ord,50))
  ListPoints[[inc]]=rbind(cbind(Xdeb_-Bord,Ord,50),cbind(Xdeb_-surlarg-0.05,Ord,50),cbind(Xdeb_-surlarg,Ord,Z),cbind(Xdeb_+Larg+surlarg,Ord,Z),cbind(Xdeb_+Larg+surlarg+0.05,Ord,50),cbind(Xdeb_+Larg+Bord,Ord,50))
  
  # réalisation des OH
  for (iOH in 1:4)
  {
    Y1=(Ydeb-PosiPont+2*ds)
    Y2=(Ydeb-PosiPont-LargPont-2*ds)
    
    listXY=list()
    for (igeo in 1:iOH)
    {
      XOH=Xdeb_+Larg*igeo/(iOH+1)
      listXY[[igeo]]=as.matrix(
        rbind(
          cbind(XOH,Y1),
          cbind(XOH,Y2)
        )
      )
    }
    
    OH_sf=st_cast(
      st_sf(data.frame(LRG=Larg,#Larg/iOH,
                       HAUT1=HPont,
                       Z1=(Y1-Ydeb)*PentePourc[inc]/100+max((Ydeb-Ord)*PentePourc[inc]/100),     
                       Z2=(Y2-Ydeb)*PentePourc[inc]/100+max((Ydeb-Ord)*PentePourc[inc]/100),       
                       HAUT2=HPont,   
                       FRIC=1/50,  
                       LENGTH=4*ds+20,  
                       CIRC=0),
            "geometry" =st_sfc(st_multilinestring(listXY),dim="XY"),crs=2154),
      "LINESTRING")
    
    if (iOH==1){ListeOH1[[inc]]=OH_sf}
    if (iOH==2){ListeOH2[[inc]]=OH_sf}
    if (iOH==3){ListeOH3[[inc]]=OH_sf}
    if (iOH==4){ListeOH4[[inc]]=OH_sf}
    
    # Table d'assemblage
    toto=list(as.matrix(rbind(
      cbind(Xdeb_,Ydeb),
      cbind(Xdeb_+Larg,Ydeb),
      cbind(Xdeb_+Larg,Ydeb-Long),
      cbind(Xdeb_,Ydeb-Long),
      cbind(Xdeb_,Ydeb)
    )))
    
    TA_list[[inc]] = st_sf(data.frame(DOSSIERASC = "SemiMNT",
                                      NOM_ASC=paste0("MiniMNT_00",inc,".TIF")),
                           "geometry" = st_sfc(st_polygon(toto, dim = "XY")),
                           crs = 2154)
  }
  
  # réalisation des débits
  PtsQ=rbind(
    cbind(Xdeb_+1/3*Larg,Ydeb-DistQ),
    cbind(Xdeb_+2/3*Larg,Ydeb-DistQ)
  )
  Debit_list[[inc]]=st_cast(
    st_sf(data.frame(Q_002="POS_00002.txt",
                     Q_005="POS_00005.txt",
                     Q_010="POS_00010.txt",
                     Q_020="POS_00020.txt",
                     Q_050="POS_00050.txt",
                     Q_080="POS_00080.txt"
    ),
    "geometry" =st_sfc(st_multipoint(PtsQ),dim="XY"),crs=2154),
    "POINT")
}

# Export des points
Points=do.call(rbind,ListPoints)
Points3D=st_cast(st_sf(geometry=st_sfc(st_multipoint(as.matrix(Points), dim = "XYZ")),crs=2154),"POINT")
nomexp="C:/AFFAIRES/Topo_Mano_Examples/CanauxTelemac/Points3D.gpkg"

st_write(Points3D,
         nomexp,
         delete_dsn=F,delete_layer=T,quiet=T)

# Export des ouvrages
for (iOH in 1:4)
{
  nomexp=paste0("C:/AFFAIRES/Topo_Mano_Examples/CanauxTelemac/OH_",iOH,".gpkg")
  if (iOH==1){OH_sf=do.call(rbind,ListeOH1)}
  if (iOH==2){OH_sf=do.call(rbind,ListeOH2)}
  if (iOH==3){OH_sf=do.call(rbind,ListeOH3)}
  if (iOH==4){OH_sf=do.call(rbind,ListeOH4)}
  
  st_write(OH_sf,
           nomexp,
           delete_dsn=F,delete_layer=T,quiet=T)
}

# Export de la TA
nomexp=paste0("C:/AFFAIRES/Topo_Mano_Examples/CanauxTelemac/TA_SemiCanaux.gpkg")
st_write(do.call(rbind,TA_list),
         nomexp,
         delete_dsn=F,delete_layer=T,quiet=T)

#Export débits
Debit_list
nomexp=paste0("C:/AFFAIRES/Topo_Mano_Examples/CanauxTelemac/InjectionDebitCanaux.gpkg")
st_write(do.call(rbind,Debit_list),
         nomexp,
         delete_dsn=F,delete_layer=T,quiet=T)