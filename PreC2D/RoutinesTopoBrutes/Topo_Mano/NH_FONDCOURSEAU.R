library(sf)

nomNH="C:\\AFFAIRES\\Montpellier\\Topographie2024\\Mosson\\Topo_Mosson\\Mosson_Test_NH_Berges\\Bief1\\lm_pts___1.shp"

NH=st_read(nomNH)

NSection=unique(NH$NSection)
R1l=list()
R2l=list()
for (iSection in 1:length(NSection))
{
  nb=which(NH$NSection==NSection[iSection])
  # R1l[[iSection]]=st_cast(st_geometry(NH[nb[1],]),"POINT")
  # R2l[[iSection]]=st_cast(st_geometry(NH[nb[length(nb)],]),"POINT")
  R1l[[iSection]]=NH[nb[1],]
  R2l[[iSection]]=NH[nb[length(nb)],]
}

R1=do.call(rbind,R1l)
R2=do.call(rbind,R2l)

R1top=st_sf(data.frame(Rive=1),
      geometry=st_cast(st_combine(st_geometry(R1)), "LINESTRING"))
R2top=st_sf(data.frame(Rive=2),
            geometry=st_cast(st_combine(st_geometry(R2)), "LINESTRING"))

st_write(rbind(R1top,R2top),file.path(dirname(nomNH),"Rives.gpkg"), delete_layer = T, quiet = T)