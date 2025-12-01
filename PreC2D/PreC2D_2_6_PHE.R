PHE_=st_transform(PHE,st_crs(contours[icontour,]))

nb=st_intersects(PHE_,contours[icontour,])
n_int = which(sapply(nb, length)>0)
if (length(n_int)>0)
{
  PHE_=PHE_[n_int,]
  
  nomPHE_=file.path(dsnDPE,"PHE_tmp.gpkg")
  st_write(PHE_,nomPHE_, delete_dsn = T,delete_layer = T, quiet = T)
  
  nomPHE_2=file.path(dsnDPE,"PHE_tmp2.gpkg")
  cmd=paste0(qgis_process, " run native:joinbynearest",
             " --distance_units=meters --area_units=m2 --ellipsoid=EPSG:7019",
             " --INPUT=",nomPHE_,
             " --INPUT_2=",file.path(dsnDPE,raci),
             " --DISCARD_NONMATCHING=false --PREFIX= --NEIGHBORS=1",
             " --OUTPUT=",nomPHE_2)
  paste(cmd);system(cmd)
  
  PHE_=st_read(nomPHE_2)
  
  PHE_$PHE_class <- cut(PHE_$distance, breaks = c(-0.1,SeuilDistPHE),
                      labels = paste0("C_",c(0,SeuilDistPHE[-length(SeuilDistPHE)]),"_",SeuilDistPHE))
  PHE_counts= table(PHE_$PHE_class)
  PHE_proportions <- prop.table(PHE_counts)
  names(PHE_proportions)=paste0("P_",c(0,SeuilDistPHE[-length(SeuilDistPHE)]),"_",SeuilDistPHE)
  
  PHE_counts_=data.frame(matrix(PHE_counts,nrow=1))
  names(PHE_counts_)=names(PHE_counts)
  
  PHE_proportions_=data.frame(matrix(PHE_proportions,nrow=1))
  names(PHE_proportions_)=names(PHE_proportions)
  
  PHE_synth=cbind(PHE_counts_,PHE_proportions_,st_centroid(contours[icontour,"NOM"]))
  nomPHE_synth=file.path(dsnDPE,"PHE_Synth.gpkg")
  st_write(PHE_synth,nomPHE_synth, delete_dsn = T,delete_layer = T, quiet = T)
  
  # # Créer le diagramme circulaire
  # ggplot(data.frame(PHE_proportions), aes(x = "", y = Freq, fill = Var1)) +
  #   geom_bar(width = 1, stat = "identity") +
  #   coord_polar("y", start = 0) +
  #   theme_void() +
  #   labs(title = "Proportions par distance",
  #        fill = "Distance") +
  #   theme(legend.position = "center")
}