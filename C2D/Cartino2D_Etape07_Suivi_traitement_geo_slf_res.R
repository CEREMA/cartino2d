

Etape7_fn <- function(){
  tryCatch({          if (ETAPE[7] == 1)
  {
    # recup de la longeur
    num = as.numeric(scan(
      file.path(dsnlayerC, paste0(nom_maillage, "_bottom.grd")),
      skip = 1,
      nlines = 1
    ))
    contours[nsecteur[ic], "NOEUDS"] = num[2]
    contours[nsecteur[ic], "ELTS"] = num[1]
    
  }}, error = function(e) {skip_to_next <<- TRUE})
  return(skip_to_next)

}


Etape7_fn_parallel <- function(){
  if (ETAPE[7] == 1)
  {
    # recup de la longeur
    # num = as.numeric(scan(
    #   file.path(dsnlayerC, paste0(nom_maillage, "_bottom.grd")),
    #   skip = 1,
    #   nlines = 1
    # ))
    # contours[nsecteur[ic], "NOEUDS"] = num[2]
    # contours[nsecteur[ic], "ELTS"] = num[1]
    print("que mettre ici ?")
    
  }
}

# 
# etapefred <= function()
# {
#   
  # liste_slf=list.files(dsnlayer,pattern=".slf",recursive=T)
  # 
  # contours_liste <- st_read(file.path(dsnlayer,nomlayerC))
  # 
  # Compa1=paste0(contours_liste$NOMPOST,"/",contours_liste$NOM)
  # commun=intersect(Compa1,dirname(liste_slf))
  # 
  # contours_liste$CHECK=""
  # contours_liste$CHECK[which(Compa1 %in% commun)]="SLF"
  # 
  # nomexport=paste0(substr(nomlayerC,1,nchar(nomlayerC)-5),"_CHECK.gpkg")
  # 
  # st_write(contours_liste, file.path(dsnlayer,nomexport), delete_layer = T, quiet = T)
# }