#################################################################################################
################   Etape3_Cli
#################################################################################################

Etape3_Cli=function()
{
  cat("\014")
  cat("ETAPE 3 - Traitement de: ",contour$NOM,"\n")
  NomSlf <- file.path(dsnlayerC,paste0(nom_Telemac,".slf"))
  
  cat(NomSlf,"\n")
  if(file.exists(NomSlf)) 
  {
    mntgrd <- file.path(dsnlayerC,paste0(nom_maillage,"_",nom_MNT,".grd"))
    
    num <- mntgrd %>% 
      scan(skip = 1,nlines = 1) %>% 
      as.numeric()
    
    # tabMNT <- mntgrd %>% 
    #   scan(sep = " ", skip = 2,nlines = num[2]) %>% 
    #   matrix(nrow = 4,ncol = num[2]) %>% 
    #   t() %>% 
    #   data.frame()
    
    tabMNT <- read.table(file.path(dsnlayerC,paste0(nom_maillage,"_",nom_MNT,".grd")),
                         skip = 2,header = FALSE, nrows = num[2])
    # indice des bords
    indi=read.table(file.path(dsnlayerC,'Cartino2D_pputil.cli'))[,12]
    # plot(tabMNT[indi,2],tabMNT[indi,3])
    
    BUGPOINTS=0
    if (BUGPOINTS==1)
    {
      Res=list()
      for (x in indi)
      {Res[[x]]=st_sf(data.frame(Indi=tabMNT[x,1],Z=tabMNT[x,4]),
                      "geometry" =st_sfc(st_point(c(tabMNT[x,2],tabMNT[x,3])),dim="XY"),crs=EPSG)
      }
      Gagne = do.call(rbind, Res) 
      st_write(Gagne,file.path(dsnlayerC,"Frontiere.shp"), delete_layer=T, quiet=T)
    }
    
    Frontiere=st_sf(data.frame(Type="Frontiere",
                               "geometry" =st_sfc(st_linestring(as.matrix(na.omit(tabMNT[indi,2:3])),dim="XY"))))
    st_write(Frontiere,file.path(dsnlayerC,"Frontiere.shp"), delete_layer=T, quiet=T) #Nabil : pourquoi est ce une ligne ?
    
    # Modification conditions limites
    nomclipputil=paste0(dsnlayerC,"\\",nom_Telemac,"_pputil.cli")
    cat(nomclipputil,"\n")
    if(file.exists(nomclipputil)) 
    {
      tab1=read.table(nomclipputil,header=F,sep=" ")
      
      nomcli=paste0(dsnlayerC,"\\",nom_Telemac,".cli")
      # faire 2 sortes de conditions limites
      # Aval max de 0.5 et des COTE_AVAL+qqch comme 0.5 ou 1m
      # reste frontières fermées
      NomUnivar=file.path(dsnlayerC,"univar.txt")
      
      COTE_AVAL=max(contour$COTEAVALM,
                    contour$COTEAVALAJ+as.numeric(scan(file=NomUnivar,NomUnivar,sep=":",skip=6,nlines=1,dec="."))[2],
                    -contour$DecAltiMNT+contour$COTEAVALAJ+min(tabMNT[indi,4]))
      
      nbLiq=data.frame(which(tabMNT[indi,4]<COTE_AVAL))
      cat(dim(nbLiq)[1]," ")
      # tambouille pour ajouter le point avant et après chaque point retenu
      # Sans cela bug si on n'a qu'un seul point bas sur une frontière, point et pas éléments
      
      if (min(nbLiq)==0){
        nbLiq=rbind(nbLiq,nbLiq[dim(nbLiq)[1],1])
        cat(dim(nbLiq)[1]," ")
      }
      if (max(nbLiq)==dim(nbLiq)[1]){
        nbLiq=rbind(1,nbLiq)
        nbLiq=unique(nbLiq)
        cat(dim(nbLiq)[1]," ")
      }
      nbLiq=unique(rbind(nbLiq,nbLiq+1,nbLiq-1))
      nbLiq=unique(nbLiq)
      cat(dim(nbLiq)[1]," ")
      
      nbLiq=nbLiq[order(nbLiq[,1]),]
      cat(dim(nbLiq)[1]," ")
      
      nbtrop=which((nbLiq<0))
      if (length(nbtrop)>0){
        print(nbtrop)
        nbLiq=nbLiq[-1,]
        cat(dim(nbLiq)[1]," ")
      }
      
      nbtrop=which(nbLiq[length(nbLiq)]>dim(indi)[1])
      if (length(nbtrop)>0){
        print(nbtrop)
        nbLiq=nbLiq[-(length(nbLiq))]
        cat(dim(nbLiq)[1]," ")
      }
      
      # Tambouille pour vérifier qu'il n'y a pas 1 seul point SOLIDE entre 2 points non solides
      # ca ne devrait pas marcher si cela arrive sur le 1er ou dernier point mais on aurait pas de chance...
      solo=which(sapply(1:(length(nbLiq)-1), 
                        function(x) {nbLiq[x+1]-nbLiq[x]})==2)
      
      if (length(solo)>0){
        nbLiq=rbind(as.matrix(nbLiq[1:solo]),nbLiq[solo]+1,as.matrix(nbLiq[(solo+1):length(nbLiq)]))
        cat(dim(nbLiq)[1]," ")
      }
      
      # Mais parfois on n'a pas de chance, gestion 1er et dernier point
      # 1er point
      if (nbLiq[1]==2 & nbLiq[length(nbLiq)]==dim(tab1)[1])
      {
        nbLiq=rbind(1,as.matrix(nbLiq))
        nbLiq=unique(nbLiq)
        cat(dim(nbLiq)[1]," ")
      }
      # dernier point
      if (nbLiq[1]==1 & nbLiq[length(nbLiq)]==(dim(tab1)[1]-1))
      {
        nbLiq=rbind(as.matrix(nbLiq),dim(tab1)[1])
        nbLiq=unique(nbLiq)
        cat(dim(nbLiq)[1]," ")
      }
      
      nici=which(nbLiq>length(indi) | nbLiq==0)
      if (length(nici)>0){nbLiq=nbLiq[-nici]}
      cat(dim(nbLiq)[1]," ")
      
      tab1[nbLiq,1]=5
      tab1[nbLiq,2]=4
      tab1[nbLiq,3]=4
      
      ########################################################################################################"
      # Il reste parfois un  2 2 2 au milieu 5 4 4 ...
      # Cas général
      bug20220301=which(sapply(2:(dim(tab1)[1]-1), function(x) {tab1[x-1,1]==5 & tab1[x+1,1]==5 & tab1[x,1]==2})==T)
      if(length(bug20220301)>0) 
      {
        tab1[bug20220301+1,1]=5
        tab1[bug20220301+1,2]=4
        tab1[bug20220301+1,3]=4
      }
      
      # Cas 1er ou dernier
      if (tab1[dim(tab1)[1],1]==5 & tab1[2,1]==5 & tab1[1,1]==2) {tab1[1,1:3]=c(5,4,4)}
      if (tab1[1,1]==5 & tab1[dim(tab1)[1]-1,1]==5 & tab1[dim(tab1)[1],1]==2) {tab1[dim(tab1)[1],1:3]=c(5,4,4)}
      
      
      # lines(tabMNT[indi[nbLiq],2],tabMNT[indi[nbLiq],3],'type'='p',col='red')
      write.table(tab1, file=nomcli, row.names=FALSE, col.names=FALSE, sep=" ")
      
      Sortie=st_sf(data.frame(Type="FrontiereSortie",
                              "geometry" =st_sfc(st_linestring(as.matrix(tabMNT[indi[nbLiq],2:3]),dim="XY"))))
      st_write(Sortie,file.path(dsnlayerC,"FrontiereSortie.shp"), delete_layer=T, quiet=T)
      
    }else{
      cat("FICHIER ",nomclipputil, " INTROUVABLE\n")
      write.table(0, file=paste0(nomclipputil,"BUGGG"), row.names=FALSE, col.names=FALSE, sep=" ")
    }
  }else{
    cat("FICHIER ",NomSlf, " INTROUVABLE\n")
    write.table(0, file=paste0(NomSlf,"BUGGG"), row.names=FALSE, col.names=FALSE, sep=" ")
  }
  ### gestion des sections de contrôle
  
  Gestion_Sections_Controle(dsnlayer, dsnlayerC, contour, 0)
}


Etape3_fn <- function(){
  tryCatch({      if (ETAPE[3] == 1){
    
    
    Etape3_Cli()
    
  }}, error = function(e) { skip_to_next <<- TRUE})
  
  return(skip_to_next)
}

Etape3_fn_parallel <- function(){
  if (ETAPE[3] == 1){
    
    
    Etape3_Cli()
    
  }
}
