Sections_Raster=function(dsnlayerTravail,nomlayerPontRivCanaux,nomRives,nomTA_LidarBase,nomTA_LidarC2D,reso,EPSG,EtapMNT)
{
  cat("\014")
  
  # Gestion des attributs
  nomlayerPontRivCanaux=PRC_Attributs(nomlayerPontRivCanaux)
  
  ##########################################################################
  #------- Gestion des croisements avec des rives ou pas -------------------
  ##########################################################################
  nom_Rives_coup=""
  nom_LigCont_export=file.path(dsnlayerTravail,"LigContr_export.gpkg")
  nomBUG=file.path(dsnlayerTravail,"BUG.gpkg")
  nom_Contour_Multi_export=file.path(dsnlayerTravail,"Contour_Multi.gpkg")
  nom_Contour_export=file.path(dsnlayerTravail,"Contour_Fusion.gpkg")
  if (nchar(nomRives)>1)
  {
    PRC =gardetouche0_2("_PRC_coup.gpkg"   ,nomlayerPontRivCanaux,nomRives             ,NA)
    Rives=gardetouche0_2("_Rives_coup.gpkg",nomRives             ,nomlayerPontRivCanaux,0)
  }else{
    PRC=st_read(nomlayerPontRivCanaux)
    PRC$Nbre=0
  }
  numPRC=sort(unique(PRC$NBief))
  
  nici=which(st_geometry_type(PRC)=="MULTILINESTRING")
  st_geometry(PRC[nici,])=st_geometry(st_cast(PRC[nici,],"LINESTRING"))
  
  
  # Fichier SIG des sections
  # Champs: Type ZsiType1 NBief NSection
  # Type 0=> Interpolation sur MNT
  # Type 1=> Interpolation sur la valeur ZsiType1
  # Type 2=> Interpolation sur ligne 3D
  ici=which(as.numeric(PRC$Type)==1)
  if (length(ici)>0)
  {
    for (iPRC in ici)
    {
      coord=st_coordinates(PRC[iPRC,])[,1:2]
      coord_new=matrix(cbind(coord,as.numeric(PRC$ZsiType1[iPRC])),nrow=nrow(coord),ncol=3)
      st_geometry(PRC[iPRC,]) =st_sfc(st_linestring(coord_new), crs = EPSG)
    }
  }
  
  # travail pour interpoler 3D
  ici=which(as.numeric(PRC$Type)==0)
  
  # Lidar
  if (nchar(nomTA_LidarBase)<=1 & length(ici)>0){onnepeutpasinterpolersurlemnts=sipasdemnt}
  
  if (EtapMNT==1 | length(ici)>0)
  {
    if (nchar(nomTA_LidarBase)>1)
    {
      Lidar=st_read(nomTA_LidarBase)
      st_crs(Lidar)=crs
    }
    
    SecteurGRASS=paste0(dirname(SecteurGRASS_),'_',format(Sys.time(),format="%Y%m%d_%H%M%S"),"/",basename(SecteurGRASS_))
    #Creation d'un monde GRASS
    unlink(dirname(SecteurGRASS),recursive=TRUE)
    system(paste0(BatGRASS," -c EPSG:",EPSG," ",dirname(SecteurGRASS)," --text"))
    system(paste0(BatGRASS," -c ",SecteurGRASS," --text"))
    
  }
  
  if (length(ici)>0)
  {
    PRC_=PRC[ici,]
    PRC_Pts3D=st_cast(st_segmentize(PRC_,reso/2),"POINT")
    PRC_Pts3D$ID=1:nrow(PRC_Pts3D)
    # export des sections qui nécessitent une interpolation 3D
    nomPRCpour3D=file.path(dsnlayerTravail,"PRCpour3D.gpkg")
    st_write(PRC_Pts3D,nomPRCpour3D, delete_dsn = T,delete_layer = T, quiet = T)
    
    # Selection des sections dans le Lidar
    nb=st_intersects(Lidar,PRC_)
    n_int = which(sapply(nb, length)>0)
    
    if (length(n_int) > 0) {
      {
        IciLidar=1
        LidarC=Lidar[n_int,]
        nom_MNT="Lidar"
        
        ########## Création de la liste des dalles concernées
        listeASC=paste0(dirname(nomTA_LidarBase),'/',LidarC$DOSSIERASC,'/',LidarC$NOM_ASC)
        nom_ascvrt=file.path(dsnlayerTravail,"listepourvrt.txt")
        file.create(nom_ascvrt)
        write(listeASC, file=nom_ascvrt, append=T)
        vrtfile = paste0(dsnlayerTravail,"/","temp",".vrt") ##chemin du vrt à créer
        cmd = paste(OSGeo4W_path, "gdalbuildvrt",vrtfile,"-input_file_list",nom_ascvrt) ## commande pour exécuter gdalbuildvrt
        system(cmd) 
        
        # Importation du VRT dans GRASS
        nomMNTtemp="MNT_Temp"
        cmd <- paste0("r.in.gdal -o --quiet --overwrite input=", vrtfile, " output=",nomMNTtemp)
        print(cmd);
        system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
        
        # Réglage de la région dans GRASS
        cmd <- paste0("g.region --quiet --overwrite raster=",nomMNTtemp," res=", as.character(reso))
        print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
        
        # 
        nom_ligneg="Ligneg"
        cmd=paste0("v.in.ogr -o --quiet --overwrite input=",nomPRCpour3D," output=",nom_ligneg)
        print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
        # 
        nomPtsg <- "Pts_Z"
        cmd <- paste0("v.drape --quiet --overwrite input=", nom_ligneg, " output=", nomPtsg, " elevation=",nomMNTtemp)
        print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
        # 
        nom_ligne=file.path(dsnlayerTravail,"LignePts3D.gpkg")
        cmd=paste0("v.out.ogr --quiet --overwrite input=",nomPtsg," output=",nom_ligne," format=GPKG")
        print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
        # 
        Pts_3D=st_read(nom_ligne)
        
        PRC_=PRC[ici,]
        for (ibief in unique(PRC_$NBief))
        {
          nbief=which(PRC_$NBief==ibief)
          for (isections in unique(PRC_$NSection[nbief]))
          {
            nbs=which(PRC_$NBief==ibief & PRC_$NSection==isections)
            nbsPts_3D=which(Pts_3D$NBief==ibief & Pts_3D$NSection==isections)
            st_geometry(PRC_[nbs,])=st_cast(st_combine(Pts_3D[nbsPts_3D,]),"LINESTRING")
          }
        }
      }
    }
    PRC[ici,]=PRC_
  }
  
  LigCont_=list()
  # Contour_=list()
  indi_LC_C=1
  for (indiPRC in 1:length(numPRC))
  {
    iPont=numPRC[indiPRC]
    # travail par bief
    nbi=which(PRC$NBief==iPont)
    PRC_=PRC[nbi,]
    
    for (inb in 1:(nrow(PRC_)-1))
    {
      if (PRC_$Nbre[inb]==0 | PRC_$Nbre[inb+1]==0)
      {
        # if(iPont==101 & inb==32){browser()}
        cat(iPont," ",inb,"\n")
        cat("Bief",iPont," ",inb, " Section: ",PRC_$NSection[inb]," Section:",PRC_$NSection[inb+1])
        st_write(PRC_[inb:(inb+1),],nomBUG, delete_dsn = T,delete_layer = T, quiet = T)
        if (length(which(PRC_$NSection==(PRC_$NSection[inb])))>1 | length(which(PRC_$NSection==(PRC_$NSection[inb+1])))>1)
        { 
          st_write(PRC_[inb:(inb+1),],nomBUG, delete_dsn = T,delete_layer = T, quiet = T)
          browser()
          BOOM=VOUS_avez_des_sections_multi_decoupees_a_coller_a_une_section_sans_rives
        }
        # Contour et Lignes Contraintes
        Pts1=st_coordinates(PRC_[inb,])[,1:2]
        Pts2=st_coordinates(PRC_[inb+1,])[,1:2]
        Pts_Deb=rbind(Pts1[1,],Pts2[1,])
        Pts_Fin=rbind(Pts1[nrow(Pts1),],Pts2[nrow(Pts2),])
        
        LigCont_[[indi_LC_C]]=rbind(
          st_sf(data.frame(Rive=iPont,Nbre=1),geometry=st_sfc(st_linestring(Pts_Deb, dim = "XY")),crs=nEPSG),
          st_sf(data.frame(Rive=iPont,Nbre=1),geometry=st_sfc(st_linestring(Pts_Fin, dim = "XY")),crs=nEPSG)
        )
        indi_LC_C=indi_LC_C+1
      }
    }
    Rives_=do.call(rbind,LigCont_)
  }
  
  cat("Bug posible si aucune rive créée\n")
  # si il y avait des rives mano

  if (exists("Rives")==T)
  {
    # s'il reste des rives mano
    if(nrow(Rives)>0)
    {
      st_geometry(Rives)="geometry"
      Rives$Rive =99
      Rives$Nbre =1
      Rives=Rives[,cbind("Rive","Nbre","geometry")]
      Rives_Final=rbind(st_zm(Rives),Rives_)
    }else{
      Rives_Final=Rives_
    }
  }else{
    Rives_Final=Rives_
  }
  cat("################ exportr lig contrainte et contour")

  st_write(Rives_Final,nom_LigCont_export, delete_layer = T, quiet = T)
  
  # Contour_Bief=st_buffer(do.call(rbind,Contour_),reso)
  cat(" Gestion du contour - parfois long => bascule en Qgis?\n")
  Contour_Bief=st_sf(data.frame(OHFLASH=""),
                     geometry=st_cast(st_union(st_union(st_buffer(st_geometry(PRC),reso),st_buffer(st_geometry(Rives_Final),reso))),"POLYGON"),crs=EPSG)
  cat(" Fin de Gestion du contour")
  
  st_write(Contour_Bief,nom_Contour_Multi_export, delete_layer = T, quiet = T)
  
  # Enlever les petits trous
  cmd <- paste0(qgis_process, " run native:deleteholes",
                " --INPUT=", shQuote(nom_Contour_Multi_export),
                " --MIN_AREA=", 999999,
                " --OUTPUT=", shQuote(nom_Contour_export))
  print(cmd); system(cmd)
  
  ##########################################################################
  #------- FIN Gestion des croisements avec des rives ou pas -------------------
  ##########################################################################
  
  ################################################################################
  ################################################################################
  #  Dossier de travail
  # ATTENTION AUX / et pas \\
  if (file.exists(dsnlayerTravail)==F){dir.create(dsnlayerTravail,recursive = T)}
  
  extR=cbind("LePlusJuste","LePlusC2D_LigContr")
  dir.create(file.path(dsnlayerTravail,extR[1]))
  dir.create(file.path(dsnlayerTravail,extR[2]))
  
  numPRC=sort(unique(PRC$NBief))
  
  nom_Pts3D_export_txt=file.path(dsnlayerTravail,"Pts3D_export.csv")
  # nom_LigCont_export=file.path(dsnlayerTravail,"LigContr_export.gpkg")
  
  nom_MNT_PRC_export1=file.path(dsnlayerTravail,"Rast_PRC1_leplusjuste.gpkg")
  nom_MNT_PRC_export2=file.path(dsnlayerTravail,"Rast_PRC2_pourC2DavecLigCont.gpkg")
  unlink(nom_Pts3D_export_txt)
  
  pgb <- txtProgressBar(min = 0, max = length(numPRC), style = 3)
  
  for (indiPRC in 1:length(numPRC))
  {
    if (EtapMNT==1)
    {
      unlink(nom_Pts3D_export_txt)
    }
    iPont=numPRC[indiPRC]
    cat("######################################################### Bief",iPont," - ",indiPRC,"/",length(numPRC),"\n")
    setTxtProgressBar(pgb, indiPRC)
    cat("\n")
    nbi=which(PRC$NBief==iPont)
    Bief=PRC[nbi,]
    if (nrow(Bief)>1)
    {
      # Voir pour remplir les colonnes si vide dans le fichier
      Bief=Bief[order(Bief$NSection),]
      # Bief$NSection=1:dim(Bief)[1]
      
      # # Contour et Lignes Contraintes
      # coords=st_coordinates(st_cast(Bief,"LINESTRING"))
      # nbchligne=which(coords[-1,"L1"]-coords[-nrow(coords),"L1"]>0)
      # # nbchligne=which(coords[-1,4]-coords[-dim(coords)[1],4]>0)
      # Pts_Deb=coords[c(1,nbchligne+1),1:2]
      # Pts_Fin=coords[c(nbchligne,dim(coords)[1]),1:2]
      # 
      # LigCont_[[indiPRC]]=rbind(
      #   st_sf(data.frame(BIEF=iPont,ID=1),geometry=st_sfc(st_linestring(Pts_Deb, dim = "XY")),crs=EPSG),
      #   st_sf(data.frame(BIEF=iPont,ID=2),geometry=st_sfc(st_linestring(Pts_Fin, dim = "XY")),crs=EPSG)
      # )
      # 
      # Contour_[[indiPRC]]=
      #   st_sf(data.frame(BIEF=iPont,ID=1),geometry=st_sfc(st_polygon(list(round(rbind(Pts_Deb,Pts_Fin[nrow(Pts_Fin):1,],Pts_Deb[1,]),2)), dim = "XY")),crs=2154)
      
      IciLidar=0
      if (EtapMNT==1)
      {
        if (nchar(nomTA_LidarBase)>1)
        {# Selection des sections dans le Lidar
          nb=st_intersects(Lidar,Bief)
          n_int = which(sapply(nb, length)>0)
          
          if (length(n_int) > 0) {
            {
              IciLidar=1
              LidarC=Lidar[n_int,]
              nom_MNT="Lidar"
              
              ########## Création de la liste des dalles concernées
              listeASC=paste0(dirname(nomTA_LidarBase),'/',LidarC$DOSSIERASC,'/',LidarC$NOM_ASC)
              nom_ascvrt=file.path(dsnlayerTravail,"listepourvrt.txt")
              file.create(nom_ascvrt)
              write(listeASC, file=nom_ascvrt, append=T)
              vrtfile = paste0(dsnlayerTravail,"/","temp",".vrt") ##chemin du vrt à créer
              cmd = paste(OSGeo4W_path, "gdalbuildvrt",vrtfile,"-input_file_list",nom_ascvrt) ## commande pour exécuter gdalbuildvrt
              system(cmd) 
              
              # Importation du VRT dans GRASS
              nomMNTtemp="MNT_Temp"
              cmd <- paste0("r.in.gdal -o --quiet --overwrite input=", vrtfile, " output=",nomMNTtemp)
              print(cmd);
              system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
              
              # Réglage de la région dans GRASS
              cmd <- paste0("g.region --quiet --overwrite raster=",nomMNTtemp," res=", as.character(reso))
              print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
            }
          }
        }
        # Création d'un buffer pour la région d'intérêt
        nom_contour <- "minibuf"
        # buffer_bief <- st_buffer(Bief, 10)
        # nomminibuf=file.path(dsnlayerTravail, paste0(nom_contour, ".gpkg"))
        # st_write(buffer_bief, nomminibuf, delete_layer=TRUE, quiet=TRUE)
        cmd <- paste0("v.in.ogr --quiet --overwrite input=", nom_Contour_export, " output=", nom_contour)
        print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))

        cmd <- paste0("g.region -a --quiet --overwrite vector=", nom_contour," res=", as.character(reso))
        print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
        
        # liste=nom_contour
        # if (nchar(nomRives)>1){liste=paste0(liste,',',nomRives)}
        # 
        # if (IciLidar==1)
        # {
        #   # cmd <- paste0("g.region -a --quiet --overwrite vector=", nom_contour," res=", as.character(reso))
        #   cmd <- paste0("g.region -a --quiet --overwrite vector=", liste)
        #   print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
        # }else{
        #   cmd <- paste0("g.region -a --quiet --overwrite vector=", liste)
        #   print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
        # }
        
        ################ importation vecteur dans grass grand contour
        ListBief=list()
        ListBiefLig=list()
        ilb=1
        
        # print(Bief$NSection)
        
        
        IndiNSection=sort(unique(Bief$NSection))
        for (inc in 2:length(IndiNSection))
          # for (inc in IndiNSection[2:length(IndiNSection)])
        {
          # if( IndiNSection[inc-1]==9){browser()}
          # cat("Indice Sections: ",IndiNSection[inc-1]," ",IndiNSection[inc],"\n")
          #### recherche de tous les morceaux de sections des indices inc-1 et inc
          nb0=which(Bief$NSection==IndiNSection[inc-1])
          nb1=which(Bief$NSection==IndiNSection[inc])
          cat("Bief",iPont," ",inc, " Section: ",nb0," Section:",nb1)
          if (length(nb0)==0)
          {
            st_write(Bief[nb1,],nomBUG, delete_dsn = T,delete_layer = T, quiet = T)
            print(nomBUG)
            browser()
            BOOM=VOUS_avez_des_trucs_a_verifier
          }
          
          capasse=0
          for (inb0 in nb0)
          {
            for (inb1 in nb1)
            {
              
              
              nbRB=st_intersects(Rives_Final,st_buffer(Bief[c(inb0,inb1),1],0.1))
              # On ne garde que ceux qui touchent  2 fois
              n_intRB = which(sapply(nbRB, length)==2)
              
              # print(n_intRB)
              
              if (length(n_intRB)==2)
              {
                capasse=1
                Rives_=Rives_Final[n_intRB,]
                
                # gestion du sens des rives
                for (isens in 1:2)
                {
                  coords=st_coordinates(Rives_[isens,])
                  
                  
                  point1 = st_point(coords[1,1:2]           )
                  point2 = st_point(coords[nrow(coords),1:2])
                  
                  combined_points <- st_sfc(point1, point2, crs = EPSG)
                  
                  distPt=st_distance(Bief[inb0,1],combined_points)
                  
                  if (min(distPt)==distPt[2])
                  {
                    st_geometry(Rives_[isens,]) =st_sfc(st_linestring(coords[nrow(coords):1,1:2]), crs = EPSG)
                  }
                }
                
                R1=Rives_[1,];coordR1=st_coordinates(R1);PtsR1=st_cast(R1,"POINT");
                AbsDistR1_=c(0,sapply(1:(nrow(PtsR1)-1), function(x) {sum(st_distance(PtsR1,PtsR1)[x,x+1])}))
                AbsDistR1=sapply(1:nrow(PtsR1), function(x) {sum(AbsDistR1_[1:x])})
                DistR1=max(AbsDistR1);
                
                
                R2=Rives_[2,];coordR2=st_coordinates(R2);PtsR2=st_cast(R2,"POINT");
                AbsDistR2_=c(0,sapply(1:(nrow(PtsR2)-1), function(x) {sum(st_distance(PtsR2,PtsR2)[x,x+1])}))
                AbsDistR2=sapply(1:nrow(PtsR2), function(x) {sum(AbsDistR2_[1:x])})
                DistR2=max(AbsDistR2);
                
                
                distR=max(DistR1,DistR2);units(distR)=NULL;nabs_new=ceiling(distR/(reso/2));
                IndiR1=(0:nabs_new)/nabs_new*DistR1
                IndiR2=(0:nabs_new)/nabs_new*DistR2
                
                interpol_R1_X = approxfun(AbsDistR1, coordR1[,1], method="linear")
                interpol_R1_Y = approxfun(AbsDistR1, coordR1[,2], method="linear") 
                R1_X=interpol_R1_X(IndiR1)
                R1_Y=interpol_R1_Y(IndiR1)
                
                interpol_R2_X = approxfun(AbsDistR2, coordR2[,1], method="linear")
                interpol_R2_Y = approxfun(AbsDistR2, coordR2[,2], method="linear") 
                R2_X=interpol_R2_X(IndiR2)
                R2_Y=interpol_R2_Y(IndiR2)
                
                R2R1=matrix(c(R2_X-R1_X,R2_Y-R1_Y),nrow=length(R2_X),ncol=2)
                Largeur=((R2_X-R1_X)^2+(R2_Y-R1_Y)^2)^0.5
                
                ###### travail sur la première section
                L0=Bief[inb0,];coordL0=st_coordinates(L0);
                L0deb=st_sfc(st_buffer(st_point(coordL0[1,1:2]),0.25),crs=EPSG)
                nbL0deb=st_intersects(L0deb,R1)
                n_intL0deb = which(sapply(nbL0deb, length)==1)
                # n_intL0deb = sapply(nbL0deb, length)
                if (length(n_intL0deb)==0){st_geometry(L0) =st_sfc(st_linestring(coordL0[nrow(coordL0):1,1:3]), crs = EPSG)}# Inversion du sens de la géométrie
                coordL0=st_coordinates(L0);
                L0_ext=st_sf(geometry=st_sfc(st_linestring(as.matrix( rbind(coordL0[1,1:2],coordL0[nrow(coordL0),1:2]))),crs=EPSG));#DistL0=st_length(L0_ext);units(DistL0)=NULL
                
                UL0=as.matrix(coordL0[nrow(coordL0),1:2]-coordL0[1,1:2])
                UL0=UL0/((UL0[1]^2+UL0[2]^2)^0.5)
                VL0=UL0;VL0[1]=-UL0[2];VL0[2]=UL0[1]
                DiffL0=matrix(0,dim(coordL0),2)
                DiffL0[,1]=coordL0[,1]-coordL0[1,1]
                DiffL0[,2]=coordL0[,2]-coordL0[1,2]
                AbsDistL0=(DiffL0 %*% UL0)
                lg0=AbsDistL0[length(AbsDistL0)]
                AbsDistL0=AbsDistL0/lg0
                OrdDistL0=(DiffL0 %*% VL0)/lg0
                interpol_L0_sX = approxfun(AbsDistL0, AbsDistL0  , method="linear")
                interpol_L0_sY = approxfun(AbsDistL0, OrdDistL0  , method="linear")
                interpol_L0_Z  = approxfun(AbsDistL0, coordL0[,3], method="linear") 
                
                ###### travail identique sur la deuxième section
                # L1=Bief[inb1,];coordL1=st_coordinates(L1);L1_ext=st_sf(geometry=st_sfc(st_linestring(as.matrix( rbind(coordL1[1,1:2],coordL1[nrow(coordL1),1:2]))),crs=EPSG));#DistL1=st_length(L1_ext);units(DistL1)=NULL
                L1=Bief[inb1,];coordL1=st_coordinates(L1);
                L1deb=st_sfc(st_buffer(st_point(coordL1[1,1:2]),0.25),crs=EPSG)
                nbL1deb=st_intersects(L1deb,R1)
                n_intL1deb = which(sapply(nbL1deb, length)==1)
                # n_intL1deb = sapply(nbL1deb, length)
                if (length(n_intL1deb)==0){st_geometry(L1) =st_sfc(st_linestring(coordL1[nrow(coordL1):1,1:3]), crs = EPSG)}# Inversion du sens de la géométrie
                coordL1=st_coordinates(L1);
                L1_ext=st_sf(geometry=st_sfc(st_linestring(as.matrix( rbind(coordL1[1,1:2],coordL1[nrow(coordL1),1:2]))),crs=EPSG));#DistL1=st_length(L0_ext);units(DistL0)=NULL
                # nomR1=file.path(dsnlayerTravail,"R1.gpkg")
                # st_write(R1,nomR1, delete_dsn = T,delete_layer = T, quiet = T)
                # nomR2=file.path(dsnlayerTravail,"R2.gpkg")
                # st_write(R2,nomR2, delete_dsn = T,delete_layer = T, quiet = T)
                # nomL0=file.path(dsnlayerTravail,"L0.gpkg")
                # st_write(L0,nomL0, delete_dsn = T,delete_layer = T, quiet = T)
                # nomL1=file.path(dsnlayerTravail,"L1.gpkg")
                # st_write(L1,nomL1, delete_dsn = T,delete_layer = T, quiet = T)
                # browser()
                
                UL1=as.matrix(coordL1[nrow(coordL1),1:2]-coordL1[1,1:2])
                UL1=UL1/((UL1[1]^2+UL1[2]^2)^0.5)
                VL1=UL1;VL1[1]=-UL1[2];VL1[2]=UL1[1]
                DiffL1=matrix(0,dim(coordL1),2)
                DiffL1[,1]=coordL1[,1]-coordL1[1,1]
                DiffL1[,2]=coordL1[,2]-coordL1[1,2]
                AbsDistL1=(DiffL1 %*% UL1)
                lg1=AbsDistL1[length(AbsDistL1)]
                AbsDistL1=AbsDistL1/lg1
                OrdDistL1=(DiffL1 %*% VL1)/lg1
                interpol_L1_sX = approxfun(AbsDistL1, AbsDistL1  , method="linear")
                interpol_L1_sY = approxfun(AbsDistL1, OrdDistL1  , method="linear")
                interpol_L1_Z  = approxfun(AbsDistL1, coordL1[,3], method="linear") 
                
                AbsDistL=unique(rbind(AbsDistL0,AbsDistL1))
                AbsDistL=AbsDistL[order(AbsDistL)]
                
                nbug=which(AbsDistL0>1 |AbsDistL0<0)
                if (length(nbug)>0) {cat("Point hors controle",iPont,coordL0[nbug,1],",",coordL0[nbug,2], "a copier dans QGIS dans l'onglet coordonnée de Qgis avec une échelle à 10")}
                nbug=which(AbsDistL0>1 |AbsDistL0<0)
                if (length(nbug)>0) {cat("Point hors controle",iPont,coordL1[nbug,1],",",coordL1[nbug,2], "a copier dans QGIS dans l'onglet coordonnée de Qgis avec une échelle à 10")}
                
                AbsDist=0:ceiling(max(Largeur)/(reso/2))
                AbsDist=AbsDist/max(AbsDist)
                
                s0X=matrix(interpol_L0_sX(AbsDist),nrow=length(AbsDist),1)
                s0Y=matrix(interpol_L0_sY(AbsDist),nrow=length(AbsDist),1)
                s0Z=matrix(interpol_L0_Z(AbsDist),nrow=length(AbsDist),1)
                
                s1X=matrix(interpol_L1_sX(AbsDist),nrow=length(AbsDist),1)
                s1Y=matrix(interpol_L1_sY(AbsDist),nrow=length(AbsDist),1)
                s1Z=matrix(interpol_L1_Z(AbsDist),nrow=length(AbsDist),1)
                
                npts3d_sce=length(R1_X)
                
                # Pts3D_=lapply(1:npts3d_sce, function(x) {
                #   matrix(
                #     cbind(
                #       R1_X[x]+(R2R1[x,1]*((npts3d_sce-x)*(s0X)+x*(s1X))-R2R1[x,2]*((npts3d_sce-x)*(s0Y)+x*(s1Y)))/npts3d_sce,
                #       R1_Y[x]+(R2R1[x,1]*((npts3d_sce-x)*(s0Y)+x*(s1Y))+R2R1[x,2]*((npts3d_sce-x)*(s0X)+x*(s1X)))/npts3d_sce,
                #       ((npts3d_sce-x)*(s0Z    ) + x*(s1Z    ))/npts3d_sce
                #     )
                #     ,nrow=npts3d_sce,ncol=3,,byrow=F)
                # })
                
                Pts3D_X=lapply(1:npts3d_sce, function(x) {
                  R1_X[x]+(R2R1[x,1]*((npts3d_sce-x)*(s0X)+x*(s1X))-R2R1[x,2]*((npts3d_sce-x)*(s0Y)+x*(s1Y)))/npts3d_sce
                })
                Pts3D_Y=lapply(1:npts3d_sce, function(x) {
                  R1_Y[x]+(R2R1[x,1]*((npts3d_sce-x)*(s0Y)+x*(s1Y))+R2R1[x,2]*((npts3d_sce-x)*(s0X)+x*(s1X)))/npts3d_sce
                })
                Pts3D_Z=lapply(1:npts3d_sce, function(x) {
                  ((npts3d_sce-x)*(s0Z)+x*(s1Z                                          ))/npts3d_sce
                })
                
                mespoints=cbind(
                  do.call(rbind,Pts3D_X),
                  do.call(rbind,Pts3D_Y),
                  do.call(rbind,Pts3D_Z)
                )
                
                ici=which(is.na(mespoints[,3]))
                if (length(ici)>0)
                {cat("Points Z NA, comprend pas!")
                  mespoints=mespoints[-ici,]
                }
                
                write.table(round(mespoints,2), file = nom_Pts3D_export_txt, row.names = FALSE, col.names = FALSE, sep = " ", append = TRUE)
                
              }
              if (capasse==0){cat("Il y a un bug sur ces sections, mauvais ordre")}
            }
          }
          cat("\n")
        }
      }
      
      if (EtapMNT==1)
      {
        cat("Conversion Points en MNT")
        # bbox <- st_bbox(Bief)
        # Ouest <- floor(bbox["xmin"])-reso
        # Est <- ceiling(bbox["xmax"])+reso
        # Sud <- floor(bbox["ymin"])-reso
        # Nord <- ceiling(bbox["ymax"])+reso
        # print(bbox)
        
        dneig=3
        XYZ=read.csv(nom_Pts3D_export_txt,sep = " ", header = FALSE)
        Ouest <- (min(floor(XYZ[,1]/reso))-dneig)*reso
        Est  <- (max(floor(XYZ[,1]/reso))+dneig)*reso
        Sud <-  (min(floor(XYZ[,2]/reso))-dneig)*reso
        Nord <-  (max(floor(XYZ[,2]/reso))+dneig)*reso

        # Limitation de la région de travail et gestion de la résolution
        cmd=paste0("g.region --quiet --overwrite"," n=",Nord," s=",Sud," e=",Est," w=",Ouest," res=",as.character(reso))
        # cmd <- paste0("g.region -a --quiet --overwrite vector=", liste)
        print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
        
        # Import du contour
        nomContourg="Contourg"
        cmd=paste0("v.in.ogr -o --quiet --overwrite input=",nom_Contour_export," output=",nomContourg)
        print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))

        # cmd <- paste0("g.region -a --quiet --overwrite vector=", nomContourg)
        # print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
        
        #---- Import du xyz pour connaitre son extension
        NomMNT_PRC1=paste0("MNT_PRC1_",extR[1],"Bief_",formatC(numPRC[indiPRC],width=5,flag="0"))
        NomMNT_PRC2=paste0("MNT_PRC2_",extR[2],"Bief_",formatC(numPRC[indiPRC],width=5,flag="0"))
        nom_MNT_PRC_export1_=file.path(dsnlayerTravail,extR[1],paste0(NomMNT_PRC1,".gpkg"))
        nom_MNT_PRC_export2_=file.path(dsnlayerTravail,extR[2],paste0(NomMNT_PRC2,".gpkg"))
        #execGRASS("r.in.xyz",flags=c("s"),parameters=list(input=Nominput, output=Nomoutput,separator="space"))

        # cmd=paste0("r.in.xyz -s input=",nom_Pts3D_export_txt," separator=space")
        # print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
        
                cmd=paste0("r.in.xyz --quiet --overwrite input=",nom_Pts3D_export_txt," output=",NomMNT_PRC1," separator=space")
        print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
        
        cmd <- paste0("g.region -a --quiet --overwrite zoom=", NomMNT_PRC1)
        print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
        # print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
        
        #---- Exportation du résultat
        cmd=paste0("r.out.gdal --quiet --overwrite -c -f input=",NomMNT_PRC1," output=",nom_MNT_PRC_export1_," type=Float32 format=GPKG nodata=-9999")
        print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
        
        # cmd <- paste0("g.region -a --quiet --overwrite vector=", nom_contour)
        # print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
        
        # Limitation de la région de travail et gestion de la résolution
        cmd=paste0("g.region --quiet --overwrite"," n=",Nord," s=",Sud," e=",Est," w=",Ouest," res=",as.character(reso))
        # cmd <- paste0("g.region -a --quiet --overwrite vector=", liste)
        print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
        
        cmd=paste0("r.mask --quiet --overwrite vector=",nomContourg)
        print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
        
        cmd=paste0("r.neighbors --quiet --overwrite input=",NomMNT_PRC1," output=",NomMNT_PRC2," size=",dneig," method=minimum")
        print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
        
        cmd <- paste0("g.region -a --quiet --overwrite zoom=", NomMNT_PRC2)
        print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
        
        #---- Exportation du résultat
        cmd=paste0("r.out.gdal --quiet --overwrite -c -f input=",NomMNT_PRC2," output=",nom_MNT_PRC_export2_," type=Float32 format=GPKG nodata=-9999")
        print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
        
        #----- Enleve le masque
        cmd=paste0("r.mask -r")
        print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
        
        cmd <- paste0("g.region -a --quiet --overwrite vector=", nom_contour)
        print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
        
      }
    }
  }
  
  # cat("################ exportr lig contrainte et contour")
  # st_write(do.call(rbind,LigCont_),nom_LigCont_export, delete_layer = T, quiet = T)
  
  ########## Création de la liste des dalles concernées
  for (iextR in 1:2)
  {
    listeASC=file.path(dsnlayerTravail,extR[iextR],list.files(file.path(dsnlayerTravail,extR[iextR]),pattern=".gpkg$"))
    nom_ascvrt=file.path(dsnlayerTravail,"listepourvrt.txt")
    file.create(nom_ascvrt)
    write(listeASC, file=nom_ascvrt, append=T)
    vrtfile = paste0(dsnlayerTravail,"/",paste0("MNT_PRC1_",extR[iextR]),".vrt") ##chemin du vrt à créer
    cmd = paste(OSGeo4W_path, "gdalbuildvrt",vrtfile,"-input_file_list",nom_ascvrt) ## commande pour exécuter gdalbuildvrt
    system(cmd) 
  }
  
  if (EtapMNT==1 & nchar(nomTA_LidarC2D)>1)
  {
    # vérifier que l'on ne modife pas MNT C2D
    LC2D=st_read(nomTA_LidarC2D)
    st_crs(LC2D)=crs
    
    nb=st_intersects(LC2D,Contour_Bief)
    n_int <-  which(sapply(nb, length) > 0)
    LC2D <- LC2D[n_int,]
    
    nomMNTModifg=extR[iextR]
    cmd <- paste0("r.in.gdal -o --quiet --overwrite input=", vrtfile, " output=",nomMNTModifg)
    print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
    for (idalle in 1:nrow(LC2D))
    {
      nomdalle=file.path(dirname(nomTA_LidarBase),LC2D$DOSSIERASC,LC2D$NOM_ASC)[idalle]
      nomdalleg=substr(LC2D$NOM_ASC[idalle],1,nchar(LC2D$NOM_ASC[idalle])-5)
      
      nomdalleg_new=paste0(nomdalleg,"_new")
      nomdalle_new=file.path(dirname(nomTA_LidarC2D),LC2D$DOSSIERASC,LC2D$NOM_ASC)[idalle]
      
      cmd <- paste0("r.in.gdal -o --quiet --overwrite input=", nomdalle, " output=",nomdalleg)
      print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
      
      cmd <- paste0("g.region --quiet --overwrite raster=",nomdalleg)
      print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
      
      cmd=paste0("r.patch --quiet --overwrite ","input=",nomMNTModifg,",",nomdalleg," output=",nomdalleg_new)
      print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
      
      cmd=paste0("r.out.gdal --quiet --overwrite -c -f input=",nomdalleg_new," output=",nomdalle_new," type=Float32 format=GPKG nodata=-9999")
      print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
    }
  }
  if (EtapMNT==1){unlink(dirname(SecteurGRASS),recursive=TRUE)}
}

##############################################################################
# Fonction pour 
# ------------couper deux sf de polylignes
# ------------ garder que les polylignes du sf1 qui touchent au moins 2 fois sf2
##############################################################################

gardetouche0_2=function(exten,nom1,nom2,ntouch)
{
  IcSep=strsplit(nom1, "\\.")
  decal=nchar(IcSep[[1]][length(IcSep[[1]])])+1
  nom_coup=paste0(substr(nom1,1,nchar(nom1)-decal),exten)
  
  # On découpe les sections par les lignes de rupture (Rives)
  cmd=paste0(qgis_process,
             " run native:splitwithlines",
             " INPUT=", nom1,
             " LINES=", nom2,
             " OUTPUT=",   nom_coup)
  print(cmd); system(cmd)
  
  Vect1=st_read(nom_coup)
  Vect=st_read(nom2)
  
  nb=st_intersects(Vect1,st_buffer(Vect,0.1))
  # On ne garde que ceux qui touchent 0 ou 2 fois
  n_int = which(sapply(nb, length)==1)
  Vect1$Nbre=sapply(nb, length)
  if (length(n_int)>0)
  {
    Vect1=Vect1[-n_int,]
  }
  
  nom_vect=paste0(substr(nom1,1,nchar(nom1)-decal),"final",exten)
  
  nici=which(Vect1$Nbre==ntouch);if(length(nici)>0){Vect1=Vect1[-nici,]}
  st_write(Vect1,nom_vect, delete_dsn = T,delete_layer = T, quiet = T)
  
  # On agrandit un peu si on veut refaire le calcul
  # Vect2=st_intersection(st_read(nom1),st_buffer(Vect1,0.05))
  # browser()
  nom_vect1_tampon=paste0(substr(nom1,1,nchar(nom1)-decal),"tamponpourreutilisation",exten)
  nom_vect2=paste0(substr(nom1,1,nchar(nom1)-decal),"pourreutilisation",exten)
  #  st_write(Vect2,nom_vect2, delete_dsn = T,delete_layer = T, quiet = T)
  
  cmd=paste0(qgis_process, " run native:buffer",
             " --INPUT=",nom_vect,
             " --DISTANCE=0.05",
             " --SEGMENTS=5 --END_CAP_STYLE=0 --JOIN_STYLE=0 --MITER_LIMIT=2 --DISSOLVE=True",
             " --OUTPUT=",nom_vect1_tampon)
  system(cmd)
  
  # couper intérieur du tampon
  cmd=paste0(qgis_process,
             " run native:clip", 
             " --distance_units=meters",
             "--area_units=m2",
             " INPUT=", nom1,
             " OVERLAY=", nom_vect1_tampon,
             " OUTPUT=", nom_vect2)
  
  print(cmd); system(cmd)
  
  return(Vect1)
}

##############################################################################
# Fonction pour gérer les attributs des sections
##############################################################################
PRC_Attributs=function(nomlayerPontRivCanaux)
{
  PRC=st_read(nomlayerPontRivCanaux)
  cat("si ca casse, bug de linestring ou multilinestring\n")
  PRC=st_cast(PRC,"LINESTRING")
  
  toto=strsplit(nomlayerPontRivCanaux,"\\.")
  ext=toto[[1]][length(toto[[1]])]
  cle=substr(basename(nomlayerPontRivCanaux),1,nchar(basename(nomlayerPontRivCanaux))-nchar(ext)-1)
  listeacopier=list.files(dirname(nomlayerPontRivCanaux),pattern=cle)
  rep_sauve=file.path(dirname(nomlayerPontRivCanaux),"SauvegardePRC",format(Sys.time(),format="%Y%m%d_%H%M%S"))
  dir.create(rep_sauve,recursive=T)
  file.copy(file.path(dirname(nomlayerPontRivCanaux),listeacopier),file.path(rep_sauve,listeacopier))
  
  #Gestion des anciennes notation
  nici=which(colnames(PRC)=="ZsiType1")
  if (length(nici)==0){PRC$ZsiType1=PRC$Zsi}
  st_crs(PRC)=crs
  
  #
  unlink(file.path(dirname(nomlayerPontRivCanaux),listeacopier))
  
  
  ici=which(PRC$NBief==0)
  if (length(ici)>0)
  {
    PRC$NBief[ici]=max(PRC$NBief,na.rm = T)+1:length(ici)
    PRC$NSection[ici]=1
  }
  
  # Travail pour compléter les attributs de PRC
  for (inc in 2:nrow(PRC))
  {
    
    # Si le type est vide ou le bief est vide ou de ZsiType1, reprise du type précédent
    if (is.na(PRC$Type[inc])==T)    {PRC$Type[inc]    =PRC$Type[inc-1]}
    if (is.na(PRC$NBief[inc])==T)   {PRC$NBief[inc]   =PRC$NBief[inc-1]}
    if (is.na(PRC$ZsiType1[inc])==T & PRC$Type[inc]==1){PRC$ZsiType1[inc]=PRC$ZsiType1[inc-1]}
    # Si le type est vide (Z hors attributs)
    # if (is.na(PRC$ZsiType1[inc])==T){PRC$ZsiType1[inc]=-99}
    # # Si le type n'est pas égal à 1 (Z hors attributs)
    # if (PRC$Type[inc]!=1){PRC$ZsiType1=-99}
    # Si la section est vide, on augmente de 1
    if (is.na(PRC$NSection[inc])==T){PRC$NSection[inc]=PRC$NSection[inc-1]+1}
  }
  # nomexp=paste0(substr(nomlayerPontRivCanaux,1,nchar(nomlayerPontRivCanaux)-nchar(ext)),"_att.gpkg")
  nomexp=paste0(substr(nomlayerPontRivCanaux,1,nchar(nomlayerPontRivCanaux)-nchar(ext)),"gpkg")
  PRC=PRC[order(PRC$NBief,PRC$NSection),]
  PRC=PRC[,cbind("NBief","NSection","Type","ZsiType1")]
  st_write(PRC,nomexp, delete_layer = T, quiet = T)
  return(nomexp)
}

##############################################################################
# Fonction pour 
##############################################################################

