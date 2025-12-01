Decrenelage=function (nomrtovect,AireMin_Inter,AireMin_Exter,EPSG,Reso)
{
  # Effacer la console
  cat("\014")
  
  # Charger le package sf et définir le chemin d'accès au fichier exécutable QGIS
  library(sf)
  
  # #~Paramètres
  # # Définir les noms de fichiers pour les données d'entrée et de sortie
  # nomrtovect <- "C:\\Cartino2D\\France\\_Exzeco\\Exzeco020_Mtp.shp"
  # nomrtovect <- "C:\\AFFAIRES\\Montpellier\\14_ALEA20240715\\Res20240715_1120\\ALEA\\emprise.gpkg"
  # nomrtovect <- "C:\\Cartino2D\\France\\MAMP2024_05m\\_FUSION\\MAMP2024_05mtSceMaxMedContFrance__ZI.gpkg"
  # nomrtovect <- "C:\\Cartino2D\\France\\MAMP2024_05m\\_FUSION\\testrastvect.gpkg"
  # nomrtovect <- "C:\\AFFAIRES\\Montpellier\\14_ALEA20240715\\Res20240715_1120\\ALEA\\BufPlus3_Moins3\\A_4_PPRi_ZI1.gpkg"
  # EPSG <- 2154
  # Reso <- 1;AireMin=100
  # Reso <- 5;AireMin=500
  # raci="shyreg_spPB_C6147_713830km_X789149Y6302130_T0100_D24_PIC08__ZI"
  EtapDecrProc=c(1,1,1) # mettre en 1ère ou 2ème place le nombre de proc mais cela ne marche pas!
  Suff_Fich_Temp=TRUE
  
  #################### Gestion des noms
  raci=basename(nomrtovect)
  raci=substr(raci,1,nchar(raci)-5)
  nomnoholes <- file.path(dirname(nomrtovect), paste0(raci,'_sanstrous.gpkg'))
  nompts <- file.path(dirname(nomrtovect), paste0(raci,'_Pts.gpkg'))
  nomcarre <- file.path(dirname(nomrtovect),paste0(raci,'_carre.gpkg'))
  nomlosange <- file.path(dirname(nomrtovect),paste0(raci,'_losange.gpkg'))
  nomdecre1 <- file.path(dirname(nomrtovect),paste0(raci,'_decre1.gpkg'))
  nommerge <- file.path(dirname(nomrtovect),paste0(raci,'_Qgis_Merge.gpkg'))
  nombuffer <- file.path(dirname(nomrtovect),paste0(raci,'_Qgis_Buffer.gpkg'))
  nomboom <- file.path(dirname(nomrtovect),paste0(raci,'_Qgis_Boom.gpkg'))
  nomsimply <- file.path(dirname(nomrtovect),paste0(raci,'_Qgis_Simpli.gpkg'))
  nomfinal1 <- file.path(dirname(nomrtovect),paste0(raci,'_Qgis_Final_Aint',AireMin_Inter,'.gpkg'))
  nomfinal2 <- file.path(dirname(nomrtovect),paste0(raci,'_Qgis_Final_Aint',AireMin_Inter,'_Aext',AireMin_Exter,'.gpkg'))
  
  # Enlever les petits trous
  cmd <- paste0(qgis_process, " run native:deleteholes",
                " --INPUT=", shQuote(nomrtovect),
                " --MIN_AREA=", AireMin_Inter,
                " --OUTPUT=", shQuote(nomnoholes))
  print(cmd); system(cmd)
  
  # Lire les données vecteur
  vect <- st_read(nomnoholes)
  
  ###############################################################################
  # GESTION DES POINTS DE CONTACT
  ###############################################################################
  # ----------------- Fonction appelée en boucle ---------------------------------
  DebutFin=function(incL2)
  {  
    # Rechercher les points début-fin identiques pour chaque polygone et supprimer les doublons
    nbL2 <- which(Coords[,4] == NPoly[incL2])
    nmorc <- unique(Coords[nbL2,3])
    for (incL1 in nmorc) {
      nbL1 <- which(Coords[nbL2,3] == incL1)
      nbL2L1 <- nbL2[nbL1]
      
      test <- identical(
        as.numeric(Coords[nbL2L1[1], 1:2]),
        as.numeric(Coords[nbL2L1[length(nbL2L1)], 1:2])
      )
      if (test == TRUE)
      {
        if (incL1==1)
        {
          Indi=nbL2L1[1]
        }else{
          Indi=c(Indi,nbL2L1[1])
        }
      }
    }
    return(Indi)
  }
  # ----------------- Fonction appelée en boucle ---------------------------------
  
  if (EtapDecrProc[1]>0)
  {
    # Extraire les coordonnées des points de contact et créer un data frame
    Coords <- data.frame(st_coordinates(vect))
    cat(dim(Coords), "\n")
    Coords$ID <- 1:dim(Coords)[1]
    Coords$DebFin <- 0
    Coords$Doublons <- 0
    cat("Recherche des points début fin identique sur chaque polygone\n")
    cat(max(Coords$L1), "\n")
    cat(max(Coords$L2), "\n")
    cat(max(Coords$ID), "\n")
    
    NPoly <- length(unique(Coords$L2))
    
    if (NPoly>1)
    {
      cat(format(Sys.time(),format="%Y%m%d_%H%M%S"),"\n")
      # Calculer les points milieux et créer de nouveaux polygones
      if (EtapDecrProc[1]==1)
      {
        pgb <- txtProgressBar(min = 0, max = length(NPoly), style = 3)
        for (incL2 in 1:length(NPoly)) 
        {
          setTxtProgressBar(pgb, incL2)
          IndiF=DebutFin(incL2)
          Coords[IndiF,6]=1
        }
        cat("\n")
      }else{
        
        #Mode Parallèle
        cat("------ ",EtapDecrProc[1]," CALCULS MODE PARALLELE -------------\n")
        require(foreach)
        cl <- parallel::makeCluster(EtapDecrProc[1])
        doParallel::registerDoParallel(cl)
        foreach(incL2 = 1:length(NPoly),
                .combine = 'c',
                .inorder = FALSE,
                .export = "DebutFin") %dopar% 
          {
            IndiF=DebutFin(incL2)
            Coords[IndiF,6]=1
          }
        parallel::stopCluster(cl)
      }

      cat(format(Sys.time(),format="%Y%m%d_%H%M%S"),"\n")
      # Supprimer les doublons
      
      # Récupération du 1er point de tous les polygones (on considère qu'il est égal au dernier)
      cat(dim(Coords), "\n")
      Coords$DebFin[sapply(unique(Coords$L2), function(x) {which(Coords$L2==x)[1]})]=1
      
      Coords <- Coords[-which(Coords$DebFin == 1), ]
      cat(dim(Coords), "\n")
      # browser()
      # Trier les coordonnées et identifier les doublons
      Coords <- Coords[order(Coords$X, Coords$Y, Coords$L1, Coords$L2), ]
      indi <- 1:(dim(Coords)[1] - 1)
      Coords$Doublons[-1] <- ifelse((Coords[indi + 1, 1] - Coords[indi, 1]) == 0 & (Coords[indi + 1, 2] - Coords[indi, 2]) == 0, 1, 0)
      # browser()
      # Sélectionner les doublons
      Coords <- Coords[which(Coords$Doublons == 1), ]
      cat(dim(Coords), "\n")
      
      # Créer des points à partir des coordonnées et les écrire dans un fichier GeoPackage
      Pts <- st_cast(st_sfc(st_multipoint(x = as.matrix(Coords[, 1:2]), dim = "XY")), "POINT")
      st_crs(Pts) <- EPSG
      st_write(Pts, nompts, delete_layer = T, quiet = T)
      
      # Créer des carrés à partir des points et les écrire dans un fichier GeoPackage
      Carre <- st_buffer(Pts, endCapStyle = "SQUARE", dist = 1.001* (Reso / 2) * 2^0.5 / 2)
      st_write(Carre, nomcarre, delete_layer = T, quiet = T)
      
      # Utiliser QGIS pour créer des losanges en faisant pivoter les carrés de 45 degrés et les écrire dans un fichier GeoPackage
      cmd <- paste0(qgis_process, " run native:rotatefeatures",
                    " --distance_units=meters --area_units=m2 --ellipsoid=EPSG:7019 ",
                    " --INPUT=", shQuote(nomcarre),
                    " ANGLE=45",
                    " --OUTPUT=", nomlosange)
      
      print(cmd); system(cmd)
    }
  }
  ################################################################################
  ############ DECALAGE DES POINTS POUR ENLEVER LES CRENEAUX #####################
  ################################################################################
  # ----------------- Fonction appelée en boucle ---------------------------------
  modif_GEOM=function(ivect)
  {
    coords_list <- list()
    
    setTxtProgressBar(pgb, ivect)
    
    Coords <- st_coordinates(st_geometry(Vect2[ivect, ]))
    nmorc <- unique(Coords[, 3])
    for (incL1 in nmorc) {
      nbL1 <- which(Coords[, 3] == incL1)
      nbL1_ <- nbL1[c(2:length(nbL1), 2)]
      coords_list[[incL1]] <- as.matrix((Coords[nbL1, 1:2] + Coords[nbL1_, 1:2]) / 2)
    }
    return(coords_list)
  }
  # ----------------- Fonction appelée en boucle ---------------------------------

  if (EtapDecrProc[2]>0)
  {
    # Segmenter les polygones
    Vect2 <- st_segmentize(vect, Reso)
    
    
    cat(format(Sys.time(),format="%Y%m%d_%H%M%S"),"\n")
    # Calculer les points milieux et créer de nouveaux polygones
    if (EtapDecrProc[2]==1)
    {
      cat("Calculer les points milieux et modifier les polygones\n")
      # Mode Classique
      pgb <- txtProgressBar(min = 0, max = dim(Vect2)[1], style = 3)
      for (ivect in 1:dim(Vect2)[1])
      {
        coords_list=modif_GEOM(ivect)
        if (length(coords_list) == 1) {
          st_geometry(Vect2[ivect, ]) <- st_sfc(st_polygon(list(coords_list[[1]])), crs = EPSG)
        } else {
          # On pourrait calculer l'aire des polygones intérieurs...
          st_geometry(Vect2[ivect, ]) <- st_sfc(st_polygon(coords_list), crs = EPSG)
        }
      }
      cat("\n")
    }else{
      #Mode Parallèle
      cat("------ ",EtapDecrProc[2]," CALCULS MODE PARALLELE -------------\n")
      require(foreach)
      cl <- parallel::makeCluster(EtapDecrProc[2])
      doParallel::registerDoParallel(cl)
      foreach(ivect = 1:dim(Vect2)[1],
              .packages = c("sf"),
              .combine = 'c',
              .inorder = FALSE) %dopar% 
        {
          coords_list=modif_GEOM(ivect)
          if (length(coords_list) == 1) {
            st_geometry(Vect2[ivect, ]) <- st_sfc(st_polygon(list(coords_list[[1]])), crs = EPSG)
          } else {
            # On pourrait calculer l'aire des polygones intérieurs...
            st_geometry(Vect2[ivect, ]) <- st_sfc(st_polygon(coords_list), crs = EPSG)
          }
        }
      
      parallel::stopCluster(cl)
      
    }
    cat(format(Sys.time(),format="%Y%m%d_%H%M%S"),"\n")
    # Écrire les nouveaux polygones dans un fichier GeoPackage
    st_write(st_simplify(Vect2, preserveTopology = T, dTolerance = 0),
             nomdecre1, delete_layer = T, quiet = T)
  }

  if (EtapDecrProc[1]>0 & EtapDecrProc[2]>0 & EtapDecrProc[3]>0)
  {
    if (NPoly>1)
    {
      # Fusionner les polygones décalés avec les losanges à l'aide de QGIS
      cmd <- paste0(qgis_process, " run native:mergevectorlayers",
                    " --LAYERS=", shQuote(nomdecre1),
                    " --LAYERS=", shQuote(nomlosange),
                    " --CRS=QgsCoordinateReferenceSystem('EPSG:", EPSG, "') ",
                    " --OUTPUT=", shQuote(nommerge))
      print(cmd); system(cmd)
      
      # Créer une mise en tampon autour des polygones fusionnés à l'aide de QGIS
      cmd <- paste0(qgis_process, " run native:buffer",
                    " --INPUT=", shQuote(nommerge),
                    " --DISTANCE=0 --SEGMENTS=5 --END_CAP_STYLE=0 --JOIN_STYLE=0 --MITER_LIMIT=2 --DISSOLVE=True",
                    " --OUTPUT=", shQuote(nombuffer))
      print(cmd); system(cmd)
    }else{
      nombuffer=nomdecre1
    }
    
    # Découper les polygones multipartites en polygones simples à l'aide de QGIS
    cmd <- paste0(qgis_process, " run native:multiparttosingleparts",
                  " --INPUT=", shQuote(nombuffer),
                  " --OUTPUT=", shQuote(nomboom))
    print(cmd); system(cmd)
    
    # Simplifier la géométrie
    cmd <- paste0(qgis_process, " run native:simplifygeometries",
                  " --distance_units=meters --area_units=m2 --ellipsoid=EPSG:7019",
                  " --INPUT=", shQuote(nomboom),
                  " --METHOD=0 --TOLERANCE=0.1",
                  " --OUTPUT=", shQuote(nomsimply))
    print(cmd); system(cmd)
    
    # Enlever les petits trous
    cmd <- paste0(qgis_process, " run native:deleteholes",
                  " --INPUT=", shQuote(nomsimply),
                  " --MIN_AREA=", AireMin_Inter,
                  " --OUTPUT=", shQuote(nomfinal1))
    print(cmd); system(cmd)
    
    VectF1=st_read(nomfinal1)
    VectF1$Aire=st_area(VectF1)
    units(VectF1$Aire)=NULL
    ici=which(VectF1$Aire>=AireMin_Exter)
    if (length(ici)>0)
    {
      VectF1=VectF1[ici,]
      st_write(VectF1,nomfinal2, delete_layer = T, quiet = T)
      cat("#####################################################################\n")
      cat("---------------------------------------------------------------------\n")
      cat("Fichier final decrenelé:",nomfinal2,"\n")
      
    }
    
    if(Suff_Fich_Temp==TRUE)
    {
      # unlink(nompts)
      # # unlink(nomcarre)
      # # unlink(nomlosange)
      # unlink(nomdecre1) 
      # unlink(nommerge) 
      # unlink(nombuffer)
      # unlink(nomboom) 
      # unlink(nomsimply)
    }
  }
  return(nomfinal2)
}