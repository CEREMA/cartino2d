#################################################################################################
################   Etape1_Exzeco
#################################################################################################
fun_creer_monde_grass <- function(SecteurGRASS, BatGRASS, EPSG, nom_secteur) {
  loc_path <- dirname(SecteurGRASS)
  
  tentative <- 1
  ok <- FALSE
  
  while (!ok && tentative <= 3) {
    if (dir.exists(loc_path)) {
      unlink(loc_path, recursive = TRUE)
    }
    
    # Appel de création GRASS
    system(paste0(BatGRASS, " -c EPSG:", EPSG, " -e \"", loc_path, "\""))
    
    # Petite pause pour laisser Windows valider la création du dossier
    Sys.sleep(1)
    
    if (dir.exists(loc_path)) {
      ok <- TRUE
    } else {
      cat("[Attente/Retry] Échec création dossier GRASS (tentative", tentative, ") :", basename(loc_path), "\n")
      Sys.sleep(1.5)
      tentative <- tentative + 1
    }
  }
  
  if (!ok) {
    stop(paste("ERREUR FATALE : Impossible de créer le dossier GRASS pour le secteur :", nom_secteur))
  }
}
fun_export_csv_grass <- function(raster_input, csv_output, SecteurGRASS, BatGRASS) {
  cmd <- paste0("r.out.xyz --quiet --overwrite input=", raster_input, " output=", csv_output, " separator=comma")
  
  tentative <- 1
  ok <- FALSE
  
  while (!ok && tentative <= 3) {
    system(paste0(BatGRASS, " ", SecteurGRASS, " --exec ", cmd))
    
    # Laisser 1 seconde à Windows pour vider le cache d'écriture
    Sys.sleep(1)
    
    if (file.exists(csv_output) && file.info(csv_output)$size > 0) {
      ok <- TRUE
    } else {
      cat("[Attente/Retry] Fichier CSV vide (tentative", tentative, ") :", basename(csv_output), "\n")
      if (file.exists(csv_output)) unlink(csv_output)
      Sys.sleep(2)
      tentative <- tentative + 1
    }
  }
  
  if (!ok) {
    stop(paste("ERREUR FATALE : Impossible de générer le CSV non vide :", csv_output))
  }
}

# Travail sur les contours d'Exzeco pour réaliser les points de contrainte et sur les limites du domaine
Etape1_Contour = function(dsnlayer, contour, crs)
{
  # Buffer du contour, simplification de la géométrie et segmentation
  cat(nomcontour, " ---- Création de la zone de calcul\n")
  
  Resultats=Etape1_GrandContourBordDecoupe(contour,contour$Cont1_Buf,contour$Exzeco)
  contour_segm_fin = Resultats[[1]]
  Contoursanstrou= Resultats[[2]]
  contour_segm= Resultats[[3]]
  
  if(is.na(contour$SourceDeb) | nchar(contour$SourceDeb)==0)
  {
    #### Forcage PLUIE
    
    # Maillage destructuré
    contour_int <- contour_segm %>% 
      st_union() %>% 
      st_buffer(contour$Cont2_Buf) %>%
      sfheaders::sf_remove_holes()
  }else{
    # Ajout 24/11/2023 FP
    nom_SourcesDebits=contour$SourceDeb
    if (file.exists(nom_SourcesDebits)==T)
    {
      SourcesDebits=st_read(file.path(nom_SourcesDebits))
      st_crs(SourcesDebits)=crs
      fusion=st_union(st_geometry(contour),st_union(st_buffer(st_geometry(SourcesDebits),2*contour$Cont1_Dx)))
      fusion=st_cast(fusion,"POLYGON")
      AireF=st_area(fusion)
      fusion=fusion[which(AireF==max(AireF))]
    }else{
      fusion=contour
    }
    # Fin Ajout 24/11/2023 FP
    
    # Forcage DEBIT
    if (is.na(as.numeric(contour$Exzeco))==T)
    {
      contour_int <- fusion %>% 
        st_union() %>% 
        st_buffer(contour$Cont1_Buf+contour$Cont2_Buf) %>% 
        sfheaders::sf_remove_holes()
    }else{
      # Maillage reglé débit
      DistBuff=2*as.numeric(contour$Exzeco)
      contour_int <- fusion %>% 
        st_buffer(contour$Cont1_Buf+contour$Cont2_Buf+DistBuff) %>% 
        st_union() %>% 
        st_buffer(-DistBuff) %>% 
        sfheaders::sf_remove_holes()
    }
  }
  
  st_write(
    contour_int,
    dsn = file.path(dsnlayerC, paste0(nom_STEP2_Zone_Valid_Calcul, ".shp")),
    layer = nom_STEP2_Zone_Valid_Calcul,
    delete_layer = T,
    quiet = T
  )
  file.copy(file.path(chem_routine,"C2D",Doss_Qml,paste0(nom_STEP2_Zone_Valid_Calcul, ".qml")),
            file.path(dsnlayerC,paste0(nom_STEP2_Zone_Valid_Calcul, ".qml")))
  
  
  
  return(list(contour_segm_fin, contour_int,Contoursanstrou))
}

########################################################################################·
Etape1_GrandContourBordDecoupe=function(GrandContour,Cont1Buf,Reso)
{
  
  if (is.na(as.numeric(contour$Exzeco))==T)
  {  
    Contoursanstrou <- GrandContour %>% 
      
      st_union() %>% 
      st_simplify(preserveTopology = FALSE,
                  dTolerance = 0.5 * contour$Cont1_Dx) %>%
      st_segmentize(contour$Cont1_Dx, crs = crs) %>% 
      st_buffer(Cont1Buf)%>% 
      sfheaders::sf_remove_holes() 
  }else{
    
    # Maillage reglé buffer plus moins pour éviter des rapprochements liés à des grandes résolutions
    DistBuff=2*as.numeric(contour$Exzeco)
    Contoursanstrou <- GrandContour %>% 
      st_union() %>% 
      st_simplify(preserveTopology = FALSE,
                  dTolerance = 0.5 * contour$Cont1_Dx) %>%
      st_segmentize(contour$Cont1_Dx, crs = crs) %>%
      st_buffer(Cont1Buf+DistBuff) %>%
      st_union() %>%
      st_buffer(-DistBuff) %>% 
      sfheaders::sf_remove_holes()
  }
  
  
  contour_segm <- Contoursanstrou %>%
    st_simplify(preserveTopology = FALSE,
                dTolerance = 0.5 * contour$Cont1_Dx) %>%
    st_segmentize(contour$Cont1_Dx, crs = crs)
  
  # Travail sur le contour segment pour commencer le MNT à 0.5
  # objetif être complètement raccord avec Lidar IGN
  
  contour_segm_Pts1 <- contour_segm %>%
    sfheaders::sf_remove_holes() %>% 
    st_cast("MULTILINESTRING", do_split = TRUE)  %>%
    st_cast("LINESTRING", do_split = TRUE) %>%
    st_cast("MULTIPOINT", do_split = TRUE) %>%
    st_cast("POINT", do_split = TRUE)
  
  contour_segm_Pts2 <-
    do.call(rbind, st_geometry(contour_segm_Pts1))
  
  contour_segm_Pts2 <-
    cbind(round(contour_segm_Pts2[, 1]), round(contour_segm_Pts2[, 2]))
  
  contour_segm_fin <- contour_segm_Pts2 %>%
    as.matrix() %>%
    st_multipoint(dim = "XYZ") %>%
    st_cast("MULTILINESTRING", do_split = TRUE) %>%
    st_cast("POLYGON") %>%
    st_sfc() %>%
    st_sf(a = 0)
  
  st_crs(contour_segm_fin) <- st_crs(contour)
  
  # Exportation du contour segmenté
  st_write(
    contour_segm_fin,
    dsn = file.path(dsnlayerC, paste0(nom_STEP1_Contour, ".shp")),
    layer = nom_STEP1_Contour,
    delete_layer = T,
    quiet = T
  )
  
  file.copy(file.path(chem_routine,"C2D",Doss_Qml,paste0(nom_STEP1_Contour, ".qml")),
            file.path(dsnlayerC,paste0(nom_STEP1_Contour, ".qml")))
  
  return(list(contour_segm_fin, Contoursanstrou,contour_segm))
}


Etape1_Exzeco = function(dsnlayer, contour, contour_int,Contoursanstrou,crs)
{ 
  # --- Sous-fonction de filtrage par BLOCS GÉOGRAPHIQUES DYNAMIQUES ---
  FiltreParBlocs = function(pts, seuil) {
    if (nrow(pts) <= 1) return(pts)
    
    # 1. Définition dynamique du nombre de blocs (1 pour 5000 pts environ)
    n_blocs <- max(1, floor(nrow(pts) / 5000))
    xmin <- min(pts[,1]); xmax <- max(pts[,1])
    if (xmax == xmin) breaks <- c(xmin - 1, xmax + 1) else breaks <- seq(xmin, xmax, length.out = n_blocs + 1)
    
    # 2. Fonction de filtrage pour un bloc (avec gestion des bords)
    TraiterUnBloc = function(k) {
      x_start <- breaks[k]; x_end <- breaks[k+1]
      idx_bloc_large <- which(pts[,1] >= (x_start - seuil) & pts[,1] <= (x_end + seuil))
      if (length(idx_bloc_large) == 0) return(NULL)
      
      sub_pts <- pts[idx_bloc_large, , drop = FALSE]
      sub_pts <- sub_pts[order(sub_pts[,1]), ] 
      
      keep <- rep(TRUE, nrow(sub_pts))
      seuil_sq <- seuil^2
      
      for (i in 1:(nrow(sub_pts) - 1)) {
        if (!keep[i]) next
        for (j in (i + 1):nrow(sub_pts)) {
          if (!keep[j]) next
          if ((sub_pts[j, 1] - sub_pts[i, 1]) > seuil) break
          d2 <- (sub_pts[j, 1] - sub_pts[i, 1])^2 + (sub_pts[j, 2] - sub_pts[i, 2])^2
          if (d2 < seuil_sq) keep[j] <- FALSE
        }
      }
      
      res_pts <- sub_pts[keep, , drop = FALSE]
      if (k == 1) {
        idx_final <- which(res_pts[,1] >= x_start & res_pts[,1] <= x_end)
      } else {
        idx_final <- which(res_pts[,1] > x_start & res_pts[,1] <= x_end)
      }
      return(res_pts[idx_final, , drop = FALSE])
    }
    
    liste_blocs <- lapply(1:n_blocs, TraiterUnBloc)
    pts_finaux <- do.call(rbind, liste_blocs)
    return(pts_finaux)
  }
  
  # Points d'extrémités buses (à protéger absolument)
  Pts_Fixes_OH <- matrix(ncol = 2, nrow = 0)
  
  ################ Lecture données
  if (is.na(contour$Exzeco) == FALSE){
    GOON=0
    if (is.na(as.numeric(contour$Exzeco)) == TRUE & nchar(contour$Exzeco)>8)
    {
      Exzeco <- st_read(contour$Exzeco); GOON=1
    }else{
      if (substr(contour$Exzeco,1,5)=="courb")
      {
        NomCourb_5="Courb_i5"
        Exzeco <- st_read(file.path(dsnlayerC,paste0(NomCourb_5,".gpkg")))
        Exzeco$AIRE=st_area(Exzeco)
        SeuilAExz=25; units(SeuilAExz) = "m2"
        nici=which(Exzeco$AIRE<SeuilAExz)
        if (length(nici)>0){Exzeco=Exzeco[-nici,]}
        st_write(Exzeco,file.path(dsnlayerC,paste0(NomCourb_5,"_nettoye.gpkg")), delete_layer = T, quiet = T)
        GOON=1
      }
    }
    
    if (GOON==1)
    {
      st_crs(Exzeco) <- crs; st_crs(contour_int) <- crs
      Exzeco <- st_crop(Exzeco,contour_int)
      nb <- st_intersects(Exzeco, contour_int)
      n_int <- which(sapply(nb, length) > 0)
      Exzeco <- Exzeco[n_int,]
      
      Exzeco_Union <- st_make_valid(st_union(Exzeco))
      Exzeco_Int <- st_intersection(Exzeco_Union, contour_int)
      Exzeco_Int <- st_collection_extract(Exzeco_Int, "POLYGON") 
      
      Exz1_Pts <- matrix(ncol = 2, nrow = 0); Exz2_Pts <- matrix(ncol = 2, nrow = 0); Exz3_Pts <- matrix(ncol = 2, nrow = 0)
      
      if (nrow(st_as_sf(Exzeco_Int)) > 0) {
        nom_exp <- "Step_Cartino2d_2_1_Exz1"; f_exz1 <- file.path(dsnlayerC, paste0(nom_exp, ".gpkg"))
        if(file.exists(f_exz1)){
          Exz1 <- st_read(f_exz1, quiet = TRUE)
          if(nrow(Exz1)>0) Exz1_Pts <- st_coordinates(Exz1)[, 1:2]
        } else {
          Result1 <- try(Buf_et_Coup(dsnlayerC, nom_exp, nom_STEP2_Zone, Exzeco_Int, Contoursanstrou, contour$Exz1_Buf, contour$Exz1_Dx, contour$Ratio_Dx, crs))
          if (!inherits(Result1, "try-error")) Exz1_Pts <- Result1$Exz_Pts
        }
        
        cat(nomcontour, " ---- 2ème contrainte Exzeco\n")
        nom_exp <- "Step_Cartino2d_2_2_Exz2"; f_exz2 <- file.path(dsnlayerC, paste0(nom_exp, ".gpkg"))
        if(file.exists(f_exz2)){
          Exz2 <- st_read(f_exz2, quiet = TRUE)
          if(nrow(Exz2)>0) Exz2_Pts <- st_coordinates(Exz2)[, 1:2]
        } else {
          Result2 <- try(Buf_et_Coup(dsnlayerC, nom_exp, nom_STEP2_Zone, Exzeco_Int, contour, contour$Exz2_Buf, contour$Exz2_Dx, contour$Ratio_Dx, crs))
          if (!inherits(Result2, "try-error")) Exz2_Pts <- Result2$Exz_Pts
        }
        
        cat(nomcontour, " ---- 3ème contrainte Exzeco\n")
        nom_exp <- "Step_Cartino2d_2_3_Exz3"; f_exz3 <- file.path(dsnlayerC, paste0(nom_exp, ".gpkg"))
        if(file.exists(f_exz3)){
          Exz3 <- st_read(f_exz3, quiet = TRUE)
          if(nrow(Exz3)>0) Exz3_Pts <- st_coordinates(Exz3)[, 1:2]
        } else {
          if (!is.na(contour$Exz3_Buf) && !is.na(contour$Exz3_Dx)) {
            Result3 <- try(Buf_et_Coup(dsnlayerC, nom_exp, nom_STEP2_Zone, Exzeco_Int, contour, contour$Exz3_Buf, contour$Exz3_Dx, contour$Ratio_Dx, crs))
            if (!inherits(Result3, "try-error")) Exz3_Pts <- Result3$Exz_Pts
          }
        }
      }
      
      Exz_Pts <- rbind(Exz1_Pts, Exz2_Pts, Exz3_Pts)
      
      # --- FILTRE 1 : FOND DE PLAN (2.49m) ---
      if (nrow(Exz_Pts) > 1) {
        cat(nomcontour, " ---- Filtrage par blocs du terrain naturel (< 2.49m)\n")
        NbInitial_Pre <- nrow(Exz_Pts)
        Exz_Pts <- FiltreParBlocs(Exz_Pts, 2.49)
        
        cat("   -> Export fond de plan filtré vers 'Verif_Points_Filtres_249.gpkg'\n")
        pts_pre_sf <- st_as_sf(data.frame(x = Exz_Pts[, 1], y = Exz_Pts[, 2]), coords = c("x", "y"), crs = crs)
        st_write(pts_pre_sf, dsn = file.path(dsnlayerC, "Verif_Points_Filtres_249.gpkg"), delete_layer = TRUE, quiet = TRUE)
      }
      
      nom_OuvHydrau <- contour$OH
      if (file.exists(nom_OuvHydrau) == TRUE)
      {
        OuvHydrau <- st_read(nom_OuvHydrau)
        nbRC <- st_intersects(OuvHydrau, contour)
        n_int <- which(sapply(nbRC, length) > 0 & OuvHydrau$CIRC >= 0 & OuvHydrau$CIRC <= 2)
        
        if (length(n_int) > 0)
        {
          cat(nomcontour," ---- Récupération des ouvrages hydrauliques\n")
          OuvHydrau <- OuvHydrau[n_int,]
          OuvHydrau$LENGTH <- round(as.numeric(st_length(OuvHydrau)), 2)
          OH_BoutsPts <- do.call(rbind, lapply(1:dim(OuvHydrau)[1], function(x) { cbind(st_coordinates(OuvHydrau[x,])[c(1, dim(st_coordinates(OuvHydrau[x,]))[1]), 1:2], x, rbind(1, 2)) }))
          
          len_num <- as.numeric(OuvHydrau$LENGTH); idx_OH_traiter <- 1:nrow(OuvHydrau)
          
          if (length(idx_OH_traiter) > 0) {
            cat(nomcontour, " ---- Création zones d'exclusion et extrémités réelles\n")
            Standard_Buf <- if(!is.null(contour$Exz1_Dx)) contour$Exz1_Dx - 0.50 else 2.5
            Buf_Sizes <- rep(Standard_Buf, nrow(OuvHydrau))
            Buf_Sizes[which(len_num < 8)] <- 1.5; Buf_Sizes[which(len_num >= 8 & len_num <= 12)] <- 2.0
            Sizes_Subset <- Buf_Sizes[idx_OH_traiter]
            
            indices_starts <- 2 * idx_OH_traiter - 1; indices_ends <- 2 * idx_OH_traiter
            Coords_Start <- OH_BoutsPts[indices_starts, 1:2, drop=FALSE]; Coords_End <- OH_BoutsPts[indices_ends,   1:2, drop=FALSE]
            Pts_Start_SF <- st_as_sf(data.frame(x=Coords_Start[,1], y=Coords_Start[,2]), coords=c("x","y"), crs=crs)
            Pts_End_SF   <- st_as_sf(data.frame(x=Coords_End[,1],   y=Coords_End[,2]),   coords=c("x","y"), crs=crs)
            
            Global_Exclusion_Zone <- st_make_valid(st_union(c(st_geometry(st_buffer(Pts_Start_SF, dist = Sizes_Subset)), st_geometry(st_buffer(Pts_End_SF, dist = Sizes_Subset)))))
            
            if (nrow(Exz_Pts) > 0) {
              inter_oh <- st_intersects(st_as_sf(data.frame(x = Exz_Pts[, 1], y = Exz_Pts[, 2]), coords = c("x", "y"), crs = crs), st_buffer(Global_Exclusion_Zone, dist = 3.0))
              Exz_Pts <- Exz_Pts[which(lengths(inter_oh) == 0), , drop = FALSE]
            }
            
            Dist_Ligne_Pts <- Sizes_Subset - 0.2
            All_Inner_Bufs <- c(st_geometry(st_buffer(Pts_Start_SF, dist = Dist_Ligne_Pts)), st_geometry(st_buffer(Pts_End_SF, dist = Dist_Ligne_Pts)))
            Geom_Union <- st_make_valid(st_union(All_Inner_Bufs))
            
            Pts_Couronne <- NULL
            if(!is.null(Geom_Union) && length(Geom_Union) > 0) {
              Indiv_Boundaries <- st_boundary(All_Inner_Bufs)
              All_Candidates <- st_cast(st_line_sample(Indiv_Boundaries, n = 6), "POINT")
              Selection <- st_is_within_distance(All_Candidates, st_boundary(Geom_Union), dist = 0.02)
              if (any(lengths(Selection) > 0)) Pts_Couronne <- st_coordinates(All_Candidates[lengths(Selection) > 0])[, 1:2, drop=FALSE]
            }
            
            Pts_Fixes_OH <- unique(rbind(Coords_Start, Coords_End))
            Aj_Pts <- Pts_Couronne
          }
          
          # --- INTEGRATION DES ATTRIBUTS HYDRAULIQUES ---
          cat("   -> Intégration des attributs hydrauliques dans le SHP...\n")
          
          # Sauvegarde de la hauteur initiale dans HAUT2 avant modification pour CIRC=2
          OuvHydrau$HAUT2 <- as.numeric(OuvHydrau$HAUT1)
          
          # Paramètres standards
          OuvHydrau$FRIC <- 1 / 80
          OuvHydrau$CE1 <- 0.5; OuvHydrau$CE2 <- 0.5; OuvHydrau$CS1 <- 1; OuvHydrau$CS2 <- 0.5
          OuvHydrau$CLP <- 1; OuvHydrau$LBUS <- 0; OuvHydrau$CV <- 0
          OuvHydrau$C56 <- if("Q_theo" %in% names(OuvHydrau)) as.numeric(OuvHydrau$Q_theo) else 0
          OuvHydrau$CV5 <- 0; OuvHydrau$C5 <- 0; OuvHydrau$CT <- 0
          OuvHydrau$D1 <- 0; OuvHydrau$D2 <- 0; OuvHydrau$A1 <- 0; OuvHydrau$A2 <- 0; OuvHydrau$AA <- 1
          
          # Cas des voûtes (CIRC=2) : on calcule la hauteur hydraulique équivalente
          nc2 = which(OuvHydrau$CIRC == 2)
          if (length(nc2) > 0) { 
            OuvHydrau[nc2,]$HAUT1 <- as.numeric(OuvHydrau[nc2,]$Surf / OuvHydrau[nc2,]$LRG)
            OuvHydrau[nc2,]$CIRC <- 0 
          }
          
          # Export du SHP avec tous les nouveaux champs
          st_write(OuvHydrau, dsn = file.path(dsnlayerC, "OuvHydrau_Select.shp"), delete_layer = T, quiet = T)
          
          # --- Suite du maillage ---
          Exz_Dx <- contour$Exz1_Dx; Ratio <- 0.6
          Pool_OH <- rbind(Pts_Fixes_OH, Aj_Pts)
          if (!is.null(Pool_OH) && nrow(Pool_OH) > 0) {
            Exz_Pts <- GardeNvxPointsLoinPointsInitiaux(Exz_Pts, Pool_OH, Ratio, Exz_Dx)
            Exz_Pts <- rbind(Aj_Pts, Exz_Pts) 
          }
        }
      }
      
      # # LIGNES DE CONTRAINTE
      # nom_LigContr <- contour$LigContr
      # if (file.exists(nom_LigContr) == TRUE)
      # {
      #   LigContr <- st_read(nom_LigContr); LigContr <- st_cast(st_cast(LigContr, "MULTILINESTRING"), "LINESTRING")
      #   nbRC <- st_intersects(LigContr, contour); n_int <- which(sapply(nbRC, length) > 0)
      #   
      #   if (length(n_int) > 0)
      #   {
      #     cat(nomcontour," ---- Récupération des lignes de contraintes\n")
      #     LigContr = st_intersection(LigContr[n_int,], st_buffer(Contoursanstrou, contour$Cont2_Buf / 2))
      #     # Nettoyage GEOMETRYCOLLECTION
      #     LigContr = st_collection_extract(LigContr, "LINESTRING")
      #     Pool_OH_Total <- if(exists("Aj_Pts")) rbind(Pts_Fixes_OH, Aj_Pts) else Pts_Fixes_OH
      #     if (!is.null(Pool_OH_Total) && nrow(Pool_OH_Total) > 0){
      #       # LigContr <- st_difference(LigContr, st_sf(st_union(st_buffer(st_as_sf(as.data.frame(Pool_OH_Total), coords=c(1,2), crs=crs), contour$Exz1_Dx))))
      #       # Buffer de nettoyage
      #       clean_buf <- st_union(st_buffer(st_as_sf(as.data.frame(Pool_OH_Total), coords=1:2, crs=crs), contour$Exz1_Dx))
      #       LigContr <- st_difference(LigContr, st_sf(geometry=st_sfc(clean_buf, crs=crs)))
      #       # Nettoyage GEOMETRYCOLLECTION post-difference
      #       LigContr = st_collection_extract(LigContr, "LINESTRING")
      #     } 
      #     
      #     
      #     # Simple2 = st_segmentize(st_simplify(LigContr, dTolerance = 0.05), 0.1)
      #     # Simplification et échantillonnage
      #     Simple2 = st_segmentize(st_simplify(LigContr, dTolerance = 0.05), 0.1)
      #     # Re-cast final pour garantir le succès de st_coordinates
      #     Simple2 = st_cast(Simple2, "LINESTRING")
      #     Simple2 = Simple2[which(as.numeric(st_length(Simple2)) > (0.68 * contour$Exz1_Dx)),]
      #     
      #     if(nrow(Simple2)>0){
      #       LigContr_Pts = st_coordinates(Simple2)[,1:2]
      #       Exz_Pts = GardeNvxPointsLoinPointsInitiaux(Exz_Pts, LigContr_Pts, 0.5, contour$Exz1_Dx)
      #       Exz_Pts = rbind(Exz_Pts, LigContr_Pts)
      #     }
      #   }
      # }
      # LIGNES DE CONTRAINTE
      nom_LigContr <- contour$LigContr
      if (file.exists(nom_LigContr) == TRUE)
      {
        LigContr <- st_read(nom_LigContr); LigContr <- st_cast(st_cast(LigContr, "MULTILINESTRING"), "LINESTRING")
        # --- SECURITE CRS ---
        if(st_crs(LigContr) != crs) LigContr <- st_transform(LigContr, crs)
        
        nbRC <- st_intersects(LigContr, contour); n_int <- which(sapply(nbRC, length) > 0)
        
        if (length(n_int) > 0)
        {
          cat(nomcontour," ---- Récupération des lignes de contraintes\n")
          LigContr = st_intersection(LigContr[n_int,], st_buffer(Contoursanstrou, contour$Cont2_Buf / 2))
          LigContr = st_collection_extract(LigContr, "LINESTRING")
          
          # Échantillonnage à 0.2m (Plus robuste que 0.1m)
          Simple2 = st_segmentize(st_simplify(LigContr, dTolerance = 0.05), 0.2)
          Simple2 = st_cast(Simple2, "LINESTRING")
          
          if(nrow(Simple2) > 0) {
            LigContr_Pts = st_coordinates(Simple2)[,1:2]
            
            # Nettoyage des points de contrainte près des buses
            Pool_OH_Total <- if(exists("Aj_Pts") && !is.null(Aj_Pts)) rbind(Pts_Fixes_OH, Aj_Pts) else Pts_Fixes_OH
            if (!is.null(Pool_OH_Total) && nrow(Pool_OH_Total) > 0) {
              LigContr_Pts = GardeNvxPointsLoinPointsInitiaux(LigContr_Pts, Pool_OH_Total, 1.0, contour$Exz1_Dx)
            }
            
            if (nrow(LigContr_Pts) > 0) {
              cat("   -> Intégration de", nrow(LigContr_Pts), "points de contrainte...\n")
              # Filtrage interne des contraintes pour supprimer les points trop proches (< 1.29m)
              LigContr_Pts <- FiltreParBlocs(LigContr_Pts, 1.29)
              
              # Nettoyage terrain vs contrainte
              Exz_Pts = GardeNvxPointsLoinPointsInitiaux(Exz_Pts, LigContr_Pts, 0.5, contour$Exz1_Dx)
              Exz_Pts = rbind(Exz_Pts, LigContr_Pts)
            }
          }
        }
      }
    } else{
      Exz_Pts = NaN
    }
  }
  
  # --- FILTRE 2 : FINAL PAR BLOCS (1.29m) ---
  if (!all(is.na(Exz_Pts)) && is.matrix(Exz_Pts) && nrow(Exz_Pts) > 1) {
    cat(nomcontour, " ---- Filtrage géographique final (< 1.29m)\n")
    Exz_Pts <- FiltreParBlocs(Exz_Pts, 1.29)
  }
  
  # --- RÉINTÉGRATION FINALE DES EXTRÉMITÉS OH (INTOUCHABLES) ---
  if (nrow(Pts_Fixes_OH) > 0) {
    Exz_Pts <- rbind(Pts_Fixes_OH, Exz_Pts)
    cat("   -> Réintégration de", nrow(Pts_Fixes_OH), "points d'extrémités buses.\n")
  }
  
  # Export final nettoyé
  if (!all(is.na(Exz_Pts)) && nrow(Exz_Pts) > 0) {
    cat("   -> Export final nettoyé vers 'Verif_Points_Filtres_129.gpkg'\n")
    pts_filt_sf <- st_as_sf(data.frame(x = Exz_Pts[, 1], y = Exz_Pts[, 2]), coords = c("x", "y"), crs = crs)
    st_write(pts_filt_sf, dsn = file.path(dsnlayerC, "Verif_Points_Filtres_129.gpkg"), delete_layer = TRUE, quiet = TRUE)
  }
  
  return(Exz_Pts)
}
#################################################################################################
################   Etape1_MNT
#################################################################################################

Etape1_MNT = function(dsnlayerC,
                      Lidar,
                      dsnLidar,
                      # Exz_Pts,
                      contour_segm,
                      contour_int)
{
  #############################################################################
  ################ LIDAR #####################################################
  #############################################################################
  cat(nomcontour, " ---- Récupération du Lidar\n")
  # Création du repertoire des fichiers asc
  dsnlayerCL = paste0(dsnlayerC, '/ASC')
  
  # récupération des dalles concernées
  nb = st_intersects(Lidar, contour_segm)
  n_int = which(sapply(nb, length) > 0)
  LidarC = Lidar[n_int,]
  
  # Création de la liste des dalles concernées
  if(length(which(is.na(LidarC$DOSSIERASC)))==0){
    listeASC = paste0(dsnLidar, '/', LidarC$DOSSIERASC, '/', LidarC$NOM_ASC)
  }else{
    listeASC = paste0(dsnLidar, '/', LidarC$NOM_ASC)
    
  }
  
  # Creation du fichier virtuel
  nom_ascvrt = file.path(dsnlayerC, "listepourvrt.txt")
  file.create(nom_ascvrt)
  write(listeASC, file = nom_ascvrt, append = T)
  vrtfile = paste0(dsnlayerC, "\\", nom_MNT, ".vrt") ##chemin du vrt à créer
  cmd = paste(OSGeo4W_path,"gdalbuildvrt",vrtfile,"-input_file_list",nom_ascvrt)
  print(cmd);system(cmd)
  unlink(nom_ascvrt)
  
  # Importation raster dans Grass
  cmd=paste0("r.in.gdal -o --quiet --overwrite input=",vrtfile," output=",nom_MNT)
  print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
  
  # importation vecteur dans grass grand contour
  cmd=paste0("v.in.ogr -o --quiet --overwrite input=",file.path(dsnlayerC, paste0(nom_STEP1_Contour, ".shp"))," output=",nom_STEP1_Contour)
  print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
  
  # importation de la zone de validité de calcul
  cmd=paste0("v.in.ogr -o --quiet --overwrite input=",file.path(dsnlayerC, paste0(nom_STEP2_Zone_Valid_Calcul, ".shp"))," output=",nom_STEP2_Zone_Valid_Calcul)
  print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
  
  # déclaration de la région sur le raster
  # Si on a un maillage reglé
  # if (is.na(Exz_Pts)[1] == TRUE)
  if (is.na(as.numeric(contour$Exzeco))==F)
  {
    # cmd=paste0("g.region --overwrite --quiet -a vector=",nom_STEP1_Contour," res=",as.character(contour$Exzeco)," align=",nom_MNT)
    cmd=paste0("g.region --overwrite --quiet -a vector=",nom_STEP1_Contour," res=",as.character(contour$Exzeco))
    print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
    
  }else{
    # Si on a un maillage destructuré
    cmd=paste0("g.region --overwrite --quiet -a vector=",nom_STEP1_Contour," align=",nom_MNT)
    print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
  }
  
  cmd=paste0("r.mask --quiet --overwrite vector=",nom_STEP1_Contour)
  print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
  
  # Ajout 15/03/2024 pour la gestion des mnt 25m sur une résolution 25m décalée en grille
  # nom_MNTMask="MasquesAGarder"
  # cmd=paste0("r.resample --quiet --overwrite input=","MASK"," output=",nom_MNTMask)
  # print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
  
  # ajout du 18/02/2025 pour gérer les -9999 consideré comme des données dans les rasters ign
  cmd=paste0("r.null map=",nom_MNT," setnull=-9999,-999")
  print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
  
  # cmd=paste0("r.resample --quiet --overwrite input=",nom_MNT," output=",nom_MNT2a)
  # Modification de l résolution
  nom_MNT2a = paste0(nom_MNT, 'mask1a')
  cmd=paste0("r.resamp.stats --quiet --overwrite input=",nom_MNT," output=",nom_MNT2a)
  print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
  
  # Ré-échantillonage des Stricklers et CN si maillage grille
  if (is.na(as.numeric(contour$Exzeco))==F)
  {
    
  }
  
  #Debut20231107
  # Comblement de la partie intérieure
  ########
  cmd=paste0("r.mask --quiet --overwrite vector=",nom_STEP2_Zone_Valid_Calcul)
  print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
  
  #----- supp 2026 04 16
  #----- 
  # NomUnivar = file.path(dsnlayerC, "univar.txt")
  # cmd=paste0("r.univar --quiet --overwrite map=",nom_MNT2a," output=",NomUnivar)
  # print(cmd);toto=system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
  # if (toto!=0){cat("Vous avez un problème dans ",BatGRASS,"\n");BUGDEGRASS=BOOM}
  # MinimMNT =max(as.numeric(scan(file = NomUnivar,NomUnivar,sep = ":",skip = 6,nlines = 1,dec = ".")[2]),-50)
  # print(MinimMNT)
  #----- supp 2026 04 16
  
  # 2026 04 16
  cmd=paste0("r.univar --quiet --overwrite map=",nom_MNT2a)
  print(cmd);toto=system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd),intern=T)
  nlig=grep(toto,pattern="minimum: ")[1]
  MinimMNT=as.numeric(strsplit(toto[nlig],":")[[1]][2])
  cat(toto[nlig],"\n")
  cat("Minimum Altitude: ",MinimMNT, "\n")
  
  # Creation d'un MNT à l'altitude mnimale
  MNTMini="MNTpourcomber"
  cmd=paste0("g.remove --quiet -f type=raster name=",MNTMini)
  print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
  
  cmd=paste0("r.mask --quiet --overwrite vector=",nom_STEP1_Contour)
  print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
  
  cmd=paste0("r.mapcalc --quiet --overwrite ",MNTMini,"=",MinimMNT,"*","MASK")
  print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
  
  # Patch avec en priorite le MNT et ailleurs le MNT a altitude minimale
  nom_MNT2 = paste0(nom_MNT, 'CombleInterieur')
  cmd=paste0("r.patch --quiet --overwrite ","input=",nom_MNT2a,",",MNTMini," output=",nom_MNT2," nprocs=",NProcGrass)
  print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
  
  cmd=paste0("r.mask --quiet --overwrite vector=",nom_STEP1_Contour)
  print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
  
  nom_MNT2a=nom_MNT2
  #Fin20231107
  
  
  ####
  nfiltre=as.numeric(substr(contour$Exzeco,6,nchar(contour$Exzeco)))
  if (is.na(nfiltre)==F)
  {
    ########
    cmd=paste0("r.mask --quiet --overwrite vector=",nom_STEP2_Zone_Valid_Calcul)
    print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
    
    #---- Creation du filtre et Ã©criture dans un fichier
    centre=(nfiltre+1)/2
    Matric=matrix(0, ncol = nfiltre, nrow=nfiltre)
    
    for (inc in 1:centre)
    {
      decal=centre-inc
      for (ij in 0:decal)
      {
        Matric[centre+ij,centre+(decal-ij)]=2*inc-1
        Matric[centre+ij,centre-(decal-ij)]=2*inc-1
        Matric[centre-ij,centre+(decal-ij)]=2*inc-1
        Matric[centre-ij,centre-(decal-ij)]=2*inc-1
      }
    }
    
    nomexport=paste0(dsnlayerC,"/Filtre_",as.character(nfiltre),".txt")
    write.table(Matric,nomexport, quote = FALSE,col.names = FALSE,row.names = FALSE)
    
    #---- Calcul dans GRASS de la moyenne sur un voisinage pondere par le filtre
    # execGRASS("r.neighbors",flags=c("quiet","overwrite"),parameters=list(input=nominput,output=nomoutput,size=ifiltre,weight=nomexport))
    nomoutput=paste0("MNT_Nei",nfiltre)
    cmd=paste0("r.neighbors --quiet --overwrite input=",nom_MNT2a," output=",nomoutput," size=",nfiltre," weight=",nomexport," nprocs=",NProcGrass)
    print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
    
    #---- Calcul de la courbure
    nominput=nomoutput
    nomoutput=contour$Exzeco
    # execGRASS("r.param.scale",flags=c("quiet","overwrite"),parameters=list(input=nominput, output=nomoutput,method="longc"))
    cmd=paste0("r.param.scale --quiet --overwrite input=",nominput," output=",nomoutput," method=longc")
    print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
    
    #---- Export
    cmd=paste0("r.out.gdal --quiet --overwrite -c -f input=",nomoutput," output=",paste0(dsnlayerC, "/", nomoutput, ".gpkg")," type=Float32 format=GPKG nodata=-9999")
    print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))  
    file.copy(file.path(chem_routine,"C2D",Doss_Qml,paste0(nomoutput, ".qml")),
              file.path(dsnlayerC,paste0(nomoutput, ".qml")))
    
    NomCourb_1="Courb_i1"
    # r.mapcalc expression=aaaaa = if( abs(Courb5)>0.052,1,null() )
    cmd=paste0("r.mapcalc --overwrite ",shQuote(paste0(NomCourb_1,"=if(abs( ",nomoutput,")>0.052,1,null())")))
    print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
    
    NomCourb_2="Courb_i2"
    # r.buffer input=aaaaa@Temp output=aaaaaabuf distances=5
    cmd=paste0("r.buffer --quiet --overwrite input=",NomCourb_1," output=",NomCourb_2," distance=",nfiltre)
    print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
    
    # r.mask -i --overwrite raster=aaaaaabuf@Temp
    cmd=paste0("r.mask -i --quiet --overwrite raster=",NomCourb_2)
    print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
    
    NomCourb_3="Courb_i3"
    # r.resample input=MASK@Temp output=aze
    cmd=paste0("r.resample --quiet --overwrite input=","MASK"," output=",NomCourb_3)
    print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
    
    # r.mask --overwrite vector=Step_Cartino2d_2_Valid_Calcul@Temp
    cmd=paste0("r.mask --quiet --overwrite vector=",nom_STEP2_Zone_Valid_Calcul)
    print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
    
    NomCourb_4="Courb_i4"
    # r.buffer input=aze@Temp output=aze2 distances=5
    cmd=paste0("r.buffer --quiet --overwrite input=",NomCourb_3," output=",NomCourb_4," distance=",ceiling(nfiltre/2))
    print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
    
    # r.mask -i raster=aze2@Temp
    cmd=paste0("r.mask -i --quiet --overwrite raster=",NomCourb_4)
    print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
    
    NomCourb_5="Courb_i5"
    # r.resample input=MASK@Temp output=toto
    cmd=paste0("r.resample --quiet --overwrite input=","MASK"," output=",NomCourb_5)
    print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
    
    # r.mask --overwrite vector=Step_Cartino2d_2_Valid_Calcul@Temp
    cmd=paste0("r.mask --quiet --overwrite vector=",nom_STEP2_Zone_Valid_Calcul)
    print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
    
    # g.region -a raster=MASK@Temp res=5 
    cmd=paste0("g.region -a --overwrite --quiet"," raster=","MASK"," res=",nfiltre)
    print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
    
    # r.to.vect -s --overwrite input=toto@Temp output=toto type=area
    cmd=paste0("r.to.vect -s --quiet --overwrite input=",NomCourb_5," output=",NomCourb_5," type=area")
    print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
    
    cmd=paste0("v.out.ogr --quiet --overwrite input=",NomCourb_5," output=",file.path(dsnlayerC,paste0(NomCourb_5,".gpkg"))," format=GPKG")
    print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
    
    # Si on a un maillage destructuré
    cmd=paste0("g.region --overwrite --quiet -a vector=",nom_STEP1_Contour," align=",nom_MNT)
    print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
    
    cmd=paste0("r.mask --quiet --overwrite vector=",nom_STEP1_Contour)
    print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
  }
  
  
  # S'il y a des DEBITS
  if(!(is.na(contour$SourceDeb) | nchar(contour$SourceDeb)==0))
  {
    # Suppression du masque (s'il existe)
    cmd=paste0("r.mask -r")
    print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
    
    nom_du_fichier_polyligne=paste0(nom_STEP2_Zone_Valid_Calcul,"polyline")
    cmd=paste0("v.type --quiet --overwrite input=",nom_STEP2_Zone_Valid_Calcul," output=",nom_du_fichier_polyligne," from=boundary to=line")
    print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
    
    nom_du_fichier_polyligne_buf=paste0(nom_STEP2_Zone_Valid_Calcul,"polylinebuf")
    # v.buffer input=Step_Cartino2d_2_Valid_Calculpolyline@Temp output=buff distance=10
    # Fred distance fixe, à améliorer
    cmd=paste0("v.buffer --quiet --overwrite input=",nom_du_fichier_polyligne," output=",nom_du_fichier_polyligne_buf," distance=25")
    print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
    
    cmd=paste0("r.mask --quiet --overwrite vector=",nom_du_fichier_polyligne_buf)
    print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
    
    #----- supp 2026 04 16
    # NomUnivar = file.path(dsnlayerC, "univar.txt")
    # cmd=paste0("r.univar --quiet --overwrite map=",nom_MNT2a," output=",NomUnivar)
    # print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
    #     MinimMNT =max(as.numeric(scan(file = NomUnivar,NomUnivar,sep = ":",skip = 6,nlines = 1,dec = ".")[2]),-50)
    # cat("Minimum de la zone valide",MinimMNT,"\n")
    #----- supp 2026 04 16
    
    # 2026 04 16
    cmd=paste0("r.univar --quiet --overwrite map=",nom_MNT2a)
    print(cmd);toto=system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd),intern=T)
    nlig=grep(toto,pattern="minimum: ")[1]
    MinimMNT=as.numeric(strsplit(toto[nlig],":")[[1]][2])
    cat(toto[nlig],"\n")
    cat("Minimum Altitude: ",MinimMNT, "\n")
    
    
    nomPtsBas="PtsBas"
    cmd=paste0("r.mapcalc --overwrite ",shQuote(paste0(nomPtsBas,"=if(",nom_MNT2a," <(",max(contour$COTEAVALM, contour$COTEAVALAJ +MinimMNT),"),",1,',null())')))
    system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
    
    # Suppression du masque (s'il existe)
    cmd=paste0("r.mask -r")
    print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
    
    nomAval="Aval"
    cmd=paste0("r.buffer --quiet --overwrite input=",nomPtsBas," output=",nomAval," distance=",contour$Cont1_Buf)
    print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
    
    nomAval2="Aval2"
    cmd=paste0("r.mapcalc --overwrite ",shQuote(paste0(nomAval2,"=if(",nomAval," >-99,",1,',null())')))
    system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
    
    cmd=paste0("r.to.vect --quiet --overwrite input=",nomAval2," output=",nomAval," type=area")
    print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
    
    cmd=paste0("v.out.ogr --quiet --overwrite input=",nomAval," output=",file.path(dsnlayerC,paste0(nomAval,".gpkg"))," format=GPKG")
    print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
    
    Aval=sfheaders:: sf_remove_holes(st_read(file.path(dsnlayerC,paste0(nomAval,".gpkg"))))
    
    st_write(Aval, file.path(dsnlayerC,paste0(nomAval,".gpkg")),delete_layer = T,quiet = T)
    
    cmd=paste0("v.in.ogr -o --quiet --overwrite input=",file.path(dsnlayerC,paste0(nomAval,".gpkg"))," output=",nomAval)
    print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
    
    if (is.na(as.numeric(contour$Exzeco))==F)
    {
      contour_int2=contour_int
    }else{
      # Si on a un maillage destructuré
      contour_int2=st_buffer(contour_int,contour$Cont1_Dx)
    }
    
    contour_segm_fin <-  sfheaders:: sf_remove_holes(st_union(st_union(contour_int2,st_geometry(Aval))))
    
    
    Etape1_GrandContourBordDecoupe(contour_segm_fin,0,contour$Exzeco)
    
    # st_write(
    #   contour_segm_fin,
    #   dsn = file.path(dsnlayerC, paste0(nom_STEP1_Contour, ".shp")),
    #   layer = nom_STEP1_Contour,
    #   delete_layer = T,
    #   quiet = T
    # )
    
    # Nouvelle importation pour mise à jour
    cmd=paste0("v.in.ogr -o --quiet --overwrite input=",file.path(dsnlayerC, paste0(nom_STEP1_Contour, ".shp"))," output=",nom_STEP1_Contour)
    print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
    
    # modif FP objectif est de mettre d'avoir un mnt plein jusqu'à la limite du contour de calcul
    #----- Partie pour calculer l'altitude minimale dans tout le buffer
    #----- Future condition limite projeteée plus loin!
    #----- supp 2026 04 16
    # NomUnivar = file.path(dsnlayerC, "univar.txt")
    # cmd=paste0("r.univar --quiet --overwrite map=",nom_MNT2a," output=",NomUnivar)
    # print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
    #     MinimMNT =max(as.numeric(scan(file = NomUnivar,NomUnivar,sep = ":",skip = 6,nlines = 1,dec = ".")[2]),-50)
    # print(MinimMNT)
    #----- supp 2026 04 16
    
    # 2026 04 16
    cmd=paste0("r.univar --quiet --overwrite map=",nom_MNT2a)
    print(cmd);toto=system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd),intern=T)
    nlig=grep(toto,pattern="minimum: ")[1]
    MinimMNT=as.numeric(strsplit(toto[nlig],":")[[1]][2])
    cat(toto[nlig],"\n")
    cat("Minimum Altitude: ",MinimMNT, "\n")
    
    # Creation d'un MNT à l'altitude mnimale
    MNTMini="MNTpourcomber"
    cmd=paste0("g.remove --quiet -f type=raster name=",MNTMini)
    print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
    
    cmd=paste0("r.mask --quiet --overwrite vector=",nom_STEP2_Zone_Valid_Calcul)
    print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
    
    cmd=paste0("r.mapcalc --quiet --overwrite ",MNTMini,"=",MinimMNT,"*","MASK")
    print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
    
    # Patch avec en priorite le MNT et ailleurs le MNT a altitude minimale
    nom_MNT2 = paste0(nom_MNT, 'mask1')
    cmd=paste0("r.patch --quiet --overwrite ","input=",nom_MNT2a,",",MNTMini," output=",nom_MNT2," nprocs=",NProcGrass)
    print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
    
  }else{
    # nom_MNT2 <- nom_MNT
  }
  
  #----- Partie pour calculer l'altitude minimale de la zone de calcul
  #----- Future condition limite projeteée plus loin!
  cmd=paste0("r.mask --quiet --overwrite vector=",nom_STEP2_Zone_Valid_Calcul)
  print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
  
  #----- supp 2026 04 16
  NomUnivar = file.path(dsnlayerC, "univar.txt")
  cmd=paste0("r.univar --quiet --overwrite map=",nom_MNT2," output=",NomUnivar)
  print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
  COTE_AVAL = max(contour$COTEAVALM,
                  contour$COTEAVALAJ +
                    as.numeric(
                      scan(file = NomUnivar,NomUnivar,sep = ":",skip = 6,nlines = 1,dec = "."))[2])
  print(COTE_AVAL)
  #----- supp 2026 04 16
  
  # 2026 04 16
  cmd=paste0("r.univar --quiet --overwrite map=",nom_MNT2)
  print(cmd);toto=system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd),intern=T)
  nlig=grep(toto,pattern="minimum: ")[1]
  MinimMNT=as.numeric(strsplit(toto[nlig],":")[[1]][2])
  cat(toto[nlig],"\n")
  cat("Minimum Altitude: ",MinimMNT, "\n")
  COTE_AVAL = max(contour$COTEAVALM, contour$COTEAVALAJ + MinimMNT)
  cat("COTE_AVAL: ",MinimMNT, "\n")
  
  cmd=paste0("r.mask -r")
  print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
  
  # v.to.rast input=Step_Cartino2d_2_Valid_Calcul@Temp output=Valid use=cat value=0
  # conversion en raster
  cmd=paste0("v.to.rast --quiet --overwrite input=",nom_STEP2_Zone_Valid_Calcul," output=",nom_STEP2_Zone_Valid_Calcul," use=cat value=0")
  print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
  
  cmd=paste0("r.mask --quiet --overwrite vector=",nom_STEP1_Contour)
  print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
  
  
  
  Bords="BordBuffer"
  nom_MNT4 = "MNTFinal"
  if(!(is.na(contour$SourceDeb) | nchar(contour$SourceDeb)==0))
  {
    # DEBIT
    # Buffer et on met une valeur basse
    cmd=paste0("r.buffer --quiet --overwrite input=",nom_STEP2_Zone_Valid_Calcul," output=",Bords," distance=",(-1.2*contour$Cont2_Buf))
    print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
    
    cmd=paste0("r.mask --quiet --overwrite vector=",nomAval)
    print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
    
    Bords2="BordBuffer2"
    cmd=paste0("r.mapcalc --quiet --overwrite ",
               paste0(Bords2,"=",MinimMNT,"+(1-",Bords,")*",-contour$DecAltiMNT))
    print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
    
    cmd=paste0("r.mask -i --quiet --overwrite vector=",nom_STEP2_Zone_Valid_Calcul)
    print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
    
    Bords3="BordBuffer3"
    cmd=paste0("r.resample --quiet --overwrite input=",Bords2," output=",Bords3)
    print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
    
    cmd=paste0("r.mask --quiet --overwrite vector=",nom_STEP1_Contour)
    print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
    
    cmd=paste0("r.series --overwrite input=",nom_MNT2,",",Bords3," output=",nom_MNT4," method=minimum"," nprocs=",NProcGrass)
    print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
  }else{
    #PLUIE
    # Buffer sur le raster avec increment de distance
    for (id in 1:(-1.2*contour$Cont2_Buf)){dista=ifelse(id==1,id,paste(dista,id,sep=","))}
    
    cmd=paste0("r.buffer --quiet --overwrite input=",nom_STEP2_Zone_Valid_Calcul," output=",Bords," distance=",dista)
    print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
    
    # r.mapcalc expression=result = MNT@Temp + (VamlidBuffer@Temp -1)*(-25)/150
    # Mise à la valeur 1
    
    cmd=paste0("r.mapcalc --quiet --overwrite ",
               paste0(nom_MNT4,"=",nom_MNT2,"-(",Bords,"-1)*(",contour$DecAltiMNT/contour$Cont2_Buf),")")
    print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
  }
  
  ### Maj 20211012
  cmd=paste0("g.region --overwrite --quiet"," raster=",nom_MNT4," zoom=",nom_MNT4)
  print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
  
  if (is.na(as.numeric(contour$Exzeco)) == T)
  {
    if (file.exists(file.path(dsnlayerC, "OuvHydrau_Select.shp")) == TRUE)
    {
      OuvHydrau = st_read(dsn = dsnlayerC,
                          layer = "OuvHydrau_Select")
      
      # Récupération de la géométrie
      OuvHydrau_Debut = sapply(1:dim(OuvHydrau)[1],
                               function(x) {
                                 cbind(st_coordinates(OuvHydrau[x,])[1, 1:2])
                               })
      
      # Creation point bout avec altitude et largeur oh
      OuvHydrau_PtDebFin = rbind(do.call(rbind,
                                         lapply(1:dim(OuvHydrau)[1],
                                                function(x)
                                                {
                                                  st_sf(data.frame(
                                                    Z = OuvHydrau$Z1[x],
                                                    LRG = OuvHydrau$LRG[x],
                                                    LENGTH = OuvHydrau$LENGTH[x], # AJOUT
                                                    "geometry" =
                                                      st_sfc(st_point(st_coordinates(OuvHydrau[x,])[1, 1:2]), dim = "XY")
                                                  ),
                                                  crs = EPSG)
                                                })),
                                 do.call(rbind,
                                         lapply(1:dim(OuvHydrau)[1],
                                                function(x)
                                                {
                                                  st_sf(data.frame(
                                                    Z = OuvHydrau$Z2[x],
                                                    LRG = OuvHydrau$LRG[x],
                                                    LENGTH = OuvHydrau$LENGTH[x], # AJOUT
                                                    "geometry" =
                                                      st_sfc(st_point(st_coordinates(OuvHydrau[x,])[dim(st_coordinates(OuvHydrau[x,]))[1], 1:2]), dim =
                                                               "XY")
                                                  ),
                                                  crs = EPSG)
                                                })))
      
      # Calcul de la taille de buffer a l entonnement et a la sortie
      RatioLRG = 2 / 3
      # bufMin = 5
      bufMin = contour$Exz1_Dx-0.5 # modifier avec la taille de la maille pour faire en format grille
      # bufMax = 5 #* contour$Exz1_Dx
      bufMax = 4 / 3 * (contour$Exz1_Dx)-0.5
      bufMaxu = bufMax
      units(bufMaxu) <- "m"
      
      # Gestion de points proches pour prendre le min
      Descendre = 25
      OuvHydrau_PtDebFin$Z = sapply(1:dim(OuvHydrau_PtDebFin)[1], function(x) {
        min(OuvHydrau_PtDebFin[which(st_distance(OuvHydrau_PtDebFin, OuvHydrau_PtDebFin[x,]) <
                                       bufMaxu),]$Z)
      }) - Descendre
      
      # buffer sur les points
      buf = sapply(1:dim(OuvHydrau_PtDebFin)[1], function(x) {
        # --- DEBUT MODIF : Taille du buffer variable selon longueur ---
        L = as.numeric(OuvHydrau_PtDebFin$LENGTH[x])
        if (!is.na(L) && L <= 12) {
          if (L < 8) return(1.5)
          if (L >= 8) return(2.0)
        }
        # --- FIN MODIF ---
        min(max(OuvHydrau_PtDebFin$LRG[x] * RatioLRG, bufMin),
            bufMax)
      })
      
      OuvHydrau_PtDebFinB = st_buffer(OuvHydrau_PtDebFin, buf)
      # export, on peut supprimer
      st_write(
        OuvHydrau_PtDebFinB,
        dsn = file.path(dsnlayerC, paste0("OuvHydrau_PtDebFin", ".shp")),
        delete_layer = T,
        quiet = T
      )
      
      # envoi dans grass
      nomOHES = "OH_ES"
      cmd=paste0("v.in.ogr -o --quiet --overwrite input=",file.path(dsnlayerC, paste0("OuvHydrau_PtDebFin", ".shp"))," output=",nomOHES)
      print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
      
      # conversion en reaster
      cmd=paste0("v.to.rast --quiet --overwrite input=",nomOHES," output=",nomOHES," use=attr attribute_column=Z")
      print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
      
      nom_MNT5 = "MNTvraifinal"
      # Minimum entre MNt de base et le ES des ouvrages
      cmd=paste0("r.series --overwrite input=",nom_MNT4,",",nomOHES," output=",nom_MNT5," method=minimum"," nprocs=",NProcGrass)
      print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
      
      nom_MNT4 = nom_MNT5
    }
  }
  
  cmd=paste0("r.mask --quiet --overwrite vector=",nom_STEP1_Contour)
  print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
  
  MNTFinalRond="MNTFinalRond"
  # r.mapcalc expression=MNTFinalRond = 0.01*round( 100*MNTFinal@Temp )              
  cmd=paste0("r.mapcalc --quiet --overwrite ",
             MNTFinalRond,"=0.01*round(100*",nom_MNT4,")")
  print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
  
  if (is.na(as.numeric(contour$Exzeco))==F)
  {
    cmd=paste0("r.mask --quiet --overwrite raster=",MNTFinalRond)
    print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
    
    Matric=matrix(0, ncol = 3, nrow=3)
    Matric[2,1]=1
    Matric[2,3]=1
    nomfilt=file.path(dsnlayerC,"FiltreDroiteGauche.txt")
    write.table(Matric,nomfilt, quote = FALSE,col.names = FALSE,row.names = FALSE)
    
    DroiteGauche="DroiteGauche"
    
    cmd=paste0("r.neighbors --quiet --overwrite input=",MNTFinalRond," output=",DroiteGauche ," method=count weighting_function=file weight=",nomfilt," nprocs=",NProcGrass)
    print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
    # r.neighbors input=MNTFinalRond@Temp output=DroiteGauche method=count weighting_function=file weight=C:\Cartino2D\France\MAMP_DEBIT\C09_241426km_X891198Y6251154\FiltreDroiteGauche.txt
    
    PixelSeul="PixelSeul"
    cmd=paste0("r.mapcalc --quiet --overwrite ",
               PixelSeul,"=if(",DroiteGauche,">0,1,null())")
    # Ajout 15/03/2024 Bug travail 25m
    # cmd=paste0("r.mapcalc --quiet --overwrite ",
    #           PixelSeul,"=if(",DroiteGauche,">0,","if(",nom_MNTMask,">-99,","1,null()),null())")
    print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
    
    cmd=paste0("r.mask --quiet --overwrite raster=",PixelSeul)
    print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
  }
  
  cmd=paste0("r.out.gdal --quiet --overwrite -c -f input=",MNTFinalRond," output=",paste0(dsnlayerC, "/", "MNTFinal", ".gpkg")," type=Float32 format=GPKG nodata=-9999")
  print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))  
  
  # Lignes 1073-1076 de Cartino2D_Etape01_Contrainte_MNT_GEO.R :
  nomcsv = paste0(dsnlayerC, "/", nom_MNT, ".csv")
  # cmd = paste0("r.out.xyz --quiet --overwrite input=", MNTFinalRond, " output=", nomcsv, " separator=comma")
  # print(cmd); system(paste0(BatGRASS, " ", SecteurGRASS, " --exec ", cmd))
  fun_export_csv_grass(MNTFinalRond, nomcsv, SecteurGRASS, BatGRASS)
  # Gestion des CN et Friction
  if (is.na(as.numeric(contour$CN)) == TRUE)
  {
    NomParamH = "CN"
    TAParamH = contour$CN
    Etape1_CNFriction(NomParamH,TAParamH,crs,contour_segm,dsnlayerC,nomcontour)
  }
  if (is.na(as.numeric(contour$Friction)) == TRUE)
  {
    NomParamH = "Friction"
    TAParamH = contour$Friction
    Etape1_CNFriction(NomParamH,TAParamH,crs,contour_segm,dsnlayerC,nomcontour)
  }
  
  Gestion_Sections_Controle(dsnlayer, dsnlayerC, contour, 0)
  
  # Export pour un maillage avec Exzeco
  # if (is.na(Exz_Pts)[1] == FALSE)
  if (is.na(as.numeric(contour$Exzeco))==T)
  {
    #############################################################################
    ################ PPUTILS #####################################################
    #############################################################################
    cat(nomcontour, " ---- Export du contouravec PPutils\n")
    # export du contour en csv
    setwd(chemin_pputils)
    #Modif 20210819 Bug_python au lieu Bug_python
    cmd = paste0(
      Bug_python,
      " shp2csv.py -i ",
      paste0(dsnlayerC, "\\", nom_STEP1_Contour, ".shp"),
      " -o ",
      paste0(dsnlayerC, "\\", nom_STEP1_Contour, ".csv")
    )
    print(cmd);system(cmd)
    if (file.exists(paste0(dsnlayerC, "\\", nom_STEP1_Contour, ".csv"))==F){cat("PPUTILS ne fonctionne pas");browser()}
  }
  
}

#################################################################################################
################   Etape1_GEO
#################################################################################################

# Création du fichier geo fichier d'entrée pour le mailleur GMSH
Etape1_GEO = function(dsnlayerC, Exz_Pts)
{
  ######################################################
  # 5.6.5 Master nodes files
  # # Copie des noeuds de contour
  cat(nomcontour, " ---- Création du fichier geo\n")
  setwd(dsnlayerC)
  tab1 = read.csv(paste0(dsnlayerC, "\\", nom_STEP1_Contour, "_nodes.csv"),
                  header = F)
  tab1[, 3] = 0
  
  # Export des points contraignats
  st_write(
    st_sfc(st_multipoint(Exz_Pts, dim = "XYZ")),
    dsn = file.path(dsnlayerC, paste0(nom_STEP2_Points, ".shp")),
    layer = nom_STEP2_Points,
    delete_layer = T,
    quiet = T
  )
  file.copy(file.path(chem_routine,"C2D",Doss_Qml,paste0(nom_STEP2_Points, ".qml")),
            file.path(dsnlayerC,paste0(nom_STEP2_Points, ".qml")))
  
  colnames(tab1) = cbind("X", "Y", "Z")
  Z = 0
  write.table(
    rbind(tab1, cbind(Exz_Pts, Z)),
    file = nom_nodes,
    row.names = FALSE,
    col.names = FALSE,
    sep = ","
  )
  
  setwd(chemin_pputils)
  
  setwd(dsnlayerC)
  nom_geo = paste0(nom_maillage, ".geo")
  file.create(nom_geo)
  dimptcontour = dim(tab1)[1]
  write(
    paste0(
      "Point(",
      1:(dimptcontour - 1),
      ") = {",
      round(tab1[-dimptcontour, 1], 3),
      ", ",
      round(tab1[-dimptcontour, 2], 3),
      ", 0.000, 0.000};"
    ),
    file = nom_geo,
    append = T
  )
  
  write(
    paste0(
      "Point(",
      (dimptcontour):(dim(Exz_Pts)[1] + dimptcontour - 1),
      ") = {",
      round(Exz_Pts[, 1], 3),
      ", ",
      round(Exz_Pts[, 2], 3),
      ", 0.000, 0.000};"
    ),
    file = nom_geo,
    append = T
  )
  
  geo3 = matrix(1:(dimptcontour - 1), dimptcontour - 1, 1)
  geo4 = geo3
  geo4[-(dimptcontour - 1),] = geo3[-1,]
  geo4[(dimptcontour - 1),] = 1
  write(
    paste0("Line(", geo3, ") = {", geo3, ", ", geo4, "};"),
    file = nom_geo,
    append = T
  )
  
  write(paste0("Line Loop(1) = {1:", (dimptcontour - 1), "};"),
        file = nom_geo,
        append = T)
  write(paste0("Physical Line(1) = {1:", (dimptcontour - 1), "};"),
        file = nom_geo,
        append = T)
  write(paste0("Plane Surface(1) = {1};"),
        file = nom_geo,
        append = T)
  write(paste0("Physical Surface(1) = {1};"),
        file = nom_geo,
        append = T)
  write(paste0(
    "Point {",
    (dimptcontour):(dim(Exz_Pts)[1] + dimptcontour - 1),
    "} In Surface {1};"
  ) ,
  file = nom_geo,
  append = T)
}

#################################################################################################
################   Ré-échantillonage
#################################################################################################
# ReechantillonageRasterC2D = function(nomok, Reso, dsnlayer)
# {
#   cmd=paste0("g.region --overwrite --quiet"," raster=",nomok," res=",as.character(Reso))
#   print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
#   
#   nomfinal = paste0(nomok, "_", formatC(Reso, width = 3, flag = "0"), "m")
#   cmd=paste0("r.resamp.stats --quiet --overwrite input=",nomok," output=",nomfinal)
#   print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
#   
#   cmd=paste0("g.region --overwrite --quiet"," zoom=",nomfinal)
#   print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
#   
#   if (dir.exists(dsnlayer) == F) {
#     dir.create(dsnlayer)
#   }
#   
#   cmd=paste0("r.out.gdal --quiet --overwrite -c -f input=",nomfinal," output=",file.path(dsnlayer, paste0(nomok, ".asc"))," format=AAIGrid"," nodata=-9999","createopt=DECIMAL_PRECISION=2")
#   print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
# }

###################################################
Etape1_CNFriction = function(NomParamH,
                             TAParamH,
                             crs,
                             contour_segm,
                             dsnlayerC,
                             nomcontour)
{
  dsnParamH = dirname(TAParamH)
  ParamH = st_read(TAParamH)
  st_crs(ParamH) = crs
  nb = st_intersects(ParamH, contour_segm)
  n_int = which(sapply(nb, length) > 0)
  ParamHC = ParamH[n_int,]
  
  # Création de la liste des dalles concernées
  ListeParamH = paste0(dsnParamH, '/', ParamHC$DOSSIERASC, '/', ParamHC$NOM_ASC)
  
  nom_vrt = file.path(dsnlayerC, paste0("listepourvrt_", NomParamH, ".txt"))
  vrtfile = paste0(NomParamH, ".vrt")
  file.create(nom_vrt)
  write(ListeParamH, file = nom_vrt, append = T)
  vrtfile = paste0(dsnlayerC, "/", NomParamH, ".vrt") ##chemin du vrt à créer
  cmd = paste(OSGeo4W_path,
              "gdalbuildvrt",
              vrtfile,
              "-input_file_list",
              nom_vrt) ## commande pour exécuter gdalbuildvrt
  print(cmd);system(cmd)
  unlink(nom_vrt)
  
  ##############Creation d'un fichier VRT###############
  cat(nomcontour, " ---- Travail du lidar dans Grass\n")
  cmd=paste0("r.in.gdal -o --quiet --overwrite input=",vrtfile," output=",NomParamH)
  print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
  
  cmd=paste0("g.region --overwrite --quiet"," raster=","MNTFinal")
  print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
  
  # masque sur le grand contour
  cmd=paste0("r.mask --quiet --overwrite raster=MNTFinal")
  print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
  
  if (is.na(as.numeric(contour$Exzeco))==F)
  {
    cat("Ré-échantillonage de ", NomParamH,"\n")
    NomParamH_=paste0(NomParamH,"Resamp")
    cmd=paste0("r.resamp.stats --quiet --overwrite input=",NomParamH," output=",NomParamH_)
    print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
  }else{
    NomParamH_=NomParamH
  }
  
  # nomcsv = paste0(dsnlayerC, "/", NomParamH, ".csv")
  # cmd=paste0("r.out.xyz --quiet --overwrite input=",NomParamH_," output=",nomcsv," separator=comma")
  # print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
  
  # 20241112 Ajout des vides au cas où les rasters de MNt et CN u Strickler ne soient pas cohérents
  ValDefautParam=ifelse(NomParamH == "CN",70,15)
  NomParamH_Vide=paste0(NomParamH,"Vide")
  cmd=paste0("r.mapcalc --quiet --overwrite ",NomParamH_Vide,"=",ValDefautParam,"*","MASK")
  print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
  
  NomParamH_patch=paste0(NomParamH,"Patch")
  cmd=paste0("r.patch --quiet --overwrite ","input=",NomParamH_,",",NomParamH_Vide," output=",NomParamH_patch," nprocs=",NProcGrass)
  print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))

  
  nomcsv = paste0(dsnlayerC, "/", NomParamH, ".csv")
  # cmd = paste0("r.out.xyz --quiet --overwrite input=", NomParamH_patch, " output=", nomcsv, " separator=comma")
  # print(cmd); system(paste0(BatGRASS, " ", SecteurGRASS, " --exec ", cmd))
  fun_export_csv_grass(NomParamH_patch, nomcsv, SecteurGRASS, BatGRASS)
  # Sécurité : si GRASS a eu un hoquet, on retente une fois après une micro-pause
  if (!file.exists(nomcsv)) {
    cat(">>> Tentative de récupération pour :", nomcsv, "\n")
    Sys.sleep(0.5)
    system(paste0(BatGRASS, " ", SecteurGRASS, " --exec ", cmd))
  }
}

Etape1_fn <- function(skip_to_next)
{
  tryCatch({ if (ETAPE[1] == 1)
  {
    cat("\014")
    cat("ETAPE 1 - Traitement de: ",contour$NOM,"\n")
    #Creation d'un monde GRASS
    unlink(dirname(SecteurGRASS),recursive=TRUE)
    toto=system(paste0(BatGRASS," -c EPSG:",EPSG," ",dirname(SecteurGRASS)," --text"))
    system(paste0(BatGRASS," -c ",SecteurGRASS," --text"))
    cat("\014")
    cat("ETAPE 1 - Traitement de: ",contour$NOM,"\n")
    # Ouverture Table Assemblage Lidar
    fun_check_exist(contour$MNT,1)
    dsnLidar = dirname(contour$MNT)
    Lidar <- st_read(contour$MNT)
    
    st_crs(Lidar) = crs
    
    # Copie du dossier base dans dossier export
    copie_prerequis()
    
    # Fichier utilisé à cette étape
    # Travail sur les contours d'Exzeco pour réaliser les points de contrainte et sur les limites du domaine
    # Gestion des maillages s'appuyant sur Exzeco
    # system.time(Etape1_Exzeco(dsnlayer, contour, crs))
    
    resultat = Etape1_Contour(dsnlayer, contour, crs)
    contour_segm = resultat[[1]]
    contour_int = resultat[[2]]
    Contoursanstrou= resultat[[3]]
    
    # Exz_Pts = Etape1_Exzeco(dsnlayer, contour,contour_int,Contoursanstrou, crs)
    # 
    # Etape1_MNT(dsnlayerC, Lidar,dsnLidar, contour_segm, contour_int)
    Exz_Pts = Etape1_Exzeco(dsnlayer, contour,contour_int,Contoursanstrou, crs)
    
    Etape1_MNT(dsnlayerC, Lidar,dsnLidar, contour_segm, contour_int)
    
    
    # Etape1_MNT(dsnlayerC, Lidar,dsnLidar, contour_segm, contour_int)
    # Exz_Pts = Etape1_Exzeco(dsnlayer, contour,contour_int,Contoursanstrou, crs)
    
    
    
    
    # Travail sur le MNT pour récupérer les données et gérer une décroissance de la topo aux frontières
    # Etape1_MNT(dsnlayerC, Lidar,dsnLidar,Exz_Pts, contour_segm, contour_int)
    
    
    cat("c est ok", "\n")
    
    if (is.na(Exz_Pts)[1] == FALSE)
    {
      # Création du fichier geo fichier d'entrée pour le mailleur GMSH
      Etape1_GEO(dsnlayerC, Exz_Pts)
    }
    # cat("\014")
    unlink(dirname(SecteurGRASS),recursive=TRUE)
    
  }},error = function(e) {skip_to_next <<- TRUE} )
  return(skip_to_next)
  
}


# Etape1_fn_parallel <- function()
# {
#   if (ETAPE[1] == 1)
#   {
#     cat("\014")
#     cat("ETAPE 1 - Traitement de: ",contour$NOM,"\n")
#     #Creation d'un monde GRASS
#     # unlink(dirname(SecteurGRASS),recursive=TRUE)
#     # toto=system(paste0(BatGRASS," -c EPSG:",EPSG," ",dirname(SecteurGRASS)," --text"))
#     # system(paste0(BatGRASS," -c ",SecteurGRASS," --text"))
#     loc_path = dirname(SecteurGRASS)
#     
#     # Nettoyer puis créer la Location avec la projection EPSG
#     unlink(loc_path, recursive = TRUE)
#     system(paste0(BatGRASS, " -c EPSG:", EPSG, " -e \"", loc_path, "\""))
#     cat("\014")
#     
#     
#     loc_path = dirname(SecteurGRASS)
#     
#     # Tentative de création avec reprise automatique si échec
#     dossier_cree = FALSE
#     tentative = 1
#     
#     while (!dossier_cree && tentative <= 3) {
#       # Nettoyage si un résidu existe
#       if (dir.exists(loc_path)) {
#         unlink(loc_path, recursive = TRUE)
#       }
#       
#       # Création de la Location GRASS
#       system(paste0(BatGRASS, " -c EPSG:", EPSG, " -e \"", loc_path, "\""))
#       
#       # Petite pause de 1 seconde pour laisser le système de fichiers valider l'écriture
#       Sys.sleep(1)
#       
#       if (dir.exists(loc_path)) {
#         dossier_cree = TRUE
#       } else {
#         cat("[Tentative", tentative, "] Échec création Location pour :", loc_path, "- Nouvelle tentative...\n", 
#             file = file.path(SecteurGRASS_,"erreurs_grass.txt", append = TRUE))
#         Sys.sleep(2)  # Pause avant de réessayer
#         tentative = tentative + 1
#       }
#     }
#     
#     if (!dossier_cree) {
#       stop(paste("ERREUR FATALE : Impossible de créer la Location GRASS après 3 tentatives :", loc_path))
#     }
#     
#     cat("ETAPE 1 - Traitement de: ",contour$NOM,"\n")
#     # Ouverture Table Assemblage Lidar
#     fun_check_exist(contour$MNT,1)
#     dsnLidar = dirname(contour$MNT)
#     Lidar <- st_read(contour$MNT)
#     
#     st_crs(Lidar) = crs
#     
#     # Copie du dossier base dans dossier export
#     copie_prerequis()
#     
#     # Fichier utilisé à cette étape
#     # Travail sur les contours d'Exzeco pour réaliser les points de contrainte et sur les limites du domaine
#     # Gestion des maillages s'appuyant sur Exzeco
#     # system.time(Etape1_Exzeco(dsnlayer, contour, crs))
#     resultat = Etape1_Contour(dsnlayer, contour, crs)
#     contour_segm = resultat[[1]]
#     contour_int = resultat[[2]]
#     Contoursanstrou= resultat[[3]]
#     
#     
#     Exz_Pts = Etape1_Exzeco(dsnlayer, contour,contour_int,Contoursanstrou, crs)   
#     Etape1_MNT(dsnlayerC, Lidar,dsnLidar, contour_segm, contour_int)
# 
#     # Travail sur le MNT pour récupérer les données et gérer une décroissance de la topo aux frontières
#     # Etape1_MNT(dsnlayerC, Lidar,dsnLidar,Exz_Pts, contour_segm, contour_int)
#     
#     
#     cat("c est ok", "\n")
#     
#     if (is.na(Exz_Pts)[1] == FALSE)
#     {
#       # Création du fichier geo fichier d'entrée pour le mailleur GMSH
#       Etape1_GEO(dsnlayerC, Exz_Pts)
#     }
#     # cat("\014")
#     
#     unlink(dirname(SecteurGRASS),recursive=TRUE)
#     
#   }
# }
Etape1_fn_parallel <- function()
{
  if (ETAPE[1] == 1)
  {
    cat("\014")
    cat("ETAPE 1 - Traitement de: ", contour$NOM, "\n")
    
    # --- CREATION SECURISEE DU MONDE GRASS ---
    loc_path = dirname(SecteurGRASS)
    dossier_cree = FALSE
    tentative = 1
    
    while (!dossier_cree && tentative <= 3) {
      # Nettoyage si un résidu existe
      if (dir.exists(loc_path)) {
        unlink(loc_path, recursive = TRUE)
      }
      
      # Création de la Location GRASS
      system(paste0(BatGRASS, " -c EPSG:", EPSG, " -e \"", loc_path, "\""))
      
      # Petite pause pour laisser Windows valider l'écriture du dossier sur disque
      Sys.sleep(1)
      
      if (dir.exists(loc_path)) {
        dossier_cree = TRUE
      } else {
        cat(paste0("[Tentative ", tentative, "] Échec création Location pour : ", loc_path, " - Nouvelle tentative...\n"), 
            file = file.path(SecteurGRASS_, "erreurs_grass.txt"), append = TRUE)
        Sys.sleep(2)  # Pause avant de réessayer
        tentative = tentative + 1
      }
    }
    
    if (!dossier_cree) {
      stop(paste("ERREUR FATALE : Impossible de créer la Location GRASS après 3 tentatives :", loc_path))
    }
    
    cat("ETAPE 1 - Traitement de: ", contour$NOM, "\n")
    # Ouverture Table Assemblage Lidar
    fun_check_exist(contour$MNT, 1)
    dsnLidar = dirname(contour$MNT)
    Lidar <- st_read(contour$MNT)
    
    st_crs(Lidar) = crs
    
    # Copie du dossier base dans dossier export
    copie_prerequis()
    
    # Fichier utilisé à cette étape
    resultat = Etape1_Contour(dsnlayer, contour, crs)
    contour_segm = resultat[[1]]
    contour_int = resultat[[2]]
    Contoursanstrou = resultat[[3]]
    
    Exz_Pts = Etape1_Exzeco(dsnlayer, contour, contour_int, Contoursanstrou, crs)   
    Etape1_MNT(dsnlayerC, Lidar, dsnLidar, contour_segm, contour_int)
    
    cat("c est ok", "\n")
    
    if (is.na(Exz_Pts)[1] == FALSE)
    {
      # Création du fichier geo fichier d'entrée pour le mailleur GMSH
      Etape1_GEO(dsnlayerC, Exz_Pts)
    }
    
    # Nettoyage final du dossier GRASS
    unlink(dirname(SecteurGRASS), recursive = TRUE)
    
  }
}