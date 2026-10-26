#################################################################################################
################   Etape10_PostTelemacGrass
#################################################################################################

Etape10_PostTelemacGrass=function()
{
  dsnlayerC_ <- file.path(dsnlayerC,"Post")
  
  listeRES=list.files(dsnlayerC,pattern=".res$")
  toremove <- (grepl(pattern = "WD",x =listeRES) & grepl(pattern = ".res",x =listeRES)) | (grepl(pattern = "SFR",x =listeRES) & grepl(pattern = ".res",x =listeRES))
  listeRES <- listeRES[!toremove]
  if (length(listeRES)>0)
  {
    for (ires in 1:length(listeRES))
    {
      ncharsupp=1
      nomres=substr(listeRES[ires],ncharsupp,(nchar(listeRES[ires])-4))
      listeRaster=list.files(dsnlayerC_,pattern=nomres)
      ici=which(substr(listeRaster,nchar(listeRaster)-8,nchar(listeRaster))=="_Brut.asc")
      if (length(ici)>0)
      { 
        #Creation d'un monde GRASS
        # SecteurGRASS=paste0(dirname(SecteurGRASS_),format(Sys.time(),format="%Y%m%d_%H%M%S"),"_",basename(dsnlayerC),"_",ic,"/",basename(SecteurGRASS_))
        # unlink(dirname(SecteurGRASS),recursive=TRUE)
        # toto=system(paste0(BatGRASS," -c EPSG:",EPSG," ",dirname(SecteurGRASS)," --text"))
        # if (toto!=0){cat("Vous avez un problème dans ",BatGRASS,"\n");BUGDEGRASS=BOOM}
        # system(paste0(BatGRASS," -c ",SecteurGRASS," --text"))
        
        # Creation propre d'un monde GRASS à 3 niveaux : GISDBASE / LOCATION / MAPSET
        loc_name = paste0("Post_", nomres, "_", basename(dsnlayerC), "_", format(Sys.time(), "%Y%m%d_%H%M%S"))
        SecteurGRASS = file.path(paste0(dirname(SecteurGRASS_),format(Sys.time(),format="%Y%m%d_%H%M%S")), loc_name, "PERMANENT")
        loc_path = dirname(SecteurGRASS)
        
        # Nettoyage préalable puis création avec projection EPSG (-e pour sortie propre)
        if (dir.exists(loc_path)) unlink(loc_path, recursive = TRUE)
        toto = system(paste0(BatGRASS, " -c EPSG:", EPSG, " -e \"", loc_path, "\""))
        if (toto != 0) {
          cat("Problème de création GRASS dans :", loc_path, "\n")
        }
        
        
        extsortieASC=substr(listeRaster[ici],nchar(nomres)+2,nchar(listeRaster[ici])-9)
        
        # S'assurer que Bottom_m est présent si le GPKG existe déjà (copié par Etape 7)
        if (file.exists(file.path(dsnlayerC_, paste0(nomres, "_Bottom_m.gpkg"))) && !("Bottom_m" %in% extsortieASC)) {
            extsortieASC = c(extsortieASC, "Bottom_m")
        }
        
        # Placer Bottom_m en premier pour qu'il soit corrigé avant les calculs dérivés
        if ("Bottom_m" %in% extsortieASC) {
          extsortieASC = c("Bottom_m", setdiff(extsortieASC, "Bottom_m"))
        }
        if (file.exists(paste0(dsnlayerC,"\\",nomres,"_",extsortieASC[length(extsortieASC)],".gpkg"))==FALSE)
        { 
          cmd=paste0("v.in.ogr -o --quiet --overwrite input=",file.path(dsnlayerC,paste0(nom_STEP2_Zone_Valid_Calcul,".shp"))," output=",nom_STEP2_Zone_Valid_Calcul)
          print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
          
          
          for (iparam in 1:length(extsortieASC))
          {
            nomsortieASC=paste0(dsnlayerC_,"\\",nomres,"_",extsortieASC[iparam],"_Brut.asc")
            nomsortieASCok=paste0(dsnlayerC_,"\\",nomres,"_",extsortieASC[iparam],".gpkg")
            
            # --- OPTIMISATION : Si le GPKG existe déjà, on l'importe et on saute le traitement (cas de Bottom_m) ---
            if (file.exists(nomsortieASCok) && extsortieASC[iparam] == "Bottom_m") {
              cat("[Optimisation] Le fichier", basename(nomsortieASCok), "existe déjà. Import direct dans GRASS...\n")
              nomASCg = paste0(nomres, "_", extsortieASC[iparam])
              cmd = paste0("r.in.gdal -o --quiet --overwrite input=", shQuote(nomsortieASCok), " output=", nomASCg)
              system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
              next
            }
            
            if(!(is.na(contour$SourceDeb) | nchar(contour$SourceDeb)==0))
            {
              contour_valide <- st_read(file.path(dsnlayerC,paste0(nom_STEP2_Zone_Valid_Calcul,".shp")))
              
              contour_valide <- st_buffer(contour_valide,-(as.numeric(contour$Exzeco)/4))
              
            }
            
            ##############Creation d'un fichier VRT###############
            print(paste0(nomcontour," ---- Travail du lidar dans Grass"))
            # Importation raster dans Grass
            nomASCg=paste0(nomres,"_",extsortieASC[iparam])
            cmd=paste0("r.in.gdal -o --quiet --overwrite input=",nomsortieASC," output=",nomASCg)
            print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
            
            # # =========================================================================
            # # CORRECTION OPTIMALE DU BOTTOM AUX OUVRAGES HYDRAULIQUES (ANTI-TROUS)
            # # =========================================================================
            # if (extsortieASC[iparam] == "Bottom_m") {
            #   shp_oh_path = file.path(dsnlayerC, "OuvHydrau_PtDebFin.shp")
            #   mnt_ref_file = file.path(dsnlayerC, paste0(nom_MNT, ".vrt"))
            #   
            #   if (file.exists(shp_oh_path) && file.exists(mnt_ref_file)) {
            #     cat("[Anti-Trous] Détection des ouvrages hydrauliques pour corriger Bottom_m...\n")
            #     
            #     # 1. Lecture des cercles et buffer de 2 * Exz1_Dx
            #     buf_dist = 2 * as.numeric(contour$Exz1_Dx)
            #     oh_pts = sf::st_read(shp_oh_path, quiet = TRUE)
            #     oh_buf = sf::st_buffer(oh_pts, dist = buf_dist)
            #     
            #     # Écriture du shapefile temporaire bufferisé
            #     shp_buf_tmp = file.path(dsnlayerC, "OuvHydrau_PtDebFin_buf.shp")
            #     sf::st_write(oh_buf, shp_buf_tmp, delete_layer = TRUE, quiet = TRUE)
            #     
            #     # 2. Importation dans GRASS et rastérisation sur la cote Z
            #     cmd = paste0("v.in.ogr -o --quiet --overwrite input=", shQuote(shp_buf_tmp), " output=OH_buf")
            #     system(paste0(BatGRASS, " ", SecteurGRASS, " --exec ", cmd))
            #     
            #     cmd = paste0("v.to.rast --quiet --overwrite input=OH_buf output=OH_Z_seuil use=attr attribute_column=Z")
            #     system(paste0(BatGRASS, " ", SecteurGRASS, " --exec ", cmd))
            #     
            #     # 3. Import du MNT naturel d'origine (MNT.vrt non creusé)
            #     cmd = paste0("r.in.gdal -o --quiet --overwrite input=", shQuote(mnt_ref_file), " output=MNT_BRUT_NAT")
            #     system(paste0(BatGRASS, " ", SecteurGRASS, " --exec ", cmd))
            #     
            #     # 4. Remplacement conditionnel ultra-rapide par r.mapcalc :
            #     # Si le pixel est dans le cercle ET que Bottom < Z_seuil -> on restaure MNT_BRUT_NAT
            #     nomASCg_clean = paste0(nomASCg, "_clean")
            #     cmd = paste0("r.mapcalc --quiet --overwrite ", shQuote(paste0(
            #       nomASCg_clean, " = if(!isnull(OH_Z_seuil) && ", nomASCg, " < OH_Z_seuil, MNT_BRUT_NAT, ", nomASCg, ")"
            #     )))
            #     system(paste0(BatGRASS, " ", SecteurGRASS, " --exec ", cmd))
            #     
            #     # On remplace le raster actif nomASCg par le raster nettoyé
            #     nomASCg = nomASCg_clean
            #     
            #     # Nettoyage du shapefile temporaire
            #     unlink(file.path(dsnlayerC, "OuvHydrau_PtDebFin_buf.*"))
            #     cat("[Anti-Trous] Bottom_m corrigé avec succès à partir du MNT naturel !\n")
            #   }
            # }
            
            
            # =========================================================================
            
            # =========================================================================
            # CORRECTION OPTIMALE DU BOTTOM (AVEC REUTILISATION MULTI-SCENARIOS)
            # =========================================================================
            # if (extsortieASC[iparam] == "Bottom_m") {
            #   
            #   # 1. Vérifier si un Bottom_m.gpkg propre a déjà été calculé pour un autre scénario
            #   autres_bottom_gpkg = list.files(dsnlayerC_, pattern = "_Bottom_m\\.gpkg$", full.names = TRUE)
            #   
            #   if (length(autres_bottom_gpkg) > 0 && file.exists(autres_bottom_gpkg[1])) {
            #     cat("[Optimisation Multi-Scénarios] Un Bottom_m.gpkg propre existe déjà. Copie instantanée...\n")
            #     
            #     # A. Copier le GPKG existant vers le nom du scénario actuel
            #     file.copy(from = autres_bottom_gpkg[1], to = nomsortieASCok, overwrite = TRUE)
            #     
            #     # B. L'importer directement dans GRASS pour les calculs de HWH_m
            #     cmd = paste0("r.in.gdal -o --quiet --overwrite input=", shQuote(nomsortieASCok), " output=", nomASCg)
            #     system(paste0(BatGRASS, " ", SecteurGRASS, " --exec ", cmd))
            #     
            #     # C. Supprimer le fichier brut .asc
            #     if (file.exists(nomsortieASC)) file.remove(nomsortieASC)
            #     
            #     # D. On saute le reste du traitement pour Bottom_m (gain de temps maximal !)
            #     next
            #     
            #   } else {
            #     # 2. Premier scénario : on effectue la correction anti-trous complète
            #     shp_oh_path = file.path(dsnlayerC, "OuvHydrau_PtDebFin.shp")
            #     mnt_ref_file = file.path(dsnlayerC, paste0(nom_MNT, ".vrt"))
            #     
            #     if (file.exists(shp_oh_path) && file.exists(mnt_ref_file)) {
            #       cat("[Anti-Trous] Première détection : correction de Bottom_m aux ouvrages hydrauliques...\n")
            #       
            #       # Buffer de 2 * Exz1_Dx
            #       buf_dist = 2 * as.numeric(contour$Exz1_Dx)
            #       oh_pts = sf::st_read(shp_oh_path, quiet = TRUE)
            #       oh_buf = sf::st_buffer(oh_pts, dist = buf_dist)
            #       
            #       shp_buf_tmp = file.path(dsnlayerC, "OuvHydrau_PtDebFin_buf.shp")
            #       sf::st_write(oh_buf, shp_buf_tmp, delete_layer = TRUE, quiet = TRUE)
            #       
            #       # Importation dans GRASS et seuil Z
            #       cmd = paste0("v.in.ogr -o --quiet --overwrite input=", shQuote(shp_buf_tmp), " output=OH_buf")
            #       system(paste0(BatGRASS, " ", SecteurGRASS, " --exec ", cmd))
            #       
            #       cmd = paste0("v.to.rast --quiet --overwrite input=OH_buf output=OH_Z_seuil use=attr attribute_column=Z")
            #       system(paste0(BatGRASS, " ", SecteurGRASS, " --exec ", cmd))
            #       
            #       # Import MNT naturel d'origine
            #       cmd = paste0("r.in.gdal -o --quiet --overwrite input=", shQuote(mnt_ref_file), " output=MNT_BRUT_NAT")
            #       system(paste0(BatGRASS, " ", SecteurGRASS, " --exec ", cmd))
            #       
            #       # Remplacement des trous sous Z
            #       nomASCg_clean = paste0(nomASCg, "_clean")
            #       cmd = paste0("r.mapcalc --quiet --overwrite ", shQuote(paste0(
            #         nomASCg_clean, " = if(!isnull(OH_Z_seuil) && ", nomASCg, " < OH_Z_seuil, MNT_BRUT_NAT, ", nomASCg, ")"
            #       )))
            #       system(paste0(BatGRASS, " ", SecteurGRASS, " --exec ", cmd))
            #       
            #       nomASCg = nomASCg_clean
            #       unlink(file.path(dsnlayerC, "OuvHydrau_PtDebFin_buf.*"))
            #       cat("[Anti-Trous] Bottom_m corrigé et sauvegardé pour tous les scénarios !\n")
            #     }
            #   }
            # }
            # =========================================================================
            
            
            # dcélaration de la réguion sur le raster
            cmd=paste0("g.region --overwrite raster=",nomASCg)
            print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
            
            # Suppression des 0
            if (iparam>1)
            {
              cmd=paste0("r.null map=",nomASCg," setnull=0-0.005")
              print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
            }
            # =========================================================================
            # CORRECTION ANTI-TROUS SUR BOTTOM_M (SI OUVRAGES PRESENTS)
            # =========================================================================
            if (extsortieASC[iparam] == "Bottom_m") {
              shp_oh_path = file.path(dsnlayerC, "OuvHydrau_PtDebFin.shp")
              
              # Chercher le fichier VRT du MNT naturel (nom_MNT.vrt ou MNT.vrt)
              mnt_ref_file = file.path(dsnlayerC, paste0(nom_MNT, ".vrt"))
              if (!file.exists(mnt_ref_file)) {
                mnt_ref_file = file.path(dsnlayerC, "MNT.vrt")
              }
              
              if (file.exists(shp_oh_path) && file.exists(mnt_ref_file)) {
                cat("[Anti-Trous] Correction du Bottom_m aux ouvrages hydrauliques (+25m seuil)...\n")
                
                # 1. Buffer de 2 * Exz1_Dx et ajout de 25m au champ Z
                buf_dist = 2 * as.numeric(contour$Exz1_Dx)
                oh_pts = sf::st_read(shp_oh_path, quiet = TRUE)
                oh_pts$Z_seuil = as.numeric(oh_pts$Z) + 25.0 # <--- AJOUT DES 25 METRES ICI
                oh_buf = sf::st_buffer(oh_pts, dist = buf_dist)
                
                shp_buf_tmp = file.path(dsnlayerC, "OuvHydrau_PtDebFin_buf.shp")
                sf::st_write(oh_buf, shp_buf_tmp, delete_layer = TRUE, quiet = TRUE)
                
                # 2. Import et rastérisation de Z_seuil dans GRASS
                cmd = paste0("v.in.ogr -o --quiet --overwrite input=", shQuote(shp_buf_tmp), " output=OH_buf")
                system(paste0(BatGRASS, " ", SecteurGRASS, " --exec ", cmd))
                
                cmd = paste0("v.to.rast --quiet --overwrite input=OH_buf output=OH_Z_seuil use=attr attribute_column=Z_seuil")
                system(paste0(BatGRASS, " ", SecteurGRASS, " --exec ", cmd))
                
                # 3. Import du MNT naturel calé sur la grille (-r)
                cmd = paste0("r.in.gdal -r -o --quiet --overwrite input=", shQuote(mnt_ref_file), " output=MNT_BRUT_NAT")
                system(paste0(BatGRASS, " ", SecteurGRASS, " --exec ", cmd))
                
                # 4. Rebouchage : si pixel dans cercle et Bottom < (Z + 25m) -> MNT naturel
                # On utilise un if() imbriqué pour eviter que GRASS ne retourne NULL sur toute la carte
                nomASCg_clean = paste0(nomASCg, "_clean")
                cmd = paste0("r.mapcalc --quiet --overwrite ", shQuote(paste0(
                  nomASCg_clean, " = if(isnull(OH_Z_seuil), ", nomASCg, ", if(", nomASCg, " < OH_Z_seuil, MNT_BRUT_NAT, ", nomASCg, "))"
                )))
                system(paste0(BatGRASS, " ", SecteurGRASS, " --exec ", cmd))
                
                # ECRASE le raster avec trous dans GRASS par le raster rebouché (pour HWH_m)
                cmd = paste0("g.rename --overwrite raster=", nomASCg_clean, ",", nomASCg)
                system(paste0(BatGRASS, " ", SecteurGRASS, " --exec ", cmd))
                
                unlink(file.path(dsnlayerC, "OuvHydrau_PtDebFin_buf.*"))
                cat("[Anti-Trous] Bottom_m rebouché avec succès et mis à jour dans GRASS !\n")
              }
            }
            # =========================================================================
            
            # Masque sur le grand contour
            cmd=paste0("r.mask --quiet --overwrite vector=", nom_STEP2_Zone_Valid_Calcul)
            print(cmd); system(paste0(BatGRASS, " ", SecteurGRASS, " --exec ", cmd))
            
            # Recalage strict de la région sur nomASCg (évite le zoom=MASK qui vidait le raster !)
            cmd=paste0("g.region --overwrite raster=", nomASCg)
            print(cmd); system(paste0(BatGRASS, " ", SecteurGRASS, " --exec ", cmd))
            
            # Arrondi propre
            nom_rond = paste0(nomASCg, "_Rond")
            cmd=paste0("r.mapcalc --quiet --overwrite ", nom_rond, "=0.01*round(100*", nomASCg, ")")
            print(cmd); system(paste0(BatGRASS, " ", SecteurGRASS, " --exec ", cmd))
            
            # Export du fichier GPKG valide et non vide
            cmd=paste0("r.out.gdal --quiet --overwrite -c -f input=", nom_rond, " output=", nomsortieASCok, " type=Float32 format=GPKG nodata=-9999")
            print(cmd); system(paste0(BatGRASS, " ", SecteurGRASS, " --exec ", cmd))
            
            cmd = paste0(shQuote(OSGeo4W_path)," gdaladdo ","--config OGR_SQLITE_SYNCHRONOUS OFF ", "-r AVERAGE ",nomsortieASCok," 2 4 8 16 32 64 128 256")
            print(cmd);system(cmd)
            
            if (file.exists(nomsortieASCok)==T){file.remove(nomsortieASC)}
          }
          
          # calcul de la cote maximale si pas présente (max de pluie à la fin par exemple
          # if (length(which(extsortieASC=="HWH_m"))==0)
          # {
          #   nTopo=which(extsortieASC=="Bottom_m")
          #   nCote=which(extsortieASC=="HWM_m")
          #   if (length(nTopo)>0 & length(nCote)>0)
          #   {
          #     nomexp=paste0(nomres,"_","HWH_m")
          #     cmd=paste0("r.mapcalc --quiet --overwrite ",nomexp,"=",nomres,"_",extsortieASC[nCote],"-",nomres,"_",extsortieASC[nTopo])
          #     print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
          #     
          #     nomsortieASCok=paste0(dsnlayerC_,"\\",nomexp,".gpkg")
          #     
          #     cmd=paste0("r.out.gdal --quiet --overwrite -c -f input=",nomexp," output=",nomsortieASCok," type=Float32 format=GPKG nodata=-9999")
          #     print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
          #     
          #     cmd = paste0(shQuote(OSGeo4W_path)," gdaladdo ","--config OGR_SQLITE_SYNCHRONOUS OFF ", "-r AVERAGE ",nomsortieASCok," 2 4 8 16 32 64 128 256")
          #     print(cmd);system(cmd)
          #   }
          # }
          # Calcul FORCÉ de la hauteur d'eau maximale HWH_m = HWM_m - Bottom_m (pour virer l'eau des trous rebouchés)
          nCote = which(extsortieASC == "HWM_m")
          
          # Vérifier si Bottom_m existe déjà en GPKG (scénario actuel ou précédent)
          bottom_existe_gpkg = file.exists(file.path(dsnlayerC_, paste0(nomres, "_Bottom_m.gpkg"))) ||
            length(list.files(dsnlayerC_, pattern = "_Bottom_m\\.gpkg$")) > 0
          
          if (length(nCote) > 0 && (("Bottom_m" %in% extsortieASC) || bottom_existe_gpkg))
          {
            # Si Bottom_m n'est pas encore présent dans cette session GRASS, on importe son GPKG
            nom_topo_grass = paste0(nomres, "_Bottom_m")
            cmd_test = paste0("g.list type=raster pattern=", nom_topo_grass)
            res_test = system(paste0(BatGRASS, " ", SecteurGRASS, " --exec ", cmd_test), intern = TRUE)
            
            if (length(res_test) == 0 || !any(grepl(nom_topo_grass, res_test))) {
              bottom_file_to_import = file.path(dsnlayerC_, paste0(nomres, "_Bottom_m.gpkg"))
              if (!file.exists(bottom_file_to_import)) {
                bottom_file_to_import = list.files(dsnlayerC_, pattern = "_Bottom_m\\.gpkg$", full.names = TRUE)[1]
              }
              cmd = paste0("r.in.gdal -o --quiet --overwrite input=", shQuote(bottom_file_to_import), " output=", nom_topo_grass)
              system(paste0(BatGRASS, " ", SecteurGRASS, " --exec ", cmd))
            }
            
            cat("[Anti-Trous] Recalcul forcé de HWH_m avec le Bottom_m rebouché...\n")
            # Calcul de HWH_m = max(HWM_m - Bottom_m, 0) pour éviter les hauteurs d'eau négatives dans les trous
            nomexp = paste0(nomres, "_", "HWH_m")
            cmd = paste0("r.mapcalc --quiet --overwrite ", nomexp, "=\"max(", nomres, "_", extsortieASC[nCote], " - ", nom_topo_grass, ", 0)\"")
            print(cmd); system(paste0(BatGRASS, " ", SecteurGRASS, " --exec ", cmd))
            
            # Export de la bonne hauteur d'eau (écrase le mauvais fichier avec les trous s'il existait)
            nomsortieASCok = paste0(dsnlayerC_, "\\", nomexp, ".gpkg")
            cmd = paste0("r.out.gdal --quiet --overwrite -c -f input=", nomexp, " output=", nomsortieASCok, " type=Float32 format=GPKG nodata=-9999")
            print(cmd); system(paste0(BatGRASS, " ", SecteurGRASS, " --exec ", cmd))
            
            cmd = paste0(shQuote(OSGeo4W_path), " gdaladdo --config OGR_SQLITE_SYNCHRONOUS OFF -r AVERAGE ", nomsortieASCok, " 2 4 8 16 32 64 128 256")
            print(cmd); system(cmd)
            
            # On l'ajoute à la liste pour les calculs suivants (HWM, etc.)
            if (!("HWH_m" %in% extsortieASC)) {
              extsortieASC = c(extsortieASC, "HWH_m")
            }
          }
          if (length(which(extsortieASC=="HWM_m"))==0)
          {
            print("Export HWM, on ne pourrait garder que là où HWH et HSF >0")
            nTopo=which(extsortieASC=="Bottom_m")
            nHeau=which(extsortieASC=="HWH_m")
            nHSF=which(extsortieASC=="HSF_m2_s")
            if (length(nTopo)>0 & length(nHeau)>0)# & length(nCote)>0)
            {
              cmd=paste0("r.mask -r")
              # cmd=paste0("r.mask --quiet --overwrite raster=",nomres,"_",extsortieASC[nHSF])
              # print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
              
              nomexp=paste0(nomres,"_","HWM_m")
              cmd=paste0("r.series --overwrite input=",nomres,"_",extsortieASC[nTopo],",",nomres,"_",extsortieASC[nHeau]," output=",nomexp," method=sum"," nprocs=",NProcGrass)
              print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
              
              nomsortieASCok=paste0(dsnlayerC_,"\\",nomres,"_","HWM_m",".gpkg")
              
              cmd=paste0("r.out.gdal --quiet --overwrite -c -f input=",nomexp," output=",nomsortieASCok," type=Float32 format=GPKG nodata=-9999")
              print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
              
              cmd = paste0(shQuote(OSGeo4W_path)," gdaladdo ","--config OGR_SQLITE_SYNCHRONOUS OFF ", "-r AVERAGE ",nomsortieASCok," 2 4 8 16 32 64 128 256")
              print(cmd);system(cmd)
            }
          }
        }else{
          print(paste0('Déjà présent: ',paste0(dsnlayerC,"\\",nomres,"_",extsortieASC[length(extsortieASC)],".gpkg")))
        }
        
        listeRasterGpkg=list.files(dsnlayerC_,pattern=nomres)
        iciGpkg=which(substr(listeRasterGpkg,nchar(listeRasterGpkg)-4,nchar(listeRasterGpkg))==".gpkg")
        if (length(iciGpkg)>0)
        {
          listeRasterGpkg=listeRasterGpkg[iciGpkg]
          extsortieGPKG=substr(listeRasterGpkg,nchar(nomres)+2,nchar(listeRasterGpkg)-5)
          nomcopieqml=paste0(dsnlayerC_,"\\",nomres,"_",extsortieGPKG,".qml")
          file.copy(file.path(chem_routine,"C2D",Doss_Qml,paste0(extsortieGPKG,".qml")),
                    nomcopieqml,overwrite = T)
        }
        
        
        unlink(dirname(SecteurGRASS),recursive=TRUE)
        unlink(loc_path, recursive = TRUE)
      }
    }
  }
}

Etape10_fn <- function(){
  tryCatch({          if (ETAPE[10] == 1)
  {
    
    Etape10_PostTelemacGrass()
    
  }}, error = function(e) {skip_to_next <<- TRUE})
  
  if(skip_to_next){
    contours$BUG[nsecteur[ic]] <- "problème étape 10"
    contours$STEP_PRE[nsecteur[ic]] <- -2
  }
}


Etape10_fn_parallel <- function(){
  if (ETAPE[10] == 1)
  {
    
    Etape10_PostTelemacGrass()
    
  }
}