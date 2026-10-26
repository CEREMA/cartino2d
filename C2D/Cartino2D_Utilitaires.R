#################################################################################################
################   Buf_et_Coup
#################################################################################################
#-----------------------------------------------------------------------
#----------------DataObs_choix----------------------------
#-----------------------------------------------------------------------
# Gestion des différents formats de pluie

DataObs_choix=function()
{
  library(readxl)
  # chem_routine=R.home(component = "Cerema")
  paste0(chem_routine,"/PreC2D")
  Link_Data = "Data_obs.xlsx"
  Link_DataXLS = read_excel(file.path(chem_routine,"PreC2D",Link_Data))
  Link_DataXLS=Link_DataXLS[order(Link_DataXLS$Evenements),]
  nchoix = select.list(Link_DataXLS$Evenements,title = "Choix", multiple = T, graphics = T)
  Search = which(Link_DataXLS$Evenements %in% nchoix)
  ListeObs = as.data.frame(Link_DataXLS[Search,])
  # iShyreg=which(Link_DataXLS$TypPluie=="Stat" & Link_DataXLS$EPSG==ListeCas$EPSG)
  # Link_DataXLS$Dossier_PluieRaster[iShyreg]
  # ListeCas$Dossier_SHYREG=Link_DataXLS$Dossier_PluieRaster[iShyreg]
  return(ListeObs)
}
# Tampon et coupure commune à tous les contours Exzeco
Buf_et_Coup = function(dsnlayerC,
                       nomsortie,
                       nom_STEP2_Zone,
                       Zone,
                       ContourMax,
                       Exz_Buf,
                       Exz_Dx,
                       Ratio_Dx,
                       crs)
{
  ###########################################################################
  # Exzeco buffer exzeco pour nouvelle 2eme contrainte point
  
  Exz <- Zone %>%
    st_buffer(Exz_Buf) %>%
    st_cast("MULTILINESTRING",
            do_split = TRUE) %>%
    st_cast("LINESTRING",
            do_split = TRUE) %>%
    st_intersection(ContourMax) %>%
    st_simplify(preserveTopology = FALSE,
                dTolerance = 0.33 * Exz_Dx) %>%
    st_segmentize(Exz_Dx, crs = crs)
  
  st_crs(Exz) = crs
  
  ###########################################################################
  print("Possible plantage export st_write.... - mettre en commentaires si c'est le cas")
  # st_write(Exz,dsn=file.path(dsnlayerC,paste0(nomsortie,".shp")),layer=nomsortie, delete_layer=T, quiet=T)
  Exz = st_collection_extract(Exz, "LINESTRING")
  st_write(
    Exz,
    dsn = file.path(dsnlayerC, paste0(nomsortie, ".shp")),
    layer = nomsortie,
    delete_layer = T,
    quiet = T
  )
  file.copy(file.path(dsnlayer, paste0(nom_STEP2_Zone, ".qml")),
            file.path(dsnlayerC, paste0(nomsortie, ".qml")),
            copy.date = TRUE)
  
  # FONCTIONNettoyage des points trop proches
  Temp = st_sf(col = 0, st_cast(st_sfc(st_cast(
    st_sfc(Exz), "MULTIPOINT"
  )), "POINT"))
  
  Pts = st_coordinates(Temp)
  Proches = sapply(1:dim(Pts)[1],
                   function(x) {
                     ifelse((min((Pts[-x, 1] - Pts[x, 1]) ^ 2 + (Pts[-x, 2] - Pts[x, 2]) ^ 2) < (Ratio_Dx * Exz_Dx) ^ 2), 1, 0)
                   })
  
  Exz_Pts = Pts[which(Proches == 0), ]
  print(dim(Exz_Pts))
  
  return(list(Exz_Pts = Exz_Pts, Exz = Exz))
}

#################################################################################################
################   Gestion_Sections_Controle
#################################################################################################

# Gestion des sections de controle
Gestion_Sections_Controle = function(dsnlayer, dsnlayerC, contour, ExportTopo)
{
  nom_SectCont = contour$SectCont
  if (file.exists(nom_SectCont) == TRUE)
  {
    # Lecture du réseau
    SectControl = st_read(nom_SectCont)
    
    # On récupère ce qui intersecte
    # nbRC=st_intersects(SectControl,contour)
    contour_2 <- sfheaders::sf_remove_holes(contour)
    nbRC = st_within(SectControl, contour_2)
    
    n_int = which(sapply(nbRC, length) > 0)
    
    #
    if (length(n_int > 0) > 0)
    {
      print(paste0(nomcontour, " ---- Récupération des sections de controle"))
      SectControl = SectControl[n_int, ]
      st_write(
        SectControl,
        dsn = file.path(dsnlayerC, "SectControl_Select.shp"),
        delete_layer = T,
        quiet = T
      )
      
      if (ExportTopo == 1)
      {
        for (is in 1:dim(SectControl)[1])
        {
          nom_contour = paste0("Sect", is)
          nom_contour_gpkg=file.path(dsnlayerC,paste0(nom_contour,".gpkg"))
          ######################################################################
          #GRAVe valeur autour de la section, à paramétrer
          Chasse = 5
          st_write(st_buffer(SectControl[is, ], Chasse),nom_contour_gpkg,delete_layer=TRUE,quiet=T)
          
          # importation vecteur dans grass grand contour
          cmd=paste0("v.in.ogr -o --quiet --overwrite input=",nom_contour_gpkg," output=",nom_contour)
          print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
          
          # browser()
          unlink(nom_contour_gpkg)
          
          cmd=paste0("r.mask -r")
          print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
          
          cmd=paste0("r.mask --quiet --overwrite vector=",nom_contour)
          print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
          
          
          nomoutput = file.path(dsnlayerC, paste0(SectControl[is, ]$ID, "topo.xyz"))
          cmd=paste0("r.out.xyz --quiet --overwrite input=","MNTFinal"," output=",nomoutput," separator=space")
          print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
          
          cmd=paste0("r.mask -r")
          print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
          
          xyz = read.table(nomoutput)
          ligne = st_coordinates(SectControl[is, ])
          # browser()
          UV = ligne[2, ] - ligne[1, ]
          UV = UV / sum(UV ^ 2) ^ 0.5
          # browser()
          nom = SectControl[is, ]$ID
          nom2 = strsplit(nom, "_")
          lg = nchar(nom2[[1]][length(nom2[[1]])])
          nom3 = substr(nom, 1, nchar(nom) - (lg + 1))
          Cote = as.numeric(nom2[[1]][length(nom2[[1]])])
          
          xyz$S = (xyz[, 1] - ligne[1, 1]) * UV[1] + (xyz[, 2] - ligne[1, 2]) *
            UV[2]
          xyz$AmAv = (xyz[, 1] - ligne[1, 1]) * UV[2] - (xyz[, 2] - ligne[1, 2]) *
            UV[1]
          
          nomoutput = file.path(dsnlayerC, paste0(SectControl[is, ]$ID, "topoSC.xyz"))
          write.table(xyz, nomoutput, row.names = F)
          
          nbcote = which(xyz$AmAv >= 0)
          
          # Enregistrement de l'image
          jpeg(
            filename = paste0(dsnlayerC, "\\", nom, "_topo.jpg"),
            width = 31.5,
            height = 44.55,
            units = "cm",
            quality = 75,
            res = 200
          )
          
          
          plot(xyz[nbcote, 4], xyz[nbcote, 3], main = nom)
          points(xyz[-nbcote, 4], xyz[-nbcote, 3], pch = 2)
          
          dz = 0.5
          points(
            c(min(xyz[, 4]), max(xyz[, 4])),
            c(Cote, Cote),
            "l",
            col = "blue",
            lwd = 3,
            lty = 1
          )
          points(
            c(min(xyz[, 4]), max(xyz[, 4])),
            c(Cote - dz, Cote - dz),
            "l",
            col = "blue",
            lwd = 2,
            lty = 3
          )
          points(
            c(min(xyz[, 4]), max(xyz[, 4])),
            c(Cote + dz, Cote + dz),
            "l",
            col = "blue",
            lwd = 2,
            lty = 3
          )
          dev.off()
        }
      }
    }
  }
}

#################################################################################################
################   Ligne limitée  à 70 caractères
#################################################################################################

LigneBonneLongueur = function(Lignes, liglong, Texte)
{
  afaire = 1
  inc = length(Lignes) + 1
  
  while (afaire == 1)
  {
    # print(Lignes)
    # print(Texte)
    if (is.na(Texte) == F)
    {
      if (nchar(Texte) > liglong)
      {
        indvirg = gregexpr(";", Texte)[[1]]
        if (max(indvirg) < liglong)
        {
          Lignes[inc] = substr(Texte, 1, max(indvirg))
          Texte = substr(Texte, max(indvirg) + 1, nchar(Texte))
        } else{
          Lignes[inc] = substr(Texte, 1, indvirg[which((indvirg < liglong) == F)[1] -
                                                   1])
          Texte = substr(Texte, indvirg[which((indvirg < liglong) == F)[1] -
                                          1] + 1, nchar(Texte))
        }
        inc = inc + 1
        
      }
    }
    
    print(Lignes)
    print(Texte)
    
    if (is.na(Texte) == F)
    {
      if (nchar(Texte) <= liglong)
      {
        Lignes[inc] = Texte
        inc = inc + 1
        afaire = 0
      }
    } else
    {
      afaire = 0
      # Lignes[inc]=""
    }
  }
  return(Lignes)
}

#################################################################################################
################   Enleve les trous d'un vecteur
#################################################################################################

EnleveTrou = function(vecteur)
{
  # # Zones d'intérêts
  # dsnlayer="D:\\Tempete_Alex_2020\\Moi\\BaseEmprise"
  # nomlayer=  "EAIP_RTM_ExzItalie_Telemac1"
  #
  # #Zones d'intéret
  # vecteur=st_read(dsnlayer,
  #                 layer = nomlayer)
  vecteur = st_cast(vecteur, "POLYGON")
  crs = st_crs(EPSG)
  Res = list()
  for (i in 1:dim(vecteur)[1])
  {
    Zone_ok = vecteur[i, ]
    Zone_ok = st_cast(Zone_ok[st_area(Zone_ok) == max(st_area(Zone_ok)), ], "POLYGON")
    nb1 = which(st_coordinates(Zone_ok)[, 3] == 1 &
                  st_coordinates(Zone_ok)[, 4] == 1)
    toto = list()
    toto[[1]] = st_coordinates(Zone_ok)[nb1, 1:2]
    Res[[i]] = st_sf(data.frame(Aire = "0"),
                     "geometry" = st_sfc(st_polygon(toto, dim = "XY")),
                     crs = crs)
  }
  final = do.call(rbind, Res)
  final[, "Aire"] = round(st_area(final) / 10)
  final = st_buffer(final, 0)
  # st_write(final ,file.path(dsnlayer,"final.shp"), delete_layer=T, quiet=T)
  return(final)
}

#################################################################################################
################   enleve les nouveaux points trop proche des anciens
#################################################################################################

GardeNvxPointsLoinPointsInitiaux = function(NewPoints, IniPoint, Ratio, Dist)
{
  for (incIni in (1:dim(IniPoint)[1]))
  {
    if (length(NewPoints) > 1)
    {
      asupp = which((NewPoints[, 1] - IniPoint[incIni, 1]) ^ 2 + (NewPoints[, 2] -
                                                                    IniPoint[incIni, 2]) ^ 2 < (Ratio * Dist) ^ 2)
      if (length(asupp) > 0)
      {
        NewPoints = NewPoints[-asupp, ]
      }
    }
    # cat(dim(NewPoints)," ",length(NewPoints),"\n")
    if (length(NewPoints) == 2)
    {
      NewPoints = NA
    }
  }
  return(NewPoints)
}


####### fichier prérequis copie ###################
# Copie du dossier base dans dossier export
# copie du cas 
# copie des bons fichiers fortran
# si pluie spatialisée raindef=4

copie_prerequis = function()
{
  if (file.exists(dsnlayerC)==F){dir.create(dsnlayerC, recursive = T)}
  qmlcasqgz = list.files(file.path(chem_routine,"C2D",Doss_Base), recursive = F)
  nici=grep(qmlcasqgz,pattern="USER_FORTRA")
  if (length(nici)>0){qmlcasqgz=qmlcasqgz[-nici]}
  file.copy(file.path(chem_routine,"C2D",Doss_Base, qmlcasqgz),
            file.path(dsnlayerC, qmlcasqgz))
  
  for (USER_FORTRAN in c("USER_FORTRAN_PH","USER_FORTRAN_PS"))
  {
    if (file.exists(file.path(dsnlayerC,USER_FORTRAN))==F){dir.create(file.path(dsnlayerC,USER_FORTRAN))}
    
    qmlcasqgz = list.files(file.path(chem_routine,"C2D",Doss_Base, USER_FORTRAN))
    
    if(is.na(contour$OH)){
      qmlcasqgz <- qmlcasqgz[-which(qmlcasqgz=="buse.f" | qmlcasqgz=="debsce.f" )]
      
    }
    file.copy(
      file.path(chem_routine,"C2D",Doss_Base, USER_FORTRAN, qmlcasqgz),
      file.path(dsnlayerC, USER_FORTRAN, qmlcasqgz)
      ,overwrite = TRUE)
  }
}



########################plot section de controle 

#
plot_sc_obs_ohh <- function() {
  library(tidyverse)
  library(sf)
  library(plotly)
  library(htmlwidgets)
  library(tools)
  
  # --- 1. FONCTIONS UTILITAIRES ---
  
  get_rain_step <- function(filename) {
    parts <- strsplit(filename, "_")[[1]]
    step_part <- grep("^\\d+min$", parts, value = TRUE)
    if (length(step_part) > 0) {
      val <- as.numeric(gsub("min", "", step_part[1]))
      return(if(is.na(val) || val == 0) 1 else val)
    }
    return(1) 
  }
  
  parse_time_meta <- function(filename) {
    parts <- strsplit(filename, "_")[[1]]
    is_evt <- grepl("Evt", filename, fixed = TRUE)
    if (is_evt) {
      date_deb <- strptime(gsub("[^0-9]", "", parts[4]), format = "%Y%m%d%H%M", tz = "UTC")
      h_m <- strsplit(parts[5], "h")[[1]]
      duree <- (as.numeric(h_m[1]) + as.numeric(substr(h_m[2], 1, 2)) / 60) * 3600
    } else {
      date_deb <- strptime("190001010000", format = "%Y%m%d%H%M", tz = "UTC")
      duree <- (as.numeric(parts[7]) + as.numeric(parts[8]) / 60) * 3600
    }
    list(date_deb = as.POSIXct(date_deb), duree = duree, is_evt = is_evt)
  }
  
  parse_telemac_sortie <- function(file_path) {
    message(paste("    [DEBUG]", Sys.time(), "- Début lecture fichier sortie:", basename(file_path)))
    if (!file.exists(file_path)) return(NULL)
    
    lines <- readLines(file_path, warn = FALSE)
    
    # Correction critique : Ancrage ^ pour ne pas capturer "VARIABLE TIME STEP" par erreur
    ts_idx <- grep("^\\s*TIME STEP", lines, useBytes = TRUE, ignore.case = TRUE)[1]
    if (is.na(ts_idx)) {
      message("    [WARN] TIME STEP introuvable, valeur par défaut 1.0 utilisée.")
      past_ts <- 1.0
    } else {
      extracted_ts <- gsub(".*TIME STEP\\s*[:=]?\\s*([0-9.]+).*", "\\1", lines[ts_idx], ignore.case = TRUE)
      past_ts <- as.numeric(extracted_ts)
      if(is.na(past_ts)) past_ts <- 1.0
    }
    
    idx_interest <- grep("ITERATION|FLUX BOUNDARY|CULVERT|RUNOFF_SCS_CN|VOLUME IN THE DOMAIN|ADDITIONAL VOLUME", lines, useBytes = TRUE)
    if (length(idx_interest) == 0) return(NULL)
    ls <- lines[idx_interest]
    
    clean_numeric <- function(txt) {
      txt <- gsub("[*]", "", txt)
      as.numeric(txt)
    }
    
    mots_cles <- tribble(
      ~key, ~start, ~end,
      "ITERATION ", 12, 22,
      "RUNOFF_SCS_CN : ACCUMULATED RAINFALL", 44, 61,
      "VOLUME IN THE DOMAIN", 28, 44,
      "ADDITIONAL VOLUME DUE TO SOURCE TERMS", 44, 61
    )
    
    flux_keys <- unique(gsub(":.*", "", grep("FLUX BOUNDARY", ls, value = TRUE, useBytes = TRUE)))
    culvert_keys <- unique(gsub(":.*", "", grep("CULVERT", ls, value = TRUE, useBytes = TRUE)))
    
    extra <- tibble(key = c(flux_keys, culvert_keys)) %>%
      mutate(start = if_else(grepl("CULVERT", key), 37, 25), end = if_else(grepl("CULVERT", key), 62, 42))
    
    all_keys <- bind_rows(mots_cles, extra)
    
    ref_idx <- grep("ITERATION ", ls, useBytes = TRUE)
    res <- matrix(NA, nrow = length(ref_idx), ncol = nrow(all_keys))
    
    message(paste("    [DEBUG]", Sys.time(), "- Extraction des valeurs..."))
    
    for (i in 1:nrow(mots_cles)) {
      m <- grep(mots_cles$key[i], ls, fixed = TRUE, useBytes = TRUE)
      if (length(m) > 0) {
        vals <- clean_numeric(substr(ls[m], mots_cles$start[i], mots_cles$end[i]))
        indices <- findInterval(m, ref_idx)
        valid <- indices > 0 & indices <= nrow(res)
        col_idx <- match(mots_cles$key[i], all_keys$key)
        res[indices[valid], col_idx] <- vals[valid]
      }
    }
    
    if (length(flux_keys) > 0) {
      m_flux <- grep("FLUX BOUNDARY", ls, fixed = TRUE, useBytes = TRUE)
      if (length(m_flux) > 0) {
        txt_flux <- ls[m_flux]
        vals_flux <- clean_numeric(substr(txt_flux, 25, 42))
        keys_flux <- gsub(":.*", "", txt_flux)
        rows <- findInterval(m_flux, ref_idx)
        cols <- match(keys_flux, all_keys$key)
        valid <- !is.na(cols) & rows > 0 & rows <= nrow(res)
        res[cbind(rows[valid], cols[valid])] <- vals_flux[valid]
      }
    }
    
    if (length(culvert_keys) > 0) {
      m_cul <- grep("CULVERT", ls, fixed = TRUE, useBytes = TRUE)
      if (length(m_cul) > 0) {
        txt_cul <- ls[m_cul]
        vals_cul <- clean_numeric(substr(txt_cul, 37, 62))
        keys_cul <- gsub(":.*", "", txt_cul)
        rows <- findInterval(m_cul, ref_idx)
        cols <- match(keys_cul, all_keys$key)
        valid <- !is.na(cols) & rows > 0 & rows <= nrow(res)
        res[cbind(rows[valid], cols[valid])] <- vals_cul[valid]
      }
    }
    
    df <- as.data.frame(res)
    colnames(df) <- all_keys$key
    df$Past_Ts <- past_ts
    
    # Nettoyage des lignes où l'itération n'a pas été trouvée (ex: header)
    df <- df[!is.na(df[, 1]), ]
    
    message(paste("    [DEBUG]", Sys.time(), "- Parsing terminé. Lignes de données valides:", nrow(df)))
    gc() 
    return(df)
  }
  
  # --- 2. INITIALISATION ---
  message(paste("[INFO]", Sys.time(), "Démarrage plot_sc_obs_oh"))
  
  sc_list <- list.files(dsnlayerC, pattern = "SC.txt")
  list_sortie <- list.files(dsnlayerC, pattern = "s.sortie")
  list_sortie <- list_sortie[!grepl("\\.txt$", list_sortie)]
  
  if (length(sc_list) == 0) return(message("Aucun fichier SC.txt trouvé."))
  
  path_shp <- file.path(dsnlayerC, "SectControl_Select.shp")
  sc_sf <- st_read(path_shp, quiet = TRUE) %>% mutate(ID = as.character(ID))
  contour_aire <- st_read(file.path(dsnlayerC, "Step_Cartino2d_1_Contour.shp"), quiet = TRUE)
  aire_val <- as.numeric(st_area(contour_aire))
  
  dsnscobs <- file.path(dsnlayer, "_SectionsControles", contour$NOMPOST)
  
  cache_sortie <- list()      
  cache_rain_plots <- list()  
  
  # --- 3. BOUCLE PRINCIPALE ---
  
  for (f_sc in sc_list) {
    message(paste("----------------------------------------------------------------"))
    message(paste("[STEP]", Sys.time(), "Traitement fichier SC :", f_sc))
    
    racine <- sub("SC.txt$", "", f_sc)
    
    p_q_out <- file.path(dsnlayerC, paste0(racine, "_PN_PB_Q_Obs.html"))
    p_z_out <- file.path(dsnlayerC, paste0(racine, "_PN_PB_Z_Obs.html"))
    
    if (file.exists(p_q_out) && file.exists(p_z_out)) {
      message(paste("  -> [SKIP] Fichiers HTML déjà présents."))
      next
    }
    
    meta <- parse_time_meta(f_sc)
    f_out_match <- list_sortie[grep(racine, list_sortie)]
    
    if (length(f_out_match) == 0) {
      message(paste("  -> [WARN] Aucun fichier .sortie trouvé."))
      tab_out <- NULL
    } else {
      if (length(f_out_match) > 1) {
        dates_out <- sub(".*\\.cas_(\\d{4}-\\d{2}-\\d{2}-\\d{2}h\\d{2}min\\d{2}s)\\.sortie", "\\1", f_out_match)
        f_out_match <- f_out_match[which.max(as.POSIXct(dates_out, format = "%Y-%m-%d-%Hh%Mmin%Ss"))]
      }
      
      if (is.null(cache_sortie[[f_out_match]])) {
        cache_sortie[[f_out_match]] <- parse_telemac_sortie(file.path(dsnlayerC, f_out_match))
      }
      tab_out <- cache_sortie[[f_out_match]]
    }
    
    if (!is.null(tab_out)) {
      parts_sc <- strsplit(basename(f_sc), "_")[[1]]
      pattern_brute <- paste0("brute_", paste(parts_sc[1:6], collapse="_"))
      dir_pluie <- tryCatch(file.path(dsnPluie, contour$NOMPOST), error = function(e) NULL)
      file_brute <- if (!is.null(dir_pluie) && dir.exists(dir_pluie)) {
        list.files(dir_pluie, pattern = pattern_brute, full.names = TRUE)[1]
      } else NULL
      
      cache_key_rain <- paste0(f_out_match, ifelse(!is.na(file_brute), basename(file_brute), "NONE"))
      
      if (is.null(cache_rain_plots[[cache_key_rain]])) {
        message(paste("  -> [PLOT] Préparation données pluie..."))
        
        past_ts <- tab_out$Past_Ts[1]
        if (is.na(past_ts)) past_ts <- 1
        
        # Calcul du pas d'export robuste (médiane des différences)
        if (nrow(tab_out) > 1) {
          # On utilise la médiane pour éviter les artefacts de début/fin ou trous
          pas_exp <- median(diff(tab_out[[1]]), na.rm = TRUE) * past_ts
        } else {
          pas_exp <- past_ts
        }
        if (is.na(pas_exp) || pas_exp <= 0) pas_exp <- 1 
        
        message(paste("    -> [INFO] Pas d'export calculé :", pas_exp, "s"))
        
        t_axis <- meta$date_deb + (tab_out[[1]] * past_ts)
        
        df_pluie <- data.frame(
          Time = t_axis,
          Intensite = replace_na(tab_out[[2]], 0) * 1000 * 3600 / pas_exp, 
          PNette = replace_na(tab_out[[4]], 0) / aire_val * 1000 * 3600 / past_ts,
          CumulBrut = replace_na(tab_out[[2]], 0) * 1000, 
          CumulNet = cumsum(replace_na(tab_out[[4]], 0) / aire_val * pas_exp / past_ts) * 1000,
          NomBrut = "Pluie brute", NomNet = "Pluie nette"
        )
        
        if (!is.na(file_brute) && file.exists(file_brute)) {
          message(paste("    -> [BRUTE] Fichier pluie trouvé :", basename(file_brute)))
          tab_brute <- read.table(file_brute, header = TRUE)
          rain_step <- get_rain_step(basename(file_brute))
          message(paste("    -> [BRUTE] Pas de temps détecté :", rain_step, "min"))
          
          col_val <- if("Val" %in% names(tab_brute)) "Val" else names(tab_brute)[2]
          
          t_brute <- meta$date_deb + (0:(nrow(tab_brute)-1)) * 60 
          
          vals_brute_interp <- approx(x = t_brute, y = tab_brute[[col_val]], xout = t_axis, rule = 2)$y
          cumul_brute_native <- cumsum(tab_brute[[col_val]] / rain_step)
          cumul_brute_interp <- approx(x = t_brute, y = cumul_brute_native, xout = t_axis, rule = 2)$y
          
          df_pluie$Intensite <- vals_brute_interp * 3600 / (pas_exp * rain_step)
          df_pluie$CumulBrut <- cumul_brute_interp
        }
        
        df_pluie <- df_pluie %>% filter(!is.na(Time))
        
        if (nrow(df_pluie) > 2000) {
          idx_keep <- unique(round(seq(1, nrow(df_pluie), length.out = 2000)))
          df_pluie <- df_pluie[idx_keep, ]
        }
        
        if (nrow(df_pluie) > 0) {
          # Hyétogramme (Carrés)
          g1 <- ggplotly(ggplot(df_pluie) +
                           geom_col(aes(x = Time, y = Intensite, fill = "Pluie brute"), position = "identity") +
                           geom_col(aes(x = Time, y = PNette, fill = "Pluie nette"), position = "identity") +
                           scale_fill_manual(values = c("Pluie brute" = "#33FFFF", "Pluie nette" = "#9933FF")) + 
                           theme_bw() + ylab("Intensité [mm/h]") + 
                           theme(axis.title.x = element_blank(), legend.title = element_blank()))
          
          # Cumul (Lignes)
          g3 <- ggplotly(ggplot(df_pluie) +
                           geom_line(aes(x = Time, y = CumulBrut, color = "Cumul brut")) +
                           geom_line(aes(x = Time, y = CumulNet, color = "Cumul net")) +
                           scale_color_manual(values = c("Cumul brut" = "#33FFFF", "Cumul net" = "#9933FF")) + 
                           theme_bw() + ylab("Cumul [mm]") + 
                           theme(axis.title.x = element_blank(), legend.title = element_blank()))
          
          cache_rain_plots[[cache_key_rain]] <- list(g1 = g1, g3 = g3, t_axis = t_axis)
        } else {
          cache_rain_plots[[cache_key_rain]] <- NULL
        }
      }
      rain_objs <- cache_rain_plots[[cache_key_rain]]
    } else {
      rain_objs <- NULL
    }
    
    if (is.null(rain_objs)) rain_objs <- list(g1 = plotly_empty(), g3 = plotly_empty(), t_axis = NULL)
    
    message(paste("  -> [DATA] Lecture données SC"))
    cl_names <- c("nom", "numsec", "Time", "Q", "Zmoy", "Zmax", "Zmin", "Hmax", "Htot")
    tab_sc <- read.csv(file.path(dsnlayerC, f_sc), header = FALSE, sep = "", skip = 2, col.names = cl_names)
    
    debut_time <- which(as.numeric(tab_sc$Time) >= 60)[1]
    if (!is.na(debut_time) && debut_time > 1) tab_sc <- tab_sc[debut_time:nrow(tab_sc), ]
    
    tab_sc[, 2:9] <- lapply(tab_sc[, 2:9], as.numeric)
    tab_sc$DATE <- meta$date_deb + tab_sc$Time
    tab_sc$nom <- as.character(tab_sc$nom)
    tab_sc <- inner_join(tab_sc, sc_sf, by = c("nom" = "ID"))
    
    message(paste("  -> [OBS] Chargement observations"))
    sc_names <- unique(tab_sc$NOM)
    sc_list_obs <- list.files(path = dsnscobs, full.names = TRUE)
    
    load_obs <- function(suffix, col_name) {
      files_match <- unlist(lapply(sc_names, function(x) grep(paste0(x, suffix), sc_list_obs, value = TRUE)))
      if (length(files_match) == 0) return(NULL)
      df_list <- lapply(files_match, read.csv)
      df_obs <- do.call(rbind, df_list)
      df_obs$Date <- as.POSIXct(ifelse(grepl(" ", df_obs$Date), df_obs$Date, paste(df_obs$Date, "00:00:00")), tz = "UTC")
      lim_inf <- min(tab_sc$DATE, na.rm = TRUE)
      lim_sup <- max(tab_sc$DATE, na.rm = TRUE)
      df_obs <- df_obs %>% filter(Date >= lim_inf & Date <= lim_sup)
      if (nrow(df_obs) > 0) {
        colnames(df_obs) <- c("Date", col_name, "NOM")
        df_obs$NOM <- paste0(df_obs$NOM, "_obs")
        return(df_obs)
      }
      return(NULL)
    }
    
    obs_q <- load_obs("_Q.csv", "Q")
    df_q_all <- bind_rows(tab_sc %>% dplyr::select(Date = DATE, Q, NOM) %>% mutate(Q = abs(Q), TYPE = "solid"),
                          if (!is.null(obs_q)) obs_q %>% mutate(Q = abs(Q), TYPE = "dashed") else NULL)
    
    obs_z <- load_obs("_Z.csv", "Z")
    df_z_all <- bind_rows(tab_sc %>% dplyr::select(Date = DATE, Z = Zmoy, NOM) %>% mutate(Z = ifelse(Z == 0, NA, Z), TYPE = "solid"),
                          if (!is.null(obs_z)) obs_z %>% mutate(TYPE = "dashed") else NULL)
    
    obs_h <- load_obs("_H.csv", "H")
    df_h_all <- bind_rows(tab_sc %>% dplyr::select(Date = DATE, H = Hmax, NOM) %>% mutate(TYPE = "solid"),
                          if (!is.null(obs_h)) obs_h %>% mutate(TYPE = "dashed") else NULL)
    
    save_master_plot <- function(df_data, y_col, y_lab, output_path, title_suffix) {
      if (file.exists(output_path)) return()
      
      df_data <- df_data %>% filter(!is.na(Date)) %>% arrange(NOM, Date) 
      if (nrow(df_data) == 0) return(message("      [WARN] Pas de données à tracer."))
      
      df_data <- df_data %>% group_by(NOM) %>%
        filter(if(n() > 2000) row_number() %in% unique(round(seq(1, n(), length.out = 2000))) else TRUE) %>% ungroup()
      
      g_main <- ggplot() +
        geom_line(data = df_data %>% filter(TYPE == "solid"), 
                  aes(x = Date, y = .data[[y_col]], color = NOM), 
                  linetype = "solid", size = 0.7) +
        geom_line(data = df_data %>% filter(TYPE != "solid"), 
                  aes(x = Date, y = .data[[y_col]], color = NOM), 
                  linetype = "dashed", size = 0.7) +
        theme_bw() + 
        ylab(y_lab) + xlab("Date") +
        theme(legend.title = element_blank())
      
      titre_graph <- paste0("Hyéto + ", title_suffix, " ", racine, " (Pluie sur tout le bassin)")
      
      p_final <- subplot(rain_objs$g3, rain_objs$g1, ggplotly(g_main), 
                         nrows = 3, shareX = TRUE, heights = c(0.1, 0.2, 0.7), titleY = TRUE) %>%
        layout(title = list(text = titre_graph), 
               hovermode = "closest",
               legend = list(orientation = "v", title = list(text = "")))
      
      saveWidget(partial_bundle(p_final), file = output_path, selfcontained = TRUE, libdir = "lib")
    }
    
    message(paste("  -> [EXPORT] Début écriture fichiers HTML"))
    p_h_out <- file.path(dsnlayerC, paste0(racine, "_PN_PB_H_Obs.html")) 
    save_master_plot(df_q_all, "Q", "Hydrogramme", p_q_out, "Hydrogramme")
    save_master_plot(df_z_all, "Z", "Côtes", p_z_out, "Côtes")
    save_master_plot(df_h_all, "H", "Hauteur", p_h_out, "Hauteur")
    
    # E. Flux et Ouvrages
    name_case_clean <- paste(strsplit(f_sc, "_")[[1]][1:6], collapse = "_") 
    p_qs_out <- file.path(dsnlayerC, paste0(name_case_clean, "_PN_PB_QS_Obs.html"))
    p_qoh_out <- file.path(dsnlayerC, paste0(name_case_clean, "_PN_PB_QOH_Obs.html"))
    
    if (!is.null(tab_out) && !file.exists(p_qs_out)) {
      df_flux <- tab_out %>% dplyr::select(matches("FLUX BOUNDARY|CULVERT"))
      if (ncol(df_flux) > 0) {
        t_flux <- meta$date_deb + (tab_out[[1]] * past_ts)
        df_long <- df_flux %>% mutate(Time = t_flux) %>% 
          pivot_longer(-Time) %>% filter(!is.na(value) & value != 0 & !is.na(Time)) 
        
        if (nrow(df_long) > 2000) {
          idx_keep <- unique(round(seq(1, nrow(df_long), length.out = 2000)))
          df_long <- df_long[idx_keep, ]
        }
        
        if (any(grepl("FLUX", df_long$name))) {
          g_qs <- ggplot(df_long %>% filter(grepl("FLUX", name)), aes(Time, abs(value), color = name)) + 
            geom_line() + theme_bw() + ylab("m3/s") + 
            theme(legend.title = element_blank())
          
          p_qs <- subplot(rain_objs$g3, rain_objs$g1, ggplotly(g_qs), nrows = 3, shareX = TRUE, heights = c(0.1, 0.2, 0.7), titleY=TRUE) %>%
            layout(title = paste("Sorties -", racine), hovermode = "closest", legend = list(title = list(text = "")))
          saveWidget(partial_bundle(p_qs), file = p_qs_out, selfcontained = TRUE, libdir = "lib")
        }
        
        if (any(grepl("CULVERT", df_long$name)) && !file.exists(p_qoh_out)) {
          g_qoh <- ggplot(df_long %>% filter(grepl("CULVERT", name)), aes(Time, value, color = name)) + 
            geom_line() + theme_bw() + ylab("m3/s") + 
            theme(legend.title = element_blank())
          
          p_qoh <- subplot(rain_objs$g3, rain_objs$g1, ggplotly(g_qoh), nrows = 3, shareX = TRUE, heights = c(0.1, 0.2, 0.7), titleY=TRUE) %>%
            layout(title = paste("Ouvrages -", racine), hovermode = "closest", legend = list(title = list(text = "")))
          saveWidget(partial_bundle(p_qoh), file = p_qoh_out, selfcontained = TRUE, libdir = "lib")
        }
      }
    }
    
    message(paste("  -> [GPKG] Mise à jour spatiale Qmax"))
    tabmaxq <- tab_sc %>% group_by(nom) %>% filter(abs(Q) == max(abs(Q))) %>% slice(1)
    new_sc_sf <- sc_sf %>% 
      left_join(tabmaxq %>% dplyr::select(nom, Q, DATE, Zmoy, Hmax), by = c("ID" = "nom")) %>%
      mutate(Q = abs(Q), NomSecteur = basename(dsnlayerC))
    
    st_write(new_sc_sf, file.path(dsnlayerC, paste0(gsub("\\..*", "", f_sc), "_Qmax.gpkg")), driver = "GPKG", delete_dsn = TRUE, quiet = TRUE)
    file.copy(file.path(chem_routine, "C2D", Doss_Qml, "SC_Qmax.qml"), file.path(dsnlayerC, paste0(gsub("\\..*", "", f_sc), "_Qmax.qml")), overwrite = TRUE)
  }
  
  if (dir.exists(file.path(dsnlayerC, "lib"))) unlink(file.path(dsnlayerC, "lib"), recursive = TRUE)
  message(paste("[FIN]", Sys.time(), "Traitement terminé avec succès."))
}







plot_sc_obs_ohgem <- function() {
  library(tidyverse)
  library(sf)
  library(plotly)
  library(htmlwidgets)
  library(tools)
  
  # --- 1. FONCTIONS UTILITAIRES ---
  
  get_rain_step <- function(filename, is_evt = TRUE) {
    # 1. Recherche explicite
    val <- as.numeric(gsub(".*[[:punct:]](\\d+)min([[:punct:]]|$).*", "\\1", filename))
    
    # 2. Sécurité si échec
    if (is.na(val) || val == 0 || val > 1440) { 
      parts <- strsplit(filename, "_")[[1]]
      mins <- grep("^\\d+min$", parts, value = TRUE)
      if (length(mins) > 0) {
        val <- as.numeric(gsub("min", "", mins[1]))
      } else {
        # 2. Règle SHYREG / Non-Evt : Défaut à 15 min
        val <- if (!is_evt || grepl("shyreg|spatial", filename, ignore.case = TRUE)) 15 else 1 
      }
    }
    return(if(is.na(val) || val == 0) 1 else val)
  }
  
  parse_time_meta <- function(filename) {
    parts <- strsplit(filename, "_")[[1]]
    is_evt <- grepl("Evt", filename, fixed = TRUE)
    duree <- 0
    date_deb <- as.POSIXct("1900-01-01", tz="UTC")
    
    if (is_evt) {
      if (length(parts) >= 5) {
        date_deb <- strptime(gsub("[^0-9]", "", parts[4]), format = "%Y%m%d%H%M", tz = "UTC")
        h_m <- strsplit(parts[5], "h")[[1]]
        if(length(h_m) >= 2) duree <- (as.numeric(h_m[1]) + as.numeric(substr(h_m[2], 1, 2)) / 60) * 3600
      }
    } else {
      if (length(parts) >= 8) {
        duree <- (as.numeric(parts[7]) + as.numeric(parts[8]) / 60) * 3600
      }
    }
    list(date_deb = as.POSIXct(date_deb), duree = duree, is_evt = is_evt)
  }
  
  parse_telemac_sortie <- function(file_path) {
    message(paste("    [DEBUG]", Sys.time(), "- Lecture sortie:", basename(file_path)))
    if (!file.exists(file_path)) return(NULL)
    
    past_ts <- NA
    dir_path <- dirname(file_path)
    base_name <- basename(file_path)
    cas_name_candidate <- sub("_\\d{4}-\\d{2}-\\d{2}-\\d{2}h\\d{2}min\\d{2}s\\.sortie$", "", base_name)
    
    if (cas_name_candidate == base_name) {
      cas_name_candidate <- substr(base_name, 1, nchar(base_name) - 30)
    }
    
    cas_files <- list.files(dir_path, pattern = paste0("^", create_regex_safe(cas_name_candidate)), full.names = TRUE)
    cas_files <- cas_files[!grepl("\\.sortie$", cas_files)]
    
    if (length(cas_files) > 0) {
      lines_cas <- readLines(cas_files[1], warn = FALSE)
      ts_idx_cas <- grep("TIME STEP|PAS DE TEMPS", lines_cas, ignore.case = TRUE)[1]
      if (!is.na(ts_idx_cas)) {
        raw_line <- lines_cas[ts_idx_cas]
        extracted_ts <- gsub(".*(?:STEP|TEMPS)[^0-9.]*([0-9.]+).*", "\\1", raw_line, ignore.case = TRUE)
        past_ts <- as.numeric(extracted_ts)
        if(!is.na(past_ts)) message(paste("    [INFO] TIME STEP lu dans .cas :", past_ts))
      }
    }
    
    lines <- readLines(file_path, warn = FALSE)
    
    if (is.na(past_ts)) {
      ts_idx <- grep("TIME STEP|PAS DE TEMPS", lines, useBytes = TRUE, ignore.case = TRUE)[1]
      if (!is.na(ts_idx)) {
        raw_line <- lines[ts_idx]
        extracted_ts <- gsub(".*(?:STEP|TEMPS)[^0-9.]*([0-9.]+).*", "\\1", raw_line, ignore.case = TRUE)
        past_ts <- as.numeric(extracted_ts)
        if(!is.na(past_ts)) message(paste("    [INFO] TIME STEP lu dans .sortie :", past_ts))
      }
    }
    
    if (is.na(past_ts)) {
      message("    [WARN] TIME STEP introuvable. Valeur par défaut 1.0 utilisée.")
      past_ts <- 1.0
    }
    
    idx_interest <- grep("ITERATION|FLUX BOUNDARY|CULVERT|RUNOFF_SCS_CN|VOLUME IN THE DOMAIN|ADDITIONAL VOLUME", lines, useBytes = TRUE)
    if (length(idx_interest) == 0) return(NULL)
    ls <- lines[idx_interest]
    
    clean_numeric <- function(txt) {
      txt <- gsub("[*]", "", txt)
      as.numeric(txt)
    }
    
    mots_cles <- tribble(
      ~key, ~start, ~end,
      "ITERATION ", 12, 22,
      "RUNOFF_SCS_CN : ACCUMULATED RAINFALL", 44, 61,
      "VOLUME IN THE DOMAIN", 28, 44,
      "ADDITIONAL VOLUME DUE TO SOURCE TERMS", 44, 61
    )
    
    flux_keys <- unique(gsub(":.*", "", grep("FLUX BOUNDARY", ls, value = TRUE, useBytes = TRUE)))
    culvert_keys <- unique(gsub(":.*", "", grep("CULVERT", ls, value = TRUE, useBytes = TRUE)))
    
    extra <- tibble(key = c(flux_keys, culvert_keys)) %>%
      mutate(start = if_else(grepl("CULVERT", key), 37, 25), end = if_else(grepl("CULVERT", key), 62, 42))
    
    all_keys <- bind_rows(mots_cles, extra)
    ref_idx <- grep("ITERATION ", ls, useBytes = TRUE)
    res <- matrix(NA, nrow = length(ref_idx), ncol = nrow(all_keys))
    
    for (i in 1:nrow(mots_cles)) {
      m <- grep(mots_cles$key[i], ls, fixed = TRUE, useBytes = TRUE)
      if (length(m) > 0) {
        vals <- clean_numeric(substr(ls[m], mots_cles$start[i], mots_cles$end[i]))
        indices <- findInterval(m, ref_idx)
        valid <- indices > 0 & indices <= nrow(res)
        col_idx <- match(mots_cles$key[i], all_keys$key)
        res[indices[valid], col_idx] <- vals[valid]
      }
    }
    
    if (length(flux_keys) > 0) {
      m_flux <- grep("FLUX BOUNDARY", ls, fixed = TRUE, useBytes = TRUE)
      if (length(m_flux) > 0) {
        txt_flux <- ls[m_flux]
        vals_flux <- clean_numeric(substr(txt_flux, 25, 42))
        keys_flux <- gsub(":.*", "", txt_flux)
        rows <- findInterval(m_flux, ref_idx)
        cols <- match(keys_flux, all_keys$key)
        valid <- !is.na(cols) & rows > 0 & rows <= nrow(res)
        res[cbind(rows[valid], cols[valid])] <- vals_flux[valid]
      }
    }
    
    if (length(culvert_keys) > 0) {
      m_cul <- grep("CULVERT", ls, fixed = TRUE, useBytes = TRUE)
      if (length(m_cul) > 0) {
        txt_cul <- ls[m_cul]
        vals_cul <- clean_numeric(substr(txt_cul, 37, 62))
        keys_cul <- gsub(":.*", "", txt_cul)
        rows <- findInterval(m_cul, ref_idx)
        cols <- match(keys_cul, all_keys$key)
        valid <- !is.na(cols) & rows > 0 & rows <= nrow(res)
        res[cbind(rows[valid], cols[valid])] <- vals_cul[valid]
      }
    }
    
    df <- as.data.frame(res)
    colnames(df) <- all_keys$key
    df$Past_Ts <- past_ts
    df <- df[!is.na(df[, 1]), ] 
    
    message(paste("    [DEBUG] Parsing fini. Lignes:", nrow(df)))
    gc() 
    return(df)
  }
  
  create_regex_safe <- function(str) {
    gsub("([.|()\\^{}+$*?]|\\[|\\])", "\\\\\\1", str)
  }
  
  # --- 2. INITIALISATION ---
  message(paste("[INFO]", Sys.time(), "Start plot_sc_obs_oh"))
  sc_list <- list.files(dsnlayerC, pattern = "SC.txt")
  list_sortie <- list.files(dsnlayerC, pattern = "s.sortie")
  list_sortie <- list_sortie[!grepl("\\.txt$", list_sortie)]
  
  if (length(sc_list) == 0) return(message("Aucun SC.txt trouvé."))
  
  path_shp <- file.path(dsnlayerC, "SectControl_Select.shp")
  sc_sf <- st_read(path_shp, quiet = TRUE) %>% mutate(ID = as.character(ID))
  contour_aire <- st_read(file.path(dsnlayerC, "Step_Cartino2d_1_Contour.shp"), quiet = TRUE)
  aire_val <- as.numeric(st_area(contour_aire))
  dsnscobs <- file.path(dsnlayer, "_SectionsControles", contour$NOMPOST)
  
  cache_sortie <- list()      
  cache_rain_plots <- list()  
  
  # --- 3. BOUCLE PRINCIPALE ---
  
  for (f_sc in sc_list) {
    message(paste("----------------------------------------------------------------"))
    message(paste("[STEP] Traitement :", f_sc))
    
    # 1. Racine pour le matching interne (sans SC)
    racine_match <- sub("SC\\.txt$", "", f_sc)
    
    # 2. Racine pour le nommage des fichiers (avec SC s'il y est, sans extension)
    racine_nom <- tools::file_path_sans_ext(f_sc)
    
    # Définition précoce des chemins de sortie avec la bonne racine
    p_q_out <- file.path(dsnlayerC, paste0(racine_nom, "_PN_PB_Q_Obs.html"))
    p_z_out <- file.path(dsnlayerC, paste0(racine_nom, "_PN_PB_Z_Obs.html"))
    p_h_out <- file.path(dsnlayerC, paste0(racine_nom, "_PN_PB_H_Obs.html"))
    
    if (file.exists(p_q_out) && file.exists(p_z_out)) {
      message("  -> [SKIP] Fichiers existants.")
      next
    }
    
    meta <- parse_time_meta(f_sc)
    f_out_match <- list_sortie[grep(racine_match, list_sortie)]
    
    if (length(f_out_match) == 0) {
      message("  -> [WARN] .sortie introuvable.")
      tab_out <- NULL
    } else {
      if (length(f_out_match) > 1) {
        dates_out <- sub(".*\\.cas_(\\d{4}-\\d{2}-\\d{2}-\\d{2}h\\d{2}min\\d{2}s)\\.sortie", "\\1", f_out_match)
        f_out_match <- f_out_match[which.max(as.POSIXct(dates_out, format = "%Y-%m-%d-%Hh%Mmin%Ss"))]
      }
      if (is.null(cache_sortie[[f_out_match]])) {
        cache_sortie[[f_out_match]] <- parse_telemac_sortie(file.path(dsnlayerC, f_out_match))
      }
      tab_out <- cache_sortie[[f_out_match]]
    }
    
    rain_objs <- NULL 
    
    if (!is.null(tab_out)) {
      dir_pluie <- tryCatch(file.path(dsnPluie, contour$NOMPOST), error = function(e) NULL)
      file_brute <- NA
      
      if (!is.null(dir_pluie) && dir.exists(dir_pluie)) {
        all_brutes <- list.files(dir_pluie, pattern = "^(brute|shyreg|spatial)_.*\\.txt$", full.names = TRUE)
        if (length(all_brutes) > 0) {
          clean_sc <- sub("SC\\.txt$", "", f_sc)
          parts_sc <- strsplit(clean_sc, "_")[[1]]
          
          dur_token <- grep("^\\d+min$", parts_sc, value = TRUE)
          if(length(dur_token) == 0) dur_token <- "IGNORE_ME" else dur_token <- dur_token[1]
          
          evt_token <- grep("^Evts\\d+", parts_sc, value = TRUE)
          if(length(evt_token) == 0) evt_token <- "IGNORE_ME" else evt_token <- evt_token[1]
          
          candidates <- all_brutes
          if (dur_token != "IGNORE_ME") {
            has_dur <- grepl(paste0("_", dur_token, "([_.]|$)"), basename(candidates)) | grepl(dur_token, basename(candidates), fixed = TRUE)
            if (any(has_dur)) candidates <- candidates[has_dur]
          }
          if (evt_token != "IGNORE_ME") {
            has_evt <- grepl(evt_token, basename(candidates))
            if (any(has_evt)) candidates <- candidates[has_evt]
          }
          if (length(candidates) > 0) {
            scores <- sapply(candidates, function(cand) {
              parts_cand <- strsplit(basename(cand), "_")[[1]]
              sum(parts_cand %in% parts_sc)
            })
            file_brute <- candidates[which.max(scores)]
            message(paste("    -> [BRUTE] Trouvé:", basename(file_brute)))
          }
        }
      }
      
      cache_key_rain <- paste0(f_out_match, ifelse(!is.na(file_brute), basename(file_brute), "NONE"))
      
      if (is.null(cache_rain_plots[[cache_key_rain]])) {
        past_ts <- tab_out$Past_Ts[1]
        if (is.na(past_ts)) past_ts <- 1.0
        
        if (nrow(tab_out) > 1) {
          delta_iter <- median(diff(tab_out[[1]]), na.rm = TRUE)
          pas_exp <- delta_iter * past_ts
        } else {
          pas_exp <- past_ts
        }
        if (is.na(pas_exp) || pas_exp <= 0) pas_exp <- 1.0
        message(paste("    -> [INFO] Pas d'export:", pas_exp, "s"))
        
        t_axis <- meta$date_deb + (tab_out[[1]] * past_ts)
        
        # --- CALCUL PLUIE DEFAUT (TELEMAC) AVEC DECUMULATION ---
        acc_rain <- replace_na(tab_out[[2]], 0)
        inc_rain <- c(acc_rain[1], diff(acc_rain))
        inc_rain[inc_rain < 0] <- 0 
        
        df_pluie <- data.frame(
          Time = t_axis,
          Intensite = inc_rain * 1000 * 3600 / pas_exp, 
          PNette = replace_na(tab_out[[4]], 0) / aire_val * 1000 * 3600 / past_ts,
          CumulBrut = replace_na(tab_out[[2]], 0) * 1000, 
          CumulNet = cumsum(replace_na(tab_out[[4]], 0) / aire_val * pas_exp / past_ts) * 1000,
          NomBrut = "Pluie brute", NomNet = "Pluie nette"
        )
        
        # --- CORRECTION PLUIE BRUTE (MAPPING DIRECT) ---
        if (!is.na(file_brute) && file.exists(file_brute)) {
          tab_brute <- read.table(file_brute, header = TRUE)
          rain_step <- get_rain_step(basename(file_brute), is_evt = meta$is_evt)
          message(paste("    -> [BRUTE] Step:", rain_step, "min"))
          
          col_val <- if("Val" %in% names(tab_brute)) "Val" else names(tab_brute)[2]
          
          n_out <- nrow(df_pluie)
          if (nrow(tab_brute) >= n_out) {
            raw_vals <- tab_brute[[col_val]][1:n_out]
          } else {
            raw_vals <- c(tab_brute[[col_val]], rep(0, n_out - nrow(tab_brute)))
          }
          
          df_pluie$CumulBrut <- cumsum(raw_vals / rain_step)
          df_pluie$Intensite <- raw_vals * 3600 / (pas_exp * rain_step)
        }
        
        df_pluie <- df_pluie %>% filter(!is.na(Time))
        
        if (nrow(df_pluie) > 2000) {
          idx_keep <- unique(round(seq(1, nrow(df_pluie), length.out = 2000)))
          df_pluie <- df_pluie[idx_keep, ]
        }
        
        if (nrow(df_pluie) > 0) {
          cols_hyeto <- c("Pluie brute" = "#33FFFF", "Pluie nette" = "#9933FF")
          cols_cumul <- c("Cumul brut" = "#33FFFF", "Cumul net" = "#9933FF")
          
          g1 <- ggplotly(ggplot(df_pluie) +
                           geom_col(aes(x = Time, y = Intensite, fill = "Pluie brute"), position = "identity", width = pas_exp) +
                           geom_col(aes(x = Time, y = PNette, fill = "Pluie nette"), position = "identity", width = pas_exp) +
                           scale_fill_manual(name="", values = cols_hyeto) + 
                           theme_bw() + ylab("Intensité [mm/h]") + 
                           theme(axis.title.x = element_blank(), legend.title = element_blank()))
          
          g3 <- ggplotly(ggplot(df_pluie) +
                           geom_line(aes(x = Time, y = CumulBrut, color = "Cumul brut"), size = 1.1) +
                           geom_line(aes(x = Time, y = CumulNet, color = "Cumul net"), size = 1.1) +
                           scale_color_manual(name="", values = cols_cumul) + 
                           theme_bw() + ylab("Cumul [mm]") + 
                           theme(axis.title.x = element_blank(), legend.title = element_blank()))
          
          cache_rain_plots[[cache_key_rain]] <- list(g1 = g1, g3 = g3, t_axis = t_axis)
        } else {
          cache_rain_plots[[cache_key_rain]] <- NULL
        }
      }
      rain_objs <- cache_rain_plots[[cache_key_rain]]
    } else {
      rain_objs <- NULL
    }
    
    if (is.null(rain_objs)) {
      message("    [WARN] Données pluie invalides ou manquantes.")
      rain_objs <- list(g1 = NULL, g3 = NULL, t_axis = NULL)
    }
    
    message("  -> [DATA] SC + Obs")
    cl_names <- c("nom", "numsec", "Time", "Q", "Zmoy", "Zmax", "Zmin", "Hmax", "Htot")
    tab_sc <- read.csv(file.path(dsnlayerC, f_sc), header = FALSE, sep = "", skip = 2, col.names = cl_names)
    tab_sc <- tab_sc %>% filter(as.numeric(Time) >= 60)
    tab_sc[, 2:9] <- lapply(tab_sc[, 2:9], as.numeric)
    tab_sc$DATE <- meta$date_deb + tab_sc$Time
    tab_sc$nom <- as.character(tab_sc$nom)
    tab_sc <- inner_join(tab_sc, sc_sf, by = c("nom" = "ID"))
    
    sc_names <- unique(tab_sc$NOM)
    sc_list_obs <- list.files(path = dsnscobs, full.names = TRUE)
    
    load_obs <- function(suffix, col_name) {
      files_match <- unlist(lapply(sc_names, function(x) grep(paste0(x, suffix), sc_list_obs, value = TRUE)))
      if (length(files_match) == 0) return(NULL)
      df_list <- lapply(files_match, read.csv)
      df_obs <- do.call(rbind, df_list)
      df_obs$Date <- as.POSIXct(ifelse(grepl(" ", df_obs$Date), df_obs$Date, paste(df_obs$Date, "00:00:00")), tz = "UTC")
      df_obs <- df_obs %>% filter(Date >= min(tab_sc$DATE) & Date <= max(tab_sc$DATE))
      if (nrow(df_obs) > 0) {
        colnames(df_obs) <- c("Date", col_name, "NOM")
        df_obs$NOM <- paste0(df_obs$NOM, "_obs")
        return(df_obs)
      }
      return(NULL)
    }
    
    obs_q <- load_obs("_Q.csv", "Q")
    df_q_all <- bind_rows(tab_sc %>% dplyr::select(Date = DATE, Q, NOM) %>% mutate(Q = abs(Q), TYPE = "solid"),
                          if (!is.null(obs_q)) obs_q %>% mutate(Q = abs(Q), TYPE = "dashed") else NULL)
    
    obs_z <- load_obs("_Z.csv", "Z")
    df_z_all <- bind_rows(tab_sc %>% dplyr::select(Date = DATE, Z = Zmoy, NOM) %>% mutate(Z = ifelse(Z == 0, NA, Z), TYPE = "solid"),
                          if (!is.null(obs_z)) obs_z %>% mutate(TYPE = "dashed") else NULL)
    
    obs_h <- load_obs("_H.csv", "H")
    df_h_all <- bind_rows(tab_sc %>% dplyr::select(Date = DATE, H = Hmax, NOM) %>% mutate(TYPE = "solid"),
                          if (!is.null(obs_h)) obs_h %>% mutate(TYPE = "dashed") else NULL)
    
    save_master_plot <- function(df_data, y_col, y_lab, output_path, title_suffix) {
      if (file.exists(output_path)) return()
      df_data <- df_data %>% filter(!is.na(Date)) %>% arrange(NOM, Date)
      if (nrow(df_data) == 0) return()
      
      df_data <- df_data %>% group_by(NOM) %>%
        filter(if(n() > 2000) row_number() %in% unique(round(seq(1, n(), length.out = 2000))) else TRUE) %>% ungroup()
      
      g_main <- ggplot()
      
      df_solid <- df_data %>% filter(TYPE == "solid")
      if(nrow(df_solid) > 0) {
        g_main <- g_main + geom_line(data = df_solid, aes(x = Date, y = .data[[y_col]], color = NOM), linetype = "solid", size = 0.7)
      }
      
      df_dashed <- df_data %>% filter(TYPE != "solid")
      if(nrow(df_dashed) > 0) {
        g_main <- g_main + geom_line(data = df_dashed, aes(x = Date, y = .data[[y_col]], color = NOM), linetype = "dashed", size = 0.7)
      }
      
      g_main <- g_main + theme_bw() + ylab(y_lab) + xlab("Date (heure météo)") + theme(legend.title = element_blank())
      
      titre <- paste0("Hyéto + ", title_suffix, " ", racine_nom, " (Pluie sur tout le bassin)")
      
      plots_list <- list()
      heights <- c()
      rows_val <- 0
      
      has_rain <- !is.null(rain_objs) && !is.null(rain_objs$g3) && !inherits(rain_objs$g3, "plotly_empty")
      
      if (has_rain) {
        plots_list[[1]] <- rain_objs$g3
        plots_list[[2]] <- rain_objs$g1
        plots_list[[3]] <- ggplotly(g_main)
        heights <- c(0.1, 0.2, 0.7)
        rows_val <- 3
      } else {
        plots_list[[1]] <- ggplotly(g_main)
        heights <- c(1)
        rows_val <- 1
      }
      
      p_final <- subplot(plots_list, nrows = rows_val, shareX = TRUE, heights = heights, titleY = TRUE) %>%
        layout(title = list(text = titre), hovermode = "closest", legend = list(title = list(text = "")))
      saveWidget(partial_bundle(p_final), file = output_path, selfcontained = TRUE, libdir = "lib")
    }
    
    message("  -> [EXPORT] HTML Q/Z/H")
    save_master_plot(df_q_all, "Q", "Débit [m3/s]", p_q_out, "Hydrogramme")
    save_master_plot(df_z_all, "Z", "Côte [m]", p_z_out, "Côtes")
    save_master_plot(df_h_all, "H", "Hauteur [m]", p_h_out, "Hauteur")
    
    name_case_clean <- paste(strsplit(f_sc, "_")[[1]][1:6], collapse = "_") 
    p_qs_out <- file.path(dsnlayerC, paste0(name_case_clean, "_PN_PB_QS_Obs.html"))
    p_qoh_out <- file.path(dsnlayerC, paste0(name_case_clean, "_PN_PB_QOH_Obs.html"))
    
    if (!is.null(tab_out) && !file.exists(p_qs_out)) {
      df_flux <- tab_out %>% dplyr::select(matches("FLUX BOUNDARY|CULVERT"))
      if (ncol(df_flux) > 0) {
        t_flux <- meta$date_deb + (tab_out[[1]] * past_ts)
        df_long <- df_flux %>% mutate(Time = t_flux) %>% 
          pivot_longer(-Time) %>% filter(!is.na(value) & value != 0 & !is.na(Time)) 
        
        if (nrow(df_long) > 2000) {
          idx_keep <- unique(round(seq(1, nrow(df_long), length.out = 2000)))
          df_long <- df_long[idx_keep, ]
        }
        
        if (any(grepl("FLUX", df_long$name))) {
          g_qs <- ggplot(df_long %>% filter(grepl("FLUX", name)), aes(Time, abs(value), color = name)) + 
            geom_line() + theme_bw() + ylab("Débit [m3/s]") + xlab("Date (heure météo)") + theme(legend.title = element_blank())
          
          plots_list <- list()
          has_rain <- !is.null(rain_objs) && !is.null(rain_objs$g3) && !inherits(rain_objs$g3, "plotly_empty")
          if (has_rain) {
            plots_list[[1]] <- rain_objs$g3
            plots_list[[2]] <- rain_objs$g1
            plots_list[[3]] <- ggplotly(g_qs)
            h <- c(0.1, 0.2, 0.7); r <- 3
          } else {
            plots_list[[1]] <- ggplotly(g_qs)
            h <- c(1); r <- 1
          }
          p_qs <- subplot(plots_list, nrows = r, shareX = TRUE, heights = h, titleY=TRUE) %>%
            layout(title = paste("Sorties -", racine_nom), hovermode = "closest", legend = list(title = list(text = "")))
          saveWidget(partial_bundle(p_qs), file = p_qs_out, selfcontained = TRUE, libdir = "lib")
        }
        
        if (any(grepl("CULVERT", df_long$name)) && !file.exists(p_qoh_out)) {
          g_qoh <- ggplot(df_long %>% filter(grepl("CULVERT", name)), aes(Time, value, color = name)) + 
            geom_line() + theme_bw() + ylab("Débit [m3/s]") + xlab("Date (heure météo)") + theme(legend.title = element_blank())
          
          plots_list <- list()
          has_rain <- !is.null(rain_objs) && !is.null(rain_objs$g3) && !inherits(rain_objs$g3, "plotly_empty")
          if (has_rain) {
            plots_list[[1]] <- rain_objs$g3
            plots_list[[2]] <- rain_objs$g1
            plots_list[[3]] <- ggplotly(g_qoh)
            h <- c(0.1, 0.2, 0.7); r <- 3
          } else {
            plots_list[[1]] <- ggplotly(g_qoh)
            h <- c(1); r <- 1
          }
          p_qoh <- subplot(plots_list, nrows = r, shareX = TRUE, heights = h, titleY=TRUE) %>%
            layout(title = paste("Ouvrages -", racine_nom), hovermode = "closest", legend = list(title = list(text = "")))
          saveWidget(partial_bundle(p_qoh), file = p_qoh_out, selfcontained = TRUE, libdir = "lib")
        }
      }
    }
    
    message("  -> [GPKG] Qmax")
    tabmaxq <- tab_sc %>% group_by(nom) %>% filter(abs(Q) == max(abs(Q))) %>% slice(1)
    new_sc_sf <- sc_sf %>% 
      left_join(tabmaxq %>% dplyr::select(nom, Q, DATE, Zmoy, Hmax), by = c("ID" = "nom")) %>%
      mutate(Q = abs(Q), NomSecteur = basename(dsnlayerC))
    st_write(new_sc_sf, file.path(dsnlayerC, paste0(gsub("\\..*", "", f_sc), "_Qmax.gpkg")), driver = "GPKG", delete_dsn = TRUE, quiet = TRUE)
    file.copy(file.path(chem_routine, "C2D", Doss_Qml, "SC_Qmax.qml"), file.path(dsnlayerC, paste0(gsub("\\..*", "", f_sc), "_Qmax.qml")), overwrite = TRUE)
  }
  
  if (dir.exists(file.path(dsnlayerC, "lib"))) unlink(file.path(dsnlayerC, "lib"), recursive = TRUE)
  message("[FIN] Terminé.")
}
plot_sc_obs_oh <- function() {
  library(tidyverse)
  library(sf)
  library(plotly)
  library(htmlwidgets)
  library(tools)
  
  # --- 1. FONCTIONS UTILITAIRES ---
  
  get_rain_step <- function(filename, is_evt = TRUE) {
    # 1. Recherche explicite
    val <- as.numeric(gsub(".*[[:punct:]](\\d+)min([[:punct:]]|$).*", "\\1", filename))
    
    # 2. Sécurité si échec
    if (is.na(val) || val == 0 || val > 1440) { 
      parts <- strsplit(filename, "_")[[1]]
      mins <- grep("^\\d+min$", parts, value = TRUE)
      if (length(mins) > 0) {
        val <- as.numeric(gsub("min", "", mins[1]))
      } else {
        # 2. Règle SHYREG / Non-Evt : Défaut à 15 min
        val <- if (!is_evt || grepl("shyreg|spatial", filename, ignore.case = TRUE)) 15 else 1 
      }
    }
    return(if(is.na(val) || val == 0) 1 else val)
  }
  
  parse_time_meta <- function(filename) {
    parts <- strsplit(filename, "_")[[1]]
    is_evt <- grepl("Evt", filename, fixed = TRUE)
    duree <- 0
    date_deb <- as.POSIXct("1900-01-01", tz="UTC")
    
    if (is_evt) {
      if (length(parts) >= 5) {
        date_deb <- strptime(gsub("[^0-9]", "", parts[4]), format = "%Y%m%d%H%M", tz = "UTC")
        h_m <- strsplit(parts[5], "h")[[1]]
        if(length(h_m) >= 2) duree <- (as.numeric(h_m[1]) + as.numeric(substr(h_m[2], 1, 2)) / 60) * 3600
      }
    } else {
      if (length(parts) >= 8) {
        duree <- (as.numeric(parts[7]) + as.numeric(parts[8]) / 60) * 3600
      }
    }
    list(date_deb = as.POSIXct(date_deb), duree = duree, is_evt = is_evt)
  }
  
  parse_telemac_sortie <- function(file_path) {
    message(paste("    [DEBUG]", Sys.time(), "- Lecture sortie:", basename(file_path)))
    if (!file.exists(file_path)) return(NULL)
    
    # --- A. Récupération du TIME STEP (Strategie .cas puis .sortie) ---
    past_ts <- NA
    dir_path <- dirname(file_path)
    base_name <- basename(file_path)
    # Regex pour enlever _YYYY-MM-DD-HHhMMminSSs.sortie
    cas_name_candidate <- sub("_\\d{4}-\\d{2}-\\d{2}-\\d{2}h\\d{2}min\\d{2}s\\.sortie$", "", base_name)
    
    if (cas_name_candidate == base_name) {
      cas_name_candidate <- substr(base_name, 1, nchar(base_name) - 30)
    }
    
    cas_files <- list.files(dir_path, pattern = paste0("^", create_regex_safe(cas_name_candidate)), full.names = TRUE)
    cas_files <- cas_files[!grepl("\\.sortie$", cas_files)]
    
    if (length(cas_files) > 0) {
      lines_cas <- readLines(cas_files[1], warn = FALSE)
      ts_idx_cas <- grep("TIME STEP|PAS DE TEMPS", lines_cas, ignore.case = TRUE)[1]
      if (!is.na(ts_idx_cas)) {
        raw_line <- lines_cas[ts_idx_cas]
        extracted_ts <- gsub(".*(?:STEP|TEMPS)[^0-9.]*([0-9.]+).*", "\\1", raw_line, ignore.case = TRUE)
        past_ts <- as.numeric(extracted_ts)
        if(!is.na(past_ts)) message(paste("    [INFO] TIME STEP lu dans .cas :", past_ts))
      }
    }
    
    lines <- readLines(file_path, warn = FALSE)
    
    if (is.na(past_ts)) {
      ts_idx <- grep("TIME STEP|PAS DE TEMPS", lines, useBytes = TRUE, ignore.case = TRUE)[1]
      if (!is.na(ts_idx)) {
        raw_line <- lines[ts_idx]
        extracted_ts <- gsub(".*(?:STEP|TEMPS)[^0-9.]*([0-9.]+).*", "\\1", raw_line, ignore.case = TRUE)
        past_ts <- as.numeric(extracted_ts)
        if(!is.na(past_ts)) message(paste("    [INFO] TIME STEP lu dans .sortie :", past_ts))
      }
    }
    
    if (is.na(past_ts)) {
      message("    [WARN] TIME STEP introuvable. Valeur par défaut 1.0 utilisée.")
      past_ts <- 1.0
    }
    
    # --- B. Parsing des données ---
    idx_interest <- grep("ITERATION|FLUX BOUNDARY|CULVERT|RUNOFF_SCS_CN|VOLUME IN THE DOMAIN|ADDITIONAL VOLUME", lines, useBytes = TRUE)
    if (length(idx_interest) == 0) return(NULL)
    ls <- lines[idx_interest]
    
    clean_numeric <- function(txt) {
      txt <- gsub("[*]", "", txt)
      as.numeric(txt)
    }
    
    mots_cles <- tribble(
      ~key, ~start, ~end,
      "ITERATION ", 12, 22,
      "RUNOFF_SCS_CN : ACCUMULATED RAINFALL", 44, 61,
      "VOLUME IN THE DOMAIN", 28, 44,
      "ADDITIONAL VOLUME DUE TO SOURCE TERMS", 44, 61
    )
    
    # Identification propre des noms de colonnes (Regex strict : NOM + ESPACES + CHIFFRES)
    flux_raw <- grep("FLUX BOUNDARY", ls, value = TRUE, useBytes = TRUE)
    flux_keys <- unique(sub(".*(FLUX BOUNDARY\\s+\\d+).*", "\\1", flux_raw))
    
    cul_raw <- grep("CULVERT", ls, value = TRUE, useBytes = TRUE)
    culvert_keys <- unique(sub(".*(CULVERT\\s+\\d+).*", "\\1", cul_raw))
    
    extra <- tibble(key = c(flux_keys, culvert_keys)) %>%
      mutate(start = if_else(grepl("CULVERT", key), 37, 25), end = if_else(grepl("CULVERT", key), 62, 42))
    
    all_keys <- bind_rows(mots_cles, extra)
    ref_idx <- grep("ITERATION ", ls, useBytes = TRUE)
    res <- matrix(NA, nrow = length(ref_idx), ncol = nrow(all_keys))
    
    for (i in 1:nrow(mots_cles)) {
      m <- grep(mots_cles$key[i], ls, fixed = TRUE, useBytes = TRUE)
      if (length(m) > 0) {
        vals <- clean_numeric(substr(ls[m], mots_cles$start[i], mots_cles$end[i]))
        indices <- findInterval(m, ref_idx)
        valid <- indices > 0 & indices <= nrow(res)
        col_idx <- match(mots_cles$key[i], all_keys$key)
        res[indices[valid], col_idx] <- vals[valid]
      }
    }
    
    if (length(flux_keys) > 0) {
      m_flux <- grep("FLUX BOUNDARY", ls, fixed = TRUE, useBytes = TRUE)
      if (length(m_flux) > 0) {
        txt_flux <- ls[m_flux]
        vals_flux <- clean_numeric(substr(txt_flux, 25, 42))
        
        # Extraction propre du nom pour le matching
        keys_flux <- sub(".*(FLUX BOUNDARY\\s+\\d+).*", "\\1", txt_flux)
        
        rows <- findInterval(m_flux, ref_idx)
        cols <- match(keys_flux, all_keys$key)
        valid <- !is.na(cols) & rows > 0 & rows <= nrow(res)
        res[cbind(rows[valid], cols[valid])] <- vals_flux[valid]
      }
    }
    
    if (length(culvert_keys) > 0) {
      m_cul <- grep("CULVERT", ls, fixed = TRUE, useBytes = TRUE)
      if (length(m_cul) > 0) {
        txt_cul <- ls[m_cul]
        vals_cul <- clean_numeric(substr(txt_cul, 37, 62))
        
        # Extraction propre du nom pour le matching
        keys_cul <- sub(".*(CULVERT\\s+\\d+).*", "\\1", txt_cul)
        
        rows <- findInterval(m_cul, ref_idx)
        cols <- match(keys_cul, all_keys$key)
        valid <- !is.na(cols) & rows > 0 & rows <= nrow(res)
        res[cbind(rows[valid], cols[valid])] <- vals_cul[valid]
      }
    }
    
    df <- as.data.frame(res)
    colnames(df) <- all_keys$key
    df$Past_Ts <- past_ts
    df <- df[!is.na(df[, 1]), ] 
    
    message(paste("    [DEBUG] Parsing fini. Lignes:", nrow(df)))
    gc() 
    return(df)
  }
  
  create_regex_safe <- function(str) {
    gsub("([.|()\\^{}+$*?]|\\[|\\])", "\\\\\\1", str)
  }
  
  # --- 2. INITIALISATION ---
  message(paste("[INFO]", Sys.time(), "Start plot_sc_obs_oh"))
  sc_list <- list.files(dsnlayerC, pattern = "SC.txt")
  list_sortie <- list.files(dsnlayerC, pattern = "s.sortie")
  list_sortie <- list_sortie[!grepl("\\.txt$", list_sortie)]
  
  if (length(sc_list) == 0) return(message("Aucun SC.txt trouvé."))
  
  path_shp <- file.path(dsnlayerC, "SectControl_Select.shp")
  sc_sf <- st_read(path_shp, quiet = TRUE) %>% mutate(ID = as.character(ID))
  contour_aire <- st_read(file.path(dsnlayerC, "Step_Cartino2d_1_Contour.shp"), quiet = TRUE)
  aire_val <- as.numeric(st_area(contour_aire))
  dsnscobs <- file.path(dsnlayer, "_SectionsControles", contour$NOMPOST)
  
  cache_sortie <- list()      
  cache_rain_plots <- list()  
  
  # --- 3. BOUCLE PRINCIPALE ---
  
  for (f_sc in sc_list) {
    message(paste("----------------------------------------------------------------"))
    message(paste("[STEP] Traitement :", f_sc))
    
    # 1. Racine pour le matching interne (sans SC)
    racine_match <- sub("SC\\.txt$", "", f_sc)
    
    # 2. Racine pour le nommage des fichiers SC (avec SC s'il y est, sans extension)
    racine_nom <- tools::file_path_sans_ext(f_sc)
    
    # Définition précoce des chemins de sortie avec la bonne racine
    p_q_out <- file.path(dsnlayerC, paste0(racine_nom, "_PN_PB_Q_Obs.html"))
    p_z_out <- file.path(dsnlayerC, paste0(racine_nom, "_PN_PB_Z_Obs.html"))
    p_h_out <- file.path(dsnlayerC, paste0(racine_nom, "_PN_PB_H_Obs.html"))
    
    if (file.exists(p_q_out) && file.exists(p_z_out)) {
      message("  -> [SKIP] Fichiers existants.")
      next
    }
    
    meta <- parse_time_meta(f_sc)
    f_out_match <- list_sortie[grep(racine_match, list_sortie)]
    
    if (length(f_out_match) == 0) {
      message("  -> [WARN] .sortie introuvable.")
      tab_out <- NULL
    } else {
      if (length(f_out_match) > 1) {
        dates_out <- sub(".*\\.cas_(\\d{4}-\\d{2}-\\d{2}-\\d{2}h\\d{2}min\\d{2}s)\\.sortie", "\\1", f_out_match)
        f_out_match <- f_out_match[which.max(as.POSIXct(dates_out, format = "%Y-%m-%d-%Hh%Mmin%Ss"))]
      }
      if (is.null(cache_sortie[[f_out_match]])) {
        cache_sortie[[f_out_match]] <- parse_telemac_sortie(file.path(dsnlayerC, f_out_match))
      }
      tab_out <- cache_sortie[[f_out_match]]
    }
    
    rain_objs <- NULL 
    
    if (!is.null(tab_out)) {
      dir_pluie <- tryCatch(file.path(dsnPluie, contour$NOMPOST), error = function(e) NULL)
      file_brute <- NA
      
      if (!is.null(dir_pluie) && dir.exists(dir_pluie)) {
        all_brutes <- list.files(dir_pluie, pattern = "^(brute)_.*\\.txt$", full.names = TRUE)
        if (length(all_brutes) > 0) {
          clean_sc <- sub("SC\\.txt$", "", f_sc)
          parts_sc <- strsplit(clean_sc, "_")[[1]]
          
          dur_token <- grep("^\\d+min$", parts_sc, value = TRUE)
          if(length(dur_token) == 0) dur_token <- "IGNORE_ME" else dur_token <- dur_token[1]
          
          evt_token <- grep("^Evts\\d+", parts_sc, value = TRUE)
          if(length(evt_token) == 0) evt_token <- "IGNORE_ME" else evt_token <- evt_token[1]
          
          candidates <- all_brutes
          if (dur_token != "IGNORE_ME") {
            has_dur <- grepl(paste0("_", dur_token, "([_.]|$)"), basename(candidates)) | grepl(dur_token, basename(candidates), fixed = TRUE)
            if (any(has_dur)) candidates <- candidates[has_dur]
          }
          if (evt_token != "IGNORE_ME") {
            has_evt <- grepl(evt_token, basename(candidates))
            if (any(has_evt)) candidates <- candidates[has_evt]
          }
          if (length(candidates) > 0) {
            scores <- sapply(candidates, function(cand) {
              parts_cand <- strsplit(basename(cand), "_")[[1]]
              sum(parts_cand %in% parts_sc)
            })
            file_brute <- candidates[which.max(scores)]
            message(paste("    -> [BRUTE] Trouvé:", basename(file_brute)))
          }
        }
      }
      
      cache_key_rain <- paste0(f_out_match, ifelse(!is.na(file_brute), basename(file_brute), "NONE"))
      
      if (is.null(cache_rain_plots[[cache_key_rain]])) {
        past_ts <- tab_out$Past_Ts[1]
        if (is.na(past_ts)) past_ts <- 1.0
        
        if (nrow(tab_out) > 1) {
          pas_exp <- median(diff(tab_out[[1]]), na.rm = TRUE) * past_ts
        } else {
          pas_exp <- past_ts
        }
        if (is.na(pas_exp) || pas_exp <= 0) pas_exp <- 1.0
        message(paste("    -> [INFO] Pas d'export:", pas_exp, "s"))
        
        t_axis <- meta$date_deb + (tab_out[[1]] * past_ts)
        
        # --- CALCUL PLUIE DEFAUT (TELEMAC) AVEC DECUMULATION ---
        acc_rain <- replace_na(tab_out[[2]], 0)
        inc_rain <- c(acc_rain[1], diff(acc_rain))
        inc_rain[inc_rain < 0] <- 0 
        
        df_pluie <- data.frame(
          Time = t_axis,
          Intensite = inc_rain * 1000 * 3600 / pas_exp, 
          PNette = replace_na(tab_out[[4]], 0) / aire_val * 1000 * 3600 / past_ts,
          CumulBrut = replace_na(tab_out[[2]], 0) * 1000, 
          CumulNet = cumsum(replace_na(tab_out[[4]], 0) / aire_val * pas_exp / past_ts) * 1000,
          NomBrut = "Pluie brute", NomNet = "Pluie nette"
        )
        bar_shift=0
        # --- CORRECTION PLUIE BRUTE (MAPPING DIRECT) ---
        if (!is.na(file_brute) && file.exists(file_brute)) {
          tab_brute <- read.table(file_brute, header = TRUE)
          rain_step <- get_rain_step(basename(file_brute), is_evt = meta$is_evt)
          message(paste("    -> [BRUTE] Step:", rain_step, "min"))
          bar_shift <- pas_exp 
          col_val <- if("Val" %in% names(tab_brute)) "Val" else names(tab_brute)[2]
          
          n_out <- nrow(df_pluie)
          if (nrow(tab_brute) >= n_out) {
            raw_vals <- tab_brute[[col_val]][1:n_out]
          } else {
            raw_vals <- c(tab_brute[[col_val]], rep(0, n_out - nrow(tab_brute)))
          }
          
          df_pluie$CumulBrut <- cumsum(raw_vals / rain_step)
          df_pluie$Intensite <- raw_vals * 3600 / (pas_exp * rain_step)
        }
        
        df_pluie <- df_pluie %>% filter(!is.na(Time))
        
        # if (nrow(df_pluie) > 2000) {
        #   idx_keep <- unique(round(seq(1, nrow(df_pluie), length.out = 2000)))
        #   df_pluie <- df_pluie[idx_keep, ]
        # }
        
        if (nrow(df_pluie) > 0) {
          cols_hyeto <- c("Pluie brute" = "#33FFFF", "Pluie nette" = "#9933FF")
          cols_cumul <- c("Cumul brut" = "#33FFFF", "Cumul net" = "#9933FF")
          # Fix: x = Time - bar_shift. Si bar_shift=0 (défaut), centré sur Time. Sinon décalé.
          g1 <- ggplotly(ggplot(df_pluie) +
                           geom_col(aes(x = Time + bar_shift, y = Intensite, fill = "Pluie brute"), position = "identity",width = pas_exp ) +
                           geom_col(aes(x = Time, y = PNette, fill = "Pluie nette"), position = "identity",width = pas_exp) +
                           scale_fill_manual(name="", values = cols_hyeto) + 
                           theme_bw() + ylab("Intensité [mm/h]") + 
                           theme(axis.title.x = element_blank(), legend.title = element_blank()))
          
          g3 <- ggplotly(ggplot(df_pluie) +
                           geom_line(aes(x = Time, y = CumulBrut, color = "Cumul brut"), size = 1.1) +
                           geom_line(aes(x = Time, y = CumulNet, color = "Cumul net"), size = 1.1) +
                           scale_color_manual(name="", values = cols_cumul) + 
                           theme_bw() + ylab("Cumul [mm]") + 
                           theme(axis.title.x = element_blank(), legend.title = element_blank()))
          
          cache_rain_plots[[cache_key_rain]] <- list(g1 = g1, g3 = g3, t_axis = t_axis)
        } else {
          cache_rain_plots[[cache_key_rain]] <- NULL
        }
      }
      rain_objs <- cache_rain_plots[[cache_key_rain]]
    } else {
      rain_objs <- NULL
    }
    
    if (is.null(rain_objs)) {
      message("    [WARN] Données pluie invalides ou manquantes.")
      rain_objs <- list(g1 = NULL, g3 = NULL, t_axis = NULL)
    }
    
    message("  -> [DATA] SC + Obs")
    cl_names <- c("nom", "numsec", "Time", "Q", "Zmoy", "Zmax", "Zmin", "Hmax", "Htot")
    tab_sc <- read.csv(file.path(dsnlayerC, f_sc), header = FALSE, sep = "", skip = 2, col.names = cl_names)
    tab_sc <- tab_sc %>% filter(as.numeric(Time) >= 60)
    tab_sc[, 2:9] <- lapply(tab_sc[, 2:9], as.numeric)
    tab_sc$DATE <- meta$date_deb + tab_sc$Time
    tab_sc$nom <- as.character(tab_sc$nom)
    tab_sc <- inner_join(tab_sc, sc_sf, by = c("nom" = "ID"))
    
    sc_names <- unique(tab_sc$NOM)
    sc_list_obs <- list.files(path = dsnscobs, full.names = TRUE)
    
    load_obs <- function(suffix, col_name) {
      files_match <- unlist(lapply(sc_names, function(x) grep(paste0(x, suffix), sc_list_obs, value = TRUE)))
      if (length(files_match) == 0) return(NULL)
      df_list <- lapply(files_match, read.csv)
      df_obs <- do.call(rbind, df_list)
      df_obs$Date <- as.POSIXct(ifelse(grepl(" ", df_obs$Date), df_obs$Date, paste(df_obs$Date, "00:00:00")), tz = "UTC")
      df_obs <- df_obs %>% filter(Date >= min(tab_sc$DATE) & Date <= max(tab_sc$DATE))
      if (nrow(df_obs) > 0) {
        colnames(df_obs) <- c("Date", col_name, "NOM")
        df_obs$NOM <- paste0(df_obs$NOM, "_obs")
        return(df_obs)
      }
      return(NULL)
    }
    
    obs_q <- load_obs("_Q.csv", "Q")
    df_q_all <- bind_rows(tab_sc %>% dplyr::select(Date = DATE, Q, NOM) %>% mutate(Q = abs(Q), TYPE = "solid"),
                          if (!is.null(obs_q)) obs_q %>% mutate(Q = abs(Q), TYPE = "dashed") else NULL)
    
    obs_z <- load_obs("_Z.csv", "Z")
    df_z_all <- bind_rows(tab_sc %>% dplyr::select(Date = DATE, Z = Zmoy, NOM) %>% mutate(Z = ifelse(Z == 0, NA, Z), TYPE = "solid"),
                          if (!is.null(obs_z)) obs_z %>% mutate(TYPE = "dashed") else NULL)
    
    obs_h <- load_obs("_H.csv", "H")
    df_h_all <- bind_rows(tab_sc %>% dplyr::select(Date = DATE, H = Hmax, NOM) %>% mutate(TYPE = "solid"),
                          if (!is.null(obs_h)) obs_h %>% mutate(TYPE = "dashed") else NULL)
    
    save_master_plot <- function(df_data, y_col, y_lab, output_path, title_suffix) {
      if (file.exists(output_path)) return()
      df_data <- df_data %>% filter(!is.na(Date)) %>% arrange(NOM, Date)
      if (nrow(df_data) == 0) return()
      
      df_data <- df_data %>% group_by(NOM) %>%
        filter(if(n() > 2000) row_number() %in% unique(round(seq(1, n(), length.out = 2000))) else TRUE) %>% ungroup()
      
      g_main <- ggplot()
      
      df_solid <- df_data %>% filter(TYPE == "solid")
      if(nrow(df_solid) > 0) {
        g_main <- g_main + geom_line(data = df_solid, aes(x = Date, y = .data[[y_col]], color = NOM), linetype = "solid", size = 0.7)
      }
      
      df_dashed <- df_data %>% filter(TYPE != "solid")
      if(nrow(df_dashed) > 0) {
        g_main <- g_main + geom_line(data = df_dashed, aes(x = Date, y = .data[[y_col]], color = NOM), linetype = "dashed", size = 0.7)
      }
      
      g_main <- g_main + theme_bw() + ylab(y_lab) + xlab("Date (heure météo)") + theme(legend.title = element_blank())
      
      titre <- paste0("Hyéto + ", title_suffix, " ", racine_nom, " (Pluie sur tout le bassin)")
      
      plots_list <- list()
      heights <- c()
      rows_val <- 0
      
      has_rain <- !is.null(rain_objs) && !is.null(rain_objs$g3) && !inherits(rain_objs$g3, "plotly_empty")
      
      if (has_rain) {
        plots_list[[1]] <- rain_objs$g3
        plots_list[[2]] <- rain_objs$g1
        plots_list[[3]] <- ggplotly(g_main)
        heights <- c(0.1, 0.2, 0.7)
        rows_val <- 3
      } else {
        plots_list[[1]] <- ggplotly(g_main)
        heights <- c(1)
        rows_val <- 1
      }
      
      p_final <- subplot(plots_list, nrows = rows_val, shareX = TRUE, heights = heights, titleY = TRUE) %>%
        layout(title = list(text = titre), hovermode = "closest", legend = list(title = list(text = "")))
      saveWidget(partial_bundle(p_final), file = output_path, selfcontained = TRUE, libdir = "lib")
    }
    
    message("  -> [EXPORT] HTML Q/Z/H")
    save_master_plot(df_q_all, "Q", "Débit [m3/s]", p_q_out, "Hydrogramme")
    save_master_plot(df_z_all, "Z", "Côte [m]", p_z_out, "Côtes")
    save_master_plot(df_h_all, "H", "Hauteur [m]", p_h_out, "Hauteur")
    
    # E. Flux et Ouvrages
    # On reconstruit le nom de sortie basé sur le fichier .sortie (sans date)
    # Ex: C0083_..._s.cas_... -> C0083_..._s
    name_case_clean <- sub("\\.cas_.*", "", basename(f_out_match))
    p_qs_out <- file.path(dsnlayerC, paste0(name_case_clean, "_PN_PB_QS_Obs.html"))
    p_qoh_out <- file.path(dsnlayerC, paste0(name_case_clean, "_PN_PB_QOH_Obs.html"))
    
    if (!is.null(tab_out) && !file.exists(p_qs_out)) {
      df_flux <- tab_out %>% dplyr::select(matches("FLUX BOUNDARY|CULVERT"))
      # Filtre: garde colonnes qui ont au moins une valeur non nulle
      keep_cols <- sapply(df_flux, function(x) any(x != 0 & !is.na(x)))
      df_flux <- df_flux[, keep_cols, drop=FALSE]
      
      if (ncol(df_flux) > 0) {
        t_flux <- meta$date_deb + (tab_out[[1]] * past_ts)
        df_long <- df_flux %>% mutate(Time = t_flux) %>% 
          pivot_longer(-Time) %>% filter(!is.na(Time)) # On garde les 0, on filtre juste les NA temps
        
        # if (nrow(df_long) > 2000) {
        #   idx_keep <- unique(round(seq(1, nrow(df_long), length.out = 2000)))
        #   df_long <- df_long[idx_keep, ]
        # }
        
        if (any(grepl("FLUX", df_long$name))) {
          g_qs <- ggplot(df_long %>% filter(grepl("FLUX", name)), aes(Time, abs(value), color = name)) + 
            geom_line() + theme_bw() + ylab("Débit [m3/s]") + xlab("Date (heure météo)") + theme(legend.title = element_blank())
          
          plots_list <- list()
          has_rain <- !is.null(rain_objs) && !is.null(rain_objs$g3) && !inherits(rain_objs$g3, "plotly_empty")
          if (has_rain) {
            plots_list[[1]] <- rain_objs$g3
            plots_list[[2]] <- rain_objs$g1
            plots_list[[3]] <- ggplotly(g_qs)
            h <- c(0.1, 0.2, 0.7); r <- 3
          } else {
            plots_list[[1]] <- ggplotly(g_qs)
            h <- c(1); r <- 1
          }
          p_qs <- subplot(plots_list, nrows = r, shareX = TRUE, heights = h, titleY=TRUE) %>%
            layout(title = paste("Sorties -", racine_nom), hovermode = "closest", legend = list(title = list(text = "")))
          saveWidget(partial_bundle(p_qs), file = p_qs_out, selfcontained = TRUE, libdir = "lib")
        }
        
        if (any(grepl("CULVERT", df_long$name)) && !file.exists(p_qoh_out)) {
          g_qoh <- ggplot(df_long %>% filter(grepl("CULVERT", name)), aes(Time, value, color = name)) + 
            geom_line() + theme_bw() + ylab("Débit [m3/s]") + xlab("Date (heure météo)") + theme(legend.title = element_blank())
          
          plots_list <- list()
          has_rain <- !is.null(rain_objs) && !is.null(rain_objs$g3) && !inherits(rain_objs$g3, "plotly_empty")
          if (has_rain) {
            plots_list[[1]] <- rain_objs$g3
            plots_list[[2]] <- rain_objs$g1
            plots_list[[3]] <- ggplotly(g_qoh)
            h <- c(0.1, 0.2, 0.7); r <- 3
          } else {
            plots_list[[1]] <- ggplotly(g_qoh)
            h <- c(1); r <- 1
          }
          p_qoh <- subplot(plots_list, nrows = r, shareX = TRUE, heights = h, titleY=TRUE) %>%
            layout(title = paste("Ouvrages -", racine_nom), hovermode = "closest", legend = list(title = list(text = "")))
          saveWidget(partial_bundle(p_qoh), file = p_qoh_out, selfcontained = TRUE, libdir = "lib")
        }
      }
    }
    
    message("  -> [GPKG] Qmax")
    tabmaxq <- tab_sc %>% group_by(nom) %>% filter(abs(Q) == max(abs(Q))) %>% slice(1)
    new_sc_sf <- sc_sf %>% 
      left_join(tabmaxq %>% dplyr::select(nom, Q, DATE, Zmoy, Hmax), by = c("ID" = "nom")) %>%
      mutate(Q = abs(Q), NomSecteur = basename(dsnlayerC))
    st_write(new_sc_sf, file.path(dsnlayerC, paste0(gsub("\\..*", "", f_sc), "_Qmax.gpkg")), driver = "GPKG", delete_dsn = TRUE, quiet = TRUE)
    file.copy(file.path(chem_routine, "C2D", Doss_Qml, "SC_Qmax.qml"), file.path(dsnlayerC, paste0(gsub("\\..*", "", f_sc), "_Qmax.qml")), overwrite = TRUE)
  }
  
  if (dir.exists(file.path(dsnlayerC, "lib"))) unlink(file.path(dsnlayerC, "lib"), recursive = TRUE)
  message("[FIN] Terminé.")
}
plot_sc_obs_oh <- function() {
  library(tidyverse)
  library(sf)
  library(plotly)
  library(htmlwidgets)
  library(tools)
  
  # --- 1. FONCTIONS UTILITAIRES ---
  
  get_rain_step <- function(filename, is_evt = TRUE) {
    val <- as.numeric(gsub(".*[[:punct:]](\\d+)min([[:punct:]]|$).*", "\\1", filename))
    if (is.na(val) || val == 0 || val > 1440) { 
      parts <- strsplit(filename, "_")[[1]]
      mins <- grep("^\\d+min$", parts, value = TRUE)
      if (length(mins) > 0) {
        val <- as.numeric(gsub("min", "", mins[1]))
      } else {
        val <- if (!is_evt || grepl("shyreg|spatial", filename, ignore.case = TRUE)) 15 else 1 
      }
    }
    return(if(is.na(val) || val == 0) 1 else val)
  }
  
  parse_time_meta <- function(filename) {
    parts <- strsplit(filename, "_")[[1]]
    is_evt <- grepl("Evt", filename, fixed = TRUE)
    duree <- 0
    date_deb <- as.POSIXct("1900-01-01", tz="UTC")
    
    if (is_evt) {
      if (length(parts) >= 5) {
        suppressWarnings({
          date_deb <- strptime(gsub("[^0-9]", "", parts[4]), format = "%Y%m%d%H%M", tz = "UTC")
          h_m <- strsplit(parts[5], "h")[[1]]
          if(length(h_m) >= 2) duree <- (as.numeric(h_m[1]) + as.numeric(substr(h_m[2], 1, 2)) / 60) * 3600
        })
      }
    } else {
      if (length(parts) >= 8) {
        duree <- (as.numeric(parts[7]) + as.numeric(parts[8]) / 60) * 3600
      }
    }
    list(date_deb = as.POSIXct(date_deb), duree = duree, is_evt = is_evt)
  }
  
  create_regex_safe <- function(str) {
    gsub("([.|()\\^{}+$*?]|\\[|\\])", "\\\\\\1", str)
  }
  
  parse_telemac_sortie <- function(file_path) {
    message(paste("    [DEBUG]", Sys.time(), "- Lecture sortie:", basename(file_path)))
    if (!file.exists(file_path)) return(NULL)
    
    past_ts <- NA
    dir_path <- dirname(file_path)
    base_name <- basename(file_path)
    cas_name_candidate <- sub("_\\d{4}-\\d{2}-\\d{2}-\\d{2}h\\d{2}min\\d{2}s\\.sortie$", "", base_name)
    
    if (cas_name_candidate == base_name) {
      cas_name_candidate <- substr(base_name, 1, nchar(base_name) - 30)
    }
    
    cas_files <- list.files(dir_path, pattern = paste0("^", create_regex_safe(cas_name_candidate)), full.names = TRUE)
    cas_files <- cas_files[!grepl("\\.sortie$", cas_files)]
    
    if (length(cas_files) > 0) {
      lines_cas <- readLines(cas_files[1], warn = FALSE)
      ts_idx_cas <- grep("TIME STEP|PAS DE TEMPS", lines_cas, ignore.case = TRUE)[1]
      if (!is.na(ts_idx_cas)) {
        raw_line <- lines_cas[ts_idx_cas]
        extracted_ts <- gsub(".*(?:STEP|TEMPS)[^0-9.]*([0-9.]+).*", "\\1", raw_line, ignore.case = TRUE)
        past_ts <- as.numeric(extracted_ts)
        if(!is.na(past_ts)) message(paste("    [INFO] TIME STEP lu dans .cas :", past_ts))
      }
    }
    
    lines <- readLines(file_path, warn = FALSE)
    
    if (is.na(past_ts)) {
      ts_idx <- grep("TIME STEP|PAS DE TEMPS", lines, useBytes = TRUE, ignore.case = TRUE)[1]
      if (!is.na(ts_idx)) {
        raw_line <- lines[ts_idx]
        extracted_ts <- gsub(".*(?:STEP|TEMPS)[^0-9.]*([0-9.]+).*", "\\1", raw_line, ignore.case = TRUE)
        past_ts <- as.numeric(extracted_ts)
        if(!is.na(past_ts)) message(paste("    [INFO] TIME STEP lu dans .sortie :", past_ts))
      }
    }
    
    if (is.na(past_ts)) {
      message("    [WARN] TIME STEP introuvable. Valeur par défaut 1.0 utilisée.")
      past_ts <- 1.0
    }
    
    idx_interest <- grep("ITERATION|FLUX BOUNDARY|CULVERT|RUNOFF_SCS_CN|VOLUME IN THE DOMAIN|ADDITIONAL VOLUME", lines, useBytes = TRUE)
    if (length(idx_interest) == 0) return(NULL)
    ls <- lines[idx_interest]
    
    clean_numeric <- function(txt) {
      txt <- gsub("[*]", "", txt)
      suppressWarnings(as.numeric(txt))
    }
    
    mots_cles <- tribble(
      ~key, ~start, ~end,
      "ITERATION ", 12, 22,
      "RUNOFF_SCS_CN : ACCUMULATED RAINFALL", 44, 61,
      "VOLUME IN THE DOMAIN", 28, 44,
      "ADDITIONAL VOLUME DUE TO SOURCE TERMS", 44, 61
    )
    
    flux_raw <- grep("FLUX BOUNDARY", ls, value = TRUE, useBytes = TRUE)
    flux_keys <- unique(sub("^\\s*(FLUX BOUNDARY\\s+\\d+).*", "\\1", flux_raw))
    
    cul_raw <- grep("CULVERT", ls, value = TRUE, useBytes = TRUE)
    culvert_keys <- unique(sub("^\\s*(CULVERT\\s+\\d+).*", "\\1", cul_raw))
    
    extra <- tibble(key = c(flux_keys, culvert_keys)) %>%
      mutate(start = if_else(grepl("CULVERT", key), 37, 25), end = if_else(grepl("CULVERT", key), 62, 42))
    
    all_keys <- bind_rows(mots_cles, extra)
    ref_idx <- grep("ITERATION ", ls, useBytes = TRUE)
    res <- matrix(NA, nrow = length(ref_idx), ncol = nrow(all_keys))
    colnames(res) <- all_keys$key
    
    for (i in 1:nrow(mots_cles)) {
      m <- grep(mots_cles$key[i], ls, fixed = TRUE, useBytes = TRUE)
      if (length(m) > 0) {
        vals <- clean_numeric(substr(ls[m], mots_cles$start[i], mots_cles$end[i]))
        indices <- findInterval(m, ref_idx)
        valid <- indices > 0 & indices <= nrow(res)
        res[indices[valid], mots_cles$key[i]] <- vals[valid]
      }
    }
    
    if (length(flux_keys) > 0) {
      m_flux <- grep("FLUX BOUNDARY", ls, fixed = TRUE, useBytes = TRUE)
      if (length(m_flux) > 0) {
        txt_flux <- ls[m_flux]
        vals_flux <- clean_numeric(substr(txt_flux, 25, 42))
        keys_flux <- sub("^\\s*(FLUX BOUNDARY\\s+\\d+).*", "\\1", txt_flux)
        
        rows <- findInterval(m_flux, ref_idx)
        cols <- match(keys_flux, colnames(res))
        valid <- !is.na(cols) & rows > 0 & rows <= nrow(res)
        res[cbind(rows[valid], cols[valid])] <- vals_flux[valid]
      }
    }
    
    if (length(culvert_keys) > 0) {
      m_cul <- grep("CULVERT", ls, fixed = TRUE, useBytes = TRUE)
      if (length(m_cul) > 0) {
        txt_cul <- ls[m_cul]
        vals_cul <- clean_numeric(substr(txt_cul, 37, 62))
        keys_cul <- sub("^\\s*(CULVERT\\s+\\d+).*", "\\1", txt_cul)
        
        rows <- findInterval(m_cul, ref_idx)
        cols <- match(keys_cul, colnames(res))
        valid <- !is.na(cols) & rows > 0 & rows <= nrow(res)
        res[cbind(rows[valid], cols[valid])] <- vals_cul[valid]
      }
    }
    
    df <- as.data.frame(res)
    df$Past_Ts <- past_ts
    df <- df[!is.na(df[, 1]), ] 
    
    message(paste("    [DEBUG] Parsing fini. Lignes:", nrow(df)))
    gc() 
    return(df)
  }
  
  # --- 2. INITIALISATION ---
  message(paste("[INFO]", Sys.time(), "Start plot_sc_obs_oh"))
  sc_list <- list.files(dsnlayerC, pattern = "SC.txt")
  list_sortie <- list.files(dsnlayerC, pattern = "s.sortie")
  list_sortie <- list_sortie[!grepl("\\.txt$", list_sortie)]
  
  if (length(sc_list) == 0) return(message("Aucun SC.txt trouvé."))
  
  path_shp <- file.path(dsnlayerC, "SectControl_Select.shp")
  sc_sf <- st_read(path_shp, quiet = TRUE) %>% mutate(ID = as.character(ID))
  contour_aire <- st_read(file.path(dsnlayerC, "Step_Cartino2d_1_Contour.shp"), quiet = TRUE)
  aire_val <- as.numeric(st_area(contour_aire))
  dsnscobs <- file.path(dsnlayer, "_SectionsControles", contour$NOMPOST)
  
  cache_sortie <- list()      
  cache_rain_plots <- list()  
  
  # --- 3. BOUCLE PRINCIPALE ---
  
  for (f_sc in sc_list) {
    message(paste("----------------------------------------------------------------"))
    message(paste("[STEP] Traitement :", f_sc))
    racine <- sub("SC.txt$", "", f_sc)
    racine_nom <- tools::file_path_sans_ext(f_sc)
    
    p_q_out <- file.path(dsnlayerC, paste0(racine_nom, "_PN_PB_Q_Obs.html"))
    p_z_out <- file.path(dsnlayerC, paste0(racine_nom, "_PN_PB_Z_Obs.html"))
    p_h_out <- file.path(dsnlayerC, paste0(racine_nom, "_PN_PB_H_Obs.html"))
    
    if (file.exists(p_q_out) && file.exists(p_z_out)) {
      message("  -> [SKIP] Fichiers existants.")
      next
    }
    
    meta <- parse_time_meta(f_sc)
    f_out_match <- list_sortie[grep(racine, list_sortie)]
    
    if (length(f_out_match) == 0) {
      message("  -> [WARN] .sortie introuvable.")
      tab_out <- NULL
    } else {
      if (length(f_out_match) > 1) {
        dates_out <- sub(".*\\.cas_(\\d{4}-\\d{2}-\\d{2}-\\d{2}h\\d{2}min\\d{2}s)\\.sortie", "\\1", f_out_match)
        f_out_match <- f_out_match[which.max(as.POSIXct(dates_out, format = "%Y-%m-%d-%Hh%Mmin%Ss"))]
      }
      if (is.null(cache_sortie[[f_out_match]])) {
        cache_sortie[[f_out_match]] <- parse_telemac_sortie(file.path(dsnlayerC, f_out_match))
      }
      tab_out <- cache_sortie[[f_out_match]]
    }
    
    rain_objs <- NULL 
    
    if (!is.null(tab_out)) {
      dir_pluie <- tryCatch(file.path(dsnPluie, contour$NOMPOST), error = function(e) NULL)
      file_brute <- NA
      
      if (!is.null(dir_pluie) && dir.exists(dir_pluie)) {
        all_brutes <- list.files(dir_pluie, pattern = "^(brute|shyreg|spatial)_.*\\.txt$", full.names = TRUE)
        if (length(all_brutes) > 0) {
          clean_sc <- sub("SC\\.txt$", "", f_sc)
          parts_sc <- strsplit(clean_sc, "_")[[1]]
          
          dur_token <- grep("^\\d+min$", parts_sc, value = TRUE)
          if(length(dur_token) == 0) dur_token <- "IGNORE_ME" else dur_token <- dur_token[1]
          
          evt_token <- grep("^Evts\\d+", parts_sc, value = TRUE)
          if(length(evt_token) == 0) evt_token <- "IGNORE_ME" else evt_token <- evt_token[1]
          
          candidates <- all_brutes
          if (dur_token != "IGNORE_ME") {
            has_dur <- grepl(paste0("_", dur_token, "([_.]|$)"), basename(candidates)) | grepl(dur_token, basename(candidates), fixed = TRUE)
            if (any(has_dur)) candidates <- candidates[has_dur]
          }
          if (evt_token != "IGNORE_ME") {
            has_evt <- grepl(evt_token, basename(candidates))
            if (any(has_evt)) candidates <- candidates[has_evt]
          }
          if (length(candidates) > 0) {
            scores <- sapply(candidates, function(cand) {
              parts_cand <- strsplit(basename(cand), "_")[[1]]
              sum(parts_cand %in% parts_sc)
            })
            file_brute <- candidates[which.max(scores)]
            message(paste("    -> [BRUTE] Trouvé:", basename(file_brute)))
          }
        }
      }
      
      cache_key_rain <- paste0(f_out_match, ifelse(!is.na(file_brute), basename(file_brute), "NONE"))
      
      if (is.null(cache_rain_plots[[cache_key_rain]])) {
        past_ts <- tab_out$Past_Ts[1]
        if (is.na(past_ts)) past_ts <- 1.0
        
        if (nrow(tab_out) > 1) {
          pas_exp <- median(diff(tab_out[[1]]), na.rm = TRUE) * past_ts
        } else {
          pas_exp <- past_ts
        }
        if (is.na(pas_exp) || pas_exp <= 0) pas_exp <- 1.0
        message(paste("    -> [INFO] Pas d'export:", pas_exp, "s"))
        
        t_axis <- meta$date_deb + (tab_out[[1]] * past_ts)
        
        acc_rain <- replace_na(tab_out[["RUNOFF_SCS_CN : ACCUMULATED RAINFALL"]], 0)
        inc_rain <- c(acc_rain[1], diff(acc_rain))
        inc_rain[inc_rain < 0] <- 0 
        
        df_pluie <- data.frame(
          Time = t_axis,
          Intensite = inc_rain * 1000 * 3600 / pas_exp, 
          PNette = replace_na(tab_out[["ADDITIONAL VOLUME DUE TO SOURCE TERMS"]], 0) / aire_val * 1000 * 3600 / past_ts,
          CumulBrut = replace_na(tab_out[["RUNOFF_SCS_CN : ACCUMULATED RAINFALL"]], 0) * 1000, 
          CumulNet = cumsum(replace_na(tab_out[["ADDITIONAL VOLUME DUE TO SOURCE TERMS"]], 0) / aire_val * pas_exp / past_ts) * 1000,
          NomBrut = "Pluie brute", NomNet = "Pluie nette"
        )
        
        if (!is.na(file_brute) && file.exists(file_brute)) {
          tab_brute <- read.table(file_brute, header = TRUE)
          rain_step <- get_rain_step(basename(file_brute), is_evt = meta$is_evt)
          message(paste("    -> [BRUTE] Step:", rain_step, "min"))
          
          col_val <- if("Val" %in% names(tab_brute)) "Val" else names(tab_brute)[2]
          
          n_out <- nrow(df_pluie)
          if (nrow(tab_brute) >= n_out) {
            raw_vals <- tab_brute[[col_val]][1:n_out]
          } else {
            raw_vals <- c(tab_brute[[col_val]], rep(0, n_out - nrow(tab_brute)))
          }
          
          df_pluie$CumulBrut <- cumsum(raw_vals / rain_step)
          df_pluie$Intensite <- raw_vals * 3600 / (pas_exp * rain_step)
        }
        
        df_pluie <- df_pluie %>% filter(!is.na(Time))
        
        if (nrow(df_pluie) > 0) {
          df_hyeto <- df_pluie %>% dplyr::select(Time, `Pluie brute` = Intensite, `Pluie nette` = PNette) %>%
            pivot_longer(cols = c("Pluie brute", "Pluie nette"), names_to = "Nom", values_to = "Val")
          df_hyeto$Nom <- factor(df_hyeto$Nom, levels = c("Pluie brute", "Pluie nette"))
          
          cols_hyeto <- c("Pluie brute" = "#33FFFF", "Pluie nette" = "#9933FF")
          
          # Fix: just=1 pour aligner la barre sur la droite (fin de l'intervalle)
          # Cela correspond visuellement à un recul d'un demi-pas si c'était centré
          g1 <- ggplotly(ggplot(df_hyeto, aes(x = Time, y = Val, fill = Nom)) +
                           geom_col(position = "identity", width = pas_exp, just = 1) +
                           scale_fill_manual(name="", values = cols_hyeto) + 
                           theme_bw() + ylab("Intensité [mm/h]") + 
                           theme(axis.title.x = element_blank(), legend.title = element_blank()), dynamicTicks = TRUE)
          
          df_cumul <- df_pluie %>% dplyr::select(Time, `Cumul brut` = CumulBrut, `Cumul net` = CumulNet) %>%
            pivot_longer(cols = c("Cumul brut", "Cumul net"), names_to = "Nom", values_to = "Val")
          cols_cumul <- c("Cumul brut" = "#33FFFF", "Cumul net" = "#9933FF")
          
          g3 <- ggplotly(ggplot(df_cumul, aes(x = Time, y = Val, color = Nom)) +
                           geom_line(size = 1.1) +
                           scale_color_manual(name="", values = cols_cumul) +
                           theme_bw() + ylab("Cumul [mm]") + 
                           theme(axis.title.x = element_blank(), legend.title = element_blank()), dynamicTicks = TRUE)
          
          cache_rain_plots[[cache_key_rain]] <- list(g1 = g1, g3 = g3, t_axis = t_axis)
        } else {
          cache_rain_plots[[cache_key_rain]] <- NULL
        }
      }
      rain_objs <- cache_rain_plots[[cache_key_rain]]
    } else {
      rain_objs <- NULL
    }
    
    if (is.null(rain_objs)) {
      message("    [WARN] Données pluie invalides ou manquantes.")
      rain_objs <- list(g1 = NULL, g3 = NULL, t_axis = NULL)
    }
    
    message("  -> [DATA] SC + Obs")
    cl_names <- c("nom", "numsec", "Time", "Q", "Zmoy", "Zmax", "Zmin", "Hmax", "Htot")
    tab_sc <- read.csv(file.path(dsnlayerC, f_sc), header = FALSE, sep = "", skip = 2, col.names = cl_names)
    tab_sc <- tab_sc %>% filter(as.numeric(Time) >= 60)
    tab_sc[, 2:9] <- lapply(tab_sc[, 2:9], as.numeric)
    tab_sc$DATE <- meta$date_deb + tab_sc$Time
    tab_sc$nom <- as.character(tab_sc$nom)
    tab_sc <- inner_join(tab_sc, sc_sf, by = c("nom" = "ID"))
    
    sc_names <- unique(tab_sc$NOM)
    sc_list_obs <- list.files(path = dsnscobs, full.names = TRUE)
    
    load_obs <- function(suffix, col_name) {
      files_match <- unlist(lapply(sc_names, function(x) grep(paste0(x, suffix), sc_list_obs, value = TRUE)))
      if (length(files_match) == 0) return(NULL)
      df_list <- lapply(files_match, read.csv)
      df_obs <- do.call(rbind, df_list)
      df_obs$Date <- as.POSIXct(ifelse(grepl(" ", df_obs$Date), df_obs$Date, paste(df_obs$Date, "00:00:00")), tz = "UTC")
      df_obs <- df_obs %>% filter(Date >= min(tab_sc$DATE) & Date <= max(tab_sc$DATE))
      if (nrow(df_obs) > 0) {
        colnames(df_obs) <- c("Date", col_name, "NOM")
        df_obs$NOM <- paste0(df_obs$NOM, "_obs") 
        df_obs$TYPE <- "dashed" 
        return(df_obs)
      }
      return(NULL)
    }
    
    obs_q <- load_obs("_Q.csv", "Q")
    df_q_all <- bind_rows(
      tab_sc %>% dplyr::select(Date = DATE, Q, NOM) %>% mutate(Q = abs(Q), TYPE = "solid"),
      if (!is.null(obs_q)) obs_q %>% mutate(Q = abs(Q), TYPE = "dashed") else NULL
    )
    
    obs_z <- load_obs("_Z.csv", "Z")
    df_z_all <- bind_rows(
      tab_sc %>% dplyr::select(Date = DATE, Z = Zmoy, NOM) %>% mutate(Z = ifelse(Z == 0, NA, Z), TYPE = "solid"),
      if (!is.null(obs_z)) obs_z %>% mutate(TYPE = "dashed") else NULL
    )
    
    obs_h <- load_obs("_H.csv", "H")
    df_h_all <- bind_rows(
      tab_sc %>% dplyr::select(Date = DATE, H = Hmax, NOM) %>% mutate(TYPE = "solid"),
      if (!is.null(obs_h)) obs_h %>% mutate(TYPE = "dashed") else NULL
    )
    
    save_master_plot <- function(df_data, y_col, y_lab, output_path, title_suffix) {
      if (file.exists(output_path)) return()
      df_data <- df_data %>% filter(!is.na(Date)) %>% arrange(NOM, Date)
      if (nrow(df_data) == 0) return()
      test_obs=df_data %>% filter(TYPE != "solid")
      if(length(test_obs$Date )!=0){
        g_main <- ggplot() +
          geom_line(data = df_data %>% filter(TYPE == "solid"), 
                    aes(x = Date, y = .data[[y_col]], color = NOM), 
                    linetype = "solid", size = 0.7) +
          geom_line(data = df_data %>% filter(TYPE != "solid"), 
                    aes(x = Date, y = .data[[y_col]], color = NOM), 
                    linetype = "dashed", size = 0.7) +
          theme_bw() + ylab(y_lab) + xlab("Date (heure météo)") + 
          theme(legend.title = element_blank())
        
      }else{
        g_main <- ggplot() +
          geom_line(data = df_data %>% filter(TYPE == "solid"), 
                    aes(x = Date, y = .data[[y_col]], color = NOM), 
                    linetype = "solid", size = 0.7) +
          theme_bw() + ylab(y_lab) + xlab("Date (heure météo)") + 
          theme(legend.title = element_blank())
      }
      
      
      titre <- paste0("Hyéto + ", title_suffix, " ", racine_nom, " (Pluie sur tout le bassin)")
      
      plots_list <- list()
      heights <- c()
      rows_val <- 0
      
      has_rain <- !is.null(rain_objs) && !is.null(rain_objs$g3) && !inherits(rain_objs$g3, "plotly_empty")
      
      if (has_rain) {
        plots_list[[1]] <- rain_objs$g3
        plots_list[[2]] <- rain_objs$g1
        plots_list[[3]] <- ggplotly(g_main, dynamicTicks = TRUE)
        heights <- c(0.1, 0.2, 0.7)
        rows_val <- 3
      } else {
        plots_list[[1]] <- ggplotly(g_main, dynamicTicks = TRUE)
        heights <- c(1)
        rows_val <- 1
      }
      
      p_final <- subplot(plots_list, nrows = rows_val, shareX = TRUE, heights = heights, titleY = TRUE) %>%
        layout(title = list(text = titre), hovermode = "closest", legend = list(title = list(text = "")))
      saveWidget(partial_bundle(p_final), file = output_path, selfcontained = TRUE, libdir = "lib")
    }
    
    message("  -> [EXPORT] HTML Q/Z/H")
    save_master_plot(df_q_all, "Q", "Débit [m3/s]", p_q_out, "Hydrogramme")
    save_master_plot(df_z_all, "Z", "Côte [m]", p_z_out, "Côtes")
    save_master_plot(df_h_all, "H", "Hauteur [m]", p_h_out, "Hauteur")
    
    # E. Flux et Ouvrages
    name_case_clean <- sub("\\.cas_.*", "", basename(f_out_match))
    p_qs_out <- file.path(dsnlayerC, paste0(name_case_clean, "_PN_PB_QS_Obs.html"))
    p_qoh_out <- file.path(dsnlayerC, paste0(name_case_clean, "_PN_PB_QOH_Obs.html"))
    p_vqs_out <- file.path(dsnlayerC, paste0(name_case_clean, "_PN_PB_VQS_Obs.html"))
    
    if (!is.null(tab_out)) {
      # 1. Ajout VQS (Volume)
      if (!file.exists(p_vqs_out)) {
        if (!exists("t_axis")) t_axis <- meta$date_deb + (tab_out[[1]] * past_ts)
        vol_domaine <- replace_na(tab_out[["VOLUME IN THE DOMAIN"]], 0)
        vol_domaine <- vol_domaine - vol_domaine[1]
        
        vol_injecte <- cumsum(replace_na(tab_out[["ADDITIONAL VOLUME DUE TO SOURCE TERMS"]], 0)) 
        
        df_flux_only <- tab_out %>% dplyr::select(matches("FLUX BOUNDARY"))
        vol_sorties_cumul <- rep(0, nrow(tab_out))
        if (ncol(df_flux_only) > 0) {
          # FIX: On garde toutes les colonnes, et on nettoie les NA
          df_flux_active <- df_flux_only %>% mutate(across(everything(), ~replace_na(., 0)))
          q_total <- rowSums(df_flux_active, na.rm=TRUE)
          vol_sorties_cumul <- cumsum(-q_total * pas_exp)
        }
        
        df_vol <- data.frame(
          Time = t_axis,
          Injecte = vol_injecte,
          Domaine = vol_domaine,
          `Domaine + Sorties` = vol_domaine + vol_sorties_cumul,
          check.names = FALSE
        ) %>% pivot_longer(-Time, names_to = "Variable", values_to = "Volume")
        
        # Ajout des volumes individuels des sorties (FIX: df_flux_active existe maintenant toujours)
        if (exists("df_flux_active") && ncol(df_flux_active) > 0) {
          for(col_name in colnames(df_flux_active)) {
            clean_name <- sub("FLUX BOUNDARY", "Sortie", col_name)
            vol_indiv <- cumsum(-df_flux_active[[col_name]] * pas_exp)
            df_temp <- data.frame(Time = t_axis, Variable = clean_name, Volume = vol_indiv)
            df_vol <- bind_rows(df_vol, df_temp)
          }
        }
        
        g_vol <- ggplot(df_vol, aes(x = Time, y = Volume, color = Variable)) +
          geom_line() + theme_bw() + labs(y = "Volume [m3]") + 
          theme(legend.title = element_blank(), legend.position="right")
        
        df_qs_total <- data.frame(Time = t_axis, Q = if(exists("q_total")) -q_total else 0)
        g_qs_total <- ggplot(df_qs_total, aes(x=Time, y=Q, color="Débit Sortie Total")) + 
          geom_line() + 
          scale_color_manual(values=c("Débit Sortie Total"="red")) +
          theme_bw() + ylab("Débit Sortie Total [m3/s]") +
          theme(legend.title = element_blank(), legend.position="right")
        
        plots_vqs <- list()
        if (!is.null(rain_objs) && !is.null(rain_objs$g3) && !inherits(rain_objs$g3, "plotly_empty")) {
          plots_vqs <- list(rain_objs$g3, ggplotly(g_vol, dynamicTicks = TRUE), ggplotly(g_qs_total, dynamicTicks = TRUE))
          p_vqs <- subplot(plots_vqs, nrows = 3, shareX = TRUE, titleY = TRUE) %>%
            layout(title = paste("Bilan Volume -", racine_nom), legend = list(title = list(text = ""), orientation = "v"))
        } else {
          plots_vqs <- list(ggplotly(g_vol, dynamicTicks = TRUE), ggplotly(g_qs_total, dynamicTicks = TRUE))
          p_vqs <- subplot(plots_vqs, nrows = 2, shareX = TRUE, titleY = TRUE) %>%
            layout(title = paste("Bilan Volume -", racine_nom), legend = list(title = list(text = ""), orientation = "v"))
        }
        saveWidget(partial_bundle(p_vqs), file = p_vqs_out, selfcontained = TRUE, libdir = "lib")
      }
      
      df_flux <- tab_out %>% dplyr::select(matches("FLUX BOUNDARY|CULVERT"))
      keep_cols <- sapply(df_flux, function(x) !all(is.na(x)))
      df_flux <- df_flux[, keep_cols, drop=FALSE]
      
      if (ncol(df_flux) > 0) {
        if (!exists("t_axis")) t_axis <- meta$date_deb + (tab_out[[1]] * past_ts)
        df_long <- df_flux %>% mutate(Time = t_axis) %>% 
          pivot_longer(-Time) %>% filter(!is.na(Time)) 
        
        if (!file.exists(p_qs_out) && any(grepl("FLUX", df_long$name))) {
          g_qs <- ggplot(df_long %>% filter(grepl("FLUX", name)), aes(Time, abs(value), color = name)) + 
            geom_line() + theme_bw() + ylab("Débit [m3/s]") + xlab("Date (heure météo)") + theme(legend.title = element_blank())
          
          plots_list <- list()
          has_rain <- !is.null(rain_objs) && !is.null(rain_objs$g3) && !inherits(rain_objs$g3, "plotly_empty")
          if (has_rain) {
            plots_list[[1]] <- rain_objs$g3
            plots_list[[2]] <- rain_objs$g1
            plots_list[[3]] <- ggplotly(g_qs, dynamicTicks = TRUE)
            h <- c(0.1, 0.2, 0.7); r <- 3
          } else {
            plots_list[[1]] <- ggplotly(g_qs, dynamicTicks = TRUE)
            h <- c(1); r <- 1
          }
          p_qs <- subplot(plots_list, nrows = r, shareX = TRUE, heights = h, titleY=TRUE) %>%
            layout(title = paste("Sorties -", racine_nom), hovermode = "closest", legend = list(title = list(text = "")))
          saveWidget(partial_bundle(p_qs), file = p_qs_out, selfcontained = TRUE, libdir = "lib")
        }
        
        if (!file.exists(p_qoh_out) && any(grepl("CULVERT", df_long$name))) {
          g_qoh <- ggplot(df_long %>% filter(grepl("CULVERT", name)), aes(Time, value, color = name)) + 
            geom_line() + theme_bw() + ylab("Débit [m3/s]") + xlab("Date (heure météo)") + theme(legend.title = element_blank())
          
          plots_list <- list()
          has_rain <- !is.null(rain_objs) && !is.null(rain_objs$g3) && !inherits(rain_objs$g3, "plotly_empty")
          if (has_rain) {
            plots_list[[1]] <- rain_objs$g3
            plots_list[[2]] <- rain_objs$g1
            plots_list[[3]] <- ggplotly(g_qoh, dynamicTicks = TRUE)
            h <- c(0.1, 0.2, 0.7); r <- 3
          } else {
            plots_list[[1]] <- ggplotly(g_qoh, dynamicTicks = TRUE)
            h <- c(1); r <- 1
          }
          p_qoh <- subplot(plots_list, nrows = r, shareX = TRUE, heights = h, titleY=TRUE) %>%
            layout(title = paste("Ouvrages -", racine_nom), hovermode = "closest", legend = list(title = list(text = "")))
          saveWidget(partial_bundle(p_qoh), file = p_qoh_out, selfcontained = TRUE, libdir = "lib")
        }
      }
    }
    
    message("  -> [GPKG] Qmax")
    tabmaxq <- tab_sc %>% group_by(nom) %>% filter(abs(Q) == max(abs(Q))) %>% slice(1)
    new_sc_sf <- sc_sf %>% 
      left_join(tabmaxq %>% dplyr::select(nom, Q, DATE, Zmoy, Hmax), by = c("ID" = "nom")) %>%
      mutate(Q = abs(Q), NomSecteur = basename(dsnlayerC))
    st_write(new_sc_sf, file.path(dsnlayerC, paste0(gsub("\\..*", "", f_sc), "_Qmax.gpkg")), driver = "GPKG", delete_dsn = TRUE, quiet = TRUE)
    file.copy(file.path(chem_routine, "C2D", Doss_Qml, "SC_Qmax.qml"), file.path(dsnlayerC, paste0(gsub("\\..*", "", f_sc), "_Qmax.qml")), overwrite = TRUE)
  }
  
  if (dir.exists(file.path(dsnlayerC, "lib"))) unlink(file.path(dsnlayerC, "lib"), recursive = TRUE)
  message("[FIN] Terminé.")
}
plot_sc_obs_oh <- function() {
  library(tidyverse)
  library(sf)
  library(plotly)
  library(htmlwidgets)
  library(tools)
  
  # --- 1. FONCTIONS UTILITAIRES ---
  
  get_rain_step <- function(filename, is_evt = TRUE) {
    val <- as.numeric(gsub(".*[[:punct:]](\\d+)min([[:punct:]]|$).*", "\\1", filename))
    if (is.na(val) || val == 0 || val > 1440) { 
      parts <- strsplit(filename, "_")[[1]]
      mins <- grep("^\\d+min$", parts, value = TRUE)
      if (length(mins) > 0) {
        val <- as.numeric(gsub("min", "", mins[1]))
      } else {
        val <- if (!is_evt || grepl("shyreg|spatial", filename, ignore.case = TRUE)) 15 else 1 
      }
    }
    return(if(is.na(val) || val == 0) 1 else val)
  }
  
  parse_time_meta <- function(filename) {
    parts <- strsplit(filename, "_")[[1]]
    is_evt <- grepl("Evt", filename, fixed = TRUE)
    duree <- 0
    date_deb <- as.POSIXct("1900-01-01", tz="UTC")
    
    if (is_evt) {
      if (length(parts) >= 5) {
        suppressWarnings({
          date_deb <- strptime(gsub("[^0-9]", "", parts[4]), format = "%Y%m%d%H%M", tz = "UTC")
          h_m <- strsplit(parts[5], "h")[[1]]
          if(length(h_m) >= 2) duree <- (as.numeric(h_m[1]) + as.numeric(substr(h_m[2], 1, 2)) / 60) * 3600
        })
      }
    } else {
      if (length(parts) >= 8) {
        duree <- (as.numeric(parts[7]) + as.numeric(parts[8]) / 60) * 3600
      }
    }
    list(date_deb = as.POSIXct(date_deb), duree = duree, is_evt = is_evt)
  }
  
  create_regex_safe <- function(str) {
    gsub("([.|()\\^{}+$*?]|\\[|\\])", "\\\\\\1", str)
  }
  
  parse_telemac_sortie <- function(file_path) {
    message(paste("    [DEBUG]", Sys.time(), "- Lecture sortie:", basename(file_path)))
    if (!file.exists(file_path)) return(NULL)
    
    past_ts <- NA
    dir_path <- dirname(file_path)
    base_name <- basename(file_path)
    cas_name_candidate <- sub("_\\d{4}-\\d{2}-\\d{2}-\\d{2}h\\d{2}min\\d{2}s\\.sortie$", "", base_name)
    
    if (cas_name_candidate == base_name) {
      cas_name_candidate <- substr(base_name, 1, nchar(base_name) - 30)
    }
    
    cas_files <- list.files(dir_path, pattern = paste0("^", create_regex_safe(cas_name_candidate)), full.names = TRUE)
    cas_files <- cas_files[!grepl("\\.sortie$", cas_files)]
    
    if (length(cas_files) > 0) {
      lines_cas <- readLines(cas_files[1], warn = FALSE)
      ts_idx_cas <- grep("TIME STEP|PAS DE TEMPS", lines_cas, ignore.case = TRUE)[1]
      if (!is.na(ts_idx_cas)) {
        raw_line <- lines_cas[ts_idx_cas]
        extracted_ts <- gsub(".*(?:STEP|TEMPS)[^0-9.]*([0-9.]+).*", "\\1", raw_line, ignore.case = TRUE)
        past_ts <- as.numeric(extracted_ts)
        if(!is.na(past_ts)) message(paste("    [INFO] TIME STEP lu dans .cas :", past_ts))
      }
    }
    
    lines <- readLines(file_path, warn = FALSE)
    
    if (is.na(past_ts)) {
      ts_idx <- grep("TIME STEP|PAS DE TEMPS", lines, useBytes = TRUE, ignore.case = TRUE)[1]
      if (!is.na(ts_idx)) {
        raw_line <- lines[ts_idx]
        extracted_ts <- gsub(".*(?:STEP|TEMPS)[^0-9.]*([0-9.]+).*", "\\1", raw_line, ignore.case = TRUE)
        past_ts <- as.numeric(extracted_ts)
        if(!is.na(past_ts)) message(paste("    [INFO] TIME STEP lu dans .sortie :", past_ts))
      }
    }
    
    if (is.na(past_ts)) {
      message("    [WARN] TIME STEP introuvable. Valeur par défaut 1.0 utilisée.")
      past_ts <- 1.0
    }
    
    idx_interest <- grep("ITERATION|FLUX BOUNDARY|CULVERT|RUNOFF_SCS_CN|VOLUME IN THE DOMAIN|ADDITIONAL VOLUME", lines, useBytes = TRUE)
    if (length(idx_interest) == 0) return(NULL)
    ls <- lines[idx_interest]
    
    clean_numeric <- function(txt) {
      txt <- gsub("[*]", "", txt)
      suppressWarnings(as.numeric(txt))
    }
    
    mots_cles <- tribble(
      ~key, ~start, ~end,
      "ITERATION ", 12, 22,
      "RUNOFF_SCS_CN : ACCUMULATED RAINFALL", 44, 61,
      "VOLUME IN THE DOMAIN", 28, 44,
      "ADDITIONAL VOLUME DUE TO SOURCE TERMS", 44, 61
    )
    
    flux_raw <- grep("FLUX BOUNDARY", ls, value = TRUE, useBytes = TRUE)
    flux_keys <- unique(sub("^\\s*(FLUX BOUNDARY\\s+\\d+).*", "\\1", flux_raw))
    
    cul_raw <- grep("CULVERT", ls, value = TRUE, useBytes = TRUE)
    culvert_keys <- unique(sub("^\\s*(CULVERT\\s+\\d+).*", "\\1", cul_raw))
    
    extra <- tibble(key = c(flux_keys, culvert_keys)) %>%
      mutate(start = if_else(grepl("CULVERT", key), 37, 25), end = if_else(grepl("CULVERT", key), 62, 42))
    
    all_keys <- bind_rows(mots_cles, extra)
    ref_idx <- grep("ITERATION ", ls, useBytes = TRUE)
    res <- matrix(NA, nrow = length(ref_idx), ncol = nrow(all_keys))
    colnames(res) <- all_keys$key
    
    for (i in 1:nrow(mots_cles)) {
      m <- grep(mots_cles$key[i], ls, fixed = TRUE, useBytes = TRUE)
      if (length(m) > 0) {
        vals <- clean_numeric(substr(ls[m], mots_cles$start[i], mots_cles$end[i]))
        indices <- findInterval(m, ref_idx)
        valid <- indices > 0 & indices <= nrow(res)
        res[indices[valid], mots_cles$key[i]] <- vals[valid]
      }
    }
    
    if (length(flux_keys) > 0) {
      m_flux <- grep("FLUX BOUNDARY", ls, fixed = TRUE, useBytes = TRUE)
      if (length(m_flux) > 0) {
        txt_flux <- ls[m_flux]
        vals_flux <- clean_numeric(substr(txt_flux, 25, 42))
        keys_flux <- sub("^\\s*(FLUX BOUNDARY\\s+\\d+).*", "\\1", txt_flux)
        
        rows <- findInterval(m_flux, ref_idx)
        cols <- match(keys_flux, colnames(res))
        valid <- !is.na(cols) & rows > 0 & rows <= nrow(res)
        res[cbind(rows[valid], cols[valid])] <- vals_flux[valid]
      }
    }
    
    if (length(culvert_keys) > 0) {
      m_cul <- grep("CULVERT", ls, fixed = TRUE, useBytes = TRUE)
      if (length(m_cul) > 0) {
        txt_cul <- ls[m_cul]
        vals_cul <- clean_numeric(substr(txt_cul, 37, 62))
        keys_cul <- sub("^\\s*(CULVERT\\s+\\d+).*", "\\1", txt_cul)
        
        rows <- findInterval(m_cul, ref_idx)
        cols <- match(keys_cul, colnames(res))
        valid <- !is.na(cols) & rows > 0 & rows <= nrow(res)
        res[cbind(rows[valid], cols[valid])] <- vals_cul[valid]
      }
    }
    
    df <- as.data.frame(res)
    df$Past_Ts <- past_ts
    df <- df[!is.na(df[, 1]), ] 
    
    message(paste("    [DEBUG] Parsing fini. Lignes:", nrow(df)))
    gc() 
    return(df)
  }
  
  # --- 2. INITIALISATION ---
  message(paste("[INFO]", Sys.time(), "Start plot_sc_obs_oh"))
  sc_list <- list.files(dsnlayerC, pattern = "SC.txt")
  list_sortie <- list.files(dsnlayerC, pattern = "s.sortie")
  list_sortie <- list_sortie[!grepl("\\.txt$", list_sortie)]
  
  if (length(sc_list) == 0) return(message("Aucun SC.txt trouvé."))
  
  path_shp <- file.path(dsnlayerC, "SectControl_Select.shp")
  sc_sf <- st_read(path_shp, quiet = TRUE) %>% mutate(ID = as.character(ID))
  contour_aire <- st_read(file.path(dsnlayerC, "Step_Cartino2d_1_Contour.shp"), quiet = TRUE)
  aire_val <- as.numeric(st_area(contour_aire))
  dsnscobs <- file.path(dsnlayer, "_SectionsControles", contour$NOMPOST)
  
  cache_sortie <- list()      
  cache_rain_plots <- list()  
  
  # --- 3. BOUCLE PRINCIPALE ---
  
  for (f_sc in sc_list) {
    message(paste("----------------------------------------------------------------"))
    message(paste("[STEP] Traitement :", f_sc))
    racine <- sub("SC.txt$", "", f_sc)
    racine_nom <- tools::file_path_sans_ext(f_sc)
    
    p_q_out <- file.path(dsnlayerC, paste0(racine_nom, "_PN_PB_Q_Obs.html"))
    p_z_out <- file.path(dsnlayerC, paste0(racine_nom, "_PN_PB_Z_Obs.html"))
    p_h_out <- file.path(dsnlayerC, paste0(racine_nom, "_PN_PB_H_Obs.html"))
    
    if (file.exists(p_q_out) && file.exists(p_z_out)) {
      message("  -> [SKIP] Fichiers existants.")
      next
    }
    
    meta <- parse_time_meta(f_sc)
    f_out_match <- list_sortie[grep(racine, list_sortie)]
    
    if (length(f_out_match) == 0) {
      message("  -> [WARN] .sortie introuvable.")
      tab_out <- NULL
    } else {
      if (length(f_out_match) > 1) {
        dates_out <- sub(".*\\.cas_(\\d{4}-\\d{2}-\\d{2}-\\d{2}h\\d{2}min\\d{2}s)\\.sortie", "\\1", f_out_match)
        f_out_match <- f_out_match[which.max(as.POSIXct(dates_out, format = "%Y-%m-%d-%Hh%Mmin%Ss"))]
      }
      if (is.null(cache_sortie[[f_out_match]])) {
        cache_sortie[[f_out_match]] <- parse_telemac_sortie(file.path(dsnlayerC, f_out_match))
      }
      tab_out <- cache_sortie[[f_out_match]]
    }
    
    rain_objs <- NULL 
    
    if (!is.null(tab_out)) {
      dir_pluie <- tryCatch(file.path(dsnPluie, contour$NOMPOST), error = function(e) NULL)
      file_brute <- NA
      
      if (!is.null(dir_pluie) && dir.exists(dir_pluie)) {
        all_brutes <- list.files(dir_pluie, pattern = "^(brute)_.*\\.txt$", full.names = TRUE)
        if (length(all_brutes) > 0) {
          name_sc_split <- strsplit(f_sc,"_")[[1]]
          name_sc_brute <- do.call(paste, c(as.list(name_sc_split[1:5]), sep = "_"))
          clean_sc <- sub("SC\\.txt$", "", name_sc_brute)
          parts_sc <- strsplit(clean_sc, "_")[[1]]
          
          dur_token <- grep("^\\d+min$", parts_sc, value = TRUE)
          if(length(dur_token) == 0) dur_token <- "IGNORE_ME" else dur_token <- dur_token[1]
          
          evt_token <- grep("^Evts\\d+", parts_sc, value = TRUE)
          if(length(evt_token) == 0) evt_token <- "IGNORE_ME" else evt_token <- evt_token[1]
          candidates <- all_brutes
          if(evt_token== "IGNORE_ME") candidates <- NULL
          
          if (dur_token != "IGNORE_ME") {
            has_dur <- grepl(paste0("_", dur_token, "([_.]|$)"), basename(candidates)) | grepl(dur_token, basename(candidates), fixed = TRUE)
            if (any(has_dur)) candidates <- candidates[has_dur]
          }
          if (evt_token != "IGNORE_ME") {
            has_evt <- grepl(evt_token, basename(candidates))
            if (any(has_evt)) candidates <- candidates[has_evt]
          }
          if (length(candidates) > 0) {
            scores <- sapply(candidates, function(cand) {
              parts_cand <- strsplit(basename(cand), "_")[[1]]
              sum(parts_cand %in% parts_sc)
            })
            file_brute <- candidates[which.max(scores)]
            message(paste("    -> [BRUTE] Trouvé:", basename(file_brute)))
          }
        }
      }
      
      cache_key_rain <- paste0(f_out_match, ifelse(!is.na(file_brute), basename(file_brute), "NONE"))
      
      if (is.null(cache_rain_plots[[cache_key_rain]])) {
        past_ts <- tab_out$Past_Ts[1]
        if (is.na(past_ts)) past_ts <- 1.0
        
        if (nrow(tab_out) > 1) {
          pas_exp <- median(diff(tab_out[[1]]), na.rm = TRUE) * past_ts
        } else {
          pas_exp <- past_ts
        }
        if (is.na(pas_exp) || pas_exp <= 0) pas_exp <- 1.0
        message(paste("    -> [INFO] Pas d'export:", pas_exp, "s"))
        
        t_axis <- meta$date_deb + (tab_out[[1]] * past_ts)
        
        acc_rain <- replace_na(tab_out[["RUNOFF_SCS_CN : ACCUMULATED RAINFALL"]], 0)
        inc_rain <- c(acc_rain[1], diff(acc_rain))
        inc_rain[inc_rain < 0] <- 0 
        
        df_pluie <- data.frame(
          Time = t_axis,
          Intensite = inc_rain * 1000 * 3600 / pas_exp, 
          PNette = replace_na(tab_out[["ADDITIONAL VOLUME DUE TO SOURCE TERMS"]], 0) / aire_val * 1000 * 3600 / past_ts,
          CumulBrut = replace_na(tab_out[["RUNOFF_SCS_CN : ACCUMULATED RAINFALL"]], 0) * 1000, 
          CumulNet = cumsum(replace_na(tab_out[["ADDITIONAL VOLUME DUE TO SOURCE TERMS"]], 0) / aire_val * pas_exp / past_ts) * 1000,
          NomBrut = "Pluie brute", NomNet = "Pluie nette"
        )
        bar_shift=0
        if (!is.na(file_brute) && file.exists(file_brute)) {
          tab_brute <- read.table(file_brute, header = TRUE)
          rain_step <- get_rain_step(basename(file_brute), is_evt = meta$is_evt)
          message(paste("    -> [BRUTE] Step:", rain_step, "min"))
          bar_shift <- pas_exp 
          col_val <- if("Val" %in% names(tab_brute)) "Val" else names(tab_brute)[2]
          
          n_out <- nrow(df_pluie)
          if (nrow(tab_brute) >= n_out) {
            raw_vals <- tab_brute[[col_val]][1:n_out]
          } else {
            raw_vals <- c(tab_brute[[col_val]], rep(0, n_out - nrow(tab_brute)))
          }
          
          df_pluie$CumulBrut <- cumsum(raw_vals / rain_step)
          df_pluie$Intensite <- raw_vals * 3600 / (pas_exp * rain_step)
        }
        
        df_pluie <- df_pluie %>% filter(!is.na(Time))
        
        if (nrow(df_pluie) > 0) {
          df_hyeto <- df_pluie %>% dplyr::select(Time, `Pluie brute` = Intensite, `Pluie nette` = PNette) %>%
            pivot_longer(cols = c("Pluie brute", "Pluie nette"), names_to = "Nom", values_to = "Val")
          df_hyeto$Nom <- factor(df_hyeto$Nom, levels = c("Pluie brute", "Pluie nette"))
          
          cols_hyeto <- c("Pluie brute" = "#33FFFF", "Pluie nette" = "#9933FF")
          df_hyeto$Time[df_hyeto$Nom=="Pluie brute"]=df_hyeto$Time[df_hyeto$Nom=="Pluie brute"]+bar_shift
          # Fix: just=1 pour aligner la barre sur la droite (fin de l'intervalle)
          # Cela correspond visuellement à un recul d'un demi-pas si c'était centré
          g1 <- ggplotly(ggplot(df_hyeto, aes(x = Time, y = Val, fill = Nom)) +
                           geom_col(position = "identity", width = pas_exp) +
                           scale_fill_manual(name="", values = cols_hyeto) + 
                           theme_bw() + ylab("Intensité [mm/h]") + 
                           theme(axis.title.x = element_blank(), legend.title = element_blank()), dynamicTicks = TRUE)
          
          df_cumul <- df_pluie %>% dplyr::select(Time, `Cumul brut` = CumulBrut, `Cumul net` = CumulNet) %>%
            pivot_longer(cols = c("Cumul brut", "Cumul net"), names_to = "Nom", values_to = "Val")
          cols_cumul <- c("Cumul brut" = "#33FFFF", "Cumul net" = "#9933FF")
          
          g3 <- ggplotly(ggplot(df_cumul, aes(x = Time, y = Val, color = Nom)) +
                           geom_line(size = 1.1) +
                           scale_color_manual(name="", values = cols_cumul) +
                           theme_bw() + ylab("Cumul [mm]") + 
                           theme(axis.title.x = element_blank(), legend.title = element_blank()), dynamicTicks = TRUE)
          
          cache_rain_plots[[cache_key_rain]] <- list(g1 = g1, g3 = g3, t_axis = t_axis)
        } else {
          cache_rain_plots[[cache_key_rain]] <- NULL
        }
      }
      rain_objs <- cache_rain_plots[[cache_key_rain]]
    } else {
      rain_objs <- NULL
    }
    
    if (is.null(rain_objs)) {
      message("    [WARN] Données pluie invalides ou manquantes.")
      rain_objs <- list(g1 = NULL, g3 = NULL, t_axis = NULL)
    }
    
    message("  -> [DATA] SC + Obs")
    cl_names <- c("nom", "numsec", "Time", "Q", "Zmoy", "Zmax", "Zmin", "Hmax", "Htot")
    tab_sc <- read.csv(file.path(dsnlayerC, f_sc), header = FALSE, sep = "", skip = 2, col.names = cl_names)
    tab_sc <- tab_sc %>% filter(as.numeric(Time) >= 60)
    tab_sc[, 2:9] <- lapply(tab_sc[, 2:9], as.numeric)
    tab_sc$DATE <- meta$date_deb + tab_sc$Time
    tab_sc$nom <- as.character(tab_sc$nom)
    if("Q" %in% names(sc_sf))  sc_sf =sc_sf[,-which(names(sc_sf)=="Q")]
    tab_sc <- inner_join(tab_sc, sc_sf, by = c("nom" = "ID"))
    
    sc_names <- unique(tab_sc$NOM) 
    sc_list_obs <- list.files(path = dsnscobs, full.names = TRUE)
    
    load_obs <- function(suffix, col_name) {
      files_match <- unlist(lapply(sc_names, function(x) grep(paste0(x, suffix), sc_list_obs, value = TRUE)))
      if (length(files_match) == 0) return(NULL)
      df_list <- lapply(files_match, read.csv)
      df_obs <- do.call(rbind, df_list)
      df_obs$Date <- as.POSIXct(ifelse(grepl(" ", df_obs$Date), df_obs$Date, paste(df_obs$Date, "00:00:00")), tz = "UTC")
      df_obs <- df_obs %>% filter(Date >= min(tab_sc$DATE) & Date <= max(tab_sc$DATE))
      if (nrow(df_obs) > 0) {
        colnames(df_obs) <- c("Date", col_name, "NOM")
        df_obs$NOM <- paste0(df_obs$NOM, "_obs") 
        df_obs$TYPE <- "dashed" 
        return(df_obs)
      }
      return(NULL)
    }
    
    obs_q <- load_obs("_Q.csv", "Q")
    df_q_all <- bind_rows(
      tab_sc %>% dplyr::select(Date = DATE, Q, NOM) %>% mutate(Q = abs(Q), TYPE = "solid"),
      if (!is.null(obs_q)) obs_q %>% mutate(Q = abs(Q), TYPE = "dashed") else NULL
    )
    
    obs_z <- load_obs("_Z.csv", "Z")
    df_z_all <- bind_rows(
      tab_sc %>% dplyr::select(Date = DATE, Z = Zmoy, NOM) %>% mutate(Z = Z, TYPE = "solid"),
      if (!is.null(obs_z)) obs_z %>% mutate(TYPE = "dashed") else NULL
    )
    
    obs_h <- load_obs("_H.csv", "H")
    df_h_all <- bind_rows(
      tab_sc %>% dplyr::select(Date = DATE, H = Hmax, NOM) %>% mutate(TYPE = "solid"),
      if (!is.null(obs_h)) obs_h %>% mutate(TYPE = "dashed") else NULL
    )
    
    save_master_plot <- function(df_data, y_col, y_lab, output_path, title_suffix) {
      if (file.exists(output_path)) return()
      df_data <- df_data %>% filter(!is.na(Date)) %>% arrange(NOM, Date)
      if (nrow(df_data) == 0) return()
      test_obs=df_data %>% filter(TYPE != "solid")
      if(length(test_obs$Date )!=0){
        g_main <- ggplot() +
          geom_line(data = df_data %>% filter(TYPE == "solid"), 
                    aes(x = Date, y = .data[[y_col]], color = NOM), 
                    linetype = "solid", size = 0.7) +
          geom_line(data = df_data %>% filter(TYPE != "solid"), 
                    aes(x = Date, y = .data[[y_col]], color = NOM), 
                    linetype = "dashed", size = 0.7) +
          theme_bw() + ylab(y_lab) + xlab("Date (heure météo)") + 
          theme(legend.title = element_blank())
        
      }else{
        g_main <- ggplot() +
          geom_line(data = df_data %>% filter(TYPE == "solid"), 
                    aes(x = Date, y = .data[[y_col]], color = NOM), 
                    linetype = "solid", size = 0.7) +
          theme_bw() + ylab(y_lab) + xlab("Date (heure météo)") + 
          theme(legend.title = element_blank())
      }
      
      
      titre <- paste0("Hyéto + ", title_suffix, " ", racine_nom, " (Pluie sur tout le bassin)")
      
      plots_list <- list()
      heights <- c()
      rows_val <- 0
      
      has_rain <- !is.null(rain_objs) && !is.null(rain_objs$g3) && !inherits(rain_objs$g3, "plotly_empty")
      
      if (has_rain) {
        plots_list[[1]] <- rain_objs$g3
        plots_list[[2]] <- rain_objs$g1
        plots_list[[3]] <- ggplotly(g_main, dynamicTicks = TRUE)
        heights <- c(0.1, 0.2, 0.7)
        rows_val <- 3
      } else {
        plots_list[[1]] <- ggplotly(g_main, dynamicTicks = TRUE)
        heights <- c(1)
        rows_val <- 1
      }
      
      p_final <- subplot(plots_list, nrows = rows_val, shareX = TRUE, heights = heights, titleY = TRUE) %>%
        layout(title = list(text = titre), hovermode = "closest", legend = list(title = list(text = "")))
      saveWidget(partial_bundle(p_final), file = output_path, selfcontained = TRUE, libdir = "lib")
    }
    
    message("  -> [EXPORT] HTML Q/Z/H")
    save_master_plot(df_q_all, "Q", "Débit [m3/s]", p_q_out, "Hydrogramme")
    save_master_plot(df_z_all, "Z", "Côte [m]", p_z_out, "Côtes")
    save_master_plot(df_h_all, "H", "Hauteur [m]", p_h_out, "Hauteur")
    
    # E. Flux et Ouvrages
    name_case_clean <- sub("\\.cas_.*", "", basename(f_out_match))
    p_qs_out <- file.path(dsnlayerC, paste0(name_case_clean, "_PN_PB_QS_Obs.html"))
    p_qoh_out <- file.path(dsnlayerC, paste0(name_case_clean, "_PN_PB_QOH_Obs.html"))
    p_vqs_out <- file.path(dsnlayerC, paste0(name_case_clean, "_PN_PB_VQS_Obs.html"))
    
    if (!is.null(tab_out)) {
      # 1. Ajout VQS (Volume)
      if (!file.exists(p_vqs_out)) {
        if (!exists("t_axis")) t_axis <- meta$date_deb + (tab_out[[1]] * past_ts)
        vol_domaine <- replace_na(tab_out[["VOLUME IN THE DOMAIN"]], 0)
        vol_domaine <- vol_domaine - vol_domaine[1]
        
        vol_injecte <- cumsum(replace_na(tab_out[["ADDITIONAL VOLUME DUE TO SOURCE TERMS"]], 0)) 
        
        df_flux_only <- tab_out %>% dplyr::select(matches("FLUX BOUNDARY"))
        vol_sorties_cumul <- rep(0, nrow(tab_out))
        if (ncol(df_flux_only) > 0) {
          # FIX: On garde toutes les colonnes, et on nettoie les NA
          df_flux_active <- df_flux_only %>% mutate(across(everything(), ~replace_na(., 0)))
          q_total <- rowSums(df_flux_active, na.rm=TRUE)
          vol_sorties_cumul <- cumsum(-q_total * pas_exp)
        }
        
        df_vol <- data.frame(
          Time = t_axis,
          Injecte = vol_injecte,
          Domaine = vol_domaine,
          `Domaine + Sorties` = vol_domaine + vol_sorties_cumul,
          check.names = FALSE
        ) %>% pivot_longer(-Time, names_to = "Variable", values_to = "Volume")
        
        # Ajout des volumes individuels des sorties (FIX: df_flux_active existe maintenant toujours)
        if (exists("df_flux_active") && ncol(df_flux_active) > 0) {
          for(col_name in colnames(df_flux_active)) {
            clean_name <- sub("FLUX BOUNDARY", "Sortie", col_name)
            vol_indiv <- cumsum(-df_flux_active[[col_name]] * pas_exp)
            df_temp <- data.frame(Time = t_axis, Variable = clean_name, Volume = vol_indiv)
            df_vol <- bind_rows(df_vol, df_temp)
          }
        }
        
        g_vol <- ggplot(df_vol, aes(x = Time, y = Volume, color = Variable)) +
          geom_line() + theme_bw() + labs(y = "Volume [m3]") + 
          theme(legend.title = element_blank(), legend.position="right")
        
        df_qs_total <- data.frame(Time = t_axis, Q = if(exists("q_total")) -q_total else 0)
        g_qs_total <- ggplot(df_qs_total, aes(x=Time, y=Q, color="Débit Sortie Total")) + 
          geom_line() + 
          scale_color_manual(values=c("Débit Sortie Total"="red")) +
          theme_bw() + ylab("Débit Sortie Total [m3/s]") +
          theme(legend.title = element_blank(), legend.position="right")
        
        plots_vqs <- list()
        if (!is.null(rain_objs) && !is.null(rain_objs$g3) && !inherits(rain_objs$g3, "plotly_empty")) {
          plots_vqs <- list(rain_objs$g3, ggplotly(g_vol, dynamicTicks = TRUE), ggplotly(g_qs_total, dynamicTicks = TRUE))
          p_vqs <- subplot(plots_vqs, nrows = 3, shareX = TRUE, titleY = TRUE) %>%
            layout(title = paste("Bilan Volume -", racine_nom), legend = list(title = list(text = ""), orientation = "v"))
        } else {
          plots_vqs <- list(ggplotly(g_vol, dynamicTicks = TRUE), ggplotly(g_qs_total, dynamicTicks = TRUE))
          p_vqs <- subplot(plots_vqs, nrows = 2, shareX = TRUE, titleY = TRUE) %>%
            layout(title = paste("Bilan Volume -", racine_nom), legend = list(title = list(text = ""), orientation = "v"))
        }
        saveWidget(partial_bundle(p_vqs), file = p_vqs_out, selfcontained = TRUE, libdir = "lib")
      }
      
      df_flux <- tab_out %>% dplyr::select(matches("FLUX BOUNDARY|CULVERT"))
      keep_cols <- sapply(df_flux, function(x) !all(is.na(x)))
      df_flux <- df_flux[, keep_cols, drop=FALSE]
      
      if (ncol(df_flux) > 0) {
        if (!exists("t_axis")) t_axis <- meta$date_deb + (tab_out[[1]] * past_ts)
        df_long <- df_flux %>% mutate(Time = t_axis) %>% 
          pivot_longer(-Time) %>% filter(!is.na(Time)) 
        
        if (!file.exists(p_qs_out) && any(grepl("FLUX", df_long$name))) {
          g_qs <- ggplot(df_long %>% filter(grepl("FLUX", name)), aes(Time, abs(value), color = name)) + 
            geom_line() + theme_bw() + ylab("Débit [m3/s]") + xlab("Date (heure météo)") + theme(legend.title = element_blank())
          
          plots_list <- list()
          has_rain <- !is.null(rain_objs) && !is.null(rain_objs$g3) && !inherits(rain_objs$g3, "plotly_empty")
          if (has_rain) {
            plots_list[[1]] <- rain_objs$g3
            plots_list[[2]] <- rain_objs$g1
            plots_list[[3]] <- ggplotly(g_qs, dynamicTicks = TRUE)
            h <- c(0.1, 0.2, 0.7); r <- 3
          } else {
            plots_list[[1]] <- ggplotly(g_qs, dynamicTicks = TRUE)
            h <- c(1); r <- 1
          }
          p_qs <- subplot(plots_list, nrows = r, shareX = TRUE, heights = h, titleY=TRUE) %>%
            layout(title = paste("Sorties -", racine_nom), hovermode = "closest", legend = list(title = list(text = "")))
          saveWidget(partial_bundle(p_qs), file = p_qs_out, selfcontained = TRUE, libdir = "lib")
        }
        
        if (!file.exists(p_qoh_out) && any(grepl("CULVERT", df_long$name))) {
          g_qoh <- ggplot(df_long %>% filter(grepl("CULVERT", name)), aes(Time, value, color = name)) + 
            geom_line() + theme_bw() + ylab("Débit [m3/s]") + xlab("Date (heure météo)") + theme(legend.title = element_blank())
          
          plots_list <- list()
          has_rain <- !is.null(rain_objs) && !is.null(rain_objs$g3) && !inherits(rain_objs$g3, "plotly_empty")
          if (has_rain) {
            plots_list[[1]] <- rain_objs$g3
            plots_list[[2]] <- rain_objs$g1
            plots_list[[3]] <- ggplotly(g_qoh, dynamicTicks = TRUE)
            h <- c(0.1, 0.2, 0.7); r <- 3
          } else {
            plots_list[[1]] <- ggplotly(g_qoh, dynamicTicks = TRUE)
            h <- c(1); r <- 1
          }
          p_qoh <- subplot(plots_list, nrows = r, shareX = TRUE, heights = h, titleY=TRUE) %>%
            layout(title = paste("Ouvrages -", racine_nom), hovermode = "closest", legend = list(title = list(text = "")))
          saveWidget(partial_bundle(p_qoh), file = p_qoh_out, selfcontained = TRUE, libdir = "lib")
        }
      }
    }
    
    message("  -> [GPKG] Qmax")
    tabmaxq <- tab_sc %>% group_by(nom) %>% filter(abs(Q) == max(abs(Q))) %>% slice(1)
    new_sc_sf <- sc_sf %>% 
      left_join(tabmaxq %>% dplyr::select(nom, Q, DATE, Zmoy, Hmax), by = c("ID" = "nom")) %>%
      mutate(Q = abs(Q), NomSecteur = basename(dsnlayerC))
    st_write(new_sc_sf, file.path(dsnlayerC, paste0(gsub("\\..*", "", f_sc), "_Qmax.gpkg")), driver = "GPKG", delete_dsn = TRUE, quiet = TRUE)
    file.copy(file.path(chem_routine, "C2D", Doss_Qml, "SC_Qmax.qml"), file.path(dsnlayerC, paste0(gsub("\\..*", "", f_sc), "_Qmax.qml")), overwrite = TRUE)
  }
  
  if (dir.exists(file.path(dsnlayerC, "lib"))) unlink(file.path(dsnlayerC, "lib"), recursive = TRUE)
  message("[FIN] Terminé.")
}
plot_sc_obs_oh <- function() {
  library(tidyverse)
  library(sf)
  library(plotly)
  library(htmlwidgets)
  library(tools)
  
  # --- 1. FONCTIONS UTILITAIRES ---
  
  get_rain_step <- function(filename, is_evt = TRUE) {
    val <- as.numeric(gsub(".*[[:punct:]](\\d+)min([[:punct:]]|$).*", "\\1", filename))
    if (is.na(val) || val == 0 || val > 1440) { 
      parts <- strsplit(filename, "_")[[1]]
      mins <- grep("^\\d+min$", parts, value = TRUE)
      if (length(mins) > 0) {
        val <- as.numeric(gsub("min", "", mins[1]))
      } else {
        val <- if (!is_evt || grepl("shyreg|spatial", filename, ignore.case = TRUE)) 15 else 1 
      }
    }
    return(if(is.na(val) || val == 0) 1 else val)
  }
  
  parse_time_meta <- function(filename) {
    parts <- strsplit(filename, "_")[[1]]
    is_evt <- grepl("Evt", filename, fixed = TRUE)
    duree <- 0
    date_deb <- as.POSIXct("1900-01-01", tz="UTC")
    
    if (is_evt) {
      if (length(parts) >= 5) {
        suppressWarnings({
          date_deb <- strptime(gsub("[^0-9]", "", parts[4]), format = "%Y%m%d%H%M", tz = "UTC")
          h_m <- strsplit(parts[5], "h")[[1]]
          if(length(h_m) >= 2) duree <- (as.numeric(h_m[1]) + as.numeric(substr(h_m[2], 1, 2)) / 60) * 3600
        })
      }
    } else {
      if (length(parts) >= 8) {
        duree <- (as.numeric(parts[7]) + as.numeric(parts[8]) / 60) * 3600
      }
    }
    list(date_deb = as.POSIXct(date_deb), duree = duree, is_evt = is_evt)
  }
  
  create_regex_safe <- function(str) {
    gsub("([.|()\\^{}+$*?]|\\[|\\])", "\\\\\\1", str)
  }
  
  parse_telemac_sortie <- function(file_path) {
    message(paste("    [DEBUG]", Sys.time(), "- Lecture sortie:", basename(file_path)))
    if (!file.exists(file_path)) return(NULL)
    
    past_ts <- NA
    dir_path <- dirname(file_path)
    base_name <- basename(file_path)
    cas_name_candidate <- sub("_\\d{4}-\\d{2}-\\d{2}-\\d{2}h\\d{2}min\\d{2}s\\.sortie$", "", base_name)
    
    if (cas_name_candidate == base_name) {
      cas_name_candidate <- substr(base_name, 1, nchar(base_name) - 30)
    }
    
    cas_files <- list.files(dir_path, pattern = paste0("^", create_regex_safe(cas_name_candidate)), full.names = TRUE)
    cas_files <- cas_files[!grepl("\\.sortie$", cas_files)]
    
    if (length(cas_files) > 0) {
      lines_cas <- readLines(cas_files[1], warn = FALSE)
      ts_idx_cas <- grep("TIME STEP|PAS DE TEMPS", lines_cas, ignore.case = TRUE)[1]
      if (!is.na(ts_idx_cas)) {
        raw_line <- lines_cas[ts_idx_cas]
        extracted_ts <- gsub(".*(?:STEP|TEMPS)[^0-9.]*([0-9.]+).*", "\\1", raw_line, ignore.case = TRUE)
        past_ts <- as.numeric(extracted_ts)
        if(!is.na(past_ts)) message(paste("    [INFO] TIME STEP lu dans .cas :", past_ts))
      }
    }
    
    lines <- readLines(file_path, warn = FALSE)
    
    if (is.na(past_ts)) {
      ts_idx <- grep("TIME STEP|PAS DE TEMPS", lines, useBytes = TRUE, ignore.case = TRUE)[1]
      if (!is.na(ts_idx)) {
        raw_line <- lines[ts_idx]
        extracted_ts <- gsub(".*(?:STEP|TEMPS)[^0-9.]*([0-9.]+).*", "\\1", raw_line, ignore.case = TRUE)
        past_ts <- as.numeric(extracted_ts)
        if(!is.na(past_ts)) message(paste("    [INFO] TIME STEP lu dans .sortie :", past_ts))
      }
    }
    
    if (is.na(past_ts)) {
      message("    [WARN] TIME STEP introuvable. Valeur par défaut 1.0 utilisée.")
      past_ts <- 1.0
    }
    
    idx_interest <- grep("ITERATION|FLUX BOUNDARY|CULVERT|RUNOFF_SCS_CN|VOLUME IN THE DOMAIN|ADDITIONAL VOLUME", lines, useBytes = TRUE)
    if (length(idx_interest) == 0) return(NULL)
    ls <- lines[idx_interest]
    
    clean_numeric <- function(txt) {
      txt <- gsub("[*]", "", txt)
      suppressWarnings(as.numeric(txt))
    }
    
    mots_cles <- tribble(
      ~key, ~start, ~end,
      "ITERATION ", 12, 22,
      "RUNOFF_SCS_CN : ACCUMULATED RAINFALL", 44, 61,
      "VOLUME IN THE DOMAIN", 28, 44,
      "ADDITIONAL VOLUME DUE TO SOURCE TERMS", 44, 61
    )
    
    flux_raw <- grep("FLUX BOUNDARY", ls, value = TRUE, useBytes = TRUE)
    flux_keys <- unique(sub("^\\s*(FLUX BOUNDARY\\s+\\d+).*", "\\1", flux_raw))
    
    cul_raw <- grep("CULVERT", ls, value = TRUE, useBytes = TRUE)
    culvert_keys <- unique(sub("^\\s*(CULVERT\\s+\\d+).*", "\\1", cul_raw))
    
    extra <- tibble(key = c(flux_keys, culvert_keys)) %>%
      mutate(start = if_else(grepl("CULVERT", key), 37, 62), end = if_else(grepl("CULVERT", key), 62, 42)) # Fix intervalle si nécessaire
    
    all_keys <- bind_rows(mots_cles, extra)
    ref_idx <- grep("ITERATION ", ls, useBytes = TRUE)
    res <- matrix(NA, nrow = length(ref_idx), ncol = nrow(all_keys))
    colnames(res) <- all_keys$key
    
    for (i in 1:nrow(mots_cles)) {
      m <- grep(mots_cles$key[i], ls, fixed = TRUE, useBytes = TRUE)
      if (length(m) > 0) {
        vals <- clean_numeric(substr(ls[m], mots_cles$start[i], mots_cles$end[i]))
        indices <- findInterval(m, ref_idx)
        valid <- indices > 0 & indices <= nrow(res)
        res[indices[valid], mots_cles$key[i]] <- vals[valid]
      }
    }
    
    if (length(flux_keys) > 0) {
      m_flux <- grep("FLUX BOUNDARY", ls, fixed = TRUE, useBytes = TRUE)
      if (length(m_flux) > 0) {
        txt_flux <- ls[m_flux]
        vals_flux <- clean_numeric(substr(txt_flux, 25, 42))
        keys_flux <- sub("^\\s*(FLUX BOUNDARY\\s+\\d+).*", "\\1", txt_flux)
        
        rows <- findInterval(m_flux, ref_idx)
        cols <- match(keys_flux, colnames(res))
        valid <- !is.na(cols) & rows > 0 & rows <= nrow(res)
        res[cbind(rows[valid], cols[valid])] <- vals_flux[valid]
      }
    }
    
    if (length(culvert_keys) > 0) {
      m_cul <- grep("CULVERT", ls, fixed = TRUE, useBytes = TRUE)
      if (length(m_cul) > 0) {
        txt_cul <- ls[m_cul]
        vals_cul <- clean_numeric(substr(txt_cul, 37, 62))
        keys_cul <- sub("^\\s*(CULVERT\\s+\\d+).*", "\\1", txt_cul)
        
        rows <- findInterval(m_cul, ref_idx)
        cols <- match(keys_cul, colnames(res))
        valid <- !is.na(cols) & rows > 0 & rows <= nrow(res)
        res[cbind(rows[valid], cols[valid])] <- vals_cul[valid]
      }
    }
    
    df <- as.data.frame(res)
    df$Past_Ts <- past_ts
    df <- df[!is.na(df[, 1]), ] 
    
    message(paste("    [DEBUG] Parsing fini. Lignes:", nrow(df)))
    gc() 
    return(df)
  }
  
  # --- 2. INITIALISATION ---
  message(paste("[INFO]", Sys.time(), "Start plot_sc_obs_oh"))
  sc_list <- list.files(dsnlayerC, pattern = "SC.txt")
  list_sortie <- list.files(dsnlayerC, pattern = "s.sortie")
  list_sortie <- list_sortie[!grepl("\\.txt$", list_sortie)]
  
  # Construction d'une liste unique de tous les scénarios (racines) détectés
  all_cases <- list()
  
  # 1. Scénarios issus des SC.txt
  for (f_sc in sc_list) {
    rac <- sub("SC\\.txt$", "", f_sc)
    out_matches <- list_sortie[grep(create_regex_safe(rac), list_sortie)]
    all_cases[[rac]] <- list(f_sc = f_sc, f_outs = out_matches)
  }
  
  # 2. Ajout des scénarios orphelins (qui n'ont qu'un .sortie et pas de SC.txt)
  for (f_out in list_sortie) {
    already_matched <- FALSE
    for (cas in all_cases) {
      if (f_out %in% cas$f_outs) already_matched <- TRUE
    }
    if (!already_matched) {
      rac <- sub("_\\d{4}-\\d{2}-\\d{2}-\\d{2}h\\d{2}min\\d{2}s\\.sortie$", "", f_out)
      rac <- sub("\\.sortie$", "", rac)
      if(!endsWith(rac, "_")) rac <- paste0(rac, "_")
      if (is.null(all_cases[[rac]])) {
        all_cases[[rac]] <- list(f_sc = NULL, f_outs = f_out)
      } else {
        all_cases[[rac]]$f_outs <- unique(c(all_cases[[rac]]$f_outs, f_out))
      }
    }
  }
  
  if (length(all_cases) == 0) return(message("Aucun SC.txt ni fichier .sortie trouvé."))
  
  path_shp <- file.path(dsnlayerC, "SectControl_Select.shp")
  if(file.exists(path_shp)) sc_sf <- st_read(path_shp, quiet = TRUE) %>% mutate(ID = as.character(ID))
  
  contour_aire <- st_read(file.path(dsnlayerC, "Step_Cartino2d_1_Contour.shp"), quiet = TRUE)
  aire_val <- as.numeric(st_area(contour_aire))
  dsnscobs <- file.path(dsnlayer, "_SectionsControles", contour$NOMPOST)
  
  cache_sortie <- list()      
  cache_rain_plots <- list()  
  
  # --- 3. BOUCLE PRINCIPALE ---
  
  for (racine in names(all_cases)) {
    message(paste("----------------------------------------------------------------"))
    message(paste("[STEP] Traitement Racine :", racine))
    
    f_sc <- all_cases[[racine]]$f_sc
    f_out_match <- all_cases[[racine]]$f_outs
    
    has_sc <- !is.null(f_sc) && file.exists(file.path(dsnlayerC, f_sc))
    pseudo_f_sc <- if (has_sc) f_sc else paste0(racine, "SC.txt") # Utilisé pour le parsing nom
    racine_nom <- if (has_sc) tools::file_path_sans_ext(f_sc) else racine
    
    if (length(f_out_match) > 1) {
      dates_out <- sub(".*\\.cas_(\\d{4}-\\d{2}-\\d{2}-\\d{2}h\\d{2}min\\d{2}s)\\.sortie", "\\1", f_out_match)
      f_out_match <- f_out_match[which.max(as.POSIXct(dates_out, format = "%Y-%m-%d-%Hh%Mmin%Ss"))]
    }
    
    if (length(f_out_match) == 0) {
      message("  -> [WARN] .sortie introuvable pour ce cas.")
      tab_out <- NULL
    } else {
      if (is.null(cache_sortie[[f_out_match]])) {
        cache_sortie[[f_out_match]] <- parse_telemac_sortie(file.path(dsnlayerC, f_out_match))
      }
      tab_out <- cache_sortie[[f_out_match]]
    }
    
    meta <- parse_time_meta(pseudo_f_sc)
    rain_objs <- NULL 
    
    if (!is.null(tab_out)) {
      dir_pluie <- tryCatch(file.path(dsnPluie, contour$NOMPOST), error = function(e) NULL)
      file_brute <- NA
      
      if (!is.null(dir_pluie) && dir.exists(dir_pluie)) {
        all_brutes <- list.files(dir_pluie, pattern = "^(brute)_.*\\.txt$", full.names = TRUE)
        if (length(all_brutes) > 0) {
          name_sc_split <- strsplit(pseudo_f_sc,"_")[[1]]
          name_sc_brute <- do.call(paste, c(as.list(name_sc_split[1:5]), sep = "_"))
          clean_sc <- sub("SC\\.txt$", "", name_sc_brute)
          parts_sc <- strsplit(clean_sc, "_")[[1]]
          
          dur_token <- grep("^\\d+min$", parts_sc, value = TRUE)
          if(length(dur_token) == 0) dur_token <- "IGNORE_ME" else dur_token <- dur_token[1]
          
          evt_token <- grep("^Evts\\d+", parts_sc, value = TRUE)
          if(length(evt_token) == 0) evt_token <- "IGNORE_ME" else evt_token <- evt_token[1]
          candidates <- all_brutes
          if(evt_token== "IGNORE_ME") candidates <- NULL
          
          if (dur_token != "IGNORE_ME") {
            has_dur <- grepl(paste0("_", dur_token, "([_.]|$)"), basename(candidates)) | grepl(dur_token, basename(candidates), fixed = TRUE)
            if (any(has_dur)) candidates <- candidates[has_dur]
          }
          if (evt_token != "IGNORE_ME") {
            has_evt <- grepl(evt_token, basename(candidates))
            if (any(has_evt)) candidates <- candidates[has_evt]
          }
          if (length(candidates) > 0) {
            scores <- sapply(candidates, function(cand) {
              parts_cand <- strsplit(basename(cand), "_")[[1]]
              sum(parts_cand %in% parts_sc)
            })
            file_brute <- candidates[which.max(scores)]
            message(paste("    -> [BRUTE] Trouvé:", basename(file_brute)))
          }
        }
      }
      
      cache_key_rain <- paste0(f_out_match, ifelse(!is.na(file_brute), basename(file_brute), "NONE"))
      
      if (is.null(cache_rain_plots[[cache_key_rain]])) {
        past_ts <- tab_out$Past_Ts[1]
        if (is.na(past_ts)) past_ts <- 1.0
        
        if (nrow(tab_out) > 1) {
          pas_exp <- median(diff(tab_out[[1]]), na.rm = TRUE) * past_ts
        } else {
          pas_exp <- past_ts
        }
        if (is.na(pas_exp) || pas_exp <= 0) pas_exp <- 1.0
        message(paste("    -> [INFO] Pas d'export:", pas_exp, "s"))
        
        t_axis <- meta$date_deb + (tab_out[[1]] * past_ts)
        
        acc_rain <- replace_na(tab_out[["RUNOFF_SCS_CN : ACCUMULATED RAINFALL"]], 0)
        inc_rain <- c(acc_rain[1], diff(acc_rain))
        inc_rain[inc_rain < 0] <- 0 
        
        df_pluie <- data.frame(
          Time = t_axis,
          Intensite = inc_rain * 1000 * 3600 / pas_exp, 
          PNette = replace_na(tab_out[["ADDITIONAL VOLUME DUE TO SOURCE TERMS"]], 0) / aire_val * 1000 * 3600 / past_ts,
          CumulBrut = replace_na(tab_out[["RUNOFF_SCS_CN : ACCUMULATED RAINFALL"]], 0) * 1000, 
          CumulNet = cumsum(replace_na(tab_out[["ADDITIONAL VOLUME DUE TO SOURCE TERMS"]], 0) / aire_val * pas_exp / past_ts) * 1000,
          NomBrut = "Pluie brute", NomNet = "Pluie nette"
        )
        bar_shift=0
        if (!is.na(file_brute) && file.exists(file_brute)) {
          tab_brute <- read.table(file_brute, header = TRUE)
          rain_step <- get_rain_step(basename(file_brute), is_evt = meta$is_evt)
          message(paste("    -> [BRUTE] Step:", rain_step, "min"))
          bar_shift <- pas_exp 
          col_val <- if("Val" %in% names(tab_brute)) "Val" else names(tab_brute)[2]
          
          n_out <- nrow(df_pluie)
          if (nrow(tab_brute) >= n_out) {
            raw_vals <- tab_brute[[col_val]][1:n_out]
          } else {
            raw_vals <- c(tab_brute[[col_val]], rep(0, n_out - nrow(tab_brute)))
          }
          
          df_pluie$CumulBrut <- cumsum(raw_vals / rain_step)
          df_pluie$Intensite <- raw_vals * 3600 / (pas_exp * rain_step)
        }
        
        df_pluie <- df_pluie %>% filter(!is.na(Time))
        
        if (nrow(df_pluie) > 0) {
          df_hyeto <- df_pluie %>% dplyr::select(Time, `Pluie brute` = Intensite, `Pluie nette` = PNette) %>%
            pivot_longer(cols = c("Pluie brute", "Pluie nette"), names_to = "Nom", values_to = "Val")
          df_hyeto$Nom <- factor(df_hyeto$Nom, levels = c("Pluie brute", "Pluie nette"))
          
          cols_hyeto <- c("Pluie brute" = "#33FFFF", "Pluie nette" = "#9933FF")
          df_hyeto$Time[df_hyeto$Nom=="Pluie brute"]=df_hyeto$Time[df_hyeto$Nom=="Pluie brute"]+bar_shift
          
          g1 <- ggplotly(ggplot(df_hyeto, aes(x = Time, y = Val, fill = Nom)) +
                           geom_col(position = "identity", width = pas_exp) +
                           scale_fill_manual(name="", values = cols_hyeto) + 
                           theme_bw() + ylab("Intensité [mm/h]") + 
                           theme(axis.title.x = element_blank(), legend.title = element_blank()), dynamicTicks = TRUE)
          
          df_cumul <- df_pluie %>% dplyr::select(Time, `Cumul brut` = CumulBrut, `Cumul net` = CumulNet) %>%
            pivot_longer(cols = c("Cumul brut", "Cumul net"), names_to = "Nom", values_to = "Val")
          cols_cumul <- c("Cumul brut" = "#33FFFF", "Cumul net" = "#9933FF")
          
          g3 <- ggplotly(ggplot(df_cumul, aes(x = Time, y = Val, color = Nom)) +
                           geom_line(size = 1.1) +
                           scale_color_manual(name="", values = cols_cumul) +
                           theme_bw() + ylab("Cumul [mm]") + 
                           theme(axis.title.x = element_blank(), legend.title = element_blank()), dynamicTicks = TRUE)
          
          cache_rain_plots[[cache_key_rain]] <- list(g1 = g1, g3 = g3, t_axis = t_axis)
        } else {
          cache_rain_plots[[cache_key_rain]] <- NULL
        }
      }
      rain_objs <- cache_rain_plots[[cache_key_rain]]
    } else {
      rain_objs <- NULL
    }
    
    if (is.null(rain_objs)) {
      message("    [WARN] Données pluie invalides ou manquantes.")
      rain_objs <- list(g1 = NULL, g3 = NULL, t_axis = NULL)
    }
    
    # ---------------------------------------------------------
    # PARTIE Q, Z, H (Dépend strictement de la présence de SC.txt)
    # ---------------------------------------------------------
    if (has_sc) {
      message("  -> [DATA] SC + Obs")
      cl_names <- c("nom", "numsec", "Time", "Q", "Zmoy", "Zmax", "Zmin", "Hmax", "Htot")
      tab_sc <- read.csv(file.path(dsnlayerC, f_sc), header = FALSE, sep = "", skip = 2, col.names = cl_names)
      tab_sc <- tab_sc %>% filter(as.numeric(Time) >= 60)
      tab_sc[, 2:9] <- lapply(tab_sc[, 2:9], as.numeric)
      tab_sc$DATE <- meta$date_deb + tab_sc$Time
      tab_sc$nom <- as.character(tab_sc$nom)
      if("Q" %in% names(sc_sf))  sc_sf =sc_sf[,-which(names(sc_sf)=="Q")]
      tab_sc <- inner_join(tab_sc, sc_sf, by = c("nom" = "ID"))
      
      sc_names <- unique(tab_sc$NOM) 
      sc_list_obs <- list.files(path = dsnscobs, full.names = TRUE)
      
      load_obs <- function(suffix, col_name) {
        files_match <- unlist(lapply(sc_names, function(x) grep(paste0(x, suffix), sc_list_obs, value = TRUE)))
        if (length(files_match) == 0) return(NULL)
        df_list <- lapply(files_match, read.csv)
        df_obs <- do.call(rbind, df_list)
        df_obs$Date <- as.POSIXct(ifelse(grepl(" ", df_obs$Date), df_obs$Date, paste(df_obs$Date, "00:00:00")), tz = "UTC")
        df_obs <- df_obs %>% filter(Date >= min(tab_sc$DATE) & Date <= max(tab_sc$DATE))
        if (nrow(df_obs) > 0) {
          colnames(df_obs) <- c("Date", col_name, "NOM")
          df_obs$NOM <- paste0(df_obs$NOM, "_obs") 
          df_obs$TYPE <- "dashed" 
          return(df_obs)
        }
        return(NULL)
      }
      
      obs_q <- load_obs("_Q.csv", "Q")
      df_q_all <- bind_rows(
        tab_sc %>% dplyr::select(Date = DATE, Q, NOM) %>% mutate(Q = abs(Q), TYPE = "solid"),
        if (!is.null(obs_q)) obs_q %>% mutate(Q = abs(Q), TYPE = "dashed") else NULL
      )
      
      obs_z <- load_obs("_Z.csv", "Z")
      df_z_all <- bind_rows(
        tab_sc %>% dplyr::select(Date = DATE, Z = Zmax, NOM) %>% mutate(Z = Z, TYPE = "solid"),
        if (!is.null(obs_z)) obs_z %>% mutate(TYPE = "dashed") else NULL
      )
      # FIX: Remplacement des 0 par NA dans la colonne Z
      df_z_all$Z[df_z_all$Z == 0] <- NA
      
      obs_h <- load_obs("_H.csv", "H")
      df_h_all <- bind_rows(
        tab_sc %>% dplyr::select(Date = DATE, H = Hmax, NOM) %>% mutate(TYPE = "solid"),
        if (!is.null(obs_h)) obs_h %>% mutate(TYPE = "dashed") else NULL
      )
      
      save_master_plot <- function(df_data, y_col, y_lab, output_path, title_suffix) {
        if (file.exists(output_path)) return()
        df_data <- df_data %>% filter(!is.na(Date)) %>% arrange(NOM, Date)
        if (nrow(df_data) == 0) return()
        test_obs = df_data %>% filter(TYPE != "solid")
        
        # S'assurer qu'il n'y a que des données non-NA avant le tracé
        df_data <- df_data %>% filter(!is.na(.data[[y_col]]))
        
        if(length(test_obs$Date) != 0) {
          g_main <- ggplot() +
            geom_line(data = df_data %>% filter(TYPE == "solid"), 
                      aes(x = Date, y = .data[[y_col]], color = NOM), 
                      linetype = "solid", size = 0.7) +
            geom_line(data = df_data %>% filter(TYPE != "solid"), 
                      aes(x = Date, y = .data[[y_col]], color = NOM), 
                      linetype = "dashed", size = 0.7) +
            theme_bw() + ylab(y_lab) + xlab("Date (heure météo)") + 
            theme(legend.title = element_blank())
        } else {
          g_main <- ggplot() +
            geom_line(data = df_data %>% filter(TYPE == "solid"), 
                      aes(x = Date, y = .data[[y_col]], color = NOM), 
                      linetype = "solid", size = 0.7) +
            theme_bw() + ylab(y_lab) + xlab("Date (heure météo)") + 
            theme(legend.title = element_blank())
        }
        
        titre <- paste0("Hyéto + ", title_suffix, " ", racine_nom, " (Pluie sur tout le bassin)")
        
        plots_list <- list()
        heights <- c()
        rows_val <- 0
        
        has_rain <- !is.null(rain_objs) && !is.null(rain_objs$g3) && !inherits(rain_objs$g3, "plotly_empty")
        
        if (has_rain) {
          plots_list[[1]] <- rain_objs$g3
          plots_list[[2]] <- rain_objs$g1
          plots_list[[3]] <- ggplotly(g_main, dynamicTicks = TRUE)
          heights <- c(0.1, 0.2, 0.7)
          rows_val <- 3
        } else {
          plots_list[[1]] <- ggplotly(g_main, dynamicTicks = TRUE)
          heights <- c(1)
          rows_val <- 1
        }
        
        p_final <- subplot(plots_list, nrows = rows_val, shareX = TRUE, heights = heights, titleY = TRUE) %>%
          layout(title = list(text = titre), hovermode = "closest", legend = list(title = list(text = "")))
        saveWidget(partial_bundle(p_final), file = output_path, selfcontained = TRUE, libdir = "lib")
      }
      
      message("  -> [EXPORT] HTML Q/Z/H")
      p_q_out <- file.path(dsnlayerC, paste0(racine_nom, "_PN_PB_Q_Obs.html"))
      p_z_out <- file.path(dsnlayerC, paste0(racine_nom, "_PN_PB_Z_Obs.html"))
      p_h_out <- file.path(dsnlayerC, paste0(racine_nom, "_PN_PB_H_Obs.html"))
      
      save_master_plot(df_q_all, "Q", "Débit [m3/s]", p_q_out, "Hydrogramme")
      save_master_plot(df_z_all, "Z", "Côte [m]", p_z_out, "Côtes")
      save_master_plot(df_h_all, "H", "Hauteur [m]", p_h_out, "Hauteur")
      
      message("  -> [GPKG] Qmax")
      tabmaxq <- tab_sc %>% group_by(nom) %>% filter(abs(Q) == max(abs(Q))) %>% slice(1)
      new_sc_sf <- sc_sf %>% 
        left_join(tabmaxq %>% dplyr::select(nom, Q, DATE, Zmoy, Hmax), by = c("ID" = "nom")) %>%
        mutate(Q = abs(Q), NomSecteur = basename(dsnlayerC))
      st_write(new_sc_sf, file.path(dsnlayerC, paste0(gsub("\\..*", "", f_sc), "_Qmax.gpkg")), driver = "GPKG", delete_dsn = TRUE, quiet = TRUE)
      file.copy(file.path(chem_routine, "C2D", Doss_Qml, "SC_Qmax.qml"), file.path(dsnlayerC, paste0(gsub("\\..*", "", f_sc), "_Qmax.qml")), overwrite = TRUE)
    } else {
      message("  -> [INFO] Pas de fichier SC.txt, graphes Q, Z, H ignorés.")
    }
    
    # ---------------------------------------------------------
    # PARTIE SORTIES: FLUX, OUVRAGES ET VOLUMES (.sortie direct)
    # ---------------------------------------------------------
    if (!is.null(tab_out)) {
      name_case_clean <- sub("\\.cas_.*", "", basename(f_out_match[1]))
      p_qs_out <- file.path(dsnlayerC, paste0(name_case_clean, "_PN_PB_QS_Obs.html"))
      p_qoh_out <- file.path(dsnlayerC, paste0(name_case_clean, "_PN_PB_QOH_Obs.html"))
      p_vqs_out <- file.path(dsnlayerC, paste0(name_case_clean, "_PN_PB_VQS_Obs.html"))
      
      if (!exists("t_axis") || is.null(t_axis)) t_axis <- meta$date_deb + (tab_out[[1]] * past_ts)
      
      # 1. Ajout VQS (Volume)
      if (!file.exists(p_vqs_out)) {
        vol_domaine <- replace_na(tab_out[["VOLUME IN THE DOMAIN"]], 0)
        vol_domaine <- vol_domaine - vol_domaine[1]
        
        # Le volume de la source imprimé dans la sortie Telemac est celui d'1 seul pas de temps de calcul (past_ts)
        rate_injecte <- replace_na(tab_out[["ADDITIONAL VOLUME DUE TO SOURCE TERMS"]], 0) / past_ts
        vol_injecte <- cumsum(rate_injecte * pas_exp) 
        
        df_flux_only <- tab_out %>% dplyr::select(matches("FLUX BOUNDARY"))
        vol_sorties_cumul <- rep(0, nrow(tab_out))
        q_total <- rep(0, nrow(tab_out))
        
        if (ncol(df_flux_only) > 0) {
          df_flux_active <- df_flux_only %>% mutate(across(everything(), ~replace_na(., 0)))
          q_total <- rowSums(df_flux_active, na.rm=TRUE)
          # FIX : On utilise -q_total exactement comme l'ancien code (-TimeStep*sum(...))
          # Les flux sortants (<0 dans Telemac) deviennent des volumes positifs
          # ce qui maintient l'équilibre physique exact pour Domaine + Sorties
          vol_sorties_cumul <- cumsum(-q_total * pas_exp)
        }
        
        df_vol <- data.frame(
          Time = t_axis,
          Injecte = vol_injecte,
          Domaine = vol_domaine,
          `Domaine + Sorties` = vol_domaine + vol_sorties_cumul,
          check.names = FALSE
        ) %>% pivot_longer(-Time, names_to = "Variable", values_to = "Volume")
        
        # Ajout des volumes individuels des sorties
        if (exists("df_flux_active") && ncol(df_flux_active) > 0) {
          for(col_name in colnames(df_flux_active)) {
            clean_name <- sub("FLUX BOUNDARY", "Sortie", col_name)
            # Volume individuel (sortant = positif grâce au signe -)
            vol_indiv <- cumsum(-df_flux_active[[col_name]] * pas_exp)
            df_temp <- data.frame(Time = t_axis, Variable = clean_name, Volume = vol_indiv)
            df_vol <- bind_rows(df_vol, df_temp)
          }
        }
        
        g_vol <- ggplot(df_vol, aes(x = Time, y = Volume, color = Variable)) +
          geom_line() + theme_bw() + labs(y = "Volume [m3]") + 
          theme(legend.title = element_blank(), legend.position="right")
        
        # Graphique débit total de sortie (conservé en valeur absolue pour le visuel graphique)
        df_qs_total <- data.frame(Time = t_axis, Q = abs(q_total))
        g_qs_total <- ggplot(df_qs_total, aes(x=Time, y=Q, color="Débit Sortie Total")) + 
          geom_line() + 
          scale_color_manual(values=c("Débit Sortie Total"="red")) +
          theme_bw() + ylab("Débit Sortie Total [m3/s]") +
          theme(legend.title = element_blank(), legend.position="right")
        
        plots_vqs <- list()
        if (!is.null(rain_objs) && !is.null(rain_objs$g3) && !inherits(rain_objs$g3, "plotly_empty")) {
          plots_vqs <- list(rain_objs$g3, ggplotly(g_vol, dynamicTicks = TRUE), ggplotly(g_qs_total, dynamicTicks = TRUE))
          p_vqs <- subplot(plots_vqs, nrows = 3, shareX = TRUE, titleY = TRUE) %>%
            layout(title = paste("Bilan Volume -", racine_nom), legend = list(title = list(text = ""), orientation = "v"))
        } else {
          plots_vqs <- list(ggplotly(g_vol, dynamicTicks = TRUE), ggplotly(g_qs_total, dynamicTicks = TRUE))
          p_vqs <- subplot(plots_vqs, nrows = 2, shareX = TRUE, titleY = TRUE) %>%
            layout(title = paste("Bilan Volume -", racine_nom), legend = list(title = list(text = ""), orientation = "v"))
        }
        saveWidget(partial_bundle(p_vqs), file = p_vqs_out, selfcontained = TRUE, libdir = "lib")
      }
      
      # 2. Flux and Culverts (QS, QOH)
      df_flux <- tab_out %>% dplyr::select(matches("FLUX BOUNDARY|CULVERT"))
      keep_cols <- sapply(df_flux, function(x) !all(is.na(x)))
      df_flux <- df_flux[, keep_cols, drop=FALSE]
      
      if (ncol(df_flux) > 0) {
        df_long <- df_flux %>% mutate(Time = t_axis) %>% 
          pivot_longer(-Time) %>% filter(!is.na(Time)) 
        
        if (!file.exists(p_qs_out) && any(grepl("FLUX", df_long$name))) {
          # Valeur absolue uniquement pour l'affichage graphique du débit !
          g_qs <- ggplot(df_long %>% filter(grepl("FLUX", name)), aes(Time, abs(value), color = name)) + 
            geom_line() + theme_bw() + ylab("Débit [m3/s]") + xlab("Date (heure météo)") + theme(legend.title = element_blank())
          
          plots_list <- list()
          has_rain <- !is.null(rain_objs) && !is.null(rain_objs$g3) && !inherits(rain_objs$g3, "plotly_empty")
          if (has_rain) {
            plots_list[[1]] <- rain_objs$g3
            plots_list[[2]] <- rain_objs$g1
            plots_list[[3]] <- ggplotly(g_qs, dynamicTicks = TRUE)
            h <- c(0.1, 0.2, 0.7); r <- 3
          } else {
            plots_list[[1]] <- ggplotly(g_qs, dynamicTicks = TRUE)
            h <- c(1); r <- 1
          }
          p_qs <- subplot(plots_list, nrows = r, shareX = TRUE, heights = h, titleY=TRUE) %>%
            layout(title = paste("Sorties -", racine_nom), hovermode = "closest", legend = list(title = list(text = "")))
          saveWidget(partial_bundle(p_qs), file = p_qs_out, selfcontained = TRUE, libdir = "lib")
        }
        
        if (!file.exists(p_qoh_out) && any(grepl("CULVERT", df_long$name))) {
          g_qoh <- ggplot(df_long %>% filter(grepl("CULVERT", name)), aes(Time, abs(value), color = name)) + 
            geom_line() + theme_bw() + ylab("Débit [m3/s]") + xlab("Date (heure météo)") + theme(legend.title = element_blank())
          
          plots_list <- list()
          has_rain <- !is.null(rain_objs) && !is.null(rain_objs$g3) && !inherits(rain_objs$g3, "plotly_empty")
          if (has_rain) {
            plots_list[[1]] <- rain_objs$g3
            plots_list[[2]] <- rain_objs$g1
            plots_list[[3]] <- ggplotly(g_qoh, dynamicTicks = TRUE)
            h <- c(0.1, 0.2, 0.7); r <- 3
          } else {
            plots_list[[1]] <- ggplotly(g_qoh, dynamicTicks = TRUE)
            h <- c(1); r <- 1
          }
          p_qoh <- subplot(plots_list, nrows = r, shareX = TRUE, heights = h, titleY=TRUE) %>%
            layout(title = paste("Ouvrages -", racine_nom), hovermode = "closest", legend = list(title = list(text = "")))
          saveWidget(partial_bundle(p_qoh), file = p_qoh_out, selfcontained = TRUE, libdir = "lib")
        }
      }
    }
  }
  
  if (dir.exists(file.path(dsnlayerC, "lib"))) unlink(file.path(dsnlayerC, "lib"), recursive = TRUE)
  message("[FIN] Terminé.")
}
plot_sc_obs_oh <- function() {
  library(tidyverse)
  library(sf)
  library(plotly)
  library(htmlwidgets)
  library(tools)
  
  # --- 1. FONCTIONS UTILITAIRES ---
  
  get_rain_step <- function(filename, is_evt = TRUE) {
    val <- as.numeric(gsub(".*[[:punct:]](\\d+)min([[:punct:]]|$).*", "\\1", filename))
    if (is.na(val) || val == 0 || val > 1440) { 
      parts <- strsplit(filename, "_")[[1]]
      mins <- grep("^\\d+min$", parts, value = TRUE)
      if (length(mins) > 0) {
        val <- as.numeric(gsub("min", "", mins[1]))
      } else {
        val <- if (!is_evt || grepl("shyreg|spatial", filename, ignore.case = TRUE)) 15 else 1 
      }
    }
    return(if(is.na(val) || val == 0) 1 else val)
  }
  
  parse_time_meta <- function(filename) {
    parts <- strsplit(filename, "_")[[1]]
    is_evt <- grepl("Evt", filename, fixed = TRUE)
    duree <- 0
    date_deb <- as.POSIXct("1900-01-01", tz="UTC")
    
    if (is_evt) {
      if (length(parts) >= 5) {
        suppressWarnings({
          date_deb <- strptime(gsub("[^0-9]", "", parts[4]), format = "%Y%m%d%H%M", tz = "UTC")
          h_m <- strsplit(parts[5], "h")[[1]]
          if(length(h_m) >= 2) duree <- (as.numeric(h_m[1]) + as.numeric(substr(h_m[2], 1, 2)) / 60) * 3600
        })
      }
    } else {
      if (length(parts) >= 8) {
        duree <- (as.numeric(parts[7]) + as.numeric(parts[8]) / 60) * 3600
      }
    }
    list(date_deb = as.POSIXct(date_deb), duree = duree, is_evt = is_evt)
  }
  
  create_regex_safe <- function(str) {
    gsub("([.|()\\^{}+$*?]|\\[|\\])", "\\\\\\1", str)
  }
  
  parse_telemac_sortie <- function(file_path) {
    message(paste("    [DEBUG]", Sys.time(), "- Lecture sortie:", basename(file_path)))
    if (!file.exists(file_path)) return(NULL)
    
    past_ts <- NA
    dir_path <- dirname(file_path)
    base_name <- basename(file_path)
    cas_name_candidate <- sub("_\\d{4}-\\d{2}-\\d{2}-\\d{2}h\\d{2}min\\d{2}s\\.sortie$", "", base_name)
    
    if (cas_name_candidate == base_name) {
      cas_name_candidate <- substr(base_name, 1, nchar(base_name) - 30)
    }
    
    cas_files <- list.files(dir_path, pattern = paste0("^", create_regex_safe(cas_name_candidate)), full.names = TRUE)
    cas_files <- cas_files[!grepl("\\.sortie$", cas_files)]
    
    if (length(cas_files) > 0) {
      lines_cas <- readLines(cas_files[1], warn = FALSE)
      ts_idx_cas <- grep("TIME STEP|PAS DE TEMPS", lines_cas, ignore.case = TRUE)[1]
      if (!is.na(ts_idx_cas)) {
        raw_line <- lines_cas[ts_idx_cas]
        extracted_ts <- gsub(".*(?:STEP|TEMPS)[^0-9.]*([0-9.]+).*", "\\1", raw_line, ignore.case = TRUE)
        past_ts <- as.numeric(extracted_ts)
        if(!is.na(past_ts)) message(paste("    [INFO] TIME STEP lu dans .cas :", past_ts))
      }
    }
    
    lines <- readLines(file_path, warn = FALSE)
    
    if (is.na(past_ts)) {
      ts_idx <- grep("TIME STEP|PAS DE TEMPS", lines, useBytes = TRUE, ignore.case = TRUE)[1]
      if (!is.na(ts_idx)) {
        raw_line <- lines[ts_idx]
        extracted_ts <- gsub(".*(?:STEP|TEMPS)[^0-9.]*([0-9.]+).*", "\\1", raw_line, ignore.case = TRUE)
        past_ts <- as.numeric(extracted_ts)
        if(!is.na(past_ts)) message(paste("    [INFO] TIME STEP lu dans .sortie :", past_ts))
      }
    }
    
    if (is.na(past_ts)) {
      message("    [WARN] TIME STEP introuvable. Valeur par défaut 1.0 utilisée.")
      past_ts <- 1.0
    }
    
    idx_interest <- grep("ITERATION|FLUX BOUNDARY|CULVERT|RUNOFF_SCS_CN|VOLUME IN THE DOMAIN|ADDITIONAL VOLUME", lines, useBytes = TRUE)
    if (length(idx_interest) == 0) return(NULL)
    ls <- lines[idx_interest]
    
    clean_numeric <- function(txt) {
      txt <- gsub("[*]", "", txt)
      suppressWarnings(as.numeric(txt))
    }
    
    mots_cles <- tribble(
      ~key, ~start, ~end,
      "ITERATION ", 12, 22,
      "RUNOFF_SCS_CN : ACCUMULATED RAINFALL", 44, 61,
      "VOLUME IN THE DOMAIN", 28, 44,
      "ADDITIONAL VOLUME DUE TO SOURCE TERMS", 44, 61
    )
    
    flux_raw <- grep("FLUX BOUNDARY", ls, value = TRUE, useBytes = TRUE)
    flux_keys <- unique(sub("^\\s*(FLUX BOUNDARY\\s+\\d+).*", "\\1", flux_raw))
    
    cul_raw <- grep("CULVERT", ls, value = TRUE, useBytes = TRUE)
    culvert_keys <- unique(sub("^\\s*(CULVERT\\s+\\d+).*", "\\1", cul_raw))
    
    extra <- tibble(key = c(flux_keys, culvert_keys)) %>%
      mutate(start = if_else(grepl("CULVERT", key), 37, 62), end = if_else(grepl("CULVERT", key), 62, 42)) # Fix intervalle si nécessaire
    
    all_keys <- bind_rows(mots_cles, extra)
    ref_idx <- grep("ITERATION ", ls, useBytes = TRUE)
    res <- matrix(NA, nrow = length(ref_idx), ncol = nrow(all_keys))
    colnames(res) <- all_keys$key
    
    for (i in 1:nrow(mots_cles)) {
      m <- grep(mots_cles$key[i], ls, fixed = TRUE, useBytes = TRUE)
      if (length(m) > 0) {
        vals <- clean_numeric(substr(ls[m], mots_cles$start[i], mots_cles$end[i]))
        indices <- findInterval(m, ref_idx)
        valid <- indices > 0 & indices <= nrow(res)
        res[indices[valid], mots_cles$key[i]] <- vals[valid]
      }
    }
    
    if (length(flux_keys) > 0) {
      m_flux <- grep("FLUX BOUNDARY", ls, fixed = TRUE, useBytes = TRUE)
      if (length(m_flux) > 0) {
        txt_flux <- ls[m_flux]
        vals_flux <- clean_numeric(substr(txt_flux, 25, 42))
        keys_flux <- sub("^\\s*(FLUX BOUNDARY\\s+\\d+).*", "\\1", txt_flux)
        
        rows <- findInterval(m_flux, ref_idx)
        cols <- match(keys_flux, colnames(res))
        valid <- !is.na(cols) & rows > 0 & rows <= nrow(res)
        res[cbind(rows[valid], cols[valid])] <- vals_flux[valid]
      }
    }
    
    if (length(culvert_keys) > 0) {
      m_cul <- grep("CULVERT", ls, fixed = TRUE, useBytes = TRUE)
      if (length(m_cul) > 0) {
        txt_cul <- ls[m_cul]
        vals_cul <- clean_numeric(substr(txt_cul, 37, 62))
        keys_cul <- sub("^\\s*(CULVERT\\s+\\d+).*", "\\1", txt_cul)
        
        rows <- findInterval(m_cul, ref_idx)
        cols <- match(keys_cul, colnames(res))
        valid <- !is.na(cols) & rows > 0 & rows <= nrow(res)
        res[cbind(rows[valid], cols[valid])] <- vals_cul[valid]
      }
    }
    
    df <- as.data.frame(res)
    df$Past_Ts <- past_ts
    df <- df[!is.na(df[, 1]), ] 
    
    message(paste("    [DEBUG] Parsing fini. Lignes:", nrow(df)))
    gc() 
    return(df)
  }
  
  # --- 2. INITIALISATION ---
  message(paste("[INFO]", Sys.time(), "Start plot_sc_obs_oh"))
  sc_list <- list.files(dsnlayerC, pattern = "SC.txt")
  list_sortie <- list.files(dsnlayerC, pattern = "s.sortie")
  list_sortie <- list_sortie[!grepl("\\.txt$", list_sortie)]
  
  # Construction d'une liste unique de tous les scénarios (racines) détectés
  all_cases <- list()
  
  # 1. Scénarios issus des SC.txt
  for (f_sc in sc_list) {
    rac <- sub("SC\\.txt$", "", f_sc)
    out_matches <- list_sortie[grep(create_regex_safe(rac), list_sortie)]
    all_cases[[rac]] <- list(f_sc = f_sc, f_outs = out_matches)
  }
  
  # 2. Ajout des scénarios orphelins (qui n'ont qu'un .sortie et pas de SC.txt)
  for (f_out in list_sortie) {
    already_matched <- FALSE
    for (cas in all_cases) {
      if (f_out %in% cas$f_outs) already_matched <- TRUE
    }
    if (!already_matched) {
      rac <- sub("_\\d{4}-\\d{2}-\\d{2}-\\d{2}h\\d{2}min\\d{2}s\\.sortie$", "", f_out)
      rac <- sub("\\.sortie$", "", rac)
      if(!endsWith(rac, "_")) rac <- paste0(rac, "_")
      if (is.null(all_cases[[rac]])) {
        all_cases[[rac]] <- list(f_sc = NULL, f_outs = f_out)
      } else {
        all_cases[[rac]]$f_outs <- unique(c(all_cases[[rac]]$f_outs, f_out))
      }
    }
  }
  
  if (length(all_cases) == 0) return(message("Aucun SC.txt ni fichier .sortie trouvé."))
  
  path_shp <- file.path(dsnlayerC, "SectControl_Select.shp")
  if(file.exists(path_shp)) sc_sf <- st_read(path_shp, quiet = TRUE) %>% mutate(ID = as.character(ID))
  
  contour_aire <- st_read(file.path(dsnlayerC, "Step_Cartino2d_1_Contour.shp"), quiet = TRUE)
  aire_val <- as.numeric(st_area(contour_aire))
  dsnscobs <- file.path(dsnlayer, "_SectionsControles", contour$NOMPOST)
  
  cache_sortie <- list()      
  cache_rain_plots <- list()  
  
  # --- 3. BOUCLE PRINCIPALE ---
  
  for (racine in names(all_cases)) {
    message(paste("----------------------------------------------------------------"))
    message(paste("[STEP] Traitement Racine :", racine))
    
    f_sc <- all_cases[[racine]]$f_sc
    f_out_match <- all_cases[[racine]]$f_outs
    
    has_sc <- !is.null(f_sc) && file.exists(file.path(dsnlayerC, f_sc))
    pseudo_f_sc <- if (has_sc) f_sc else paste0(racine, "SC.txt") # Utilisé pour le parsing nom
    racine_nom <- if (has_sc) tools::file_path_sans_ext(f_sc) else racine
    
    if (length(f_out_match) > 1) {
      dates_out <- sub(".*\\.cas_(\\d{4}-\\d{2}-\\d{2}-\\d{2}h\\d{2}min\\d{2}s)\\.sortie", "\\1", f_out_match)
      f_out_match <- f_out_match[which.max(as.POSIXct(dates_out, format = "%Y-%m-%d-%Hh%Mmin%Ss"))]
    }
    
    if (length(f_out_match) == 0) {
      message("  -> [WARN] .sortie introuvable pour ce cas.")
      tab_out <- NULL
    } else {
      if (is.null(cache_sortie[[f_out_match]])) {
        cache_sortie[[f_out_match]] <- parse_telemac_sortie(file.path(dsnlayerC, f_out_match))
      }
      tab_out <- cache_sortie[[f_out_match]]
    }
    
    meta <- parse_time_meta(pseudo_f_sc)
    rain_objs <- NULL 
    
    if (!is.null(tab_out)) {
      dir_pluie <- tryCatch(file.path(dsnPluie, contour$NOMPOST), error = function(e) NULL)
      file_brute <- NA
      
      if (!is.null(dir_pluie) && dir.exists(dir_pluie)) {
        all_brutes <- list.files(dir_pluie, pattern = "^(brute)_.*\\.txt$", full.names = TRUE)
        if (length(all_brutes) > 0) {
          name_sc_split <- strsplit(pseudo_f_sc,"_")[[1]]
          name_sc_brute <- do.call(paste, c(as.list(name_sc_split[1:5]), sep = "_"))
          clean_sc <- sub("SC\\.txt$", "", name_sc_brute)
          parts_sc <- strsplit(clean_sc, "_")[[1]]
          
          dur_token <- grep("^\\d+min$", parts_sc, value = TRUE)
          if(length(dur_token) == 0) dur_token <- "IGNORE_ME" else dur_token <- dur_token[1]
          
          evt_token <- grep("^Evts\\d+", parts_sc, value = TRUE)
          if(length(evt_token) == 0) evt_token <- "IGNORE_ME" else evt_token <- evt_token[1]
          candidates <- all_brutes
          if(evt_token== "IGNORE_ME") candidates <- NULL
          
          if (dur_token != "IGNORE_ME") {
            has_dur <- grepl(paste0("_", dur_token, "([_.]|$)"), basename(candidates)) | grepl(dur_token, basename(candidates), fixed = TRUE)
            if (any(has_dur)) candidates <- candidates[has_dur]
          }
          if (evt_token != "IGNORE_ME") {
            has_evt <- grepl(evt_token, basename(candidates))
            if (any(has_evt)) candidates <- candidates[has_evt]
          }
          if (length(candidates) > 0) {
            scores <- sapply(candidates, function(cand) {
              parts_cand <- strsplit(basename(cand), "_")[[1]]
              sum(parts_cand %in% parts_sc)
            })
            file_brute <- candidates[which.max(scores)]
            message(paste("    -> [BRUTE] Trouvé:", basename(file_brute)))
          }
        }
      }
      
      cache_key_rain <- paste0(f_out_match, ifelse(!is.na(file_brute), basename(file_brute), "NONE"))
      
      if (is.null(cache_rain_plots[[cache_key_rain]])) {
        past_ts <- tab_out$Past_Ts[1]
        if (is.na(past_ts)) past_ts <- 1.0
        
        if (nrow(tab_out) > 1) {
          pas_exp <- median(diff(tab_out[[1]]), na.rm = TRUE) * past_ts
        } else {
          pas_exp <- past_ts
        }
        if (is.na(pas_exp) || pas_exp <= 0) pas_exp <- 1.0
        message(paste("    -> [INFO] Pas d'export:", pas_exp, "s"))
        
        t_axis <- meta$date_deb + (tab_out[[1]] * past_ts)
        
        acc_rain <- replace_na(tab_out[["RUNOFF_SCS_CN : ACCUMULATED RAINFALL"]], 0)
        inc_rain <- c(acc_rain[1], diff(acc_rain))
        inc_rain[inc_rain < 0] <- 0 
        
        df_pluie <- data.frame(
          Time = t_axis,
          Intensite = inc_rain * 1000 * 3600 / pas_exp, 
          PNette = replace_na(tab_out[["ADDITIONAL VOLUME DUE TO SOURCE TERMS"]], 0) / aire_val * 1000 * 3600 / past_ts,
          CumulBrut = replace_na(tab_out[["RUNOFF_SCS_CN : ACCUMULATED RAINFALL"]], 0) * 1000, 
          CumulNet = cumsum(replace_na(tab_out[["ADDITIONAL VOLUME DUE TO SOURCE TERMS"]], 0) / aire_val * pas_exp / past_ts) * 1000,
          NomBrut = "Pluie brute", NomNet = "Pluie nette"
        )
        bar_shift=0
        if (!is.na(file_brute) && file.exists(file_brute)) {
          tab_brute <- read.table(file_brute, header = TRUE)
          rain_step <- get_rain_step(basename(file_brute), is_evt = meta$is_evt)
          message(paste("    -> [BRUTE] Step:", rain_step, "min"))
          bar_shift <- pas_exp 
          col_val <- if("Val" %in% names(tab_brute)) "Val" else names(tab_brute)[2]
          
          n_out <- nrow(df_pluie)
          if (nrow(tab_brute) >= n_out) {
            raw_vals <- tab_brute[[col_val]][1:n_out]
          } else {
            raw_vals <- c(tab_brute[[col_val]], rep(0, n_out - nrow(tab_brute)))
          }
          
          df_pluie$CumulBrut <- cumsum(raw_vals / rain_step)
          df_pluie$Intensite <- raw_vals * 3600 / (pas_exp * rain_step)
        }
        
        df_pluie <- df_pluie %>% filter(!is.na(Time))
        
        if (nrow(df_pluie) > 0) {
          df_hyeto <- df_pluie %>% dplyr::select(Time, `Pluie brute` = Intensite, `Pluie nette` = PNette) %>%
            pivot_longer(cols = c("Pluie brute", "Pluie nette"), names_to = "Nom", values_to = "Val")
          df_hyeto$Nom <- factor(df_hyeto$Nom, levels = c("Pluie brute", "Pluie nette"))
          
          cols_hyeto <- c("Pluie brute" = "#33FFFF", "Pluie nette" = "#9933FF")
          df_hyeto$Time[df_hyeto$Nom=="Pluie brute"]=df_hyeto$Time[df_hyeto$Nom=="Pluie brute"]+bar_shift
          
          g1 <- ggplotly(ggplot(df_hyeto, aes(x = Time, y = Val, fill = Nom)) +
                           geom_col(position = "identity", width = pas_exp) +
                           scale_fill_manual(name="", values = cols_hyeto) + 
                           theme_bw() + ylab("Intensité [mm/h]") + 
                           theme(axis.title.x = element_blank(), legend.title = element_blank()), dynamicTicks = TRUE)
          
          df_cumul <- df_pluie %>% dplyr::select(Time, `Cumul brut` = CumulBrut, `Cumul net` = CumulNet) %>%
            pivot_longer(cols = c("Cumul brut", "Cumul net"), names_to = "Nom", values_to = "Val")
          cols_cumul <- c("Cumul brut" = "#33FFFF", "Cumul net" = "#9933FF")
          
          g3 <- ggplotly(ggplot(df_cumul, aes(x = Time, y = Val, color = Nom)) +
                           geom_line(size = 1.1) +
                           scale_color_manual(name="", values = cols_cumul) +
                           theme_bw() + ylab("Cumul [mm]") + 
                           theme(axis.title.x = element_blank(), legend.title = element_blank()), dynamicTicks = TRUE)
          
          cache_rain_plots[[cache_key_rain]] <- list(g1 = g1, g3 = g3, t_axis = t_axis)
        } else {
          cache_rain_plots[[cache_key_rain]] <- NULL
        }
      }
      rain_objs <- cache_rain_plots[[cache_key_rain]]
    } else {
      rain_objs <- NULL
    }
    
    if (is.null(rain_objs)) {
      message("    [WARN] Données pluie invalides ou manquantes.")
      rain_objs <- list(g1 = NULL, g3 = NULL, t_axis = NULL)
    }
    
    # ---------------------------------------------------------
    # PARTIE Q, Z, H (Dépend strictement de la présence de SC.txt)
    # ---------------------------------------------------------
    if (has_sc) {
      message("  -> [DATA] SC + Obs")
      cl_names <- c("nom", "numsec", "Time", "Q", "Zmoy", "Zmax", "Zmin", "Hmax", "Htot")
      tab_sc <- read.csv(file.path(dsnlayerC, f_sc), header = FALSE, sep = "", skip = 2, col.names = cl_names)
      tab_sc <- tab_sc %>% filter(as.numeric(Time) >= 60)
      tab_sc[, 2:9] <- lapply(tab_sc[, 2:9], as.numeric)
      tab_sc$DATE <- meta$date_deb + tab_sc$Time
      tab_sc$nom <- as.character(tab_sc$nom)
      if("Q" %in% names(sc_sf))  sc_sf =sc_sf[,-which(names(sc_sf)=="Q")]
      tab_sc <- inner_join(tab_sc, sc_sf, by = c("nom" = "ID"))
      
      sc_names <- unique(tab_sc$NOM) 
      sc_list_obs <- list.files(path = dsnscobs, full.names = TRUE)
      
      load_obs <- function(suffix, col_name) {
        files_match <- unlist(lapply(sc_names, function(x) grep(paste0(x, suffix), sc_list_obs, value = TRUE)))
        if (length(files_match) == 0) return(NULL)
        df_list <- lapply(files_match, read.csv)
        df_obs <- do.call(rbind, df_list)
        df_obs$Date <- as.POSIXct(ifelse(grepl(" ", df_obs$Date), df_obs$Date, paste(df_obs$Date, "00:00:00")), tz = "UTC")
        df_obs <- df_obs %>% filter(Date >= min(tab_sc$DATE) & Date <= max(tab_sc$DATE))
        if (nrow(df_obs) > 0) {
          colnames(df_obs) <- c("Date", col_name, "NOM")
          df_obs$NOM <- paste0(df_obs$NOM, "_obs") 
          df_obs$TYPE <- "dashed" 
          return(df_obs)
        }
        return(NULL)
      }
      
      obs_q <- load_obs("_Q.csv", "Q")
      df_q_all <- bind_rows(
        tab_sc %>% dplyr::select(Date = DATE, Q, NOM) %>% mutate(Q = abs(Q), TYPE = "solid"),
        if (!is.null(obs_q)) obs_q %>% mutate(Q = abs(Q), TYPE = "dashed") else NULL
      )
      
      obs_z <- load_obs("_Z.csv", "Z")
      df_z_all <- bind_rows(
        tab_sc %>% dplyr::select(Date = DATE, Z = Zmax, NOM) %>% mutate(Z = Z, TYPE = "solid"),
        if (!is.null(obs_z)) obs_z %>% mutate(TYPE = "dashed") else NULL
      )
      # FIX: Remplacement des 0 par NA dans la colonne Z
      df_z_all$Z[df_z_all$Z == 0] <- NA
      
      obs_h <- load_obs("_H.csv", "H")
      df_h_all <- bind_rows(
        tab_sc %>% dplyr::select(Date = DATE, H = Hmax, NOM) %>% mutate(TYPE = "solid"),
        if (!is.null(obs_h)) obs_h %>% mutate(TYPE = "dashed") else NULL
      )
      
      save_master_plot <- function(df_data, y_col, y_lab, output_path, title_suffix) {
        if (file.exists(output_path)) return()
        df_data <- df_data %>% filter(!is.na(Date)) %>% arrange(NOM, Date)
        if (nrow(df_data) == 0) return()
        test_obs = df_data %>% filter(TYPE != "solid")
        
        # S'assurer qu'il n'y a que des données non-NA avant le tracé
        df_data <- df_data %>% filter(!is.na(.data[[y_col]]))
        
        if(length(test_obs$Date) != 0) {
          g_main <- ggplot() +
            geom_line(data = df_data %>% filter(TYPE == "solid"), 
                      aes(x = Date, y = .data[[y_col]], color = NOM), 
                      linetype = "solid", size = 0.7) +
            geom_line(data = df_data %>% filter(TYPE != "solid"), 
                      aes(x = Date, y = .data[[y_col]], color = NOM), 
                      linetype = "dashed", size = 0.7) +
            theme_bw() + ylab(y_lab) + xlab("Date (heure météo)") + 
            theme(legend.title = element_blank())
        } else {
          g_main <- ggplot() +
            geom_line(data = df_data %>% filter(TYPE == "solid"), 
                      aes(x = Date, y = .data[[y_col]], color = NOM), 
                      linetype = "solid", size = 0.7) +
            theme_bw() + ylab(y_lab) + xlab("Date (heure météo)") + 
            theme(legend.title = element_blank())
        }
        
        titre <- paste0("Hyéto + ", title_suffix, " ", racine_nom, " (Pluie sur tout le bassin)")
        
        plots_list <- list()
        heights <- c()
        rows_val <- 0
        
        has_rain <- !is.null(rain_objs) && !is.null(rain_objs$g3) && !inherits(rain_objs$g3, "plotly_empty")
        
        if (has_rain) {
          plots_list[[1]] <- rain_objs$g3
          plots_list[[2]] <- rain_objs$g1
          plots_list[[3]] <- ggplotly(g_main, dynamicTicks = TRUE)
          heights <- c(0.1, 0.2, 0.7)
          rows_val <- 3
        } else {
          plots_list[[1]] <- ggplotly(g_main, dynamicTicks = TRUE)
          heights <- c(1)
          rows_val <- 1
        }
        
        p_final <- subplot(plots_list, nrows = rows_val, shareX = TRUE, heights = heights, titleY = TRUE) %>%
          layout(title = list(text = titre), hovermode = "closest", legend = list(title = list(text = "")))
        saveWidget(partial_bundle(p_final), file = output_path, selfcontained = TRUE, libdir = "lib")
      }
      
      message("  -> [EXPORT] HTML Q/Z/H")
      p_q_out <- file.path(dsnlayerC, paste0(racine_nom, "_PN_PB_Q_Obs.html"))
      p_z_out <- file.path(dsnlayerC, paste0(racine_nom, "_PN_PB_Z_Obs.html"))
      p_h_out <- file.path(dsnlayerC, paste0(racine_nom, "_PN_PB_H_Obs.html"))
      
      save_master_plot(df_q_all, "Q", "Débit [m3/s]", p_q_out, "Hydrogramme")
      save_master_plot(df_z_all, "Z", "Côte [m]", p_z_out, "Côtes")
      save_master_plot(df_h_all, "H", "Hauteur [m]", p_h_out, "Hauteur")
      
      message("  -> [GPKG] Qmax")
      tabmaxq <- tab_sc %>% group_by(nom) %>% filter(abs(Q) == max(abs(Q))) %>% slice(1)
      new_sc_sf <- sc_sf %>% 
        left_join(tabmaxq %>% dplyr::select(nom, Q, DATE, Zmoy, Hmax), by = c("ID" = "nom")) %>%
        mutate(Q = abs(Q), NomSecteur = basename(dsnlayerC))
      st_write(new_sc_sf, file.path(dsnlayerC, paste0(gsub("\\..*", "", f_sc), "_Qmax.gpkg")), driver = "GPKG", delete_dsn = TRUE, quiet = TRUE)
      file.copy(file.path(chem_routine, "C2D", Doss_Qml, "SC_Qmax.qml"), file.path(dsnlayerC, paste0(gsub("\\..*", "", f_sc), "_Qmax.qml")), overwrite = TRUE)
    } else {
      message("  -> [INFO] Pas de fichier SC.txt, graphes Q, Z, H ignorés.")
    }
    
    # ---------------------------------------------------------
    # PARTIE SORTIES: FLUX, OUVRAGES ET VOLUMES (.sortie direct)
    # ---------------------------------------------------------
    if (!is.null(tab_out)) {
      name_case_clean <- sub("\\.cas_.*", "", basename(f_out_match[1]))
      p_qs_out <- file.path(dsnlayerC, paste0(name_case_clean, "_PN_PB_QS_Obs.html"))
      p_qoh_out <- file.path(dsnlayerC, paste0(name_case_clean, "_PN_PB_QOH_Obs.html"))
      p_vqs_out <- file.path(dsnlayerC, paste0(name_case_clean, "_PN_PB_VQS_Obs.html"))
      
      if (!exists("t_axis") || is.null(t_axis)) t_axis <- meta$date_deb + (tab_out[[1]] * past_ts)
      
      # 1. Ajout VQS (Volume)
      if (!file.exists(p_vqs_out)) {
        
        # FIX : Récupérer le premier volume valide pour ne pas soustraire 0 si la première ligne est NA
        vol_domaine_raw <- tab_out[["VOLUME IN THE DOMAIN"]]
        val_ref_domaine <- vol_domaine_raw[!is.na(vol_domaine_raw)][1]
        if (is.na(val_ref_domaine)) val_ref_domaine <- 0
        
        # On remplace les éventuels NA par le vrai volume de référence initial
        vol_domaine <- replace_na(vol_domaine_raw, val_ref_domaine)
        vol_domaine <- vol_domaine - val_ref_domaine
        
        # Le volume de la source imprimé dans la sortie Telemac est celui d'1 seul pas de temps de calcul (past_ts)
        rate_injecte <- replace_na(tab_out[["ADDITIONAL VOLUME DUE TO SOURCE TERMS"]], 0) / past_ts
        vol_injecte <- cumsum(rate_injecte * pas_exp) 
        
        df_flux_only <- tab_out %>% dplyr::select(matches("FLUX BOUNDARY"))
        vol_sorties_cumul <- rep(0, nrow(tab_out))
        q_total <- rep(0, nrow(tab_out))
        
        if (ncol(df_flux_only) > 0) {
          df_flux_active <- df_flux_only %>% mutate(across(everything(), ~replace_na(., 0)))
          q_total <- rowSums(df_flux_active, na.rm=TRUE)
          # On utilise -q_total exactement comme l'ancien code (-TimeStep*sum(...))
          # Les flux sortants (<0 dans Telemac) deviennent des volumes positifs
          # ce qui maintient l'équilibre physique exact pour Domaine + Sorties
          vol_sorties_cumul <- cumsum(-q_total * pas_exp)
        }
        
        df_vol <- data.frame(
          Time = t_axis,
          Injecte = vol_injecte,
          Domaine = vol_domaine,
          `Domaine + Sorties` = vol_domaine + vol_sorties_cumul,
          check.names = FALSE
        ) %>% pivot_longer(-Time, names_to = "Variable", values_to = "Volume")
        
        # Ajout des volumes individuels des sorties
        if (exists("df_flux_active") && ncol(df_flux_active) > 0) {
          for(col_name in colnames(df_flux_active)) {
            clean_name <- sub("FLUX BOUNDARY", "Sortie", col_name)
            # Volume individuel (sortant = positif grâce au signe -)
            vol_indiv <- cumsum(-df_flux_active[[col_name]] * pas_exp)
            df_temp <- data.frame(Time = t_axis, Variable = clean_name, Volume = vol_indiv)
            df_vol <- bind_rows(df_vol, df_temp)
          }
        }
        
        g_vol <- ggplot(df_vol, aes(x = Time, y = Volume, color = Variable)) +
          geom_line() + theme_bw() + labs(y = "Volume [m3]") + 
          theme(legend.title = element_blank(), legend.position="right")
        
        # Graphique débit total de sortie (conservé en valeur absolue pour le visuel graphique)
        df_qs_total <- data.frame(Time = t_axis, Q = abs(q_total))
        g_qs_total <- ggplot(df_qs_total, aes(x=Time, y=Q, color="Débit Sortie Total")) + 
          geom_line() + 
          scale_color_manual(values=c("Débit Sortie Total"="red")) +
          theme_bw() + ylab("Débit Sortie Total [m3/s]") +
          theme(legend.title = element_blank(), legend.position="right")
        
        plots_vqs <- list()
        if (!is.null(rain_objs) && !is.null(rain_objs$g3) && !inherits(rain_objs$g3, "plotly_empty")) {
          plots_vqs <- list(rain_objs$g3, ggplotly(g_vol, dynamicTicks = TRUE), ggplotly(g_qs_total, dynamicTicks = TRUE))
          p_vqs <- subplot(plots_vqs, nrows = 3, shareX = TRUE, titleY = TRUE) %>%
            layout(title = paste("Bilan Volume -", racine_nom), legend = list(title = list(text = ""), orientation = "v"))
        } else {
          plots_vqs <- list(ggplotly(g_vol, dynamicTicks = TRUE), ggplotly(g_qs_total, dynamicTicks = TRUE))
          p_vqs <- subplot(plots_vqs, nrows = 2, shareX = TRUE, titleY = TRUE) %>%
            layout(title = paste("Bilan Volume -", racine_nom), legend = list(title = list(text = ""), orientation = "v"))
        }
        saveWidget(partial_bundle(p_vqs), file = p_vqs_out, selfcontained = TRUE, libdir = "lib")
      }
      
      # 2. Flux and Culverts (QS, QOH)
      df_flux <- tab_out %>% dplyr::select(matches("FLUX BOUNDARY|CULVERT"))
      keep_cols <- sapply(df_flux, function(x) !all(is.na(x)))
      df_flux <- df_flux[, keep_cols, drop=FALSE]
      
      if (ncol(df_flux) > 0) {
        df_long <- df_flux %>% mutate(Time = t_axis) %>% 
          pivot_longer(-Time) %>% filter(!is.na(Time)) 
        
        if (!file.exists(p_qs_out) && any(grepl("FLUX", df_long$name))) {
          # Valeur absolue uniquement pour l'affichage graphique du débit !
          g_qs <- ggplot(df_long %>% filter(grepl("FLUX", name)), aes(Time, abs(value), color = name)) + 
            geom_line() + theme_bw() + ylab("Débit [m3/s]") + xlab("Date (heure météo)") + theme(legend.title = element_blank())
          
          plots_list <- list()
          has_rain <- !is.null(rain_objs) && !is.null(rain_objs$g3) && !inherits(rain_objs$g3, "plotly_empty")
          if (has_rain) {
            plots_list[[1]] <- rain_objs$g3
            plots_list[[2]] <- rain_objs$g1
            plots_list[[3]] <- ggplotly(g_qs, dynamicTicks = TRUE)
            h <- c(0.1, 0.2, 0.7); r <- 3
          } else {
            plots_list[[1]] <- ggplotly(g_qs, dynamicTicks = TRUE)
            h <- c(1); r <- 1
          }
          p_qs <- subplot(plots_list, nrows = r, shareX = TRUE, heights = h, titleY=TRUE) %>%
            layout(title = paste("Sorties -", racine_nom), hovermode = "closest", legend = list(title = list(text = "")))
          saveWidget(partial_bundle(p_qs), file = p_qs_out, selfcontained = TRUE, libdir = "lib")
        }
        
        if (!file.exists(p_qoh_out) && any(grepl("CULVERT", df_long$name))) {
          g_qoh <- ggplot(df_long %>% filter(grepl("CULVERT", name)), aes(Time, abs(value), color = name)) + 
            geom_line() + theme_bw() + ylab("Débit [m3/s]") + xlab("Date (heure météo)") + theme(legend.title = element_blank())
          
          plots_list <- list()
          has_rain <- !is.null(rain_objs) && !is.null(rain_objs$g3) && !inherits(rain_objs$g3, "plotly_empty")
          if (has_rain) {
            plots_list[[1]] <- rain_objs$g3
            plots_list[[2]] <- rain_objs$g1
            plots_list[[3]] <- ggplotly(g_qoh, dynamicTicks = TRUE)
            h <- c(0.1, 0.2, 0.7); r <- 3
          } else {
            plots_list[[1]] <- ggplotly(g_qoh, dynamicTicks = TRUE)
            h <- c(1); r <- 1
          }
          p_qoh <- subplot(plots_list, nrows = r, shareX = TRUE, heights = h, titleY=TRUE) %>%
            layout(title = paste("Ouvrages -", racine_nom), hovermode = "closest", legend = list(title = list(text = "")))
          saveWidget(partial_bundle(p_qoh), file = p_qoh_out, selfcontained = TRUE, libdir = "lib")
        }
      }
    }
  }
  
  if (dir.exists(file.path(dsnlayerC, "lib"))) unlink(file.path(dsnlayerC, "lib"), recursive = TRUE)
  message("[FIN] Terminé.")
}
plot_sc_obs_oh <- function() {
  library(tidyverse)
  library(sf)
  library(plotly)
  library(htmlwidgets)
  library(tools)
  
  # --- 1. FONCTIONS UTILITAIRES ---
  
  get_rain_step <- function(filename, is_evt = TRUE) {
    val <- as.numeric(gsub(".*[[:punct:]](\\d+)min([[:punct:]]|$).*", "\\1", filename))
    if (is.na(val) || val == 0 || val > 1440) { 
      parts <- strsplit(filename, "_")[[1]]
      mins <- grep("^\\d+min$", parts, value = TRUE)
      if (length(mins) > 0) {
        val <- as.numeric(gsub("min", "", mins[1]))
      } else {
        val <- if (!is_evt || grepl("shyreg|spatial", filename, ignore.case = TRUE)) 15 else 1 
      }
    }
    return(if(is.na(val) || val == 0) 1 else val)
  }
  
  parse_time_meta <- function(filename) {
    parts <- strsplit(filename, "_")[[1]]
    is_evt <- grepl("Evt", filename, fixed = TRUE)
    duree <- 0
    date_deb <- as.POSIXct("1900-01-01", tz="UTC")
    
    if (is_evt) {
      if (length(parts) >= 5) {
        suppressWarnings({
          date_deb <- strptime(gsub("[^0-9]", "", parts[4]), format = "%Y%m%d%H%M", tz = "UTC")
          h_m <- strsplit(parts[5], "h")[[1]]
          if(length(h_m) >= 2) duree <- (as.numeric(h_m[1]) + as.numeric(substr(h_m[2], 1, 2)) / 60) * 3600
        })
      }
    } else {
      if (length(parts) >= 8) {
        duree <- (as.numeric(parts[7]) + as.numeric(parts[8]) / 60) * 3600
      }
    }
    list(date_deb = as.POSIXct(date_deb), duree = duree, is_evt = is_evt)
  }
  
  create_regex_safe <- function(str) {
    gsub("([.|()\\^{}+$*?]|\\[|\\])", "\\\\\\1", str)
  }
  
  parse_telemac_sortie <- function(file_path) {
    message(paste("    [DEBUG]", Sys.time(), "- Lecture sortie:", basename(file_path)))
    if (!file.exists(file_path)) return(NULL)
    
    past_ts <- NA
    dir_path <- dirname(file_path)
    base_name <- basename(file_path)
    cas_name_candidate <- sub("_\\d{4}-\\d{2}-\\d{2}-\\d{2}h\\d{2}min\\d{2}s\\.sortie$", "", base_name)
    
    if (cas_name_candidate == base_name) {
      cas_name_candidate <- substr(base_name, 1, nchar(base_name) - 30)
    }
    
    cas_files <- list.files(dir_path, pattern = paste0("^", create_regex_safe(cas_name_candidate)), full.names = TRUE)
    cas_files <- cas_files[!grepl("\\.sortie$", cas_files)]
    
    if (length(cas_files) > 0) {
      lines_cas <- readLines(cas_files[1], warn = FALSE)
      ts_idx_cas <- grep("TIME STEP|PAS DE TEMPS", lines_cas, ignore.case = TRUE)[1]
      if (!is.na(ts_idx_cas)) {
        raw_line <- lines_cas[ts_idx_cas]
        extracted_ts <- gsub(".*(?:STEP|TEMPS)[^0-9.]*([0-9.]+).*", "\\1", raw_line, ignore.case = TRUE)
        past_ts <- as.numeric(extracted_ts)
        if(!is.na(past_ts)) message(paste("    [INFO] TIME STEP lu dans .cas :", past_ts))
      }
    }
    
    lines <- readLines(file_path, warn = FALSE)
    
    if (is.na(past_ts)) {
      ts_idx <- grep("TIME STEP|PAS DE TEMPS", lines, useBytes = TRUE, ignore.case = TRUE)[1]
      if (!is.na(ts_idx)) {
        raw_line <- lines[ts_idx]
        extracted_ts <- gsub(".*(?:STEP|TEMPS)[^0-9.]*([0-9.]+).*", "\\1", raw_line, ignore.case = TRUE)
        past_ts <- as.numeric(extracted_ts)
        if(!is.na(past_ts)) message(paste("    [INFO] TIME STEP lu dans .sortie :", past_ts))
      }
    }
    
    if (is.na(past_ts)) {
      message("    [WARN] TIME STEP introuvable. Valeur par défaut 1.0 utilisée.")
      past_ts <- 1.0
    }
    
    idx_interest <- grep("ITERATION|FLUX BOUNDARY|CULVERT|RUNOFF_SCS_CN|VOLUME IN THE DOMAIN|ADDITIONAL VOLUME", lines, useBytes = TRUE)
    if (length(idx_interest) == 0) return(NULL)
    ls <- lines[idx_interest]
    
    clean_numeric <- function(txt) {
      txt <- gsub("[*]", "", txt)
      suppressWarnings(as.numeric(txt))
    }
    
    mots_cles <- tribble(
      ~key, ~start, ~end,
      "ITERATION ", 12, 22,
      "RUNOFF_SCS_CN : ACCUMULATED RAINFALL", 44, 61,
      "VOLUME IN THE DOMAIN", 28, 44,
      "ADDITIONAL VOLUME DUE TO SOURCE TERMS", 44, 61
    )
    
    flux_raw <- grep("FLUX BOUNDARY", ls, value = TRUE, useBytes = TRUE)
    flux_keys <- unique(sub("^\\s*(FLUX BOUNDARY\\s+\\d+).*", "\\1", flux_raw))
    
    cul_raw <- grep("CULVERT", ls, value = TRUE, useBytes = TRUE)
    culvert_keys <- unique(sub("^\\s*(CULVERT\\s+\\d+).*", "\\1", cul_raw))
    
    extra <- tibble(key = c(flux_keys, culvert_keys)) %>%
      mutate(start = if_else(grepl("CULVERT", key), 37, 62), end = if_else(grepl("CULVERT", key), 62, 42)) # Fix intervalle si nécessaire
    
    all_keys <- bind_rows(mots_cles, extra)
    ref_idx <- grep("ITERATION ", ls, useBytes = TRUE)
    res <- matrix(NA, nrow = length(ref_idx), ncol = nrow(all_keys))
    colnames(res) <- all_keys$key
    
    for (i in 1:nrow(mots_cles)) {
      m <- grep(mots_cles$key[i], ls, fixed = TRUE, useBytes = TRUE)
      if (length(m) > 0) {
        vals <- clean_numeric(substr(ls[m], mots_cles$start[i], mots_cles$end[i]))
        indices <- findInterval(m, ref_idx)
        valid <- indices > 0 & indices <= nrow(res)
        res[indices[valid], mots_cles$key[i]] <- vals[valid]
      }
    }
    
    if (length(flux_keys) > 0) {
      m_flux <- grep("FLUX BOUNDARY", ls, fixed = TRUE, useBytes = TRUE)
      if (length(m_flux) > 0) {
        txt_flux <- ls[m_flux]
        vals_flux <- clean_numeric(substr(txt_flux, 25, 42))
        keys_flux <- sub("^\\s*(FLUX BOUNDARY\\s+\\d+).*", "\\1", txt_flux)
        
        rows <- findInterval(m_flux, ref_idx)
        cols <- match(keys_flux, colnames(res))
        valid <- !is.na(cols) & rows > 0 & rows <= nrow(res)
        res[cbind(rows[valid], cols[valid])] <- vals_flux[valid]
      }
    }
    
    if (length(culvert_keys) > 0) {
      m_cul <- grep("CULVERT", ls, fixed = TRUE, useBytes = TRUE)
      if (length(m_cul) > 0) {
        txt_cul <- ls[m_cul]
        vals_cul <- clean_numeric(substr(txt_cul, 37, 62))
        keys_cul <- sub("^\\s*(CULVERT\\s+\\d+).*", "\\1", txt_cul)
        
        rows <- findInterval(m_cul, ref_idx)
        cols <- match(keys_cul, colnames(res))
        valid <- !is.na(cols) & rows > 0 & rows <= nrow(res)
        res[cbind(rows[valid], cols[valid])] <- vals_cul[valid]
      }
    }
    
    df <- as.data.frame(res)
    df$Past_Ts <- past_ts
    df <- df[!is.na(df[, 1]), ] 
    
    message(paste("    [DEBUG] Parsing fini. Lignes:", nrow(df)))
    gc() 
    return(df)
  }
  
  # --- 2. INITIALISATION ---
  message(paste("[INFO]", Sys.time(), "Start plot_sc_obs_oh"))
  sc_list <- list.files(dsnlayerC, pattern = "SC.txt")
  list_sortie <- list.files(dsnlayerC, pattern = "s.sortie")
  list_sortie <- list_sortie[!grepl("\\.txt$", list_sortie)]
  
  # Construction d'une liste unique de tous les scénarios (racines) détectés
  all_cases <- list()
  
  # 1. Scénarios issus des SC.txt
  for (f_sc in sc_list) {
    rac <- sub("SC\\.txt$", "", f_sc)
    out_matches <- list_sortie[grep(create_regex_safe(rac), list_sortie)]
    all_cases[[rac]] <- list(f_sc = f_sc, f_outs = out_matches)
  }
  
  # 2. Ajout des scénarios orphelins (qui n'ont qu'un .sortie et pas de SC.txt)
  for (f_out in list_sortie) {
    already_matched <- FALSE
    for (cas in all_cases) {
      if (f_out %in% cas$f_outs) already_matched <- TRUE
    }
    if (!already_matched) {
      rac <- sub("_\\d{4}-\\d{2}-\\d{2}-\\d{2}h\\d{2}min\\d{2}s\\.sortie$", "", f_out)
      rac <- sub("\\.sortie$", "", rac)
      if(!endsWith(rac, "_")) rac <- paste0(rac, "_")
      if (is.null(all_cases[[rac]])) {
        all_cases[[rac]] <- list(f_sc = NULL, f_outs = f_out)
      } else {
        all_cases[[rac]]$f_outs <- unique(c(all_cases[[rac]]$f_outs, f_out))
      }
    }
  }
  
  if (length(all_cases) == 0) return(message("Aucun SC.txt ni fichier .sortie trouvé."))
  
  path_shp <- file.path(dsnlayerC, "SectControl_Select.shp")
  if(file.exists(path_shp)) sc_sf <- st_read(path_shp, quiet = TRUE) %>% mutate(ID = as.character(ID))
  
  contour_aire <- st_read(file.path(dsnlayerC, "Step_Cartino2d_1_Contour.shp"), quiet = TRUE)
  aire_val <- as.numeric(st_area(contour_aire))
  dsnscobs <- file.path(dsnlayer, "_SectionsControles", contour$NOMPOST)
  
  cache_sortie <- list()      
  cache_rain_plots <- list()  
  
  # --- 3. BOUCLE PRINCIPALE ---
  
  for (racine in names(all_cases)) {
    message(paste("----------------------------------------------------------------"))
    message(paste("[STEP] Traitement Racine :", racine))
    
    f_sc <- all_cases[[racine]]$f_sc
    f_out_match <- all_cases[[racine]]$f_outs
    
    has_sc <- !is.null(f_sc) && file.exists(file.path(dsnlayerC, f_sc))
    pseudo_f_sc <- if (has_sc) f_sc else paste0(racine, "SC.txt") # Utilisé pour le parsing nom
    racine_nom <- if (has_sc) tools::file_path_sans_ext(f_sc) else racine
    
    if (length(f_out_match) > 1) {
      dates_out <- sub(".*\\.cas_(\\d{4}-\\d{2}-\\d{2}-\\d{2}h\\d{2}min\\d{2}s)\\.sortie", "\\1", f_out_match)
      f_out_match <- f_out_match[which.max(as.POSIXct(dates_out, format = "%Y-%m-%d-%Hh%Mmin%Ss"))]
    }
    
    if (length(f_out_match) == 0) {
      message("  -> [WARN] .sortie introuvable pour ce cas.")
      tab_out <- NULL
    } else {
      if (is.null(cache_sortie[[f_out_match]])) {
        cache_sortie[[f_out_match]] <- parse_telemac_sortie(file.path(dsnlayerC, f_out_match))
      }
      tab_out <- cache_sortie[[f_out_match]]
    }
    
    meta <- parse_time_meta(pseudo_f_sc)
    rain_objs <- NULL 
    
    if (!is.null(tab_out)) {
      dir_pluie <- tryCatch(file.path(dsnPluie, contour$NOMPOST), error = function(e) NULL)
      file_brute <- NA
      
      if (!is.null(dir_pluie) && dir.exists(dir_pluie)) {
        all_brutes <- list.files(dir_pluie, pattern = "^(brute)_.*\\.txt$", full.names = TRUE)
        if (length(all_brutes) > 0) {
          name_sc_split <- strsplit(pseudo_f_sc,"_")[[1]]
          name_sc_brute <- do.call(paste, c(as.list(name_sc_split[1:5]), sep = "_"))
          clean_sc <- sub("SC\\.txt$", "", name_sc_brute)
          parts_sc <- strsplit(clean_sc, "_")[[1]]
          
          dur_token <- grep("^\\d+min$", parts_sc, value = TRUE)
          if(length(dur_token) == 0) dur_token <- "IGNORE_ME" else dur_token <- dur_token[1]
          
          evt_token <- grep("^Evts\\d+", parts_sc, value = TRUE)
          if(length(evt_token) == 0) evt_token <- "IGNORE_ME" else evt_token <- evt_token[1]
          candidates <- all_brutes
          if(evt_token== "IGNORE_ME") candidates <- NULL
          
          if (dur_token != "IGNORE_ME") {
            has_dur <- grepl(paste0("_", dur_token, "([_.]|$)"), basename(candidates)) | grepl(dur_token, basename(candidates), fixed = TRUE)
            if (any(has_dur)) candidates <- candidates[has_dur]
          }
          if (evt_token != "IGNORE_ME") {
            has_evt <- grepl(evt_token, basename(candidates))
            if (any(has_evt)) candidates <- candidates[has_evt]
          }
          if (length(candidates) > 0) {
            scores <- sapply(candidates, function(cand) {
              parts_cand <- strsplit(basename(cand), "_")[[1]]
              sum(parts_cand %in% parts_sc)
            })
            file_brute <- candidates[which.max(scores)]
            message(paste("    -> [BRUTE] Trouvé:", basename(file_brute)))
          }
        }
      }
      
      cache_key_rain <- paste0(f_out_match, ifelse(!is.na(file_brute), basename(file_brute), "NONE"))
      
      if (is.null(cache_rain_plots[[cache_key_rain]])) {
        past_ts <- tab_out$Past_Ts[1]
        if (is.na(past_ts)) past_ts <- 1.0
        
        if (nrow(tab_out) > 1) {
          pas_exp <- median(diff(tab_out[[1]]), na.rm = TRUE) * past_ts
        } else {
          pas_exp <- past_ts
        }
        if (is.na(pas_exp) || pas_exp <= 0) pas_exp <- 1.0
        message(paste("    -> [INFO] Pas d'export:", pas_exp, "s"))
        
        t_axis <- meta$date_deb + (tab_out[[1]] * past_ts)
        
        acc_rain <- replace_na(tab_out[["RUNOFF_SCS_CN : ACCUMULATED RAINFALL"]], 0)
        inc_rain <- c(acc_rain[1], diff(acc_rain))
        inc_rain[inc_rain < 0] <- 0 
        
        df_pluie <- data.frame(
          Time = t_axis,
          Intensite = inc_rain * 1000 * 3600 / pas_exp, 
          PNette = replace_na(tab_out[["ADDITIONAL VOLUME DUE TO SOURCE TERMS"]], 0) / aire_val * 1000 * 3600 / past_ts,
          CumulBrut = replace_na(tab_out[["RUNOFF_SCS_CN : ACCUMULATED RAINFALL"]], 0) * 1000, 
          CumulNet = cumsum(replace_na(tab_out[["ADDITIONAL VOLUME DUE TO SOURCE TERMS"]], 0) / aire_val * pas_exp / past_ts) * 1000,
          NomBrut = "Pluie brute", NomNet = "Pluie nette"
        )
        bar_shift=0
        if (!is.na(file_brute) && file.exists(file_brute)) {
          tab_brute <- read.table(file_brute, header = TRUE)
          rain_step <- get_rain_step(basename(file_brute), is_evt = meta$is_evt)
          message(paste("    -> [BRUTE] Step:", rain_step, "min"))
          bar_shift <- pas_exp 
          col_val <- if("Val" %in% names(tab_brute)) "Val" else names(tab_brute)[2]
          
          n_out <- nrow(df_pluie)
          if (nrow(tab_brute) >= n_out) {
            raw_vals <- tab_brute[[col_val]][1:n_out]
          } else {
            raw_vals <- c(tab_brute[[col_val]], rep(0, n_out - nrow(tab_brute)))
          }
          
          df_pluie$CumulBrut <- cumsum(raw_vals / rain_step)
          df_pluie$Intensite <- raw_vals * 3600 / (pas_exp * rain_step)
        }
        
        df_pluie <- df_pluie %>% filter(!is.na(Time))
        
        if (nrow(df_pluie) > 0) {
          df_hyeto <- df_pluie %>% dplyr::select(Time, `Pluie brute` = Intensite, `Pluie nette` = PNette) %>%
            pivot_longer(cols = c("Pluie brute", "Pluie nette"), names_to = "Nom", values_to = "Val")
          df_hyeto$Nom <- factor(df_hyeto$Nom, levels = c("Pluie brute", "Pluie nette"))
          
          cols_hyeto <- c("Pluie brute" = "#33FFFF", "Pluie nette" = "#9933FF")
          df_hyeto$Time[df_hyeto$Nom=="Pluie brute"]=df_hyeto$Time[df_hyeto$Nom=="Pluie brute"]+bar_shift
          
          g1 <- ggplotly(ggplot(df_hyeto, aes(x = Time, y = Val, fill = Nom)) +
                           geom_col(position = "identity", width = pas_exp) +
                           scale_fill_manual(name="", values = cols_hyeto) + 
                           theme_bw() + ylab("Intensité [mm/h]") + 
                           theme(axis.title.x = element_blank(), legend.title = element_blank()), dynamicTicks = TRUE)
          
          df_cumul <- df_pluie %>% dplyr::select(Time, `Cumul brut` = CumulBrut, `Cumul net` = CumulNet) %>%
            pivot_longer(cols = c("Cumul brut", "Cumul net"), names_to = "Nom", values_to = "Val")
          cols_cumul <- c("Cumul brut" = "#33FFFF", "Cumul net" = "#9933FF")
          
          g3 <- ggplotly(ggplot(df_cumul, aes(x = Time, y = Val, color = Nom)) +
                           geom_line(size = 1.1) +
                           scale_color_manual(name="", values = cols_cumul) +
                           theme_bw() + ylab("Cumul [mm]") + 
                           theme(axis.title.x = element_blank(), legend.title = element_blank()), dynamicTicks = TRUE)
          
          cache_rain_plots[[cache_key_rain]] <- list(g1 = g1, g3 = g3, t_axis = t_axis)
        } else {
          cache_rain_plots[[cache_key_rain]] <- NULL
        }
      }
      rain_objs <- cache_rain_plots[[cache_key_rain]]
    } else {
      rain_objs <- NULL
    }
    
    if (is.null(rain_objs)) {
      message("    [WARN] Données pluie invalides ou manquantes.")
      rain_objs <- list(g1 = NULL, g3 = NULL, t_axis = NULL)
    }
    
    # ---------------------------------------------------------
    # PARTIE Q, Z, H (Dépend strictement de la présence de SC.txt)
    # ---------------------------------------------------------
    if (has_sc) {
      message("  -> [DATA] SC + Obs")
      cl_names <- c("nom", "numsec", "Time", "Q", "Zmoy", "Zmax", "Zmin", "Hmax", "Htot")
      tab_sc <- read.csv(file.path(dsnlayerC, f_sc), header = FALSE, sep = "", skip = 2, col.names = cl_names)
      tab_sc <- tab_sc %>% filter(as.numeric(Time) >= 60)
      tab_sc[, 2:9] <- lapply(tab_sc[, 2:9], as.numeric)
      
      tab_sc$DATE <- meta$date_deb + tab_sc$Time
      tab_sc$nom <- as.character(tab_sc$nom)
      # if("Q" %in% names(sc_sf))  sc_sf =sc_sf[,-which(names(sc_sf)=="Q")]
      c("ID","NOM") %in% names(sc_sf)
      
      
      sc_sf=sc_sf[,c(which(names(sc_sf)== "ID" | names(sc_sf)== "NOM" ),ncol(sc_sf))]
      tab_sc <- inner_join(tab_sc, sc_sf, by = c("nom" = "ID"),keep=FALSE)
      
      sc_names <- unique(tab_sc$NOM) 
      sc_list_obs <- list.files(path = dsnscobs, full.names = TRUE)
      
      load_obs <- function(suffix, col_name) {
        files_match <- unlist(lapply(sc_names, function(x) grep(paste0(x, suffix), sc_list_obs, value = TRUE)))
        if (length(files_match) == 0) return(NULL)
        df_list <- lapply(files_match, read.csv)
        df_obs <- do.call(rbind, df_list)
        df_obs$Date <- as.POSIXct(ifelse(grepl(" ", df_obs$Date), df_obs$Date, paste(df_obs$Date, "00:00:00")), tz = "UTC")
        df_obs <- df_obs %>% filter(Date >= min(tab_sc$DATE) & Date <= max(tab_sc$DATE))
        if (nrow(df_obs) > 0) {
          colnames(df_obs) <- c("Date", col_name, "NOM")
          df_obs$NOM <- paste0(df_obs$NOM, "_obs") 
          df_obs$TYPE <- "dashed" 
          return(df_obs)
        }
        return(NULL)
      }
      
      obs_q <- load_obs("_Q.csv", "Q")
      df_q_all <- bind_rows(
        tab_sc %>% dplyr::select(Date = DATE, Q, NOM) %>% mutate(Q = abs(Q), TYPE = "solid"),
        if (!is.null(obs_q)) obs_q %>% mutate(Q = abs(Q), TYPE = "dashed") else NULL
      )
      
      obs_z <- load_obs("_Z.csv", "Z")
      df_z_all <- bind_rows(
        tab_sc %>% dplyr::select(Date = DATE, Z = Zmax, NOM) %>% mutate(Z = Z, TYPE = "solid"),
        if (!is.null(obs_z)) obs_z %>% mutate(TYPE = "dashed") else NULL
      )
      # FIX: Remplacement des 0 par NA dans la colonne Z
      df_z_all$Z[df_z_all$Z == 0] <- NA
      
      obs_h <- load_obs("_H.csv", "H")
      df_h_all <- bind_rows(
        tab_sc %>% dplyr::select(Date = DATE, H = Hmax, NOM) %>% mutate(TYPE = "solid"),
        if (!is.null(obs_h)) obs_h %>% mutate(TYPE = "dashed") else NULL
      )
      
      save_master_plot <- function(df_data, y_col, y_lab, output_path, title_suffix) {
        if (file.exists(output_path)) return()
        df_data <- df_data %>% filter(!is.na(Date)) %>% arrange(NOM, Date)
        if (nrow(df_data) == 0) return()
        test_obs = df_data %>% filter(TYPE != "solid")
        
        # S'assurer qu'il n'y a que des données non-NA avant le tracé
        df_data <- df_data %>% filter(!is.na(.data[[y_col]]))
        
        if(length(test_obs$Date) != 0) {
          g_main <- ggplot() +
            geom_line(data = df_data %>% filter(TYPE == "solid"), 
                      aes(x = Date, y = .data[[y_col]], color = NOM), 
                      linetype = "solid", size = 0.7) +
            geom_line(data = df_data %>% filter(TYPE != "solid"), 
                      aes(x = Date, y = .data[[y_col]], color = NOM), 
                      linetype = "dashed", size = 0.7) +
            theme_bw() + ylab(y_lab) + xlab("Date (heure météo)") + 
            theme(legend.title = element_blank())
        } else {
          g_main <- ggplot() +
            geom_line(data = df_data %>% filter(TYPE == "solid"), 
                      aes(x = Date, y = .data[[y_col]], color = NOM), 
                      linetype = "solid", size = 0.7) +
            theme_bw() + ylab(y_lab) + xlab("Date (heure météo)") + 
            theme(legend.title = element_blank())
        }
        
        titre <- paste0("Hyéto + ", title_suffix, " ", racine_nom, " (Pluie sur tout le bassin)")
        
        plots_list <- list()
        heights <- c()
        rows_val <- 0
        
        has_rain <- !is.null(rain_objs) && !is.null(rain_objs$g3) && !inherits(rain_objs$g3, "plotly_empty")
        
        if (has_rain) {
          plots_list[[1]] <- rain_objs$g3
          plots_list[[2]] <- rain_objs$g1
          plots_list[[3]] <- ggplotly(g_main, dynamicTicks = TRUE)
          heights <- c(0.1, 0.2, 0.7)
          rows_val <- 3
        } else {
          plots_list[[1]] <- ggplotly(g_main, dynamicTicks = TRUE)
          heights <- c(1)
          rows_val <- 1
        }
        
        p_final <- subplot(plots_list, nrows = rows_val, shareX = TRUE, heights = heights, titleY = TRUE) %>%
          layout(title = list(text = titre), hovermode = "closest", legend = list(title = list(text = "")))
        saveWidget(partial_bundle(p_final), file = output_path, selfcontained = TRUE, libdir = "lib")
      }
      
      message("  -> [EXPORT] HTML Q/Z/H")
      p_q_out <- file.path(dsnlayerC, paste0(racine_nom, "_PN_PB_Q_Obs.html"))
      p_z_out <- file.path(dsnlayerC, paste0(racine_nom, "_PN_PB_Z_Obs.html"))
      p_h_out <- file.path(dsnlayerC, paste0(racine_nom, "_PN_PB_H_Obs.html"))
      
      save_master_plot(df_q_all, "Q", "Débit [m3/s]", p_q_out, "Hydrogramme")
      save_master_plot(df_z_all, "Z", "Côte [m]", p_z_out, "Côtes")
      save_master_plot(df_h_all, "H", "Hauteur [m]", p_h_out, "Hauteur")
      
      message("  -> [GPKG] Qmax")
      
      tabmaxq <- tab_sc %>% group_by(nom) %>% filter(abs(Q) == max(abs(Q))) %>% slice(1)
      new_sc_sf <- sc_sf %>% 
        left_join(tabmaxq %>% dplyr::select(nom, Q, DATE, Zmoy, Hmax), by = c("ID" = "nom")) %>%
        mutate(Q = abs(Q), NomSecteur = basename(dsnlayerC))
      # browser()
      ici=which(colnames(new_sc_sf)=="fid")
      if (length(ici)>0){new_sc_sf=new_sc_sf[,-ici]}
      st_write(new_sc_sf, file.path(dsnlayerC, paste0(gsub("\\..*", "", f_sc), "_Qmax.gpkg")), driver = "GPKG", delete_dsn = TRUE, quiet = TRUE)
      file.copy(file.path(chem_routine, "C2D", Doss_Qml, "SC_Qmax.qml"), file.path(dsnlayerC, paste0(gsub("\\..*", "", f_sc), "_Qmax.qml")), overwrite = TRUE)
    } else {
      message("  -> [INFO] Pas de fichier SC.txt, graphes Q, Z, H ignorés.")
    }
    
    # ---------------------------------------------------------
    # PARTIE SORTIES: FLUX, OUVRAGES ET VOLUMES (.sortie direct)
    # ---------------------------------------------------------
    if (!is.null(tab_out)) {
      name_case_clean <- sub("\\.cas_.*", "", basename(f_out_match[1]))
      p_qs_out <- file.path(dsnlayerC, paste0(name_case_clean, "_PN_PB_QS_Obs.html"))
      p_qoh_out <- file.path(dsnlayerC, paste0(name_case_clean, "_PN_PB_QOH_Obs.html"))
      p_vqs_out <- file.path(dsnlayerC, paste0(name_case_clean, "_PN_PB_VQS_Obs.html"))
      
      if (!exists("t_axis") || is.null(t_axis)) t_axis <- meta$date_deb + (tab_out[[1]] * past_ts)
      
      # 1. Ajout VQS (Volume)
      if (!file.exists(p_vqs_out)) {
        
        # FIX : Récupérer le premier volume valide pour ne pas soustraire 0 si la première ligne est NA
        vol_domaine_raw <- tab_out[["VOLUME IN THE DOMAIN"]]
        val_ref_domaine <- vol_domaine_raw[!is.na(vol_domaine_raw)][1]
        if (is.na(val_ref_domaine)) val_ref_domaine <- 0
        
        # On remplace les éventuels NA par le vrai volume de référence initial
        vol_domaine <- replace_na(vol_domaine_raw, val_ref_domaine)
        vol_domaine <- vol_domaine - val_ref_domaine
        
        # Le volume de la source imprimé dans la sortie Telemac est celui d'1 seul pas de temps de calcul (past_ts)
        rate_injecte <- replace_na(tab_out[["ADDITIONAL VOLUME DUE TO SOURCE TERMS"]], 0) / past_ts
        vol_injecte <- cumsum(rate_injecte * pas_exp) 
        
        df_flux_only <- tab_out %>% dplyr::select(matches("FLUX BOUNDARY"))
        vol_sorties_cumul <- rep(0, nrow(tab_out))
        q_total <- rep(0, nrow(tab_out))
        
        if (ncol(df_flux_only) > 0) {
          df_flux_active <- df_flux_only %>% mutate(across(everything(), ~replace_na(., 0)))
          q_total <- rowSums(df_flux_active, na.rm=TRUE)
          # On utilise -q_total exactement comme l'ancien code (-TimeStep*sum(...))
          # Les flux sortants (<0 dans Telemac) deviennent des volumes positifs
          # ce qui maintient l'équilibre physique exact pour Domaine + Sorties
          vol_sorties_cumul <- cumsum(-q_total * pas_exp)
        }
        
        df_vol <- data.frame(
          Time = t_axis,
          Injecte = vol_injecte,
          Domaine = vol_domaine,
          `Domaine + Sorties` = vol_domaine + vol_sorties_cumul,
          check.names = FALSE
        ) %>% pivot_longer(-Time, names_to = "Variable", values_to = "Volume")
        
        # Ajout des volumes individuels des sorties
        if (exists("df_flux_active") && ncol(df_flux_active) > 0) {
          for(col_name in colnames(df_flux_active)) {
            clean_name <- sub("FLUX BOUNDARY", "Sortie", col_name)
            # Volume individuel (sortant = positif grâce au signe -)
            vol_indiv <- cumsum(-df_flux_active[[col_name]] * pas_exp)
            df_temp <- data.frame(Time = t_axis, Variable = clean_name, Volume = vol_indiv)
            df_vol <- bind_rows(df_vol, df_temp)
          }
        }
        
        g_vol <- ggplot(df_vol, aes(x = Time, y = Volume, color = Variable)) +
          geom_line() + theme_bw() + labs(y = "Volume [m3]") + 
          theme(legend.title = element_blank(), legend.position="right")
        
        # Graphique débit total de sortie (conservé en valeur absolue pour le visuel graphique)
        df_qs_total <- data.frame(Time = t_axis, Q = abs(q_total))
        g_qs_total <- ggplot(df_qs_total, aes(x=Time, y=Q, color="Débit Sortie Total")) + 
          geom_line() + 
          scale_color_manual(values=c("Débit Sortie Total"="red")) +
          theme_bw() + ylab("Débit Sortie Total [m3/s]") +
          theme(legend.title = element_blank(), legend.position="right")
        
        plots_vqs <- list()
        if (!is.null(rain_objs) && !is.null(rain_objs$g3) && !inherits(rain_objs$g3, "plotly_empty")) {
          plots_vqs <- list(rain_objs$g3, ggplotly(g_vol, dynamicTicks = TRUE), ggplotly(g_qs_total, dynamicTicks = TRUE))
          p_vqs <- subplot(plots_vqs, nrows = 3, shareX = TRUE, titleY = TRUE) %>%
            layout(title = paste("Bilan Volume -", racine_nom), legend = list(title = list(text = ""), orientation = "v"))
        } else {
          plots_vqs <- list(ggplotly(g_vol, dynamicTicks = TRUE), ggplotly(g_qs_total, dynamicTicks = TRUE))
          p_vqs <- subplot(plots_vqs, nrows = 2, shareX = TRUE, titleY = TRUE) %>%
            layout(title = paste("Bilan Volume -", racine_nom), legend = list(title = list(text = ""), orientation = "v"))
        }
        saveWidget(partial_bundle(p_vqs), file = p_vqs_out, selfcontained = TRUE, libdir = "lib")
      }
      
      # 2. Flux and Culverts (QS, QOH)
      df_flux <- tab_out %>% dplyr::select(matches("FLUX BOUNDARY|CULVERT"))
      keep_cols <- sapply(df_flux, function(x) !all(is.na(x)))
      df_flux <- df_flux[, keep_cols, drop=FALSE]
      
      if (ncol(df_flux) > 0) {
        df_long <- df_flux %>% mutate(Time = t_axis) %>% 
          pivot_longer(-Time) %>% filter(!is.na(Time)) 
        
        if (!file.exists(p_qs_out) && any(grepl("FLUX", df_long$name))) {
          # Valeur absolue uniquement pour l'affichage graphique du débit !
          g_qs <- ggplot(df_long %>% filter(grepl("FLUX", name)), aes(Time, abs(value), color = name)) + 
            geom_line() + theme_bw() + ylab("Débit [m3/s]") + xlab("Date (heure météo)") + theme(legend.title = element_blank())
          
          plots_list <- list()
          has_rain <- !is.null(rain_objs) && !is.null(rain_objs$g3) && !inherits(rain_objs$g3, "plotly_empty")
          if (has_rain) {
            plots_list[[1]] <- rain_objs$g3
            plots_list[[2]] <- rain_objs$g1
            plots_list[[3]] <- ggplotly(g_qs, dynamicTicks = TRUE)
            h <- c(0.1, 0.2, 0.7); r <- 3
          } else {
            plots_list[[1]] <- ggplotly(g_qs, dynamicTicks = TRUE)
            h <- c(1); r <- 1
          }
          p_qs <- subplot(plots_list, nrows = r, shareX = TRUE, heights = h, titleY=TRUE) %>%
            layout(title = paste("Sorties -", racine_nom), hovermode = "closest", legend = list(title = list(text = "")))
          saveWidget(partial_bundle(p_qs), file = p_qs_out, selfcontained = TRUE, libdir = "lib")
        }
        
        if (!file.exists(p_qoh_out) && any(grepl("CULVERT", df_long$name))) {
          
          # ==========================================
          # 1. Traitement du Tibble
          # ==========================================
          cat("Filtrage et extraction des numéros du tibble...\n")
          
          tibble_filtre <- df_long %>%
            # On garde uniquement les lignes où 'name' contient "CULVERT" (insensible à la casse)
            filter(str_detect(name, "(?i)CULVERT")) %>%
            # On extrait le numéro contenu dans la chaîne de caractères (ex: "CULVERT 3" devient 3)
            # et on le stocke dans une nouvelle colonne pour la jointure
            mutate(id_jointure = as.integer(str_extract(name, "\\d+")))
          
          # ==========================================
          # 2. Traitement du Shapefile (.shp)
          # ==========================================
          chemin_shp <- file.path(dsnlayerC,"OuvHydrau_Select.shp")
          cat("Lecture du fichier shapefile...\n")
          
          # Lecture du fichier spatial
          shp_data <- st_read(chemin_shp, quiet = TRUE)
          
          # Préparation de la table issue du shapefile
          table_shp <- shp_data %>%
            # Optionnel : retirer la géométrie pour alléger les calculs si on ne veut qu'un tableau
            st_drop_geometry() %>%
            # Création de la colonne id2 qui va de 1 à la taille du dataframe (length de NOM)
            mutate(id2 = row_number()) %>%
            # On ne conserve que les colonnes utiles pour éviter d'alourdir le résultat final
            dplyr::select(id2, NOM)
          
          # ==========================================
          # 3. Jointure des deux tables
          # ==========================================
          cat("Jointure des données...\n")
          
          tibble_final <- tibble_filtre %>%
            # Jointure à gauche : on garde tous les culverts du tibble, 
            # et on y associe le NOM correspondant du shapefile
            left_join(table_shp, by = c("id_jointure" = "id2"))
          
          # Affichage des premières lignes du résultat
          cat("Aperçu du résultat :\n")
          print(head(tibble_final))
          
          
          
          g_qoh <- ggplot(tibble_final, aes(Time, abs(value), color = NOM)) + 
            geom_line() + theme_bw() + ylab("Débit [m3/s]") + xlab("Date (heure météo)") + theme(legend.title = element_blank())
          
          plots_list <- list()
          has_rain <- !is.null(rain_objs) && !is.null(rain_objs$g3) && !inherits(rain_objs$g3, "plotly_empty")
          if (has_rain) {
            plots_list[[1]] <- rain_objs$g3
            plots_list[[2]] <- rain_objs$g1
            plots_list[[3]] <- ggplotly(g_qoh, dynamicTicks = TRUE)
            h <- c(0.1, 0.2, 0.7); r <- 3
          } else {
            plots_list[[1]] <- ggplotly(g_qoh, dynamicTicks = TRUE)
            h <- c(1); r <- 1
          }
          p_qoh <- subplot(plots_list, nrows = r, shareX = TRUE, heights = h, titleY=TRUE) %>%
            layout(title = paste("Ouvrages -", racine_nom), hovermode = "closest", legend = list(title = list(text = "")))
          saveWidget(partial_bundle(p_qoh), file = p_qoh_out, selfcontained = TRUE, libdir = "lib")
        }
      }
    }
  }
  
  if (dir.exists(file.path(dsnlayerC, "lib"))) unlink(file.path(dsnlayerC, "lib"), recursive = TRUE)
  message("[FIN] Terminé.")
}
#######################plot section de controle 
Export_GRD <- function(NomGRD,dimensionPtsElem,PTS,ELEM)
{
  # zzNomGRD=file.create(NomGRD)
  cat("Ecriture parfois longue du fichier ",NomGRD)
  # writeLines(Entete, file = zzNomGRD)
  # writeLines("ADCIRC", NomGRD)
  # writeLines(as.character(dimensionPtsElem), NomGRD, append=T)
  writeLines(c("ADCIRC", dimensionPtsElem), NomGRD)
  write.table(PTS,file =NomGRD,col.names = FALSE,row.names = FALSE,sep = " ",append = TRUE )
  write.table(ELEM,file =NomGRD,col.names = FALSE,row.names = FALSE,sep = " ",append = TRUE )
  cat(" - Terminé \n")
}