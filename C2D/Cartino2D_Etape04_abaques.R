#-----------------------------------------------------------------------
# Script R - WORKFLOW COMPLET AUTOMATISÉ VIA SHAPEFILE ET MNT 
#
#
# Objectif: Lire les données de buses depuis un shapefile, puis pour chaque buse:
#       - Permet de choisir entre le traitement séquentiel ou parallèle des buses.
#       - Permet de choisir entre l'extraction de profils depuis un MNT ou la génération de profils trapézoïdaux.
#       - Centre les profils sur un axe de thalweg unique.
#       - Génère un graphique de profil en long pour chaque scénario.
#       - Agrège les résultats finaux dans un fichier d'abaque unique.
#
# Modifications v124 (Reprise intelligente):
#       1- AJOUT D'UNE VÉRIFICATION PRÉALABLE :
#          Si le fichier "abaque_partiel.txt" existe déjà dans le dossier de la buse,
#          la fonction s'arrête immédiatement et passe à la buse suivante.
#-----------------------------------------------------------------------

#########################################################################
# PRÉREQUIS : CHARGEMENT DES LIBRAIRIES
#########################################################################
# Assurez-vous que ces packages sont installés: install.packages(c("sf", "terra", "dplyr", "fs", "stringr", "foreach", "doParallel", "data.table", "plotly", "htmlwidgets"))
abacus=function(){
  library(sf)
  library(terra)
  library(dplyr)
  library(fs)
  library(stringr)
  library(foreach)
  library(doParallel)
  library(data.table)
  library(plotly)
  library(htmlwidgets)
  
  #########################################################################
  # PARTIE 0: PARAMÈTRES UTILISATEUR
  # Modifiez les valeurs dans cette section pour configurer votre simulation.
  #########################################################################
  # --- 0.1. Configuration des chemins et fichiers ---
  # chem_routine <- "C:/Users/CartoNationale/Desktop/Nabil"
  main_output_dir <- file.path(dsnlayerC, "abaques") 
  telemac_env_script <- "C:/TELEMAC/V8P4/configs/pysource.bat"
  xcas_template_filename <- file.path(chem_routine,"C2D",Doss_Base, "masc.xcas.txt")
  
  # --- 0.2. Paramètres de parallélisation ---
  ncore_buse <- ncore_buse_abaques # Nombre de cœurs pour la boucle sur les buses. 1 = séquentiel, >=2 = parallèle
  num_cores_sim <- num_cores_sim_abaques
  # Nombre de cœurs pour les simulations d'une MÊME buse
  
  # --- 0.3. Contrôle de la géométrie --- 
  use_mnt_for_profiles <- FALSE # Mettre à TRUE pour extraire du MNT, FALSE pour utiliser des profils trapézoïdaux
  
  # --- Chemins pour le mode MNT ---
  shapefile_path <- file.path(dsnlayerC, "OuvHydrau_Select.shp") 
  mnt_path <- file.path(dsnlayerC, "MNTFinal.gpkg") # <--- CHEMIN VERS VOTRE MNT
  
  # --- 0.4. Paramètres de la géométrie ---
  dist_amont_limite <- 1000.0
  dist_p4_p6 <- 2000 # Distance entre la sortie de la buse (corrigée à p5) et la limite aval (p6)
  num_points_per_profile <- 101
  dist_extraction_profil <- 3.0 
  preissmann_slot_width <- 0.01 # Largeur de la fente de Preissmann en mètres
  
  first_profile_shape <- list(
    type = "trapezoidal", bottom_width = 6.5, left_slope = 0.01,
    right_slope = 0.01, bank_height_above_bottom = 40
  )
  
  # --- 0.5. Paramètres temporels et hydrauliques ---
  duree_palier <- 400
  duree_montee <- 50
  strickler_zones_base <- c(35.0, 50.0, 35.0) 
  val_pas_planimetrage <- 0.02
  g <- 9.81
  
  # --- 0.6. Paramètres de maillage et de temps de simulation adaptatifs ---
  # Pour buses COURTES (< 20m)
  pas_maillage_court <- c(1, 0.5, 1)
  pas_temps_court <- 0.5
  duree_palier_court <- 20
  duree_montee_court <- 10
  
  # Pour buses MOYENNES (20m à 100m)
  pas_maillage_moyen <- c(1, 1, 1)
  pas_temps_moyen <- 1
  duree_palier_moyen <- 100
  duree_montee_moyen <- 20
  # Pour buses LONGUES (> 100m)
  pas_maillage_long <- c(2, 2, 2)
  pas_temps_long <- 2
  duree_palier_long <- 400
  duree_montee_long <- 50
  # --- 0.7. Paramètres de sortie (NOUVEAU) ---
  generate_long_profile_plots <- FALSE # Mettre à TRUE pour générer les graphiques de profil en long
  generate_abaque_plots <- TRUE       # Mettre à TRUE pour générer les graphiques d'abaque 2D/3D
  cleanup_calc_files <- TRUE           # <--- NOUVEAU: TRUE pour garder 1 dossier temp et supprimer le reste
  
  
  #########################################################################
  # LECTURE DES DONNÉES SPATIALES ET INITIALISATION
  #########################################################################
  message("--- LECTURE DES DONNÉES SPATIALES ---")
  if (!file.exists(shapefile_path)) stop("Le fichier shapefile n'a pas été trouvé: ", shapefile_path)
  busel_data <- st_read(shapefile_path, quiet = TRUE)
  message(paste("Lecture de", nrow(busel_data), "buse(s) depuis le shapefile."))
  
  mnt_raster <- NULL # Initialisation à NULL
  if(use_mnt_for_profiles) {
    if (!file.exists(mnt_path)) stop("Le fichier MNT n'a pas été trouvé: ", mnt_path)
    message("Chargement du MNT pour vérification CRS...")
    temp_mnt <- rast(mnt_path)
    message("MNT chargé.")
    if (st_crs(busel_data) != st_crs(temp_mnt)) {
      message("ATTENTION: CRS différents. Reprojection du shapefile vers le CRS du MNT.")
      busel_data <- st_transform(busel_data, st_crs(temp_mnt))
    }
    rm(temp_mnt) # On supprime l'objet, il sera rechargé dans chaque worker
    gc() # Force le nettoyage de la mémoire
  }
  
  dir.create(main_output_dir, showWarnings = FALSE, recursive = TRUE)
  
  #########################################################################
  # DÉFINITION DE LA FONCTION DE TRAITEMENT D'UNE BUSE
  #########################################################################
  process_buse <- function(i, busel_data, main_output_dir, telemac_env_script, xcas_template_filename, use_mnt_for_profiles, mnt_path, dist_amont_limite, dist_p4_p6, num_points_per_profile, dist_extraction_profil, first_profile_shape, duree_palier, duree_montee, num_cores_sim, strickler_zones_base, val_pas_planimetrage, g, pas_maillage_court, pas_temps_court, pas_maillage_moyen, pas_temps_moyen, pas_maillage_long, pas_temps_long, preissmann_slot_width, generate_long_profile_plots, generate_abaque_plots, cleanup_calc_files) {
    
    # Charger les librairies nécessaires dans l'environnement parallèle
    library(sf)
    library(terra)
    library(dplyr)
    library(fs)
    library(stringr)
    library(foreach)
    library(doParallel)
    library(data.table)
    library(plotly)
    library(htmlwidgets)
    
    # Si on utilise le MNT, chaque worker charge sa propre copie.
    if (use_mnt_for_profiles) {
      mnt_raster <- rast(mnt_path)
    }
    
    buse_row <- busel_data[i, ]
    numero_buse <- buse_row$id
    
    # Création du dossier de sortie
    current_buse_output_dir <- file.path(main_output_dir, paste0("buse_", numero_buse))
    dir.create(current_buse_output_dir, showWarnings = FALSE, recursive = TRUE)
    
    # -----------------------------------------------------------------------
    # CHECK DE DÉJÀ FAIT (NOUVELLE FONCTIONNALITÉ v124)
    # -----------------------------------------------------------------------
    abaque_check_file <- file.path(current_buse_output_dir, "abaque_partiel.txt")
    if (file.exists(abaque_check_file) && file.info(abaque_check_file)$size > 0) {
      message(paste(">>> BUSE", numero_buse, ": DÉJÀ TRAITÉE (Fichier abaque trouvé). PASSAGE À LA SUIVANTE. <<<"))
      return(NULL)
    }
    # -----------------------------------------------------------------------
    
    CIRC <- buse_row$CIRC
    if(is.na(buse_row$HAUT1) | buse_row$HAUT1==0) buse_row$HAUT1=buse_row$HAUT2
    nozzle_data <- list(LARG = buse_row$LRG, HAUT1 = buse_row$HAUT1, Z1 = buse_row$Z1, Z2 = buse_row$Z2, LENGTH = buse_row$LENGTH)
    first_profile_shape$bottom_width=nozzle_data$LARG*2
    if ("FRIC" %in% names(buse_row) && !is.na(buse_row$FRIC) && buse_row$FRIC > 0) {
      strickler_buse_value <- 1 / buse_row$FRIC
      if (strickler_buse_value < 1) {
        warning(paste("Buse", numero_buse, ": La valeur FRIC=", buse_row$FRIC, "donne un Strickler de", round(strickler_buse_value, 2), ". Utilisation de la valeur par défaut de 50.0."))
        strickler_buse_value <- 60.0
      }
    } else {
      strickler_buse_value <- 60.0
      warning(paste("Buse", numero_buse, ": Colonne 'FRIC' non valide ou absente. Utilisation de la valeur par défaut de Strickler:", strickler_buse_value))
    }
    strickler_zones <- c(strickler_zones_base[1], strickler_buse_value, strickler_zones_base[3])
    
    
    message(paste("\n\n############################################################"))
    message(paste("--- DÉMARRAGE DU TRAITEMENT POUR LA BUSE :", numero_buse, "---"))
    message(paste("--- Fichiers de sortie dans :", current_buse_output_dir, "---"))
    message(paste("############################################################\n"))
    
    if (nozzle_data$LENGTH < 20) {
      current_pas_maillage <- pas_maillage_court
      current_pas_temps <- pas_temps_court
      duree_palier <- duree_palier_court
      duree_montee <- duree_montee_court
      dist_amont_limite <- 20
      dist_p4_p6 <- 50 
      message("      - Buse courte (< 20m). Maillage fin et pas de temps court sélectionnés.")
    } else if (nozzle_data$LENGTH >= 20 && nozzle_data$LENGTH <= 250) {
      current_pas_maillage <- pas_maillage_moyen
      current_pas_temps <- pas_temps_moyen
      duree_palier <- duree_palier_moyen
      duree_montee <- duree_montee_moyen
      dist_amont_limite <- 50
      dist_p4_p6 <- 400
      message("      - Buse moyenne (20-100m). Maillage et pas de temps moyens sélectionnés.")
    } else {
      current_pas_maillage <- pas_maillage_long
      current_pas_temps <- pas_temps_long
      duree_palier <- duree_palier_long
      duree_montee <- duree_montee_long
      dist_amont_limite <- 500
      dist_p4_p6 <- 2000 
      message("      - Buse longue (> 100m). Maillage large et pas de temps long sélectionnés.")
    }
    
    I <- (nozzle_data$Z1 - nozzle_data$Z2) / nozzle_data$LENGTH
    if (CIRC == 1) {
      D <- nozzle_data$HAUT1; R <- D / 2; S <- pi * R^2; P <- 2 * pi * R; Rh <- R/2
      Q <- strickler_buse_value * S * (Rh^(2/3)) * sqrt(abs(I))
    } else {
      S <- nozzle_data$LARG * nozzle_data$HAUT1; P <- nozzle_data$LARG + (2 * nozzle_data$HAUT1); Rh <- S / P
      Q <- strickler_buse_value * S * (Rh^(2/3)) * sqrt(abs(I))
    }
    Q <- round(Q)
    
    if (Q < 15) {
      message(paste("      - Le débit max estimé (", Q, "m³/s) est inférieur à 15 m³/s. Il est forcé à 15 m³/s pour la simulation."))
      Q <- 15
    }
    
    Q_sim_max <- Q * 1.5
    if (Q_sim_max > 180) {
      message(paste("      - Le débit max de simulation (", round(Q_sim_max), "m³/s) dépasse 180 m³/s. Il est plafonné à 180 m³/s."))
      Q_sim_max <- 180
    }
    
    debits_q_list <- list()
    current_max_q <- 0
    
    end_q_p1 <- min(Q_sim_max, 1.0)
    if (0.2 <= end_q_p1) {
      debits_q_list[[length(debits_q_list) + 1]] <- seq(0.2, end_q_p1, by = 0.1)
      current_max_q <- end_q_p1
    }
    
    start_val_p2 <- current_max_q + 0.2
    end_q_p2 <- min(Q_sim_max, 10.0)
    if (start_val_p2 <= end_q_p2) {
      debits_q_list[[length(debits_q_list) + 1]] <- seq(start_val_p2, end_q_p2, by = 0.2)
      current_max_q <- end_q_p2
    }
    
    start_val_p3 <- current_max_q + 1
    end_q_p3 <- min(Q_sim_max, 20.0)
    if (start_val_p3 <= end_q_p3) {
      debits_q_list[[length(debits_q_list) + 1]] <- seq(start_val_p3, end_q_p3, by = 1)
      current_max_q <- end_q_p3
    }
    
    start_val_p4 <- current_max_q + 2.0
    end_q_p4 <- min(Q_sim_max, 40.0)
    if (start_val_p4 <= end_q_p4) {
      debits_q_list[[length(debits_q_list) + 1]] <- seq(start_val_p4, end_q_p4, by = 2.0)
      current_max_q <- end_q_p4
    }
    
    start_val_p5 <- current_max_q + 5.0
    end_q_p5 <- Q_sim_max
    if (start_val_p5 <= end_q_p5) {
      debits_q_list[[length(debits_q_list) + 1]] <- seq(start_val_p5, end_q_p5, by = 5.0)
    }
    
    debits_q <- unique(sort(c(unlist(debits_q_list))))
    debits_q <- round(debits_q[debits_q > 0], 2)
    
    if (CIRC == 1) {
      dimension_str <- sprintf("Diamètre: %.2f m", nozzle_data$HAUT1)
    } else {
      dimension_str <- sprintf("Largeur: %.2f m, Hauteur: %.2f m", nozzle_data$LARG, nozzle_data$HAUT1)
    }
    pente_pourcent <- I * 100
    title_details_str <- paste(
      dimension_str, 
      sprintf("Longueur: %.1f m", nozzle_data$LENGTH), 
      sprintf("Pente: %.2f %%", pente_pourcent),
      sprintf("Qmax simulé: %.1f m³/s", Q_sim_max), 
      sep=" | "
    )
    
    #########################################################################
    # PARTIE 1: GÉNÉRATION DU FICHIER DE GÉOMÉTRIE (.geo)
    #########################################################################
    message("--- PARTIE 1 : GÉOMÉTRIE (.geo) ---")
    
    create_rectangular_profile <- function(nozzle_width, nozzle_height, nozzle_bottom_z, target_center_y, preissmann_slot_width = 0.01, slot_height = 50, bank_y_offset = 10) {
      y_center <- target_center_y
      z_bottom <- nozzle_bottom_z
      z_top <- z_bottom + nozzle_height
      z_slot_top <- z_top + slot_height
      
      y_bottom_left  <- y_center - nozzle_width/2
      y_bottom_right <- y_center + nozzle_width/2
      y_top_left     <- y_bottom_left - 0.01
      y_top_right    <- y_bottom_right + 0.01
      
      y_slot_left    <- y_center - preissmann_slot_width/2
      y_slot_right   <- y_center + preissmann_slot_width/2
      
      y_bank_left    <- y_top_left - bank_y_offset
      y_bank_right   <- y_top_right + bank_y_offset
      
      full_profile_df <- rbind(
        data.frame(Y = y_bank_left, Z = z_slot_top), 
        data.frame(Y = y_slot_left, Z = z_slot_top), 
        data.frame(Y = y_slot_left, Z = z_top), 
        data.frame(Y = y_top_left, Z = z_top), 
        data.frame(Y = y_bottom_left, Z = z_bottom), 
        data.frame(Y = y_bottom_right, Z = z_bottom), 
        data.frame(Y = y_top_right, Z = z_top), 
        data.frame(Y = y_slot_right, Z = z_top), 
        data.frame(Y = y_slot_right, Z = z_slot_top), 
        data.frame(Y = y_bank_right, Z = z_slot_top)
      )
      full_profile_df$Marker <- "B"
      return(full_profile_df[!duplicated(round(full_profile_df[, c("Y", "Z")], 5)), ])
    }
    create_circular_profile <- function(diameter, nozzle_bottom_z, target_center_y, slot_width = 0.01, slot_height = 50, bank_y_offset = 10, n_points_arch = 41) {
      radius <- diameter/2; y_center <- target_center_y; z_center <- nozzle_bottom_z + radius; z_connection <- z_center + sqrt(max(0, radius^2 - (slot_width/2)^2)); z_slot_top <- z_connection + slot_height; y_slot_left <- y_center - slot_width/2; y_slot_right <- y_center + slot_width/2; y_structure_left <- y_center - radius; y_structure_right <- y_center + radius; p_berge_gauche_loin <- data.frame(Y = y_structure_left - bank_y_offset, Z = z_slot_top); p_fente_bas_gauche  <- data.frame(Y = y_slot_left, Z = z_connection); theta_start <- atan2(z_connection - z_center, y_slot_left - y_center); theta_end     <- atan2(z_connection - z_center, y_slot_right - y_center); theta_sequence <- seq(from = theta_start, to = theta_end + 2*pi, length.out = n_points_arch); arch_df <- data.frame(Y = y_center + radius * cos(theta_sequence), Z = z_center + radius * sin(theta_sequence)); p_fente_bas_droite  <- data.frame(Y = y_slot_right, Z = z_connection); p_berge_droite_loin <- data.frame(Y = y_structure_right + bank_y_offset, Z = z_slot_top)
      full_profile_df <- rbind(p_berge_gauche_loin, data.frame(Y = y_slot_left, Z = z_slot_top), p_fente_bas_gauche, arch_df, p_fente_bas_droite, data.frame(Y = y_slot_right, Z = z_slot_top), p_berge_droite_loin)
      full_profile_df$Marker <- "B"
      return(full_profile_df[!duplicated(round(full_profile_df[, c("Y", "Z")], 5)), ])
    }
    
    generate_points_for_trapezoidal <- function(bottom_width, left_slope, right_slope, bottom_elevation, bank_height_above_bottom, target_center_y, bank_y_offset = 10) {
      half_bottom_width <- bottom_width / 2
      top_z <- bottom_elevation + bank_height_above_bottom
      
      # Les points sont d'abord calculés de manière centrée autour de Y=0
      y_top_left_rel <- -half_bottom_width - (left_slope * bank_height_above_bottom)
      y_top_right_rel <- half_bottom_width + (right_slope * bank_height_above_bottom)
      y_rel <- c(y_top_left_rel - bank_y_offset, y_top_left_rel, -half_bottom_width, half_bottom_width, y_top_right_rel, y_top_right_rel + bank_y_offset)
      
      # Le thalweg de ce profil relatif est à Y=0. On décale tous les points pour que le thalweg soit à target_center_y.
      y_abs <- y_rel + target_center_y
      
      z_abs <- c(top_z, top_z, bottom_elevation, bottom_elevation, top_z, top_z)
      points_df <- data.frame(Y = y_abs, Z = z_abs, Marker = "B")
      return(points_df[order(points_df$Y), ])
    }
    
    extract_profile_from_mnt <- function(mnt, center_point_sf, direction_vector, profile_width, num_points = 101) {
      perp_vector <- c(-direction_vector[2], direction_vector[1])
      perp_vector_norm <- perp_vector / sqrt(sum(perp_vector^2))
      start_point <- st_coordinates(center_point_sf) - (profile_width / 2) * perp_vector_norm
      end_point <- st_coordinates(center_point_sf) + (profile_width / 2) * perp_vector_norm
      transect_line <- st_sfc(st_linestring(rbind(start_point, end_point)), crs = st_crs(mnt))
      sampled_points_sf <- st_line_sample(transect_line, n = num_points, type = "regular")
      if (st_is_empty(sampled_points_sf)) return(NULL)
      sampled_points_sf <- st_cast(sampled_points_sf, "POINT")
      sampled_points_vect <- vect(sampled_points_sf)
      extracted_data <- terra::extract(mnt, sampled_points_vect, xy = TRUE, ID = FALSE)
      if (nrow(extracted_data) == 0 || all(is.na(extracted_data[, 1]))) {
        warning(paste0("L'extraction MNT a échoué (autour de X=", round(st_coordinates(center_point_sf)[1]),", Y=", round(st_coordinates(center_point_sf)[2]), "). Profil hors emprise?"))
        return(NULL)
      }
      colnames(extracted_data)[1] <- "Z"
      coords <- extracted_data[, c("x", "y")]
      y_relative <- sqrt((coords$x - coords$x[1])^2 + (coords$y - coords$y[1])^2)
      profile_df <- data.frame(Y = y_relative, Z = extracted_data$Z, Marker = "B")
      profile_df <- na.omit(profile_df)
      if (nrow(profile_df) < 2) return(NULL)
      return(profile_df)
    }
    
    shift_profile_z <- function(profile_df, target_min_z) {
      if(is.null(profile_df) || nrow(profile_df) == 0) return(NULL)
      current_min_z <- min(profile_df$Z, na.rm = TRUE)
      delta_z <- target_min_z - current_min_z
      profile_df$Z <- profile_df$Z + delta_z
      return(profile_df)
    }
    
    center_profile_on_thalweg <- function(profile_df, target_y_center) {
      if(is.null(profile_df) || nrow(profile_df) == 0) return(NULL)
      min_z_index <- which.min(profile_df$Z)
      y_of_min_z <- mean(profile_df$Y[min_z_index]) 
      shift_y <- target_y_center - y_of_min_z
      profile_df$Y <- profile_df$Y + shift_y
      return(profile_df)
    }
    
    add_vertical_walls <- function(profile_df) {
      if(is.null(profile_df) || nrow(profile_df) < 2) return(profile_df)
      
      first_point <- profile_df[1, ]
      wall_start_point <- data.frame(Y = first_point$Y, Z = first_point$Z + 20, Marker = "B")
      
      last_point <- profile_df[nrow(profile_df), ]
      wall_end_point <- data.frame(Y = last_point$Y, Z = last_point$Z + 20, Marker = "B")
      
      new_profile <- rbind(wall_start_point, profile_df, wall_end_point)
      return(new_profile)
    }
    
    write_geo_profile <- function(con, bief_name, prof_name, prof_abs, points_df) {
      writeLines(paste0("PROFIL ", bief_name, " ", prof_name, " ", sprintf("%.3f", prof_abs)), con)
      for (j in 1:nrow(points_df)) {
        writeLines(paste(sprintf("%.3f", points_df$Y[j]), sprintf("%.5f", points_df$Z[j]), points_df$Marker[j]), con)
      }
    }
    
    model_total_length <- dist_amont_limite + nozzle_data$LENGTH + dist_p4_p6
    x_nozzle_start <- dist_amont_limite
    x_nozzle_end <- x_nozzle_start + nozzle_data$LENGTH
    x_coords <- c(0, x_nozzle_start - 2.0, x_nozzle_start, x_nozzle_end, x_nozzle_end + 2.0, model_total_length)
    num_profiles_total <- length(x_coords)
    
    buse_geom <- st_geometry(buse_row)
    
    geo_base_filename <- paste0("geom_buse", numero_buse, ifelse(use_mnt_for_profiles, "_MNT", "_Trapeze"))
    geo_filename_with_ext <- paste0(geo_base_filename, ".geo")
    geo_filepath <- file.path(current_buse_output_dir, geo_filename_with_ext)
    message(paste("Génération du fichier de géométrie :", geo_filename_with_ext))
    
    bief_name <- "Bief_1"
    indices <- list(p1 = 1, p2 = 2, p3 = 3, p4 = 4, p5 = 5, p6 = 6)
    
    all_profiles <- vector("list", num_profiles_total)
    
    # Déterminer une abscisse Y cible unique pour le centrage de tous les profils.
    half_width_calc <- first_profile_shape$bottom_width / 2 + 
      max(first_profile_shape$left_slope, first_profile_shape$right_slope) * first_profile_shape$bank_height_above_bottom + 
      10.0 # bank_y_offset de la fonction generate_points_for_trapezoidal
    target_center_y <- ceiling(half_width_calc) + 5.0 # Ajout d'une marge de sécurité
    message(paste("      - Abscisse Y cible pour le centrage de tous les profils:", sprintf("%.3f", target_center_y)))
    
    if(use_mnt_for_profiles) {
      message("      - Mode MNT: Extraction des profils depuis le MNT...")
      
      buse_coords <- st_coordinates(buse_geom)
      if(nrow(buse_coords) < 2) stop("Géométrie de buse invalide (moins de 2 points) pour la buse ", numero_buse)
      
      start_node <- buse_coords[1, 1:2]
      end_node <- buse_coords[nrow(buse_coords), 1:2]
      direction_vector <- end_node - start_node
      direction_vector_norm <- direction_vector / sqrt(sum(direction_vector^2))
      profile_width_mnt <- 10 * ifelse(CIRC == 1, nozzle_data$HAUT1, nozzle_data$LARG)
      
      pt_extraction_amont <- st_sfc(st_point(start_node - dist_extraction_profil * direction_vector_norm), crs = st_crs(mnt_raster))
      profil_amont_base <- extract_profile_from_mnt(mnt_raster, pt_extraction_amont, direction_vector_norm, profile_width_mnt, num_points_per_profile)
      if(is.null(profil_amont_base)) stop("Échec de l'extraction du profil de base amont. Arrêt.")
      
      pt_extraction_aval <- st_sfc(st_point(end_node + dist_extraction_profil * direction_vector_norm), crs = st_crs(mnt_raster))
      profil_aval_base <- extract_profile_from_mnt(mnt_raster, pt_extraction_aval, direction_vector_norm, profile_width_mnt, num_points_per_profile)
      if(is.null(profil_aval_base)) stop("Échec de l'extraction du profil de base aval. Arrêt.")
      
      # Altitude des profils en extrapolant la pente de la buse (I)
      
      # Profil 1 (limite amont)
      z_profil_1 <- nozzle_data$Z1 + abs(I) * (x_coords[indices$p3] - x_coords[indices$p1])
      profil_1_vshifted <- shift_profile_z(profil_amont_base, z_profil_1)
      profil_1_centered <- center_profile_on_thalweg(profil_1_vshifted, target_center_y)
      profil_1 <- add_vertical_walls(profil_1_centered)
      all_profiles[[indices$p1]] <- list(name = "1", abscissa = x_coords[indices$p1], data = profil_1)
      
      # Profil 2 (juste avant la buse)
      z_profil_2 <- nozzle_data$Z1 + abs(I) * (x_coords[indices$p3] - x_coords[indices$p2])
      profil_2_vshifted <- shift_profile_z(profil_amont_base, z_profil_2)
      profil_2_centered <- center_profile_on_thalweg(profil_2_vshifted, target_center_y)
      profil_2 <- add_vertical_walls(profil_2_centered)
      all_profiles[[indices$p2]] <- list(name = "2", abscissa = x_coords[indices$p2], data = profil_2)
      
      # Profil 5 (juste après la buse)
      z_profil_5 <- nozzle_data$Z2 - abs(I) * (x_coords[indices$p5] - x_coords[indices$p4])
      profil_5_vshifted <- shift_profile_z(profil_aval_base, z_profil_5)
      profil_5_centered <- center_profile_on_thalweg(profil_5_vshifted, target_center_y)
      profil_5 <- add_vertical_walls(profil_5_centered)
      all_profiles[[indices$p5]] <- list(name = "5", abscissa = x_coords[indices$p5], data = profil_5)
      
      # Profil 6 (limite aval)
      z_profil_6 <- nozzle_data$Z2 - abs(I) * (x_coords[indices$p6] - x_coords[indices$p4])
      profil_6_vshifted <- shift_profile_z(profil_aval_base, z_profil_6)
      profil_6_centered <- center_profile_on_thalweg(profil_6_vshifted, target_center_y)
      profil_6 <- add_vertical_walls(profil_6_centered)
      all_profiles[[indices$p6]] <- list(name = "6", abscissa = x_coords[indices$p6], data = profil_6)
      
    } else {
      message("      - Mode Trapézoïdal: Génération des profils génériques...")
      
      calculated_bottom_z <- sapply(x_coords, function(x) {
        if (x <= x_nozzle_start) return(nozzle_data$Z1 + abs(I) * (x_nozzle_start - x))
        if (x > x_nozzle_start && x < x_nozzle_end) return(nozzle_data$Z1 - I * (x - x_nozzle_start))
        return(nozzle_data$Z2 - abs(I) * (x - x_nozzle_end))
      })
      
      for (k in c(indices$p1, indices$p2, indices$p5, indices$p6)) {
        points_raw <- generate_points_for_trapezoidal(
          bottom_width = first_profile_shape$bottom_width, 
          left_slope = first_profile_shape$left_slope, 
          right_slope = first_profile_shape$right_slope, 
          bottom_elevation = calculated_bottom_z[k], 
          bank_height_above_bottom = first_profile_shape$bank_height_above_bottom,
          target_center_y = target_center_y,
          bank_y_offset = 10.0
        )
        points <- add_vertical_walls(points_raw)
        all_profiles[[k]] <- list(name = as.character(k), abscissa = x_coords[k], data = points)
      }
    }
    
    points_p3 <- if(CIRC == 1) create_circular_profile(nozzle_data$HAUT1, nozzle_data$Z1, target_center_y) else create_rectangular_profile(nozzle_data$LARG, nozzle_data$HAUT1, nozzle_data$Z1, target_center_y, preissmann_slot_width)
    all_profiles[[indices$p3]] <- list(name = "3amont", abscissa = x_coords[indices$p3], data = points_p3)
    
    points_p4 <- if(CIRC == 1) create_circular_profile(nozzle_data$HAUT1, nozzle_data$Z2, target_center_y) else create_rectangular_profile(nozzle_data$LARG, nozzle_data$HAUT1, nozzle_data$Z2, target_center_y, preissmann_slot_width)
    all_profiles[[indices$p4]] <- list(name = "3aval", abscissa = x_coords[indices$p4], data = points_p4)
    
    con <- file(geo_filepath, "wt")
    message("      - Écriture ordonnée des profils dans le fichier .geo...")
    for (prof in all_profiles) {
      if (!is.null(prof) && !is.null(prof$data)) {
        write_geo_profile(con, bief_name, prof$name, prof$abscissa, prof$data)
      }
    }
    close(con)
    message("--- Fichier .geo généré avec succès. ---")
    
    #########################################################################
    # PARTIE 2: GÉNÉRATION DES FICHIERS DE LOIS (.loi)
    #########################################################################
    message("\n--- PARTIE 2 : LOIS HYDRAULIQUES (.loi) ---")
    
    duree_totale_sim <- length(debits_q) * (duree_montee + duree_palier)
    hydro_file <- file.path(current_buse_output_dir, "hydrogramme_complet.loi")
    
    con_q <- NULL
    tryCatch({
      con_q <- file(hydro_file, open = "wt")
      writeLines(c("# Hydrogramme...", "# T(s) Q(m3/s)", "S"), con_q)
      current_time <- 0.0
      writeLines(paste(sprintf("%.1f", current_time), "0.20000"), con_q)
      for (q in debits_q) { 
        current_time <- current_time + duree_montee
        writeLines(paste(sprintf("%.1f", current_time), sprintf("%.5f", q)), con_q)
        current_time <- current_time + duree_palier
        writeLines(paste(sprintf("%.1f", current_time), sprintf("%.5f", q)), con_q) 
      }
    }, finally = {
      if(!is.null(con_q)) close(con_q)
    })
    
    message("      - Génération des cotes Z aval selon la logique personnalisée...")
    cotes_z_aval <- c()
    slope=((nozzle_data$Z1-nozzle_data$Z2)/nozzle_data$LENGTH)
    
    z_start=nozzle_data$Z2-(slope*dist_p4_p6)/2
    z_phase1_end = nozzle_data$Z2
    cotes_z_aval1=seq(from = z_start, to = z_phase1_end, by = 0.5)
    z_phase2_end  <- z_phase1_end + nozzle_data$HAUT1+0.2
    cotes_z_aval2=seq(from = z_phase1_end, to = z_phase2_end, by = 0.1)
    z_phase3_end  <- z_phase1_end + nozzle_data$HAUT1 + 2
    cotes_z_aval3 <- seq(from = z_phase2_end, to = z_phase3_end, by = 0.5)
    cotes_z_aval <- unique(sort(c(cotes_z_aval1,cotes_z_aval2,cotes_z_aval3)))
    
    message(paste("La cote Z aval minimale sera:", sprintf("%.3f", z_start)))
    message(paste("Nombre de conditions aval générées:", length(cotes_z_aval)))
    
    for (z in cotes_z_aval) { 
      filename_z <- file.path(current_buse_output_dir, paste0("limni_Z", sprintf("%.3f", z), ".loi"))
      con_z <- NULL
      tryCatch({
        con_z <- file(filename_z, open = "wt")
        writeLines(c(paste("# Limnigramme... Z=", sprintf("%.3f", z)), "# T(s) Z(m)", "S"), con_z)
        writeLines(paste(sprintf("%.1f", 0.0), sprintf("%.5f", z)), con_z)
        writeLines(paste(sprintf("%.1f", duree_totale_sim), sprintf("%.5f", z)), con_z)
      }, finally = {
        if(!is.null(con_z)) close(con_z)
      })
    }
    message(paste(length(cotes_z_aval), "fichiers .loi générés avec succès."))
    
    #########################################################################
    # PARTIE 3: GÉNÉRATION DES FICHIERS DE PARAMÈTRES (.xcas)
    #########################################################################
    message("\n--- PARTIE 3 : PARAMÈTRES (.xcas) ---")
    limni_files <- list.files(current_buse_output_dir, pattern = "^limni_Z.*\\.loi$", full.names = TRUE)
    
    for (limni_file in limni_files) {
      current_xcas_lines <- readLines(xcas_template_filename, warn = FALSE)
      limni_id <- sub("^limni_(Z.*)\\.loi$", "\\1", basename(limni_file))
      scenario_base_name <- paste0("buse", numero_buse, "_scenario_", limni_id)
      xcas_output_filename <- file.path(current_buse_output_dir, paste0(scenario_base_name, ".xcas"))
      
      pas_stockage_value <- ceiling((duree_montee + duree_palier) / current_pas_temps)
      critere_arret_value <- 1
      
      current_xcas_lines <- gsub("<fichMotsCles>.*</fichMotsCles>", paste0("<fichMotsCles>", scenario_base_name, ".xcas</fichMotsCles>"), current_xcas_lines)
      current_xcas_lines <- gsub("<fichier>.*</fichier>", paste0("<fichier>", geo_filename_with_ext, "</fichier>"), current_xcas_lines)
      current_xcas_lines <- gsub("<abscFin>.*</abscFin>", paste0("<abscFin>", sprintf("%.3f", model_total_length), "</abscFin>"), current_xcas_lines)
      current_xcas_lines <- gsub("<absFinZone>.*</absFinZone>", paste0("<absFinZone>", sprintf("%.3f", model_total_length), "</absFinZone>"), current_xcas_lines)
      current_xcas_lines <- gsub("<pasTemps>.*</pasTemps>", paste0("<pasTemps>", sprintf("%.1f", current_pas_temps), "</pasTemps>"), current_xcas_lines)
      current_xcas_lines <- gsub("<tempsMax>.*</tempsMax>", paste0("<tempsMax>", sprintf("%.1f", duree_totale_sim), "</tempsMax>"), current_xcas_lines)
      current_xcas_lines <- gsub("<critereArret>.*</critereArret>", paste0("<critereArret>", critere_arret_value, "</critereArret>"), current_xcas_lines)
      current_xcas_lines <- gsub("<pasStock>.*</pasStock>", paste0("<pasStock>", sprintf("%.0f", pas_stockage_value), "</pasStock>"), current_xcas_lines)
      current_xcas_lines <- gsub("<fichResultat>.*</fichResultat>", paste0("<fichResultat>", scenario_base_name, "_ecr.opt</fichResultat>"), current_xcas_lines)
      current_xcas_lines <- gsub("<fichListing>.*</fichListing>", paste0("<fichListing>", scenario_base_name, ".lis</fichListing>"), current_xcas_lines)
      current_xcas_lines <- gsub("<valeursPas>.*</valeursPas>",paste0("<valeursPas>",sprintf("%.2f", val_pas_planimetrage),"</valeursPas>"), current_xcas_lines)
      current_xcas_lines <- gsub("<numDerProf>.*</numDerProf>", paste0("<numDerProf>", num_profiles_total, "</numDerProf>"), current_xcas_lines)
      
      pas_maillage_str <- paste(current_pas_maillage, collapse = " ")
      current_xcas_lines <- gsub("<num1erProfPlage>.*</num1erProfPlage>", "<num1erProfPlage>1 3 5</num1erProfPlage>", current_xcas_lines)
      current_xcas_lines <- gsub("<numDerProfPlage>.*</numDerProfPlage>", paste0("<numDerProfPlage>2 4 ", num_profiles_total, "</numDerProfPlage>"), current_xcas_lines)
      current_xcas_lines <- gsub("<pasEspacePlage>.*</pasEspacePlage>", paste0("<pasEspacePlage>", pas_maillage_str, "</pasEspacePlage>"), current_xcas_lines)
      
      start_frottement_idx <- grep("<frottement>", current_xcas_lines)[1]
      end_frottement_idx <- grep("</frottement>", current_xcas_lines)[1]
      if (!is.na(start_frottement_idx) && !is.na(end_frottement_idx)) {
        abs_deb_zone_str <- paste(sprintf("%.3f", x_coords[c(1, 3, 5)]), collapse = " ")
        abs_fin_zone_str <- paste(sprintf("%.3f", x_coords[c(2, 4, 6)]), collapse = " ")
        strickler_zones_str <- paste(sprintf("%.1f", strickler_zones), collapse = " ")
        frottement_block <- c("      <frottement>", "        <loi>1</loi>", "        <nbZone>3</nbZone>", "        <numBranche>1 1 1</numBranche>", paste0("        <absDebZone>", abs_deb_zone_str, "</absDebZone>"), paste0("        <absFinZone>", abs_fin_zone_str, "</absFinZone>"), paste0("        <coefLitMin>", strickler_zones_str, "</coefLitMin>"), paste0("        <coefLitMaj>", strickler_zones_str, "</coefLitMaj>"), "      </frottement>")
        current_xcas_lines <- c(current_xcas_lines[1:(start_frottement_idx - 1)], frottement_block, current_xcas_lines[(end_frottement_idx + 1):length(current_xcas_lines)])
      }
      
      start_index <- grep("<parametresLoisHydrauliques>", current_xcas_lines)[1]
      end_index <- grep("</parametresLoisHydrauliques>", current_xcas_lines)[1]
      if (!is.na(start_index) && !is.na(end_index)) {
        lois_lines <- c("  <parametresLoisHydrauliques>","    <nb>2</nb>","    <lois>","      <structureParametresLoi>","        <nom>hydro_amont</nom>","        <type>1</type>","        <donnees>","          <modeEntree>1</modeEntree>",paste0("          <fichier>", basename(hydro_file), "</fichier>"),"        </donnees>","      </structureParametresLoi>","      <structureParametresLoi>",paste0("        <nom>limni_", limni_id, "</nom>"),"        <type>2</type>","        <donnees>","          <modeEntree>1</modeEntree>",paste0("          <fichier>", basename(limni_file), "</fichier>"),"        </donnees>","      </structureParametresLoi>","    </lois>","  </parametresLoisHydrauliques>")
        current_xcas_lines <- c(current_xcas_lines[1:(start_index-1)], lois_lines, current_xcas_lines[(end_index+1):length(current_xcas_lines)])
      }
      
      con <- NULL
      tryCatch({
        con <- file(xcas_output_filename, "w")
        writeLines(current_xcas_lines, con)
      }, finally = {
        if(!is.null(con)) close(con)
      })
    }
    message("--- Fichiers .xcas générés avec succès. ---")
    
    # PARTIE 4
    message("\n--- PARTIE 4 : LANCEMENT DES SIMULATIONS ---")
    xcas_files <- dir_ls(current_buse_output_dir, regexp = "\\.xcas$", recurse = FALSE)
    registerDoParallel(cores = num_cores_sim)
    foreach( xcas_file_path = xcas_files, .combine = 'c', .errorhandling = 'pass') %dopar% {
      library(fs)
      xcas_filename <- path_file(xcas_file_path)
      xcas_basename <- path_ext_remove(xcas_filename)
      temp_sim_dir <- path(current_buse_output_dir, paste0("temp_", xcas_basename))
      results_dir <- path(current_buse_output_dir, "results_mascaret")
      
      final_opt_path <- path(results_dir, paste0(xcas_basename, "_ecr.opt"))
      
      if(!file_exists(final_opt_path) || file_info(final_opt_path)$size == 0) {
        dir_create(temp_sim_dir)
        file_copy(xcas_file_path, path(temp_sim_dir, xcas_filename), overwrite = TRUE)
        file_copy(path(current_buse_output_dir, geo_filename_with_ext), temp_sim_dir, overwrite = TRUE)
        file_copy(path(current_buse_output_dir, "hydrogramme_complet.loi"), temp_sim_dir, overwrite = TRUE)
        limni_id_for_copy <- sub(paste0("buse", numero_buse, "_scenario_"), "", xcas_basename)
        limni_to_copy <- path(current_buse_output_dir, paste0("limni_", limni_id_for_copy, ".loi"))
        if(file_exists(limni_to_copy)) file_copy(limni_to_copy, temp_sim_dir, overwrite = TRUE)
        
        batch_content <- sprintf('@echo off\nCALL "%s"\ncd /D "%%~dp0"\npython "%%HOMETEL%%\\scripts\\python3\\mascaret.py" "%s"\nexit /b %%ERRORLEVEL%%', telemac_env_script, xcas_filename)
        batch_file_path <- path(temp_sim_dir, "run_simulation.bat")
        writeLines(batch_content, batch_file_path)
        
        system2("cmd", args = c("/c", path_abs(batch_file_path)), stdout = TRUE, stderr = TRUE)
      }
    }
    stopImplicitCluster()
    message("--- Lancement des simulations terminé. ---")
    
    # NOUVELLE PARTIE 4.5: Copie des résultats après la fin de tous les calculs
    message("\n--- PARTIE 4.5 : COLLECTE DES RÉSULTATS ---")
    results_dir <- path(current_buse_output_dir, "results_mascaret")
    dir_create(results_dir)
    temp_dirs <- dir_ls(current_buse_output_dir, regexp = "temp_buse", type = "directory")
    
    for(temp_dir in temp_dirs) {
      opt_files_in_temp <- dir_ls(temp_dir, regexp = "\\.(opt|lis)$")
      if(length(opt_files_in_temp) > 0) {
        file_copy(opt_files_in_temp, results_dir, overwrite = TRUE)
      }
    }
    message("--- Collecte des fichiers .opt terminée. ---")
    
    
    # PARTIE 5
    message("\n--- PARTIE 5 : LECTURE DES RÉSULTATS ET CRÉATION DE L'ABAQUE ---")
    opt_dir <- file.path(current_buse_output_dir, "results_mascaret")
    
    ABSCISSA_BUSE_ENTREE_TARGET <- x_coords[3]
    ABSCISSA_BUSE_SORTIE_TARGET <- x_coords[4]
    
    opt_files <- list.files(opt_dir, pattern = "\\.opt$", full.names = TRUE)
    results_list <- list()
    
    for (opt_file in opt_files) {
      opt_content_lines <- readLines(opt_file, warn = FALSE)
      resultats_start_line_index <- grep("[resultats]", opt_content_lines, fixed = TRUE)
      if (length(resultats_start_line_index) == 0) next
      opt_data_text <- opt_content_lines[(resultats_start_line_index[1] + 1):length(opt_content_lines)]
      if(length(opt_data_text) == 0 || all(nchar(trimws(opt_data_text)) == 0)) next
      
      opt_data <- tryCatch({ fread(text = opt_data_text, sep = ";", strip.white=TRUE, header = FALSE, colClasses = "numeric", fill = TRUE, select = c(1, 4, 6, 11, 12, 14), col.names = c("Time", "Abscissa", "S_value", "Froude", "Velocity", "DebitTotal")) }, error = function(e) { NULL })
      if(is.null(opt_data) || nrow(opt_data) == 0) next
      
      plateau_end_times <- (1:length(debits_q)) * (duree_montee + duree_palier)
      
      for(time_step in plateau_end_times){
        time_to_extract <- time_step - current_pas_temps
        if(time_to_extract < 0 || time_to_extract > max(opt_data$Time)) next
        
        steady_state_data <- opt_data[abs(opt_data$Time - time_to_extract) < 1e-3, ]
        if(nrow(steady_state_data) == 0) next
        
        entree_buse_row <- steady_state_data[which.min(abs(steady_state_data$Abscissa - ABSCISSA_BUSE_ENTREE_TARGET)), ]
        sortie_buse_row <- steady_state_data[which.min(abs(steady_state_data$Abscissa - ABSCISSA_BUSE_SORTIE_TARGET)), ]
        
        if(nrow(entree_buse_row) == 1 && nrow(sortie_buse_row) == 1){
          avg_velocity <- (entree_buse_row$Velocity + sortie_buse_row$Velocity) / 2
          
          results_list[[length(results_list) + 1]] <- data.frame(
            Debit = entree_buse_row$DebitTotal, 
            S1 = entree_buse_row$S_value, 
            S2 = sortie_buse_row$S_value,
            Froude_amont = entree_buse_row$Froude,
            Avg_Velocity = avg_velocity
          )
        }
      }
    }
    
    results_for_file <- data.frame()
    all_results_df <- data.frame()
    results_df_sorted <- data.frame()
    
    if (length(results_list) > 0) {
      all_results_df <- do.call(rbind, results_list)
      s1_max_threshold <- nozzle_data$Z1 + nozzle_data$HAUT1 + 10
      
      all_results_filtered <- all_results_df[
        all_results_df$S1 <= s1_max_threshold & 
          all_results_df$Froude_amont <= 5 &
          # all_results_df$Avg_Velocity < 7.0 &
          (all_results_df$S1 - all_results_df$S2) >= 0.05,
      ]
      if(nrow(all_results_filtered) > 0) {
        results_df_unique <- unique(all_results_filtered)
        results_df_sorted <- results_df_unique[order(results_df_unique$Debit, results_df_unique$S1), ]
        
        final_points_list <- lapply(split(results_df_sorted, results_df_sorted$Debit), function(df_debit) {
          torrential_points <- df_debit %>% filter(Froude_amont >= 1)
          if (nrow(torrential_points) > 0) return(torrential_points)
          good_fluvial_points <- df_debit %>% filter(Froude_amont < 1 & S1 < (nozzle_data$Z1 + nozzle_data$HAUT1) & S2 < (nozzle_data$Z2 + nozzle_data$HAUT1))
          if (nrow(good_fluvial_points) > 0) return(good_fluvial_points)
          return(df_debit %>% filter(Froude_amont < 1))
        })
        
        results_for_file <- bind_rows(final_points_list) %>% unique() %>% arrange(Debit, S1)
        if(length(which(results_for_file$Froude_amont>=1))>0){
          max_debit_torrential=max(results_for_file$Debit[which(results_for_file$Froude_amont>=1)])
          max_S1_torrentiel=max(results_for_file$S1[which(results_for_file$Debit==max_debit_torrential)])
          max_S2_torrentiel=max(results_for_file$S2[which(results_for_file$Debit==max_debit_torrential)])
          
          points_fluviaux1=which(results_for_file$Debit<max_debit_torrential & (results_for_file$S1 >max_S1_torrentiel | results_for_file$S2 >max_S2_torrentiel ) )
          points_fluviaux2=which(results_for_file$Debit>max_debit_torrential & results_for_file$S2 >nozzle_data$Z2 + nozzle_data$HAUT1 + 0.05  )
          if(length(points_fluviaux1)>0 | length(points_fluviaux2)>0){
            results_for_file=results_for_file[-c(points_fluviaux1,points_fluviaux2),]
            results_for_file <- results_for_file %>% unique() %>% arrange(Debit, S1)
          }
        }
        
        output_txt_file_S_partiel <- file.path(current_buse_output_dir, "abaque_partiel.txt")
        if (nrow(results_for_file) > 0) {
          file_conn_partiel <- file(output_txt_file_S_partiel, "w")
          writeLines(c(as.character(numero_buse), as.character(nrow(results_for_file))), file_conn_partiel)
          cat(paste(sprintf("%.2f", results_for_file$Debit), collapse = ","), "\n", file = file_conn_partiel)
          cat(paste(sprintf("%.2f", results_for_file$S1), collapse = ","), "\n", file = file_conn_partiel)
          cat(paste(sprintf("%.2f", results_for_file$S2), collapse = ","), "\n", file = file_conn_partiel)
          close(file_conn_partiel)
        }
        
        output_txt_file_QS1_partiel <- file.path(current_buse_output_dir, "abaque_partiel_QS1.txt")
        if (nrow(results_for_file) > 0) {
          results_QS1_unique <- results_for_file %>% dplyr::select(Debit, S1) %>% distinct() %>% arrange(Debit, S1)
          file_conn_QS1 <- file(output_txt_file_QS1_partiel, "w")
          writeLines(c(as.character(numero_buse), as.character(nrow(results_QS1_unique))), file_conn_QS1)
          cat(paste(sprintf("%.2f", results_QS1_unique$Debit), collapse = ","), "\n", file = file_conn_QS1)
          cat(paste(sprintf("%.2f", results_QS1_unique$S1), collapse = ","), "\n", file = file_conn_QS1)
          close(file_conn_QS1)
        }
      }
    }
    
    if(generate_long_profile_plots) {
      message("\n--- GÉNÉRATION DES PROFILS EN LONG ---")
      for (opt_file in opt_files) {
        scenario_name <- sub("_ecr\\.opt$", "", basename(opt_file)); opt_content_lines <- readLines(opt_file, warn = FALSE); resultats_start_line_index <- grep("[resultats]", opt_content_lines, fixed = TRUE); if (length(resultats_start_line_index) == 0) next
        opt_data_full <- tryCatch({ fread(text = opt_content_lines[(resultats_start_line_index[1] + 1):length(opt_content_lines)], sep = ";", strip.white=TRUE, header = FALSE, colClasses = "numeric", fill = TRUE, select = c(1, 4, 5, 6, 14), col.names = c("Time", "Abscissa", "Z_fond", "Z_eau", "DebitTotal")) }, error = function(e) { NULL })
        if(is.null(opt_data_full) || nrow(opt_data_full) == 0) next
        fig_pl <- plot_ly(); profil_fond <- opt_data_full[abs(Time - min(Time)) < 1e-3, ]; fig_pl <- add_trace(fig_pl, data = profil_fond, x = ~Abscissa, y = ~Z_fond, type = 'scatter', mode = 'lines', line = list(color = 'black', width = 3), name = 'Fond')
        for (time_p in ((1:length(debits_q)) * (duree_montee + duree_palier) - current_pas_temps)) { if (time_p > 0 && time_p <= max(opt_data_full$Time)) { steady_data <- opt_data_full[abs(opt_data_full$Time - time_p) < 1e-3, ]; if (nrow(steady_data) > 0) fig_pl <- add_trace(fig_pl, data = steady_data, x = ~Abscissa, y = ~Z_eau, type = 'scatter', mode = 'lines', name = paste('Q =', round(steady_data$DebitTotal[1], 2), 'm³/s')) } }
        y_range <- range(c(profil_fond$Z_fond, opt_data_full$Z_eau), na.rm = TRUE); fig_pl <- fig_pl %>% layout(title = scenario_name, xaxis = list(title = "Abscisse"), yaxis = list(title = "Altitude", range = y_range), shapes = list(list(type = 'line', x0 = x_coords[indices$p3], x1 = x_coords[indices$p3], y0 = y_range[1], y1 = y_range[2], line = list(color = 'grey', dash = 'dash')), list(type = 'line', x0 = x_coords[indices$p4], x1 = x_coords[indices$p4], y0 = y_range[1], y1 = y_range[2], line = list(color = 'grey', dash = 'dash')), list(type = 'line', x0 = x_coords[indices$p3], x1 = x_coords[indices$p4], y0 = nozzle_data$Z1 + nozzle_data$HAUT1, y1 = nozzle_data$Z1 + nozzle_data$HAUT1, line = list(color = 'red', dash = 'longdash'))))
        saveWidget(fig_pl, file = file.path(opt_dir, paste0("profil_en_long_", scenario_name, ".html")), selfcontained = TRUE)
      }
    }
    
    
    if(generate_abaque_plots) {
      message("\n--- PARTIE 6 : GÉNÉRATION DES GRAPHIQUES D'ABAQUE ---")
      
      # Utiliser `all_results_df` pour définir les points à estomper
      points_to_keep <- results_for_file
      points_to_fade <- dplyr::anti_join(all_results_df, points_to_keep, by = c("Debit", "S1", "S2", "Froude_amont", "Avg_Velocity"))
      
      if (nrow(points_to_keep) > 0 || nrow(points_to_fade) > 0) {
        
        # Préparation des données pour les points principaux
        results_df_graph <- points_to_keep
        if(nrow(results_df_graph) > 0) {
          colnames(results_df_graph) <- c("Debit", "S1_Charge_Amont", "S2_Charge_Aval", "Froude_amont", "Avg_Velocity")
          results_df_graph$Froude_Condition <- cut(results_df_graph$Froude_amont, 
                                                   breaks = c(-Inf, 0.99, 1.01, Inf), 
                                                   labels = c("Fluvial (Fr < 1)", "Critique (Fr = 1)", "Torrentiel (Fr > 1)"))
        }
        
        # --- Graphique 3D ---
        title_3d <- paste("Relation Débit-Charges S1/S2 - Buse", numero_buse, "<br>", title_details_str)
        fig_3d_S <- plot_ly()
        
        if(nrow(points_to_fade) > 0) {
          fig_3d_S <- fig_3d_S %>% add_trace(
            data = points_to_fade, x=~Debit, y=~S2, z=~S1,
            type='scatter3d', mode='markers',
            marker=list(size=3, color='rgba(211, 211, 211, 0.5)'), # Gris clair, semi-transparent
            name = "Points exclus",
            hoverinfo = "text",
            text = ~paste('Exclu<br>Débit:', sprintf("%.2f", Debit), '<br>S1:', sprintf("%.2f", S1), '<br>S2:', sprintf("%.2f", S2), '<br>Froude:', sprintf("%.2f", Froude_amont))
          )
        }
        
        if(nrow(results_df_graph) > 0) {
          fig_3d_S <- fig_3d_S %>% add_trace(
            data = results_df_graph, x=~Debit, y=~S2_Charge_Aval, z=~S1_Charge_Amont,
            type='scatter3d', mode='markers',
            color=~S1_Charge_Amont, colorscale='Viridis',
            symbol=~Froude_Condition, symbols=c('circle','square','triangle-up'),
            marker=list(size=4),
            customdata=~Froude_amont,
            name = "Points de l'abaque",
            hovertemplate=paste('<b>Débit:</b> %{x:.2f} m³/s<br>',
                                '<b>S2 (Aval):</b> %{y:.2f} m<br>',
                                '<b>S1 (Amont):</b> %{z:.2f} m<br>',
                                '<b>Froude (Amont):</b> %{customdata:.2f}<br>',
                                '<extra></extra>')
          )
        }
        
        fig_3d_S <- fig_3d_S %>% layout(title=title_3d,
                                        legend=list(title=list(text='<b>Régime Froude</b>')),
                                        scene=list(xaxis=list(title="Débit (m³/s)"),
                                                   yaxis=list(title="S2 Charge Aval (m)"),
                                                   zaxis=list(title="S1 Charge Amont (m)")))
        
        # --- Graphique 2D ---
        title_2d <- paste("Relation S1-S2 - Buse", numero_buse, "<br>", title_details_str)
        fig_2d_S <- plot_ly()
        
        if(nrow(points_to_fade) > 0) {
          fig_2d_S <- fig_2d_S %>% add_trace(
            data = points_to_fade, x=~S1, y=~S2,
            type='scatter', mode='markers',
            marker=list(size=5, color='rgba(211, 211, 211, 0.6)'), # Gris clair, semi-transparent
            name = "Points exclus",
            hoverinfo = "text",
            text = ~paste('Exclu<br>Débit:', sprintf("%.2f", Debit), '<br>S1:', sprintf("%.2f", S1), '<br>S2:', sprintf("%.2f", S2), '<br>Froude:', sprintf("%.2f", Froude_amont))
          )
        }
        
        if(nrow(results_df_graph) > 0) {
          fig_2d_S <- fig_2d_S %>% add_trace(
            data = results_df_graph, x=~S1_Charge_Amont, y=~S2_Charge_Aval,
            type='scatter', mode='markers',
            color=~Debit, colorscale='Plasma',
            symbol=~Froude_Condition, symbols=c('circle','square','triangle-up'),
            marker=list(size=6),
            customdata=~Froude_amont,
            name = "Points de l'abaque",
            hovertemplate=paste('<b>S1 (Amont):</b> %{x:.2f} m<br>',
                                '<b>S2 (Aval):</b> %{y:.2f} m<br>',
                                '<b>Débit:</b> %{marker.color:.2f} m³/s<br>',
                                '<b>Froude:</b> %{customdata:.2f}<br>',
                                '<extra></extra>')
          )
          
          shapes_list <- list(
            list(type = 'line', x0 = nozzle_data$Z1, x1 = nozzle_data$Z1, y0 = min(results_df_graph$S2_Charge_Aval), y1 = max(results_df_graph$S2_Charge_Aval), line = list(color = 'blue', dash = 'longdash', width = 2)),
            list(type = 'line', x0 = nozzle_data$Z1 + nozzle_data$HAUT1, x1 = nozzle_data$Z1 + nozzle_data$HAUT1, y0 = min(results_df_graph$S2_Charge_Aval), y1 = max(results_df_graph$S2_Charge_Aval), line = list(color = 'red', dash = 'longdash', width = 2)),
            list(type = 'line', y0 = nozzle_data$Z2, y1 = nozzle_data$Z2, x0 = min(results_df_graph$S1_Charge_Amont), x1 = max(results_df_graph$S1_Charge_Amont), line = list(color = 'blue', dash = 'longdash', width = 2)),
            list(type = 'line', y0 = nozzle_data$Z2 + nozzle_data$HAUT1, y1 = nozzle_data$Z2 + nozzle_data$HAUT1, x0 = min(results_df_graph$S1_Charge_Amont), x1 = max(results_df_graph$S1_Charge_Amont), line = list(color = 'red', dash = 'longdash', width = 2))
          )
          fig_2d_S <- fig_2d_S %>% layout(shapes = shapes_list)
        }
        
        fig_2d_S <- fig_2d_S %>% layout(
          title=title_2d,
          xaxis=list(title="S1 Charge Amont (m)"),
          yaxis=list(title="S2 Charge Aval (m)"),
          legend=list(title=list(text='<b>Régime Froude</b>'))
        )
        
        htmlwidgets::saveWidget(fig_3d_S, file.path(current_buse_output_dir, "graph_abaque_S_3d.html"), selfcontained = TRUE)
        htmlwidgets::saveWidget(fig_2d_S, file.path(current_buse_output_dir, "graph_abaque_S_2d.html"), selfcontained = TRUE)
        message("Graphiques d'abaque 2D et 3D (incluant les points exclus) exportés.")
        
      } else {
        message("Aucune donnée à visualiser, les graphiques d'abaque n'ont pas été générés pour cette buse.")
      }
    }
    
    # --- BLOC DE NETTOYAGE SÉLECTIF (AJOUTÉ v123) ---
    if (cleanup_calc_files) {
      message(paste("      - Nettoyage sélectif pour la buse", numero_buse))
      
      # 1. Identifier tous les dossiers temporaires temp_...
      tdirs <- fs::dir_ls(current_buse_output_dir, regexp = "temp_", type = "directory")
      if (length(tdirs) > 1) {
        # Garder uniquement le premier dossier pour vérification, supprimer les autres
        fs::dir_delete(tdirs[-1]) 
      }
      
      # 2. Supprimer le dossier global results_mascaret (doublon inutile une fois les données extraites)
      res_fold <- file.path(current_buse_output_dir, "results_mascaret")
      if(fs::dir_exists(res_fold)) fs::dir_delete(res_fold)
      
      # 3. Supprimer les fichiers d'entrée à la racine (déjà sauvegardés dans le dossier temp conservé)
      root_files <- fs::dir_ls(current_buse_output_dir, regexp = "\\.(xcas|geo|loi|lis)$")
      if(length(root_files) > 0) fs::file_delete(root_files)
    }
  } 
  
  #########################################################################
  # DÉMARRAGE DE LA BOUCLE PRINCIPALE
  #########################################################################
  if (ncore_buse <= 1) {
    message("--- DÉMARRAGE DU TRAITEMENT SÉQUENTIEL ---")
    for (i in 1:nrow(busel_data)) {
      process_buse(i, busel_data, main_output_dir, telemac_env_script, xcas_template_filename, use_mnt_for_profiles, mnt_path, dist_amont_limite, dist_p4_p6, num_points_per_profile, dist_extraction_profil, first_profile_shape, duree_palier, duree_montee, num_cores_sim, strickler_zones_base, val_pas_planimetrage, g, pas_maillage_court, pas_temps_court, pas_maillage_moyen, pas_temps_moyen, pas_maillage_long, pas_temps_long, preissmann_slot_width, generate_long_profile_plots, generate_abaque_plots, cleanup_calc_files)
    }
  } else {
    message(paste("--- DÉMARRAGE DU TRAITEMENT PARALLÈLE SUR", ncore_buse, "CŒURS ---"))
    registerDoParallel(cores = ncore_buse)
    foreach(i = 1:nrow(busel_data)) %dopar% { 
      process_buse(i, busel_data, main_output_dir, telemac_env_script, xcas_template_filename, use_mnt_for_profiles, mnt_path, dist_amont_limite, dist_p4_p6, num_points_per_profile, dist_extraction_profil, first_profile_shape, duree_palier, duree_montee, num_cores_sim, strickler_zones_base, val_pas_planimetrage, g, pas_maillage_court, pas_temps_court, pas_maillage_moyen, pas_temps_moyen, pas_maillage_long, pas_temps_long, preissmann_slot_width, generate_long_profile_plots, generate_abaque_plots, cleanup_calc_files) 
    }
    stopImplicitCluster()
  }
  
  #########################################################################
  # PARTIE 7: ASSEMBLAGE FINAL DE L'ABAQUE
  #########################################################################
  message("\n--- PARTIE 7 : ASSEMBLAGE DU FICHIER D'ABAQUE FINAL ---")
  output_txt_file_S_global <- file.path(main_output_dir, "resultats_buse_format_abaqueSQ.txt")
  file_conn_main <- file(output_txt_file_S_global, "w")
  writeLines("#abaqueSQ", file_conn_main)
  output_txt_file_QS1_global <- file.path(main_output_dir, "abaques.txt")
  file_conn_qs1 <- file(output_txt_file_QS1_global, "w")
  writeLines("#abaqueQS1", file_conn_qs1)
  
  for (i in 1:nrow(busel_data)) {
    numero_buse <- busel_data$id[i]
    current_buse_output_dir <- file.path(main_output_dir, paste0("buse_", numero_buse))
    
    p1 <- file.path(current_buse_output_dir, "abaque_partiel.txt")
    if (file.exists(p1)) writeLines(readLines(p1), file_conn_main)
    
    p2 <- file.path(current_buse_output_dir, "abaque_partiel_QS1.txt")
    if (file.exists(p2)) writeLines(readLines(p2), file_conn_qs1)
  }
  
  close(file_conn_main); close(file_conn_qs1)
  message("\n--- WORKFLOW COMPLET TERMINÉ ---")
  file.copy(from = file.path(main_output_dir, "abaques.txt"),to = dsnlayerC,overwrite = TRUE,recursive = TRUE)
  # 1. Lire toutes les lignes du fichier d'origine
  lignes <- readLines(file.path(dsnlayerC, "abaques.txt"))
  
  # 2. Nettoyer les éventuelles lignes vides à la fin du fichier
  lignes <- lignes[lignes != ""]
  
  # 3. Calculer le nombre de buses
  # On soustrait 1 (pour la ligne de commentaire) avant de diviser par 4
  n_buses <- floor((length(lignes) - 1) / 4)
  
  # 4. Générer les indices des IDs
  # On commence à la ligne 2, et on fait des sauts de 4
  indices_id <- seq(2, by = 4, length.out = n_buses)
  
  # 5. Remplacer les lignes d'identifiants par la nouvelle numérotation
  lignes[indices_id] <- 1:n_buses
  
  # 6. Sauvegarder le résultat en écrasant le fichier
  writeLines(as.character(lignes), file.path(dsnlayerC, "abaques.txt"))
  # Script R pour lisser les abaques Telemac-2D
  # Objectif : Garantir une croissance stricte des hauteurs d'eau (S1) 
  # pour éviter les divisions par zéro dans DISCHARGE_INTERPOL.
  
  # Noms des fichiers (à adapter si besoin)
  file_in <- file.path(dsnlayerC, "abaques.txt")
  file_out <- file.path(dsnlayerC, "abaques2.txt")
  
  # Lecture du fichier
  lines <- readLines(file_in, warn = FALSE)
  # Nettoyage des lignes vides éventuelles
  lines <- lines[trimws(lines) != ""]
  
  # On conserve l'entête (ex: #abaqueQS1)
  output_lines <- c(lines[1])
  
  idx <- 2
  while (idx <= length(lines)) {
    # Extraction du bloc de 4 lignes
    abaque_id <- trimws(lines[idx])
    n_pts     <- as.integer(trimws(lines[idx+1]))
    q_str     <- lines[idx+2]  # On garde la ligne des débits intacte
    s1_str    <- lines[idx+3]
    
    # Conversion de la ligne S1 en vecteur numérique
    s1_vals <- as.numeric(unlist(strsplit(s1_str, ",")))
    s1_fixed <- s1_vals
    n <- length(s1_fixed)
    
    # --- ALGORITHME DE LISSAGE (STRICTEMENT CROISSANT) ---
    i <- 1
    while (i < n) {
      if (s1_fixed[i] >= s1_fixed[i+1]) {
        
        # 1. On cherche le prochain index où S1 finit par monter
        next_idx <- -1
        for (j in (i+1):n) {
          if (s1_fixed[j] > s1_fixed[i]) {
            next_idx <- j
            break
          }
        }
        
        # 2. Si on a trouvé un point plus haut, on lisse tout l'intervalle
        if (next_idx != -1) {
          # seq() crée une interpolation linéaire parfaite
          s1_fixed[i:next_idx] <- seq(s1_fixed[i], s1_fixed[next_idx], length.out = next_idx - i + 1)
          i <- next_idx # On saute directement à la fin de notre lissage
          
          # 3. Sécurité extrême : si la courbe ne remonte JAMAIS jusqu'à la fin
        } else {
          s1_fixed[i+1] <- s1_fixed[i] + 0.001
          i <- i + 1
        }
      } else {
        # Tout va bien, croissance normale, on avance d'un point
        i <- i + 1
      }
    }
    
    # Formatage de la ligne de sortie (on arrondit à 4 décimales pour rester propre)
    s1_out <- paste(round(s1_fixed, 4), collapse=",")
    
    # Stockage des 4 lignes du bloc corrigé
    output_lines <- c(output_lines, abaque_id, n_pts, q_str, s1_out)
    
    # On passe à l'abaque suivant
    idx <- idx + 4
  }
  
  # Écriture du nouveau fichier
  writeLines(output_lines, file_out)
  cat("Traitement terminé avec succès !\nLe fichier corrigé est :", file_out, "\n")
}
abacus=function(){
  library(sf)
  library(terra)
  library(dplyr)
  library(fs)
  library(stringr)
  library(foreach)
  library(doParallel)
  library(data.table)
  library(plotly)
  library(htmlwidgets)
  
  #########################################################################
  # PARTIE 0: PARAMÈTRES UTILISATEUR
  # Modifiez les valeurs dans cette section pour configurer votre simulation.
  #########################################################################
  # --- 0.1. Configuration des chemins et fichiers ---
  # chem_routine <- "C:/Users/CartoNationale/Desktop/Nabil"
  
  main_output_dir <- file.path(dsnlayerC, "abaques") 
  telemac_env_script <- "C:/TELEMAC/V8P4/configs/pysource.bat"
  xcas_template_filename <- file.path(chem_routine,"C2D",Doss_Base, "masc.xcas.txt")
  
  # --- 0.2. Paramètres de parallélisation ---
  ncore_buse <- ncore_buse_abaques # Nombre de cœurs pour la boucle sur les buses. 1 = séquentiel, >=2 = parallèle
  num_cores_sim <- num_cores_sim_abaques
  # Nombre de cœurs pour les simulations d'une MÊME buse
  
  # --- 0.3. Contrôle de la géométrie --- 
  use_mnt_for_profiles <- FALSE # Mettre à TRUE pour extraire du MNT, FALSE pour utiliser des profils trapézoïdaux
  
  # --- Chemins pour le mode MNT ---
  shapefile_path <- file.path(dsnlayerC, "OuvHydrau_Select.shp") 
  mnt_path <- file.path(dsnlayerC, "MNTFinal.gpkg") # <--- CHEMIN VERS VOTRE MNT
  
  # --- 0.4. Paramètres de la géométrie ---
  dist_amont_limite <- 1000.0
  dist_p4_p6 <- 2000 # Distance entre la sortie de la buse (corrigée à p5) et la limite aval (p6)
  num_points_per_profile <- 101
  dist_extraction_profil <- 3.0 
  preissmann_slot_width <- 0.01 # Largeur de la fente de Preissmann en mètres
  
  first_profile_shape <- list(
    type = "trapezoidal", bottom_width = 3, left_slope = 1/1.5,
    right_slope = 1/1.5, bank_height_above_bottom = 40
  )
  
  # --- 0.5. Paramètres temporels et hydrauliques ---
  duree_palier <- 400
  duree_montee <- 50
  strickler_zones_base <- c(20.0, 20.0, 20.0) 
  val_pas_planimetrage <- 0.02
  g <- 9.81
  
  # --- 0.6. Paramètres de maillage et de temps de simulation adaptatifs ---
  # Pour buses COURTES (< 20m)
  pas_maillage_court <- c(1, 0.5, 1)
  pas_temps_court <- 0.5
  duree_palier_court <- 20
  duree_montee_court <- 10
  
  # Pour buses MOYENNES (20m à 100m)
  pas_maillage_moyen <- c(1, 1, 1)
  pas_temps_moyen <- 1
  duree_palier_moyen <- 100
  duree_montee_moyen <- 20
  # Pour buses LONGUES (> 100m)
  pas_maillage_long <- c(2, 2, 2)
  pas_temps_long <- 2
  duree_palier_long <- 400
  duree_montee_long <- 50
  # --- 0.7. Paramètres de sortie (NOUVEAU) ---
  generate_long_profile_plots <- TRUE # Mettre à TRUE pour générer les graphiques de profil en long
  generate_abaque_plots <- TRUE       # Mettre à TRUE pour générer les graphiques d'abaque 2D/3D
  cleanup_calc_files <- FALSE           # <--- NOUVEAU: TRUE pour garder 1 dossier temp et supprimer le reste
  
  
  #########################################################################
  # LECTURE DES DONNÉES SPATIALES ET INITIALISATION
  #########################################################################
  message("--- LECTURE DES DONNÉES SPATIALES ---")
  if (!file.exists(shapefile_path)) stop("Le fichier shapefile n'a pas été trouvé: ", shapefile_path)
  busel_data <- st_read(shapefile_path, quiet = TRUE)
  message(paste("Lecture de", nrow(busel_data), "buse(s) depuis le shapefile."))
  
  mnt_raster <- NULL # Initialisation à NULL
  if(use_mnt_for_profiles) {
    if (!file.exists(mnt_path)) stop("Le fichier MNT n'a pas été trouvé: ", mnt_path)
    message("Chargement du MNT pour vérification CRS...")
    temp_mnt <- rast(mnt_path)
    message("MNT chargé.")
    if (st_crs(busel_data) != st_crs(temp_mnt)) {
      message("ATTENTION: CRS différents. Reprojection du shapefile vers le CRS du MNT.")
      busel_data <- st_transform(busel_data, st_crs(temp_mnt))
    }
    rm(temp_mnt) # On supprime l'objet, il sera rechargé dans chaque worker
    gc() # Force le nettoyage de la mémoire
  }
  
  dir.create(main_output_dir, showWarnings = FALSE, recursive = TRUE)
  
  #########################################################################
  # DÉFINITION DE LA FONCTION DE TRAITEMENT D'UNE BUSE
  #########################################################################
  process_buse <- function(i, busel_data, main_output_dir, telemac_env_script, xcas_template_filename, use_mnt_for_profiles, mnt_path, dist_amont_limite, dist_p4_p6, num_points_per_profile, dist_extraction_profil, first_profile_shape, duree_palier, duree_montee, num_cores_sim, strickler_zones_base, val_pas_planimetrage, g, pas_maillage_court, pas_temps_court, pas_maillage_moyen, pas_temps_moyen, pas_maillage_long, pas_temps_long, preissmann_slot_width, generate_long_profile_plots, generate_abaque_plots, cleanup_calc_files) {
    
    # Charger les librairies nécessaires dans l'environnement parallèle
    library(sf)
    library(terra)
    library(dplyr)
    library(fs)
    library(stringr)
    library(foreach)
    library(doParallel)
    library(data.table)
    library(plotly)
    library(htmlwidgets)
    
    # Si on utilise le MNT, chaque worker charge sa propre copie.
    if (use_mnt_for_profiles) {
      mnt_raster <- rast(mnt_path)
    }
    
    buse_row <- busel_data[i, ]
    numero_buse <- buse_row$id
    
    # Création du dossier de sortie
    current_buse_output_dir <- file.path(main_output_dir, paste0("buse_", numero_buse))
    dir.create(current_buse_output_dir, showWarnings = FALSE, recursive = TRUE)
    
    # -----------------------------------------------------------------------
    # CHECK DE DÉJÀ FAIT (NOUVELLE FONCTIONNALITÉ v124)
    # -----------------------------------------------------------------------
    abaque_check_file <- file.path(current_buse_output_dir, "abaque_partiel.txt")
    if (file.exists(abaque_check_file) && file.info(abaque_check_file)$size > 0) {
      message(paste(">>> BUSE", numero_buse, ": DÉJÀ TRAITÉE (Fichier abaque trouvé). PASSAGE À LA SUIVANTE. <<<"))
      return(NULL)
    }
    # -----------------------------------------------------------------------
    
    CIRC <- buse_row$CIRC
    if(is.na(buse_row$HAUT1) | buse_row$HAUT1==0) buse_row$HAUT1=buse_row$HAUT2
    nozzle_data <- list(LARG = buse_row$LRG, HAUT1 = buse_row$HAUT1, Z1 = buse_row$Z1, Z2 = buse_row$Z2, LENGTH = buse_row$LENGTH)
    first_profile_shape$bottom_width=nozzle_data$LARG*2
    if ("FRIC" %in% names(buse_row) && !is.na(buse_row$FRIC) && buse_row$FRIC > 0) {
      strickler_buse_value <- 1 / buse_row$FRIC
      if (strickler_buse_value < 1) {
        warning(paste("Buse", numero_buse, ": La valeur FRIC=", buse_row$FRIC, "donne un Strickler de", round(strickler_buse_value, 2), ". Utilisation de la valeur par défaut de 50.0."))
        strickler_buse_value <- 60.0
      }
    } else {
      strickler_buse_value <- 60.0
      warning(paste("Buse", numero_buse, ": Colonne 'FRIC' non valide ou absente. Utilisation de la valeur par défaut de Strickler:", strickler_buse_value))
    }
    strickler_zones <- c(strickler_zones_base[1], strickler_buse_value, strickler_zones_base[3])
    
    
    message(paste("\n\n############################################################"))
    message(paste("--- DÉMARRAGE DU TRAITEMENT POUR LA BUSE :", numero_buse, "---"))
    message(paste("--- Fichiers de sortie dans :", current_buse_output_dir, "---"))
    message(paste("############################################################\n"))
    
    if (nozzle_data$LENGTH < 20) {
      current_pas_maillage <- pas_maillage_court
      current_pas_temps <- pas_temps_court
      duree_palier <- duree_palier_court
      duree_montee <- duree_montee_court
      dist_amont_limite <- 20
      dist_p4_p6 <- 50 
      message("      - Buse courte (< 20m). Maillage fin et pas de temps court sélectionnés.")
    } else if (nozzle_data$LENGTH >= 20 && nozzle_data$LENGTH <= 250) {
      current_pas_maillage <- pas_maillage_moyen
      current_pas_temps <- pas_temps_moyen
      duree_palier <- duree_palier_moyen
      duree_montee <- duree_montee_moyen
      dist_amont_limite <- 50
      dist_p4_p6 <- 400
      message("      - Buse moyenne (20-100m). Maillage et pas de temps moyens sélectionnés.")
    } else {
      current_pas_maillage <- pas_maillage_long
      current_pas_temps <- pas_temps_long
      duree_palier <- duree_palier_long
      duree_montee <- duree_montee_long
      dist_amont_limite <- 500
      dist_p4_p6 <- 2000 
      message("      - Buse longue (> 100m). Maillage large et pas de temps long sélectionnés.")
    }
    
    I <- (nozzle_data$Z1 - nozzle_data$Z2) / nozzle_data$LENGTH
    if (CIRC == 1) {
      D <- nozzle_data$HAUT1; R <- D / 2; S <- pi * R^2; P <- 2 * pi * R; Rh <- R/2
      Q <- strickler_buse_value * S * (Rh^(2/3)) * sqrt(abs(I))
    } else {
      S <- nozzle_data$LARG * nozzle_data$HAUT1; P <- nozzle_data$LARG + (2 * nozzle_data$HAUT1); Rh <- S / P
      Q <- strickler_buse_value * S * (Rh^(2/3)) * sqrt(abs(I))
    }
    Q <- round(Q)
    
    if (Q < 15) {
      message(paste("      - Le débit max estimé (", Q, "m³/s) est inférieur à 15 m³/s. Il est forcé à 15 m³/s pour la simulation."))
      Q <- 15
    }
    
    Q_sim_max <- Q * 1.5
    if (Q_sim_max > 180) {
      message(paste("      - Le débit max de simulation (", round(Q_sim_max), "m³/s) dépasse 180 m³/s. Il est plafonné à 180 m³/s."))
      Q_sim_max <- 180
    }
    
    debits_q_list <- list()
    current_max_q <- 0
    
    end_q_p1 <- min(Q_sim_max, 1.0)
    if (0.2 <= end_q_p1) {
      debits_q_list[[length(debits_q_list) + 1]] <- seq(0.2, end_q_p1, by = 0.1)
      current_max_q <- end_q_p1
    }
    
    start_val_p2 <- current_max_q + 0.2
    end_q_p2 <- min(Q_sim_max, 10.0)
    if (start_val_p2 <= end_q_p2) {
      debits_q_list[[length(debits_q_list) + 1]] <- seq(start_val_p2, end_q_p2, by = 0.2)
      current_max_q <- end_q_p2
    }
    
    start_val_p3 <- current_max_q + 1
    end_q_p3 <- min(Q_sim_max, 20.0)
    if (start_val_p3 <= end_q_p3) {
      debits_q_list[[length(debits_q_list) + 1]] <- seq(start_val_p3, end_q_p3, by = 1)
      current_max_q <- end_q_p3
    }
    
    start_val_p4 <- current_max_q + 2.0
    end_q_p4 <- min(Q_sim_max, 40.0)
    if (start_val_p4 <= end_q_p4) {
      debits_q_list[[length(debits_q_list) + 1]] <- seq(start_val_p4, end_q_p4, by = 2.0)
      current_max_q <- end_q_p4
    }
    
    start_val_p5 <- current_max_q + 5.0
    end_q_p5 <- Q_sim_max
    if (start_val_p5 <= end_q_p5) {
      debits_q_list[[length(debits_q_list) + 1]] <- seq(start_val_p5, end_q_p5, by = 5.0)
    }
    
    debits_q <- unique(sort(c(unlist(debits_q_list))))
    debits_q <- round(debits_q[debits_q > 0], 2)
    
    if (CIRC == 1) {
      dimension_str <- sprintf("Diamètre: %.2f m", nozzle_data$HAUT1)
    } else {
      dimension_str <- sprintf("Largeur: %.2f m, Hauteur: %.2f m", nozzle_data$LARG, nozzle_data$HAUT1)
    }
    pente_pourcent <- I * 100
    title_details_str <- paste(
      dimension_str, 
      sprintf("Longueur: %.1f m", nozzle_data$LENGTH), 
      sprintf("Pente: %.2f %%", pente_pourcent),
      sprintf("Qmax simulé: %.1f m³/s", Q_sim_max), 
      sep=" | "
    )
    
    #########################################################################
    # PARTIE 1: GÉNÉRATION DU FICHIER DE GÉOMÉTRIE (.geo)
    #########################################################################
    message("--- PARTIE 1 : GÉOMÉTRIE (.geo) ---")
    
    create_rectangular_profile <- function(nozzle_width, nozzle_height, nozzle_bottom_z, target_center_y, preissmann_slot_width = 0.01, slot_height = 50, bank_y_offset = 10) {
      y_center <- target_center_y
      z_bottom <- nozzle_bottom_z
      z_top <- z_bottom + nozzle_height
      z_slot_top <- z_top + slot_height
      
      y_bottom_left  <- y_center - nozzle_width/2
      y_bottom_right <- y_center + nozzle_width/2
      y_top_left     <- y_bottom_left - 0.01
      y_top_right    <- y_bottom_right + 0.01
      
      y_slot_left    <- y_center - preissmann_slot_width/2
      y_slot_right   <- y_center + preissmann_slot_width/2
      
      y_bank_left    <- y_top_left - bank_y_offset
      y_bank_right   <- y_top_right + bank_y_offset
      
      full_profile_df <- rbind(
        data.frame(Y = y_bank_left, Z = z_slot_top), 
        data.frame(Y = y_slot_left, Z = z_slot_top), 
        data.frame(Y = y_slot_left, Z = z_top), 
        data.frame(Y = y_top_left, Z = z_top), 
        data.frame(Y = y_bottom_left, Z = z_bottom), 
        data.frame(Y = y_bottom_right, Z = z_bottom), 
        data.frame(Y = y_top_right, Z = z_top), 
        data.frame(Y = y_slot_right, Z = z_top), 
        data.frame(Y = y_slot_right, Z = z_slot_top), 
        data.frame(Y = y_bank_right, Z = z_slot_top)
      )
      full_profile_df$Marker <- "B"
      return(full_profile_df[!duplicated(round(full_profile_df[, c("Y", "Z")], 5)), ])
    }
    create_circular_profile <- function(diameter, nozzle_bottom_z, target_center_y, slot_width = 0.01, slot_height = 50, bank_y_offset = 10, n_points_arch = 41) {
      radius <- diameter/2; y_center <- target_center_y; z_center <- nozzle_bottom_z + radius; z_connection <- z_center + sqrt(max(0, radius^2 - (slot_width/2)^2)); z_slot_top <- z_connection + slot_height; y_slot_left <- y_center - slot_width/2; y_slot_right <- y_center + slot_width/2; y_structure_left <- y_center - radius; y_structure_right <- y_center + radius; p_berge_gauche_loin <- data.frame(Y = y_structure_left - bank_y_offset, Z = z_slot_top); p_fente_bas_gauche  <- data.frame(Y = y_slot_left, Z = z_connection); theta_start <- atan2(z_connection - z_center, y_slot_left - y_center); theta_end     <- atan2(z_connection - z_center, y_slot_right - y_center); theta_sequence <- seq(from = theta_start, to = theta_end + 2*pi, length.out = n_points_arch); arch_df <- data.frame(Y = y_center + radius * cos(theta_sequence), Z = z_center + radius * sin(theta_sequence)); p_fente_bas_droite  <- data.frame(Y = y_slot_right, Z = z_connection); p_berge_droite_loin <- data.frame(Y = y_structure_right + bank_y_offset, Z = z_slot_top)
      full_profile_df <- rbind(p_berge_gauche_loin, data.frame(Y = y_slot_left, Z = z_slot_top), p_fente_bas_gauche, arch_df, p_fente_bas_droite, data.frame(Y = y_slot_right, Z = z_slot_top), p_berge_droite_loin)
      full_profile_df$Marker <- "B"
      return(full_profile_df[!duplicated(round(full_profile_df[, c("Y", "Z")], 5)), ])
    }
    
    generate_points_for_trapezoidal <- function(bottom_width, left_slope, right_slope, bottom_elevation, bank_height_above_bottom, target_center_y, bank_y_offset = 10) {
      half_bottom_width <- bottom_width / 2
      top_z <- bottom_elevation + bank_height_above_bottom
      
      # Les points sont d'abord calculés de manière centrée autour de Y=0
      y_top_left_rel <- -half_bottom_width - (left_slope * bank_height_above_bottom)
      y_top_right_rel <- half_bottom_width + (right_slope * bank_height_above_bottom)
      y_rel <- c(y_top_left_rel - bank_y_offset, y_top_left_rel, -half_bottom_width, half_bottom_width, y_top_right_rel, y_top_right_rel + bank_y_offset)
      
      # Le thalweg de ce profil relatif est à Y=0. On décale tous les points pour que le thalweg soit à target_center_y.
      y_abs <- y_rel + target_center_y
      
      z_abs <- c(top_z, top_z, bottom_elevation, bottom_elevation, top_z, top_z)
      points_df <- data.frame(Y = y_abs, Z = z_abs, Marker = "B")
      return(points_df[order(points_df$Y), ])
    }
    
    extract_profile_from_mnt <- function(mnt, center_point_sf, direction_vector, profile_width, num_points = 101) {
      perp_vector <- c(-direction_vector[2], direction_vector[1])
      perp_vector_norm <- perp_vector / sqrt(sum(perp_vector^2))
      start_point <- st_coordinates(center_point_sf) - (profile_width / 2) * perp_vector_norm
      end_point <- st_coordinates(center_point_sf) + (profile_width / 2) * perp_vector_norm
      transect_line <- st_sfc(st_linestring(rbind(start_point, end_point)), crs = st_crs(mnt))
      sampled_points_sf <- st_line_sample(transect_line, n = num_points, type = "regular")
      if (st_is_empty(sampled_points_sf)) return(NULL)
      sampled_points_sf <- st_cast(sampled_points_sf, "POINT")
      sampled_points_vect <- vect(sampled_points_sf)
      extracted_data <- terra::extract(mnt, sampled_points_vect, xy = TRUE, ID = FALSE)
      if (nrow(extracted_data) == 0 || all(is.na(extracted_data[, 1]))) {
        warning(paste0("L'extraction MNT a échoué (autour de X=", round(st_coordinates(center_point_sf)[1]),", Y=", round(st_coordinates(center_point_sf)[2]), "). Profil hors emprise?"))
        return(NULL)
      }
      colnames(extracted_data)[1] <- "Z"
      coords <- extracted_data[, c("x", "y")]
      y_relative <- sqrt((coords$x - coords$x[1])^2 + (coords$y - coords$y[1])^2)
      profile_df <- data.frame(Y = y_relative, Z = extracted_data$Z, Marker = "B")
      profile_df <- na.omit(profile_df)
      if (nrow(profile_df) < 2) return(NULL)
      return(profile_df)
    }
    
    shift_profile_z <- function(profile_df, target_min_z) {
      if(is.null(profile_df) || nrow(profile_df) == 0) return(NULL)
      current_min_z <- min(profile_df$Z, na.rm = TRUE)
      delta_z <- target_min_z - current_min_z
      profile_df$Z <- profile_df$Z + delta_z
      return(profile_df)
    }
    
    center_profile_on_thalweg <- function(profile_df, target_y_center) {
      if(is.null(profile_df) || nrow(profile_df) == 0) return(NULL)
      min_z_index <- which.min(profile_df$Z)
      y_of_min_z <- mean(profile_df$Y[min_z_index]) 
      shift_y <- target_y_center - y_of_min_z
      profile_df$Y <- profile_df$Y + shift_y
      return(profile_df)
    }
    
    add_vertical_walls <- function(profile_df) {
      if(is.null(profile_df) || nrow(profile_df) < 2) return(profile_df)
      
      first_point <- profile_df[1, ]
      wall_start_point <- data.frame(Y = first_point$Y, Z = first_point$Z + 20, Marker = "B")
      
      last_point <- profile_df[nrow(profile_df), ]
      wall_end_point <- data.frame(Y = last_point$Y, Z = last_point$Z + 20, Marker = "B")
      
      new_profile <- rbind(wall_start_point, profile_df, wall_end_point)
      return(new_profile)
    }
    
    write_geo_profile <- function(con, bief_name, prof_name, prof_abs, points_df) {
      writeLines(paste0("PROFIL ", bief_name, " ", prof_name, " ", sprintf("%.3f", prof_abs)), con)
      for (j in 1:nrow(points_df)) {
        writeLines(paste(sprintf("%.3f", points_df$Y[j]), sprintf("%.5f", points_df$Z[j]), points_df$Marker[j]), con)
      }
    }
    
    model_total_length <- dist_amont_limite + nozzle_data$LENGTH + dist_p4_p6
    x_nozzle_start <- dist_amont_limite
    x_nozzle_end <- x_nozzle_start + nozzle_data$LENGTH
    x_coords <- c(0, x_nozzle_start - 2.0, x_nozzle_start, x_nozzle_end, x_nozzle_end + 2.0, model_total_length)
    num_profiles_total <- length(x_coords)
    
    buse_geom <- st_geometry(buse_row)
    
    geo_base_filename <- paste0("geom_buse", numero_buse, ifelse(use_mnt_for_profiles, "_MNT", "_Trapeze"))
    geo_filename_with_ext <- paste0(geo_base_filename, ".geo")
    geo_filepath <- file.path(current_buse_output_dir, geo_filename_with_ext)
    message(paste("Génération du fichier de géométrie :", geo_filename_with_ext))
    
    bief_name <- "Bief_1"
    indices <- list(p1 = 1, p2 = 2, p3 = 3, p4 = 4, p5 = 5, p6 = 6)
    
    all_profiles <- vector("list", num_profiles_total)
    
    # Déterminer une abscisse Y cible unique pour le centrage de tous les profils.
    half_width_calc <- first_profile_shape$bottom_width / 2 + 
      max(first_profile_shape$left_slope, first_profile_shape$right_slope) * first_profile_shape$bank_height_above_bottom + 
      10.0 # bank_y_offset de la fonction generate_points_for_trapezoidal
    target_center_y <- ceiling(half_width_calc) + 5.0 # Ajout d'une marge de sécurité
    message(paste("      - Abscisse Y cible pour le centrage de tous les profils:", sprintf("%.3f", target_center_y)))
    
    if(use_mnt_for_profiles) {
      message("      - Mode MNT: Extraction des profils depuis le MNT...")
      
      buse_coords <- st_coordinates(buse_geom)
      if(nrow(buse_coords) < 2) stop("Géométrie de buse invalide (moins de 2 points) pour la buse ", numero_buse)
      
      start_node <- buse_coords[1, 1:2]
      end_node <- buse_coords[nrow(buse_coords), 1:2]
      direction_vector <- end_node - start_node
      direction_vector_norm <- direction_vector / sqrt(sum(direction_vector^2))
      profile_width_mnt <- 10 * ifelse(CIRC == 1, nozzle_data$HAUT1, nozzle_data$LARG)
      
      pt_extraction_amont <- st_sfc(st_point(start_node - dist_extraction_profil * direction_vector_norm), crs = st_crs(mnt_raster))
      profil_amont_base <- extract_profile_from_mnt(mnt_raster, pt_extraction_amont, direction_vector_norm, profile_width_mnt, num_points_per_profile)
      if(is.null(profil_amont_base)) stop("Échec de l'extraction du profil de base amont. Arrêt.")
      
      pt_extraction_aval <- st_sfc(st_point(end_node + dist_extraction_profil * direction_vector_norm), crs = st_crs(mnt_raster))
      profil_aval_base <- extract_profile_from_mnt(mnt_raster, pt_extraction_aval, direction_vector_norm, profile_width_mnt, num_points_per_profile)
      if(is.null(profil_aval_base)) stop("Échec de l'extraction du profil de base aval. Arrêt.")
      
      # Altitude des profils en extrapolant la pente de la buse (I)
      
      # Profil 1 (limite amont)
      z_profil_1 <- nozzle_data$Z1 + abs(I) * (x_coords[indices$p3] - x_coords[indices$p1])
      profil_1_vshifted <- shift_profile_z(profil_amont_base, z_profil_1)
      profil_1_centered <- center_profile_on_thalweg(profil_1_vshifted, target_center_y)
      profil_1 <- add_vertical_walls(profil_1_centered)
      all_profiles[[indices$p1]] <- list(name = "1", abscissa = x_coords[indices$p1], data = profil_1)
      
      # Profil 2 (juste avant la buse)
      z_profil_2 <- nozzle_data$Z1 + abs(I) * (x_coords[indices$p3] - x_coords[indices$p2])
      profil_2_vshifted <- shift_profile_z(profil_amont_base, z_profil_2)
      profil_2_centered <- center_profile_on_thalweg(profil_2_vshifted, target_center_y)
      profil_2 <- add_vertical_walls(profil_2_centered)
      all_profiles[[indices$p2]] <- list(name = "2", abscissa = x_coords[indices$p2], data = profil_2)
      
      # Profil 5 (juste après la buse)
      z_profil_5 <- nozzle_data$Z2 - abs(I) * (x_coords[indices$p5] - x_coords[indices$p4])
      profil_5_vshifted <- shift_profile_z(profil_aval_base, z_profil_5)
      profil_5_centered <- center_profile_on_thalweg(profil_5_vshifted, target_center_y)
      profil_5 <- add_vertical_walls(profil_5_centered)
      all_profiles[[indices$p5]] <- list(name = "5", abscissa = x_coords[indices$p5], data = profil_5)
      
      # Profil 6 (limite aval)
      z_profil_6 <- nozzle_data$Z2 - abs(I) * (x_coords[indices$p6] - x_coords[indices$p4])
      profil_6_vshifted <- shift_profile_z(profil_aval_base, z_profil_6)
      profil_6_centered <- center_profile_on_thalweg(profil_6_vshifted, target_center_y)
      profil_6 <- add_vertical_walls(profil_6_centered)
      all_profiles[[indices$p6]] <- list(name = "6", abscissa = x_coords[indices$p6], data = profil_6)
      
    } else {
      message("      - Mode Trapézoïdal: Génération des profils génériques...")
      
      calculated_bottom_z <- sapply(x_coords, function(x) {
        if (x <= x_nozzle_start) return(nozzle_data$Z1 + abs(I) * (x_nozzle_start - x))
        if (x > x_nozzle_start && x < x_nozzle_end) return(nozzle_data$Z1 - I * (x - x_nozzle_start))
        return(nozzle_data$Z2 - abs(I) * (x - x_nozzle_end))
      })
      
      for (k in c(indices$p1, indices$p2, indices$p5, indices$p6)) {
        points_raw <- generate_points_for_trapezoidal(
          bottom_width = first_profile_shape$bottom_width, 
          left_slope = first_profile_shape$left_slope, 
          right_slope = first_profile_shape$right_slope, 
          bottom_elevation = calculated_bottom_z[k], 
          bank_height_above_bottom = first_profile_shape$bank_height_above_bottom,
          target_center_y = target_center_y,
          bank_y_offset = 10.0
        )
        points <- add_vertical_walls(points_raw)
        all_profiles[[k]] <- list(name = as.character(k), abscissa = x_coords[k], data = points)
      }
    }
    
    points_p3 <- if(CIRC == 1) create_circular_profile(nozzle_data$HAUT1, nozzle_data$Z1, target_center_y) else create_rectangular_profile(nozzle_data$LARG, nozzle_data$HAUT1, nozzle_data$Z1, target_center_y, preissmann_slot_width)
    all_profiles[[indices$p3]] <- list(name = "3amont", abscissa = x_coords[indices$p3], data = points_p3)
    
    points_p4 <- if(CIRC == 1) create_circular_profile(nozzle_data$HAUT1, nozzle_data$Z2, target_center_y) else create_rectangular_profile(nozzle_data$LARG, nozzle_data$HAUT1, nozzle_data$Z2, target_center_y, preissmann_slot_width)
    all_profiles[[indices$p4]] <- list(name = "3aval", abscissa = x_coords[indices$p4], data = points_p4)
    
    con <- file(geo_filepath, "wt")
    message("      - Écriture ordonnée des profils dans le fichier .geo...")
    for (prof in all_profiles) {
      if (!is.null(prof) && !is.null(prof$data)) {
        write_geo_profile(con, bief_name, prof$name, prof$abscissa, prof$data)
      }
    }
    close(con)
    message("--- Fichier .geo généré avec succès. ---")
    
    #########################################################################
    # PARTIE 2: GÉNÉRATION DES FICHIERS DE LOIS (.loi)
    #########################################################################
    message("\n--- PARTIE 2 : LOIS HYDRAULIQUES (.loi) ---")
    
    duree_totale_sim <- length(debits_q) * (duree_montee + duree_palier)
    hydro_file <- file.path(current_buse_output_dir, "hydrogramme_complet.loi")
    
    con_q <- NULL
    tryCatch({
      con_q <- file(hydro_file, open = "wt")
      writeLines(c("# Hydrogramme...", "# T(s) Q(m3/s)", "S"), con_q)
      current_time <- 0.0
      writeLines(paste(sprintf("%.1f", current_time), "0.20000"), con_q)
      for (q in debits_q) { 
        current_time <- current_time + duree_montee
        writeLines(paste(sprintf("%.1f", current_time), sprintf("%.5f", q)), con_q)
        current_time <- current_time + duree_palier
        writeLines(paste(sprintf("%.1f", current_time), sprintf("%.5f", q)), con_q) 
      }
    }, finally = {
      if(!is.null(con_q)) close(con_q)
    })
    
    message("      - Génération des cotes Z aval selon la logique personnalisée...")
    cotes_z_aval <- c()
    slope=((nozzle_data$Z1-nozzle_data$Z2)/nozzle_data$LENGTH)
    
    z_start=nozzle_data$Z2-(slope*dist_p4_p6)/2
    z_phase1_end = nozzle_data$Z2
    cotes_z_aval1=seq(from = z_start, to = z_phase1_end, by = 0.5)
    z_phase2_end  <- z_phase1_end + nozzle_data$HAUT1+0.2
    cotes_z_aval2=seq(from = z_phase1_end, to = z_phase2_end, by = 0.1)
    z_phase3_end  <- z_phase1_end + nozzle_data$HAUT1 + 2
    cotes_z_aval3 <- seq(from = z_phase2_end, to = z_phase3_end, by = 0.5)
    cotes_z_aval <- unique(sort(c(cotes_z_aval1,cotes_z_aval2,cotes_z_aval3)))
    #à enlever
    z_start=nozzle_data$Z2-(slope*dist_p4_p6)
    cotes_z_aval<- seq(from = z_start, to = z_phase1_end + nozzle_data$HAUT1+0.5, by = 0.5)
    ##
    message(paste("La cote Z aval minimale sera:", sprintf("%.3f", z_start)))
    message(paste("Nombre de conditions aval générées:", length(cotes_z_aval)))
    
    for (z in cotes_z_aval) { 
      filename_z <- file.path(current_buse_output_dir, paste0("limni_Z", sprintf("%.3f", z), ".loi"))
      con_z <- NULL
      tryCatch({
        con_z <- file(filename_z, open = "wt")
        writeLines(c(paste("# Limnigramme... Z=", sprintf("%.3f", z)), "# T(s) Z(m)", "S"), con_z)
        writeLines(paste(sprintf("%.1f", 0.0), sprintf("%.5f", z)), con_z)
        writeLines(paste(sprintf("%.1f", duree_totale_sim), sprintf("%.5f", z)), con_z)
      }, finally = {
        if(!is.null(con_z)) close(con_z)
      })
    }
    message(paste(length(cotes_z_aval), "fichiers .loi générés avec succès."))
    
    #########################################################################
    # PARTIE 3: GÉNÉRATION DES FICHIERS DE PARAMÈTRES (.xcas)
    #########################################################################
    message("\n--- PARTIE 3 : PARAMÈTRES (.xcas) ---")
    limni_files <- list.files(current_buse_output_dir, pattern = "^limni_Z.*\\.loi$", full.names = TRUE)
    
    for (limni_file in limni_files) {
      current_xcas_lines <- readLines(xcas_template_filename, warn = FALSE)
      limni_id <- sub("^limni_(Z.*)\\.loi$", "\\1", basename(limni_file))
      scenario_base_name <- paste0("buse", numero_buse, "_scenario_", limni_id)
      xcas_output_filename <- file.path(current_buse_output_dir, paste0(scenario_base_name, ".xcas"))
      
      pas_stockage_value <- ceiling((duree_montee + duree_palier) / current_pas_temps)
      critere_arret_value <- 1
      
      current_xcas_lines <- gsub("<fichMotsCles>.*</fichMotsCles>", paste0("<fichMotsCles>", scenario_base_name, ".xcas</fichMotsCles>"), current_xcas_lines)
      current_xcas_lines <- gsub("<fichier>.*</fichier>", paste0("<fichier>", geo_filename_with_ext, "</fichier>"), current_xcas_lines)
      current_xcas_lines <- gsub("<abscFin>.*</abscFin>", paste0("<abscFin>", sprintf("%.3f", model_total_length), "</abscFin>"), current_xcas_lines)
      current_xcas_lines <- gsub("<absFinZone>.*</absFinZone>", paste0("<absFinZone>", sprintf("%.3f", model_total_length), "</absFinZone>"), current_xcas_lines)
      current_xcas_lines <- gsub("<pasTemps>.*</pasTemps>", paste0("<pasTemps>", sprintf("%.1f", current_pas_temps), "</pasTemps>"), current_xcas_lines)
      current_xcas_lines <- gsub("<tempsMax>.*</tempsMax>", paste0("<tempsMax>", sprintf("%.1f", duree_totale_sim), "</tempsMax>"), current_xcas_lines)
      current_xcas_lines <- gsub("<critereArret>.*</critereArret>", paste0("<critereArret>", critere_arret_value, "</critereArret>"), current_xcas_lines)
      current_xcas_lines <- gsub("<pasStock>.*</pasStock>", paste0("<pasStock>", sprintf("%.0f", pas_stockage_value), "</pasStock>"), current_xcas_lines)
      current_xcas_lines <- gsub("<fichResultat>.*</fichResultat>", paste0("<fichResultat>", scenario_base_name, "_ecr.opt</fichResultat>"), current_xcas_lines)
      current_xcas_lines <- gsub("<fichListing>.*</fichListing>", paste0("<fichListing>", scenario_base_name, ".lis</fichListing>"), current_xcas_lines)
      current_xcas_lines <- gsub("<valeursPas>.*</valeursPas>",paste0("<valeursPas>",sprintf("%.2f", val_pas_planimetrage),"</valeursPas>"), current_xcas_lines)
      current_xcas_lines <- gsub("<numDerProf>.*</numDerProf>", paste0("<numDerProf>", num_profiles_total, "</numDerProf>"), current_xcas_lines)
      
      pas_maillage_str <- paste(current_pas_maillage, collapse = " ")
      current_xcas_lines <- gsub("<num1erProfPlage>.*</num1erProfPlage>", "<num1erProfPlage>1 3 5</num1erProfPlage>", current_xcas_lines)
      current_xcas_lines <- gsub("<numDerProfPlage>.*</numDerProfPlage>", paste0("<numDerProfPlage>2 4 ", num_profiles_total, "</numDerProfPlage>"), current_xcas_lines)
      current_xcas_lines <- gsub("<pasEspacePlage>.*</pasEspacePlage>", paste0("<pasEspacePlage>", pas_maillage_str, "</pasEspacePlage>"), current_xcas_lines)
      
      start_frottement_idx <- grep("<frottement>", current_xcas_lines)[1]
      end_frottement_idx <- grep("</frottement>", current_xcas_lines)[1]
      if (!is.na(start_frottement_idx) && !is.na(end_frottement_idx)) {
        abs_deb_zone_str <- paste(sprintf("%.3f", x_coords[c(1, 3, 5)]), collapse = " ")
        abs_fin_zone_str <- paste(sprintf("%.3f", x_coords[c(2, 4, 6)]), collapse = " ")
        strickler_zones_str <- paste(sprintf("%.1f", strickler_zones), collapse = " ")
        frottement_block <- c("      <frottement>", "        <loi>1</loi>", "        <nbZone>3</nbZone>", "        <numBranche>1 1 1</numBranche>", paste0("        <absDebZone>", abs_deb_zone_str, "</absDebZone>"), paste0("        <absFinZone>", abs_fin_zone_str, "</absFinZone>"), paste0("        <coefLitMin>", strickler_zones_str, "</coefLitMin>"), paste0("        <coefLitMaj>", strickler_zones_str, "</coefLitMaj>"), "      </frottement>")
        current_xcas_lines <- c(current_xcas_lines[1:(start_frottement_idx - 1)], frottement_block, current_xcas_lines[(end_frottement_idx + 1):length(current_xcas_lines)])
      }
      
      start_index <- grep("<parametresLoisHydrauliques>", current_xcas_lines)[1]
      end_index <- grep("</parametresLoisHydrauliques>", current_xcas_lines)[1]
      if (!is.na(start_index) && !is.na(end_index)) {
        lois_lines <- c("  <parametresLoisHydrauliques>","    <nb>2</nb>","    <lois>","      <structureParametresLoi>","        <nom>hydro_amont</nom>","        <type>1</type>","        <donnees>","          <modeEntree>1</modeEntree>",paste0("          <fichier>", basename(hydro_file), "</fichier>"),"        </donnees>","      </structureParametresLoi>","      <structureParametresLoi>",paste0("        <nom>limni_", limni_id, "</nom>"),"        <type>2</type>","        <donnees>","          <modeEntree>1</modeEntree>",paste0("          <fichier>", basename(limni_file), "</fichier>"),"        </donnees>","      </structureParametresLoi>","    </lois>","  </parametresLoisHydrauliques>")
        current_xcas_lines <- c(current_xcas_lines[1:(start_index-1)], lois_lines, current_xcas_lines[(end_index+1):length(current_xcas_lines)])
      }
      
      con <- NULL
      tryCatch({
        con <- file(xcas_output_filename, "w")
        writeLines(current_xcas_lines, con)
      }, finally = {
        if(!is.null(con)) close(con)
      })
    }
    message("--- Fichiers .xcas générés avec succès. ---")
    
    # PARTIE 4
    message("\n--- PARTIE 4 : LANCEMENT DES SIMULATIONS ---")
    xcas_files <- dir_ls(current_buse_output_dir, regexp = "\\.xcas$", recurse = FALSE)
    registerDoParallel(cores = num_cores_sim)
    foreach( xcas_file_path = xcas_files, .combine = 'c', .errorhandling = 'pass') %dopar% {
      library(fs)
      xcas_filename <- path_file(xcas_file_path)
      xcas_basename <- path_ext_remove(xcas_filename)
      temp_sim_dir <- path(current_buse_output_dir, paste0("temp_", xcas_basename))
      results_dir <- path(current_buse_output_dir, "results_mascaret")
      
      final_opt_path <- path(results_dir, paste0(xcas_basename, "_ecr.opt"))
      
      if(!file_exists(final_opt_path) || file_info(final_opt_path)$size == 0) {
        dir_create(temp_sim_dir)
        file_copy(xcas_file_path, path(temp_sim_dir, xcas_filename), overwrite = TRUE)
        file_copy(path(current_buse_output_dir, geo_filename_with_ext), temp_sim_dir, overwrite = TRUE)
        file_copy(path(current_buse_output_dir, "hydrogramme_complet.loi"), temp_sim_dir, overwrite = TRUE)
        limni_id_for_copy <- sub(paste0("buse", numero_buse, "_scenario_"), "", xcas_basename)
        limni_to_copy <- path(current_buse_output_dir, paste0("limni_", limni_id_for_copy, ".loi"))
        if(file_exists(limni_to_copy)) file_copy(limni_to_copy, temp_sim_dir, overwrite = TRUE)
        
        batch_content <- sprintf('@echo off\nCALL "%s"\ncd /D "%%~dp0"\npython "%%HOMETEL%%\\scripts\\python3\\mascaret.py" "%s"\nexit /b %%ERRORLEVEL%%', telemac_env_script, xcas_filename)
        batch_file_path <- path(temp_sim_dir, "run_simulation.bat")
        writeLines(batch_content, batch_file_path)
        
        system2("cmd", args = c("/c", path_abs(batch_file_path)), stdout = TRUE, stderr = TRUE)
      }
    }
    stopImplicitCluster()
    message("--- Lancement des simulations terminé. ---")
    
    # NOUVELLE PARTIE 4.5: Copie des résultats après la fin de tous les calculs
    message("\n--- PARTIE 4.5 : COLLECTE DES RÉSULTATS ---")
    results_dir <- path(current_buse_output_dir, "results_mascaret")
    dir_create(results_dir)
    temp_dirs <- dir_ls(current_buse_output_dir, regexp = "temp_buse", type = "directory")
    
    for(temp_dir in temp_dirs) {
      opt_files_in_temp <- dir_ls(temp_dir, regexp = "\\.(opt|lis)$")
      if(length(opt_files_in_temp) > 0) {
        file_copy(opt_files_in_temp, results_dir, overwrite = TRUE)
      }
    }
    message("--- Collecte des fichiers .opt terminée. ---")
    
    
    # PARTIE 5
    message("\n--- PARTIE 5 : LECTURE DES RÉSULTATS ET CRÉATION DE L'ABAQUE ---")
    opt_dir <- file.path(current_buse_output_dir, "results_mascaret")
    
    ABSCISSA_BUSE_ENTREE_TARGET <- x_coords[3]
    ABSCISSA_BUSE_SORTIE_TARGET <- x_coords[4]
    
    opt_files <- list.files(opt_dir, pattern = "\\.opt$", full.names = TRUE)
    results_list <- list()
    
    for (opt_file in opt_files) {
      opt_content_lines <- readLines(opt_file, warn = FALSE)
      resultats_start_line_index <- grep("[resultats]", opt_content_lines, fixed = TRUE)
      if (length(resultats_start_line_index) == 0) next
      opt_data_text <- opt_content_lines[(resultats_start_line_index[1] + 1):length(opt_content_lines)]
      if(length(opt_data_text) == 0 || all(nchar(trimws(opt_data_text)) == 0)) next
      
      opt_data <- tryCatch({ fread(text = opt_data_text, sep = ";", strip.white=TRUE, header = FALSE, colClasses = "numeric", fill = TRUE, select = c(1, 4, 6, 11, 12, 14), col.names = c("Time", "Abscissa", "S_value", "Froude", "Velocity", "DebitTotal")) }, error = function(e) { NULL })
      if(is.null(opt_data) || nrow(opt_data) == 0) next
      
      plateau_end_times <- (1:length(debits_q)) * (duree_montee + duree_palier)
      
      for(time_step in plateau_end_times){
        time_to_extract <- time_step - current_pas_temps
        if(time_to_extract < 0 || time_to_extract > max(opt_data$Time)) next
        
        steady_state_data <- opt_data[abs(opt_data$Time - time_to_extract) < 1e-3, ]
        if(nrow(steady_state_data) == 0) next
        
        entree_buse_row <- steady_state_data[which.min(abs(steady_state_data$Abscissa - ABSCISSA_BUSE_ENTREE_TARGET)), ]
        sortie_buse_row <- steady_state_data[which.min(abs(steady_state_data$Abscissa - ABSCISSA_BUSE_SORTIE_TARGET)), ]
        
        if(nrow(entree_buse_row) == 1 && nrow(sortie_buse_row) == 1){
          avg_velocity <- (entree_buse_row$Velocity + sortie_buse_row$Velocity) / 2
          
          results_list[[length(results_list) + 1]] <- data.frame(
            Debit = entree_buse_row$DebitTotal, 
            S1 = entree_buse_row$S_value, 
            S2 = sortie_buse_row$S_value,
            Froude_amont = entree_buse_row$Froude,
            Avg_Velocity = avg_velocity
          )
        }
      }
    }
    
    results_for_file <- data.frame()
    all_results_df <- data.frame()
    results_df_sorted <- data.frame()
    
    if (length(results_list) > 0) {
      all_results_df <- do.call(rbind, results_list)
      s1_max_threshold <- nozzle_data$Z1 + nozzle_data$HAUT1 + 10
      
      all_results_filtered <- all_results_df[
        all_results_df$S1 <= s1_max_threshold & 
          all_results_df$Froude_amont <= 5 &
          # all_results_df$Avg_Velocity < 7.0 &
          (all_results_df$S1 - all_results_df$S2) >= 0.05,
      ]
      if(nrow(all_results_filtered) > 0) {
        results_df_unique <- unique(all_results_filtered)
        results_df_sorted <- results_df_unique[order(results_df_unique$Debit, results_df_unique$S1), ]
        
        final_points_list <- lapply(split(results_df_sorted, results_df_sorted$Debit), function(df_debit) {
          torrential_points <- df_debit %>% filter(Froude_amont >= 1)
          if (nrow(torrential_points) > 0) return(torrential_points)
          good_fluvial_points <- df_debit %>% filter(Froude_amont < 1 & S1 < (nozzle_data$Z1 + nozzle_data$HAUT1) & S2 < (nozzle_data$Z2 + nozzle_data$HAUT1))
          if (nrow(good_fluvial_points) > 0) return(good_fluvial_points)
          return(df_debit %>% filter(Froude_amont < 1))
        })
        
        results_for_file <- bind_rows(final_points_list) %>% unique() %>% arrange(Debit, S1)
        if(length(which(results_for_file$Froude_amont>=1))>0){
          max_debit_torrential=max(results_for_file$Debit[which(results_for_file$Froude_amont>=1)])
          max_S1_torrentiel=max(results_for_file$S1[which(results_for_file$Debit==max_debit_torrential)])
          max_S2_torrentiel=max(results_for_file$S2[which(results_for_file$Debit==max_debit_torrential)])
          
          points_fluviaux1=which(results_for_file$Debit<max_debit_torrential & (results_for_file$S1 >max_S1_torrentiel | results_for_file$S2 >max_S2_torrentiel ) )
          points_fluviaux2=which(results_for_file$Debit>max_debit_torrential & results_for_file$S2 >nozzle_data$Z2 + nozzle_data$HAUT1 + 0.05  )
          if(length(points_fluviaux1)>0 | length(points_fluviaux2)>0){
            results_for_file=results_for_file[-c(points_fluviaux1,points_fluviaux2),]
            results_for_file <- results_for_file %>% unique() %>% arrange(Debit, S1)
          }
        }
        
        # --- NETTOYAGE ET STRICTE MONOTONIE HYDRAULIQUE (Q et S1) ---
        if (nrow(results_for_file) > 0) {
          # 1. Règle anti-doublons de débit : un seul point par débit (cote max en régime établi)
          results_for_file <- results_for_file %>% 
            group_by(Debit) %>% 
            slice_max(S1, n = 1, with_ties = FALSE) %>% 
            ungroup() %>% 
            arrange(Debit)
          
          # 2. Règle du palier horizontal : ne garder que la 1ère apparition de chaque cote S1
          results_for_file <- results_for_file %>% 
            distinct(S1, .keep_all = TRUE)
          
          # 3. Filtrage de stricte croissance : S1[i] doit être strictement supérieur à S1[i-1]
          if (nrow(results_for_file) > 1) {
            indices_valides <- c(1)
            derniere_cote <- results_for_file$S1[1]
            for (idx_pt in 2:nrow(results_for_file)) {
              if (results_for_file$S1[idx_pt] > derniere_cote) {
                indices_valides <- c(indices_valides, idx_pt)
                derniere_cote <- results_for_file$S1[idx_pt]
              }
            }
            results_for_file <- results_for_file[indices_valides, ]
          }
        }
        
        output_txt_file_S_partiel <- file.path(current_buse_output_dir, "abaque_partiel.txt")
        if (nrow(results_for_file) > 0) {
          file_conn_partiel <- file(output_txt_file_S_partiel, "w")
          writeLines(c(as.character(numero_buse), as.character(nrow(results_for_file))), file_conn_partiel)
          cat(paste(sprintf("%.2f", results_for_file$Debit), collapse = ","), "\n", file = file_conn_partiel)
          cat(paste(sprintf("%.2f", results_for_file$S1), collapse = ","), "\n", file = file_conn_partiel)
          cat(paste(sprintf("%.2f", results_for_file$S2), collapse = ","), "\n", file = file_conn_partiel)
          close(file_conn_partiel)
        }
        
        output_txt_file_QS1_partiel <- file.path(current_buse_output_dir, "abaque_partiel_QS1.txt")
        if (nrow(results_for_file) > 0) {
          results_QS1_unique <- results_for_file %>% dplyr::select(Debit, S1) %>% distinct() %>% arrange(Debit, S1)
          file_conn_QS1 <- file(output_txt_file_QS1_partiel, "w")
          writeLines(c(as.character(numero_buse), as.character(nrow(results_QS1_unique))), file_conn_QS1)
          cat(paste(sprintf("%.2f", results_QS1_unique$Debit), collapse = ","), "\n", file = file_conn_QS1)
          cat(paste(sprintf("%.2f", results_QS1_unique$S1), collapse = ","), "\n", file = file_conn_QS1)
          close(file_conn_QS1)
        }
      }
    }
    
    if(generate_long_profile_plots) {
      message("\n--- GÉNÉRATION DES PROFILS EN LONG ---")
      for (opt_file in opt_files) {
        scenario_name <- sub("_ecr\\.opt$", "", basename(opt_file)); opt_content_lines <- readLines(opt_file, warn = FALSE); resultats_start_line_index <- grep("[resultats]", opt_content_lines, fixed = TRUE); if (length(resultats_start_line_index) == 0) next
        opt_data_full <- tryCatch({ fread(text = opt_content_lines[(resultats_start_line_index[1] + 1):length(opt_content_lines)], sep = ";", strip.white=TRUE, header = FALSE, colClasses = "numeric", fill = TRUE, select = c(1, 4, 5, 6, 14), col.names = c("Time", "Abscissa", "Z_fond", "Z_eau", "DebitTotal")) }, error = function(e) { NULL })
        if(is.null(opt_data_full) || nrow(opt_data_full) == 0) next
        fig_pl <- plot_ly(); profil_fond <- opt_data_full[abs(Time - min(Time)) < 1e-3, ]; fig_pl <- add_trace(fig_pl, data = profil_fond, x = ~Abscissa, y = ~Z_fond, type = 'scatter', mode = 'lines', line = list(color = 'black', width = 3), name = 'Fond')
        for (time_p in ((1:length(debits_q)) * (duree_montee + duree_palier) - current_pas_temps)) { if (time_p > 0 && time_p <= max(opt_data_full$Time)) { steady_data <- opt_data_full[abs(opt_data_full$Time - time_p) < 1e-3, ]; if (nrow(steady_data) > 0) fig_pl <- add_trace(fig_pl, data = steady_data, x = ~Abscissa, y = ~Z_eau, type = 'scatter', mode = 'lines', name = paste('Q =', round(steady_data$DebitTotal[1], 2), 'm³/s')) } }
        y_range <- range(c(profil_fond$Z_fond, opt_data_full$Z_eau), na.rm = TRUE); fig_pl <- fig_pl %>% layout(title = scenario_name, xaxis = list(title = "Abscisse"), yaxis = list(title = "Altitude", range = y_range), shapes = list(list(type = 'line', x0 = x_coords[indices$p3], x1 = x_coords[indices$p3], y0 = y_range[1], y1 = y_range[2], line = list(color = 'grey', dash = 'dash')), list(type = 'line', x0 = x_coords[indices$p4], x1 = x_coords[indices$p4], y0 = y_range[1], y1 = y_range[2], line = list(color = 'grey', dash = 'dash')), list(type = 'line', x0 = x_coords[indices$p3], x1 = x_coords[indices$p4], y0 = nozzle_data$Z1 + nozzle_data$HAUT1, y1 = nozzle_data$Z2 + nozzle_data$HAUT1, line = list(color = 'red', dash = 'longdash'))))
        saveWidget(fig_pl, file = file.path(opt_dir, paste0("profil_en_long_", scenario_name, ".html")), selfcontained = TRUE)
      }
    }
    
    
    if(generate_abaque_plots) {
      message("\n--- PARTIE 6 : GÉNÉRATION DES GRAPHIQUES D'ABAQUE ---")
      
      # Utiliser `all_results_df` pour définir les points à estomper
      points_to_keep <- results_for_file
      points_to_fade <- dplyr::anti_join(all_results_df, points_to_keep, by = c("Debit", "S1", "S2", "Froude_amont", "Avg_Velocity"))
      
      if (nrow(points_to_keep) > 0 || nrow(points_to_fade) > 0) {
        
        # Préparation des données pour les points principaux
        results_df_graph <- points_to_keep
        if(nrow(results_df_graph) > 0) {
          colnames(results_df_graph) <- c("Debit", "S1_Charge_Amont", "S2_Charge_Aval", "Froude_amont", "Avg_Velocity")
          results_df_graph$Froude_Condition <- cut(results_df_graph$Froude_amont, 
                                                   breaks = c(-Inf, 0.99, 1.01, Inf), 
                                                   labels = c("Fluvial (Fr < 1)", "Critique (Fr = 1)", "Torrentiel (Fr > 1)"))
        }
        
        # --- Graphique 3D ---
        title_3d <- paste("Relation Débit-Charges S1/S2 - Buse", numero_buse, "<br>", title_details_str)
        fig_3d_S <- plot_ly()
        
        if(nrow(points_to_fade) > 0) {
          fig_3d_S <- fig_3d_S %>% add_trace(
            data = points_to_fade, x=~Debit, y=~S2, z=~S1,
            type='scatter3d', mode='markers',
            marker=list(size=3, color='rgba(211, 211, 211, 0.5)'), # Gris clair, semi-transparent
            name = "Points exclus",
            hoverinfo = "text",
            text = ~paste('Exclu<br>Débit:', sprintf("%.2f", Debit), '<br>S1:', sprintf("%.2f", S1), '<br>S2:', sprintf("%.2f", S2), '<br>Froude:', sprintf("%.2f", Froude_amont))
          )
        }
        
        if(nrow(results_df_graph) > 0) {
          fig_3d_S <- fig_3d_S %>% add_trace(
            data = results_df_graph, x=~Debit, y=~S2_Charge_Aval, z=~S1_Charge_Amont,
            type='scatter3d', mode='markers',
            color=~S1_Charge_Amont, colorscale='Viridis',
            symbol=~Froude_Condition, symbols=c('circle','square','triangle-up'),
            marker=list(size=4),
            customdata=~Froude_amont,
            name = "Points de l'abaque",
            hovertemplate=paste('<b>Débit:</b> %{x:.2f} m³/s<br>',
                                '<b>S2 (Aval):</b> %{y:.2f} m<br>',
                                '<b>S1 (Amont):</b> %{z:.2f} m<br>',
                                '<b>Froude (Amont):</b> %{customdata:.2f}<br>',
                                '<extra></extra>')
          )
        }
        
        fig_3d_S <- fig_3d_S %>% layout(title=title_3d,
                                        legend=list(title=list(text='<b>Régime Froude</b>')),
                                        scene=list(xaxis=list(title="Débit (m³/s)"),
                                                   yaxis=list(title="S2 Charge Aval (m)"),
                                                   zaxis=list(title="S1 Charge Amont (m)")))
        
        # --- Graphique 2D ---
        title_2d <- paste("Relation S1-S2 - Buse", numero_buse, "<br>", title_details_str)
        fig_2d_S <- plot_ly()
        
        if(nrow(points_to_fade) > 0) {
          fig_2d_S <- fig_2d_S %>% add_trace(
            data = points_to_fade, x=~S1, y=~S2,
            type='scatter', mode='markers',
            marker=list(size=5, color='rgba(211, 211, 211, 0.6)'), # Gris clair, semi-transparent
            name = "Points exclus",
            hoverinfo = "text",
            text = ~paste('Exclu<br>Débit:', sprintf("%.2f", Debit), '<br>S1:', sprintf("%.2f", S1), '<br>S2:', sprintf("%.2f", S2), '<br>Froude:', sprintf("%.2f", Froude_amont))
          )
        }
        
        if(nrow(results_df_graph) > 0) {
          fig_2d_S <- fig_2d_S %>% add_trace(
            data = results_df_graph, x=~S1_Charge_Amont, y=~S2_Charge_Aval,
            type='scatter', mode='markers',
            color=~Debit, colorscale='Plasma',
            symbol=~Froude_Condition, symbols=c('circle','square','triangle-up'),
            marker=list(size=6),
            customdata=~Froude_amont,
            name = "Points de l'abaque",
            hovertemplate=paste('<b>S1 (Amont):</b> %{x:.2f} m<br>',
                                '<b>S2 (Aval):</b> %{y:.2f} m<br>',
                                '<b>Débit:</b> %{marker.color:.2f} m³/s<br>',
                                '<b>Froude:</b> %{customdata:.2f}<br>',
                                '<extra></extra>')
          )
          
          shapes_list <- list(
            list(type = 'line', x0 = nozzle_data$Z1, x1 = nozzle_data$Z1, y0 = min(results_df_graph$S2_Charge_Aval), y1 = max(results_df_graph$S2_Charge_Aval), line = list(color = 'blue', dash = 'longdash', width = 2)),
            list(type = 'line', x0 = nozzle_data$Z1 + nozzle_data$HAUT1, x1 = nozzle_data$Z1 + nozzle_data$HAUT1, y0 = min(results_df_graph$S2_Charge_Aval), y1 = max(results_df_graph$S2_Charge_Aval), line = list(color = 'red', dash = 'longdash', width = 2)),
            list(type = 'line', y0 = nozzle_data$Z2, y1 = nozzle_data$Z2, x0 = min(results_df_graph$S1_Charge_Amont), x1 = max(results_df_graph$S1_Charge_Amont), line = list(color = 'blue', dash = 'longdash', width = 2)),
            list(type = 'line', y0 = nozzle_data$Z2 + nozzle_data$HAUT1, y1 = nozzle_data$Z2 + nozzle_data$HAUT1, x0 = min(results_df_graph$S1_Charge_Amont), x1 = max(results_df_graph$S1_Charge_Amont), line = list(color = 'red', dash = 'longdash', width = 2))
          )
          fig_2d_S <- fig_2d_S %>% layout(shapes = shapes_list)
        }
        
        fig_2d_S <- fig_2d_S %>% layout(
          title=title_2d,
          xaxis=list(title="S1 Charge Amont (m)"),
          yaxis=list(title="S2 Charge Aval (m)"),
          legend=list(title=list(text='<b>Régime Froude</b>'))
        )
        
        htmlwidgets::saveWidget(fig_3d_S, file.path(current_buse_output_dir, "graph_abaque_S_3d.html"), selfcontained = TRUE)
        htmlwidgets::saveWidget(fig_2d_S, file.path(current_buse_output_dir, "graph_abaque_S_2d.html"), selfcontained = TRUE)
        message("Graphiques d'abaque 2D et 3D (incluant les points exclus) exportés.")
        
      } else {
        message("Aucune donnée à visualiser, les graphiques d'abaque n'ont pas été générés pour cette buse.")
      }
    }
    
    # --- BLOC DE NETTOYAGE SÉLECTIF (AJOUTÉ v123) ---
    if (cleanup_calc_files) {
      message(paste("      - Nettoyage sélectif pour la buse", numero_buse))
      
      # 1. Identifier tous les dossiers temporaires temp_...
      tdirs <- fs::dir_ls(current_buse_output_dir, regexp = "temp_", type = "directory")
      if (length(tdirs) > 1) {
        # Garder uniquement le premier dossier pour vérification, supprimer les autres
        fs::dir_delete(tdirs[-1]) 
      }
      
      # 2. Supprimer le dossier global results_mascaret (doublon inutile une fois les données extraites)
      res_fold <- file.path(current_buse_output_dir, "results_mascaret")
      if(fs::dir_exists(res_fold)) fs::dir_delete(res_fold)
      
      # 3. Supprimer les fichiers d'entrée à la racine (déjà sauvegardés dans le dossier temp conservé)
      root_files <- fs::dir_ls(current_buse_output_dir, regexp = "\\.(xcas|geo|loi|lis)$")
      if(length(root_files) > 0) fs::file_delete(root_files)
    }
  } 
  
  #########################################################################
  # DÉMARRAGE DE LA BOUCLE PRINCIPALE
  #########################################################################
  if (ncore_buse <= 1) {
    message("--- DÉMARRAGE DU TRAITEMENT SÉQUENTIEL ---")
    for (i in 1:nrow(busel_data)) {
      abaques_check_file <- file.path(dsnlayerC, "abaques2.txt")
      if (file.exists(abaques_check_file) && file.info(abaques_check_file)$size > 0) {
        message(paste(">>> BUSE", abaques_check_file, ": DÉJÀ TRAITÉE (Fichier abaque trouvé). PASSAGE À LA SUIVANTE. <<<"))
        next
      }
      process_buse(i, busel_data, main_output_dir, telemac_env_script, xcas_template_filename, use_mnt_for_profiles, mnt_path, dist_amont_limite, dist_p4_p6, num_points_per_profile, dist_extraction_profil, first_profile_shape, duree_palier, duree_montee, num_cores_sim, strickler_zones_base, val_pas_planimetrage, g, pas_maillage_court, pas_temps_court, pas_maillage_moyen, pas_temps_moyen, pas_maillage_long, pas_temps_long, preissmann_slot_width, generate_long_profile_plots, generate_abaque_plots, cleanup_calc_files)
    }
  } else {
    message(paste("--- DÉMARRAGE DU TRAITEMENT PARALLÈLE SUR", ncore_buse, "CŒURS ---"))
    registerDoParallel(cores = ncore_buse)
    foreach(i = 1:nrow(busel_data)) %dopar% { 
      process_buse(i, busel_data, main_output_dir, telemac_env_script, xcas_template_filename, use_mnt_for_profiles, mnt_path, dist_amont_limite, dist_p4_p6, num_points_per_profile, dist_extraction_profil, first_profile_shape, duree_palier, duree_montee, num_cores_sim, strickler_zones_base, val_pas_planimetrage, g, pas_maillage_court, pas_temps_court, pas_maillage_moyen, pas_temps_moyen, pas_maillage_long, pas_temps_long, preissmann_slot_width, generate_long_profile_plots, generate_abaque_plots, cleanup_calc_files) 
    }
    stopImplicitCluster()
  }
  
  #########################################################################
  # PARTIE 7: ASSEMBLAGE FINAL DE L'ABAQUE
  #########################################################################
  message("\n--- PARTIE 7 : ASSEMBLAGE DU FICHIER D'ABAQUE FINAL ---")
  output_txt_file_S_global <- file.path(main_output_dir, "resultats_buse_format_abaqueSQ.txt")
  file_conn_main <- file(output_txt_file_S_global, "w")
  writeLines("#abaqueSQ", file_conn_main)
  output_txt_file_QS1_global <- file.path(main_output_dir, "abaques.txt")
  file_conn_qs1 <- file(output_txt_file_QS1_global, "w")
  writeLines("#abaqueQS1", file_conn_qs1)
  
  for (i in 1:nrow(busel_data)) {
    numero_buse <- busel_data$id[i]
    current_buse_output_dir <- file.path(main_output_dir, paste0("buse_", numero_buse))
    
    p1 <- file.path(current_buse_output_dir, "abaque_partiel.txt")
    if (file.exists(p1)) writeLines(readLines(p1), file_conn_main)
    
    p2 <- file.path(current_buse_output_dir, "abaque_partiel_QS1.txt")
    if (file.exists(p2)) writeLines(readLines(p2), file_conn_qs1)
  }
  
  close(file_conn_main); close(file_conn_qs1)
  message("\n--- WORKFLOW COMPLET TERMINÉ ---")
  file.copy(from = file.path(main_output_dir, "abaques.txt"),to = dsnlayerC,overwrite = TRUE,recursive = TRUE)
  # 1. Lire toutes les lignes du fichier d'origine
  lignes <- readLines(file.path(dsnlayerC, "abaques.txt"))
  
  # 2. Nettoyer les éventuelles lignes vides à la fin du fichier
  lignes <- lignes[lignes != ""]
  
  # 3. Calculer le nombre de buses
  # On soustrait 1 (pour la ligne de commentaire) avant de diviser par 4
  n_buses <- floor((length(lignes) - 1) / 4)
  
  # 4. Générer les indices des IDs
  # On commence à la ligne 2, et on fait des sauts de 4
  indices_id <- seq(2, by = 4, length.out = n_buses)
  
  # 5. Remplacer les lignes d'identifiants par la nouvelle numérotation
  lignes[indices_id] <- 1:n_buses
  
  # 6. Sauvegarder le résultat en écrasant le fichier
  writeLines(as.character(lignes), file.path(dsnlayerC, "abaques.txt"))
  # Script R pour lisser les abaques Telemac-2D
  # Objectif : Garantir une croissance stricte des hauteurs d'eau (S1) 
  # pour éviter les divisions par zéro dans DISCHARGE_INTERPOL.
  
  # Noms des fichiers (à adapter si besoin)
  file_in <- file.path(dsnlayerC, "abaques.txt")
  file_out <- file.path(dsnlayerC, "abaques2.txt")
  
  # Lecture du fichier
  lines <- readLines(file_in, warn = FALSE)
  # Nettoyage des lignes vides éventuelles
  lines <- lines[trimws(lines) != ""]
  
  # On conserve l'entête (ex: #abaqueQS1)
  output_lines <- c(lines[1])
  
  idx <- 2
  while (idx <= length(lines)) {
    # Extraction du bloc de 4 lignes
    abaque_id <- trimws(lines[idx])
    # Conversion des lignes Q et S1 en vecteurs numériques
    q_vals  <- as.numeric(unlist(strsplit(q_str, ",")))
    s1_vals <- as.numeric(unlist(strsplit(s1_str, ",")))
    
    # Construction du dataframe pour nettoyage
    df_abaque <- data.frame(Debit = q_vals, S1 = s1_vals)
    
    # 1. Anti-doublons de débit (garder cote max en régime établi)
    df_abaque <- df_abaque %>%
      group_by(Debit) %>%
      slice_max(S1, n = 1, with_ties = FALSE) %>%
      ungroup() %>%
      arrange(Debit)
    
    # 2. Règle du palier horizontal : ne garder que la 1ère apparition de chaque cote S1
    df_abaque <- df_abaque %>%
      distinct(S1, .keep_all = TRUE)
    
    # 3. Filtrage de stricte croissance : S1[i] > S1[i-1]
    if (nrow(df_abaque) > 1) {
      indices_valides <- c(1)
      derniere_cote <- df_abaque$S1[1]
      for (idx_pt in 2:nrow(df_abaque)) {
        if (df_abaque$S1[idx_pt] > derniere_cote) {
          indices_valides <- c(indices_valides, idx_pt)
          derniere_cote <- df_abaque$S1[idx_pt]
        }
      }
      df_abaque <- df_abaque[indices_valides, ]
    }
    
    n_pts  <- nrow(df_abaque)
    q_out  <- paste(sprintf("%.2f", df_abaque$Debit), collapse = ",")
    s1_out <- paste(sprintf("%.2f", df_abaque$S1), collapse = ",")
    
    # Stockage des 4 lignes du bloc corrigé
    output_lines <- c(output_lines, abaque_id, as.character(n_pts), q_out, s1_out)
    
    # On passe à l'abaque suivant
    idx <- idx + 4
  }
  
  # Écriture du nouveau fichier
  writeLines(output_lines, file_out)
  cat("Traitement terminé avec succès !\nLe fichier corrigé est :", file_out, "\n")
}
abacus=function(){
  library(sf)
  library(terra)
  library(dplyr)
  library(fs)
  library(stringr)
  library(foreach)
  library(doParallel)
  library(data.table)
  library(plotly)
  library(htmlwidgets)
  
  #########################################################################
  # PARTIE 0: PARAMÈTRES UTILISATEUR
  # Modifiez les valeurs dans cette section pour configurer votre simulation.
  #########################################################################
  # --- 0.1. Configuration des chemins et fichiers ---
  # chem_routine <- "C:/Users/CartoNationale/Desktop/Nabil"
  
  main_output_dir <- file.path(dsnlayerC, "abaques") 
  telemac_env_script <- "C:/TELEMAC/V8P4/configs/pysource.bat"
  xcas_template_filename <- file.path(chem_routine,"C2D",Doss_Base, "masc.xcas.txt")
  
  # --- 0.2. Paramètres de parallélisation ---
  ncore_buse <- ncore_buse_abaques # Nombre de cœurs pour la boucle sur les buses. 1 = séquentiel, >=2 = parallèle
  num_cores_sim <- num_cores_sim_abaques
  # Nombre de cœurs pour les simulations d'une MÊME buse
  
  # --- 0.3. Contrôle de la géométrie --- 
  use_mnt_for_profiles <- FALSE # Mettre à TRUE pour extraire du MNT, FALSE pour utiliser des profils trapézoïdaux
  
  # --- Chemins pour le mode MNT ---
  shapefile_path <- file.path(dsnlayerC, "OuvHydrau_Select.shp") 
  mnt_path <- file.path(dsnlayerC, "MNTFinal.gpkg") # <--- CHEMIN VERS VOTRE MNT
  
  # --- 0.4. Paramètres de la géométrie ---
  dist_amont_limite <- 1000.0
  dist_p4_p6 <- 2000 # Distance entre la sortie de la buse (corrigée à p5) et la limite aval (p6)
  num_points_per_profile <- 101
  dist_extraction_profil <- 3.0 
  preissmann_slot_width <- 0.01 # Largeur de la fente de Preissmann en mètres
  
  first_profile_shape <- list(
    type = "trapezoidal", bottom_width = 3, left_slope = 1/1.5,
    right_slope = 1/1.5, bank_height_above_bottom = 40
  )
  
  # --- 0.5. Paramètres temporels et hydrauliques ---
  duree_palier <- 400
  duree_montee <- 50
  strickler_zones_base <- c(20.0, 20.0, 20.0) 
  val_pas_planimetrage <- 0.02
  g <- 9.81
  
  # --- 0.6. Paramètres de maillage et de temps de simulation adaptatifs ---
  # Pour buses COURTES (< 20m)
  pas_maillage_court <- c(1, 0.5, 1)
  pas_temps_court <- 0.5
  duree_palier_court <- 20
  duree_montee_court <- 10
  
  # Pour buses MOYENNES (20m à 100m)
  pas_maillage_moyen <- c(1, 1, 1)
  pas_temps_moyen <- 1
  duree_palier_moyen <- 100
  duree_montee_moyen <- 20
  # Pour buses LONGUES (> 100m)
  pas_maillage_long <- c(2, 2, 2)
  pas_temps_long <- 2
  duree_palier_long <- 400
  duree_montee_long <- 50
  # --- 0.7. Paramètres de sortie (NOUVEAU) ---
  generate_long_profile_plots <- TRUE # Mettre à TRUE pour générer les graphiques de profil en long
  generate_abaque_plots <- TRUE       # Mettre à TRUE pour générer les graphiques d'abaque 2D/3D
  cleanup_calc_files <- FALSE           # <--- NOUVEAU: TRUE pour garder 1 dossier temp et supprimer le reste
  
  
  #########################################################################
  # LECTURE DES DONNÉES SPATIALES ET INITIALISATION
  #########################################################################
  message("--- LECTURE DES DONNÉES SPATIALES ---")
  if (!file.exists(shapefile_path)) stop("Le fichier shapefile n'a pas été trouvé: ", shapefile_path)
  busel_data <- st_read(shapefile_path, quiet = TRUE)
  message(paste("Lecture de", nrow(busel_data), "buse(s) depuis le shapefile."))
  
  mnt_raster <- NULL # Initialisation à NULL
  if(use_mnt_for_profiles) {
    if (!file.exists(mnt_path)) stop("Le fichier MNT n'a pas été trouvé: ", mnt_path)
    message("Chargement du MNT pour vérification CRS...")
    temp_mnt <- rast(mnt_path)
    message("MNT chargé.")
    if (st_crs(busel_data) != st_crs(temp_mnt)) {
      message("ATTENTION: CRS différents. Reprojection du shapefile vers le CRS du MNT.")
      busel_data <- st_transform(busel_data, st_crs(temp_mnt))
    }
    rm(temp_mnt) # On supprime l'objet, il sera rechargé dans chaque worker
    gc() # Force le nettoyage de la mémoire
  }
  
  dir.create(main_output_dir, showWarnings = FALSE, recursive = TRUE)
  
  #########################################################################
  # DÉFINITION DE LA FONCTION DE TRAITEMENT D'UNE BUSE
  #########################################################################
  process_buse <- function(i, busel_data, main_output_dir, telemac_env_script, xcas_template_filename, use_mnt_for_profiles, mnt_path, dist_amont_limite, dist_p4_p6, num_points_per_profile, dist_extraction_profil, first_profile_shape, duree_palier, duree_montee, num_cores_sim, strickler_zones_base, val_pas_planimetrage, g, pas_maillage_court, pas_temps_court, pas_maillage_moyen, pas_temps_moyen, pas_maillage_long, pas_temps_long, preissmann_slot_width, generate_long_profile_plots, generate_abaque_plots, cleanup_calc_files) {
    
    # Charger les librairies nécessaires dans l'environnement parallèle
    library(sf)
    library(terra)
    library(dplyr)
    library(fs)
    library(stringr)
    library(foreach)
    library(doParallel)
    library(data.table)
    library(plotly)
    library(htmlwidgets)
    
    # Si on utilise le MNT, chaque worker charge sa propre copie.
    if (use_mnt_for_profiles) {
      mnt_raster <- rast(mnt_path)
    }
    
    buse_row <- busel_data[i, ]
    numero_buse <- buse_row$id
    
    # Création du dossier de sortie
    current_buse_output_dir <- file.path(main_output_dir, paste0("buse_", numero_buse))
    dir.create(current_buse_output_dir, showWarnings = FALSE, recursive = TRUE)
    
    # -----------------------------------------------------------------------
    # CHECK DE DÉJÀ FAIT (NOUVELLE FONCTIONNALITÉ v124)
    # -----------------------------------------------------------------------
    abaque_check_file <- file.path(current_buse_output_dir, "abaque_partiel.txt")
    if (file.exists(abaque_check_file) && file.info(abaque_check_file)$size > 0) {
      message(paste(">>> BUSE", numero_buse, ": DÉJÀ TRAITÉE (Fichier abaque trouvé). PASSAGE À LA SUIVANTE. <<<"))
      return(NULL)
    }
    # -----------------------------------------------------------------------
    
    CIRC <- buse_row$CIRC
    if(is.na(buse_row$HAUT1) | buse_row$HAUT1==0) buse_row$HAUT1=buse_row$HAUT2
    nozzle_data <- list(LARG = buse_row$LRG, HAUT1 = buse_row$HAUT1, Z1 = buse_row$Z1, Z2 = buse_row$Z2, LENGTH = buse_row$LENGTH)
    first_profile_shape$bottom_width=nozzle_data$LARG*2
    if ("FRIC" %in% names(buse_row) && !is.na(buse_row$FRIC) && buse_row$FRIC > 0) {
      strickler_buse_value <- 1 / buse_row$FRIC
      if (strickler_buse_value < 1) {
        warning(paste("Buse", numero_buse, ": La valeur FRIC=", buse_row$FRIC, "donne un Strickler de", round(strickler_buse_value, 2), ". Utilisation de la valeur par défaut de 50.0."))
        strickler_buse_value <- 60.0
      }
    } else {
      strickler_buse_value <- 60.0
      warning(paste("Buse", numero_buse, ": Colonne 'FRIC' non valide ou absente. Utilisation de la valeur par défaut de Strickler:", strickler_buse_value))
    }
    strickler_zones <- c(strickler_zones_base[1], strickler_buse_value, strickler_zones_base[3])
    
    
    message(paste("\n\n############################################################"))
    message(paste("--- DÉMARRAGE DU TRAITEMENT POUR LA BUSE :", numero_buse, "---"))
    message(paste("--- Fichiers de sortie dans :", current_buse_output_dir, "---"))
    message(paste("############################################################\n"))
    
    if (nozzle_data$LENGTH < 20) {
      current_pas_maillage <- pas_maillage_court
      current_pas_temps <- pas_temps_court
      duree_palier <- duree_palier_court
      duree_montee <- duree_montee_court
      dist_amont_limite <- 20
      dist_p4_p6 <- 50 
      message("      - Buse courte (< 20m). Maillage fin et pas de temps court sélectionnés.")
    } else if (nozzle_data$LENGTH >= 20 && nozzle_data$LENGTH <= 250) {
      current_pas_maillage <- pas_maillage_moyen
      current_pas_temps <- pas_temps_moyen
      duree_palier <- duree_palier_moyen
      duree_montee <- duree_montee_moyen
      dist_amont_limite <- 50
      dist_p4_p6 <- 400
      message("      - Buse moyenne (20-100m). Maillage et pas de temps moyens sélectionnés.")
    } else {
      current_pas_maillage <- pas_maillage_long
      current_pas_temps <- pas_temps_long
      duree_palier <- duree_palier_long
      duree_montee <- duree_montee_long
      dist_amont_limite <- 500
      dist_p4_p6 <- 2000 
      message("      - Buse longue (> 100m). Maillage large et pas de temps long sélectionnés.")
    }
    
    I <- (nozzle_data$Z1 - nozzle_data$Z2) / nozzle_data$LENGTH
    if (CIRC == 1) {
      D <- nozzle_data$HAUT1; R <- D / 2; S <- pi * R^2; P <- 2 * pi * R; Rh <- R/2
      Q <- strickler_buse_value * S * (Rh^(2/3)) * sqrt(abs(I))
    } else {
      S <- nozzle_data$LARG * nozzle_data$HAUT1; P <- nozzle_data$LARG + (2 * nozzle_data$HAUT1); Rh <- S / P
      Q <- strickler_buse_value * S * (Rh^(2/3)) * sqrt(abs(I))
    }
    Q <- round(Q)
    
    if (Q < 15) {
      message(paste("      - Le débit max estimé (", Q, "m³/s) est inférieur à 15 m³/s. Il est forcé à 15 m³/s pour la simulation."))
      Q <- 15
    }
    
    Q_sim_max <- Q * 1.5
    if (Q_sim_max > 180) {
      message(paste("      - Le débit max de simulation (", round(Q_sim_max), "m³/s) dépasse 180 m³/s. Il est plafonné à 180 m³/s."))
      Q_sim_max <- 180
    }
    
    debits_q_list <- list()
    current_max_q <- 0
    
    end_q_p1 <- min(Q_sim_max, 1.0)
    if (0.2 <= end_q_p1) {
      debits_q_list[[length(debits_q_list) + 1]] <- seq(0.2, end_q_p1, by = 0.1)
      current_max_q <- end_q_p1
    }
    
    start_val_p2 <- current_max_q + 0.2
    end_q_p2 <- min(Q_sim_max, 10.0)
    if (start_val_p2 <= end_q_p2) {
      debits_q_list[[length(debits_q_list) + 1]] <- seq(start_val_p2, end_q_p2, by = 0.2)
      current_max_q <- end_q_p2
    }
    
    start_val_p3 <- current_max_q + 1
    end_q_p3 <- min(Q_sim_max, 20.0)
    if (start_val_p3 <= end_q_p3) {
      debits_q_list[[length(debits_q_list) + 1]] <- seq(start_val_p3, end_q_p3, by = 1)
      current_max_q <- end_q_p3
    }
    
    start_val_p4 <- current_max_q + 2.0
    end_q_p4 <- min(Q_sim_max, 40.0)
    if (start_val_p4 <= end_q_p4) {
      debits_q_list[[length(debits_q_list) + 1]] <- seq(start_val_p4, end_q_p4, by = 2.0)
      current_max_q <- end_q_p4
    }
    
    start_val_p5 <- current_max_q + 5.0
    end_q_p5 <- Q_sim_max
    if (start_val_p5 <= end_q_p5) {
      debits_q_list[[length(debits_q_list) + 1]] <- seq(start_val_p5, end_q_p5, by = 5.0)
    }
    
    debits_q <- unique(sort(c(unlist(debits_q_list))))
    debits_q <- round(debits_q[debits_q > 0], 2)
    
    if (CIRC == 1) {
      dimension_str <- sprintf("Diamètre: %.2f m", nozzle_data$HAUT1)
    } else {
      dimension_str <- sprintf("Largeur: %.2f m, Hauteur: %.2f m", nozzle_data$LARG, nozzle_data$HAUT1)
    }
    pente_pourcent <- I * 100
    title_details_str <- paste(
      dimension_str, 
      sprintf("Longueur: %.1f m", nozzle_data$LENGTH), 
      sprintf("Pente: %.2f %%", pente_pourcent),
      sprintf("Qmax simulé: %.1f m³/s", Q_sim_max), 
      sep=" | "
    )
    
    #########################################################################
    # PARTIE 1: GÉNÉRATION DU FICHIER DE GÉOMÉTRIE (.geo)
    #########################################################################
    message("--- PARTIE 1 : GÉOMÉTRIE (.geo) ---")
    
    create_rectangular_profile <- function(nozzle_width, nozzle_height, nozzle_bottom_z, target_center_y, preissmann_slot_width = 0.01, slot_height = 50, bank_y_offset = 10) {
      y_center <- target_center_y
      z_bottom <- nozzle_bottom_z
      z_top <- z_bottom + nozzle_height
      z_slot_top <- z_top + slot_height
      
      y_bottom_left  <- y_center - nozzle_width/2
      y_bottom_right <- y_center + nozzle_width/2
      y_top_left     <- y_bottom_left - 0.01
      y_top_right    <- y_bottom_right + 0.01
      
      y_slot_left    <- y_center - preissmann_slot_width/2
      y_slot_right   <- y_center + preissmann_slot_width/2
      
      y_bank_left    <- y_top_left - bank_y_offset
      y_bank_right   <- y_top_right + bank_y_offset
      
      full_profile_df <- rbind(
        data.frame(Y = y_bank_left, Z = z_slot_top), 
        data.frame(Y = y_slot_left, Z = z_slot_top), 
        data.frame(Y = y_slot_left, Z = z_top), 
        data.frame(Y = y_top_left, Z = z_top), 
        data.frame(Y = y_bottom_left, Z = z_bottom), 
        data.frame(Y = y_bottom_right, Z = z_bottom), 
        data.frame(Y = y_top_right, Z = z_top), 
        data.frame(Y = y_slot_right, Z = z_top), 
        data.frame(Y = y_slot_right, Z = z_slot_top), 
        data.frame(Y = y_bank_right, Z = z_slot_top)
      )
      full_profile_df$Marker <- "B"
      return(full_profile_df[!duplicated(round(full_profile_df[, c("Y", "Z")], 5)), ])
    }
    create_circular_profile <- function(diameter, nozzle_bottom_z, target_center_y, slot_width = 0.01, slot_height = 50, bank_y_offset = 10, n_points_arch = 41) {
      radius <- diameter/2; y_center <- target_center_y; z_center <- nozzle_bottom_z + radius; z_connection <- z_center + sqrt(max(0, radius^2 - (slot_width/2)^2)); z_slot_top <- z_connection + slot_height; y_slot_left <- y_center - slot_width/2; y_slot_right <- y_center + slot_width/2; y_structure_left <- y_center - radius; y_structure_right <- y_center + radius; p_berge_gauche_loin <- data.frame(Y = y_structure_left - bank_y_offset, Z = z_slot_top); p_fente_bas_gauche  <- data.frame(Y = y_slot_left, Z = z_connection); theta_start <- atan2(z_connection - z_center, y_slot_left - y_center); theta_end     <- atan2(z_connection - z_center, y_slot_right - y_center); theta_sequence <- seq(from = theta_start, to = theta_end + 2*pi, length.out = n_points_arch); arch_df <- data.frame(Y = y_center + radius * cos(theta_sequence), Z = z_center + radius * sin(theta_sequence)); p_fente_bas_droite  <- data.frame(Y = y_slot_right, Z = z_connection); p_berge_droite_loin <- data.frame(Y = y_structure_right + bank_y_offset, Z = z_slot_top)
      full_profile_df <- rbind(p_berge_gauche_loin, data.frame(Y = y_slot_left, Z = z_slot_top), p_fente_bas_gauche, arch_df, p_fente_bas_droite, data.frame(Y = y_slot_right, Z = z_slot_top), p_berge_droite_loin)
      full_profile_df$Marker <- "B"
      return(full_profile_df[!duplicated(round(full_profile_df[, c("Y", "Z")], 5)), ])
    }
    
    generate_points_for_trapezoidal <- function(bottom_width, left_slope, right_slope, bottom_elevation, bank_height_above_bottom, target_center_y, bank_y_offset = 10) {
      half_bottom_width <- bottom_width / 2
      top_z <- bottom_elevation + bank_height_above_bottom
      
      # Les points sont d'abord calculés de manière centrée autour de Y=0
      y_top_left_rel <- -half_bottom_width - (left_slope * bank_height_above_bottom)
      y_top_right_rel <- half_bottom_width + (right_slope * bank_height_above_bottom)
      y_rel <- c(y_top_left_rel - bank_y_offset, y_top_left_rel, -half_bottom_width, half_bottom_width, y_top_right_rel, y_top_right_rel + bank_y_offset)
      
      # Le thalweg de ce profil relatif est à Y=0. On décale tous les points pour que le thalweg soit à target_center_y.
      y_abs <- y_rel + target_center_y
      
      z_abs <- c(top_z, top_z, bottom_elevation, bottom_elevation, top_z, top_z)
      points_df <- data.frame(Y = y_abs, Z = z_abs, Marker = "B")
      return(points_df[order(points_df$Y), ])
    }
    
    extract_profile_from_mnt <- function(mnt, center_point_sf, direction_vector, profile_width, num_points = 101) {
      perp_vector <- c(-direction_vector[2], direction_vector[1])
      perp_vector_norm <- perp_vector / sqrt(sum(perp_vector^2))
      start_point <- st_coordinates(center_point_sf) - (profile_width / 2) * perp_vector_norm
      end_point <- st_coordinates(center_point_sf) + (profile_width / 2) * perp_vector_norm
      transect_line <- st_sfc(st_linestring(rbind(start_point, end_point)), crs = st_crs(mnt))
      sampled_points_sf <- st_line_sample(transect_line, n = num_points, type = "regular")
      if (st_is_empty(sampled_points_sf)) return(NULL)
      sampled_points_sf <- st_cast(sampled_points_sf, "POINT")
      sampled_points_vect <- vect(sampled_points_sf)
      extracted_data <- terra::extract(mnt, sampled_points_vect, xy = TRUE, ID = FALSE)
      if (nrow(extracted_data) == 0 || all(is.na(extracted_data[, 1]))) {
        warning(paste0("L'extraction MNT a échoué (autour de X=", round(st_coordinates(center_point_sf)[1]),", Y=", round(st_coordinates(center_point_sf)[2]), "). Profil hors emprise?"))
        return(NULL)
      }
      colnames(extracted_data)[1] <- "Z"
      coords <- extracted_data[, c("x", "y")]
      y_relative <- sqrt((coords$x - coords$x[1])^2 + (coords$y - coords$y[1])^2)
      profile_df <- data.frame(Y = y_relative, Z = extracted_data$Z, Marker = "B")
      profile_df <- na.omit(profile_df)
      if (nrow(profile_df) < 2) return(NULL)
      return(profile_df)
    }
    
    shift_profile_z <- function(profile_df, target_min_z) {
      if(is.null(profile_df) || nrow(profile_df) == 0) return(NULL)
      current_min_z <- min(profile_df$Z, na.rm = TRUE)
      delta_z <- target_min_z - current_min_z
      profile_df$Z <- profile_df$Z + delta_z
      return(profile_df)
    }
    
    center_profile_on_thalweg <- function(profile_df, target_y_center) {
      if(is.null(profile_df) || nrow(profile_df) == 0) return(NULL)
      min_z_index <- which.min(profile_df$Z)
      y_of_min_z <- mean(profile_df$Y[min_z_index]) 
      shift_y <- target_y_center - y_of_min_z
      profile_df$Y <- profile_df$Y + shift_y
      return(profile_df)
    }
    
    add_vertical_walls <- function(profile_df) {
      if(is.null(profile_df) || nrow(profile_df) < 2) return(profile_df)
      
      first_point <- profile_df[1, ]
      wall_start_point <- data.frame(Y = first_point$Y, Z = first_point$Z + 20, Marker = "B")
      
      last_point <- profile_df[nrow(profile_df), ]
      wall_end_point <- data.frame(Y = last_point$Y, Z = last_point$Z + 20, Marker = "B")
      
      new_profile <- rbind(wall_start_point, profile_df, wall_end_point)
      return(new_profile)
    }
    
    write_geo_profile <- function(con, bief_name, prof_name, prof_abs, points_df) {
      writeLines(paste0("PROFIL ", bief_name, " ", prof_name, " ", sprintf("%.3f", prof_abs)), con)
      for (j in 1:nrow(points_df)) {
        writeLines(paste(sprintf("%.3f", points_df$Y[j]), sprintf("%.5f", points_df$Z[j]), points_df$Marker[j]), con)
      }
    }
    
    model_total_length <- dist_amont_limite + nozzle_data$LENGTH + dist_p4_p6
    x_nozzle_start <- dist_amont_limite
    x_nozzle_end <- x_nozzle_start + nozzle_data$LENGTH
    x_coords <- c(0, x_nozzle_start - 2.0, x_nozzle_start, x_nozzle_end, x_nozzle_end + 2.0, model_total_length)
    num_profiles_total <- length(x_coords)
    
    buse_geom <- st_geometry(buse_row)
    
    geo_base_filename <- paste0("geom_buse", numero_buse, ifelse(use_mnt_for_profiles, "_MNT", "_Trapeze"))
    geo_filename_with_ext <- paste0(geo_base_filename, ".geo")
    geo_filepath <- file.path(current_buse_output_dir, geo_filename_with_ext)
    message(paste("Génération du fichier de géométrie :", geo_filename_with_ext))
    
    bief_name <- "Bief_1"
    indices <- list(p1 = 1, p2 = 2, p3 = 3, p4 = 4, p5 = 5, p6 = 6)
    
    all_profiles <- vector("list", num_profiles_total)
    
    # Déterminer une abscisse Y cible unique pour le centrage de tous les profils.
    half_width_calc <- first_profile_shape$bottom_width / 2 + 
      max(first_profile_shape$left_slope, first_profile_shape$right_slope) * first_profile_shape$bank_height_above_bottom + 
      10.0 # bank_y_offset de la fonction generate_points_for_trapezoidal
    target_center_y <- ceiling(half_width_calc) + 5.0 # Ajout d'une marge de sécurité
    message(paste("      - Abscisse Y cible pour le centrage de tous les profils:", sprintf("%.3f", target_center_y)))
    
    if(use_mnt_for_profiles) {
      message("      - Mode MNT: Extraction des profils depuis le MNT...")
      
      buse_coords <- st_coordinates(buse_geom)
      if(nrow(buse_coords) < 2) stop("Géométrie de buse invalide (moins de 2 points) pour la buse ", numero_buse)
      
      start_node <- buse_coords[1, 1:2]
      end_node <- buse_coords[nrow(buse_coords), 1:2]
      direction_vector <- end_node - start_node
      direction_vector_norm <- direction_vector / sqrt(sum(direction_vector^2))
      profile_width_mnt <- 10 * ifelse(CIRC == 1, nozzle_data$HAUT1, nozzle_data$LARG)
      
      pt_extraction_amont <- st_sfc(st_point(start_node - dist_extraction_profil * direction_vector_norm), crs = st_crs(mnt_raster))
      profil_amont_base <- extract_profile_from_mnt(mnt_raster, pt_extraction_amont, direction_vector_norm, profile_width_mnt, num_points_per_profile)
      if(is.null(profil_amont_base)) stop("Échec de l'extraction du profil de base amont. Arrêt.")
      
      pt_extraction_aval <- st_sfc(st_point(end_node + dist_extraction_profil * direction_vector_norm), crs = st_crs(mnt_raster))
      profil_aval_base <- extract_profile_from_mnt(mnt_raster, pt_extraction_aval, direction_vector_norm, profile_width_mnt, num_points_per_profile)
      if(is.null(profil_aval_base)) stop("Échec de l'extraction du profil de base aval. Arrêt.")
      
      # Altitude des profils en extrapolant la pente de la buse (I)
      
      # Profil 1 (limite amont)
      z_profil_1 <- nozzle_data$Z1 + abs(I) * (x_coords[indices$p3] - x_coords[indices$p1])
      profil_1_vshifted <- shift_profile_z(profil_amont_base, z_profil_1)
      profil_1_centered <- center_profile_on_thalweg(profil_1_vshifted, target_center_y)
      profil_1 <- add_vertical_walls(profil_1_centered)
      all_profiles[[indices$p1]] <- list(name = "1", abscissa = x_coords[indices$p1], data = profil_1)
      
      # Profil 2 (juste avant la buse)
      z_profil_2 <- nozzle_data$Z1 + abs(I) * (x_coords[indices$p3] - x_coords[indices$p2])
      profil_2_vshifted <- shift_profile_z(profil_amont_base, z_profil_2)
      profil_2_centered <- center_profile_on_thalweg(profil_2_vshifted, target_center_y)
      profil_2 <- add_vertical_walls(profil_2_centered)
      all_profiles[[indices$p2]] <- list(name = "2", abscissa = x_coords[indices$p2], data = profil_2)
      
      # Profil 5 (juste après la buse)
      z_profil_5 <- nozzle_data$Z2 - abs(I) * (x_coords[indices$p5] - x_coords[indices$p4])
      profil_5_vshifted <- shift_profile_z(profil_aval_base, z_profil_5)
      profil_5_centered <- center_profile_on_thalweg(profil_5_vshifted, target_center_y)
      profil_5 <- add_vertical_walls(profil_5_centered)
      all_profiles[[indices$p5]] <- list(name = "5", abscissa = x_coords[indices$p5], data = profil_5)
      
      # Profil 6 (limite aval)
      z_profil_6 <- nozzle_data$Z2 - abs(I) * (x_coords[indices$p6] - x_coords[indices$p4])
      profil_6_vshifted <- shift_profile_z(profil_aval_base, z_profil_6)
      profil_6_centered <- center_profile_on_thalweg(profil_6_vshifted, target_center_y)
      profil_6 <- add_vertical_walls(profil_6_centered)
      all_profiles[[indices$p6]] <- list(name = "6", abscissa = x_coords[indices$p6], data = profil_6)
      
    } else {
      message("      - Mode Trapézoïdal: Génération des profils génériques...")
      
      calculated_bottom_z <- sapply(x_coords, function(x) {
        if (x <= x_nozzle_start) return(nozzle_data$Z1 + abs(I) * (x_nozzle_start - x))
        if (x > x_nozzle_start && x < x_nozzle_end) return(nozzle_data$Z1 - I * (x - x_nozzle_start))
        return(nozzle_data$Z2 - abs(I) * (x - x_nozzle_end))
      })
      
      for (k in c(indices$p1, indices$p2, indices$p5, indices$p6)) {
        points_raw <- generate_points_for_trapezoidal(
          bottom_width = first_profile_shape$bottom_width, 
          left_slope = first_profile_shape$left_slope, 
          right_slope = first_profile_shape$right_slope, 
          bottom_elevation = calculated_bottom_z[k], 
          bank_height_above_bottom = first_profile_shape$bank_height_above_bottom,
          target_center_y = target_center_y,
          bank_y_offset = 10.0
        )
        points <- add_vertical_walls(points_raw)
        all_profiles[[k]] <- list(name = as.character(k), abscissa = x_coords[k], data = points)
      }
    }
    
    points_p3 <- if(CIRC == 1) create_circular_profile(nozzle_data$HAUT1, nozzle_data$Z1, target_center_y) else create_rectangular_profile(nozzle_data$LARG, nozzle_data$HAUT1, nozzle_data$Z1, target_center_y, preissmann_slot_width)
    all_profiles[[indices$p3]] <- list(name = "3amont", abscissa = x_coords[indices$p3], data = points_p3)
    
    points_p4 <- if(CIRC == 1) create_circular_profile(nozzle_data$HAUT1, nozzle_data$Z2, target_center_y) else create_rectangular_profile(nozzle_data$LARG, nozzle_data$HAUT1, nozzle_data$Z2, target_center_y, preissmann_slot_width)
    all_profiles[[indices$p4]] <- list(name = "3aval", abscissa = x_coords[indices$p4], data = points_p4)
    
    con <- file(geo_filepath, "wt")
    message("      - Écriture ordonnée des profils dans le fichier .geo...")
    for (prof in all_profiles) {
      if (!is.null(prof) && !is.null(prof$data)) {
        write_geo_profile(con, bief_name, prof$name, prof$abscissa, prof$data)
      }
    }
    close(con)
    message("--- Fichier .geo généré avec succès. ---")
    
    #########################################################################
    # PARTIE 2: GÉNÉRATION DES FICHIERS DE LOIS (.loi)
    #########################################################################
    message("\n--- PARTIE 2 : LOIS HYDRAULIQUES (.loi) ---")
    
    duree_totale_sim <- length(debits_q) * (duree_montee + duree_palier)
    hydro_file <- file.path(current_buse_output_dir, "hydrogramme_complet.loi")
    
    con_q <- NULL
    tryCatch({
      con_q <- file(hydro_file, open = "wt")
      writeLines(c("# Hydrogramme...", "# T(s) Q(m3/s)", "S"), con_q)
      current_time <- 0.0
      writeLines(paste(sprintf("%.1f", current_time), "0.20000"), con_q)
      for (q in debits_q) { 
        current_time <- current_time + duree_montee
        writeLines(paste(sprintf("%.1f", current_time), sprintf("%.5f", q)), con_q)
        current_time <- current_time + duree_palier
        writeLines(paste(sprintf("%.1f", current_time), sprintf("%.5f", q)), con_q) 
      }
    }, finally = {
      if(!is.null(con_q)) close(con_q)
    })
    
    message("      - Génération des cotes Z aval selon la logique personnalisée...")
    cotes_z_aval <- c()
    slope=((nozzle_data$Z1-nozzle_data$Z2)/nozzle_data$LENGTH)
    
    z_start=nozzle_data$Z2-(slope*dist_p4_p6)/2
    z_phase1_end = nozzle_data$Z2
    cotes_z_aval1=seq(from = z_start, to = z_phase1_end, by = 0.5)
    z_phase2_end  <- z_phase1_end + nozzle_data$HAUT1+0.2
    cotes_z_aval2=seq(from = z_phase1_end, to = z_phase2_end, by = 0.1)
    z_phase3_end  <- z_phase1_end + nozzle_data$HAUT1 + 2
    cotes_z_aval3 <- seq(from = z_phase2_end, to = z_phase3_end, by = 0.5)
    cotes_z_aval <- unique(sort(c(cotes_z_aval1,cotes_z_aval2,cotes_z_aval3)))
    #à enlever
    z_start=nozzle_data$Z2-(slope*dist_p4_p6)
    cotes_z_aval<- seq(from = z_start, to = z_phase1_end + nozzle_data$HAUT1+0.5, by = 0.5)
    ##
    message(paste("La cote Z aval minimale sera:", sprintf("%.3f", z_start)))
    message(paste("Nombre de conditions aval générées:", length(cotes_z_aval)))
    
    for (z in cotes_z_aval) { 
      filename_z <- file.path(current_buse_output_dir, paste0("limni_Z", sprintf("%.3f", z), ".loi"))
      con_z <- NULL
      tryCatch({
        con_z <- file(filename_z, open = "wt")
        writeLines(c(paste("# Limnigramme... Z=", sprintf("%.3f", z)), "# T(s) Z(m)", "S"), con_z)
        writeLines(paste(sprintf("%.1f", 0.0), sprintf("%.5f", z)), con_z)
        writeLines(paste(sprintf("%.1f", duree_totale_sim), sprintf("%.5f", z)), con_z)
      }, finally = {
        if(!is.null(con_z)) close(con_z)
      })
    }
    message(paste(length(cotes_z_aval), "fichiers .loi générés avec succès."))
    
    #########################################################################
    # PARTIE 3: GÉNÉRATION DES FICHIERS DE PARAMÈTRES (.xcas)
    #########################################################################
    message("\n--- PARTIE 3 : PARAMÈTRES (.xcas) ---")
    limni_files <- list.files(current_buse_output_dir, pattern = "^limni_Z.*\\.loi$", full.names = TRUE)
    
    for (limni_file in limni_files) {
      current_xcas_lines <- readLines(xcas_template_filename, warn = FALSE)
      limni_id <- sub("^limni_(Z.*)\\.loi$", "\\1", basename(limni_file))
      scenario_base_name <- paste0("buse", numero_buse, "_scenario_", limni_id)
      xcas_output_filename <- file.path(current_buse_output_dir, paste0(scenario_base_name, ".xcas"))
      
      pas_stockage_value <- ceiling((duree_montee + duree_palier) / current_pas_temps)
      critere_arret_value <- 1
      
      current_xcas_lines <- gsub("<fichMotsCles>.*</fichMotsCles>", paste0("<fichMotsCles>", scenario_base_name, ".xcas</fichMotsCles>"), current_xcas_lines)
      current_xcas_lines <- gsub("<fichier>.*</fichier>", paste0("<fichier>", geo_filename_with_ext, "</fichier>"), current_xcas_lines)
      current_xcas_lines <- gsub("<abscFin>.*</abscFin>", paste0("<abscFin>", sprintf("%.3f", model_total_length), "</abscFin>"), current_xcas_lines)
      current_xcas_lines <- gsub("<absFinZone>.*</absFinZone>", paste0("<absFinZone>", sprintf("%.3f", model_total_length), "</absFinZone>"), current_xcas_lines)
      current_xcas_lines <- gsub("<pasTemps>.*</pasTemps>", paste0("<pasTemps>", sprintf("%.1f", current_pas_temps), "</pasTemps>"), current_xcas_lines)
      current_xcas_lines <- gsub("<tempsMax>.*</tempsMax>", paste0("<tempsMax>", sprintf("%.1f", duree_totale_sim), "</tempsMax>"), current_xcas_lines)
      current_xcas_lines <- gsub("<critereArret>.*</critereArret>", paste0("<critereArret>", critere_arret_value, "</critereArret>"), current_xcas_lines)
      current_xcas_lines <- gsub("<pasStock>.*</pasStock>", paste0("<pasStock>", sprintf("%.0f", pas_stockage_value), "</pasStock>"), current_xcas_lines)
      current_xcas_lines <- gsub("<fichResultat>.*</fichResultat>", paste0("<fichResultat>", scenario_base_name, "_ecr.opt</fichResultat>"), current_xcas_lines)
      current_xcas_lines <- gsub("<fichListing>.*</fichListing>", paste0("<fichListing>", scenario_base_name, ".lis</fichListing>"), current_xcas_lines)
      current_xcas_lines <- gsub("<valeursPas>.*</valeursPas>",paste0("<valeursPas>",sprintf("%.2f", val_pas_planimetrage),"</valeursPas>"), current_xcas_lines)
      current_xcas_lines <- gsub("<numDerProf>.*</numDerProf>", paste0("<numDerProf>", num_profiles_total, "</numDerProf>"), current_xcas_lines)
      
      pas_maillage_str <- paste(current_pas_maillage, collapse = " ")
      current_xcas_lines <- gsub("<num1erProfPlage>.*</num1erProfPlage>", "<num1erProfPlage>1 3 5</num1erProfPlage>", current_xcas_lines)
      current_xcas_lines <- gsub("<numDerProfPlage>.*</numDerProfPlage>", paste0("<numDerProfPlage>2 4 ", num_profiles_total, "</numDerProfPlage>"), current_xcas_lines)
      current_xcas_lines <- gsub("<pasEspacePlage>.*</pasEspacePlage>", paste0("<pasEspacePlage>", pas_maillage_str, "</pasEspacePlage>"), current_xcas_lines)
      
      start_frottement_idx <- grep("<frottement>", current_xcas_lines)[1]
      end_frottement_idx <- grep("</frottement>", current_xcas_lines)[1]
      if (!is.na(start_frottement_idx) && !is.na(end_frottement_idx)) {
        abs_deb_zone_str <- paste(sprintf("%.3f", x_coords[c(1, 3, 5)]), collapse = " ")
        abs_fin_zone_str <- paste(sprintf("%.3f", x_coords[c(2, 4, 6)]), collapse = " ")
        strickler_zones_str <- paste(sprintf("%.1f", strickler_zones), collapse = " ")
        frottement_block <- c("      <frottement>", "        <loi>1</loi>", "        <nbZone>3</nbZone>", "        <numBranche>1 1 1</numBranche>", paste0("        <absDebZone>", abs_deb_zone_str, "</absDebZone>"), paste0("        <absFinZone>", abs_fin_zone_str, "</absFinZone>"), paste0("        <coefLitMin>", strickler_zones_str, "</coefLitMin>"), paste0("        <coefLitMaj>", strickler_zones_str, "</coefLitMaj>"), "      </frottement>")
        current_xcas_lines <- c(current_xcas_lines[1:(start_frottement_idx - 1)], frottement_block, current_xcas_lines[(end_frottement_idx + 1):length(current_xcas_lines)])
      }
      
      start_index <- grep("<parametresLoisHydrauliques>", current_xcas_lines)[1]
      end_index <- grep("</parametresLoisHydrauliques>", current_xcas_lines)[1]
      if (!is.na(start_index) && !is.na(end_index)) {
        lois_lines <- c("  <parametresLoisHydrauliques>","    <nb>2</nb>","    <lois>","      <structureParametresLoi>","        <nom>hydro_amont</nom>","        <type>1</type>","        <donnees>","          <modeEntree>1</modeEntree>",paste0("          <fichier>", basename(hydro_file), "</fichier>"),"        </donnees>","      </structureParametresLoi>","      <structureParametresLoi>",paste0("        <nom>limni_", limni_id, "</nom>"),"        <type>2</type>","        <donnees>","          <modeEntree>1</modeEntree>",paste0("          <fichier>", basename(limni_file), "</fichier>"),"        </donnees>","      </structureParametresLoi>","    </lois>","  </parametresLoisHydrauliques>")
        current_xcas_lines <- c(current_xcas_lines[1:(start_index-1)], lois_lines, current_xcas_lines[(end_index+1):length(current_xcas_lines)])
      }
      
      con <- NULL
      tryCatch({
        con <- file(xcas_output_filename, "w")
        writeLines(current_xcas_lines, con)
      }, finally = {
        if(!is.null(con)) close(con)
      })
    }
    message("--- Fichiers .xcas générés avec succès. ---")
    
    # PARTIE 4
    message("\n--- PARTIE 4 : LANCEMENT DES SIMULATIONS ---")
    xcas_files <- dir_ls(current_buse_output_dir, regexp = "\\.xcas$", recurse = FALSE)
    registerDoParallel(cores = num_cores_sim)
    foreach( xcas_file_path = xcas_files, .combine = 'c', .errorhandling = 'pass') %dopar% {
      library(fs)
      xcas_filename <- path_file(xcas_file_path)
      xcas_basename <- path_ext_remove(xcas_filename)
      temp_sim_dir <- path(current_buse_output_dir, paste0("temp_", xcas_basename))
      results_dir <- path(current_buse_output_dir, "results_mascaret")
      
      final_opt_path <- path(results_dir, paste0(xcas_basename, "_ecr.opt"))
      
      if(!file_exists(final_opt_path) || file_info(final_opt_path)$size == 0) {
        dir_create(temp_sim_dir)
        file_copy(xcas_file_path, path(temp_sim_dir, xcas_filename), overwrite = TRUE)
        file_copy(path(current_buse_output_dir, geo_filename_with_ext), temp_sim_dir, overwrite = TRUE)
        file_copy(path(current_buse_output_dir, "hydrogramme_complet.loi"), temp_sim_dir, overwrite = TRUE)
        limni_id_for_copy <- sub(paste0("buse", numero_buse, "_scenario_"), "", xcas_basename)
        limni_to_copy <- path(current_buse_output_dir, paste0("limni_", limni_id_for_copy, ".loi"))
        if(file_exists(limni_to_copy)) file_copy(limni_to_copy, temp_sim_dir, overwrite = TRUE)
        
        batch_content <- sprintf('@echo off\nCALL "%s"\ncd /D "%%~dp0"\npython "%%HOMETEL%%\\scripts\\python3\\mascaret.py" "%s"\nexit /b %%ERRORLEVEL%%', telemac_env_script, xcas_filename)
        batch_file_path <- path(temp_sim_dir, "run_simulation.bat")
        writeLines(batch_content, batch_file_path)
        
        system2("cmd", args = c("/c", path_abs(batch_file_path)), stdout = TRUE, stderr = TRUE)
      }
    }
    stopImplicitCluster()
    message("--- Lancement des simulations terminé. ---")
    
    # NOUVELLE PARTIE 4.5: Copie des résultats après la fin de tous les calculs
    message("\n--- PARTIE 4.5 : COLLECTE DES RÉSULTATS ---")
    results_dir <- path(current_buse_output_dir, "results_mascaret")
    dir_create(results_dir)
    temp_dirs <- dir_ls(current_buse_output_dir, regexp = "temp_buse", type = "directory")
    
    for(temp_dir in temp_dirs) {
      opt_files_in_temp <- dir_ls(temp_dir, regexp = "\\.(opt|lis)$")
      if(length(opt_files_in_temp) > 0) {
        file_copy(opt_files_in_temp, results_dir, overwrite = TRUE)
      }
    }
    message("--- Collecte des fichiers .opt terminée. ---")
    
    
    # PARTIE 5
    message("\n--- PARTIE 5 : LECTURE DES RÉSULTATS ET CRÉATION DE L'ABAQUE ---")
    opt_dir <- file.path(current_buse_output_dir, "results_mascaret")
    
    ABSCISSA_BUSE_ENTREE_TARGET <- x_coords[3]
    ABSCISSA_BUSE_SORTIE_TARGET <- x_coords[4]
    
    opt_files <- list.files(opt_dir, pattern = "\\.opt$", full.names = TRUE)
    results_list <- list()
    
    for (opt_file in opt_files) {
      opt_content_lines <- readLines(opt_file, warn = FALSE)
      resultats_start_line_index <- grep("[resultats]", opt_content_lines, fixed = TRUE)
      if (length(resultats_start_line_index) == 0) next
      opt_data_text <- opt_content_lines[(resultats_start_line_index[1] + 1):length(opt_content_lines)]
      if(length(opt_data_text) == 0 || all(nchar(trimws(opt_data_text)) == 0)) next
      
      opt_data <- tryCatch({ fread(text = opt_data_text, sep = ";", strip.white=TRUE, header = FALSE, colClasses = "numeric", fill = TRUE, select = c(1, 4, 6, 11, 12, 14), col.names = c("Time", "Abscissa", "S_value", "Froude", "Velocity", "DebitTotal")) }, error = function(e) { NULL })
      if(is.null(opt_data) || nrow(opt_data) == 0) next
      
      plateau_end_times <- (1:length(debits_q)) * (duree_montee + duree_palier)
      
      for(time_step in plateau_end_times){
        time_to_extract <- time_step - current_pas_temps
        if(time_to_extract < 0 || time_to_extract > max(opt_data$Time)) next
        
        steady_state_data <- opt_data[abs(opt_data$Time - time_to_extract) < 1e-3, ]
        if(nrow(steady_state_data) == 0) next
        
        entree_buse_row <- steady_state_data[which.min(abs(steady_state_data$Abscissa - ABSCISSA_BUSE_ENTREE_TARGET)), ]
        sortie_buse_row <- steady_state_data[which.min(abs(steady_state_data$Abscissa - ABSCISSA_BUSE_SORTIE_TARGET)), ]
        
        if(nrow(entree_buse_row) == 1 && nrow(sortie_buse_row) == 1){
          avg_velocity <- (entree_buse_row$Velocity + sortie_buse_row$Velocity) / 2
          
          results_list[[length(results_list) + 1]] <- data.frame(
            Debit = entree_buse_row$DebitTotal, 
            S1 = entree_buse_row$S_value, 
            S2 = sortie_buse_row$S_value,
            Froude_amont = entree_buse_row$Froude,
            Avg_Velocity = avg_velocity
          )
        }
      }
    }
    
    results_for_file <- data.frame()
    all_results_df <- data.frame()
    results_df_sorted <- data.frame()
    
    if (length(results_list) > 0) {
      all_results_df <- do.call(rbind, results_list)
      s1_max_threshold <- nozzle_data$Z1 + nozzle_data$HAUT1 + 10
      
      all_results_filtered <- all_results_df[
        all_results_df$S1 <= s1_max_threshold & 
          all_results_df$Froude_amont <= 5 &
          # all_results_df$Avg_Velocity < 7.0 &
          (all_results_df$S1 - all_results_df$S2) >= 0.05,
      ]
      if(nrow(all_results_filtered) > 0) {
        results_df_unique <- unique(all_results_filtered)
        results_df_sorted <- results_df_unique[order(results_df_unique$Debit, results_df_unique$S1), ]
        
        final_points_list <- lapply(split(results_df_sorted, results_df_sorted$Debit), function(df_debit) {
          torrential_points <- df_debit %>% filter(Froude_amont >= 1)
          if (nrow(torrential_points) > 0) return(torrential_points)
          good_fluvial_points <- df_debit %>% filter(Froude_amont < 1 & S1 < (nozzle_data$Z1 + nozzle_data$HAUT1) & S2 < (nozzle_data$Z2 + nozzle_data$HAUT1))
          if (nrow(good_fluvial_points) > 0) return(good_fluvial_points)
          return(df_debit %>% filter(Froude_amont < 1))
        })
        
        results_for_file <- bind_rows(final_points_list) %>% unique() %>% arrange(Debit, S1)
        if(length(which(results_for_file$Froude_amont>=1))>0){
          max_debit_torrential=max(results_for_file$Debit[which(results_for_file$Froude_amont>=1)])
          max_S1_torrentiel=max(results_for_file$S1[which(results_for_file$Debit==max_debit_torrential)])
          max_S2_torrentiel=max(results_for_file$S2[which(results_for_file$Debit==max_debit_torrential)])
          
          points_fluviaux1=which(results_for_file$Debit<max_debit_torrential & (results_for_file$S1 >max_S1_torrentiel | results_for_file$S2 >max_S2_torrentiel ) )
          points_fluviaux2=which(results_for_file$Debit>max_debit_torrential & results_for_file$S2 >nozzle_data$Z2 + nozzle_data$HAUT1 + 0.05  )
          if(length(points_fluviaux1)>0 | length(points_fluviaux2)>0){
            results_for_file=results_for_file[-c(points_fluviaux1,points_fluviaux2),]
            results_for_file <- results_for_file %>% unique() %>% arrange(Debit, S1)
          }
        }
        
        # --- NETTOYAGE ET STRICTE MONOTONIE HYDRAULIQUE (Q et S1) ---
        if (nrow(results_for_file) > 0) {
          # 1. Règle anti-doublons de débit : un seul point par débit (cote max en régime établi)
          results_for_file <- results_for_file %>% 
            group_by(Debit) %>% 
            slice_max(S1, n = 1, with_ties = FALSE) %>% 
            ungroup() %>% 
            arrange(Debit)
          
          # 2. Règle du palier horizontal : ne garder que la 1ère apparition de chaque cote S1
          results_for_file <- results_for_file %>% 
            distinct(S1, .keep_all = TRUE)
          
          # 3. Filtrage de stricte croissance : S1[i] doit être strictement supérieur à S1[i-1]
          if (nrow(results_for_file) > 1) {
            indices_valides <- c(1)
            derniere_cote <- results_for_file$S1[1]
            for (idx_pt in 2:nrow(results_for_file)) {
              if (results_for_file$S1[idx_pt] > derniere_cote) {
                indices_valides <- c(indices_valides, idx_pt)
                derniere_cote <- results_for_file$S1[idx_pt]
              }
            }
            results_for_file <- results_for_file[indices_valides, ]
          }
        }
        
        output_txt_file_S_partiel <- file.path(current_buse_output_dir, "abaque_partiel.txt")
        if (nrow(results_for_file) > 0) {
          file_conn_partiel <- file(output_txt_file_S_partiel, "w")
          writeLines(c(as.character(numero_buse), as.character(nrow(results_for_file))), file_conn_partiel)
          cat(paste(sprintf("%.2f", results_for_file$Debit), collapse = ","), "\n", file = file_conn_partiel)
          cat(paste(sprintf("%.2f", results_for_file$S1), collapse = ","), "\n", file = file_conn_partiel)
          cat(paste(sprintf("%.2f", results_for_file$S2), collapse = ","), "\n", file = file_conn_partiel)
          close(file_conn_partiel)
        }
        
        output_txt_file_QS1_partiel <- file.path(current_buse_output_dir, "abaque_partiel_QS1.txt")
        if (nrow(results_for_file) > 0) {
          results_QS1_unique <- results_for_file %>% dplyr::select(Debit, S1) %>% distinct() %>% arrange(Debit, S1)
          file_conn_QS1 <- file(output_txt_file_QS1_partiel, "w")
          writeLines(c(as.character(numero_buse), as.character(nrow(results_QS1_unique))), file_conn_QS1)
          cat(paste(sprintf("%.2f", results_QS1_unique$Debit), collapse = ","), "\n", file = file_conn_QS1)
          cat(paste(sprintf("%.2f", results_QS1_unique$S1), collapse = ","), "\n", file = file_conn_QS1)
          close(file_conn_QS1)
        }
      }
    }
    
    if(generate_long_profile_plots) {
      message("\n--- GÉNÉRATION DES PROFILS EN LONG ---")
      for (opt_file in opt_files) {
        scenario_name <- sub("_ecr\\.opt$", "", basename(opt_file)); opt_content_lines <- readLines(opt_file, warn = FALSE); resultats_start_line_index <- grep("[resultats]", opt_content_lines, fixed = TRUE); if (length(resultats_start_line_index) == 0) next
        opt_data_full <- tryCatch({ fread(text = opt_content_lines[(resultats_start_line_index[1] + 1):length(opt_content_lines)], sep = ";", strip.white=TRUE, header = FALSE, colClasses = "numeric", fill = TRUE, select = c(1, 4, 5, 6, 14), col.names = c("Time", "Abscissa", "Z_fond", "Z_eau", "DebitTotal")) }, error = function(e) { NULL })
        if(is.null(opt_data_full) || nrow(opt_data_full) == 0) next
        fig_pl <- plot_ly(); profil_fond <- opt_data_full[abs(Time - min(Time)) < 1e-3, ]; fig_pl <- add_trace(fig_pl, data = profil_fond, x = ~Abscissa, y = ~Z_fond, type = 'scatter', mode = 'lines', line = list(color = 'black', width = 3), name = 'Fond')
        for (time_p in ((1:length(debits_q)) * (duree_montee + duree_palier) - current_pas_temps)) { if (time_p > 0 && time_p <= max(opt_data_full$Time)) { steady_data <- opt_data_full[abs(opt_data_full$Time - time_p) < 1e-3, ]; if (nrow(steady_data) > 0) fig_pl <- add_trace(fig_pl, data = steady_data, x = ~Abscissa, y = ~Z_eau, type = 'scatter', mode = 'lines', name = paste('Q =', round(steady_data$DebitTotal[1], 2), 'm³/s')) } }
        y_range <- range(c(profil_fond$Z_fond, opt_data_full$Z_eau), na.rm = TRUE); fig_pl <- fig_pl %>% layout(title = scenario_name, xaxis = list(title = "Abscisse"), yaxis = list(title = "Altitude", range = y_range), shapes = list(list(type = 'line', x0 = x_coords[indices$p3], x1 = x_coords[indices$p3], y0 = y_range[1], y1 = y_range[2], line = list(color = 'grey', dash = 'dash')), list(type = 'line', x0 = x_coords[indices$p4], x1 = x_coords[indices$p4], y0 = y_range[1], y1 = y_range[2], line = list(color = 'grey', dash = 'dash')), list(type = 'line', x0 = x_coords[indices$p3], x1 = x_coords[indices$p4], y0 = nozzle_data$Z1 + nozzle_data$HAUT1, y1 = nozzle_data$Z2 + nozzle_data$HAUT1, line = list(color = 'red', dash = 'longdash'))))
        saveWidget(fig_pl, file = file.path(opt_dir, paste0("profil_en_long_", scenario_name, ".html")), selfcontained = TRUE)
      }
    }
    
    
    if(generate_abaque_plots) {
      message("\n--- PARTIE 6 : GÉNÉRATION DES GRAPHIQUES D'ABAQUE ---")
      
      # Utiliser `all_results_df` pour définir les points à estomper
      points_to_keep <- results_for_file
      points_to_fade <- dplyr::anti_join(all_results_df, points_to_keep, by = c("Debit", "S1", "S2", "Froude_amont", "Avg_Velocity"))
      
      if (nrow(points_to_keep) > 0 || nrow(points_to_fade) > 0) {
        
        # Préparation des données pour les points principaux
        results_df_graph <- points_to_keep
        if(nrow(results_df_graph) > 0) {
          colnames(results_df_graph) <- c("Debit", "S1_Charge_Amont", "S2_Charge_Aval", "Froude_amont", "Avg_Velocity")
          results_df_graph$Froude_Condition <- cut(results_df_graph$Froude_amont, 
                                                   breaks = c(-Inf, 0.99, 1.01, Inf), 
                                                   labels = c("Fluvial (Fr < 1)", "Critique (Fr = 1)", "Torrentiel (Fr > 1)"))
        }
        
        # --- Graphique 3D ---
        title_3d <- paste("Relation Débit-Charges S1/S2 - Buse", numero_buse, "<br>", title_details_str)
        fig_3d_S <- plot_ly()
        
        if(nrow(points_to_fade) > 0) {
          fig_3d_S <- fig_3d_S %>% add_trace(
            data = points_to_fade, x=~Debit, y=~S2, z=~S1,
            type='scatter3d', mode='markers',
            marker=list(size=3, color='rgba(211, 211, 211, 0.5)'), # Gris clair, semi-transparent
            name = "Points exclus",
            hoverinfo = "text",
            text = ~paste('Exclu<br>Débit:', sprintf("%.2f", Debit), '<br>S1:', sprintf("%.2f", S1), '<br>S2:', sprintf("%.2f", S2), '<br>Froude:', sprintf("%.2f", Froude_amont))
          )
        }
        
        if(nrow(results_df_graph) > 0) {
          fig_3d_S <- fig_3d_S %>% add_trace(
            data = results_df_graph, x=~Debit, y=~S2_Charge_Aval, z=~S1_Charge_Amont,
            type='scatter3d', mode='markers',
            color=~S1_Charge_Amont, colorscale='Viridis',
            symbol=~Froude_Condition, symbols=c('circle','square','triangle-up'),
            marker=list(size=4),
            customdata=~Froude_amont,
            name = "Points de l'abaque",
            hovertemplate=paste('<b>Débit:</b> %{x:.2f} m³/s<br>',
                                '<b>S2 (Aval):</b> %{y:.2f} m<br>',
                                '<b>S1 (Amont):</b> %{z:.2f} m<br>',
                                '<b>Froude (Amont):</b> %{customdata:.2f}<br>',
                                '<extra></extra>')
          )
        }
        
        fig_3d_S <- fig_3d_S %>% layout(title=title_3d,
                                        legend=list(title=list(text='<b>Régime Froude</b>')),
                                        scene=list(xaxis=list(title="Débit (m³/s)"),
                                                   yaxis=list(title="S2 Charge Aval (m)"),
                                                   zaxis=list(title="S1 Charge Amont (m)")))
        
        # --- Graphique 2D ---
        title_2d <- paste("Relation S1-S2 - Buse", numero_buse, "<br>", title_details_str)
        fig_2d_S <- plot_ly()
        
        if(nrow(points_to_fade) > 0) {
          fig_2d_S <- fig_2d_S %>% add_trace(
            data = points_to_fade, x=~S1, y=~S2,
            type='scatter', mode='markers',
            marker=list(size=5, color='rgba(211, 211, 211, 0.6)'), # Gris clair, semi-transparent
            name = "Points exclus",
            hoverinfo = "text",
            text = ~paste('Exclu<br>Débit:', sprintf("%.2f", Debit), '<br>S1:', sprintf("%.2f", S1), '<br>S2:', sprintf("%.2f", S2), '<br>Froude:', sprintf("%.2f", Froude_amont))
          )
        }
        
        if(nrow(results_df_graph) > 0) {
          fig_2d_S <- fig_2d_S %>% add_trace(
            data = results_df_graph, x=~S1_Charge_Amont, y=~S2_Charge_Aval,
            type='scatter', mode='markers',
            color=~Debit, colorscale='Plasma',
            symbol=~Froude_Condition, symbols=c('circle','square','triangle-up'),
            marker=list(size=6),
            customdata=~Froude_amont,
            name = "Points de l'abaque",
            hovertemplate=paste('<b>S1 (Amont):</b> %{x:.2f} m<br>',
                                '<b>S2 (Aval):</b> %{y:.2f} m<br>',
                                '<b>Débit:</b> %{marker.color:.2f} m³/s<br>',
                                '<b>Froude:</b> %{customdata:.2f}<br>',
                                '<extra></extra>')
          )
          
          shapes_list <- list(
            list(type = 'line', x0 = nozzle_data$Z1, x1 = nozzle_data$Z1, y0 = min(results_df_graph$S2_Charge_Aval), y1 = max(results_df_graph$S2_Charge_Aval), line = list(color = 'blue', dash = 'longdash', width = 2)),
            list(type = 'line', x0 = nozzle_data$Z1 + nozzle_data$HAUT1, x1 = nozzle_data$Z1 + nozzle_data$HAUT1, y0 = min(results_df_graph$S2_Charge_Aval), y1 = max(results_df_graph$S2_Charge_Aval), line = list(color = 'red', dash = 'longdash', width = 2)),
            list(type = 'line', y0 = nozzle_data$Z2, y1 = nozzle_data$Z2, x0 = min(results_df_graph$S1_Charge_Amont), x1 = max(results_df_graph$S1_Charge_Amont), line = list(color = 'blue', dash = 'longdash', width = 2)),
            list(type = 'line', y0 = nozzle_data$Z2 + nozzle_data$HAUT1, y1 = nozzle_data$Z2 + nozzle_data$HAUT1, x0 = min(results_df_graph$S1_Charge_Amont), x1 = max(results_df_graph$S1_Charge_Amont), line = list(color = 'red', dash = 'longdash', width = 2))
          )
          fig_2d_S <- fig_2d_S %>% layout(shapes = shapes_list)
        }
        
        fig_2d_S <- fig_2d_S %>% layout(
          title=title_2d,
          xaxis=list(title="S1 Charge Amont (m)"),
          yaxis=list(title="S2 Charge Aval (m)"),
          legend=list(title=list(text='<b>Régime Froude</b>'))
        )
        
        htmlwidgets::saveWidget(fig_3d_S, file.path(current_buse_output_dir, "graph_abaque_S_3d.html"), selfcontained = TRUE)
        htmlwidgets::saveWidget(fig_2d_S, file.path(current_buse_output_dir, "graph_abaque_S_2d.html"), selfcontained = TRUE)
        message("Graphiques d'abaque 2D et 3D (incluant les points exclus) exportés.")
        
      } else {
        message("Aucune donnée à visualiser, les graphiques d'abaque n'ont pas été générés pour cette buse.")
      }
    }
    
    # --- BLOC DE NETTOYAGE SÉLECTIF (AJOUTÉ v123) ---
    if (cleanup_calc_files) {
      message(paste("      - Nettoyage sélectif pour la buse", numero_buse))
      
      # 1. Identifier tous les dossiers temporaires temp_...
      tdirs <- fs::dir_ls(current_buse_output_dir, regexp = "temp_", type = "directory")
      if (length(tdirs) > 1) {
        # Garder uniquement le premier dossier pour vérification, supprimer les autres
        fs::dir_delete(tdirs[-1]) 
      }
      
      # 2. Supprimer le dossier global results_mascaret (doublon inutile une fois les données extraites)
      res_fold <- file.path(current_buse_output_dir, "results_mascaret")
      if(fs::dir_exists(res_fold)) fs::dir_delete(res_fold)
      
      # 3. Supprimer les fichiers d'entrée à la racine (déjà sauvegardés dans le dossier temp conservé)
      root_files <- fs::dir_ls(current_buse_output_dir, regexp = "\\.(xcas|geo|loi|lis)$")
      if(length(root_files) > 0) fs::file_delete(root_files)
    }
  } 
  
  #########################################################################
  # DÉMARRAGE DE LA BOUCLE PRINCIPALE
  #########################################################################
  if (ncore_buse <= 1) {
    message("--- DÉMARRAGE DU TRAITEMENT SÉQUENTIEL ---")
    for (i in 1:nrow(busel_data)) {
      abaques_check_file <- file.path(dsnlayerC, "abaques2.txt")
      if (file.exists(abaques_check_file) && file.info(abaques_check_file)$size > 0) {
        message(paste(">>> BUSE", abaques_check_file, ": DÉJÀ TRAITÉE (Fichier abaque trouvé). PASSAGE À LA SUIVANTE. <<<"))
        next
      }
      process_buse(i, busel_data, main_output_dir, telemac_env_script, xcas_template_filename, use_mnt_for_profiles, mnt_path, dist_amont_limite, dist_p4_p6, num_points_per_profile, dist_extraction_profil, first_profile_shape, duree_palier, duree_montee, num_cores_sim, strickler_zones_base, val_pas_planimetrage, g, pas_maillage_court, pas_temps_court, pas_maillage_moyen, pas_temps_moyen, pas_maillage_long, pas_temps_long, preissmann_slot_width, generate_long_profile_plots, generate_abaque_plots, cleanup_calc_files)
    }
  } else {
    message(paste("--- DÉMARRAGE DU TRAITEMENT PARALLÈLE SUR", ncore_buse, "CŒURS ---"))
    registerDoParallel(cores = ncore_buse)
    foreach(i = 1:nrow(busel_data)) %dopar% { 
      process_buse(i, busel_data, main_output_dir, telemac_env_script, xcas_template_filename, use_mnt_for_profiles, mnt_path, dist_amont_limite, dist_p4_p6, num_points_per_profile, dist_extraction_profil, first_profile_shape, duree_palier, duree_montee, num_cores_sim, strickler_zones_base, val_pas_planimetrage, g, pas_maillage_court, pas_temps_court, pas_maillage_moyen, pas_temps_moyen, pas_maillage_long, pas_temps_long, preissmann_slot_width, generate_long_profile_plots, generate_abaque_plots, cleanup_calc_files) 
    }
    stopImplicitCluster()
  }
  
  #########################################################################
  # PARTIE 7: ASSEMBLAGE FINAL DE L'ABAQUE
  #########################################################################
  message("\n--- PARTIE 7 : ASSEMBLAGE DU FICHIER D'ABAQUE FINAL ---")
  output_txt_file_S_global <- file.path(main_output_dir, "resultats_buse_format_abaqueSQ.txt")
  file_conn_main <- file(output_txt_file_S_global, "w")
  writeLines("#abaqueSQ", file_conn_main)
  output_txt_file_QS1_global <- file.path(main_output_dir, "abaques.txt")
  file_conn_qs1 <- file(output_txt_file_QS1_global, "w")
  writeLines("#abaqueQS1", file_conn_qs1)
  
  for (i in 1:nrow(busel_data)) {
    numero_buse <- busel_data$id[i]
    current_buse_output_dir <- file.path(main_output_dir, paste0("buse_", numero_buse))
    
    p1 <- file.path(current_buse_output_dir, "abaque_partiel.txt")
    if (file.exists(p1)) writeLines(readLines(p1), file_conn_main)
    
    p2 <- file.path(current_buse_output_dir, "abaque_partiel_QS1.txt")
    if (file.exists(p2)) writeLines(readLines(p2), file_conn_qs1)
  }
  
  close(file_conn_main); close(file_conn_qs1)
  message("\n--- WORKFLOW COMPLET TERMINÉ ---")
  file.copy(from = file.path(main_output_dir, "abaques.txt"),to = dsnlayerC,overwrite = TRUE,recursive = TRUE)
  # 1. Lire toutes les lignes du fichier d'origine
  lignes <- readLines(file.path(dsnlayerC, "abaques.txt"))
  
  # 2. Nettoyer les éventuelles lignes vides à la fin du fichier
  lignes <- lignes[lignes != ""]
  
  # 3. Calculer le nombre de buses
  # On soustrait 1 (pour la ligne de commentaire) avant de diviser par 4
  n_buses <- floor((length(lignes) - 1) / 4)
  
  # 4. Générer les indices des IDs
  # On commence à la ligne 2, et on fait des sauts de 4
  indices_id <- seq(2, by = 4, length.out = n_buses)
  
  # 5. Remplacer les lignes d'identifiants par la nouvelle numérotation
  lignes[indices_id] <- 1:n_buses
  
  # 6. Sauvegarder le résultat en écrasant le fichier
  writeLines(as.character(lignes), file.path(dsnlayerC, "abaques.txt"))
  # Script R pour lisser les abaques Telemac-2D
  # Objectif : Garantir une croissance stricte des hauteurs d'eau (S1) 
  # pour éviter les divisions par zéro dans DISCHARGE_INTERPOL.
  
  # Noms des fichiers (à adapter si besoin)
  file_in <- file.path(dsnlayerC, "abaques.txt")
  file_out <- file.path(dsnlayerC, "abaques2.txt")
  
  # Lecture du fichier
  lines <- readLines(file_in, warn = FALSE)
  # Nettoyage des lignes vides éventuelles
  lines <- lines[trimws(lines) != ""]
  
  # On conserve l'entête (ex: #abaqueQS1)
  output_lines <- c(lines[1])
  
  idx <- 2
  while (idx <= length(lines)) {
    # Extraction du bloc de 4 lignes
    abaque_id <- trimws(lines[idx])
    n_pts_old <- as.integer(trimws(lines[idx + 1]))
    q_str     <- lines[idx + 2]
    s1_str    <- lines[idx + 3]
    
    # Conversion des lignes Q et S1 en vecteurs numériques
    q_vals  <- as.numeric(unlist(strsplit(q_str, ",")))
    s1_vals <- as.numeric(unlist(strsplit(s1_str, ",")))
    
    # Construction du dataframe pour nettoyage
    df_abaque <- data.frame(Debit = q_vals, S1 = s1_vals)
    
    # 1. Anti-doublons de débit (garder cote max en régime établi)
    df_abaque <- df_abaque %>%
      group_by(Debit) %>%
      slice_max(S1, n = 1, with_ties = FALSE) %>%
      ungroup() %>%
      arrange(Debit)
    
    # 2. Règle du palier horizontal : ne garder que la 1ère apparition de chaque cote S1
    df_abaque <- df_abaque %>%
      distinct(S1, .keep_all = TRUE)
    
    # 3. Filtrage de stricte croissance : S1[i] > S1[i-1]
    if (nrow(df_abaque) > 1) {
      indices_valides <- c(1)
      derniere_cote <- df_abaque$S1[1]
      for (idx_pt in 2:nrow(df_abaque)) {
        if (df_abaque$S1[idx_pt] > derniere_cote) {
          indices_valides <- c(indices_valides, idx_pt)
          derniere_cote <- df_abaque$S1[idx_pt]
        }
      }
      df_abaque <- df_abaque[indices_valides, ]
    }
    
    n_pts  <- nrow(df_abaque)
    q_out  <- paste(sprintf("%.2f", df_abaque$Debit), collapse = ",")
    s1_out <- paste(sprintf("%.2f", df_abaque$S1), collapse = ",")
    
    # Stockage des 4 lignes du bloc corrigé
    output_lines <- c(output_lines, abaque_id, as.character(n_pts), q_out, s1_out)
    
    # On passe à l'abaque suivant
    idx <- idx + 4
  }
  
  # Écriture du nouveau fichier
  writeLines(output_lines, file_out)
  cat("Traitement terminé avec succès !\nLe fichier corrigé est :", file_out, "\n")
}
abacus=function(){
  library(sf)
  library(terra)
  library(dplyr)
  library(fs)
  library(stringr)
  library(foreach)
  library(doParallel)
  library(data.table)
  library(plotly)
  library(htmlwidgets)
  
  #########################################################################
  # PARTIE 0: PARAMÈTRES UTILISATEUR
  # Modifiez les valeurs dans cette section pour configurer votre simulation.
  #########################################################################
  # --- 0.1. Configuration des chemins et fichiers ---
  # chem_routine <- "C:/Users/CartoNationale/Desktop/Nabil"
  
  main_output_dir <- file.path(dsnlayerC, "abaques") 
  telemac_env_script <- "C:/TELEMAC/V8P4/configs/pysource.bat"
  xcas_template_filename <- file.path(chem_routine,"C2D",Doss_Base, "masc.xcas.txt")
  
  # --- 0.2. Paramètres de parallélisation ---
  ncore_buse <- ncore_buse_abaques # Nombre de cœurs pour la boucle sur les buses. 1 = séquentiel, >=2 = parallèle
  num_cores_sim <- num_cores_sim_abaques
  # Nombre de cœurs pour les simulations d'une MÊME buse
  
  # --- 0.3. Contrôle de la géométrie --- 
  use_mnt_for_profiles <- FALSE # Mettre à TRUE pour extraire du MNT, FALSE pour utiliser des profils trapézoïdaux
  
  # --- Chemins pour le mode MNT ---
  shapefile_path <- file.path(dsnlayerC, "OuvHydrau_Select.shp") 
  mnt_path <- file.path(dsnlayerC, "MNTFinal.gpkg") # <--- CHEMIN VERS VOTRE MNT
  
  # --- 0.4. Paramètres de la géométrie ---
  dist_amont_limite <- 1000.0
  dist_p4_p6 <- 2000 # Distance entre la sortie de la buse (corrigée à p5) et la limite aval (p6)
  num_points_per_profile <- 101
  dist_extraction_profil <- 3.0 
  preissmann_slot_width <- 0.01 # Largeur de la fente de Preissmann en mètres
  
  first_profile_shape <- list(
    type = "trapezoidal", bottom_width = 3, left_slope = 1/1.5,
    right_slope = 1/1.5, bank_height_above_bottom = 40
  )
  
  # --- 0.5. Paramètres temporels et hydrauliques ---
  duree_palier <- 400
  duree_montee <- 50
  strickler_zones_base <- c(20.0, 20.0, 20.0) 
  val_pas_planimetrage <- 0.02
  g <- 9.81
  
  # --- 0.6. Paramètres de maillage et de temps de simulation adaptatifs ---
  # Pour buses COURTES (< 20m)
  pas_maillage_court <- c(1, 0.5, 1)
  pas_temps_court <- 0.5
  duree_palier_court <- 20
  duree_montee_court <- 10
  
  # Pour buses MOYENNES (20m à 100m)
  pas_maillage_moyen <- c(1, 1, 1)
  pas_temps_moyen <- 1
  duree_palier_moyen <- 100
  duree_montee_moyen <- 20
  # Pour buses LONGUES (> 100m)
  pas_maillage_long <- c(2, 2, 2)
  pas_temps_long <- 2
  duree_palier_long <- 400
  duree_montee_long <- 50
  # --- 0.7. Paramètres de sortie (NOUVEAU) ---
  generate_long_profile_plots <- TRUE # Mettre à TRUE pour générer les graphiques de profil en long
  generate_abaque_plots <- TRUE       # Mettre à TRUE pour générer les graphiques d'abaque 2D/3D
  cleanup_calc_files <- FALSE           # <--- NOUVEAU: TRUE pour garder 1 dossier temp et supprimer le reste
  
  
  #########################################################################
  # LECTURE DES DONNÉES SPATIALES ET INITIALISATION
  #########################################################################
  message("--- LECTURE DES DONNÉES SPATIALES ---")
  if (!file.exists(shapefile_path)) stop("Le fichier shapefile n'a pas été trouvé: ", shapefile_path)
  busel_data <- st_read(shapefile_path, quiet = TRUE)
  message(paste("Lecture de", nrow(busel_data), "buse(s) depuis le shapefile."))
  
  mnt_raster <- NULL # Initialisation à NULL
  if(use_mnt_for_profiles) {
    if (!file.exists(mnt_path)) stop("Le fichier MNT n'a pas été trouvé: ", mnt_path)
    message("Chargement du MNT pour vérification CRS...")
    temp_mnt <- rast(mnt_path)
    message("MNT chargé.")
    if (st_crs(busel_data) != st_crs(temp_mnt)) {
      message("ATTENTION: CRS différents. Reprojection du shapefile vers le CRS du MNT.")
      busel_data <- st_transform(busel_data, st_crs(temp_mnt))
    }
    rm(temp_mnt) # On supprime l'objet, il sera rechargé dans chaque worker
    gc() # Force le nettoyage de la mémoire
  }
  
  dir.create(main_output_dir, showWarnings = FALSE, recursive = TRUE)
  
  #########################################################################
  # DÉFINITION DE LA FONCTION DE TRAITEMENT D'UNE BUSE
  #########################################################################
  process_buse <- function(i, busel_data, main_output_dir, telemac_env_script, xcas_template_filename, use_mnt_for_profiles, mnt_path, dist_amont_limite, dist_p4_p6, num_points_per_profile, dist_extraction_profil, first_profile_shape, duree_palier, duree_montee, num_cores_sim, strickler_zones_base, val_pas_planimetrage, g, pas_maillage_court, pas_temps_court, pas_maillage_moyen, pas_temps_moyen, pas_maillage_long, pas_temps_long, preissmann_slot_width, generate_long_profile_plots, generate_abaque_plots, cleanup_calc_files) {
    
    # Charger les librairies nécessaires dans l'environnement parallèle
    library(sf)
    library(terra)
    library(dplyr)
    library(fs)
    library(stringr)
    library(foreach)
    library(doParallel)
    library(data.table)
    library(plotly)
    library(htmlwidgets)
    
    # Si on utilise le MNT, chaque worker charge sa propre copie.
    if (use_mnt_for_profiles) {
      mnt_raster <- rast(mnt_path)
    }
    
    buse_row <- busel_data[i, ]
    numero_buse <- buse_row$id
    
    # Création du dossier de sortie
    current_buse_output_dir <- file.path(main_output_dir, paste0("buse_", numero_buse))
    dir.create(current_buse_output_dir, showWarnings = FALSE, recursive = TRUE)
    
    # -----------------------------------------------------------------------
    # CHECK DE DÉJÀ FAIT (NOUVELLE FONCTIONNALITÉ v124)
    # -----------------------------------------------------------------------
    abaque_check_file <- file.path(current_buse_output_dir, "abaque_partiel.txt")
    if (file.exists(abaque_check_file) && file.info(abaque_check_file)$size > 0) {
      message(paste(">>> BUSE", numero_buse, ": DÉJÀ TRAITÉE (Fichier abaque trouvé). PASSAGE À LA SUIVANTE. <<<"))
      return(NULL)
    }
    # -----------------------------------------------------------------------
    
    CIRC <- buse_row$CIRC
    if(is.na(buse_row$HAUT1) | buse_row$HAUT1==0) buse_row$HAUT1=buse_row$HAUT2
    nozzle_data <- list(LARG = buse_row$LRG, HAUT1 = buse_row$HAUT1, Z1 = buse_row$Z1, Z2 = buse_row$Z2, LENGTH = buse_row$LENGTH)
    first_profile_shape$bottom_width=nozzle_data$LARG*2
    if ("FRIC" %in% names(buse_row) && !is.na(buse_row$FRIC) && buse_row$FRIC > 0) {
      strickler_buse_value <- 1 / buse_row$FRIC
      if (strickler_buse_value < 1) {
        warning(paste("Buse", numero_buse, ": La valeur FRIC=", buse_row$FRIC, "donne un Strickler de", round(strickler_buse_value, 2), ". Utilisation de la valeur par défaut de 50.0."))
        strickler_buse_value <- 60.0
      }
    } else {
      strickler_buse_value <- 60.0
      warning(paste("Buse", numero_buse, ": Colonne 'FRIC' non valide ou absente. Utilisation de la valeur par défaut de Strickler:", strickler_buse_value))
    }
    strickler_zones <- c(strickler_zones_base[1], strickler_buse_value, strickler_zones_base[3])
    
    
    message(paste("\n\n############################################################"))
    message(paste("--- DÉMARRAGE DU TRAITEMENT POUR LA BUSE :", numero_buse, "---"))
    message(paste("--- Fichiers de sortie dans :", current_buse_output_dir, "---"))
    message(paste("############################################################\n"))
    
    if (nozzle_data$LENGTH < 20) {
      current_pas_maillage <- pas_maillage_court
      current_pas_temps <- pas_temps_court
      duree_palier <- duree_palier_court
      duree_montee <- duree_montee_court
      dist_amont_limite <- 20
      dist_p4_p6 <- 50 
      message("      - Buse courte (< 20m). Maillage fin et pas de temps court sélectionnés.")
    } else if (nozzle_data$LENGTH >= 20 && nozzle_data$LENGTH <= 250) {
      current_pas_maillage <- pas_maillage_moyen
      current_pas_temps <- pas_temps_moyen
      duree_palier <- duree_palier_moyen
      duree_montee <- duree_montee_moyen
      dist_amont_limite <- 50
      dist_p4_p6 <- 400
      message("      - Buse moyenne (20-100m). Maillage et pas de temps moyens sélectionnés.")
    } else {
      current_pas_maillage <- pas_maillage_long
      current_pas_temps <- pas_temps_long
      duree_palier <- duree_palier_long
      duree_montee <- duree_montee_long
      dist_amont_limite <- 500
      dist_p4_p6 <- 2000 
      message("      - Buse longue (> 100m). Maillage large et pas de temps long sélectionnés.")
    }
    
    I <- (nozzle_data$Z1 - nozzle_data$Z2) / nozzle_data$LENGTH
    if (CIRC == 1) {
      D <- nozzle_data$HAUT1; R <- D / 2; S <- pi * R^2; P <- 2 * pi * R; Rh <- R/2
      Q <- strickler_buse_value * S * (Rh^(2/3)) * sqrt(abs(I))
    } else {
      S <- nozzle_data$LARG * nozzle_data$HAUT1; P <- nozzle_data$LARG + (2 * nozzle_data$HAUT1); Rh <- S / P
      Q <- strickler_buse_value * S * (Rh^(2/3)) * sqrt(abs(I))
    }
    Q <- round(Q)
    
    if (Q < 15) {
      message(paste("      - Le débit max estimé (", Q, "m³/s) est inférieur à 15 m³/s. Il est forcé à 15 m³/s pour la simulation."))
      Q <- 15
    }
    
    Q_sim_max <- Q * 1.5
    if (Q_sim_max > 180) {
      message(paste("      - Le débit max de simulation (", round(Q_sim_max), "m³/s) dépasse 180 m³/s. Il est plafonné à 180 m³/s."))
      Q_sim_max <- 180
    }
    
    debits_q_list <- list()
    current_max_q <- 0
    
    end_q_p1 <- min(Q_sim_max, 1.0)
    if (0.2 <= end_q_p1) {
      debits_q_list[[length(debits_q_list) + 1]] <- seq(0.2, end_q_p1, by = 0.1)
      current_max_q <- end_q_p1
    }
    
    start_val_p2 <- current_max_q + 0.2
    end_q_p2 <- min(Q_sim_max, 10.0)
    if (start_val_p2 <= end_q_p2) {
      debits_q_list[[length(debits_q_list) + 1]] <- seq(start_val_p2, end_q_p2, by = 0.2)
      current_max_q <- end_q_p2
    }
    
    start_val_p3 <- current_max_q + 1
    end_q_p3 <- min(Q_sim_max, 20.0)
    if (start_val_p3 <= end_q_p3) {
      debits_q_list[[length(debits_q_list) + 1]] <- seq(start_val_p3, end_q_p3, by = 1)
      current_max_q <- end_q_p3
    }
    
    start_val_p4 <- current_max_q + 2.0
    end_q_p4 <- min(Q_sim_max, 40.0)
    if (start_val_p4 <= end_q_p4) {
      debits_q_list[[length(debits_q_list) + 1]] <- seq(start_val_p4, end_q_p4, by = 2.0)
      current_max_q <- end_q_p4
    }
    
    start_val_p5 <- current_max_q + 5.0
    end_q_p5 <- Q_sim_max
    if (start_val_p5 <= end_q_p5) {
      debits_q_list[[length(debits_q_list) + 1]] <- seq(start_val_p5, end_q_p5, by = 5.0)
    }
    
    debits_q <- unique(sort(c(unlist(debits_q_list))))
    debits_q <- round(debits_q[debits_q > 0], 2)
    
    if (CIRC == 1) {
      dimension_str <- sprintf("Diamètre: %.2f m", nozzle_data$HAUT1)
    } else {
      dimension_str <- sprintf("Largeur: %.2f m, Hauteur: %.2f m", nozzle_data$LARG, nozzle_data$HAUT1)
    }
    pente_pourcent <- I * 100
    title_details_str <- paste(
      dimension_str, 
      sprintf("Longueur: %.1f m", nozzle_data$LENGTH), 
      sprintf("Pente: %.2f %%", pente_pourcent),
      sprintf("Qmax simulé: %.1f m³/s", Q_sim_max), 
      sep=" | "
    )
    
    #########################################################################
    # PARTIE 1: GÉNÉRATION DU FICHIER DE GÉOMÉTRIE (.geo)
    #########################################################################
    message("--- PARTIE 1 : GÉOMÉTRIE (.geo) ---")
    
    create_rectangular_profile <- function(nozzle_width, nozzle_height, nozzle_bottom_z, target_center_y, preissmann_slot_width = 0.01, slot_height = 50, bank_y_offset = 10) {
      y_center <- target_center_y
      z_bottom <- nozzle_bottom_z
      z_top <- z_bottom + nozzle_height
      z_slot_top <- z_top + slot_height
      
      y_bottom_left  <- y_center - nozzle_width/2
      y_bottom_right <- y_center + nozzle_width/2
      y_top_left     <- y_bottom_left - 0.01
      y_top_right    <- y_bottom_right + 0.01
      
      y_slot_left    <- y_center - preissmann_slot_width/2
      y_slot_right   <- y_center + preissmann_slot_width/2
      
      y_bank_left    <- y_top_left - bank_y_offset
      y_bank_right   <- y_top_right + bank_y_offset
      
      full_profile_df <- rbind(
        data.frame(Y = y_bank_left, Z = z_slot_top), 
        data.frame(Y = y_slot_left, Z = z_slot_top), 
        data.frame(Y = y_slot_left, Z = z_top), 
        data.frame(Y = y_top_left, Z = z_top), 
        data.frame(Y = y_bottom_left, Z = z_bottom), 
        data.frame(Y = y_bottom_right, Z = z_bottom), 
        data.frame(Y = y_top_right, Z = z_top), 
        data.frame(Y = y_slot_right, Z = z_top), 
        data.frame(Y = y_slot_right, Z = z_slot_top), 
        data.frame(Y = y_bank_right, Z = z_slot_top)
      )
      full_profile_df$Marker <- "B"
      return(full_profile_df[!duplicated(round(full_profile_df[, c("Y", "Z")], 5)), ])
    }
    create_circular_profile <- function(diameter, nozzle_bottom_z, target_center_y, slot_width = 0.01, slot_height = 50, bank_y_offset = 10, n_points_arch = 41) {
      radius <- diameter/2; y_center <- target_center_y; z_center <- nozzle_bottom_z + radius; z_connection <- z_center + sqrt(max(0, radius^2 - (slot_width/2)^2)); z_slot_top <- z_connection + slot_height; y_slot_left <- y_center - slot_width/2; y_slot_right <- y_center + slot_width/2; y_structure_left <- y_center - radius; y_structure_right <- y_center + radius; p_berge_gauche_loin <- data.frame(Y = y_structure_left - bank_y_offset, Z = z_slot_top); p_fente_bas_gauche  <- data.frame(Y = y_slot_left, Z = z_connection); theta_start <- atan2(z_connection - z_center, y_slot_left - y_center); theta_end     <- atan2(z_connection - z_center, y_slot_right - y_center); theta_sequence <- seq(from = theta_start, to = theta_end + 2*pi, length.out = n_points_arch); arch_df <- data.frame(Y = y_center + radius * cos(theta_sequence), Z = z_center + radius * sin(theta_sequence)); p_fente_bas_droite  <- data.frame(Y = y_slot_right, Z = z_connection); p_berge_droite_loin <- data.frame(Y = y_structure_right + bank_y_offset, Z = z_slot_top)
      full_profile_df <- rbind(p_berge_gauche_loin, data.frame(Y = y_slot_left, Z = z_slot_top), p_fente_bas_gauche, arch_df, p_fente_bas_droite, data.frame(Y = y_slot_right, Z = z_slot_top), p_berge_droite_loin)
      full_profile_df$Marker <- "B"
      return(full_profile_df[!duplicated(round(full_profile_df[, c("Y", "Z")], 5)), ])
    }
    
    generate_points_for_trapezoidal <- function(bottom_width, left_slope, right_slope, bottom_elevation, bank_height_above_bottom, target_center_y, bank_y_offset = 10) {
      half_bottom_width <- bottom_width / 2
      top_z <- bottom_elevation + bank_height_above_bottom
      
      # Les points sont d'abord calculés de manière centrée autour de Y=0
      y_top_left_rel <- -half_bottom_width - (left_slope * bank_height_above_bottom)
      y_top_right_rel <- half_bottom_width + (right_slope * bank_height_above_bottom)
      y_rel <- c(y_top_left_rel - bank_y_offset, y_top_left_rel, -half_bottom_width, half_bottom_width, y_top_right_rel, y_top_right_rel + bank_y_offset)
      
      # Le thalweg de ce profil relatif est à Y=0. On décale tous les points pour que le thalweg soit à target_center_y.
      y_abs <- y_rel + target_center_y
      
      z_abs <- c(top_z, top_z, bottom_elevation, bottom_elevation, top_z, top_z)
      points_df <- data.frame(Y = y_abs, Z = z_abs, Marker = "B")
      return(points_df[order(points_df$Y), ])
    }
    
    extract_profile_from_mnt <- function(mnt, center_point_sf, direction_vector, profile_width, num_points = 101) {
      perp_vector <- c(-direction_vector[2], direction_vector[1])
      perp_vector_norm <- perp_vector / sqrt(sum(perp_vector^2))
      start_point <- st_coordinates(center_point_sf) - (profile_width / 2) * perp_vector_norm
      end_point <- st_coordinates(center_point_sf) + (profile_width / 2) * perp_vector_norm
      transect_line <- st_sfc(st_linestring(rbind(start_point, end_point)), crs = st_crs(mnt))
      sampled_points_sf <- st_line_sample(transect_line, n = num_points, type = "regular")
      if (st_is_empty(sampled_points_sf)) return(NULL)
      sampled_points_sf <- st_cast(sampled_points_sf, "POINT")
      sampled_points_vect <- vect(sampled_points_sf)
      extracted_data <- terra::extract(mnt, sampled_points_vect, xy = TRUE, ID = FALSE)
      if (nrow(extracted_data) == 0 || all(is.na(extracted_data[, 1]))) {
        warning(paste0("L'extraction MNT a échoué (autour de X=", round(st_coordinates(center_point_sf)[1]),", Y=", round(st_coordinates(center_point_sf)[2]), "). Profil hors emprise?"))
        return(NULL)
      }
      colnames(extracted_data)[1] <- "Z"
      coords <- extracted_data[, c("x", "y")]
      y_relative <- sqrt((coords$x - coords$x[1])^2 + (coords$y - coords$y[1])^2)
      profile_df <- data.frame(Y = y_relative, Z = extracted_data$Z, Marker = "B")
      profile_df <- na.omit(profile_df)
      if (nrow(profile_df) < 2) return(NULL)
      return(profile_df)
    }
    
    shift_profile_z <- function(profile_df, target_min_z) {
      if(is.null(profile_df) || nrow(profile_df) == 0) return(NULL)
      current_min_z <- min(profile_df$Z, na.rm = TRUE)
      delta_z <- target_min_z - current_min_z
      profile_df$Z <- profile_df$Z + delta_z
      return(profile_df)
    }
    
    center_profile_on_thalweg <- function(profile_df, target_y_center) {
      if(is.null(profile_df) || nrow(profile_df) == 0) return(NULL)
      min_z_index <- which.min(profile_df$Z)
      y_of_min_z <- mean(profile_df$Y[min_z_index]) 
      shift_y <- target_y_center - y_of_min_z
      profile_df$Y <- profile_df$Y + shift_y
      return(profile_df)
    }
    
    add_vertical_walls <- function(profile_df) {
      if(is.null(profile_df) || nrow(profile_df) < 2) return(profile_df)
      
      first_point <- profile_df[1, ]
      wall_start_point <- data.frame(Y = first_point$Y, Z = first_point$Z + 20, Marker = "B")
      
      last_point <- profile_df[nrow(profile_df), ]
      wall_end_point <- data.frame(Y = last_point$Y, Z = last_point$Z + 20, Marker = "B")
      
      new_profile <- rbind(wall_start_point, profile_df, wall_end_point)
      return(new_profile)
    }
    
    write_geo_profile <- function(con, bief_name, prof_name, prof_abs, points_df) {
      writeLines(paste0("PROFIL ", bief_name, " ", prof_name, " ", sprintf("%.3f", prof_abs)), con)
      for (j in 1:nrow(points_df)) {
        writeLines(paste(sprintf("%.3f", points_df$Y[j]), sprintf("%.5f", points_df$Z[j]), points_df$Marker[j]), con)
      }
    }
    
    model_total_length <- dist_amont_limite + nozzle_data$LENGTH + dist_p4_p6
    x_nozzle_start <- dist_amont_limite
    x_nozzle_end <- x_nozzle_start + nozzle_data$LENGTH
    x_coords <- c(0, x_nozzle_start - 2.0, x_nozzle_start, x_nozzle_end, x_nozzle_end + 2.0, model_total_length)
    num_profiles_total <- length(x_coords)
    
    buse_geom <- st_geometry(buse_row)
    
    geo_base_filename <- paste0("geom_buse", numero_buse, ifelse(use_mnt_for_profiles, "_MNT", "_Trapeze"))
    geo_filename_with_ext <- paste0(geo_base_filename, ".geo")
    geo_filepath <- file.path(current_buse_output_dir, geo_filename_with_ext)
    message(paste("Génération du fichier de géométrie :", geo_filename_with_ext))
    
    bief_name <- "Bief_1"
    indices <- list(p1 = 1, p2 = 2, p3 = 3, p4 = 4, p5 = 5, p6 = 6)
    
    all_profiles <- vector("list", num_profiles_total)
    
    # Déterminer une abscisse Y cible unique pour le centrage de tous les profils.
    half_width_calc <- first_profile_shape$bottom_width / 2 + 
      max(first_profile_shape$left_slope, first_profile_shape$right_slope) * first_profile_shape$bank_height_above_bottom + 
      10.0 # bank_y_offset de la fonction generate_points_for_trapezoidal
    target_center_y <- ceiling(half_width_calc) + 5.0 # Ajout d'une marge de sécurité
    message(paste("      - Abscisse Y cible pour le centrage de tous les profils:", sprintf("%.3f", target_center_y)))
    
    if(use_mnt_for_profiles) {
      message("      - Mode MNT: Extraction des profils depuis le MNT...")
      
      buse_coords <- st_coordinates(buse_geom)
      if(nrow(buse_coords) < 2) stop("Géométrie de buse invalide (moins de 2 points) pour la buse ", numero_buse)
      
      start_node <- buse_coords[1, 1:2]
      end_node <- buse_coords[nrow(buse_coords), 1:2]
      direction_vector <- end_node - start_node
      direction_vector_norm <- direction_vector / sqrt(sum(direction_vector^2))
      profile_width_mnt <- 10 * ifelse(CIRC == 1, nozzle_data$HAUT1, nozzle_data$LARG)
      
      pt_extraction_amont <- st_sfc(st_point(start_node - dist_extraction_profil * direction_vector_norm), crs = st_crs(mnt_raster))
      profil_amont_base <- extract_profile_from_mnt(mnt_raster, pt_extraction_amont, direction_vector_norm, profile_width_mnt, num_points_per_profile)
      if(is.null(profil_amont_base)) stop("Échec de l'extraction du profil de base amont. Arrêt.")
      
      pt_extraction_aval <- st_sfc(st_point(end_node + dist_extraction_profil * direction_vector_norm), crs = st_crs(mnt_raster))
      profil_aval_base <- extract_profile_from_mnt(mnt_raster, pt_extraction_aval, direction_vector_norm, profile_width_mnt, num_points_per_profile)
      if(is.null(profil_aval_base)) stop("Échec de l'extraction du profil de base aval. Arrêt.")
      
      # Altitude des profils en extrapolant la pente de la buse (I)
      
      # Profil 1 (limite amont)
      z_profil_1 <- nozzle_data$Z1 + abs(I) * (x_coords[indices$p3] - x_coords[indices$p1])
      profil_1_vshifted <- shift_profile_z(profil_amont_base, z_profil_1)
      profil_1_centered <- center_profile_on_thalweg(profil_1_vshifted, target_center_y)
      profil_1 <- add_vertical_walls(profil_1_centered)
      all_profiles[[indices$p1]] <- list(name = "1", abscissa = x_coords[indices$p1], data = profil_1)
      
      # Profil 2 (juste avant la buse)
      z_profil_2 <- nozzle_data$Z1 + abs(I) * (x_coords[indices$p3] - x_coords[indices$p2])
      profil_2_vshifted <- shift_profile_z(profil_amont_base, z_profil_2)
      profil_2_centered <- center_profile_on_thalweg(profil_2_vshifted, target_center_y)
      profil_2 <- add_vertical_walls(profil_2_centered)
      all_profiles[[indices$p2]] <- list(name = "2", abscissa = x_coords[indices$p2], data = profil_2)
      
      # Profil 5 (juste après la buse)
      z_profil_5 <- nozzle_data$Z2 - abs(I) * (x_coords[indices$p5] - x_coords[indices$p4])
      profil_5_vshifted <- shift_profile_z(profil_aval_base, z_profil_5)
      profil_5_centered <- center_profile_on_thalweg(profil_5_vshifted, target_center_y)
      profil_5 <- add_vertical_walls(profil_5_centered)
      all_profiles[[indices$p5]] <- list(name = "5", abscissa = x_coords[indices$p5], data = profil_5)
      
      # Profil 6 (limite aval)
      z_profil_6 <- nozzle_data$Z2 - abs(I) * (x_coords[indices$p6] - x_coords[indices$p4])
      profil_6_vshifted <- shift_profile_z(profil_aval_base, z_profil_6)
      profil_6_centered <- center_profile_on_thalweg(profil_6_vshifted, target_center_y)
      profil_6 <- add_vertical_walls(profil_6_centered)
      all_profiles[[indices$p6]] <- list(name = "6", abscissa = x_coords[indices$p6], data = profil_6)
      
    } else {
      message("      - Mode Trapézoïdal: Génération des profils génériques...")
      
      calculated_bottom_z <- sapply(x_coords, function(x) {
        if (x <= x_nozzle_start) return(nozzle_data$Z1 + abs(I) * (x_nozzle_start - x))
        if (x > x_nozzle_start && x < x_nozzle_end) return(nozzle_data$Z1 - I * (x - x_nozzle_start))
        return(nozzle_data$Z2 - abs(I) * (x - x_nozzle_end))
      })
      
      for (k in c(indices$p1, indices$p2, indices$p5, indices$p6)) {
        points_raw <- generate_points_for_trapezoidal(
          bottom_width = first_profile_shape$bottom_width, 
          left_slope = first_profile_shape$left_slope, 
          right_slope = first_profile_shape$right_slope, 
          bottom_elevation = calculated_bottom_z[k], 
          bank_height_above_bottom = first_profile_shape$bank_height_above_bottom,
          target_center_y = target_center_y,
          bank_y_offset = 10.0
        )
        points <- add_vertical_walls(points_raw)
        all_profiles[[k]] <- list(name = as.character(k), abscissa = x_coords[k], data = points)
      }
    }
    
    points_p3 <- if(CIRC == 1) create_circular_profile(nozzle_data$HAUT1, nozzle_data$Z1, target_center_y) else create_rectangular_profile(nozzle_data$LARG, nozzle_data$HAUT1, nozzle_data$Z1, target_center_y, preissmann_slot_width)
    all_profiles[[indices$p3]] <- list(name = "3amont", abscissa = x_coords[indices$p3], data = points_p3)
    
    points_p4 <- if(CIRC == 1) create_circular_profile(nozzle_data$HAUT1, nozzle_data$Z2, target_center_y) else create_rectangular_profile(nozzle_data$LARG, nozzle_data$HAUT1, nozzle_data$Z2, target_center_y, preissmann_slot_width)
    all_profiles[[indices$p4]] <- list(name = "3aval", abscissa = x_coords[indices$p4], data = points_p4)
    
    con <- file(geo_filepath, "wt")
    message("      - Écriture ordonnée des profils dans le fichier .geo...")
    for (prof in all_profiles) {
      if (!is.null(prof) && !is.null(prof$data)) {
        write_geo_profile(con, bief_name, prof$name, prof$abscissa, prof$data)
      }
    }
    close(con)
    message("--- Fichier .geo généré avec succès. ---")
    
    #########################################################################
    # PARTIE 2: GÉNÉRATION DES FICHIERS DE LOIS (.loi)
    #########################################################################
    message("\n--- PARTIE 2 : LOIS HYDRAULIQUES (.loi) ---")
    
    duree_totale_sim <- length(debits_q) * (duree_montee + duree_palier)
    hydro_file <- file.path(current_buse_output_dir, "hydrogramme_complet.loi")
    
    con_q <- NULL
    tryCatch({
      con_q <- file(hydro_file, open = "wt")
      writeLines(c("# Hydrogramme...", "# T(s) Q(m3/s)", "S"), con_q)
      current_time <- 0.0
      writeLines(paste(sprintf("%.1f", current_time), "0.20000"), con_q)
      for (q in debits_q) { 
        current_time <- current_time + duree_montee
        writeLines(paste(sprintf("%.1f", current_time), sprintf("%.5f", q)), con_q)
        current_time <- current_time + duree_palier
        writeLines(paste(sprintf("%.1f", current_time), sprintf("%.5f", q)), con_q) 
      }
    }, finally = {
      if(!is.null(con_q)) close(con_q)
    })
    
    message("      - Génération des cotes Z aval selon la logique personnalisée...")
    cotes_z_aval <- c()
    slope=((nozzle_data$Z1-nozzle_data$Z2)/nozzle_data$LENGTH)
    
    z_start=nozzle_data$Z2-(slope*dist_p4_p6)/2
    z_phase1_end = nozzle_data$Z2
    cotes_z_aval1=seq(from = z_start, to = z_phase1_end, by = 0.5)
    z_phase2_end  <- z_phase1_end + nozzle_data$HAUT1+0.2
    cotes_z_aval2=seq(from = z_phase1_end, to = z_phase2_end, by = 0.1)
    z_phase3_end  <- z_phase1_end + nozzle_data$HAUT1 + 2
    cotes_z_aval3 <- seq(from = z_phase2_end, to = z_phase3_end, by = 0.5)
    cotes_z_aval <- unique(sort(c(cotes_z_aval1,cotes_z_aval2,cotes_z_aval3)))
    #à enlever
    z_start=nozzle_data$Z2-(slope*dist_p4_p6)
    cotes_z_aval<- seq(from = z_start, to = z_phase1_end + nozzle_data$HAUT1+0.5, by = 0.5)
    ##
    message(paste("La cote Z aval minimale sera:", sprintf("%.3f", z_start)))
    message(paste("Nombre de conditions aval générées:", length(cotes_z_aval)))
    
    for (z in cotes_z_aval) { 
      filename_z <- file.path(current_buse_output_dir, paste0("limni_Z", sprintf("%.3f", z), ".loi"))
      con_z <- NULL
      tryCatch({
        con_z <- file(filename_z, open = "wt")
        writeLines(c(paste("# Limnigramme... Z=", sprintf("%.3f", z)), "# T(s) Z(m)", "S"), con_z)
        writeLines(paste(sprintf("%.1f", 0.0), sprintf("%.5f", z)), con_z)
        writeLines(paste(sprintf("%.1f", duree_totale_sim), sprintf("%.5f", z)), con_z)
      }, finally = {
        if(!is.null(con_z)) close(con_z)
      })
    }
    message(paste(length(cotes_z_aval), "fichiers .loi générés avec succès."))
    
    #########################################################################
    # PARTIE 3: GÉNÉRATION DES FICHIERS DE PARAMÈTRES (.xcas)
    #########################################################################
    message("\n--- PARTIE 3 : PARAMÈTRES (.xcas) ---")
    limni_files <- list.files(current_buse_output_dir, pattern = "^limni_Z.*\\.loi$", full.names = TRUE)
    
    for (limni_file in limni_files) {
      current_xcas_lines <- readLines(xcas_template_filename, warn = FALSE)
      limni_id <- sub("^limni_(Z.*)\\.loi$", "\\1", basename(limni_file))
      scenario_base_name <- paste0("buse", numero_buse, "_scenario_", limni_id)
      xcas_output_filename <- file.path(current_buse_output_dir, paste0(scenario_base_name, ".xcas"))
      
      pas_stockage_value <- ceiling((duree_montee + duree_palier) / current_pas_temps)
      critere_arret_value <- 1
      
      current_xcas_lines <- gsub("<fichMotsCles>.*</fichMotsCles>", paste0("<fichMotsCles>", scenario_base_name, ".xcas</fichMotsCles>"), current_xcas_lines)
      current_xcas_lines <- gsub("<fichier>.*</fichier>", paste0("<fichier>", geo_filename_with_ext, "</fichier>"), current_xcas_lines)
      current_xcas_lines <- gsub("<abscFin>.*</abscFin>", paste0("<abscFin>", sprintf("%.3f", model_total_length), "</abscFin>"), current_xcas_lines)
      current_xcas_lines <- gsub("<absFinZone>.*</absFinZone>", paste0("<absFinZone>", sprintf("%.3f", model_total_length), "</absFinZone>"), current_xcas_lines)
      current_xcas_lines <- gsub("<pasTemps>.*</pasTemps>", paste0("<pasTemps>", sprintf("%.1f", current_pas_temps), "</pasTemps>"), current_xcas_lines)
      current_xcas_lines <- gsub("<tempsMax>.*</tempsMax>", paste0("<tempsMax>", sprintf("%.1f", duree_totale_sim), "</tempsMax>"), current_xcas_lines)
      current_xcas_lines <- gsub("<critereArret>.*</critereArret>", paste0("<critereArret>", critere_arret_value, "</critereArret>"), current_xcas_lines)
      current_xcas_lines <- gsub("<pasStock>.*</pasStock>", paste0("<pasStock>", sprintf("%.0f", pas_stockage_value), "</pasStock>"), current_xcas_lines)
      current_xcas_lines <- gsub("<fichResultat>.*</fichResultat>", paste0("<fichResultat>", scenario_base_name, "_ecr.opt</fichResultat>"), current_xcas_lines)
      current_xcas_lines <- gsub("<fichListing>.*</fichListing>", paste0("<fichListing>", scenario_base_name, ".lis</fichListing>"), current_xcas_lines)
      current_xcas_lines <- gsub("<valeursPas>.*</valeursPas>",paste0("<valeursPas>",sprintf("%.2f", val_pas_planimetrage),"</valeursPas>"), current_xcas_lines)
      current_xcas_lines <- gsub("<numDerProf>.*</numDerProf>", paste0("<numDerProf>", num_profiles_total, "</numDerProf>"), current_xcas_lines)
      
      pas_maillage_str <- paste(current_pas_maillage, collapse = " ")
      current_xcas_lines <- gsub("<num1erProfPlage>.*</num1erProfPlage>", "<num1erProfPlage>1 3 5</num1erProfPlage>", current_xcas_lines)
      current_xcas_lines <- gsub("<numDerProfPlage>.*</numDerProfPlage>", paste0("<numDerProfPlage>2 4 ", num_profiles_total, "</numDerProfPlage>"), current_xcas_lines)
      current_xcas_lines <- gsub("<pasEspacePlage>.*</pasEspacePlage>", paste0("<pasEspacePlage>", pas_maillage_str, "</pasEspacePlage>"), current_xcas_lines)
      
      start_frottement_idx <- grep("<frottement>", current_xcas_lines)[1]
      end_frottement_idx <- grep("</frottement>", current_xcas_lines)[1]
      if (!is.na(start_frottement_idx) && !is.na(end_frottement_idx)) {
        abs_deb_zone_str <- paste(sprintf("%.3f", x_coords[c(1, 3, 5)]), collapse = " ")
        abs_fin_zone_str <- paste(sprintf("%.3f", x_coords[c(2, 4, 6)]), collapse = " ")
        strickler_zones_str <- paste(sprintf("%.1f", strickler_zones), collapse = " ")
        frottement_block <- c("      <frottement>", "        <loi>1</loi>", "        <nbZone>3</nbZone>", "        <numBranche>1 1 1</numBranche>", paste0("        <absDebZone>", abs_deb_zone_str, "</absDebZone>"), paste0("        <absFinZone>", abs_fin_zone_str, "</absFinZone>"), paste0("        <coefLitMin>", strickler_zones_str, "</coefLitMin>"), paste0("        <coefLitMaj>", strickler_zones_str, "</coefLitMaj>"), "      </frottement>")
        current_xcas_lines <- c(current_xcas_lines[1:(start_frottement_idx - 1)], frottement_block, current_xcas_lines[(end_frottement_idx + 1):length(current_xcas_lines)])
      }
      
      start_index <- grep("<parametresLoisHydrauliques>", current_xcas_lines)[1]
      end_index <- grep("</parametresLoisHydrauliques>", current_xcas_lines)[1]
      if (!is.na(start_index) && !is.na(end_index)) {
        lois_lines <- c("  <parametresLoisHydrauliques>","    <nb>2</nb>","    <lois>","      <structureParametresLoi>","        <nom>hydro_amont</nom>","        <type>1</type>","        <donnees>","          <modeEntree>1</modeEntree>",paste0("          <fichier>", basename(hydro_file), "</fichier>"),"        </donnees>","      </structureParametresLoi>","      <structureParametresLoi>",paste0("        <nom>limni_", limni_id, "</nom>"),"        <type>2</type>","        <donnees>","          <modeEntree>1</modeEntree>",paste0("          <fichier>", basename(limni_file), "</fichier>"),"        </donnees>","      </structureParametresLoi>","    </lois>","  </parametresLoisHydrauliques>")
        current_xcas_lines <- c(current_xcas_lines[1:(start_index-1)], lois_lines, current_xcas_lines[(end_index+1):length(current_xcas_lines)])
      }
      
      con <- NULL
      tryCatch({
        con <- file(xcas_output_filename, "w")
        writeLines(current_xcas_lines, con)
      }, finally = {
        if(!is.null(con)) close(con)
      })
    }
    message("--- Fichiers .xcas générés avec succès. ---")
    
    # PARTIE 4
    message("\n--- PARTIE 4 : LANCEMENT DES SIMULATIONS ---")
    xcas_files <- dir_ls(current_buse_output_dir, regexp = "\\.xcas$", recurse = FALSE)
    registerDoParallel(cores = num_cores_sim)
    foreach( xcas_file_path = xcas_files, .combine = 'c', .errorhandling = 'pass') %dopar% {
      library(fs)
      xcas_filename <- path_file(xcas_file_path)
      xcas_basename <- path_ext_remove(xcas_filename)
      temp_sim_dir <- path(current_buse_output_dir, paste0("temp_", xcas_basename))
      results_dir <- path(current_buse_output_dir, "results_mascaret")
      
      final_opt_path <- path(results_dir, paste0(xcas_basename, "_ecr.opt"))
      
      if(!file_exists(final_opt_path) || file_info(final_opt_path)$size == 0) {
        dir_create(temp_sim_dir)
        file_copy(xcas_file_path, path(temp_sim_dir, xcas_filename), overwrite = TRUE)
        file_copy(path(current_buse_output_dir, geo_filename_with_ext), temp_sim_dir, overwrite = TRUE)
        file_copy(path(current_buse_output_dir, "hydrogramme_complet.loi"), temp_sim_dir, overwrite = TRUE)
        limni_id_for_copy <- sub(paste0("buse", numero_buse, "_scenario_"), "", xcas_basename)
        limni_to_copy <- path(current_buse_output_dir, paste0("limni_", limni_id_for_copy, ".loi"))
        if(file_exists(limni_to_copy)) file_copy(limni_to_copy, temp_sim_dir, overwrite = TRUE)
        
        batch_content <- sprintf('@echo off\nCALL "%s"\ncd /D "%%~dp0"\npython "%%HOMETEL%%\\scripts\\python3\\mascaret.py" "%s"\nexit /b %%ERRORLEVEL%%', telemac_env_script, xcas_filename)
        batch_file_path <- path(temp_sim_dir, "run_simulation.bat")
        writeLines(batch_content, batch_file_path)
        
        system2("cmd", args = c("/c", path_abs(batch_file_path)), stdout = TRUE, stderr = TRUE)
      }
    }
    stopImplicitCluster()
    message("--- Lancement des simulations terminé. ---")
    
    # NOUVELLE PARTIE 4.5: Copie des résultats après la fin de tous les calculs
    message("\n--- PARTIE 4.5 : COLLECTE DES RÉSULTATS ---")
    results_dir <- path(current_buse_output_dir, "results_mascaret")
    dir_create(results_dir)
    temp_dirs <- dir_ls(current_buse_output_dir, regexp = "temp_buse", type = "directory")
    
    for(temp_dir in temp_dirs) {
      opt_files_in_temp <- dir_ls(temp_dir, regexp = "\\.(opt|lis)$")
      if(length(opt_files_in_temp) > 0) {
        file_copy(opt_files_in_temp, results_dir, overwrite = TRUE)
      }
    }
    message("--- Collecte des fichiers .opt terminée. ---")
    
    
    # PARTIE 5
    message("\n--- PARTIE 5 : LECTURE DES RÉSULTATS ET CRÉATION DE L'ABAQUE ---")
    opt_dir <- file.path(current_buse_output_dir, "results_mascaret")
    
    ABSCISSA_BUSE_ENTREE_TARGET <- x_coords[3]
    ABSCISSA_BUSE_SORTIE_TARGET <- x_coords[4]
    
    opt_files <- list.files(opt_dir, pattern = "\\.opt$", full.names = TRUE)
    results_list <- list()
    
    for (opt_file in opt_files) {
      opt_content_lines <- readLines(opt_file, warn = FALSE)
      resultats_start_line_index <- grep("[resultats]", opt_content_lines, fixed = TRUE)
      if (length(resultats_start_line_index) == 0) next
      opt_data_text <- opt_content_lines[(resultats_start_line_index[1] + 1):length(opt_content_lines)]
      if(length(opt_data_text) == 0 || all(nchar(trimws(opt_data_text)) == 0)) next
      
      opt_data <- tryCatch({ fread(text = opt_data_text, sep = ";", strip.white=TRUE, header = FALSE, colClasses = "numeric", fill = TRUE, select = c(1, 4, 6, 11, 12, 14), col.names = c("Time", "Abscissa", "S_value", "Froude", "Velocity", "DebitTotal")) }, error = function(e) { NULL })
      if(is.null(opt_data) || nrow(opt_data) == 0) next
      
      plateau_end_times <- (1:length(debits_q)) * (duree_montee + duree_palier)
      
      for(time_step in plateau_end_times){
        time_to_extract <- time_step - current_pas_temps
        if(time_to_extract < 0 || time_to_extract > max(opt_data$Time)) next
        
        steady_state_data <- opt_data[abs(opt_data$Time - time_to_extract) < 1e-3, ]
        if(nrow(steady_state_data) == 0) next
        
        entree_buse_row <- steady_state_data[which.min(abs(steady_state_data$Abscissa - ABSCISSA_BUSE_ENTREE_TARGET)), ]
        sortie_buse_row <- steady_state_data[which.min(abs(steady_state_data$Abscissa - ABSCISSA_BUSE_SORTIE_TARGET)), ]
        
        if(nrow(entree_buse_row) == 1 && nrow(sortie_buse_row) == 1){
          avg_velocity <- (entree_buse_row$Velocity + sortie_buse_row$Velocity) / 2
          
          results_list[[length(results_list) + 1]] <- data.frame(
            Debit = entree_buse_row$DebitTotal, 
            S1 = entree_buse_row$S_value, 
            S2 = sortie_buse_row$S_value,
            Froude_amont = entree_buse_row$Froude,
            Avg_Velocity = avg_velocity
          )
        }
      }
    }
    
    results_for_file <- data.frame()
    all_results_df <- data.frame()
    results_df_sorted <- data.frame()
    
    if (length(results_list) > 0) {
      all_results_df <- do.call(rbind, results_list)
      s1_max_threshold <- nozzle_data$Z1 + nozzle_data$HAUT1 + 10
      
      all_results_filtered <- all_results_df[
        all_results_df$S1 <= s1_max_threshold & 
          all_results_df$Froude_amont <= 5 &
          # all_results_df$Avg_Velocity < 7.0 &
          (all_results_df$S1 - all_results_df$S2) >= 0.05,
      ]
      if(nrow(all_results_filtered) > 0) {
        results_df_unique <- unique(all_results_filtered)
        results_df_sorted <- results_df_unique[order(results_df_unique$Debit, results_df_unique$S1), ]
        
        final_points_list <- lapply(split(results_df_sorted, results_df_sorted$Debit), function(df_debit) {
          torrential_points <- df_debit %>% filter(Froude_amont >= 1)
          if (nrow(torrential_points) > 0) return(torrential_points)
          good_fluvial_points <- df_debit %>% filter(Froude_amont < 1 & S1 < (nozzle_data$Z1 + nozzle_data$HAUT1) & S2 < (nozzle_data$Z2 + nozzle_data$HAUT1))
          if (nrow(good_fluvial_points) > 0) return(good_fluvial_points)
          return(df_debit %>% filter(Froude_amont < 1))
        })
        
        results_for_file <- bind_rows(final_points_list) %>% unique() %>% arrange(Debit, S1)
        if(length(which(results_for_file$Froude_amont>=1))>0){
          max_debit_torrential=max(results_for_file$Debit[which(results_for_file$Froude_amont>=1)])
          max_S1_torrentiel=max(results_for_file$S1[which(results_for_file$Debit==max_debit_torrential)])
          max_S2_torrentiel=max(results_for_file$S2[which(results_for_file$Debit==max_debit_torrential)])
          
          points_fluviaux1=which(results_for_file$Debit<max_debit_torrential & (results_for_file$S1 >max_S1_torrentiel | results_for_file$S2 >max_S2_torrentiel ) )
          points_fluviaux2=which(results_for_file$Debit>max_debit_torrential & results_for_file$S2 >nozzle_data$Z2 + nozzle_data$HAUT1 + 0.05  )
          if(length(points_fluviaux1)>0 | length(points_fluviaux2)>0){
            results_for_file=results_for_file[-c(points_fluviaux1,points_fluviaux2),]
            results_for_file <- results_for_file %>% unique() %>% arrange(Debit, S1)
          }
        }
        
        # --- NETTOYAGE ET STRICTE MONOTONIE HYDRAULIQUE (Q et S1) ---
        if (nrow(results_for_file) > 0) {
          
          # On trie impérativement par S1 (hauteur d'eau) en premier lieu pour s'assurer
          # que la courbe part toujours du point le plus bas (le radier) sans sauter de points à cause du tri par débit
          results_for_file <- results_for_file[order(results_for_file$S1, results_for_file$Debit), ]
          
          # Filtrage ultra-simple pour garantir la stricte croissance de S1 requise par MASCARET
          if (nrow(results_for_file) > 1) {
            indices_valides <- c(1)
            derniere_cote <- results_for_file$S1[1]
            
            for (idx_pt in 2:nrow(results_for_file)) {
              if (results_for_file$S1[idx_pt] > derniere_cote) {
                indices_valides <- c(indices_valides, idx_pt)
                derniere_cote <- results_for_file$S1[idx_pt]
              }
            }
            results_for_file <- results_for_file[indices_valides, ]
          }
        }
        
        output_txt_file_S_partiel <- file.path(current_buse_output_dir, "abaque_partiel.txt")
        if (nrow(results_for_file) > 0) {
          file_conn_partiel <- file(output_txt_file_S_partiel, "w")
          writeLines(c(as.character(numero_buse), as.character(nrow(results_for_file))), file_conn_partiel)
          cat(paste(sprintf("%.2f", results_for_file$Debit), collapse = ","), "\n", file = file_conn_partiel)
          cat(paste(sprintf("%.2f", results_for_file$S1), collapse = ","), "\n", file = file_conn_partiel)
          cat(paste(sprintf("%.2f", results_for_file$S2), collapse = ","), "\n", file = file_conn_partiel)
          close(file_conn_partiel)
        }
        
        output_txt_file_QS1_partiel <- file.path(current_buse_output_dir, "abaque_partiel_QS1.txt")
        if (nrow(results_for_file) > 0) {
          results_QS1_unique <- results_for_file %>% dplyr::select(Debit, S1) %>% distinct() %>% arrange(Debit, S1)
          file_conn_QS1 <- file(output_txt_file_QS1_partiel, "w")
          writeLines(c(as.character(numero_buse), as.character(nrow(results_QS1_unique))), file_conn_QS1)
          cat(paste(sprintf("%.2f", results_QS1_unique$Debit), collapse = ","), "\n", file = file_conn_QS1)
          cat(paste(sprintf("%.2f", results_QS1_unique$S1), collapse = ","), "\n", file = file_conn_QS1)
          close(file_conn_QS1)
        }
      }
    }
    
    if(generate_long_profile_plots) {
      message("\n--- GÉNÉRATION DES PROFILS EN LONG ---")
      for (opt_file in opt_files) {
        scenario_name <- sub("_ecr\\.opt$", "", basename(opt_file)); opt_content_lines <- readLines(opt_file, warn = FALSE); resultats_start_line_index <- grep("[resultats]", opt_content_lines, fixed = TRUE); if (length(resultats_start_line_index) == 0) next
        opt_data_full <- tryCatch({ fread(text = opt_content_lines[(resultats_start_line_index[1] + 1):length(opt_content_lines)], sep = ";", strip.white=TRUE, header = FALSE, colClasses = "numeric", fill = TRUE, select = c(1, 4, 5, 6, 14), col.names = c("Time", "Abscissa", "Z_fond", "Z_eau", "DebitTotal")) }, error = function(e) { NULL })
        if(is.null(opt_data_full) || nrow(opt_data_full) == 0) next
        fig_pl <- plot_ly(); profil_fond <- opt_data_full[abs(Time - min(Time)) < 1e-3, ]; fig_pl <- add_trace(fig_pl, data = profil_fond, x = ~Abscissa, y = ~Z_fond, type = 'scatter', mode = 'lines', line = list(color = 'black', width = 3), name = 'Fond')
        for (time_p in ((1:length(debits_q)) * (duree_montee + duree_palier) - current_pas_temps)) { if (time_p > 0 && time_p <= max(opt_data_full$Time)) { steady_data <- opt_data_full[abs(opt_data_full$Time - time_p) < 1e-3, ]; if (nrow(steady_data) > 0) fig_pl <- add_trace(fig_pl, data = steady_data, x = ~Abscissa, y = ~Z_eau, type = 'scatter', mode = 'lines', name = paste('Q =', round(steady_data$DebitTotal[1], 2), 'm³/s')) } }
        y_range <- range(c(profil_fond$Z_fond, opt_data_full$Z_eau), na.rm = TRUE); fig_pl <- fig_pl %>% layout(title = scenario_name, xaxis = list(title = "Abscisse"), yaxis = list(title = "Altitude", range = y_range), shapes = list(list(type = 'line', x0 = x_coords[indices$p3], x1 = x_coords[indices$p3], y0 = y_range[1], y1 = y_range[2], line = list(color = 'grey', dash = 'dash')), list(type = 'line', x0 = x_coords[indices$p4], x1 = x_coords[indices$p4], y0 = y_range[1], y1 = y_range[2], line = list(color = 'grey', dash = 'dash')), list(type = 'line', x0 = x_coords[indices$p3], x1 = x_coords[indices$p4], y0 = nozzle_data$Z1 + nozzle_data$HAUT1, y1 = nozzle_data$Z2 + nozzle_data$HAUT1, line = list(color = 'red', dash = 'longdash'))))
        saveWidget(fig_pl, file = file.path(opt_dir, paste0("profil_en_long_", scenario_name, ".html")), selfcontained = TRUE)
      }
    }
    
    
    if(generate_abaque_plots) {
      message("\n--- PARTIE 6 : GÉNÉRATION DES GRAPHIQUES D'ABAQUE ---")
      
      # Utiliser `all_results_df` pour définir les points à estomper
      points_to_keep <- results_for_file
      points_to_fade <- dplyr::anti_join(all_results_df, points_to_keep, by = c("Debit", "S1", "S2", "Froude_amont", "Avg_Velocity"))
      
      if (nrow(points_to_keep) > 0 || nrow(points_to_fade) > 0) {
        
        # Préparation des données pour les points principaux
        results_df_graph <- points_to_keep
        if(nrow(results_df_graph) > 0) {
          colnames(results_df_graph) <- c("Debit", "S1_Charge_Amont", "S2_Charge_Aval", "Froude_amont", "Avg_Velocity")
          results_df_graph$Froude_Condition <- cut(results_df_graph$Froude_amont, 
                                                   breaks = c(-Inf, 0.99, 1.01, Inf), 
                                                   labels = c("Fluvial (Fr < 1)", "Critique (Fr = 1)", "Torrentiel (Fr > 1)"))
        }
        
        # --- Graphique 3D ---
        title_3d <- paste("Relation Débit-Charges S1/S2 - Buse", numero_buse, "<br>", title_details_str)
        fig_3d_S <- plot_ly()
        
        if(nrow(points_to_fade) > 0) {
          fig_3d_S <- fig_3d_S %>% add_trace(
            data = points_to_fade, x=~Debit, y=~S2, z=~S1,
            type='scatter3d', mode='markers',
            marker=list(size=3, color='rgba(211, 211, 211, 0.5)'), # Gris clair, semi-transparent
            name = "Points exclus",
            hoverinfo = "text",
            text = ~paste('Exclu<br>Débit:', sprintf("%.2f", Debit), '<br>S1:', sprintf("%.2f", S1), '<br>S2:', sprintf("%.2f", S2), '<br>Froude:', sprintf("%.2f", Froude_amont))
          )
        }
        
        if(nrow(results_df_graph) > 0) {
          fig_3d_S <- fig_3d_S %>% add_trace(
            data = results_df_graph, x=~Debit, y=~S2_Charge_Aval, z=~S1_Charge_Amont,
            type='scatter3d', mode='markers',
            color=~S1_Charge_Amont, colorscale='Viridis',
            symbol=~Froude_Condition, symbols=c('circle','square','triangle-up'),
            marker=list(size=4),
            customdata=~Froude_amont,
            name = "Points de l'abaque",
            hovertemplate=paste('<b>Débit:</b> %{x:.2f} m³/s<br>',
                                '<b>S2 (Aval):</b> %{y:.2f} m<br>',
                                '<b>S1 (Amont):</b> %{z:.2f} m<br>',
                                '<b>Froude (Amont):</b> %{customdata:.2f}<br>',
                                '<extra></extra>')
          )
        }
        
        fig_3d_S <- fig_3d_S %>% layout(title=title_3d,
                                        legend=list(title=list(text='<b>Régime Froude</b>')),
                                        scene=list(xaxis=list(title="Débit (m³/s)"),
                                                   yaxis=list(title="S2 Charge Aval (m)"),
                                                   zaxis=list(title="S1 Charge Amont (m)")))
        
        # --- Graphique 2D ---
        title_2d <- paste("Relation S1-S2 - Buse", numero_buse, "<br>", title_details_str)
        fig_2d_S <- plot_ly()
        
        if(nrow(points_to_fade) > 0) {
          fig_2d_S <- fig_2d_S %>% add_trace(
            data = points_to_fade, x=~S1, y=~S2,
            type='scatter', mode='markers',
            marker=list(size=5, color='rgba(211, 211, 211, 0.6)'), # Gris clair, semi-transparent
            name = "Points exclus",
            hoverinfo = "text",
            text = ~paste('Exclu<br>Débit:', sprintf("%.2f", Debit), '<br>S1:', sprintf("%.2f", S1), '<br>S2:', sprintf("%.2f", S2), '<br>Froude:', sprintf("%.2f", Froude_amont))
          )
        }
        
        if(nrow(results_df_graph) > 0) {
          fig_2d_S <- fig_2d_S %>% add_trace(
            data = results_df_graph, x=~S1_Charge_Amont, y=~S2_Charge_Aval,
            type='scatter', mode='markers',
            color=~Debit, colorscale='Plasma',
            symbol=~Froude_Condition, symbols=c('circle','square','triangle-up'),
            marker=list(size=6),
            customdata=~Froude_amont,
            name = "Points de l'abaque",
            hovertemplate=paste('<b>S1 (Amont):</b> %{x:.2f} m<br>',
                                '<b>S2 (Aval):</b> %{y:.2f} m<br>',
                                '<b>Débit:</b> %{marker.color:.2f} m³/s<br>',
                                '<b>Froude:</b> %{customdata:.2f}<br>',
                                '<extra></extra>')
          )
          
          shapes_list <- list(
            list(type = 'line', x0 = nozzle_data$Z1, x1 = nozzle_data$Z1, y0 = min(results_df_graph$S2_Charge_Aval), y1 = max(results_df_graph$S2_Charge_Aval), line = list(color = 'blue', dash = 'longdash', width = 2)),
            list(type = 'line', x0 = nozzle_data$Z1 + nozzle_data$HAUT1, x1 = nozzle_data$Z1 + nozzle_data$HAUT1, y0 = min(results_df_graph$S2_Charge_Aval), y1 = max(results_df_graph$S2_Charge_Aval), line = list(color = 'red', dash = 'longdash', width = 2)),
            list(type = 'line', y0 = nozzle_data$Z2, y1 = nozzle_data$Z2, x0 = min(results_df_graph$S1_Charge_Amont), x1 = max(results_df_graph$S1_Charge_Amont), line = list(color = 'blue', dash = 'longdash', width = 2)),
            list(type = 'line', y0 = nozzle_data$Z2 + nozzle_data$HAUT1, y1 = nozzle_data$Z2 + nozzle_data$HAUT1, x0 = min(results_df_graph$S1_Charge_Amont), x1 = max(results_df_graph$S1_Charge_Amont), line = list(color = 'red', dash = 'longdash', width = 2))
          )
          fig_2d_S <- fig_2d_S %>% layout(shapes = shapes_list)
        }
        
        fig_2d_S <- fig_2d_S %>% layout(
          title=title_2d,
          xaxis=list(title="S1 Charge Amont (m)"),
          yaxis=list(title="S2 Charge Aval (m)"),
          legend=list(title=list(text='<b>Régime Froude</b>'))
        )
        
        htmlwidgets::saveWidget(fig_3d_S, file.path(current_buse_output_dir, "graph_abaque_S_3d.html"), selfcontained = TRUE)
        htmlwidgets::saveWidget(fig_2d_S, file.path(current_buse_output_dir, "graph_abaque_S_2d.html"), selfcontained = TRUE)
        message("Graphiques d'abaque 2D et 3D (incluant les points exclus) exportés.")
        
      } else {
        message("Aucune donnée à visualiser, les graphiques d'abaque n'ont pas été générés pour cette buse.")
      }
    }
    
    # --- BLOC DE NETTOYAGE SÉLECTIF (AJOUTÉ v123) ---
    if (cleanup_calc_files) {
      message(paste("      - Nettoyage sélectif pour la buse", numero_buse))
      
      # 1. Identifier tous les dossiers temporaires temp_...
      tdirs <- fs::dir_ls(current_buse_output_dir, regexp = "temp_", type = "directory")
      if (length(tdirs) > 1) {
        # Garder uniquement le premier dossier pour vérification, supprimer les autres
        fs::dir_delete(tdirs[-1]) 
      }
      
      # 2. Supprimer le dossier global results_mascaret (doublon inutile une fois les données extraites)
      res_fold <- file.path(current_buse_output_dir, "results_mascaret")
      if(fs::dir_exists(res_fold)) fs::dir_delete(res_fold)
      
      # 3. Supprimer les fichiers d'entrée à la racine (déjà sauvegardés dans le dossier temp conservé)
      root_files <- fs::dir_ls(current_buse_output_dir, regexp = "\\.(xcas|geo|loi|lis)$")
      if(length(root_files) > 0) fs::file_delete(root_files)
    }
  } 
  
  #########################################################################
  # DÉMARRAGE DE LA BOUCLE PRINCIPALE
  #########################################################################
  if (ncore_buse <= 1) {
    message("--- DÉMARRAGE DU TRAITEMENT SÉQUENTIEL ---")
    for (i in 1:nrow(busel_data)) {
      abaques_check_file <- file.path(dsnlayerC, "abaques2.txt")
      if (file.exists(abaques_check_file) && file.info(abaques_check_file)$size > 0) {
        message(paste(">>> BUSE", abaques_check_file, ": DÉJÀ TRAITÉE (Fichier abaque trouvé). PASSAGE À LA SUIVANTE. <<<"))
        next
      }
      process_buse(i, busel_data, main_output_dir, telemac_env_script, xcas_template_filename, use_mnt_for_profiles, mnt_path, dist_amont_limite, dist_p4_p6, num_points_per_profile, dist_extraction_profil, first_profile_shape, duree_palier, duree_montee, num_cores_sim, strickler_zones_base, val_pas_planimetrage, g, pas_maillage_court, pas_temps_court, pas_maillage_moyen, pas_temps_moyen, pas_maillage_long, pas_temps_long, preissmann_slot_width, generate_long_profile_plots, generate_abaque_plots, cleanup_calc_files)
    }
  } else {
    message(paste("--- DÉMARRAGE DU TRAITEMENT PARALLÈLE SUR", ncore_buse, "CŒURS ---"))
    registerDoParallel(cores = ncore_buse)
    foreach(i = 1:nrow(busel_data)) %dopar% { 
      process_buse(i, busel_data, main_output_dir, telemac_env_script, xcas_template_filename, use_mnt_for_profiles, mnt_path, dist_amont_limite, dist_p4_p6, num_points_per_profile, dist_extraction_profil, first_profile_shape, duree_palier, duree_montee, num_cores_sim, strickler_zones_base, val_pas_planimetrage, g, pas_maillage_court, pas_temps_court, pas_maillage_moyen, pas_temps_moyen, pas_maillage_long, pas_temps_long, preissmann_slot_width, generate_long_profile_plots, generate_abaque_plots, cleanup_calc_files) 
    }
    stopImplicitCluster()
  }
  
  #########################################################################
  # PARTIE 7: ASSEMBLAGE FINAL DE L'ABAQUE
  #########################################################################
  message("\n--- PARTIE 7 : ASSEMBLAGE DU FICHIER D'ABAQUE FINAL ---")
  output_txt_file_S_global <- file.path(main_output_dir, "resultats_buse_format_abaqueSQ.txt")
  file_conn_main <- file(output_txt_file_S_global, "w")
  writeLines("#abaqueSQ", file_conn_main)
  output_txt_file_QS1_global <- file.path(main_output_dir, "abaques.txt")
  file_conn_qs1 <- file(output_txt_file_QS1_global, "w")
  writeLines("#abaqueQS1", file_conn_qs1)
  
  for (i in 1:nrow(busel_data)) {
    numero_buse <- busel_data$id[i]
    current_buse_output_dir <- file.path(main_output_dir, paste0("buse_", numero_buse))
    
    p1 <- file.path(current_buse_output_dir, "abaque_partiel.txt")
    if (file.exists(p1)) writeLines(readLines(p1), file_conn_main)
    
    p2 <- file.path(current_buse_output_dir, "abaque_partiel_QS1.txt")
    if (file.exists(p2)) writeLines(readLines(p2), file_conn_qs1)
  }
  
  close(file_conn_main); close(file_conn_qs1)
  message("\n--- WORKFLOW COMPLET TERMINÉ ---")
  file.copy(from = file.path(main_output_dir, "abaques.txt"),to = dsnlayerC,overwrite = TRUE,recursive = TRUE)
  # 1. Lire toutes les lignes du fichier d'origine
  lignes <- readLines(file.path(dsnlayerC, "abaques.txt"))
  
  # 2. Nettoyer les éventuelles lignes vides à la fin du fichier
  lignes <- lignes[lignes != ""]
  
  # 3. Calculer le nombre de buses
  # On soustrait 1 (pour la ligne de commentaire) avant de diviser par 4
  n_buses <- floor((length(lignes) - 1) / 4)
  
  # 4. Générer les indices des IDs
  # On commence à la ligne 2, et on fait des sauts de 4
  indices_id <- seq(2, by = 4, length.out = n_buses)
  
  # 5. Remplacer les lignes d'identifiants par la nouvelle numérotation
  lignes[indices_id] <- 1:n_buses
  
  # 6. Sauvegarder le résultat en écrasant le fichier
  writeLines(as.character(lignes), file.path(dsnlayerC, "abaques.txt"))
  # Script R pour lisser les abaques Telemac-2D
  # Objectif : Garantir une croissance stricte des hauteurs d'eau (S1) 
  # pour éviter les divisions par zéro dans DISCHARGE_INTERPOL.
  
  # Noms des fichiers (à adapter si besoin)
  file_in <- file.path(dsnlayerC, "abaques.txt")
  file_out <- file.path(dsnlayerC, "abaques2.txt")
  
  # Lecture du fichier
  lines <- readLines(file_in, warn = FALSE)
  # Nettoyage des lignes vides éventuelles
  lines <- lines[trimws(lines) != ""]
  
  # On conserve l'entête (ex: #abaqueQS1)
  output_lines <- c(lines[1])
  
  idx <- 2
  while (idx <= length(lines)) {
    # Extraction du bloc de 4 lignes
    abaque_id <- trimws(lines[idx])
    n_pts_old <- as.integer(trimws(lines[idx + 1]))
    q_str     <- lines[idx + 2]
    s1_str    <- lines[idx + 3]
    
    # Conversion des lignes Q et S1 en vecteurs numériques
    q_vals  <- as.numeric(unlist(strsplit(q_str, ",")))
    s1_vals <- as.numeric(unlist(strsplit(s1_str, ",")))
    
    # Construction du dataframe pour nettoyage
    df_abaque <- data.frame(Debit = q_vals, S1 = s1_vals)
    
    # Filtrage ultra-simple : on parcourt les points dans leur ordre naturel
    # et on ne garde que ceux où la cote d'eau (S1) augmente strictement.
    # On ne trie pas, et on ne modifie JAMAIS les valeurs de débit !
    if (nrow(df_abaque) > 1) {
      indices_valides <- c(1) # On garde toujours le 1er point (fond)
      derniere_cote <- df_abaque$S1[1]
      
      for (idx_pt in 2:nrow(df_abaque)) {
        if (df_abaque$S1[idx_pt] > derniere_cote) {
          indices_valides <- c(indices_valides, idx_pt)
          derniere_cote <- df_abaque$S1[idx_pt]
        }
      }
      df_abaque <- df_abaque[indices_valides, ]
    }
    
    n_pts  <- nrow(df_abaque)
    q_out  <- paste(sprintf("%.2f", df_abaque$Debit), collapse = ",")
    s1_out <- paste(sprintf("%.2f", df_abaque$S1), collapse = ",")
    
    # Stockage des 4 lignes du bloc corrigé
    output_lines <- c(output_lines, abaque_id, as.character(n_pts), q_out, s1_out)
    
    # On passe à l'abaque suivant
    idx <- idx + 4
  }
  
  # Écriture du nouveau fichier
  writeLines(output_lines, file_out)
  cat("Traitement terminé avec succès !\nLe fichier corrigé est :", file_out, "\n")
}
abacus=function(){
  library(sf)
  library(terra)
  library(dplyr)
  library(fs)
  library(stringr)
  library(foreach)
  library(doParallel)
  library(data.table)
  library(plotly)
  library(htmlwidgets)
  
  #########################################################################
  # PARTIE 0: PARAMÈTRES UTILISATEUR
  # Modifiez les valeurs dans cette section pour configurer votre simulation.
  #########################################################################
  # --- 0.1. Configuration des chemins et fichiers ---
  # chem_routine <- "C:/Users/CartoNationale/Desktop/Nabil"
  
  main_output_dir <- file.path(dsnlayerC, "abaques") 
  telemac_env_script <- "C:/TELEMAC/V8P4/configs/pysource.bat"
  xcas_template_filename <- file.path(chem_routine,"C2D",Doss_Base, "masc.xcas.txt")
  
  # --- 0.2. Paramètres de parallélisation ---
  ncore_buse <- ncore_buse_abaques # Nombre de cœurs pour la boucle sur les buses. 1 = séquentiel, >=2 = parallèle
  num_cores_sim <- num_cores_sim_abaques
  # Nombre de cœurs pour les simulations d'une MÊME buse
  
  # --- 0.3. Contrôle de la géométrie --- 
  use_mnt_for_profiles <- FALSE # Mettre à TRUE pour extraire du MNT, FALSE pour utiliser des profils trapézoïdaux
  
  # --- Chemins pour le mode MNT ---
  shapefile_path <- file.path(dsnlayerC, "OuvHydrau_Select.shp") 
  mnt_path <- file.path(dsnlayerC, "MNTFinal.gpkg") # <--- CHEMIN VERS VOTRE MNT
  
  # --- 0.4. Paramètres de la géométrie ---
  dist_amont_limite <- 1000.0
  dist_p4_p6 <- 2000 # Distance entre la sortie de la buse (corrigée à p5) et la limite aval (p6)
  num_points_per_profile <- 101
  dist_extraction_profil <- 3.0 
  preissmann_slot_width <- 0.01 # Largeur de la fente de Preissmann en mètres
  
  first_profile_shape <- list(
    type = "trapezoidal", bottom_width = 3, left_slope = 1/1.5,
    right_slope = 1/1.5, bank_height_above_bottom = 40
  )
  
  # --- 0.5. Paramètres temporels et hydrauliques ---
  duree_palier <- 400
  duree_montee <- 50
  strickler_zones_base <- c(20.0, 20.0, 20.0) 
  val_pas_planimetrage <- 0.02
  g <- 9.81
  
  # --- 0.6. Paramètres de maillage et de temps de simulation adaptatifs ---
  # Pour buses COURTES (< 20m)
  pas_maillage_court <- c(1, 0.5, 1)
  pas_temps_court <- 0.5
  duree_palier_court <- 20
  duree_montee_court <- 10
  
  # Pour buses MOYENNES (20m à 100m)
  pas_maillage_moyen <- c(1, 1, 1)
  pas_temps_moyen <- 1
  duree_palier_moyen <- 100
  duree_montee_moyen <- 20
  # Pour buses LONGUES (> 100m)
  pas_maillage_long <- c(2, 2, 2)
  pas_temps_long <- 2
  duree_palier_long <- 400
  duree_montee_long <- 50
  # --- 0.7. Paramètres de sortie (NOUVEAU) ---
  generate_long_profile_plots <- TRUE # Mettre à TRUE pour générer les graphiques de profil en long
  generate_abaque_plots <- TRUE       # Mettre à TRUE pour générer les graphiques d'abaque 2D/3D
  cleanup_calc_files <- FALSE           # <--- NOUVEAU: TRUE pour garder 1 dossier temp et supprimer le reste
  
  
  #########################################################################
  # LECTURE DES DONNÉES SPATIALES ET INITIALISATION
  #########################################################################
  message("--- LECTURE DES DONNÉES SPATIALES ---")
  if (!file.exists(shapefile_path)) stop("Le fichier shapefile n'a pas été trouvé: ", shapefile_path)
  busel_data <- st_read(shapefile_path, quiet = TRUE)
  message(paste("Lecture de", nrow(busel_data), "buse(s) depuis le shapefile."))
  
  mnt_raster <- NULL # Initialisation à NULL
  if(use_mnt_for_profiles) {
    if (!file.exists(mnt_path)) stop("Le fichier MNT n'a pas été trouvé: ", mnt_path)
    message("Chargement du MNT pour vérification CRS...")
    temp_mnt <- rast(mnt_path)
    message("MNT chargé.")
    if (st_crs(busel_data) != st_crs(temp_mnt)) {
      message("ATTENTION: CRS différents. Reprojection du shapefile vers le CRS du MNT.")
      busel_data <- st_transform(busel_data, st_crs(temp_mnt))
    }
    rm(temp_mnt) # On supprime l'objet, il sera rechargé dans chaque worker
    gc() # Force le nettoyage de la mémoire
  }
  
  dir.create(main_output_dir, showWarnings = FALSE, recursive = TRUE)
  
  #########################################################################
  # DÉFINITION DE LA FONCTION DE TRAITEMENT D'UNE BUSE
  #########################################################################
  process_buse <- function(i, busel_data, main_output_dir, telemac_env_script, xcas_template_filename, use_mnt_for_profiles, mnt_path, dist_amont_limite, dist_p4_p6, num_points_per_profile, dist_extraction_profil, first_profile_shape, duree_palier, duree_montee, num_cores_sim, strickler_zones_base, val_pas_planimetrage, g, pas_maillage_court, pas_temps_court, pas_maillage_moyen, pas_temps_moyen, pas_maillage_long, pas_temps_long, preissmann_slot_width, generate_long_profile_plots, generate_abaque_plots, cleanup_calc_files) {
    
    # Charger les librairies nécessaires dans l'environnement parallèle
    library(sf)
    library(terra)
    library(dplyr)
    library(fs)
    library(stringr)
    library(foreach)
    library(doParallel)
    library(data.table)
    library(plotly)
    library(htmlwidgets)
    
    # Si on utilise le MNT, chaque worker charge sa propre copie.
    if (use_mnt_for_profiles) {
      mnt_raster <- rast(mnt_path)
    }
    
    buse_row <- busel_data[i, ]
    numero_buse <- buse_row$id
    
    # Création du dossier de sortie
    current_buse_output_dir <- file.path(main_output_dir, paste0("buse_", numero_buse))
    dir.create(current_buse_output_dir, showWarnings = FALSE, recursive = TRUE)
    
    # -----------------------------------------------------------------------
    # CHECK DE DÉJÀ FAIT (NOUVELLE FONCTIONNALITÉ v124)
    # -----------------------------------------------------------------------
    abaque_check_file <- file.path(current_buse_output_dir, "abaque_partiel.txt")
    if (file.exists(abaque_check_file) && file.info(abaque_check_file)$size > 0) {
      message(paste(">>> BUSE", numero_buse, ": DÉJÀ TRAITÉE (Fichier abaque trouvé). PASSAGE À LA SUIVANTE. <<<"))
      return(NULL)
    }
    # -----------------------------------------------------------------------
    
    CIRC <- buse_row$CIRC
    if(is.na(buse_row$HAUT1) | buse_row$HAUT1==0) buse_row$HAUT1=buse_row$HAUT2
    nozzle_data <- list(LARG = buse_row$LRG, HAUT1 = buse_row$HAUT1, Z1 = buse_row$Z1, Z2 = buse_row$Z2, LENGTH = buse_row$LENGTH)
    first_profile_shape$bottom_width=nozzle_data$LARG*2
    if ("FRIC" %in% names(buse_row) && !is.na(buse_row$FRIC) && buse_row$FRIC > 0) {
      strickler_buse_value <- 1 / buse_row$FRIC
      if (strickler_buse_value < 1) {
        warning(paste("Buse", numero_buse, ": La valeur FRIC=", buse_row$FRIC, "donne un Strickler de", round(strickler_buse_value, 2), ". Utilisation de la valeur par défaut de 50.0."))
        strickler_buse_value <- 60.0
      }
    } else {
      strickler_buse_value <- 60.0
      warning(paste("Buse", numero_buse, ": Colonne 'FRIC' non valide ou absente. Utilisation de la valeur par défaut de Strickler:", strickler_buse_value))
    }
    strickler_zones <- c(strickler_zones_base[1], strickler_buse_value, strickler_zones_base[3])
    
    
    message(paste("\n\n############################################################"))
    message(paste("--- DÉMARRAGE DU TRAITEMENT POUR LA BUSE :", numero_buse, "---"))
    message(paste("--- Fichiers de sortie dans :", current_buse_output_dir, "---"))
    message(paste("############################################################\n"))
    
    if (nozzle_data$LENGTH < 20) {
      current_pas_maillage <- pas_maillage_court
      current_pas_temps <- pas_temps_court
      duree_palier <- duree_palier_court
      duree_montee <- duree_montee_court
      dist_amont_limite <- 20
      dist_p4_p6 <- 50 
      message("      - Buse courte (< 20m). Maillage fin et pas de temps court sélectionnés.")
    } else if (nozzle_data$LENGTH >= 20 && nozzle_data$LENGTH <= 250) {
      current_pas_maillage <- pas_maillage_moyen
      current_pas_temps <- pas_temps_moyen
      duree_palier <- duree_palier_moyen
      duree_montee <- duree_montee_moyen
      dist_amont_limite <- 50
      dist_p4_p6 <- 400
      message("      - Buse moyenne (20-100m). Maillage et pas de temps moyens sélectionnés.")
    } else {
      current_pas_maillage <- pas_maillage_long
      current_pas_temps <- pas_temps_long
      duree_palier <- duree_palier_long
      duree_montee <- duree_montee_long
      dist_amont_limite <- 500
      dist_p4_p6 <- 2000 
      message("      - Buse longue (> 100m). Maillage large et pas de temps long sélectionnés.")
    }
    
    # BRIDE ANTI-CRASH MASCARET :
    # Si la pente est torrentielle (ex: > 5%), on raccourcit drastiquement le canal aval
    # pour éviter que MASCARET ne calcule une chute de plusieurs mètres qui ferait
    # planter son solveur permanent (SARAP) lors de la remontée de la ligne d'eau.
    slope_check <- (nozzle_data$Z1 - nozzle_data$Z2) / nozzle_data$LENGTH
    if (slope_check > 0.05) {
      dist_p4_p6 <- 10.0
      message("      - Pente très forte détectée (", sprintf("%.1f", slope_check*100), "%). Réduction dist aval à 10m pour éviter crash MASCARET.")
    }
    
    I <- (nozzle_data$Z1 - nozzle_data$Z2) / nozzle_data$LENGTH
    if (CIRC == 1) {
      D <- nozzle_data$HAUT1; R <- D / 2; S <- pi * R^2; P <- 2 * pi * R; Rh <- R/2
      Q <- strickler_buse_value * S * (Rh^(2/3)) * sqrt(abs(I))
    } else {
      S <- nozzle_data$LARG * nozzle_data$HAUT1; P <- nozzle_data$LARG + (2 * nozzle_data$HAUT1); Rh <- S / P
      Q <- strickler_buse_value * S * (Rh^(2/3)) * sqrt(abs(I))
    }
    Q <- round(Q)
    
    if (Q < 15) {
      message(paste("      - Le débit max estimé (", Q, "m³/s) est inférieur à 15 m³/s. Il est forcé à 15 m³/s pour la simulation."))
      Q <- 15
    }
    
    Q_sim_max <- Q * 1.5
    if (Q_sim_max > 180) {
      message(paste("      - Le débit max de simulation (", round(Q_sim_max), "m³/s) dépasse 180 m³/s. Il est plafonné à 180 m³/s."))
      Q_sim_max <- 180
    }
    
    debits_q_list <- list()
    current_max_q <- 0
    
    end_q_p1 <- min(Q_sim_max, 1.0)
    if (0.2 <= end_q_p1) {
      debits_q_list[[length(debits_q_list) + 1]] <- seq(0.2, end_q_p1, by = 0.1)
      current_max_q <- end_q_p1
    }
    
    start_val_p2 <- current_max_q + 0.2
    end_q_p2 <- min(Q_sim_max, 10.0)
    if (start_val_p2 <= end_q_p2) {
      debits_q_list[[length(debits_q_list) + 1]] <- seq(start_val_p2, end_q_p2, by = 0.2)
      current_max_q <- end_q_p2
    }
    
    start_val_p3 <- current_max_q + 1
    end_q_p3 <- min(Q_sim_max, 20.0)
    if (start_val_p3 <= end_q_p3) {
      debits_q_list[[length(debits_q_list) + 1]] <- seq(start_val_p3, end_q_p3, by = 1)
      current_max_q <- end_q_p3
    }
    
    start_val_p4 <- current_max_q + 2.0
    end_q_p4 <- min(Q_sim_max, 40.0)
    if (start_val_p4 <= end_q_p4) {
      debits_q_list[[length(debits_q_list) + 1]] <- seq(start_val_p4, end_q_p4, by = 2.0)
      current_max_q <- end_q_p4
    }
    
    start_val_p5 <- current_max_q + 5.0
    end_q_p5 <- Q_sim_max
    if (start_val_p5 <= end_q_p5) {
      debits_q_list[[length(debits_q_list) + 1]] <- seq(start_val_p5, end_q_p5, by = 5.0)
    }
    
    debits_q <- unique(sort(c(unlist(debits_q_list))))
    debits_q <- round(debits_q[debits_q > 0], 2)
    
    if (CIRC == 1) {
      dimension_str <- sprintf("Diamètre: %.2f m", nozzle_data$HAUT1)
    } else {
      dimension_str <- sprintf("Largeur: %.2f m, Hauteur: %.2f m", nozzle_data$LARG, nozzle_data$HAUT1)
    }
    pente_pourcent <- I * 100
    title_details_str <- paste(
      dimension_str, 
      sprintf("Longueur: %.1f m", nozzle_data$LENGTH), 
      sprintf("Pente: %.2f %%", pente_pourcent),
      sprintf("Qmax simulé: %.1f m³/s", Q_sim_max), 
      sep=" | "
    )
    
    #########################################################################
    # PARTIE 1: GÉNÉRATION DU FICHIER DE GÉOMÉTRIE (.geo)
    #########################################################################
    message("--- PARTIE 1 : GÉOMÉTRIE (.geo) ---")
    
    create_rectangular_profile <- function(nozzle_width, nozzle_height, nozzle_bottom_z, target_center_y, preissmann_slot_width = 0.01, slot_height = 50, bank_y_offset = 10) {
      y_center <- target_center_y
      z_bottom <- nozzle_bottom_z
      z_top <- z_bottom + nozzle_height
      z_slot_top <- z_top + slot_height
      
      y_bottom_left  <- y_center - nozzle_width/2
      y_bottom_right <- y_center + nozzle_width/2
      y_top_left     <- y_bottom_left - 0.01
      y_top_right    <- y_bottom_right + 0.01
      
      y_slot_left    <- y_center - preissmann_slot_width/2
      y_slot_right   <- y_center + preissmann_slot_width/2
      
      y_bank_left    <- y_top_left - bank_y_offset
      y_bank_right   <- y_top_right + bank_y_offset
      
      full_profile_df <- rbind(
        data.frame(Y = y_bank_left, Z = z_slot_top), 
        data.frame(Y = y_slot_left, Z = z_slot_top), 
        data.frame(Y = y_slot_left, Z = z_top), 
        data.frame(Y = y_top_left, Z = z_top), 
        data.frame(Y = y_bottom_left, Z = z_bottom), 
        data.frame(Y = y_bottom_right, Z = z_bottom), 
        data.frame(Y = y_top_right, Z = z_top), 
        data.frame(Y = y_slot_right, Z = z_top), 
        data.frame(Y = y_slot_right, Z = z_slot_top), 
        data.frame(Y = y_bank_right, Z = z_slot_top)
      )
      full_profile_df$Marker <- "B"
      return(full_profile_df[!duplicated(round(full_profile_df[, c("Y", "Z")], 5)), ])
    }
    create_circular_profile <- function(diameter, nozzle_bottom_z, target_center_y, slot_width = 0.01, slot_height = 50, bank_y_offset = 10, n_points_arch = 41) {
      radius <- diameter/2; y_center <- target_center_y; z_center <- nozzle_bottom_z + radius; z_connection <- z_center + sqrt(max(0, radius^2 - (slot_width/2)^2)); z_slot_top <- z_connection + slot_height; y_slot_left <- y_center - slot_width/2; y_slot_right <- y_center + slot_width/2; y_structure_left <- y_center - radius; y_structure_right <- y_center + radius; p_berge_gauche_loin <- data.frame(Y = y_structure_left - bank_y_offset, Z = z_slot_top); p_fente_bas_gauche  <- data.frame(Y = y_slot_left, Z = z_connection); theta_start <- atan2(z_connection - z_center, y_slot_left - y_center); theta_end     <- atan2(z_connection - z_center, y_slot_right - y_center); theta_sequence <- seq(from = theta_start, to = theta_end + 2*pi, length.out = n_points_arch); arch_df <- data.frame(Y = y_center + radius * cos(theta_sequence), Z = z_center + radius * sin(theta_sequence)); p_fente_bas_droite  <- data.frame(Y = y_slot_right, Z = z_connection); p_berge_droite_loin <- data.frame(Y = y_structure_right + bank_y_offset, Z = z_slot_top)
      full_profile_df <- rbind(p_berge_gauche_loin, data.frame(Y = y_slot_left, Z = z_slot_top), p_fente_bas_gauche, arch_df, p_fente_bas_droite, data.frame(Y = y_slot_right, Z = z_slot_top), p_berge_droite_loin)
      full_profile_df$Marker <- "B"
      return(full_profile_df[!duplicated(round(full_profile_df[, c("Y", "Z")], 5)), ])
    }
    
    generate_points_for_trapezoidal <- function(bottom_width, left_slope, right_slope, bottom_elevation, bank_height_above_bottom, target_center_y, bank_y_offset = 10) {
      half_bottom_width <- bottom_width / 2
      top_z <- bottom_elevation + bank_height_above_bottom
      
      # Les points sont d'abord calculés de manière centrée autour de Y=0
      y_top_left_rel <- -half_bottom_width - (left_slope * bank_height_above_bottom)
      y_top_right_rel <- half_bottom_width + (right_slope * bank_height_above_bottom)
      y_rel <- c(y_top_left_rel - bank_y_offset, y_top_left_rel, -half_bottom_width, half_bottom_width, y_top_right_rel, y_top_right_rel + bank_y_offset)
      
      # Le thalweg de ce profil relatif est à Y=0. On décale tous les points pour que le thalweg soit à target_center_y.
      y_abs <- y_rel + target_center_y
      
      z_abs <- c(top_z, top_z, bottom_elevation, bottom_elevation, top_z, top_z)
      points_df <- data.frame(Y = y_abs, Z = z_abs, Marker = "B")
      return(points_df[order(points_df$Y), ])
    }
    
    extract_profile_from_mnt <- function(mnt, center_point_sf, direction_vector, profile_width, num_points = 101) {
      perp_vector <- c(-direction_vector[2], direction_vector[1])
      perp_vector_norm <- perp_vector / sqrt(sum(perp_vector^2))
      start_point <- st_coordinates(center_point_sf) - (profile_width / 2) * perp_vector_norm
      end_point <- st_coordinates(center_point_sf) + (profile_width / 2) * perp_vector_norm
      transect_line <- st_sfc(st_linestring(rbind(start_point, end_point)), crs = st_crs(mnt))
      sampled_points_sf <- st_line_sample(transect_line, n = num_points, type = "regular")
      if (st_is_empty(sampled_points_sf)) return(NULL)
      sampled_points_sf <- st_cast(sampled_points_sf, "POINT")
      sampled_points_vect <- vect(sampled_points_sf)
      extracted_data <- terra::extract(mnt, sampled_points_vect, xy = TRUE, ID = FALSE)
      if (nrow(extracted_data) == 0 || all(is.na(extracted_data[, 1]))) {
        warning(paste0("L'extraction MNT a échoué (autour de X=", round(st_coordinates(center_point_sf)[1]),", Y=", round(st_coordinates(center_point_sf)[2]), "). Profil hors emprise?"))
        return(NULL)
      }
      colnames(extracted_data)[1] <- "Z"
      coords <- extracted_data[, c("x", "y")]
      y_relative <- sqrt((coords$x - coords$x[1])^2 + (coords$y - coords$y[1])^2)
      profile_df <- data.frame(Y = y_relative, Z = extracted_data$Z, Marker = "B")
      profile_df <- na.omit(profile_df)
      if (nrow(profile_df) < 2) return(NULL)
      return(profile_df)
    }
    
    shift_profile_z <- function(profile_df, target_min_z) {
      if(is.null(profile_df) || nrow(profile_df) == 0) return(NULL)
      current_min_z <- min(profile_df$Z, na.rm = TRUE)
      delta_z <- target_min_z - current_min_z
      profile_df$Z <- profile_df$Z + delta_z
      return(profile_df)
    }
    
    center_profile_on_thalweg <- function(profile_df, target_y_center) {
      if(is.null(profile_df) || nrow(profile_df) == 0) return(NULL)
      min_z_index <- which.min(profile_df$Z)
      y_of_min_z <- mean(profile_df$Y[min_z_index]) 
      shift_y <- target_y_center - y_of_min_z
      profile_df$Y <- profile_df$Y + shift_y
      return(profile_df)
    }
    
    add_vertical_walls <- function(profile_df) {
      if(is.null(profile_df) || nrow(profile_df) < 2) return(profile_df)
      
      first_point <- profile_df[1, ]
      wall_start_point <- data.frame(Y = first_point$Y, Z = first_point$Z + 20, Marker = "B")
      
      last_point <- profile_df[nrow(profile_df), ]
      wall_end_point <- data.frame(Y = last_point$Y, Z = last_point$Z + 20, Marker = "B")
      
      new_profile <- rbind(wall_start_point, profile_df, wall_end_point)
      return(new_profile)
    }
    
    write_geo_profile <- function(con, bief_name, prof_name, prof_abs, points_df) {
      writeLines(paste0("PROFIL ", bief_name, " ", prof_name, " ", sprintf("%.3f", prof_abs)), con)
      for (j in 1:nrow(points_df)) {
        writeLines(paste(sprintf("%.3f", points_df$Y[j]), sprintf("%.5f", points_df$Z[j]), points_df$Marker[j]), con)
      }
    }
    
    model_total_length <- dist_amont_limite + nozzle_data$LENGTH + dist_p4_p6
    x_nozzle_start <- dist_amont_limite
    x_nozzle_end <- x_nozzle_start + nozzle_data$LENGTH
    x_coords <- c(0, x_nozzle_start - 2.0, x_nozzle_start, x_nozzle_end, x_nozzle_end + 2.0, model_total_length)
    num_profiles_total <- length(x_coords)
    
    buse_geom <- st_geometry(buse_row)
    
    geo_base_filename <- paste0("geom_buse", numero_buse, ifelse(use_mnt_for_profiles, "_MNT", "_Trapeze"))
    geo_filename_with_ext <- paste0(geo_base_filename, ".geo")
    geo_filepath <- file.path(current_buse_output_dir, geo_filename_with_ext)
    message(paste("Génération du fichier de géométrie :", geo_filename_with_ext))
    
    bief_name <- "Bief_1"
    indices <- list(p1 = 1, p2 = 2, p3 = 3, p4 = 4, p5 = 5, p6 = 6)
    
    all_profiles <- vector("list", num_profiles_total)
    
    # Déterminer une abscisse Y cible unique pour le centrage de tous les profils.
    half_width_calc <- first_profile_shape$bottom_width / 2 + 
      max(first_profile_shape$left_slope, first_profile_shape$right_slope) * first_profile_shape$bank_height_above_bottom + 
      10.0 # bank_y_offset de la fonction generate_points_for_trapezoidal
    target_center_y <- ceiling(half_width_calc) + 5.0 # Ajout d'une marge de sécurité
    message(paste("      - Abscisse Y cible pour le centrage de tous les profils:", sprintf("%.3f", target_center_y)))
    
    if(use_mnt_for_profiles) {
      message("      - Mode MNT: Extraction des profils depuis le MNT...")
      
      buse_coords <- st_coordinates(buse_geom)
      if(nrow(buse_coords) < 2) stop("Géométrie de buse invalide (moins de 2 points) pour la buse ", numero_buse)
      
      start_node <- buse_coords[1, 1:2]
      end_node <- buse_coords[nrow(buse_coords), 1:2]
      direction_vector <- end_node - start_node
      direction_vector_norm <- direction_vector / sqrt(sum(direction_vector^2))
      profile_width_mnt <- 10 * ifelse(CIRC == 1, nozzle_data$HAUT1, nozzle_data$LARG)
      
      pt_extraction_amont <- st_sfc(st_point(start_node - dist_extraction_profil * direction_vector_norm), crs = st_crs(mnt_raster))
      profil_amont_base <- extract_profile_from_mnt(mnt_raster, pt_extraction_amont, direction_vector_norm, profile_width_mnt, num_points_per_profile)
      if(is.null(profil_amont_base)) stop("Échec de l'extraction du profil de base amont. Arrêt.")
      
      pt_extraction_aval <- st_sfc(st_point(end_node + dist_extraction_profil * direction_vector_norm), crs = st_crs(mnt_raster))
      profil_aval_base <- extract_profile_from_mnt(mnt_raster, pt_extraction_aval, direction_vector_norm, profile_width_mnt, num_points_per_profile)
      if(is.null(profil_aval_base)) stop("Échec de l'extraction du profil de base aval. Arrêt.")
      
      # Altitude des profils en extrapolant la pente de la buse (I)
      
      # Profil 1 (limite amont)
      z_profil_1 <- nozzle_data$Z1 + abs(I) * (x_coords[indices$p3] - x_coords[indices$p1])
      profil_1_vshifted <- shift_profile_z(profil_amont_base, z_profil_1)
      profil_1_centered <- center_profile_on_thalweg(profil_1_vshifted, target_center_y)
      profil_1 <- add_vertical_walls(profil_1_centered)
      all_profiles[[indices$p1]] <- list(name = "1", abscissa = x_coords[indices$p1], data = profil_1)
      
      # Profil 2 (juste avant la buse)
      z_profil_2 <- nozzle_data$Z1 + abs(I) * (x_coords[indices$p3] - x_coords[indices$p2])
      profil_2_vshifted <- shift_profile_z(profil_amont_base, z_profil_2)
      profil_2_centered <- center_profile_on_thalweg(profil_2_vshifted, target_center_y)
      profil_2 <- add_vertical_walls(profil_2_centered)
      all_profiles[[indices$p2]] <- list(name = "2", abscissa = x_coords[indices$p2], data = profil_2)
      
      # Profil 5 (juste après la buse)
      z_profil_5 <- nozzle_data$Z2 - abs(I) * (x_coords[indices$p5] - x_coords[indices$p4])
      profil_5_vshifted <- shift_profile_z(profil_aval_base, z_profil_5)
      profil_5_centered <- center_profile_on_thalweg(profil_5_vshifted, target_center_y)
      profil_5 <- add_vertical_walls(profil_5_centered)
      all_profiles[[indices$p5]] <- list(name = "5", abscissa = x_coords[indices$p5], data = profil_5)
      
      # Profil 6 (limite aval)
      z_profil_6 <- nozzle_data$Z2 - abs(I) * (x_coords[indices$p6] - x_coords[indices$p4])
      profil_6_vshifted <- shift_profile_z(profil_aval_base, z_profil_6)
      profil_6_centered <- center_profile_on_thalweg(profil_6_vshifted, target_center_y)
      profil_6 <- add_vertical_walls(profil_6_centered)
      all_profiles[[indices$p6]] <- list(name = "6", abscissa = x_coords[indices$p6], data = profil_6)
      
    } else {
      message("      - Mode Trapézoïdal: Génération des profils génériques...")
      
      calculated_bottom_z <- sapply(x_coords, function(x) {
        if (x <= x_nozzle_start) return(nozzle_data$Z1 + abs(I) * (x_nozzle_start - x))
        if (x > x_nozzle_start && x < x_nozzle_end) return(nozzle_data$Z1 - I * (x - x_nozzle_start))
        return(nozzle_data$Z2 - abs(I) * (x - x_nozzle_end))
      })
      
      for (k in c(indices$p1, indices$p2, indices$p5, indices$p6)) {
        points_raw <- generate_points_for_trapezoidal(
          bottom_width = first_profile_shape$bottom_width, 
          left_slope = first_profile_shape$left_slope, 
          right_slope = first_profile_shape$right_slope, 
          bottom_elevation = calculated_bottom_z[k], 
          bank_height_above_bottom = first_profile_shape$bank_height_above_bottom,
          target_center_y = target_center_y,
          bank_y_offset = 10.0
        )
        points <- add_vertical_walls(points_raw)
        all_profiles[[k]] <- list(name = as.character(k), abscissa = x_coords[k], data = points)
      }
    }
    
    points_p3 <- if(CIRC == 1) create_circular_profile(nozzle_data$HAUT1, nozzle_data$Z1, target_center_y) else create_rectangular_profile(nozzle_data$LARG, nozzle_data$HAUT1, nozzle_data$Z1, target_center_y, preissmann_slot_width)
    all_profiles[[indices$p3]] <- list(name = "3amont", abscissa = x_coords[indices$p3], data = points_p3)
    
    points_p4 <- if(CIRC == 1) create_circular_profile(nozzle_data$HAUT1, nozzle_data$Z2, target_center_y) else create_rectangular_profile(nozzle_data$LARG, nozzle_data$HAUT1, nozzle_data$Z2, target_center_y, preissmann_slot_width)
    all_profiles[[indices$p4]] <- list(name = "3aval", abscissa = x_coords[indices$p4], data = points_p4)
    
    con <- file(geo_filepath, "wt")
    message("      - Écriture ordonnée des profils dans le fichier .geo...")
    for (prof in all_profiles) {
      if (!is.null(prof) && !is.null(prof$data)) {
        write_geo_profile(con, bief_name, prof$name, prof$abscissa, prof$data)
      }
    }
    close(con)
    message("--- Fichier .geo généré avec succès. ---")
    
    #########################################################################
    # PARTIE 2: GÉNÉRATION DES FICHIERS DE LOIS (.loi)
    #########################################################################
    message("\n--- PARTIE 2 : LOIS HYDRAULIQUES (.loi) ---")
    
    duree_totale_sim <- length(debits_q) * (duree_montee + duree_palier)
    hydro_file <- file.path(current_buse_output_dir, "hydrogramme_complet.loi")
    
    con_q <- NULL
    tryCatch({
      con_q <- file(hydro_file, open = "wt")
      writeLines(c("# Hydrogramme...", "# T(s) Q(m3/s)", "S"), con_q)
      current_time <- 0.0
      writeLines(paste(sprintf("%.1f", current_time), "0.20000"), con_q)
      for (q in debits_q) { 
        current_time <- current_time + duree_montee
        writeLines(paste(sprintf("%.1f", current_time), sprintf("%.5f", q)), con_q)
        current_time <- current_time + duree_palier
        writeLines(paste(sprintf("%.1f", current_time), sprintf("%.5f", q)), con_q) 
      }
    }, finally = {
      if(!is.null(con_q)) close(con_q)
    })
    
    message("      - Génération des cotes Z aval selon la logique personnalisée...")
    cotes_z_aval <- c()
    slope=((nozzle_data$Z1-nozzle_data$Z2)/nozzle_data$LENGTH)
    
    z_start = max(nozzle_data$Z2 - 0.2, nozzle_data$Z2 - (slope*dist_p4_p6)/2)
    z_phase1_end = nozzle_data$Z2
    cotes_z_aval1=seq(from = z_start, to = z_phase1_end, by = 0.5)
    z_phase2_end  <- z_phase1_end + nozzle_data$HAUT1+0.2
    cotes_z_aval2=seq(from = z_phase1_end, to = z_phase2_end, by = 0.1)
    z_phase3_end  <- z_phase1_end + nozzle_data$HAUT1 + 2
    cotes_z_aval3 <- seq(from = z_phase2_end, to = z_phase3_end, by = 0.5)
    cotes_z_aval <- unique(sort(c(cotes_z_aval1,cotes_z_aval2,cotes_z_aval3)))
    #à enlever
    # z_start=nozzle_data$Z2-(slope*dist_p4_p6)
    # cotes_z_aval<- seq(from = z_start, to = z_phase1_end + nozzle_data$HAUT1+0.5, by = 0.5)
    ##
    message(paste("La cote Z aval minimale sera:", sprintf("%.3f", z_start)))
    message(paste("Nombre de conditions aval générées:", length(cotes_z_aval)))
    
    for (z in cotes_z_aval) { 
      filename_z <- file.path(current_buse_output_dir, paste0("limni_Z", sprintf("%.3f", z), ".loi"))
      con_z <- NULL
      tryCatch({
        con_z <- file(filename_z, open = "wt")
        writeLines(c(paste("# Limnigramme... Z=", sprintf("%.3f", z)), "# T(s) Z(m)", "S"), con_z)
        writeLines(paste(sprintf("%.1f", 0.0), sprintf("%.5f", z)), con_z)
        writeLines(paste(sprintf("%.1f", duree_totale_sim), sprintf("%.5f", z)), con_z)
      }, finally = {
        if(!is.null(con_z)) close(con_z)
      })
    }
    message(paste(length(cotes_z_aval), "fichiers .loi générés avec succès."))
    
    #########################################################################
    # PARTIE 3: GÉNÉRATION DES FICHIERS DE PARAMÈTRES (.xcas)
    #########################################################################
    message("\n--- PARTIE 3 : PARAMÈTRES (.xcas) ---")
    limni_files <- list.files(current_buse_output_dir, pattern = "^limni_Z.*\\.loi$", full.names = TRUE)
    
    for (limni_file in limni_files) {
      current_xcas_lines <- readLines(xcas_template_filename, warn = FALSE)
      limni_id <- sub("^limni_(Z.*)\\.loi$", "\\1", basename(limni_file))
      scenario_base_name <- paste0("buse", numero_buse, "_scenario_", limni_id)
      xcas_output_filename <- file.path(current_buse_output_dir, paste0(scenario_base_name, ".xcas"))
      
      pas_stockage_value <- ceiling((duree_montee + duree_palier) / current_pas_temps)
      critere_arret_value <- 1
      
      current_xcas_lines <- gsub("<fichMotsCles>.*</fichMotsCles>", paste0("<fichMotsCles>", scenario_base_name, ".xcas</fichMotsCles>"), current_xcas_lines)
      current_xcas_lines <- gsub("<fichier>.*</fichier>", paste0("<fichier>", geo_filename_with_ext, "</fichier>"), current_xcas_lines)
      current_xcas_lines <- gsub("<abscFin>.*</abscFin>", paste0("<abscFin>", sprintf("%.3f", model_total_length), "</abscFin>"), current_xcas_lines)
      current_xcas_lines <- gsub("<absFinZone>.*</absFinZone>", paste0("<absFinZone>", sprintf("%.3f", model_total_length), "</absFinZone>"), current_xcas_lines)
      current_xcas_lines <- gsub("<pasTemps>.*</pasTemps>", paste0("<pasTemps>", sprintf("%.1f", current_pas_temps), "</pasTemps>"), current_xcas_lines)
      current_xcas_lines <- gsub("<tempsMax>.*</tempsMax>", paste0("<tempsMax>", sprintf("%.1f", duree_totale_sim), "</tempsMax>"), current_xcas_lines)
      current_xcas_lines <- gsub("<critereArret>.*</critereArret>", paste0("<critereArret>", critere_arret_value, "</critereArret>"), current_xcas_lines)
      current_xcas_lines <- gsub("<pasStock>.*</pasStock>", paste0("<pasStock>", sprintf("%.0f", pas_stockage_value), "</pasStock>"), current_xcas_lines)
      current_xcas_lines <- gsub("<fichResultat>.*</fichResultat>", paste0("<fichResultat>", scenario_base_name, "_ecr.opt</fichResultat>"), current_xcas_lines)
      current_xcas_lines <- gsub("<fichListing>.*</fichListing>", paste0("<fichListing>", scenario_base_name, ".lis</fichListing>"), current_xcas_lines)
      current_xcas_lines <- gsub("<valeursPas>.*</valeursPas>",paste0("<valeursPas>",sprintf("%.2f", val_pas_planimetrage),"</valeursPas>"), current_xcas_lines)
      current_xcas_lines <- gsub("<numDerProf>.*</numDerProf>", paste0("<numDerProf>", num_profiles_total, "</numDerProf>"), current_xcas_lines)
      
      pas_maillage_str <- paste(c(current_pas_maillage[1], current_pas_maillage[2], current_pas_maillage[2], current_pas_maillage[2], current_pas_maillage[3]), collapse = " ")
      current_xcas_lines <- gsub("<num1erProfPlage>.*</num1erProfPlage>", "<num1erProfPlage>1 2 3 4 5</num1erProfPlage>", current_xcas_lines)
      current_xcas_lines <- gsub("<numDerProfPlage>.*</numDerProfPlage>", paste0("<numDerProfPlage>2 3 4 5 ", num_profiles_total, "</numDerProfPlage>"), current_xcas_lines)
      current_xcas_lines <- gsub("<pasEspacePlage>.*</pasEspacePlage>", paste0("<pasEspacePlage>", pas_maillage_str, "</pasEspacePlage>"), current_xcas_lines)
      
      start_frottement_idx <- grep("<frottement>", current_xcas_lines)[1]
      end_frottement_idx <- grep("</frottement>", current_xcas_lines)[1]
      if (!is.na(start_frottement_idx) && !is.na(end_frottement_idx)) {
        abs_deb_zone_str <- paste(sprintf("%.3f", x_coords[c(1, 3, 5)]), collapse = " ")
        abs_fin_zone_str <- paste(sprintf("%.3f", x_coords[c(2, 4, 6)]), collapse = " ")
        strickler_zones_str <- paste(sprintf("%.1f", strickler_zones), collapse = " ")
        frottement_block <- c("      <frottement>", "        <loi>1</loi>", "        <nbZone>3</nbZone>", "        <numBranche>1 1 1</numBranche>", paste0("        <absDebZone>", abs_deb_zone_str, "</absDebZone>"), paste0("        <absFinZone>", abs_fin_zone_str, "</absFinZone>"), paste0("        <coefLitMin>", strickler_zones_str, "</coefLitMin>"), paste0("        <coefLitMaj>", strickler_zones_str, "</coefLitMaj>"), "      </frottement>")
        current_xcas_lines <- c(current_xcas_lines[1:(start_frottement_idx - 1)], frottement_block, current_xcas_lines[(end_frottement_idx + 1):length(current_xcas_lines)])
      }
      
      start_index <- grep("<parametresLoisHydrauliques>", current_xcas_lines)[1]
      end_index <- grep("</parametresLoisHydrauliques>", current_xcas_lines)[1]
      if (!is.na(start_index) && !is.na(end_index)) {
        lois_lines <- c("  <parametresLoisHydrauliques>","    <nb>2</nb>","    <lois>","      <structureParametresLoi>","        <nom>hydro_amont</nom>","        <type>1</type>","        <donnees>","          <modeEntree>1</modeEntree>",paste0("          <fichier>", basename(hydro_file), "</fichier>"),"        </donnees>","      </structureParametresLoi>","      <structureParametresLoi>",paste0("        <nom>limni_", limni_id, "</nom>"),"        <type>2</type>","        <donnees>","          <modeEntree>1</modeEntree>",paste0("          <fichier>", basename(limni_file), "</fichier>"),"        </donnees>","      </structureParametresLoi>","    </lois>","  </parametresLoisHydrauliques>")
        current_xcas_lines <- c(current_xcas_lines[1:(start_index-1)], lois_lines, current_xcas_lines[(end_index+1):length(current_xcas_lines)])
      }
      
      con <- NULL
      tryCatch({
        con <- file(xcas_output_filename, "w")
        writeLines(current_xcas_lines, con)
      }, finally = {
        if(!is.null(con)) close(con)
      })
    }
    message("--- Fichiers .xcas générés avec succès. ---")
    
    # PARTIE 4
    message("\n--- PARTIE 4 : LANCEMENT DES SIMULATIONS ---")
    xcas_files <- dir_ls(current_buse_output_dir, regexp = "\\.xcas$", recurse = FALSE)
    registerDoParallel(cores = num_cores_sim)
    foreach( xcas_file_path = xcas_files, .combine = 'c', .errorhandling = 'pass') %dopar% {
      library(fs)
      xcas_filename <- path_file(xcas_file_path)
      xcas_basename <- path_ext_remove(xcas_filename)
      temp_sim_dir <- path(current_buse_output_dir, paste0("temp_", xcas_basename))
      results_dir <- path(current_buse_output_dir, "results_mascaret")
      
      final_opt_path <- path(results_dir, paste0(xcas_basename, "_ecr.opt"))
      
      if(!file_exists(final_opt_path) || file_info(final_opt_path)$size == 0) {
        dir_create(temp_sim_dir)
        file_copy(xcas_file_path, path(temp_sim_dir, xcas_filename), overwrite = TRUE)
        file_copy(path(current_buse_output_dir, geo_filename_with_ext), temp_sim_dir, overwrite = TRUE)
        file_copy(path(current_buse_output_dir, "hydrogramme_complet.loi"), temp_sim_dir, overwrite = TRUE)
        limni_id_for_copy <- sub(paste0("buse", numero_buse, "_scenario_"), "", xcas_basename)
        limni_to_copy <- path(current_buse_output_dir, paste0("limni_", limni_id_for_copy, ".loi"))
        if(file_exists(limni_to_copy)) file_copy(limni_to_copy, temp_sim_dir, overwrite = TRUE)
        
        batch_content <- sprintf('@echo off\nCALL "%s"\ncd /D "%%~dp0"\npython "%%HOMETEL%%\\scripts\\python3\\mascaret.py" "%s"\nexit /b %%ERRORLEVEL%%', telemac_env_script, xcas_filename)
        batch_file_path <- path(temp_sim_dir, "run_simulation.bat")
        writeLines(batch_content, batch_file_path)
        
        system2("cmd", args = c("/c", path_abs(batch_file_path)), stdout = TRUE, stderr = TRUE)
      }
    }
    stopImplicitCluster()
    message("--- Lancement des simulations terminé. ---")
    
    # NOUVELLE PARTIE 4.5: Copie des résultats après la fin de tous les calculs
    message("\n--- PARTIE 4.5 : COLLECTE DES RÉSULTATS ---")
    results_dir <- path(current_buse_output_dir, "results_mascaret")
    dir_create(results_dir)
    temp_dirs <- dir_ls(current_buse_output_dir, regexp = "temp_buse", type = "directory")
    
    for(temp_dir in temp_dirs) {
      opt_files_in_temp <- dir_ls(temp_dir, regexp = "\\.(opt|lis)$")
      if(length(opt_files_in_temp) > 0) {
        file_copy(opt_files_in_temp, results_dir, overwrite = TRUE)
      }
    }
    message("--- Collecte des fichiers .opt terminée. ---")
    
    
    # PARTIE 5
    message("\n--- PARTIE 5 : LECTURE DES RÉSULTATS ET CRÉATION DE L'ABAQUE ---")
    opt_dir <- file.path(current_buse_output_dir, "results_mascaret")
    
    ABSCISSA_BUSE_ENTREE_TARGET <- x_coords[3]
    ABSCISSA_BUSE_SORTIE_TARGET <- x_coords[4]
    
    opt_files <- list.files(opt_dir, pattern = "\\.opt$", full.names = TRUE)
    results_list <- list()
    
    for (opt_file in opt_files) {
      opt_content_lines <- readLines(opt_file, warn = FALSE)
      resultats_start_line_index <- grep("[resultats]", opt_content_lines, fixed = TRUE)
      if (length(resultats_start_line_index) == 0) next
      opt_data_text <- opt_content_lines[(resultats_start_line_index[1] + 1):length(opt_content_lines)]
      if(length(opt_data_text) == 0 || all(nchar(trimws(opt_data_text)) == 0)) next
      
      opt_data <- tryCatch({ fread(text = opt_data_text, sep = ";", strip.white=TRUE, header = FALSE, colClasses = "numeric", fill = TRUE, select = c(1, 4, 6, 11, 12, 14), col.names = c("Time", "Abscissa", "S_value", "Froude", "Velocity", "DebitTotal")) }, error = function(e) { NULL })
      if(is.null(opt_data) || nrow(opt_data) == 0) next
      
      plateau_end_times <- (1:length(debits_q)) * (duree_montee + duree_palier)
      
      for(time_step in plateau_end_times){
        time_to_extract <- time_step - current_pas_temps
        if(time_to_extract < 0 || time_to_extract > max(opt_data$Time)) next
        
        steady_state_data <- opt_data[abs(opt_data$Time - time_to_extract) < 1e-3, ]
        if(nrow(steady_state_data) == 0) next
        
        entree_buse_row <- steady_state_data[which.min(abs(steady_state_data$Abscissa - ABSCISSA_BUSE_ENTREE_TARGET)), ]
        sortie_buse_row <- steady_state_data[which.min(abs(steady_state_data$Abscissa - ABSCISSA_BUSE_SORTIE_TARGET)), ]
        
        if(nrow(entree_buse_row) == 1 && nrow(sortie_buse_row) == 1){
          avg_velocity <- (entree_buse_row$Velocity + sortie_buse_row$Velocity) / 2
          
          results_list[[length(results_list) + 1]] <- data.frame(
            Debit = entree_buse_row$DebitTotal, 
            S1 = entree_buse_row$S_value, 
            S2 = sortie_buse_row$S_value,
            Froude_amont = entree_buse_row$Froude,
            Avg_Velocity = avg_velocity
          )
        }
      }
    }
    
    results_for_file <- data.frame()
    all_results_df <- data.frame()
    results_df_sorted <- data.frame()
    
    if (length(results_list) > 0) {
      all_results_df <- do.call(rbind, results_list)
      s1_max_threshold <- nozzle_data$Z1 + nozzle_data$HAUT1 + 10
      
      all_results_filtered <- all_results_df[
        all_results_df$S1 <= s1_max_threshold & 
          all_results_df$Froude_amont <= 5 &
          # all_results_df$Avg_Velocity < 7.0 &
          (all_results_df$S1 - all_results_df$S2) >= 0.05,
      ]
      if(nrow(all_results_filtered) > 0) {
        results_df_unique <- unique(all_results_filtered)
        results_df_sorted <- results_df_unique[order(results_df_unique$Debit, results_df_unique$S1), ]
        
        final_points_list <- lapply(split(results_df_sorted, results_df_sorted$Debit), function(df_debit) {
          torrential_points <- df_debit %>% filter(Froude_amont >= 1)
          if (nrow(torrential_points) > 0) return(torrential_points)
          good_fluvial_points <- df_debit %>% filter(Froude_amont < 1 & S1 < (nozzle_data$Z1 + nozzle_data$HAUT1) & S2 < (nozzle_data$Z2 + nozzle_data$HAUT1))
          if (nrow(good_fluvial_points) > 0) return(good_fluvial_points)
          return(df_debit %>% filter(Froude_amont < 1))
        })
        
        results_for_file <- bind_rows(final_points_list) %>% unique() %>% arrange(Debit, S1)
        if(length(which(results_for_file$Froude_amont>=1))>0){
          max_debit_torrential=max(results_for_file$Debit[which(results_for_file$Froude_amont>=1)])
          max_S1_torrentiel=max(results_for_file$S1[which(results_for_file$Debit==max_debit_torrential)])
          max_S2_torrentiel=max(results_for_file$S2[which(results_for_file$Debit==max_debit_torrential)])
          
          points_fluviaux1=which(results_for_file$Debit<max_debit_torrential & (results_for_file$S1 >max_S1_torrentiel | results_for_file$S2 >max_S2_torrentiel ) )
          points_fluviaux2=which(results_for_file$Debit>max_debit_torrential & results_for_file$S2 >nozzle_data$Z2 + nozzle_data$HAUT1 + 0.05  )
          if(length(points_fluviaux1)>0 | length(points_fluviaux2)>0){
            results_for_file=results_for_file[-c(points_fluviaux1,points_fluviaux2),]
            results_for_file <- results_for_file %>% unique() %>% arrange(Debit, S1)
          }
        }
        
        # --- NETTOYAGE ET STRICTE MONOTONIE HYDRAULIQUE (Q et S1) ---
        if (nrow(results_for_file) > 0) {
          
          # On trie impérativement par S1 (hauteur d'eau) en premier lieu pour s'assurer
          # que la courbe part toujours du point le plus bas (le radier) sans sauter de points à cause du tri par débit
          results_for_file <- results_for_file[order(results_for_file$S1, results_for_file$Debit), ]
          
          # Filtrage ultra-simple pour garantir la stricte croissance de S1 requise par MASCARET
          if (nrow(results_for_file) > 1) {
            indices_valides <- c(1)
            derniere_cote <- results_for_file$S1[1]
            
            for (idx_pt in 2:nrow(results_for_file)) {
              if (results_for_file$S1[idx_pt] > derniere_cote) {
                indices_valides <- c(indices_valides, idx_pt)
                derniere_cote <- results_for_file$S1[idx_pt]
              }
            }
            results_for_file <- results_for_file[indices_valides, ]
          }
        }
        
        output_txt_file_S_partiel <- file.path(current_buse_output_dir, "abaque_partiel.txt")
        if (nrow(results_for_file) > 0) {
          file_conn_partiel <- file(output_txt_file_S_partiel, "w")
          writeLines(c(as.character(numero_buse), as.character(nrow(results_for_file))), file_conn_partiel)
          cat(paste(sprintf("%.2f", results_for_file$Debit), collapse = ","), "\n", file = file_conn_partiel)
          cat(paste(sprintf("%.2f", results_for_file$S1), collapse = ","), "\n", file = file_conn_partiel)
          cat(paste(sprintf("%.2f", results_for_file$S2), collapse = ","), "\n", file = file_conn_partiel)
          close(file_conn_partiel)
        }
        
        output_txt_file_QS1_partiel <- file.path(current_buse_output_dir, "abaque_partiel_QS1.txt")
        if (nrow(results_for_file) > 0) {
          results_QS1_unique <- results_for_file %>% dplyr::select(Debit, S1) %>% distinct() %>% arrange(Debit, S1)
          file_conn_QS1 <- file(output_txt_file_QS1_partiel, "w")
          writeLines(c(as.character(numero_buse), as.character(nrow(results_QS1_unique))), file_conn_QS1)
          cat(paste(sprintf("%.2f", results_QS1_unique$Debit), collapse = ","), "\n", file = file_conn_QS1)
          cat(paste(sprintf("%.2f", results_QS1_unique$S1), collapse = ","), "\n", file = file_conn_QS1)
          close(file_conn_QS1)
        }
      }
    }
    
    if(generate_long_profile_plots) {
      message("\n--- GÉNÉRATION DES PROFILS EN LONG ---")
      for (opt_file in opt_files) {
        scenario_name <- sub("_ecr\\.opt$", "", basename(opt_file)); opt_content_lines <- readLines(opt_file, warn = FALSE); resultats_start_line_index <- grep("[resultats]", opt_content_lines, fixed = TRUE); if (length(resultats_start_line_index) == 0) next
        opt_data_full <- tryCatch({ fread(text = opt_content_lines[(resultats_start_line_index[1] + 1):length(opt_content_lines)], sep = ";", strip.white=TRUE, header = FALSE, colClasses = "numeric", fill = TRUE, select = c(1, 4, 5, 6, 14), col.names = c("Time", "Abscissa", "Z_fond", "Z_eau", "DebitTotal")) }, error = function(e) { NULL })
        if(is.null(opt_data_full) || nrow(opt_data_full) == 0) next
        fig_pl <- plot_ly(); profil_fond <- opt_data_full[abs(Time - min(Time)) < 1e-3, ]; fig_pl <- add_trace(fig_pl, data = profil_fond, x = ~Abscissa, y = ~Z_fond, type = 'scatter', mode = 'lines', line = list(color = 'black', width = 3), name = 'Fond')
        for (time_p in ((1:length(debits_q)) * (duree_montee + duree_palier) - current_pas_temps)) { if (time_p > 0 && time_p <= max(opt_data_full$Time)) { steady_data <- opt_data_full[abs(opt_data_full$Time - time_p) < 1e-3, ]; if (nrow(steady_data) > 0) fig_pl <- add_trace(fig_pl, data = steady_data, x = ~Abscissa, y = ~Z_eau, type = 'scatter', mode = 'lines', name = paste('Q =', round(steady_data$DebitTotal[1], 2), 'm³/s')) } }
        y_range <- range(c(profil_fond$Z_fond, opt_data_full$Z_eau), na.rm = TRUE); fig_pl <- fig_pl %>% layout(title = scenario_name, xaxis = list(title = "Abscisse"), yaxis = list(title = "Altitude", range = y_range), shapes = list(list(type = 'line', x0 = x_coords[indices$p3], x1 = x_coords[indices$p3], y0 = y_range[1], y1 = y_range[2], line = list(color = 'grey', dash = 'dash')), list(type = 'line', x0 = x_coords[indices$p4], x1 = x_coords[indices$p4], y0 = y_range[1], y1 = y_range[2], line = list(color = 'grey', dash = 'dash')), list(type = 'line', x0 = x_coords[indices$p3], x1 = x_coords[indices$p4], y0 = nozzle_data$Z1 + nozzle_data$HAUT1, y1 = nozzle_data$Z2 + nozzle_data$HAUT1, line = list(color = 'red', dash = 'longdash'))))
        saveWidget(fig_pl, file = file.path(opt_dir, paste0("profil_en_long_", scenario_name, ".html")), selfcontained = TRUE)
      }
    }
    
    
    if(generate_abaque_plots) {
      message("\n--- PARTIE 6 : GÉNÉRATION DES GRAPHIQUES D'ABAQUE ---")
      
      # Utiliser `all_results_df` pour définir les points à estomper
      points_to_keep <- results_for_file
      points_to_fade <- dplyr::anti_join(all_results_df, points_to_keep, by = c("Debit", "S1", "S2", "Froude_amont", "Avg_Velocity"))
      
      if (nrow(points_to_keep) > 0 || nrow(points_to_fade) > 0) {
        
        # Préparation des données pour les points principaux
        results_df_graph <- points_to_keep
        if(nrow(results_df_graph) > 0) {
          colnames(results_df_graph) <- c("Debit", "S1_Charge_Amont", "S2_Charge_Aval", "Froude_amont", "Avg_Velocity")
          results_df_graph$Froude_Condition <- cut(results_df_graph$Froude_amont, 
                                                   breaks = c(-Inf, 0.99, 1.01, Inf), 
                                                   labels = c("Fluvial (Fr < 1)", "Critique (Fr = 1)", "Torrentiel (Fr > 1)"))
        }
        
        # --- Graphique 3D ---
        title_3d <- paste("Relation Débit-Charges S1/S2 - Buse", numero_buse, "<br>", title_details_str)
        fig_3d_S <- plot_ly()
        
        if(nrow(points_to_fade) > 0) {
          fig_3d_S <- fig_3d_S %>% add_trace(
            data = points_to_fade, x=~Debit, y=~S2, z=~S1,
            type='scatter3d', mode='markers',
            marker=list(size=3, color='rgba(211, 211, 211, 0.5)'), # Gris clair, semi-transparent
            name = "Points exclus",
            hoverinfo = "text",
            text = ~paste('Exclu<br>Débit:', sprintf("%.2f", Debit), '<br>S1:', sprintf("%.2f", S1), '<br>S2:', sprintf("%.2f", S2), '<br>Froude:', sprintf("%.2f", Froude_amont))
          )
        }
        
        if(nrow(results_df_graph) > 0) {
          fig_3d_S <- fig_3d_S %>% add_trace(
            data = results_df_graph, x=~Debit, y=~S2_Charge_Aval, z=~S1_Charge_Amont,
            type='scatter3d', mode='markers',
            color=~S1_Charge_Amont, colorscale='Viridis',
            symbol=~Froude_Condition, symbols=c('circle','square','triangle-up'),
            marker=list(size=4),
            customdata=~Froude_amont,
            name = "Points de l'abaque",
            hovertemplate=paste('<b>Débit:</b> %{x:.2f} m³/s<br>',
                                '<b>S2 (Aval):</b> %{y:.2f} m<br>',
                                '<b>S1 (Amont):</b> %{z:.2f} m<br>',
                                '<b>Froude (Amont):</b> %{customdata:.2f}<br>',
                                '<extra></extra>')
          )
        }
        
        fig_3d_S <- fig_3d_S %>% layout(title=title_3d,
                                        legend=list(title=list(text='<b>Régime Froude</b>')),
                                        scene=list(xaxis=list(title="Débit (m³/s)"),
                                                   yaxis=list(title="S2 Charge Aval (m)"),
                                                   zaxis=list(title="S1 Charge Amont (m)")))
        
        # --- Graphique 2D ---
        title_2d <- paste("Relation S1-S2 - Buse", numero_buse, "<br>", title_details_str)
        fig_2d_S <- plot_ly()
        
        if(nrow(points_to_fade) > 0) {
          fig_2d_S <- fig_2d_S %>% add_trace(
            data = points_to_fade, x=~S1, y=~S2,
            type='scatter', mode='markers',
            marker=list(size=5, color='rgba(211, 211, 211, 0.6)'), # Gris clair, semi-transparent
            name = "Points exclus",
            hoverinfo = "text",
            text = ~paste('Exclu<br>Débit:', sprintf("%.2f", Debit), '<br>S1:', sprintf("%.2f", S1), '<br>S2:', sprintf("%.2f", S2), '<br>Froude:', sprintf("%.2f", Froude_amont))
          )
        }
        
        if(nrow(results_df_graph) > 0) {
          fig_2d_S <- fig_2d_S %>% add_trace(
            data = results_df_graph, x=~S1_Charge_Amont, y=~S2_Charge_Aval,
            type='scatter', mode='markers',
            color=~Debit, colorscale='Plasma',
            symbol=~Froude_Condition, symbols=c('circle','square','triangle-up'),
            marker=list(size=6),
            customdata=~Froude_amont,
            name = "Points de l'abaque",
            hovertemplate=paste('<b>S1 (Amont):</b> %{x:.2f} m<br>',
                                '<b>S2 (Aval):</b> %{y:.2f} m<br>',
                                '<b>Débit:</b> %{marker.color:.2f} m³/s<br>',
                                '<b>Froude:</b> %{customdata:.2f}<br>',
                                '<extra></extra>')
          )
          
          shapes_list <- list(
            list(type = 'line', x0 = nozzle_data$Z1, x1 = nozzle_data$Z1, y0 = min(results_df_graph$S2_Charge_Aval), y1 = max(results_df_graph$S2_Charge_Aval), line = list(color = 'blue', dash = 'longdash', width = 2)),
            list(type = 'line', x0 = nozzle_data$Z1 + nozzle_data$HAUT1, x1 = nozzle_data$Z1 + nozzle_data$HAUT1, y0 = min(results_df_graph$S2_Charge_Aval), y1 = max(results_df_graph$S2_Charge_Aval), line = list(color = 'red', dash = 'longdash', width = 2)),
            list(type = 'line', y0 = nozzle_data$Z2, y1 = nozzle_data$Z2, x0 = min(results_df_graph$S1_Charge_Amont), x1 = max(results_df_graph$S1_Charge_Amont), line = list(color = 'blue', dash = 'longdash', width = 2)),
            list(type = 'line', y0 = nozzle_data$Z2 + nozzle_data$HAUT1, y1 = nozzle_data$Z2 + nozzle_data$HAUT1, x0 = min(results_df_graph$S1_Charge_Amont), x1 = max(results_df_graph$S1_Charge_Amont), line = list(color = 'red', dash = 'longdash', width = 2))
          )
          fig_2d_S <- fig_2d_S %>% layout(shapes = shapes_list)
        }
        
        fig_2d_S <- fig_2d_S %>% layout(
          title=title_2d,
          xaxis=list(title="S1 Charge Amont (m)"),
          yaxis=list(title="S2 Charge Aval (m)"),
          legend=list(title=list(text='<b>Régime Froude</b>'))
        )
        
        htmlwidgets::saveWidget(fig_3d_S, file.path(current_buse_output_dir, "graph_abaque_S_3d.html"), selfcontained = TRUE)
        htmlwidgets::saveWidget(fig_2d_S, file.path(current_buse_output_dir, "graph_abaque_S_2d.html"), selfcontained = TRUE)
        message("Graphiques d'abaque 2D et 3D (incluant les points exclus) exportés.")
        
      } else {
        message("Aucune donnée à visualiser, les graphiques d'abaque n'ont pas été générés pour cette buse.")
      }
    }
    
    # --- BLOC DE NETTOYAGE SÉLECTIF (AJOUTÉ v123) ---
    if (cleanup_calc_files) {
      message(paste("      - Nettoyage sélectif pour la buse", numero_buse))
      
      # 1. Identifier tous les dossiers temporaires temp_...
      tdirs <- fs::dir_ls(current_buse_output_dir, regexp = "temp_", type = "directory")
      if (length(tdirs) > 1) {
        # Garder uniquement le premier dossier pour vérification, supprimer les autres
        fs::dir_delete(tdirs[-1]) 
      }
      
      # 2. Supprimer le dossier global results_mascaret (doublon inutile une fois les données extraites)
      res_fold <- file.path(current_buse_output_dir, "results_mascaret")
      if(fs::dir_exists(res_fold)) fs::dir_delete(res_fold)
      
      # 3. Supprimer les fichiers d'entrée à la racine (déjà sauvegardés dans le dossier temp conservé)
      root_files <- fs::dir_ls(current_buse_output_dir, regexp = "\\.(xcas|geo|loi|lis)$")
      if(length(root_files) > 0) fs::file_delete(root_files)
    }
  } 
  
  #########################################################################
  # DÉMARRAGE DE LA BOUCLE PRINCIPALE
  #########################################################################
  if (ncore_buse <= 1) {
    message("--- DÉMARRAGE DU TRAITEMENT SÉQUENTIEL ---")
    for (i in 1:nrow(busel_data)) {
      abaques_check_file <- file.path(dsnlayerC, "abaques2.txt")
      if (file.exists(abaques_check_file) && file.info(abaques_check_file)$size > 0) {
        message(paste(">>> BUSE", abaques_check_file, ": DÉJÀ TRAITÉE (Fichier abaque trouvé). PASSAGE À LA SUIVANTE. <<<"))
        next
      }
      process_buse(i, busel_data, main_output_dir, telemac_env_script, xcas_template_filename, use_mnt_for_profiles, mnt_path, dist_amont_limite, dist_p4_p6, num_points_per_profile, dist_extraction_profil, first_profile_shape, duree_palier, duree_montee, num_cores_sim, strickler_zones_base, val_pas_planimetrage, g, pas_maillage_court, pas_temps_court, pas_maillage_moyen, pas_temps_moyen, pas_maillage_long, pas_temps_long, preissmann_slot_width, generate_long_profile_plots, generate_abaque_plots, cleanup_calc_files)
    }
  } else {
    message(paste("--- DÉMARRAGE DU TRAITEMENT PARALLÈLE SUR", ncore_buse, "CŒURS ---"))
    registerDoParallel(cores = ncore_buse)
    foreach(i = 1:nrow(busel_data)) %dopar% { 
      process_buse(i, busel_data, main_output_dir, telemac_env_script, xcas_template_filename, use_mnt_for_profiles, mnt_path, dist_amont_limite, dist_p4_p6, num_points_per_profile, dist_extraction_profil, first_profile_shape, duree_palier, duree_montee, num_cores_sim, strickler_zones_base, val_pas_planimetrage, g, pas_maillage_court, pas_temps_court, pas_maillage_moyen, pas_temps_moyen, pas_maillage_long, pas_temps_long, preissmann_slot_width, generate_long_profile_plots, generate_abaque_plots, cleanup_calc_files) 
    }
    stopImplicitCluster()
  }
  
  #########################################################################
  # PARTIE 7: ASSEMBLAGE FINAL DE L'ABAQUE
  #########################################################################
  message("\n--- PARTIE 7 : ASSEMBLAGE DU FICHIER D'ABAQUE FINAL ---")
  output_txt_file_S_global <- file.path(main_output_dir, "resultats_buse_format_abaqueSQ.txt")
  file_conn_main <- file(output_txt_file_S_global, "w")
  writeLines("#abaqueSQ", file_conn_main)
  output_txt_file_QS1_global <- file.path(main_output_dir, "abaques.txt")
  file_conn_qs1 <- file(output_txt_file_QS1_global, "w")
  writeLines("#abaqueQS1", file_conn_qs1)
  
  for (i in 1:nrow(busel_data)) {
    numero_buse <- busel_data$id[i]
    current_buse_output_dir <- file.path(main_output_dir, paste0("buse_", numero_buse))
    
    p1 <- file.path(current_buse_output_dir, "abaque_partiel.txt")
    if (file.exists(p1)) writeLines(readLines(p1), file_conn_main)
    
    p2 <- file.path(current_buse_output_dir, "abaque_partiel_QS1.txt")
    if (file.exists(p2)) writeLines(readLines(p2), file_conn_qs1)
  }
  
  close(file_conn_main); close(file_conn_qs1)
  message("\n--- WORKFLOW COMPLET TERMINÉ ---")
  file.copy(from = file.path(main_output_dir, "abaques.txt"),to = dsnlayerC,overwrite = TRUE,recursive = TRUE)
  # 1. Lire toutes les lignes du fichier d'origine
  lignes <- readLines(file.path(dsnlayerC, "abaques.txt"))
  
  # 2. Nettoyer les éventuelles lignes vides à la fin du fichier
  lignes <- lignes[lignes != ""]
  
  # 3. Calculer le nombre de buses
  # On soustrait 1 (pour la ligne de commentaire) avant de diviser par 4
  n_buses <- floor((length(lignes) - 1) / 4)
  
  # 4. Générer les indices des IDs
  # On commence à la ligne 2, et on fait des sauts de 4
  indices_id <- seq(2, by = 4, length.out = n_buses)
  
  # 5. Remplacer les lignes d'identifiants par la nouvelle numérotation
  lignes[indices_id] <- 1:n_buses
  
  # 6. Sauvegarder le résultat en écrasant le fichier
  writeLines(as.character(lignes), file.path(dsnlayerC, "abaques.txt"))
  # Script R pour lisser les abaques Telemac-2D
  # Objectif : Garantir une croissance stricte des hauteurs d'eau (S1) 
  # pour éviter les divisions par zéro dans DISCHARGE_INTERPOL.
  
  # Noms des fichiers (à adapter si besoin)
  file_in <- file.path(dsnlayerC, "abaques.txt")
  file_out <- file.path(dsnlayerC, "abaques2.txt")
  
  # Lecture du fichier
  lines <- readLines(file_in, warn = FALSE)
  # Nettoyage des lignes vides éventuelles
  lines <- lines[trimws(lines) != ""]
  
  # On conserve l'entête (ex: #abaqueQS1)
  output_lines <- c(lines[1])
  
  idx <- 2
  while (idx <= length(lines)) {
    # Extraction du bloc de 4 lignes
    abaque_id <- trimws(lines[idx])
    n_pts_old <- as.integer(trimws(lines[idx + 1]))
    q_str     <- lines[idx + 2]
    s1_str    <- lines[idx + 3]
    
    # Conversion des lignes Q et S1 en vecteurs numériques
    q_vals  <- as.numeric(unlist(strsplit(q_str, ",")))
    s1_vals <- as.numeric(unlist(strsplit(s1_str, ",")))
    
    # Construction du dataframe pour nettoyage
    df_abaque <- data.frame(Debit = q_vals, S1 = s1_vals)
    
    # Filtrage ultra-simple : on parcourt les points dans leur ordre naturel
    # et on ne garde que ceux où la cote d'eau (S1) augmente strictement.
    # On ne trie pas, et on ne modifie JAMAIS les valeurs de débit !
    if (nrow(df_abaque) > 1) {
      indices_valides <- c(1) # On garde toujours le 1er point (fond)
      derniere_cote <- df_abaque$S1[1]
      
      for (idx_pt in 2:nrow(df_abaque)) {
        if (df_abaque$S1[idx_pt] > derniere_cote) {
          indices_valides <- c(indices_valides, idx_pt)
          derniere_cote <- df_abaque$S1[idx_pt]
        }
      }
      df_abaque <- df_abaque[indices_valides, ]
    }
    
    n_pts  <- nrow(df_abaque)
    q_out  <- paste(sprintf("%.2f", df_abaque$Debit), collapse = ",")
    s1_out <- paste(sprintf("%.2f", df_abaque$S1), collapse = ",")
    
    # Stockage des 4 lignes du bloc corrigé
    output_lines <- c(output_lines, abaque_id, as.character(n_pts), q_out, s1_out)
    
    # On passe à l'abaque suivant
    idx <- idx + 4
  }
  
  # Écriture du nouveau fichier
  writeLines(output_lines, file_out)
  cat("Traitement terminé avec succès !\nLe fichier corrigé est :", file_out, "\n")
}
abacus=function(){
  library(sf)
  library(terra)
  library(dplyr)
  library(fs)
  library(stringr)
  library(foreach)
  library(doParallel)
  library(data.table)
  library(plotly)
  library(htmlwidgets)
  
  #########################################################################
  # PARTIE 0: PARAMÃƒÆ’Ã†â€™Ãƒâ€¹Ã¢â‚¬Â TRES UTILISATEUR
  # Modifiez les valeurs dans cette section pour configurer votre simulation.
  #########################################################################
  # --- 0.1. Configuration des chemins et fichiers ---
  # chem_routine <- "C:/Users/CartoNationale/Desktop/Nabil"
  
  main_output_dir <- file.path(dsnlayerC, "abaques") 
  telemac_env_script <- "C:/TELEMAC/V8P4/configs/pysource.bat"
  xcas_template_filename <- file.path(chem_routine,"C2D",Doss_Base, "masc.xcas.txt")
  
  # --- 0.2. ParamÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â¨tres de parallÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â©lisation ---
  ncore_buse <- ncore_buse_abaques # Nombre de cÃƒÆ’Ã¢â‚¬Â¦ÃƒÂ¢Ã¢â€šÂ¬Ã…â€œurs pour la boucle sur les buses. 1 = sÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â©quentiel, >=2 = parallÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â¨le
  num_cores_sim <- num_cores_sim_abaques
  # Nombre de cÃƒÆ’Ã¢â‚¬Â¦ÃƒÂ¢Ã¢â€šÂ¬Ã…â€œurs pour les simulations d'une MÃƒÆ’Ã†â€™Ãƒâ€¦Ã‚Â ME buse
  
  # --- 0.3. ContrÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â´le de la gÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â©omÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â©trie --- 
  use_mnt_for_profiles <- FALSE # Mettre ÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â  TRUE pour extraire du MNT, FALSE pour utiliser des profils trapÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â©zoÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â¯daux
  
  # --- Chemins pour le mode MNT ---
  shapefile_path <- file.path(dsnlayerC, "OuvHydrau_Select.shp") 
  mnt_path <- file.path(dsnlayerC, "MNTFinal.gpkg") # <--- CHEMIN VERS VOTRE MNT
  
  # --- 0.4. ParamÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â¨tres de la gÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â©omÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â©trie ---
  dist_amont_limite <- 1000.0
  dist_p4_p6 <- 2000 # Distance entre la sortie de la buse (corrigÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â©e ÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â  p5) et la limite aval (p6)
  num_points_per_profile <- 101
  dist_extraction_profil <- 3.0 
  preissmann_slot_width <- 0.01 # Largeur de la fente de Preissmann en mÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â¨tres
  
  first_profile_shape <- list(
    type = "trapezoidal", bottom_width = 3, left_slope = 1/1.5,
    right_slope = 1/1.5, bank_height_above_bottom = 40
  )
  
  # --- 0.5. ParamÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â¨tres temporels et hydrauliques ---
  duree_palier <- 400
  duree_montee <- 50
  strickler_zones_base <- c(20.0, 20.0, 20.0) 
  val_pas_planimetrage <- 0.02
  g <- 9.81
  
  # --- 0.6. ParamÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â¨tres de maillage et de temps de simulation adaptatifs ---
  # Pour buses COURTES (< 20m)
  pas_maillage_court <- c(1, 0.5, 1)
  pas_temps_court <- 0.5
  duree_palier_court <- 20
  duree_montee_court <- 10
  
  # Pour buses MOYENNES (20m ÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â  100m)
  pas_maillage_moyen <- c(1, 1, 1)
  pas_temps_moyen <- 1
  duree_palier_moyen <- 100
  duree_montee_moyen <- 20
  # Pour buses LONGUES (> 100m)
  pas_maillage_long <- c(2, 2, 2)
  pas_temps_long <- 2
  duree_palier_long <- 400
  duree_montee_long <- 50
  # --- 0.7. ParamÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â¨tres de sortie (NOUVEAU) ---
  generate_long_profile_plots <- TRUE # Mettre ÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â  TRUE pour gÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â©nÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â©rer les graphiques de profil en long
  generate_abaque_plots <- TRUE       # Mettre ÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â  TRUE pour gÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â©nÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â©rer les graphiques d'abaque 2D/3D
  cleanup_calc_files <- FALSE           # <--- NOUVEAU: TRUE pour garder 1 dossier temp et supprimer le reste
  
  
  #########################################################################
  # LECTURE DES DONNÃƒÆ’Ã†â€™ÃƒÂ¢Ã¢â€šÂ¬Ã‚Â°ES SPATIALES ET INITIALISATION
  #########################################################################
  message("--- LECTURE DES DONNÃƒÆ’Ã†â€™ÃƒÂ¢Ã¢â€šÂ¬Ã‚Â°ES SPATIALES ---")
  if (!file.exists(shapefile_path)) stop("Le fichier shapefile n'a pas ÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â©tÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â© trouvÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â©: ", shapefile_path)
  busel_data <- st_read(shapefile_path, quiet = TRUE)
  message(paste("Lecture de", nrow(busel_data), "buse(s) depuis le shapefile."))
  
  mnt_raster <- NULL # Initialisation ÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â  NULL
  if(use_mnt_for_profiles) {
    if (!file.exists(mnt_path)) stop("Le fichier MNT n'a pas ÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â©tÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â© trouvÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â©: ", mnt_path)
    message("Chargement du MNT pour vÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â©rification CRS...")
    temp_mnt <- rast(mnt_path)
    message("MNT chargÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â©.")
    if (st_crs(busel_data) != st_crs(temp_mnt)) {
      message("ATTENTION: CRS diffÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â©rents. Reprojection du shapefile vers le CRS du MNT.")
      busel_data <- st_transform(busel_data, st_crs(temp_mnt))
    }
    rm(temp_mnt) # On supprime l'objet, il sera rechargÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â© dans chaque worker
    gc() # Force le nettoyage de la mÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â©moire
  }
  
  dir.create(main_output_dir, showWarnings = FALSE, recursive = TRUE)
  
  #########################################################################
  # DÃƒÆ’Ã†â€™ÃƒÂ¢Ã¢â€šÂ¬Ã‚Â°FINITION DE LA FONCTION DE TRAITEMENT D'UNE BUSE
  #########################################################################
  process_buse <- function(i, busel_data, main_output_dir, telemac_env_script, xcas_template_filename, use_mnt_for_profiles, mnt_path, dist_amont_limite, dist_p4_p6, num_points_per_profile, dist_extraction_profil, first_profile_shape, duree_palier, duree_montee, num_cores_sim, strickler_zones_base, val_pas_planimetrage, g, pas_maillage_court, pas_temps_court, pas_maillage_moyen, pas_temps_moyen, pas_maillage_long, pas_temps_long, preissmann_slot_width, generate_long_profile_plots, generate_abaque_plots, cleanup_calc_files) {
    
    # Charger les librairies nÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â©cessaires dans l'environnement parallÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â¨le
    library(sf)
    library(terra)
    library(dplyr)
    library(fs)
    library(stringr)
    library(foreach)
    library(doParallel)
    library(data.table)
    library(plotly)
    library(htmlwidgets)
    
    # Si on utilise le MNT, chaque worker charge sa propre copie.
    if (use_mnt_for_profiles) {
      mnt_raster <- rast(mnt_path)
    }
    
    buse_row <- busel_data[i, ]
    numero_buse <- buse_row$id
    
    # CrÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â©ation du dossier de sortie
    current_buse_output_dir <- file.path(main_output_dir, paste0("buse_", numero_buse))
    dir.create(current_buse_output_dir, showWarnings = FALSE, recursive = TRUE)
    
    # -----------------------------------------------------------------------
    # CHECK DE DÃƒÆ’Ã†â€™ÃƒÂ¢Ã¢â€šÂ¬Ã‚Â°JÃƒÆ’Ã†â€™ÃƒÂ¢Ã¢â‚¬Å¡Ã‚Â¬ FAIT (NOUVELLE FONCTIONNALITÃƒÆ’Ã†â€™ÃƒÂ¢Ã¢â€šÂ¬Ã‚Â° v124)
    # -----------------------------------------------------------------------
    abaque_check_file <- file.path(current_buse_output_dir, "abaque_partiel.txt")
    if (file.exists(abaque_check_file) && file.info(abaque_check_file)$size > 0) {
      message(paste(">>> BUSE", numero_buse, ": DÃƒÆ’Ã†â€™ÃƒÂ¢Ã¢â€šÂ¬Ã‚Â°JÃƒÆ’Ã†â€™ÃƒÂ¢Ã¢â‚¬Å¡Ã‚Â¬ TRAITÃƒÆ’Ã†â€™ÃƒÂ¢Ã¢â€šÂ¬Ã‚Â°E (Fichier abaque trouvÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â©). PASSAGE ÃƒÆ’Ã†â€™ÃƒÂ¢Ã¢â‚¬Å¡Ã‚Â¬ LA SUIVANTE. <<<"))
      return(NULL)
    }
    # -----------------------------------------------------------------------
    
    CIRC <- buse_row$CIRC
    if(is.na(buse_row$HAUT1) | buse_row$HAUT1==0) buse_row$HAUT1=buse_row$HAUT2
    nozzle_data <- list(LARG = buse_row$LRG, HAUT1 = buse_row$HAUT1, Z1 = buse_row$Z1, Z2 = buse_row$Z2, LENGTH = buse_row$LENGTH)
    first_profile_shape$bottom_width=nozzle_data$LARG*2
    if ("FRIC" %in% names(buse_row) && !is.na(buse_row$FRIC) && buse_row$FRIC > 0) {
      strickler_buse_value <- 1 / buse_row$FRIC
      if (strickler_buse_value < 1) {
        warning(paste("Buse", numero_buse, ": La valeur FRIC=", buse_row$FRIC, "donne un Strickler de", round(strickler_buse_value, 2), ". Utilisation de la valeur par dÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â©faut de 50.0."))
        strickler_buse_value <- 60.0
      }
    } else {
      strickler_buse_value <- 60.0
      warning(paste("Buse", numero_buse, ": Colonne 'FRIC' non valide ou absente. Utilisation de la valeur par dÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â©faut de Strickler:", strickler_buse_value))
    }
    strickler_zones <- c(strickler_zones_base[1], strickler_buse_value, strickler_zones_base[3])
    
    
    message(paste("\n\n############################################################"))
    message(paste("--- DÃƒÆ’Ã†â€™ÃƒÂ¢Ã¢â€šÂ¬Ã‚Â°MARRAGE DU TRAITEMENT POUR LA BUSE :", numero_buse, "---"))
    message(paste("--- Fichiers de sortie dans :", current_buse_output_dir, "---"))
    message(paste("############################################################\n"))
    
    if (nozzle_data$LENGTH < 20) {
      current_pas_maillage <- pas_maillage_court
      current_pas_temps <- pas_temps_court
      duree_palier <- duree_palier_court
      duree_montee <- duree_montee_court
      dist_amont_limite <- 20
      dist_p4_p6 <- 50 
      message("      - Buse courte (< 20m). Maillage fin et pas de temps court sÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â©lectionnÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â©s.")
    } else if (nozzle_data$LENGTH >= 20 && nozzle_data$LENGTH <= 250) {
      current_pas_maillage <- pas_maillage_moyen
      current_pas_temps <- pas_temps_moyen
      duree_palier <- duree_palier_moyen
      duree_montee <- duree_montee_moyen
      dist_amont_limite <- 50
      dist_p4_p6 <- 400
      message("      - Buse moyenne (20-100m). Maillage et pas de temps moyens sÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â©lectionnÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â©s.")
    } else {
      current_pas_maillage <- pas_maillage_long
      current_pas_temps <- pas_temps_long
      duree_palier <- duree_palier_long
      duree_montee <- duree_montee_long
      dist_amont_limite <- 500
      dist_p4_p6 <- 2000 
      message("      - Buse longue (> 100m). Maillage large et pas de temps long sÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â©lectionnÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â©s.")
    }
    
    # BRIDE ANTI-CRASH MASCARET :
    # Si la pente est torrentielle (ex: > 5%), on raccourcit drastiquement le canal aval
    # pour ÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â©viter que MASCARET ne calcule une chute de plusieurs mÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â¨tres qui ferait
    # planter son solveur permanent (SARAP) lors de la remontÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â©e de la ligne d'eau.
    slope_check <- (nozzle_data$Z1 - nozzle_data$Z2) / nozzle_data$LENGTH
    if (slope_check > 0.05) {
      dist_p4_p6 <- 10.0
      message("      - Pente trÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â¨s forte dÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â©tectÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â©e (", sprintf("%.1f", slope_check*100), "%). RÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â©duction dist aval ÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â  10m pour ÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â©viter crash MASCARET.")
    }
    
    I <- (nozzle_data$Z1 - nozzle_data$Z2) / nozzle_data$LENGTH
    if (CIRC == 1) {
      D <- nozzle_data$HAUT1; R <- D / 2; S <- pi * R^2; P <- 2 * pi * R; Rh <- R/2
      Q <- strickler_buse_value * S * (Rh^(2/3)) * sqrt(abs(I))
    } else {
      S <- nozzle_data$LARG * nozzle_data$HAUT1; P <- nozzle_data$LARG + (2 * nozzle_data$HAUT1); Rh <- S / P
      Q <- strickler_buse_value * S * (Rh^(2/3)) * sqrt(abs(I))
    }
    Q <- round(Q)
    
    if (Q < 15) {
      message(paste("      - Le dÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â©bit max estimÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â© (", Q, "mÃƒÆ’Ã¢â‚¬Å¡Ãƒâ€šÃ‚Â³/s) est infÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â©rieur ÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â  15 mÃƒÆ’Ã¢â‚¬Å¡Ãƒâ€šÃ‚Â³/s. Il est forcÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â© ÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â  15 mÃƒÆ’Ã¢â‚¬Å¡Ãƒâ€šÃ‚Â³/s pour la simulation."))
      Q <- 15
    }
    
    Q_sim_max <- Q * 1.5
    if (Q_sim_max > 180) {
      message(paste("      - Le dÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â©bit max de simulation (", round(Q_sim_max), "mÃƒÆ’Ã¢â‚¬Å¡Ãƒâ€šÃ‚Â³/s) dÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â©passe 180 mÃƒÆ’Ã¢â‚¬Å¡Ãƒâ€šÃ‚Â³/s. Il est plafonnÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â© ÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â  180 mÃƒÆ’Ã¢â‚¬Å¡Ãƒâ€šÃ‚Â³/s."))
      Q_sim_max <- 180
    }
    
    debits_q_list <- list()
    current_max_q <- 0
    
    end_q_p1 <- min(Q_sim_max, 1.0)
    if (0.2 <= end_q_p1) {
      debits_q_list[[length(debits_q_list) + 1]] <- seq(0.2, end_q_p1, by = 0.1)
      current_max_q <- end_q_p1
    }
    
    start_val_p2 <- current_max_q + 0.2
    end_q_p2 <- min(Q_sim_max, 10.0)
    if (start_val_p2 <= end_q_p2) {
      debits_q_list[[length(debits_q_list) + 1]] <- seq(start_val_p2, end_q_p2, by = 0.2)
      current_max_q <- end_q_p2
    }
    
    start_val_p3 <- current_max_q + 1
    end_q_p3 <- min(Q_sim_max, 20.0)
    if (start_val_p3 <= end_q_p3) {
      debits_q_list[[length(debits_q_list) + 1]] <- seq(start_val_p3, end_q_p3, by = 1)
      current_max_q <- end_q_p3
    }
    
    start_val_p4 <- current_max_q + 2.0
    end_q_p4 <- min(Q_sim_max, 40.0)
    if (start_val_p4 <= end_q_p4) {
      debits_q_list[[length(debits_q_list) + 1]] <- seq(start_val_p4, end_q_p4, by = 2.0)
      current_max_q <- end_q_p4
    }
    
    start_val_p5 <- current_max_q + 5.0
    end_q_p5 <- Q_sim_max
    if (start_val_p5 <= end_q_p5) {
      debits_q_list[[length(debits_q_list) + 1]] <- seq(start_val_p5, end_q_p5, by = 5.0)
    }
    
    debits_q <- unique(sort(c(unlist(debits_q_list))))
    debits_q <- round(debits_q[debits_q > 0], 2)
    
    if (CIRC == 1) {
      dimension_str <- sprintf("DiamÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â¨tre: %.2f m", nozzle_data$HAUT1)
    } else {
      dimension_str <- sprintf("Largeur: %.2f m, Hauteur: %.2f m", nozzle_data$LARG, nozzle_data$HAUT1)
    }
    pente_pourcent <- I * 100
    title_details_str <- paste(
      dimension_str, 
      sprintf("Longueur: %.1f m", nozzle_data$LENGTH), 
      sprintf("Pente: %.2f %%", pente_pourcent),
      sprintf("Qmax simulÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â©: %.1f mÃƒÆ’Ã¢â‚¬Å¡Ãƒâ€šÃ‚Â³/s", Q_sim_max), 
      sep=" | "
    )
    
    #########################################################################
    # PARTIE 1: GÃƒÆ’Ã†â€™ÃƒÂ¢Ã¢â€šÂ¬Ã‚Â°NÃƒÆ’Ã†â€™ÃƒÂ¢Ã¢â€šÂ¬Ã‚Â°RATION DU FICHIER DE GÃƒÆ’Ã†â€™ÃƒÂ¢Ã¢â€šÂ¬Ã‚Â°OMÃƒÆ’Ã†â€™ÃƒÂ¢Ã¢â€šÂ¬Ã‚Â°TRIE (.geo)
    #########################################################################
    message("--- PARTIE 1 : GÃƒÆ’Ã†â€™ÃƒÂ¢Ã¢â€šÂ¬Ã‚Â°OMÃƒÆ’Ã†â€™ÃƒÂ¢Ã¢â€šÂ¬Ã‚Â°TRIE (.geo) ---")
    
    create_rectangular_profile <- function(nozzle_width, nozzle_height, nozzle_bottom_z, target_center_y, preissmann_slot_width = 0.01, slot_height = 50, bank_y_offset = 10) {
      y_center <- target_center_y
      z_bottom <- nozzle_bottom_z
      z_top <- z_bottom + nozzle_height
      z_slot_top <- z_top + slot_height
      
      y_bottom_left  <- y_center - nozzle_width/2
      y_bottom_right <- y_center + nozzle_width/2
      y_top_left     <- y_bottom_left - 0.01
      y_top_right    <- y_bottom_right + 0.01
      
      y_slot_left    <- y_center - preissmann_slot_width/2
      y_slot_right   <- y_center + preissmann_slot_width/2
      
      y_bank_left    <- y_top_left - bank_y_offset
      y_bank_right   <- y_top_right + bank_y_offset
      
      full_profile_df <- rbind(
        data.frame(Y = y_bank_left, Z = z_slot_top), 
        data.frame(Y = y_slot_left, Z = z_slot_top), 
        data.frame(Y = y_slot_left, Z = z_top), 
        data.frame(Y = y_top_left, Z = z_top), 
        data.frame(Y = y_bottom_left, Z = z_bottom), 
        data.frame(Y = y_bottom_right, Z = z_bottom), 
        data.frame(Y = y_top_right, Z = z_top), 
        data.frame(Y = y_slot_right, Z = z_top), 
        data.frame(Y = y_slot_right, Z = z_slot_top), 
        data.frame(Y = y_bank_right, Z = z_slot_top)
      )
      full_profile_df$Marker <- "B"
      return(full_profile_df[!duplicated(round(full_profile_df[, c("Y", "Z")], 5)), ])
    }
    create_circular_profile <- function(diameter, nozzle_bottom_z, target_center_y, slot_width = 0.01, slot_height = 50, bank_y_offset = 10, n_points_arch = 41) {
      radius <- diameter/2; y_center <- target_center_y; z_center <- nozzle_bottom_z + radius; z_connection <- z_center + sqrt(max(0, radius^2 - (slot_width/2)^2)); z_slot_top <- z_connection + slot_height; y_slot_left <- y_center - slot_width/2; y_slot_right <- y_center + slot_width/2; y_structure_left <- y_center - radius; y_structure_right <- y_center + radius; p_berge_gauche_loin <- data.frame(Y = y_structure_left - bank_y_offset, Z = z_slot_top); p_fente_bas_gauche  <- data.frame(Y = y_slot_left, Z = z_connection); theta_start <- atan2(z_connection - z_center, y_slot_left - y_center); theta_end     <- atan2(z_connection - z_center, y_slot_right - y_center); theta_sequence <- seq(from = theta_start, to = theta_end + 2*pi, length.out = n_points_arch); arch_df <- data.frame(Y = y_center + radius * cos(theta_sequence), Z = z_center + radius * sin(theta_sequence)); p_fente_bas_droite  <- data.frame(Y = y_slot_right, Z = z_connection); p_berge_droite_loin <- data.frame(Y = y_structure_right + bank_y_offset, Z = z_slot_top)
      full_profile_df <- rbind(p_berge_gauche_loin, data.frame(Y = y_slot_left, Z = z_slot_top), p_fente_bas_gauche, arch_df, p_fente_bas_droite, data.frame(Y = y_slot_right, Z = z_slot_top), p_berge_droite_loin)
      full_profile_df$Marker <- "B"
      return(full_profile_df[!duplicated(round(full_profile_df[, c("Y", "Z")], 5)), ])
    }
    
    generate_points_for_trapezoidal <- function(bottom_width, left_slope, right_slope, bottom_elevation, bank_height_above_bottom, target_center_y, bank_y_offset = 10) {
      half_bottom_width <- bottom_width / 2
      top_z <- bottom_elevation + bank_height_above_bottom
      
      # Les points sont d'abord calculÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â©s de maniÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â¨re centrÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â©e autour de Y=0
      y_top_left_rel <- -half_bottom_width - (left_slope * bank_height_above_bottom)
      y_top_right_rel <- half_bottom_width + (right_slope * bank_height_above_bottom)
      y_rel <- c(y_top_left_rel - bank_y_offset, y_top_left_rel, -half_bottom_width, half_bottom_width, y_top_right_rel, y_top_right_rel + bank_y_offset)
      
      # Le thalweg de ce profil relatif est ÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â  Y=0. On dÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â©cale tous les points pour que le thalweg soit ÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â  target_center_y.
      y_abs <- y_rel + target_center_y
      
      z_abs <- c(top_z, top_z, bottom_elevation, bottom_elevation, top_z, top_z)
      points_df <- data.frame(Y = y_abs, Z = z_abs, Marker = "B")
      return(points_df[order(points_df$Y), ])
    }
    
    extract_profile_from_mnt <- function(mnt, center_point_sf, direction_vector, profile_width, num_points = 101) {
      perp_vector <- c(-direction_vector[2], direction_vector[1])
      perp_vector_norm <- perp_vector / sqrt(sum(perp_vector^2))
      start_point <- st_coordinates(center_point_sf) - (profile_width / 2) * perp_vector_norm
      end_point <- st_coordinates(center_point_sf) + (profile_width / 2) * perp_vector_norm
      transect_line <- st_sfc(st_linestring(rbind(start_point, end_point)), crs = st_crs(mnt))
      sampled_points_sf <- st_line_sample(transect_line, n = num_points, type = "regular")
      if (st_is_empty(sampled_points_sf)) return(NULL)
      sampled_points_sf <- st_cast(sampled_points_sf, "POINT")
      sampled_points_vect <- vect(sampled_points_sf)
      extracted_data <- terra::extract(mnt, sampled_points_vect, xy = TRUE, ID = FALSE)
      if (nrow(extracted_data) == 0 || all(is.na(extracted_data[, 1]))) {
        warning(paste0("L'extraction MNT a ÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â©chouÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â© (autour de X=", round(st_coordinates(center_point_sf)[1]),", Y=", round(st_coordinates(center_point_sf)[2]), "). Profil hors emprise?"))
        return(NULL)
      }
      colnames(extracted_data)[1] <- "Z"
      coords <- extracted_data[, c("x", "y")]
      y_relative <- sqrt((coords$x - coords$x[1])^2 + (coords$y - coords$y[1])^2)
      profile_df <- data.frame(Y = y_relative, Z = extracted_data$Z, Marker = "B")
      profile_df <- na.omit(profile_df)
      if (nrow(profile_df) < 2) return(NULL)
      return(profile_df)
    }
    
    shift_profile_z <- function(profile_df, target_min_z) {
      if(is.null(profile_df) || nrow(profile_df) == 0) return(NULL)
      current_min_z <- min(profile_df$Z, na.rm = TRUE)
      delta_z <- target_min_z - current_min_z
      profile_df$Z <- profile_df$Z + delta_z
      return(profile_df)
    }
    
    center_profile_on_thalweg <- function(profile_df, target_y_center) {
      if(is.null(profile_df) || nrow(profile_df) == 0) return(NULL)
      min_z_index <- which.min(profile_df$Z)
      y_of_min_z <- mean(profile_df$Y[min_z_index]) 
      shift_y <- target_y_center - y_of_min_z
      profile_df$Y <- profile_df$Y + shift_y
      return(profile_df)
    }
    
    add_vertical_walls <- function(profile_df) {
      if(is.null(profile_df) || nrow(profile_df) < 2) return(profile_df)
      
      first_point <- profile_df[1, ]
      wall_start_point <- data.frame(Y = first_point$Y, Z = first_point$Z + 20, Marker = "B")
      
      last_point <- profile_df[nrow(profile_df), ]
      wall_end_point <- data.frame(Y = last_point$Y, Z = last_point$Z + 20, Marker = "B")
      
      new_profile <- rbind(wall_start_point, profile_df, wall_end_point)
      return(new_profile)
    }
    
    write_geo_profile <- function(con, bief_name, prof_name, prof_abs, points_df) {
      writeLines(paste0("PROFIL ", bief_name, " ", prof_name, " ", sprintf("%.3f", prof_abs)), con)
      for (j in 1:nrow(points_df)) {
        writeLines(paste(sprintf("%.3f", points_df$Y[j]), sprintf("%.5f", points_df$Z[j]), points_df$Marker[j]), con)
      }
    }
    
    model_total_length <- dist_amont_limite + nozzle_data$LENGTH + dist_p4_p6
    x_nozzle_start <- dist_amont_limite
    x_nozzle_end <- x_nozzle_start + nozzle_data$LENGTH
    x_coords <- c(0, x_nozzle_start - 2.0, x_nozzle_start, x_nozzle_end, x_nozzle_end + 2.0, model_total_length)
    num_profiles_total <- length(x_coords)
    
    buse_geom <- st_geometry(buse_row)
    
    geo_base_filename <- paste0("geom_buse", numero_buse, ifelse(use_mnt_for_profiles, "_MNT", "_Trapeze"))
    geo_filename_with_ext <- paste0(geo_base_filename, ".geo")
    geo_filepath <- file.path(current_buse_output_dir, geo_filename_with_ext)
    message(paste("GÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â©nÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â©ration du fichier de gÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â©omÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â©trie :", geo_filename_with_ext))
    
    bief_name <- "Bief_1"
    indices <- list(p1 = 1, p2 = 2, p3 = 3, p4 = 4, p5 = 5, p6 = 6)
    
    all_profiles <- vector("list", num_profiles_total)
    
    # DÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â©terminer une abscisse Y cible unique pour le centrage de tous les profils.
    half_width_calc <- first_profile_shape$bottom_width / 2 + 
      max(first_profile_shape$left_slope, first_profile_shape$right_slope) * first_profile_shape$bank_height_above_bottom + 
      10.0 # bank_y_offset de la fonction generate_points_for_trapezoidal
    target_center_y <- ceiling(half_width_calc) + 5.0 # Ajout d'une marge de sÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â©curitÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â©
    message(paste("      - Abscisse Y cible pour le centrage de tous les profils:", sprintf("%.3f", target_center_y)))
    
    if(use_mnt_for_profiles) {
      message("      - Mode MNT: Extraction des profils depuis le MNT...")
      
      buse_coords <- st_coordinates(buse_geom)
      if(nrow(buse_coords) < 2) stop("GÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â©omÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â©trie de buse invalide (moins de 2 points) pour la buse ", numero_buse)
      
      start_node <- buse_coords[1, 1:2]
      end_node <- buse_coords[nrow(buse_coords), 1:2]
      direction_vector <- end_node - start_node
      direction_vector_norm <- direction_vector / sqrt(sum(direction_vector^2))
      profile_width_mnt <- 10 * ifelse(CIRC == 1, nozzle_data$HAUT1, nozzle_data$LARG)
      
      pt_extraction_amont <- st_sfc(st_point(start_node - dist_extraction_profil * direction_vector_norm), crs = st_crs(mnt_raster))
      profil_amont_base <- extract_profile_from_mnt(mnt_raster, pt_extraction_amont, direction_vector_norm, profile_width_mnt, num_points_per_profile)
      if(is.null(profil_amont_base)) stop("ÃƒÆ’Ã†â€™ÃƒÂ¢Ã¢â€šÂ¬Ã‚Â°chec de l'extraction du profil de base amont. ArrÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Âªt.")
      
      pt_extraction_aval <- st_sfc(st_point(end_node + dist_extraction_profil * direction_vector_norm), crs = st_crs(mnt_raster))
      profil_aval_base <- extract_profile_from_mnt(mnt_raster, pt_extraction_aval, direction_vector_norm, profile_width_mnt, num_points_per_profile)
      if(is.null(profil_aval_base)) stop("ÃƒÆ’Ã†â€™ÃƒÂ¢Ã¢â€šÂ¬Ã‚Â°chec de l'extraction du profil de base aval. ArrÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Âªt.")
      
      # Altitude des profils en extrapolant la pente de la buse (I)
      
      # Profil 1 (limite amont)
      z_profil_1 <- nozzle_data$Z1 + abs(I) * (x_coords[indices$p3] - x_coords[indices$p1])
      profil_1_vshifted <- shift_profile_z(profil_amont_base, z_profil_1)
      profil_1_centered <- center_profile_on_thalweg(profil_1_vshifted, target_center_y)
      profil_1 <- add_vertical_walls(profil_1_centered)
      all_profiles[[indices$p1]] <- list(name = "1", abscissa = x_coords[indices$p1], data = profil_1)
      
      # Profil 2 (juste avant la buse)
      z_profil_2 <- nozzle_data$Z1 + abs(I) * (x_coords[indices$p3] - x_coords[indices$p2])
      profil_2_vshifted <- shift_profile_z(profil_amont_base, z_profil_2)
      profil_2_centered <- center_profile_on_thalweg(profil_2_vshifted, target_center_y)
      profil_2 <- add_vertical_walls(profil_2_centered)
      all_profiles[[indices$p2]] <- list(name = "2", abscissa = x_coords[indices$p2], data = profil_2)
      
      # Profil 5 (juste aprÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â¨s la buse)
      z_profil_5 <- nozzle_data$Z2 - abs(I) * (x_coords[indices$p5] - x_coords[indices$p4])
      profil_5_vshifted <- shift_profile_z(profil_aval_base, z_profil_5)
      profil_5_centered <- center_profile_on_thalweg(profil_5_vshifted, target_center_y)
      profil_5 <- add_vertical_walls(profil_5_centered)
      all_profiles[[indices$p5]] <- list(name = "5", abscissa = x_coords[indices$p5], data = profil_5)
      
      # Profil 6 (limite aval)
      z_profil_6 <- nozzle_data$Z2 - abs(I) * (x_coords[indices$p6] - x_coords[indices$p4])
      profil_6_vshifted <- shift_profile_z(profil_aval_base, z_profil_6)
      profil_6_centered <- center_profile_on_thalweg(profil_6_vshifted, target_center_y)
      profil_6 <- add_vertical_walls(profil_6_centered)
      all_profiles[[indices$p6]] <- list(name = "6", abscissa = x_coords[indices$p6], data = profil_6)
      
    } else {
      message("      - Mode TrapÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â©zoÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â¯dal: GÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â©nÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â©ration des profils gÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â©nÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â©riques...")
      
      calculated_bottom_z <- sapply(x_coords, function(x) {
        if (x <= x_nozzle_start) return(nozzle_data$Z1 + abs(I) * (x_nozzle_start - x))
        if (x > x_nozzle_start && x < x_nozzle_end) return(nozzle_data$Z1 - I * (x - x_nozzle_start))
        return(nozzle_data$Z2 - abs(I) * (x - x_nozzle_end))
      })
      
      for (k in c(indices$p1, indices$p2, indices$p5, indices$p6)) {
        points_raw <- generate_points_for_trapezoidal(
          bottom_width = first_profile_shape$bottom_width, 
          left_slope = first_profile_shape$left_slope, 
          right_slope = first_profile_shape$right_slope, 
          bottom_elevation = calculated_bottom_z[k], 
          bank_height_above_bottom = first_profile_shape$bank_height_above_bottom,
          target_center_y = target_center_y,
          bank_y_offset = 10.0
        )
        points <- add_vertical_walls(points_raw)
        all_profiles[[k]] <- list(name = as.character(k), abscissa = x_coords[k], data = points)
      }
    }
    
    points_p3 <- if(CIRC == 1) create_circular_profile(nozzle_data$HAUT1, nozzle_data$Z1, target_center_y) else create_rectangular_profile(nozzle_data$LARG, nozzle_data$HAUT1, nozzle_data$Z1, target_center_y, preissmann_slot_width)
    all_profiles[[indices$p3]] <- list(name = "3amont", abscissa = x_coords[indices$p3], data = points_p3)
    
    points_p4 <- if(CIRC == 1) create_circular_profile(nozzle_data$HAUT1, nozzle_data$Z2, target_center_y) else create_rectangular_profile(nozzle_data$LARG, nozzle_data$HAUT1, nozzle_data$Z2, target_center_y, preissmann_slot_width)
    all_profiles[[indices$p4]] <- list(name = "3aval", abscissa = x_coords[indices$p4], data = points_p4)
    
    con <- file(geo_filepath, "wt")
    message("      - ÃƒÆ’Ã†â€™ÃƒÂ¢Ã¢â€šÂ¬Ã‚Â°criture ordonnÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â©e des profils dans le fichier .geo...")
    for (prof in all_profiles) {
      if (!is.null(prof) && !is.null(prof$data)) {
        write_geo_profile(con, bief_name, prof$name, prof$abscissa, prof$data)
      }
    }
    close(con)
    message("--- Fichier .geo gÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â©nÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â©rÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â© avec succÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â¨s. ---")
    
    #########################################################################
    # PARTIE 2: GÃƒÆ’Ã†â€™ÃƒÂ¢Ã¢â€šÂ¬Ã‚Â°NÃƒÆ’Ã†â€™ÃƒÂ¢Ã¢â€šÂ¬Ã‚Â°RATION DES FICHIERS DE LOIS (.loi)
    #########################################################################
    message("\n--- PARTIE 2 : LOIS HYDRAULIQUES (.loi) ---")
    
    duree_totale_sim <- length(debits_q) * (duree_montee + duree_palier)
    hydro_file <- file.path(current_buse_output_dir, "hydrogramme_complet.loi")
    
    con_q <- NULL
    tryCatch({
      con_q <- file(hydro_file, open = "wt")
      writeLines(c("# Hydrogramme...", "# T(s) Q(m3/s)", "S"), con_q)
      current_time <- 0.0
      writeLines(paste(sprintf("%.1f", current_time), "0.20000"), con_q)
      for (q in debits_q) { 
        current_time <- current_time + duree_montee
        writeLines(paste(sprintf("%.1f", current_time), sprintf("%.5f", q)), con_q)
        current_time <- current_time + duree_palier
        writeLines(paste(sprintf("%.1f", current_time), sprintf("%.5f", q)), con_q) 
      }
    }, finally = {
      if(!is.null(con_q)) close(con_q)
    })
    
    message("      - GÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â©nÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â©ration des cotes Z aval selon la logique personnalisÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â©e...")
    cotes_z_aval <- c()
    slope=((nozzle_data$Z1-nozzle_data$Z2)/nozzle_data$LENGTH)
    
    z_start = max(nozzle_data$Z2 - 0.2, nozzle_data$Z2 - (slope*dist_p4_p6)/2)
    z_phase1_end = nozzle_data$Z2
    cotes_z_aval1=seq(from = z_start, to = z_phase1_end, by = 0.5)
    z_phase2_end  <- z_phase1_end + nozzle_data$HAUT1+0.2
    cotes_z_aval2=seq(from = z_phase1_end, to = z_phase2_end, by = 0.1)
    z_phase3_end  <- z_phase1_end + nozzle_data$HAUT1 + 2
    cotes_z_aval3 <- seq(from = z_phase2_end, to = z_phase3_end, by = 0.5)
    cotes_z_aval <- unique(sort(c(cotes_z_aval1,cotes_z_aval2,cotes_z_aval3)))
    #ÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â  enlever
    # z_start=nozzle_data$Z2-(slope*dist_p4_p6)
    # cotes_z_aval<- seq(from = z_start, to = z_phase1_end + nozzle_data$HAUT1+0.5, by = 0.5)
    ##
    message(paste("La cote Z aval minimale sera:", sprintf("%.3f", z_start)))
    message(paste("Nombre de conditions aval gÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â©nÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â©rÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â©es:", length(cotes_z_aval)))
    
    for (z in cotes_z_aval) { 
      filename_z <- file.path(current_buse_output_dir, paste0("limni_Z", sprintf("%.3f", z), ".loi"))
      con_z <- NULL
      tryCatch({
        con_z <- file(filename_z, open = "wt")
        writeLines(c(paste("# Limnigramme... Z=", sprintf("%.3f", z)), "# T(s) Z(m)", "S"), con_z)
        writeLines(paste(sprintf("%.1f", 0.0), sprintf("%.5f", z)), con_z)
        writeLines(paste(sprintf("%.1f", duree_totale_sim), sprintf("%.5f", z)), con_z)
      }, finally = {
        if(!is.null(con_z)) close(con_z)
      })
    }
    message(paste(length(cotes_z_aval), "fichiers .loi gÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â©nÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â©rÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â©s avec succÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â¨s."))
    
    #########################################################################
    # PARTIE 3: GÃƒÆ’Ã†â€™ÃƒÂ¢Ã¢â€šÂ¬Ã‚Â°NÃƒÆ’Ã†â€™ÃƒÂ¢Ã¢â€šÂ¬Ã‚Â°RATION DES FICHIERS DE PARAMÃƒÆ’Ã†â€™Ãƒâ€¹Ã¢â‚¬Â TRES (.xcas)
    #########################################################################
    message("\n--- PARTIE 3 : PARAMÃƒÆ’Ã†â€™Ãƒâ€¹Ã¢â‚¬Â TRES (.xcas) ---")
    limni_files <- list.files(current_buse_output_dir, pattern = "^limni_Z.*\\.loi$", full.names = TRUE)
    
    for (limni_file in limni_files) {
      current_xcas_lines <- readLines(xcas_template_filename, warn = FALSE)
      limni_id <- sub("^limni_(Z.*)\\.loi$", "\\1", basename(limni_file))
      scenario_base_name <- paste0("buse", numero_buse, "_scenario_", limni_id)
      xcas_output_filename <- file.path(current_buse_output_dir, paste0(scenario_base_name, ".xcas"))
      
      pas_stockage_value <- ceiling((duree_montee + duree_palier) / current_pas_temps)
      critere_arret_value <- 1
      
      current_xcas_lines <- gsub("<fichMotsCles>.*</fichMotsCles>", paste0("<fichMotsCles>", scenario_base_name, ".xcas</fichMotsCles>"), current_xcas_lines)
      current_xcas_lines <- gsub("<fichier>.*</fichier>", paste0("<fichier>", geo_filename_with_ext, "</fichier>"), current_xcas_lines)
      current_xcas_lines <- gsub("<abscFin>.*</abscFin>", paste0("<abscFin>", sprintf("%.3f", model_total_length), "</abscFin>"), current_xcas_lines)
      current_xcas_lines <- gsub("<absFinZone>.*</absFinZone>", paste0("<absFinZone>", sprintf("%.3f", model_total_length), "</absFinZone>"), current_xcas_lines)
      current_xcas_lines <- gsub("<pasTemps>.*</pasTemps>", paste0("<pasTemps>", sprintf("%.1f", current_pas_temps), "</pasTemps>"), current_xcas_lines)
      current_xcas_lines <- gsub("<tempsMax>.*</tempsMax>", paste0("<tempsMax>", sprintf("%.1f", duree_totale_sim), "</tempsMax>"), current_xcas_lines)
      current_xcas_lines <- gsub("<critereArret>.*</critereArret>", paste0("<critereArret>", critere_arret_value, "</critereArret>"), current_xcas_lines)
      current_xcas_lines <- gsub("<pasStock>.*</pasStock>", paste0("<pasStock>", sprintf("%.0f", pas_stockage_value), "</pasStock>"), current_xcas_lines)
      current_xcas_lines <- gsub("<fichResultat>.*</fichResultat>", paste0("<fichResultat>", scenario_base_name, "_ecr.opt</fichResultat>"), current_xcas_lines)
      current_xcas_lines <- gsub("<fichListing>.*</fichListing>", paste0("<fichListing>", scenario_base_name, ".lis</fichListing>"), current_xcas_lines)
      current_xcas_lines <- gsub("<valeursPas>.*</valeursPas>",paste0("<valeursPas>",sprintf("%.2f", val_pas_planimetrage),"</valeursPas>"), current_xcas_lines)
      current_xcas_lines <- gsub("<numDerProf>.*</numDerProf>", paste0("<numDerProf>", num_profiles_total, "</numDerProf>"), current_xcas_lines)
      
      pas_maillage_str <- paste(c(current_pas_maillage[1], current_pas_maillage[2], current_pas_maillage[2], current_pas_maillage[2], current_pas_maillage[3]), collapse = " ")
      current_xcas_lines <- gsub("<nbPlages>.*</nbPlages>", "<nbPlages>5</nbPlages>", current_xcas_lines)
      current_xcas_lines <- gsub("<num1erProfPlage>.*</num1erProfPlage>", "<num1erProfPlage>1 2 3 4 5</num1erProfPlage>", current_xcas_lines)
      current_xcas_lines <- gsub("<numDerProfPlage>.*</numDerProfPlage>", paste0("<numDerProfPlage>2 3 4 5 ", num_profiles_total, "</numDerProfPlage>"), current_xcas_lines)
      current_xcas_lines <- gsub("<pasEspacePlage>.*</pasEspacePlage>", paste0("<pasEspacePlage>", pas_maillage_str, "</pasEspacePlage>"), current_xcas_lines)
      
      start_frottement_idx <- grep("<frottement>", current_xcas_lines)[1]
      end_frottement_idx <- grep("</frottement>", current_xcas_lines)[1]
      if (!is.na(start_frottement_idx) && !is.na(end_frottement_idx)) {
        abs_deb_zone_str <- paste(sprintf("%.3f", x_coords[c(1, 3, 4)]), collapse = " ")
        abs_fin_zone_str <- paste(sprintf("%.3f", x_coords[c(3, 4, 6)]), collapse = " ")
        strickler_zones_str <- paste(sprintf("%.1f", strickler_zones), collapse = " ")
        frottement_block <- c("      <frottement>", "        <loi>1</loi>", "        <nbZone>3</nbZone>", "        <numBranche>1 1 1</numBranche>", paste0("        <absDebZone>", abs_deb_zone_str, "</absDebZone>"), paste0("        <absFinZone>", abs_fin_zone_str, "</absFinZone>"), paste0("        <coefLitMin>", strickler_zones_str, "</coefLitMin>"), paste0("        <coefLitMaj>", strickler_zones_str, "</coefLitMaj>"), "      </frottement>")
        current_xcas_lines <- c(current_xcas_lines[1:(start_frottement_idx - 1)], frottement_block, current_xcas_lines[(end_frottement_idx + 1):length(current_xcas_lines)])
      }
      
      start_index <- grep("<parametresLoisHydrauliques>", current_xcas_lines)[1]
      end_index <- grep("</parametresLoisHydrauliques>", current_xcas_lines)[1]
      if (!is.na(start_index) && !is.na(end_index)) {
        lois_lines <- c("  <parametresLoisHydrauliques>","    <nb>2</nb>","    <lois>","      <structureParametresLoi>","        <nom>hydro_amont</nom>","        <type>1</type>","        <donnees>","          <modeEntree>1</modeEntree>",paste0("          <fichier>", basename(hydro_file), "</fichier>"),"        </donnees>","      </structureParametresLoi>","      <structureParametresLoi>",paste0("        <nom>limni_", limni_id, "</nom>"),"        <type>2</type>","        <donnees>","          <modeEntree>1</modeEntree>",paste0("          <fichier>", basename(limni_file), "</fichier>"),"        </donnees>","      </structureParametresLoi>","    </lois>","  </parametresLoisHydrauliques>")
        current_xcas_lines <- c(current_xcas_lines[1:(start_index-1)], lois_lines, current_xcas_lines[(end_index+1):length(current_xcas_lines)])
      }
      
      con <- NULL
      tryCatch({
        con <- file(xcas_output_filename, "w")
        writeLines(current_xcas_lines, con)
      }, finally = {
        if(!is.null(con)) close(con)
      })
    }
    message("--- Fichiers .xcas gÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â©nÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â©rÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â©s avec succÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â¨s. ---")
    
    # PARTIE 4
    message("\n--- PARTIE 4 : LANCEMENT DES SIMULATIONS ---")
    xcas_files <- dir_ls(current_buse_output_dir, regexp = "\\.xcas$", recurse = FALSE)
    registerDoParallel(cores = num_cores_sim)
    foreach( xcas_file_path = xcas_files, .combine = 'c', .errorhandling = 'pass') %dopar% {
      library(fs)
      xcas_filename <- path_file(xcas_file_path)
      xcas_basename <- path_ext_remove(xcas_filename)
      temp_sim_dir <- path(current_buse_output_dir, paste0("temp_", xcas_basename))
      results_dir <- path(current_buse_output_dir, "results_mascaret")
      
      final_opt_path <- path(results_dir, paste0(xcas_basename, "_ecr.opt"))
      
      if(!file_exists(final_opt_path) || file_info(final_opt_path)$size == 0) {
        dir_create(temp_sim_dir)
        file_copy(xcas_file_path, path(temp_sim_dir, xcas_filename), overwrite = TRUE)
        file_copy(path(current_buse_output_dir, geo_filename_with_ext), temp_sim_dir, overwrite = TRUE)
        file_copy(path(current_buse_output_dir, "hydrogramme_complet.loi"), temp_sim_dir, overwrite = TRUE)
        limni_id_for_copy <- sub(paste0("buse", numero_buse, "_scenario_"), "", xcas_basename)
        limni_to_copy <- path(current_buse_output_dir, paste0("limni_", limni_id_for_copy, ".loi"))
        if(file_exists(limni_to_copy)) file_copy(limni_to_copy, temp_sim_dir, overwrite = TRUE)
        
        batch_content <- sprintf('@echo off\nCALL "%s"\ncd /D "%%~dp0"\npython "%%HOMETEL%%\\scripts\\python3\\mascaret.py" "%s"\nexit /b %%ERRORLEVEL%%', telemac_env_script, xcas_filename)
        batch_file_path <- path(temp_sim_dir, "run_simulation.bat")
        writeLines(batch_content, batch_file_path)
        
        system2("cmd", args = c("/c", path_abs(batch_file_path)), stdout = TRUE, stderr = TRUE)
      }
    }
    stopImplicitCluster()
    message("--- Lancement des simulations terminÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â©. ---")
    
    # NOUVELLE PARTIE 4.5: Copie des rÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â©sultats aprÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â¨s la fin de tous les calculs
    message("\n--- PARTIE 4.5 : COLLECTE DES RÃƒÆ’Ã†â€™ÃƒÂ¢Ã¢â€šÂ¬Ã‚Â°SULTATS ---")
    results_dir <- path(current_buse_output_dir, "results_mascaret")
    dir_create(results_dir)
    temp_dirs <- dir_ls(current_buse_output_dir, regexp = "temp_buse", type = "directory")
    
    for(temp_dir in temp_dirs) {
      opt_files_in_temp <- dir_ls(temp_dir, regexp = "\\.(opt|lis)$")
      if(length(opt_files_in_temp) > 0) {
        file_copy(opt_files_in_temp, results_dir, overwrite = TRUE)
      }
    }
    message("--- Collecte des fichiers .opt terminÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â©e. ---")
    
    
    # PARTIE 5
    message("\n--- PARTIE 5 : LECTURE DES RÃƒÆ’Ã†â€™ÃƒÂ¢Ã¢â€šÂ¬Ã‚Â°SULTATS ET CRÃƒÆ’Ã†â€™ÃƒÂ¢Ã¢â€šÂ¬Ã‚Â°ATION DE L'ABAQUE ---")
    opt_dir <- file.path(current_buse_output_dir, "results_mascaret")
    
    ABSCISSA_BUSE_ENTREE_TARGET <- x_coords[3]
    ABSCISSA_BUSE_SORTIE_TARGET <- x_coords[4]
    
    opt_files <- list.files(opt_dir, pattern = "\\.opt$", full.names = TRUE)
    results_list <- list()
    
    for (opt_file in opt_files) {
      opt_content_lines <- readLines(opt_file, warn = FALSE)
      resultats_start_line_index <- grep("[resultats]", opt_content_lines, fixed = TRUE)
      if (length(resultats_start_line_index) == 0) next
      opt_data_text <- opt_content_lines[(resultats_start_line_index[1] + 1):length(opt_content_lines)]
      if(length(opt_data_text) == 0 || all(nchar(trimws(opt_data_text)) == 0)) next
      
      opt_data <- tryCatch({ fread(text = opt_data_text, sep = ";", strip.white=TRUE, header = FALSE, colClasses = "numeric", fill = TRUE, select = c(1, 4, 6, 11, 12, 14), col.names = c("Time", "Abscissa", "S_value", "Froude", "Velocity", "DebitTotal")) }, error = function(e) { NULL })
      if(is.null(opt_data) || nrow(opt_data) == 0) next
      
      plateau_end_times <- (1:length(debits_q)) * (duree_montee + duree_palier)
      
      for(time_step in plateau_end_times){
        time_to_extract <- time_step - current_pas_temps
        if(time_to_extract < 0 || time_to_extract > max(opt_data$Time)) next
        
        steady_state_data <- opt_data[abs(opt_data$Time - time_to_extract) < 1e-3, ]
        if(nrow(steady_state_data) == 0) next
        
        entree_buse_row <- steady_state_data[which.min(abs(steady_state_data$Abscissa - ABSCISSA_BUSE_ENTREE_TARGET)), ]
        sortie_buse_row <- steady_state_data[which.min(abs(steady_state_data$Abscissa - ABSCISSA_BUSE_SORTIE_TARGET)), ]
        
        if(nrow(entree_buse_row) == 1 && nrow(sortie_buse_row) == 1){
          avg_velocity <- (entree_buse_row$Velocity + sortie_buse_row$Velocity) / 2
          
          results_list[[length(results_list) + 1]] <- data.frame(
            Debit = entree_buse_row$DebitTotal, 
            S1 = entree_buse_row$S_value, 
            S2 = sortie_buse_row$S_value,
            Froude_amont = entree_buse_row$Froude,
            Avg_Velocity = avg_velocity
          )
        }
      }
    }
    
    results_for_file <- data.frame()
    all_results_df <- data.frame()
    results_df_sorted <- data.frame()
    
    if (length(results_list) > 0) {
      all_results_df <- do.call(rbind, results_list)
      s1_max_threshold <- nozzle_data$Z1 + nozzle_data$HAUT1 + 10
      
      all_results_filtered <- all_results_df[
        all_results_df$S1 <= s1_max_threshold & 
          all_results_df$Froude_amont <= 5 &
          # all_results_df$Avg_Velocity < 7.0 &
          (all_results_df$S1 - all_results_df$S2) >= 0.05,
      ]
      if(nrow(all_results_filtered) > 0) {
        results_df_unique <- unique(all_results_filtered)
        results_df_sorted <- results_df_unique[order(results_df_unique$Debit, results_df_unique$S1), ]
        
        final_points_list <- lapply(split(results_df_sorted, results_df_sorted$Debit), function(df_debit) {
          torrential_points <- df_debit %>% filter(Froude_amont >= 0.8)
          if (nrow(torrential_points) > 0) return(torrential_points)
          good_fluvial_points <- df_debit %>% filter(Froude_amont < 0.8 & S1 < (nozzle_data$Z1 + nozzle_data$HAUT1) & S2 < (nozzle_data$Z2 + nozzle_data$HAUT1))
          if (nrow(good_fluvial_points) > 0) return(good_fluvial_points)
          return(df_debit %>% filter(Froude_amont < 0.8))
        })
        
        results_for_file <- bind_rows(final_points_list) %>% unique() %>% arrange(Debit, S1)
        if(length(which(results_for_file$Froude_amont>=1))>0){
          max_debit_torrential=max(results_for_file$Debit[which(results_for_file$Froude_amont>=1)])
          max_S1_torrentiel=max(results_for_file$S1[which(results_for_file$Debit==max_debit_torrential)])
          max_S2_torrentiel=max(results_for_file$S2[which(results_for_file$Debit==max_debit_torrential)])
          
          points_fluviaux1=which(results_for_file$Debit<max_debit_torrential & (results_for_file$S1 >max_S1_torrentiel | results_for_file$S2 >max_S2_torrentiel ) )
          points_fluviaux2=which(results_for_file$Debit>max_debit_torrential & results_for_file$S2 >nozzle_data$Z2 + nozzle_data$HAUT1 + 0.05  )
          if(length(points_fluviaux1)>0 | length(points_fluviaux2)>0){
            results_for_file=results_for_file[-c(points_fluviaux1,points_fluviaux2),]
            results_for_file <- results_for_file %>% unique() %>% arrange(Debit, S1)
          }
        }
        
        # --- NETTOYAGE ET STRICTE MONOTONIE HYDRAULIQUE (Q et S1) ---
        if (nrow(results_for_file) > 0) {
          
          # On trie impÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â©rativement par S1 (hauteur d'eau) en premier lieu pour s'assurer
          # que la courbe part toujours du point le plus bas (le radier) sans sauter de points ÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â  cause du tri par dÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â©bit
          results_for_file <- results_for_file[order(results_for_file$S1, results_for_file$Debit), ]
          
          # Filtrage ultra-simple pour garantir la stricte croissance de S1 requise par MASCARET
          if (nrow(results_for_file) > 1) {
            indices_valides <- c(1)
            derniere_cote <- results_for_file$S1[1]
            
            for (idx_pt in 2:nrow(results_for_file)) {
              if (results_for_file$S1[idx_pt] > derniere_cote) {
                indices_valides <- c(indices_valides, idx_pt)
                derniere_cote <- results_for_file$S1[idx_pt]
              }
            }
            results_for_file <- results_for_file[indices_valides, ]
          }
        }
        
        output_txt_file_S_partiel <- file.path(current_buse_output_dir, "abaque_partiel.txt")
        if (nrow(results_for_file) > 0) {
          file_conn_partiel <- file(output_txt_file_S_partiel, "w")
          writeLines(c(as.character(numero_buse), as.character(nrow(results_for_file))), file_conn_partiel)
          cat(paste(sprintf("%.2f", results_for_file$Debit), collapse = ","), "\n", file = file_conn_partiel)
          cat(paste(sprintf("%.2f", results_for_file$S1), collapse = ","), "\n", file = file_conn_partiel)
          cat(paste(sprintf("%.2f", results_for_file$S2), collapse = ","), "\n", file = file_conn_partiel)
          close(file_conn_partiel)
        }
        
        output_txt_file_QS1_partiel <- file.path(current_buse_output_dir, "abaque_partiel_QS1.txt")
        if (nrow(results_for_file) > 0) {
          results_QS1_unique <- results_for_file %>% dplyr::select(Debit, S1) %>% distinct() %>% arrange(Debit, S1)
          file_conn_QS1 <- file(output_txt_file_QS1_partiel, "w")
          writeLines(c(as.character(numero_buse), as.character(nrow(results_QS1_unique))), file_conn_QS1)
          cat(paste(sprintf("%.2f", results_QS1_unique$Debit), collapse = ","), "\n", file = file_conn_QS1)
          cat(paste(sprintf("%.2f", results_QS1_unique$S1), collapse = ","), "\n", file = file_conn_QS1)
          close(file_conn_QS1)
        }
      }
    }
    
    if(generate_long_profile_plots) {
      message("\n--- GÃƒÆ’Ã†â€™ÃƒÂ¢Ã¢â€šÂ¬Ã‚Â°NÃƒÆ’Ã†â€™ÃƒÂ¢Ã¢â€šÂ¬Ã‚Â°RATION DES PROFILS EN LONG ---")
      for (opt_file in opt_files) {
        scenario_name <- sub("_ecr\\.opt$", "", basename(opt_file)); opt_content_lines <- readLines(opt_file, warn = FALSE); resultats_start_line_index <- grep("[resultats]", opt_content_lines, fixed = TRUE); if (length(resultats_start_line_index) == 0) next
        opt_data_full <- tryCatch({ fread(text = opt_content_lines[(resultats_start_line_index[1] + 1):length(opt_content_lines)], sep = ";", strip.white=TRUE, header = FALSE, colClasses = "numeric", fill = TRUE, select = c(1, 4, 5, 6, 14), col.names = c("Time", "Abscissa", "Z_fond", "Z_eau", "DebitTotal")) }, error = function(e) { NULL })
        if(is.null(opt_data_full) || nrow(opt_data_full) == 0) next
        fig_pl <- plot_ly(); profil_fond <- opt_data_full[abs(Time - min(Time)) < 1e-3, ]; fig_pl <- add_trace(fig_pl, data = profil_fond, x = ~Abscissa, y = ~Z_fond, type = 'scatter', mode = 'lines', line = list(color = 'black', width = 3), name = 'Fond')
        for (time_p in ((1:length(debits_q)) * (duree_montee + duree_palier) - current_pas_temps)) { if (time_p > 0 && time_p <= max(opt_data_full$Time)) { steady_data <- opt_data_full[abs(opt_data_full$Time - time_p) < 1e-3, ]; if (nrow(steady_data) > 0) fig_pl <- add_trace(fig_pl, data = steady_data, x = ~Abscissa, y = ~Z_eau, type = 'scatter', mode = 'lines', name = paste('Q =', round(steady_data$DebitTotal[1], 2), 'mÃƒÆ’Ã¢â‚¬Å¡Ãƒâ€šÃ‚Â³/s')) } }
        y_range <- range(c(profil_fond$Z_fond, opt_data_full$Z_eau), na.rm = TRUE); fig_pl <- fig_pl %>% layout(title = scenario_name, xaxis = list(title = "Abscisse"), yaxis = list(title = "Altitude", range = y_range), shapes = list(list(type = 'line', x0 = x_coords[indices$p3], x1 = x_coords[indices$p3], y0 = y_range[1], y1 = y_range[2], line = list(color = 'grey', dash = 'dash')), list(type = 'line', x0 = x_coords[indices$p4], x1 = x_coords[indices$p4], y0 = y_range[1], y1 = y_range[2], line = list(color = 'grey', dash = 'dash')), list(type = 'line', x0 = x_coords[indices$p3], x1 = x_coords[indices$p4], y0 = nozzle_data$Z1 + nozzle_data$HAUT1, y1 = nozzle_data$Z2 + nozzle_data$HAUT1, line = list(color = 'red', dash = 'longdash'))))
        saveWidget(fig_pl, file = file.path(opt_dir, paste0("profil_en_long_", scenario_name, ".html")), selfcontained = TRUE)
      }
    }
    
    
    if(generate_abaque_plots) {
      message("\n--- PARTIE 6 : GÃƒÆ’Ã†â€™ÃƒÂ¢Ã¢â€šÂ¬Ã‚Â°NÃƒÆ’Ã†â€™ÃƒÂ¢Ã¢â€šÂ¬Ã‚Â°RATION DES GRAPHIQUES D'ABAQUE ---")
      
      # Utiliser `all_results_df` pour dÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â©finir les points ÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â  estomper
      points_to_keep <- results_for_file
      points_to_fade <- dplyr::anti_join(all_results_df, points_to_keep, by = c("Debit", "S1", "S2", "Froude_amont", "Avg_Velocity"))
      
      if (nrow(points_to_keep) > 0 || nrow(points_to_fade) > 0) {
        
        # PrÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â©paration des donnÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â©es pour les points principaux
        results_df_graph <- points_to_keep
        if(nrow(results_df_graph) > 0) {
          colnames(results_df_graph) <- c("Debit", "S1_Charge_Amont", "S2_Charge_Aval", "Froude_amont", "Avg_Velocity")
          results_df_graph$Froude_Condition <- cut(results_df_graph$Froude_amont, 
                                                   breaks = c(-Inf, 0.99, 1.01, Inf), 
                                                   labels = c("Fluvial (Fr < 1)", "Critique (Fr = 1)", "Torrentiel (Fr > 1)"))
        }
        
        # --- Graphique 3D ---
        title_3d <- paste("Relation DÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â©bit-Charges S1/S2 - Buse", numero_buse, "<br>", title_details_str)
        fig_3d_S <- plot_ly()
        
        if(nrow(points_to_fade) > 0) {
          fig_3d_S <- fig_3d_S %>% add_trace(
            data = points_to_fade, x=~Debit, y=~S2, z=~S1,
            type='scatter3d', mode='markers',
            marker=list(size=3, color='rgba(211, 211, 211, 0.5)'), # Gris clair, semi-transparent
            name = "Points exclus",
            hoverinfo = "text",
            text = ~paste('Exclu<br>DÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â©bit:', sprintf("%.2f", Debit), '<br>S1:', sprintf("%.2f", S1), '<br>S2:', sprintf("%.2f", S2), '<br>Froude:', sprintf("%.2f", Froude_amont))
          )
        }
        
        if(nrow(results_df_graph) > 0) {
          fig_3d_S <- fig_3d_S %>% add_trace(
            data = results_df_graph, x=~Debit, y=~S2_Charge_Aval, z=~S1_Charge_Amont,
            type='scatter3d', mode='markers',
            color=~S1_Charge_Amont, colorscale='Viridis',
            symbol=~Froude_Condition, symbols=c('circle','square','triangle-up'),
            marker=list(size=4),
            customdata=~Froude_amont,
            name = "Points de l'abaque",
            hovertemplate=paste('<b>DÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â©bit:</b> %{x:.2f} mÃƒÆ’Ã¢â‚¬Å¡Ãƒâ€šÃ‚Â³/s<br>',
                                '<b>S2 (Aval):</b> %{y:.2f} m<br>',
                                '<b>S1 (Amont):</b> %{z:.2f} m<br>',
                                '<b>Froude (Amont):</b> %{customdata:.2f}<br>',
                                '<extra></extra>')
          )
        }
        
        fig_3d_S <- fig_3d_S %>% layout(title=title_3d,
                                        legend=list(title=list(text='<b>RÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â©gime Froude</b>')),
                                        scene=list(xaxis=list(title="DÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â©bit (mÃƒÆ’Ã¢â‚¬Å¡Ãƒâ€šÃ‚Â³/s)"),
                                                   yaxis=list(title="S2 Charge Aval (m)"),
                                                   zaxis=list(title="S1 Charge Amont (m)")))
        
        # --- Graphique 2D ---
        title_2d <- paste("Relation S1-S2 - Buse", numero_buse, "<br>", title_details_str)
        fig_2d_S <- plot_ly()
        
        if(nrow(points_to_fade) > 0) {
          fig_2d_S <- fig_2d_S %>% add_trace(
            data = points_to_fade, x=~S1, y=~S2,
            type='scatter', mode='markers',
            marker=list(size=5, color='rgba(211, 211, 211, 0.6)'), # Gris clair, semi-transparent
            name = "Points exclus",
            hoverinfo = "text",
            text = ~paste('Exclu<br>DÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â©bit:', sprintf("%.2f", Debit), '<br>S1:', sprintf("%.2f", S1), '<br>S2:', sprintf("%.2f", S2), '<br>Froude:', sprintf("%.2f", Froude_amont))
          )
        }
        
        if(nrow(results_df_graph) > 0) {
          fig_2d_S <- fig_2d_S %>% add_trace(
            data = results_df_graph, x=~S1_Charge_Amont, y=~S2_Charge_Aval,
            type='scatter', mode='markers',
            color=~Debit, colorscale='Plasma',
            symbol=~Froude_Condition, symbols=c('circle','square','triangle-up'),
            marker=list(size=6),
            customdata=~Froude_amont,
            name = "Points de l'abaque",
            hovertemplate=paste('<b>S1 (Amont):</b> %{x:.2f} m<br>',
                                '<b>S2 (Aval):</b> %{y:.2f} m<br>',
                                '<b>DÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â©bit:</b> %{marker.color:.2f} mÃƒÆ’Ã¢â‚¬Å¡Ãƒâ€šÃ‚Â³/s<br>',
                                '<b>Froude:</b> %{customdata:.2f}<br>',
                                '<extra></extra>')
          )
          
          shapes_list <- list(
            list(type = 'line', x0 = nozzle_data$Z1, x1 = nozzle_data$Z1, y0 = min(results_df_graph$S2_Charge_Aval), y1 = max(results_df_graph$S2_Charge_Aval), line = list(color = 'blue', dash = 'longdash', width = 2)),
            list(type = 'line', x0 = nozzle_data$Z1 + nozzle_data$HAUT1, x1 = nozzle_data$Z1 + nozzle_data$HAUT1, y0 = min(results_df_graph$S2_Charge_Aval), y1 = max(results_df_graph$S2_Charge_Aval), line = list(color = 'red', dash = 'longdash', width = 2)),
            list(type = 'line', y0 = nozzle_data$Z2, y1 = nozzle_data$Z2, x0 = min(results_df_graph$S1_Charge_Amont), x1 = max(results_df_graph$S1_Charge_Amont), line = list(color = 'blue', dash = 'longdash', width = 2)),
            list(type = 'line', y0 = nozzle_data$Z2 + nozzle_data$HAUT1, y1 = nozzle_data$Z2 + nozzle_data$HAUT1, x0 = min(results_df_graph$S1_Charge_Amont), x1 = max(results_df_graph$S1_Charge_Amont), line = list(color = 'red', dash = 'longdash', width = 2))
          )
          fig_2d_S <- fig_2d_S %>% layout(shapes = shapes_list)
        }
        
        fig_2d_S <- fig_2d_S %>% layout(
          title=title_2d,
          xaxis=list(title="S1 Charge Amont (m)"),
          yaxis=list(title="S2 Charge Aval (m)"),
          legend=list(title=list(text='<b>RÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â©gime Froude</b>'))
        )
        
        htmlwidgets::saveWidget(fig_3d_S, file.path(current_buse_output_dir, "graph_abaque_S_3d.html"), selfcontained = TRUE)
        htmlwidgets::saveWidget(fig_2d_S, file.path(current_buse_output_dir, "graph_abaque_S_2d.html"), selfcontained = TRUE)
        message("Graphiques d'abaque 2D et 3D (incluant les points exclus) exportÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â©s.")
        
      } else {
        message("Aucune donnÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â©e ÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â  visualiser, les graphiques d'abaque n'ont pas ÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â©tÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â© gÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â©nÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â©rÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â©s pour cette buse.")
      }
    }
    
    # --- BLOC DE NETTOYAGE SÃƒÆ’Ã†â€™ÃƒÂ¢Ã¢â€šÂ¬Ã‚Â°LECTIF (AJOUTÃƒÆ’Ã†â€™ÃƒÂ¢Ã¢â€šÂ¬Ã‚Â° v123) ---
    if (cleanup_calc_files) {
      message(paste("      - Nettoyage sÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â©lectif pour la buse", numero_buse))
      
      # 1. Identifier tous les dossiers temporaires temp_...
      tdirs <- fs::dir_ls(current_buse_output_dir, regexp = "temp_", type = "directory")
      if (length(tdirs) > 1) {
        # Garder uniquement le premier dossier pour vÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â©rification, supprimer les autres
        fs::dir_delete(tdirs[-1]) 
      }
      
      # 2. Supprimer le dossier global results_mascaret (doublon inutile une fois les donnÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â©es extraites)
      res_fold <- file.path(current_buse_output_dir, "results_mascaret")
      if(fs::dir_exists(res_fold)) fs::dir_delete(res_fold)
      
      # 3. Supprimer les fichiers d'entrÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â©e ÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â  la racine (dÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â©jÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â  sauvegardÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â©s dans le dossier temp conservÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â©)
      root_files <- fs::dir_ls(current_buse_output_dir, regexp = "\\.(xcas|geo|loi|lis)$")
      if(length(root_files) > 0) fs::file_delete(root_files)
    }
  } 
  
  #########################################################################
  # DÃƒÆ’Ã†â€™ÃƒÂ¢Ã¢â€šÂ¬Ã‚Â°MARRAGE DE LA BOUCLE PRINCIPALE
  #########################################################################
  if (ncore_buse <= 1) {
    message("--- DÃƒÆ’Ã†â€™ÃƒÂ¢Ã¢â€šÂ¬Ã‚Â°MARRAGE DU TRAITEMENT SÃƒÆ’Ã†â€™ÃƒÂ¢Ã¢â€šÂ¬Ã‚Â°QUENTIEL ---")
    for (i in 1:nrow(busel_data)) {
      abaques_check_file <- file.path(dsnlayerC, "abaques2.txt")
      if (file.exists(abaques_check_file) && file.info(abaques_check_file)$size > 0) {
        message(paste(">>> BUSE", abaques_check_file, ": DÃƒÆ’Ã†â€™ÃƒÂ¢Ã¢â€šÂ¬Ã‚Â°JÃƒÆ’Ã†â€™ÃƒÂ¢Ã¢â‚¬Å¡Ã‚Â¬ TRAITÃƒÆ’Ã†â€™ÃƒÂ¢Ã¢â€šÂ¬Ã‚Â°E (Fichier abaque trouvÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â©). PASSAGE ÃƒÆ’Ã†â€™ÃƒÂ¢Ã¢â‚¬Å¡Ã‚Â¬ LA SUIVANTE. <<<"))
        next
      }
      process_buse(i, busel_data, main_output_dir, telemac_env_script, xcas_template_filename, use_mnt_for_profiles, mnt_path, dist_amont_limite, dist_p4_p6, num_points_per_profile, dist_extraction_profil, first_profile_shape, duree_palier, duree_montee, num_cores_sim, strickler_zones_base, val_pas_planimetrage, g, pas_maillage_court, pas_temps_court, pas_maillage_moyen, pas_temps_moyen, pas_maillage_long, pas_temps_long, preissmann_slot_width, generate_long_profile_plots, generate_abaque_plots, cleanup_calc_files)
    }
  } else {
    message(paste("--- DÃƒÆ’Ã†â€™ÃƒÂ¢Ã¢â€šÂ¬Ã‚Â°MARRAGE DU TRAITEMENT PARALLÃƒÆ’Ã†â€™Ãƒâ€¹Ã¢â‚¬Â LE SUR", ncore_buse, "CÃƒÆ’Ã¢â‚¬Â¦ÃƒÂ¢Ã¢â€šÂ¬Ã¢â€žÂ¢URS ---"))
    registerDoParallel(cores = ncore_buse)
    foreach(i = 1:nrow(busel_data)) %dopar% { 
      process_buse(i, busel_data, main_output_dir, telemac_env_script, xcas_template_filename, use_mnt_for_profiles, mnt_path, dist_amont_limite, dist_p4_p6, num_points_per_profile, dist_extraction_profil, first_profile_shape, duree_palier, duree_montee, num_cores_sim, strickler_zones_base, val_pas_planimetrage, g, pas_maillage_court, pas_temps_court, pas_maillage_moyen, pas_temps_moyen, pas_maillage_long, pas_temps_long, preissmann_slot_width, generate_long_profile_plots, generate_abaque_plots, cleanup_calc_files) 
    }
    stopImplicitCluster()
  }
  
  #########################################################################
  # PARTIE 7: ASSEMBLAGE FINAL DE L'ABAQUE
  #########################################################################
  message("\n--- PARTIE 7 : ASSEMBLAGE DU FICHIER D'ABAQUE FINAL ---")
  output_txt_file_S_global <- file.path(main_output_dir, "resultats_buse_format_abaqueSQ.txt")
  file_conn_main <- file(output_txt_file_S_global, "w")
  writeLines("#abaqueSQ", file_conn_main)
  output_txt_file_QS1_global <- file.path(main_output_dir, "abaques.txt")
  file_conn_qs1 <- file(output_txt_file_QS1_global, "w")
  writeLines("#abaqueQS1", file_conn_qs1)
  
  for (i in 1:nrow(busel_data)) {
    numero_buse <- busel_data$id[i]
    current_buse_output_dir <- file.path(main_output_dir, paste0("buse_", numero_buse))
    
    p1 <- file.path(current_buse_output_dir, "abaque_partiel.txt")
    if (file.exists(p1)) writeLines(readLines(p1), file_conn_main)
    
    p2 <- file.path(current_buse_output_dir, "abaque_partiel_QS1.txt")
    if (file.exists(p2)) writeLines(readLines(p2), file_conn_qs1)
  }
  
  close(file_conn_main); close(file_conn_qs1)
  message("\n--- WORKFLOW COMPLET TERMINÃƒÆ’Ã†â€™ÃƒÂ¢Ã¢â€šÂ¬Ã‚Â° ---")
  file.copy(from = file.path(main_output_dir, "abaques.txt"),to = dsnlayerC,overwrite = TRUE,recursive = TRUE)
  # 1. Lire toutes les lignes du fichier d'origine
  lignes <- readLines(file.path(dsnlayerC, "abaques.txt"))
  
  # 2. Nettoyer les ÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â©ventuelles lignes vides ÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â  la fin du fichier
  lignes <- lignes[lignes != ""]
  
  # 3. Calculer le nombre de buses
  # On soustrait 1 (pour la ligne de commentaire) avant de diviser par 4
  n_buses <- floor((length(lignes) - 1) / 4)
  
  # 4. GÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â©nÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â©rer les indices des IDs
  # On commence ÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â  la ligne 2, et on fait des sauts de 4
  indices_id <- seq(2, by = 4, length.out = n_buses)
  
  # 5. Remplacer les lignes d'identifiants par la nouvelle numÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â©rotation
  lignes[indices_id] <- 1:n_buses
  
  # 6. Sauvegarder le rÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â©sultat en ÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â©crasant le fichier
  writeLines(as.character(lignes), file.path(dsnlayerC, "abaques.txt"))
  # Script R pour lisser les abaques Telemac-2D
  # Objectif : Garantir une croissance stricte des hauteurs d'eau (S1) 
  # pour ÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â©viter les divisions par zÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â©ro dans DISCHARGE_INTERPOL.
  
  # Noms des fichiers (ÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â  adapter si besoin)
  file_in <- file.path(dsnlayerC, "abaques.txt")
  file_out <- file.path(dsnlayerC, "abaques2.txt")
  
  # Lecture du fichier
  lines <- readLines(file_in, warn = FALSE)
  # Nettoyage des lignes vides ÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â©ventuelles
  lines <- lines[trimws(lines) != ""]
  
  # On conserve l'entÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Âªte (ex: #abaqueQS1)
  output_lines <- c(lines[1])
  
  idx <- 2
  while (idx <= length(lines)) {
    # Extraction du bloc de 4 lignes
    abaque_id <- trimws(lines[idx])
    n_pts_old <- as.integer(trimws(lines[idx + 1]))
    q_str     <- lines[idx + 2]
    s1_str    <- lines[idx + 3]
    
    # Conversion des lignes Q et S1 en vecteurs numÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â©riques
    q_vals  <- as.numeric(unlist(strsplit(q_str, ",")))
    s1_vals <- as.numeric(unlist(strsplit(s1_str, ",")))
    
    # Construction du dataframe pour nettoyage
    df_abaque <- data.frame(Debit = q_vals, S1 = s1_vals)
    
    # Filtrage ultra-simple : on parcourt les points dans leur ordre naturel
    # et on ne garde que ceux oÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â¹ la cote d'eau (S1) augmente strictement.
    # On ne trie pas, et on ne modifie JAMAIS les valeurs de dÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â©bit !
    if (nrow(df_abaque) > 1) {
      indices_valides <- c(1) # On garde toujours le 1er point (fond)
      derniere_cote <- df_abaque$S1[1]
      
      for (idx_pt in 2:nrow(df_abaque)) {
        if (df_abaque$S1[idx_pt] > derniere_cote) {
          indices_valides <- c(indices_valides, idx_pt)
          derniere_cote <- df_abaque$S1[idx_pt]
        }
      }
      df_abaque <- df_abaque[indices_valides, ]
    }
    
    # 4. Interpolation et Extrapolation pour boucher TOUS les trous
    if (nrow(df_abaque) > 0) {
      buse_info <- busel_data[busel_data$id == as.numeric(abaque_id), ]
      if (nrow(buse_info) > 0) {
        Z1 <- buse_info$Z1[1]
        
        # Liste complete theorique des debits (selon ton pas de base)
        Q_max_actuel <- max(df_abaque$Debit)
        q_theorique <- c(seq(0.2, min(1.0, Q_max_actuel), by = 0.1),
                         seq(1.2, min(10.0, Q_max_actuel), by = 0.2),
                         seq(11.0, min(20.0, Q_max_actuel), by = 1.0),
                         seq(22.0, min(40.0, Q_max_actuel), by = 2.0),
                         seq(45.0, Q_max_actuel, by = 5.0))
        q_theorique <- unique(sort(round(q_theorique, 2)))
        
        # On ajoute Z1 a Q=0 pour l'extrapolation vers le bas
        df_full <- data.frame(Debit = c(0, q_theorique))
        df_abaque <- merge(df_full, df_abaque, by = "Debit", all.x = TRUE)
        df_abaque$S1[df_abaque$Debit == 0] <- Z1
        
        # Interpolation lineaire pour tous les NA (trous au milieu + vers le bas)
        if (any(is.na(df_abaque$S1))) {
          # approx() interpole lineairement entre les points non-NA
          valeurs_interpolees <- approx(df_abaque$Debit[!is.na(df_abaque$S1)], 
                                        df_abaque$S1[!is.na(df_abaque$S1)], 
                                        xout = df_abaque$Debit)
          df_abaque$S1 <- round(valeurs_interpolees$y, 2)
        }
        
        # On retire le Q=0 qui ne servait que pour interpoler proprement
        df_abaque <- df_abaque[df_abaque$Debit > 0, ]
      }
    }
    
    n_pts  <- nrow(df_abaque)
    q_out  <- paste(sprintf("%.2f", df_abaque$Debit), collapse = ",")
    s1_out <- paste(sprintf("%.2f", df_abaque$S1), collapse = ",")
    
    # Stockage des 4 lignes du bloc corrigÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â©
    output_lines <- c(output_lines, abaque_id, as.character(n_pts), q_out, s1_out)
    
    # On passe ÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â  l'abaque suivant
    idx <- idx + 4
  }
  
  # ÃƒÆ’Ã†â€™ÃƒÂ¢Ã¢â€šÂ¬Ã‚Â°criture du nouveau fichier
  writeLines(output_lines, file_out)
  cat("Traitement terminÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â© avec succÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â¨s !\nLe fichier corrigÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â© est :", file_out, "\n")
}
abacus=function(){
  library(sf)
  library(terra)
  library(dplyr)
  library(fs)
  library(stringr)
  library(foreach)
  library(doParallel)
  library(data.table)
  library(plotly)
  library(htmlwidgets)
  
  #########################################################################
  # PARTIE 0: PARAMÈTRES UTILISATEUR
  # Modifiez les valeurs dans cette section pour configurer votre simulation.
  #########################################################################
  # --- 0.1. Configuration des chemins et fichiers ---
  # chem_routine <- "C:/Users/CartoNationale/Desktop/Nabil"
  
  main_output_dir <- file.path(dsnlayerC, "abaques") 
  telemac_env_script <- "C:/TELEMAC/V8P4/configs/pysource.bat"
  xcas_template_filename <- file.path(chem_routine,"C2D",Doss_Base, "masc.xcas.txt")
  
  # --- 0.2. Paramètres de parallélisation ---
  ncore_buse <- ncore_buse_abaques # Nombre de cœurs pour la boucle sur les buses. 1 = séquentiel, >=2 = parallèle
  num_cores_sim <- num_cores_sim_abaques
  # Nombre de cœurs pour les simulations d'une MÊME buse
  
  # --- 0.3. Contrôle de la géométrie --- 
  use_mnt_for_profiles <- FALSE # Mettre à TRUE pour extraire du MNT, FALSE pour utiliser des profils trapézoïdaux
  
  # --- Chemins pour le mode MNT ---
  shapefile_path <- file.path(dsnlayerC, "OuvHydrau_Select.shp") 
  mnt_path <- file.path(dsnlayerC, "MNTFinal.gpkg") # <--- CHEMIN VERS VOTRE MNT
  
  # --- 0.4. Paramètres de la géométrie ---
  dist_amont_limite <- 1000.0
  dist_p4_p6 <- 2000 # Distance entre la sortie de la buse (corrigée à p5) et la limite aval (p6)
  num_points_per_profile <- 101
  dist_extraction_profil <- 3.0 
  preissmann_slot_width <- 0.01 # Largeur de la fente de Preissmann en mètres
  
  first_profile_shape <- list(
    type = "trapezoidal", bottom_width = 3, left_slope = 1/1.5,
    right_slope = 1/1.5, bank_height_above_bottom = 40
  )
  
  # --- 0.5. Paramètres temporels et hydrauliques ---
  duree_palier <- 400
  duree_montee <- 50
  strickler_zones_base <- c(20.0, 20.0, 20.0) 
  val_pas_planimetrage <- 0.02
  g <- 9.81
  
  # --- 0.6. Paramètres de maillage et de temps de simulation adaptatifs ---
  # Pour buses COURTES (< 20m)
  pas_maillage_court <- c(1, 0.5, 1)
  pas_temps_court <- 0.5
  duree_palier_court <- 20
  duree_montee_court <- 10
  
  # Pour buses MOYENNES (20m à 100m)
  pas_maillage_moyen <- c(1, 1, 1)
  pas_temps_moyen <- 1
  duree_palier_moyen <- 100
  duree_montee_moyen <- 20
  # Pour buses LONGUES (> 100m)
  pas_maillage_long <- c(2, 2, 2)
  pas_temps_long <- 2
  duree_palier_long <- 400
  duree_montee_long <- 50
  # --- 0.7. Paramètres de sortie (NOUVEAU) ---
  generate_long_profile_plots <- TRUE # Mettre à TRUE pour générer les graphiques de profil en long
  generate_abaque_plots <- TRUE       # Mettre à TRUE pour générer les graphiques d'abaque 2D/3D
  cleanup_calc_files <- FALSE           # <--- NOUVEAU: TRUE pour garder 1 dossier temp et supprimer le reste
  
  
  #########################################################################
  # LECTURE DES DONNÉES SPATIALES ET INITIALISATION
  #########################################################################
  message("--- LECTURE DES DONNÉES SPATIALES ---")
  if (!file.exists(shapefile_path)) stop("Le fichier shapefile n'a pas été trouvé: ", shapefile_path)
  busel_data <- st_read(shapefile_path, quiet = TRUE)
  message(paste("Lecture de", nrow(busel_data), "buse(s) depuis le shapefile."))
  
  mnt_raster <- NULL # Initialisation à NULL
  if(use_mnt_for_profiles) {
    if (!file.exists(mnt_path)) stop("Le fichier MNT n'a pas été trouvé: ", mnt_path)
    message("Chargement du MNT pour vérification CRS...")
    temp_mnt <- rast(mnt_path)
    message("MNT chargé.")
    if (st_crs(busel_data) != st_crs(temp_mnt)) {
      message("ATTENTION: CRS différents. Reprojection du shapefile vers le CRS du MNT.")
      busel_data <- st_transform(busel_data, st_crs(temp_mnt))
    }
    rm(temp_mnt) # On supprime l'objet, il sera rechargé dans chaque worker
    gc() # Force le nettoyage de la mémoire
  }
  
  dir.create(main_output_dir, showWarnings = FALSE, recursive = TRUE)
  
  #########################################################################
  # DÉFINITION DE LA FONCTION DE TRAITEMENT D'UNE BUSE
  #########################################################################
  process_buse <- function(i, busel_data, main_output_dir, telemac_env_script, xcas_template_filename, use_mnt_for_profiles, mnt_path, dist_amont_limite, dist_p4_p6, num_points_per_profile, dist_extraction_profil, first_profile_shape, duree_palier, duree_montee, num_cores_sim, strickler_zones_base, val_pas_planimetrage, g, pas_maillage_court, pas_temps_court, pas_maillage_moyen, pas_temps_moyen, pas_maillage_long, pas_temps_long, preissmann_slot_width, generate_long_profile_plots, generate_abaque_plots, cleanup_calc_files) {
    
    # Charger les librairies nécessaires dans l'environnement parallèle
    library(sf)
    library(terra)
    library(dplyr)
    library(fs)
    library(stringr)
    library(foreach)
    library(doParallel)
    library(data.table)
    library(plotly)
    library(htmlwidgets)
    
    # Si on utilise le MNT, chaque worker charge sa propre copie.
    if (use_mnt_for_profiles) {
      mnt_raster <- rast(mnt_path)
    }
    
    buse_row <- busel_data[i, ]
    numero_buse <- buse_row$id
    
    # Création du dossier de sortie
    current_buse_output_dir <- file.path(main_output_dir, paste0("buse_", numero_buse))
    dir.create(current_buse_output_dir, showWarnings = FALSE, recursive = TRUE)
    
    # -----------------------------------------------------------------------
    # CHECK DE DÉJÀ FAIT (NOUVELLE FONCTIONNALITÉ v124)
    # -----------------------------------------------------------------------
    abaque_check_file <- file.path(current_buse_output_dir, "abaque_partiel.txt")
    if (file.exists(abaque_check_file) && file.info(abaque_check_file)$size > 0) {
      message(paste(">>> BUSE", numero_buse, ": DÉJÀ TRAITÉE (Fichier abaque trouvé). PASSAGE À LA SUIVANTE. <<<"))
      return(NULL)
    }
    # -----------------------------------------------------------------------
    
    CIRC <- buse_row$CIRC
    if(is.na(buse_row$HAUT1) | buse_row$HAUT1==0) buse_row$HAUT1=buse_row$HAUT2
    nozzle_data <- list(LARG = buse_row$LRG, HAUT1 = buse_row$HAUT1, Z1 = buse_row$Z1, Z2 = buse_row$Z2, LENGTH = buse_row$LENGTH)
    first_profile_shape$bottom_width=nozzle_data$LARG*2
    if ("FRIC" %in% names(buse_row) && !is.na(buse_row$FRIC) && buse_row$FRIC > 0) {
      strickler_buse_value <- 1 / buse_row$FRIC
      if (strickler_buse_value < 1) {
        warning(paste("Buse", numero_buse, ": La valeur FRIC=", buse_row$FRIC, "donne un Strickler de", round(strickler_buse_value, 2), ". Utilisation de la valeur par défaut de 50.0."))
        strickler_buse_value <- 60.0
      }
    } else {
      strickler_buse_value <- 60.0
      warning(paste("Buse", numero_buse, ": Colonne 'FRIC' non valide ou absente. Utilisation de la valeur par défaut de Strickler:", strickler_buse_value))
    }
    strickler_zones <- c(strickler_zones_base[1], strickler_buse_value, strickler_zones_base[3])
    
    
    message(paste("\n\n############################################################"))
    message(paste("--- DÉMARRAGE DU TRAITEMENT POUR LA BUSE :", numero_buse, "---"))
    message(paste("--- Fichiers de sortie dans :", current_buse_output_dir, "---"))
    message(paste("############################################################\n"))
    
    if (nozzle_data$LENGTH < 20) {
      current_pas_maillage <- pas_maillage_court
      current_pas_temps <- pas_temps_court
      duree_palier <- duree_palier_court
      duree_montee <- duree_montee_court
      dist_amont_limite <- 20
      dist_p4_p6 <- 50 
      message("      - Buse courte (< 20m). Maillage fin et pas de temps court sélectionnés.")
    } else if (nozzle_data$LENGTH >= 20 && nozzle_data$LENGTH <= 250) {
      current_pas_maillage <- pas_maillage_moyen
      current_pas_temps <- pas_temps_moyen
      duree_palier <- duree_palier_moyen
      duree_montee <- duree_montee_moyen
      dist_amont_limite <- 50
      dist_p4_p6 <- 400
      message("      - Buse moyenne (20-100m). Maillage et pas de temps moyens sélectionnés.")
    } else {
      current_pas_maillage <- pas_maillage_long
      current_pas_temps <- pas_temps_long
      duree_palier <- duree_palier_long
      duree_montee <- duree_montee_long
      dist_amont_limite <- 500
      dist_p4_p6 <- 2000 
      message("      - Buse longue (> 100m). Maillage large et pas de temps long sélectionnés.")
    }
    
    # BRIDE ANTI-CRASH MASCARET :
    # Si la pente est torrentielle (ex: > 5%), on raccourcit drastiquement le canal aval
    # pour éviter que MASCARET ne calcule une chute de plusieurs mètres qui ferait
    # planter son solveur permanent (SARAP) lors de la remontée de la ligne d'eau.
    slope_check <- (nozzle_data$Z1 - nozzle_data$Z2) / nozzle_data$LENGTH
    if (slope_check > 0.05) {
      dist_p4_p6 <- 10.0
      message("      - Pente très forte détectée (", sprintf("%.1f", slope_check*100), "%). Réduction dist aval à 10m pour éviter crash MASCARET.")
    }
    
    I <- (nozzle_data$Z1 - nozzle_data$Z2) / nozzle_data$LENGTH
    if (CIRC == 1) {
      D <- nozzle_data$HAUT1; R <- D / 2; S <- pi * R^2; P <- 2 * pi * R; Rh <- R/2
      Q <- strickler_buse_value * S * (Rh^(2/3)) * sqrt(abs(I))
    } else {
      S <- nozzle_data$LARG * nozzle_data$HAUT1; P <- nozzle_data$LARG + (2 * nozzle_data$HAUT1); Rh <- S / P
      Q <- strickler_buse_value * S * (Rh^(2/3)) * sqrt(abs(I))
    }
    Q <- round(Q)
    
    if (Q < 15) {
      message(paste("      - Le débit max estimé (", Q, "m³/s) est inférieur à 15 m³/s. Il est forcé à 15 m³/s pour la simulation."))
      Q <- 15
    }
    
    Q_sim_max <- Q * 1.5
    if (Q_sim_max > 180) {
      message(paste("      - Le débit max de simulation (", round(Q_sim_max), "m³/s) dépasse 180 m³/s. Il est plafonné à 180 m³/s."))
      Q_sim_max <- 180
    }
    
    debits_q_list <- list()
    current_max_q <- 0
    
    end_q_p1 <- min(Q_sim_max, 1.0)
    if (0.2 <= end_q_p1) {
      debits_q_list[[length(debits_q_list) + 1]] <- seq(0.2, end_q_p1, by = 0.1)
      current_max_q <- end_q_p1
    }
    
    start_val_p2 <- current_max_q + 0.2
    end_q_p2 <- min(Q_sim_max, 10.0)
    if (start_val_p2 <= end_q_p2) {
      debits_q_list[[length(debits_q_list) + 1]] <- seq(start_val_p2, end_q_p2, by = 0.2)
      current_max_q <- end_q_p2
    }
    
    start_val_p3 <- current_max_q + 1
    end_q_p3 <- min(Q_sim_max, 20.0)
    if (start_val_p3 <= end_q_p3) {
      debits_q_list[[length(debits_q_list) + 1]] <- seq(start_val_p3, end_q_p3, by = 1)
      current_max_q <- end_q_p3
    }
    
    start_val_p4 <- current_max_q + 2.0
    end_q_p4 <- min(Q_sim_max, 40.0)
    if (start_val_p4 <= end_q_p4) {
      debits_q_list[[length(debits_q_list) + 1]] <- seq(start_val_p4, end_q_p4, by = 2.0)
      current_max_q <- end_q_p4
    }
    
    start_val_p5 <- current_max_q + 5.0
    end_q_p5 <- Q_sim_max
    if (start_val_p5 <= end_q_p5) {
      debits_q_list[[length(debits_q_list) + 1]] <- seq(start_val_p5, end_q_p5, by = 5.0)
    }
    
    debits_q <- unique(sort(c(unlist(debits_q_list))))
    debits_q <- round(debits_q[debits_q > 0], 2)
    
    if (CIRC == 1) {
      dimension_str <- sprintf("Diamètre: %.2f m", nozzle_data$HAUT1)
    } else {
      dimension_str <- sprintf("Largeur: %.2f m, Hauteur: %.2f m", nozzle_data$LARG, nozzle_data$HAUT1)
    }
    pente_pourcent <- I * 100
    title_details_str <- paste(
      dimension_str, 
      sprintf("Longueur: %.1f m", nozzle_data$LENGTH), 
      sprintf("Pente: %.2f %%", pente_pourcent),
      sprintf("Qmax simulé: %.1f m³/s", Q_sim_max), 
      sep=" | "
    )
    
    #########################################################################
    # PARTIE 1: GÉNÉRATION DU FICHIER DE GÉOMÉTRIE (.geo)
    #########################################################################
    message("--- PARTIE 1 : GÉOMÉTRIE (.geo) ---")
    
    create_rectangular_profile <- function(nozzle_width, nozzle_height, nozzle_bottom_z, target_center_y, preissmann_slot_width = 0.01, slot_height = 50, bank_y_offset = 10) {
      y_center <- target_center_y
      z_bottom <- nozzle_bottom_z
      z_top <- z_bottom + nozzle_height
      z_slot_top <- z_top + slot_height
      
      y_bottom_left  <- y_center - nozzle_width/2
      y_bottom_right <- y_center + nozzle_width/2
      y_top_left     <- y_bottom_left - 0.01
      y_top_right    <- y_bottom_right + 0.01
      
      y_slot_left    <- y_center - preissmann_slot_width/2
      y_slot_right   <- y_center + preissmann_slot_width/2
      
      y_bank_left    <- y_top_left - bank_y_offset
      y_bank_right   <- y_top_right + bank_y_offset
      
      full_profile_df <- rbind(
        data.frame(Y = y_bank_left, Z = z_slot_top), 
        data.frame(Y = y_slot_left, Z = z_slot_top), 
        data.frame(Y = y_slot_left, Z = z_top), 
        data.frame(Y = y_top_left, Z = z_top), 
        data.frame(Y = y_bottom_left, Z = z_bottom), 
        data.frame(Y = y_bottom_right, Z = z_bottom), 
        data.frame(Y = y_top_right, Z = z_top), 
        data.frame(Y = y_slot_right, Z = z_top), 
        data.frame(Y = y_slot_right, Z = z_slot_top), 
        data.frame(Y = y_bank_right, Z = z_slot_top)
      )
      full_profile_df$Marker <- "B"
      return(full_profile_df[!duplicated(round(full_profile_df[, c("Y", "Z")], 5)), ])
    }
    create_circular_profile <- function(diameter, nozzle_bottom_z, target_center_y, slot_width = 0.01, slot_height = 50, bank_y_offset = 10, n_points_arch = 41) {
      radius <- diameter/2; y_center <- target_center_y; z_center <- nozzle_bottom_z + radius; z_connection <- z_center + sqrt(max(0, radius^2 - (slot_width/2)^2)); z_slot_top <- z_connection + slot_height; y_slot_left <- y_center - slot_width/2; y_slot_right <- y_center + slot_width/2; y_structure_left <- y_center - radius; y_structure_right <- y_center + radius; p_berge_gauche_loin <- data.frame(Y = y_structure_left - bank_y_offset, Z = z_slot_top); p_fente_bas_gauche  <- data.frame(Y = y_slot_left, Z = z_connection); theta_start <- atan2(z_connection - z_center, y_slot_left - y_center); theta_end     <- atan2(z_connection - z_center, y_slot_right - y_center); theta_sequence <- seq(from = theta_start, to = theta_end + 2*pi, length.out = n_points_arch); arch_df <- data.frame(Y = y_center + radius * cos(theta_sequence), Z = z_center + radius * sin(theta_sequence)); p_fente_bas_droite  <- data.frame(Y = y_slot_right, Z = z_connection); p_berge_droite_loin <- data.frame(Y = y_structure_right + bank_y_offset, Z = z_slot_top)
      full_profile_df <- rbind(p_berge_gauche_loin, data.frame(Y = y_slot_left, Z = z_slot_top), p_fente_bas_gauche, arch_df, p_fente_bas_droite, data.frame(Y = y_slot_right, Z = z_slot_top), p_berge_droite_loin)
      full_profile_df$Marker <- "B"
      return(full_profile_df[!duplicated(round(full_profile_df[, c("Y", "Z")], 5)), ])
    }
    
    generate_points_for_trapezoidal <- function(bottom_width, left_slope, right_slope, bottom_elevation, bank_height_above_bottom, target_center_y, bank_y_offset = 10) {
      half_bottom_width <- bottom_width / 2
      top_z <- bottom_elevation + bank_height_above_bottom
      
      # Les points sont d'abord calculés de manière centrée autour de Y=0
      y_top_left_rel <- -half_bottom_width - (left_slope * bank_height_above_bottom)
      y_top_right_rel <- half_bottom_width + (right_slope * bank_height_above_bottom)
      y_rel <- c(y_top_left_rel - bank_y_offset, y_top_left_rel, -half_bottom_width, half_bottom_width, y_top_right_rel, y_top_right_rel + bank_y_offset)
      
      # Le thalweg de ce profil relatif est à Y=0. On décale tous les points pour que le thalweg soit à target_center_y.
      y_abs <- y_rel + target_center_y
      
      z_abs <- c(top_z, top_z, bottom_elevation, bottom_elevation, top_z, top_z)
      points_df <- data.frame(Y = y_abs, Z = z_abs, Marker = "B")
      return(points_df[order(points_df$Y), ])
    }
    
    extract_profile_from_mnt <- function(mnt, center_point_sf, direction_vector, profile_width, num_points = 101) {
      perp_vector <- c(-direction_vector[2], direction_vector[1])
      perp_vector_norm <- perp_vector / sqrt(sum(perp_vector^2))
      start_point <- st_coordinates(center_point_sf) - (profile_width / 2) * perp_vector_norm
      end_point <- st_coordinates(center_point_sf) + (profile_width / 2) * perp_vector_norm
      transect_line <- st_sfc(st_linestring(rbind(start_point, end_point)), crs = st_crs(mnt))
      sampled_points_sf <- st_line_sample(transect_line, n = num_points, type = "regular")
      if (st_is_empty(sampled_points_sf)) return(NULL)
      sampled_points_sf <- st_cast(sampled_points_sf, "POINT")
      sampled_points_vect <- vect(sampled_points_sf)
      extracted_data <- terra::extract(mnt, sampled_points_vect, xy = TRUE, ID = FALSE)
      if (nrow(extracted_data) == 0 || all(is.na(extracted_data[, 1]))) {
        warning(paste0("L'extraction MNT a échoué (autour de X=", round(st_coordinates(center_point_sf)[1]),", Y=", round(st_coordinates(center_point_sf)[2]), "). Profil hors emprise?"))
        return(NULL)
      }
      colnames(extracted_data)[1] <- "Z"
      coords <- extracted_data[, c("x", "y")]
      y_relative <- sqrt((coords$x - coords$x[1])^2 + (coords$y - coords$y[1])^2)
      profile_df <- data.frame(Y = y_relative, Z = extracted_data$Z, Marker = "B")
      profile_df <- na.omit(profile_df)
      if (nrow(profile_df) < 2) return(NULL)
      return(profile_df)
    }
    
    shift_profile_z <- function(profile_df, target_min_z) {
      if(is.null(profile_df) || nrow(profile_df) == 0) return(NULL)
      current_min_z <- min(profile_df$Z, na.rm = TRUE)
      delta_z <- target_min_z - current_min_z
      profile_df$Z <- profile_df$Z + delta_z
      return(profile_df)
    }
    
    center_profile_on_thalweg <- function(profile_df, target_y_center) {
      if(is.null(profile_df) || nrow(profile_df) == 0) return(NULL)
      min_z_index <- which.min(profile_df$Z)
      y_of_min_z <- mean(profile_df$Y[min_z_index]) 
      shift_y <- target_y_center - y_of_min_z
      profile_df$Y <- profile_df$Y + shift_y
      return(profile_df)
    }
    
    add_vertical_walls <- function(profile_df) {
      if(is.null(profile_df) || nrow(profile_df) < 2) return(profile_df)
      
      first_point <- profile_df[1, ]
      wall_start_point <- data.frame(Y = first_point$Y, Z = first_point$Z + 20, Marker = "B")
      
      last_point <- profile_df[nrow(profile_df), ]
      wall_end_point <- data.frame(Y = last_point$Y, Z = last_point$Z + 20, Marker = "B")
      
      new_profile <- rbind(wall_start_point, profile_df, wall_end_point)
      return(new_profile)
    }
    
    write_geo_profile <- function(con, bief_name, prof_name, prof_abs, points_df) {
      writeLines(paste0("PROFIL ", bief_name, " ", prof_name, " ", sprintf("%.3f", prof_abs)), con)
      for (j in 1:nrow(points_df)) {
        writeLines(paste(sprintf("%.3f", points_df$Y[j]), sprintf("%.5f", points_df$Z[j]), points_df$Marker[j]), con)
      }
    }
    
    model_total_length <- dist_amont_limite + nozzle_data$LENGTH + dist_p4_p6
    x_nozzle_start <- dist_amont_limite
    x_nozzle_end <- x_nozzle_start + nozzle_data$LENGTH
    x_coords <- c(0, x_nozzle_start - 2.0, x_nozzle_start, x_nozzle_end, x_nozzle_end + 2.0, model_total_length)
    num_profiles_total <- length(x_coords)
    
    buse_geom <- st_geometry(buse_row)
    
    geo_base_filename <- paste0("geom_buse", numero_buse, ifelse(use_mnt_for_profiles, "_MNT", "_Trapeze"))
    geo_filename_with_ext <- paste0(geo_base_filename, ".geo")
    geo_filepath <- file.path(current_buse_output_dir, geo_filename_with_ext)
    message(paste("Génération du fichier de géométrie :", geo_filename_with_ext))
    
    bief_name <- "Bief_1"
    indices <- list(p1 = 1, p2 = 2, p3 = 3, p4 = 4, p5 = 5, p6 = 6)
    
    all_profiles <- vector("list", num_profiles_total)
    
    # Déterminer une abscisse Y cible unique pour le centrage de tous les profils.
    half_width_calc <- first_profile_shape$bottom_width / 2 + 
      max(first_profile_shape$left_slope, first_profile_shape$right_slope) * first_profile_shape$bank_height_above_bottom + 
      10.0 # bank_y_offset de la fonction generate_points_for_trapezoidal
    target_center_y <- ceiling(half_width_calc) + 5.0 # Ajout d'une marge de sécurité
    message(paste("      - Abscisse Y cible pour le centrage de tous les profils:", sprintf("%.3f", target_center_y)))
    
    if(use_mnt_for_profiles) {
      message("      - Mode MNT: Extraction des profils depuis le MNT...")
      
      buse_coords <- st_coordinates(buse_geom)
      if(nrow(buse_coords) < 2) stop("Géométrie de buse invalide (moins de 2 points) pour la buse ", numero_buse)
      
      start_node <- buse_coords[1, 1:2]
      end_node <- buse_coords[nrow(buse_coords), 1:2]
      direction_vector <- end_node - start_node
      direction_vector_norm <- direction_vector / sqrt(sum(direction_vector^2))
      profile_width_mnt <- 10 * ifelse(CIRC == 1, nozzle_data$HAUT1, nozzle_data$LARG)
      
      pt_extraction_amont <- st_sfc(st_point(start_node - dist_extraction_profil * direction_vector_norm), crs = st_crs(mnt_raster))
      profil_amont_base <- extract_profile_from_mnt(mnt_raster, pt_extraction_amont, direction_vector_norm, profile_width_mnt, num_points_per_profile)
      if(is.null(profil_amont_base)) stop("Échec de l'extraction du profil de base amont. Arrêt.")
      
      pt_extraction_aval <- st_sfc(st_point(end_node + dist_extraction_profil * direction_vector_norm), crs = st_crs(mnt_raster))
      profil_aval_base <- extract_profile_from_mnt(mnt_raster, pt_extraction_aval, direction_vector_norm, profile_width_mnt, num_points_per_profile)
      if(is.null(profil_aval_base)) stop("Échec de l'extraction du profil de base aval. Arrêt.")
      
      # Altitude des profils en extrapolant la pente de la buse (I)
      
      # Profil 1 (limite amont)
      z_profil_1 <- nozzle_data$Z1 + abs(I) * (x_coords[indices$p3] - x_coords[indices$p1])
      profil_1_vshifted <- shift_profile_z(profil_amont_base, z_profil_1)
      profil_1_centered <- center_profile_on_thalweg(profil_1_vshifted, target_center_y)
      profil_1 <- add_vertical_walls(profil_1_centered)
      all_profiles[[indices$p1]] <- list(name = "1", abscissa = x_coords[indices$p1], data = profil_1)
      
      # Profil 2 (juste avant la buse)
      z_profil_2 <- nozzle_data$Z1 + abs(I) * (x_coords[indices$p3] - x_coords[indices$p2])
      profil_2_vshifted <- shift_profile_z(profil_amont_base, z_profil_2)
      profil_2_centered <- center_profile_on_thalweg(profil_2_vshifted, target_center_y)
      profil_2 <- add_vertical_walls(profil_2_centered)
      all_profiles[[indices$p2]] <- list(name = "2", abscissa = x_coords[indices$p2], data = profil_2)
      
      # Profil 5 (juste après la buse)
      z_profil_5 <- nozzle_data$Z2 - abs(I) * (x_coords[indices$p5] - x_coords[indices$p4])
      profil_5_vshifted <- shift_profile_z(profil_aval_base, z_profil_5)
      profil_5_centered <- center_profile_on_thalweg(profil_5_vshifted, target_center_y)
      profil_5 <- add_vertical_walls(profil_5_centered)
      all_profiles[[indices$p5]] <- list(name = "5", abscissa = x_coords[indices$p5], data = profil_5)
      
      # Profil 6 (limite aval)
      z_profil_6 <- nozzle_data$Z2 - abs(I) * (x_coords[indices$p6] - x_coords[indices$p4])
      profil_6_vshifted <- shift_profile_z(profil_aval_base, z_profil_6)
      profil_6_centered <- center_profile_on_thalweg(profil_6_vshifted, target_center_y)
      profil_6 <- add_vertical_walls(profil_6_centered)
      all_profiles[[indices$p6]] <- list(name = "6", abscissa = x_coords[indices$p6], data = profil_6)
      
    } else {
      message("      - Mode Trapézoïdal: Génération des profils génériques...")
      
      calculated_bottom_z <- sapply(x_coords, function(x) {
        if (x <= x_nozzle_start) return(nozzle_data$Z1 + abs(I) * (x_nozzle_start - x))
        if (x > x_nozzle_start && x < x_nozzle_end) return(nozzle_data$Z1 - I * (x - x_nozzle_start))
        return(nozzle_data$Z2 - abs(I) * (x - x_nozzle_end))
      })
      
      for (k in c(indices$p1, indices$p2, indices$p5, indices$p6)) {
        points_raw <- generate_points_for_trapezoidal(
          bottom_width = first_profile_shape$bottom_width, 
          left_slope = first_profile_shape$left_slope, 
          right_slope = first_profile_shape$right_slope, 
          bottom_elevation = calculated_bottom_z[k], 
          bank_height_above_bottom = first_profile_shape$bank_height_above_bottom,
          target_center_y = target_center_y,
          bank_y_offset = 10.0
        )
        points <- add_vertical_walls(points_raw)
        all_profiles[[k]] <- list(name = as.character(k), abscissa = x_coords[k], data = points)
      }
    }
    
    points_p3 <- if(CIRC == 1) create_circular_profile(nozzle_data$HAUT1, nozzle_data$Z1, target_center_y) else create_rectangular_profile(nozzle_data$LARG, nozzle_data$HAUT1, nozzle_data$Z1, target_center_y, preissmann_slot_width)
    all_profiles[[indices$p3]] <- list(name = "3amont", abscissa = x_coords[indices$p3], data = points_p3)
    
    points_p4 <- if(CIRC == 1) create_circular_profile(nozzle_data$HAUT1, nozzle_data$Z2, target_center_y) else create_rectangular_profile(nozzle_data$LARG, nozzle_data$HAUT1, nozzle_data$Z2, target_center_y, preissmann_slot_width)
    all_profiles[[indices$p4]] <- list(name = "3aval", abscissa = x_coords[indices$p4], data = points_p4)
    
    con <- file(geo_filepath, "wt")
    message("      - Écriture ordonnée des profils dans le fichier .geo...")
    for (prof in all_profiles) {
      if (!is.null(prof) && !is.null(prof$data)) {
        write_geo_profile(con, bief_name, prof$name, prof$abscissa, prof$data)
      }
    }
    close(con)
    message("--- Fichier .geo généré avec succès. ---")
    
    #########################################################################
    # PARTIE 2: GÉNÉRATION DES FICHIERS DE LOIS (.loi)
    #########################################################################
    message("\n--- PARTIE 2 : LOIS HYDRAULIQUES (.loi) ---")
    
    duree_totale_sim <- length(debits_q) * (duree_montee + duree_palier)
    hydro_file <- file.path(current_buse_output_dir, "hydrogramme_complet.loi")
    
    con_q <- NULL
    tryCatch({
      con_q <- file(hydro_file, open = "wt")
      writeLines(c("# Hydrogramme...", "# T(s) Q(m3/s)", "S"), con_q)
      current_time <- 0.0
      writeLines(paste(sprintf("%.1f", current_time), "0.20000"), con_q)
      for (q in debits_q) { 
        current_time <- current_time + duree_montee
        writeLines(paste(sprintf("%.1f", current_time), sprintf("%.5f", q)), con_q)
        current_time <- current_time + duree_palier
        writeLines(paste(sprintf("%.1f", current_time), sprintf("%.5f", q)), con_q) 
      }
    }, finally = {
      if(!is.null(con_q)) close(con_q)
    })
    
    message("      - Génération des cotes Z aval selon la logique personnalisée...")
    cotes_z_aval <- c()
    slope=((nozzle_data$Z1-nozzle_data$Z2)/nozzle_data$LENGTH)
    
    z_start = max(nozzle_data$Z2 - 0.2, nozzle_data$Z2 - (slope*dist_p4_p6)/2)
    z_phase1_end = nozzle_data$Z2
    cotes_z_aval1=seq(from = z_start, to = z_phase1_end, by = 0.5)
    z_phase2_end  <- z_phase1_end + nozzle_data$HAUT1+0.2
    cotes_z_aval2=seq(from = z_phase1_end, to = z_phase2_end, by = 0.1)
    z_phase3_end  <- z_phase1_end + nozzle_data$HAUT1 + 2
    cotes_z_aval3 <- seq(from = z_phase2_end, to = z_phase3_end, by = 0.5)
    cotes_z_aval <- unique(sort(c(cotes_z_aval1,cotes_z_aval2,cotes_z_aval3)))
    #à enlever
    # z_start=nozzle_data$Z2-(slope*dist_p4_p6)
    # cotes_z_aval<- seq(from = z_start, to = z_phase1_end + nozzle_data$HAUT1+0.5, by = 0.5)
    ##
    message(paste("La cote Z aval minimale sera:", sprintf("%.3f", z_start)))
    message(paste("Nombre de conditions aval générées:", length(cotes_z_aval)))
    
    for (z in cotes_z_aval) { 
      filename_z <- file.path(current_buse_output_dir, paste0("limni_Z", sprintf("%.3f", z), ".loi"))
      con_z <- NULL
      tryCatch({
        con_z <- file(filename_z, open = "wt")
        writeLines(c(paste("# Limnigramme... Z=", sprintf("%.3f", z)), "# T(s) Z(m)", "S"), con_z)
        writeLines(paste(sprintf("%.1f", 0.0), sprintf("%.5f", z)), con_z)
        writeLines(paste(sprintf("%.1f", duree_totale_sim), sprintf("%.5f", z)), con_z)
      }, finally = {
        if(!is.null(con_z)) close(con_z)
      })
    }
    message(paste(length(cotes_z_aval), "fichiers .loi générés avec succès."))
    
    #########################################################################
    # PARTIE 3: GÉNÉRATION DES FICHIERS DE PARAMÈTRES (.xcas)
    #########################################################################
    message("\n--- PARTIE 3 : PARAMÈTRES (.xcas) ---")
    limni_files <- list.files(current_buse_output_dir, pattern = "^limni_Z.*\\.loi$", full.names = TRUE)
    
    for (limni_file in limni_files) {
      current_xcas_lines <- readLines(xcas_template_filename, warn = FALSE)
      limni_id <- sub("^limni_(Z.*)\\.loi$", "\\1", basename(limni_file))
      scenario_base_name <- paste0("buse", numero_buse, "_scenario_", limni_id)
      xcas_output_filename <- file.path(current_buse_output_dir, paste0(scenario_base_name, ".xcas"))
      
      pas_stockage_value <- ceiling((duree_montee + duree_palier) / current_pas_temps)
      critere_arret_value <- 1
      
      current_xcas_lines <- gsub("<fichMotsCles>.*</fichMotsCles>", paste0("<fichMotsCles>", scenario_base_name, ".xcas</fichMotsCles>"), current_xcas_lines)
      current_xcas_lines <- gsub("<fichier>.*</fichier>", paste0("<fichier>", geo_filename_with_ext, "</fichier>"), current_xcas_lines)
      current_xcas_lines <- gsub("<abscFin>.*</abscFin>", paste0("<abscFin>", sprintf("%.3f", model_total_length), "</abscFin>"), current_xcas_lines)
      current_xcas_lines <- gsub("<absFinZone>.*</absFinZone>", paste0("<absFinZone>", sprintf("%.3f", model_total_length), "</absFinZone>"), current_xcas_lines)
      current_xcas_lines <- gsub("<pasTemps>.*</pasTemps>", paste0("<pasTemps>", sprintf("%.1f", current_pas_temps), "</pasTemps>"), current_xcas_lines)
      current_xcas_lines <- gsub("<tempsMax>.*</tempsMax>", paste0("<tempsMax>", sprintf("%.1f", duree_totale_sim), "</tempsMax>"), current_xcas_lines)
      current_xcas_lines <- gsub("<critereArret>.*</critereArret>", paste0("<critereArret>", critere_arret_value, "</critereArret>"), current_xcas_lines)
      current_xcas_lines <- gsub("<pasStock>.*</pasStock>", paste0("<pasStock>", sprintf("%.0f", pas_stockage_value), "</pasStock>"), current_xcas_lines)
      current_xcas_lines <- gsub("<fichResultat>.*</fichResultat>", paste0("<fichResultat>", scenario_base_name, "_ecr.opt</fichResultat>"), current_xcas_lines)
      current_xcas_lines <- gsub("<fichListing>.*</fichListing>", paste0("<fichListing>", scenario_base_name, ".lis</fichListing>"), current_xcas_lines)
      current_xcas_lines <- gsub("<valeursPas>.*</valeursPas>",paste0("<valeursPas>",sprintf("%.2f", val_pas_planimetrage),"</valeursPas>"), current_xcas_lines)
      current_xcas_lines <- gsub("<numDerProf>.*</numDerProf>", paste0("<numDerProf>", num_profiles_total, "</numDerProf>"), current_xcas_lines)
      
      pas_maillage_str <- paste(c(current_pas_maillage[1], current_pas_maillage[2], current_pas_maillage[2], current_pas_maillage[2], current_pas_maillage[3]), collapse = " ")
      current_xcas_lines <- gsub("<nbPlages>.*</nbPlages>", "<nbPlages>5</nbPlages>", current_xcas_lines)
      current_xcas_lines <- gsub("<num1erProfPlage>.*</num1erProfPlage>", "<num1erProfPlage>1 2 3 4 5</num1erProfPlage>", current_xcas_lines)
      current_xcas_lines <- gsub("<numDerProfPlage>.*</numDerProfPlage>", paste0("<numDerProfPlage>2 3 4 5 ", num_profiles_total, "</numDerProfPlage>"), current_xcas_lines)
      current_xcas_lines <- gsub("<pasEspacePlage>.*</pasEspacePlage>", paste0("<pasEspacePlage>", pas_maillage_str, "</pasEspacePlage>"), current_xcas_lines)
      
      start_frottement_idx <- grep("<frottement>", current_xcas_lines)[1]
      end_frottement_idx <- grep("</frottement>", current_xcas_lines)[1]
      if (!is.na(start_frottement_idx) && !is.na(end_frottement_idx)) {
        abs_deb_zone_str <- paste(sprintf("%.3f", x_coords[c(1, 3, 4)]), collapse = " ")
        abs_fin_zone_str <- paste(sprintf("%.3f", x_coords[c(3, 4, 6)]), collapse = " ")
        strickler_zones_str <- paste(sprintf("%.1f", strickler_zones), collapse = " ")
        frottement_block <- c("      <frottement>", "        <loi>1</loi>", "        <nbZone>3</nbZone>", "        <numBranche>1 1 1</numBranche>", paste0("        <absDebZone>", abs_deb_zone_str, "</absDebZone>"), paste0("        <absFinZone>", abs_fin_zone_str, "</absFinZone>"), paste0("        <coefLitMin>", strickler_zones_str, "</coefLitMin>"), paste0("        <coefLitMaj>", strickler_zones_str, "</coefLitMaj>"), "      </frottement>")
        current_xcas_lines <- c(current_xcas_lines[1:(start_frottement_idx - 1)], frottement_block, current_xcas_lines[(end_frottement_idx + 1):length(current_xcas_lines)])
      }
      
      start_index <- grep("<parametresLoisHydrauliques>", current_xcas_lines)[1]
      end_index <- grep("</parametresLoisHydrauliques>", current_xcas_lines)[1]
      if (!is.na(start_index) && !is.na(end_index)) {
        lois_lines <- c("  <parametresLoisHydrauliques>","    <nb>2</nb>","    <lois>","      <structureParametresLoi>","        <nom>hydro_amont</nom>","        <type>1</type>","        <donnees>","          <modeEntree>1</modeEntree>",paste0("          <fichier>", basename(hydro_file), "</fichier>"),"        </donnees>","      </structureParametresLoi>","      <structureParametresLoi>",paste0("        <nom>limni_", limni_id, "</nom>"),"        <type>2</type>","        <donnees>","          <modeEntree>1</modeEntree>",paste0("          <fichier>", basename(limni_file), "</fichier>"),"        </donnees>","      </structureParametresLoi>","    </lois>","  </parametresLoisHydrauliques>")
        current_xcas_lines <- c(current_xcas_lines[1:(start_index-1)], lois_lines, current_xcas_lines[(end_index+1):length(current_xcas_lines)])
      }
      
      con <- NULL
      tryCatch({
        con <- file(xcas_output_filename, "w")
        writeLines(current_xcas_lines, con)
      }, finally = {
        if(!is.null(con)) close(con)
      })
    }
    message("--- Fichiers .xcas générés avec succès. ---")
    
    # PARTIE 4
    message("\n--- PARTIE 4 : LANCEMENT DES SIMULATIONS ---")
    xcas_files <- dir_ls(current_buse_output_dir, regexp = "\\.xcas$", recurse = FALSE)
    registerDoParallel(cores = num_cores_sim)
    foreach( xcas_file_path = xcas_files, .combine = 'c', .errorhandling = 'pass') %dopar% {
      library(fs)
      xcas_filename <- path_file(xcas_file_path)
      xcas_basename <- path_ext_remove(xcas_filename)
      temp_sim_dir <- path(current_buse_output_dir, paste0("temp_", xcas_basename))
      results_dir <- path(current_buse_output_dir, "results_mascaret")
      
      final_opt_path <- path(results_dir, paste0(xcas_basename, "_ecr.opt"))
      
      if(!file_exists(final_opt_path) || file_info(final_opt_path)$size == 0) {
        dir_create(temp_sim_dir)
        file_copy(xcas_file_path, path(temp_sim_dir, xcas_filename), overwrite = TRUE)
        file_copy(path(current_buse_output_dir, geo_filename_with_ext), temp_sim_dir, overwrite = TRUE)
        file_copy(path(current_buse_output_dir, "hydrogramme_complet.loi"), temp_sim_dir, overwrite = TRUE)
        limni_id_for_copy <- sub(paste0("buse", numero_buse, "_scenario_"), "", xcas_basename)
        limni_to_copy <- path(current_buse_output_dir, paste0("limni_", limni_id_for_copy, ".loi"))
        if(file_exists(limni_to_copy)) file_copy(limni_to_copy, temp_sim_dir, overwrite = TRUE)
        
        batch_content <- sprintf('@echo off\nCALL "%s"\ncd /D "%%~dp0"\npython "%%HOMETEL%%\\scripts\\python3\\mascaret.py" "%s"\nexit /b %%ERRORLEVEL%%', telemac_env_script, xcas_filename)
        batch_file_path <- path(temp_sim_dir, "run_simulation.bat")
        writeLines(batch_content, batch_file_path)
        
        system2("cmd", args = c("/c", path_abs(batch_file_path)), stdout = TRUE, stderr = TRUE)
      }
    }
    stopImplicitCluster()
    message("--- Lancement des simulations terminé. ---")
    
    # NOUVELLE PARTIE 4.5: Copie des résultats après la fin de tous les calculs
    message("\n--- PARTIE 4.5 : COLLECTE DES RÉSULTATS ---")
    results_dir <- path(current_buse_output_dir, "results_mascaret")
    dir_create(results_dir)
    temp_dirs <- dir_ls(current_buse_output_dir, regexp = "temp_buse", type = "directory")
    
    for(temp_dir in temp_dirs) {
      opt_files_in_temp <- dir_ls(temp_dir, regexp = "\\.(opt|lis)$")
      if(length(opt_files_in_temp) > 0) {
        file_copy(opt_files_in_temp, results_dir, overwrite = TRUE)
      }
    }
    message("--- Collecte des fichiers .opt terminée. ---")
    
    
    # PARTIE 5
    message("\n--- PARTIE 5 : LECTURE DES RÉSULTATS ET CRÉATION DE L'ABAQUE ---")
    opt_dir <- file.path(current_buse_output_dir, "results_mascaret")
    
    ABSCISSA_BUSE_ENTREE_TARGET <- x_coords[3]
    ABSCISSA_BUSE_SORTIE_TARGET <- x_coords[4]
    
    opt_files <- list.files(opt_dir, pattern = "\\.opt$", full.names = TRUE)
    results_list <- list()
    
    for (opt_file in opt_files) {
      opt_content_lines <- readLines(opt_file, warn = FALSE)
      resultats_start_line_index <- grep("[resultats]", opt_content_lines, fixed = TRUE)
      if (length(resultats_start_line_index) == 0) next
      opt_data_text <- opt_content_lines[(resultats_start_line_index[1] + 1):length(opt_content_lines)]
      if(length(opt_data_text) == 0 || all(nchar(trimws(opt_data_text)) == 0)) next
      
      opt_data <- tryCatch({ fread(text = opt_data_text, sep = ";", strip.white=TRUE, header = FALSE, colClasses = "numeric", fill = TRUE, select = c(1, 4, 6, 11, 12, 14), col.names = c("Time", "Abscissa", "S_value", "Froude", "Velocity", "DebitTotal")) }, error = function(e) { NULL })
      if(is.null(opt_data) || nrow(opt_data) == 0) next
      
      plateau_end_times <- (1:length(debits_q)) * (duree_montee + duree_palier)
      
      for(time_step in plateau_end_times){
        time_to_extract <- time_step - current_pas_temps
        if(time_to_extract < 0 || time_to_extract > max(opt_data$Time)) next
        
        steady_state_data <- opt_data[abs(opt_data$Time - time_to_extract) < 1e-3, ]
        if(nrow(steady_state_data) == 0) next
        
        entree_buse_row <- steady_state_data[which.min(abs(steady_state_data$Abscissa - ABSCISSA_BUSE_ENTREE_TARGET)), ]
        sortie_buse_row <- steady_state_data[which.min(abs(steady_state_data$Abscissa - ABSCISSA_BUSE_SORTIE_TARGET)), ]
        
        if(nrow(entree_buse_row) == 1 && nrow(sortie_buse_row) == 1){
          avg_velocity <- (entree_buse_row$Velocity + sortie_buse_row$Velocity) / 2
          
          results_list[[length(results_list) + 1]] <- data.frame(
            Debit = entree_buse_row$DebitTotal, 
            S1 = entree_buse_row$S_value, 
            S2 = sortie_buse_row$S_value,
            Froude_amont = entree_buse_row$Froude,
            Avg_Velocity = avg_velocity
          )
        }
      }
    }
    
    results_for_file <- data.frame()
    all_results_df <- data.frame()
    results_df_sorted <- data.frame()
    
    if (length(results_list) > 0) {
      all_results_df <- do.call(rbind, results_list)
      s1_max_threshold <- nozzle_data$Z1 + nozzle_data$HAUT1 + 10
      
      all_results_filtered <- all_results_df[
        all_results_df$S1 <= s1_max_threshold & 
          all_results_df$Froude_amont <= 5 &
          # all_results_df$Avg_Velocity < 7.0 &
          (all_results_df$S1 - all_results_df$S2) >= 0.05,
      ]
      if(nrow(all_results_filtered) > 0) {
        results_df_unique <- unique(all_results_filtered)
        results_df_sorted <- results_df_unique[order(results_df_unique$Debit, results_df_unique$S1), ]
        
        final_points_list <- lapply(split(results_df_sorted, results_df_sorted$Debit), function(df_debit) {
          torrential_points <- df_debit %>% filter(Froude_amont >= 0.8)
          if (nrow(torrential_points) > 0) return(torrential_points)
          good_fluvial_points <- df_debit %>% filter(Froude_amont < 0.8 & S1 < (nozzle_data$Z1 + nozzle_data$HAUT1) & S2 < (nozzle_data$Z2 + nozzle_data$HAUT1))
          if (nrow(good_fluvial_points) > 0) return(good_fluvial_points)
          return(df_debit %>% filter(Froude_amont < 0.8))
        })
        
        results_for_file <- bind_rows(final_points_list) %>% unique() %>% arrange(Debit, S1)
        if(length(which(results_for_file$Froude_amont>=1))>0){
          max_debit_torrential=max(results_for_file$Debit[which(results_for_file$Froude_amont>=1)])
          max_S1_torrentiel=max(results_for_file$S1[which(results_for_file$Debit==max_debit_torrential)])
          max_S2_torrentiel=max(results_for_file$S2[which(results_for_file$Debit==max_debit_torrential)])
          
          points_fluviaux1=which(results_for_file$Debit<max_debit_torrential & (results_for_file$S1 >max_S1_torrentiel | results_for_file$S2 >max_S2_torrentiel ) )
          points_fluviaux2=which(results_for_file$Debit>max_debit_torrential & results_for_file$S2 >nozzle_data$Z2 + nozzle_data$HAUT1 + 0.05  )
          if(length(points_fluviaux1)>0 | length(points_fluviaux2)>0){
            results_for_file=results_for_file[-c(points_fluviaux1,points_fluviaux2),]
            results_for_file <- results_for_file %>% unique() %>% arrange(Debit, S1)
          }
        }
        
        # --- NETTOYAGE ET STRICTE MONOTONIE HYDRAULIQUE (Q et S1) ---
        if (nrow(results_for_file) > 0) {
          
          # On trie impérativement par S1 (hauteur d'eau) en premier lieu pour s'assurer
          # que la courbe part toujours du point le plus bas (le radier) sans sauter de points à cause du tri par débit
          results_for_file <- results_for_file[order(results_for_file$S1, results_for_file$Debit), ]
          
          # Filtrage ultra-simple pour garantir la stricte croissance de S1 requise par MASCARET
          if (nrow(results_for_file) > 1) {
            indices_valides <- c(1)
            derniere_cote <- results_for_file$S1[1]
            
            for (idx_pt in 2:nrow(results_for_file)) {
              if (results_for_file$S1[idx_pt] > derniere_cote) {
                indices_valides <- c(indices_valides, idx_pt)
                derniere_cote <- results_for_file$S1[idx_pt]
              }
            }
            results_for_file <- results_for_file[indices_valides, ]
          }
        }
        
        output_txt_file_S_partiel <- file.path(current_buse_output_dir, "abaque_partiel.txt")
        if (nrow(results_for_file) > 0) {
          file_conn_partiel <- file(output_txt_file_S_partiel, "w")
          writeLines(c(as.character(numero_buse), as.character(nrow(results_for_file))), file_conn_partiel)
          cat(paste(sprintf("%.2f", results_for_file$Debit), collapse = ","), "\n", file = file_conn_partiel)
          cat(paste(sprintf("%.2f", results_for_file$S1), collapse = ","), "\n", file = file_conn_partiel)
          cat(paste(sprintf("%.2f", results_for_file$S2), collapse = ","), "\n", file = file_conn_partiel)
          close(file_conn_partiel)
        }
        
        output_txt_file_QS1_partiel <- file.path(current_buse_output_dir, "abaque_partiel_QS1.txt")
        if (nrow(results_for_file) > 0) {
          results_QS1_unique <- results_for_file %>% dplyr::select(Debit, S1) %>% distinct() %>% arrange(Debit, S1)
          file_conn_QS1 <- file(output_txt_file_QS1_partiel, "w")
          writeLines(c(as.character(numero_buse), as.character(nrow(results_QS1_unique))), file_conn_QS1)
          cat(paste(sprintf("%.2f", results_QS1_unique$Debit), collapse = ","), "\n", file = file_conn_QS1)
          cat(paste(sprintf("%.2f", results_QS1_unique$S1), collapse = ","), "\n", file = file_conn_QS1)
          close(file_conn_QS1)
        }
      }
    }
    
    if(generate_long_profile_plots) {
      message("\n--- GÉNÉRATION DES PROFILS EN LONG ---")
      for (opt_file in opt_files) {
        scenario_name <- sub("_ecr\\.opt$", "", basename(opt_file)); opt_content_lines <- readLines(opt_file, warn = FALSE); resultats_start_line_index <- grep("[resultats]", opt_content_lines, fixed = TRUE); if (length(resultats_start_line_index) == 0) next
        opt_data_full <- tryCatch({ fread(text = opt_content_lines[(resultats_start_line_index[1] + 1):length(opt_content_lines)], sep = ";", strip.white=TRUE, header = FALSE, colClasses = "numeric", fill = TRUE, select = c(1, 4, 5, 6, 14), col.names = c("Time", "Abscissa", "Z_fond", "Z_eau", "DebitTotal")) }, error = function(e) { NULL })
        if(is.null(opt_data_full) || nrow(opt_data_full) == 0) next
        fig_pl <- plot_ly(); profil_fond <- opt_data_full[abs(Time - min(Time)) < 1e-3, ]; fig_pl <- add_trace(fig_pl, data = profil_fond, x = ~Abscissa, y = ~Z_fond, type = 'scatter', mode = 'lines', line = list(color = 'black', width = 3), name = 'Fond')
        for (time_p in ((1:length(debits_q)) * (duree_montee + duree_palier) - current_pas_temps)) { if (time_p > 0 && time_p <= max(opt_data_full$Time)) { steady_data <- opt_data_full[abs(opt_data_full$Time - time_p) < 1e-3, ]; if (nrow(steady_data) > 0) fig_pl <- add_trace(fig_pl, data = steady_data, x = ~Abscissa, y = ~Z_eau, type = 'scatter', mode = 'lines', name = paste('Q =', round(steady_data$DebitTotal[1], 2), 'm³/s')) } }
        y_range <- range(c(profil_fond$Z_fond, opt_data_full$Z_eau), na.rm = TRUE); fig_pl <- fig_pl %>% layout(title = scenario_name, xaxis = list(title = "Abscisse"), yaxis = list(title = "Altitude", range = y_range), shapes = list(list(type = 'line', x0 = x_coords[indices$p3], x1 = x_coords[indices$p3], y0 = y_range[1], y1 = y_range[2], line = list(color = 'grey', dash = 'dash')), list(type = 'line', x0 = x_coords[indices$p4], x1 = x_coords[indices$p4], y0 = y_range[1], y1 = y_range[2], line = list(color = 'grey', dash = 'dash')), list(type = 'line', x0 = x_coords[indices$p3], x1 = x_coords[indices$p4], y0 = nozzle_data$Z1 + nozzle_data$HAUT1, y1 = nozzle_data$Z2 + nozzle_data$HAUT1, line = list(color = 'red', dash = 'longdash'))))
        saveWidget(fig_pl, file = file.path(opt_dir, paste0("profil_en_long_", scenario_name, ".html")), selfcontained = TRUE)
      }
    }
    
    
    if(generate_abaque_plots) {
      message("\n--- PARTIE 6 : GÉNÉRATION DES GRAPHIQUES D'ABAQUE ---")
      
      # Utiliser `all_results_df` pour définir les points à estomper
      points_to_keep <- results_for_file
      points_to_fade <- dplyr::anti_join(all_results_df, points_to_keep, by = c("Debit", "S1", "S2", "Froude_amont", "Avg_Velocity"))
      
      if (nrow(points_to_keep) > 0 || nrow(points_to_fade) > 0) {
        
        # Préparation des données pour les points principaux
        results_df_graph <- points_to_keep
        if(nrow(results_df_graph) > 0) {
          colnames(results_df_graph) <- c("Debit", "S1_Charge_Amont", "S2_Charge_Aval", "Froude_amont", "Avg_Velocity")
          results_df_graph$Froude_Condition <- cut(results_df_graph$Froude_amont, 
                                                   breaks = c(-Inf, 0.99, 1.01, Inf), 
                                                   labels = c("Fluvial (Fr < 1)", "Critique (Fr = 1)", "Torrentiel (Fr > 1)"))
        }
        
        # --- Graphique 3D ---
        title_3d <- paste("Relation Débit-Charges S1/S2 - Buse", numero_buse, "<br>", title_details_str)
        fig_3d_S <- plot_ly()
        
        if(nrow(points_to_fade) > 0) {
          fig_3d_S <- fig_3d_S %>% add_trace(
            data = points_to_fade, x=~Debit, y=~S2, z=~S1,
            type='scatter3d', mode='markers',
            marker=list(size=3, color='rgba(211, 211, 211, 0.5)'), # Gris clair, semi-transparent
            name = "Points exclus",
            hoverinfo = "text",
            text = ~paste('Exclu<br>Débit:', sprintf("%.2f", Debit), '<br>S1:', sprintf("%.2f", S1), '<br>S2:', sprintf("%.2f", S2), '<br>Froude:', sprintf("%.2f", Froude_amont))
          )
        }
        
        if(nrow(results_df_graph) > 0) {
          fig_3d_S <- fig_3d_S %>% add_trace(
            data = results_df_graph, x=~Debit, y=~S2_Charge_Aval, z=~S1_Charge_Amont,
            type='scatter3d', mode='markers',
            color=~S1_Charge_Amont, colorscale='Viridis',
            symbol=~Froude_Condition, symbols=c('circle','square','triangle-up'),
            marker=list(size=4),
            customdata=~Froude_amont,
            name = "Points de l'abaque",
            hovertemplate=paste('<b>Débit:</b> %{x:.2f} m³/s<br>',
                                '<b>S2 (Aval):</b> %{y:.2f} m<br>',
                                '<b>S1 (Amont):</b> %{z:.2f} m<br>',
                                '<b>Froude (Amont):</b> %{customdata:.2f}<br>',
                                '<extra></extra>')
          )
        }
        
        fig_3d_S <- fig_3d_S %>% layout(title=title_3d,
                                        legend=list(title=list(text='<b>Régime Froude</b>')),
                                        scene=list(xaxis=list(title="Débit (m³/s)"),
                                                   yaxis=list(title="S2 Charge Aval (m)"),
                                                   zaxis=list(title="S1 Charge Amont (m)")))
        
        # --- Graphique 2D ---
        title_2d <- paste("Relation S1-S2 - Buse", numero_buse, "<br>", title_details_str)
        fig_2d_S <- plot_ly()
        
        if(nrow(points_to_fade) > 0) {
          fig_2d_S <- fig_2d_S %>% add_trace(
            data = points_to_fade, x=~S1, y=~S2,
            type='scatter', mode='markers',
            marker=list(size=5, color='rgba(211, 211, 211, 0.6)'), # Gris clair, semi-transparent
            name = "Points exclus",
            hoverinfo = "text",
            text = ~paste('Exclu<br>Débit:', sprintf("%.2f", Debit), '<br>S1:', sprintf("%.2f", S1), '<br>S2:', sprintf("%.2f", S2), '<br>Froude:', sprintf("%.2f", Froude_amont))
          )
        }
        
        if(nrow(results_df_graph) > 0) {
          fig_2d_S <- fig_2d_S %>% add_trace(
            data = results_df_graph, x=~S1_Charge_Amont, y=~S2_Charge_Aval,
            type='scatter', mode='markers',
            color=~Debit, colorscale='Plasma',
            symbol=~Froude_Condition, symbols=c('circle','square','triangle-up'),
            marker=list(size=6),
            customdata=~Froude_amont,
            name = "Points de l'abaque",
            hovertemplate=paste('<b>S1 (Amont):</b> %{x:.2f} m<br>',
                                '<b>S2 (Aval):</b> %{y:.2f} m<br>',
                                '<b>Débit:</b> %{marker.color:.2f} m³/s<br>',
                                '<b>Froude:</b> %{customdata:.2f}<br>',
                                '<extra></extra>')
          )
          
          shapes_list <- list(
            list(type = 'line', x0 = nozzle_data$Z1, x1 = nozzle_data$Z1, y0 = min(results_df_graph$S2_Charge_Aval), y1 = max(results_df_graph$S2_Charge_Aval), line = list(color = 'blue', dash = 'longdash', width = 2)),
            list(type = 'line', x0 = nozzle_data$Z1 + nozzle_data$HAUT1, x1 = nozzle_data$Z1 + nozzle_data$HAUT1, y0 = min(results_df_graph$S2_Charge_Aval), y1 = max(results_df_graph$S2_Charge_Aval), line = list(color = 'red', dash = 'longdash', width = 2)),
            list(type = 'line', y0 = nozzle_data$Z2, y1 = nozzle_data$Z2, x0 = min(results_df_graph$S1_Charge_Amont), x1 = max(results_df_graph$S1_Charge_Amont), line = list(color = 'blue', dash = 'longdash', width = 2)),
            list(type = 'line', y0 = nozzle_data$Z2 + nozzle_data$HAUT1, y1 = nozzle_data$Z2 + nozzle_data$HAUT1, x0 = min(results_df_graph$S1_Charge_Amont), x1 = max(results_df_graph$S1_Charge_Amont), line = list(color = 'red', dash = 'longdash', width = 2))
          )
          fig_2d_S <- fig_2d_S %>% layout(shapes = shapes_list)
        }
        
        fig_2d_S <- fig_2d_S %>% layout(
          title=title_2d,
          xaxis=list(title="S1 Charge Amont (m)"),
          yaxis=list(title="S2 Charge Aval (m)"),
          legend=list(title=list(text='<b>Régime Froude</b>'))
        )
        
        htmlwidgets::saveWidget(fig_3d_S, file.path(current_buse_output_dir, "graph_abaque_S_3d.html"), selfcontained = TRUE)
        htmlwidgets::saveWidget(fig_2d_S, file.path(current_buse_output_dir, "graph_abaque_S_2d.html"), selfcontained = TRUE)
        message("Graphiques d'abaque 2D et 3D (incluant les points exclus) exportés.")
        
      } else {
        message("Aucune donnée à visualiser, les graphiques d'abaque n'ont pas été générés pour cette buse.")
      }
    }
    
    # --- BLOC DE NETTOYAGE SÉLECTIF (AJOUTÉ v123) ---
    if (cleanup_calc_files) {
      message(paste("      - Nettoyage sélectif pour la buse", numero_buse))
      
      # 1. Identifier tous les dossiers temporaires temp_...
      tdirs <- fs::dir_ls(current_buse_output_dir, regexp = "temp_", type = "directory")
      if (length(tdirs) > 1) {
        # Garder uniquement le premier dossier pour vérification, supprimer les autres
        fs::dir_delete(tdirs[-1]) 
      }
      
      # 2. Supprimer le dossier global results_mascaret (doublon inutile une fois les données extraites)
      res_fold <- file.path(current_buse_output_dir, "results_mascaret")
      if(fs::dir_exists(res_fold)) fs::dir_delete(res_fold)
      
      # 3. Supprimer les fichiers d'entrée à la racine (déjà sauvegardés dans le dossier temp conservé)
      root_files <- fs::dir_ls(current_buse_output_dir, regexp = "\\.(xcas|geo|loi|lis)$")
      if(length(root_files) > 0) fs::file_delete(root_files)
    }
  } 
  
  #########################################################################
  # DÉMARRAGE DE LA BOUCLE PRINCIPALE
  #########################################################################
  if (ncore_buse <= 1) {
    message("--- DÉMARRAGE DU TRAITEMENT SÉQUENTIEL ---")
    for (i in 1:nrow(busel_data)) {
      abaques_check_file <- file.path(dsnlayerC, "abaques2.txt")
      if (file.exists(abaques_check_file) && file.info(abaques_check_file)$size > 0) {
        message(paste(">>> BUSE", abaques_check_file, ": DÉJÀ TRAITÉE (Fichier abaque trouvé). PASSAGE À LA SUIVANTE. <<<"))
        next
      }
      process_buse(i, busel_data, main_output_dir, telemac_env_script, xcas_template_filename, use_mnt_for_profiles, mnt_path, dist_amont_limite, dist_p4_p6, num_points_per_profile, dist_extraction_profil, first_profile_shape, duree_palier, duree_montee, num_cores_sim, strickler_zones_base, val_pas_planimetrage, g, pas_maillage_court, pas_temps_court, pas_maillage_moyen, pas_temps_moyen, pas_maillage_long, pas_temps_long, preissmann_slot_width, generate_long_profile_plots, generate_abaque_plots, cleanup_calc_files)
    }
  } else {
    message(paste("--- DÉMARRAGE DU TRAITEMENT PARALLÈLE SUR", ncore_buse, "CŒURS ---"))
    registerDoParallel(cores = ncore_buse)
    foreach(i = 1:nrow(busel_data)) %dopar% { 
      process_buse(i, busel_data, main_output_dir, telemac_env_script, xcas_template_filename, use_mnt_for_profiles, mnt_path, dist_amont_limite, dist_p4_p6, num_points_per_profile, dist_extraction_profil, first_profile_shape, duree_palier, duree_montee, num_cores_sim, strickler_zones_base, val_pas_planimetrage, g, pas_maillage_court, pas_temps_court, pas_maillage_moyen, pas_temps_moyen, pas_maillage_long, pas_temps_long, preissmann_slot_width, generate_long_profile_plots, generate_abaque_plots, cleanup_calc_files) 
    }
    stopImplicitCluster()
  }
  
  #########################################################################
  # PARTIE 7: ASSEMBLAGE FINAL DE L'ABAQUE
  #########################################################################
  message("\n--- PARTIE 7 : ASSEMBLAGE DU FICHIER D'ABAQUE FINAL ---")
  output_txt_file_S_global <- file.path(main_output_dir, "resultats_buse_format_abaqueSQ.txt")
  file_conn_main <- file(output_txt_file_S_global, "w")
  writeLines("#abaqueSQ", file_conn_main)
  output_txt_file_QS1_global <- file.path(main_output_dir, "abaques.txt")
  file_conn_qs1 <- file(output_txt_file_QS1_global, "w")
  writeLines("#abaqueQS1", file_conn_qs1)
  
  for (i in 1:nrow(busel_data)) {
    numero_buse <- busel_data$id[i]
    current_buse_output_dir <- file.path(main_output_dir, paste0("buse_", numero_buse))
    
    p1 <- file.path(current_buse_output_dir, "abaque_partiel.txt")
    if (file.exists(p1)) writeLines(readLines(p1), file_conn_main)
    
    p2 <- file.path(current_buse_output_dir, "abaque_partiel_QS1.txt")
    if (file.exists(p2)) writeLines(readLines(p2), file_conn_qs1)
  }
  
  close(file_conn_main); close(file_conn_qs1)
  message("\n--- WORKFLOW COMPLET TERMINÉ ---")
  file.copy(from = file.path(main_output_dir, "abaques.txt"),to = dsnlayerC,overwrite = TRUE,recursive = TRUE)
  # 1. Lire toutes les lignes du fichier d'origine
  lignes <- readLines(file.path(dsnlayerC, "abaques.txt"))
  
  # 2. Nettoyer les éventuelles lignes vides à la fin du fichier
  lignes <- lignes[lignes != ""]
  
  # 3. Calculer le nombre de buses
  # On soustrait 1 (pour la ligne de commentaire) avant de diviser par 4
  n_buses <- floor((length(lignes) - 1) / 4)
  
  # 4. Générer les indices des IDs
  # On commence à la ligne 2, et on fait des sauts de 4
  indices_id <- seq(2, by = 4, length.out = n_buses)
  
  # 5. Remplacer les lignes d'identifiants par la nouvelle numérotation
  lignes[indices_id] <- 1:n_buses
  
  # 6. Sauvegarder le résultat en écrasant le fichier
  writeLines(as.character(lignes), file.path(dsnlayerC, "abaques.txt"))
  # Script R pour lisser les abaques Telemac-2D
  # Objectif : Garantir une croissance stricte des hauteurs d'eau (S1) 
  # pour éviter les divisions par zéro dans DISCHARGE_INTERPOL.
  
  # Noms des fichiers (à adapter si besoin)
  file_in <- file.path(dsnlayerC, "abaques.txt")
  file_out <- file.path(dsnlayerC, "abaques2.txt")
  
  # Lecture du fichier
  lines <- readLines(file_in, warn = FALSE)
  # Nettoyage des lignes vides éventuelles
  lines <- lines[trimws(lines) != ""]
  
  # On conserve l'entête (ex: #abaqueQS1)
  output_lines <- c(lines[1])
  
  idx <- 2
  while (idx <= length(lines)) {
    # Extraction du bloc de 4 lignes
    abaque_id <- trimws(lines[idx])
    n_pts_old <- as.integer(trimws(lines[idx + 1]))
    q_str     <- lines[idx + 2]
    s1_str    <- lines[idx + 3]
    
    # Conversion des lignes Q et S1 en vecteurs numériques
    q_vals  <- as.numeric(unlist(strsplit(q_str, ",")))
    s1_vals <- as.numeric(unlist(strsplit(s1_str, ",")))
    
    # Construction du dataframe pour nettoyage
    df_abaque <- data.frame(Debit = q_vals, S1 = s1_vals)
    
    # Filtrage ultra-simple : on parcourt les points dans leur ordre naturel
    # et on ne garde que ceux où la cote d'eau (S1) augmente strictement.
    # On ne trie pas, et on ne modifie JAMAIS les valeurs de débit !
    if (nrow(df_abaque) > 1) {
      indices_valides <- c(1) # On garde toujours le 1er point (fond)
      derniere_cote <- df_abaque$S1[1]
      
      for (idx_pt in 2:nrow(df_abaque)) {
        if (df_abaque$S1[idx_pt] > derniere_cote) {
          indices_valides <- c(indices_valides, idx_pt)
          derniere_cote <- df_abaque$S1[idx_pt]
        }
      }
      df_abaque <- df_abaque[indices_valides, ]
    }
    
    # 4. Interpolation et Extrapolation pour boucher TOUS les trous
    if (nrow(df_abaque) > 0) {
      buse_info <- busel_data[busel_data$id == as.numeric(abaque_id), ]
      if (nrow(buse_info) > 0) {
        Z1 <- buse_info$Z1[1]
        
        # Liste complete theorique des debits (selon ton pas de base)
        Q_max_actuel <- max(df_abaque$Debit)
        q_theorique <- c()
        if (Q_max_actuel >= 0.2) q_theorique <- c(q_theorique, seq(0.2, min(1.0, Q_max_actuel), by = 0.1))
        if (Q_max_actuel >= 1.2) q_theorique <- c(q_theorique, seq(1.2, min(10.0, Q_max_actuel), by = 0.2))
        if (Q_max_actuel >= 11.0) q_theorique <- c(q_theorique, seq(11.0, min(20.0, Q_max_actuel), by = 1.0))
        if (Q_max_actuel >= 22.0) q_theorique <- c(q_theorique, seq(22.0, min(40.0, Q_max_actuel), by = 2.0))
        if (Q_max_actuel >= 45.0) q_theorique <- c(q_theorique, seq(45.0, Q_max_actuel, by = 5.0))
        q_theorique <- unique(sort(round(q_theorique, 2)))
        
        # On ajoute Z1 a Q=0 pour l'extrapolation vers le bas
        df_full <- data.frame(Debit = c(0, q_theorique))
        df_abaque <- merge(df_full, df_abaque, by = "Debit", all.x = TRUE)
        df_abaque$S1[df_abaque$Debit == 0] <- Z1
        
        # Interpolation lineaire pour tous les NA (trous au milieu + vers le bas)
        if (any(is.na(df_abaque$S1))) {
          # approx() interpole lineairement entre les points non-NA
          valeurs_interpolees <- approx(df_abaque$Debit[!is.na(df_abaque$S1)], 
                                        df_abaque$S1[!is.na(df_abaque$S1)], 
                                        xout = df_abaque$Debit)
          df_abaque$S1 <- round(valeurs_interpolees$y, 2)
        }
        
        # On retire le Q=0 qui ne servait que pour interpoler proprement
        df_abaque <- df_abaque[df_abaque$Debit > 0, ]
      }
    }
    
    n_pts  <- nrow(df_abaque)
    q_out  <- paste(sprintf("%.2f", df_abaque$Debit), collapse = ",")
    s1_out <- paste(sprintf("%.2f", df_abaque$S1), collapse = ",")
    
    # Stockage des 4 lignes du bloc corrigé
    output_lines <- c(output_lines, abaque_id, as.character(n_pts), q_out, s1_out)
    
    # On passe à l'abaque suivant
    idx <- idx + 4
  }
  
  # Écriture du nouveau fichier
  writeLines(output_lines, file_out)
  cat("Traitement terminé avec succès !\nLe fichier corrigé est :", file_out, "\n")
}
abacus=function(){
  library(sf)
  library(terra)
  library(dplyr)
  library(fs)
  library(stringr)
  library(foreach)
  library(doParallel)
  library(data.table)
  library(plotly)
  library(htmlwidgets)
  
  #########################################################################
  # PARTIE 0: PARAMÈTRES UTILISATEUR
  # Modifiez les valeurs dans cette section pour configurer votre simulation.
  #########################################################################
  # --- 0.1. Configuration des chemins et fichiers ---
  # chem_routine <- "C:/Users/CartoNationale/Desktop/Nabil"
  
  main_output_dir <- file.path(dsnlayerC, "abaques") 
  telemac_env_script <- "C:/TELEMAC/V8P4/configs/pysource.bat"
  xcas_template_filename <- file.path(chem_routine,"C2D",Doss_Base, "masc.xcas.txt")
  
  # --- 0.2. Paramètres de parallélisation ---
  ncore_buse <- ncore_buse_abaques # Nombre de cœurs pour la boucle sur les buses. 1 = séquentiel, >=2 = parallèle
  num_cores_sim <- num_cores_sim_abaques
  # Nombre de cœurs pour les simulations d'une MÊME buse
  
  # --- 0.3. Contrôle de la géométrie --- 
  use_mnt_for_profiles <- FALSE # Mettre à TRUE pour extraire du MNT, FALSE pour utiliser des profils trapézoïdaux
  
  # --- Chemins pour le mode MNT ---
  shapefile_path <- file.path(dsnlayerC, "OuvHydrau_Select.shp") 
  mnt_path <- file.path(dsnlayerC, "MNTFinal.gpkg") # <--- CHEMIN VERS VOTRE MNT
  
  # --- 0.4. Paramètres de la géométrie ---
  dist_amont_limite <- 1000.0
  dist_p4_p6 <- 2000 # Distance entre la sortie de la buse (corrigée à p5) et la limite aval (p6)
  num_points_per_profile <- 101
  dist_extraction_profil <- 3.0 
  preissmann_slot_width <- 0.01 # Largeur de la fente de Preissmann en mètres
  
  first_profile_shape <- list(
    type = "trapezoidal", bottom_width = 3, left_slope = 1/1.5,
    right_slope = 1/1.5, bank_height_above_bottom = 40
  )
  
  # --- 0.5. Paramètres temporels et hydrauliques ---
  duree_palier <- 400
  duree_montee <- 50
  strickler_zones_base <- c(20.0, 20.0, 20.0) 
  val_pas_planimetrage <- 0.02
  g <- 9.81
  
  # --- 0.6. Paramètres de maillage et de temps de simulation adaptatifs ---
  # Pour buses COURTES (< 20m)
  pas_maillage_court <- c(1, 0.5, 1)
  pas_temps_court <- 0.5
  duree_palier_court <- 20
  duree_montee_court <- 10
  
  # Pour buses MOYENNES (20m à 100m)
  pas_maillage_moyen <- c(1, 1, 1)
  pas_temps_moyen <- 1
  duree_palier_moyen <- 100
  duree_montee_moyen <- 20
  # Pour buses LONGUES (> 100m)
  pas_maillage_long <- c(2, 2, 2)
  pas_temps_long <- 2
  duree_palier_long <- 400
  duree_montee_long <- 50
  # --- 0.7. Paramètres de sortie (NOUVEAU) ---
  generate_long_profile_plots <- FALSE # Mettre à TRUE pour générer les graphiques de profil en long
  generate_abaque_plots <- TRUE       # Mettre à TRUE pour générer les graphiques d'abaque 2D/3D
  cleanup_calc_files <- TRUE           # <--- NOUVEAU: TRUE pour garder 1 dossier temp et supprimer le reste
  
  
  #########################################################################
  # LECTURE DES DONNÉES SPATIALES ET INITIALISATION
  #########################################################################
  message("--- LECTURE DES DONNÉES SPATIALES ---")
  if (!file.exists(shapefile_path)) stop("Le fichier shapefile n'a pas été trouvé: ", shapefile_path)
  busel_data <- st_read(shapefile_path, quiet = TRUE)
  message(paste("Lecture de", nrow(busel_data), "buse(s) depuis le shapefile."))
  
  mnt_raster <- NULL # Initialisation à NULL
  if(use_mnt_for_profiles) {
    if (!file.exists(mnt_path)) stop("Le fichier MNT n'a pas été trouvé: ", mnt_path)
    message("Chargement du MNT pour vérification CRS...")
    temp_mnt <- rast(mnt_path)
    message("MNT chargé.")
    if (st_crs(busel_data) != st_crs(temp_mnt)) {
      message("ATTENTION: CRS différents. Reprojection du shapefile vers le CRS du MNT.")
      busel_data <- st_transform(busel_data, st_crs(temp_mnt))
    }
    rm(temp_mnt) # On supprime l'objet, il sera rechargé dans chaque worker
    gc() # Force le nettoyage de la mémoire
  }
  
  dir.create(main_output_dir, showWarnings = FALSE, recursive = TRUE)
  
  #########################################################################
  # DÉFINITION DE LA FONCTION DE TRAITEMENT D'UNE BUSE
  #########################################################################
  process_buse <- function(i, busel_data, main_output_dir, telemac_env_script, xcas_template_filename, use_mnt_for_profiles, mnt_path, dist_amont_limite, dist_p4_p6, num_points_per_profile, dist_extraction_profil, first_profile_shape, duree_palier, duree_montee, num_cores_sim, strickler_zones_base, val_pas_planimetrage, g, pas_maillage_court, pas_temps_court, pas_maillage_moyen, pas_temps_moyen, pas_maillage_long, pas_temps_long, preissmann_slot_width, generate_long_profile_plots, generate_abaque_plots, cleanup_calc_files) {
    
    # Charger les librairies nécessaires dans l'environnement parallèle
    library(sf)
    library(terra)
    library(dplyr)
    library(fs)
    library(stringr)
    library(foreach)
    library(doParallel)
    library(data.table)
    library(plotly)
    library(htmlwidgets)
    
    # Si on utilise le MNT, chaque worker charge sa propre copie.
    if (use_mnt_for_profiles) {
      mnt_raster <- rast(mnt_path)
    }
    
    buse_row <- busel_data[i, ]
    numero_buse <- buse_row$id
    if(is.null(numero_buse)){
      buse_row$id=buse_row$ID
      numero_buse <- buse_row$id
    }
    
    # Création du dossier de sortie
    current_buse_output_dir <- file.path(main_output_dir, paste0("buse_", numero_buse))
    dir.create(current_buse_output_dir, showWarnings = FALSE, recursive = TRUE)
    
    # -----------------------------------------------------------------------
    # CHECK DE DÉJÀ FAIT (NOUVELLE FONCTIONNALITÉ v124)
    # -----------------------------------------------------------------------
    abaque_check_file <- file.path(current_buse_output_dir, "abaque_partiel.txt")
    if (file.exists(abaque_check_file) && file.info(abaque_check_file)$size > 0) {
      message(paste(">>> BUSE", numero_buse, ": DÉJÀ TRAITÉE (Fichier abaque trouvé). PASSAGE À LA SUIVANTE. <<<"))
      return(NULL)
    }
    # -----------------------------------------------------------------------
    
    CIRC <- buse_row$CIRC
    if(is.na(buse_row$HAUT1) | buse_row$HAUT1==0) buse_row$HAUT1=buse_row$HAUT2
    nozzle_data <- list(LARG = buse_row$LRG, HAUT1 = buse_row$HAUT1, Z1 = buse_row$Z1, Z2 = buse_row$Z2, LENGTH = buse_row$LENGTH)
    first_profile_shape$bottom_width=nozzle_data$LARG*2
    if ("FRIC" %in% names(buse_row) && !is.na(buse_row$FRIC) && buse_row$FRIC > 0) {
      strickler_buse_value <- 1 / buse_row$FRIC
      if (strickler_buse_value < 1) {
        warning(paste("Buse", numero_buse, ": La valeur FRIC=", buse_row$FRIC, "donne un Strickler de", round(strickler_buse_value, 2), ". Utilisation de la valeur par défaut de 50.0."))
        strickler_buse_value <- 60.0
      }
    } else {
      strickler_buse_value <- 60.0
      warning(paste("Buse", numero_buse, ": Colonne 'FRIC' non valide ou absente. Utilisation de la valeur par défaut de Strickler:", strickler_buse_value))
    }
    strickler_zones <- c(strickler_zones_base[1], strickler_buse_value, strickler_zones_base[3])
    
    
    message(paste("\n\n############################################################"))
    message(paste("--- DÉMARRAGE DU TRAITEMENT POUR LA BUSE :", numero_buse, "---"))
    message(paste("--- Fichiers de sortie dans :", current_buse_output_dir, "---"))
    message(paste("############################################################\n"))
    
    if (nozzle_data$LENGTH < 20) {
      current_pas_maillage <- pas_maillage_court
      current_pas_temps <- pas_temps_court
      duree_palier <- duree_palier_court
      duree_montee <- duree_montee_court
      dist_amont_limite <- 20
      dist_p4_p6 <- 50 
      message("      - Buse courte (< 20m). Maillage fin et pas de temps court sélectionnés.")
    } else if (nozzle_data$LENGTH >= 20 && nozzle_data$LENGTH <= 250) {
      current_pas_maillage <- pas_maillage_moyen
      current_pas_temps <- pas_temps_moyen
      duree_palier <- duree_palier_moyen
      duree_montee <- duree_montee_moyen
      dist_amont_limite <- 50
      dist_p4_p6 <- 400
      message("      - Buse moyenne (20-100m). Maillage et pas de temps moyens sélectionnés.")
    } else {
      current_pas_maillage <- pas_maillage_long
      current_pas_temps <- pas_temps_long
      duree_palier <- duree_palier_long
      duree_montee <- duree_montee_long
      dist_amont_limite <- 500
      dist_p4_p6 <- 2000 
      message("      - Buse longue (> 100m). Maillage large et pas de temps long sélectionnés.")
    }
    
    # BRIDE ANTI-CRASH MASCARET :
    # Si la pente est torrentielle (ex: > 5%), on raccourcit drastiquement le canal aval
    # pour éviter que MASCARET ne calcule une chute de plusieurs mètres qui ferait
    # planter son solveur permanent (SARAP) lors de la remontée de la ligne d'eau.
    slope_check <- (nozzle_data$Z1 - nozzle_data$Z2) / nozzle_data$LENGTH
    if (slope_check > 0.05) {
      dist_p4_p6 <- 10.0
      message("      - Pente très forte détectée (", sprintf("%.1f", slope_check*100), "%). Réduction dist aval à 10m pour éviter crash MASCARET.")
    }
    
    I <- (nozzle_data$Z1 - nozzle_data$Z2) / nozzle_data$LENGTH
    if (CIRC == 1) {
      D <- nozzle_data$HAUT1; R <- D / 2; S <- pi * R^2; P <- 2 * pi * R; Rh <- R/2
      Q <- strickler_buse_value * S * (Rh^(2/3)) * sqrt(abs(I))
    } else {
      S <- nozzle_data$LARG * nozzle_data$HAUT1; P <- nozzle_data$LARG + (2 * nozzle_data$HAUT1); Rh <- S / P
      Q <- strickler_buse_value * S * (Rh^(2/3)) * sqrt(abs(I))
    }
    Q <- round(Q)
    
    if (Q < 15) {
      message(paste("      - Le débit max estimé (", Q, "m³/s) est inférieur à 15 m³/s. Il est forcé à 15 m³/s pour la simulation."))
      Q <- 15
    }
    
    Q_sim_max <- Q * 1.5
    if (Q_sim_max > 180) {
      message(paste("      - Le débit max de simulation (", round(Q_sim_max), "m³/s) dépasse 180 m³/s. Il est plafonné à 180 m³/s."))
      Q_sim_max <- 180
    }
    
    debits_q_list <- list()
    current_max_q <- 0
    
    end_q_p1 <- min(Q_sim_max, 1.0)
    if (0.2 <= end_q_p1) {
      debits_q_list[[length(debits_q_list) + 1]] <- seq(0.2, end_q_p1, by = 0.1)
      current_max_q <- end_q_p1
    }
    
    start_val_p2 <- current_max_q + 0.2
    end_q_p2 <- min(Q_sim_max, 10.0)
    if (start_val_p2 <= end_q_p2) {
      debits_q_list[[length(debits_q_list) + 1]] <- seq(start_val_p2, end_q_p2, by = 0.2)
      current_max_q <- end_q_p2
    }
    
    start_val_p3 <- current_max_q + 1
    end_q_p3 <- min(Q_sim_max, 20.0)
    if (start_val_p3 <= end_q_p3) {
      debits_q_list[[length(debits_q_list) + 1]] <- seq(start_val_p3, end_q_p3, by = 1)
      current_max_q <- end_q_p3
    }
    
    start_val_p4 <- current_max_q + 2.0
    end_q_p4 <- min(Q_sim_max, 40.0)
    if (start_val_p4 <= end_q_p4) {
      debits_q_list[[length(debits_q_list) + 1]] <- seq(start_val_p4, end_q_p4, by = 2.0)
      current_max_q <- end_q_p4
    }
    
    start_val_p5 <- current_max_q + 5.0
    end_q_p5 <- Q_sim_max
    if (start_val_p5 <= end_q_p5) {
      debits_q_list[[length(debits_q_list) + 1]] <- seq(start_val_p5, end_q_p5, by = 5.0)
    }
    
    debits_q <- unique(sort(c(unlist(debits_q_list))))
    debits_q <- round(debits_q[debits_q > 0], 2)
    
    if (CIRC == 1) {
      dimension_str <- sprintf("Diamètre: %.2f m", nozzle_data$HAUT1)
    } else {
      dimension_str <- sprintf("Largeur: %.2f m, Hauteur: %.2f m", nozzle_data$LARG, nozzle_data$HAUT1)
    }
    pente_pourcent <- I * 100
    title_details_str <- paste(
      dimension_str, 
      sprintf("Longueur: %.1f m", nozzle_data$LENGTH), 
      sprintf("Pente: %.2f %%", pente_pourcent),
      sprintf("Qmax simulé: %.1f m³/s", Q_sim_max), 
      sep=" | "
    )
    
    #########################################################################
    # PARTIE 1: GÉNÉRATION DU FICHIER DE GÉOMÉTRIE (.geo)
    #########################################################################
    message("--- PARTIE 1 : GÉOMÉTRIE (.geo) ---")
    
    create_rectangular_profile <- function(nozzle_width, nozzle_height, nozzle_bottom_z, target_center_y, preissmann_slot_width = 0.01, slot_height = 50, bank_y_offset = 10) {
      y_center <- target_center_y
      z_bottom <- nozzle_bottom_z
      z_top <- z_bottom + nozzle_height
      z_slot_top <- z_top + slot_height
      
      y_bottom_left  <- y_center - nozzle_width/2
      y_bottom_right <- y_center + nozzle_width/2
      y_top_left     <- y_bottom_left - 0.01
      y_top_right    <- y_bottom_right + 0.01
      
      y_slot_left    <- y_center - preissmann_slot_width/2
      y_slot_right   <- y_center + preissmann_slot_width/2
      
      y_bank_left    <- y_top_left - bank_y_offset
      y_bank_right   <- y_top_right + bank_y_offset
      
      full_profile_df <- rbind(
        data.frame(Y = y_bank_left, Z = z_slot_top), 
        data.frame(Y = y_slot_left, Z = z_slot_top), 
        data.frame(Y = y_slot_left, Z = z_top), 
        data.frame(Y = y_top_left, Z = z_top), 
        data.frame(Y = y_bottom_left, Z = z_bottom), 
        data.frame(Y = y_bottom_right, Z = z_bottom), 
        data.frame(Y = y_top_right, Z = z_top), 
        data.frame(Y = y_slot_right, Z = z_top), 
        data.frame(Y = y_slot_right, Z = z_slot_top), 
        data.frame(Y = y_bank_right, Z = z_slot_top)
      )
      full_profile_df$Marker <- "B"
      return(full_profile_df[!duplicated(round(full_profile_df[, c("Y", "Z")], 5)), ])
    }
    create_circular_profile <- function(diameter, nozzle_bottom_z, target_center_y, slot_width = 0.01, slot_height = 50, bank_y_offset = 10, n_points_arch = 41) {
      radius <- diameter/2; y_center <- target_center_y; z_center <- nozzle_bottom_z + radius; z_connection <- z_center + sqrt(max(0, radius^2 - (slot_width/2)^2)); z_slot_top <- z_connection + slot_height; y_slot_left <- y_center - slot_width/2; y_slot_right <- y_center + slot_width/2; y_structure_left <- y_center - radius; y_structure_right <- y_center + radius; p_berge_gauche_loin <- data.frame(Y = y_structure_left - bank_y_offset, Z = z_slot_top); p_fente_bas_gauche  <- data.frame(Y = y_slot_left, Z = z_connection); theta_start <- atan2(z_connection - z_center, y_slot_left - y_center); theta_end     <- atan2(z_connection - z_center, y_slot_right - y_center); theta_sequence <- seq(from = theta_start, to = theta_end + 2*pi, length.out = n_points_arch); arch_df <- data.frame(Y = y_center + radius * cos(theta_sequence), Z = z_center + radius * sin(theta_sequence)); p_fente_bas_droite  <- data.frame(Y = y_slot_right, Z = z_connection); p_berge_droite_loin <- data.frame(Y = y_structure_right + bank_y_offset, Z = z_slot_top)
      full_profile_df <- rbind(p_berge_gauche_loin, data.frame(Y = y_slot_left, Z = z_slot_top), p_fente_bas_gauche, arch_df, p_fente_bas_droite, data.frame(Y = y_slot_right, Z = z_slot_top), p_berge_droite_loin)
      full_profile_df$Marker <- "B"
      return(full_profile_df[!duplicated(round(full_profile_df[, c("Y", "Z")], 5)), ])
    }
    
    generate_points_for_trapezoidal <- function(bottom_width, left_slope, right_slope, bottom_elevation, bank_height_above_bottom, target_center_y, bank_y_offset = 10) {
      half_bottom_width <- bottom_width / 2
      top_z <- bottom_elevation + bank_height_above_bottom
      
      # Les points sont d'abord calculés de manière centrée autour de Y=0
      y_top_left_rel <- -half_bottom_width - (left_slope * bank_height_above_bottom)
      y_top_right_rel <- half_bottom_width + (right_slope * bank_height_above_bottom)
      y_rel <- c(y_top_left_rel - bank_y_offset, y_top_left_rel, -half_bottom_width, half_bottom_width, y_top_right_rel, y_top_right_rel + bank_y_offset)
      
      # Le thalweg de ce profil relatif est à Y=0. On décale tous les points pour que le thalweg soit à target_center_y.
      y_abs <- y_rel + target_center_y
      
      z_abs <- c(top_z, top_z, bottom_elevation, bottom_elevation, top_z, top_z)
      points_df <- data.frame(Y = y_abs, Z = z_abs, Marker = "B")
      return(points_df[order(points_df$Y), ])
    }
    
    extract_profile_from_mnt <- function(mnt, center_point_sf, direction_vector, profile_width, num_points = 101) {
      perp_vector <- c(-direction_vector[2], direction_vector[1])
      perp_vector_norm <- perp_vector / sqrt(sum(perp_vector^2))
      start_point <- st_coordinates(center_point_sf) - (profile_width / 2) * perp_vector_norm
      end_point <- st_coordinates(center_point_sf) + (profile_width / 2) * perp_vector_norm
      transect_line <- st_sfc(st_linestring(rbind(start_point, end_point)), crs = st_crs(mnt))
      sampled_points_sf <- st_line_sample(transect_line, n = num_points, type = "regular")
      if (st_is_empty(sampled_points_sf)) return(NULL)
      sampled_points_sf <- st_cast(sampled_points_sf, "POINT")
      sampled_points_vect <- vect(sampled_points_sf)
      extracted_data <- terra::extract(mnt, sampled_points_vect, xy = TRUE, ID = FALSE)
      if (nrow(extracted_data) == 0 || all(is.na(extracted_data[, 1]))) {
        warning(paste0("L'extraction MNT a échoué (autour de X=", round(st_coordinates(center_point_sf)[1]),", Y=", round(st_coordinates(center_point_sf)[2]), "). Profil hors emprise?"))
        return(NULL)
      }
      colnames(extracted_data)[1] <- "Z"
      coords <- extracted_data[, c("x", "y")]
      y_relative <- sqrt((coords$x - coords$x[1])^2 + (coords$y - coords$y[1])^2)
      profile_df <- data.frame(Y = y_relative, Z = extracted_data$Z, Marker = "B")
      profile_df <- na.omit(profile_df)
      if (nrow(profile_df) < 2) return(NULL)
      return(profile_df)
    }
    
    shift_profile_z <- function(profile_df, target_min_z) {
      if(is.null(profile_df) || nrow(profile_df) == 0) return(NULL)
      current_min_z <- min(profile_df$Z, na.rm = TRUE)
      delta_z <- target_min_z - current_min_z
      profile_df$Z <- profile_df$Z + delta_z
      return(profile_df)
    }
    
    center_profile_on_thalweg <- function(profile_df, target_y_center) {
      if(is.null(profile_df) || nrow(profile_df) == 0) return(NULL)
      min_z_index <- which.min(profile_df$Z)
      y_of_min_z <- mean(profile_df$Y[min_z_index]) 
      shift_y <- target_y_center - y_of_min_z
      profile_df$Y <- profile_df$Y + shift_y
      return(profile_df)
    }
    
    add_vertical_walls <- function(profile_df) {
      if(is.null(profile_df) || nrow(profile_df) < 2) return(profile_df)
      
      first_point <- profile_df[1, ]
      wall_start_point <- data.frame(Y = first_point$Y, Z = first_point$Z + 20, Marker = "B")
      
      last_point <- profile_df[nrow(profile_df), ]
      wall_end_point <- data.frame(Y = last_point$Y, Z = last_point$Z + 20, Marker = "B")
      
      new_profile <- rbind(wall_start_point, profile_df, wall_end_point)
      return(new_profile)
    }
    
    write_geo_profile <- function(con, bief_name, prof_name, prof_abs, points_df) {
      writeLines(paste0("PROFIL ", bief_name, " ", prof_name, " ", sprintf("%.3f", prof_abs)), con)
      for (j in 1:nrow(points_df)) {
        writeLines(paste(sprintf("%.3f", points_df$Y[j]), sprintf("%.5f", points_df$Z[j]), points_df$Marker[j]), con)
      }
    }
    
    model_total_length <- dist_amont_limite + nozzle_data$LENGTH + dist_p4_p6
    x_nozzle_start <- dist_amont_limite
    x_nozzle_end <- x_nozzle_start + nozzle_data$LENGTH
    x_coords <- c(0, x_nozzle_start - 2.0, x_nozzle_start, x_nozzle_end, x_nozzle_end + 2.0, model_total_length)
    num_profiles_total <- length(x_coords)
    
    buse_geom <- st_geometry(buse_row)
    
    geo_base_filename <- paste0("geom_buse", numero_buse, ifelse(use_mnt_for_profiles, "_MNT", "_Trapeze"))
    geo_filename_with_ext <- paste0(geo_base_filename, ".geo")
    geo_filepath <- file.path(current_buse_output_dir, geo_filename_with_ext)
    message(paste("Génération du fichier de géométrie :", geo_filename_with_ext))
    
    bief_name <- "Bief_1"
    indices <- list(p1 = 1, p2 = 2, p3 = 3, p4 = 4, p5 = 5, p6 = 6)
    
    all_profiles <- vector("list", num_profiles_total)
    
    # Déterminer une abscisse Y cible unique pour le centrage de tous les profils.
    half_width_calc <- first_profile_shape$bottom_width / 2 + 
      max(first_profile_shape$left_slope, first_profile_shape$right_slope) * first_profile_shape$bank_height_above_bottom + 
      10.0 # bank_y_offset de la fonction generate_points_for_trapezoidal
    target_center_y <- ceiling(half_width_calc) + 5.0 # Ajout d'une marge de sécurité
    message(paste("      - Abscisse Y cible pour le centrage de tous les profils:", sprintf("%.3f", target_center_y)))
    
    if(use_mnt_for_profiles) {
      message("      - Mode MNT: Extraction des profils depuis le MNT...")
      
      buse_coords <- st_coordinates(buse_geom)
      if(nrow(buse_coords) < 2) stop("Géométrie de buse invalide (moins de 2 points) pour la buse ", numero_buse)
      
      start_node <- buse_coords[1, 1:2]
      end_node <- buse_coords[nrow(buse_coords), 1:2]
      direction_vector <- end_node - start_node
      direction_vector_norm <- direction_vector / sqrt(sum(direction_vector^2))
      profile_width_mnt <- 10 * ifelse(CIRC == 1, nozzle_data$HAUT1, nozzle_data$LARG)
      
      pt_extraction_amont <- st_sfc(st_point(start_node - dist_extraction_profil * direction_vector_norm), crs = st_crs(mnt_raster))
      profil_amont_base <- extract_profile_from_mnt(mnt_raster, pt_extraction_amont, direction_vector_norm, profile_width_mnt, num_points_per_profile)
      if(is.null(profil_amont_base)) stop("Échec de l'extraction du profil de base amont. Arrêt.")
      
      pt_extraction_aval <- st_sfc(st_point(end_node + dist_extraction_profil * direction_vector_norm), crs = st_crs(mnt_raster))
      profil_aval_base <- extract_profile_from_mnt(mnt_raster, pt_extraction_aval, direction_vector_norm, profile_width_mnt, num_points_per_profile)
      if(is.null(profil_aval_base)) stop("Échec de l'extraction du profil de base aval. Arrêt.")
      
      # Altitude des profils en extrapolant la pente de la buse (I)
      
      # Profil 1 (limite amont)
      z_profil_1 <- nozzle_data$Z1 + abs(I) * (x_coords[indices$p3] - x_coords[indices$p1])
      profil_1_vshifted <- shift_profile_z(profil_amont_base, z_profil_1)
      profil_1_centered <- center_profile_on_thalweg(profil_1_vshifted, target_center_y)
      profil_1 <- add_vertical_walls(profil_1_centered)
      all_profiles[[indices$p1]] <- list(name = "1", abscissa = x_coords[indices$p1], data = profil_1)
      
      # Profil 2 (juste avant la buse)
      z_profil_2 <- nozzle_data$Z1 + abs(I) * (x_coords[indices$p3] - x_coords[indices$p2])
      profil_2_vshifted <- shift_profile_z(profil_amont_base, z_profil_2)
      profil_2_centered <- center_profile_on_thalweg(profil_2_vshifted, target_center_y)
      profil_2 <- add_vertical_walls(profil_2_centered)
      all_profiles[[indices$p2]] <- list(name = "2", abscissa = x_coords[indices$p2], data = profil_2)
      
      # Profil 5 (juste après la buse)
      z_profil_5 <- nozzle_data$Z2 - abs(I) * (x_coords[indices$p5] - x_coords[indices$p4])
      profil_5_vshifted <- shift_profile_z(profil_aval_base, z_profil_5)
      profil_5_centered <- center_profile_on_thalweg(profil_5_vshifted, target_center_y)
      profil_5 <- add_vertical_walls(profil_5_centered)
      all_profiles[[indices$p5]] <- list(name = "5", abscissa = x_coords[indices$p5], data = profil_5)
      
      # Profil 6 (limite aval)
      z_profil_6 <- nozzle_data$Z2 - abs(I) * (x_coords[indices$p6] - x_coords[indices$p4])
      profil_6_vshifted <- shift_profile_z(profil_aval_base, z_profil_6)
      profil_6_centered <- center_profile_on_thalweg(profil_6_vshifted, target_center_y)
      profil_6 <- add_vertical_walls(profil_6_centered)
      all_profiles[[indices$p6]] <- list(name = "6", abscissa = x_coords[indices$p6], data = profil_6)
      
    } else {
      message("      - Mode Trapézoïdal: Génération des profils génériques...")
      
      calculated_bottom_z <- sapply(x_coords, function(x) {
        if (x <= x_nozzle_start) return(nozzle_data$Z1 + abs(I) * (x_nozzle_start - x))
        if (x > x_nozzle_start && x < x_nozzle_end) return(nozzle_data$Z1 - I * (x - x_nozzle_start))
        return(nozzle_data$Z2 - abs(I) * (x - x_nozzle_end))
      })
      
      for (k in c(indices$p1, indices$p2, indices$p5, indices$p6)) {
        points_raw <- generate_points_for_trapezoidal(
          bottom_width = first_profile_shape$bottom_width, 
          left_slope = first_profile_shape$left_slope, 
          right_slope = first_profile_shape$right_slope, 
          bottom_elevation = calculated_bottom_z[k], 
          bank_height_above_bottom = first_profile_shape$bank_height_above_bottom,
          target_center_y = target_center_y,
          bank_y_offset = 10.0
        )
        points <- add_vertical_walls(points_raw)
        all_profiles[[k]] <- list(name = as.character(k), abscissa = x_coords[k], data = points)
      }
    }
    
    points_p3 <- if(CIRC == 1) create_circular_profile(nozzle_data$HAUT1, nozzle_data$Z1, target_center_y) else create_rectangular_profile(nozzle_data$LARG, nozzle_data$HAUT1, nozzle_data$Z1, target_center_y, preissmann_slot_width)
    all_profiles[[indices$p3]] <- list(name = "3amont", abscissa = x_coords[indices$p3], data = points_p3)
    
    points_p4 <- if(CIRC == 1) create_circular_profile(nozzle_data$HAUT1, nozzle_data$Z2, target_center_y) else create_rectangular_profile(nozzle_data$LARG, nozzle_data$HAUT1, nozzle_data$Z2, target_center_y, preissmann_slot_width)
    all_profiles[[indices$p4]] <- list(name = "3aval", abscissa = x_coords[indices$p4], data = points_p4)
    
    con <- file(geo_filepath, "wt")
    message("      - Écriture ordonnée des profils dans le fichier .geo...")
    for (prof in all_profiles) {
      if (!is.null(prof) && !is.null(prof$data)) {
        write_geo_profile(con, bief_name, prof$name, prof$abscissa, prof$data)
      }
    }
    close(con)
    message("--- Fichier .geo généré avec succès. ---")
    
    #########################################################################
    # PARTIE 2: GÉNÉRATION DES FICHIERS DE LOIS (.loi)
    #########################################################################
    message("\n--- PARTIE 2 : LOIS HYDRAULIQUES (.loi) ---")
    
    duree_totale_sim <- length(debits_q) * (duree_montee + duree_palier)
    hydro_file <- file.path(current_buse_output_dir, "hydrogramme_complet.loi")
    
    con_q <- NULL
    tryCatch({
      con_q <- file(hydro_file, open = "wt")
      writeLines(c("# Hydrogramme...", "# T(s) Q(m3/s)", "S"), con_q)
      current_time <- 0.0
      writeLines(paste(sprintf("%.1f", current_time), "0.20000"), con_q)
      for (q in debits_q) { 
        current_time <- current_time + duree_montee
        writeLines(paste(sprintf("%.1f", current_time), sprintf("%.5f", q)), con_q)
        current_time <- current_time + duree_palier
        writeLines(paste(sprintf("%.1f", current_time), sprintf("%.5f", q)), con_q) 
      }
    }, finally = {
      if(!is.null(con_q)) close(con_q)
    })
    
    message("      - Génération des cotes Z aval selon la logique personnalisée...")
    cotes_z_aval <- c()
    slope=((nozzle_data$Z1-nozzle_data$Z2)/nozzle_data$LENGTH)
    
    z_start = max(nozzle_data$Z2 - 0.2, nozzle_data$Z2 - (slope*dist_p4_p6)/2)
    z_phase1_end = nozzle_data$Z2
    cotes_z_aval1=seq(from = z_start, to = z_phase1_end, by = 0.5)
    z_phase2_end  <- z_phase1_end + nozzle_data$HAUT1+0.2
    cotes_z_aval2=seq(from = z_phase1_end, to = z_phase2_end, by = 0.25)
    # cotes_z_aval2=seq(from = z_phase1_end, to = z_phase2_end, by = 0.1)
    z_phase3_end  <- z_phase1_end + nozzle_data$HAUT1 + 2
    cotes_z_aval3 <- seq(from = z_phase2_end, to = z_phase3_end, by = 1)
    # cotes_z_aval3 <- seq(from = z_phase2_end, to = z_phase3_end, by = 0.5)
    cotes_z_aval <- unique(sort(c(cotes_z_aval1,cotes_z_aval2,cotes_z_aval3)))
    #à enlever
    # z_start=nozzle_data$Z2-(slope*dist_p4_p6)
    # cotes_z_aval<- seq(from = z_start, to = z_phase1_end + nozzle_data$HAUT1+0.5, by = 0.5)
    ##
    message(paste("La cote Z aval minimale sera:", sprintf("%.3f", z_start)))
    message(paste("Nombre de conditions aval générées:", length(cotes_z_aval)))
    
    for (z in cotes_z_aval) { 
      filename_z <- file.path(current_buse_output_dir, paste0("limni_Z", sprintf("%.3f", z), ".loi"))
      con_z <- NULL
      tryCatch({
        con_z <- file(filename_z, open = "wt")
        writeLines(c(paste("# Limnigramme... Z=", sprintf("%.3f", z)), "# T(s) Z(m)", "S"), con_z)
        writeLines(paste(sprintf("%.1f", 0.0), sprintf("%.5f", z)), con_z)
        writeLines(paste(sprintf("%.1f", duree_totale_sim), sprintf("%.5f", z)), con_z)
      }, finally = {
        if(!is.null(con_z)) close(con_z)
      })
    }
    message(paste(length(cotes_z_aval), "fichiers .loi générés avec succès."))
    
    #########################################################################
    # PARTIE 3: GÉNÉRATION DES FICHIERS DE PARAMÈTRES (.xcas)
    #########################################################################
    message("\n--- PARTIE 3 : PARAMÈTRES (.xcas) ---")
    limni_files <- list.files(current_buse_output_dir, pattern = "^limni_Z.*\\.loi$", full.names = TRUE)
    
    for (limni_file in limni_files) {
      current_xcas_lines <- readLines(xcas_template_filename, warn = FALSE)
      limni_id <- sub("^limni_(Z.*)\\.loi$", "\\1", basename(limni_file))
      scenario_base_name <- paste0("buse", numero_buse, "_scenario_", limni_id)
      xcas_output_filename <- file.path(current_buse_output_dir, paste0(scenario_base_name, ".xcas"))
      
      pas_stockage_value <- ceiling((duree_montee + duree_palier) / current_pas_temps)
      critere_arret_value <- 1
      
      current_xcas_lines <- gsub("<fichMotsCles>.*</fichMotsCles>", paste0("<fichMotsCles>", scenario_base_name, ".xcas</fichMotsCles>"), current_xcas_lines)
      current_xcas_lines <- gsub("<fichier>.*</fichier>", paste0("<fichier>", geo_filename_with_ext, "</fichier>"), current_xcas_lines)
      current_xcas_lines <- gsub("<abscFin>.*</abscFin>", paste0("<abscFin>", sprintf("%.3f", model_total_length), "</abscFin>"), current_xcas_lines)
      current_xcas_lines <- gsub("<absFinZone>.*</absFinZone>", paste0("<absFinZone>", sprintf("%.3f", model_total_length), "</absFinZone>"), current_xcas_lines)
      current_xcas_lines <- gsub("<pasTemps>.*</pasTemps>", paste0("<pasTemps>", sprintf("%.1f", current_pas_temps), "</pasTemps>"), current_xcas_lines)
      current_xcas_lines <- gsub("<tempsMax>.*</tempsMax>", paste0("<tempsMax>", sprintf("%.1f", duree_totale_sim), "</tempsMax>"), current_xcas_lines)
      current_xcas_lines <- gsub("<critereArret>.*</critereArret>", paste0("<critereArret>", critere_arret_value, "</critereArret>"), current_xcas_lines)
      current_xcas_lines <- gsub("<pasStock>.*</pasStock>", paste0("<pasStock>", sprintf("%.0f", pas_stockage_value), "</pasStock>"), current_xcas_lines)
      current_xcas_lines <- gsub("<fichResultat>.*</fichResultat>", paste0("<fichResultat>", scenario_base_name, "_ecr.opt</fichResultat>"), current_xcas_lines)
      current_xcas_lines <- gsub("<fichListing>.*</fichListing>", paste0("<fichListing>", scenario_base_name, ".lis</fichListing>"), current_xcas_lines)
      current_xcas_lines <- gsub("<valeursPas>.*</valeursPas>",paste0("<valeursPas>",sprintf("%.2f", val_pas_planimetrage),"</valeursPas>"), current_xcas_lines)
      current_xcas_lines <- gsub("<numDerProf>.*</numDerProf>", paste0("<numDerProf>", num_profiles_total, "</numDerProf>"), current_xcas_lines)
      
      pas_maillage_str <- paste(c(current_pas_maillage[1], current_pas_maillage[2], current_pas_maillage[2], current_pas_maillage[2], current_pas_maillage[3]), collapse = " ")
      current_xcas_lines <- gsub("<nbPlages>.*</nbPlages>", "<nbPlages>5</nbPlages>", current_xcas_lines)
      current_xcas_lines <- gsub("<num1erProfPlage>.*</num1erProfPlage>", "<num1erProfPlage>1 2 3 4 5</num1erProfPlage>", current_xcas_lines)
      current_xcas_lines <- gsub("<numDerProfPlage>.*</numDerProfPlage>", paste0("<numDerProfPlage>2 3 4 5 ", num_profiles_total, "</numDerProfPlage>"), current_xcas_lines)
      current_xcas_lines <- gsub("<pasEspacePlage>.*</pasEspacePlage>", paste0("<pasEspacePlage>", pas_maillage_str, "</pasEspacePlage>"), current_xcas_lines)
      
      start_frottement_idx <- grep("<frottement>", current_xcas_lines)[1]
      end_frottement_idx <- grep("</frottement>", current_xcas_lines)[1]
      if (!is.na(start_frottement_idx) && !is.na(end_frottement_idx)) {
        abs_deb_zone_str <- paste(sprintf("%.3f", x_coords[c(1, 3, 4)]), collapse = " ")
        abs_fin_zone_str <- paste(sprintf("%.3f", x_coords[c(3, 4, 6)]), collapse = " ")
        strickler_zones_str <- paste(sprintf("%.1f", strickler_zones), collapse = " ")
        frottement_block <- c("      <frottement>", "        <loi>1</loi>", "        <nbZone>3</nbZone>", "        <numBranche>1 1 1</numBranche>", paste0("        <absDebZone>", abs_deb_zone_str, "</absDebZone>"), paste0("        <absFinZone>", abs_fin_zone_str, "</absFinZone>"), paste0("        <coefLitMin>", strickler_zones_str, "</coefLitMin>"), paste0("        <coefLitMaj>", strickler_zones_str, "</coefLitMaj>"), "      </frottement>")
        current_xcas_lines <- c(current_xcas_lines[1:(start_frottement_idx - 1)], frottement_block, current_xcas_lines[(end_frottement_idx + 1):length(current_xcas_lines)])
      }
      
      start_index <- grep("<parametresLoisHydrauliques>", current_xcas_lines)[1]
      end_index <- grep("</parametresLoisHydrauliques>", current_xcas_lines)[1]
      if (!is.na(start_index) && !is.na(end_index)) {
        lois_lines <- c("  <parametresLoisHydrauliques>","    <nb>2</nb>","    <lois>","      <structureParametresLoi>","        <nom>hydro_amont</nom>","        <type>1</type>","        <donnees>","          <modeEntree>1</modeEntree>",paste0("          <fichier>", basename(hydro_file), "</fichier>"),"        </donnees>","      </structureParametresLoi>","      <structureParametresLoi>",paste0("        <nom>limni_", limni_id, "</nom>"),"        <type>2</type>","        <donnees>","          <modeEntree>1</modeEntree>",paste0("          <fichier>", basename(limni_file), "</fichier>"),"        </donnees>","      </structureParametresLoi>","    </lois>","  </parametresLoisHydrauliques>")
        current_xcas_lines <- c(current_xcas_lines[1:(start_index-1)], lois_lines, current_xcas_lines[(end_index+1):length(current_xcas_lines)])
      }
      
      con <- NULL
      tryCatch({
        con <- file(xcas_output_filename, "w")
        writeLines(current_xcas_lines, con)
      }, finally = {
        if(!is.null(con)) close(con)
      })
    }
    message("--- Fichiers .xcas générés avec succès. ---")
    
    # PARTIE 4
    message("\n--- PARTIE 4 : LANCEMENT DES SIMULATIONS ---")
    xcas_files <- dir_ls(current_buse_output_dir, regexp = "\\.xcas$", recurse = FALSE)
    registerDoParallel(cores = num_cores_sim)
    foreach( xcas_file_path = xcas_files, .combine = 'c', .errorhandling = 'pass') %dopar% {
      library(fs)
      xcas_filename <- path_file(xcas_file_path)
      xcas_basename <- path_ext_remove(xcas_filename)
      temp_sim_dir <- path(current_buse_output_dir, paste0("temp_", xcas_basename))
      results_dir <- path(current_buse_output_dir, "results_mascaret")
      
      final_opt_path <- path(results_dir, paste0(xcas_basename, "_ecr.opt"))
      
      if(!file_exists(final_opt_path) || file_info(final_opt_path)$size == 0) {
        dir_create(temp_sim_dir)
        file_copy(xcas_file_path, path(temp_sim_dir, xcas_filename), overwrite = TRUE)
        file_copy(path(current_buse_output_dir, geo_filename_with_ext), temp_sim_dir, overwrite = TRUE)
        file_copy(path(current_buse_output_dir, "hydrogramme_complet.loi"), temp_sim_dir, overwrite = TRUE)
        limni_id_for_copy <- sub(paste0("buse", numero_buse, "_scenario_"), "", xcas_basename)
        limni_to_copy <- path(current_buse_output_dir, paste0("limni_", limni_id_for_copy, ".loi"))
        if(file_exists(limni_to_copy)) file_copy(limni_to_copy, temp_sim_dir, overwrite = TRUE)
        
        batch_content <- sprintf('@echo off\nCALL "%s"\ncd /D "%%~dp0"\npython "%%HOMETEL%%\\scripts\\python3\\mascaret.py" "%s"\nexit /b %%ERRORLEVEL%%', telemac_env_script, xcas_filename)
        batch_file_path <- path(temp_sim_dir, "run_simulation.bat")
        writeLines(batch_content, batch_file_path)
        
        system2("cmd", args = c("/c", path_abs(batch_file_path)), stdout = TRUE, stderr = TRUE)
      }
    }
    stopImplicitCluster()
    message("--- Lancement des simulations terminé. ---")
    
    # NOUVELLE PARTIE 4.5: Copie des résultats après la fin de tous les calculs
    message("\n--- PARTIE 4.5 : COLLECTE DES RÉSULTATS ---")
    results_dir <- path(current_buse_output_dir, "results_mascaret")
    dir_create(results_dir)
    temp_dirs <- dir_ls(current_buse_output_dir, regexp = "temp_buse", type = "directory")
    
    for(temp_dir in temp_dirs) {
      opt_files_in_temp <- dir_ls(temp_dir, regexp = "\\.(opt|lis)$")
      if(length(opt_files_in_temp) > 0) {
        file_copy(opt_files_in_temp, results_dir, overwrite = TRUE)
      }
    }
    message("--- Collecte des fichiers .opt terminée. ---")
    
    
    # PARTIE 5
    message("\n--- PARTIE 5 : LECTURE DES RÉSULTATS ET CRÉATION DE L'ABAQUE ---")
    opt_dir <- file.path(current_buse_output_dir, "results_mascaret")
    
    ABSCISSA_BUSE_ENTREE_TARGET <- x_coords[3]
    ABSCISSA_BUSE_SORTIE_TARGET <- x_coords[4]
    
    opt_files <- list.files(opt_dir, pattern = "\\.opt$", full.names = TRUE)
    results_list <- list()
    
    for (opt_file in opt_files) {
      opt_content_lines <- readLines(opt_file, warn = FALSE)
      resultats_start_line_index <- grep("[resultats]", opt_content_lines, fixed = TRUE)
      if (length(resultats_start_line_index) == 0) next
      opt_data_text <- opt_content_lines[(resultats_start_line_index[1] + 1):length(opt_content_lines)]
      if(length(opt_data_text) == 0 || all(nchar(trimws(opt_data_text)) == 0)) next
      
      opt_data <- tryCatch({ fread(text = opt_data_text, sep = ";", strip.white=TRUE, header = FALSE, colClasses = "numeric", fill = TRUE, select = c(1, 4, 6, 11, 12, 14), col.names = c("Time", "Abscissa", "S_value", "Froude", "Velocity", "DebitTotal")) }, error = function(e) { NULL })
      if(is.null(opt_data) || nrow(opt_data) == 0) next
      
      plateau_end_times <- (1:length(debits_q)) * (duree_montee + duree_palier)
      
      for(time_step in plateau_end_times){
        time_to_extract <- time_step - current_pas_temps
        if(time_to_extract < 0 || time_to_extract > max(opt_data$Time)) next
        
        steady_state_data <- opt_data[abs(opt_data$Time - time_to_extract) < 1e-3, ]
        if(nrow(steady_state_data) == 0) next
        
        entree_buse_row <- steady_state_data[which.min(abs(steady_state_data$Abscissa - ABSCISSA_BUSE_ENTREE_TARGET)), ]
        sortie_buse_row <- steady_state_data[which.min(abs(steady_state_data$Abscissa - ABSCISSA_BUSE_SORTIE_TARGET)), ]
        
        if(nrow(entree_buse_row) == 1 && nrow(sortie_buse_row) == 1){
          avg_velocity <- (entree_buse_row$Velocity + sortie_buse_row$Velocity) / 2
          
          results_list[[length(results_list) + 1]] <- data.frame(
            Debit = entree_buse_row$DebitTotal, 
            S1 = entree_buse_row$S_value, 
            S2 = sortie_buse_row$S_value,
            Froude_amont = entree_buse_row$Froude,
            Avg_Velocity = avg_velocity
          )
        }
      }
    }
    
    results_for_file <- data.frame()
    all_results_df <- data.frame()
    results_df_sorted <- data.frame()
    
    if (length(results_list) > 0) {
      all_results_df <- do.call(rbind, results_list)
      s1_max_threshold <- nozzle_data$Z1 + nozzle_data$HAUT1 + 10
      
      all_results_filtered <- all_results_df[
        all_results_df$S1 <= s1_max_threshold & 
          all_results_df$Froude_amont <= 5 &
          (all_results_df$S1 - all_results_df$S2) >= 0.05,
        # all_results_df$Avg_Velocity < 7.0 
      ]
      if(nrow(all_results_filtered) > 0) {
        results_df_unique <- unique(all_results_filtered)
        results_df_sorted <- results_df_unique[order(results_df_unique$Debit, results_df_unique$S1), ]
        
        final_points_list <- lapply(split(results_df_sorted, results_df_sorted$Debit), function(df_debit) {
          torrential_points <- df_debit %>% filter(Froude_amont >= 0.8)
          if (nrow(torrential_points) > 0) return(torrential_points)
          good_fluvial_points <- df_debit %>% filter(Froude_amont < 0.8 & S1 < (nozzle_data$Z1 + nozzle_data$HAUT1) & S2 < (nozzle_data$Z2 + nozzle_data$HAUT1))
          if (nrow(good_fluvial_points) > 0) return(good_fluvial_points)
          return(df_debit %>% filter(Froude_amont < 0.8))
        })
        
        results_for_file <- bind_rows(final_points_list) %>% unique() %>% arrange(Debit, S1)
        if(length(which(results_for_file$Froude_amont>=1))>0){
          max_debit_torrential=max(results_for_file$Debit[which(results_for_file$Froude_amont>=1)])
          max_S1_torrentiel=max(results_for_file$S1[which(results_for_file$Debit==max_debit_torrential)])
          max_S2_torrentiel=max(results_for_file$S2[which(results_for_file$Debit==max_debit_torrential)])
          
          points_fluviaux1=which(results_for_file$Debit<max_debit_torrential & (results_for_file$S1 >max_S1_torrentiel | results_for_file$S2 >max_S2_torrentiel ) )
          points_fluviaux2=which(results_for_file$Debit>max_debit_torrential & results_for_file$S2 >nozzle_data$Z2 + nozzle_data$HAUT1 + 0.05  )
          if(length(points_fluviaux1)>0 | length(points_fluviaux2)>0){
            results_for_file=results_for_file[-c(points_fluviaux1,points_fluviaux2),]
            results_for_file <- results_for_file %>% unique() %>% arrange(Debit, S1)
            
          }
        }
      }
      # REPECHAGE DES DEBITS PERDUS A CAUSE DES FILTRES
      debits_initiaux <- unique(all_results_df$Debit)
      debits_restants <- unique(results_for_file$Debit)
      debits_perdus <- sort(setdiff(debits_initiaux, debits_restants))
      if (length(debits_perdus) > 0 && nrow(results_for_file) > 0) {
        # REPECHAGE INTELLIGENT (Base sur l abaque filtre)
        # On ajoute (0, Z1) comme point d ancrage pour l interpolation vers le bas
        q_valides <- c(0, results_for_file$Debit)
        s1_valides <- c(nozzle_data$Z1, results_for_file$S1)
        
        # On estime un S1 "ideal" pour chaque debit perdu en interpolant les points valides
        s1_ideaux <- approx(q_valides, s1_valides, xout = debits_perdus, rule = 2)$y
        
        points_repeches <- data.frame()
        for (j in 1:length(debits_perdus)) {
          q_perdu <- debits_perdus[j]
          s1_ideal <- s1_ideaux[j]
          candidats <- all_results_df %>% filter(Debit == q_perdu)
          if (nrow(candidats) > 0) {
            # On choisit le candidat dont le S1 est le plus proche de la courbe ideale
            meilleur_idx <- which.min(abs(candidats$S1 - s1_ideal))
            points_repeches <- bind_rows(points_repeches, candidats[meilleur_idx, ])
          }
        }
        results_for_file <- bind_rows(results_for_file, points_repeches) %>% arrange(Debit, S1)
      }
      
      output_txt_file_S_partiel <- file.path(current_buse_output_dir, "abaque_partiel.txt")
      if (nrow(results_for_file) > 0) {
        file_conn_partiel <- file(output_txt_file_S_partiel, "w")
        writeLines(c(as.character(numero_buse), as.character(nrow(results_for_file))), file_conn_partiel)
        cat(paste(sprintf("%.2f", results_for_file$Debit), collapse = ","), "\n", file = file_conn_partiel)
        cat(paste(sprintf("%.2f", results_for_file$S1), collapse = ","), "\n", file = file_conn_partiel)
        cat(paste(sprintf("%.2f", results_for_file$S2), collapse = ","), "\n", file = file_conn_partiel)
        close(file_conn_partiel)
      }
      
      output_txt_file_QS1_partiel <- file.path(current_buse_output_dir, "abaque_partiel_QS1.txt")
      file_conn_QS1 <- file(output_txt_file_QS1_partiel, "w")
      if (nrow(results_for_file) > 0) {
        results_QS1_unique <- results_for_file %>% dplyr::select(Debit, S1) %>% distinct() %>% arrange(Debit, S1)
        writeLines(c(as.character(numero_buse), as.character(nrow(results_QS1_unique))), file_conn_QS1)
        cat(paste(sprintf("%.2f", results_QS1_unique$Debit), collapse = ","), "\n", file = file_conn_QS1)
        cat(paste(sprintf("%.2f", results_QS1_unique$S1), collapse = ","), "\n", file = file_conn_QS1)
      } else {
        writeLines(c(as.character(numero_buse), "0", "", ""), file_conn_QS1)
      }
      close(file_conn_QS1)
    }
    
    if(generate_long_profile_plots) {
      message("\n--- GÉNÉRATION DES PROFILS EN LONG ---")
      for (opt_file in opt_files) {
        scenario_name <- sub("_ecr\\.opt$", "", basename(opt_file)); opt_content_lines <- readLines(opt_file, warn = FALSE); resultats_start_line_index <- grep("[resultats]", opt_content_lines, fixed = TRUE); if (length(resultats_start_line_index) == 0) next
        opt_data_full <- tryCatch({ fread(text = opt_content_lines[(resultats_start_line_index[1] + 1):length(opt_content_lines)], sep = ";", strip.white=TRUE, header = FALSE, colClasses = "numeric", fill = TRUE, select = c(1, 4, 5, 6, 14), col.names = c("Time", "Abscissa", "Z_fond", "Z_eau", "DebitTotal")) }, error = function(e) { NULL })
        if(is.null(opt_data_full) || nrow(opt_data_full) == 0) next
        fig_pl <- plot_ly(); profil_fond <- opt_data_full[abs(Time - min(Time)) < 1e-3, ]; fig_pl <- add_trace(fig_pl, data = profil_fond, x = ~Abscissa, y = ~Z_fond, type = 'scatter', mode = 'lines', line = list(color = 'black', width = 3), name = 'Fond')
        for (time_p in ((1:length(debits_q)) * (duree_montee + duree_palier) - current_pas_temps)) { if (time_p > 0 && time_p <= max(opt_data_full$Time)) { steady_data <- opt_data_full[abs(opt_data_full$Time - time_p) < 1e-3, ]; if (nrow(steady_data) > 0) fig_pl <- add_trace(fig_pl, data = steady_data, x = ~Abscissa, y = ~Z_eau, type = 'scatter', mode = 'lines', name = paste('Q =', round(steady_data$DebitTotal[1], 2), 'm³/s')) } }
        y_range <- range(c(profil_fond$Z_fond, opt_data_full$Z_eau), na.rm = TRUE); fig_pl <- fig_pl %>% layout(title = scenario_name, xaxis = list(title = "Abscisse"), yaxis = list(title = "Altitude", range = y_range), shapes = list(list(type = 'line', x0 = x_coords[indices$p3], x1 = x_coords[indices$p3], y0 = y_range[1], y1 = y_range[2], line = list(color = 'grey', dash = 'dash')), list(type = 'line', x0 = x_coords[indices$p4], x1 = x_coords[indices$p4], y0 = y_range[1], y1 = y_range[2], line = list(color = 'grey', dash = 'dash')), list(type = 'line', x0 = x_coords[indices$p3], x1 = x_coords[indices$p4], y0 = nozzle_data$Z1 + nozzle_data$HAUT1, y1 = nozzle_data$Z2 + nozzle_data$HAUT1, line = list(color = 'red', dash = 'longdash'))))
        saveWidget(fig_pl, file = file.path(opt_dir, paste0("profil_en_long_", scenario_name, ".html")), selfcontained = TRUE)
      }
    }
    
    
    if(generate_abaque_plots) {
      message("\n--- PARTIE 6 : GÉNÉRATION DES GRAPHIQUES D'ABAQUE ---")
      
      # Utiliser `all_results_df` pour définir les points à estomper
      points_to_keep <- results_for_file
      points_to_fade <- dplyr::anti_join(all_results_df, points_to_keep, by = c("Debit", "S1", "S2", "Froude_amont", "Avg_Velocity"))
      
      if (nrow(points_to_keep) > 0 || nrow(points_to_fade) > 0) {
        
        # Préparation des données pour les points principaux
        results_df_graph <- points_to_keep
        if(nrow(results_df_graph) > 0) {
          colnames(results_df_graph) <- c("Debit", "S1_Charge_Amont", "S2_Charge_Aval", "Froude_amont", "Avg_Velocity")
          results_df_graph$Froude_Condition <- cut(results_df_graph$Froude_amont, 
                                                   breaks = c(-Inf, 0.99, 1.01, Inf), 
                                                   labels = c("Fluvial (Fr < 1)", "Critique (Fr = 1)", "Torrentiel (Fr > 1)"))
        }
        
        # --- Graphique 3D ---
        title_3d <- paste("Relation Débit-Charges S1/S2 - Buse", numero_buse, "<br>", title_details_str)
        fig_3d_S <- plot_ly()
        
        if(nrow(points_to_fade) > 0) {
          fig_3d_S <- fig_3d_S %>% add_trace(
            data = points_to_fade, x=~Debit, y=~S2, z=~S1,
            type='scatter3d', mode='markers',
            marker=list(size=3, color='rgba(211, 211, 211, 0.5)'), # Gris clair, semi-transparent
            name = "Points exclus",
            hoverinfo = "text",
            text = ~paste('Exclu<br>Débit:', sprintf("%.2f", Debit), '<br>S1:', sprintf("%.2f", S1), '<br>S2:', sprintf("%.2f", S2), '<br>Froude:', sprintf("%.2f", Froude_amont))
          )
        }
        
        if(nrow(results_df_graph) > 0) {
          fig_3d_S <- fig_3d_S %>% add_trace(
            data = results_df_graph, x=~Debit, y=~S2_Charge_Aval, z=~S1_Charge_Amont,
            type='scatter3d', mode='markers',
            color=~S1_Charge_Amont, colorscale='Viridis',
            symbol=~Froude_Condition, symbols=c('circle','square','triangle-up'),
            marker=list(size=4),
            customdata=~Froude_amont,
            name = "Points de l'abaque",
            hovertemplate=paste('<b>Débit:</b> %{x:.2f} m³/s<br>',
                                '<b>S2 (Aval):</b> %{y:.2f} m<br>',
                                '<b>S1 (Amont):</b> %{z:.2f} m<br>',
                                '<b>Froude (Amont):</b> %{customdata:.2f}<br>',
                                '<extra></extra>')
          )
        }
        
        fig_3d_S <- fig_3d_S %>% layout(title=title_3d,
                                        legend=list(title=list(text='<b>Régime Froude</b>')),
                                        scene=list(xaxis=list(title="Débit (m³/s)"),
                                                   yaxis=list(title="S2 Charge Aval (m)"),
                                                   zaxis=list(title="S1 Charge Amont (m)")))
        
        # --- Graphique 2D ---
        title_2d <- paste("Relation S1-S2 - Buse", numero_buse, "<br>", title_details_str)
        fig_2d_S <- plot_ly()
        
        if(nrow(points_to_fade) > 0) {
          fig_2d_S <- fig_2d_S %>% add_trace(
            data = points_to_fade, x=~S1, y=~S2,
            type='scatter', mode='markers',
            marker=list(size=5, color='rgba(211, 211, 211, 0.6)'), # Gris clair, semi-transparent
            name = "Points exclus",
            hoverinfo = "text",
            text = ~paste('Exclu<br>Débit:', sprintf("%.2f", Debit), '<br>S1:', sprintf("%.2f", S1), '<br>S2:', sprintf("%.2f", S2), '<br>Froude:', sprintf("%.2f", Froude_amont))
          )
        }
        
        if(nrow(results_df_graph) > 0) {
          fig_2d_S <- fig_2d_S %>% add_trace(
            data = results_df_graph, x=~S1_Charge_Amont, y=~S2_Charge_Aval,
            type='scatter', mode='markers',
            color=~Debit, colorscale='Plasma',
            symbol=~Froude_Condition, symbols=c('circle','square','triangle-up'),
            marker=list(size=6),
            customdata=~Froude_amont,
            name = "Points de l'abaque",
            hovertemplate=paste('<b>S1 (Amont):</b> %{x:.2f} m<br>',
                                '<b>S2 (Aval):</b> %{y:.2f} m<br>',
                                '<b>Débit:</b> %{marker.color:.2f} m³/s<br>',
                                '<b>Froude:</b> %{customdata:.2f}<br>',
                                '<extra></extra>')
          )
          
          shapes_list <- list(
            list(type = 'line', x0 = nozzle_data$Z1, x1 = nozzle_data$Z1, y0 = min(results_df_graph$S2_Charge_Aval), y1 = max(results_df_graph$S2_Charge_Aval), line = list(color = 'blue', dash = 'longdash', width = 2)),
            list(type = 'line', x0 = nozzle_data$Z1 + nozzle_data$HAUT1, x1 = nozzle_data$Z1 + nozzle_data$HAUT1, y0 = min(results_df_graph$S2_Charge_Aval), y1 = max(results_df_graph$S2_Charge_Aval), line = list(color = 'red', dash = 'longdash', width = 2)),
            list(type = 'line', y0 = nozzle_data$Z2, y1 = nozzle_data$Z2, x0 = min(results_df_graph$S1_Charge_Amont), x1 = max(results_df_graph$S1_Charge_Amont), line = list(color = 'blue', dash = 'longdash', width = 2)),
            list(type = 'line', y0 = nozzle_data$Z2 + nozzle_data$HAUT1, y1 = nozzle_data$Z2 + nozzle_data$HAUT1, x0 = min(results_df_graph$S1_Charge_Amont), x1 = max(results_df_graph$S1_Charge_Amont), line = list(color = 'red', dash = 'longdash', width = 2))
          )
          fig_2d_S <- fig_2d_S %>% layout(shapes = shapes_list)
        }
        
        fig_2d_S <- fig_2d_S %>% layout(
          title=title_2d,
          xaxis=list(title="S1 Charge Amont (m)"),
          yaxis=list(title="S2 Charge Aval (m)"),
          legend=list(title=list(text='<b>Régime Froude</b>'))
        )
        
        htmlwidgets::saveWidget(fig_3d_S, file.path(current_buse_output_dir, "graph_abaque_S_3d.html"), selfcontained = TRUE)
        htmlwidgets::saveWidget(fig_2d_S, file.path(current_buse_output_dir, "graph_abaque_S_2d.html"), selfcontained = TRUE)
        message("Graphiques d'abaque 2D et 3D (incluant les points exclus) exportés.")
        
      } else {
        message("Aucune donnée à visualiser, les graphiques d'abaque n'ont pas été générés pour cette buse.")
      }
    }
    
    # --- BLOC DE NETTOYAGE SÉLECTIF (AJOUTÉ v123) ---
    if (cleanup_calc_files) {
      message(paste("      - Nettoyage sélectif pour la buse", numero_buse))
      
      # 1. Identifier tous les dossiers temporaires temp_...
      tdirs <- fs::dir_ls(current_buse_output_dir, regexp = "temp_", type = "directory")
      if (length(tdirs) > 1) {
        # Garder uniquement le premier dossier pour vérification, supprimer les autres
        fs::dir_delete(tdirs[-1]) 
      }
      
      # 2. Supprimer le dossier global results_mascaret (doublon inutile une fois les données extraites)
      res_fold <- file.path(current_buse_output_dir, "results_mascaret")
      if(fs::dir_exists(res_fold)) fs::dir_delete(res_fold)
      
      # 3. Supprimer les fichiers d'entrée à la racine (déjà sauvegardés dans le dossier temp conservé)
      root_files <- fs::dir_ls(current_buse_output_dir, regexp = "\\.(xcas|geo|loi|lis)$")
      if(length(root_files) > 0) fs::file_delete(root_files)
    }
  } 
  
  #########################################################################
  # DÉMARRAGE DE LA BOUCLE PRINCIPALE
  #########################################################################
  if (ncore_buse <= 1) {
    message("--- DÉMARRAGE DU TRAITEMENT SÉQUENTIEL ---")
    for (i in 1:nrow(busel_data)) {
      abaques_check_file <- file.path(dsnlayerC, "abaques2.txt")
      if (file.exists(abaques_check_file) && file.info(abaques_check_file)$size > 0) {
        message(paste(">>> BUSE", abaques_check_file, ": DÉJÀ TRAITÉE (Fichier abaque trouvé). PASSAGE À LA SUIVANTE. <<<"))
        next
      }
      process_buse(i, busel_data, main_output_dir, telemac_env_script, xcas_template_filename, use_mnt_for_profiles, mnt_path, dist_amont_limite, dist_p4_p6, num_points_per_profile, dist_extraction_profil, first_profile_shape, duree_palier, duree_montee, num_cores_sim, strickler_zones_base, val_pas_planimetrage, g, pas_maillage_court, pas_temps_court, pas_maillage_moyen, pas_temps_moyen, pas_maillage_long, pas_temps_long, preissmann_slot_width, generate_long_profile_plots, generate_abaque_plots, cleanup_calc_files)
    }
  } else {
    message(paste("--- DÉMARRAGE DU TRAITEMENT PARALLÈLE SUR", ncore_buse, "CŒURS ---"))
    registerDoParallel(cores = ncore_buse)
    foreach(i = 1:nrow(busel_data)) %dopar% { 
      process_buse(i, busel_data, main_output_dir, telemac_env_script, xcas_template_filename, use_mnt_for_profiles, mnt_path, dist_amont_limite, dist_p4_p6, num_points_per_profile, dist_extraction_profil, first_profile_shape, duree_palier, duree_montee, num_cores_sim, strickler_zones_base, val_pas_planimetrage, g, pas_maillage_court, pas_temps_court, pas_maillage_moyen, pas_temps_moyen, pas_maillage_long, pas_temps_long, preissmann_slot_width, generate_long_profile_plots, generate_abaque_plots, cleanup_calc_files) 
    }
    stopImplicitCluster()
  }
  
  #########################################################################
  # PARTIE 7: ASSEMBLAGE FINAL DE L'ABAQUE
  #########################################################################
  message("\n--- PARTIE 7 : ASSEMBLAGE DU FICHIER D'ABAQUE FINAL ---")
  output_txt_file_S_global <- file.path(main_output_dir, "resultats_buse_format_abaqueSQ.txt")
  file_conn_main <- file(output_txt_file_S_global, "w")
  writeLines("#abaqueSQ", file_conn_main)
  output_txt_file_QS1_global <- file.path(main_output_dir, "abaques.txt")
  file_conn_qs1 <- file(output_txt_file_QS1_global, "w")
  writeLines("#abaqueQS1", file_conn_qs1)
  
  for (i in 1:nrow(busel_data)) {
    numero_buse <- busel_data$id[i]
    if(is.null(numero_buse)){
      busel_data$id <- busel_data$ID
      numero_buse <- busel_data$id[i]
    }
    current_buse_output_dir <- file.path(main_output_dir, paste0("buse_", numero_buse))
    
    p1 <- file.path(current_buse_output_dir, "abaque_partiel.txt")
    if (file.exists(p1)) writeLines(readLines(p1), file_conn_main)
    
    p2 <- file.path(current_buse_output_dir, "abaque_partiel_QS1.txt")
    if (file.exists(p2)) writeLines(readLines(p2), file_conn_qs1)
  }
  
  close(file_conn_main); close(file_conn_qs1)
  message("\n--- WORKFLOW COMPLET TERMINÉ ---")
  file.copy(from = file.path(main_output_dir, "abaques.txt"),to = dsnlayerC,overwrite = TRUE,recursive = TRUE)
  # 1. Lire toutes les lignes du fichier d'origine
  lignes <- readLines(file.path(dsnlayerC, "abaques.txt"))
  
  # 2. Nettoyer les éventuelles lignes vides à la fin du fichier
  lignes <- lignes[lignes != ""]
  
  # 3. Calculer le nombre de buses
  # On soustrait 1 (pour la ligne de commentaire) avant de diviser par 4
  n_buses <- floor((length(lignes) - 1) / 4)
  
  # 4. Générer les indices des IDs
  # On commence à la ligne 2, et on fait des sauts de 4
  indices_id <- seq(2, by = 4, length.out = n_buses)
  
  # 5. Remplacer les lignes d'identifiants par la nouvelle numérotation
  lignes[indices_id] <- 1:n_buses
  
  # 6. Sauvegarder le résultat en écrasant le fichier
  writeLines(as.character(lignes), file.path(dsnlayerC, "abaques.txt"))
  # Script R pour lisser les abaques Telemac-2D
  # Objectif : Garantir une croissance stricte des hauteurs d'eau (S1) 
  # pour éviter les divisions par zéro dans DISCHARGE_INTERPOL.
  
  # Noms des fichiers (à adapter si besoin)
  file_in <- file.path(dsnlayerC, "abaques.txt")
  file_out <- file.path(dsnlayerC, "abaques2.txt")
  
  # Lecture du fichier
  lines <- readLines(file_in, warn = FALSE)
  # Nettoyage des lignes vides éventuelles
  lines <- lines[trimws(lines) != ""]
  
  # On conserve l'entête (ex: #abaqueQS1)
  output_lines <- c(lines[1])
  
  idx <- 2
  while (idx <= length(lines)) {
    # Extraction du bloc de 4 lignes
    abaque_id <- trimws(lines[idx])
    n_pts_old <- as.integer(trimws(lines[idx + 1]))
    q_str     <- lines[idx + 2]
    s1_str    <- lines[idx + 3]
    
    # Conversion des lignes Q et S1 en vecteurs numériques
    q_vals  <- as.numeric(unlist(strsplit(q_str, ",")))
    s1_vals <- as.numeric(unlist(strsplit(s1_str, ",")))
    
    # Construction du dataframe pour nettoyage
    df_abaque <- data.frame(Debit = q_vals, S1 = s1_vals)
    
    # 1. Anti-doublons de debit (garder cote max en regime etabli)
    df_abaque <- df_abaque %>%
      group_by(Debit) %>%
      slice_max(S1, n = 1, with_ties = FALSE) %>%
      ungroup() %>%
      arrange(Debit)
    
    # 2. Anti-doublons de hauteur (si S1 stagne)
    df_abaque <- df_abaque %>% distinct(S1, .keep_all = TRUE)
    
    # 3. Filtrage de stricte croissance : S1[i] > S1[i-1]
    if (nrow(df_abaque) > 1) {
      indices_valides <- c(1)
      derniere_cote <- df_abaque$S1[1]
      
      for (idx_pt in 2:nrow(df_abaque)) {
        if (df_abaque$S1[idx_pt] > derniere_cote) {
          indices_valides <- c(indices_valides, idx_pt)
          derniere_cote <- df_abaque$S1[idx_pt]
        }
      }
      df_abaque <- df_abaque[indices_valides, ]
    }
    
    # 4. Interpolation et Extrapolation pour boucher TOUS les trous
    if (nrow(df_abaque) > 0) {
      buse_info <- busel_data[busel_data$id == as.numeric(abaque_id), ]
      if (nrow(buse_info) > 0) {
        Z1 <- buse_info$Z1[1]
        
        # Liste complete theorique des debits (selon ton pas de base)
        Q_max_actuel <- if(nrow(df_abaque) > 0) max(df_abaque$Debit) else 45.0
        q_theorique <- c()
        if (Q_max_actuel >= 0.2) q_theorique <- c(q_theorique, seq(0.2, min(1.0, Q_max_actuel), by = 0.1))
        if (Q_max_actuel >= 1.2) q_theorique <- c(q_theorique, seq(1.2, min(10.0, Q_max_actuel), by = 0.2))
        if (Q_max_actuel >= 11.0) q_theorique <- c(q_theorique, seq(11.0, min(20.0, Q_max_actuel), by = 1.0))
        if (Q_max_actuel >= 22.0) q_theorique <- c(q_theorique, seq(22.0, min(40.0, Q_max_actuel), by = 2.0))
        if (Q_max_actuel >= 45.0) q_theorique <- c(q_theorique, seq(45.0, Q_max_actuel, by = 5.0))
        q_theorique <- unique(sort(round(q_theorique, 2)))
        
        # On ajoute Z1 a Q=0 pour l'extrapolation vers le bas
        df_full <- data.frame(Debit = c(0, q_theorique))
        df_abaque <- merge(df_full, df_abaque, by = "Debit", all.x = TRUE)
        df_abaque$S1[df_abaque$Debit == 0] <- Z1
        
        # Interpolation lineaire pour tous les NA (trous au milieu + vers le bas)
        if (any(is.na(df_abaque$S1))) {
          # approx() interpole lineairement entre les points non-NA
          non_na_idx <- !is.na(df_abaque$S1)
          if (sum(non_na_idx) >= 2) {
            valeurs_interpolees <- approx(df_abaque$Debit[non_na_idx], df_abaque$S1[non_na_idx], xout = df_abaque$Debit, rule = 2)
            df_abaque$S1 <- round(valeurs_interpolees$y, 2)
          } else {
            # Si on n a que Q=0
            df_abaque$S1 <- Z1
          }
        }
        
        # On retire le Q=0 qui ne servait que pour interpoler proprement
        df_abaque <- df_abaque[df_abaque$Debit > 0, ]
      }
    }
    
    n_pts  <- nrow(df_abaque)
    q_out  <- paste(sprintf("%.2f", df_abaque$Debit), collapse = ",")
    s1_out <- paste(sprintf("%.2f", df_abaque$S1), collapse = ",")
    
    # Stockage des 4 lignes du bloc corrigé
    output_lines <- c(output_lines, abaque_id, as.character(n_pts), q_out, s1_out)
    
    # On passe à l'abaque suivant
    idx <- idx + 4
  }
  
  # Écriture du nouveau fichier
  writeLines(output_lines, file_out)
  cat("Traitement terminé avec succès !\nLe fichier corrigé est :", file_out, "\n")
  if(ETAPE[4] == 1){
    
    if(OH_verif==TRUE){
      verifhaut=function(){
        # Chemin vers votre fichier (assurez-vous que le script est dans le même dossier 
        # ou spécifiez le chemin complet)
        fichier_txt <- file.path(dsnlayerC,"OuvHydrau_Select.txt")
        
        # 1. Lecture des deux premières lignes spécifiques du fichier
        cat("Lecture de l'en-tête du fichier...\n")
        entete <- readLines(fichier_txt, n = 2)
        
        # 2. Lecture du tableau de données (en ignorant les 2 premières lignes lues ci-dessus)
        cat("Lecture des données...\n")
        donnees <- read.table(fichier_txt, skip = 2, header = TRUE, stringsAsFactors = FALSE)
        
        # 3. Remplacement des valeurs de HAUT1 par HAUT2 si HAUT1 est NA ou vaut 0
        cat("Application de la correction sur HAUT1...\n")
        donnees$HAUT1 <- ifelse(is.na(donnees$HAUT1) | donnees$HAUT1 == 0, donnees$HAUT2, donnees$HAUT1)
        
        # 4. Écriture du fichier modifié (ceci va écraser l'ancien fichier avec le même nom)
        cat("Sauvegarde du fichier corrigé...\n")
        
        # On écrit d'abord les deux lignes d'en-tête
        writeLines(entete, fichier_txt)
        
        # On ajoute ensuite le tableau de données à la suite du fichier
        write.table(donnees, 
                    fichier_txt, 
                    append = TRUE,      # Ajoute à la suite du writeLines
                    quote = FALSE,      # Pas de guillemets autour des valeurs
                    sep = " ",          # Séparateur espace
                    row.names = FALSE,  # Ne pas écrire les numéros de lignes
                    col.names = TRUE)   # Garder le nom des colonnes
        
        cat("Traitement terminé ! Le fichier", fichier_txt, "a été mis à jour avec succès.\n")
      }
      # verifhaut()
      
    }
  }
}
if(ETAPE[4] == 1){

if(OH_verif==TRUE){
  verifhaut=function(){
    # Chemin vers votre fichier (assurez-vous que le script est dans le même dossier 
    # ou spécifiez le chemin complet)
    fichier_txt <- file.path(dsnlayerC,"OuvHydrau_Select.txt")
    
    # 1. Lecture des deux premières lignes spécifiques du fichier
    cat("Lecture de l'en-tête du fichier...\n")
    entete <- readLines(fichier_txt, n = 2)
    
    # 2. Lecture du tableau de données (en ignorant les 2 premières lignes lues ci-dessus)
    cat("Lecture des données...\n")
    donnees <- read.table(fichier_txt, skip = 2, header = TRUE, stringsAsFactors = FALSE)
    
    # 3. Remplacement des valeurs de HAUT1 par HAUT2 si HAUT1 est NA ou vaut 0
    cat("Application de la correction sur HAUT1...\n")
    donnees$HAUT1 <- ifelse(is.na(donnees$HAUT1) | donnees$HAUT1 == 0, donnees$HAUT2, donnees$HAUT1)
    
    # 4. Écriture du fichier modifié (ceci va écraser l'ancien fichier avec le même nom)
    cat("Sauvegarde du fichier corrigé...\n")
    
    # On écrit d'abord les deux lignes d'en-tête
    writeLines(entete, fichier_txt)
    
    # On ajoute ensuite le tableau de données à la suite du fichier
    write.table(donnees, 
                fichier_txt, 
                append = TRUE,      # Ajoute à la suite du writeLines
                quote = FALSE,      # Pas de guillemets autour des valeurs
                sep = " ",          # Séparateur espace
                row.names = FALSE,  # Ne pas écrire les numéros de lignes
                col.names = TRUE)   # Garder le nom des colonnes
    
    cat("Traitement terminé ! Le fichier", fichier_txt, "a été mis à jour avec succès.\n")
  }
  # verifhaut()
  
}
}
