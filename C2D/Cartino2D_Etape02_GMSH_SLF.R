#################################################################################################
################   Etape2_PPutils_GMSH
#################################################################################################

# Travail PPutils autour de GMSH
Etape2_PPutils_GMSH=function()
{
  setwd(chemin_pputils)
  # réaliser maillage 2D avec GMSH, lancement par ligne de commande :
  # cas ouverture gmsh
  cmd=paste0(Bug_GMSH,"\\gmsh -2 ", paste0(dsnlayerC,"\\",nom_maillage,".geo"),
             " -o ",
             paste0(dsnlayerC,"\\",nom_maillage,".msh2"),
             " -saveall")
  print(cmd);system(cmd)
  if (file.exists(paste0(dsnlayerC,"\\",nom_maillage,".msh2"))==F){cat("PPUTILS ne fonctionne pas");browser()}
  
  # convertir fichier msh en ADCIRC
  cmd=paste0(Bug_python," gmsh2adcirc.py -i ", paste0(dsnlayerC,"\\",nom_maillage,".msh2"),
             " -o ",
             paste0(dsnlayerC,"\\",nom_maillage,".grd"))
  print(cmd);system(cmd)
  if (file.exists(paste0(dsnlayerC,"\\",nom_maillage,".grd"))==F){cat("PPUTILS ne fonctionne pas");browser()}
  
  # Interpolation CN/Strickler
  
  for (nom_Rast in cbind(nom_MNT,"CN","Friction"))
  {
    cat(nom_Rast,"\n")
    if (file.exists(paste0(dsnlayerC,"/",nom_Rast,".csv"))==T)
    {
      cmd=paste0(Bug_python," interp_from_pts.py -p ", paste0(dsnlayerC,"/",nom_Rast,".csv"),
                 " -m ",
                 paste0(dsnlayerC,"\\",nom_maillage,".grd"),
                 " -o ",
                 paste0(dsnlayerC,"\\",nom_maillage,"_",nom_Rast,".grd"),
                 " -n ",as.character(contour$NINTERPMNT))
      print(cmd);system(cmd)
      if (file.exists(paste0(dsnlayerC,"\\",nom_maillage,"_",nom_Rast,".grd"))==F){cat(nom_Rast," PPUTILS ne fonctionne pas");browser()}
    }
  }
}


#################################################################################################
################   Etape2_Maillage_Grille
#################################################################################################
# Maillage directement sur la grille
Etape2_Maillage_Grille=function()
{
  # Boucle sur toutes les lignes
  IncBase=0
  ielt=0
  
  tabMNTl= list()
  itabMNTl=1
  eltl=list()
  ieltl=1
  
  
  Cor_zz <- read.table(file.path(dsnlayerC,"MNT.csv"),sep=",",dec = ".")
  
  Cor_zz <- Cor_zz[which(Cor_zz$V3!=-9999),]
  
  Numb_zz_csv=dim(Cor_zz)[1]
  Cor_zz=cbind(Cor_zz,Numer=1:Numb_zz_csv)
  
  if (max(Cor_zz[,2])>8000000)
  {
    Cor_zz[,2]=round(Cor_zz[,2]-0.49)
    print("ATTENTION GRANDES COORDONNEES DECALAGE DE 0.5")
    print("ATTENTION GRANDES COORDONNEES DECALAGE DE 0.5")
    print("ATTENTION GRANDES COORDONNEES DECALAGE DE 0.5")
    print("ATTENTION GRANDES COORDONNEES DECALAGE DE 0.5")
    print("ATTENTION GRANDES COORDONNEES DECALAGE DE 0.5")
  }
  
  # Analyse des lignes hautes
  # for (HB in c(1,2))
  # {
  #   Verif=1
  #   while (Verif==1)
  #   { 
  #     # Si ca plante par là il y a des piels seuls au début et fin
  #     Verif=0
  #     YHB=ifelse(HB==1,min(Cor_zz[,2]),max(Cor_zz[,2]))
  #     nVal=which(Cor_zz[,2]==YHB)
  #     if (length(nVal)<=1)
  #     {
  #       # Suppression d'un point isolé
  #       Cor_zz=Cor_zz[-nVal,]
  #       Verif=1
  #     }else{
  #       if (length(which(nVal[2:length(nVal)]-nVal[1:(length(nVal)-1)]==1))==0)
  #       {
  #         Cor_zz=Cor_zz[-nVal,] # FRED on pourrrait supprimer que les pixels isolés...
  #         Verif=1
  #       }
  #     }
  #   }
  # }
  ################nouvelle méthode###################################################
  ######################################################################################
  # Script R pour supprimer les points de bordure non supportés par des lignes d'au moins 3 points
  # LOGIQUE MODIFIÉE: un point de bordure est supprimé s'il manque de support en X OU en Y.
  # VERSION OPTIMISÉE: Utilisation d'opérations vectorielles pour accélérer le traitement.
  # Optimisation supplémentaire: Préparation plus rapide des clés de points existants.
  
  # # Charger les données du MNT
  # tryCatch({
  #   dtm_data <- read.csv(file.path(dsnlayerC,"MNT.csv"), header = FALSE, col.names = c("X", "Y", "Z"))
  #   cat("Données MNT chargées avec succès :", nrow(dtm_data), "points initiaux.\n")
  # }, error = function(e) {
  #   stop("Erreur lors de la lecture du fichier MNT.csv : ", e$message, 
  #        "\nVérifiez que le fichier existe et est au bon format.")
  # })
  # 
  # # --- Détermination de la résolution du MNT ---
  # sorted_x <- sort(unique(dtm_data$X))
  # sorted_y <- sort(unique(dtm_data$Y))
  # 
  # if (length(sorted_x) <= 1 || length(sorted_y) <= 1) {
  #   stop("Pas assez de coordonnées X ou Y uniques pour déterminer la résolution.")
  # }
  # 
  # diff_x <- diff(sorted_x)
  # diff_y <- diff(sorted_y)
  # res_x <- min(diff_x[diff_x > 1e-9])
  # res_y <- min(diff_y[diff_y > 1e-9])
  # 
  # if (!is.finite(res_x) || !is.finite(res_y) || res_x <= 1e-9 || res_y <= 1e-9) {
  #   stop("Impossible de déterminer une résolution valide. Vérifiez les coordonnées.")
  # }
  # 
  # cat("Résolution X déterminée :", res_x, "\n")
  # cat("Résolution Y déterminée :", res_y, "\n")
  # 
  # # Déplacements pour les 8 voisins (défini une seule fois)
  # dx_8_neighbors <- c(0,  res_x, res_x, res_x, 0, -res_x, -res_x, -res_x)
  # dy_8_neighbors <- c(res_y, res_y, 0, -res_y, -res_y, -res_y, 0, res_y)
  # 
  # # --- Processus itératif de suppression ---
  # current_dtm <- dtm_data
  # points_removed_total <- 0
  # iteration_count <- 0
  # max_iterations <- 100 
  # 
  # repeat {
  #   iteration_count <- iteration_count + 1
  #   cat("\n--- Itération:", iteration_count, "---\n")
  #   
  #   num_points_start_iter <- nrow(current_dtm)
  #   if (num_points_start_iter == 0) {
  #     cat("Aucun point restant dans le MNT.\n")
  #     break
  #   }
  #   cat("Nombre de points au début de l'itération :", num_points_start_iter, "\n")
  #   
  #   # 1. Obtenir les clés de tous les points MNT actuels pour la recherche rapide
  #   # Ces clés sont simplement les combinaisons "X_Y" des points existants.
  #   # L'environnement n'est plus nécessaire ici car toutes les vérifications utilisent `%in%`.
  #   all_current_point_keys <- paste(current_dtm$X, current_dtm$Y, sep = "_")
  #   # Optionnel: s'assurer de l'unicité si current_dtm pourrait avoir des doublons X,Y.
  #   # Normalement, current_dtm devrait contenir des points uniques (X,Y).
  #   # Si l'unicité n'est pas garantie à 100%, décommentez la ligne suivante :
  #   # all_current_point_keys <- unique(all_current_point_keys)
  #   
  #   # 2. Comptage vectorisé des voisins
  #   num_8_neighbors_vec <- numeric(num_points_start_iter)
  #   for (j_neighbor in 1:8) { # Petite boucle (8 itérations) sur les directions des voisins
  #     neighbor_x_coords <- current_dtm$X + dx_8_neighbors[j_neighbor]
  #     neighbor_y_coords <- current_dtm$Y + dy_8_neighbors[j_neighbor]
  #     neighbor_keys <- paste(neighbor_x_coords, neighbor_y_coords, sep = "_")
  #     # Vérification vectorisée de l'existence des clés
  #     exists_flags <- neighbor_keys %in% all_current_point_keys 
  #     num_8_neighbors_vec <- num_8_neighbors_vec + exists_flags
  #   }
  #   
  #   # Identifier les points de bordure (ceux avec < 8 voisins)
  #   is_border_point_vec <- num_8_neighbors_vec < 8
  #   
  #   # Initialiser le vecteur des points à supprimer (tous à FALSE au début)
  #   points_to_remove_flags <- logical(num_points_start_iter) 
  #   
  #   # Obtenir les indices des points de bordure dans current_dtm
  #   border_point_indices_in_current_dtm <- which(is_border_point_vec)
  #   
  #   if (length(border_point_indices_in_current_dtm) > 0) {
  #     # Coordonnées des points de bordure uniquement
  #     border_px <- current_dtm$X[border_point_indices_in_current_dtm]
  #     border_py <- current_dtm$Y[border_point_indices_in_current_dtm]
  #     
  #     # Fonction utilitaire pour vérifier l'existence de points (vectorisée)
  #     # Elle prend maintenant `all_current_point_keys` comme référence.
  #     check_points_exist_vectorized <- function(x_coords, y_coords, existing_point_keys_vec) {
  #       keys <- paste(x_coords, y_coords, sep = "_")
  #       keys %in% existing_point_keys_vec
  #     }
  #     
  #     # Vérification du support en X pour les points de bordure
  #     bp_m1_x_exists <- check_points_exist_vectorized(border_px - res_x, border_py, all_current_point_keys)
  #     bp_p1_x_exists <- check_points_exist_vectorized(border_px + res_x, border_py, all_current_point_keys)
  #     bp_m2_x_exists <- check_points_exist_vectorized(border_px - 2 * res_x, border_py, all_current_point_keys)
  #     bp_p2_x_exists <- check_points_exist_vectorized(border_px + 2 * res_x, border_py, all_current_point_keys)
  #     
  #     border_supported_in_x <- (bp_m1_x_exists & bp_p1_x_exists) | 
  #       (bp_m1_x_exists & bp_m2_x_exists) | 
  #       (bp_p1_x_exists & bp_p2_x_exists)    
  #     
  #     # Vérification du support en Y pour les points de bordure
  #     bp_m1_y_exists <- check_points_exist_vectorized(border_px, border_py - res_y, all_current_point_keys) 
  #     bp_p1_y_exists <- check_points_exist_vectorized(border_px, border_py + res_y, all_current_point_keys) 
  #     bp_m2_y_exists <- check_points_exist_vectorized(border_px, border_py - 2 * res_y, all_current_point_keys) 
  #     bp_p2_y_exists <- check_points_exist_vectorized(border_px, border_py + 2 * res_y, all_current_point_keys) 
  #     
  #     border_supported_in_y <- (bp_m1_y_exists & bp_p1_y_exists) | 
  #       (bp_m1_y_exists & bp_m2_y_exists) | 
  #       (bp_p1_y_exists & bp_p2_y_exists)    
  #     
  #     # Identifier les points de bordure à supprimer (manque de support en X OU en Y)
  #     border_points_to_remove_subset_flags <- (!border_supported_in_x | !border_supported_in_y)
  #     
  #     # Mettre à jour le vecteur principal points_to_remove_flags
  #     # Seuls les points de bordure qui satisfont la condition de suppression sont marqués TRUE
  #     points_to_remove_flags[border_point_indices_in_current_dtm[border_points_to_remove_subset_flags]] <- TRUE
  #   }
  #   
  #   num_removed_this_iteration <- sum(points_to_remove_flags)
  #   
  #   if (num_removed_this_iteration == 0) {
  #     cat("Aucun point de bordure non supporté (selon la logique OU) à supprimer. Le processus est terminé.\n")
  #     break
  #   } else {
  #     cat("Itération", iteration_count, ": Suppression de", num_removed_this_iteration, "points (logique OU).\n")
  #     current_dtm <- current_dtm[!points_to_remove_flags, ]
  #     points_removed_total <- points_removed_total + num_removed_this_iteration
  #     
  #     if (nrow(current_dtm) == 0) {
  #       cat("Tous les points ont été supprimés.\n")
  #       break
  #     }
  #   }
  #   
  #   if (iteration_count >= max_iterations) {
  #     cat("Attention : Nombre maximum d'itérations (", max_iterations, ") atteint. Arrêt du processus.\n")
  #     break
  #   }
  # }
  # Ce script est une continuation, utilisant la variable 'current_dtm' 
  # issue du script de nettoyage de MNT précédent.
  # 'current_dtm' doit contenir les colonnes X, Y, Z.
  cat("--- Début du script de suppression de pixels (Passes H puis V) ---\n")
  
  # 1. Charger les données du MNT
  tryCatch({
    dtm_data <- read.csv(file.path(dsnlayerC,"MNT.csv"), header = FALSE, col.names = c("X", "Y", "Z"))
    cat("Données MNT chargées avec succès :", nrow(dtm_data), "points initiaux.\n")
  }, error = function(e) {
    stop("Erreur lors de la lecture du fichier MNT.csv : ", e$message)
  })
  
  if (nrow(dtm_data) == 0) {
    stop("Le fichier MNT.csv est vide ou n'a pas pu être lu correctement.")
  }
  
  # 2. Déterminer la résolution
  sorted_x <- sort(unique(dtm_data$X))
  sorted_y <- sort(unique(dtm_data$Y))
  
  if (length(sorted_x) <= 1 || length(sorted_y) <= 1) {
    stop("Pas assez de coordonnées X ou Y uniques pour déterminer la résolution.")
  }
  diff_x <- diff(sorted_x); res_x <- min(diff_x[diff_x > 1e-9])
  diff_y <- diff(sorted_y); res_y <- min(diff_y[diff_y > 1e-9])
  
  if (!is.finite(res_x) || !is.finite(res_y) || res_x <= 1e-9 || res_y <= 1e-9) {
    stop("Impossible de déterminer une résolution valide.")
  }
  cat("Résolution X déterminée :", res_x, "\n")
  cat("Résolution Y déterminée :", res_y, "\n")
  
  # Initialisation pour la boucle itérative principale
  current_dtm_iterative <- dtm_data
  total_points_removed_iterative <- 0
  iteration_loop_count <- 0
  max_iterations <- 50 # Limite de sécurité pour la boucle principale
  
  repeat {
    iteration_loop_count <- iteration_loop_count + 1
    cat("\n--- Itération Principale de suppression de pixels:", iteration_loop_count, "---\n")
    
    points_removed_in_main_iteration <- 0
    
    # --- PASSE 1: SUPPRESSIONS HORIZONTALES ---
    cat("  --- Passe Horizontale ---\n")
    num_points_start_pass_h <- nrow(current_dtm_iterative)
    cat("  Nombre de points au début de la passe H:", num_points_start_pass_h, "\n")
    
    if (num_points_start_pass_h == 0) {
      cat("  Aucun point restant pour la passe H. Arrêt de l'itération principale.\n")
      points_removed_in_main_iteration <- 0 # Assurer l'arrêt
      break
    }
    
    px_vec_h <- current_dtm_iterative$X
    py_vec_h <- current_dtm_iterative$Y
    all_dtm_keys_vector_h <- paste(px_vec_h, py_vec_h, sep = "_")
    
    # Vérification du support horizontal
    l1_h_keys <- paste(px_vec_h - res_x, py_vec_h, sep = "_")
    r1_h_keys <- paste(px_vec_h + res_x, py_vec_h, sep = "_")
    l2_h_keys <- paste(px_vec_h - 2 * res_x, py_vec_h, sep = "_")
    r2_h_keys <- paste(px_vec_h + 2 * res_x, py_vec_h, sep = "_")
    
    l1_h_exists <- l1_h_keys %in% all_dtm_keys_vector_h
    r1_h_exists <- r1_h_keys %in% all_dtm_keys_vector_h
    l2_h_exists <- l2_h_keys %in% all_dtm_keys_vector_h
    r2_h_exists <- r2_h_keys %in% all_dtm_keys_vector_h
    
    supported_horizontally_vec <- (l1_h_exists & r1_h_exists) | # P est au milieu: L-P-R
      (l1_h_exists & l2_h_exists) | # P est à droite: LL-L-P
      (r1_h_exists & r2_h_exists)   # P est à gauche: P-R-RR
    
    indices_to_remove_h <- which(!supported_horizontally_vec)
    points_removed_this_pass_h <- length(indices_to_remove_h)
    
    if (points_removed_this_pass_h > 0) {
      current_dtm_iterative <- current_dtm_iterative[-indices_to_remove_h, , drop = FALSE]
      total_points_removed_iterative <- total_points_removed_iterative + points_removed_this_pass_h
      points_removed_in_main_iteration <- points_removed_in_main_iteration + points_removed_this_pass_h
      cat("  Passe Horizontale :", points_removed_this_pass_h, "points supprimés.\n")
    } else {
      cat("  Passe Horizontale : Aucun point supprimé.\n")
    }
    
    
    # --- PASSE 2: SUPPRESSIONS VERTICALES ---
    cat("  --- Passe Verticale ---\n")
    num_points_start_pass_v <- nrow(current_dtm_iterative)
    cat("  Nombre de points au début de la passe V:", num_points_start_pass_v, "\n")
    
    if (num_points_start_pass_v == 0) {
      cat("  Aucun point restant pour la passe V. Arrêt de l'itération principale.\n")
      # points_removed_in_main_iteration est déjà à 0 ou a la valeur de la passe H
      # Si la passe H a tout supprimé, la condition d'arrêt principale fonctionnera.
      if(points_removed_this_pass_h > 0) {
        # Points ont été supprimés en H, donc l'itération n'est pas "sans changement"
      } else {
        points_removed_in_main_iteration <- 0 # Assurer l'arrêt si H n'a rien fait et V est vide
      }
      break 
    }
    
    px_vec_v <- current_dtm_iterative$X
    py_vec_v <- current_dtm_iterative$Y
    all_dtm_keys_vector_v <- paste(px_vec_v, py_vec_v, sep = "_") # Recalculer avec les points restants
    
    # Vérification du support vertical
    d1_v_keys <- paste(px_vec_v, py_vec_v - res_y, sep = "_") # Down 1
    u1_v_keys <- paste(px_vec_v, py_vec_v + res_y, sep = "_") # Up 1
    d2_v_keys <- paste(px_vec_v, py_vec_v - 2 * res_y, sep = "_")# Down 2
    u2_v_keys <- paste(px_vec_v, py_vec_v + 2 * res_y, sep = "_")# Up 2
    
    d1_v_exists <- d1_v_keys %in% all_dtm_keys_vector_v
    u1_v_exists <- u1_v_keys %in% all_dtm_keys_vector_v
    d2_v_exists <- d2_v_keys %in% all_dtm_keys_vector_v
    u2_v_exists <- u2_v_keys %in% all_dtm_keys_vector_v
    
    supported_vertically_vec <- (d1_v_exists & u1_v_exists) | # P est au milieu: D-P-U
      (d1_v_exists & d2_v_exists) | # P est en haut: DD-D-P
      (u1_v_exists & u2_v_exists)   # P est en bas: P-U-UU
    
    indices_to_remove_v <- which(!supported_vertically_vec)
    points_removed_this_pass_v <- length(indices_to_remove_v)
    
    if (points_removed_this_pass_v > 0) {
      current_dtm_iterative <- current_dtm_iterative[-indices_to_remove_v, , drop = FALSE]
      total_points_removed_iterative <- total_points_removed_iterative + points_removed_this_pass_v
      points_removed_in_main_iteration <- points_removed_in_main_iteration + points_removed_this_pass_v
      cat("  Passe Verticale :", points_removed_this_pass_v, "points supprimés.\n")
    } else {
      cat("  Passe Verticale : Aucun point supprimé.\n")
    }
    
    # Condition d'arrêt de la boucle principale
    if (points_removed_in_main_iteration == 0) {
      cat("Aucun nouveau point unique supprimé dans l'itération principale", iteration_loop_count, ". Arrêt de la boucle.\n")
      break
    }
    if (iteration_loop_count >= max_iterations) {
      cat("Nombre maximum d'itérations principales (", max_iterations, ") atteint. Arrêt de la boucle.\n")
      break
    }
  } # Fin de la boucle repeat principale
  
  current_dtm_cleaned_subtractive <- current_dtm_iterative
  cat("\nNombre total de points supprimés sur toutes les itérations:", total_points_removed_iterative, "\n")
  cat("Taille finale de current_dtm_cleaned_subtractive:", nrow(current_dtm_cleaned_subtractive), "points.\n")
  
  # Tri final
  if (nrow(current_dtm_cleaned_subtractive) > 0) {
    cat("Tri de current_dtm_cleaned_subtractive par Y puis X...\n")
    current_dtm_cleaned_subtractive <- current_dtm_cleaned_subtractive[order(current_dtm_cleaned_subtractive$Y, current_dtm_cleaned_subtractive$X), ]
  }
  
  cat("--- Fin du script de suppression de pixels (Passes H puis V) ---\n")
  current_dtm <- current_dtm_cleaned_subtractive
  if (!exists("current_dtm") || !is.data.frame(current_dtm) || !all(c("X","Y","Z") %in% names(current_dtm))) {
    stop("Le dataframe 'current_dtm' avec les colonnes X, Y, Z n'est pas défini. Veuillez exécuter le script de nettoyage MNT d'abord.")
  }
  
  # Vérifier si dplyr est installé, sinon suggérer l'installation
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Le package 'dplyr' est requis. Veuillez l'installer avec : install.packages('dplyr')", call. = FALSE)
  }
  library(dplyr)
  
  # Renommer les colonnes de current_dtm pour correspondre à V1, V2, V3
  # En supposant que current_dtm a les colonnes X et Y
  if (nrow(current_dtm) > 0) {
    current_dtm <- current_dtm[order(current_dtm$Y,decreasing = TRUE), ]
  }
  if (exists("current_dtm") && nrow(current_dtm) > 0) {
    cat("\n--- Début de la création des masques d'amas et filtrage (méthode terra) ---\n")
    
    num_points_before_component_filter <- nrow(current_dtm)
    
    # Vérifier si le package 'terra' est installé
    if (!requireNamespace("terra", quietly = TRUE)) {
      cat("Le package 'terra' est requis pour cette étape. Veuillez l'installer avec : install.packages('terra')\n")
      cat("L'étape de création des masques d'amas et filtrage va être sautée.\n")
    } else {
      library(terra)
      
      # 1. Préparation et vérification des résolutions et des données
      if (!exists("res_x") || !exists("res_y") || res_x <= 0 || res_y <= 0) {
        stop("Les variables 'res_x' et 'res_y' doivent être définies et positives.")
      }
      cat("Utilisation de res_x =", res_x, "et res_y =", res_y, "\n")
      
      if (!all(c("X", "Y") %in% names(current_dtm))) {
        stop("current_dtm doit contenir les colonnes 'X' et 'Y'.")
      }
      cat("Nombre de points dans current_dtm:", nrow(current_dtm), "\n")
      
      # 2. Créer un objet SpatVector à partir des points
      points_for_rasterize <- current_dtm[, c("X", "Y")]
      points_for_rasterize$value <- 1 # Valeur à assigner aux cellules rasterisées
      
      cat("Conversion de current_dtm en SpatVector...\n")
      points_sv <- tryCatch({
        terra::vect(points_for_rasterize, geom = c("X", "Y"), crs = "") 
      }, error = function(e) {
        cat("Erreur lors de la conversion en SpatVector:", e$message, "\n")
        return(NULL)
      })
      
      if (is.null(points_sv)) {
        cat("Échec de la conversion en SpatVector. Étape de création des masques sautée.\n")
      } else {
        cat(nrow(points_sv), "points convertis en SpatVector.\n")
        
        # 3. Définir l'étendue et créer un raster vide (template)
        min_x <- min(current_dtm$X); max_x <- max(current_dtm$X)
        min_y <- min(current_dtm$Y); max_y <- max(current_dtm$Y)
        
        rast_template <- terra::rast(xmin=min_x, xmax=max_x, ymin=min_y, ymax=max_y, 
                                     resolution=c(res_x, res_y), crs="")
        cat("Raster template créé. Dimensions (lignes, colonnes, couches):", dim(rast_template)[1], dim(rast_template)[2], dim(rast_template)[3], "\n")
        
        # 4. Rasteriser les points
        cat("Rasterisation des points...\n")
        point_raster <- terra::rasterize(points_sv, rast_template, field="value", fun="max", background=NA)
        
        cat("Rasterisation terminée.\n")
        # cat("Résumé des valeurs du raster de points (avant patches):\n"); print(summary(point_raster)) # Peut être verbeux
        
        # 5. Identifier les amas de pixels connectés (patches)
        cat("Identification des amas (patches)...\n")
        patch_raster <- terra::patches(point_raster) 
        
        cat("Identification des amas terminée.\n")
        # cat("Résumé des valeurs du raster des amas (chaque valeur est un ID d'amas):\n"); print(summary(patch_raster)) # Peut être verbeux
        # cat("Nombre d'amas uniques trouvés (excluant NA):", length(unique(values(patch_raster, na.rm=TRUE))), "\n") # Déjà calculé plus tard
        
        # 'patch_raster' contient maintenant les masques des amas.
        cat("\nLe raster 'patch_raster' a été créé. Il contient les masques des amas de pixels.\n")
        # cat("Vous pouvez le visualiser avec : terra::plot(patch_raster, main='Masques des amas de pixels')\n")
        
        # Filtrer current_dtm pour ne garder que les points du plus grand amas:
        cat("\nDébut du filtrage de current_dtm basé sur le plus grand amas...\n")
        # 1. Calculer les tailles des amas:
        patch_values <- terra::values(patch_raster, na.rm = TRUE) # Récupère toutes les valeurs non-NA (IDs des patches)
        
        if (length(patch_values) > 0) {
          patch_table <- table(patch_values) # Compte le nombre de pixels pour chaque ID de patch
          cat("Tailles des amas (ID de l'amas = nombre de pixels dans l'amas):\n"); print(patch_table)
          
          if (length(patch_table) > 0) {
            largest_patch_id <- as.integer(names(patch_table)[which.max(patch_table)])
            cat("ID du plus grand amas:", largest_patch_id, "avec", max(patch_table), "pixels.\n")
            
            # 2. Extraire les labels du raster pour chaque point de current_dtm
            # La fonction extract retourne un dataframe. La colonne avec les valeurs du raster
            # aura le nom de la couche du raster, qui est 'patches' par défaut pour terra::patches().
            cat("Extraction des IDs d'amas pour les points originaux...\n")
            extracted_data <- terra::extract(patch_raster, points_sv)
            # La première colonne de extracted_data est l'ID du SpatVector, la seconde est la valeur extraite.
            # Le nom de la seconde colonne est le nom de la couche de patch_raster.
            # Vérifions le nom de la couche :
            layer_name <- names(patch_raster)[1] # Devrait être "patches"
            cat("Nom de la couche dans patch_raster utilisé pour l'extraction:", layer_name, "\n")
            
            if (!(layer_name %in% names(extracted_data))) {
              stop(paste("La colonne attendue '", layer_name, "' n'est pas trouvée dans les données extraites. Colonnes trouvées:", paste(names(extracted_data), collapse=", ")))
            }
            extracted_labels <- extracted_data[, layer_name]
            
            # 3. Filtrer current_dtm
            # Garder uniquement les points dont l'ID d'amas extrait correspond à largest_patch_id
            # S'assurer de gérer les NA dans extracted_labels (points qui ne tombent sur aucun patch)
            valid_indices_to_keep <- which(extracted_labels == largest_patch_id & !is.na(extracted_labels))
            
            current_dtm_filtered <- current_dtm[valid_indices_to_keep, ]
            cat("Nombre de points dans le plus grand amas (après filtrage de current_dtm):", nrow(current_dtm_filtered), "\n")
            
            if (nrow(current_dtm_filtered) < num_points_before_component_filter) { 
              cat(num_points_before_component_filter - nrow(current_dtm_filtered), "points ont été supprimés (n'appartenant pas au plus grand amas).\n")
            }
            current_dtm <- current_dtm_filtered # Mettre à jour current_dtm
          } else {
            cat("Aucun amas avec des pixels n'a été trouvé dans patch_table. current_dtm n'est pas modifié par cette étape.\n")
          }
        } else {
          cat("Aucun amas (valeur non-NA) trouvé dans patch_raster. current_dtm n'est pas modifié par cette étape.\n")
          # Optionnellement, vider current_dtm si aucun patch n'est trouvé
          # current_dtm <- current_dtm[0, , drop = FALSE] 
        }
      }
    }
  } else {
    cat("\n'current_dtm' est vide ou n'existe pas avant la création des masques d'amas. Étape sautée.\n")
  }
  if (nrow(current_dtm) > 0) {
    # S'assurer que current_dtm a les bonnes colonnes si jamais il en avait plus.
    # Pour ce script, current_dtm devrait toujours avoir X, Y, Z.
    dtm_to_save <- current_dtm[, c("X", "Y", "Z")] 
    # write.table(dtm_to_save, file.path(dsnlayerC,"MNT_2.csv"), sep = ",", row.names = FALSE, col.names = FALSE)
    cat("\nLe MNT traité a été sauvegardé dans 'MNT_2.csv'.\n")
  } else {
    cat("\nLe MNT traité est vide, aucun fichier 'MNT_2.csv' n'a été créé.\n")
  }
   # current_dtm2 <- read.csv(file.path(dsnlayerC,"MNT_2.csv"), header = FALSE)
  
  Cor_zz <- dtm_to_save
  names(Cor_zz) <- c("V1", "V2", "V3") # V1=X, V2=Y, V3=Z
  
  # Ajouter la colonne 'Numer' si elle est utilisée plus tard
  Cor_zz$Numer <- 1:nrow(Cor_zz) 
  
  cat("\n--- Début de la génération du maillage ---\n")
  cat("Nombre de points initiaux pour le maillage :", nrow(Cor_zz), "\n")
  
  # Opérations dplyr initiales de l'utilisateur
  Cor_zz <- Cor_zz %>%
    group_by(V2) %>%
    mutate(ds = c(diff(V1),NA))
  # Cor_zz <- Cor_zz %>%
  #   group_by(V2) %>% # V2 correspond à Y
  #   arrange(V1, .by_group = TRUE) %>% # S'assurer que V1 (X) est trié au sein de chaque groupe V2 (Y)
  #   mutate(ds = c(diff(V1), NA)) %>%
  #   ungroup()
  
  # Suppression conditionnelle de lignes (logique utilisateur)
  if(nrow(Cor_zz) > 0 && length(which(Cor_zz$V2 == Cor_zz$V2[1])) == 1) {
    Cor_zz <- Cor_zz[-1, , drop = FALSE]
  }
  if(nrow(Cor_zz) > 0 && length(which(Cor_zz$V2 == Cor_zz$V2[nrow(Cor_zz)])) == 1) { # Utiliser nrow() pour la dernière ligne
    Cor_zz <- Cor_zz[-nrow(Cor_zz), , drop = FALSE]
  }
  
  # Si Cor_zz devient vide après les suppressions, arrêter proprement.
  if(nrow(Cor_zz) == 0) {
    stop("Cor_zz est vide après les suppressions initiales de lignes. Impossible de continuer la génération du maillage.")
  }
  
  Cor_zz$ID <- 1:nrow(Cor_zz) # ID unique pour chaque point APRÈS les premières suppressions
  
  # Définition de reso_m et header
  # ATTENTION: 'contour$Exzeco' doit être défini dans votre environnement.
  # Si 'contour' n'existe pas, la ligne suivante produira une ERREUR.
  # reso_m <- as.numeric(contour$Exzeco) 
  # Solution de repli / Exemple (à remplacer par votre vraie valeur de reso_m):
  if (!exists("contour") || !("Exzeco" %in% names(contour))) {
    warning("L'objet 'contour' ou 'contour$Exzeco' n'est pas défini. Utilisation d'une valeur par défaut pour reso_m = 25. CECI EST PROBABLEMENT INCORRECT POUR VOS DONNÉES.")
    reso_m <- 25.0 # Remplacez par la résolution spatiale correcte de votre grille (par exemple, 25 si vos points sont espacés de 25m)
  } else {
    reso_m <- as.numeric(contour$Exzeco)
    if(!is.finite(reso_m) || reso_m <= 0){
      stop("reso_m issue de contour$Exzeco n'est pas une valeur positive valide.")
    }
  }
  
  
  header=c((max(Cor_zz[,1])-min(Cor_zz[,1]))/reso_m+1,
           (max(Cor_zz[,2])-min(Cor_zz[,2]))/reso_m+1)
  
  cat("\n Nombre de noeuds après filtrage initial: ", nrow(Cor_zz),"\n")
  
  # Calcul de nrows (index de ligne basé sur Y)
  # Cor_zz <- Cor_zz %>%
  #   arrange(V2, V1) %>% # Assurer l'ordre pour cur_group_id basé sur l'apparition de V2
  #   group_by(V2) %>%
  #   mutate(nrows = cur_group_id()) %>% # Crée un ID pour chaque groupe V2 unique
  #   ungroup()
  Cor_zz <- Cor_zz %>%
    group_by(V2,V2=factor(V2, levels = unique(V2))) %>%
    mutate(nrows = cur_group_id())
  # Calcul de indrow (index de point au sein de chaque ligne Y)
  Cor_zz$nb <- 1
  Cor_zz <- Cor_zz %>%
    group_by(V2) %>%
    mutate(indrow = cumsum(nb))
  
  # Calcul de ncols (index de colonne basé sur X)
  Cor_zz <- Cor_zz %>%
    group_by(V1) %>%
    mutate(ncols = cur_group_id())
  if (nrow(Cor_zz) > 0) {
    Cor_zz <- Cor_zz[order(Cor_zz$V2,decreasing = TRUE), ]
  }
  # Réaffecter ID si la structure de Cor_zz a changé (nombre de lignes)
  # Cependant, l'ID original basé sur Cor_zz après les premières suppressions est celui utilisé pour les éléments.
  # Il est important que les ID dans df_L0 et df_L1 correspondent à une table de points cohérente.
  # L'ID défini AVANT la création de nrows/ncols est probablement celui qui doit être utilisé.
  
  # Préparation finale de Cor_zz pour la création de liste (Cor_zz_df_final dans votre code original)
  # S'assurer que toutes les colonnes nécessaires sont présentes et correctement nommées.
  # On utilise 'ID' de Cor_zz qui a été assigné après les suppressions initiales.
  Cor_zz_for_list <- data.frame(ID = Cor_zz$ID,
                                X = Cor_zz$V1,
                                Y = as.numeric(as.character(Cor_zz$V2)), # S'assurer que Y est numérique
                                Z = round(as.numeric(Cor_zz$V3), digits = 2),
                                indrow = Cor_zz$nrows, # Ceci est l'ID de la ligne Y
                                indpoint = Cor_zz$indrow, # Ceci est l'index du point DANS sa ligne Y
                                indcol = Cor_zz$ncols,   # Ceci est l'ID de la colonne X
                                Numer = Cor_zz$Numer)
  if (nrow(Cor_zz_for_list) > 0) {
    Cor_zz_for_list <- Cor_zz_for_list[order(Cor_zz_for_list$Y,decreasing = TRUE), ]
  }
  # Création de la liste de dataframes, un par 'indrow' (ligne Y)
  Cor_zz_list <- Cor_zz_for_list %>% # Très important: trier pour que group_split et les opérations suivantes soient correctes
    group_by(indrow) %>%
    group_split(.keep = TRUE) # .keep = TRUE est par défaut
  
  # --- Début de la section optimisée pour la boucle de maillage ---
  
  if (length(Cor_zz_list) < 2) {
    stop("Pas assez de lignes (indrow) dans Cor_zz_list pour créer des éléments. Cor_zz_list a ", length(Cor_zz_list), " éléments.")
  }
  
  Cor_zz_list1 <- Cor_zz_list[-length(Cor_zz_list)]
  Cor_zz_list2 <- Cor_zz_list[-1]
  
  # Initialiser une liste pour stocker tous les triangles
  all_triangles_list <- list()
  list_counter <- 1 # Compteur pour all_triangles_list
  
  cat("Si cela plante dans cette étape, c'est souvent parce qu'il y a un trou dans le maillage (NoData)\n")
  cat("Normalement, cela doit être reglé, merci d'envoyer le secteur à problème\n")
  cat("Parfois étendre le buffer ou faire des buffers positfs/négatifs si on utilise des limites administratives (îles) est une solution\n")
  pgb <- txtProgressBar(min = 0, max = length(Cor_zz_list1), style = 3)
  
  for (i in 1:length(Cor_zz_list1)) {
    setTxtProgressBar(pgb, i)
    
    df_L0 <- Cor_zz_list1[[i]] # Ligne Y actuelle
    df_L1 <- Cor_zz_list2[[i]] # Ligne Y suivante
    
    # Créer des vecteurs nommés pour des recherches d'ID rapides par indcol
    lookup_L0_ID_by_indcol <- setNames(df_L0$ID, as.character(df_L0$indcol))
    lookup_L1_ID_by_indcol <- setNames(df_L1$ID, as.character(df_L1$indcol))
    
    # Colonnes 'indcol' communes aux deux lignes Y
    LigIndDiff <- intersect(df_L0$indcol, df_L1$indcol) 
    
    if (length(LigIndDiff) == 0) next # Passer à l'itération suivante si pas de colonnes communes
    
    # Logique pour définir 'secteur_matrix' (blocs contigus de 'indcol' dans LigIndDiff)
    secteur_matrix <- matrix(0, nrow = 0, ncol = 2) 
    LigIndDiff_sorted <- sort(LigIndDiff) # Toujours travailler avec des indcols triés
    
    if (length(LigIndDiff_sorted) == 1) {
      secteur_matrix <- rbind(secteur_matrix, c(LigIndDiff_sorted[1], LigIndDiff_sorted[1]))
    } else if (length(LigIndDiff_sorted) > 1) {
      EcLig <- diff(LigIndDiff_sorted)
      if (all(EcLig == 1)) { 
        secteur_matrix <- rbind(secteur_matrix, c(min(LigIndDiff_sorted), max(LigIndDiff_sorted)))
      } else {
        nbs <- which(EcLig > 1)
        deb_idx <- 1
        for (is_nbs in 1:length(nbs)) {
          secteur_matrix <- rbind(secteur_matrix, c(LigIndDiff_sorted[deb_idx], LigIndDiff_sorted[nbs[is_nbs]]))
          deb_idx <- nbs[is_nbs] + 1
        }
        secteur_matrix <- rbind(secteur_matrix, c(LigIndDiff_sorted[deb_idx], LigIndDiff_sorted[length(LigIndDiff_sorted)]))
      }
    }
    
    if (nrow(secteur_matrix) > 0) {
      for (is_secteur in 1:nrow(secteur_matrix)) {
        current_secteur_start_indcol <- secteur_matrix[is_secteur, 1]
        current_secteur_end_indcol <- secteur_matrix[is_secteur, 2]
        
        # Création des triangles principaux pour le secteur
        if (current_secteur_start_indcol <= current_secteur_end_indcol - 1) { # Nécessite au moins 2 colonnes dans le secteur pour former un quad
          indcol_seq_part1_char <- as.character(current_secteur_start_indcol : (current_secteur_end_indcol - 1))
          indcol_seq_part2_char <- as.character((current_secteur_start_indcol + 1) : current_secteur_end_indcol)
          
          ids_L0_part1 <- lookup_L0_ID_by_indcol[indcol_seq_part1_char]
          ids_L0_part2 <- lookup_L0_ID_by_indcol[indcol_seq_part2_char]
          ids_L1_part1 <- lookup_L1_ID_by_indcol[indcol_seq_part1_char]
          ids_L1_part2 <- lookup_L1_ID_by_indcol[indcol_seq_part2_char]
          
          # Filtrer les NAs (points non trouvés dans les lookups pour une indcol donnée)
          valid_indices1 <- !is.na(ids_L0_part1) & !is.na(ids_L0_part2) & !is.na(ids_L1_part2)
          if(sum(valid_indices1) > 0){ # sum(TRUE) > 0
            eltaj_main1 <- cbind(ids_L0_part1[valid_indices1], ids_L0_part2[valid_indices1], ids_L1_part2[valid_indices1])
            all_triangles_list[[list_counter]] <- eltaj_main1; list_counter <- list_counter + 1
          }
          
          valid_indices2 <- !is.na(ids_L0_part1) & !is.na(ids_L1_part2) & !is.na(ids_L1_part1)
          if(sum(valid_indices2) > 0){
            eltaj_main2 <- cbind(ids_L0_part1[valid_indices2], ids_L1_part2[valid_indices2], ids_L1_part1[valid_indices2])
            all_triangles_list[[list_counter]] <- eltaj_main2; list_counter <- list_counter + 1
          }
        }
        
        # --- GESTION BORDS ---
        prev_indcol_char <- as.character(current_secteur_start_indcol - 1)
        curr_start_indcol_char <- as.character(current_secteur_start_indcol)
        next_indcol_char <- as.character(current_secteur_end_indcol + 1)
        curr_end_indcol_char <- as.character(current_secteur_end_indcol)
        
        # Bord gauche du secteur
        if (current_secteur_start_indcol > 1) { 
          id_L0_prev <- lookup_L0_ID_by_indcol[prev_indcol_char]; id_L0_curr_start <- lookup_L0_ID_by_indcol[curr_start_indcol_char]
          id_L1_curr_start <- lookup_L1_ID_by_indcol[curr_start_indcol_char]; id_L1_prev <- lookup_L1_ID_by_indcol[prev_indcol_char]
          
          if (!is.na(id_L0_prev) && !is.na(id_L0_curr_start) && !is.na(id_L1_curr_start)) {
            all_triangles_list[[list_counter]] <- matrix(c(id_L0_prev, id_L0_curr_start, id_L1_curr_start), nrow = 1); list_counter <- list_counter + 1
          }
          if (!is.na(id_L0_curr_start) && !is.na(id_L1_curr_start) && !is.na(id_L1_prev)) {
            all_triangles_list[[list_counter]] <- matrix(c(id_L0_curr_start, id_L1_curr_start, id_L1_prev), nrow = 1); list_counter <- list_counter + 1
          }
        }
        
        # Bord droit du secteur
        if (current_secteur_end_indcol < header[1]) { 
          id_L0_curr_end <- lookup_L0_ID_by_indcol[curr_end_indcol_char]; id_L0_next <- lookup_L0_ID_by_indcol[next_indcol_char]
          id_L1_curr_end <- lookup_L1_ID_by_indcol[curr_end_indcol_char]; id_L1_next <- lookup_L1_ID_by_indcol[next_indcol_char]
          
          if (!is.na(id_L0_curr_end) && !is.na(id_L0_next) && !is.na(id_L1_curr_end)) {
            all_triangles_list[[list_counter]] <- matrix(c(id_L0_curr_end, id_L0_next, id_L1_curr_end), nrow = 1); list_counter <- list_counter + 1
          }
          # Correction pour la dernière partie de la gestion des bords droits (bas plus à droite)
          # L'original utilisait Lig1Num$ID[which(Lig1Num$indcol==(secteur[is,2]))] soit id_L1_curr_end
          if (!is.na(id_L0_curr_end) && !is.na(id_L1_next) && !is.na(id_L1_curr_end)) { 
            all_triangles_list[[list_counter]] <- matrix(c(id_L0_curr_end, id_L1_next, id_L1_curr_end), nrow = 1); list_counter <- list_counter + 1
          }
        }
      } 
    } 
  } 
  close(pgb)
  cat("\n")
  
  # Combiner tous les triangles stockés dans la liste
  if (length(all_triangles_list) > 0) {
    Ele_m <- do.call(rbind, all_triangles_list)
    Ele_m <- as.data.frame(Ele_m)
    # Ele_m <- Ele_m[-1,]
    # S'assurer que les noms de colonnes sont V1, V2, V3 pour correspondre à la suite
    if (ncol(Ele_m) == 3) {
      names(Ele_m) <- c("V1", "V2", "V3") 
    } else {
      warning("Ele_m n'a pas 3 colonnes après rbind. Vérifiez la logique de création des triangles.")
    }
    Ele_m$ID <- 1:nrow(Ele_m)
    Ele_m$nb <- 3 # Nombre de noeuds par élément (triangle)
  } else {
    Ele_m <- data.frame(V1=integer(), V2=integer(), V3=integer(), ID=integer(), nb=integer())
    cat("Attention : Aucun triangle n'a été généré. Ele_m est vide.\n")
  }
  
  # La suite de votre code utilise Ele_m
  cat("Nombre d'éléments (triangles) générés :", nrow(Ele_m), "\n")

  
  
  
  
  
  ########################################################################################
  ########################################################################################
  # Cor_zz <- Cor_zz %>% 
  #   group_by(V2) %>% 
  #   mutate(ds = c(diff(V1),NA))
  # 
  # if(length(which(Cor_zz$V2==Cor_zz$V2[1]))==1)
  # {
  #   Cor_zz <- Cor_zz[-1,]
  # }
  # if(length(which(Cor_zz$V2==Cor_zz$V2[length(Cor_zz$V2)]))==1)
  # {
  #   Cor_zz <- Cor_zz[-length(Cor_zz$V2),]
  # }
  # 
  # Cor_zz$ID <- 1:length(Cor_zz$V1)
  # reso_m <- as.numeric(contour$Exzeco)
  # 
  # header=c((max(Cor_zz[,1])-min(Cor_zz[,1]))/reso_m+1,
  #          (max(Cor_zz[,2])-min(Cor_zz[,2]))/reso_m+1)
  # 
  # 
  # # drix <- which(Cor_zz$ds>reso_m)
  # # Iav=0
  # # 
  # # pgb <- txtProgressBar(min = 0, max = max(1,length(drix)),style=3)
  # # for (slix in drix) 
  # # {
  # #   Iav=Iav+1
  # #   setTxtProgressBar(pgb, Iav)
  # #   if(slix> 1 &(Cor_zz[which(Cor_zz$ID==slix),5]>reso_m & is.na(Cor_zz[which(Cor_zz$ID==(slix-1)),5]))){
  # #     Cor_zz = Cor_zz[-which(Cor_zz$ID==slix),]
  # #   }
  # #   else if(slix> 1 &(Cor_zz[which(Cor_zz$ID==slix),5]>reso_m & is.na(Cor_zz[which(Cor_zz$ID==(slix+1)),5]))){
  # #     Cor_zz = Cor_zz[-which(Cor_zz$ID==(slix+1)),]
  # #   }
  # #   else if(slix> 1 & ((Cor_zz[which(Cor_zz$ID==(slix-1)),5]!=reso_m & Cor_zz[which(Cor_zz$ID==slix),5]>reso_m) | (Cor_zz[which(Cor_zz$ID==(slix-1)),5]>reso_m & is.na(Cor_zz[which(Cor_zz$ID==slix),5])))) {
  # #     Cor_zz = Cor_zz[-which(Cor_zz$ID==slix),]
  # #   }
  # #   else if(slix==1 & Cor_zz[which(Cor_zz$ID==slix),5]>reso_m ){
  # #     Cor_zz = Cor_zz[-which(Cor_zz$ID==slix),]
  # #   }
  # # }
  # # setTxtProgressBar(pgb, max(1,length(drix)))
  # 
  # cat("\n Nombre de noeuds: ",length(Cor_zz$V1),"\n")
  # 
  # Cor_zz <- Cor_zz %>% 
  #   group_by(V2,V2=factor(V2, levels = unique(V2))) %>% 
  #   mutate(nrows = cur_group_id())
  # 
  # Cor_zz$nb <- 1
  # 
  # Cor_zz <- Cor_zz %>% 
  #   group_by(V2) %>% 
  #   mutate(indrow = cumsum(nb))
  # 
  # Cor_zz <- Cor_zz %>% 
  #   group_by(V1) %>% 
  #   mutate(ncols = cur_group_id())
  # Cor_zz$ID <- 1:length(Cor_zz$ID)
  # 
  # Cor_zz <- data.frame(ID = Cor_zz$ID,
  #                      X = Cor_zz$V1,
  #                      Y = as.numeric(as.character(Cor_zz$V2)),
  #                      Z = round(as.numeric(Cor_zz$V3),digits = 2),
  #                      indrow = Cor_zz$nrows,
  #                      indpoint = Cor_zz$indrow,
  #                      indcol = Cor_zz$ncols,
  #                      Numer = Cor_zz$Numer)
  # 
  # Cor_zz_list <- Cor_zz %>% 
  #   group_by(indrow) %>% 
  #   group_split(.keep = T)
  # 
  # ielt=0
  # 
  # Cor_zz_list1 <- Cor_zz_list[-length(Cor_zz_list)]
  # Cor_zz_list2 <- Cor_zz_list[-1]
  # 
  # Ele_m <- matrix(ncol = 3)
  # 
  # cat("Si cela plante dans cette étape, c'est souvent parce qu'il y a un trou dans le maillage (NoData)\n")
  # cat("Normalement, cela doit être reglé, merci d'envoyer le secteur à problème\n")
  # cat("Parfois étendre le buffer ou faire des buffers positfs/négatifs si on utilise des limites administratives (îles) est une solution\n")
  # pgb <- txtProgressBar(min = 0, max = length(Cor_zz_list1),style=3)
  # for (i in 1:length(Cor_zz_list1)) {
  #   setTxtProgressBar(pgb, i)
  #   secteur <- cbind(0,0)
  #   Lig0Num <- data.frame(indcol=Cor_zz_list1[[i]]$indcol, ID = Cor_zz_list1[[i]]$ID)
  #   Lig1Num <- data.frame(indcol=Cor_zz_list2[[i]]$indcol, ID = Cor_zz_list2[[i]]$ID)
  #   LigIndDiff=intersect(Lig0Num$indcol,Lig1Num$indcol)
  #   EcLig=(LigIndDiff[-1]-LigIndDiff[-length(LigIndDiff)])
  #   if (max(EcLig)==1)
  #   {secteur[1,1:2]=cbind(min(LigIndDiff),max(LigIndDiff))}
  #   else
  #   {
  #     nbs=which(EcLig>1)
  #     for (is in 1:length(nbs))
  #     {
  #       if (is==1)
  #       {deb=1
  #       secteur[,1:2]=cbind(LigIndDiff[deb],LigIndDiff[nbs[is]])}
  #       else
  #       {secteur=rbind(secteur,
  #                      cbind(LigIndDiff[deb],LigIndDiff[nbs[is]]))
  #       }
  #       deb=nbs[is]+1
  #     }
  #     secteur=rbind(secteur,
  #                   cbind(LigIndDiff[deb],LigIndDiff[length(LigIndDiff)]))
  #   }
  #   for (is in 1:dim(secteur)[1])
  #   {
  #     eltaj=rbind(
  #       cbind(Lig0Num$ID[which(Lig0Num$indcol==(secteur[is,1])):which(Lig0Num$indcol==(secteur[is,2]-1))],
  #             Lig0Num$ID[which(Lig0Num$indcol==(secteur[is,1]+1)):which(Lig0Num$indcol==(secteur[is,2]))],
  #             Lig1Num$ID[which(Lig1Num$indcol==(secteur[is,1]+1)):which(Lig1Num$indcol==(secteur[is,2]))]),
  #       cbind(Lig0Num$ID[which(Lig0Num$indcol==(secteur[is,1])):which(Lig0Num$indcol==(secteur[is,2]-1))],
  #             Lig1Num$ID[which(Lig1Num$indcol==(secteur[is,1]+1)):which(Lig1Num$indcol==(secteur[is,2]))],
  #             Lig1Num$ID[which(Lig1Num$indcol==(secteur[is,1])):which(Lig1Num$indcol==(secteur[is,2]-1))]))
  #     
  #     
  #     if(ielt==0)
  #     {
  #       
  #       Ele_m <- rbind(Ele_m,eltaj)
  #       
  #       ielt=1
  #     }else{
  #       
  #       Ele_m <- rbind(Ele_m,eltaj)
  #     }
  #     
  #     ################# GESTION BORDS
  #     # Début de séquence
  #     if (secteur[is,1]>1)
  #     {
  #       # Haut plus à gauche
  #       if (length(which(Lig0Num$indcol==(secteur[is,1]-1)))>0){
  #         if (Lig0Num$indcol[which(Lig0Num$indcol==(secteur[is,1]-1))]>0)
  #         {eltaj=cbind(Lig0Num$ID[which(Lig0Num$indcol==(secteur[is,1]-1))],
  #                      Lig0Num$ID[which(Lig0Num$indcol==(secteur[is,1]))],
  #                      Lig1Num$ID[which(Lig1Num$indcol==(secteur[is,1]))])
  #         Ele_m <- rbind(Ele_m,eltaj)
  #         }
  #       }
  #       #bas plus à gauche
  #       if(length(which(Lig1Num$indcol==(secteur[is,1]-1)))>0){
  #         if (Lig1Num$indcol[which(Lig1Num$indcol==(secteur[is,1]-1))]>0)
  #         {eltaj=cbind(Lig0Num$ID[which(Lig0Num$indcol==(secteur[is,1]))],
  #                      Lig1Num$ID[which(Lig1Num$indcol==(secteur[is,1]))],
  #                      Lig1Num$ID[which(Lig1Num$indcol==(secteur[is,1]-1))])
  #         Ele_m <- rbind(Ele_m,eltaj)
  #         }
  #       }
  #     }
  #     # Fin de séquence
  #     if (secteur[is,2]<header[1])
  #     {
  #       # Haut plus à droite
  #       if(length(which(Lig0Num$indcol==(secteur[is,2]+1)))>0){
  #         if (Lig0Num$indcol[which(Lig0Num$indcol==(secteur[is,2]+1))]>0)
  #         {eltaj=cbind(Lig0Num$ID[which(Lig0Num$indcol==(secteur[is,2]))],
  #                      Lig0Num$ID[which(Lig0Num$indcol==(secteur[is,2]+1))],
  #                      Lig1Num$ID[which(Lig1Num$indcol==(secteur[is,2]))])
  #         Ele_m <- rbind(Ele_m,eltaj)
  #         }
  #       }
  #       #bas plus à droite
  #       if(length(which(Lig1Num$indcol==(secteur[is,2]+1)))>0){
  #         if (Lig1Num$indcol[which(Lig1Num$indcol==(secteur[is,2]+1))]>0)
  #         {eltaj=cbind(Lig0Num$ID[which(Lig0Num$indcol==(secteur[is,2]))],
  #                      Lig1Num$ID[which(Lig1Num$indcol==(secteur[is,2]+1))],
  #                      Lig1Num$ID[which(Lig1Num$indcol==(secteur[is,2]))])
  #         Ele_m <- rbind(Ele_m,eltaj)
  #         }
  #       }
  #     }
  #   }
  # }
  # cat("\n")
  # 
  # Ele_m <- Ele_m[-1,]
  # Ele_m <- as.data.frame(Ele_m)
  # Ele_m$ID <- 1:length(Ele_m$V1)
  # Ele_m$nb <- 3
  
  # pts <- Cor_zz[,1:4]
  pts <- Cor_zz_for_list[,1:4]
  Elems <- data.frame(ID = Ele_m$ID,
                      nb = Ele_m$nb,
                      V1 = Ele_m$V1,
                      V2 = Ele_m$V2,
                      V3 = Ele_m$V3)
  # tabMNT=read.table ("Cor.txt")
  # elt=as.matrix(read.table ("Ele.txt"))
  # Suppression des éléments mauvais
  
  IndPtsElt=sort(unique(rbind(as.matrix(Ele_m[,1]),as.matrix(Ele_m[,2]),as.matrix(Ele_m[,3]))))
  
  if (length(pts$ID)!=length(IndPtsElt))
  {
    zz_maillagedelicat=file.path(dsnlayerC,"MailleurRasterDelicat2.txt")
    file.create(zz_maillagedelicat)
    IndBug=pts[-intersect(pts[,1],IndPtsElt),1]
    pts=pts[-IndBug,]
    print(IndBug)
    for (imodif in 1:length(IndBug))
    {
      nptsbug=which(pts[,1]>IndBug[imodif])
      pts[nptsbug,1]=pts[nptsbug,1]-1
      
      for (ielt in 3:5)
      {
        neltbug=which(Elems[,ielt]>IndBug[imodif])
        Elems[neltbug,ielt]= Elems[neltbug,ielt]-1
      }
      IndBug=IndBug-1
      IndBug[imodif]=imodif
      
    }
    
  }
  
  setwd(dsnlayerC)
  
  # Export des points
  Export_GRD(paste0(dsnlayerC,"\\",nom_maillage,"_",nom_MNT,".grd"),
             paste(length(Elems$ID)," ",length(pts$ID)),
             pts,
             Elems)
  
  # ### exporter les GRd CN et Friction
  ParamCSV=rbind(cbind("Friction","friction",15),
                 cbind("CN","CN",70))
  for (i in 1:2)
  {
    NomCSV_P=paste0(dsnlayerC,"/",ParamCSV[i,1],".csv")
    NomGRD_P=paste0(dsnlayerC,"\\",nom_maillage,"_",ParamCSV[i,2],".grd")
    if (file.exists(NomCSV_P)==T)
    {
      Cor_P <- read.table(NomCSV_P,sep=",") 
      Numb_P_csv=dim(Cor_P)[1]
      if (Numb_P_csv==Numb_zz_csv)
      {
        tab_Pz = inner_join(x = Cor_zz_for_list,y =Cor_P,by = c("X"="V1","Y"="V2") )
        # tab_P= cbind(Cor_zz_for_list[,1],Cor_P[Cor_zz_for_list[,1],])
      }else{
        cat("Les données spatialisées",ParamCSV[i,1],"ne sont pas cohérentes avec les points du maillage XYZ\n")
        cat("L'emprise de vos données est sans doute différente\n")
        colnames(Cor_P)=cbind("X","Y","V3")
        Cor_P$Id=1:nrow(Cor_P)
        resultat <- anti_join(Cor_P,Cor_zz_for_list, by = c("X","Y"))
        if (nrow(resultat)>0)
        {
          cat("Traitement automatique du problème\n")
          Cor_P=Cor_P[-resultat$Id,]
          tab_P= cbind(Cor_zz[,1],Cor_P[Cor_zz[,1],1:3])
        }else{
          cat(ParamCSV[i,1],"mis à ",ParamCSV[i,3],"par défaut si vous continuer le process\n");browser()
          tab_P= cbind(pts[,1:3],15)
        }
      }
      Export_GRD(NomGRD_P,
                 paste(length(Elems$ID)," ",length(pts$ID)),
                 tab_Pz[,c(1:3,9)],
                 Elems)
    }
  }
}

#################################################################################################
################   Etape2_Fr_CN_SLF
#################################################################################################

# Préparation du CN, Friction et conversion MSH2 en SELAFIN
Etape2_Fr_CN_SLF=function()
{
  # 5.11.1 TELEMAC inutile, on veut avec fritction et cn
  # cmd=paste0(Bug_python," adcirc2sel.py -i ", paste0(dsnlayerC,"\\",nom_maillage,".grd"),
  #            " -p single -o ",
  #            paste0(dsnlayerC,"\\",nom_maillage,".slf"))
  # print(cmd)
  # texte=rbind(texte,cmd)
  
  # Lecture des fichier grd
  # Lecture des points des bords
  # zzcn <- file(file.path(dsnlayerC,paste0(nom_maillage,"_CN")))
  
  # IMPORTANT
  # Le curve number a 2 options
  # 1/ soit on met la valeur directement dans telemac
  #     Cela implique de prendre la totalité de la pluie
  # 2/ soit on met la valeur 100 dans telemac et on applique la valeur définie dans le hyétogramme d'entrée
  #     Cela implique d'avoir des pluies plutôt statistique avec un pic et cela permet de réduire drastiquement
  #     le temps de pluie réel
  #     Ex: Une pluie shyreg de 72h passe en 2-3h avec cette option
  # Maintenant les 2 exports sont réalisés dès le début pour permettre à l'utilisateur de revenir sur ses choix ensuite
  
  if (file.exists(paste0(dsnlayerC,"/","CN",".csv"))==F | file.exists(paste0(dsnlayerC,"/","Friction",".csv"))==F)
  {
    options( "digits"=7, "scipen"=0)
    
    num=as.numeric(scan(file.path(dsnlayerC,paste0(nom_maillage,"_",nom_MNT,".grd")),skip=1,nlines=1))
    
    setwd(dsnlayerC)
    
    coord_data <- read.table(file.path(dsnlayerC,paste0(nom_maillage,"_",nom_MNT,".grd")),
                             skip = 2,header = FALSE, nrows = num[2])
    element_data <- read.table(file.path(dsnlayerC,paste0(nom_maillage,"_",nom_MNT,".grd")), skip = num[2]+2)
    
    # pts_tmp=pts
    # pts[,5]=as.numeric(contour$Friction)
    
    if (file.exists(paste0(dsnlayerC,"/","Friction",".csv"))==F)
    {
      Export_GRD(paste0(dsnlayerC,"\\",nom_maillage,"_friction.grd"),
                 paste(num[1],num[2]),
                 c(coord_data[,c(1,2,3)],as.numeric(contour$Friction)),
                 element_data)
    }
    
    if (file.exists(paste0(dsnlayerC,"/","CN",".csv"))==F)
    {
      Export_GRD(paste0(dsnlayerC,"\\",nom_maillage,"_CN.grd"),
                 paste(num[1],num[2]),
                 c(coord_data[,c(1,2,3)],as.numeric(contour$CN)),
                 element_data)
      
      if(CN_100)
      {
        Export_GRD(paste0(dsnlayerC,"\\",nom_maillage,"_CN100.grd"),
                   paste(num[1],num[2]),
                   c(coord_data[,c(1,2,3)],100),
                   element_data)
      }
    }
    
    if(CN_100)
    {
      nomCN100slf=paste0(dsnlayerC,"\\",nom_Telemac,"_CN100.slf")
      if (file.Exists(nomCN100slf)==T){unlink(nomCN100slf)}
      nomCN100cli=paste0(dsnlayerC,"\\",nom_Telemac,"_CN100.cli")
      if (file.Exists(nomCN100cli)==T){unlink(nomCN100cli)}
      
      setwd(chemin_pputils)
      cmd=paste0(Bug_python," append_adcirc_cerema.py -b ", paste0(dsnlayerC,"\\",nom_maillage,"_",nom_MNT,".grd"),
                 " -f ",
                 paste0(dsnlayerC,"\\",nom_maillage,"_friction.grd"),
                 " -c ",
                 paste0(dsnlayerC,"\\",nom_maillage,"_CN100.grd"),
                 " -p single -o ",
                 paste0(dsnlayerC,"\\",nom_Telemac,"_CN100.slf"))
      print(cmd);system(cmd)
      if (file.exists(paste0(dsnlayerC,"\\",nom_Telemac,"_CN100.slf"))==F){cat(paste0(dsnlayerC,"\\",nom_Telemac,"_CN100.slf")," PPUTILS ne fonctionne pas");browser()}
      if (file.exists(paste0(dsnlayerC,"\\",nom_Telemac,"_CN100.cli"))==F){cat(paste0(dsnlayerC,"\\",nom_Telemac,"_CN100.cli")," PPUTILS ne fonctionne pas");browser()}
      
    }
  }
  # 5.11.1 TELEMAC Ajout de la bathy et du frottement et d'un CN "classique"
  if (file.exists(paste0(dsnlayerC,"\\",nom_Telemac,".slf"))==T){unlink(paste0(dsnlayerC,"\\",nom_Telemac,".slf"))}
  if (file.exists(paste0(dsnlayerC,"\\",nom_Telemac,"_pputil.cli"))==T){unlink(paste0(dsnlayerC,"\\",nom_Telemac,"_pputil.cli"))}
  if (file.exists(paste0(dsnlayerC,"\\",nom_Telemac,".cli"))==T){unlink(paste0(dsnlayerC,"\\",nom_Telemac,".cli"))}
  
  setwd(chemin_pputils)
  cmd=paste0(Bug_python," append_adcirc_cerema.py -b ", paste0(dsnlayerC,"\\",nom_maillage,"_",nom_MNT,".grd"),
             " -f ",
             paste0(dsnlayerC,"\\",nom_maillage,"_friction.grd"),
             " -c ",
             paste0(dsnlayerC,"\\",nom_maillage,"_CN.grd"),
             " -p single -o ",
             paste0(dsnlayerC,"\\",nom_Telemac,".slf"))
  print(cmd);system(cmd)
  if (file.exists(paste0(dsnlayerC,"\\",nom_Telemac,".slf"))==F){cat(paste0(dsnlayerC,"\\",nom_Telemac,".slf")," PPUTILS ne fonctionne pas");browser()}
  if (file.exists(paste0(dsnlayerC,"\\",nom_Telemac,".cli"))==F){cat(paste0(dsnlayerC,"\\",nom_Telemac,".cli")," PPUTILS ne fonctionne pas");browser()}
  
  # Modification conditions limites 
  nomcli=paste0(dsnlayerC,"\\",nom_Telemac,".cli")
  file.copy(nomcli,paste0(dsnlayerC,"\\",nom_Telemac,"_pputil.cli"),overwrite = T)
  
  # Gestion des ouvrages
  if (file.exists(file.path(dsnlayerC,"OuvHydrau_Select.shp"))==TRUE)
  {
    OuvHydrau=st_read(dsn = dsnlayerC,
                      layer = "OuvHydrau_Select")
    
    # Récupération des entrées et sorties
    Res_Pts=do.call(rbind,lapply(1:dim(OuvHydrau)[1],function(x) {cbind(st_coordinates(OuvHydrau[x,])[c(1,dim(st_coordinates(OuvHydrau[x,]))[1]),1:2],x,rbind(1,2))}))
    
    # Recuperation des indices des points entree/sortie du reseau
    
    for (ib in 1 : dim(Res_Pts)[1])
    {
      # Lecture du fichier si pas ouvert avant
      # if (file.exists(paste0(dsnlayerC,"/","CN",".csv"))==T & file.exists(paste0(dsnlayerC,"/","Friction",".csv"))==T)
      # {
      #Lecture des donnees MNT si CN et Strickler non BDD
      num=as.numeric(scan(file.path(dsnlayerC,paste0(nom_maillage,"_",nom_MNT,".grd")),skip=1,nlines=1))
      tabMNT=data.frame(t(matrix(scan(file.path(dsnlayerC,paste0(nom_maillage,"_",nom_MNT,".grd")),sep=" ",skip=2,nlines=num[2]),4,num[2])))
      # }
      diff=(tabMNT[,2]-Res_Pts[ib,1])^2+(tabMNT[,3]-Res_Pts[ib,2])^2
      
      num=which(diff==min(diff))
      if (length(num)>1)
      {
        print("BUG: Points confondus dans le maillage")
      }
      # champ=paste0("I",Res_Pts[ib,4])
      OuvHydrau[Res_Pts[ib,3],paste0("I",Res_Pts[ib,4])]=num
    }
    
    st_write(OuvHydrau,dsn=file.path(dsnlayerC,"OuvHydrau_Select.shp"), delete_layer=T, quiet=T)
    
    # Export du fichier bridge
    zzOH=paste0(file.path(dsnlayerC,"OuvHydrau_Select.txt"))
    unlink(zzOH)
    file.create(zzOH)
    write("Relaxation, Number of culverts", file = zzOH, append=T)
    write(cbind(0.2,dim(OuvHydrau)[1]), file = zzOH, append=T)
    toto="I1 I2 CE1 CE2 CS1 CS2 LRG HAUT1 CLP LBUS Z1 Z2 CV C56 CV5 C5 CT HAUT2 FRIC LENGTH CIRC D1 D2 A1 A2 AA"
    write(toto, file = zzOH, append=T)
    toto=paste(OuvHydrau$I1,OuvHydrau$I2,
               OuvHydrau$CE1,OuvHydrau$CE2,OuvHydrau$CS1,OuvHydrau$CS2,OuvHydrau$LRG,OuvHydrau$HAUT1,
               OuvHydrau$CLP,OuvHydrau$LBUS,OuvHydrau$Z1,OuvHydrau$Z2,OuvHydrau$CV,OuvHydrau$C56,
               OuvHydrau$CV5,OuvHydrau$C5,OuvHydrau$CT,OuvHydrau$HAUT2,OuvHydrau$FRIC,round(OuvHydrau$LENGTH,2),
               OuvHydrau$CIRC,OuvHydrau$D1,OuvHydrau$D2,OuvHydrau$A1,OuvHydrau$A2,OuvHydrau$AA)
    write(toto, file = zzOH, append=T)
  }
  
  if (is.na(as.numeric(contour$Exzeco))==T & file.exists(paste0(dsnlayerC,"\\",nom_Telemac,".slf"))==T)
  {
    Etape2_Check_Mesh(paste0(dsnlayerC,"\\",nom_Telemac,".slf"),qgis_process)
  }
}



Etape2_fn <- function(){
  tryCatch({      if (ETAPE[2] == 1)
  {
    cat("\014")
    cat("ETAPE 2 - Traitement de: ",contour$NOM,"\n")
    # cat("\014")
    # Fichier utilisé à cette étape
    
    if (file.exists(file.path(dsnlayerC, nom_nodes)))
    {
      # Travail PPutils autour de GMSH
      Etape2_PPutils_GMSH()
    } else{
      # Maillage directement sur la grille
      Etape2_Maillage_Grille()
    }
    
    # Préparation du CN, Friction et conversion MSH2 en SELAFIN
    Etape2_Fr_CN_SLF()
  }}, error = function(e) { skip_to_next <<- TRUE})
  return(skip_to_next)
  
}

Etape2_fn_parallel <- function(){
  if (ETAPE[2] == 1)
  {
    cat("\014")
    cat("ETAPE 2 - Traitement de: ",contour$NOM,"\n")
    # cat("\014")
    # Fichier utilisé à cette étape
    
    if (file.exists(file.path(dsnlayerC, nom_nodes)))
    {
      # Travail PPutils autour de GMSH
      Etape2_PPutils_GMSH()
    } else{
      # Maillage directement sur la grille
      Etape2_Maillage_Grille()
    }
    
    # Préparation du CN, Friction et conversion MSH2 en SELAFIN
    Etape2_Fr_CN_SLF()
  }
  
}

Etape2_Check_Mesh=function(nomMESH,qgis_process)
{
  nom_poly_mesh=paste0(substr(nomMESH,1,nchar(nomMESH)-4),"_poly_mesh.gpkg")
  
  ngroup=0
  DATE_inutile="1900-01-01T00:00:00Z"
  
  cmd <- paste0(qgis_process, " run native:exportmeshfaces",
                " --INPUT=",shQuote(paste0("SELAFIN:",shQuote(nomMESH))),
                " --DATASET_GROUPS=",ngroup,
                " --DATASET_TIME=",shQuote(DATE_inutile),
                " --EPSG=",EPSG,
                " VECTOR_OPTION=0",
                " --OUTPUT=",shQuote(nom_poly_mesh))
  print(cmd);toto=system(cmd)
  print(toto)
  
  Poly_Triangle=st_read(nom_poly_mesh)
  
  Poly_Triangle$AIRE=st_area(Poly_Triangle)
  
  units(Poly_Triangle$AIRE)=NULL
  
  nb=which(Poly_Triangle$AIRE<1)
  
  if (length(nb)>0)
  {
    nom_export=paste0(substr(nom_poly_mesh,1,nchar(nom_poly_mesh)-5),"_BUG_PETIT_ELEMENT",0.01*round(100*min(Poly_Triangle$AIRE)),"m2.gpkg")
    st_write(Poly_Triangle[nb,],nom_export, delete_layer = T, quiet = T)
  }
  unlink(nom_poly_mesh)
}

