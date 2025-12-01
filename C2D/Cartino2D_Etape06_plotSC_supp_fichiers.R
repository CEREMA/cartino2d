




Etape6_fn <- function(){
  tryCatch({          if (ETAPE[6] == 1)
  {
    # plot_sc()
    plot_sc_obs_oh()
    file.remove(file.path(file.path(dsnlayerC, "ASC"), list.files(file.path(
      dsnlayerC, "ASC"
    ))))
    
    fichasupp = cbind(
      paste0(nom_MNT, ".csv"),
      paste0(nom_MNT, "bord.xyz"),
      paste0(nom_MNT, 'exterieur.csv'),
      paste0(nom_maillage, ".msh2"),
      paste0(nom_maillage, ".grd"),
      paste0(nom_maillage, "_CN.grd"),
      paste0(nom_maillage, "_friction.grd"),
      "GMSH_node.csv",
      "ASC"
    )
    file.remove(file.path(dsnlayerC, fichasupp))
  }}, error = function(e) {skip_to_next <<- TRUE})
  return(skip_to_next)
  

  
}

Etape6_fn_parallel <- function(){
  if (ETAPE[6] == 1)
  {
    # plot_sc()
    plot_sc_obs_oh()
    # file.remove(file.path(file.path(dsnlayerC, "ASC"), list.files(file.path(
    #   dsnlayerC, "ASC"
    # ))))
    # 
    # fichasupp = cbind(
    #   paste0(nom_MNT, ".csv"),
    #   paste0(nom_MNT, "bord.xyz"),
    #   paste0(nom_MNT, 'exterieur.csv'),
    #   paste0(nom_maillage, ".msh2"),
    #   paste0(nom_maillage, ".grd"),
    #   paste0(nom_maillage, "_CN.grd"),
    #   paste0(nom_maillage, "_friction.grd"),
    #   "GMSH_node.csv",
    #   "ASC"
    # )
    # file.remove(file.path(dsnlayerC, fichasupp))
  }
  
}

