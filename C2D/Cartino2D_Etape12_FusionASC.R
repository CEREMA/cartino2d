#################################################################################################
################   Etape12_FusionASC
#################################################################################################
Etape12_FusionASC=function(dsnlayer,contour,contours,SecteurGRASS_,BatGRASS,EPSG,OSGeo4W_path,NProcGrass,chem_routine,Doss_Qml)
{  
  cat("\n")
  cat("\n")
  cat("########################################################################################################\n")
  cat("######################### C2D A LIRE SVP ###############################################################\n")
  cat("---------------- Choix des scénarios à fusionner--------------------------------------------------------\n")
  cat("Si vous avez lancé cette étape, vous devez disposer de résultats Raster de hauter;, débits...\n")
  cat("1/ sur un certains nombres de secteurs\n")
  cat("2/ pour plusieurs scénarios de pluie ou de débit\n")
  cat("\n")
  cat("vous allez devoir choisir un mot clé du scénario pour fusionner les scénarios calculés dans chaque secteur\n")
  cat("Les mots clé de la boite de dialogue se gère dans la variable nommée 'Scena' de votre fichier de paramètre\n ")
  cat("######################### Fin C2D A LIRE ###############################################################\n")
  
  # Pas urgent VOIR POUR DEMANDER LA FUSION HORS DE CETTE FONCTION ET LANCER EN AUTO
  Scena=select.list(Scena,preselect=Scena,title = "Choix",multiple=T, graphics = T)
  if (length(Scena)==0){break}

  # faire avec un autre champ de fusion
  nsecteur=which(contours$STEP_PRE==0)
  nompost=unique(contours$NOMPOST)
  for (ipost in 1:length(nompost))
  {
    dsnlayerRes=file.path(dsnlayer, nompost[ipost],"_FUSION")
    if (file.exists(dsnlayerRes)==F){dir.create(dsnlayerRes)}
    
    nsecteur=which(contours$STEP_PRE==0 & contours$NOMPOST==nompost[ipost])
    if (length(nsecteur)>0)
    {
      if(nb_proc_post<=1 | length(Scena)==1)
      {
        for (isc in 1:length(Scena))
        {
          Cartino2D_Etape12_FusionASC_job(dsnlayer,contour,contours,SecteurGRASS_,BatGRASS,EPSG,nsecteur,OSGeo4W_path,NProcGrass,chem_routine,Doss_Qml,Scena,isc,nompost,ipost,dsnlayerRes)
        }
      }else{
        cat("------ ",nb_proc_post," CALCULS MODE PARALLELE -------------\n")
        require(foreach)
        cl <- parallel::makeCluster(min(length(Scena),nb_proc_post))
        registerDoParallel(cl)
        foreach::foreach(isc = 1:length(Scena),
                         .combine = 'c',
                         .inorder = FALSE,
                         .packages = c("sf","dplyr","utils","sfheaders","tidyverse","readxl")) %dopar%{
                           
                           skip_to_next <- FALSE
                           source("C:/R/R-4.4.1/Cerema/C2D/Cartino2D_Etape12_FusionASC_job.R")
                           Cartino2D_Etape12_FusionASC_job(dsnlayer,contour,contours,SecteurGRASS_,BatGRASS,EPSG,nsecteur,OSGeo4W_path,NProcGrass,chem_routine,Doss_Qml,Scena,isc,nompost,ipost,dsnlayerRes)
                         }
        stopCluster(cl)
      }
    }
  }
}
