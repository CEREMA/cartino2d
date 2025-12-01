# if (nb_proc_preC2D<=1)
# {
source(file.path(chem_routine,"PreC2D","PreC2D_2_1_Zone_DPE_job.R"))
for (iraci in 1:length(listraci))
  # for (raci in substr(listraci,1,nchar(listraci)-10)[11])
{
  raci=substr(listraci[iraci],1,nchar(listraci[iraci])-10)
  nomendo=paste0(raci,"_Endo.gpkg")
  
  if (file.exists(file.path(dsnDPE,nomendo))==F)
  {
    # nomendomanuel=PreC2D_1_1_Zone_DPE_job(SecteurGRASS_,iraci,raci,Extens,dsnDPE,resolution)
    PreC2D_1_1_Zone_DPE_job(SecteurGRASS_,iraci,raci,Extens,dsnDPE,resolution)
  }else{
    cat(nomendo, "existe déjà")
  }
}
# }else{
#   nb_proc=nb_proc_preC2D
#   cat("------ ",nb_proc            ," CALCULS MODE PARALLELE -------------\n")
#   require(foreach)
#   cl <- parallel::makeCluster(nb_proc)
#   registerDoParallel(cl)
#   foreach(iraci = 1:length(listraci),
#           .inorder = FALSE,
#           .packages = c("sf","dplyr")
#   ) %dopar%
#     {
#       PreC2D_1_1_Zone_DPE_job(SecteurGRASS_,iraci,raci,Extens,dsnDPE,resolution)
#     }
#   stopCluster(cl)
# }





cat("\n")
cat("\n")
cat("\n")
cat("\n")
cat("###################################################################################\n")
cat("######################### C2D A LIRE SVP ###########################################\n")
cat("----------- Etape 2_1-------------------------------------------------------------\n")
cat("Vous venez de créer de très nombreux fichiers pour les zones Crue, Flash et Endoréiques\n")
cat("La 1ère fois où vous faites cette étape, il est très fortement conseillé d'ouvrir le fichier qgis cité ci-dessous\n")
if (file.exists(file.path(dsnDPE,paste0(raci,Extens[1,3],Extens[2,3],".gpkg")))==T)
{
  cat("Il vous faut faire un travail manuel avec OHFlash par exemple pour garder ou pas les endoréismes sur le fichier:\n")
  # cat(nomendomanuel,"\n")
  cat("Modifier les attributs du champ Verif et mettre 1 pour ceux où vous êtes sûr que c'est un endoréisme.\n")
}
cat("\n")
cat("######################### Fin C2D A LIRE ###########################################\n")