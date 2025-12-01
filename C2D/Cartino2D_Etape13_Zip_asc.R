

Etape13_fn <- function(){
  tryCatch({          if (ETAPE[13] == 1)
  {
    setwd(dsnlayerC)
    
    ListeASC = list.files(dsnlayerC, pattern = ".asc")
    
    for (iasc in 1:length(ListeASC))
    {
      nom = ListeASC[iasc]
      if ((substr(nom, nchar(nom) - 3, nchar(nom)) == ".asc") == TRUE &
          (substr(nom, 1, 12) == "MNTFinal.asc") == FALSE)
      {
        print(nom)
        zip(paste0(nom, '.zip'), nom, flags = "-j")
        unlink(nom)
        
      }
    }
  }}, error = function(e) {skip_to_next <<- TRUE})
  
  if(skip_to_next){
    contours$BUG[nsecteur[ic]] <- "problème étape 13"
    contours$STEP_PRE[nsecteur[ic]] <- -2
  } 
  
  
}
