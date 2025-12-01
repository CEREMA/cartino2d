



Etape11_fn <- function(){
  tryCatch({          if (ETAPE[11] == 1)
  {
    ##### vérification cas res pour savoir où on en est
    listeCAS = list.files(dsnlayerC, pattern = ".cas")
    for (icas in 1:(length(listeCAS) - 1))
    {
      extcas = substr(listeCAS[icas], (nchar(listeCAS[icas]) - 3), nchar(listeCAS[icas]))
      if (is.na(match(extcas, '.cas')) == FALSE)
      {
        listeRES = list.files(dsnlayerC, pattern = paste0(substr(listeCAS[icas], 1, (
          nchar(listeCAS[icas]) - 4
        )), '.res'))
        listeASC1 = list.files(dsnlayerC,
                               pattern = paste0(
                                 substr(listeCAS[icas], 1, (nchar(listeCAS[icas]) - 4)),
                                 "_",
                                 extsortieASC[1],
                                 "_Brut.asc"
                               ))
        
        listeASC2 = list.files(dsnlayerC,
                               pattern = paste0(substr(listeCAS[icas], 1, (
                                 nchar(listeCAS[icas]) - 4
                               )), "_", extsortieASC[1], ".asc"))
        ASC_Final = listeASC2[1]
        for (isort in 2:length(extsortieASC))
        {
          listeASC2 = list.files(dsnlayerC,
                                 pattern = paste0(substr(listeCAS[icas], 1, (
                                   nchar(listeCAS[icas]) - 4
                                 )), "_", extsortieASC[isort], ".asc"))
          ASC_Final = paste(ASC_Final, listeASC2[1], sep = ";")
        }
        write(
          paste(listeCAS[icas], listeRES[1], listeASC1[1], ASC_Final, sep = ";"),
          file = Avanc,
          append = T
        )
      }
    }
  }}, error = function(e) {skip_to_next <<- TRUE})
  
  if(skip_to_next){
    contours$BUG[nsecteur[ic]] <- "problème étape 11"
    contours$STEP_PRE[nsecteur[ic]] <- -2
  }
  
  
}
