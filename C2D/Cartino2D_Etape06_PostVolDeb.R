#################################################################################################
################  Etape8_PostVolDeb
#################################################################################################
motscles_ajout = function(Lignes, type) {
  # --- MODIFICATION : Sécurisation si le mot clé n'est pas trouvé ---
  lignes_trouvees = grep(Lignes, pattern=type[1])
  if (length(lignes_trouvees) == 0) {
    return(NULL) # On retourne NULL si rien n'est trouvé
  }
  
  Nombre = sort(unique(substr(Lignes[lignes_trouvees], as.numeric(type[2]), as.numeric(type[3]))))
  if (length(Nombre) > 1) { Nombre = Nombre[length(Nombre)] }
  Nombre = as.numeric(Nombre)
  
  # --- MODIFICATION : Sécurisation si le nombre n'est pas valide ---
  if (is.na(Nombre) || length(Nombre) == 0) {
    return(NULL)
  }
  
  MotsClesaj = cbind(paste0(type[1], sprintf("%5d", 1:Nombre), type[6]), type[4], type[5])
  return(MotsClesaj)
}


Etape8_PostVolDeb = function(dsnlayerC, nomcontour) {
  debit = as.data.frame(matrix(NA, 1, 9))
  colnames(debit) = cbind("NOM", "0002", "0005", "0010", "0020", "0050", "0100", "TPM2006", "2010")
  debit[1] = nomcontour
  
  Listsortie = list.files(file.path(dsnlayerC), pattern="s.sortie")
  print(Listsortie)
  if (length(grep(Listsortie, pattern = '.txt')) > 0) { Listsortie = Listsortie[-grep(Listsortie, pattern = '.txt')] }
  print(Listsortie)
  
  if (length(Listsortie) > 0) {
    for (ils in 1:length(Listsortie)) {
      nomsortie = Listsortie[ils]
      print(nomsortie)
      
      substr(nomsortie, 1, nchar(nomsortie)-30)
      
      if (file.exists(file.path(dsnlayerC, substr(nomsortie, 1, nchar(nomsortie)-30))) == T) {
        filenamejpg = file.path(dsnlayerC, paste0(substr(nomsortie, 1, nchar(nomsortie)-7), ".jpg"))
        if (!file.exists(filenamejpg)) {
          Lignes = readLines(con = file.path(dsnlayerC, substr(nomsortie, 1, nchar(nomsortie)-30)))
          
          RecupTps = rbind("TIME STEP ")
          indic = regexpr(RecupTps[1,1], as.character(Lignes))
          Past_Ts = as.numeric(substr(Lignes[which(indic > -1)], 38, nchar(Lignes[which(indic > -1)])))
          Lignes = readLines(con = file.path(dsnlayerC, nomsortie))
          
          type = cbind("FLUX BOUNDARY", 19, 23, 25, 42, "")
          MotsClesFB = motscles_ajout(Lignes, type)
          
          type = cbind("CULVERT        ", 17, 21, 37, 62, "  DIS")
          MotsClesCUd = motscles_ajout(Lignes, type)
          
          type = cbind("CULVERT        ", 17, 21, 37, 62, "  LIMI")
          MotsClesCUl = motscles_ajout(Lignes, type)
          
          # rbind ignorera automatiquement les objets NULL si motscles_ajout n'a rien trouvé
          MotsCles = rbind(cbind("RELATIVE ERROR IN VOLUME AT T", 37, 54),
                           cbind("RUNOFF_SCS_CN : ACCUMULATED RAINFALL", 44, 61),
                           cbind("VOLUME IN THE DOMAIN", 28, 44),
                           cbind("ADDITIONAL VOLUME DUE TO SOURCE TERMS", 44, 61),
                           cbind("RELATIVE ERROR IN VOLUME AT T", 58, 75),
                           MotsClesFB,
                           MotsClesCUd,
                           MotsClesCUl)
          
          PosFB = grep(MotsCles[,1], pattern="FLUX BOUNDARY")
          PosCu = grep(MotsCles[,1], pattern="CULVERT")
          
          # --- MODIFICATION : Prévention d'erreur si aucun Culvert n'est présent ---
          if (length(PosCu) > 0) {
            PosCuL = PosCu[(length(PosCu)/2 + 1):length(PosCu)]
            PosCu = PosCu[1:(length(PosCu)/2)]
          } else {
            PosCuL = integer(0)
            PosCu = integer(0)
          }
          
          for (ic in 1:dim(MotsCles)[1]) {
            indic = regexpr(MotsCles[ic,1], as.character(Lignes))
            correspond = which(indic > -1)
            correspond = correspond[which(is.na(as.numeric(substr(Lignes[correspond], as.numeric(MotsCles[ic,2]), as.numeric(MotsCles[ic,3])))) == F)]
            
            if (ic == 1) {
              niter = length(correspond)
              tab = matrix(0, niter, dim(MotsCles)[1])
              referenceT = correspond
              tab[,1] = matrix(as.numeric(substr(Lignes[correspond], as.numeric(MotsCles[ic,2]), as.numeric(MotsCles[ic,3]))))
            } else {
              if (length(correspond) > 0) {
                bonindice = sapply(1:length(correspond), function(x) {which(referenceT >= correspond[x])[1]})
                tab[bonindice, ic] = matrix(as.numeric(substr(Lignes[correspond], as.numeric(MotsCles[ic,2]), as.numeric(MotsCles[ic,3]))))
              }
            }
          }
          
          colnames(tab) = MotsCles[,1]
          write.table(tab,
                      file = file.path(dsnlayerC, paste0(substr(nomsortie, 1, nchar(nomsortie)-7), "_sortie.txt")),
                      sep = ";")
          
          # Suppression du volume initial (condition limite)
          if (dim(tab)[2] > 2) {
            tab[,3] = tab[,3] - tab[1,3]
            
            TimeStep = tab[1,1]
            CumulDebit = matrix(0, dim(tab)[1], max(10, length(PosFB)))
            nsortie = 0
            
            # --- MODIFICATION : Calcul des débits si FLUX BOUNDARY existe ---
            if (length(PosFB) > 0) {
              for (ip in PosFB) {
                CumulDebit[, ip-min(PosFB)+1] = sapply(1:dim(tab)[1], function(x) {-TimeStep * sum(tab[1:x, ip])})
                tab[is.na(tab[,ip]) == T, ip] = 0
                if (max(abs(tab[,ip])) > 0) {
                  nsortie = nsortie + 1
                }
              }
            }
            
            tab[,4] = tab[,4] / Past_Ts
            
            CumulAddVol = sapply(1:dim(tab)[1], function(x) {TimeStep * sum(tab[1:x, 4])})
            
            # --- MODIFICATION : Création robuste du shapefile même sans CULVERT ---
            if (file.exists(file.path(dsnlayerC, "OuvHydrau_Select.shp")) == TRUE) {
              OuvHydrau = st_read(dsn = dsnlayerC, layer = "OuvHydrau_Select")
              
              if (length(PosCu) > 0) {
                OuvHydrau$MaxQ = sapply(PosCu, function(x) {round(max(tab[,x]), 2)})[1:dim(OuvHydrau)[1]]
                OuvHydrau$LDE = sapply(PosCuL, function(x) {length(which((tab[,x]) > 0))})[1:dim(OuvHydrau)[1]]
              } else {
                OuvHydrau$MaxQ = 0
                OuvHydrau$LDE = 0
              }
              
              st_write(OuvHydrau, dsn = file.path(dsnlayerC, paste0(substr(nomsortie, 1, nchar(nomsortie)-7), "_OH.gpkg")), delete_layer = T, quiet = T)
              file.copy(file.path(dsnlayerC, "MaxQ_OuvHydrau_Select.qml"), file.path(dsnlayerC, paste0(substr(nomsortie, 1, nchar(nomsortie)-7), "_OH.qml")))
            }
            
            couleurs = cbind("green","magenta","brown","orange","yellow","gray","cyan","black")
            couleurs = cbind(couleurs,couleurs,couleurs,couleurs,couleurs,couleurs,couleurs,couleurs,couleurs,couleurs,couleurs,couleurs,couleurs,couleurs,couleurs,couleurs,couleurs,couleurs,couleurs,couleurs,couleurs,couleurs,couleurs,couleurs)
            titSort = paste0("Sortie ", 1:max(1, length(PosFB)))
            
            jpeg(filename = file.path(dsnlayerC, paste0(substr(nomsortie, 1, nchar(nomsortie)-7), ".jpg")),
                 width = 47.25, height = 66.825, units = "cm", quality = 75, res = 150)
            
            x1 = 0.025; x2 = 0.975; y1 = 0.81; y2 = 0.99
            par(fig = c(x1, x2, y1, y2), new = TRUE)
            
            plot(tab[,1], CumulAddVol, main = nomsortie, type = "l", xlab = "Temps Telemac (s)", ylab = "Volume (m3)", col = "black")
            lines(tab[,1], tab[,3], col = "red")
            
            # Trace les lignes uniquement si nsortie > 0
            if (nsortie > 0) {
              for (isor in 1:nsortie) {
                lines(tab[,1], CumulDebit[, isor], col = couleurs[isor])
              }
              somme_debit = rowSums(CumulDebit[, 1:nsortie, drop = FALSE])
              lines(tab[,1], tab[,3] + somme_debit, col = "blue")
              
              legend("topleft",
                     legend = rbind("Injecte", "Domaine", "Domaine + sorties", as.matrix(titSort[1:nsortie])),
                     col = rbind(rbind("black", "red", "blue"), as.matrix(couleurs[1:nsortie])),
                     lty = cbind(1, 1, 1, 1, 1, 1, 1, 1),
                     pch = cbind(1, 1, 1, 1, 1, 1, 1, 1),
                     title = "Volume")
            } else {
              legend("topleft", legend = c("Injecte", "Domaine"), col = c("black", "red"), lty=1)
            }
            
            x1 = 0.025; x2 = 0.975; y1 = 0.61; y2 = 0.79
            par(fig = c(x1, x2, y1, y2), new = TRUE)
            plot(tab[,1], tab[,2], main = "Pluie Injectee", typ = "l", xlab = "Temps Telemac (s)", ylab = "Cumul (m)")
            
            x1 = 0.025; x2 = 0.975; y1 = 0.41; y2 = 0.59
            par(fig = c(x1, x2, y1, y2), new = TRUE)
            plot(tab[,1], tab[,4], main = "Volume net", type = "b", xlab = "Temps Telemac (s)", ylab = "Apport a chaque pas de calcul (m3)")
            
            ltyl = cbind(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1)
            pchl = cbind(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1)
            
            x1 = 0.025; x2 = 0.975; y1 = 0.21; y2 = 0.39
            par(fig = c(x1, x2, y1, y2), new = TRUE)
            
            #### Débits sortie (SÉCURISÉ)
            if (length(PosFB) > 0) {
              decal = min(PosFB) - 1
              plot(tab[,1], -tab[,min(PosFB)], main = "Debit Sorties", ylim = c(0, -min(tab[,PosFB])), typ = "l", col = couleurs[min(PosFB)-decal], xlab = "Temps Telemac (s)", ylab = "Debit (m3/s)")
              nsortie_graph = 0
              for (ip in PosFB) {
                if (max(abs(tab[,ip])) > 0) {
                  lines(tab[,1], -tab[,ip], col = couleurs[ip-decal])
                  if (min(nsortie_graph) == 0) { nsortie_graph = ip-decal } else { nsortie_graph = cbind(nsortie_graph, ip-decal) }
                }
              }
              if (min(nsortie_graph) > 0) {
                legend("topleft",
                       legend = titSort[PosFB[nsortie_graph]-decal],
                       col = couleurs[PosFB[nsortie_graph]-decal],
                       lty = ltyl[PosFB[nsortie_graph]-decal],
                       pch = pchl[PosFB[nsortie_graph]-decal],
                       title = "Debits")
              }
            } else {
              plot(1, type="n", main="Debit Sorties (Aucune Flux Boundary)", xlab="Temps Telemac (s)", ylab="Debit (m3/s)")
            }
            
            x1 = 0.025; x2 = 0.975; y1 = 0.01; y2 = 0.19
            par(fig = c(x1, x2, y1, y2), new = TRUE)
            
            #### Culverts (SÉCURISÉ)
            if (length(PosCu) > 0) {
              decal = min(PosCu) - 1
              nculv = 0
              ylimmax = max(tab[,PosCu][which(is.na(tab[,PosCu]) == F)])
              plot(tab[,1], -tab[,min(PosCu)], main = "Debit Ouvrages", ylim = c(0, ylimmax), typ = "l", col = couleurs[min(PosCu)-decal], xlab = "Temps Telemac (s)", ylab = "Debit (m3/s)")
              titCulv = paste0("Culvert ", 1:length(PosCu))      
              
              for (ip in PosCu) {
                if (max(abs(tab[,ip][which(is.na(tab[,ip]) == F)])) > 0) {
                  lines(tab[,1], tab[,ip], col = couleurs[ip-decal])
                  if (min(nculv) == 0) { nculv = ip-decal } else { nculv = cbind(nculv, ip-decal) }
                }
              }
              
              if (min(nculv) > 0) {
                legend("topleft",
                       legend = titCulv[as.vector(nculv)],
                       col = couleurs[PosCu[as.vector(nculv)]-decal],
                       lty = ltyl[as.vector(nculv)],
                       pch = pchl[as.vector(nculv)],
                       title = "Ouvrages")
              }
            } else {
              plot(1, type="n", main="Debit Ouvrages (Aucun Culvert)", xlab="Temps Telemac (s)", ylab="Debit (m3/s)")
            }
            dev.off()
            
            # --- MODIFICATION : Calcul robuste du débit global ---
            # Remplace la somme manuelle tab[,6]+tab[,7]+tab[,8]+tab[,9]+tab[,10] qui plante si les colonnes n'existent pas
            if (length(PosFB) > 0) {
              somme_flux = rowSums(tab[, PosFB, drop = FALSE], na.rm = TRUE)
              debit_max_val = round(-min(somme_flux), 2)
            } else {
              debit_max_val = 0
            }
            
            # récupération de la somme des débits max
            for (ic in 1:dim(debit)[2]) {
              indic = regexpr(colnames(debit)[ic], nomsortie)
              if (indic > -1) {
                if (is.na(debit[ic]) == TRUE) {
                  debit[ic] = debit_max_val
                } else {
                  if (debit[ic] == debit_max_val) {
                    print(paste0("Même débit, deux fichiers sorties pour le même scénario, nom du dernier: ", nomsortie))
                  } else {
                    print(paste0("BUG, deux fichiers sorties pour le même scénario, nom du dernier: ", nomsortie))
                  }
                }
              }
            }
          }
        }
      }
    }
  }
  print(debit)
  return(debit)
}

Etape8_fn <- function(){
  tryCatch({          
    if (ETAPE[8] == 1) {
      debit_sc[[ic]] = Etape8_PostVolDeb(dsnlayerC, nomcontour)
    }
  }, error = function(e) {skip_to_next <<- TRUE})
  
  if(skip_to_next){
    contours$BUG[nsecteur[ic]] <- "problème étape 8"
    contours$STEP_PRE[nsecteur[ic]] <- -2
  }
}

Etape8_fn_parallel <- function(){
  if (ETAPE[8] == 1) {
    debits = Etape8_PostVolDeb(dsnlayerC, nomcontour)
    return(debits)
  }
}