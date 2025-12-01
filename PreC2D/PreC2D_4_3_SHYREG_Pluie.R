# library(raster)
# library(sf)
# library(ggplot2)
# library(scales)
# library(tidyr)
# library(plotly)
cat("\014")
#--------------------------------------------------------------------------------#
#------------------------Chargement des données SHYREG---------------------------#
#--------------------------------------------------------------------------------#
# Chargement du fichier .RData contenant les données Shyreg
# load(ras_min)
# load(ras_h)
# dsnPluie_="C:\\Cartino2D\\France\\_Pluie"
# nomDpt="C:/AFFAIRES/FILINO_Travail/00_SIGBase/DEPARTEMENT.shp"
# nomDpt="H:\\FILINO_Travail\\00_SIGBase\\DEPARTEMENT.shp"
# nduree=cbind(15,30,60,2*60,3*60,4*60,6*60,12*60,24*60,48*60,72*60)
# duree <- 12  # Durée totale en heures
# nomdpt=cbind("11")#,"13","30","34","66","74","76")
# # time_step_sim <- 180  # Pas de temps en secondes
# tpic <- 8 # Heure du pic de pluie
# Periode_retour <- c(100) # Période de retour ==> 1000, 500, 100, 50, 20, 10, 5, 2
# ras_min <- "H:/Pluie/SHYREG/SHYREGPluieBrute/ras_P15-60mm_SHYREG_2018_France_L93_deterministe.RData" # Fichier .RData contenant les données Shyreg infra-horaire (15 à 60 min)
# ras_h <- "H:/Pluie/SHYREG/SHYREGPluieBrute/ras_Ph_SHYREG_2018_France_L93_deterministe.Rdata" # Fichier .RData contenant les données Shyreg horaires (1 à 72 h)
# ras_h=""
# ras_Pn_DIR="H:/Pluie/SHYREG/SHYREGPluieNette"
# FournitureINRAEDptouSecteur=0


# dsnlayer="C:/Cartino2D/France"
# nomlayerC="SecteurCNIR_AUDE_Q.gpkg"
# nomlayerC="SecteurCNIR_AUDE_Q_Patrick.gpkg"
# nomlayerC="Cartino2D_Secteur_NASSIM_LEZ_MOSSON_BV_ZPP.gpkg"
# nomlayerC="Cartino2D_Secteur_20240710_MTP_GMSH_PN.gpkg"
# nomlayerC="Cartino2D_Secteur_CNIR.gpkg"
# EPSG=2154



# # contours d'étude
# contours <- st_read(file.path(dsnlayer, nomlayerC))
# 
# # Choix des secteurs à traiter
# if (dim(contours)[1]<150)
# {
#   # par choix en boite de dialogue
#   nchoixS = select.list(contours$NOM,preselect = contours$NOM[which(contours$STEP_PRE==0)],
#                         title = "Choisir les secteurs à traiter ( boite si moins de 150 pré-choix",multiple = T,graphics = T)
#   nlalaS = which(contours$NOM %in% nchoixS)
#   if (length(nlalaS)==0){VOUSAVEZPASCHOISI=BADABOOM}
#   # On focalise sur le champ ATRAITER==1
#   contours=contours[nlalaS,]
#   contours$STEP_PRE=0
# }
# 
# nici=which(contours$STEP_PRE==0)
# contours=contours[nici,]

dsnPluie_=dsnPluie

print(paste0(contours$NOMPOST," - ",contours$NOM))
cat("Vérifier STEP_PRE =0 pour les secteurs souhaités\n")
cat("ATTENTION on ne peut pas avoir de pluie brute et nette sur un même secteur\n")
cat("CN spatialisé ou inférieur à 100       ou    CN=100\n")
cat("Prendre un pic plutôt au MILIEU en pluie BRUTE\n")
cat("Prendre un pic plutôt au 2/3    en pluie NETTE\n")

listSect=cbind("Pluie Brute","Pluie Nette","BOOM")
nchoixPBN = select.list(
  listSect,
  title = "ATTENTION à STEP_PRE pour le secteur souhaité",
  multiple = F,
  graphics = T
)
nPBN = which(listSect %in% nchoixPBN)

if (length(which(nPBN==1))){ras_Pn_DIR=""}
if (length(which(nPBN==2))){ras_h="" }# si vide, pluie nette sinon pluie brute
if (length(which(nPBN==3))){BOOM=VOUSVOULIEZARRETER}


# Récupération de données SHYREG pour la période de retour souhaitée
for (ind in 1:length(Periode_retour))
{
  PR = formatC(x = Periode_retour[ind], digits = 3, flag = "0", format = "d")
  cat("\nPériode de retour: ",PR,"\n")
  for (typPluie in 1:2)
  {
    do_it=0
    if (typPluie==1 & nchar(ras_min)>0 & nchar(ras_h)>0)
    {
      #################################################
      # ------------ CAS PLUIE BRUTE ------------------
      #################################################
      SHYraci="shyreg_spPB_"
      SHYraci_="Shyreg Spatialisé Pluie Brute "
      cat("\n## ---- Cas Pluie Brute -----\n")
      ras_P_SHYREG_BMin=get(load(ras_min))
      ras_P_SHYREG_BHeu=get(load(ras_h))
      
      # nomcolBMin=cbind(paste0("PM15.",PR),paste0("PM30.",PR),paste0("PM60.",PR))
      nomcolBMin=paste0("PM",seq(5,120,5),".",PR)
      nomcolBMin=intersect(nomcolBMin,names(ras_P_SHYREG_BMin))
      ras1=ras_P_SHYREG_BMin[[nomcolBMin]]
      
      nomcolm=sapply(strsplit(nomcolBMin,"\\."), function(x) {x[[1]][1]})
      nomcolm=substr(nomcolm,3,nchar(nomcolm))
      names(ras1)=paste0("LM",nomcolm,".",PR)
      
      # nomcolBHeu=cbind(paste0("PM02.",PR),paste0("PM03.",PR), paste0("PM04.",PR), paste0("PM06.",PR), paste0("PM12.",PR), paste0("PM24.",PR), paste0("PM48.",PR), paste0("PM72.",PR))
      PasHShyreg=c(1,2,3,4,6,12,24,48,72)
      PasHShyreg=PasHShyreg[which(PasHShyreg>max(as.numeric(nomcolm))/60)]
      PasHShyreg=formatC(PasHShyreg, digits = 1, flag = "0", format = "d")
      nomcolBHeu=paste0("PM",PasHShyreg,".",PR)
      nomcolBHeu=paste0("PM",PasHShyreg,".",PR)
      nomcolBHeu=intersect(nomcolBHeu,names(ras_P_SHYREG_BHeu))
      ras2=ras_P_SHYREG_BHeu[[nomcolBHeu]]
      
      
      nomcol=sapply(strsplit(nomcolBHeu,"\\."), function(x) {x[[1]][1]})
      nomcol=substr(nomcol,3,nchar(nomcol))
      names(ras2)=paste0("LM",as.numeric(nomcol)*60,"_",PR)
      
      ras_P_SHYREG=brick(stack(ras1,ras2))
      
      # names(ras_P_SHYREG)=cbind( "LM0.25_500","LM0.5_500","LM1_500","LM2_500","LM3_500","LM4_500","LM6_500","LM12_500","LM24_500","LM48_500","LM72_500")
      
      do_it=1
    }
    if (typPluie==2 & nchar(ras_Pn_DIR)>0)
    {
      Plantage=ilfautmettrelescontoursshyregPN
      
      # Contour des Départements
      Departement=st_read(nomDpt)
      # Intersection des départements
      nb=st_intersects(Departement,contours)
      n_int = which(sapply(nb, length)>0)
      Dpt=Departement[n_int,]
      
      #################################################
      # ------------ CAS PLUIE NETTE ------------------
      #################################################
      SecteurPNINRAE=get(load("ContourPN_INRAE"))
      
      SHYraci="shyreg_spPN_"
      SHYraci_="Shyreg Spatialisé Pluie Nette "
      cat("\n## ---- Cas Pluie Nette -----\n")
      if (FournitureINRAEDptouSecteur==1)
      {
        # Boucle sur les départements qui intersectent la donnée
        LSHYREG=list()
        nbdpt=1
        for (iDpt in Dpt$INSEE_DEP)
        {
          cat(iDpt)
          ras_Pn=file.path(ras_Pn_DIR,paste0("HSMF_pixel_T",Periode_retour[ind],"_",iDpt,".Rdata"))
          if (file.exists(ras_Pn)==T)
          {
            cat(" dispo")
            ras_P_SHYREG_=get(load(ras_Pn))
            if (nbdpt==1)
            {
              nomcol=names(ras_P_SHYREG_)
              ras_P_SHYREG=ras_P_SHYREG_
            }else{
              ras_P_SHYREG=merge(ras_P_SHYREG,ras_P_SHYREG_)
              names(ras_P_SHYREG)=nomcol
            }
            nbdpt=nbdpt+1
          }else{
            cat(" non dispo")
          }
          cat("\n")
        }
        do_it=1
      }else{
        ras_Pn=file.path(ras_Pn_DIR,paste0("HSMF_pixel_T",Periode_retour[ind],"_",SectPN_INRAE,".Rdata"))
        if (file.exists(ras_Pn)==T)
        {
          cat(" dispo")
          ras_P_SHYREG=get(load(ras_Pn))
          do_it=1
        }
      }
    }
    if (do_it==1)
    {
      cat("\nBoucle sur les secteurs\n")
      pb <- txtProgressBar(min = 0, max = nrow(contours), style = 3)
      for (atraiter in 1:nrow(contours))
      {
        setTxtProgressBar(pb, atraiter)
        # contoursatraiter = contours[atraiter,]
        
        bassin_ <- contours[atraiter,] %>%
          st_simplify(preserveTopology = FALSE,
                      dTolerance = 1) %>%
          st_segmentize(1, crs = crs)
        
        contoursatraiter <-  st_sf(geometry = st_buffer(st_geometry(bassin_),1000),crs=EPSG)
        # contoursatraiter <-  st_buffer(st_geometry(bassin_),1000)
        
        
        ras_P_SHYREG_mask=mask(crop(ras_P_SHYREG,contoursatraiter),contoursatraiter) #st_buffer(contours,1000)
        # transformation des rasters découpés en points 
        ras_pts <- rasterToPoints(ras_P_SHYREG_mask)
        
        ras_pts_=as.data.frame(ras_pts[,3:ncol(ras_pts)])
        INRAE=colnames(ras_pts_)
        # nduree=as.numeric(sapply(1:length(INRAE),function(x) {strsplit(INRAE, split="LM|_")[[x]][2]}))
        nduree=1/60*as.numeric(sapply(1:length(INRAE),function(x) {strsplit(INRAE, split="LM|_")[[x]][2]}))
        
        ras_pts_=ras_pts_[,which(nduree<=duree)]
        nduree=nduree[which(nduree<=duree)]
        
        ras_pts__=cbind(ras_pts_[,1],
                        sapply(2:ncol(ras_pts_), function(x) {ras_pts_[,x]-ras_pts_[,x-1]}))
        
        npast=length(nduree)
        
        hyeto=sapply(1:npast, function(x) {ras_pts__[,x]/nduree[x]})
        
        courbe=cbind(hyeto[,ncol(hyeto)]*tpic/max(duree),
                     hyeto[,ncol(hyeto):1]*(tpic/max(duree)),
                     hyeto*(1-tpic/max(duree)))
        
        # cumul_par_temps=cbind(ras_pts__[,ncol(ras_pts__)]*tpic/max(duree),
        #                       ras_pts__[,ncol(ras_pts__):1]*(tpic/max(duree)),
        #                       ras_pts__*(1-tpic/max(duree)))
        cumul_par_temps=cbind(ras_pts__[,ncol(ras_pts__):1]*(tpic/max(duree)),
                              ras_pts__*(1-tpic/max(duree)))
        
        cumul_par_temps_exp=cbind(cumul_par_temps[,1],cumul_par_temps,0)
        
        Temps=matrix(0,1,1+2*npast)
        
        Temps[1:npast]=tpic-nduree[npast:1]*tpic/max(duree)
        Temps[1+npast]=tpic
        Temps[(2+npast):(1+2*npast)]=tpic+nduree*(1-tpic/max(duree))
        Temps=cbind(Temps,9999999/3600)
        
        # output_file=file.path(dsnlayer,"pluiespat.txt")
        
        
        raci_exp=paste0(SHYraci, bassin_$NOM, 
                        "_T", PR, 
                        "_D", formatC(x = duree, digits = 1, flag = "0", format = "d"), 
                        "_PIC", formatC(x = tpic, digits = 1, flag = "0", format = "d"))
        
        raci_exp_=paste0(SHYraci_, bassin_$NOM, 
                         "_T", PR, 
                         "_D", formatC(x = duree, digits = 1, flag = "0", format = "d"), 
                         "_PIC", formatC(x = tpic, digits = 1, flag = "0", format = "d"))
        
        output_file <- file.path(dsnPluie_, bassin_$NOMPOST, bassin_$NOM,
                                 paste0(raci_exp, ".txt"))
        
        if (file.exists(dirname(output_file))==F){dir.create(dirname(output_file),recursive = T)}
        
        write("#Pluie SHYREG spatialisee", output_file)
        write(paste0(nrow(ras_pts),",",length(Temps)), output_file, append = TRUE)
        write.table(ras_pts[,1:2], output_file, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)
        write.table(t(rbind(Temps*3600,cbind(cumul_par_temps[,1],cumul_par_temps,0))), output_file, sep = ", ", row.names = FALSE, col.names = FALSE, append = TRUE)
        
        #Plot
        fun_stat=c("Maximum","Moyenne","Minimum")
        width = as.numeric(Temps[2:(length(Temps)-1)]-Temps[1:(length(Temps)-2)])
        x     = as.numeric(Temps[2:(length(Temps)-1)])
        y     = as.numeric(apply(cumul_par_temps, 2, "max"))/width
        intensity_graph <- data.frame(x,y,width,stat_name="Maximum")
        y     = as.numeric(apply(cumul_par_temps, 2, "mean"))/width
        intensity_graph <- rbind(intensity_graph,data.frame(x,y,width,stat_name="Moyenne"))
        y     = as.numeric(apply(cumul_par_temps, 2, "min"))/width
        intensity_graph <- rbind(intensity_graph,data.frame(x,y,width,stat_name="Minimum"))
        intensity_graph$stat_name <- factor(intensity_graph$stat_name, levels =fun_stat )
        
        # Calculer les positions des barres
        intensity_graph$xmin <- intensity_graph$x - intensity_graph$width
        intensity_graph$xmax <- intensity_graph$x
        
        # Créer le graphique
        g1 <- ggplot(intensity_graph, aes(xmin = xmin, xmax = xmax, ymin = 0, ymax = y, fill = stat_name)) +
          geom_rect() +
          theme_minimal() +
          scale_x_continuous(limits = c(0, duree*1), name = "Temps (h)", breaks = seq(0, 12, by = 2)) +
          ylab("Intensité (mm/h)") +
          ggtitle(paste0("Hyétogramme SHYREG mono-fréquence de période de retour ", 100, " ans")) +
          scale_fill_manual(values = c("Maximum" = "red", "Moyenne" = "blue", "Minimum" = "green"))
        
        #### Volume
        cumul_global=cbind(cumul_par_temps[,1],
                           sapply(2:dim(cumul_par_temps)[2], function(x) {rowSums(cumul_par_temps[,1:x])}))
        
        width = as.numeric(Temps[2:(length(Temps)-1)]-Temps[1:(length(Temps)-2)])
        x     = c(0,as.numeric(Temps[2:(length(Temps)-1)]))
        y_     = c(0,as.numeric(apply(cumul_global, 2, "max")))
        cumul_graph <- data.frame(x,y_,stat_name="Maximum")
        y_     = c(0,as.numeric(apply(cumul_global, 2, "mean")))
        cumul_graph <- rbind(cumul_graph,data.frame(x,y_,stat_name="Moyenne"))
        y_     = c(0,as.numeric(apply(cumul_global, 2, "min")))
        cumul_graph <- rbind(cumul_graph,data.frame(x,y_,stat_name="Minimum"))
        cumul_graph$stat_name <- factor(cumul_graph$stat_name, levels =fun_stat )
        
        # pourquoi je n'ai pas les couleurs quand je demande cela, le reste est bon
        
        g2=ggplot(data =cumul_graph,aes(x = x, y=y_,color = stat_name))+
          geom_line()+
          theme_minimal() +
          scale_x_continuous(limits = c(0, duree*1), name = "Temps (h)", breaks = seq(0, 12, by = 2)) +
          ylab("Cumul (mm)") +
          ggtitle(paste0("Cumul SHYREG mono-fréquence de période de retour ", 100, " ans")) +
          scale_color_manual(values = c("Maximum" = "red", "Moyenne" = "blue", "Minimum" = "green"))
        
        fig1 <- plotly::ggplotly(g1,dynamicTicks = T) 
        fig2 <- plotly::ggplotly(g2,dynamicTicks = T)
        fig <- plotly::subplot(fig2, fig1, nrows = 2,shareX=TRUE,heights = c(1/4,3/4), titleY = TRUE, titleX = TRUE) %>%
          plotly::layout(title = list(text = paste0("Hyétogramme ",raci_exp_)),legend=list(title=list(text='')),
                         plot_bgcolor='#e5ecf6',
                         xaxis = list(
                           zerolinecolor = '#ffff',
                           zerolinewidth = 2,
                           gridcolor = 'ffff'       #☻ type = 'date'
                           # tickformat = "%d %B (%a)<br>%Y"
                         ),
                         yaxis = list(
                           zerolinecolor = '#ffff',
                           zerolinewidth = 2,
                           gridcolor = 'ffff'))
        
        dir.create((dirname(file.path(dsnPluie_, bassin_$NOMPOST, bassin_$NOM,
                                      paste0(raci_exp, ".html")))))
        filepath_plotly_all <- file.path(dsnPluie_, bassin_$NOMPOST, bassin_$NOM,
                                         paste0(raci_exp, ".html"))
        
        htmlwidgets::saveWidget(plotly::partial_bundle(fig),
                                file = filepath_plotly_all,
                                libdir = "lib")
      }
    }
  }
}

cat("\n")
cat("\n")
cat("\n")
cat("\n")
cat("###################################################################################\n")
cat("######################### C2D A LIRE SVP ###########################################\n")
cat("------------------------- Etape PreC2D 4_3-------------------------------------------\n")
cat("Vous avez fini cette étape.\n")
cat("Vos traitements sont dans de nombreux dossiers\n")
cat("Par exemple, le dernier se trouve dans le dossier:\n")
cat(file.path(dsnPluie_, bassin_$NOMPOST, bassin_$NOM,paste0(raci_exp, ".html")),"\n")
cat("######################### Fin C2D A LIRE ###########################################\n")

# 
# motcleSHYS=paste0("^", "shyreg_spPB_", ".*\\.txt$","|","^", "shyreg_spPN_", ".*\\.txt$")
# # listeSHYS=data.frame(list.files(dirname(filepath_plotly_all), pattern = paste0("^", motcleSHYS, ".*\\.txt$")))
# listeSHYS=data.frame(list.files(dirname(filepath_plotly_all), pattern = motcleSHYS))

# ras_P_SHYREG=get(load(ras_h))
# for (icol in names(ras_P_SHYREG))
# {
#   nom_Rast=file.path(dirname(ras_h),paste0(icol,"_cerema.tif"))
#   writeRaster(ras_P_SHYREG[[icol]], nom_Rast, format = "Gtiff", overwrite = TRUE)
# }
# for (idpt in nomdpt)
# {
#     ras_P_SHYREG=get(load(ras_Pn))
# for (icol in names(ras_P_SHYREG))
# {
#   nom_Rast=file.path(dirname(ras_Pn),paste0("Dpt",idpt,"_",icol,"_cerema.tif"))
#   writeRaster(ras_P_SHYREG[[icol]], nom_Rast, format = "Gtiff", overwrite = TRUE)
# }

# Récupération de données SHYREG pour la période de retour souhaitée
# for (ind in 1:length(Periode_retour))
# {
#   
#   PR = formatC(x = Periode_retour[ind], digits = 3, flag = "0", format = "d")