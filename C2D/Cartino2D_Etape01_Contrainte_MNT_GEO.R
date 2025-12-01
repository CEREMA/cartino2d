#################################################################################################
################   Etape1_Exzeco
#################################################################################################

# Travail sur les contours d'Exzeco pour réaliser les points de contrainte et sur les limites du domaine
Etape1_Contour = function(dsnlayer, contour, crs)
{
  # Buffer du contour, simplification de la géométrie et segmentation
  cat(nomcontour, " ---- Création de la zone de calcul\n")
  
  Resultats=Etape1_GrandContourBordDecoupe(contour,contour$Cont1_Buf,contour$Exzeco)
  contour_segm_fin = Resultats[[1]]
  Contoursanstrou= Resultats[[2]]
  contour_segm= Resultats[[3]]
  
  if(is.na(contour$SourceDeb) | nchar(contour$SourceDeb)==0)
  {
    #### Forcage PLUIE
    
    # Maillage destructuré
    contour_int <- contour_segm %>% 
      st_union() %>% 
      st_buffer(contour$Cont2_Buf) %>%
      sfheaders::sf_remove_holes()
  }else{
    # Ajout 24/11/2023 FP
    nom_SourcesDebits=contour$SourceDeb
    if (file.exists(nom_SourcesDebits)==T)
    {
      SourcesDebits=st_read(file.path(nom_SourcesDebits))
      st_crs(SourcesDebits)=crs
      fusion=st_union(st_geometry(contour),st_union(st_buffer(st_geometry(SourcesDebits),2*contour$Cont1_Dx)))
      fusion=st_cast(fusion,"POLYGON")
      AireF=st_area(fusion)
      fusion=fusion[which(AireF==max(AireF))]
    }else{
      fusion=contour
    }
    # Fin Ajout 24/11/2023 FP
    
    # Forcage DEBIT
    if (is.na(as.numeric(contour$Exzeco))==T)
    {
      contour_int <- fusion %>% 
        st_union() %>% 
        st_buffer(contour$Cont1_Buf+contour$Cont2_Buf) %>% 
        sfheaders::sf_remove_holes()
    }else{
      # Maillage reglé débit
      DistBuff=2*as.numeric(contour$Exzeco)
      contour_int <- fusion %>% 
        st_buffer(contour$Cont1_Buf+contour$Cont2_Buf+DistBuff) %>% 
        st_union() %>% 
        st_buffer(-DistBuff) %>% 
        sfheaders::sf_remove_holes()
    }
  }
  
  st_write(
    contour_int,
    dsn = file.path(dsnlayerC, paste0(nom_STEP2_Zone_Valid_Calcul, ".shp")),
    layer = nom_STEP2_Zone_Valid_Calcul,
    delete_layer = T,
    quiet = T
  )
  file.copy(file.path(chem_routine,"C2D",Doss_Qml,paste0(nom_STEP2_Zone_Valid_Calcul, ".qml")),
            file.path(dsnlayerC,paste0(nom_STEP2_Zone_Valid_Calcul, ".qml")))
  
  
  
  return(list(contour_segm_fin, contour_int,Contoursanstrou))
}

########################################################################################·
Etape1_GrandContourBordDecoupe=function(GrandContour,Cont1Buf,Reso)
{
  
  if (is.na(as.numeric(contour$Exzeco))==T)
  {  
    Contoursanstrou <- GrandContour %>% 
      
      st_union() %>% 
      st_simplify(preserveTopology = FALSE,
                  dTolerance = 0.5 * contour$Cont1_Dx) %>%
      st_segmentize(contour$Cont1_Dx, crs = crs) %>% 
      st_buffer(Cont1Buf)%>% 
      sfheaders::sf_remove_holes() 
  }else{
    
    # Maillage reglé buffer plus moins pour éviter des rapprochements liés à des grandes résolutions
    DistBuff=2*as.numeric(contour$Exzeco)
    Contoursanstrou <- GrandContour %>% 
      st_union() %>% 
      st_simplify(preserveTopology = FALSE,
                  dTolerance = 0.5 * contour$Cont1_Dx) %>%
      st_segmentize(contour$Cont1_Dx, crs = crs) %>%
      st_buffer(Cont1Buf+DistBuff) %>%
      st_union() %>%
      st_buffer(-DistBuff) %>% 
      sfheaders::sf_remove_holes()
  }
  
  
  contour_segm <- Contoursanstrou %>%
    st_simplify(preserveTopology = FALSE,
                dTolerance = 0.5 * contour$Cont1_Dx) %>%
    st_segmentize(contour$Cont1_Dx, crs = crs)
  
  # Travail sur le contour segment pour commencer le MNT à 0.5
  # objetif être complètement raccord avec Lidar IGN
  
  contour_segm_Pts1 <- contour_segm %>%
    sfheaders::sf_remove_holes() %>% 
    st_cast("MULTILINESTRING", do_split = TRUE)  %>%
    st_cast("LINESTRING", do_split = TRUE) %>%
    st_cast("MULTIPOINT", do_split = TRUE) %>%
    st_cast("POINT", do_split = TRUE)
  
  contour_segm_Pts2 <-
    do.call(rbind, st_geometry(contour_segm_Pts1))
  
  contour_segm_Pts2 <-
    cbind(round(contour_segm_Pts2[, 1]), round(contour_segm_Pts2[, 2]))
  
  contour_segm_fin <- contour_segm_Pts2 %>%
    as.matrix() %>%
    st_multipoint(dim = "XYZ") %>%
    st_cast("MULTILINESTRING", do_split = TRUE) %>%
    st_cast("POLYGON") %>%
    st_sfc() %>%
    st_sf(a = 0)
  
  st_crs(contour_segm_fin) <- st_crs(contour)
  
  # Exportation du contour segmenté
  st_write(
    contour_segm_fin,
    dsn = file.path(dsnlayerC, paste0(nom_STEP1_Contour, ".shp")),
    layer = nom_STEP1_Contour,
    delete_layer = T,
    quiet = T
  )
  
  file.copy(file.path(chem_routine,"C2D",Doss_Qml,paste0(nom_STEP1_Contour, ".qml")),
            file.path(dsnlayerC,paste0(nom_STEP1_Contour, ".qml")))
  
  return(list(contour_segm_fin, Contoursanstrou,contour_segm))
}

#############################################################################
################ EXZECO #####################################################
#############################################################################
Etape1_Exzeco = function(dsnlayer, contour, contour_int,Contoursanstrou,crs)
{ 
  ################ Lecture exzeco par secteur
  # Exz_Pts <- NaN
  if (is.na(contour$Exzeco) == FALSE){
    GOON=0
    if (is.na(as.numeric(contour$Exzeco)) == TRUE & nchar(contour$Exzeco)>8)
    {
      # lecture des données Exzeco
      Exzeco <- st_read(contour$Exzeco)
      GOON=1
    }else{
      if (substr(contour$Exzeco,1,5)=="Courb")
      {
        NomCourb_5="Courb_i5" # FRED pas bien coder
        Exzeco <- st_read(file.path(dsnlayerC,paste0(NomCourb_5,".gpkg")))
        Exzeco$AIRE=st_area(Exzeco)
        SeuilAExz=25
        units(SeuilAExz) = "m2"
        nici=which(Exzeco$AIRE<SeuilAExz)
        if (length(nici)>0){Exzeco=Exzeco[-nici,]}
        st_write(Exzeco,file.path(dsnlayerC,paste0(NomCourb_5,"_nettoye.gpkg")), delete_layer = T, quiet = T)
        GOON=1
      }else{
        # cat("Votre champ contour$Exzeco n'est pas conforme, lien vers Exzeco, entier ou CourbXXX: ",contour$Exzeco,"\n")
        # BUG=Caplante
      }
    }
    if (GOON==1)
    {
      st_crs(Exzeco) <- crs
      st_crs(contour_int) <- crs
      Exzeco <- st_crop(Exzeco,contour_int)
      #-- On ne garde que les Exzeco qui intersectent le bord
      cat(nomcontour, " ---- Intersection d'exzeco sur ce secteur\n")
      # modif 11/01/2022 nb=st_intersects(Exzeco,contour)
      nb <- st_intersects(Exzeco, contour_int)
      n_int <-  which(sapply(nb, length) > 0)
      Exzeco <- Exzeco[n_int,]
      
      Exzeco_Union <-
        st_simplify(st_union(Exzeco),
                    preserveTopology = FALSE,
                    dTolerance = 0)
      # Coupure de Exzeco dans le contour
      Exzeco_Int <- st_intersection(Exzeco_Union, contour_int)
      
      # ###########################################################################
      # # Exzeco buffer exzeco pour nouvelle 1eme contrainte point
      
      nom_exp <- "Step_Cartino2d_2_1_Exz1"
      Result <-
        Buf_et_Coup(
          dsnlayerC,
          nom_exp,
          nom_STEP2_Zone,
          Exzeco_Int,
          Contoursanstrou,
          contour$Exz1_Buf,
          contour$Exz1_Dx,
          contour$Ratio_Dx,
          crs
        )
      Exz1_Pts <- Result$Exz_Pts
      Exz1 <- Result$Exz
      st_crs(Exz1) <- crs
      
      # ###########################################################################
      # # Exzeco buffer exzeco pour nouvelle 2eme contrainte point
      cat(nomcontour, " ---- 2ème contrainte Exzeco\n")
      
      nom_exp <- "Step_Cartino2d_2_2_Exz2"
      Result2 <-
        Buf_et_Coup(
          dsnlayerC,
          nom_exp,
          nom_STEP2_Zone,
          Exzeco_Int,
          Contoursanstrou,#contour,
          contour$Exz2_Buf,
          contour$Exz2_Dx,
          contour$Ratio_Dx,
          crs
        )
      Exz2_Pts <- Result2$Exz_Pts
      Exz2 <- Result2$Exz
      st_crs(Exz2) <- crs
      
      ###########################################################################
      # Exzeco buffer exzeco pour nouvelle 3eme contrainte point
      cat(nomcontour, " ---- 3ème contrainte Exzeco\n")
      nom_exp <- "Step_Cartino2d_2_3_Exz3"
      Result <-
        Buf_et_Coup(
          dsnlayerC,
          nom_exp,
          nom_STEP2_Zone,
          Exzeco_Int,
          Contoursanstrou,#contour,
          contour$Exz3_Buf,
          contour$Exz3_Dx,
          contour$Ratio_Dx,
          crs
        )
      Exz3_Pts <- Result$Exz_Pts
      Exz3 <- Result$Exz
      st_crs(Exz3) <- crs
      
      Exz_Pts <- rbind(Exz1_Pts, Exz2_Pts, Exz3_Pts)
      
      nom_OuvHydrau <- contour$OH
      
      if (file.exists(nom_OuvHydrau) == TRUE)
      {
        # Voir intersection si non vide
        # Lecture du réseau
        OuvHydrau <- st_read(nom_OuvHydrau)
        
        # On récupère ce qui intersecte
        # et ce qui a un code >=0
        nbRC <- st_intersects(OuvHydrau, contour)
        n_int <-
          which(sapply(nbRC, length) > 0 &
                  OuvHydrau$CIRC >= 0 & OuvHydrau$CIRC <= 2)
        
        if (length(n_int > 0) > 0)
        {
          cat(nomcontour," ---- Récupération des ouvrages hydrauliques\n")
          OuvHydrau <- OuvHydrau[n_int,]
          
          # Calcul de la longeur
          OuvHydrau$LENGTH <- round(st_length(OuvHydrau), 2)
          OuvHydrau$FRIC <- 1 / 80
          OuvHydrau$CE1 <- 0.5
          OuvHydrau$CE2 <- 0.5
          OuvHydrau$CS1 <- 1
          OuvHydrau$CS2 <- 0.5
          OuvHydrau$CLP <- 1
          OuvHydrau$LBUS <- 0
          OuvHydrau$CV <- 0
          OuvHydrau$C56 <- 0
          OuvHydrau$CV5 <- 0
          OuvHydrau$C5 <- 0
          OuvHydrau$CT <- 0
          OuvHydrau$D1 <- 0
          OuvHydrau$D2 <- 0
          OuvHydrau$A1 <- 0
          OuvHydrau$A2 <- 0
          OuvHydrau$AA <- 1
          
          nc2 = which(OuvHydrau$CIRC == 2)
          if (length(nc2 > 0))
          {
            OuvHydrau[nc2,]$HAUT1 <-
              OuvHydrau[nc2,]$Surf / OuvHydrau[nc2,]$LRG
            OuvHydrau[nc2,]$CIRC <- 0
          }
          
          st_write(
            OuvHydrau,
            dsn = file.path(dsnlayerC, "OuvHydrau_Select.shp"),
            delete_layer = T,
            quiet = T
          )
          file.copy(file.path(chem_routine,"C2D",Doss_Qml,paste0("OuvHydrau_Select", ".qml")),
                    file.path(dsnlayerC,paste0("OuvHydrau_Select", ".qml")))
          OuvHydrau <-
            st_intersection(OuvHydrau,
                            st_buffer(Contoursanstrou, contour$Cont2_Buf / 2))
          # On coupe les ouvrages, s ils n ont pas la meme longueur on exporte un fichier pour le faire voir
          difflong <-
            OuvHydrau$LENGTH - round(st_length(OuvHydrau), 2)
          print(difflong)
          zer <- 0
          units(zer) <- "m"
          if (max(difflong) > zer)
          {
            st_write(
              OuvHydrau[which(difflong > zer),],
              dsn = file.path(dsnlayerC, "OuvHydrau_SelectCoup.shp"),
              delete_layer = TRUE,
              quiet = TRUE
            )
            file.copy(file.path(chem_routine,"C2D",Doss_Qml,paste0("OuvHydrau_Select", ".qml")),
                      file.path(dsnlayerC,paste0("OuvHydrau_SelectCoup", ".qml")))
          }
          
          # Récupération de la géométrie
          OuvHydrau_Debut_Fin <- sapply(1:dim(OuvHydrau)[1],
                                        function(x) {
                                          cbind(st_coordinates(OuvHydrau[x,])[1, 1:2],
                                                st_coordinates(OuvHydrau[x,])[2, 1:2])
                                        })
          
          # Récupération des entrées et sorties
          OH_BoutsPts <-
            do.call(rbind, lapply(1:dim(OuvHydrau)[1], function(x) {
              cbind(st_coordinates(OuvHydrau[x,])[c(1, dim(st_coordinates(OuvHydrau[x,]))[1]), 1:2], x, rbind(1, 2))
            }))
          # Suppression des points de contraintes trop proches
          
          Ratio <- 3
          Exz_Dx <- contour$Exz1_Dx
          
          Exz_Pts <-
            GardeNvxPointsLoinPointsInitiaux(Exz_Pts, OH_BoutsPts, Ratio, Exz_Dx)
          
          # et ajouter les points du réseau en dernier
          for (ip in unique(OH_BoutsPts[, 3]))
          {
            vecte <- (OH_BoutsPts[ip * 2 - 1, 1:2] - OH_BoutsPts[ip * 2, 1:2])
            # On pourrait faire un theta 1 et theta 2 avec les deux points continus et pas les extrémité si besoin!
            Theta <- atan(vecte[2] / vecte[1])
            
            if (vecte[1] < 0) {
              Theta <- Theta + pi
            }
            
            distES <- (sum(vecte ^ 2)) ^ 0.5
            if (distES > (Ratio * Exz_Dx))
            {
              iang <- 1:6
            } else{
              # print("OuvHydrau trop court pour instant")
              iang <- 1:3
            }
            
            # 6 ou 3 points a l entree
            RatioOH <- 2
            for (ies in c(-1, 0))
            {
              Aj_Ptstmp <-
                t(sapply(iang, function(x) {
                  OH_BoutsPts[ip * 2 + ies, 1:2] + Exz_Dx * cbind(cos((x - 2) * 2 * pi / 6 +
                                                                        Theta), sin((x - 2) * 2 * pi / 6 + Theta))
                }))
              #Verification qu ils ne soient pas trop proche de points existants
              if (ip > 1 | ies != -1)
              {
                Aj_Ptstmp <-
                  GardeNvxPointsLoinPointsInitiaux(Aj_Ptstmp, Aj_Pts, RatioOH, Exz_Dx)
                if (length(Aj_Ptstmp) > 2)
                {
                  Aj_Pts <- rbind(Aj_Pts, Aj_Ptstmp)
                }
              } else{
                Aj_Pts <- Aj_Ptstmp
              }
            }
          }
          
          Aj_Pts <-
            GardeNvxPointsLoinPointsInitiaux(Aj_Pts, unique(OH_BoutsPts[, 1:2]), 0.99, Exz_Dx)
          if (dim(Aj_Pts)[1] > 0)
          {
            Exz_Pts <- rbind(Exz_Pts, Aj_Pts) #,unique(OH_BoutsPts[,1:2]))
          }
        }
      }
      
      nom_LigContr <- contour$LigContr
      if (file.exists(nom_LigContr) == TRUE)
      {
        # Lecture du réseau
        LigContr <- st_read(nom_LigContr)
        LigContr <-
          st_cast(st_cast(LigContr, "MULTILINESTRING"), "LINESTRING")
        
        # On récupère ce qui intersecte
        # et ce qui a un code >=0
        nbRC <- st_intersects(LigContr, contour)
        n_int <-  which(sapply(nbRC, length) > 0)
        
        if (length(n_int > 0) > 0)
        {
          cat(nomcontour," ---- Récupération des lignes de contraintes\n")
          LigContr = LigContr[n_int,]
          
          LigContr = st_intersection(LigContr,
                                     st_buffer(Contoursanstrou, contour$Cont2_Buf / 2))
          LigContr = st_cast(st_cast(LigContr, "MULTILINESTRING"), "LINESTRING")
          st_write(
            LigContr,
            dsn = file.path(dsnlayerC, "LigContr_Select.shp"),
            delete_layer = T,
            quiet = T
          )
          file.copy(file.path(chem_routine,"C2D",Doss_Qml,paste0("LigContr_Select", ".qml")),
                    file.path(dsnlayerC,paste0("LigContr_Select", ".qml")))
          
          # S'il y a des ouvrages, les ouvrages prioritaires
          if (file.exists(nom_OuvHydrau) == TRUE)
          {
            OH_BoutsPts = do.call(rbind, lapply(1:dim(OuvHydrau)[1], function(x) {
              cbind(st_coordinates(OuvHydrau[x,])[c(1, dim(st_coordinates(OuvHydrau[x,]))[1]), 1:2], x, rbind(1, 2))
            }))
            
            OH_Bouts = do.call(rbind, lapply(1:dim(OH_BoutsPts)[1],
                                             function(x)
                                             {
                                               st_sf(
                                                 data.frame(
                                                   NumOH = OH_BoutsPts[x, 3],
                                                   ES = OH_BoutsPts[x, 4],
                                                   "geometry" =
                                                     st_sfc(st_point(OH_BoutsPts[x, 1:2], dim = "XY"))
                                                 ),
                                                 crs = EPSG
                                               )
                                             }))
            
            # parametres à sortir
            BufOH_Bouts=contour$Exz1_Dx
            
            OH_Bouts_Buf=st_union(st_buffer(OH_Bouts, BufOH_Bouts))
            # calcul des morceaux de lignes coupés
            # S'ils sont trop petits, il faut élargir la zone supprimée
            # sinon possible maillage trop petit
            LigContr_Supp=st_intersection(LigContr,OH_Bouts_Buf)
            if (dim(LigContr_Supp)[1]>0)
            {
              LigContr_Supp=st_cast(st_cast(LigContr_Supp,"MULTILINESTRING"),"LINESTRING")
              
              LigContr_Supp$longueur=st_length(LigContr_Supp)
              units(LigContr_Supp$longueur)=NULL
              nbtropcourt=which(LigContr_Supp$longueur<2/3*BufOH_Bouts)
              if (length(nbtropcourt)>0)
              {
                OH_Bouts_Buf=st_union(OH_Bouts_Buf,
                                      st_union(st_buffer(LigContr_Supp[nbtropcourt,],1/3*BufOH_Bouts)))
                # st_write(
                #   OH_Bouts_Buf,
                #   dsn = file.path(dsnlayerC, "OH_Bouts_Buf.gpkg"),
                #   delete_layer = T,
                #   quiet = T
                # )
                # 
                # st_write(
                #   st_union(st_buffer(LigContr_Supp[nbtropcourt,],1/3*BufOH_Bouts)),
                #   dsn = file.path(dsnlayerC, "nbtropcourt.gpkg"),
                #   delete_layer = T,
                #   quiet = T
                # )
                # 
                # st_write(
                #   LigContr_Supp[nbtropcourt,],
                #   dsn = file.path(dsnlayerC, "lignetropcourt.gpkg"),
                #   delete_layer = T,
                #   quiet = T
                # )
                # 
                # st_write(
                #   LigContr_Supp,
                #   dsn = file.path(dsnlayerC, "lignetoute.gpkg"),
                #   delete_layer = T,
                #   quiet = T
                # )
              }else{
              }
            }
            
            # st_erase = function(x, y)
            #   st_difference(x, st_union(st_combine(y)))
            # 
            # LigContr <-
            #   st_cast(st_cast(st_erase(
            #     LigContr, st_union(st_buffer(OH_Bouts, 3))
            #   ), "MULTILINESTRING"),
            #   "LINESTRING")
            
            # On ne garde que les morceaux de ligne eloigne des bouts des OH
            LigContr <-
              st_cast(st_cast(st_difference(
                LigContr, st_union(st_combine(OH_Bouts_Buf))
              ), "MULTILINESTRING"),
              "LINESTRING")
            
            # st_write(
            #   st_geometry(LigContr),
            #   dsn = file.path(dsnlayerC, "LigContr_Selectvf.gpkg"),
            #   delete_layer = T,
            #   quiet = T
            # )
          }
          LigContr$Longueur = st_length(LigContr)
          
          # On coup en tres petits morceaux les lignes
          LigC_Dx = 0.85 * contour$Exz1_Dx
          pasinter = 0.1
          Simple2 = st_segmentize(
            st_simplify(
              LigContr,
              preserveTopology = FALSE,
              dTolerance = 0.05
            ),
            pasinter
          )
          Simple2$Longeur = st_length(Simple2)
          Seuil = 4 / 5 * LigC_Dx
          units(Seuil) = "m"
          Simple2 = Simple2[which(Simple2$Longueur > Seuil),]
          
          XYaj = list()
          XYaj2 = list()
          
          for (ipt in 1:dim(Simple2)[1])
          {
            # plot(Simple2[ipt,1])
            # st_write(
            #   Simple2[ipt,1],
            #   dsn = file.path(dsnlayerC, paste0("LigContr_Select",ipt,".gpkg")),
            #   delete_layer = T,
            #   quiet = T
            # )
            XY = cbind(as.data.frame(st_coordinates(Simple2[ipt,])), 0, 0)
            XY[-1, 4] = sapply(2:dim(XY)[1], function(x) {
              ((XY[x, 1] - XY[x - 1, 1]) ^ 2 + (XY[x, 2] - XY[x - 1, 2]) ^ 2) ^ 0.5
            })
            XY[, 5] = sapply(1:dim(XY)[1], function(x) {
              sum(XY[1:x, 4])
            })
            # Calcul du nombre d intervalles entre 2 points pour les lignes de contraintes
            Interva = ifelse(max(XY[, 5]) < 2 * contour$Exz1_Dx,
                             2,
                             ceiling((max(XY[, 5])) / LigC_Dx))
            
            IndGood = sapply(max(XY[, 5]) * seq(0, Interva, 1) / Interva,
                             function (x) {
                               which(abs(XY[, 5] - x) == min(abs(XY[, 5] - x)))[1]
                             })
            
            XY = as.matrix(XY[IndGood,])
            
            # inc1=dim(XY)[1]
            #   inc2=1
            # while(((XY[1,inc1]-XY[inc2,1])^2+(XY[inc1,2]-XY[inc2,2])^2)^0.5<0.5*LigC_Dx)
            # {
            #   XY=XY[-dim(XY)[1],]
            # }
            
            XYaj[[ipt]] = do.call(rbind, lapply(1:dim(XY)[1],
                                                function(x)
                                                {
                                                  st_sf(data.frame(
                                                    Num = x,
                                                    "geometry" =
                                                      st_sfc(st_point(XY[x, 1:2], dim = "XY"))
                                                  ),
                                                  crs = EPSG)
                                                }))
          }
          LigContr_Pts = do.call(rbind, XYaj)
          st_write(
            LigContr_Pts,
            dsn = file.path(dsnlayerC, "XYaj.shp"),
            delete_layer = T,
            quiet = T
          )
          
          Exz_Dx <- contour$Exz1_Dx
          
          RatioLC = 2
          
          Exz_Pts = GardeNvxPointsLoinPointsInitiaux(Exz_Pts,
                                                     st_coordinates(LigContr_Pts)[, 1:2],
                                                     RatioLC,
                                                     Exz_Dx)
          
          Exz_Pts = rbind(Exz_Pts, st_coordinates(LigContr_Pts)[, 1:2])
        }
      }
      if (file.exists(file.path(dsnlayerC, "OuvHydrau_Select.shp")) == TRUE)
      {
        Exz_Pts = unique(rbind(Exz_Pts, OH_BoutsPts[, 1:2]))
      }
    } else{
      # Gestion des maillages s appuyant sur le raster
      Exz_Pts = NaN
    }
  }
  # # Gestion des sections de controle
  # source(paste(chem_routine,"\\C2D\\Cartino2D_Utilitaires.R",sep=""), encoding="utf-8")
  # Gestion_Sections_Controle(dsnlayer,dsnlayerC,nom_SectCont,contour)
  
  # return(list(Exz_Pts,contour_segm,contour_int))
  return(Exz_Pts)
}

#################################################################################################
################   Etape1_MNT
#################################################################################################
Etape1_MNT = function(dsnlayerC,
                      Lidar,
                      dsnLidar,
                      # Exz_Pts,
                      contour_segm,
                      contour_int)
{
  #############################################################################
  ################ LIDAR #####################################################
  #############################################################################
  cat(nomcontour, " ---- Récupération du Lidar\n")
  # Création du repertoire des fichiers asc
  dsnlayerCL = paste0(dsnlayerC, '/ASC')
  
  # récupération des dalles concernées
  nb = st_intersects(Lidar, contour_segm)
  n_int = which(sapply(nb, length) > 0)
  LidarC = Lidar[n_int,]
  
  # Création de la liste des dalles concernées
  if(length(which(is.na(LidarC$DOSSIERASC)))==0){
    listeASC = paste0(dsnLidar, '/', LidarC$DOSSIERASC, '/', LidarC$NOM_ASC)
  }else{
    listeASC = paste0(dsnLidar, '/', LidarC$NOM_ASC)
    
  }
  
  
  # Creation du fichier virtuel
  nom_ascvrt = file.path(dsnlayerC, "listepourvrt.txt")
  file.create(nom_ascvrt)
  write(listeASC, file = nom_ascvrt, append = T)
  vrtfile = paste0(dsnlayerC, "\\", nom_MNT, ".vrt") ##chemin du vrt à créer
  cmd = paste(shQuote(OSGeo4W_path),"gdalbuildvrt",vrtfile,"-input_file_list",nom_ascvrt)
  print(cmd);system(cmd)
  unlink(nom_ascvrt)
  
  # Importation raster dans Grass
  cmd=paste0("r.in.gdal -o --quiet --overwrite input=",vrtfile," output=",nom_MNT)
  print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
  
  # importation vecteur dans grass grand contour
  cmd=paste0("v.in.ogr -o --quiet --overwrite input=",file.path(dsnlayerC, paste0(nom_STEP1_Contour, ".shp"))," output=",nom_STEP1_Contour)
  print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
  
  # importation de la zone de validité de calcul
  cmd=paste0("v.in.ogr -o --quiet --overwrite input=",file.path(dsnlayerC, paste0(nom_STEP2_Zone_Valid_Calcul, ".shp"))," output=",nom_STEP2_Zone_Valid_Calcul)
  print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
  
  # déclaration de la région sur le raster
  # Si on a un maillage reglé
  # if (is.na(Exz_Pts)[1] == TRUE)
  if (is.na(as.numeric(contour$Exzeco))==F)
  {
    # cmd=paste0("g.region --overwrite --quiet -a vector=",nom_STEP1_Contour," res=",as.character(contour$Exzeco)," align=",nom_MNT)
    cmd=paste0("g.region --overwrite --quiet -a vector=",nom_STEP1_Contour," res=",as.character(contour$Exzeco))
    print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
    
  }else{
    # Si on a un maillage destructuré
    cmd=paste0("g.region --overwrite --quiet -a vector=",nom_STEP1_Contour," align=",nom_MNT)
    print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
  }
  
  cmd=paste0("r.mask --quiet --overwrite vector=",nom_STEP1_Contour)
  print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
  
  # Ajout 15/03/2024 pour la gestion des mnt 25m sur une résolution 25m décalée en grille
  # nom_MNTMask="MasquesAGarder"
  # cmd=paste0("r.resample --quiet --overwrite input=","MASK"," output=",nom_MNTMask)
  # print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
  
  # ajout du 18/02/2025 pour gérer les -9999 consideré comme des données dans les rasters ign
  cmd=paste0("r.null map=",nom_MNT," setnull=-9999,-999")
  print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
  
  # cmd=paste0("r.resample --quiet --overwrite input=",nom_MNT," output=",nom_MNT2a)
  # Modification de l résolution
  nom_MNT2a = paste0(nom_MNT, 'mask1a')
  cmd=paste0("r.resamp.stats --quiet --overwrite input=",nom_MNT," output=",nom_MNT2a)
  print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
  
  # Ré-échantillonage des Stricklers et CN si maillage grille
  if (is.na(as.numeric(contour$Exzeco))==F)
  {
    
  }
  
  #Debut20231107
  # Comblement de la partie intérieure
  ########
  cmd=paste0("r.mask --quiet --overwrite vector=",nom_STEP2_Zone_Valid_Calcul)
  print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
  
  #----- 
  NomUnivar = file.path(dsnlayerC, "univar.txt")
  cmd=paste0("r.univar --quiet --overwrite map=",nom_MNT2a," output=",NomUnivar)
  print(cmd);toto=system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
  if (toto!=0){cat("Vous avez un problème dans ",BatGRASS,"\n");BUGDEGRASS=BOOM}
  MinimMNT =max(as.numeric(scan(file = NomUnivar,NomUnivar,sep = ":",skip = 6,nlines = 1,dec = ".")[2]),-50)
  print(MinimMNT)
  
  # Creation d'un MNT à l'altitude mnimale
  MNTMini="MNTpourcomber"
  cmd=paste0("g.remove --quiet -f type=raster name=",MNTMini)
  print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
  
  cmd=paste0("r.mask --quiet --overwrite vector=",nom_STEP1_Contour)
  print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
  
  cmd=paste0("r.mapcalc --quiet --overwrite ",MNTMini,"=",MinimMNT,"*","MASK")
  print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
  
  # Patch avec en priorite le MNT et ailleurs le MNT a altitude minimale
  nom_MNT2 = paste0(nom_MNT, 'CombleInterieur')
  cmd=paste0("r.patch --quiet --overwrite ","input=",nom_MNT2a,",",MNTMini," output=",nom_MNT2," nprocs=",NProcGrass)
  print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
  
  cmd=paste0("r.mask --quiet --overwrite vector=",nom_STEP1_Contour)
  print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
  
  nom_MNT2a=nom_MNT2
  #Fin20231107
  
  
  ####
  nfiltre=as.numeric(substr(contour$Exzeco,6,nchar(contour$Exzeco)))
  if (is.na(nfiltre)==F)
  {
    ########
    cmd=paste0("r.mask --quiet --overwrite vector=",nom_STEP2_Zone_Valid_Calcul)
    print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
    
    #---- Creation du filtre et Ã©criture dans un fichier
    centre=(nfiltre+1)/2
    Matric=matrix(0, ncol = nfiltre, nrow=nfiltre)
    
    for (inc in 1:centre)
    {
      decal=centre-inc
      for (ij in 0:decal)
      {
        Matric[centre+ij,centre+(decal-ij)]=2*inc-1
        Matric[centre+ij,centre-(decal-ij)]=2*inc-1
        Matric[centre-ij,centre+(decal-ij)]=2*inc-1
        Matric[centre-ij,centre-(decal-ij)]=2*inc-1
      }
    }
    
    nomexport=paste0(dsnlayerC,"/Filtre_",as.character(nfiltre),".txt")
    write.table(Matric,nomexport, quote = FALSE,col.names = FALSE,row.names = FALSE)
    
    #---- Calcul dans GRASS de la moyenne sur un voisinage pondere par le filtre
    # execGRASS("r.neighbors",flags=c("quiet","overwrite"),parameters=list(input=nominput,output=nomoutput,size=ifiltre,weight=nomexport))
    nomoutput=paste0("MNT_Nei",nfiltre)
    cmd=paste0("r.neighbors --quiet --overwrite input=",nom_MNT2a," output=",nomoutput," size=",nfiltre," weight=",nomexport," nprocs=",NProcGrass)
    print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
    
    #---- Calcul de la courbure
    nominput=nomoutput
    nomoutput=contour$Exzeco
    # execGRASS("r.param.scale",flags=c("quiet","overwrite"),parameters=list(input=nominput, output=nomoutput,method="longc"))
    cmd=paste0("r.param.scale --quiet --overwrite input=",nominput," output=",nomoutput," method=longc")
    print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
    
    #---- Export
    cmd=paste0("r.out.gdal --quiet --overwrite -c -f input=",nomoutput," output=",paste0(dsnlayerC, "/", nomoutput, ".gpkg")," type=Float32 format=GPKG nodata=-9999")
    print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))  
    file.copy(file.path(chem_routine,"C2D",Doss_Qml,paste0(nomoutput, ".qml")),
              file.path(dsnlayerC,paste0(nomoutput, ".qml")))
    
    NomCourb_1="Courb_i1"
    # r.mapcalc expression=aaaaa = if( abs(Courb5)>0.052,1,null() )
    cmd=paste0("r.mapcalc --overwrite ",shQuote(paste0(NomCourb_1,"=if(abs( ",nomoutput,")>0.052,1,null())")))
    print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
    
    NomCourb_2="Courb_i2"
    # r.buffer input=aaaaa@Temp output=aaaaaabuf distances=5
    cmd=paste0("r.buffer --quiet --overwrite input=",NomCourb_1," output=",NomCourb_2," distance=",nfiltre)
    print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
    
    # r.mask -i --overwrite raster=aaaaaabuf@Temp
    cmd=paste0("r.mask -i --quiet --overwrite raster=",NomCourb_2)
    print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
    
    NomCourb_3="Courb_i3"
    # r.resample input=MASK@Temp output=aze
    cmd=paste0("r.resample --quiet --overwrite input=","MASK"," output=",NomCourb_3)
    print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
    
    # r.mask --overwrite vector=Step_Cartino2d_2_Valid_Calcul@Temp
    cmd=paste0("r.mask --quiet --overwrite vector=",nom_STEP2_Zone_Valid_Calcul)
    print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
    
    NomCourb_4="Courb_i4"
    # r.buffer input=aze@Temp output=aze2 distances=5
    cmd=paste0("r.buffer --quiet --overwrite input=",NomCourb_3," output=",NomCourb_4," distance=",ceiling(nfiltre/2))
    print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
    
    # r.mask -i raster=aze2@Temp
    cmd=paste0("r.mask -i --quiet --overwrite raster=",NomCourb_4)
    print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
    
    NomCourb_5="Courb_i5"
    # r.resample input=MASK@Temp output=toto
    cmd=paste0("r.resample --quiet --overwrite input=","MASK"," output=",NomCourb_5)
    print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
    
    # r.mask --overwrite vector=Step_Cartino2d_2_Valid_Calcul@Temp
    cmd=paste0("r.mask --quiet --overwrite vector=",nom_STEP2_Zone_Valid_Calcul)
    print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
    
    # g.region -a raster=MASK@Temp res=5 
    cmd=paste0("g.region -a --overwrite --quiet"," raster=","MASK"," res=",nfiltre)
    print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
    
    # r.to.vect -s --overwrite input=toto@Temp output=toto type=area
    cmd=paste0("r.to.vect -s --quiet --overwrite input=",NomCourb_5," output=",NomCourb_5," type=area")
    print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
    
    cmd=paste0("v.out.ogr --quiet --overwrite input=",NomCourb_5," output=",file.path(dsnlayerC,paste0(NomCourb_5,".gpkg"))," format=GPKG")
    print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
    
    # Si on a un maillage destructuré
    cmd=paste0("g.region --overwrite --quiet -a vector=",nom_STEP1_Contour," align=",nom_MNT)
    print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
    
    cmd=paste0("r.mask --quiet --overwrite vector=",nom_STEP1_Contour)
    print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
  }
  
  
  # S'il y a des DEBITS
  if(!(is.na(contour$SourceDeb) | nchar(contour$SourceDeb)==0))
  {
    # Suppression du masque (s'il existe)
    cmd=paste0("r.mask -r")
    print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
    
    nom_du_fichier_polyligne=paste0(nom_STEP2_Zone_Valid_Calcul,"polyline")
    cmd=paste0("v.type --quiet --overwrite input=",nom_STEP2_Zone_Valid_Calcul," output=",nom_du_fichier_polyligne," from=boundary to=line")
    print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
    
    nom_du_fichier_polyligne_buf=paste0(nom_STEP2_Zone_Valid_Calcul,"polylinebuf")
    # v.buffer input=Step_Cartino2d_2_Valid_Calculpolyline@Temp output=buff distance=10
    # Fred distance fixe, à améliorer
    cmd=paste0("v.buffer --quiet --overwrite input=",nom_du_fichier_polyligne," output=",nom_du_fichier_polyligne_buf," distance=25")
    print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
    
    cmd=paste0("r.mask --quiet --overwrite vector=",nom_du_fichier_polyligne_buf)
    print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
    
    NomUnivar = file.path(dsnlayerC, "univar.txt")
    cmd=paste0("r.univar --quiet --overwrite map=",nom_MNT2a," output=",NomUnivar)
    print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
    
    MinimMNT =max(as.numeric(scan(file = NomUnivar,NomUnivar,sep = ":",skip = 6,nlines = 1,dec = ".")[2]),-50)
    cat("Minimum de la zone valide",MinimMNT,"\n")
    
    nomPtsBas="PtsBas"
    cmd=paste0("r.mapcalc --overwrite ",shQuote(paste0(nomPtsBas,"=if(",nom_MNT2a," <(",max(contour$COTEAVALM, contour$COTEAVALAJ +MinimMNT),"),",1,',null())')))
    system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
    
    # Suppression du masque (s'il existe)
    cmd=paste0("r.mask -r")
    print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
    
    nomAval="Aval"
    cmd=paste0("r.buffer --quiet --overwrite input=",nomPtsBas," output=",nomAval," distance=",contour$Cont1_Buf)
    print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
    
    nomAval2="Aval2"
    cmd=paste0("r.mapcalc --overwrite ",shQuote(paste0(nomAval2,"=if(",nomAval," >-99,",1,',null())')))
    system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
    
    cmd=paste0("r.to.vect --quiet --overwrite input=",nomAval2," output=",nomAval," type=area")
    print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
    
    cmd=paste0("v.out.ogr --quiet --overwrite input=",nomAval," output=",file.path(dsnlayerC,paste0(nomAval,".gpkg"))," format=GPKG")
    print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
    
    Aval=sfheaders:: sf_remove_holes(st_read(file.path(dsnlayerC,paste0(nomAval,".gpkg"))))
    
    st_write(Aval, file.path(dsnlayerC,paste0(nomAval,".gpkg")),delete_layer = T,quiet = T)
    
    cmd=paste0("v.in.ogr -o --quiet --overwrite input=",file.path(dsnlayerC,paste0(nomAval,".gpkg"))," output=",nomAval)
    print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
    
    if (is.na(as.numeric(contour$Exzeco))==F)
    {
      contour_int2=contour_int
    }else{
      # Si on a un maillage destructuré
      contour_int2=st_buffer(contour_int,contour$Cont1_Dx)
    }
    
    contour_segm_fin <-  sfheaders:: sf_remove_holes(st_union(st_union(contour_int2,st_geometry(Aval))))
    
    
    Etape1_GrandContourBordDecoupe(contour_segm_fin,0,contour$Exzeco)
    
    # st_write(
    #   contour_segm_fin,
    #   dsn = file.path(dsnlayerC, paste0(nom_STEP1_Contour, ".shp")),
    #   layer = nom_STEP1_Contour,
    #   delete_layer = T,
    #   quiet = T
    # )
    
    # Nouvelle importation pour mise à jour
    cmd=paste0("v.in.ogr -o --quiet --overwrite input=",file.path(dsnlayerC, paste0(nom_STEP1_Contour, ".shp"))," output=",nom_STEP1_Contour)
    print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
    
    # modif FP objectif est de mettre d'avoir un mnt plein jusqu'à la limite du contour de calcul
    #----- Partie pour calculer l'altitude minimale dans tout le buffer
    #----- Future condition limite projeteée plus loin!
    NomUnivar = file.path(dsnlayerC, "univar.txt")
    cmd=paste0("r.univar --quiet --overwrite map=",nom_MNT2a," output=",NomUnivar)
    print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
    
    MinimMNT =max(as.numeric(scan(file = NomUnivar,NomUnivar,sep = ":",skip = 6,nlines = 1,dec = ".")[2]),-50)
    print(MinimMNT)
    
    # Creation d'un MNT à l'altitude mnimale
    MNTMini="MNTpourcomber"
    cmd=paste0("g.remove --quiet -f type=raster name=",MNTMini)
    print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
    
    cmd=paste0("r.mask --quiet --overwrite vector=",nom_STEP2_Zone_Valid_Calcul)
    print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
    
    cmd=paste0("r.mapcalc --quiet --overwrite ",MNTMini,"=",MinimMNT,"*","MASK")
    print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
    
    # Patch avec en priorite le MNT et ailleurs le MNT a altitude minimale
    nom_MNT2 = paste0(nom_MNT, 'mask1')
    cmd=paste0("r.patch --quiet --overwrite ","input=",nom_MNT2a,",",MNTMini," output=",nom_MNT2," nprocs=",NProcGrass)
    print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
    
  }else{
    # nom_MNT2 <- nom_MNT
  }
  
  #----- Partie pour calculer l'altitude minimale de la zone de calcul
  #----- Future condition limite projeteée plus loin!
  cmd=paste0("r.mask --quiet --overwrite vector=",nom_STEP2_Zone_Valid_Calcul)
  print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
  
  
  NomUnivar = file.path(dsnlayerC, "univar.txt")
  cmd=paste0("r.univar --quiet --overwrite map=",nom_MNT2," output=",NomUnivar)
  print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
  
  COTE_AVAL = max(contour$COTEAVALM,
                  contour$COTEAVALAJ + 
                    as.numeric(
                      scan(file = NomUnivar,NomUnivar,sep = ":",skip = 6,nlines = 1,dec = "."))[2])
  print(COTE_AVAL)
  cmd=paste0("r.mask -r")
  print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
  
  # v.to.rast input=Step_Cartino2d_2_Valid_Calcul@Temp output=Valid use=cat value=0
  # conversion en raster
  cmd=paste0("v.to.rast --quiet --overwrite input=",nom_STEP2_Zone_Valid_Calcul," output=",nom_STEP2_Zone_Valid_Calcul," use=cat value=0")
  print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
  
  cmd=paste0("r.mask --quiet --overwrite vector=",nom_STEP1_Contour)
  print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
  
  
  
  Bords="BordBuffer"
  nom_MNT4 = "MNTFinal"
  if(!(is.na(contour$SourceDeb) | nchar(contour$SourceDeb)==0))
  {
    # DEBIT
    # Buffer et on met une valeur basse
    cmd=paste0("r.buffer --quiet --overwrite input=",nom_STEP2_Zone_Valid_Calcul," output=",Bords," distance=",(-1.2*contour$Cont2_Buf))
    print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
    
    cmd=paste0("r.mask --quiet --overwrite vector=",nomAval)
    print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
    
    Bords2="BordBuffer2"
    cmd=paste0("r.mapcalc --quiet --overwrite ",
               paste0(Bords2,"=",MinimMNT,"+(1-",Bords,")*",-contour$DecAltiMNT))
    print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
    
    cmd=paste0("r.mask -i --quiet --overwrite vector=",nom_STEP2_Zone_Valid_Calcul)
    print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
    
    Bords3="BordBuffer3"
    cmd=paste0("r.resample --quiet --overwrite input=",Bords2," output=",Bords3)
    print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
    
    cmd=paste0("r.mask --quiet --overwrite vector=",nom_STEP1_Contour)
    print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
    
    cmd=paste0("r.series --overwrite input=",nom_MNT2,",",Bords3," output=",nom_MNT4," method=minimum"," nprocs=",NProcGrass)
    print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
  }else{
    #PLUIE
    # Buffer sur le raster avec increment de distance
    for (id in 1:(-1.2*contour$Cont2_Buf)){dista=ifelse(id==1,id,paste(dista,id,sep=","))}
    
    cmd=paste0("r.buffer --quiet --overwrite input=",nom_STEP2_Zone_Valid_Calcul," output=",Bords," distance=",dista)
    print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
    
    # r.mapcalc expression=result = MNT@Temp + (VamlidBuffer@Temp -1)*(-25)/150
    # Mise à la valeur 1
    
    cmd=paste0("r.mapcalc --quiet --overwrite ",
               paste0(nom_MNT4,"=",nom_MNT2,"-(",Bords,"-1)*(",contour$DecAltiMNT/contour$Cont2_Buf),")")
    print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
  }
  
  ### Maj 20211012
  cmd=paste0("g.region --overwrite --quiet"," raster=",nom_MNT4," zoom=",nom_MNT4)
  print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
  
  if (is.na(as.numeric(contour$Exzeco)) == T)
  {
    if (file.exists(file.path(dsnlayerC, "OuvHydrau_Select.shp")) == TRUE)
    {
      OuvHydrau = st_read(dsn = dsnlayerC,
                          layer = "OuvHydrau_Select")
      
      # Récupération de la géométrie
      OuvHydrau_Debut = sapply(1:dim(OuvHydrau)[1],
                               function(x) {
                                 cbind(st_coordinates(OuvHydrau[x,])[1, 1:2])
                               })
      
      # Creation point bout avec altitude et largeur oh
      OuvHydrau_PtDebFin = rbind(do.call(rbind,
                                         lapply(1:dim(OuvHydrau)[1],
                                                function(x)
                                                {
                                                  st_sf(data.frame(
                                                    Z = OuvHydrau$Z1[x],
                                                    LRG = OuvHydrau$LRG[x],
                                                    "geometry" =
                                                      st_sfc(st_point(st_coordinates(OuvHydrau[x,])[1, 1:2]), dim = "XY")
                                                  ),
                                                  crs = EPSG)
                                                })),
                                 do.call(rbind,
                                         lapply(1:dim(OuvHydrau)[1],
                                                function(x)
                                                {
                                                  st_sf(data.frame(
                                                    Z = OuvHydrau$Z2[x],
                                                    LRG = OuvHydrau$LRG[x],
                                                    "geometry" =
                                                      st_sfc(st_point(st_coordinates(OuvHydrau[x,])[dim(st_coordinates(OuvHydrau[x,]))[1], 1:2]), dim =
                                                               "XY")
                                                  ),
                                                  crs = EPSG)
                                                })))
      
      # Calcul de la taille de buffer a l entonnement et a la sortie
      RatioLRG = 2 / 3
      bufMin = 4 / 3 * contour$Exz1_Dx # modifier avec la taille de la maille pour faire en format grille
      bufMax = 2 * contour$Exz1_Dx
      bufMaxu = bufMax
      units(bufMaxu) <- "m"
      
      # Gestion de points proches pour prendre le min
      Descendre = 1
      OuvHydrau_PtDebFin$Z = sapply(1:dim(OuvHydrau_PtDebFin)[1], function(x) {
        min(OuvHydrau_PtDebFin[which(st_distance(OuvHydrau_PtDebFin, OuvHydrau_PtDebFin[x,]) <
                                       bufMaxu),]$Z)
      }) - Descendre
      
      # buffer sur les points
      buf = sapply(1:dim(OuvHydrau_PtDebFin)[1], function(x) {
        min(max(OuvHydrau_PtDebFin$LRG[x] * RatioLRG, bufMin),
            bufMax)
      })
      
      OuvHydrau_PtDebFinB = st_buffer(OuvHydrau_PtDebFin, buf)
      # export, on peut supprimer
      st_write(
        OuvHydrau_PtDebFinB,
        dsn = file.path(dsnlayerC, paste0("OuvHydrau_PtDebFin", ".shp")),
        delete_layer = T,
        quiet = T
      )
      
      # envoi dans grass
      nomOHES = "OH_ES"
      cmd=paste0("v.in.ogr -o --quiet --overwrite input=",file.path(dsnlayerC, paste0("OuvHydrau_PtDebFin", ".shp"))," output=",nomOHES)
      print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
      
      # conversion en reaster
      cmd=paste0("v.to.rast --quiet --overwrite input=",nomOHES," output=",nomOHES," use=attr attribute_column=Z")
      print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
      
      nom_MNT5 = "MNTvraifinal"
      # Minimum entre MNt de base et le ES des ouvrages
      cmd=paste0("r.series --overwrite input=",nom_MNT4,",",nomOHES," output=",nom_MNT5," method=minimum"," nprocs=",NProcGrass)
      print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
      
      nom_MNT4 = nom_MNT5
    }
  }
  
  cmd=paste0("r.mask --quiet --overwrite vector=",nom_STEP1_Contour)
  print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
  
  MNTFinalRond="MNTFinalRond"
  # r.mapcalc expression=MNTFinalRond = 0.01*round( 100*MNTFinal@Temp )             
  cmd=paste0("r.mapcalc --quiet --overwrite ",
             MNTFinalRond,"=0.01*round(100*",nom_MNT4,")")
  print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
  
  if (is.na(as.numeric(contour$Exzeco))==F)
  {
    cmd=paste0("r.mask --quiet --overwrite raster=",MNTFinalRond)
    print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
    
    Matric=matrix(0, ncol = 3, nrow=3)
    Matric[2,1]=1
    Matric[2,3]=1
    nomfilt=file.path(dsnlayerC,"FiltreDroiteGauche.txt")
    write.table(Matric,nomfilt, quote = FALSE,col.names = FALSE,row.names = FALSE)
    
    DroiteGauche="DroiteGauche"
    
    cmd=paste0("r.neighbors --quiet --overwrite input=",MNTFinalRond," output=",DroiteGauche ," method=count weighting_function=file weight=",nomfilt," nprocs=",NProcGrass)
    print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
    # r.neighbors input=MNTFinalRond@Temp output=DroiteGauche method=count weighting_function=file weight=C:\Cartino2D\France\MAMP_DEBIT\C09_241426km_X891198Y6251154\FiltreDroiteGauche.txt
    
    PixelSeul="PixelSeul"
    cmd=paste0("r.mapcalc --quiet --overwrite ",
               PixelSeul,"=if(",DroiteGauche,">0,1,null())")
    # Ajout 15/03/2024 Bug travail 25m
    # cmd=paste0("r.mapcalc --quiet --overwrite ",
    #           PixelSeul,"=if(",DroiteGauche,">0,","if(",nom_MNTMask,">-99,","1,null()),null())")
    print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
    
    cmd=paste0("r.mask --quiet --overwrite raster=",PixelSeul)
    print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
  }
  
  cmd=paste0("r.out.gdal --quiet --overwrite -c -f input=",MNTFinalRond," output=",paste0(dsnlayerC, "/", "MNTFinal", ".gpkg")," type=Float32 format=GPKG nodata=-9999")
  print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))  
  
  # Export
  nomcsv = paste0(dsnlayerC, "/", nom_MNT, ".csv")
  cmd=paste0("r.out.xyz --quiet --overwrite input=",MNTFinalRond," output=",nomcsv," separator=comma")
  print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
  
  # Gestion des CN et Friction
  if (is.na(as.numeric(contour$CN)) == TRUE)
  {
    NomParamH = "CN"
    TAParamH = contour$CN
    Etape1_CNFriction(NomParamH,TAParamH,crs,contour_segm,dsnlayerC,nomcontour)
  }
  if (is.na(as.numeric(contour$Friction)) == TRUE)
  {
    NomParamH = "Friction"
    TAParamH = contour$Friction
    Etape1_CNFriction(NomParamH,TAParamH,crs,contour_segm,dsnlayerC,nomcontour)
  }
  
  Gestion_Sections_Controle(dsnlayer, dsnlayerC, contour, 0)
  
  # Export pour un maillage avec Exzeco
  # if (is.na(Exz_Pts)[1] == FALSE)
  if (is.na(as.numeric(contour$Exzeco))==T)
  {
    #############################################################################
    ################ PPUTILS #####################################################
    #############################################################################
    cat(nomcontour, " ---- Export du contouravec PPutils\n")
    # export du contour en csv
    setwd(chemin_pputils)
    #Modif 20210819 Bug_python au lieu Bug_python
    cmd = paste0(
      Bug_python,
      " shp2csv.py -i ",
      paste0(dsnlayerC, "\\", nom_STEP1_Contour, ".shp"),
      " -o ",
      paste0(dsnlayerC, "\\", nom_STEP1_Contour, ".csv")
    )
    print(cmd);system(cmd)
    if (file.exists(paste0(dsnlayerC, "\\", nom_STEP1_Contour, ".csv"))==F){cat("PPUTILS ne fonctionne pas");browser()}
  }
  
}

#################################################################################################
################   Etape1_GEO
#################################################################################################

# Création du fichier geo fichier d'entrée pour le mailleur GMSH
Etape1_GEO = function(dsnlayerC, Exz_Pts)
{
  ######################################################
  # 5.6.5 Master nodes files
  # # Copie des noeuds de contour
  cat(nomcontour, " ---- Création du fichier geo\n")
  setwd(dsnlayerC)
  tab1 = read.csv(paste0(dsnlayerC, "\\", nom_STEP1_Contour, "_nodes.csv"),
                  header = F)
  tab1[, 3] = 0
  
  # Export des points contraignats
  st_write(
    st_sfc(st_multipoint(Exz_Pts, dim = "XYZ")),
    dsn = file.path(dsnlayerC, paste0(nom_STEP2_Points, ".shp")),
    layer = nom_STEP2_Points,
    delete_layer = T,
    quiet = T
  )
  file.copy(file.path(chem_routine,"C2D",Doss_Qml,paste0(nom_STEP2_Points, ".qml")),
            file.path(dsnlayerC,paste0(nom_STEP2_Points, ".qml")))
  
  colnames(tab1) = cbind("X", "Y", "Z")
  Z = 0
  write.table(
    rbind(tab1, cbind(Exz_Pts, Z)),
    file = nom_nodes,
    row.names = FALSE,
    col.names = FALSE,
    sep = ","
  )
  
  setwd(chemin_pputils)
  
  setwd(dsnlayerC)
  nom_geo = paste0(nom_maillage, ".geo")
  file.create(nom_geo)
  dimptcontour = dim(tab1)[1]
  write(
    paste0(
      "Point(",
      1:(dimptcontour - 1),
      ") = {",
      round(tab1[-dimptcontour, 1], 3),
      ", ",
      round(tab1[-dimptcontour, 2], 3),
      ", 0.000, 0.000};"
    ),
    file = nom_geo,
    append = T
  )
  
  write(
    paste0(
      "Point(",
      (dimptcontour):(dim(Exz_Pts)[1] + dimptcontour - 1),
      ") = {",
      round(Exz_Pts[, 1], 3),
      ", ",
      round(Exz_Pts[, 2], 3),
      ", 0.000, 0.000};"
    ),
    file = nom_geo,
    append = T
  )
  
  geo3 = matrix(1:(dimptcontour - 1), dimptcontour - 1, 1)
  geo4 = geo3
  geo4[-(dimptcontour - 1),] = geo3[-1,]
  geo4[(dimptcontour - 1),] = 1
  write(
    paste0("Line(", geo3, ") = {", geo3, ", ", geo4, "};"),
    file = nom_geo,
    append = T
  )
  
  write(paste0("Line Loop(1) = {1:", (dimptcontour - 1), "};"),
        file = nom_geo,
        append = T)
  write(paste0("Physical Line(1) = {1:", (dimptcontour - 1), "};"),
        file = nom_geo,
        append = T)
  write(paste0("Plane Surface(1) = {1};"),
        file = nom_geo,
        append = T)
  write(paste0("Physical Surface(1) = {1};"),
        file = nom_geo,
        append = T)
  write(paste0(
    "Point {",
    (dimptcontour):(dim(Exz_Pts)[1] + dimptcontour - 1),
    "} In Surface {1};"
  ) ,
  file = nom_geo,
  append = T)
}

#################################################################################################
################   Ré-échantillonage
#################################################################################################
# ReechantillonageRasterC2D = function(nomok, Reso, dsnlayer)
# {
#   cmd=paste0("g.region --overwrite --quiet"," raster=",nomok," res=",as.character(Reso))
#   print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
#   
#   nomfinal = paste0(nomok, "_", formatC(Reso, width = 3, flag = "0"), "m")
#   cmd=paste0("r.resamp.stats --quiet --overwrite input=",nomok," output=",nomfinal)
#   print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
#   
#   cmd=paste0("g.region --overwrite --quiet"," zoom=",nomfinal)
#   print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
#   
#   if (dir.exists(dsnlayer) == F) {
#     dir.create(dsnlayer)
#   }
#   
#   cmd=paste0("r.out.gdal --quiet --overwrite -c -f input=",nomfinal," output=",file.path(dsnlayer, paste0(nomok, ".asc"))," format=AAIGrid"," nodata=-9999","createopt=DECIMAL_PRECISION=2")
#   print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
# }

###################################################
Etape1_CNFriction = function(NomParamH,
                             TAParamH,
                             crs,
                             contour_segm,
                             dsnlayerC,
                             nomcontour)
{
  dsnParamH = dirname(TAParamH)
  ParamH = st_read(TAParamH)
  st_crs(ParamH) = crs
  nb = st_intersects(ParamH, contour_segm)
  n_int = which(sapply(nb, length) > 0)
  ParamHC = ParamH[n_int,]
  nomcsv = paste0(dsnlayerC, "/", "MNT", ".csv")
  # Création de la liste des dalles concernées
  ListeParamH = paste0(dsnParamH, '/', ParamHC$DOSSIERASC, '/', ParamHC$NOM_ASC)
  
  nom_vrt = file.path(dsnlayerC, paste0("listepourvrt_", NomParamH, ".txt"))
  vrtfile = paste0(NomParamH, ".vrt")
  file.create(nom_vrt)
  write(ListeParamH, file = nom_vrt, append = T)
  vrtfile = paste0(dsnlayerC, "/", NomParamH, ".vrt") ##chemin du vrt à créer
  cmd = paste(shQuote(OSGeo4W_path),
              "gdalbuildvrt",
              vrtfile,
              "-input_file_list",
              nom_vrt) ## commande pour exécuter gdalbuildvrt
  print(cmd);system(cmd)
  unlink(nom_vrt)
  
  ##############Creation d'un fichier VRT###############
  cat(nomcontour, " ---- Travail du lidar dans Grass\n")
  cmd=paste0("r.in.gdal -o --quiet --overwrite input=",vrtfile," output=",NomParamH)
  print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
  cmd=paste0("r.in.xyz --quiet --overwrite input=",nomcsv," output=","MNTxyzmask"," separator=comma")
  print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
  cmd=paste0("g.region --overwrite --quiet"," raster=","MNTxyzmask")
  print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
  
  # masque sur le grand contour
  cmd=paste0("r.mask --quiet --overwrite raster=MNTxyzmask")
  print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
  
  if (is.na(as.numeric(contour$Exzeco))==F)
  {
    cat("Ré-échantillonage de ", NomParamH,"\n")
    NomParamH_=paste0(NomParamH,"Resamp")
    cmd=paste0("r.resamp.stats --quiet --overwrite input=",NomParamH," output=",NomParamH_)
    print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
  }else{
    NomParamH_=NomParamH
  }
  
  # nomcsv = paste0(dsnlayerC, "/", NomParamH, ".csv")
  # cmd=paste0("r.out.xyz --quiet --overwrite input=",NomParamH_," output=",nomcsv," separator=comma")
  # print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
  
  # 20241112 Ajout des vides au cas où les rasters de MNt et CN u Strickler ne soient pas cohérents
  ValDefautParam=ifelse(NomParamH == "CN",70,15)
  NomParamH_Vide=paste0(NomParamH,"Vide")
  cmd=paste0("r.mapcalc --quiet --overwrite ",NomParamH_Vide,"=",ValDefautParam,"*","MASK")
  print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
  
  NomParamH_patch=paste0(NomParamH,"Patch")
  cmd=paste0("r.patch --quiet --overwrite ","input=",NomParamH_,",",NomParamH_Vide," output=",NomParamH_patch," nprocs=",NProcGrass)
  print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
  
  
  nomcsv = paste0(dsnlayerC, "/", NomParamH, ".csv")
  cmd=paste0("r.out.xyz --quiet --overwrite input=",NomParamH_patch," output=",nomcsv," separator=comma")
  print(cmd);system(paste0(BatGRASS," ",SecteurGRASS," --exec ",cmd))
  
  # nomcsv = paste0(dsnlayerC, "/", NomParamH, ".csv")

  
}

Etape1_fn <- function(skip_to_next)
{
  tryCatch({ if (ETAPE[1] == 1)
  {
    cat("\014")
    cat("ETAPE 1 - Traitement de: ",contour$NOM,"\n")
    #Creation d'un monde GRASS
    unlink(dirname(SecteurGRASS),recursive=TRUE)
    toto=system(paste0(BatGRASS," -c EPSG:",shQuote(EPSG)," ",dirname(SecteurGRASS)," --text"))
    if (toto!=0){cat("Vous avez un problème dans ",BatGRASS,"\n");BUGDEGRASS=BOOM}
    system(paste0(BatGRASS," -c ",SecteurGRASS," --text"))
    cat("\014")
    cat("ETAPE 1 - Traitement de: ",contour$NOM,"\n")
    # Ouverture Table Assemblage Lidar
    fun_check_exist(contour$MNT,1)
    dsnLidar = dirname(contour$MNT)
    Lidar <- st_read(contour$MNT)
    
    st_crs(Lidar) = crs
    
    # Copie du dossier base dans dossier export
    copie_prerequis()
    
    # Fichier utilisé à cette étape
    # Travail sur les contours d'Exzeco pour réaliser les points de contrainte et sur les limites du domaine
    # Gestion des maillages s'appuyant sur Exzeco
    # system.time(Etape1_Exzeco(dsnlayer, contour, crs))
    
    resultat = Etape1_Contour(dsnlayer, contour, crs)
    contour_segm = resultat[[1]]
    contour_int = resultat[[2]]
    Contoursanstrou= resultat[[3]]
    
    # Exz_Pts = Etape1_Exzeco(dsnlayer, contour,contour_int,Contoursanstrou, crs)
    # 
    # Etape1_MNT(dsnlayerC, Lidar,dsnLidar, contour_segm, contour_int)
    Exz_Pts = Etape1_Exzeco(dsnlayer, contour,contour_int,Contoursanstrou, crs)
    Etape1_MNT(dsnlayerC, Lidar,dsnLidar, contour_segm, contour_int)
    
    
    
    
    
    # Travail sur le MNT pour récupérer les données et gérer une décroissance de la topo aux frontières
    # Etape1_MNT(dsnlayerC, Lidar,dsnLidar,Exz_Pts, contour_segm, contour_int)
    
    
    cat("c est ok", "\n")
    
    if (is.na(Exz_Pts)[1] == FALSE)
    {
      # Création du fichier geo fichier d'entrée pour le mailleur GMSH
      Etape1_GEO(dsnlayerC, Exz_Pts)
    }
    # cat("\014")
    unlink(dirname(SecteurGRASS),recursive=TRUE)
    
  }},error = function(e) {skip_to_next <<- TRUE} )
  return(skip_to_next)
  
}


Etape1_fn_parallel <- function()
{
  if (ETAPE[1] == 1)
  {
    cat("\014")
    cat("ETAPE 1 - Traitement de: ",contour$NOM,"\n")
    #Creation d'un monde GRASS
    unlink(dirname(SecteurGRASS),recursive=TRUE)
    toto=system(paste0(BatGRASS," -c EPSG:",EPSG," ",dirname(SecteurGRASS)," --text"))
    if (toto!=0){cat("Vous avez un problème dans ",BatGRASS,"\n");BUGDEGRASS=BOOM}
    system(paste0(BatGRASS," -c ",SecteurGRASS," --text"))
    cat("\014")
    cat("ETAPE 1 - Traitement de: ",contour$NOM,"\n")
    # Ouverture Table Assemblage Lidar
    fun_check_exist(contour$MNT,1)
    dsnLidar = dirname(contour$MNT)
    Lidar <- st_read(contour$MNT)
    
    st_crs(Lidar) = crs
    
    # Copie du dossier base dans dossier export
    copie_prerequis()
    
    # Fichier utilisé à cette étape
    # Travail sur les contours d'Exzeco pour réaliser les points de contrainte et sur les limites du domaine
    # Gestion des maillages s'appuyant sur Exzeco
    # system.time(Etape1_Exzeco(dsnlayer, contour, crs))
    resultat = Etape1_Contour(dsnlayer, contour, crs)
    contour_segm = resultat[[1]]
    contour_int = resultat[[2]]
    Contoursanstrou= resultat[[3]]
    
    Exz_Pts = Etape1_Exzeco(dsnlayer, contour,contour_int,Contoursanstrou, crs)
    
    Etape1_MNT(dsnlayerC, Lidar,dsnLidar, contour_segm, contour_int)
    
    # Travail sur le MNT pour récupérer les données et gérer une décroissance de la topo aux frontières
    # Etape1_MNT(dsnlayerC, Lidar,dsnLidar,Exz_Pts, contour_segm, contour_int)
    
    
    cat("c est ok", "\n")
    
    if (is.na(Exz_Pts)[1] == FALSE)
    {
      # Création du fichier geo fichier d'entrée pour le mailleur GMSH
      Etape1_GEO(dsnlayerC, Exz_Pts)
    }
    # cat("\014")
    
    unlink(dirname(SecteurGRASS),recursive=TRUE)
    
  }
}