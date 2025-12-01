cat("\014")

nomzpp3=file.path(dsnexport,paste0(basename(dirname(dsnexport)),"_","ZPP_Final",".gpkg"))
if (file.exists(nomzpp3)==F)
{
  ################################################################################
  ############### Travail BDTopo #################################################
  ################################################################################
  
  Buf_Ferre=
    rbind(
      cbind("Etroite",2,3),
      cbind("Normale",4,6),
      cbind("Large",5,7.5)
    )
  colnames(Buf_Ferre)=cbind("Code","Buf_Min","Buf_Max")
  
  Buf_Hydro=rbind(
    cbind("Entre 0 et 5 m",1,5),
    cbind("Entre 5 et 15 m",5,15),
    cbind("Entre 15 et 50 m",15,50),
    cbind("Plus de 50 m",50,100),
    cbind("Entre 250 et 1250 m",250,1250),
    cbind("Plus de 1250 m",1250,1250),
    cbind("En attente de mise à jour",0.5,0.5),
    cbind("Sans objet",0.1,0.1)
  )
  colnames(Buf_Hydro)=cbind("Code","Buf_Min","Buf_Max")
  
  type_route= cbind("cleabs","nature","precision_planimetrique","precision_altimetrique","largeur_de_chaussee","nombre_de_voies")
  type_ferree=cbind("cleabs","nature","precision_planimetrique","precision_altimetrique","largeur"            ,"nombre_de_voies")
  type_hydro= cbind("cleabs","nature","precision_planimetrique","precision_altimetrique","classe_de_largeur"  ,"liens_vers_cours_d_eau")
  
  ColADupliquer= cbind("cleabs","nature","precision_planimetrique","precision_altimetrique","largeur_de_chaussee","nombre_de_voies","largeur","classe_de_largeur","liens_vers_cours_d_eau","NOM","INC","POSITION","geometry")
  
  # if (Cest_Contour_pas_Zone==0)
  # {
  #   Zones <- st_read(nomZone)
  #   st_crs(Zones)=EPSG
  #   Zones=arrange(Zones,ZONE)
  #   
  #   # par choix en boite de dialogue
  #   nchoix = select.list(Zones$ZONE,preselect = Zones$ZONE[which(Zones$ATRAITER==1)],
  #                        title = "Choisir les etapes a effectuer",multiple = T,graphics = T)
  #   nlala = which(Zones$ZONE %in% nchoix)
  #   if (length(nlala)==0){print("VOUSAVEZVOULUQUECAFASSEBADABOOM_CESTGAGNE");BOOM=BOOOM}
  #   # On focalise sur le champ ATRAITER==1
  #   Zones=Zones[nlala,]
  # }
  
  Departement=st_read(nomDpt)
  
  #### Fonctions utilisées
  
  BUF_PONTS=function(nomPont,FactMultP,Ch_long,Ch_larg,Zone,raciexp)
  {
    Pont =st_read(nomPont)
    nb=st_within(Pont,Zone)
    n_int = which(sapply(nb, length)>0)
    if (length(n_int)>0) 
    {
      print(dim(Pont))
      Pont=Pont[n_int,]
      print(dim(Pont))
      
      if (Ch_larg=="Rien")
      {
        Buf=1
      }else{
        Buf=pmax(Pont[[Ch_long]],Pont[[Ch_larg]])
        nici=which(Buf>50)      ; if (length(nici)>0){Buf[nici]=50/FactMultP}
        nici=which(Buf<5)       ; if (length(nici)>0){Buf[nici]=5}
        nici=which(is.na(Buf))  ; if (length(nici)>0){Buf[nici]=5}
      }
      
      PontBuf=st_buffer(Pont,FactMultP*Buf)
      st_write(st_geometry(PontBuf),
               file.path(dsnexport,paste0(Zone$ZONE,"_",raciexp,"_ZPP",".gpkg")), delete_layer=T, quiet=T)
    }
  }
  
  LectTroncons=function(nomGPKG,nomlayer,bbo_wkt)
  {
    # Lire les noms des couches disponibles dans le GeoPackage
    couches_disponibles <- st_layers(nomGPKG)
    
    # Vérifier si la couche souhaitée existe
    if (nomlayer %in% couches_disponibles$name) {
      troncon=st_read(dsn=nomGPKG,layer=nomlayer,wkt_filter = bbox_wkt)
      st_geometry(troncon)="geometry"
      LTR[[iDpt]]=troncon
    } else {
      cat("La couche spécifiée ",nomlayer," n'existe pas dans le fichier GeoPackage.\n")
      LTR[[iDpt]]=NA
    }
  }
  
  ColonPlus=function(tr_)
  {
    ColenPlus=setdiff(ColADupliquer,colnames(tr_))
    aj=data.frame(matrix(NaN,dim(tr_)[1],length(ColenPlus)))
    colnames(aj)=ColenPlus
    tr=cbind(tr_,aj)
    
    trc <- tr[, ColADupliquer]
    
    trc$Buf_Min=0;trc$Buf_Max=0
    
    for (itrc in 1:dim(trc)[1])
    {
      # cat(substr(trc$cleabs[itrc],1,8),"\n")
      if (substr(trc$cleabs[itrc],1,8)=="TRONROUT")
      {
        if (is.na(trc$largeur_de_chaussee[itrc]))
        {
          if (trc$nature[itrc]=="Chemin")
          {
            trc$Buf_Min[itrc]=FactMultiBDTopo*1.5;trc$Buf_Max[itrc]=FactMultiBDTopo*2.5
          }else{
            trc$Buf_Min[itrc]=FactMultiBDTopo*0.5;trc$Buf_Max[itrc]=FactMultiBDTopo*1.5
          }
        }
        else
        {
          trc$Buf_Min[itrc]=FactMultiBDTopo*round(trc$largeur_de_chaussee[itrc]/2  ,2)
          trc$Buf_Max[itrc]=FactMultiBDTopo*round(trc$largeur_de_chaussee[itrc]*2/3,2)
        }
      }
      
      if (substr(trc$cleabs[itrc],1,8)=="TRONFERR")
      {
        if (is.na(trc$largeur[itrc])==T)
        {trc$Buf_Min[itrc]=FactMultiBDTopo*1;trc$Buf_Max[itrc]=FactMultiBDTopo*2}
        else
        {
          nbf=which(Buf_Ferre[,1]==trc$largeur[itrc])
          trc$Buf_Min[itrc]=FactMultiBDTopo*as.numeric(trc$nombre_de_voies[itrc])*as.numeric(Buf_Ferre[nbf,2])/2
          trc$Buf_Max[itrc]=FactMultiBDTopo*as.numeric(trc$nombre_de_voies[itrc])*as.numeric(Buf_Ferre[nbf,3])/2
        }
      }
      
      if (substr(trc$cleabs[itrc],1,8)=="TRON_EAU")
      {
        if (is.na(trc$classe_de_largeur[itrc]))
        {
          trc$Buf_Min[itrc]=FactMultiBDTopo*0.5;trc$Buf_Max[itrc]=FactMultiBDTopo*1.5
        }
        else
        {
          nbf=which(Buf_Hydro[,1]==trc$classe_de_largeur[itrc])
          # print(nbf)
          # print(length(nbf))
          if (length(nbf)>0)
          {
            trc$Buf_Min[itrc]=FactMultiBDTopo*as.numeric(Buf_Hydro[nbf,2])/2
            trc$Buf_Max[itrc]=FactMultiBDTopo*as.numeric(Buf_Hydro[nbf,3])/2
          }
        }
      }
    }
    # Réorganisation des colonnes selon l'ordre obtenu
    
    return(trc)
  }
  
  Croise=function(nom1,tr1,typ1,nom2,tr2,typ2,inc)
  {
    inc_=1
    Postr1=sort(unique(tr1$position_par_rapport_au_sol))
    
    Postr2=sort(unique(tr2$position_par_rapport_au_sol))
    
    CroiseTop=list()
    
    for (isol1 in Postr1)
    {
      tr1_=tr1[which(tr1$position_par_rapport_au_sol==isol1),]
      isol2=as.numeric(isol1)+1
      tr2_=tr2[which(tr2$position_par_rapport_au_sol==isol2),]
      cat(nom1,"_",isol1, "_",nom2," ",isol2,"\n")
      if (dim(tr2_)[1]>0)
      {
        
        nb <- st_crosses(tr1_, tr2_)
        n_int <-  which(sapply(nb, length) > 0)
        tr1_ <- tr1_[n_int,typ1]
        if (length(n_int)>0)
        {
          # cat("st_crosses(tr1_, tr2_)","\n")
          tr1_$NOM=paste(nom1,isol1,nom2,isol2);tr1_$INC=inc;tr1_$POSITION="DESSOUS"
          tr1f=ColonPlus(tr1_)
          nb <- st_crosses(tr2_, tr1_)
          n_int <-  which(sapply(nb, length) > 0)
          tr2_ <- tr2_[n_int,typ2]
          if (length(n_int)>0)
          {
            tr2_$NOM=paste(nom1,isol1,nom2,isol2);tr2_$INC=inc;tr2_$POSITION="DESSUS"
            tr2f=ColonPlus(tr2_)
            CroiseTop[[inc_]]=rbind(tr1f,tr2f)
            inc_=inc_+1
          }
        }
      } 
    }
    if(inc_>1)
    {
      tmp___=dplyr::bind_rows(CroiseTop, .id = NULL)
      # return(do.call(rbind,CroiseTop))
      return(tmp___)
    }
  }
  
  for (iZone in 1:dim(Zones)[1])
  { 
    #---- BBox pour n'importer que ce qui est dedans dans R
    #---- On fait sur des carré km
    Zone=Zones[iZone,]
    bbox=st_bbox(Zone)
    
    bbox$xmin=floor(bbox$xmin/1000)*1000
    bbox$xmax=ceiling(bbox$xmax/1000)*1000
    bbox$ymin=floor(bbox$ymin/1000)*1000
    bbox$ymax=ceiling(bbox$ymax/1000)*1000
    
    bbox_wkt <- paste0("POLYGON((",bbox$xmin, " ",bbox$ymin, ",",bbox$xmax, " ",bbox$ymin, ",",bbox$xmax, " ",bbox$ymax, ",",bbox$xmin, " ",bbox$ymax, ",",bbox$xmin, " ",bbox$ymin, "))")
    
    # Intersection des départements
    nb=st_intersects(Departement,Zone)
    n_int = which(sapply(nb, length)>0)
    Dpt=Departement[n_int,]
    
    if (nrow(Dpt)>0)
    {
      
      # Boucle sur les départements qui intersectent la donnée
      LTR=list()
      LTVF=list()
      LTH=list()
      LCS=list()
      LCL=list()
      
      # fusion des divers départements intersecté
      # la BDTopo n'a pas la même structure, on doit gérer les champs...
      for (iDpt in 1:dim(Dpt)[1])
      {
        # ouverture de la BDTopo
        listeDpt=list.files(dsnDptBDTopo,pattern=paste0("_D",ifelse(nchar(Dpt$INSEE_DEP[iDpt])==2,paste0("0",Dpt$INSEE_DEP[iDpt]),Dpt$INSEE_DEP[1]),"-"),recursive=T)
        listeDpt=file.path(dsnDptBDTopo,listeDpt[grep(listeDpt,pattern=".gpkg")])[1]
        if (is.na(listeDpt)==T)
        {
          cat("###################################################################\n")
          cat("BDTopo non présente ",Dpt$INSEE_DEP[iDpt]," Merci de la télécharger\n")
          cat("###################################################################\n")
          Badaboom=boom
        }
        
        dsnlayerCE=dirname(listeDpt)
        nomgpkgCE=basename(listeDpt)
        
        ######################################################################################
        LTR[[iDpt]]=LectTroncons(file.path(dsnlayerCE,nomgpkgCE),"troncon_de_route",bbox_wkt)
        ######################################################################################
        LTVF[[iDpt]]=LectTroncons(file.path(dsnlayerCE,nomgpkgCE),"troncon_de_voie_ferree",bbox_wkt)
        ######################################################################################
        LTH[[iDpt]]=LectTroncons(file.path(dsnlayerCE,nomgpkgCE),"troncon_hydrographique",bbox_wkt)
        ######################################################################################
        
        LCS[[iDpt]]=LectTroncons(file.path(dsnlayerCE,nomgpkgCE),"construction_surfacique",bbox_wkt)
        if (is.null(dim(LCS[[iDpt]][1]))==F)
        {
          construction_surfacique=LCS[[iDpt]]
          st_geometry(construction_surfacique)="geometry"
          LCS[[iDpt]]=construction_surfacique[which(construction_surfacique$nature=="Pont" | construction_surfacique$nature=="Tunnel"),]
        }
        
        ######################################################################################
        nomlayer="construction_lineaire"
        
        construction_lineaire=st_read(dsn=file.path(dsnlayerCE,nomgpkgCE),layer=nomlayer,wkt_filter = bbox_wkt)
        st_geometry(construction_lineaire)="geometry"
        
        LongTunnelMax=100
        units(LongTunnelMax)="m"
        LCL[[iDpt]]=construction_lineaire[which(construction_lineaire$nature=="Pont" |
                                                  (construction_lineaire$nature=="Tunnel" & st_length(construction_lineaire)<LongTunnelMax)),]
      }
      
      # nettoyage des doublons de deux départements...
      cat("Fusion des troncons de route\n")
      # trroute=do.call(rbind, LTR)
      trroute=dplyr::bind_rows(LTR, .id = NULL)
      
      
      cat("Fusion des troncons ferrées\n")
      # trferre=do.call(rbind, LTVF)
      if (is.null(dim(LTVF[[iDpt]][1]))==F)
      {
        trferre=dplyr::bind_rows(LTVF, .id = NULL)
      }
      
      cat("Fusion des troncons hydro\n")
      # trhydro=do.call(rbind, LTH)
      # trhydro=trhydro[trhydro$origine!="Artificielle",]
      trhydro=dplyr::bind_rows(LTH, .id = NULL)
      
      if (is.null(dim(LCS[[iDpt]][1]))==F)
      {
        cat("Fusion des constructions surfacique et export\n")
        # ConstSu=do.call(rbind, LCS)
        ConstSu=dplyr::bind_rows(LCS, .id = NULL)
        if (dim(ConstSu)[1]>0)
        {
          NomB=file.path(dsnexport,paste0(Zone$ZONE,"_","ConstrSurf",".gpkg"))
          st_write(ConstSu,
                   NomB, delete_layer=T, quiet=T)
        }
      }
      
      cat("Fusion des constructions linéaires et export\n")
      # ConstLi=do.call(rbind, LCL)
      ConstLi=dplyr::bind_rows(LCL, .id = NULL)
      if (dim(ConstLi)[1]>0)
      {
        st_write(ConstLi,
                 file.path(dsnexport,paste0(Zone$ZONE,"_","ConstrLine",".gpkg")), delete_layer=T, quiet=T)
      }
      
      LTRONCONS=list()
      
      inc=1
      incTRONCONS=1
      if(is.null(dim(LTVF[[iDpt]][1]))==T){tour=c(1,2)}else{tour=c(1,2,3)}
      for (itr in tour)
      {
        if (itr==1){nom1="Route" ;tr1=trroute;typ1=type_route }
        if (itr==2){nom1="Hydro" ;tr1=trhydro;typ1=type_hydro }
        if (itr==3){nom1="Ferree";tr1=trferre;typ1=type_ferree}
        
        for (jtr in tour)
        {
          if (jtr==1){nom2="Route" ;tr2=trroute;typ2=type_route }
          if (jtr==2){nom2="Hydro" ;tr2=trhydro;typ2=type_hydro }
          if (jtr==3){nom2="Ferree";tr2=trferre;typ2=type_ferree}
          
          
          zut=Croise(nom1,tr1,typ1 ,nom2,tr2,typ2 ,inc)
          
          if (is.null(zut)==F)
          { 
            # print(zut$cleabs)
            if (dim(zut)[1]>length(unique(zut$cleabs)))
            {
              # browser()
            }
            cat("Bilan tronçons qui se croisent",nom1," ",nom2," sous ",dim(zut)[1],"\n")
            LTRONCONS[[incTRONCONS]]=zut
            inc=zut$INC[1]+1
            incTRONCONS=incTRONCONS+1
          }
        }
      }
      
      if (incTRONCONS>1)
      {
        cat("Fusion des troncons qui se croisent et export\n")
        Croisement=do.call(rbind,LTRONCONS)
        # Croisement=dplyr::bind_rows(LTRONCONS, .id = NULL)
        st_write(Croisement,
                 file.path(dsnexport,paste0(Zone$ZONE,"_","LTRONCONS",".gpkg")), delete_layer=T, quiet=T)
      }
      
      # calcul de la longeur à faire
      st_write(trroute[which(trroute$position_par_rapport_au_sol<0),],
               file.path(dsnexport,paste0(Zone$ZONE,"_","RouteSous",".gpkg")), delete_layer=T, quiet=T)
      
      if (is.null(dim(LTVF[[iDpt]][1]))==F)
      {
        st_write(trferre[which(trferre$position_par_rapport_au_sol<0),],
                 file.path(dsnexport,paste0(Zone$ZONE,"_","FerreSous",".gpkg")), delete_layer=T, quiet=T)
      }
      
      st_write(trhydro[which(trhydro$position_par_rapport_au_sol<0),],
               file.path(dsnexport,paste0(Zone$ZONE,"_","HydroSous",".gpkg")), delete_layer=T, quiet=T)
      
      CroisPonct=list()
      CroisSurfMin=list()
      CroisSurfMax=list()
      incCP=1
      incCS=1
      
      cat("Gestion de tous les croisements - long sur de grands domaines avec une forte urbanisation\n")
      cat(basename(dirname(dsnexport)), "\n")
      for (nomcrois in unique(Croisement$NOM))
      { 
        ncrois=which(Croisement$NOM==nomcrois)
        cat(nomcrois," - ",length(ncrois),"\n")
        Croisement_=Croisement[ncrois,]
        Croisement_DESSUS =Croisement_[which(Croisement_$POSITION=="DESSUS") ,]
        Croisement_DESSOUS=Croisement_[which(Croisement_$POSITION=="DESSOUS"),]
        
        pgb <- txtProgressBar(min = 0, max = dim(Croisement_DESSUS)[1],style=3)
        for (i in 1:dim(Croisement_DESSUS)[1])
        {
          setTxtProgressBar(pgb, i)
          Croisement_DESSUS_=Croisement_DESSUS[i,]
          nbss=st_crosses(Croisement_DESSOUS,Croisement_DESSUS_)
          n_intss = which(sapply(nbss, length)>0)
          Croisement_DESSOUS_=Croisement_DESSOUS[n_intss,]
          CroisPonct[[incCS]]=st_intersection(Croisement_DESSUS_,Croisement_DESSOUS_)
          resMin=st_intersection(
            st_buffer(Croisement_DESSUS_,as.numeric(Croisement_DESSUS_$Buf_Min)),
            st_buffer(Croisement_DESSOUS_,as.numeric(Croisement_DESSOUS_$Buf_Min)))
          CroisSurfMin[[incCS]]=resMin
          resMax=st_intersection(
            st_buffer(Croisement_DESSUS_,as.numeric(Croisement_DESSUS_$Buf_Max)),
            st_buffer(Croisement_DESSOUS_,as.numeric(Croisement_DESSOUS_$Buf_Max)))
          CroisSurfMax[[incCS]]=resMax
          incCS=incCS+1
        }
        cat("\n")
      }
      
      cat("Fusion CroisementSurfaceMin\n")
      CroisSurfMin=dplyr::bind_rows(CroisSurfMin, .id = NULL)
      st_write(CroisSurfMin[1:nrow(CroisSurfMin),],
               file.path(dsnexport,paste0(Zone$ZONE,"_","CroisementSurfaceMin",".gpkg")), delete_layer=T, quiet=T)
      
      cat("Fusion CroisementSurfaceMax\n") 
      CroisSurfMax=dplyr::bind_rows(CroisSurfMax, .id = NULL)
      st_write(CroisSurfMax[1:nrow(CroisSurfMax),],
               file.path(dsnexport,paste0(Zone$ZONE,"_","CroisementSurfaceMax",".gpkg")), delete_layer=T, quiet=T)
      
      cat("Fusion CroisementPonctuel\n") 
      NomA=file.path(dsnexport,paste0(Zone$ZONE,"_","CroisementPoint_tmp",".gpkg"))
      CroisPonct=dplyr::bind_rows(CroisPonct, .id = NULL)
      st_write(CroisPonct,
               NomA, delete_layer=T, quiet=T)
      
      # Comparaison avec les ponts
      CroisPonct$PONT=""
      ndessus=which(CroisPonct$POSITION=="DESSUS")
      cat("Gestion des intersections des points de croisement avec les constructions linéaires - long sur de grands domaines avec une forte urbanisation\n")
      if (dim(ConstLi)[1]>0)
      {
        nbdp= st_intersects(st_buffer(CroisPonct,1),ConstLi) 
        n_intdp = which(sapply(nbdp, length)>0)
        CroisPonct$PONT[ndessus[n_intdp]]="ConstLi_IGN"
      }
      
      cat("Gestion des intersections des points de croisement avec les constructions surfaciques - long sur de grands domaines avec une forte urbanisation\n")
      
      if (exists("ConstSu")==T){
        if (dim(ConstSu)[1]>0)
      {
        
        source(file.path(chem_filino,"FILINO_Utils.R")) 
        NomC=file.path(dsnexport,"LiaisonCroisPonct_ConstSurf.csv")
        liaison=FILINO_Intersect_Qgis(NomA,NomB,NomC)
        CroisPonct$PONT[which(is.na(match(CroisPonct$cleabs,liaison$cleabs))==F)]="ConstSu_IGN"
      }
      }
      # Savoir si cela enjambe un cours d'eau
      nbCE=which(is.na(CroisPonct$liens_vers_cours_d_eau.1)==F)
      nbCE2=which(substr(CroisPonct$liens_vers_cours_d_eau.1[nbCE],1,8)=="COURDEAU")
      CroisPonct$PONT[nbCE[nbCE2]]=paste0(CroisPonct$PONT[nbCE[nbCE2]],"_COURDEAU")
      
      st_write(CroisPonct,
               file.path(dsnexport,paste0(Zone$ZONE,"_","CroisementPoint",".gpkg")), delete_layer=T, quiet=T)
      
      ##############################################################################
      ############### Export des ZPP BDtopo- Zones Potentielles de Ponts (~IGN) #####
      ##############################################################################
      st_write(st_geometry(CroisSurfMax[1:nrow(CroisSurfMax),]),
               file.path(dsnexport,paste0(Zone$ZONE,"_","CroisementSurfaceMax_ZPP",".gpkg")), delete_layer=T, quiet=T)

      if (exists("ConstSu")==T){
      st_write(st_geometry(st_buffer(ConstSu,BufConstSurf)),
               file.path(dsnexport,paste0(Zone$ZONE,"_","ConstrSurf_ZPP",".gpkg")), delete_layer=T, quiet=T)
      }
      
      st_write(st_geometry(st_buffer(ConstLi,BufConstLi)),
               file.path(dsnexport,paste0(Zone$ZONE,"_","ConstrLine_ZPP",".gpkg")), delete_layer=T, quiet=T)
      
      
      ##############################################################################
      # Export des ZPP OHloacaux Isodor-et PNP  Zones Potentielles de Ponts (~IGN) #
      ##############################################################################
      if (file.exists(nomOHlocaux)){BUF_PONTS(nomOHlocaux ,BufOHlocaux   ,"Rien"           ,"Rien"          ,Zone,"OHlocaux" )}
      if (file.exists(nomPontRRNc)){BUF_PONTS(nomPontRRNc ,FactMultIsidor,"longueurPo"     ,"largeur"       ,Zone,"RRNc" )}
      if (file.exists(nomPontRRNnc)){BUF_PONTS(nomPontRRNnc,FactMultIsidor,"long"           ,"larg"          ,Zone,"RRNnc")}
      if (file.exists(nomPontPNP)){BUF_PONTS(nomPontPNP  ,FactMultPNP   ,"ph1_longueur_ur","ph1_largeur_ge",Zone,"PNP"  )}
      
      ##############################################################################
      ############### fusion desZones Potentielles de Ponts (~IGN) #####
      ##############################################################################
      listZPP=list.files(dsnexport,pattern=Zone$ZONE)
      nici=grep(listZPP,pattern="ZPP.gpkg")
      if (length(nici)>0)
      {
        listZPP=listZPP[nici]
        
        nomzpp1=file.path(dsnexport,paste0(Zone$ZONE,"_","ZPP_1merge",".gpkg"))
        nomzpp2=file.path(dsnexport,paste0(Zone$ZONE,"_","ZPP_2buffer",".gpkg"))
        nomzpp3=file.path(dsnexport,paste0(Zone$ZONE,"_","ZPP_Final",".gpkg"))
        
        dsnini=getwd()
        setwd(dsnexport)
        cmd=paste0(qgis_process, " run native:mergevectorlayers")
        for (iZPP in listZPP)
        {
          Vzpp=st_read(iZPP)
          if (nrow(Vzpp)>0)
          {
            cmd=paste0(cmd," --LAYERS=",shQuote(iZPP))
          }
        }
        
        cmd=paste0(cmd,
                   paste0(" --CRS=QgsCoordinateReferenceSystem('EPSG:",EPSG,"') "),
                   " --OUTPUT=",shQuote(nomzpp1))
        print(cmd);system(cmd)
        
        cmd=paste0(qgis_process, " run native:buffer",
                   " --INPUT=",shQuote(nomzpp1),
                   " --DISTANCE=",ResoMNTpourTaudem," --SEGMENTS=5 --END_CAP_STYLE=0 --JOIN_STYLE=0 --MITER_LIMIT=2 --DISSOLVE=True",
                   " --OUTPUT=",shQuote(nomzpp2))
        print(cmd);system(cmd)
        
        cmd=paste0(qgis_process, " run native:multiparttosingleparts",
                   " --INPUT=",shQuote(nomzpp2),
                   " --OUTPUT=",shQuote(nomzpp3))
        print(cmd);system(cmd)
        
        cat("\n")
        cat("\n")
        cat("\n")
        cat("\n")
        cat("###################################################################################\n")
        cat("######################### C2D A LIRE SVP ###########################################\n")
        cat("------------------------- Etape PreC2D 2_2-------------------------------------------\n")
        cat("le fichier",nomzpp3,"a été créé.R\n")
        cat("Vous pouvez vérifier les calculs\n")
        cat("######################### Fin C2D A LIRE ###########################################\n")
        
        
        unlink(nomzpp1)
        unlink(nomzpp2)
        setwd(dsnini)
      }
    }
  }
}else{
  cat(nomzpp3," déjà fait\n")
}