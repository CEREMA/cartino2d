
if (file.exists(nomCNIR)==T)
{
  
  AireMinTrouSC=10000
  nom_tmp=paste0(dirname(nomCNIR),"tmp.gpkg")
  # Enlever les petits trous
  cmd <- paste0(qgis_process, " run native:deleteholes",
                " --INPUT=", shQuote(nomCNIR),
                " --MIN_AREA=", AireMinTrouSC,
                " --OUTPUT=", shQuote(nom_tmp))
  print(cmd); system(cmd)
  # unlink(nom_tmp)
  
  CNIR=st_read(nom_tmp)
  
  nb <- st_intersects(SH, CNIR)
  n_int <-  which(sapply(nb, length) > 0)
  if (length(n_int)>0)
  {
    SH_ <- SH[n_int,]
    SH_=SH_[,"NOM"]
    
    coords=st_coordinates(SH_)
    listeSF=list()
    nangle=72
    for (iangle in 1:nangle)
    {
      
      listeLignes=lapply(1:nrow(coords), function(x) {matrix(c(
        coords[x,1],#-RayonSC*cos(2*pi*iangle/nangle),
        coords[x,1]+RayonSC*cos(2*pi*iangle/nangle),
        coords[x,2],#-RayonSC*sin(2*pi*iangle/nangle),
        coords[x,2]+RayonSC*sin(2*pi*iangle/nangle)),
        ncol=2)})
      
      listeSF[[iangle]]=st_sf(data.frame(NOM=SH_$NOM,ANGLE=iangle,geometry=st_cast(st_sfc(geometry=st_multilinestring(x = listeLignes,dim="XY")),"LINESTRING")),crs=st_crs(CNIR))
    }

    SF=do.call(rbind,listeSF)
    nomSF                =file.path(dirname(nomCNIR),"SF1.gpkg")
    nomSFinters          =file.path(dirname(nomCNIR),"SF2inters.gpkg")
    nomSFinterspt        =file.path(dirname(nomCNIR),"SF3interspt.gpkg")
    nomSFSection         =file.path(dirname(nomCNIR),"SF4Sections_1ercote.gpkg")
    nomSFSectionenface   =file.path(dirname(nomCNIR),"SF5Sectionsenface.gpkg")
    nomSFSectionenfaceok =file.path(dirname(nomCNIR),"SF6Sectionsenfaceok.gpkg")
    nomSFSectionControl1 =file.path(dirname(nomCNIR),"SF7SectionsControl.gpkg")
    nomSFSectionControl2 =file.path(dirname(nomCNIR),"SF8SectionsControl.gpkg")
    st_write(SF,nomSF, delete_dsn = T,delete_layer = T, quiet = T)
    
    interqgisouR=0
    deb=format(Sys.time(),format="%Y%m%d_%H%M")
    if (interqgisouR==1) #### TRES TRES LONG
    {
      cmd <- paste0(qgis_process, " run native:createspatialindex",
                    " --distance_units=meters --area_units=m2 --ellipsoid=EPSG:7019 ",
                    " --INPUT=",nomSF)
      system(cmd)
      cmd <- paste0(qgis_process, " run native:createspatialindex",
                    " --distance_units=meters --area_units=m2 --ellipsoid=EPSG:7019 ",
                    " --INPUT=",nomCNIR)
      system(cmd)
      cat("st_intersection longue - Qgisprocess\n")
      cmd <- paste0(qgis_process, " run native:intersection",
                    " --INPUT=",shQuote(nomSF),
                    " --OVERLAY=",nomCNIR,
                    " --OUTPUT=",shQuote(nomSFinters),
                    " --GRID_SIZE=None")
      print(cmd); system(cmd)
      SF_int0=st_read(nomSFinters)
      cat("st_intersection longue - Qgisprocess\n")
    }else{
      cat("st_intersection longue - R\n")
      SF_int0=st_intersection(SF,CNIR)
      
      cat("st_intersection longue - R\n")
    }
    fin=format(Sys.time(),format="%Y%m%d_%H%M")
    cat(deb," - ",fin,"\n")
    
    # SF_int=st_cast(SF_int0,"LINESTRING")
    iciLML=which(st_geometry_type(SF_int0)=="LINESTRING" | st_geometry_type(SF_int0)=="MULTILINESTRING" )
    SF_int=st_cast(SF_int0[iciLML,],"LINESTRING")
    nb <- st_intersects(SF_int, st_buffer(SH_,1))
    n_int <-  which(sapply(nb, length) > 0)
    SF_int <- SF_int[n_int,]
    
    #### 
    SF_Sections=SF_int
    SF_Sections$longueur=st_length(SF_Sections)
    st_write(SF_Sections,nomSFinterspt, delete_dsn = T,delete_layer = T, quiet = T)
    
    SF_Sections=SF_Sections[order(SF_Sections$NOM,SF_Sections$longueur),]
    SF_Sections$ID=1:nrow(SF_Sections)

    ICI=sapply(unique(SF_Sections$NOM), function(x)
    {SF_Sections$ID[which(SF_Sections$NOM==x)[which(SF_Sections$longueur[which(SF_Sections$NOM==x)]==min(SF_Sections$longueur[which(SF_Sections$NOM==x)]))]]})
    # BonID=do.call(rbind,ICI)[,1]
    # ICI=sapply(1:length(ICI), function(x) {ICI[[x]][1]})
    BonID_ICI=sapply(1:length(ICI), function(x) {ICI[[x]][1]})
    SF_1ercote=SF_Sections[BonID_ICI,]
    units(SF_1ercote$longueur)=NULL
    nici=SF_1ercote$longueur<=RayonSC
    if (length(nici)>0)
    {
      if (nrow(SF_1ercote)!=length(nici)){browser}
      SF_1ercote=SF_1ercote[nici,]
      
      st_write(SF_1ercote,nomSFSection, delete_dsn = T,delete_layer = T, quiet = T)
      
      ICI2=matrix(BonID_ICI,nrow=1)
      colnames(ICI2)=names(ICI)
      ICI2=rbind(ICI2,SF_Sections[BonID_ICI,]$ANGLE)
      
      # On va supprimer 
      nasupp=function(inc,nangle)
      {
        # browser()
        asupp=(nangle+((inc-1/3*nangle):(inc+1/3*nangle)))%%nangle
        ici0=which(asupp==0)
        if (length(ici0)==1)
        {
          asupp[ici0]=nangle
        }
        return(asupp)
      }
      
      SF_Sections=SF_Sections[order(SF_Sections$NOM,SF_Sections$ANGLE),]
      
      enfacel=lapply(1:length(ICI), function(x) {which(SF_Sections$NOM==colnames(ICI2)[x])[-nasupp(ICI2[2,x],nangle)]})
      
      enface=do.call(c,enfacel)
      
      SF_enface=SF_Sections[enface,]
      st_write(SF_enface,nomSFSectionenface, delete_dsn = T,delete_layer = T, quiet = T)
      
      SF_enface=SF_enface[order(SF_enface$NOM,SF_enface$longueur),]
      SF_enface$ID=1:nrow(SF_enface)
      
      ICIenface=sapply(unique(SF_enface$NOM), function(x)
      {SF_enface$ID[which(SF_enface$NOM==x)[which(SF_enface$longueur[which(SF_enface$NOM==x)]==min(SF_enface$longueur[which(SF_enface$NOM==x)]))]]})
      #Bug ici parfois
      # SF_enfacecourt=SF_enface[do.call(c,ICIenface),]
      ICIenface=sapply(1:length(ICIenface), function(x) {ICIenface[[x]][1]})
      SF_enfacecourt=SF_enface[ICIenface,]
      
      units(SF_enfacecourt$longueur)=NULL
      nici=SF_enfacecourt$longueur<=RayonSC
      if (length(nici)>0)
      {
        if (nrow(SF_enfacecourt)!=length(nici)){browser}
        SF_enfacecourt=SF_enfacecourt[nici,]
        
        st_write(SF_enfacecourt,nomSFSectionenfaceok, delete_dsn = T,delete_layer = T, quiet = T)
        
        SF_1ercote$cote=1
        SF_enfacecourt$cote=2
        
        SF_2morc=rbind(SF_1ercote,SF_enfacecourt)
        SF_2morc=SF_2morc[order(SF_2morc$NOM),]
        
        Secti=list()
        Nom_l=list()
        inc=1
        for (inom in unique(SF_2morc$NOM))
        {
          nb=which(SF_2morc$NOM==inom) 
          if (length(nb)==2) 
          {
            coords=st_coordinates(SF_2morc[nb,])[c(2,4),1:2]
            # coords=st_coordinates(SF_2morc[nb,])[,1:2]
            # # Secti[[inc]]=matrix(coords,ncol=2)
            # sapply(1:4, function(x) {which(coords[x,1]==coords[-x,1] & coords[x,2]==coords[-x,2])})
            
            UV=coords[2,]-coords[1,]
            norme=(UV[1]^2+UV[2]^2)^0.5
            UV=UV/norme
            
            Secti[[inc]]=matrix(rbind(coords[1,]-ElargissementSection*UV,
                                      coords[2,]+ElargissementSection*UV),ncol=2)
            if (is.na(Secti[[inc]][1])==T){a=ez}
            
            Nom_l[[inc]]=inom
            inc=inc+1
          } 
        }
        nomexp=do.call(c,Nom_l)
        SectionControl=st_sf(data.frame(ID=1:length(nomexp),NOM=nomexp,OHFlash="OK",
                                        geometry=st_cast(st_sfc(geometry=st_multilinestring(x = Secti,dim="XY")),"LINESTRING")),crs=st_crs(CNIR))
        st_write(SectionControl,nomSFSectionControl1, delete_dsn = T,delete_layer = T, quiet = T)

        nbSC_C=st_within(SectionControl,contours[icontour,])
        n_int = which(sapply(nbSC_C, length)>0)
        SectionControl=SectionControl[n_int,]
        st_write(SectionControl,nomSFSectionControl2, delete_dsn = T,delete_layer = T, quiet = T)
      }else{
        cat("Toutes les sections sont trop larges en face\n")
        browser()
      }
    }else{
      cat("Toutes les sections sont trop larges dès le 1er coté\n")
      browser()
    }
  }
}