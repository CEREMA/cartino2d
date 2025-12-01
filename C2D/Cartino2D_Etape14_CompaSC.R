#################################################################################################
################   Etape14_Post_Sect_Controle
#################################################################################################

# Travail sur les contours d'Exzeco pour réaliser les points de contrainte et sur les limites du domaine
Etape14_CompaSC=function(dsnlayer,dsnlayerC,nomsc)
{
  library(readxl)  
  # dsnlayerC="C:\\Cartino2D\\France"
  # nomsc="C72km541629_Evt201510031815_04h50min_AMC3_Anti_15min_J1_ProjMF_MFSC.txt"
  # nomsc="C13km218271_Evt201510031815_03h10min_AMC3_Anti_15min_J1_4326_MFSC.txt"
  # # nomsc="C13km218271_Evt201510031830_03h10min_AMC3_Anti_15min_J1_ProjMF_MFSC.txt"
  
  # Ouverture des sections de calculs
  iocon <- file(file.path(dsnlayerC,nomsc),"r+")
  # Lecture du titre
  header <- readLines(iocon,n=1)
  titre=substr(header,22,nchar(header)-1)
  print(header)
  print(titre)
  
  # browser()
  # Lecture des variables
  header <- readLines(iocon,n=1)
  if (length(nchar(header))!=0)
  {
    print(header)
    Varia=strsplit(substr(header,14,nchar(header)), " ")
    print(Varia)
    
    header <- readLines(iocon)
    # print(header)
    
    unlink(iocon)
    
    
    # methode pour passer format normal ou chasse
    tab=readLines(file.path(dsnlayerC,nomsc),skip=2)
    tab=tab[which(is.na(as.numeric(substr(tab,1,22)))==F)]
    DEBITl=list()
    # DEBITl[[1]]=as.numeric(substr(tab,1,22))
    nlig=ceiling(length(Varia[[1]])/4)
    for (iv in 1:length(Varia[[1]]))
    {
      poslig=iv-(ceiling(iv/4)-1)*4
      poscol=seq(ceiling(iv/4),length(tab),nlig)
      # substr(tab,19,40) substr(tab,41,62)
      DEBITl[[iv]]=as.numeric(substr(tab[poscol],1+22*(poslig-1),23+22*(poslig-1)))
    }
    DEBIT=do.call(cbind,DEBITl)
    
    #ancienne methode
    # browser()
    # Tab=scan(file.path(dsnlayerC,nomsc),skip=2)
    # length(Tab)/length(Varia[[1]])
    # DEBIT=t(matrix(Tab,nrow=length(Varia[[1]]),ncol=length(Tab)/length(Varia[[1]])))
    colnames(DEBIT)=Varia[[1]]
    
    
    # Embrouille pour recuperer les fichiers avec sections chasse
    dsnlayerChasse=file.path("D:\\ClusterREM\\202203251529PC",basename(dsnlayerC))
    if (file.exists(file.path(dsnlayerChasse,nomsc))==T)
    {print(paste("Fichier QZ",file.path(dsnlayerC,nomsc)))
      VZ=1
      iocon2 <- file(file.path(dsnlayerChasse,nomsc),"r+")
      # Lecture du titre
      header <- readLines(iocon2,n=2)
      VariaC=strsplit(substr(header,14,nchar(header)), " ")
      print(VariaC)
      
      header <- readLines(iocon2)
      tabChasse=cbind(substr(header,1,30),
                      substr(header,31,34),
                      substr(header,35,43),
                      substr(header,44,52),
                      substr(header,53,61),
                      substr(header,62,70),
                      substr(header,71,79),
                      substr(header,80,88))
      
      colnames(tabChasse)=cbind("NOM_Section","IdSect","Temps","Debit","hmin","hmax","hmoy","Charmoy" )
      
      # Suppression des périodes sans calcul
      nbcal=which(as.numeric(tabChasse[,"hmoy"])>0) 
      tabChasse=tabChasse[nbcal,]
      
      # Remise en numérique de colonnes
      for (icol in 2:8)
      {
        tabChasse[,icol]=as.numeric(tabChasse[,icol])
      }  
    }else{
      VZ=0
    }
    
    
    
    couleurs=cbind("green","magenta","brown","orange","yellow","gray","cyan","black",
                   "green","magenta","brown","orange","yellow","gray","cyan","black",
                   "green","magenta","brown","orange","yellow","gray","cyan","black",
                   "green","magenta","brown","orange","yellow","gray","cyan","black",
                   "green","magenta","brown","orange","yellow","gray","cyan","black",
                   "green","magenta","brown","orange","yellow","gray","cyan","black")
    
    plty=cbind( "solid","solid","solid","solid","solid","solid","solid","solid",
                "dashed","dashed","dashed","dashed","dashed","dashed","dashed","dashed",
                "dotted","dotted","dotted","dotted","dotted","dotted","dotted","dotted",
                "dotdash","dotdash","dotdash","dotdash","dotdash","dotdash","dotdash","dotdash",
                "longdash","longdash","longdash","longdash","longdash","longdash","longdash","longdash",
                "twodash","twodash","twodash","twodash","twodash","twodash","twodash","twodash")
    
    
    jpeg(filename = file.path(dsnlayerC,paste0(substr(nomsc,1,nchar(nomsc)-4),".jpg")),
         width = 44.55, height = 31.5, units = "cm", quality = 75, res = 200)
    # Gestion de la Date avec Nom Evt ou pas
    
    plot(DEBIT[,1],abs(DEBIT[,2]),
         ylim=cbind(0,max(abs(DEBIT[,-1]))),
         col=couleurs[1],typ="l",lty=plty[1],
         xlab="Temps",
         ylab="Débits (m3/s)",main=substr(nomsc,1,nchar(nomsc)-6))
    
    if (dim(DEBIT)[2]>2)
    {
      for (i in 3:dim(DEBIT)[2])
      {
        lines(DEBIT[,1],abs(DEBIT[,i]),col=couleurs[i-1],lty=plty[i-1])
      }
    }
    legend("topleft",
           legend = Varia[[1]][2:dim(DEBIT)[2]],#colnames(DEBIT[,-1]),
           col=couleurs,
           lty=plty)
    
    dev.off()
    
    # Récupération des données de comparaisons historiques
    if (file.exists(file.path(dsnlayerC,"SectControl_Select.shp"))==TRUE)
    {
      SectControl=st_read(dsn = dsnlayerC,
                          layer = "SectControl_Select")
      SectControl$MaxQ=NA
      
      for (isc in 2:dim(DEBIT)[2])
      {  
        
        SectControl[isc-1,]$MaxQ=round(max(abs(DEBIT[,isc])),2)
      }
      st_write(SectControl,dsn=file.path(dsnlayerC,paste0(substr(nomsc,1,nchar(nomsc)-4),".shp")), delete_layer=T, quiet=T)
      file.copy(file.path(dsnlayerC,"MaxQ_SectControl_Select.qml"),
                file.path(dsnlayerC,paste0(substr(nomsc,1,nchar(nomsc)-4),".qml")))
      
      
      ################################################################################################################
      ################################################################################################################
      # comparaison avec les capteurs de Montpellier
      # recherche si dans nomSC il y a min_ pour dire que c est un evt reel
      # browser()
      if (length(grep(nomsc,pattern="min_"))==1)
      {
        nomXLSX="C:\\Users\\frederic.pons\\Box\\Dossier Personnel de Frederic PONS\\PPRI_MtpCast\\Echanges\\01_Capteurs\\LienPDFCSVSIG_20220103.xlsx"
        
        if (file.exists(nomXLSX)==T)
        {
          my_data <- read_excel(nomXLSX)
          "2014-09-29 04:00"
          date_NomSC=strptime(paste0(substr(nomsc,16,19),
                                     "-",
                                     substr(nomsc,20,21),
                                     "-",
                                     substr(nomsc,22,23),
                                     " ",
                                     substr(nomsc,24,25),
                                     ":",
                                     substr(nomsc,26,27)),
                              "%Y-%m-%d %H:%M")
          
          my_data=my_data[which(is.na(my_data$Nom_SC)==F),]
          my_data$ZeroEchNGF[which(is.na(my_data$ZeroEchNGF))]=0
          duree_NomSC=as.numeric(substr(nomsc,29,30))*60+as.numeric(substr(nomsc,32,33))
          
          my_data$Nom_SC
          # recherche des seection commune avec capteurs
          for (im in 1:dim(my_data)[1])
          {
            SC_Capt=grep(SectControl$ID,pattern=my_data$Nom_SC[im])
            
            if (length(SC_Capt)>0)
            {
              cat(SectControl$ID[SC_Capt],"\n")
              nomCapt=file.path(dirname(nomXLSX),"Mesures",my_data$Nom_CSV[im])
              if (file.exists(nomCapt)==T)
              {
                tabCapt=read.table(file=nomCapt,sep=",",encoding="UTF8")
                Dat=strptime(tabCapt[,4],"%Y-%m-%d %H:%M:%S") 
                
                for (iSC_Capt in SC_Capt)
                {
                  
                  cat(my_data$Nom_SC[im]," ",my_data$Nom_CSV[im],SectControl$ID[iSC_Capt],as.character(date_NomSC),"\n")
                  # browser()
                  TpsAvAp=3
                  cestla=which(Dat>(date_NomSC-TpsAvAp*3600) & Dat<(date_NomSC+duree_NomSC*60+TpsAvAp*3600))
                  
                  jpeg(filename = file.path(dsnlayerC,paste0(SectControl$ID[iSC_Capt],"_",substr(nomsc,1,nchar(nomsc)-4),".jpg")),
                       width = 44.55, height = 31.5, units = "cm", quality = 75, res = 200)
                  # Gestion de la Date avec Nom Evt ou pas
                  # Graphique Débit/Hauteur
                  x1=0.05;x2=0.95;y1=0.525;y2=0.975
                  marge=c(4,4,4,0)
                  par(fig=c(x1,x2,y1,y2),mar=marge)  
                  
                  decalTU=-3600
                  if (VZ==1) 
                  {
                    indiZ=grep(tabChasse[,1],pattern=colnames(DEBIT)[iSC_Capt+1])
                    ylimi=c(min(min(as.numeric(tabChasse[indiZ,"hmoy"])),min(my_data$ZeroEchNGF[im]+tabCapt[cestla,5])),
                            max(max(as.numeric(tabChasse[indiZ,"hmoy"])),max(my_data$ZeroEchNGF[im]+tabCapt[cestla,5])))
                  }else{
                    ylimi=c(min(my_data$ZeroEchNGF[im]+tabCapt[cestla,5]-1),
                            max(my_data$ZeroEchNGF[im]+tabCapt[cestla,5]+1))
                  }
                  
                  plot(Dat[cestla],my_data$ZeroEchNGF[im]+tabCapt[cestla,5],
                       xlim=cbind(date_NomSC-TpsAvAp*3600,date_NomSC+duree_NomSC*60+TpsAvAp*3600),
                       ylim=ylimi,
                       xlab="Temps",
                       ylab="Capteur: Hauteur mesuree (m)",main=paste0(SectControl$ID[iSC_Capt],"_",substr(nomsc,1,nchar(nomsc)-4)))
                  
                  if (VZ==1)
                  {
                    points(date_NomSC+as.numeric(tabChasse[indiZ,3])+decalTU,as.numeric(tabChasse[indiZ,"hmoy"]),typ="b",col='blue')
                  }
                  
                  x1=0.05;x2=0.95;y1=0.025;y2=0.4725
                  marge=c(4,4,4,0)
                  par(fig=c(x1,x2,y1,y2),mar=marge,new=TRUE)  
                  
                  plot(date_NomSC+DEBIT[,1]+decalTU,abs(DEBIT[,iSC_Capt+1]), 
                       xlim=cbind(date_NomSC-TpsAvAp*3600,date_NomSC+duree_NomSC*60+TpsAvAp*3600),
                       xlab="Temps decele de 2h par rapport au TU",
                       ylab="Debits simules (m3/s)")
                  
                  if (VZ==1)
                  {
                    points(date_NomSC+as.numeric(tabChasse[indiZ,3])+decalTU,abs(as.numeric(tabChasse[indiZ,"Debit"])),typ="b",col='blue')
                  }
                  
                  dev.off()
                }
              }
            }
          }
          
        }
        ################################################################################################################
        ################################################################################################################
        
        
        # print(isc)
        # print(Varia[[1]][isc])
        # which(SectControl$ID)
        # vérifier le type
        # Attention, on ne vérifie pas l'ordre... donc ça va planter
        # bug dans le fichier avec les noms des fichiers sources \
        # SOURCE="_SectionsControles\\Synthese_evenements_IPEC.csv"
        # if (file.exists(file.path(dsnlayer,SOURCE))==T)
        # {
        #   HYMEX=read.csv(file.path(dsnlayer,SOURCE),sep=";")
        #   nb=grep(Varia[[1]][isc],HYMEX[,1])
        #   print(nb)
        #   if (length(nb)>0)
        #   {
        #     PD=HYMEX[nb,"Peak.discharge..m3.s."]
        #     PDl=HYMEX[nb,"Peak.discharge.lower.bound..m3.s."]
        #     PDu=HYMEX[nb,"Peak.discharge.upper.bound...m3.s."]
        #     lines(cbind(DEBIT[1,1],max(DEBIT[,1])),cbind(PD,PD)  ,col=couleurs[isc-1],lty=plty[isc-1],lwd=2)
        #     lines(cbind(DEBIT[1,1],max(DEBIT[,1])),cbind(PDl,PDl),col=couleurs[isc-1],lty=plty[isc-1],lwd=0.25)
        #     lines(cbind(DEBIT[1,1],max(DEBIT[,1])),cbind(PDu,PDu),col=couleurs[isc-1],lty=plty[isc-1],lwd=0.25)
        #   }
        # }
      }
    }
    
  }
}

Etape14_fn <- function(){
  tryCatch({          if (ETAPE[14] == 1)
  {
    if (file.exists(dsnlayerC) == T)
    {
      setwd(dsnlayerC)
      
      ListeSC = list.files(dsnlayerC, pattern = "SC.txt")
      print(ListeSC)
      if (length(ListeSC) > 0)
      {
        for (isc in 1:length(ListeSC))
        {
          nomsc = ListeSC[isc]
          ##### interpolation des résultats avec Pputils
          source(
            paste(
              chem_routine,
              "\\C2D\\Cartino2D_Etape14_CompaSC.R",
              sep = ""
            ),
            encoding = "utf-8"
          )
          Etape14_CompaSC(dsnlayer, dsnlayerC, nomsc)
        }
      }
    }
  }}, error = function(e) {skip_to_next <<- TRUE})
  
  if(skip_to_next){
    contours$BUG[nsecteur[ic]] <- "problème étape 14"
    contours$STEP_PRE[nsecteur[ic]] <- -2
  }
}
