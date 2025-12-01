#################################################################################################
################   Etape15_CompaSC_DebitNiveau
#################################################################################################

# Travail sur les contours d'Exzeco pour réaliser les points de contrainte et sur les limites du domaine
Etape15_CompaSC_DebitNiveau=function(dsnlayer,dsnlayerC,nomlayer)
{
  FroudeSeuil=c(0.8,1,1.5)
  result=matrix(-9999,1,17)
  inc=0
  dz=0.25
  
  if (file.exists(file.path(dsnlayerC,nomlayer))==F){print(paste("Fichier introuvable",file.path(dsnlayerC,nomlayer)))}
  
  tab=read.table(file.path(dsnlayerC,nomlayer),header=F,skip=2)
  tab=tab[which(is.na(tab$V1)==F),]
  colnames(tab)=cbind("NOM_Section","IdSect","Temps","Debit","hmin","hmax","hmoy","Charmoy" )
  
  # Suppression des périodes sans calcul
  nbcal=which(tab$hmoy>0) 
  tab=tab[nbcal,]
  
  # Remise en numérique de colonnes
  for (icol in 5:6)
  {
    tab[,icol]=as.numeric(tab[,icol])
  }  
  # browser()
  for (nom in unique(tab$NOM_Section))
  {
    print(nom)
    nb=which(tab$NOM_Section==nom)
    tmp=tab[nb,]
    nom2=strsplit(nom,"_")
    lg=nchar(nom2[[1]][length(nom2[[1]])])
    nom3=substr(nom,1,nchar(nom)-(lg+1))
    Cote=as.numeric(nom2[[1]][length(nom2[[1]])])
    
    titre=paste(nom,substr(nomlayer,1,11))
    
    # profil en travers
    if (file.exists(file.path(dsnlayerC,paste0(nom,"topoSC.xyz")))==F){print(paste("Fichier introuvable",file.path(dsnlayerC,paste0(nom,"topoSC.xyz"))))}
    
    xyz=read.table(file.path(dsnlayerC,paste0(nom,"topoSC.xyz")),header=T)
    nbcote=which(xyz$AmAv>=0)
    if (-max(abs(tmp$Debit))==min(tmp$Debit))
    {
      xyz[,4]=-xyz[,4]
      xyz[,4]=xyz[,4]-min(xyz[,4])
    }
    xyz=xyz[order(xyz[,4]),]
    
    # Débit mis positivement
    tmp$Debit=abs(tmp$Debit)
    
    nsuppour=which(tmp$Debit>0.1*max(tmp$Debit))
    if (is.na(which(tmp$hmoy>Cote)[1])==F)
    {
      nqqch=(which(tmp$hmoy>Cote)[1]-1):dim(tmp)[1]
      # browser()
      nceuxla=unique(nsuppour,nqqch)
      nceuxla=nceuxla[order(nceuxla)]
      if (nceuxla[1]>0)
      {
        tmp=tmp[nceuxla,]
        
        # Enregistrement de l'image
        marge=c(4,4,4,0)
        jpeg(filename = paste0(dsnlayerC,"\\",titre,".jpg"), width = 44.55 , height = 31.5, units = "cm", quality = 75, res = 200)
        
        
        # Graphique Profil Travers
        
        # Limite verticale des graphs
        Zmaxbas =1/10*round(10*min(xyz[,3]))
        Zmaxhaut=max(as.numeric(c(tmp$hmin,tmp$hmax,tmp$hmoy,tmp$Charmoy)))  
        
        x1=0.675
        x2=0.975
        y1=0.325
        y2=0.93
        par(fig=c(x1,x2,y1,y2),mar=marge,new=TRUE)
        
        
        ncroise=which( (xyz[-1,3]>=Cote & xyz[-dim(xyz)[1],3]<=Cote) | (xyz[-1,3]<=Cote & xyz[-dim(xyz)[1],3]>=Cote))
        
        # dZmin=xyz[which(xyz[,3]==min(xyz[,3])),4]
        xyz=xyz[order(xyz[,4]),]
        colnames(xyz)=cbind("X","Y","Z","S","AmAv")
        ncroise=which( (xyz[-1,3]>=Cote & xyz[-dim(xyz)[1],3]<=Cote) | (xyz[-1,3]<=Cote & xyz[-dim(xyz)[1],3]>=Cote))
        
        ds=(xyz[ncroise+1,4]-xyz[ncroise,4])*(Cote-xyz[ncroise,3])/(xyz[ncroise+1,3]-xyz[ncroise,3])
        ncroise=ncroise[which(is.na(ds)==F)]
        ds=ds[which(is.na(ds)==F)]
        xyzcote=cbind(-9999,-9999,Cote,xyz[ncroise,4]+ds,0)
        colnames(xyzcote)=cbind("X","Y","Z","S","AmAv")
        
        # calcul de la largeur au miroir
        # maximum
        Lm_max=max(xyzcote[,"S"])-min(xyzcote[,"S"])
        # Minimum
        nZmin=which(xyz[,3]==min(xyz[,3]))[1]
        dZmin=xyz[nZmin,4]
        
        xyzcote[length(which(xyzcote[,"S"]<=dZmin)),"S"]
        Lm_min=xyzcote[which(xyzcote[,"S"]>=dZmin)[1],"S"]-xyzcote[length(which(xyzcote[,"S"]<=dZmin)),"S"]
        if (length(Lm_min)==0){Lm_min=NA}
        
        # calcul de la surface
        xyzaire=rbind(xyz,xyzcote)
        xyzaire=xyzaire[order(xyzaire[,4]),]
        xyzaire[which(xyzaire[,"Z"]>=Cote),"Z"]=Cote
        
        dsa=xyzaire[-1,4]-xyzaire[-dim(xyzaire)[1],4]
        dza=(Cote-xyzaire[-1,3])+(Cote-xyzaire[-dim(xyzaire)[1],3])
        Aire=sum(dsa*dza)/2
        
        plot(xyz[nbcote,4],xyz[nbcote,3], 
             ylim=c(Zmaxbas,Zmaxhaut),
             xlab="Distance (m)", ylab="Altitude (m NGF)",main="Profil en travers")
        points(xyz[-nbcote,4],xyz[-nbcote,3],pch=2)
        
        points(xyzaire[,4],xyzaire[,3],"l",col="magenta") 
        points(c(min(xyz[,4]-10),max(xyz[,4])+10),c(Cote,Cote),"l",col="blue",lwd=3,lty = 1)
        points(c(min(xyz[,4]-10),max(xyz[,4])+10),c(Cote-dz,Cote-dz),"l",col="blue",lwd=2,lty = 3)
        points(c(min(xyz[,4]-10),max(xyz[,4])+10),c(Cote+dz,Cote+dz),"l",col="blue",lwd=2,lty = 3)
        
        text(xyzcote[which(xyzcote[,"S"]>=dZmin)[1],"S"]-Lm_min/2,
             # xyz[nZmin,3]+0.66*(Cote-xyz[nZmin,3]),
             Cote+1/3*(Zmaxhaut-Cote),
             col="magenta",lwd=10,labels=paste("Lm=",round(Lm_min),"m"),pos=c(3),cex=1)
        
        text(xyzcote[which(xyzcote[,"S"]>=dZmin)[1],"S"]-Lm_min/2,
             # xyz[nZmin,3]+0.33*(Cote-xyz[nZmin,3]),
             Cote+2/3*(Zmaxhaut-Cote),
             col="magenta",lwd=10,labels=paste("Aire=",round(Aire),"m²"),pos=c(3),cex=1)
        
        legend("bottomright", legend=cbind("Points Amont","Points Aval"),
               col = cbind("black","black","black","black","blue"),
               pch = c(1,2))
        
        # points(xyz[c(ncroise,ncroise+1),4],xyz[c(ncroise,ncroise+1),3],col="red",pch=3)
        # points(xyzcote[,4],xyzcote[,3],col="green",pch=3)
        
        # Graphique Débit/Cote d'eau
        x1=0.025
        x2=0.65
        y1=0.325
        y2=0.93
        
        # 
        par(fig=c(x1,x2,y1,y2),mar=marge,new=TRUE)
        
        
        
        plot(tmp$Debit,tmp$hmoy,"l",lwd=3,lty = 1, 
             xlim=c(0,10*round(max(tmp$Debit/10))),
             ylim=c(Zmaxbas,Zmaxhaut),
             xlab="Débit (m3/s)", ylab="Cote d'eau (m NGF)",main="Courbe H(Q)")
        points(tmp$Debit,tmp$hmin,"l",lwd=2,lty = 2) 
        points(tmp$Debit,tmp$hmax,"l",lwd=2,lty = 2) 
        points(tmp$Debit,tmp$Charmoy,"l",lwd=1,lty = 3) 
        points(c(-1000,2*max(tmp$Debit)),c(Cote,Cote),"l",col="blue",lwd=3,lty = 1)
        points(c(-1000,2*max(tmp$Debit)),c(Cote-dz,Cote-dz),"l",col="blue",lwd=2,lty = 3)
        points(c(-1000,2*max(tmp$Debit)),c(Cote+dz,Cote+dz),"l",col="blue",lwd=2,lty = 3)
        
        
        
        # Boucle sur 3 cote
        Zmaxbas
        Zmaxhaut
        deltadessous=Cote-Zmaxbas #Cote-min(tmp$hmoy)
        deltadessus=Zmaxhaut-Cote #max(tmp$hmoy)-Cote
        nbhmoy=which((tmp[-1,]$hmoy>=Cote & tmp[-dim(tmp)[1],]$hmoy<=Cote) | (tmp[-1,]$hmoy<=Cote & tmp[-dim(tmp)[1],]$hmoy>=Cote))
        
        if (length(nbhmoy)==1) {
          nbhmoy=c(nbhmoy,nbhmoy)
        }
        if (length(nbhmoy)>1)
        {
          Debhmoy=as.numeric(c(min(tmp[nbhmoy,]$Debit),max(tmp[nbhmoy,]$Debit)))
          Debhmoy[3]=mean(Debhmoy)
          
          # Calcul de la vitesse à partir de la charge dH=V²/2g
          # tmp[,]$V=(2*9.81*(tmp[,]$Charmoy-tmp[,]$hmoy))^0.5
          # browser()
          Vitesse=0.01*round(100*Debhmoy[2]/Aire)
          Froude=0.01*round(100*(Debhmoy[2]^2*Lm_min/(9.81*Aire^3))^0.5)
          
          if (is.na(Lm_min)){Froude=NA}
          points(c(Debhmoy[1],Debhmoy[1],Debhmoy[2],Debhmoy[2]),c(Cote+3/4*deltadessus,Cote,Cote,Cote-1/2*deltadessous),"l",lwd=3,lty = 3,col="magenta")
          text(Debhmoy[1],tmp[nbhmoy[1],]$hmoy+c(3/4*deltadessus)  ,col="magenta",lwd=10,labels=round(Debhmoy[1]),pos=c(3),cex=1.5)
          text(Debhmoy[2],tmp[nbhmoy[2],]$hmoy+c(-1/2*deltadessous),col="magenta",lwd=10,labels=paste(round(Debhmoy[2]),"m3/s"),pos=c(1),cex=2)
          
          nbhmin=which((tmp[-1,]$hmin>=Cote+dz & tmp[-dim(tmp)[1],]$hmin<=Cote+dz) | (tmp[-1,]$hmin<=Cote+dz & tmp[-dim(tmp)[1],]$hmin>=Cote+dz))
          Debhmin=c(min(tmp[nbhmin,]$Debit),max(tmp[nbhmin,]$Debit))
          nbhmax=which((tmp[-1,]$hmax>=Cote-dz & tmp[-dim(tmp)[1],]$hmax<=Cote-dz) | (tmp[-1,]$hmax<=Cote-dz & tmp[-dim(tmp)[1],]$hmax>=Cote-dz))
          Debhmax=c(min(tmp[nbhmax,]$Debit),max(tmp[nbhmax,]$Debit))
          nbCharmoy=which((tmp[-1,]$Charmoy>=Cote-dz & tmp[-dim(tmp)[1],]$Charmoy<=Cote-dz) | (tmp[-1,]$Charmoy<=Cote-dz & tmp[-dim(tmp)[1],]$Charmoy>=Cote-dz))
          DebCharmoy=c(min(tmp[nbCharmoy,]$Debit),max(tmp[nbCharmoy,]$Debit))
          
          posxyh=c(min(Debhmax),max(Debhmin))
          posxyC=min(DebCharmoy)
          points(posxyh[c(1,1)],c(Cote+1/2*deltadessus,Cote-dz),"l",lwd=3,lty = 3,col="magenta")
          points(posxyh[c(2,2)],c(Cote+dz,Cote-1/3*deltadessous),"l",lwd=3,lty = 3,col="magenta")
          text(posxyh[1],Cote+1/2*deltadessus,col="magenta",lwd=10,labels=round(posxyh[1]),pos=c(2),cex=1)
          text(posxyh[2],Cote-1/3*deltadessous,col="magenta",lwd=10,labels=round(posxyh[2]),pos=c(4),cex=1)
          
          points(posxyC[c(1,1)],c(Cote+1/3*deltadessus,Cote-dz),"l",lwd=2,lty = 3,col="magenta")
          text(posxyC[1],Cote+1/3*deltadessus,col="magenta",lwd=10,labels=round(posxyC[1]),pos=c(2),cex=0.7)
          Strickler=substr(nomlayer,10,11)
          
          aj=cbind(paste0(nom,"_S",Strickler),substr(nomlayer,1,11),substr(nomlayer,1,8),Strickler,nom3,Cote,
                   Debhmoy[1],Debhmoy[2],Debhmoy[3],posxyh[1],posxyh[2],posxyC,Aire,Lm_min,Lm_max,Vitesse,Froude)
          colnames(aj)=cbind("Id","NomCalcul","NomSecteur","Strickler","Section","CoteRef",
                             "DebitminCmoy","DebitmaxCmoy","DebitCmoy","DebitminCminmax","DebitmaxCminmax","DebitminChar","Aire","Lm_min","Lm_max","Vitesse","Froude")
          
          if (inc==0)
          {result=aj
          inc=inc+1}
          else{
            result=rbind(result,aj)
          }
          max(Debhmin)
        }else{
          Debhmoy=c(-9999,-9999) #"BUG: Pas assez de debit ou PHE trop haute"
          Vitesse=-9999
          Froude=-9999
        }
        # Position des froude
        # browser()
        
        FrPos=(9.81*Aire^3/Lm_min)^0.5*FroudeSeuil
        coulFr=cbind("yellow","orange","red")
        for (iFr in 1:length(FrPos))
        {
          points(c(FrPos[iFr],FrPos[iFr]),c(0,2*Zmaxhaut),"l",col=coulFr[iFr],lwd=3,lty = 3)
          text(FrPos[iFr],Zmaxhaut,col=coulFr[iFr],lwd=10,labels=paste("Fr",FroudeSeuil[iFr]),pos=c(4),cex=1)
          # points(c(-1000,2*max(tmp$Debit)),c(Cote-dz,Cote-dz),"l",col="blue",lwd=2,lty = 3)
          # points(c(-1000,2*max(tmp$Debit)),c(Cote+dz,Cote+dz),"l",col="blue",lwd=2,lty = 3)
          
          # plot(,"l",lwd=2,lty = 3,col="magenta"))
        }
        
        legend("bottomright", legend=cbind("Cote d'eau Moyenne","Cote d'eau Minimum","Cote d'eau Maximum","Moyenne de la Charge","Cote estimée ou relevée sur la section",paste0("Incertitude de ",dz,"m sur la Cote estimée")),
               col = cbind("black","black","black","black","blue","blue"),
               lty = c(1,2,2,3,1,3),
               lwd = c(3,2,2,1,3,2))
        
        # # Graphique Temps débit
        x1=0.525
        x2=0.975
        y1=0.025
        y2=0.3   
        par(fig=c(x1,x2,y1,y2),mar=marge,new=TRUE)
        plot(tmp$Temps,tmp$Debit,
             xlim=c(0,max(tmp$Temps)),
             ylim=c(0,max(tmp$Debit)),
             "l",lwd=3,lty = 1, xlab="Temps Numérique (s)", ylab="Débit (m3/s)",main="Hydrogramme")
        
        legend("bottom", legend=cbind("Débit"),
               col = cbind("black"),
               lty = 1,
               lwd=3)
        
        # Graphique Temps Cote d'eau
        x1=0.025
        x2=0.475
        y1=0.025
        y2=0.3
        
        par(fig=c(x1,x2,y1,y2),mar=marge,new=TRUE)
        plot(tmp$Temps,tmp$hmoy,
             xlim=c(0,max(tmp$Temps)),
             ylim=c(Zmaxbas,Zmaxhaut),
             "l",lwd=3,lty = 1, xlab="Temps Numérique (s)", ylab="Cote d'eau (m NGF)",main="Liminigramme")
        points(tmp$Temps,tmp$hmin,"l",lwd=2,lty = 2)
        points(tmp$Temps,tmp$hmax,"l",lwd=2,lty = 2)
        points(tmp$Temps,tmp$Charmoy,"l",lwd=1,lty = 3)
        points(c(-1000,2*max(tmp$Temps)),c(Cote,Cote),"l",col="blue",lwd=3,lty = 1)
        points(c(-1000,2*max(tmp$Temps)),c(Cote-dz,Cote-dz),"l",col="blue",lwd=2,lty = 3)
        points(c(-1000,2*max(tmp$Temps)),c(Cote+dz,Cote+dz),"l",col="blue",lwd=2,lty = 3)
        
        # legend("bottom", legend=cbind("Cote d'eau Moyenne","Cote d'eau Minimum","Cote d'eau Maximum","Moyenne de la Charge","Cote estimée ou relevée sur la section"),
        #        col = cbind("black","black","black","black","blue"),
        #        lty = c(1,2,2,3,1),
        #        lwd=cbind(3,2,2,1,3))
        
        # Titre de la fiche
        x1=0
        x2=1
        y1=0.93
        y2=1
        
        par(fig=c(x1,x2,y1,y2),mar=cbind(0,0,0,0),new=TRUE)
        plot(1:2, type='n',xaxt = "n",yaxt = "n", xlab=NULL,  ylab=NULL,fg="white")
        text(1.5,1,labels=paste("Section",nom,"-",
                                "Strickler",substr(titre,nchar(titre)-1,nchar(titre)),"-",
                                "Débit",round(Debhmoy[2]),"m3/s -",
                                "Vitesse moy",Vitesse,"m/s -",
                                "Froude moy",Froude),pos=3,cex=2,col="black")
        text(2,1.8,labels=paste('édité le',format(Sys.Date(), "%d/%m/%Y")),pos=2,cex=1,col="gray")
        dev.off()
      }
    }
  }
  return(result)
}

#################################################################################################
################   Etape15_Laurent
#################################################################################################

# Travail sur les contours d'Exzeco pour réaliser les points de contrainte et sur les limites du domaine
Etape15_LB=function(dsnlayer,Valeur)
{
  # couleur=rbind(cbind("#000000", "#434343", "#666666", "#999999", "#b7b7b7", "#cccccc", "#d9d9d9", "#efefef", "#f3f3f3", "#ffffff"),
  #               cbind("#980000", "#ff0000", "#ff9900", "#ffff00", "#00ff00", "#00ffff", "#4a86e8", "#0000ff", "#9900ff", "#ff00ff"), 
  #               cbind("#e6b8af", "#f4cccc", "#fce5cd", "#fff2cc", "#d9ead3", "#d0e0e3", "#c9daf8", "#cfe2f3", "#d9d2e9", "#ead1dc"), 
  #               cbind("#dd7e6b", "#ea9999", "#f9cb9c", "#ffe599", "#b6d7a8", "#a2c4c9", "#a4c2f4", "#9fc5e8", "#b4a7d6", "#d5a6bd"), 
  #               cbind("#cc4125", "#e06666", "#f6b26b", "#ffd966", "#93c47d", "#76a5af", "#6d9eeb", "#6fa8dc", "#8e7cc3", "#c27ba0"), 
  #               cbind("#a61c00", "#cc0000", "#e69138", "#f1c232", "#6aa84f", "#45818e", "#3c78d8", "#3d85c6", "#674ea7", "#a64d79"), 
  #               cbind("#85200c", "#990000", "#b45f06", "#bf9000", "#38761d", "#134f5c", "#1155cc", "#0b5394", "#351c75", "#741b47"), 
  #               cbind("#5b0f00", "#660000", "#783f04", "#7f6000", "#274e13", "#0c343d", "#1c4587", "#073763", "#20124d", "#4c1130"))
  
  print(Valeur)
  # browser()
  nbsupp=which(Valeur[,1]==-9999)
  if (length(nbsupp>0)) {Valeur=Valeur[-nbsupp,]}
  Valeur=Valeur[order(Valeur[,"Id"]),]
  affiche=cbind("DebitminCmoy","DebitmaxCmoy","DebitminCminmax","DebitmaxCminmax","DebitminChar","DebitCmoy","Vitesse","Froude")
  
  for (Id in unique(Valeur[,"Section"]))
  {
    tmp=Valeur[which(Valeur[,"Section"]==Id),]
    if (length(unique(tmp[,"Strickler"]))>1)
    {
      if (is.null(dim(tmp))==F)
      {  
        # print(tmp[,"CoteRef"])
        
        couleurs=cbind("#980000","#ff9900","#00ff00", "#4a86e8", "#9900ff","#ff0000","#0000ff",#ff00ff,"#00ffff","#ffff00,
                       "#ea9999",  "#f6b26b",  "#f1c232","#38761d", "#0c343d","#1155cc","#3d85c6","#8e7cc3","#d5a6bd",  "#5b0f00", "#990000",
                       "#b6d7a8","#d0e0e3",  "#a4c2f4","#351c75","#4c1130","#000000")
        
        # print(tmp)     
        # 
        nchoix=unique(tmp[,"CoteRef"])
        lg=length(nchoix)
        if (ETAPE[16]==1)
        {
          nchoix=select.list(nchoix,preselect =nchoix,title = Id,multiple=T, graphics = T)
          print(nchoix)
        }
        #
        
        nlala=which(unique(tmp[,"CoteRef"]) %in% nchoix)
        
        couleurs=couleurs[nlala]
        
        exten=ifelse(lg==length(nchoix),Id,paste(Id,"selection"))
        
        # Enregistrement de l'image
        jpeg(filename = paste0(dsnlayer,"\\",exten,".jpg"), width =  44.55, height =31.5, units = "cm", quality = 75, res = 200)
        write.table(tmp,paste0(dsnlayer,"\\",exten,".txt"),row.names = F)
        
        nvar=rbind(c(1,5),c(6,6),c(7,7),c(8,8))
        for (ipar in 1:4)
        {
          # Graphique Débit/Cote d'eau
          switch
          if (ipar==1){x1=0.025;x2=0.475;y1=0.025;y2=0.475}
          if (ipar==2){x1=0.025;x2=0.475;y1=0.500;y2=0.95}
          if (ipar==3){x1=0.525;x2=0.975;y1=0.025;y2=0.475}
          if (ipar==4){x1=0.525;x2=0.975;y1=0.500;y2=0.95}
          marge=c(4,4,4,0)
          # 
          par(fig=c(x1,x2,y1,y2),mar=marge,new=TRUE)
          
          # tmp[,"Strickler"]=as.numeric(tmp[,"Strickler"])
          
          incg=0
          incoul=1
          tmp_Med=list()
          iMed=0
          for (IdR in nchoix)#unique(tmp[,"CoteRef"]))
          {
            tmp_=tmp[which(tmp[,"CoteRef"]==IdR),]
            
            # Récup de tableau complet avec la sélection pour calcul médiane
            iMed=iMed+1
            tmp_Med[[iMed]]=tmp_
            
            # print(tmp_)
            # DebitCmoy=(as.numeric(tmp_[,"DebitminCmoy"])+as.numeric(tmp_[,"DebitmaxCmoy"]))/2
            # tmp_=cbind(tmp_,DebitCmoy)
            
            clwd=c(3,3,2,2,1,3,3,3)
            clty=c(1,1,2,2,3,1,1,1)
            for (ig in nvar[ipar,1]:nvar[ipar,2])
            {
              print("Si ca plante, c'est que vous avez le résultat que pour 1 strickler pour un ensemble de section de controle")
              print(paste(IdR, ig))
              # browser()
              if (is.null(dim(tmp_))==F)
              { 
                nselec=which((tmp_[,affiche[ig]]!="Inf")==T)
                if (length(nselec)>0)
                {
                  if (incg==0)
                  {
                    limx=as.numeric(c(min(tmp_[nselec,"Strickler"]),max(tmp_[nselec,"Strickler"])))+c(0,+5)
                    if (ipar<=2)
                    {
                      if (length(which((tmp[,"DebitmaxCminmax"]!="Inf")==T))>0)
                      {
                        limy=c(0,as.numeric(max(as.numeric(tmp[which((tmp[,"DebitmaxCminmax"]!="Inf")==T),"DebitmaxCminmax"]))))
                      }else{
                        limy=c(0,1.5*max(as.numeric(tmp[,"DebitCmoy"])))
                      }
                      titord="Débit (m3/s)"
                      titmain="Débit (Strickler)"
                      if (ipar==1){titmain="Débit et ses incertitudes (Strickler)"}
                    }
                    if (ipar==3)
                    {
                      limy=c(0,0.5+0.2*as.numeric(max(5*as.numeric(tmp[which((tmp[,affiche[ig]]!="Inf")==T),affiche[ig]]))))
                      # round(max(5*as.numeric(tmp_[nselec,affiche[ig]]))))
                      titord="Vitesse (m/s)"
                      titmain="Vitesse (Strickler)"
                    }
                    if (ipar==4)
                    {
                      limy=c(0,0.1+0.1*as.numeric(max(10*as.numeric(tmp[which((tmp[,affiche[ig]]!="Inf")==T),affiche[ig]]))))
                      # limy=c(0,0.1+0.1*round(max(10*as.numeric(tmp_[nselec,affiche[ig]]))))
                      # browser()
                      titord="Froude"
                      titmain="Froude (Strickler)"
                    }
                    plot(tmp_[nselec,"Strickler"],tmp_[nselec,affiche[ig]],"l",lwd=clwd[ig],lty = clty[ig],col=couleurs[incoul],
                         xlim=limx,
                         ylim=limy,
                         xlab="Strickler", ylab=titord,main=titmain)#Id
                    incg=incg+1
                  }else{
                    points(tmp_[nselec,"Strickler"],tmp_[nselec,affiche[ig]],"l",lwd=clwd[ig], lty = clty[ig],col=couleurs[incoul])
                    
                  }
                  
                }
              }
            }
            incoul=incoul+1
          }
          coulleg=cbind(couleurs[1:(incoul-1)],"gray","gray","gray")
          # coulleg=couleurs
          CoteMult=nchoix
          CoteMult=matrix(CoteMult,1,length(CoteMult))
          
          # legende=nchoix
          cltyf=c(matrix(1,incoul-1,1),1,2,3)
          clwdf=c(matrix(3,incoul-1,1),3,2,1)
          
          if (ipar==1){legende=cbind(CoteMult,"Cote Moyenne","Incertitude Cote","Charge")}
          if (ipar==2){legende=cbind(CoteMult)}
          if (ipar==3){legende=cbind(CoteMult)}
          if (ipar==4){legende=cbind(CoteMult)}
          
          
          legend("bottomright",
                 legend=legende,
                 col = coulleg,
                 lty = cltyf,
                 lwd = clwdf)
          
          if (ipar==2)
          {
            
            FroudeSeuil=c(0.8,1,1.5)
            coulFr=cbind("yellow","orange","red")
            
            for (iFr in 1:length(FroudeSeuil))
            {
              nbF=which(tmp[,"Froude"]>FroudeSeuil[iFr])
              if (length(nbF)>0)
              {
                points(tmp[nbF,"Strickler"],tmp[nbF,"DebitCmoy"],col="black",pch=19,lwd=5)
                points(tmp[nbF,"Strickler"],tmp[nbF,"DebitCmoy"],col=coulFr[iFr],pch=19,lwd=3)
              }
            }
            legend("topleft",
                   legend=paste("Froude >",FroudeSeuil),
                   col = coulFr,
                   pch=19)
          }
          
          # Calcul de la médiane
          if (ipar==2)
          {
            tmp_MedF=do.call(rbind, tmp_Med)
            
            Mediane=matrix(NA,nrow =length(unique(tmp_MedF[,"Strickler"])),2 )
            indStr=0
            for (iStr in unique(tmp_MedF[,"Strickler"]))
            {
              indStr=indStr+1
              Mediane[indStr,1:2]=c(as.numeric(iStr),median(as.numeric(tmp_MedF[which(tmp_MedF[,"Strickler"]==iStr),"DebitCmoy"])))
            }
            # points(Mediane[,1],Mediane[,2],"l",lwd=10, lty = 1,col="black")
            points(Mediane[,1],Mediane[,2],"p",pch=15,cex=2,col="black")#,pch=1,cex=1)
            text(Mediane[,1],0,labels=round(Mediane[,2]),pos=3,cex=1.1,col="black")
            
            legend("topright",
                   legend="Médiane",
                   col = "black",
                   pch=15)
          }
        }
        
        x1=0
        x2=1
        y1=0.93
        y2=1
        
        par(fig=c(x1,x2,y1,y2),mar=cbind(0,0,0,0),new=TRUE)
        plot(1:2, type='n',xaxt = "n",yaxt = "n", xlab=NULL,  ylab=NULL,fg="white")
        text(1.5,1,labels=exten,pos=3,cex=2,col="black")
        text(2,1.8,labels=paste('édité le',format(Sys.Date(), "%d/%m/%Y")),pos=2,cex=1,col="gray")
        dev.off()
        # browser()
      }
    }
  }
}



Etape15_fn <- function(){
  
  
  tryCatch({          if (ETAPE[15] == 1)
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
              "\\C2D\\Cartino2D_Etape15_SC_DebitNiveau.R",
              sep = ""
            ),
            encoding = "utf-8"
          )
          result = Etape15_CompaSC_DebitNiveau(dsnlayer, dsnlayerC, nomsc)
          
          print(result)
          
          if (inc15 == 0)
          {
            Valeur = result
          } else{
            Valeur = rbind(Valeur, result)
          }
          inc15 = inc15 + 1
        }
      }
    }
  }}, error = function(e) {skip_to_next <<- TRUE})
  
  if(skip_to_next){
    contours$BUG[nsecteur[ic]] <- "problème étape 15"
    contours$STEP_PRE[nsecteur[ic]] <- -2
  }
}