#################################################################################################
################   Etape4_Cas_Hyeto
#################################################################################################

Etape4_Cas_Hyeto=function(dsnlayerC,nom_Cas,dsnPluie,contour)
{
  NomUnivar=file.path(dsnlayerC,"univar.txt")
  if (file.exists(NomUnivar)==T){
    COTE_AVAL=max(contour$COTEAVALM,contour$COTEAVALAJ+as.numeric(scan(file=NomUnivar,NomUnivar,sep=":",skip=6,nlines=1,dec="."))[2])
  }else{
    COTE_AVAL=contour$COTEAVALM
  }

  Lignes=readLines(con=file.path(dsnlayerC,nom_Cas))
  
  TelFred=file.path(dsnlayerC,"TelemacFred.txt")
  file.create(TelFred)
  TelElMa=file.path(dsnlayerC,"TelemacElMa.txt")
  file.create(TelElMa)
  Tel_ParaPC=file.path(dsnlayerC,"TelemacFred_ParalPC.txt")
  file.create(Tel_ParaPC)
  Tel_ParaREM=file.path(dsnlayerC,"TelemacFred_ParalREM.txt")
  file.create(Tel_ParaREM)
  
  # Passage v8p2
  write(as.character(paste0("cd ",dsnlayerC)),file=TelFred, append=T)
  write(as.character(paste0("cd ",dsnlayerC)),file=TelElMa, append=T)
  write(as.character(paste0("cd ",dsnlayerC)),file=Tel_ParaPC, append=T)
  write(as.character(paste0("cd ",basename(dsnlayerC))),file=Tel_ParaREM, append=T)
  
  motcleSHY=paste0("hyeto_",as.character(contour$NOM),"_PM")
  Type="SHYREG"
  
  if (file.exists(file.path(dsnPluie,contour$NOMPOST))==T)
  {
    dsnPluie_tmp=file.path(dsnPluie,contour$NOMPOST)
  }else{
    dsnPluie_tmp=dsnPluie
  }
  
  listeSHY=data.frame(list.files(dsnPluie_tmp, pattern = motcleSHY))
  
  if (length(listeSHY)==0){
    nshy=0
    print("pas de hyeto SHYREG")
  }else{
    nshy=dim(listeSHY)[1]
    colnames(listeSHY)="Nom"
  }
  
  # ajout nassim 25/07/2024
  if (file.exists(file.path(dsnPluie,contour$NOMPOST, contour$NOM))==T)
  {
    dsnPluie_tmp_SHYS=file.path(dsnPluie,contour$NOMPOST, contour$NOM)
  } else if (file.exists(file.path(dsnPluie,contour$NOM))==T){
    dsnPluie_tmp_SHYS=file.path(dsnPluie,contour$NOM)
  } else {
    dsnPluie_tmp_SHYS=dsnPluie_tmp
  }
  
  # if (file.exists(file.path(dsnPluie,contour$NOMPOST, contour$NOM))==T)
  # {
  #   dsnPluie_tmp_SHYS=file.path(dsnPluie,contour$NOMPOST, contour$NOM)
  
  
  Type="SHYS"
  motcleSHYS=paste0("^", "shyreg_spat_", ".*\\.txt$","|","^", "shyreg_spPB_", ".*\\.txt$","|","^", "shyreg_spPN_", ".*\\.txt$")
  motcleSHYS=paste0("^", "shyreg_spPB_", ".*\\.txt$","|","^", "shyreg_spPN_", ".*\\.txt$")
  listeSHYS=data.frame(Nom=list.files(dsnPluie_tmp_SHYS, pattern = motcleSHYS))
  # Old Nassim
  # motcleSHYS=paste0("shyreg_spat_")
  # listeSHYS=data.frame(list.files(dsnPluie_tmp_SHYS, pattern = paste0("^", motcleSHYS, ".*\\.txt$")))
  
  if (length(listeSHYS)==0){  
    print("pas de hyeto SHYREG spatialise")
  }else{
    # colnames(listeSHYS)="Nom"
    nshys=nshy+dim(listeSHYS)[1]
  }
  # }else
  # {
  #   listeSHYS=NULL
  #   nshys=nshy+0
  # }
  
  motcleHist=paste0("hyeto_Hist_")
  Type="HIST"
  
  listeHist=data.frame(list.files(dsnPluie_tmp, pattern = motcleHist))
  if (length(listeHist)==0){  
    print("pas de hyeto Historique")
  }else{
    colnames(listeHist)="Nom"
    nhist=nshys+dim(listeHist)[1]
  }
  
  motcleEvt=paste0("hyeto_",as.character(contour$NOM),"_Evt")
  Type="Evt"
  listeEvt=data.frame(list.files(dsnPluie_tmp, pattern = motcleEvt))
  if (length(listeEvt)==0){  
    print("pas de hyeto Evenement")
  }else{
    colnames(listeEvt)="Nom"
    nEvt=nhist+dim(listeEvt)[1]
  }

  motcleEvts=paste0("spatial_",as.character(contour$NOM),"_Evt")
  Type="Evts"
  listeEvts=data.frame(list.files(dsnPluie_tmp, pattern = motcleEvts))
  if (length(listeEvts)==0){  
    print("pas de hyeto spatialisé Evenement")
  }else{
    colnames(listeEvts)="Nom"
    nEvts=nEvt+dim(listeEvts)[1]
  }
  
  liste=rbind(listeSHY,listeSHYS,listeHist,listeEvt,listeEvts)
  if (nshy>0)    {Type="SHYREG" ;liste[1:nshy         ,2]=Type}
  if (nshys>nshy){Type="SHYS" ;liste[(nshy+1):nshys     ,2]=Type}
  if (nhist>nshys){Type="HIST"  ;liste[(nshys+1) :nhist,2]=Type}
  if (nEvt>nhist){Type="EVT"   ;liste[(nhist+1):nEvt ,2]=Type}
  if (nEvts>nEvt){Type="EVTS"   ;liste[(nEvt+1):nEvts ,2]=Type}
  #liste=listeSHY
  # Gestion de 2 types de pluvio, soit shyreg soit historqie
  # Si shyreg assez simple on garde le début et fin imposée par utlisatuer
  # Si historique, on ne regarde que la durée, on récupère la période avec le plus fort cumul et on rajoute 1/2 période ensuite
  
  if (dim(liste)[1]>0){
    for (ih in dim(liste)[1]:1){
      Lignes=readLines(con=file.path(dsnlayerC,nom_Cas))
      # la gestion de AMC est juste pour les évèneemnts, ce serait à faire dans shyreg!!!
      
      AMC=2
      NOMSLF=paste0(nom_Telemac,".slf")   
      print(ih)
      Nom_Ent=as.character(liste[ih,1])
      Type=liste[ih,2]
      
      if(Type=="SHYS"){
        dsnPluie_tmp = dsnPluie_tmp_SHYS
      } else {
        if (file.exists(file.path(dsnPluie,contour$NOMPOST))==T)
        {
          dsnPluie_tmp=file.path(dsnPluie,contour$NOMPOST)
        }else{
          dsnPluie_tmp=dsnPluie
        }
      }
      
      # if(!Pluie_spatial){
      if(Type!="EVTS" & Type!="SHYS"){
        Pluie=scan(file.path(dsnPluie_tmp,Nom_Ent),skip=2)
        Pluie=data.frame(t(matrix(Pluie,2,length(Pluie)/2)))
        PluieBase=Pluie
        
        Plui=sapply(1:dim(Pluie)[1], 
                    function(x) {sum(Pluie[1:x,2])})
      }
      
      DUREE=contour$DURATION
      
      if (Type=="HIST"){ #if (ih>nshy)
        IciouLa=gregexpr("_AMC", liste$Nom[ih])
        indi_amc=IciouLa[[1]][1]
        ValAMC=substr(liste$Nom[ih],indi_amc+4,indi_amc+4)
        if (is.na(as.numeric(ValAMC))==F)
        {AMC=ValAMC}
        
        x=1:dim(Pluie)[1]
        avg=function(x) {sum(Pluie[which(Pluie[,1]-Pluie[x,1]>=0 & Pluie[,1]-Pluie[x,1]<7200),2])}
        MoyGlissante=sapply(x, 
                            function(x) {sum(Pluie[which(Pluie[,1]-Pluie[x,1]>=0 & Pluie[,1]-Pluie[x,1]<7200),2])})
        
        ndebhist=which(Pluie[,1]>=0)
        DebutT=Pluie[ndebhist[1],1]/3600
        
        # fait de manière provisoire
        # On rajoute 1/2h avant le début de la crue et la durée demandé -1h après...
        # il faut que ce soit divisible par 300...
        # DUREE=0.5+contour$DURATION+(contour$DURATION-1)
        DUREE=contour$DURATION
        raci=paste0("hyeto_",as.character(contour$NOM),'_',substr(Nom_Ent,12,nchar(Nom_Ent)-4),"_")
        
        # if (ih==7) ##### DANGEREUX QUE TPM
        # {
        #   DebutT=5
        #   DUREE=contour$DURATION
        # }
      }
      
      if (Type=="SHYREG"){
        DebutT=contour$TEMPS_DEBH
        DUREE=round(12*contour$DURATION)/12
        raci=paste0(substr(Nom_Ent,1,nchar(Nom_Ent)-4),"_")
      }
      
      
      if (Type=="EVT" ){
        name_split <- strsplit(Nom_Ent,"_")[[1]]
        
        DebutT=0
        DUREE=as.numeric(substr(name_split[6],1,2))+as.numeric(substr(name_split[6],4,5))/60
        
        aire=st_area(contour)
        
        # On rajoute du temps en fonction de la taille du Bv
        # si V=1.5m/s, en 1h sur un rectangle longuer 2 largeur 1
        DUREE=round(12*(DUREE+as.numeric(as.character(aire))/(3600*1800)))/12
        
        nb=which(Pluie[,1]<=3*(DUREE*3600))
        Pluie=Pluie[nb,]
        PluieBase=PluieBase[nb,]
        Plui=Plui[nb]
        
        nom_exp=substr(Nom_Ent,1,nchar(Nom_Ent)-4)
        AMC <- gsub('[^0-9]','',name_split[7])
        
      }else{
        if(Type!="EVTS" & Type!="SHYS"){
          DUREE=max(DUREE,2)
          DebutHour=floor(DebutT)
          DebutMin=round((DebutT-DebutHour)*60)
          FinHour=floor(DebutT+DUREE)
          FinMin=round((DebutT+DUREE-FinHour)*60)
          nom_exp=paste0(raci,
                         formatC(DebutHour ,width=2, flag="0"),"h_",
                         formatC(DebutMin  ,width=2, flag="0"),"m_",
                         formatC(FinHour   ,width=2, flag="0"),"h_",
                         formatC(FinMin    ,width=2, flag="0"),"m") 
          
          #----- Modification de la pluie d'entrée uniquement pour du stat ou de l'historique
          if (contour$CN_HYETO==1){
            CN_Telemac=as.numeric(contour$CN)
            Rm=(25400/CN_Telemac-254)
            Hru=ifelse((Plui>0.2*Rm),(Plui-0.2*Rm)^2/(Plui+0.8*Rm),0)
            plot(Plui)
            lines(Hru)
            PluieNette=sapply(1:(dim(Pluie)[1]-1), 
                              function(x) {Hru[x+1]-Hru[x]})
            Pluie[2:(dim(Pluie)[1]),2]=PluieNette
            
            # Il faut que l'on modifie aussi le lien vers le fichier slf'
            NOMSLF=paste0(nom_Telemac,"_CN100.slf")
          }
        }
      }
      
      if(Type=="EVTS"){
        
        name_split <- strsplit(Nom_Ent,"_")[[1]]
        
        DebutT=0
        DUREE=as.numeric(substr(name_split[6],1,2))+as.numeric(substr(name_split[6],4,5))/60
        
        aire=st_area(contour)
        # On rajoute du temps en fonction de la taille du Bv
        # si V=1.5m/s, en 1h sur un rectangle longuer 2 largeur 1
        DUREE=round(12*(DUREE+as.numeric(as.character(aire))/(3600*1800)))/12
        DUREE=max(DUREE,2)
        DebutHour=floor(DebutT)
        DebutMin=round((DebutT-DebutHour)*60)
        FinHour=floor(DebutT+DUREE)
        FinMin=round((DebutT+DUREE-FinHour)*60)
        
        raci <- tools::file_path_sans_ext(Nom_Ent)
        
        nom_exp=substr(Nom_Ent,1,nchar(Nom_Ent)-4)
        hyeto_nom=paste0(nom_exp,".txt")
        hyeto_name=file.path(dsnlayerC,hyeto_nom)
        file.copy(file.path(dsnPluie_tmp,Nom_Ent),hyeto_name,overwrite = TRUE)
        
        # DUREE <- contour$DURATION
        nom_exp=substr(Nom_Ent,1,nchar(Nom_Ent)-4)
        
        # AMC=substr(Nom_Ent,47,47)
        AMC <- gsub('[^0-9]','',name_split[7])
      }
      
      if(Type=="SHYS"){
        name_split <- strsplit(Nom_Ent,"_")[[1]]
        
        DebutT=0
        DUREE=as.numeric(substr(name_split[7],2,3))
        
        aire=st_area(contour)
        # On rajoute du temps en fonction de la taille du Bv
        # si V=1.5m/s, en 1h sur un rectangle longuer 2 largeur 1
        DUREE=round(12*(DUREE+as.numeric(as.character(aire))/(3600*1800)))/12
        DUREE=max(DUREE,2)
        
        ### On remet cela car pas bon au dessus
        DUREE=contour$DURATION
        
        Debut=floor(DebutT)
        Fin=floor(DebutT+DUREE)
        
        raci <- tools::file_path_sans_ext(Nom_Ent)
        
        nom_exp=substr(Nom_Ent,1,nchar(Nom_Ent)-4)
        hyeto_nom=paste0(nom_exp,".txt")
        hyeto_name=file.path(dsnlayerC,hyeto_nom)
        file.copy(file.path(dsnPluie_tmp,Nom_Ent),hyeto_name,overwrite = TRUE)
        
        # DUREE <- contour$DURATION
        nom_exp=substr(Nom_Ent,1,nchar(Nom_Ent)-4)
        
        # AMC=substr(Nom_Ent,47,47)
      }
      
      # -----------------------Graphiques ----------------------------------------------------------------------------------------------        
      if(Type!="EVTS" & Type!="SHYS"){       
        jpeg(filename = file.path(dsnlayerC,paste0(nom_exp,".jpg")),
             width = 31.5, height = 44.55, units = "cm", quality = 75, res = 200)
        x1=0.025; x2=0.975 ;y1=0.75; y2=0.99
        par(fig=c(x1,x2,y1,y2),new=TRUE)
        
        if (Type=="SHYREG"){plot(PluieBase,main = paste0(nom_exp,' - Pluie extraite de SHYREG'),
                                 xlab="Temps (s)",ylab="Cumul sur le pas de temps (mm)",
                                 col="blue",typ="s")}
        if (Type=="HIST"){plot(PluieBase,main = paste0(nom_exp,' - Scenario ou Pluie historique'),
                               xlab="Temps (s)",ylab="Cumul sur le pas de temps (mm)",
                               col="blue",typ="s")}
        if (Type=="EVT"){plot(PluieBase,main = paste0(nom_exp,' - Evenement Bassin'),
                              xlab="Temps (s)",ylab="Cumul sur le pas de temps (mm)",
                              col="blue",typ="s")}
        
        lines(cbind(3600*DebutT,3600*DebutT),cbind(-1,100))
        lines(cbind(3600*(DebutT+DUREE),3600*(DebutT+DUREE)),cbind(-1,100))
        DUREEtext=paste0(formatC(round(round(12*DUREE)/12),width=2, flag="0"),"h",
                         formatC(round((round(12*DUREE)/12-floor(round(12*DUREE)/12))*60),width=2, flag="0"),"min")
        text(cbind(3600*(DebutT+DUREE),3600*(DebutT+DUREE)),max(PluieBase[,2]/2),DUREEtext,pos=4)
        legend("topright",
               legend = cbind(paste0("Selection pour le calcul Telemac: ",DUREEtext)),
               col=cbind("black"),
               lty=cbind(1),
               pch==cbind(1))
        
        if (Type=="EVT"){
          nb=1:dim(Pluie)[1]
        }else{
          # nb=which(((Pluie[,1]/3600)>=DebutT) & ((Pluie[,1]/3600)<=(DebutT+DUREE)))
          # carte socle, ? sur utilité de la ligne précédente
          nb=1:dim(Pluie)[1]
        }   
        PluieTelemac=Pluie[nb,]
        PluieTelemac[,1]=PluieTelemac[,1]-PluieTelemac[1,1]
        
        
        y1=0.51; y2=0.74
        par(fig=c(x1,x2,y1,y2),new=TRUE)  
        plot(PluieTelemac,main = 'Pluie introduite dans Telemac2D',xlab="Temps Telemac (s)",ylab="Cumul sur le pas de temps (mm)",typ="s")
        
        
        y1=0.26; y2=0.49
        par(fig=c(x1,x2,y1,y2),new=TRUE)
        plot(PluieTelemac[,1],PluieTelemac[,2]*3600/(PluieTelemac[2,1]-PluieTelemac[1,1]),main = 'Intensité',xlab="Temps Telemac (s)",ylab="Intensité sur le pas de temps (mm/h)",typ="s")
        
        
        CumulPluieBase=sapply(1:dim(Pluie)[1], 
                              function(x) {sum(PluieBase[1:x,2])})
        CumulPluie=sapply(1:dim(Pluie)[1], 
                          function(x) {sum(Pluie[1:x,2])})
        CumulTelemac=sapply(1:dim(PluieTelemac)[1], 
                            function(x) {sum(PluieTelemac[1:x,2])})
        y1=0.01; y2=0.24
        par(fig=c(x1,x2,y1,y2),new=TRUE)
        plot(Pluie[,1],CumulPluieBase,main = 'Cumul',xlab="Temps (s)",ylab="Cumul en mm",type="l",col="blue")
        lines(Pluie[,1],CumulPluie,col="green")
        lines(PluieTelemac[,1]+DebutT*3600,CumulTelemac,col="black")
        
        legend("bottomright",
               legend = cbind("Pluie Brute","Pluie je ne sais pas comment appeler si telemac ou hyeto","Pluie Telemac"),
               col=cbind("blue","green","black"),
               lty=cbind(1,1,1),
               pch==cbind(1,1,1),
               title = "Cumul de Pluie")
        
        dev.off()
        aj=rbind(cbind(max(PluieTelemac[,1])+60,0),cbind(max(max(PluieTelemac[,1])+60,3*24*60*60,0),0))
        colnames(aj)=colnames(PluieTelemac)=cbind('X1','X2')
        PluieTelemac=rbind(PluieTelemac,aj)
        
        # -----------------------Réécriture hyeto pluie homogène----------------------------------------------------------------------------------------------          
        # ## write first line
        hyeto_nom=paste0(nom_exp,".txt")
        hyeto_name=file.path(dsnlayerC,hyeto_nom)
        write("#HYETOGRAPH FILE", file=hyeto_name)
        write("#T (s) RAINFALL (mm)", file=hyeto_name, append=T)
        write(paste(PluieTelemac[,1],PluieTelemac[,2]), file=hyeto_name, append=T)
        
        #-------------------------------------------------------------------------------------------------------------------------------------------        
      }
      
      ###### 
      if (is.null(contour$PRINTPERIO)){contour$PRINTPERIO=5}
      
      #### gestion des exports
      print(Nom_Ent)
      
      # Modif du 30/04/2020
      if (contour$PRINTPERIO>0){ 
        # if ((regexpr("T0100an",Nom_Ent)>-1)  || (regexpr("T0010an",Nom_Ent)>-1) || (regexpr("TPM2006",Nom_Ent)>-1))
        # 10 et 100 on garde la dynamique VARIABLES FOR GRAPHIC PRINTOUTS =U,V,B,H,Z,M,F,Q,MAXZ,TMXZ,MAXV 
        GPP=round(contour$PRINTPERIO*60/contour$TIME_STEP)
        VGP="U,V,B,H,Z,M,F,Q,MAXZ,TMXZ,MAXV,TMXV"
      }else if(contour$PRINTPERIO==-1){
        # reste que les données aggregées VARIABLES FOR GRAPHIC PRINTOUTS =B,MAXZ,TMXZ,MAXV pour tous sauf 10 et 100
        GPP=DUREE*3600/contour$TIME_STEP
        VGP="B,MAXZ,TMXZ,MAXV,TMXV"
      }else{
        GPP=DUREE*3600/contour$TIME_STEP
        VGP="U,V,B,H,Z,M,F,Q"
      }
      
      # Gestion du nombre de conditions limites
      nomcli=paste0(dsnlayerC,"\\",nom_Telemac,".cli")
      if(file.exists(nomcli)) 
      {
        tab1=read.table(nomcli,header=F,sep=" ")
      }
      nCL=max(length(which(abs(tab1[-1,1]-tab1[-dim(tab1)[1],1])>0))+1,3)
      
      # Valeurs sorties libre
      CavCavCavCavCav=COTE_AVAL
      if (nCL>1){for (incl in 2:nCL){CavCavCavCavCav=paste(CavCavCavCavCav,COTE_AVAL,sep=";")}}
      
      lg=nchar(CavCavCavCavCav)
      ligMax=71
      ligMin=31
      n1=floor((ligMax-ligMin)/(1+nchar(COTE_AVAL)))*(1+nchar(COTE_AVAL))
      ligMin=0
      n2=floor((ligMax-ligMin)/(1+nchar(COTE_AVAL)))*(1+nchar(COTE_AVAL))
      
      PrElev1 =substr(CavCavCavCavCav,1,n1)
      PrElev=function(lg,na,nb,incr)
      {
        Result=ifelse(lg>na+(incr-2)*na,substr(CavCavCavCavCav,na+(incr-2)*nb+1,min(lg,na+(incr-1)*nb)),"")
        return(Result)
      }
      # browser()
      PrElev2=PrElev(lg,n1,n2,2)
      PrElev3=PrElev(lg,n1,n2,3)
      PrElev4=PrElev(lg,n1,n2,4)
      PrElev5=PrElev(lg,n1,n2,5)
      PrElev6=PrElev(lg,n1,n2,6)
      PrElev7=PrElev(lg,n1,n2,7)
      PrElev8=PrElev(lg,n1,n2,8)
      PrElev9=PrElev(lg,n1,n2,9)
      PrElev10=PrElev(lg,n1,n2,10)
      PrElev11=PrElev(lg,n1,n2,11)
      PrElev12=PrElev(lg,n1,n2,12)
      PrElev13=PrElev(lg,n1,n2,13)
      PrElev14=PrElev(lg,n1,n2,14)
      PrElev15=PrElev(lg,n1,n2,15)
      PrElev16=PrElev(lg,n1,n2,16)
      
      # PrElev2 =ifelse(lg>n1+0*n2,substr(CavCavCavCavCav,n1+0*n2+1,min(lg,n1+1*n2)),"")
      # PrElev3 =ifelse(lg>n1+1*n2,substr(CavCavCavCavCav,n1+1*n2+1,min(lg,n1+2*n2)),"")
      # PrElev4 =ifelse(lg>n1+2*n2,substr(CavCavCavCavCav,n1+2*n2+1,min(lg,n1+3*n2)),"")
      # PrElev5 =ifelse(lg>n1+3*n2,substr(CavCavCavCavCav,n1+3*n2+1,min(lg,n1+4*n2)),"")
      # PrElev6 =ifelse(lg>n1+4*n2,substr(CavCavCavCavCav,n1+4*n2+1,min(lg,n1+5*n2)),"")
      # PrElev7 =ifelse(lg>n1+5*n2,substr(CavCavCavCavCav,n1+5*n2+1,min(lg,n1+6*n2)),"")
      # PrElev8 =ifelse(lg>n1+6*n2,substr(CavCavCavCavCav,n1+4*n2+1,min(lg,n1+5*n2)),"")
      # PrElev9 =ifelse(lg>n1+7*n2,substr(CavCavCavCavCav,n1+4*n2+1,min(lg,n1+5*n2)),"")
      # PrElev10=ifelse(lg>n1+8*n2,substr(CavCavCavCavCav,n1+4*n2+1,min(lg,n1+5*n2)),"")
      # PrElev11=ifelse(lg>n1+9*n2,substr(CavCavCavCavCav,n1+4*n2+1,min(lg,n1+5*n2)),"")
      # PrElev12=ifelse(lg>n1+10*n2,substr(CavCavCavCavCav,n1+4*n2+1,min(lg,n1+5*n2)),"")
      # PrElev13=ifelse(lg>n1+11*n2,substr(CavCavCavCavCav,n1+4*n2+1,min(lg,n1+5*n2)),"")
      # PrElev14=ifelse(lg>n1+12*n2,substr(CavCavCavCavCav,n1+4*n2+1,min(lg,n1+5*n2)),"")
      # PrElev15=ifelse(lg>n1+13*n2,substr(CavCavCavCavCav,n1+4*n2+1,min(lg,n1+5*n2)),"")
      
      if (lg>n1+(16-1)*n2){Il_n_y_a_pas_assez_de_sorties_val=BOOM}
      
      # Code sorties libre
      CoSortLib=1
      CavCavCavCavCav=CoSortLib
      if (nCL>1){for (incl in 2:nCL){CavCavCavCavCav=paste(CavCavCavCavCav,CoSortLib,sep=";")}}
      lg=nchar(CavCavCavCavCav)
      ligMax=71
      ligMin=31
      n1=floor((ligMax-ligMin)/(1+nchar(CoSortLib)))*(1+nchar(CoSortLib))
      ligMin=0
      n2=floor((ligMax-ligMin)/(1+nchar(CoSortLib)))*(1+nchar(CoSortLib))
      
      LiBo1=substr(CavCavCavCavCav,1,n1)
      LiBo2=ifelse(lg>n1+0*n2,substr(CavCavCavCavCav,n1+0*n2+1,min(lg,n1+1*n2)),"")
      LiBo3=ifelse(lg>n1+1*n2,substr(CavCavCavCavCav,n1+1*n2+1,min(lg,n1+2*n2)),"")
      LiBo4=ifelse(lg>n1+2*n2,substr(CavCavCavCavCav,n1+2*n2+1,min(lg,n1+3*n2)),"")
      
      if (lg>n1+3*n2){Il_n_y_a_pas_assez_de_sorties_code=BOOM}
      
      
      Strickler=ifelse(file.exists(paste0(dsnlayerC,"/","Friction",".csv"))==TRUE,
                       "13",
                       contour$Friction)
      
      if(Type!="EVTS" & Type!="SHYS")
      {
        Rep_USERFORTRAN="USER_FORTRAN_PH"
        FormattedDataFile="FORMATTED DATA FILE 1 ="
      }else{
        Rep_USERFORTRAN="USER_FORTRAN_PS"
        FormattedDataFile="FORMATTED DATA FILE 2 ="
      }
      
      Changement=rbind(cbind("GEOMETRY FILE","GEOMETRY FILE            =",NOMSLF),
                       cbind("FORMATTED DATA FILE 1 =",FormattedDataFile,hyeto_nom),
                       cbind("FRICTION COEFFICIENT","FRICTION COEFFICIENT   =",Strickler),
                       cbind("PRESCRIBED ELEVATIONS " ,"PRESCRIBED ELEVATIONS        =",PrElev1),
                       cbind("PRESCRIBED ELEVATION2S " ,"",PrElev2),
                       cbind("PRESCRIBED ELEVATION3S " ,"",PrElev3),
                       cbind("PRESCRIBED ELEVATION4S " ,"",PrElev4),
                       cbind("PRESCRIBED ELEVATION5S " ,"",PrElev5),
                       cbind("PRESCRIBED ELEVATION6S " ,"",PrElev6),
                       cbind("PRESCRIBED ELEVATION7S " ,"",PrElev7),
                       cbind("PRESCRIBED ELEVATION8S " ,"",PrElev8),
                       cbind("PRESCRIBED ELEVATION9S " ,"",PrElev9),
                       cbind("PRESCRIBED ELEVATION10S " ,"",PrElev10),
                       cbind("PRESCRIBED ELEVATION11S " ,"",PrElev11),
                       cbind("PRESCRIBED ELEVATION12S " ,"",PrElev12),
                       cbind("PRESCRIBED ELEVATION13S " ,"",PrElev13),
                       cbind("PRESCRIBED ELEVATION14S " ,"",PrElev14),
                       cbind("PRESCRIBED ELEVATION15S " ,"",PrElev15),
                       cbind("PRESCRIBED ELEVATION16S " ,"",PrElev16),
                       cbind('OPTION FOR LIQUID BOUNDARIES',"OPTION FOR LIQUID BOUNDARIES =",LiBo1),
                       cbind('OPTION FOR LIQUID BOUNDARIE2S',"",LiBo2),
                       cbind('OPTION FOR LIQUID BOUNDARIE3S',"",LiBo3),
                       cbind('OPTION FOR LIQUID BOUNDARIE4S',"",LiBo4),
                       cbind("INITIAL ELEVATION"   ,"INITIAL ELEVATION  =",COTE_AVAL),
                       cbind("RESULTS FILE    "        ,"RESULTS FILE             =",paste0(substr(hyeto_nom,1,nchar(hyeto_nom)-4),".res")),
                       cbind("FORTRAN FILE    "        ,"FORTRAN FILE             =",Rep_USERFORTRAN),
                       cbind("TITLE      ="         ,"TITLE      =",substr(hyeto_nom,1,nchar(hyeto_nom)-4)),
                       cbind("TIME STEP    "          ,"TIME STEP                           =",contour$TIME_STEP),
                       cbind("DURATION   " ,"DURATION                            =",DUREE*3600),
                       cbind("LISTING PRINTOUT PERIOD","LISTING PRINTOUT PERIOD         =",60/contour$TIME_STEP),
                       cbind("GRAPHIC PRINTOUT PERIOD","GRAPHIC PRINTOUT PERIOD         =",GPP),
                       cbind("VARIABLES FOR GRAPHIC PRINTOUTS","VARIABLES FOR GRAPHIC PRINTOUTS =",VGP),
                       cbind("ANTECEDENT MOISTURE CONDITIONS","ANTECEDENT MOISTURE CONDITIONS           =",AMC))
      
      Lignes=readLines(con=file.path(dsnlayerC,nom_Cas))
      
      for (ic in 1:dim(Changement)[1])
      {
        indic=regexpr(Changement[ic,1],as.character(Lignes))
        Lignes[which(indic>-1)]=paste0(Changement[ic,2],Changement[ic,3])
      }  
      
      # Gestion des ouvrages
      if (file.exists(file.path(dsnlayerC,"OuvHydrau_Select.txt"))==TRUE)
      {
        OuvHydrau=st_read(dsn = dsnlayerC,
                          layer = "OuvHydrau_Select")
        lg=length(Lignes)
        
        Lignes[lg+1]="/---------------------------------------------------------------------"
        Lignes[lg+2]="/ TUBES"
        Lignes[lg+3]="/---------------------------------------------------------------------"
        Lignes[lg+4]="CULVERTS DATA FILE       ='OuvHydrau_Select.txt'"
        Lignes[lg+5]=paste0("NUMBER OF CULVERTS                   = ",dim(OuvHydrau)[1])
        Lignes[lg+6]="TYPE OF SOURCES : 1"
        Lignes[lg+7]="OPTION FOR CULVERTS  = 2"
        Lignes[lg+8]='TURBULENCE MODEL     =1'
        Lignes[lg+9]='VELOCITY DIFFUSIVITY =1.E-2'
      }

      # Gestion des sections de controle
      Gestion_Sections_Controle(dsnlayer,dsnlayerC,contour,0)

      if (file.exists(file.path(dsnlayerC,"SectControl_Select.shp"))==TRUE)
      {
        SectControl=st_read(dsn = dsnlayerC,
                            layer = "SectControl_Select") 
        
        if(Type=="EVTS"){
          nom_exp_SC=paste0(substr(hyeto_nom,9,nchar(hyeto_nom)-4),'SC.txt')
        }else if (Type=="SHYS"){
          nom_exp_SC=paste0(substr(hyeto_nom,13,nchar(hyeto_nom)-4),'SC.txt')
        }else{
          nom_exp_SC=paste0(substr(hyeto_nom,7,nchar(hyeto_nom)-4),'SC.txt')
        }
        
        # nom_exp_SC=paste0(substr(hyeto_nom,1,nchar(hyeto_nom)-4),'SC.txt')
        lg=length(Lignes)
        
        # Export du fichier des sections de controle
        zzSC=paste0(file.path(dsnlayerC,"SectControl_Select.txt"))
        unlink(zzSC)
        file.create(zzSC)
        write("# Sections de contrôle ", file = zzSC, append=T)
        write(cbind(dim(SectControl)[1],0), file = zzSC, append=T)
        
        for (isc in 1:dim(SectControl)[1])
        {
          nomchaquesect=SectControl[isc,"ID"]
          st_geometry(nomchaquesect)=NULL
          # récupération du nom de la section et 
          # Limitation du nom à 24 caractères pour la sortie telemac
          aremplir=ifelse(nchar(as.character(nomchaquesect$ID))>0,as.character(nomchaquesect$ID),paste0("section_",isc))
          
          aremplir=substr(aremplir,nchar(aremplir)-min(nchar(aremplir),24)+1,nchar(aremplir))
          
          write(aremplir, file = zzSC, append=T)
          # Récupération de la géométrie
          coord=st_coordinates(SectControl[isc,])
          lc=""
          for (isc in 1:dim(coord)[1])
          {
            lc=paste(lc,round(coord[isc,1],2),round(coord[isc,2],2))
          }
          write(lc, file = zzSC, append=T)
        }
        
        # Ecriture dans le fichier .cas
        if(newmethodsc==TRUE){
          
          choosen_sec <- st_read(dsn = dsnlayerC,
                                 layer = "SectControl_Select") 
          
          coords_line <- st_coordinates(choosen_sec)
          if(is.numeric(contour$Exzeco)){
            
            buff_sec <- st_buffer(x = choosen_sec,dist = contour$Exzeco*1.5,joinStyle = "BEVEL",endCapStyle = "SQUARE")
          }else{
            # buff_sec <- st_buffer(x = choosen_sec,dist = contour$Cont1_Dx,joinStyle = "BEVEL",endCapStyle = "SQUARE")
            buff_sec <- st_buffer(x = choosen_sec,dist = contour$Cont1_Dx,joinStyle = "BEVEL",endCapStyle = "SQUARE")
          }
          
          coords_buff <- st_coordinates(buff_sec)
          
          
          
          coords_list_line <- sapply(choosen_sec$geometry, function(line) {
            coords <- st_coordinates(line)
            c(x1 = coords[1,1],x2 = coords[2,1],
              y1 = coords[1,2],
              y2=coords[2,2])
          }, simplify = "data.frame")
          
          
          coords_list <- sapply(buff_sec$geometry, function(polygon) {
            coords <- st_coordinates(polygon)
            c(xmin = min(coords[, "X"]),xmax = max(coords[, "X"]),
              ymin = min(coords[, "Y"]),
              ymax = max(coords[, "Y"]))
          },simplify = "data.frame")
          
          
          choosen_sec$x1 <- coords_list_line[1,]
          choosen_sec$x2 <- coords_list_line[2,]
          choosen_sec$y1 <- coords_list_line[3,]
          choosen_sec$y2 <- coords_list_line[4,]
          choosen_sec$xmin <- coords_list[1,]
          choosen_sec$xmax <- coords_list[2,]
          choosen_sec$ymin <- coords_list[3,]
          choosen_sec$ymax <- coords_list[4,]
          # choosen_sec$nom <- c(213,54)
          
          coords_df <- data.frame(x1=choosen_sec$x1, 
                                  y1 =choosen_sec$y1,
                                  x2 = choosen_sec$x2,
                                  y2=choosen_sec$y2,
                                  xmin=choosen_sec$xmin,
                                  ymin=choosen_sec$ymin,
                                  xmax=choosen_sec$xmax,
                                  ymax=choosen_sec$ymax,
                                  pourien=1,
                                  nom=as.integer(choosen_sec$ID))
          
          unlink(zzSC)
          zzSCnew=file(zzSC,"w")
          
          
          # Écrire l'en-tête du fichier texte
          write(length(coords_df$x1), file = zzSCnew, append = TRUE)
          
          # Écrire chaque ligne de la data.frame dans le fichier texte
          lapply(1:nrow(coords_df), function(i) {
            write(paste(round(coords_df[i, ],digits = 2), collapse = " "), file = zzSCnew, append = TRUE)
            # write("\n", file = file, append = TRUE)
          })
          
          # Fermer le fichier texte
          close(zzSCnew)
          
          
          
          
          
          
          Lignes[lg+1]="/---------------------------------------------------------------------"
          Lignes[lg+2]="/ CONTROL SECTIONS"
          Lignes[lg+3]="/---------------------------------------------------------------------"
          Lignes[lg+4]="FLUXLINE = YES "
          Lignes[lg+5]="FLUXLINE INPUT FILE = 'SectControl_Select.txt'"
          Lignes[lg+6]=paste0("SECTIONS OUTPUT FILE     =",nom_exp_SC)
        }else{
          Lignes[lg+1]="/---------------------------------------------------------------------"
          Lignes[lg+2]="/ CONTROL SECTIONS"
          Lignes[lg+3]="/---------------------------------------------------------------------"
          Lignes[lg+4]="SECTIONS INPUT FILE      ='SectControl_Select.txt'"
          Lignes[lg+5]=paste0("SECTIONS OUTPUT FILE     =",nom_exp_SC)
        }
        
        
      }
      
      # gestion des trucs en + quand on fait évènements réel
      # date exacte
      if (Type=="EVT"){
        lg=length(Lignes)
        Lignes[lg+1]="/---------------------------------------------------------------------"
        Lignes[lg+2]="INITIAL TIME SET TO ZERO        =true"
        Lignes[lg+3]=paste0("ORIGINAL DATE OF TIME =",
                            as.numeric(substr(name_split[5],4,7)),";",
                            as.numeric(substr(name_split[5],8,9)),";",
                            as.numeric(substr(name_split[5],10,11)))
        Lignes[lg+4]=paste0("ORIGINAL HOUR OF TIME =",
                            as.numeric(substr(name_split[5],12,13)),";",
                            as.numeric(substr(name_split[5],14,15)),";",0)
        
        
      }else if(Type=="EVTS"){
        
        lg=length(Lignes)
        Lignes[lg+1]="/---------------------------------------------------------------------"
        Lignes[lg+2]="INITIAL TIME SET TO ZERO        =true"
        Lignes[lg+3]=paste0("ORIGINAL DATE OF TIME =",
                            as.numeric(substr(name_split[5],5,8)),";",
                            as.numeric(substr(name_split[5],9,10)),";",
                            as.numeric(substr(name_split[5],11,12)))
        Lignes[lg+4]=paste0("ORIGINAL HOUR OF TIME =",
                            as.numeric(substr(name_split[5],13,14)),";",
                            as.numeric(substr(name_split[5],15,16)),";",0)
      }
      # browser()
      if (!(is.na(contour$SourceDeb) | nchar(contour$SourceDeb)==0))
      {
        # Lecture du réseau
        nom_SourcesDebits=contour$SourceDeb
        fun_check_exist(file.path(nom_SourcesDebits),1)
        SourcesDebits=st_read(file.path(nom_SourcesDebits))
        st_crs(SourcesDebits)=crs
        # On récupère ce qui intersecte
        nbRC=st_within(SourcesDebits,contour)
        n_int = which(sapply(nbRC, length)>0)
        # 
        if (length(n_int>0)>0)
        {
          lg=length(Lignes)
          print(paste0(nomcontour," ---- Récupération des Source Débits"))
          SourcesDebits=SourcesDebits[n_int,]
          st_write(SourcesDebits,dsn=file.path(dsnlayerC,"SourcesDebits_Select.shp"), delete_layer=T, quiet=T)
          
          listeDebits=which(substr(colnames(SourcesDebits),1,2)=="Q_" | colnames(SourcesDebits)=="SOURCEFILE")
          
          for (idebit in listeDebits)
          {
            LignesQ=Lignes
            Lien_Fichier=SourcesDebits[,idebit]
            Lien_Fichier <- st_drop_geometry(Lien_Fichier)
            
            TextS=paste0("MAXIMUM NUMBER OF SOURCES                = ",max(20,dim(SourcesDebits)[1]))
            
            XY=st_coordinates(SourcesDebits)
            TextX=paste0("ABSCISSAE OF SOURCES                     =",format(XY[1,1],digits=4))
            TextY=paste0("ORDINATES OF SOURCES                     =",format(XY[1,2],digits=4))
            TextWDS="WATER DISCHARGE OF SOURCES               =0.0"
            if (dim(SourcesDebits)[1]>1)
            {
              for (isc in 2:dim(SourcesDebits)[1])
              {
                TextX=paste(TextX,format(XY[isc,1],digits=4),sep=";")
                TextY=paste(TextY,format(XY[isc,2],digits=4),sep=";")
                TextWDS=paste(TextWDS,"0.0",sep=";")
              }
            }
            liglong=70
            LignesQ=LigneBonneLongueur(LignesQ,liglong,TextS)
            LignesQ=LigneBonneLongueur(LignesQ,liglong,TextX)
            LignesQ=LigneBonneLongueur(LignesQ,liglong,TextY)
            LignesQ=LigneBonneLongueur(LignesQ,liglong,TextWDS)
            
            lg=length(LignesQ)
            nomsourceliq=paste0("source",colnames(Lien_Fichier),".liq")
            LignesQ[lg+1]=paste0("SOURCES FILE            = ",nomsourceliq)
            
            
            for (iLF in 1:dim(Lien_Fichier)[1])
            {
              cheminsourcedebit="_SourcesDebits"
              cheminsourcedebit=file.path(dirname(file.path(nom_SourcesDebits)),"_SourcesDebits")
              print(file.path(cheminsourcedebit,Lien_Fichier[iLF,1]))
              fun_check_exist(file.path(cheminsourcedebit,Lien_Fichier[iLF,1]),1)
              
              print(Lien_Fichier[iLF,1])
              ficiLF=read.table(file.path(cheminsourcedebit,Lien_Fichier[iLF,1]),header=T)
              print(Lien_Fichier[iLF,1])
              print(dim(ficiLF))

              if (iLF==1)
              {
                DSour=ficiLF
                titcol1=paste("T","Q(1)")
                titcol2=paste("s","m3/s")
              }else{
                # browser()
                DSour=cbind(DSour,DSour[,1])
                DSour[,iLF+1]=as.numeric(ficiLF[,2])
                titcol1=paste(titcol1,paste0("Q(",iLF,")"))
                titcol2=paste(titcol2,"m3/s")
              }
            }
            
            Etape4_ExportLiq(dsnlayerC,nomsourceliq,titcol1,titcol2,DSour)
            
            hyeto_name =file.path(dsnlayerC,hyeto_nom)
            hyeto_nameQ=paste0(substr(hyeto_name,1,nchar(hyeto_name)-4),"_",colnames(Lien_Fichier),".txt")
            file.copy(hyeto_name,hyeto_nameQ)
            
            Changement=rbind(
              cbind("FORMATTED DATA FILE 1 =",FormattedDataFile,basename(hyeto_nameQ)),
              cbind("RESULTS FILE    "        ,"RESULTS FILE             =",paste0(substr(hyeto_nom,1,nchar(hyeto_nom)-4),"_",colnames(Lien_Fichier),".res"))
            )
            
            for (ic in 1:dim(Changement)[1])
            {
              
              indic=regexpr(Changement[ic,1],as.character(Lignes))
              LignesQ[which(indic>-1)]=paste0(Changement[ic,2],Changement[ic,3])
            } 
            
            cas_name=paste0(substr(hyeto_nom,7,nchar(hyeto_nom)-4),"_",colnames(Lien_Fichier),".cas")
            Etape4_ExportCas(dsnlayerC,cas_name,LignesQ)
            
            
          }
        }else{
          cas_name=paste0(substr(hyeto_nom,7,nchar(hyeto_nom)-4),".cas")
          Etape4_ExportCas(dsnlayerC,cas_name,Lignes)
        }
        
        # 
      }else{
        
        cas_name=paste0(substr(hyeto_nom,7,nchar(hyeto_nom)-4),".cas")
        if(Type=="EVTS"){
          cas_name=paste0(substr(hyeto_nom,9,nchar(hyeto_nom)-4),"_s.cas")
        }else if (Type=="SHYS"){
          # cas_name=paste0(substr(hyeto_nom,13,nchar(hyeto_nom)-4),"_SHYS.cas")
          cas_name=paste0(substr(hyeto_nom,13,nchar(hyeto_nom)-4),"_",substr(hyeto_nom,8,11),".cas")
        }
        Etape4_ExportCas(dsnlayerC,cas_name,Lignes)
      }
      
    }
  }
  
  #### Recherche si fichier Liquide.txt pour imposer plusieurs débits fixes
  if (file.exists(file.path(dsnlayerC,"Liquide.txt"))==T)
  {    ##### Lecture des différents nomcas
    print("#Ajout de débits permanents")
    listecas=list.files(dsnlayerC,pattern = ".cas",recursive = F)
    listecas = listecas[grep(substr(listecas,nchar(listecas)-3,nchar(listecas)), pattern=".cas")]
    listecas = listecas[nchar(listecas)>13]
    listecas = substr(listecas,1,nchar(listecas)-4)
    print(listecas)
    
    tab=read.table(file.path(dsnlayerC,"Liquide.txt"))
    
    for (icas in 1:length(listecas))
    {
      for (iliq in 1:dim(tab)[1])
      {
        NomLiq=paste0("SourceQ",tab[iliq,1])
        Nom_Cas2=paste0(listecas[icas],"_Q",tab[iliq,1])
        if (dim(tab)[2]>1) 
        {
          for (iliq2 in 2:dim(tab)[2]) 
          {
            NomLiq=paste0(NomLiq,"_",tab[iliq,iliq2])
            Nom_Cas2=paste0(Nom_Cas2,"_",tab[iliq,iliq2])
          }
        }
        
        NomLiq=paste0(NomLiq,".liq")
        
        Changement=rbind(cbind("RESULTS FILE    "        ,"RESULTS FILE             =",paste0(Nom_Cas2,".res")),
                         cbind("SOURCES FILE            =","SOURCES FILE            =",NomLiq))
        
        for (ic in 1:dim(Changement)[1])
        {
          indic=regexpr(Changement[ic,1],as.character(Lignes))
          Lignes[which(indic>-1)]=paste0(Changement[ic,2],Changement[ic,3])
        }  
        cas_name=paste0(Nom_Cas2,".cas")
        Etape4_ExportCas(dsnlayerC,cas_name,Lignes)
        
        for (iLF in 1:dim(tab)[2])
        {
          cheminsourcedebit="_SourcesDebits"
          ficiLF[-1,2]=tab[iliq,iLF]
          if (iLF==1)
          {
            DSour=ficiLF
            titcol1=paste("T","Q(1)")
            titcol2=paste("s","m3/s")
          }else{
            DSour=cbind(DSour,ficiLF[,2])
            titcol1=paste(titcol1,paste0("Q(",iLF,")"))
            titcol2=paste(titcol2,"m3/s")
          }
        }
        Etape4_ExportLiq(dsnlayerC,NomLiq,titcol1,titcol2,DSour)
      }
    }
  }
  return(cas_name) ## ajout nabil
}

#################################################################################################
################   Etape4_ExportLiq
#################################################################################################
Etape4_ExportLiq=function(dsnlayerC,nomsourceliq,titcol1,titcol2,DSour)
{
  zzSD =file.path(dsnlayerC,nomsourceliq)
  write(titcol1, file = zzSD)
  write(titcol2, file = zzSD,append=T)
  write.table(DSour,file=zzSD, sep = " ",row.names = F,col.names = F,append=T)
}

#################################################################################################
################   Etape4_ExportCas
#################################################################################################
Etape4_ExportCas=function(dsnlayerC,cas_name,Lignes)
{
  print(cas_name)
  zzPPutils <- file(file.path(dsnlayerC,cas_name))
  writeLines(Lignes, con = zzPPutils, sep = "\n", useBytes = FALSE)
  close(zzPPutils)
  # browser()
  # Travail sur l'obturation des ouvrages
  if (exists("RelSce_OHObstrue")==T & exists("nomxlsGrilleOHobstrue")==T)
  {
    cas_name_new <-Etape04_Cas_OHRegleObstruration(dsnlayerC,nomxlsGrilleOHobstrue,RelSce_OHObstrue,cas_name)
  }
}

#################################################################################################
################   Etape04_Cas_OHRegleObstruration
#################################################################################################
Etape04_Cas_OHRegleObstruration=function(dsnlayerC,nomxlsGrilleOHobstrue,RelSce_OHObstrue,cas_name_)
{
  # browser()
  # Lecture du fichier comprenant les règles des capacités des ouvrages
  xls=read_excel(nomxlsGrilleOHobstrue)
  print(xls)
  
  nomOH=file.path(dsnlayerC,"OuvHydrau_Select.txt")
  # Vérification quil y a des ouvrages sinon cela ne sert à rien
  if (file.exists(nomOH)==T)
  {
    for (irel in 1:dim(RelSce_OHObstrue)[1])
    {
      cat(cas_name_,RelSce_OHObstrue[irel,])
      # Anbalyse pour voir si le cas telemac correspond à un scénario à traiter
      ici=grep(cas_name_,pattern=RelSce_OHObstrue[irel,1])
      if (length(ici)>0)    
      {
        cat(" Présent\n")
        # Lecture des ouvrages de ce cas Telemac
        OuvHydrau=read.table(nomOH,header=T,skip=2)
        reg=sapply(1:dim(xls)[1], function(x) {
          min(as.numeric(xls$AEvt[x]),as.numeric(xls[x,RelSce_OHObstrue[irel,2]]))})
        print(reg)
        # print(OuvHydrau)
        # plot(as.numeric(xls$Dimension), reg,'l')
        
        # Ouvrages Rectanglulaires
        nb=which(OuvHydrau$CIRC==0)
        if (length(nb)>0)
        {
          # Calcul de la surface
          Surf=OuvHydrau$LRG[nb]*OuvHydrau$HAUT1[nb]
          # Fonction d'interpolation
          interpole=approxfun(as.numeric(xls$Dimension), reg, method="linear") 
          # Affectation des nouvelles dimensions
          OuvHydrau$LRG[nb]  =round(interpole(Surf)^0.5*OuvHydrau$LRG[nb],2)
          OuvHydrau$HAUT1[nb]=round(interpole(Surf)^0.5*OuvHydrau$HAUT1[nb],2)
          OuvHydrau$HAUT2[nb]=round(interpole(Surf)^0.5*OuvHydrau$HAUT2[nb],2)
        }
        # Ouvrages circulaire
        nb=which(OuvHydrau$CIRC==1)
        if (length(nb)>0)
        {
          # Calcul de la surface
          Surf=pi*(OuvHydrau$LRG[nb]/2)^2
          # Fonction d'interpolation
          interpole=approxfun(as.numeric(xls$Dimension), reg, method="linear") 
          # Affectation des nouvelles dimensions
          OuvHydrau$LRG[nb]  =round(interpole(Surf)^0.5*OuvHydrau$LRG[nb],2)
          OuvHydrau$HAUT1[nb]=round(interpole(Surf)^0.5*OuvHydrau$HAUT1[nb],2)
          OuvHydrau$HAUT2[nb]=round(interpole(Surf)^0.5*OuvHydrau$HAUT2[nb],2)
        }
        # Export du nouveau fichier
        nomOH_new=paste0("OuvHydrau_Select_",RelSce_OHObstrue[irel,2],".txt")
        zzOH=file.path(dsnlayerC,nomOH_new)
        unlink(zzOH)
        file.create(zzOH)
        write("Relaxation, Number of culverts", file = zzOH, append=T)
        write(cbind(0.2,dim(OuvHydrau)[1]), file = zzOH, append=T)
        toto="I1 I2 CE1 CE2 CS1 CS2 LRG HAUT1 CLP LBUS Z1 Z2 CV C56 CV5 C5 CT HAUT2 FRIC LENGTH CIRC D1 D2 A1 A2 AA"
        write(toto, file = zzOH, append=T)
        toto=paste(OuvHydrau$I1,OuvHydrau$I2,
                   OuvHydrau$CE1,OuvHydrau$CE2,OuvHydrau$CS1,OuvHydrau$CS2,OuvHydrau$LRG,OuvHydrau$HAUT1,
                   OuvHydrau$CLP,OuvHydrau$LBUS,OuvHydrau$Z1,OuvHydrau$Z2,OuvHydrau$CV,OuvHydrau$C56,
                   OuvHydrau$CV5,OuvHydrau$C5,OuvHydrau$CT,OuvHydrau$HAUT2,OuvHydrau$FRIC,round(OuvHydrau$LENGTH,2),
                   OuvHydrau$CIRC,OuvHydrau$D1,OuvHydrau$D2,OuvHydrau$A1,OuvHydrau$A2,OuvHydrau$AA)
        write(toto, file = zzOH, append=T)
        
        # Creation d'un fichier cas
        Lignes_new=readLines(con=file.path(dsnlayerC,cas_name_))
        
        
        cas_name_new=substr(cas_name_,1,nchar(cas_name_)-4)
        print(cas_name_new)
        cas_name_new=gsub(RelSce_OHObstrue[irel,1], paste0(RelSce_OHObstrue[irel,1],RelSce_OHObstrue[irel,2]), cas_name_new)
        print(cas_name_new)
        
        Changement=rbind(
          cbind("RESULTS FILE    "        ,"RESULTS FILE             =",paste0("hyeto_",cas_name_new,".res")),
          cbind("TITLE      ="         ,"TITLE      =",print(cas_name_new)),
          cbind("CULVERTS DATA FILE       =","CULVERTS DATA FILE       =",nomOH_new),
          cbind("SECTIONS OUTPUT FILE     =","SECTIONS OUTPUT FILE     =",paste0(cas_name_new,"SC.txt"))   
        )
        
        for (ic in 1:dim(Changement)[1])
        {
          indic=regexpr(Changement[ic,1],as.character(Lignes_new))
          Lignes_new[which(indic>-1)]=paste0(Changement[ic,2],Changement[ic,3])
        }  
        print(cas_name_new)
        zzPPutils <- file(file.path(dsnlayerC,paste0(cas_name_new,".cas")))
        writeLines(Lignes_new, con = zzPPutils, sep = "\n", useBytes = FALSE)
        close(zzPPutils)
        
      }else{
        cat(" Absent\n")
      }
    }
  }else{
    cas_name_new=""
  }
  # return(cas_name_new)
}

Etape4_fn <- function(){
  tryCatch({
    if(ETAPE[4] == 1){
      cat("\014")
      cat("ETAPE 4 - Traitement de: ",contour$NOM,"\n")
      
      name_cas_all <- c()
      
      cas_name_ <- Etape4_Cas_Hyeto(dsnlayerC, nom_Cas, dsnPluie, contour)
      name_cas_all <- append(name_cas_all,cas_name_)
      
      if(file.exists(file.path(dsnlayerC,"casfiles.txt"))) file.remove(file.path(dsnlayerC,"casfiles.txt"))
      writeLines(name_cas_all,file.path(dsnlayerC,"casfiles.txt"))
    }}, error = function(e) {skip_to_next <<- TRUE})
  
  return(skip_to_next)
}

Etape4_fn_parallel <- function(){
  
  if(ETAPE[4] == 1){
    cat("\014")
    cat("ETAPE 4 - Traitement de: ",contour$NOM,"\n")
    
    name_cas_all <- c()
    
    cas_name_ <- Etape4_Cas_Hyeto(dsnlayerC, nom_Cas, dsnPluie, contour)
    name_cas_all <- append(name_cas_all,cas_name_)
    if(file.exists(file.path(dsnlayerC,"casfiles.txt"))) file.remove(file.path(dsnlayerC,"casfiles.txt"))
    writeLines(name_cas_all,file.path(dsnlayerC,"casfiles.txt"))
  }
}