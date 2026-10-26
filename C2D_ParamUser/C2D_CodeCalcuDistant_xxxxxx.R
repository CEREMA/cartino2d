# ====================================================================================================================
# CARTINO2D - TEMPLATE : C2D_CodeCalcuDistant.R
# Objet : Identifiants, connexions SSH et paramètres de soumission vers les clusters distants
# ====================================================================================================================
# Instructions :
# 1. Copier ce fichier dans "C2D_ParamUser/C2D_CodeCalcuDistant.R"
# 2. Renseigner vos identifiants d'accès selon les grappes de calcul que vous utilisez.
# 3. Ce fichier est sourcé automatiquement par "C2D_RUN.R" (lignes 44-45).
# ====================================================================================================================

####################################################################################################################
#### 1. CENTRE DE CALCUL CEREMA / DTECREM (Margny-lès-Compiègne) ##################################################
####################################################################################################################

# Répertoire utilisateur sur le cluster distant (ex: "prenom.nom" ou login)
dir_cluster_margny <- "votre_login"

# Hôte SSH de connexion (login@ip_ou_dns)
hoteDTecREM        <- "votre_login@hote_cluster_ou_ip"

# Chemin vers la clé privée PuTTY (.ppk) ou OpenSSH (.pem / id_rsa)
clef_margny_ppk    <- "C:/Cartino2D/ClesSSH/cle_margny.ppk"

# Chemin vers le fichier contenant le mot de passe (ou clé texte)
clef_margny        <- "C:/Cartino2D/ClesSSH/pass_margny"


####################################################################################################################
#### 2. CENTRE DE CALCUL IFREMER (Supercalculateur DATARMOR) #######################################################
####################################################################################################################

# Répertoire de travail utilisateur sur Datarmor
dir_cluster_ifremer <- "votre_login"

# Hôte SSH Datarmor frontal
hoteIfremer         <- "votre_login@datarmor.ifremer.fr"

# Mot de passe ou passphrase SSH Datarmor
MdPIfremer          <- "votre_mot_de_passe"

# Hôte et mot de passe pour le transfert externe (DMZ / passerelle)
hoteextrifremer     <- "votre_login_ext"
Mdpextrifremer      <- "votre_mot_de_passe_ext"

# Allocation de ressources Datarmor :
# q_ifremer = nombre de nœuds alloués (chaque nœud comporte 28 cœurs).
# Si q_ifremer = 2, le calcul se lance sur 2 * 28 = 56 cœurs.
q_ifremer           <- 2

# Temps d'exécution maximal alloué (format HH:MM:SS)
walltime_ifremer    <- "48:00:00"


####################################################################################################################
#### 3. CENTRE DE CALCUL NATIONAL GENCI / TGCC (Supercalculateur IRÈNE CEA) #######################################
####################################################################################################################

# Nom du compte utilisateur sur Irène (TGCC / CCRT)
dir_cluster_tgcc    <- "votre_login"

# Hôte SSH de connexion Irène
hotetgcc            <- "votre_login@irene-fr.ccc.cea.fr"

# Mot de passe / token d'authentification TGCC
MdPtgcc             <- "votre_mot_de_passe"

# Identifiant du projet / allocation GENCI (ex: "gen14287")
project_tgcc        <- "votre_projet_genci"

# Durée maximale de calcul en heures (défaut : 8h ou 24h selon la file d'attente Slurm)
walltime_tgcc       <- 8

# Seuil de découpage en cœurs par taille de maillage (typiquement 25000 à 50000 nœuds par cœur)
DecoupCoeurs        <- 25000
