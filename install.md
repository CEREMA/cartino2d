# Notice d’Installation de Cartino2D
*(Version optimisée Mistral.ai – Novembre 2025)*
**Compatibilité** : Windows uniquement
**Langue** : Français

---

### 1. Prérequis
Avant de commencer, assurez-vous :
- D’avoir les **droits d’administration** sur votre machine.
- D’utiliser un **système d’exploitation Windows**.

---

### 2. Installation de QGIS
- Téléchargez et installez **QGIS** depuis : [https://qgis.org/](https://qgis.org/)
  **Chemin d’installation recommandé** : `C:\QGIS`
- **Attention** : Si le fichier `grass.bat` ne fonctionne pas correctement, installez **GRASS GIS** depuis : [https://grass.osgeo.org/](https://grass.osgeo.org/)

---

### 3. Création du répertoire Cartino2D
- Créez un dossier nommé **Cartino2D** à la racine du disque `C:`.
  **Chemin recommandé** : `C:\Cartino2D`

---

### 4. Installation de GMSH
- Téléchargez **GMSH** (dernière version pour Windows 64-bit) depuis : [https://gmsh.info/bin/Windows/](https://gmsh.info/bin/Windows/)
- Décompressez l’archive et copiez son contenu dans :
  **Chemin de destination** : `C:\Cartino2D`

---

### 5. Installation de R et RStudio
- Téléchargez et installez **R** (dernière version) depuis : [https://cran.r-project.org/bin/windows/base/](https://cran.r-project.org/bin/windows/base/)
  **Chemin d’installation** : `C:\R\R-x.x.x`
- Téléchargez et installez **RStudio** depuis : [https://posit.co/download/rstudio-desktop/](https://posit.co/download/rstudio-desktop/)
  **Chemin d’installation** : `C:\RStudio`
- Installez **Rtools** depuis : [https://cran.r-project.org/bin/windows/Rtools/](https://cran.r-project.org/bin/windows/Rtools/)
- Copiez le dossier **Cerema** (contenant les codes R) dans `C:\R\R-x.x.x`, puis :
  - Extrayez son contenu.
  - Renommez le dossier extrait en **Cerema**.

---

### 6. Installation de TauDEM
- Téléchargez **TauDEM** (version complète) depuis : [https://hydrology.usu.edu/taudem/taudem5/downloads.html](https://hydrology.usu.edu/taudem/taudem5/downloads.html)
- Installez-le dans le répertoire suivant :
  **Chemin d’installation** : `C:\TauDEM`
- Lors de l’installation, choisissez l’option :
  **Type de configuration** : *Typical*

---

### 7. Installation de Python
- Installez **Python** (dernière version) depuis : [https://www.python.org/](https://www.python.org/)
  **Options d’installation** :
  - Choisissez l’option pour **tous les utilisateurs**.
  - Décochez l’option d’installation "classique" (pour inclure **pip**).
- **Vérifiez l’installation** :
  - Ouvrez une console DOS (`cmd`).
  - Tapez les commandes suivantes :
    ```bash
    python --version
    pip --version
    ```
  - Si les versions s’affichent, l’installation est correcte.
- Installez les bibliothèques requises :
  ```bash
  pip install numpy scipy matplotlib
  ```
- Ajoutez Python au PATH (si ce n’est pas déjà fait)
- Ouvrez les **variables d’environnement** (recherchez "Environnement" dans le menu Démarrer).
- Ajoutez le chemin d’installation de Python (ex: `C:\Python310`) à la variable **Path**.

---

### 8. Copie de pputils
- Téléchargez le fichier **pputils** depuis : [https://codeberg.org/pprodano/pputils](https://codeberg.org/pprodano/pputils)
- Copiez-le dans :
  **Chemin de destination** : `C:\Cartino2D`
- **Modifications spécifiques à Cartino2D** :
  Glissez les dossiers et fichiers de `pputils_cerema` dans le dossier `pputils`, en conservant les bonnes positions.

---

### 9. Téléchargement de pscp.exe
- Téléchargez **pscp.exe** depuis : [https://the.earth.li/~sgtatham/putty/latest/w64/pscp.exe](https://the.earth.li/~sgtatham/putty/latest/w64/pscp.exe)
- Copiez le fichier dans :
  **Chemin de destination** : `C:\Cartino2D`
  *(Ne lancez pas l’exécutable.)*

---

### 10. Installation de Telemac v8p4r0
- Téléchargez **Telemac v8p4r0** depuis : [http://www.opentelemac.org/](http://www.opentelemac.org/)
- Installez-le, par exemple, dans le répertoire : `C:\TELEMAC\V8P4`
- Mettez à jour **MPI** en téléchargeant et en exécutant `msmpisetup.exe` depuis : [https://www.microsoft.com/en-us/download/details.aspx?id=105289](https://www.microsoft.com/en-us/download/details.aspx?id=105289)
  *(Cochez `msmpisetup.exe`)*
- Testez si Telemac fonctionne en exécutant un exemple comme `t2d_pluie.cas`.
  **Conseil** : Prenez en main Telemac avant de vous lancer dans Cartino2D.

---

### 11. Vérification finale
- **Redémarrez votre machine** pour appliquer toutes les modifications.
- Vérifiez que tous les logiciels sont accessibles depuis leurs chemins respectifs.

---

### 12. Lancement de Cartino2D
- Allez dans `C:\R\R-x.x.x\Cerema` et lancez **C2D_Run**.
- **RStudio** s’ouvrira automatiquement.
- Dans l’interface, installez les librairies demandées.
- Cliquez sur **Source** pour commencer à utiliser Cartino2D de manière autonome.
- Des exemples sont fournis en cliquant sur l'étape 0 de C2D

---

**Remarque** :
Les utilisateurs sont invités à suivre ces instructions avec attention pour une installation réussie. **Aucune hotline n’est fournie.**

Bonne utilisation !
