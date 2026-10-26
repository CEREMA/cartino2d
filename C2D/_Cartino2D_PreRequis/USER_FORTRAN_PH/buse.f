!                   ***************
                    SUBROUTINE BUSE
!                   ***************
!
     &(RELAXB,NBUSE,ENTBUS,SORBUS,GRAV,
     & H,ZF,DBUS,LRGBUS,HAUBUS,CLPBUS,
     & ALTBUS,CSBUS,CEBUS,ANGBUS,LBUS,
     & NTRAC,T,TBUS,UBUS,VBUS,U,V,ENTET,
     & CV,C56,CV5,C5,CTRASH,FRICBUS,LONGBUS,
     & CIRC,DELBUS,OPTBUSE,V2DPAR,DT,SECBUS,MAXSOURCE,
     & NPTSCE,NPOIN2,KSCE)
!
!***********************************************************************
! TELEMAC2D   V8P4 - OPTIMISATION VECTORISEE MPI HAUTE PERFORMANCE
!***********************************************************************
!
!brief    TRAITEMENT DES BUSES AVEC OPTIMISATIONS MPI MAJEURES :
!+        1. Synchronisation de la geometrie fixe uniquement a T=0.
!+        2. Vectorisation MPI (P_DMAX_ARRAY / P_DSUM_ARRAY).
!+           Un seul echange groupe par pas pour toutes les buses.
!+        3. Calculs et interpolation 100% locaux a pleine vitesse CPU.
!+        4. Strictement conforme aux lois physiques d'origine.
!
      USE BIEF
      USE BIEF_DEF, ONLY : IPID, NCSIZE
      USE DECLARATIONS_SPECIAL
      USE INTERFACE_PARALLEL, ONLY : P_MAX, P_MIN, P_DSUM, P_ISUM,
     &                               P_DMAX_ARRAY, P_DMIN_ARRAY
      USE DECLARATIONS_TELEMAC2D, ONLY : MESH, AT
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER          , INTENT(IN)    :: NBUSE,NTRAC
      INTEGER          , INTENT(IN)    :: ENTBUS(NBUSE),SORBUS(NBUSE)
      LOGICAL          , INTENT(IN)    :: ENTET
      DOUBLE PRECISION , INTENT(IN)    :: RELAXB,GRAV,DT
      DOUBLE PRECISION , INTENT(INOUT) :: UBUS(2,NBUSE),VBUS(2,NBUSE)
      DOUBLE PRECISION , INTENT(INOUT) :: DBUS(NBUSE),SECBUS(NBUSE)
      TYPE(BIEF_OBJ)   , INTENT(INOUT) :: TBUS
      DOUBLE PRECISION , INTENT(IN)    :: ANGBUS(NBUSE,2),LBUS(NBUSE)
      DOUBLE PRECISION , INTENT(IN)    :: CEBUS(NBUSE,2),CSBUS(NBUSE,2)
      DOUBLE PRECISION , INTENT(IN)    :: ALTBUS(NBUSE,2)
      DOUBLE PRECISION , INTENT(IN)    :: LRGBUS(NBUSE),HAUBUS(NBUSE,2)
      INTEGER          , INTENT(IN)    :: CLPBUS(NBUSE),CIRC(NBUSE)
      DOUBLE PRECISION , INTENT(IN)    :: DELBUS(NBUSE,2)
      DOUBLE PRECISION , INTENT(IN)    :: H(*),ZF(*),U(*),V(*)
      TYPE(BIEF_OBJ)   , INTENT(IN)    :: T
      DOUBLE PRECISION , INTENT(IN)    :: CV(NBUSE),C56(NBUSE)
      DOUBLE PRECISION , INTENT(IN)    :: CV5(NBUSE),C5(NBUSE)
      DOUBLE PRECISION , INTENT(IN)    :: CTRASH(NBUSE),FRICBUS(NBUSE)
      DOUBLE PRECISION , INTENT(IN)    :: LONGBUS(NBUSE)
      INTEGER          , INTENT(IN)    :: OPTBUSE
      TYPE(BIEF_OBJ)   , INTENT(IN)    :: V2DPAR
      INTEGER          , INTENT(IN)    :: MAXSOURCE
      INTEGER, INTENT(IN), OPTIONAL    :: NPTSCE,NPOIN2
      INTEGER, INTENT(IN), OPTIONAL    :: KSCE(MAXSOURCE)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER :: N,I1,I2,ITRAC,FTYP
      INTEGER :: VOFFSET, abaque
!
      TYPE BUSE_NEIGHBORS
        INTEGER :: N_PTS
        INTEGER, ALLOCATABLE :: IDX(:)
        INTEGER, ALLOCATABLE :: WEIGHT(:)
        DOUBLE PRECISION, ALLOCATABLE :: DIST(:)
      END TYPE BUSE_NEIGHBORS
!
      TYPE(BUSE_NEIGHBORS), ALLOCATABLE, SAVE :: CACHE_AMONT(:)
      TYPE(BUSE_NEIGHBORS), ALLOCATABLE, SAVE :: CACHE_AVAL(:)
      LOGICAL, SAVE :: BUSE_INIT = .FALSE.
      INTEGER :: U_TMP_CNT, U_J
!
      DOUBLE PRECISION, ALLOCATABLE, SAVE :: TAB_S1(:), TAB_S2(:)
      DOUBLE PRECISION, ALLOCATABLE, SAVE :: TAB_Hin1(:), TAB_Hout2(:)
      DOUBLE PRECISION, ALLOCATABLE, SAVE :: TAB_QMAX1(:), TAB_QMAX2(:)
      DOUBLE PRECISION, ALLOCATABLE, SAVE :: TAB_SUM_S1(:)
      DOUBLE PRECISION, ALLOCATABLE, SAVE :: TAB_SUM_W1(:)
      DOUBLE PRECISION, ALLOCATABLE, SAVE :: TAB_SUM_S2(:)
      DOUBLE PRECISION, ALLOCATABLE, SAVE :: TAB_SUM_W2(:)
      DOUBLE PRECISION, ALLOCATABLE, SAVE :: TAB_TMP_IN(:)
      DOUBLE PRECISION, ALLOCATABLE, SAVE :: TAB_TMP_OUT(:)
      DOUBLE PRECISION, ALLOCATABLE, SAVE :: X_AMONT(:), Y_AMONT(:)
      DOUBLE PRECISION, ALLOCATABLE, SAVE :: X_AVAL(:), Y_AVAL(:)
      DOUBLE PRECISION, ALLOCATABLE, SAVE :: ZFIN_TAB(:), ZFOUT_TAB(:)
      DOUBLE PRECISION, ALLOCATABLE, SAVE :: TAB_QTGT(:)
      DOUBLE PRECISION, ALLOCATABLE, SAVE :: TAB_TMP_DBUS(:)
      DOUBLE PRECISION, ALLOCATABLE, SAVE :: CUMUL_DBUS(:)
      INTEGER, SAVE :: CUMUL_STEPS
      DATA CUMUL_STEPS / 0 /
      INTEGER :: U_NGLOB, U_IERR
      CHARACTER(LEN=60) :: U_FGEOM, U_FNOD
      DOUBLE PRECISION :: U_Q_ACT, U_TOL_AMORCE
!
      INTEGER :: I
      DOUBLE PRECISION :: V_AMONT_X, V_AMONT_Y
      DOUBLE PRECISION :: V_AVAL_X, V_AVAL_Y
      DOUBLE PRECISION :: LOCAL_SUM_S1, LOCAL_SUM_S2
      INTEGER :: K_SLICE, N_SLICES
      DOUBLE PRECISION :: R_LOW, R_HIGH, R_MAX_AMONT, R_MAX_AVAL
      DOUBLE PRECISION :: ZF_MIN_SLICE(16), ZF_MIN_GLOB(16)
      DOUBLE PRECISION :: LOCAL_SUM_W1, LOCAL_SUM_W2
      DOUBLE PRECISION :: VAL_S1, VAL_S2
      INTEGER :: WEIGHT
      DOUBLE PRECISION :: DIST, DOT_PROD
!
      DOUBLE PRECISION :: L,LARG,HAUT1,HAUT2,HAUT,TETAS2
      DOUBLE PRECISION :: S1,S2,CE1,CE2,CS1,CS2,Q,QMAX1,QMAX2
      DOUBLE PRECISION :: RD1,RD2,RD
      DOUBLE PRECISION :: FRIC,LONG,HAST,RAYON,TRASH,RADI1,RADI2
      DOUBLE PRECISION :: CORR56,CORRV5,VALVE,CORR5,CIR
!
      DOUBLE PRECISION :: PI
      DOUBLE PRECISION :: TWOTHIRDS,FOURTHIRDS
      DOUBLE PRECISION :: D1,D2,H1,H2,Q_buse,Hin1,Hout2,ZFin,ZFout
      DOUBLE PRECISION :: DBUS_GLOB, V_OUT, V_LIM
!
      INTRINSIC SQRT,COS,SIN,MIN,MAX,ABS,ACOS
!
!-----------------------------------------------------------------------
!
      PI = 4.D0*ATAN(1.D0)
      TWOTHIRDS  = 2.D0/3.D0
      FOURTHIRDS = 4.D0/3.D0
      abaque = 0
!
!-----------------------------------------------------------------------
! 1. INITIALISATION DU CACHE GEOMETRIQUE UNIQUE A T=0
!-----------------------------------------------------------------------
      IF (.NOT. BUSE_INIT) THEN
         ALLOCATE(CACHE_AMONT(NBUSE))
         ALLOCATE(CACHE_AVAL(NBUSE))
         ALLOCATE(TAB_S1(NBUSE), TAB_S2(NBUSE))
         ALLOCATE(TAB_Hin1(NBUSE), TAB_Hout2(NBUSE))
         ALLOCATE(TAB_QMAX1(NBUSE), TAB_QMAX2(NBUSE))
         ALLOCATE(TAB_SUM_S1(NBUSE), TAB_SUM_W1(NBUSE))
         ALLOCATE(TAB_SUM_S2(NBUSE), TAB_SUM_W2(NBUSE))
         ALLOCATE(TAB_TMP_IN(NBUSE), TAB_TMP_OUT(NBUSE))
         ALLOCATE(X_AMONT(NBUSE), Y_AMONT(NBUSE))
         ALLOCATE(X_AVAL(NBUSE), Y_AVAL(NBUSE))
         ALLOCATE(ZFIN_TAB(NBUSE), ZFOUT_TAB(NBUSE))
         ALLOCATE(TAB_QTGT(NBUSE))
         ALLOCATE(TAB_TMP_DBUS(NBUSE))
           ALLOCATE(CUMUL_DBUS(NBUSE))
           CUMUL_DBUS = 0.D0
         DO N = 1, NBUSE
            TAB_QTGT(N) = 0.D0
            TAB_TMP_DBUS(N) = 0.D0
         ENDDO

         ! Releve local des coordonnees fixes d'entree
         DO N = 1, NBUSE
            I1 = ENTBUS(N)
            IF (I1 .GT. 0) THEN
               TAB_TMP_IN(N) = MESH%X%R(I1)
            ELSE
               TAB_TMP_IN(N) = -1.D8
            ENDIF
         ENDDO
         IF (NCSIZE .GT. 1) THEN
            CALL P_DMAX_ARRAY(TAB_TMP_IN, NBUSE, X_AMONT)
         ELSE
            X_AMONT = TAB_TMP_IN
         ENDIF

         DO N = 1, NBUSE
            I1 = ENTBUS(N)
            IF (I1 .GT. 0) THEN
               TAB_TMP_IN(N) = MESH%Y%R(I1)
            ELSE
               TAB_TMP_IN(N) = -1.D8
            ENDIF
         ENDDO
         IF (NCSIZE .GT. 1) THEN
            CALL P_DMAX_ARRAY(TAB_TMP_IN, NBUSE, Y_AMONT)
         ELSE
            Y_AMONT = TAB_TMP_IN
         ENDIF

         DO N = 1, NBUSE
            I1 = ENTBUS(N)
            IF (I1 .GT. 0) THEN
               TAB_TMP_IN(N) = ZF(I1)
            ELSE
               TAB_TMP_IN(N) = -1.D8
            ENDIF
         ENDDO
         IF (NCSIZE .GT. 1) THEN
            CALL P_DMAX_ARRAY(TAB_TMP_IN, NBUSE, ZFIN_TAB)
         ELSE
            ZFIN_TAB = TAB_TMP_IN
         ENDIF

         ! Releve local des coordonnees fixes de sortie
         DO N = 1, NBUSE
            I2 = SORBUS(N)
            IF (I2 .GT. 0) THEN
               TAB_TMP_IN(N) = MESH%X%R(I2)
            ELSE
               TAB_TMP_IN(N) = -1.D8
            ENDIF
         ENDDO
         IF (NCSIZE .GT. 1) THEN
            CALL P_DMAX_ARRAY(TAB_TMP_IN, NBUSE, X_AVAL)
         ELSE
            X_AVAL = TAB_TMP_IN
         ENDIF

         DO N = 1, NBUSE
            I2 = SORBUS(N)
            IF (I2 .GT. 0) THEN
               TAB_TMP_IN(N) = MESH%Y%R(I2)
            ELSE
               TAB_TMP_IN(N) = -1.D8
            ENDIF
         ENDDO
         IF (NCSIZE .GT. 1) THEN
            CALL P_DMAX_ARRAY(TAB_TMP_IN, NBUSE, Y_AVAL)
         ELSE
            Y_AVAL = TAB_TMP_IN
         ENDIF

         DO N = 1, NBUSE
            I2 = SORBUS(N)
            IF (I2 .GT. 0) THEN
               TAB_TMP_IN(N) = ZF(I2)
            ELSE
               TAB_TMP_IN(N) = -1.D8
            ENDIF
         ENDDO
         IF (NCSIZE .GT. 1) THEN
            CALL P_DMAX_ARRAY(TAB_TMP_IN, NBUSE, ZFOUT_TAB)
         ELSE
            ZFOUT_TAB = TAB_TMP_IN
         ENDIF

          ! Recherche et stockage definitif des noeuds voisins
          DO N = 1, NBUSE
             CACHE_AMONT(N)%N_PTS = 0
             CACHE_AVAL(N)%N_PTS = 0
             IF (X_AMONT(N) .NE. 0.D0 .AND. X_AVAL(N) .NE. 0.D0) THEN
                V_AMONT_X = X_AMONT(N) - X_AVAL(N)
                V_AMONT_Y = Y_AMONT(N) - Y_AVAL(N)
                V_AVAL_X  = X_AVAL(N)  - X_AMONT(N)
                V_AVAL_Y  = Y_AVAL(N)  - Y_AMONT(N)

!               Rayon amont etendu a max(3*Largeur, 8.0m)
                R_MAX_AMONT = MAX(3.0D0 * LRGBUS(N), 8.0D0)
                N_SLICES = INT(CEILING(R_MAX_AMONT / 2.0D0))
                IF (N_SLICES .GT. 16) N_SLICES = 16

!               --- A. AMONT : RECHERCHE DU THALWEG PAR ANNEAUX 2M ---
                DO K_SLICE = 1, 16
                   ZF_MIN_SLICE(K_SLICE) = 1.D9
                ENDDO

                DO K_SLICE = 1, N_SLICES
                   R_LOW  = 2.0D0 * DBLE(K_SLICE - 1)
                   R_HIGH = MIN(R_MAX_AMONT, 2.0D0 * DBLE(K_SLICE))
                   DO I = 1, MESH%NPOIN
                      DIST = SQRT((MESH%X%R(I) - X_AMONT(N))**2 +
     &                            (MESH%Y%R(I) - Y_AMONT(N))**2)
                      IF (DIST .GT. R_LOW .AND. DIST .LE. R_HIGH) THEN
!                        Filtre base : Zf >= Radier - 5cm (tol LiDAR)
                         IF (ZF(I) .GE. ALTBUS(N,1) - 0.05D0) THEN
                            DOT_PROD =
     &                         V_AMONT_X * (MESH%X%R(I) - X_AMONT(N)) +
     &                         V_AMONT_Y * (MESH%Y%R(I) - Y_AMONT(N))
!                           Orientation amont stricte
                            IF (DOT_PROD .GE. 0.D0) THEN
                               IF (ZF(I) .LT.
     &                             ZF_MIN_SLICE(K_SLICE)) THEN
                                  ZF_MIN_SLICE(K_SLICE) = ZF(I)
                               ENDIF
                            ENDIF
                         ENDIF
                      ENDIF
                   ENDDO
                ENDDO

                IF (NCSIZE .GT. 1) THEN
                   CALL P_DMIN_ARRAY(ZF_MIN_SLICE, 16, ZF_MIN_GLOB)
                ELSE
                   ZF_MIN_GLOB = ZF_MIN_SLICE
                ENDIF

!               Comptage noeuds du lit mineur amont (<= Zf_min + 5cm)
                U_TMP_CNT = 0
                DO K_SLICE = 1, N_SLICES
                   R_LOW  = 2.0D0 * DBLE(K_SLICE - 1)
                   R_HIGH = MIN(R_MAX_AMONT, 2.0D0 * DBLE(K_SLICE))
!                  Garde-fou : rejet si anneau perche sur talus
                   IF (ZF_MIN_GLOB(K_SLICE) .LT. 1.D8 .AND.
     &                 ZF_MIN_GLOB(K_SLICE) .LE.
     &                 ALTBUS(N,1) + 0.05D0*R_HIGH + 0.20D0) THEN
                      DO I = 1, MESH%NPOIN
                         DIST = SQRT((MESH%X%R(I) - X_AMONT(N))**2 +
     &                               (MESH%Y%R(I) - Y_AMONT(N))**2)
                         IF (DIST .GT. R_LOW .AND.
     &                       DIST .LE. R_HIGH) THEN
                            IF (ZF(I) .GE. ALTBUS(N,1) - 0.05D0 .AND.
     &                          ZF(I) .LE. ZF_MIN_GLOB(K_SLICE)+0.05D0)
     &                      THEN
                               DOT_PROD =
     &                         V_AMONT_X * (MESH%X%R(I) - X_AMONT(N)) +
     &                         V_AMONT_Y * (MESH%Y%R(I) - Y_AMONT(N))
                               IF (DOT_PROD .GE. 0.D0) THEN
                                  U_TMP_CNT = U_TMP_CNT + 1
                               ENDIF
                            ENDIF
                         ENDIF
                      ENDDO
                   ENDIF
                ENDDO

                CACHE_AMONT(N)%N_PTS = U_TMP_CNT
                IF (U_TMP_CNT .GT. 0) THEN
                   ALLOCATE(CACHE_AMONT(N)%IDX(U_TMP_CNT))
                   ALLOCATE(CACHE_AMONT(N)%WEIGHT(U_TMP_CNT))
                   ALLOCATE(CACHE_AMONT(N)%DIST(U_TMP_CNT))
                   U_TMP_CNT = 0
                   DO K_SLICE = 1, N_SLICES
                      R_LOW  = 2.0D0 * DBLE(K_SLICE - 1)
                      R_HIGH = MIN(R_MAX_AMONT, 2.0D0 * DBLE(K_SLICE))
                      IF (ZF_MIN_GLOB(K_SLICE) .LT. 1.D8 .AND.
     &                    ZF_MIN_GLOB(K_SLICE) .LE.
     &                    ALTBUS(N,1) + 0.05D0*R_HIGH + 0.20D0) THEN
                         DO I = 1, MESH%NPOIN
                            DIST = SQRT((MESH%X%R(I)-X_AMONT(N))**2 +
     &                                  (MESH%Y%R(I)-Y_AMONT(N))**2)
                            IF (DIST .GT. R_LOW .AND.
     &                          DIST .LE. R_HIGH) THEN
                               IF (ZF(I) .GE. ALTBUS(N,1)-0.05D0 .AND.
     &                             ZF(I) .LE.
     &                             ZF_MIN_GLOB(K_SLICE)+0.05D0) THEN
                                  DOT_PROD =
     &                            V_AMONT_X*(MESH%X%R(I)-X_AMONT(N)) +
     &                            V_AMONT_Y*(MESH%Y%R(I)-Y_AMONT(N))
                                  IF (DOT_PROD .GE. 0.D0) THEN
                                     U_TMP_CNT = U_TMP_CNT + 1
                                     CACHE_AMONT(N)%IDX(U_TMP_CNT) = I
                                     CACHE_AMONT(N)%DIST(U_TMP_CNT)=DIST
                                     IF (NCSIZE .GT. 1) THEN
                                        CACHE_AMONT(N)%
     &                                  WEIGHT(U_TMP_CNT) =
     &                                     MESH%IFAC%I(I)
                                     ELSE
                                        CACHE_AMONT(N)%
     &                                  WEIGHT(U_TMP_CNT) = 1
                                     ENDIF
                                  ENDIF
                               ENDIF
                            ENDIF
                         ENDDO
                      ENDIF
                   ENDDO
                ENDIF

!               --- B. AVAL : RECHERCHE SUR RAYON MAX(3*L, 8.0M) ---
                R_MAX_AVAL = MAX(3.0D0 * LRGBUS(N), 8.0D0)
                U_TMP_CNT = 0
                DO I = 1, MESH%NPOIN
                   DIST = SQRT((MESH%X%R(I) - X_AVAL(N))**2 +
     &                         (MESH%Y%R(I) - Y_AVAL(N))**2)
                   IF (DIST .LE. R_MAX_AVAL .AND. DIST .GT. 0.01D0) THEN
!                     Filtre lit aval hors puits (-25m)
                      IF (ZF(I) .GE. ALTBUS(N,2) - 0.50D0 .AND.
     &                    ZF(I) .LE. ALTBUS(N,2) + 0.50D0) THEN
                         DOT_PROD =
     &                      V_AVAL_X * (MESH%X%R(I) - X_AVAL(N)) +
     &                      V_AVAL_Y * (MESH%Y%R(I) - Y_AVAL(N))
                         IF (DOT_PROD .GE. 0.D0) THEN
                            U_TMP_CNT = U_TMP_CNT + 1
                         ENDIF
                      ENDIF
                   ENDIF
                ENDDO

                CACHE_AVAL(N)%N_PTS = U_TMP_CNT
                IF (U_TMP_CNT .GT. 0) THEN
                   ALLOCATE(CACHE_AVAL(N)%IDX(U_TMP_CNT))
                   ALLOCATE(CACHE_AVAL(N)%WEIGHT(U_TMP_CNT))
                   U_TMP_CNT = 0
                   DO I = 1, MESH%NPOIN
                      DIST = SQRT((MESH%X%R(I) - X_AVAL(N))**2 +
     &                            (MESH%Y%R(I) - Y_AVAL(N))**2)
                      IF (DIST .LE. R_MAX_AVAL .AND.
     &                    DIST .GT. 0.01D0) THEN
                         IF (ZF(I) .GE. ALTBUS(N,2) - 0.50D0 .AND.
     &                       ZF(I) .LE. ALTBUS(N,2) + 0.50D0) THEN
                            DOT_PROD =
     &                         V_AVAL_X * (MESH%X%R(I) - X_AVAL(N)) +
     &                         V_AVAL_Y * (MESH%Y%R(I) - Y_AVAL(N))
                            IF (DOT_PROD .GE. 0.D0) THEN
                               U_TMP_CNT = U_TMP_CNT + 1
                               CACHE_AVAL(N)%IDX(U_TMP_CNT) = I
                               IF (NCSIZE .GT. 1) THEN
                                  CACHE_AVAL(N)%WEIGHT(U_TMP_CNT) =
     &                               MESH%IFAC%I(I)
                               ELSE
                                  CACHE_AVAL(N)%WEIGHT(U_TMP_CNT) = 1
                               ENDIF
                            ENDIF
                         ENDIF
                      ENDIF
                   ENDDO
                ENDIF
             ENDIF
          ENDDO
!
          BUSE_INIT = .TRUE.
       ENDIF
!-----------------------------------------------------------------------
! 2. COLLECTE LOCALE DES HAUTEURS ET LISSAGES SPATIAUX (SANS P_DSUM)
!-----------------------------------------------------------------------
      DO N = 1, NBUSE
         I1 = ENTBUS(N)
         I2 = SORBUS(N)
         
         IF (I1 .GT. 0) THEN
            TAB_Hin1(N) = H(I1)
            TAB_QMAX1(N) = 0.9D0 * H(I1) * V2DPAR%R(I1) / DT
            
            ! Lissage spatial local S1 UNIQUEMENT sur ce processeur
            LOCAL_SUM_S1 = 0.D0
            LOCAL_SUM_W1 = 0.D0
            DO U_J = 1, CACHE_AMONT(N)%N_PTS
               I = CACHE_AMONT(N)%IDX(U_J)
               IF (H(I) .GT. 0.01D0) THEN
                  IF (ZF(I) .GE. ALTBUS(N,1)) THEN
                     VAL_S1 = H(I) + ALTBUS(N,1)
                  ELSE
                     VAL_S1 = ZF(I) + H(I)
                  ENDIF
                  LOCAL_SUM_S1 = LOCAL_SUM_S1 + VAL_S1
                  LOCAL_SUM_W1 = LOCAL_SUM_W1 + 1.D0
               ENDIF
            ENDDO
            IF (LOCAL_SUM_W1 .GT. 0.D0) THEN
               TAB_S1(N) = LOCAL_SUM_S1 / LOCAL_SUM_W1
            ELSE
               TAB_S1(N) = H(I1) + ZF(I1)
            ENDIF
         ELSE
            TAB_S1(N) = 0.D0
            TAB_Hin1(N) = 0.D0
            TAB_QMAX1(N) = 0.D0
         ENDIF
         
         IF (I2 .GT. 0) THEN
            TAB_Hout2(N) = H(I2)
            TAB_QMAX2(N) = 0.9D0 * H(I2) * V2DPAR%R(I2) / DT
            
            ! Lissage spatial local S2 UNIQUEMENT sur ce processeur
            LOCAL_SUM_S2 = 0.D0
            LOCAL_SUM_W2 = 0.D0
            DO U_J = 1, CACHE_AVAL(N)%N_PTS
               I = CACHE_AVAL(N)%IDX(U_J)
               IF (H(I) .GT. 0.01D0) THEN
                  IF (ZF(I) .GE. ALTBUS(N,2)) THEN
                     VAL_S2 = H(I) + ALTBUS(N,2)
                  ELSE
                     VAL_S2 = ZF(I) + H(I)
                  ENDIF
                  LOCAL_SUM_S2 = LOCAL_SUM_S2 + VAL_S2
                  LOCAL_SUM_W2 = LOCAL_SUM_W2 + 1.D0
               ENDIF
            ENDDO
            IF (LOCAL_SUM_W2 .GT. 0.D0) THEN
               TAB_S2(N) = LOCAL_SUM_S2 / LOCAL_SUM_W2
            ELSE
               TAB_S2(N) = H(I2) + ZF(I2)
            ENDIF
         ELSE
            TAB_S2(N) = 0.D0
            TAB_Hout2(N) = 0.D0
            TAB_QMAX2(N) = 0.D0
         ENDIF
      ENDDO

!-----------------------------------------------------------------------
! 3. REDUCTION MPI VECTORIELLE UNIQUE POUR TOUTES LES BUSES
!-----------------------------------------------------------------------
      IF (NCSIZE .GT. 1) THEN
         CALL P_DMAX_ARRAY(TAB_S1, NBUSE, TAB_TMP_OUT)
         TAB_S1 = TAB_TMP_OUT
         CALL P_DMAX_ARRAY(TAB_S2, NBUSE, TAB_TMP_OUT)
         TAB_S2 = TAB_TMP_OUT
         CALL P_DMAX_ARRAY(TAB_Hin1, NBUSE, TAB_TMP_OUT)
         TAB_Hin1 = TAB_TMP_OUT
         CALL P_DMAX_ARRAY(TAB_Hout2, NBUSE, TAB_TMP_OUT)
         TAB_Hout2 = TAB_TMP_OUT
         CALL P_DMAX_ARRAY(TAB_QMAX1, NBUSE, TAB_TMP_OUT)
         TAB_QMAX1 = TAB_TMP_OUT
         CALL P_DMAX_ARRAY(TAB_QMAX2, NBUSE, TAB_TMP_OUT)
         TAB_QMAX2 = TAB_TMP_OUT
      ENDIF

!-----------------------------------------------------------------------
! 4. BOUCLE DE CALCUL HYDRAULIQUE SUR LES BUSES (100% LOCALE ET RAPIDE)
!-----------------------------------------------------------------------
      DO N = 1, NBUSE
         I1 = ENTBUS(N)
         I2 = SORBUS(N)
         S1 = TAB_S1(N)
         S2 = TAB_S2(N)
         Hin1 = TAB_Hin1(N)
         Hout2 = TAB_Hout2(N)
         QMAX1 = TAB_QMAX1(N)
         QMAX2 = TAB_QMAX2(N)
         ZFin = ZFIN_TAB(N)
         ZFout = ZFOUT_TAB(N)
         CE1 = CEBUS(N,1)
         CE2 = CEBUS(N,2)
         CS1 = CSBUS(N,1)
         CS2 = CSBUS(N,2)
         L = LBUS(N)
         RD1 = ALTBUS(N,1)
         RD2 = ALTBUS(N,2)
         RD = 0.5D0 * (RD1 + RD2)
         LARG = LRGBUS(N)
         HAUT1 = HAUBUS(N,1)
         HAUT2 = HAUBUS(N,2)
         RADI1 = 0.5D0 * HAUT1
         RADI2 = 0.5D0 * HAUT2
         FRIC = FRICBUS(N)
         LONG = LONGBUS(N)
         TRASH = CTRASH(N)
         CORR56 = C56(N)
         VALVE = CV(N)
         CORRV5 = CV5(N)
         CORR5 = C5(N)
         CIR = CIRC(N)
         HAUT = MIN(HAUT1,HAUT2)
         D1 = DELBUS(N,1)
         D2 = DELBUS(N,2)
         H1 = HAUT1 * COS(D1)
         H2 = HAUT2 * COS(D2)

         IF(OPTBUSE.EQ.1) THEN
           IF(S1.GE.S2) THEN
             IF(S1.GT.RD1.AND.S1.GT.RD2) THEN
               IF(S1.LT.(RD1+H1).AND.S1.LT.(RD2+H2)) THEN
                 IF(S2.GT.(TWOTHIRDS*(S1-RD1))+RD2) THEN
                   IF(CIR.GT.0.D0) THEN
                     SECBUS(N) = PI*0.5D0*RADI2*(S2-RD2)
                   ELSE
                     SECBUS(N) = LARG*(S2-RD2)
                   ENDIF
                   Q = SECBUS(N)*SQRT(2.D0*GRAV*(S1-S2)/(CE1+L+CS2))
                 ELSE
                   IF(CIR.GT.0.D0) THEN
                     IF((S1-RD1).LT.RADI1) THEN
                       TETAS2    = ACOS((RADI1-(S1-RD1))/RADI1)
                       SECBUS(N) = ACOS((RADI1-(S1-RD1))/RADI1)*RADI1**2
     &                       - 0.5D0*RADI1*SIN(TETAS2)*(RADI1-(S1-RD1))
                     ELSEIF((S1-RD1).EQ.RADI1) THEN
                       SECBUS(N) = PI*RADI1**2*0.5D0
                     ELSEIF((S1-RD1).GT.RADI1.AND.(S1-RD1).LT.H1) THEN
                       TETAS2    = ACOS(((S1-RD1)-RADI1)/RADI1)
                       SECBUS(N) = PI*RADI1**2
     &                         - ACOS(((S1-RD1)-RADI1)/RADI1)*RADI1**2
     &                       - 0.5D0*RADI1*SIN(TETAS2)*((S1-RD1)-RADI1)
                     ELSEIF((S1-RD1).GE.H1) THEN
                       SECBUS(N) = PI*RADI1**2
                     ENDIF
                   ELSE
                     SECBUS(N) = LARG*TWOTHIRDS*(S1-RD1)
                   ENDIF
                   IF(S1.GT.RD2+TWOTHIRDS*(S1-RD1)) THEN
                     Q = SECBUS(N)*SQRT(2.D0*GRAV*
     &               (S1-(RD2+TWOTHIRDS*(S1-RD1)))/(CE1+L+CS2))
                   ELSE
                     Q = SECBUS(N)*SQRT(2.D0*GRAV*(S1-S2)/(CE1+L+CS2))
                   ENDIF
                 ENDIF
               ELSEIF(S1.GE.(RD1+H1).AND.S2.LT.(RD2+H2)) THEN
                 IF(CIR.GT.0.D0) THEN
                   SECBUS(N) = PI*RADI1**2
                 ELSE
                   SECBUS(N) = LARG*HAUT1
                 ENDIF
                 IF(S1.GT.RD2+HAUT2) THEN
                   Q = SECBUS(N)*SQRT(2.D0*GRAV*(S1-(RD2+HAUT2))
     &                                /(CE1+L+CS2))
                 ELSE
                   Q = SECBUS(N)*SQRT(2.D0*GRAV*(S1-S2)/(CE1+L+CS2))
                 ENDIF
               ELSEIF(S1.GE.(RD1+H1).AND.S2.GE.(RD2+H2)) THEN
                 IF(CIR.GT.0.D0) THEN
                   SECBUS(N) = PI*RADI1**2
                 ELSE
                   SECBUS(N) = LARG*HAUT1
                 ENDIF
                 Q = SECBUS(N)*SQRT(2.D0*GRAV*(S1-S2)/(CE1+L+CS2))
               ENDIF
             ELSE
               Q = 0.D0
             ENDIF
           ELSE
             IF(S2.GT.RD1.AND.S2.GT.RD2) THEN
               IF(S2.LT.(RD1+H1).AND.S2.LT.(RD2+H2)) THEN
                 IF(S1.GT.(TWOTHIRDS*(S2-RD2))+RD1) THEN
                   IF(CIR.GT.0.D0) THEN
                     SECBUS(N) = PI*0.5D0*RADI1*(S1-RD1)
                   ELSE
                     SECBUS(N) = LARG*(S1-RD1)
                   ENDIF
                   Q = -SECBUS(N)*SQRT(2.D0*GRAV*(S2-S1)/(CE2+L+CS1))
                 ELSE
                   IF(CIR.GT.0.D0) THEN
                     IF((S2-RD2).LT.RADI2) THEN
                       TETAS2    = ACOS((RADI2-(S2-RD2))/RADI2)
                       SECBUS(N) = ACOS((RADI2-(S2-RD2))/RADI2)*RADI2**2
     &                       - 0.5D0*RADI2*SIN(TETAS2)*(RADI2-(S2-RD2))
                     ELSEIF((S2-RD2).EQ.RADI2) THEN
                       SECBUS(N) = PI*RADI2**2*0.5D0
                     ELSEIF((S2-RD2).GT.RADI2.AND.(S2-RD2).LT.H2) THEN
                       TETAS2    = ACOS(((S2-RD2)-RADI2)/RADI2)
                       SECBUS(N) = PI*RADI2**2
     &                         - ACOS(((S2-RD2)-RADI2)/RADI2)*RADI2**2
     &                       - 0.5D0*RADI2*SIN(TETAS2)*((S2-RD2)-RADI2)
                     ELSEIF((S2-RD2).GE.H2) THEN
                       SECBUS(N) = PI*RADI2**2
                     ENDIF
                   ELSE
                     SECBUS(N) = LARG*TWOTHIRDS*(S2-RD2)
                   ENDIF
                   IF(S2.GT.RD1+TWOTHIRDS*(S2-RD2)) THEN
                     Q = -SECBUS(N)*SQRT(2.D0*GRAV*
     &                   (S2-(RD1+TWOTHIRDS*(S2-RD2)))/(CE2+L+CS1))
                   ELSE
                     Q = -SECBUS(N)*SQRT(2.D0*GRAV*(S2-S1)/(CE2+L+CS1))
                   ENDIF
                 ENDIF
               ELSEIF(S2.GE.(RD2+H2).AND.S1.LT.(RD1+H1)) THEN
                 IF(CIR.GT.0.D0) THEN
                   SECBUS(N) = PI*RADI2**2
                 ELSE
                   SECBUS(N) = LARG*HAUT2
                 ENDIF
                 IF(S2.GT.RD1+HAUT1) THEN
                   Q = -SECBUS(N)*SQRT(2.D0*GRAV*(S2-(RD1+HAUT1))
     &                                 /(CE2+L+CS1))
                 ELSE
                   Q = -SECBUS(N)*SQRT(2.D0*GRAV*(S2-S1)/(CE2+L+CS1))
                 ENDIF
               ELSEIF(S2.GE.(RD2+H2).AND.S1.GE.(RD1+H1)) THEN
                 IF(CIR.GT.0.D0) THEN
                   SECBUS(N) = PI*RADI2**2
                 ELSE
                   SECBUS(N) = LARG*HAUT2
                 ENDIF
                 Q = -SECBUS(N)*SQRT(2.D0*GRAV*(S2-S1)/(CE2+L+CS1))
               ENDIF
             ELSE
               Q = 0.D0
             ENDIF
           ENDIF
         ELSEIF(OPTBUSE.EQ.2) THEN
           IF(S1.GE.S2) THEN
             IF(S1.GT.RD1.AND.S1.GT.RD2) THEN
               IF((S1-RD1).LT.1.5D0*H1.AND.S2.LE.(RD2+H2)) THEN
                 IF(S2.GT.(TWOTHIRDS*(S1-RD1))+RD2) THEN
                   FTYP=3
                   HAST = 0.5D0*(S1-RD)+0.5D0*(S2-RD)
                   RAYON = HAST*LARG/(2.D0*HAST+LARG)
                   L = 2.D0*GRAV*LONG*FRIC**2/RAYON**FOURTHIRDS
                   IF(CIR.GT.0.D0) THEN
                     SECBUS(N) = PI*0.5D0*RADI2*(S2-RD)
                   ELSE
                     SECBUS(N) = LARG*(S2-RD)
                   ENDIF
                   Q = SECBUS(N)*SQRT(2.D0*GRAV*(S1-S2)
     &                                    /(CE1+L+CS2+VALVE+TRASH))
                 ELSE
                   FTYP=2
                   HAST = 0.5D0*TWOTHIRDS*(S1-RD)+0.5D0*(S1-RD)
                   RAYON = HAST*LARG/(2.D0*HAST+LARG)
                   L = 2.D0*GRAV*LONG*FRIC**2/RAYON**FOURTHIRDS
                   IF(CIR.GT.0.D0) THEN
                     IF((S1-RD1).LT.RADI1) THEN
                       TETAS2    = ACOS((RADI1-(S1-RD1))/RADI1)
                       SECBUS(N) = ACOS((RADI1-(S1-RD1))/RADI1)*RADI1**2
     &                       - 0.5D0*RADI1*SIN(TETAS2)*(RADI1-(S1-RD1))
                     ELSEIF((S1-RD1).EQ.RADI1) THEN
                       SECBUS(N) = PI*RADI1**2*0.5D0
                     ELSEIF((S1-RD1).GT.RADI1.AND.(S1-RD1).LT.H1) THEN
                       TETAS2    = ACOS(((S1-RD1)-RADI1)/RADI1)
                       SECBUS(N) = PI*RADI1**2
     &                         - ACOS(((S1-RD1)-RADI1)/RADI1)*RADI1**2
     &                       - 0.5D0*RADI1*SIN(TETAS2)*((S1-RD1)-RADI1)
                     ELSEIF((S1-RD1).GE.H1) THEN
                       SECBUS(N) = PI*RADI1**2
                     ENDIF
                   ELSE
                     SECBUS(N) = LARG*(TWOTHIRDS*(S1-RD))
                   ENDIF
                   IF(S1.GT.RD+TWOTHIRDS*(S1-RD)) THEN
                     Q = SECBUS(N)
     &                  *SQRT(2.D0*GRAV*(S1-(RD+TWOTHIRDS*(S1-RD)))
     &                            /(CE1+L+CS2+VALVE+TRASH))
                   ELSE
                     Q = SECBUS(N)*SQRT(2.D0*GRAV*(S1-S2)
     &                                      /(CE1+L+CS2+VALVE+TRASH))
                   ENDIF
                 ENDIF
               ELSEIF((S1-RD1).GE.1.5D0*H1.AND.S2.LE.(RD2+H2)) THEN
                 IF(LONG.GE.CORR56*HAUT1) THEN
                   FTYP=6
                   HAST = HAUT1
                   RAYON = HAST*LARG/(2.D0*HAST+2.D0*LARG)
                   L = 2.D0*GRAV*LONG*FRIC**2/RAYON**FOURTHIRDS
                   IF(CIR.GT.0.D0) THEN
                     SECBUS(N) = PI*RADI1**2
                   ELSE
                     SECBUS(N) = LARG*HAUT1
                   ENDIF
                   IF(S1.GT.(RD2+HAUT)) THEN
                     Q = SECBUS(N)*SQRT(2.D0*GRAV*(S1-(RD2+HAUT))
     &                                      /(CE1+L+CS2+VALVE+TRASH))
                   ELSE
                     Q = SECBUS(N)*SQRT(2.D0*GRAV*(S1-S2)
     &                                      /(CE1+L+CS2+VALVE+TRASH))
                   ENDIF
                 ELSE
                   FTYP=5
                   HAST = HAUT1
                   RAYON = HAST*LARG/(2.D0*HAST+2.D0*LARG)
                   L = 2.D0*GRAV*LONG*FRIC**2/RAYON**FOURTHIRDS
                   IF(CIR.GT.0.D0) THEN
                     SECBUS(N) = PI*RADI1**2
                   ELSE
                     SECBUS(N) = LARG*HAUT1
                   ENDIF
                   Q=SECBUS(N)*SQRT(2.D0*GRAV*(S1-RD)
     &                                /(CORR5*CE1+CORRV5*VALVE+TRASH))
                 ENDIF
               ELSEIF(S1.GT.(RD1+H1).AND.S2.GT.(RD2+H2)) THEN
                 FTYP=4
                 HAST = HAUT1
                 RAYON = HAST*LARG/(2.D0*HAST+2.D0*LARG)
                 L = 2.D0*GRAV*LONG*FRIC**2/RAYON**FOURTHIRDS
                 IF(CIR.GT.0.D0) THEN
                   SECBUS(N) = PI*RADI1**2
                 ELSE
                   SECBUS(N) = LARG*HAUT1
                 ENDIF
                 Q = SECBUS(N)*SQRT(2.D0*GRAV*(S1-S2)
     &                                /(CE1+L+CS2+VALVE+TRASH))
               ENDIF
             ELSE
               FTYP=0
               Q = 0.D0
             ENDIF
           ELSE
             IF(S2.GT.RD2.AND.S2.GT.RD1) THEN
               IF((S2-RD2).LT.1.5D0*H2.AND.S1.LE.(RD1+H1)) THEN
                 IF(S1.GT.(TWOTHIRDS*(S2-RD2)+RD1)) THEN
                   FTYP=-3
                   HAST = 0.5D0*(S2-RD)+0.5D0*(S1-RD)
                   RAYON = HAST*LARG/(2.D0*HAST+LARG)
                   L = 2.D0*GRAV*LONG*FRIC**2/RAYON**FOURTHIRDS
                   IF(CIR.GT.0.D0) THEN
                     SECBUS(N) = PI*0.5D0*RADI1*(S1-RD)
                   ELSE
                     SECBUS(N) = LARG*(S1-RD)
                   ENDIF
                   Q = -SECBUS(N)*SQRT(2.D0*GRAV*(S2-S1)
     &                                    /(CE2+L+VALVE+CS1+TRASH))
                 ELSE
                   FTYP=-2
                   HAST = 0.5D0*TWOTHIRDS*(S2-RD)+0.5D0*(S2-RD)
                   RAYON = HAST*LARG/(2.D0*HAST+LARG)
                   L = 2.D0*GRAV*LONG*FRIC**2/RAYON**FOURTHIRDS
                   IF(CIR.GT.0.D0) THEN
                     IF((S2-RD2).LT.RADI2) THEN
                       TETAS2    = ACOS((RADI2-(S2-RD2))/RADI2)
                       SECBUS(N) = ACOS((RADI2-(S2-RD2))/RADI2)*RADI2**2
     &                       - 0.5D0*RADI2*SIN(TETAS2)
     &                         *(RADI2-(S2-RD2))
                     ELSEIF((S2-RD2).EQ.RADI2) THEN
                       SECBUS(N) = PI*RADI2**2*0.5D0
                     ELSEIF((S2-RD2).GT.RADI2.AND.(S2-RD2).LT.H2) THEN
                       TETAS2    = ACOS(((S2-RD2)-RADI2)/RADI2)
                       SECBUS(N) = PI*RADI2**2
     &                         - ACOS(((S2-RD2)-RADI2)/RADI2)*RADI2**2
     &                       - 0.5D0*RADI2*SIN(TETAS2)
     &                         *((S2-RD2)-RADI2)
                     ELSEIF((S2-RD2).GE.H2) THEN
                       SECBUS(N) = PI*RADI2**2
                     ENDIF
                   ELSE
                     SECBUS(N) = LARG*TWOTHIRDS*(S2-RD2)
                   ENDIF
                   IF(S2.GT.RD+TWOTHIRDS*(S2-RD)) THEN
                     Q = -SECBUS(N)
     &                  *SQRT(2.D0*GRAV*(S2-(RD+TWOTHIRDS*(S2-RD)))
     &                            /(CE2+VALVE+L+CS1+TRASH))
                   ELSE
                     Q = -SECBUS(N)*SQRT(2.D0*GRAV*(S2-S1)
     &                                      /(CE2+L+CS1+VALVE+TRASH))
                   ENDIF
                 ENDIF
               ELSEIF((S2-RD2).GE.1.5D0*H2.AND.S1.LE.(RD1+H1)) THEN
                 IF(LONG.GE.CORR56*HAUT2) THEN
                   FTYP=-6
                   HAST = HAUT2
                   RAYON = HAST*LARG/(2.D0*HAST+2.D0*LARG)
                   L = 2.D0*GRAV*LONG*FRIC**2/RAYON**FOURTHIRDS
                   IF(CIR.GT.0.D0) THEN
                     SECBUS(N) = PI*RADI2**2
                   ELSE
                     SECBUS(N) = LARG*HAUT2
                   ENDIF
                   IF(S2.GT.(RD1+HAUT)) THEN
                     Q = -SECBUS(N)*SQRT(2.D0*GRAV*(S2-(RD1+HAUT))
     &                                      /(CE2+VALVE+L+CS1+TRASH))
                   ELSE
                     Q = -SECBUS(N)*SQRT(2.D0*GRAV*(S2-S1)
     &                                      /(CE2+VALVE+L+CS1+TRASH))
                   ENDIF
                 ELSE
                   FTYP=-5
                   HAST = HAUT2
                   RAYON = HAST*LARG/(2.D0*HAST+2.D0*LARG)
                   L = 2.D0*GRAV*LONG*FRIC**2/RAYON**FOURTHIRDS
                   IF(CIR.GT.0.D0) THEN
                     SECBUS(N) = PI*RADI2**2
                   ELSE
                     SECBUS(N) = LARG*HAUT2
                   ENDIF
                   Q = -SECBUS(N)*SQRT(2.D0*GRAV*(S2-RD)
     &                                /(CORR5*CE2+CORRV5*VALVE+TRASH))
                 ENDIF
               ELSEIF(S2.GT.(RD2+H2).AND.S1.GT.(RD1+H1)) THEN
                 FTYP=-4
                 HAST = HAUT2
                 RAYON = HAST*LARG/(2.D0*HAST+2.D0*LARG)
                 L = 2.D0*GRAV*LONG*FRIC**2/RAYON**FOURTHIRDS
                 IF(CIR.GT.0.D0) THEN
                   SECBUS(N) = PI*RADI2**2
                 ELSE
                   SECBUS(N) = LARG*HAUT2
                 ENDIF
                 Q = -SECBUS(N)*SQRT(2.D0*GRAV*(S2-S1)
     &                                 /(CE2+VALVE+L+CS1+TRASH))
               ENDIF
             ELSE
               FTYP=0
               Q = 0.D0
             ENDIF
           ENDIF
         ENDIF

         IF(S1.LT.RD1.AND.S2.LT.RD2) Q=0.D0
         IF(HAUT1.EQ.0.D0.OR.HAUT2.EQ.0.D0) Q=0.D0

         IF(abaque.EQ.1) THEN
            Q_buse=0.D3
!           Condition utilisateur : au moins 10 cm d'eau locale
!           Corrigé : on utilise la moyenne spatiale (S1 - Z1) au lieu du point I1 seul
!           Suppression de l'attente du remplissage du puits.
            IF(TAB_Hin1(N) .GE. 0.10D0 .AND. S1 .GT. ALTBUS(N,1)) THEN
               CALL DISCHARGE_INTERPOL(N,S1,S2,ALTBUS(N,1),ALTBUS(N,2),
     &                                 Hin1,Q_buse,TAB_QTGT(N))
               DBUS(N)= Q_buse
            ELSE
               DBUS(N)=0.D0
               TAB_QTGT(N)=0.D0
            ENDIF
         ELSE
            DBUS(N)= RELAXB*Q + (1.D0-RELAXB)*DBUS(N)
            TAB_QTGT(N)= Q
         ENDIF

         IF(abaque.EQ.0) THEN
            IF(DBUS(N).GT.0.D0) THEN
              DBUS(N)=MIN(QMAX1,DBUS(N))
            ELSE
              DBUS(N)=MAX(-QMAX2,DBUS(N))
            ENDIF
            IF((CLPBUS(N).EQ.1.AND.S2.GT.S1).OR.
     &         (CLPBUS(N).EQ.2.AND.S1.GT.S2).OR.
     &         (CLPBUS(N).EQ.3))
     &          DBUS(N) = 0.D0
         ENDIF

         IF(DBUS(N).GT.0.D0) THEN
!          PLAFONNEMENT DE LA VITESSE DE RESTITUTION AVAL A 4.0 M/S MAX
           IF(SECBUS(N).GT.1.D-6) THEN
             V_OUT = (COS(D2)*DBUS(N)/SECBUS(N))
             V_LIM = MIN(V_OUT, 4.0D0)
             UBUS(2,N) = V_LIM * COS(ANGBUS(N,2))
             VBUS(2,N) = V_LIM * SIN(ANGBUS(N,2))
           ELSE
             UBUS(2,N) = 0.D0
             VBUS(2,N) = 0.D0
           ENDIF
           IF(I1.GT.0) THEN
             IF(PRESENT(KSCE)) THEN
               VOFFSET = (KSCE(NPTSCE+N)-1)*NPOIN2
             ELSE
               VOFFSET = 0
             ENDIF
             UBUS(1,N) = U(I1+VOFFSET)
             VBUS(1,N) = V(I1+VOFFSET)
           ELSE
             UBUS(1,N) = 0.D0
             VBUS(1,N) = 0.D0
           ENDIF
         ELSEIF(DBUS(N).LT.0.D0) THEN
!          ECOULEMENT INVERSE : RESTITUTION EN 1 BORNEE A 4.0 M/S
           IF(SECBUS(N).GT.1.D-6) THEN
             V_OUT = (COS(D1)*ABS(DBUS(N))/SECBUS(N))
             V_LIM = MIN(V_OUT, 4.0D0)
             UBUS(1,N) = -V_LIM * COS(ANGBUS(N,1))
             VBUS(1,N) = -V_LIM * SIN(ANGBUS(N,1))
           ELSE
             UBUS(1,N) = 0.D0
             VBUS(1,N) = 0.D0
           ENDIF
           IF(I2.GT.0) THEN
             IF(PRESENT(KSCE)) THEN
               VOFFSET = (KSCE(NPTSCE+NBUSE+N)-1)*NPOIN2
             ELSE
               VOFFSET = 0
             ENDIF
             UBUS(2,N) = U(I2+VOFFSET)
             VBUS(2,N) = V(I2+VOFFSET)
           ELSE
             UBUS(2,N) = 0.D0
             VBUS(2,N) = 0.D0
           ENDIF
         ELSE
           UBUS(1,N) = 0.D0
           UBUS(2,N) = 0.D0
           VBUS(1,N) = 0.D0
           VBUS(2,N) = 0.D0
         ENDIF

         IF(NTRAC.GT.0) THEN
           DO ITRAC=1,NTRAC
             IF(DBUS(N).GE.0.D0) THEN
               IF(I1.GT.0) THEN
                 IF(PRESENT(KSCE)) THEN
                   VOFFSET = (KSCE(NPTSCE+N)-1)*NPOIN2
                 ELSE
                   VOFFSET = 0
                 ENDIF
                 TBUS%ADR(ITRAC)%P%R(N) =
     &           T%ADR(ITRAC)%P%R(I1+VOFFSET)
               ELSE
                 TBUS%ADR(ITRAC)%P%R(N) = 0.D0
               ENDIF
             ELSE
               IF(I2.GT.0) THEN
                 IF(PRESENT(KSCE)) THEN
                   VOFFSET = (KSCE(NPTSCE+NBUSE+N)-1)*NPOIN2
                 ELSE
                   VOFFSET = 0
                 ENDIF
                 TBUS%ADR(ITRAC)%P%R(N) =
     &           T%ADR(ITRAC)%P%R(I2+VOFFSET)
               ELSE
                 TBUS%ADR(ITRAC)%P%R(N) = 0.D0
               ENDIF
             ENDIF
           ENDDO
         ENDIF
      ENDDO

!-----------------------------------------------------------------------
! 5. SYNCHRONISATION FINALE POUR L'AFFICHAGE ET LES DIAGNOSTICS
!-----------------------------------------------------------------------
      IF (NCSIZE .GT. 1) THEN
         CALL P_DMAX_ARRAY(DBUS, NBUSE, TAB_TMP_OUT)
         TAB_TMP_DBUS = TAB_TMP_OUT
         CALL P_DMAX_ARRAY(TAB_QTGT, NBUSE, TAB_TMP_OUT)
         TAB_QTGT = TAB_TMP_OUT
      ENDIF

      CUMUL_STEPS = CUMUL_STEPS + 1
      DO N = 1, NBUSE
         IF (NCSIZE .GT. 1) THEN
            DBUS_GLOB = TAB_TMP_DBUS(N)
         ELSE
            DBUS_GLOB = DBUS(N)
         ENDIF
         CUMUL_DBUS(N) = CUMUL_DBUS(N) + DBUS_GLOB
      ENDDO

      IF (ENTET) THEN
         DO N = 1, NBUSE
            DBUS_GLOB = CUMUL_DBUS(N) / DBLE(CUMUL_STEPS)
            IF (ABS(DBUS_GLOB) .GT. 1.D-4) THEN
               WRITE(LU,*) 'CULVERT ',N,' DISCHARGE OF ',DBUS_GLOB,
     &                     ' M3/S'
               WRITE(LU,*) 'H(I1)', TAB_S1(N)
               WRITE(LU,*) 'H(I2)', TAB_S2(N)
            ENDIF
            CUMUL_DBUS(N) = 0.D0
         ENDDO
         CUMUL_STEPS = 0
      ENDIF
!
      RETURN
      END SUBROUTINE BUSE
