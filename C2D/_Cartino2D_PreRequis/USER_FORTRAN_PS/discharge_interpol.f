!                      *****************************
      SUBROUTINE DISCHARGE_INTERPOL
!                      *****************************
     &(N, S1_IN, S2_IN, ALTBUS_AMONT, ALTBUS_AVAL, Q_BUSE_OUT)
!
! Version optimisée : Lecture unique du fichier + LOGIQUE ORIGINALE
! 1. Format Fortran Fixe strict (limite a la colonne 72).
! 2. Allocation dynamique du cache (RAM) au premier appel.
! 3. Ligne 3 = Debit (Q), Ligne 4 = Altitude (S1).
! 4. Maintien de la relaxation, du dQ/dt et des seuils originaux.
!
!***********************************************************************
      USE DECLARATIONS_SPECIAL
      USE DECLARATIONS_TELEMAC2D, ONLY : T2D_FILES, T2DFO2, AT, DT
      IMPLICIT NONE
!
!     PARAMETRES D'ENTREE
      INTEGER, INTENT(IN)          :: N
      DOUBLE PRECISION, INTENT(IN)  :: S1_IN, S2_IN
      DOUBLE PRECISION, INTENT(IN)  :: ALTBUS_AMONT, ALTBUS_AVAL
      DOUBLE PRECISION, INTENT(OUT) :: Q_BUSE_OUT
!
!     PARAMETRES DE STABILISATION (REPRIS DE VOTRE CODE)
      DOUBLE PRECISION, PARAMETER :: ALPHA_Q_RELAX = 0.2D0
      DOUBLE PRECISION, PARAMETER :: MAX_ABS_DQ_PER_SECOND = 0.025D0
      LOGICAL, PARAMETER          :: LIMIT_DQ_DT_ACTIVE = .TRUE.
      DOUBLE PRECISION, PARAMETER :: ZERO_TOL = 1.0D-9
      DOUBLE PRECISION, PARAMETER :: MIN_H_AMONT_START_FLOW = 0.02D0
      DOUBLE PRECISION, PARAMETER :: H_DIFF_THRESHOLD = 0.02D0
!
!     STRUCTURE POUR LE CACHE DES ABAQUES
      TYPE ABACUS_DATA
        INTEGER :: N_PTS
        DOUBLE PRECISION, ALLOCATABLE :: Q_TAB(:)  ! Ligne 3 (Q)
        DOUBLE PRECISION, ALLOCATABLE :: S1_TAB(:) ! Ligne 4 (S1)
      END TYPE ABACUS_DATA
!
!     VARIABLES PERSISTANTES (SAVE)
      LOGICAL, SAVE :: INITIALIZED = .FALSE.
      TYPE(ABACUS_DATA), ALLOCATABLE, SAVE :: CACHE(:)
      DOUBLE PRECISION, ALLOCATABLE, SAVE :: Q_BUSE_PREV(:)
      DOUBLE PRECISION, SAVE :: TIME_PREV = -1.D0
      INTEGER, SAVE :: U_MAX_ID_ABA = 0
!
!     VARIABLES LOCALES (PREFIXE U_ POUR EVITER LES CONFLITS)
      INTEGER :: U_IFIC, U_IERR, U_ID_TMP, U_NP_TMP, U_I, U_J
      CHARACTER(LEN=100) :: U_BUFFER
      DOUBLE PRECISION :: U_FRAC, U_H1, U_H2, U_Q_INT, U_Q_REL, U_DQ
!
      U_IFIC = T2D_FILES(T2DFO2)%LU
!
!-----------------------------------------------------------------------
! 1. INITIALISATION DU CACHE (AU TOUT PREMIER APPEL)
!-----------------------------------------------------------------------
      IF (.NOT. INITIALIZED) THEN
!
!       A. PREMIER PASSAGE POUR DETERMINER LA TAILLE DU CACHE
        REWIND(U_IFIC)
        READ(U_IFIC, '(A)', IOSTAT=U_IERR) U_BUFFER 
        U_MAX_ID_ABA = 0
10      CONTINUE
        READ(U_IFIC, *, IOSTAT=U_IERR) U_ID_TMP
        IF (U_IERR .EQ. 0) THEN
          IF (U_ID_TMP .GT. U_MAX_ID_ABA) U_MAX_ID_ABA = U_ID_TMP
          READ(U_IFIC, *) U_NP_TMP
          READ(U_IFIC, *) 
          READ(U_IFIC, *) 
          GOTO 10
        ENDIF
!
!       B. ALLOCATION DYNAMIQUE
        ALLOCATE(CACHE(U_MAX_ID_ABA))
        ALLOCATE(Q_BUSE_PREV(U_MAX_ID_ABA))
        DO U_I = 1, U_MAX_ID_ABA
          CACHE(U_I)%N_PTS = 0
          Q_BUSE_PREV(U_I) = 0.D0
        ENDDO
!
!       C. DEUXIEME PASSAGE POUR CHARGER LES DONNEES
        REWIND(U_IFIC)
        READ(U_IFIC, '(A)') U_BUFFER
20      CONTINUE
        READ(U_IFIC, *, IOSTAT=U_IERR) U_ID_TMP
        IF (U_IERR .EQ. 0) THEN
          READ(U_IFIC, *) U_NP_TMP
          CACHE(U_ID_TMP)%N_PTS = U_NP_TMP
          ALLOCATE(CACHE(U_ID_TMP)%Q_TAB(U_NP_TMP))
          ALLOCATE(CACHE(U_ID_TMP)%S1_TAB(U_NP_TMP))
!         LIGNE 3 = DEBITS (Q)
          READ(U_IFIC,*) (CACHE(U_ID_TMP)%Q_TAB(U_J), U_J=1, U_NP_TMP)
!         LIGNE 4 = ALTITUDES (S1)
          READ(U_IFIC,*) (CACHE(U_ID_TMP)%S1_TAB(U_J), U_J=1, U_NP_TMP)
          GOTO 20
        ENDIF
!
        INITIALIZED = .TRUE.
      ENDIF
!
!-----------------------------------------------------------------------
! 2. LOGIQUE DE CALCUL PHYSIQUE (REPRISE DE VOTRE CODE)
!-----------------------------------------------------------------------
!
      U_H1 = S1_IN - ALTBUS_AMONT
      U_H2 = S2_IN - ALTBUS_AVAL
!
      IF (U_H1 .LT. MIN_H_AMONT_START_FLOW .OR. 
     &    (U_H1 - U_H2) .LT. H_DIFF_THRESHOLD) THEN
        U_Q_INT = 0.D0
      ELSE
!
        IF (N.LE.0 .OR. N.GT.U_MAX_ID_ABA) THEN
          U_Q_INT = 0.D0
        ELSEIF (CACHE(N)%N_PTS .EQ. 0) THEN
          U_Q_INT = 0.D0
        ELSE
!         INTERPOLATION (RECHERCHE DE S1_IN DANS S1_TAB)
          IF (S1_IN .LE. CACHE(N)%S1_TAB(1)) THEN
            U_Q_INT = CACHE(N)%Q_TAB(1)
          ELSEIF (S1_IN .GE. CACHE(N)%S1_TAB(CACHE(N)%N_PTS)) THEN
            U_Q_INT = CACHE(N)%Q_TAB(CACHE(N)%N_PTS)
          ELSE
            DO U_I = 1, CACHE(N)%N_PTS - 1
              IF (S1_IN .GE. CACHE(N)%S1_TAB(U_I) .AND. 
     &            S1_IN .LE. CACHE(N)%S1_TAB(U_I+1)) THEN
                ! --- DEBUT DE LA CORRECTION ---
                IF (ABS(CACHE(N)%S1_TAB(U_I+1) - 
     &				CACHE(N)%S1_TAB(U_I)) .LT. 1.D-6) THEN
                  U_FRAC = 0.D0
                ELSE
                  U_FRAC = (S1_IN - CACHE(N)%S1_TAB(U_I)) / 
     &                   (CACHE(N)%S1_TAB(U_I+1)-CACHE(N)%S1_TAB(U_I))
                ENDIF
                ! --- FIN DE LA CORRECTION ---
                U_Q_INT = CACHE(N)%Q_TAB(U_I) + U_FRAC *
     &                   (CACHE(N)%Q_TAB(U_I+1)-CACHE(N)%Q_TAB(U_I))
                EXIT
              ENDIF
            ENDDO
          ENDIF
        ENDIF
      ENDIF
!
      U_Q_REL = Q_BUSE_PREV(N) + ALPHA_Q_RELAX *
     &            (U_Q_INT - Q_BUSE_PREV(N))
!
      IF (LIMIT_DQ_DT_ACTIVE .AND. TIME_PREV .GE. 0.D0) THEN
        U_DQ = U_Q_REL - Q_BUSE_PREV(N)
        IF (ABS(U_DQ) .GT. MAX_ABS_DQ_PER_SECOND * DT) THEN
          U_Q_REL = Q_BUSE_PREV(N) + 
     &                SIGN(MAX_ABS_DQ_PER_SECOND * DT, U_DQ)
        ENDIF
      ENDIF
!
      Q_BUSE_OUT = U_Q_REL
      Q_BUSE_PREV(N) = U_Q_REL
      IF (AT .GT. TIME_PREV + 1.D-6) TIME_PREV = AT
!
      RETURN
      END SUBROUTINE