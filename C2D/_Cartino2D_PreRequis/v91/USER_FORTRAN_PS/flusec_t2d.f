!                   *********************
                    SUBROUTINE FLUSEC_T2D
!                   *********************
!
     &(GLOSEG,DIMGLO,DT,MESH,FLODEL,DOPLOT)
!
!***********************************************************************
! TELEMAC2D   V7P2
!***********************************************************************
!
!brief  COMPUTES FLUXES OVER LINES (FLUXLINES/CONTROL SECTIONS) VIA
!+      FLODEL
!+
!+      THE FLUXES OF THE SEGMENTS ARE ALLREADY COMPUTED IN THE POSITIVE
!+      DEPTHS ROUTINE (BIEF)
!+
!+      IN A FIRST STEP WE SEARCH AND SAVE ALL NECESSARY SEGMENTS
!+      (ONE NODE IS ON THE LEFT SIDE , THE OTHER ON THE RIGHT SIDE
!+      OF THE FLUXLINE.
!+
!+      DURING LATER CALLS WE SUM UP THE FLUXES FOR EACH SEGMENT AND USE
!+      FLUXPR_TELEMAC2D TO WRITE OUT THE FLUXES
!
!history  L. STADLER (BAW)
!+        17/03/2016
!+        V7P2
!+   New way of computing discharges through control sections.
!+   First version.
!
!history  J,RIEHME (ADJOINTWARE)
!+        November 2016
!+        V7P2
!+   Replaced EXTERNAL statements to parallel functions / subroutines
!+   by the INTERFACE_PARALLEL
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| DT             |-->| TIME_FLUSECT2D STEP
!| DIMGLO         |-->| FIRST DIMENSION OF GLOSEG
!| FLODEL         |<--| FLUXES BETWEEN POINTS (PER SEGMENT)
!| GLOSEG         |-->| GLOBAL NUMBERS OF APICES OF SEGMENTS
!| MESH           |-->| MESH STRUCTURE
!| X              |-->| ABSCISSAE OF POINTS IN THE MESH
!| Y              |-->| ORDINATES OF POINTS IN THE MESH
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      USE BIEF_DEF, ONLY: IPID
      USE BIEF
      USE DECLARATIONS_SPECIAL
      USE DECLARATIONS_TELEMAC2D, ONLY : T2D_FILES,T2DFLX,TITCAS,
     &                            FLUXLINEDATA_FLUSECT2D,T2DSEO,
     &                            DEJA_FLUSECT2D,VOLFLUX_FLUSECT2D,
     &                            FLX_FLUSECT2D,NSEO_FPR,INIT_FPR,
     &                            NUMBEROFLINES_FLUSECT2D,WORK_FPR,
     &                            TIME_FLUSECT2D,ZF,H,U,V,GRAV
      USE DECLARATIONS_SPECIAL
!
      USE INTERFACE_PARALLEL, ONLY : P_MAX,P_MIN,P_SUM
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)          :: DIMGLO
      INTEGER, INTENT(IN)          :: GLOSEG(DIMGLO,2)
      DOUBLE PRECISION, INTENT(IN) :: DT
      TYPE(BIEF_MESH)              :: MESH
      TYPE(BIEF_OBJ),   INTENT(IN) :: FLODEL
      LOGICAL, INTENT(IN)          :: DOPLOT
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!     VOLFLUX_FLUSECT2D: CUMULATED VOLUME THROUGH SECTIONS
!     FLX_FLUSECT2D: FLUX THROUGH CONTROL SECTIONS
!
      INTEGER, PARAMETER :: MAXEDGES=2000
!
      INTEGER I,INP,ISEC,MYPOS,IERR,MYPOS2,NSEC,I1,I2,ERR
	  INTEGER II,nan,nan1,nan2,FILESEC
      INTEGER, DIMENSION(:), ALLOCATABLE, SAVE :: IDFLXLINE,MYPOSTAB
!	  INTEGER, DIMENSION(:,:,:), ALLOCATABLE, SAVE :: IPNSEGTOT
	  INTEGER, DIMENSION(:,:,:), ALLOCATABLE, SAVE::IPNSEG
      DOUBLE PRECISION, DIMENSION (2) :: SEG1,SEG2
      DOUBLE PRECISION :: SEGMENTFLUX,SIGN1,SIGN2
!
      CHARACTER(LEN=34) :: FMTZON  
      DOUBLE PRECISION :: SEGXMIN,SEGXMAX
      DOUBLE PRECISION :: SEGYMIN,SEGYMAX
      DOUBLE PRECISION,ALLOCATABLE :: FLUXLINES(:,:)
!
      DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: SUMFLX,SUMVOLFLUX
	  DOUBLE PRECISION :: Hmax1,Hmax2,Hmin1,Hmin2
	  DOUBLE PRECISION :: Zmax1,Zmax2,Zmin1,Zmin2
	  DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: H11,H22,Z11,Z22
	  DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: Z1, Z2, H1, H2
	  DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: Zmin,Zmax, Zmoy
	  DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: Hmin,Hmax, Hmoy
	  DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: ZZmin,ZZmax,HT1
	  DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: HHmin,HHmax,HT2
	  DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: HHmoy,ZZmoy,HHtot
	  DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: vel1,vel2,HV1,HV2
	  DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: Htot1,Htot2,Htot
	  INTEGER, DIMENSION(:), ALLOCATABLE :: SIZENC_SEC,SEC_NC
	  DOUBLE PRECISION, PARAMETER :: Hlim=0.05D0
!
!
!
!----------------------------------------------------------------------
!
!     PART I
!
!     SEARCH AND SAVE SEGMENTS (FIRST RUN ONLY)
!
!----------------------------------------------------------------------
      FMTZON='(I10,4X,I10,4X,8(F16.2,4X))' 
      IF(.NOT.DEJA_FLUSECT2D) THEN
!        INIT_FPR=.TRUE.
        INP=T2D_FILES(T2DFLX)%LU
        TIME_FLUSECT2D = 0.D0
!       READ FLUXLINE FILE
        READ(INP,*) NUMBEROFLINES_FLUSECT2D
!
!       ALLOCATE THE FLUXLINES
        IF(.NOT.ALLOCATED(FLUXLINES)) THEN
          ALLOCATE (FLUXLINES(NUMBEROFLINES_FLUSECT2D,10), STAT=IERR)
		  ALLOCATE (IDFLXLINE(NUMBEROFLINES_FLUSECT2D), STAT=IERR)
          CALL CHECK_ALLOCATE(IERR, 'FLUXLINE:FLUXLINES')
        ENDIF
!       READ NODES INTO FLUXLINE
        DO I = 1,NUMBEROFLINES_FLUSECT2D
          READ(INP,*) ( FLUXLINES(I,ISEC), ISEC=1, 10 )
		  IDFLXLINE(I)=FLUXLINES(I,10)
        ENDDO
!
        WRITE(LU,*) 'FLUXLINES FOUND ',NUMBEROFLINES_FLUSECT2D
!
!------- DYNAMIC ALLOCATION OF FLX_FLUSECT2D, VOLFLUX_FLUSECT2D,...
!
        ALLOCATE(FLX_FLUSECT2D(NUMBEROFLINES_FLUSECT2D,1),STAT=IERR)
        ALLOCATE(VOLFLUX_FLUSECT2D(NUMBEROFLINES_FLUSECT2D,1),STAT=IERR)
		ALLOCATE(MYPOSTAB(NUMBEROFLINES_FLUSECT2D))
        ALLOCATE(FLUXLINEDATA_FLUSECT2D(NUMBEROFLINES_FLUSECT2D),
     &           STAT=IERR)
        DO I = 1,NUMBEROFLINES_FLUSECT2D
          ALLOCATE(FLUXLINEDATA_FLUSECT2D(I)%SECTIONIDS(MAXEDGES),
     &           STAT=IERR)
          ALLOCATE(FLUXLINEDATA_FLUSECT2D(I)%DIRECTION(MAXEDGES),
     &           STAT=IERR)
        ENDDO
        CALL CHECK_ALLOCATE(IERR,"FLUXLINE:DATA")
!		WRITE(LU,*) 'DIMGLO :',DIMGLO
!
!------ CLEANUP
!
        DO ISEC =1,NUMBEROFLINES_FLUSECT2D
          VOLFLUX_FLUSECT2D(ISEC,1) = 0.D0
          FLUXLINEDATA_FLUSECT2D(ISEC)%NOFSECTIONS = 0
        ENDDO
!
!-------LOOP OVER ALL MESH SEGMENTS TO STORE THEM FOR EACH FLUXLINE
!

		ALLOCATE(IPNSEG(NUMBEROFLINES_FLUSECT2D,10000,2))
        DO I = 1,MESH%NSEG

		  
          SEG1(1) = MESH%X%R(GLOSEG(I,1))
          SEG1(2) = MESH%Y%R(GLOSEG(I,1))
          SEG2(1) = MESH%X%R(GLOSEG(I,2))
          SEG2(2) = MESH%Y%R(GLOSEG(I,2))

!          
!         LOOP OVER ALL FLUXLINES
          DO ISEC =1,NUMBEROFLINES_FLUSECT2D
!
!----------------------------------------------------------
!
! SIGN IS USED TO LOOK ON WHICH SIDE OF THE LINE A NODE IS
!
!  - SIGN IS NEGATIVE IF WE ARE ON THE RIGHT SIDE
!  - SIGN IS POSITIVE IF WE ARE ON THE LEFT SIDE
!  - SIGN IS ZERO IF WE ARE ON A POINT
!
!---------------------------------------------------------
!
            SIGN1 = (SEG1(1) - FLUXLINES(ISEC,3))*
     &              (FLUXLINES(ISEC,2) - FLUXLINES(ISEC,4)) -
     &              (SEG1(2) - FLUXLINES(ISEC,4)) *
     &              (FLUXLINES(ISEC,1) - FLUXLINES(ISEC,3))

            SIGN2 = (SEG2(1) - FLUXLINES(ISEC,3))*
     &              (FLUXLINES(ISEC,2) - FLUXLINES(ISEC,4)) -
     &              (SEG2(2) - FLUXLINES(ISEC,4)) *
     &              (FLUXLINES(ISEC,1) - FLUXLINES(ISEC,3))
!
!---------------------------------------------------------
!
! THE FLUXLINE SHOULD NEVER CROSS A NODE (BE ZERO)
! IF THIS HAPPENS WE SHIFT THE NODE (RIGHT AND UPWARDS)
!
!---------------------------------------------------------
!
            IF(SIGN1.EQ.0.D0) THEN
              SIGN1 = (SEG1(1)+0.001D0 - FLUXLINES(ISEC,3)) *
     &                (FLUXLINES(ISEC,2) - FLUXLINES(ISEC,4))-
     &                (SEG1(2)+0.001D0 - FLUXLINES(ISEC,4)) *
     &                (FLUXLINES(ISEC,1) - FLUXLINES(ISEC,3))
            ENDIF
            IF(SIGN2.EQ.0.D0) THEN
              SIGN2 = (SEG2(1)+0.001D0 - FLUXLINES(ISEC,3)) *
     &                (FLUXLINES(ISEC,2) - FLUXLINES(ISEC,4))-
     &                (SEG2(2)+0.001D0 - FLUXLINES(ISEC,4)) *
     &                (FLUXLINES(ISEC,1) - FLUXLINES(ISEC,3))
            ENDIF
!
!           ADD THE SEGMENT ID TO THE NODES
!
            IF(SIGN1*SIGN2.LT.0.D0) THEN
!
              SEGXMIN = MIN(SEG1(1),SEG2(1))
              SEGXMAX = MAX(SEG1(1),SEG2(1))
              SEGYMIN = MIN(SEG1(2),SEG2(2))
              SEGYMAX = MAX(SEG1(2),SEG2(2))

!
              IF(SEGXMIN > FLUXLINES(ISEC,5).AND.
     &           SEGXMAX < FLUXLINES(ISEC,7).AND.
     &           SEGYMIN > FLUXLINES(ISEC,6).AND.
     &           SEGYMAX < FLUXLINES(ISEC,8)) THEN
!
                MYPOS = FLUXLINEDATA_FLUSECT2D(ISEC)%NOFSECTIONS + 1
                IPNSEG(ISEC,MYPOS,1)=GLOSEG(I,1)
				IPNSEG(ISEC,MYPOS,2)=GLOSEG(I,2)

                IF(MYPOS.EQ.MAXEDGES) THEN
                  WRITE(LU,*) 'FLUSEC_T2D:'
                  WRITE(LU,*) 'TOO MANY SEGMENTS IN A SECTION'
                  WRITE(LU,*) 'INCREASE MAXEDGES'
                  CALL PLANTE(1)
                  STOP
                ENDIF
                FLUXLINEDATA_FLUSECT2D(ISEC)%SECTIONIDS(MYPOS) = I
                IF(SIGN1.GT.0.D0) THEN
                  FLUXLINEDATA_FLUSECT2D(ISEC)%DIRECTION(MYPOS) = 1
                ELSE
                  FLUXLINEDATA_FLUSECT2D(ISEC)%DIRECTION(MYPOS) = -1
                ENDIF
                FLUXLINEDATA_FLUSECT2D(ISEC)%NOFSECTIONS = MYPOS
!
!               FOR DEBUGGING
!
!               WRITE(LU,*)'ADDED SEGMENTS ',
!    &                      I,GLOSEG(I,1),GLOSEG(I,2)
!               WRITE(LU,*)'AT COORDINATES ',SEG1,SEG2
!               WRITE(LU,*)'SECTIONS FOUND ',
!    &                      FLUXLINEDATA_FLUSECT2D(ISEC)%NOFSECTIONS
              ENDIF
            ENDIF
          ENDDO
        ENDDO
		
!		DEALLOCATE(IPNSEG)
        DO ISEC=1,NUMBEROFLINES_FLUSECT2D
!		  MYPOS2=FLUXLINEDATA_FLUSECT2D(ISEC)%NOFSECTIONS
!		  ALLOCATE(IPNSEGTOT(NUMBEROFLINES_FLUSECT2D,MYPOS2))
!       CASE WHERE NO SEGEMENT WAS ADDED
          IF(FLUXLINEDATA_FLUSECT2D(ISEC)%NOFSECTIONS.LE.2)THEN
            WRITE(LU,*)'FLUSEC_T2D: WARNING, SECTION (FLUXLINE)',ISEC
            WRITE(LU,*)'            CONTAINS LESS THAN 2 SEGMENTS.'
            WRITE(LU,*)'            POSSIBLE CAUSE: BOX CONTAINING'
            WRITE(LU,*)'            THE FLUXLINE IS TOO SMALL'
          ENDIF
        ENDDO
      ENDIF
!     END SEARCH SEGEMENT (DEJA_FLUSECT2D)
      DEJA_FLUSECT2D = .TRUE.
!
!----------------------------------------------------------------------
!
!     PART II
!
!     ADD THE FLUXES (FLODEL FROM POSITIVE DEPTHS) FOR SEGMENTS
!
!     TODO WE SHOULD THINK ABOUT HOW WE CAN HANDLE THIS IN THE PARALLEL
!          CASE! IF A SEGMENT IS SHARED WE NEED THE HALF FLUX?
!
!----------------------------------------------------------------------
!
      TIME_FLUSECT2D = TIME_FLUSECT2D + DT
!     LOOP OVER ALL FLUXLINES
      NSEC=NUMBEROFLINES_FLUSECT2D
	  ALLOCATE(Hmin(NSEC),Hmax(NSEC),Hmoy(NSEC),Zmin(NSEC),Zmax(NSEC),
     &         Zmoy(NSEC),H11(NSEC),H22(NSEC),Z11(NSEC),Z22(NSEC),
     &         SIZENC_SEC(NSEC),SEC_NC(NSEC),HHmin(NSEC),HHmax(NSEC),
     &         HHmoy(NSEC),ZZmin(NSEC),ZZmax(NSEC),ZZmoy(NSEC),
     &         Htot(NSEC),HHtot(NSEC),HT1(NSEC),HT2(NSEC),
     &         SUMFLX(NSEC),SUMVOLFLUX(NSEC))	 
      DO ISEC =1,NUMBEROFLINES_FLUSECT2D
	    
		MYPOS2=FLUXLINEDATA_FLUSECT2D(ISEC)%NOFSECTIONS
        FLX_FLUSECT2D(ISEC,1) = 0.D0
		ALLOCATE(Z1(MYPOS2),Z2(MYPOS2),H1(MYPOS2),
     &         H2(MYPOS2),vel1(MYPOS2),vel2(MYPOS2),
     &         HV1(MYPOS2),HV2(MYPOS2),Htot1(MYPOS2),	
     &         Htot2(MYPOS2))
	 	nan1 = MYPOS2
		nan2 = MYPOS2
!       LOOP OVER SEGMENT
        DO I = 1,FLUXLINEDATA_FLUSECT2D(ISEC)%NOFSECTIONS
		  I1=IPNSEG(ISEC,I,1)
          I2=IPNSEG(ISEC,I,2)
		  
		  vel1(I)=sqrt(U%R(I1)**2+V%R(I1)**2)
		  HV1(I)= vel1(I)**2/(2.0d0*GRAV)
		  H1(I) = H%R(I1)
		  IF(H1(I).LT.Hlim) THEN
		     HV1(I)=0
		     H1(I)= 0
		     Z1(I) = 0
		     Htot1(I)=0
		     nan1 = nan1-1
		  ELSE
		     Z1(I) = H1(I) + ZF%R(I1)
		     Htot1(I) = Z1(I)+HV1(I)
		     
		  ENDIF
		  vel2(I)=sqrt(U%R(I2)**2+V%R(I2)**2)
		  HV2(I)= vel2(I)**2/(2.0d0*GRAV)
		  H2(I) = H%R(I2)
		  IF(H2(I).LT.Hlim) THEN
		     HV2(I)=0
		     H2(I)= 0
		     Z2(I) = 0
		     Htot2(I) = 0
		     nan2 = nan2-1
		  ELSE
		     Z2(I) = H2(I) + ZF%R(I2)
		     Htot2(I)=Z2(I)+HV2(I)
		  ENDIF

          SEGMENTFLUX =  FLUXLINEDATA_FLUSECT2D(ISEC)%DIRECTION(I) *
     &             FLODEL%R(FLUXLINEDATA_FLUSECT2D(ISEC)%SECTIONIDS(I))

          FLX_FLUSECT2D(ISEC,1) = FLX_FLUSECT2D(ISEC,1) + SEGMENTFLUX
          VOLFLUX_FLUSECT2D(ISEC,1) = VOLFLUX_FLUSECT2D(ISEC,1) +
     &                                (SEGMENTFLUX * DT)
        ENDDO
		nan=nan1+nan2
		IF(nan.EQ.0) THEN 
		   nan = 1
		ENDIF
		Hmax1 = maxval(H1,MASK = H1.GT.0)
		Hmax2 = maxval(H2,MASK = H2.GT.0)
		Hmax(ISEC) = max(Hmax1,Hmax2)
		
		Hmin1 = minval(H1,MASK = H1.GT.0.D0)
		Hmin2 = minval(H2,MASK = H2.GT.0.D0)
		
		
		Hmin(ISEC) = min(Hmin1,Hmin2)
		H11(ISEC) = sum(H1,MASK = H1.GT.0.D0)
		H22(ISEC) = sum(H2,MASK = H2.GT.0.D0)
		
		Hmoy(ISEC) = (H11(ISEC)+H22(ISEC))/nan
		IF(isnan(Hmoy(ISEC))) THEN
		   Hmoy(ISEC)=0.D0
		ENDIF
		
		Zmax1 = maxval(Z1,MASK = Z1.GT.0.D0)
		Zmax2 = maxval(Z2,MASK = Z2.GT.0.D0)
		Zmax(ISEC) = max(Zmax1,Zmax2)
		
		Zmin1 = minval(Z1,MASK = Z1.GT.0.D0)
		Zmin2 = minval(Z2,MASK = Z2.GT.0.D0)
		Zmin(ISEC) = min(Zmin1,Zmin2)
		Z11(ISEC) = sum(Z1,MASK = Z1.GT.0.D0)
		Z22(ISEC) = sum(Z2,MASK = Z2.GT.0.D0)
		Zmoy(ISEC) = (Z11(ISEC)+Z22(ISEC))/nan
		SIZENC_SEC(ISEC) = 1
		IF(isnan(Zmoy(ISEC)).OR.Zmoy(ISEC).EQ.0.D0) THEN
		   Zmoy(ISEC)=0.D0
		   Zmin(ISEC)=0.D0
		   Zmax(ISEC)=0.D0
		   Hmax(ISEC)=0.D0
		   SIZENC_SEC(ISEC) = 0
		ENDIF
		HT1(ISEC)=sum(Htot1,MASK = Htot1.GT.0.D0)
		HT2(ISEC)=sum(Htot2,MASK = Htot2.GT.0.D0)
		Htot(ISEC)=(HT1(ISEC)+HT2(ISEC))/nan
		IF(isnan(Htot(ISEC))) THEN
		   Htot(ISEC)=0.D0
		ENDIF
		DEALLOCATE(Z1,Z2,H1,H2,vel1,vel2,HV1,HV2,Htot1,Htot2)
		
		
		
      ENDDO
!
!----------------------------------------------------------------------
!----------------------------------------------------------------------
!
!     PART III
!
!----------------------------------------------------------------------
!
      IF (INIT_FPR) THEN
        INIT_FPR=.FALSE.
        IF ((NCSIZE.GT.1 .AND. IPID.EQ.0).OR.(NCSIZE.LE.1)) THEN
          FILESEC=T2D_FILES(T2DSEO)%LU
          IF(LNG.EQ.LNG_FR) THEN
            WRITE(FILESEC,*) 'TITRE = "FLUX ET HAUTEURS POUR ',
     &       TRIM(TITCAS),'"'
          ELSEIF(LNG.EQ.LNG_EN) THEN
            WRITE(FILESEC,*) 'TITLE = "FLUXES AND HEIGHTS FOR ',
     &       TRIM(TITCAS),'"'
          ENDIF
		  WRITE(FILESEC,*) 'VARIABLES =           NAME ', 
     &       '       ID       TIME   ',
     &       '  DISCHARGE       WL_MEAN     WL_MAX    ',	 
     &       '  WL_MIN     MAX_DEPTH   HYDRAULIC HEAD'
        ENDIF
        IF (NCSIZE.GT.1) THEN
          ALLOCATE (WORK_FPR(NUMBEROFLINES_FLUSECT2D), STAT=ERR)
          IF (ERR.NE.0) THEN
            WRITE(LU,*)
     &        'FLUXPR_TELEMAC2D: ERROR ALLOCATING WORK_FPR:',ERR
            CALL PLANTE(1)
            STOP
          ENDIF
        ENDIF
      ENDIF
      IF(DOPLOT) THEN

!       PARALLEL CASE
        IF (NCSIZE.GT.1) THEN
		  DO I=1,NUMBEROFLINES_FLUSECT2D
            SEC_NC(I) = P_SUM(SIZENC_SEC(I))
            IF(SEC_NC(I).EQ.0) THEN 
			  SEC_NC(I)=1
			ENDIF
			ZZmoy(I) = P_SUM(Zmoy(I))/SEC_NC(I)
			ZZmax(I) = P_MAX(Zmax(I))
			ZZmin(I) = P_MIN(Zmin(I))
			HHtot(I) = P_SUM(Htot(I))/SEC_NC(I)
			HHmax(I) = P_MAX(Hmax(I))
            SUMFLX(I) = P_SUM(FLX_FLUSECT2D(I,1))
            SUMVOLFLUX(I) = P_SUM(VOLFLUX_FLUSECT2D(I,1))
          ENDDO
		
        IF(IPID.EQ.0) THEN
!         PREPARE SINGLE DATA FOR SENDING
          FILESEC=T2D_FILES(T2DSEO)%LU
          DO I=1,NUMBEROFLINES_FLUSECT2D


			WRITE(FILESEC,FMT=FMTZON) IDFLXLINE(I),I,
     &      TIME_FLUSECT2D,SUMFLX(I),
     &      ZZmoy(I),ZZmax(I),ZZmin(I),	 
     &      HHmax(I),HHtot(I)

          ENDDO
		ENDIF  
!       SERIAL CASE
        ELSE

          DO I=1,NUMBEROFLINES_FLUSECT2D

			WRITE(FILESEC,FMT=FMTZON) IDFLXLINE(I),I,TIME_FLUSECT2D, 
     &      FLX_FLUSECT2D(I,1),
     &      Zmoy(I),Zmax(I),Zmin(I),	 
     &      Hmax(I),Htot(I)
          ENDDO
        ENDIF
      ENDIF
!
1000  FORMAT (5X,A17,G16.7,G16.7,G16.7)
!
!----------------------------------------------------------------------
!
      RETURN
      END
