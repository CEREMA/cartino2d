!                   ***************************
                    SUBROUTINE FLUXPR_TELEMAC2D
!                   ***************************
!
     &(NSEC,CTRLSC,FLX,VOLNEG,VOLPOS,INFO,TPS,NSEG,NCSIZE,CUMFLO)
!
!***********************************************************************
! TELEMAC2D   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    COMPUTES FLUXES THROUGH CONTROL SECTIONS
!+                AND SUMS THESE UP TO EVALUATE OSCILLATING VOLUMES.
!
!note     PRINTOUTS OF DISCHARGES THROUGH CONTROL SECTIONS ARE DONE
!+            IN THIS ROUTINE. YOU CAN REWRITE IT TO DIVERT THESE
!+            PRINTOUTS TO A FILE OR TO CHANGE THE FORMAT
!
!history  J-M HERVOUET (LNHE)
!+        25/03/1999
!+        V5P5
!+
!
!history  N.DURAND (HRW), S.E.BOURBAN (HRW)
!+        13/07/2010
!+        V6P0
!+   Translation of French comments within the FORTRAN sources into
!+   English comments
!
!history  N.DURAND (HRW), S.E.BOURBAN (HRW)
!+        21/08/2010
!+        V6P0
!+   Creation of DOXYGEN tags for automated documentation and
!+   cross-referencing of the FORTRAN sources
!
!history  J,RIEHME (ADJOINTWARE)
!+        November 2016
!+        V7P2
!+   Replaced EXTERNAL statements to parallel functions / subroutines
!+   by the INTERFACE_PARALLEL
!history  N.HOCINI, F.PONS (CEREMA)
!+        12/07/2023
!+        V8P4
!+   Adding new output variables : mean, max, min Waterline elevation,
!+   Max water depth, total hydraulic head + new format for output file
!+   Works for both scalar and parallel computing
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| CTRLSC         |-->| NUMBERS OF POINTS IN THE CONTROL SECTIONS
!| CUMFLO         |-->| KEYWORD: PRINTING CUMULATED FLOWRATES
!| FLX            |-->| FLUXES THROUGH CONTROL SECTIONS
!| INFO           |-->| IF YES : INFORMATION IS PRINTED
!| NCSIZE         |-->| NUMBER OF PROCESSORS
!| NSEC           |-->| NUMBER OF CONTROL SECTIONS
!| NSEG           |-->| NUMBER OF SEGMENTS
!| TPS            |-->| TIME IN SECONDS
!| VOLNEG         |-->| CUMULATED NEGATIVE VOLUME THROUGH SECTIONS
!| VOLPOS         |-->| CUMULATED POSITIVE VOLUME THROUGH SECTIONS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF_DEF, ONLY: IPID
      USE DECLARATIONS_TELEMAC2D, ONLY: T2D_FILES,T2DSEO,CHAIN,TITCAS,
     &                                  WORK_FPR, OLD_METHOD_FPR,ZF,
     &                                  INIT_FPR, NSEO_FPR,H,U,V,
     &                                  LISTE_FS,GRAV
      USE DECLARATIONS_SPECIAL
      USE INTERFACE_PARALLEL, ONLY : P_MAX,P_MIN,P_SUM
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)          :: NSEC,NCSIZE
      INTEGER, INTENT(IN)          :: CTRLSC(*)
      INTEGER, INTENT(IN)          :: NSEG(NSEC)
      LOGICAL, INTENT(IN)          :: INFO,CUMFLO
      DOUBLE PRECISION, INTENT(IN) :: FLX(NSEC)
      DOUBLE PRECISION, INTENT(IN) :: VOLNEG(NSEC),VOLPOS(NSEC)
      DOUBLE PRECISION, INTENT(IN) :: TPS
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!
      INTEGER ISEC,II,I1,I2,ISEG,nan,nan1,nan2,ERR
      CHARACTER(LEN=34) :: FMTZON                  
      DOUBLE PRECISION :: DTMP1,DTMP2,DTMP3,DTMP4
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
!-----------------------------------------------------------------------
!     NEW VARIABLES AND FORMAT FOR OUTPUT FILE
	  FMTZON='(A30,4X,I3,4X,F16.2,4X,6(F8.2,4X))' 
	  ALLOCATE(Hmin(NSEC),Hmax(NSEC),Hmoy(NSEC),Zmin(NSEC),Zmax(NSEC),
     &         Zmoy(NSEC),H11(NSEC),H22(NSEC),Z11(NSEC),Z22(NSEC),
     &         SIZENC_SEC(NSEC),SEC_NC(NSEC),HHmin(NSEC),HHmax(NSEC),
     &         HHmoy(NSEC),ZZmin(NSEC),ZZmax(NSEC),ZZmoy(NSEC),
     &         Htot(NSEC),HHtot(NSEC),HT1(NSEC),HT2(NSEC))

!
      IF (.NOT.ALLOCATED(CHAIN)) OLD_METHOD_FPR=.TRUE.
!
      IF(INFO) THEN
!
      IF (OLD_METHOD_FPR) THEN ! FOLLOW FLUXPR.F OF BIEF BLINDLY
!
      IF(NCSIZE.LE.1) THEN
	  

!
      DO ISEC = 1,NSEC
 
!
      IF(CUMFLO) THEN
      WRITE(LU,131) ISEC,CTRLSC(1+2*(ISEC-1)),
     &              CTRLSC(2+2*(ISEC-1)),
     &              FLX(ISEC),
     &              VOLNEG(ISEC),
     &              VOLPOS(ISEC)
      ELSE
      WRITE(LU,137) ISEC,CTRLSC(1+2*(ISEC-1)),
     &              CTRLSC(2+2*(ISEC-1)),
     &              FLX(ISEC)
      ENDIF
131   FORMAT(1X,/,1X,'CONTROL SECTION NUMBER ',1I2,
     &               ' (BETWEEN POINTS ',1I5,' AND ',1I5,')',//,5X,
     &               'DISCHARGE: '                 ,G16.7,/,5X,
     &               'NEGATIVE VOLUME THROUGH THE SECTION: ',G16.7,/,5X,
     &               'POSITIVE VOLUME THROUGH THE SECTION: ',G16.7)
137   FORMAT(1X,/,1X,'CONTROL SECTION NUMBER ',1I2,
     &               ' (BETWEEN POINTS ',1I5,' AND ',1I5,')',//,5X,
     &               'DISCHARGE: '                 ,G16.7)
!
      ENDDO
!
      ELSE
!
      DO ISEC = 1,NSEC
!     SECTIONS ACROSS 2 SUB-DOMAINS WILL HAVE NSEG=0 OR -1
!     AND -1 WANTED HERE FOR RELEVANT MESSAGE.
      II=P_MIN(NSEG(ISEC))
!
      IF(II.GE.0) THEN
!
        DTMP1 = P_MIN(FLX(ISEC))
        DTMP2 = P_MAX(FLX(ISEC))
        DTMP3 = P_MIN(VOLNEG(ISEC))
        DTMP4 = P_MAX(VOLPOS(ISEC))
!
        WRITE(LU,133) ISEC,CTRLSC(1+2*(ISEC-1)),
     &                CTRLSC(2+2*(ISEC-1)),
     &                DTMP1+DTMP2,DTMP3,DTMP4
133     FORMAT(1X,/,1X,'CONTROL SECTION NUMBER ',1I2,
     &               ' (BETWEEN POINTS ',1I5,' AND ',1I5,')',//,5X,
     &               'DISCHARGE: '                 ,G16.7,/,5X,
     &               'NEGATIVE VOLUME THROUGH THE SECTION: ',G16.7,/,5X,
     &               'POSITIVE VOLUME THROUGH THE SECTION: ',G16.7)
!
      ELSE
!
        WRITE(LU,135) ISEC,CTRLSC(1+2*(ISEC-1)),
     &                                CTRLSC(2+2*(ISEC-1))
135     FORMAT(1X,/,1X,'CONTROL SECTION NUMBER ',1I2,
     &               ' (BETWEEN POINTS ',1I5,' AND ',1I5,')',//,5X,
     &               'ACROSS TWO SUB-DOMAINS, NO COMPUTATION')
      ENDIF
!
      ENDDO
!
      ENDIF
!
!-----------------------------------------------------------------------
! CHAIN ALLOCATED, I.E. SERIAL OR PARALLEL CASE FROM SECTIONS INPUT FILE
!       WE CAN APPLY CO-ORDINATES INSTEAD AND/OR NAMES OF SECTIONS
!
      ELSE
        IF(NCSIZE.LE.1) THEN ! SERIAL
          DO ISEC = 1,NSEC
			ALLOCATE(Z1(NSEG(ISEC)),Z2(NSEG(ISEC)),H1(NSEG(ISEC)),
     &         H2(NSEG(ISEC)),vel1(NSEG(ISEC)),vel2(NSEG(ISEC)),
     &         HV1(NSEG(ISEC)),HV2(NSEG(ISEC)),Htot1(NSEG(ISEC)),	
     &         Htot2(NSEG(ISEC)))
			nan1 = NSEG(ISEC)
			nan2 = NSEG(ISEC)
		    DO ISEG = 1 , NSEG(ISEC)
				I1 = LISTE_FS(ISEC,ISEG,1)
				I2 = LISTE_FS(ISEC,ISEG,2)		
				vel1(ISEG)=sqrt(U%R(I1)**2+V%R(I1)**2)
				HV1(ISEG)= vel1(ISEG)**2/(2.0d0*GRAV)
				H1(ISEG) = H%R(I1)
				IF(H1(ISEG).LT.Hlim) THEN
				   HV1(ISEG)=0
				   H1(ISEG)= 0
				   Z1(ISEG) = 0
				   Htot1(ISEG)=0
				   nan1 = nan1-1
				ELSE
				   Z1(ISEG) = H1(ISEG) + ZF%R(I1)
				   Htot1(ISEG) = Z1(ISEG)+HV1(ISEG)
				   
				ENDIF
				vel2(ISEG)=sqrt(U%R(I2)**2+V%R(I2)**2)
				HV2(ISEG)= vel2(ISEG)**2/(2.0d0*GRAV)
				H2(ISEG) = H%R(I2)
				IF(H2(ISEG).LT.Hlim) THEN
				   HV2(ISEG)=0
				   H2(ISEG)= 0
				   Z2(ISEG) = 0
				   Htot2(ISEG) = 0
				   nan2 = nan2-1
				ELSE
				   Z2(ISEG) = H2(ISEG) + ZF%R(I2)
				   Htot2(ISEG)=Z2(ISEG)+HV2(ISEG)
				ENDIF
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
            IF(CUMFLO) THEN
              WRITE(LU,231) ISEC,TRIM(CHAIN(ISEC)%DESCR),
     &                      FLX(ISEC),VOLNEG(ISEC),VOLPOS(ISEC)
            ELSE
              WRITE(LU,237) ISEC,TRIM(CHAIN(ISEC)%DESCR),
     &                      FLX(ISEC)
            ENDIF
231   FORMAT(1X,/,1X,'CONTROL SECTION NUMBER ',1I2,
     &               ' (NAME ',A,')',//,5X,
     &               'DISCHARGE: '                 ,G16.7,/,5X,
     &               'NEGATIVE VOLUME THROUGH THE SECTION: ',G16.7,/,5X,
     &               'POSITIVE VOLUME THROUGH THE SECTION: ',G16.7)
237   FORMAT(1X,/,1X,'CONTROL SECTION NUMBER ',1I2,
     &               ' (NAME ',A,')',//,5X,
     &               'DISCHARGE: '                 ,G16.7)
          ENDDO
!
        ELSE
          DO ISEC = 1,NSEC
			ALLOCATE(Z1(NSEG(ISEC)),Z2(NSEG(ISEC)),H1(NSEG(ISEC)),
     &         H2(NSEG(ISEC)),vel1(NSEG(ISEC)),vel2(NSEG(ISEC)),
     &         HV1(NSEG(ISEC)),HV2(NSEG(ISEC)),Htot1(NSEG(ISEC)),	
     &         Htot2(NSEG(ISEC)))		
			nan1 = NSEG(ISEC)
			nan2 = NSEG(ISEC)
			DO ISEG = 1 , NSEG(ISEC)
				I1 = CHAIN(ISEC)%LISTE(ISEG,1)
				I2 = CHAIN(ISEC)%LISTE(ISEG,2)
				vel1(ISEG)=sqrt(U%R(I1)**2+V%R(I1)**2)
				HV1(ISEG)= vel1(ISEG)**2/(2.0d0*GRAV)
				H1(ISEG) = H%R(I1)
				IF(H1(ISEG).LT.Hlim) THEN
				   HV1(ISEG)=0
				   H1(ISEG)= 0
				   Z1(ISEG) = 0
				   Htot1(ISEG)=0
				   nan1 = nan1-1
				ELSE
				   Z1(ISEG) = H1(ISEG) + ZF%R(I1)
				   Htot1(ISEG) = Z1(ISEG)+HV1(ISEG)
				ENDIF
				vel2(ISEG)=sqrt(U%R(I2)**2+V%R(I2)**2)
				HV2(ISEG)= vel2(ISEG)**2/(2.0d0*GRAV)
				H2(ISEG) = H%R(I2)
				IF(H2(ISEG).LT.Hlim) THEN
				   HV2(ISEG)=0
				   H2(ISEG)= 0
				   Z2(ISEG) = 0
				   Htot2(ISEG) = 0
				   nan2 = nan2-1
				ELSE
				   Z2(ISEG) = H2(ISEG) + ZF%R(I2)
				   Htot2(ISEG)=Z2(ISEG)+HV2(ISEG)
				   
				ENDIF

				
			ENDDO
			nan=nan1+nan2
			IF(nan.EQ.0) THEN 
			   nan = 1
			ENDIF
			Hmax1 = maxval(H1,MASK = H1.GT.0.D0)
			Hmax2 = maxval(H2,MASK = H2.GT.0.D0)
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
!		
			DEALLOCATE(Z1,Z2,H1,H2,vel1,vel2,HV1,HV2,Htot1,Htot2)
!
            DTMP1 = P_SUM(FLX(ISEC))
            DTMP2 = P_SUM(VOLNEG(ISEC))
            DTMP3 = P_SUM(VOLPOS(ISEC))
!
            WRITE(LU,233) ISEC,TRIM(CHAIN(ISEC)%DESCR),
     &                    DTMP1,DTMP2,DTMP3
233         FORMAT(1X,/,1X,'CONTROL SECTION NUMBER ',1I2,
     &               ' (NAME ',A,')',//,5X,
     &               'DISCHARGE: '                 ,G16.7,/,5X,
     &               'NEGATIVE VOLUME THROUGH THE SECTION: ',G16.7,/,5X,
     &               'POSITIVE VOLUME THROUGH THE SECTION: ',G16.7)
!
          ENDDO
        ENDIF
!
      ENDIF
      ENDIF
!
!-----------------------------------------------------------------------
! MASTER WRITES A NICE SECTIONS OUTPUT FILE, THE HEADER ONLY ONCE
!
      IF ( (.NOT.OLD_METHOD_FPR) .AND.
     &      (TRIM(T2D_FILES(T2DSEO)%NAME).NE.'') ) THEN
        IF (INIT_FPR) THEN
          INIT_FPR=.FALSE.
          IF ((NCSIZE.GT.1 .AND. IPID.EQ.0).OR.(NCSIZE.LE.1)) THEN
            NSEO_FPR=T2D_FILES(T2DSEO)%LU
            IF(LNG.EQ.LNG_FR) THEN
              WRITE(NSEO_FPR,*) 'TITRE = "FLUX ET HAUTEURS POUR ',
     &         TRIM(TITCAS),'"'
            ELSEIF(LNG.EQ.LNG_EN) THEN
              WRITE(NSEO_FPR,*) 'TITLE = "FLUXES AND HEIGHTS FOR ',
     &         TRIM(TITCAS),'"'
            ENDIF
			WRITE(NSEO_FPR,*) 'VARIABLES =           NAME ', 
     &         '       ID       TIME    ',
     &         '  DISCHARGE      WL_MEAN     WL_MAX    ',	 
     &         '  WL_MIN     MAX_DEPTH   HYDRAULIC HEAD'
          ENDIF
          IF (NCSIZE.GT.1) THEN
            ALLOCATE (WORK_FPR(NSEC), STAT=ERR)
            IF (ERR.NE.0) THEN
              WRITE(LU,*)
     &          'FLUXPR_TELEMAC2D: ERROR ALLOCATING WORK_FPR:',ERR
              CALL PLANTE(1)
              STOP
            ENDIF
          ENDIF
        ENDIF
        ! DEADLOCK WITH WRITE AND P_SUM IN AN IMPLIED WRITE LOOP
        ! BECAUSE IT IS ONLY MASTER TO WRITE THE MESSAGE...
        IF (NCSIZE.GT.1) THEN
          DO ISEC=1,NSEC
            WORK_FPR(ISEC) = P_SUM(FLX(ISEC))
			SEC_NC(ISEC) = P_SUM(SIZENC_SEC(ISEC))
			IF(SEC_NC(ISEC).EQ.0) THEN 
			  SEC_NC(ISEC)=1
			ENDIF
			ZZmoy(ISEC) = P_SUM(Zmoy(ISEC))/SEC_NC(ISEC)
			ZZmax(ISEC) = P_MAX(Zmax(ISEC))
			ZZmin(ISEC) = P_MIN(Zmin(ISEC))
			HHtot(ISEC) = P_SUM(Htot(ISEC))/SEC_NC(ISEC)
			HHmax(ISEC) = P_MAX(Hmax(ISEC))

			
          END DO
		  
!2000    FORMAT(A30,4X,I3,4X,F8.2,4X,6(F8.2,4X))

        IF (IPID.EQ.0) THEN
			DO ISEC=1,NSEC
				WRITE(NSEO_FPR,FMT=FMTZON) TRIM(CHAIN(ISEC)%DESCR),ISEC,
     &                      TPS,WORK_FPR(ISEC),
     &                      ZZmoy(ISEC),ZZmax(ISEC),ZZmin(ISEC),	 
     &                      HHmax(ISEC),HHtot(ISEC)
			END DO	
		ENDIF		



        ELSE
			DO ISEC=1,NSEC
				WRITE(NSEO_FPR,FMT=FMTZON) TRIM(CHAIN(ISEC)%DESCR),ISEC,
     &                      TPS,FLX(ISEC),
     &                      Zmoy(ISEC),Zmax(ISEC),Zmin(ISEC),	 
     &                      Hmax(ISEC),Htot(ISEC)
			END DO	
        ENDIF
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
