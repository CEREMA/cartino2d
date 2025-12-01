!                    ***************************
                     SUBROUTINE FLUXPR_TELEMAC2D
!                    ***************************
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
      USE DECLARATIONS_TELEMAC2D, ONLY: AT,DT,ZF,H,NPOIN,X,Y,
     &                                  LISPRD,U,V,T2D_FILES,T2DSEC,
     &                                  T2D_FILES,T2DSEO,CHAIN,TITCAS,
     &                                  WORK_FPR, OLD_METHOD_FPR,
     &                                  INIT_FPR, NSEO_FPR
      USE DECLARATIONS_SPECIAL
      USE INTERFACE_PARALLEL, ONLY : P_DMAX,P_DMIN,P_DSUM,P_IMIN
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
      INTEGER ISEC,II,ERR
      CHARACTER(LEN=16), PARAMETER :: FMTZON='(4(1X,1PG21.14))'
      DOUBLE PRECISION :: DTMP1,DTMP2,DTMP3,DTMP4
!
!----------------------------------------------------------------------
!     Declaration P.CHASSE
!----------------------------------------------------------------------
!
	  DOUBLE PRECISION :: Zmin, Zmax
	  DOUBLE PRECISION a1,b1,Z1, Zmoy, Hmoy
	  DOUBLE PRECISION a2,b2,u1, u2, v1, v2, ds, velocity
	  DOUBLE PRECISION XPOIN, YPOIN
	  DOUBLE PRECISION, PARAMETER :: Lpoly=5.0D0
	  DOUBLE PRECISION, PARAMETER :: Hmin=0.20D0
	  DOUBLE PRECISION, PARAMETER :: gravity=9.81D0
	  DOUBLE PRECISION, DIMENSION(100,5) :: XPOLY,YPOLY
	  DOUBLE PRECISION, DIMENSION(4) :: XSOM,YSOM
	  DOUBLE PRECISION, DIMENSION(100) :: x0,y0,x1,y1,xx0,yy0,xx1,yy1
	  INTEGER, DIMENSION(100) :: NP_SEC
	  INTEGER, DIMENSION(100,2000) :: IP_SEC
	  INTEGER I1,I2,i,j,IPOIN,NSOM,IS,Nmoy
	  Character car
	  character*80 ligne
	  Character*44 NomFichier
	  Logical First, ok, inpoly
	  SAVE First,NP_SEC,IP_SEC
!
	  IF (TPS.LE.LISPRD*DT) THEN
			do IS=1,NSEC
				I1=CTRLSC(1+2*(IS-1))
				I2=CTRLSC(2+2*(IS-1))
				x0(IS)=X(I1)
				y0(IS)=Y(I1)
				x1(IS)=X(I2)
				y1(IS)=Y(I2)
			end do
!-----------------------------------------------------------------------
!       Calcul des polygones
!-----------------------------------------------------------------------
		DO IS=1,NSEC
		  a1=x0(IS)
		  b1=Y0(IS)
		  a2=x1(IS)
		  b2=y1(IS)
		  ds=sqrt((a2-a1)**2+(b2-b1)**2)
		  u1=(a2-a1)/ds
		  u2=(b2-b1)/ds
		  v1=u2
		  v2=-u1
		  NSOM=4
		  XPOLY(IS,1)=a1-v1*Lpoly
		  YPOLY(IS,1)=b1-v2*Lpoly
		  XPOLY(IS,2)=XPOLY(IS,1)+u1*ds
		  YPOLY(IS,2)=YPOLY(IS,1)+u2*ds
		  XPOLY(IS,3)=XPOLY(IS,2)+2*v1*Lpoly
		  YPOLY(IS,3)=YPOLY(IS,2)+2*v2*Lpoly
		  XPOLY(IS,4)=XPOLY(IS,3)-u1*ds
		  YPOLY(IS,4)=YPOLY(IS,3)-u2*ds
		  write(*,*) NSOM
		  write(*,*) (XPOLY(IS,j), j=1,NSOM)
		  write(*,*) (YPOLY(IS,j), j=1,NSOM)
		  ! read(*,*) car
		ENDDO
		DO IS=1,NSEC
			NP_SEC(IS)=0
		ENDDO
		ok=.FALSE.
		do IPOIN=1,NPOIN
			DO IS=1,NSEC
				DO i=1,NSOM
					XSOM(i)=XPOLY(IS,i)
					YSOM(i)=YPOLY(IS,i)
				END DO
				XPOIN=X(IPOIN)
				YPOIN=Y(IPOIN)
				ok=inpoly(XPOIN,YPOIN,XSOM,YSOM,NSOM)
				if (ok) then
					NP_SEC(IS)=NP_SEC(IS)+1
					IP_SEC(IS,NP_SEC(IS))=IPOIN
					ok=.FALSE.
				end if
			ENDDO
		ENDDO
		do IS=1,NSEC
			do j=1,NP_SEC(IS)
				write(LU,3000) IS,NP_SEC(IS),IP_SEC(IS,j)
				write(NSEO_FPR,3000) IS,NP_SEC(IS),IP_SEC(IS,j)
			end do
		end do
3000    format('fluxpr : ','IS = ',I4,2X,'NP = 'I4,2X,'IPOIN = ',I8)
	  END IF
!
!-----------------------------------------------------------------------
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
      II=P_IMIN(NSEG(ISEC))
!
      IF(II.GE.0) THEN
!
        DTMP1 = P_DMIN(FLX(ISEC))
        DTMP2 = P_DMAX(FLX(ISEC))
        DTMP3 = P_DMIN(VOLNEG(ISEC))
        DTMP4 = P_DMAX(VOLPOS(ISEC))
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
!
          DO ISEC = 1,NSEC
!
            DTMP1 = P_DSUM(FLX(ISEC))
            DTMP2 = P_DSUM(VOLNEG(ISEC))
            DTMP3 = P_DSUM(VOLPOS(ISEC))
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
              WRITE(NSEO_FPR,*) 'TITRE = "FLUX POUR ',TRIM(TITCAS),'"'
            ELSEIF(LNG.EQ.LNG_EN) THEN
              WRITE(NSEO_FPR,*) 'TITLE = "FLUXES FOR ',TRIM(TITCAS),'"'
            ENDIF
            WRITE(NSEO_FPR,*) 'VARIABLES = TIME',
     &         (' '//TRIM(CHAIN(ISEC)%DESCR),ISEC=1,NSEC)
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
        ! DEADLOCK WITH WRITE AND P_DSUM IN AN IMPLIED WRITE LOOP
        ! BECAUSE IT IS ONLY MASTER TO WRITE THE MESSAGE...
        IF (NCSIZE.GT.1) THEN
          DO ISEC=1,NSEC
            WORK_FPR(ISEC) = P_DSUM(FLX(ISEC))
          END DO
          IF (IPID.EQ.0)
     &      WRITE (NSEO_FPR, FMT=FMTZON) TPS,
     &                     (WORK_FPR(ISEC), ISEC=1,NSEC)
        ELSE
        !  WRITE (NSEO_FPR, FMT=FMTZON) TPS, (FLX(ISEC), ISEC=1,NSEC)
        ENDIF
      ENDIF
!----------------------------------------------------------------------
!       P.CHASSE
!----------------------------------------------------------------------
		DO IS=1,NSEC
		  write(*,*) IS,NP_SEC(IS)
		  Nmoy=0
		  Zmin=+1.0D6
		  Zmax=-1.0D6
		  Zmoy=0.0D0
		  Hmoy=0.0D0
		  do i=1,NP_SEC(IS)
			j=IP_SEC(IS,i)
			Z1=ZF%R(j)+H%R(j)
			if (AT.EQ.3000.0) then
				write(*,*) IS,j,Z1
!				read(*,*) car
			end if
			if (H%R(j).GT.Hmin) then
				Nmoy=Nmoy+1
				if (Z1.LT.Zmin) then
					Zmin=Z1
				endif
				if (Z1.GT.Zmax) then
					Zmax=Z1
				endif
				Zmoy=Zmoy+Z1
				velocity=sqrt(U%R(j)**2+v%R(j)**2)
				Hmoy=Hmoy+Z1+velocity**2/(2.0d0*gravity)
			end if
		  end do
		  if (Nmoy.NE.0) then
			Zmoy=Zmoy/Nmoy
			Hmoy=Hmoy/Nmoy
		  end if
		  write(NSEO_FPR,2000) TRIM(CHAIN(IS)%DESCR),IS,TPS,FLX(IS),
     &                         Zmin,Zmax,Zmoy,Hmoy
		  write(*,2000) TRIM(CHAIN(IS)%DESCR),IS,TPS,FLX(IS),Zmin,
     &                         Zmax,Zmoy,Hmoy
		ENDDO
2000    FORMAT(A30,1X,I3,1X,7(F8.2,1X))
!----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
!
      RETURN
      END
