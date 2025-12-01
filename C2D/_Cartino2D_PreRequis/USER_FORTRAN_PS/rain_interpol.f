!                   *******************
                    SUBROUTINE RAIN_INTERPOL
!                   *******************
     &(NPOIN,MM_AT2_S2,AT11,AT22,FILES)
!
!***********************************************************************
! TELEMAC2D   V8P3                                   09/11/2022
!***********************************************************************
!
!brief    USES MINIMUM DISTANCE TO INTERPOLATE SPATIAL RAINFALL
!			 ON MESH NODES 
!
!history  N.HOCINI (CEREMA MED)
!+        09/11/2022
!+        V8P3
!+   Initial version.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| NPOIN          |-->| NUMBER OF NODES IN THE MESH
!| AT11           |<->| FIRST TIMESTEP IN RAIN FILE 
!| AT22           |<->| SECOND TIME STEP IN RAIN FILE
!| MM_AT2_S2      |<->| RAIN VECTOR IN MM 
!| FILES          |-->| BIEF_FILES STRUCTURES OF ALL FILES
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_SPECIAL
      USE DECLARATIONS_TELEMAC2D, ONLY: T2DFO2,X,Y,AT,DT
 
!
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!     VARIABLES THAT ARE USED BY THE SUBROUTINE TO PASS DATA IN AND OUT
      INTEGER,INTENT(IN) :: NPOIN ! NUMBER OF MESH NODES
	  DOUBLE PRECISION, INTENT(INOUT) :: AT11,AT22 ! RAINFALL TIMES TO READ
	  DOUBLE PRECISION, INTENT(INOUT) :: MM_AT2_S2(NPOIN) ! RAINFALL FOR EVERY NODE TO EXPORT 
	  TYPE(BIEF_FILE), INTENT(IN) :: FILES(*) ! IMPORTED TO READ RAIN FILE 
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+


!     VARIABLES THAT ARE INTERNAL TO THE SOUBROUTINE
	! INTEGERS USED IN LOOPS
      INTEGER NUMPIXELS, NUMTIME,IDIST_MIN(1),UC, NLINE 
	  INTEGER BB, KK, JJ,I, J

	! VARIABLES FROM RAIN FILE 
      DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: XX, YY, DIST ! COORDS OF RAIN POINTS AND DISTANCES  
	  DOUBLE PRECISION, DIMENSION(:,:), ALLOCATABLE, SAVE :: MAT_R ! MATRIX FOR X, Y AND RAIN VALUE 
      DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE, SAVE :: MM_AT2_SA ! FOR READNIG RAIN VECTOR 
	  INTEGER, DIMENSION(:), ALLOCATABLE :: IND_MIN ! INDEX OF NEAREST RAIN POINT 
	  INTEGER, DIMENSION(:), ALLOCATABLE, SAVE :: IDM ! VECTOR OF NPOIN INDICES OF RAIN POINTS 
	  
	! SAVING NUMBER OF LINES READ FROM RAIN FILE? AND NUMBER OF RAIN POINTS 
	  SAVE NLINE,NUMPIXELS 
!
!-----------------------------------------------------------------------

!

! CONDITION ON TIME SO THAT THE COORDINATES ARE ONLY READ 
! ONE TIME WHERE THE NEAREST RAIN POINT INDICES IS CREATED 

	  IF(AT.EQ.DT) THEN
	  
!		LOGICAL UNIT OF RAIN FILE	  
	    UC = FILES(T2DFO2)%LU
		REWIND(UC)
	   
!	    READING FIRST LINE WHICH IS EMPTY 
        READ(UC,*)
!       READING NUMPIXELS AND NUMTIME 
        READ(UC,*) NUMPIXELS, NUMTIME

!		ALLOCATE THE ARRAYS
        ALLOCATE(XX(NUMPIXELS), YY(NUMPIXELS))
	    ALLOCATE(MAT_R(NUMPIXELS,3))
        ALLOCATE(IDM(NPOIN))
	    ALLOCATE(MM_AT2_SA(NUMPIXELS))
        ALLOCATE(DIST(NUMPIXELS))  
!   	READ RAIN COORDINATES
        DO BB = 1,NUMPIXELS
          READ(UC,*) XX(BB), YY(BB)
        ENDDO
!		READ RAIN TIME STEPS AND FIRST VECTOR OF RAIN VALUES 
	    READ(UC,*) AT11 !HERE WE DON'T NEED TO READ THE RAINFALL QUANTITY
        READ(UC,*) AT22,MM_AT2_SA
		
!       SAVING LAST LINE READ NUMBER 
	    NLINE = 4+NUMPIXELS

!		FILLING THE RAIN TABLE	    
	    MAT_R(:,1) = XX
        MAT_R(:,2) = YY
	    MAT_R(:,3) = MM_AT2_SA

!		LOOP TO CALCULATE MINIMUM DISTANCES 	    
	    DO J = 1, NPOIN
          DIST=SQRT((MAT_R(:,1)-X(J))**2 +
     &      (MAT_R(:,2)-Y(J))**2)		  
		  IND_MIN = MINLOC(DIST)

		  
          IDM(J) = IND_MIN(1)
		  MM_AT2_S2(J) = MAT_R(IDM(J),3)
          DO I = 1,NUMPIXELS
            DIST(I) = -999.0
          END DO

        END DO 
!       AFTER FIRST TIME STEP 
	  ELSE
	  	UC = FILES(T2DFO2)%LU
		REWIND(UC)
 	

		DO JJ = 1,NLINE
		  READ(UC,*)
		ENDDO
!		READING RAIN TIMESTEP AND VALUES         
        READ(UC,*) AT22,MM_AT2_SA
		MAT_R(:,3) = MM_AT2_SA
		  
!       RAIN VECTOR OF NPOIN 
        DO KK = 1,NPOIN
          MM_AT2_S2(KK) = MAT_R(IDM(KK),3)
        ENDDO

!		INCREMENT THE LAST LINE READ NUMBER 
		NLINE =  NLINE + 1 

      ENDIF	  	  

!-----------------------------------------------------------------------
!
      RETURN
      END
