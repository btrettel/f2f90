!***********************************************************************
!                                                                      *
!     f2f90 - A program to convert FORTRAN 77 fixed source form to     *
!             FORTRAN 90 free source form.                             *
!                                                                      *
!     Copyright (C) 1997  Peter A. Rochford, Sverdrup Technology, Inc. *
!                                                                      *
!     This program conforms to the GNU GENERAL PUBLIC LICENSE          *
!     version 2                                                        *
!                                                                      *
!     This program is free software; you can redistribute it and/or    *
!     modify it under the terms of the GNU General Public License as   *
!     published by the Free Software Foundation; either version 2 of   *
!     the License, or (at your option) any later version.              *
!                                                                      *
!     This program is distributed in the hope that it will be useful,  *
!     but WITHOUT ANY WARRANTY; without even the implied warranty of   *
!     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the    *
!     GNU General Public License for more details.                     *
!                                                                      *
!     A copy of the GNU General Public License can be obtained via     *
!     anonymous ftp at                                                 *
!                                                                      *
!     ftp://prep.ai.mit.edu/pub/gnu/COPYING-2.0                        *
!                                                                      *
!     or by writing to                                                 *
!                                                                      *
!     Free Software Foundation, Inc.                                   *
!     675 Mass Ave                                                     *
!     Cambridge, MA 02139                                              *
!     USA.                                                             *
!                                                                      *
!     f2f90 is a modified version of the Copyright convert program     *
!     developed by Michael Metcalf at CERN, Geneva.                    *
!                                                                      *
!     F2F90 PROGRAM                                                    *
!     Author: Peter Rochford  (rochford@nrlssc.navy.mil)               *
!             Advanced Systems Group                                   *
!             MSAAP Building 9110                                      *
!             Stennis Space Center, MS 39529                           *
!                                                                      *
!     Version 1.6. Differs from CONVERT in that:                       *
!      (08/06/97)                                                      *
!             The f90 "::" syntax is applied for type declaration      *
!             statements CHARACTER, INTEGER, LOGICAL, and REAL.        *
!                                                                      *
!             Command line processing, Makefile, and man page made     *
!             available for UNIX systems.                              *
!                                                                      *
!                                                                      *
!     CONVERT PROGRAM                                                  *
!     Copyright CERN, Geneva 1991, 1997 - Copyright and any other      *
!     appropriate legal protection of these computer programs          *
!     and associated documentation reserved in all countries           *
!     of the world.                                                    *
!                                                                      *
!     Author: Michael Metcalf  (metcalf@cern.ch)                       *
!                                                                      *
!     Requires the option -qcharlen=14400 with IBM's xlf.              *
!                                                                      *
!     Version 1.5. Differs from previous versions in that:             *
!      (12/19/96)                                                      *
!             Code modified to be Fortran 95 and ELF compatible (no    *
!             functional changes).                                     *
!                                                                      *
!***********************************************************************
!                                                                      *
!                                                                      *
!    A program to convert FORTRAN 77 source form to Fortran 90 source  *
!  form. It also formats the code by indenting the bodies of DO-loops  *
!  and IF-blocks by ISHIFT columns. Statement keywords are             *
!  followed if necessary by a blank, and blanks within tokens are      *
!  suppressed; this handling of blanks is optional.                    *
!    If a CONTINUE statement terminates a single DO loop, it is        *
!  replaced by END DO.                                                 *
!    Procedure END statements have the procedure name added, if        *
!  blanks are handled.                                                 *
!    Statements like INTEGER*2 are converted to INTEGER(2), if blanks  *
!  are handled. Depending on the target processor, a further global    *
!  edit might be required (e.g. where 2 bytes correspond to KIND=1).   *
!  Typed functions and assumed-length character specifications are     *
!  treated similarly. The length specification *4 is removed for all   *
!  data types except CHARACTER, as is *8 for COMPLEX. This             *
!  treatment of non-standard type declarations includes any            *
!  non-standard IMPLICIT statements.                                   *
!    Optionally, interface blocks only may be produced; this requires  *
!  blanks processing to be requested. The interface blocks are         *
!  compatible with both the old and new source forms. Files containing *
!  interface blocks will have a ".h" extension.                        *
!                                                                      *
!   Usage: the program reads one data record in free format from the   *
!          default input unit. This contains:                          *
!                                                                      *
!                        name of file                                  *
!                        indentation depth                             *
!                        maximum indentation level                     *
!                        whether significant blanks should be handled  *
!                        whether interface blocks only are required    *
!                                                                      *
!   The default values in the absence of this record are:              *
!                               name 3 10 T F                          *
!   To do nothing but change the source form of a file prog.f type     *
!                               prog 0  0 F F                          *
!   or simply                                                          *
!                               prog /                                 *
!   For more extensive processing type, say,                           *
!                               prog 3 10 t f                          *
!   and for interface blocks only type                                 *
!                               prog 0 0 t t                           *
!   The input is read from prog.f, the output is written to prog.f90;  *
!   there should be no tabs in the input.                              *
!                                                                      *
!   Restrictions:  The program does not indent FORMAT statements or    *
!                any statement containing a character string with an   *
!                embedded multiple blank.                              *
!                  The order of comment lines and Fortran statements   *
!                is slightly modified if there are sequences of        *
!                more than KKLIM (=200) comment lines.                 *
!                  If there are syntax errors, continued lines do not  *
!                have a trailing &.                                    *
!                  When producing interface blocks, a check is required*
!                that any dummy argument that is a procedure has a     *
!                corresponding EXTERNAL statement. Also, since no      *
!                COMMON blocks or PARAMETER statements are copied,     *
!                part of an assumed-size array declaration may be      *
!                missing. Similarly, parts of an assumed-length        *
!                character symbolic constant might be copied and have  *
!                to be deleted. BLOCK DATA statements are copied and   *
!                must be deleted. These problems would normally be     *
!                detected by a compiler and are trivially corrected.   *
!                  Within a given keyword, the case must be all upper  *
!                or all lower, and lower case programs require         *
!                blank handling for correct indenting.                 *
!                                                                      *
!***********************************************************************
!
   MODULE STRUCTURE
!
!***********************************************************************
!   Define maximum level of DO-loop nesting, and maximum length of     *
!   a Fortran statement. LEN may be reduced for                        *
!   compilers accepting a maximum character                            *
!   length below 2640 and this will cause any excess                   *
!   continuation lines and all following lines to be copied unchanged. *
!   KKLIM defines the length of the comment line buffer. If this       *
!   length is exceeded, the statement preceding the comments will      *
!   appear after them.                                                 *
!***********************************************************************
      implicit none
      public
      INTEGER, PARAMETER :: NEST = 32 , LEN = 2640 , KKLIM = 200,      &
      KLEN = 72*KKLIM
!
      INTEGER :: KNTDO , KNTIF , KNTCOM , LABEL , LENST , LABLNO, NOARG
      INTEGER, DIMENSION(NEST) :: LABLDO
!
      LOGICAL :: SYNERR, BLNKFL, INTFL
!
      CHARACTER(LEN=LEN) :: STAMNT
      CHARACTER(LEN=KLEN):: CBUF
      CHARACTER(LEN=42)  :: NAME
!
   END MODULE STRUCTURE
   MODULE DATA
   implicit none
   public
!
      CHARACTER (LEN=256) :: NAMEORIG
      INTEGER, PARAMETER :: INFILE=5,NIN=1,NOUT=2
      INTEGER, SAVE :: ISHIFT, MXDPTH, TIME0
      LOGICAL, SAVE :: BLANKS, INTBFL
!
   END MODULE DATA
   MODULE STATISTICS
   implicit none
   public
!
      INTEGER, SAVE :: MXDO , MXIF , KARD , KNTPU
!
      LOGICAL, SAVE :: SYNTAX, OVFLW, NONSTD
!
   END MODULE STATISTICS
   MODULE ALL_PROCEDURES
   private
   public :: start, program_units, terminate
   CONTAINS
!***********************************************************************
   SUBROUTINE ARGUMENT(ARGNAM, LENARG, STAMNT, LENST, NOARG)
   implicit none
!
!   To store the argument names and function name, if any, for later
!   use in checking whether a specification statement is relevant to an
!   interface block.
      CHARACTER(LEN=*), INTENT(IN OUT), dimension(:) :: ARGNAM
      CHARACTER(LEN=*), INTENT(IN)         :: STAMNT
      INTEGER, INTENT(OUT), dimension(:)   :: LENARG
      INTEGER, INTENT(IN OUT) :: NOARG
      INTEGER, INTENT(IN)    :: LENST
!
      integer :: ind1, ind2, newind
!
!   Correct length of function name
      IF (NOARG == 1) LENARG(1) = LEN_TRIM(ARGNAM(1))
!
!   Get any other arguments
      IND1 = index(STAMNT(:LENST), '(') + 1
      IF (IND1  /=  1 .AND. STAMNT(IND1:IND1)  /=  ')') THEN
         NEWIND = index(STAMNT(IND1+1:LENST), '(')
         IF (NEWIND /= 0) IND1 = NEWIND + 1 + IND1
    3    IND2 = index(STAMNT(IND1:LENST), ',') - 1
         IF (IND2  ==  -1) IND2 = index(STAMNT(IND1:LENST), ')') - 1
         IND2 = IND2 + IND1 - 1
         IF (STAMNT(IND1+1:IND1+1)  /=  '*' ) THEN
            NOARG = NOARG +1
            ARGNAM(NOARG) = STAMNT(IND1:IND2)
            LENARG(NOARG) = IND2 - IND1 +1
         END IF
            IF (STAMNT(IND2+1:IND2+1)  ==  ')') GO TO 4
         IND1 = IND2 + 3
         GO TO 3
      END IF
    4 LENARG(:NOARG) = MIN(LENARG(:NOARG), 6)
!
   RETURN
   END SUBROUTINE ARGUMENT
   SUBROUTINE BLANK( )
!
!   To suppress all blanks in the statement, and then to place
!   a blank on each side of =,  +, -, * and / (but not ** or //), a
!   blank after each ) and , and a blank before each (.
!   No changes are made within character strings or FORMAT statememts.
!
      USE DATA
!
      USE STATISTICS
!
      USE STRUCTURE
   implicit none
!
      CHARACTER(LEN=LEN) :: BUFFER
      integer :: l1, l2, lchar, napost, lenold
!
!   Reduce length to that of significant characters
      BLNKFL = .FALSE.
      LENST = LEN_TRIM(STAMNT(1:LENST))
      IF (.NOT.BLANKS) THEN
         IF (LEN-LENST >= 2) STAMNT(LENST+1:LENST+2) = '  '
         LENST = MIN(LENST+2, LEN)
         GO TO 99
      END IF
      BLNKFL = .TRUE.
!
!   Suppress blanks (add 2 to catch
!   odd number of apostrophes on a line in REFORM).
      LCHAR = 0
      NAPOST = 0
      DO L1 = 1, LENST
         IF (STAMNT(L1:L1)  ==  "'") NAPOST = 1-NAPOST
         IF (NAPOST == 0 .AND. STAMNT(L1:L1)  ==  ' ') CYCLE
         LCHAR = LCHAR+1
         BUFFER(LCHAR:LCHAR) = STAMNT(L1:L1)
      END DO
      IF (LEN-LCHAR >= 2) BUFFER(LCHAR+1:LCHAR+2) = '  '
      LCHAR = MIN(LCHAR+2, LEN)
!
!   Eliminate FORMATS
       IF( LABEL  /=  0 .AND.                                          &
     & LCHAR  >=  11 .AND.(BUFFER(:7)  ==  'FORMAT(' .OR.              &
     &                     BUFFER(:7)  ==  'format(') .AND.            &
     & BUFFER(LCHAR-2:LCHAR-2)  ==  ')') THEN
         IF (LEN-LENST >= 2) STAMNT(LENST+1:LENST+2) = '  '
         LENST = MIN(LENST+2, LEN)
         GO TO 99
       END IF
!
!   Insert blanks
      LENOLD = LENST
      LENST = 0
      NAPOST = 0
      DO L2 = 1, LCHAR
!
!   Check size of statement
         IF(LENST+3 > LEN) THEN
            LENST = LCHAR
            STAMNT(:LENST) = BUFFER(:LENST)
            OVFLW = .TRUE.
            GO TO 99
         END IF
!
!   Whether inside character string
         IF (BUFFER(L2:L2)  ==  "'") NAPOST = 1-NAPOST
         IF (NAPOST == 1) GO TO 3
!
!   Add blank padding according to character
         SELECT CASE (BUFFER(L2:L2))
         CASE ( ')' )
            STAMNT(LENST+1:LENST+2) = ') '
            LENST = LENST+2
         CASE ( '(' )
            STAMNT(LENST+1:LENST+2) = ' ('
            LENST = LENST + 2
         CASE ( ',' )
            STAMNT(LENST+1:LENST+2) = ', '
            LENST = LENST + 2
         CASE ( '=' )
            STAMNT(LENST+1:LENST+3) = ' = '
            LENST = LENST + 3
         CASE ( '*' )
            IF (BUFFER(L2-1:L2-1)  /=  '*' .AND. BUFFER(L2+1:L2+1)     &
             /=  '*') THEN
               STAMNT(LENST+1:LENST+3) = ' * '
               LENST = LENST + 3
            ELSE
               GO TO 3
            END IF
         CASE ( '/' )
            IF (BUFFER(L2-1:L2-1)  /=  '/' .AND. BUFFER(L2+1:L2+1)     &
             /=  '/') THEN
               STAMNT(LENST+1:LENST+3) = ' / '
               LENST = LENST + 3
            ELSE
               GO TO 3
            END IF
         CASE ('+')
            IF (BUFFER(L2-1:L2-1)  /=  'E' .AND.                       &
                BUFFER(L2-1:L2-1)  /=  'e' .AND.                       &
                BUFFER(L2-1:L2-1)  /=  'D' .AND.                       &
                BUFFER(L2-1:L2-1)  /=  'd' .OR.                        &
          LLT(BUFFER(L2+1:L2+1), '0') .AND. LGT(BUFFER(L2+1:L2+1), '9')&
               ) THEN
               STAMNT(LENST+1:LENST+3) = ' + '
               LENST = LENST + 3
            ELSE
               GO TO 3
            END IF
         CASE ('-')
            IF (BUFFER(L2-1:L2-1)  /=  'E' .AND.                       &
                BUFFER(L2-1:L2-1)  /=  'e' .AND.                       &
                BUFFER(L2-1:L2-1)  /=  'D' .AND.                       &
                BUFFER(L2-1:L2-1)  /=  'd' .OR.                        &
          LLT(BUFFER(L2+1:L2+1), '0') .AND. LGT(BUFFER(L2+1:L2+1), '9')&
               ) THEN
               STAMNT(LENST+1:LENST+3) = ' - '
               LENST = LENST + 3
            ELSE
               GO TO 3
            END IF
         CASE DEFAULT
            GO TO 3
         END SELECT
         CYCLE
    3    STAMNT(LENST+1:LENST+1) = BUFFER(L2:L2)
         LENST = LENST +1
      END DO
!
!   Blank out end of statement
      IF (LENOLD > LENST) STAMNT(LENST+1:LENOLD) = ' '
      IF (LENST < LEN .AND. MOD(LENST, 66) /= 0)                       &
          STAMNT(LENST+1: LENST+66-MOD(LENST, 66)) = ' '
!
99 RETURN
   END SUBROUTINE BLANK
   SUBROUTINE CONVCASE(CIN,COUT,LEN,UPCASE)
   IMPLICIT NONE
!
   INTEGER ::    LEN
   CHARACTER (LEN=*) :: CIN,COUT
   LOGICAL ::    UPCASE
!
!**********
!*
!  1) CONVERT A CHARACTER STRING TO ALL UPPER CASE OR ALL
!     LOWERCASE.
!
!  2) ARGUMENTS:
!
!     CIN    = INPUT CHARACTER STRING
!     COUT   = OUTPUT CHARACTER STRING IN CONVERTED CASE
!     LEN    = THE CHARACTER LENGTH OF CIN
!     UPCASE = CASE CONVERSION FLAG
!            = .TRUE.  FOR UPPER CASE
!            = .FALSE. FOR LOWER CASE
!
!  2) ALAN J. WALLCRAFT, PLANNING SYSTEMS INC., NOVEMBER 1992.
!     PETER A. ROCHFORD, SVERDRUP TECHNOLOGY INC., MAY 1996.
!       ALLOWED OPTION OF CONVERSION TO LOWERCASE.
!*
!**********
!
   INTEGER :: I,ILETTR,ILOWR0,IUPPR0
!
   IUPPR0 = IACHAR('A') - 1
   ILOWR0 = IACHAR('a') - 1
   COUT = ''
!
   IF(UPCASE) THEN
     DO I= 1,LEN
       ILETTR = IACHAR(CIN(I:I)) - ILOWR0
       IF     (ILETTR.GE.1 .AND. ILETTR.LE.26) THEN
!
!        CONVERT TO UPPER CASE.
!
         COUT(I:I) = ACHAR( IUPPR0 + ILETTR )
       ELSE
!
!        COPY UNCHANGED.
!
         COUT(I:I) = CIN(I:I)
       ENDIF
     ENDDO
   ELSE
     DO I= 1,LEN
       ILETTR = IACHAR(CIN(I:I)) - IUPPR0
       IF     (ILETTR.GE.1 .AND. ILETTR.LE.26) THEN
!
!        CONVERT TO UPPER CASE.
!
         COUT(I:I) = ACHAR( ILOWR0 + ILETTR )
       ELSE
!
!        COPY UNCHANGED.
!
         COUT(I:I) = CIN(I:I)
       ENDIF
     ENDDO
   ENDIF
   END SUBROUTINE CONVCASE
   SUBROUTINE IDENTIFY (IRET)
!
!***********************************************************************
!   To identify statement as beginning or end of DO-loop or            *
!   IF-block, or as probable FORMAT.                                   *
!   Attempt to scan as few of the input characters as possible.        *
!***********************************************************************
!
      USE STRUCTURE
      USE DATA
   implicit none
!
      CHARACTER(LEN=5), PARAMETER :: ENDIF='ENDIF' , THEN='NEHT)',     &
                                     ENDIFLC='endif', THENLC='neht)'
      CHARACTER(LEN=3), PARAMETER :: BIF='IF(',BIFLC='if('
      CHARACTER(LEN=2), PARAMETER :: DO='DO',DOLC='do'
      CHARACTER(LEN=7), PARAMETER :: FORMAT='FORMAT(', FORMATLC='format('
      CHARACTER(LEN=4), PARAMETER :: ELSE='ELSE', ELSELC='else'
      CHARACTER(LEN=5)            :: INTFIL
      INTEGER, INTENT(OUT)        :: IRET
!
      integer :: l1, l2, l3, l4, l5, l6, l7, l8, l9, l10, l11, l12,    &
                 k1, k2, k3, k5, k6, k7, k8, lparen, kntch, napos
!
      IRET = 0
!
!   Check whether end of DO-loop
      IF (KNTDO /= 0) THEN
         IF (LABEL == LABLDO(KNTDO)) THEN
            IRET = 2
            RETURN
         END IF
      END IF
!
!   Check whether any of remaining possibilities
      DO L7 = 1 , LENST
         IF (STAMNT(L7:L7) == ' ') CYCLE
         IF (STAMNT(L7:L7) == 'E' .OR. STAMNT(L7:L7) == 'e') THEN
            DO L11 = L7+1 , LENST
               IF (STAMNT(L11:L11) == ' ') CYCLE
               IF (STAMNT(L11:L11) == ENDIF(2:2) .OR. &
                   STAMNT(L11:L11) == ENDIFLC(2:2) ) GO TO 6
               IF (STAMNT(L11:L11) == ELSE(2:2) .OR.  &
                   STAMNT(L11:L11) == ELSELC(2:2) ) GO TO 3
               RETURN
            END DO
         END IF
         IF (STAMNT(L7:L7) == BIF(:1) .OR. &
             STAMNT(L7:L7) == BIFLC(:1) ) GO TO 9
         IF (STAMNT(L7:L7) == DO(:1) .OR. &
             STAMNT(L7:L7) == DOLC(:1)) GO TO 15
         IF (STAMNT(L7:L7) == FORMAT(:1) .OR. &
             STAMNT(L7:L7) == FORMATLC(:1)) GO TO 31
         RETURN
      END DO
      RETURN
!
!   Check whether ELSE or ELSEIF
    3 K8 = 3
      DO L12 = L11+1 , LENST
         IF (STAMNT(L12:L12) == ' ') CYCLE
         IF (STAMNT(L12:L12) /= ELSE(K8:K8) .AND. &
             STAMNT(L12:L12) /= ELSELC(K8:K8)) RETURN
         IF (K8 == 4) GO TO 5
         K8 = K8+1
      END DO
      RETURN
    5 IF (L12 >= LENST) THEN
         IRET = 6
         RETURN
      END IF
      IF (STAMNT(L12+1:LENST) == ' ') THEN
         IRET = 6
         RETURN
      END IF
      K2 = 1
      IRET = 6
      L7 = L12
      GO TO  10
!
!   Check whether end of IF-block
    6 K1 = 3
      DO L1 = L11+1 , LENST
         IF (STAMNT(L1:L1) == ' ') CYCLE
         IF (STAMNT(L1:L1) /= ENDIF(K1:K1) .AND. &
             STAMNT(L1:L1) /= ENDIFLC(K1:K1)) RETURN
         IF (K1 == 5) EXIT
         K1 = K1+1
      END DO
      IF (L1 >= LENST) THEN
         IRET = 4
         RETURN
      END IF
      IF (STAMNT(L1+1:LENST) == ' ') IRET = 4
      RETURN
!
!   Check whether beginning of IF-block
    9 K2 = 2
      IRET = 3
   10 DO L2 = L7+1 , LENST
         IF (STAMNT(L2:L2) == ' ') CYCLE
         IF (STAMNT(L2:L2) /= BIF(K2:K2) .AND. &
             STAMNT(L2:L2) /= BIFLC(K2:K2)) THEN
            IRET = 0
            RETURN
         END IF
         IF (K2 == 3) GO TO 12
         K2 = K2+1
      END DO
      IRET = 0
      RETURN
!
!   Backward search for )THEN at end of IF statement (to save
!   scanning the condition).
   12 K3 = 1
      DO L3 = LENST , L2+1 , -1
         IF (STAMNT(L3:L3) == ' ') CYCLE
         IF (STAMNT(L3:L3) /= THEN(K3:K3) .AND.                        &
             STAMNT(L3:L3) /= THENLC(K3:K3)) THEN
            IRET = 0
            RETURN
         END IF
         IF (K3 == 5) RETURN
         K3 = K3+1
      END DO
      IRET = 0
      RETURN
!
!   Check whether beginning of DO-loop
   15 DO L4 = L7+1 , LENST
         IF (STAMNT(L4:L4) == ' ') CYCLE
         IF (STAMNT(L4:L4) == DO(2:2) .OR. &
             STAMNT(L4:L4) == DOLC(2:2)) GO TO 17
         RETURN
      END DO
      RETURN
!
!   Have DO - check label
   17 K5 = 0
      INTFIL = ' '
      DO L5 = L4+1 , LENST
         IF (STAMNT(L5:L5) == ' ') CYCLE
         IF (LLT(STAMNT(L5:L5) , '0') .OR. LGT(STAMNT(L5:L5) , '9'))   &
         EXIT
         K5 = K5+1
         IF (K5 > 5) GO TO 20
         INTFIL(K5:K5) = STAMNT(L5:L5)
      END DO
      IF (K5 == 0) RETURN
   20 READ (INTFIL , '(BN , I5)') LABLNO
      IF (LABLNO == 0) RETURN
!
!   Have label - check comma
      DO L8 = L5, LENST
         IF (STAMNT(L8:L8) == ' ') CYCLE
         IF (STAMNT(L8:L8) == ',') EXIT
         GO TO 23
      END DO
      IRET = 1
      RETURN
!
!   Have a DO and label with no comma.
!   Check for variable whose first of maximum of six
!   characters is alphabetic, followed by an equals sign,
!   followed by a character string containing a comma which is
!   not enclosed in parentheses.
   23 K6 = 0
      DO L9 = L8 , LENST
         IF (STAMNT(L9:L9) == ' ') CYCLE
         IF (K6 == 0) THEN
            IF ((LLT(STAMNT(L9:L9), 'A') .OR. LGT(STAMNT(L9:L9), 'Z')) &
           .AND.(LLT(STAMNT(L9:L9), 'a') .OR. LGT(STAMNT(L9:L9), 'z')))&
            RETURN
            K6 = 1
         ELSE IF (LGE(STAMNT(L9:L9) , 'A') .AND. LLE(STAMNT(L9:L9),'Z')&
            .OR. LGE(STAMNT(L9:L9) , 'a') .AND. LLE(STAMNT(L9:L9) ,'z')&
         .OR. LGE(STAMNT(L9:L9) , '0') .AND. LLE(STAMNT(L9:L9) , '9')) &
         THEN
            K6 = K6+1
            IF (K6 == 6) GO TO 26
         ELSE
            IF (K6 == 0) RETURN
            GO TO 25
         END IF
      END DO
      RETURN
!
!   Expect an equals sign
   25 L9=L9-1
   26 DO L10 = L9+1 , LENST
         IF (STAMNT(L10:L10) == ' ') CYCLE
         IF (STAMNT(L10:L10) == '=') GO TO 28
         RETURN
      END DO
      RETURN
!
!   Search for bare comma
   28 LPAREN = 0
      KNTCH = 0
      NAPOS = 0
      DO L6 = L10+1 , LENST
         IF (STAMNT(L6:L6) == ' ') CYCLE
         IF (STAMNT(L6:L6) == "'") NAPOS = 1 - NAPOS
         IF (NAPOS == 1) CYCLE
         IF (STAMNT(L6:L6) == ',') THEN
            IF (KNTCH /= 0) THEN
               IF (LPAREN == 0) GO TO 30
               CYCLE
            ELSE
               RETURN
            END IF
         ELSE IF (STAMNT(L6:L6) == '(') THEN
            LPAREN = LPAREN+1
         ELSE IF (STAMNT(L6:L6) == ')') THEN
            LPAREN = LPAREN-1
         END IF
         KNTCH = 1
      END DO
      RETURN
   30 IRET = 1
!
!   Insert blank after label
      IF (.NOT.BLANKS .OR. LENST >= LEN) RETURN
      DO L10 = LENST, L5, -1
         STAMNT(L10+1:L10+1) = STAMNT(L10:L10)
      END DO
      STAMNT(L5:L5) = ' '
      LENST = LENST  + 1
      RETURN
!
!   Identify FORMAT statement
   31 IF (LABEL == 0) RETURN
      K7 = 2
      DO L11 = L7+1 , LENST
         IF (STAMNT(L11:L11) == ' ') CYCLE
         IF (STAMNT(L11:L11) /= FORMAT(K7:K7) .AND. &
             STAMNT(L11:L11) /= FORMATLC(K7:K7)) RETURN
         IF (K7 == 7) GO TO 33
         K7 = K7+1
      END DO
      RETURN
   33 IRET = 5
!
   END SUBROUTINE IDENTIFY
   SUBROUTINE INPUT(NAME)
      USE DATA
!
      CHARACTER (LEN=*) :: NAME
      CHARACTER (LEN=512) :: CARG
      CHARACTER (LEN=256) :: CRECORD
      INTEGER :: I,IBLANK,NARGC
      LOGICAL :: LGETARG
!
!     External functions
      INTEGER :: IARGC
!
      LGETARG = .TRUE. ! Obtain arguments directly from command line.
!      LGETARG = .FALSE. ! Obtain arguments from program prompt.
!
      CARG = ''
      IF(LGETARG) THEN 
!       Obtain arguments directly from command line.
!       Assumes C library is available
        NARGC = IARGC()
        IF (NARGC /= 1 .AND. NARGC /= 5) CALL USAGE

          DO I = 1, NARGC
          CALL GET_ARGUMENT(I,CARG)

          ARGS_CASE: SELECT CASE(I)
           CASE (1)
             NAME = TRIM(CARG)
           CASE (2)
             READ (CARG, *) ISHIFT
           CASE (3)
             READ (CARG, *) MXDPTH
           CASE (4)
             IF(CARG(1:1) /= 'f' .AND. CARG(1:1) /= 'F' .AND. &
                CARG(1:1) /= 't' .AND. CARG(1:1) /= 'T') THEN
               WRITE(*,*) ' '
               WRITE(*,*) 'blank_flag must be one of [ftFT].'
               WRITE(*,*) ' '
               WRITE(*,*) 'Usage: f2f90 filename ishift indent_level &
                          &blank_flag interface_flag'
               WRITE(*,*) ' '
               CALL TERMINATE(1)
             ENDIF
             READ (CARG, *) BLANKS
           CASE (5)
             IF(CARG(1:1) /= 'f' .AND. CARG(1:1) /= 'F' .AND. &
                CARG(1:1) /= 't' .AND. CARG(1:1) /= 'T') THEN
               WRITE(*,*) ' '
               WRITE(*,*) 'interface_flag must be one of [ftFT].'
               WRITE(*,*) ' '
               WRITE(*,*) 'Usage: f2f90 filename ishift indent_level &
                          &blank_flag interface_flag'
               WRITE(*,*) ' '
               CALL TERMINATE(1)
             ENDIF
             READ (CARG, *) INTBFL
          END SELECT ARGS_CASE
        ENDDO
        IF(NARGC == 1) THEN
          WRITE (*,'(" Defaults used for options.")')
        ENDIF
!
      ELSE
!
!      Obtain arguments from prompts because C library is unavailable
       WRITE (6, FMT='(A)', ADVANCE= 'NO') &
         'Enter arguments within single quotes:'
       READ (*,*) CRECORD
       CRECORD = ADJUSTL(CRECORD)

!      Process record for arguments
       IBLANK = INDEX(CRECORD,' ')
       IF(IBLANK==0) CALL USAGE
       NAME = CRECORD(:IBLANK)      
       IF(NAME == ' ') CALL USAGE
       CRECORD=ADJUSTL(CRECORD(IBLANK:))
       LENGTH = LEN_TRIM(CRECORD)
       IF(LENGTH > 0) THEN
         READ (CRECORD, *, END = 1, ERR = 1) ISHIFT, MXDPTH , BLANKS, INTBFL
       ELSE
         WRITE (*,'(" Defaults used for options.")')
       ENDIF
       RETURN
!      Specify usage
1      CALL USAGE
     ENDIF
   END SUBROUTINE INPUT
   SUBROUTINE KEYWORD(ASSIGN, SKIP, LOWERCASE)
!
!   To check whether those initial keywords of the statement which
!   require it are followed by a blank, to add one if necessary, and
!   to suppress any embedded blanks.
!
      USE STATISTICS
!
      USE STRUCTURE
   implicit none
!
      LOGICAL, INTENT(OUT) :: ASSIGN, SKIP, LOWERCASE
!
      INTEGER, PARAMETER    :: NKEY = 42, MAXLEN = 15
      CHARACTER(LEN=MAXLEN) :: BEGIN
      CHARACTER(LEN=LEN)    :: BUFFER
      CHARACTER(LEN=3)      :: THREE
      CHARACTER(LEN=256)     :: NAMEOF
      CHARACTER(LEN=6), SAVE :: ARGNAM(445)
      LOGICAL               :: IFASS,LARGUMENT,LPROGRAM_ENTITY
      INTEGER, SAVE         :: LENARG(445)
!
      CHARACTER(LEN=MAXLEN), PARAMETER, DIMENSION(NKEY) :: KEYS = (/   &
      'ASSIGN         ', 'BACKSPACE      ', 'BLOCKDATA      ',         &
      'CALL           ', 'CHARACTER      ', 'CLOSE          ',         &
      'COMMON         ', 'COMPLEX        ', 'CONTINUE       ',         &
      'DATA           ', 'DIMENSION      ', 'DOUBLEPRECISION',         &
      'DO             ', 'ELSEIF         ', 'ELSE           ',         &
      'ENDFILE        ', 'ENDIF          ', 'ENTRY          ',         &
      'EXTERNAL       ', 'EQUIVALENCE    ', 'FORMAT         ',         &
      'FUNCTION       ', 'GOTO           ', 'IF             ',         &
      'IMPLICIT       ', 'INQUIRE        ', 'INTEGER        ',         &
      'INTRINSIC      ', 'LOGICAL        ', 'OPEN           ',         &
      'PARAMETER      ', 'PAUSE          ', 'PRINT          ',         &
      'PROGRAM        ', 'READ           ', 'REAL           ',         &
      'RETURN         ', 'REWIND         ', 'SAVE           ',         &
      'STOP           ', 'SUBROUTINE     ', 'WRITE          '/)
      INTEGER, PARAMETER, DIMENSION(NKEY) :: LK =                      &
            (/6, 9, 9, 4,                                              &
              9, 5, 6, 7, 8, 4,                                        &
              9,15, 2, 6, 4,                                           &
              7, 5, 5, 8,11,                                           &
              6, 8, 4, 2, 8, 7,                                        &
              7, 9, 7, 4, 9,                                           &
              5, 5, 7, 4, 4, 6,                                        &
              6, 4, 4,10, 5    /)
      LOGICAL, PARAMETER, dimension(nkey) :: BLANK =                   &
               (/.TRUE.,  .TRUE.,  .TRUE.,  .TRUE.,                    &
                 .TRUE.,  .FALSE., .TRUE.,  .TRUE.,  .FALSE., .TRUE.,  &
                 .TRUE.,  .TRUE.,  .TRUE.,  .FALSE., .FALSE.,          &
                 .TRUE.,  .FALSE., .TRUE.,  .TRUE.,  .FALSE.,          &
                 .FALSE., .TRUE.,  .TRUE.,  .FALSE., .TRUE.,  .FALSE., &
                 .TRUE.,  .TRUE.,  .TRUE.,  .FALSE., .TRUE.,           &
                 .TRUE.,  .TRUE.,  .TRUE.,  .TRUE.,  .TRUE.,  .TRUE.,  &
                 .TRUE.,  .TRUE.,  .TRUE.,  .TRUE.,  .FALSE./)
      LOGICAL, PARAMETER, dimension(nkey) :: FOLLOW =                  &
               (/.TRUE.,  .TRUE.,  .FALSE., .TRUE.,                    &
                 .FALSE., .FALSE., .FALSE., .FALSE., .FALSE., .FALSE., &
                 .FALSE., .FALSE., .FALSE., .FALSE., .FALSE.,          &
                 .TRUE.,  .FALSE., .FALSE., .FALSE., .FALSE.,          &
                 .FALSE., .FALSE., .TRUE.,  .FALSE., .FALSE., .FALSE., &
                 .FALSE., .FALSE., .FALSE., .FALSE., .FALSE.,          &
                 .TRUE.,  .TRUE.,  .FALSE., .TRUE.,  .FALSE., .TRUE.,  &
                 .TRUE.,  .FALSE., .TRUE.,  .FALSE., .FALSE./)
!
      CHARACTER(LEN=MAXLEN), PARAMETER, dimension(nkey) :: KEYSLC = (/ &
      'assign         ', 'backspace      ', 'blockdata      ',         &
      'call           ', 'character      ', 'close          ',         &
      'common         ', 'complex        ', 'continue       ',         &
      'data           ', 'dimension      ', 'doubleprecision',         &
      'do             ', 'elseif         ', 'else           ',         &
      'endfile        ', 'endif          ', 'entry          ',         &
      'external       ', 'equivalence    ', 'format         ',         &
      'function       ', 'goto           ', 'if             ',         &
      'implicit       ', 'inquire        ', 'integer        ',         &
      'intrinsic      ', 'logical        ', 'open           ',         &
      'parameter      ', 'pause          ', 'print          ',         &
      'program        ', 'read           ', 'real           ',         &
      'return         ', 'rewind         ', 'save           ',         &
      'stop           ', 'subroutine     ', 'write          '/)
!
      integer :: l1, l2, l3, l4, l5, l6, l7, l8, l20, lparen, kntap, kntch,&
                 lc, lcc, l33, next, napos, lsave, name_length
!
!   Test for statement function statement or assignment statement
      SKIP = INTFL
      ASSIGN = .FALSE.
      IFASS = .FALSE.
      THREE = ' '
      LPAREN = 0
      KNTAP = 0
      KNTCH = 0
      DO L1 = 1, LENST
         IF (STAMNT(L1:L1)==' ') CYCLE
         IF (STAMNT(L1:L1)=='=') THEN
            IF (KNTCH==0) SYNERR = .TRUE.
            IF (LPAREN==0 .AND. KNTAP==0) THEN
               ASSIGN = .TRUE.
               GO TO 9
            ELSE
               EXIT
            END IF
         ELSE IF (STAMNT(L1:L1)=='(') THEN
            LPAREN = LPAREN+1
         ELSE IF (STAMNT(L1:L1)==')') THEN
            LPAREN = LPAREN-1
         ELSE IF (STAMNT(L1:L1)=="'") THEN
            KNTAP = 1-KNTAP
         END IF
         KNTCH = KNTCH+1
         IF (KNTCH<=3) THREE(KNTCH:KNTCH) = STAMNT(L1:L1)
      END DO
!
!   Suppress blanks in first 15 non-blank characters
   10 BEGIN = ' '
      LC = 0
      DO L2 = 1, LENST
         IF (STAMNT(L2:L2)==' ') CYCLE
         LC = LC+1
         BEGIN(LC:LC) = STAMNT(L2:L2)
         IF (LC==MAXLEN) GO TO 3
      END DO
      L2 = L2-1
!
!   Is this a keyword? Failure of this test is not fatal, in order to
!   allow for non-standard syntax extensions.
      LOWERCASE = .FALSE.
    3 DO L3 = 1, NKEY
         IF     (BEGIN(:LK(L3)) == KEYS(L3)(:LK(L3))) THEN
            GO TO 5
         ELSE IF (BEGIN(:LK(L3)) == KEYSLC(L3)(:LK(L3))) THEN
            LCC = 0
            DO  L33 = 1, L2
               IF (STAMNT(L33:L33) == ' ') CYCLE
               LCC = LCC + 1
               IF (LCC == LK(L3)) EXIT
            END DO
            STAMNT(:L33) = KEYSLC(L3)(:LK(L3))
            LOWERCASE = .TRUE.
            GO TO 5
         END IF
      END DO
      NONSTD = .TRUE.
      GO TO  98
!
!   Test for embedded blanks in keyword
    5 IF (L2 /= LC) THEN
         LC = 0
         DO L4 =1, LENST
            IF (STAMNT(L4:L4)==' ') CYCLE
            LC = LC+1
            IF (LC==LK(L3)) GO TO 7
         END DO
         GO TO 8
    7    IF (L4==LC) GO TO 8
         STAMNT(:L4) = KEYS(L3)(:LC)
         GO TO 99
      END IF
!
!   Keyword has no blanks - is it followed by a blank if it needs one?
    8 IF (.NOT.BLANK(L3)) GO TO 99
      NEXT = 0
      DO L8 = 1, LK(L3)
   17    NEXT = NEXT+1
         IF (STAMNT(NEXT:NEXT)==' ') GO TO 17
      END DO
      NEXT = NEXT+1
      IF (STAMNT(NEXT:NEXT)==' ') GO TO 99
!
!   Sometimes a delimiter may be present
      IF (L3==2.OR.L3==16.OR.L3==23.OR.L3==35.OR.L3==38) THEN
         IF (STAMNT(NEXT:NEXT)=='(') GO TO 99
      ELSE IF (L3==5) THEN
         IF (STAMNT(NEXT:NEXT)=='*') GO TO 99
      ELSE IF (L3==7.OR.L3==39) THEN
         IF (STAMNT(NEXT:NEXT)=='/') GO TO 99
      END IF
      IF (LENST==LEN) THEN
         OVFLW = .TRUE.
         GO TO 99
      END IF
!
!   Insert the blank
      BUFFER(NEXT:LENST) = STAMNT(NEXT:LENST)
      LENST = LENST+1
      STAMNT(NEXT:NEXT) = ' '
      STAMNT(NEXT+1:LENST) = BUFFER(NEXT:LENST-1)
      BLNKFL = .TRUE.
      GO TO  99
!
!   Check whether, in fact, a DO-loop
    9 IF (THREE(:2) /= 'DO' .AND. THREE(:2) /= 'do') GO TO 12
      LPAREN = 0
      NAPOS = 0
      DO L5 = L1+2, LENST
         IF (STAMNT(L5:L5)==' ') CYCLE
         IF (STAMNT(L5:L5) == "'") NAPOS = 1 - NAPOS
         IF (NAPOS == 1) CYCLE
         IF (STAMNT(L5:L5)==',') THEN
            IF (LPAREN==0) THEN
               ASSIGN = .FALSE.
               GO TO 10
            END IF
         ELSE IF (STAMNT(L5:L5)=='(') THEN
            LPAREN = LPAREN+1
         ELSE IF (STAMNT(L5:L5)==')') THEN
            LPAREN = LPAREN-1
         END IF
      END DO
      GO TO  99
!
!   Check whether, in fact, a logical IF followed by an assignment
   12 IF (THREE /= 'IF(' .AND. THREE /= 'if(') GO TO 99
      IFASS = .TRUE.
      DO L6 = L1-1, 1, -1
         IF (STAMNT(L6:L6)==' ') CYCLE
         IF (STAMNT(L6:L6)==')') THEN
!
!   Is there a second pair of first-level parentheses
            IF (index(STAMNT(:L6), ')')==0) GO TO 99
            LPAREN = 1
            DO L7 = L6-1, 4, -1
               IF (STAMNT(L7:L7)==' ') CYCLE
               IF (STAMNT(L7:L7)==')') THEN
                  IF (LPAREN==0) THEN
                     GO TO 14
                  ELSE
                     LPAREN = LPAREN+1
                  END IF
               ELSE IF (STAMNT(L7:L7)=='(') THEN
                  LPAREN = LPAREN-1
               END IF
            END DO
            GO TO 99
   14       ASSIGN = .FALSE.
            GO TO 10
         ELSE
            ASSIGN = .FALSE.
            GO TO 10
         END IF
      END DO
!
!   Test for non-executable statement keyword
   99 IF (ASSIGN) GO TO 98
!
      CALL TYPE_DECLARATIONS(STAMNT,L3,LENST,LK(L3),LOWERCASE)

      LPROGRAM_ENTITY = L3 == 3 .OR. L3 == 22 .OR. L3 == 34 .OR. L3 == 41
      IF (INTFL) THEN
!       Check for program entities
        SKIP = L3 ==  3 .OR. L3 ==  5 .OR. L3 ==  8 .OR.                 &
               L3 == 11 .OR. L3 == 12 .OR. L3 == 19 .OR. L3 == 22 .OR.   &
               L3 == 25 .OR. L3 == 27 .OR. L3 == 29 .OR.                 &
               L3 == 34 .OR. L3 == 36 .OR. L3 == 41
        SKIP = .NOT.SKIP
        IF (SKIP) THEN
          IF(.NOT.ASSIGN .AND. LPROGRAM_ENTITY) &
            WRITE (*, '('' Starting '', A)') TRIM(NAME)
          RETURN
        ENDIF
!
!       Check whether this statement refers to an argument or a 
!       function name
        ENTITY: IF (.NOT. LPROGRAM_ENTITY) THEN
          FUNC: IF(INDEX(STAMNT(LK(L3)+1:LENST), 'FUNCTION') /= 0 .OR.     &
             INDEX(STAMNT(LK(L3)+1:LENST), 'function') /= 0) THEN
            LPROGRAM_ENTITY = .TRUE.
          ELSE
            LARGUMENT = .FALSE.
            DO L20 = 1, NOARG
              IF(INDEX(STAMNT(LK(L3)+1:LENST), ARGNAM(L20)(:LENARG(L20)))  &
                 /=  0) THEN
                LARGUMENT = .TRUE.
                EXIT
              ENDIF
            END DO
            IF(.NOT. LARGUMENT) THEN
              SKIP = .TRUE.
              IF (SKIP) THEN
                IF(.NOT.ASSIGN .AND. LPROGRAM_ENTITY) &
                  WRITE (*, '('' Starting '', A)') TRIM(NAME)
                RETURN
              ENDIF
            ENDIF
          ENDIF FUNC
        ENDIF ENTITY
      ENDIF
!
!     Keep procedure name for END statement
      CALL NAME_OF(NAMEOF, STAMNT(LK(L3)+2:LENST), NAME_LENGTH)
      IF(LPROGRAM_ENTITY) THEN
        IF(LOWERCASE) THEN
          NAME = KEYSLC(L3)(:LK(L3))//NAMEOF(:NAME_LENGTH)
        ELSE
          NAME = KEYS(L3)(:LK(L3))//NAMEOF(:NAME_LENGTH)
        ENDIF
      ENDIF
!
!   Get argument names for later use in skipping unnecessary
!   specifications
   21 IF (INTFL) THEN
         IF (L3 == 22) THEN
            ARGNAM(1) = NAME(10:15)
            NOARG = 1
         END IF
         IF (L3 == 22 .OR. L3 == 41)                                   &
         CALL ARGUMENT(ARGNAM, LENARG, STAMNT, LENST, NOARG)
      END IF
!
!   Deal with awkward cases
      LSAVE = L3
      IF(L3 == 1.OR.L3 == 5.OR.L3 == 8.OR.L3 == 12 .OR. L3 == 13       &
      .OR. L3 == 25                                                    &
      .OR.L3 == 24.AND..NOT.IFASS.OR.L3 == 27.OR.L3 == 29.OR.L3 == 36) &
        CALL SPECIAL(L3, NEXT, BUFFER, NKEY, KEYS, KEYSLC, LK, FOLLOW, &
                     NAMEOF,LOWERCASE)
!
!   Was, in fact, a function
      IF (INTFL.AND.L3 == 22.AND.LSAVE /= 22) THEN
         SKIP = .FALSE.
         GO TO 21
      END IF
!
!   Print procedure name
98    IF(.NOT.ASSIGN .AND. (L3 == 3.OR.L3 == 22.OR.L3 == 34.OR.L3 == 41)) &
        WRITE (*, '('' Starting '', A)') TRIM(NAME)
   END SUBROUTINE KEYWORD
   SUBROUTINE NAME_OF(NAMEOF, HEADER, NAME_LENGTH)
!
   IMPLICIT NONE
!   PICK OUT NAME OF PROCEDURE
      CHARACTER(LEN=*), INTENT(IN) :: HEADER
      CHARACTER(LEN=*), INTENT(OUT):: NAMEOF
      INTEGER, INTENT(OUT)         :: NAME_LENGTH
      INTEGER :: IND, INDAST
!
      NAMEOF = ' '
      NAME_LENGTH = 1
!
!   Is there a left parenthesis or an asterisk?
      IND = INDEX(HEADER, '(' )
      INDAST = INDEX(HEADER, '*')
      IF (IND /= 0 .AND. INDAST /= 0) IND = MIN(IND, INDAST)
      IF (IND <= LEN(NAMEOF)) THEN
         IF (IND == 0) THEN
            NAMEOF(2:) = HEADER(:LEN_TRIM(HEADER))
            NAME_LENGTH = MIN(LEN_TRIM(HEADER)+1, LEN_TRIM(NAMEOF))
         ELSE
            NAMEOF(2:IND) = HEADER(:IND-1)
            NAME_LENGTH = IND
         END IF
      END IF
   END SUBROUTINE NAME_OF
   SUBROUTINE PROGRAM_UNITS( )
!
!***********************************************************************
!   The principal subroutine of CONVERT processes the                  *
!   input stream, which is assumed to contain syntactically correct    *
!   Fortran program units. To protect itself from bad data, failure    *
!   to pass a primitive syntax check will cause the program to copy    *
!   the input stream to the output unit unchanged, until an END line is*
!   encountered.                                                       *
!***********************************************************************
!
      USE DATA
!
      USE STATISTICS
!
      USE STRUCTURE
   implicit none
!
!***********************************************************************
!   USER is a character which may be defined to identify lines         *
!   in the input stream which are to be treated as                     *
!   comment lines ( + in this example).                                *
!***********************************************************************
!
      CHARACTER(LEN=1) :: CONTIN
      CHARACTER(LEN=3), PARAMETER :: FIN='END', FINLC='end'
      CHARACTER(LEN=5) :: CLABEL
      CHARACTER(LEN=66) :: FIELD
      CHARACTER(LEN=72) :: LINE
      CHARACTER(LEN=72), PARAMETER :: BLANKV=' '
!
      LOGICAL :: ASSIGN, ELSEBL, FORM, LCOMMENT, LOWERCASE, NEWDO , NEWIF
!
      CHARACTER(LEN=1), PARAMETER :: USER = '+'
      LOGICAL :: STAT = .FALSE. , SKIP = .FALSE.
!
      integer :: irtcod, l1, l5, l9, l22, lab, length, k1, kntcon, napo, nend
!
!   Start processing program units
      MXDO = 0
      MXIF = 0
      KARD = 0
      KNTPU = 0
      SYNTAX = .FALSE.
      SYNERR = .FALSE.
      OVFLW = .FALSE.
      NONSTD = .FALSE.
      KNTDO = 0
      KNTIF = 0
      KNTCOM = 0
      NAME = ' '
      NOARG = 0
      INTFL = INTBFL ! interface blocks only flag
!
!   Set continuation line counter
    1 KNTCON = 0
!
!   Set statement length counter
      LENST = 0
!
!   Read one line into an internal file,
!   columns 73-80 of all lines are ignored.
    2 CONTINUE
      LENGTH = 0
      BLANK_LINES: DO WHILE(LENGTH == 0)
        READ (NIN , '(A)' , END = 100 , ERR = 100) LINE
        IF(LINE(1:1) == ACHAR(9)) THEN
!         Tab in first column, replace with 8 blanks
          LINE = '        '//LINE(2:)
        ENDIF
        LENGTH = LEN_TRIM(LINE)
        IF(LENGTH == 0) THEN
!         Entire blank line, put null in first column
          IF(.NOT.INTFL) WRITE (NOUT , '(A)') ''
        ENDIF
        KARD = KARD+1
      ENDDO BLANK_LINES
!
!     Check whether a comment line and if so copy to buffer.
      LCOMMENT = .FALSE.
!     Does comment occur in first column?
      LCOMMENT = LINE(:1) == 'C' .OR. LINE(:1) == '*' .OR. &
                 LINE(:1) == USER .OR. LINE == ' ' .OR. &
                 LINE(:1) == 'c' .OR. LINE(:1) == '!'
!     Does "!" comment begin after leading blanks?
      CLABEL = ADJUSTL(LINE)
      LCOMMENT = LCOMMENT .OR. CLABEL(:1) == '!'
!
      IF (LCOMMENT) THEN
         IF (INTFL) GO TO 2
         IF (LINE(:1) == 'C' .OR. LINE(:1) == '*'                      &
         .OR. LINE(:1) == 'c') LINE(:1) = '!'
         IF (KNTCOM == KKLIM) THEN
            WRITE (NOUT , '(A)') (TRIM(CBUF(72*L5-71:72*L5)) ,         &
                                   L5 = 1,KNTCOM) , TRIM(LINE)
            KNTCOM = 0
         ELSE IF (SYNERR .OR. .NOT.STAT) THEN
            WRITE (NOUT , '(A)') TRIM(LINE)
         ELSE
            KNTCOM = KNTCOM+1
            CBUF(72*KNTCOM-71:72*KNTCOM) = LINE
         END IF
         GO TO 2
      END IF
!
!   Some form of embedded comment?
      NAPO = 0
      DO L22 = 2, 72
         IF (LINE(L22:L22)  ==  '''') NAPO = 1 - NAPO
         IF (L22 == 6) CYCLE
         IF (LINE(L22:L22)  /=  '!') CYCLE
         IF (NAPO  /=  0) CYCLE
         IF (.NOT. INTFL) THEN
            IF (KNTCOM  <  KKLIM) THEN
               KNTCOM = KNTCOM +1
               CBUF(72*KNTCOM-71:72*KNTCOM) =                          &
                                BLANKV(:L22-1)//LINE(L22:72)
            ELSE
               WRITE (NOUT, '(A)') TRIM(BLANKV(:L22-1)//LINE(L22:72))
            END IF
         END IF
         LINE(L22:72) = ' '
         EXIT
      END DO
!
!   Line is some form of statement; re-read.
      READ (LINE , '(BN , I5 , A1 , A66)') LAB , CONTIN , FIELD
      STAT = .TRUE.
!
!   Check on syntax and copy to statement buffer
    3 IF (CONTIN == '0') CONTIN = ' '
      IF (CONTIN /= ' ') THEN
         CONTIN = '&'
         IF (SYNERR) THEN
            GO TO 6
         ELSE IF (LENST == 0 .OR. LENST+66 > LEN .OR. LAB /= 0) THEN
            SYNERR = .TRUE.
            IF (LENST > 0) THEN
               IF (LABEL /= 0) THEN
                  WRITE(CLABEL,'(I5)') LABEL
                  WRITE (NOUT , '(A,1X,A,:,"&"/(5X, "&", A:"&"))') &
                        TRIM(CLABEL),                              &
                        (TRIM(STAMNT(66*L9-65:66*L9)), L9 = 1,(LENST+65)/66)
               ELSE
                  WRITE (NOUT , '(6X,A,:"&"/(5X, "&", A:"&"))')    &
                        (TRIM(STAMNT(66*L9-65:66*L9)), L9 = 1,(LENST+65)/66)
               END IF
            END IF
            IF (LAB /= 0) THEN
               WRITE (NOUT , 1000) LAB , CONTIN , TRIM(FIELD)
            ELSE
               WRITE (NOUT , 1006) CONTIN , TRIM(FIELD)
            END IF
            GO TO 1
         ELSE
            KNTCON = KNTCON+1
            STAMNT(LENST+1:LENST+66) = FIELD
            LENST = LENST+66
            GO TO 2
         END IF
      ELSE IF (KNTCON == 0) THEN
         IF (LENST /= 0) GO TO 4
         STAMNT(1:66) = FIELD
         LENST = 66
         LABEL = LAB
         IF (SYNERR) GO TO 4
         GO TO 2
      END IF
      IF (KNTCON > 0) GO TO 6
!
!   Have a complete statement ready for processing (the last line
!   read is still waiting in LINE). The statement now needs to be
!   identified.
!   The END statement is a special case - if found it will be copied
!   and the next program unit processed.
    4 K1 = 1
      DO  L1 = 1 , LENST
         IF (STAMNT(L1:L1) == ' ') CYCLE
         IF (STAMNT(L1:L1) /= FIN(K1:K1) .AND.                         &
             STAMNT(L1:L1) /= FINLC(K1:K1)) THEN
            EXIT
         ELSE
            K1 = K1+1
            IF (K1 > 3 .AND. (L1 >= LENST .OR. STAMNT(L1+1:LENST)      &
            == ' ')) THEN
               IF (.NOT.SYNERR) THEN
                  KNTPU=KNTPU+1
                  IF (LABEL == 0) THEN
                     IF(LOWERCASE) THEN
                       WRITE (NOUT , 1001) FINLC, TRIM(NAME)
                     ELSE
                       WRITE (NOUT , 1001) FIN, TRIM(NAME)
                     ENDIF
                  ELSE
                     WRITE(CLABEL,'(I5)') LABEL
                     IF(LOWERCASE) THEN
                       WRITE (NOUT , 1002) TRIM(CLABEL), FINLC, TRIM(NAME)
                     ELSE
                       WRITE (NOUT , 1002) TRIM(CLABEL), FIN, TRIM(NAME)
                     ENDIF
                  END IF
               END IF
!
!   Set counters for new program unit
               SYNTAX = SYNTAX .OR. SYNERR
               KNTDO = 0
               KNTIF = 0
               SYNERR = .FALSE.
               KNTCON = 0
               LENST = 0
               IF (KNTCOM /= 0) THEN
                 DO L5 = 1 , KNTCOM
                   WRITE (NOUT , '(A)') TRIM( CBUF(72*L5-71:72*L5) )
                 ENDDO
               ENDIF
               KNTCOM = 0
               NAME = ' '
               NOARG = 0
               GO TO 3
            ELSE
               IF (K1 > 3) EXIT
            END IF
         END IF
      END DO
!
!   If syntax error flag set, copy and take next statement
    6 IF (SYNERR) THEN
         IF (LAB /= 0) THEN
            WRITE (CLABEL,'(I5)') LAB
            WRITE (NOUT , 1000) TRIM(CLABEL), CONTIN , TRIM(FIELD)
         ELSE
            WRITE (NOUT , 1006) CONTIN , TRIM(FIELD)
         END IF
         LENST = 0
         GO TO 2
      END IF
!
!   Compress blanks and insert blanks around special characters
      CALL BLANK( )
!
!   Handle Fortran keywords
      NEWDO = .FALSE.
      NEWIF = .FALSE.
      FORM  = .FALSE.
      ELSEBL = .FALSE.
      ASSIGN = .FALSE.
      IF (BLANKS) CALL KEYWORD(ASSIGN, SKIP,LOWERCASE)
      IF (SKIP) GO TO 16
      IF (SYNERR) GO TO 6
      IF (BLANKS .AND. ASSIGN .AND. LABEL == 0) GO TO 14
!
!   Have a valid statement which is not an END line or assignment
!   Identify statement as    DO
!                            IF ( ) THEN
!                            DO terminator
!                            END IF
!                            FORMAT
!                            ELSE or ELSEIF
!                            none of these.
      CALL IDENTIFY(IRTCOD)
      SELECT CASE (IRTCOD)
         CASE (0)
            GO TO  14
!
!   New DO-loop
         CASE (1)
            IF (KNTDO == NEST) GO TO 14
            NEWDO = .TRUE.
            LABLDO(KNTDO+1) = LABLNO
!
!   End of DO-loop(s)
         CASE (2)
            NEND = 0
            DO  L5 = KNTDO , 1 , -1
               IF (LABLDO(L5) /= LABEL) EXIT
               NEND = NEND + 1
            END DO
!
!   Replace CONTINUE by END DO
      KNTDO = KNTDO - NEND
      IF (NEND == 1 .AND. LENST == 10 .AND. &
          (STAMNT(:LENST) == 'CONTINUE  ' .OR. &
           STAMNT(:LENST) == 'continue  ') )THEN
         IF (LOWERCASE) THEN
            STAMNT(:8) = 'end do  '
         ELSE
            STAMNT(:8) = 'END DO  '
         ENDIF
         LENST = 6
      END IF
!
!   Beginning of IF-block
         CASE (3)
            NEWIF = .TRUE.
!
!   End of IF-block
         CASE (4)
            KNTIF = KNTIF-1
            IF (KNTIF < 0) THEN
               SYNERR = .TRUE.
               KNTIF = 0
            END IF
!
!   FORMAT statement
         CASE (5)
            FORM =.TRUE.
!
!   Beginning of ELSE-block
         CASE (6)
            IF (KNTIF  >  0) THEN
               ELSEBL = .TRUE.
            ELSE
              SYNERR = .TRUE.
            END IF
      END SELECT
!
!   Reformat statements and write
   14 CALL REFORM (FORM , ELSEBL)
!
!   Set variables for next statement
      IF (NEWDO) KNTDO = KNTDO+1
      IF (NEWIF) KNTIF = KNTIF+1
      MXDO = MAX(MXDO , KNTDO)
      MXIF = MAX(MXIF , KNTIF)
   16 KNTCON = 0
      LENST = 0
      GO TO   3
!
!   End of data. Last line must be an END.
  100 IF (LABEL == 0) THEN
        IF(LOWERCASE) THEN
          WRITE (NOUT , 1001) FINLC, TRIM(NAME)
        ELSE
          WRITE (NOUT , 1001) FIN, TRIM(NAME)
        ENDIF
      ELSE
        WRITE(CLABEL,'(I5)') LABEL
        IF(LOWERCASE) THEN
          WRITE (NOUT , 1002) TRIM(CLABEL), FINLC, TRIM(NAME)
        ELSE
          WRITE (NOUT , 1002) TRIM(CLABEL), FIN, TRIM(NAME)
        ENDIF
      ENDIF
      KNTPU=KNTPU+1
      IF (INTFL) THEN
        IF(LOWERCASE) THEN
          WRITE (NOUT, '(''END INTERFACE'',/,''END MODULE '',A)') &
                TRIM(NAMEORIG)
        ELSE
          WRITE (NOUT, '(''end interface'',/,''end module '',A)') &
                TRIM(NAMEORIG)
        ENDIF
      ENDIF
!
!   Note: if there is a syntax error, continued
!         statements do not have a trailing &
 1000 FORMAT(A, A1 , A)
 1001 FORMAT(A3 ,TR1, A)
 1002 FORMAT(A, TR1 , A3 ,TR1, A)
 1006 FORMAT(A1 , A)
!
   RETURN
   END SUBROUTINE PROGRAM_UNITS
   SUBROUTINE REFORM (FORM , ELSEBL)
!
!   Performs reformatting and output of accepted statements
!
      USE DATA
!
      USE STRUCTURE
   implicit none
!
      INTEGER, PARAMETER :: LLIMIT = LEN-(LEN/66-1)*6
      CHARACTER(LEN=LEN) :: OUT
      CHARACTER(LEN=1)   :: AMP      
      CHARACTER(LEN=5)   :: CLABEL
!
      LOGICAL, INTENT(IN) :: FORM , ELSEBL
!
      integer :: ind, ipnt, l6, l2, l3, l4, lout, idepth, kntap, kadd, &
                 l5, jpnt
!
!   If FORMAT statement, do not indent
      IF (FORM) GO TO 9
!
!   Remove the blanks before commas if no character string
      IF (BLNKFL .AND. INDEX(STAMNT(:LENST), "'") == 0) THEN
         IPNT = 1
         DO
            IND = INDEX(STAMNT(IPNT:LENST), ' , ')
            IF (IND == 0) EXIT
            IND = IPNT + IND - 1
            STAMNT(IND:IND+2) = ',  '
            IPNT = IND + 3
         END DO
      END IF
!
!   Reformat indented statement and write. If reformatting causes it
!   to exceed LEN characters, it will be copied unchanged.
      IDEPTH = MIN(KNTDO+KNTIF , MXDPTH)
      IF (IDEPTH == 0 .AND. .NOT.BLNKFL) GO TO  9
      IF (ELSEBL) IDEPTH = IDEPTH-1
      IPNT = 1
      JPNT = 1
    1 IF (MOD(IPNT , 66) == 1) THEN
         IF (IPNT+65 > LEN) GO TO 9
         OUT(IPNT:IPNT+65) = ' '
         IPNT = IPNT+IDEPTH*ISHIFT
      END IF
!
!   Find first non-blank character
      DO  L2 = JPNT , LENST
         IF (STAMNT(L2:L2) /= ' ') GO TO 3
      END DO
      IF (JPNT == 1) THEN
         SYNERR = .TRUE.
         GO TO 9
      ELSE
         GO TO 10
      END IF
!
!   Find first multiple blank (but not in a character string)
    3 KNTAP = 0
      DO  L3 = L2, LENST-1
         IF (STAMNT(L3:L3) == "'") KNTAP = 1-KNTAP
         IF (STAMNT(L3:L3+1) == '  ') THEN
            IF (KNTAP == 0) GO TO 5
            GO TO 9
         END IF
      END DO
      L3 = LENST
!
!   Have section with no multiple blanks. This can be copied to OUT
!   if there is room on the current line. Otherwise cut the
!   section after the non-alphanumeric character nearest to the end of
!   the line, if one exists.
!   An apostrophe and period are considered to be alphanumeric
!   characters, in order to hold character strings,
!   and real and logical constants together;
!   underscores and dollars are so treated to handle common extensions,
!   and the ** and // operators and real literal constants are treated.
    5 KADD = 0
      IF (L3-L2  <=  66-MOD(IPNT , 66)) GO TO  8
      DO L4 = 66+L2-MOD(IPNT , 66) , L2 , -1
         IF (STAMNT(L4:L4) == ' ') GO TO 7
         IF (LGE(STAMNT(L4:L4) , 'A') .AND. LLE(STAMNT(L4:L4) , 'Z'))  &
         CYCLE
         IF(LGE(STAMNT(L4:L4), '0') .AND.                              &
                  LLE(STAMNT(L4:L4), '9')) CYCLE
         IF (STAMNT(L4:L4)  ==  "'" .OR.                               &
         STAMNT(L4:L4)  ==  '_' .OR. STAMNT(L4:L4)  ==  '$' .OR.       &
         STAMNT(L4:L4)  ==  '.') CYCLE
         IF (L4 /= LENST) THEN
            IF (STAMNT(L4:L4+1)  ==  '**' .OR.                         &
                STAMNT(L4:L4+1)  ==  '//' ) CYCLE
            IF (L4 /= L2) THEN
               IF(LGE(STAMNT(L4+1:L4+1), '0') .AND.                    &
                  LLE(STAMNT(L4+1:L4+1), '9')) THEN
                  IF (STAMNT(L4-1:L4)  ==  'E+' .OR.                   &
                      STAMNT(L4-1:L4)  ==  'e+' .OR.                   &
                      STAMNT(L4-1:L4)  ==  'E-' .OR.                   &
                      STAMNT(L4-1:L4)  ==  'e-' .OR.                   &
                      STAMNT(L4-1:L4)  ==  'D+' .OR.                   &
                      STAMNT(L4-1:L4)  ==  'd+' .OR.                   &
                      STAMNT(L4-1:L4)  ==  'D-' .OR.                   &
                      STAMNT(L4-1:L4)  ==  'd-' ) CYCLE
               END IF
            END IF
         END IF
         IF (LGE(STAMNT(L4:L4) , 'a') .AND. LLE(STAMNT(L4:L4) , 'z'))  &
         CYCLE
         GO TO 7
      END DO
!
!   No break character found
      IF (BLNKFL) GO TO 9
      L4 = 66-MOD(IPNT , 66)+L2
!
!   Cut here
    7 L3 = L4
      KADD = 1
    8 LOUT = IPNT+L3-L2
      IF (LOUT > LEN) GO TO  9
      OUT(IPNT:LOUT) = STAMNT(L2:L3)
      IF (L3 == LENST) GO TO 10
!
!   Set pointers for next section of statement
      IPNT = LOUT+1
      IF (KADD == 1 .AND. MOD(IPNT , 66) /= 1 .OR. MOD(IPNT , 66)      &
       >= 60) IPNT = ((IPNT+65)/66)*66+1
      IF (MOD(IPNT , 66) == 0) IPNT = IPNT+1
      JPNT = L3+1
      IF (KADD == 0) JPNT = JPNT+1
      GO TO   1
!
!   Copied statement (if adding 6 cols. to initial line would cause
!   total length to exceed 2640, must start it in col.1)
    9 LENST = LEN_TRIM(STAMNT(:LENST))
      IF (LENST > 66) THEN
         AMP = '&'
      ELSE
         AMP = ' '
      END IF
      IF (LABEL /= 0) THEN
         WRITE(CLABEL,'(I5)') LABEL
         WRITE (NOUT , 1003) TRIM(CLABEL), TRIM(STAMNT(:MIN(LENST, 66))), AMP
      ELSE
         IF (LENST < LEN-6) THEN
            WRITE (NOUT , 1004) TRIM(STAMNT(:MIN(LENST,66))), AMP
         ELSE
            WRITE (NOUT , '(A,A1)') TRIM(STAMNT(:MIN(LENST, 66))), AMP
         END IF
      END IF
      IF (LENST > 66) WRITE (NOUT , 1005) &
         ('&', TRIM(STAMNT(66*L6-65:66*L6)) , L6 = 2,(LENST+65)/66)
      GO TO  11
!
!   Write OUT to output unit
   10 LOUT = LEN_TRIM(OUT(:LOUT))
      IF (LOUT > 66) THEN
         AMP = '&'
      ELSE
         AMP =' '
      END IF
      IF (LABEL /= 0) THEN
         WRITE(CLABEL,'(I5)') LABEL
         WRITE (NOUT , 1003) TRIM(CLABEL), TRIM(OUT(:MIN(LOUT, 66))), AMP
      ELSE
         WRITE (NOUT , 1004) TRIM(OUT(:MIN(LOUT, 66))), AMP
      END IF
!
!   An & is required in col. 6 if statement has more than 2412
!   characters, otherwise total length including cols. 1-6 will
!   exceed 2640. Also if making interface blocks, in order to be
!   compatible with both old and new source forms.
      IF (LOUT > 66) THEN
         IF (LOUT > LLIMIT .OR. INTFL) THEN
            AMP = '&'
         ELSE
            AMP = ' '
         END IF
         WRITE (NOUT , 1005) (AMP , TRIM(OUT(66*L5-65:66*L5)), &
                             L5 = 2,(LOUT+65)/66)
      END IF
!
!   Write any comments following statement
   11 IF (KNTCOM /= 0) THEN
         DO L5 = 1 , KNTCOM
           WRITE (NOUT ,'(A)') TRIM( CBUF(72*L5-71:72*L5) )
         ENDDO
         KNTCOM = 0
      END IF
!
 1003 FORMAT(A, TR1, A, TR1, A)
 1004 FORMAT(A, TR1, A)
 1005 FORMAT(A, A, :, TR1,'&' )
!
   RETURN
   END SUBROUTINE REFORM
   SUBROUTINE SPECIAL(L3, NEXT, BUFFER, NKEY, KEYS, KEYSLC, LK, FOLLOW, &
                      NAMEOF,LOWERCASE)
!
!   Special treatment for peculiar Fortran syntax
!
      USE STRUCTURE
!
      USE STATISTICS
   implicit none
      CHARACTER (LEN=*), INTENT(OUT) :: BUFFER
      CHARACTER (LEN=*), INTENT(IN), DIMENSION(:) :: KEYS, KEYSLC
      INTEGER, INTENT(IN OUT) :: L3, NEXT
      INTEGER, INTENT(IN) :: NKEY
      INTEGER, INTENT(IN), DIMENSION(:) :: LK
      LOGICAL :: LOWERCASE
      LOGICAL, INTENT(IN), DIMENSION(:) :: FOLLOW
!
      CHARACTER (LEN=*), INTENT(INOUT)  :: NAMEOF
      INTEGER, PARAMETER :: NUMLEN = 5
      CHARACTER(LEN=NUMLEN) :: NUMBER

!
      LOGICAL :: IFASSIGN
      INTEGER :: ILP, IND, IPOS, ISSS, ISTAR, L10, L20,  LIMIT, LPAREN, &
                 NAME_LENGTH, NAPOS, NDIGIT, NPAREN
!
      IFASSIGN = .FALSE.
!
!  Deal with labelled DO WHILE
      IF (L3 == 13) THEN
         IND = index(STAMNT(:LENST), 'WHILE')
         IF (IND == 0) IND = index(STAMNT(:LENST), 'while')
         IF (IND /= 0) THEN
            IF(LGE(STAMNT(IND-1:IND-1), '0') .AND.                     &
               LLE(STAMNT(IND-1:IND-1), '9'))                          &
               STAMNT(IND:IND+5) = ' WHILE'
         END IF
         RETURN
      END IF
!
!   Deal with IMPLICIT with non-standard length specifiers
      IF (L3  ==  25) THEN
         IF (index(STAMNT(:LENST), '*')  /=  0) THEN
!
!   first, CHARACTER*(len)
   11       IND = index(STAMNT(:LENST), 'CHARACTER *  (')
            IF (IND  ==  0) IND = index(STAMNT(:LENST),'character *  (')
            IF (IND /=  0) THEN
               STAMNT(IND+10:IND+10) = ' '
               GO TO 11
            END IF
!
!   then, type*nn
            NPAREN = 0
            NAPOS = 0
            DO L10 = 15, LENST
               IF (STAMNT(L10:L10)  ==  "'") THEN
                  NAPOS = 1 - NAPOS
               ELSE IF (STAMNT(L10:L10)  ==  '(') THEN
                  IF (NAPOS  ==  0) NPAREN = NPAREN + 1
               ELSE IF (STAMNT(L10:L10)  ==  ')') THEN
                  IF (NAPOS  ==  0) NPAREN = NPAREN - 1
               ELSE IF (STAMNT(L10:L10)  ==  '*') THEN
                  IF (NPAREN  ==  0) THEN
                     STAMNT(L10:L10+1) = ' ('
                     ILP = index(STAMNT(L10+2:LENST), '(')
                     IF (ILP  ==  0) THEN
                        SYNERR = .TRUE.
                        RETURN
                     ELSE
                        STAMNT(L10+ILP:L10+ILP) = ')'
                     END IF
                     IF (STAMNT(L10+1:L10+3)  ==  '(4)') THEN
                        IF (STAMNT(L10-5:L10-5)  /=  'C' .AND.         &
                            STAMNT(L10-5:L10-5)  /=  'c')              &
                            STAMNT(L10+1:L10+3) = '   '
                     ELSE IF (STAMNT(L10-2:L10+3)  ==  'X  (8)' .OR.   &
                             STAMNT(L10-2:L10+3)  ==  'x  (8)')THEN
                        STAMNT(L10+1:L10+3) = '   '
                     END IF

                  END IF
               END IF
            END DO
         END IF
         RETURN
      END IF
!
!   An ASSIGN label must be followed by a blank and a * specifier
!   converted to (...)
      IF(L3 == 1 .AND. STAMNT(:7 )  ==  'ASSIGN '     .OR.             &
         L3 == 1 .AND. STAMNT(:7 )  ==  'assign '     .OR.             &
         L3 == 5 .AND. STAMNT(:11)  ==  'CHARACTER *' .OR.             &
         L3 == 5 .AND. STAMNT(:11)  ==  'character *' .OR.             &
         L3 == 8 .AND. STAMNT(:9 )  ==  'COMPLEX *'   .OR.             &
         L3 == 8 .AND. STAMNT(:9 )  ==  'complex *'   .OR.             &
         L3 == 27.AND. STAMNT(:9 )  ==  'INTEGER *'   .OR.             &
         L3 == 27.AND. STAMNT(:9 )  ==  'integer *'   .OR.             &
         L3 == 29.AND. STAMNT(:9 )  ==  'LOGICAL *'   .OR.             &
         L3 == 29.AND. STAMNT(:9 )  ==  'logical *'   .OR.             &
         L3 == 36.AND. STAMNT(:6 )  ==  'REAL *'      .OR.             &
         L3 == 36.AND. STAMNT(:6 )  ==  'real *'          ) THEN
         IF (L3 < 8) THEN
            ISSS = L3+7
         ELSE IF (L3 < 30) THEN
            ISSS = 10
         ELSE
            ISSS = 7
         END IF
!
!   Extract the length parameter
         NDIGIT = 1
         NUMBER = '  '
         DO L20 = ISSS, LENST
            IF(STAMNT(L20:L20)  ==  ' ') CYCLE
            NUMBER(:1) = STAMNT(L20:L20)
            IF(LGE(STAMNT(L20:L20),'0') .AND. LLE(STAMNT(L20:L20),'9') ) &
              GO TO 21
            GO TO 1
         END DO
         SYNERR = .TRUE.
         RETURN
   21    DO IPOS = L20+1, LENST
            IF(STAMNT(IPOS:IPOS)  ==  ' ') CYCLE
            IF(LLT(STAMNT(IPOS:IPOS), '0') .OR. &
               LGT(STAMNT(IPOS:IPOS), '9')) GO TO 19
            NDIGIT = NDIGIT + 1
            IF (NDIGIT  >  NUMLEN) THEN
               SYNERR = .TRUE.
               RETURN
            END IF
            NUMBER(NDIGIT:NDIGIT) = STAMNT(IPOS:IPOS)
         END DO
         SYNERR = .TRUE.
         RETURN
      END IF
      GO TO 1
!
!   Insert the blank or parentheses
   19 NEXT = IPOS
      IF (LENST >= LEN-1) THEN
         OVFLW = .TRUE.
         RETURN
      END IF
      BUFFER(NEXT:LENST) = STAMNT(NEXT:LENST)
      IF (L3 == 1) THEN
         LENST = LENST+2
         IF(LOWERCASE) THEN
           STAMNT(NEXT:NEXT+3) = ' to '
         ELSE
           STAMNT(NEXT:NEXT+3) = ' TO '
         ENDIF
         STAMNT(NEXT+4:LENST) = BUFFER(NEXT+2:LENST-2)
      ELSE
         LENST = LENST+1
         STAMNT(NEXT:NEXT) = ' '
         STAMNT(NEXT+1:LENST) = BUFFER(NEXT:LENST-1)
         IF (L3 /= 5.AND. NDIGIT  ==  1 .AND. NUMBER(:1)  ==  '4') THEN
            STAMNT(NEXT-4:NEXT-1) = '    '
         ELSE IF (L3 == 8.AND.NDIGIT  ==  1 .AND. NUMBER(:1)  ==  '8') &
         THEN
            STAMNT(NEXT-4:NEXT-1) = '    '
         ELSE
            STAMNT(NEXT-3-NDIGIT:NEXT-1) = '('//NUMBER(:NDIGIT)//')'
         END IF
      END IF
      GO TO 2
!
!   Handle (*) case
    1 IF(L3 == 5 .AND. STAMNT(:18)  ==  'CHARACTER *  ( * )') THEN
         NEXT = 19
         STAMNT(11:11) = ' '
      END IF
!
!   IF statement may be followed by a keyword
    2 IF (L3  ==  24 ) THEN
         LPAREN = 1
         NAPOS = 0
         DO IPOS = 5, LENST
            IF (STAMNT(IPOS:IPOS)  ==  "'") NAPOS = 1 - NAPOS
            IF (NAPOS  ==  1) CYCLE
            IF (STAMNT(IPOS:IPOS)  ==  '(' ) LPAREN = LPAREN+1
            IF (STAMNT(IPOS:IPOS)  ==  ')' ) LPAREN = LPAREN-1
            IF (LPAREN  ==  0) GO TO 5
         END DO
         RETURN
    5    NEXT = IPOS+1
         DO IPOS = 1, NKEY
            IF (FOLLOW(IPOS) .AND.(STAMNT(NEXT+1:NEXT+LK(IPOS)) == KEYS(IPOS)&
                          .OR. STAMNT(NEXT+1:NEXT+LK(IPOS)) == KEYSLC(IPOS)&
             )) THEN
               L3 = IPOS
               NEXT = NEXT+LK(L3)+1
               IF(L3 == 1) IFASSIGN = .TRUE.
               GO TO 9
            END IF
         END DO
      ELSE
!
! Typed function
         IF(STAMNT(NEXT+1:NEXT+8)  ==  'FUNCTION'  .OR.                &
            STAMNT(NEXT+1:NEXT+8)  ==  'function') THEN
            NEXT = NEXT+9
            CALL NAME_OF(NAMEOF, STAMNT(NEXT:LENST), NAME_LENGTH)
            IF(LOWERCASE) THEN
              NAME = 'function'//NAMEOF(:name_length)
            ELSE
              NAME = 'FUNCTION'//NAMEOF(:name_length)
            ENDIF
            L3 = 22
!
!   Deal with any *
            LIMIT = index(STAMNT(:LENST), '(')
            IF (LIMIT /= 0) THEN
               ISTAR = index(STAMNT(:LIMIT), '*')
               IF (ISTAR  /=  0) THEN
                  NDIGIT = LIMIT - ISTAR -3
                  IF (NDIGIT  >  NUMLEN) THEN
                     SYNERR = .TRUE.
                     RETURN
                  END IF
                  NUMBER(:NDIGIT) = STAMNT(ISTAR+2:LIMIT-2)
                  IF(LOWERCASE) THEN
                    STAMNT(NEXT-5+NDIGIT:LIMIT-2) =                      &
                                        'function'//NAME(10:ISTAR-NEXT+8)
                  ELSE
                    STAMNT(NEXT-5+NDIGIT:LIMIT-2) =                      &
                                        'FUNCTION'//NAME(10:ISTAR-NEXT+8)
                  ENDIF
                  STAMNT(NEXT-8:NEXT-6+NDIGIT)  =                      &
                                      '('//NUMBER(:NDIGIT)//') '
                  IF (NDIGIT  ==  1 .AND. NUMBER(:1)  ==  '4') THEN
                     STAMNT(NEXT-8:NEXT-5) = '    '
                  ELSE IF (NDIGIT  ==  1 .AND. NUMBER(:1)  ==  '8'.AND.&
                     (STAMNT(7:7) == 'X'.OR.STAMNT(7:7) == 'x')) THEN
                     STAMNT(NEXT-8:NEXT-5) = '    '
                  END IF
                  NEXT = NEXT + 3 + NDIGIT
               END IF
            ELSE
               SYNERR = .TRUE.
               RETURN
            END IF
            GO TO 9
         END IF
      END IF
      RETURN
!
!   Insert the blank
    9 IF (LENST >= LEN-2) THEN
         OVFLW = .TRUE.
         RETURN
      END IF
      BUFFER(NEXT:LENST) = STAMNT(NEXT:LENST)
      LENST = LENST+1
      STAMNT(NEXT:NEXT) = ' '
      STAMNT(NEXT+1:LENST) = BUFFER(NEXT:LENST-1)
!
!   ASSIGN may follow IF
      IF(.NOT.IFASSIGN) RETURN
      NEXT = index(STAMNT(:LENST), 'TO')
      IF (NEXT == 0) NEXT = index(STAMNT(:LENST), 'to')
      IF(NEXT == 0) THEN
         SYNERR = .TRUE.
         RETURN
      ELSE
         LENST = LENST+2
         STAMNT(NEXT:NEXT+3) = ' TO '
         STAMNT(NEXT+4:LENST) = BUFFER(NEXT+1:LENST-3)
      END IF
   END SUBROUTINE SPECIAL
   SUBROUTINE START
!
!   To prepare for PROGRAM_UNITS
!
      USE DATA
      IMPLICIT NONE
      CHARACTER (LEN=256) :: NAME
      INTEGER :: ISLASH,LENGTH
      LOGICAL :: LEXIST,LEXTENSION
!
!   Set default values
      NAME = ' '
      ISHIFT = 0
      MXDPTH = 0
      BLANKS = .TRUE.
      INTBFL = .FALSE.
!
!   Obtain command line arguments
      CALL INPUT(NAME)
!
!   If record present, check input values are reasonable
      ISHIFT = MIN(MAX(ISHIFT , 0) , 10)
      MXDPTH = MIN(MAX(MXDPTH , 0) , 36/MAX(ISHIFT,1))
      IF (INTBFL.AND..NOT.BLANKS) WRITE (*, '('' Interface block &
         &processing cancelled as blank processing not requested'')')
      INTBFL = BLANKS.AND.INTBFL

      IF(ISHIFT == 0) ISHIFT = 3
      IF(MXDPTH == 0) MXDPTH = 10

!     Check whether ".f" extension was given
      length=len_trim(name)
      lextension=.false.
      nameorig=name
      if(length > 2) then
        if(name(length-1:length) == '.f') then
          nameorig=name(1:length-2)
          lextension=.true.
        else if(length > 4) then
          if(name(length-3:length) == '.for') then
            nameorig=name(1:length-4)
            lextension=.true.
          else if(name(length-3:length) == '.ftn') then
            nameorig=name(1:length-4)
            lextension=.true.
          endif
        endif
      endif

      if(.not. lextension) then
          name=trim(name)//'.f'
      endif
!
!     Open files

      inquire(file=name,exist=lexist)
      if(.not. lexist) then
        print *,'Input file ',trim(name),' does not exist!'
        call terminate(1)
      else
        OPEN (UNIT=NIN, FILE=TRIM(NAME), ACTION='READ',access='SEQUENTIAL', &
          status='OLD')
      endif

      if(.not. intbfl) then
        name=trim(nameorig)//'.f90'
      else
        nameorig=trim(nameorig)//'_module'
        name=trim(nameorig)//'.f90'
      endif
      inquire(file=name,exist=lexist)
      if(lexist) then
        print *,'Output file ',trim(name),' already exists!'
        call terminate(1)
      else
        OPEN (UNIT=NOUT, FILE=TRIM(NAME), ACTION='WRITE',access='SEQUENTIAL', &
          status='NEW')
      endif
!     Now remove preceding path to name if it exists.
      islash = index(nameorig,'/',.true.)
      if(islash /= 0) nameorig = nameorig(islash+1:)

!   Print values to be used
      WRITE (*,'(" Loop bodies will be indented by",I3/                &
     &           " Maximum indenting level is     ",I3)')              &
              ISHIFT , MXDPTH
      IF (BLANKS) WRITE (*,                                            &
      '(" Significant blank processing requested.")')
      IF (INTBFL) WRITE (*,                                            &
      '('' Only interface blocks will be produced.'')')
      IF (INTBFL) WRITE (NOUT, '(''MODULE '',A,/,''INTERFACE'')') &
         TRIM(NAMEORIG)
      WRITE(*,'(" ")')
!
      CALL SYSTEM_CLOCK(TIME0)
   END SUBROUTINE START
   SUBROUTINE TERMINATE(ICONDITION)
!
!   To print the final summary
!
      USE STATISTICS
      USE DATA
   implicit none
!
      integer :: icondition
      integer :: itick, itime
!
      if(icondition == 1) call exit(1)
!
      CALL SYSTEM_CLOCK(ITIME, ITICK)
      IF (ITICK /= 0)                                                  &
      WRITE (*,'(/," Processing complete in ", F7.3, " seconds")')     &
            REAL(ITIME-TIME0)/REAL(ITICK)
      WRITE (*,'(" Maximum depth of DO-loop nesting ",I3/              &
     &           " Maximum depth of IF-block nesting",I3/              &
     &" No. of lines read  ",I17/" No. of program units read   ",I8/   &
     &           " Global syntax error flag",L12,/)')                  &
                MXDO , MXIF , KARD , KNTPU , SYNTAX
!
      IF (OVFLW) WRITE(*,  '(" At least one statement was too long to h&
     &ave a necessary blank added")')
      IF (NONSTD) WRITE (*,  '(" At least one statement began with a no&
     &n-standard keyword.")')
!
    RETURN
    END SUBROUTINE TERMINATE
    SUBROUTINE TYPE_DECLARATIONS(STAMNT,ITYPE,LENST,LTYPE,LOWERCASE)
!
!   To check whether it is a type declaration keyword and convert to the
!   new "::" syntax.
!
!   STAMNT - STATEMENT IN SOURCE CODE BEING PROCESSED
!   ITYPE  - NUMERIC TYPE IN DECLARATION STATEMENT, E.G. 5=CHARACTER, 
!            9=INTEGER, ETC.
!   LENST  - LENGTH OF SIGNIFICANT PORTION OF STAMNT
!   LTYPE  - LENGTH OF NUMERIC TYPE, E.G. 7 FOR INTEGER AND 4 FOR REAL.
!   LOWERCASE - FLAG FOR CASE TYPE OF STAMNT
!          = .TRUE. FOR LOWERCASE
!          = .FALSE. FOR UPPERCASE
!
!   Peter A. Rochford, Sverdrup Technology, Inc., July 1997
!
    IMPLICIT NONE
    CHARACTER (LEN=*) :: STAMNT
    INTEGER, INTENT(IN) :: ITYPE,LTYPE
    INTEGER, INTENT(INOUT) :: LENST
    LOGICAL, INTENT(IN) :: LOWERCASE
!
    CHARACTER (LEN=LEN(STAMNT)) :: CSTRING
    CHARACTER :: CHARTYPE_LENGTH*14
    INTEGER :: IASTERIX,IASCIIZERO,IBLANK,IDIGITS,IFIRSTDIGIT,IFUNCTION, &
               ILASTDIGIT,ILETTERLOW,ILETTERUP,ILOWR0,ITEST,IUPPR0, &
               IVARARRAY
    LOGICAL :: LNUMERIC
!
    iuppr0 = iachar('A') - 1
    ilowr0 = iachar('a') - 1
    iasciizero = iachar('0') - 1
    type_declare: select case (itype)
      case (5)
!       Character type
        call convcase(stamnt(1:lenst),cstring,lenst,.true.)
        ifunction = index(cstring,'FUNCTION')
        char_function_check: if(ifunction == 0) then

          iasterix = index(stamnt(1:lenst),' * ')
          iblank = index(stamnt(1:lenst),' ')
          if(iasterix==0 .or. iasterix > ltype+1) then
            cstring = stamnt(1:iblank)//':: '//stamnt(iblank+1:)
            stamnt = cstring
            lenst = lenst + 3
          else
!           Is character length arbitrary?
            itest = index(stamnt(1:lenst),'( * )')
            if(itest == 0) then
!             Find location of last integer digit declaring character length
              ifirstdigit = iasterix + 3
              do idigits=ifirstdigit,lenst
                 iletterlow = iachar(stamnt(idigits:idigits)) - ilowr0
                 iletterup  = iachar(stamnt(idigits:idigits)) - iuppr0
                 if(stamnt(idigits:idigits) == ' ' .or. &
                    stamnt(idigits:idigits) == '(') then
                    ifirstdigit = ifirstdigit + 1
                    cycle
                 endif
                 if(stamnt(idigits:idigits) == ')' .or. &
                    (iletterlow>=1 .and. iletterlow<=26) .or. &
                    (iletterup>=1 .and. iletterup<=26) ) then
                    exit
                 endif
              enddo
              if(stamnt(idigits:idigits) == ')') then
                ilastdigit = idigits
              else
                ilastdigit = idigits - 1
              endif
              idigits = idigits - 1
              if(lowercase) then
                chartype_length= ' (len='//stamnt(ifirstdigit:idigits)//') ::'
              else
                chartype_length= ' (LEN='//stamnt(ifirstdigit:idigits)//') ::'
              endif
              cstring = stamnt(1:iasterix-1)//trim(chartype_length)//' ' &
                       //stamnt(ilastdigit+1:)
              stamnt = cstring
              lenst = lenst + 9 + (idigits - ifirstdigit + 1)
            else
              if(lowercase) then
                chartype_length= ' (len=*) ::'
              else
                chartype_length= ' (LEN=*) ::'
              endif
              cstring = stamnt(1:iasterix-1)//trim(chartype_length)//' ' &
                       //stamnt(itest+5:)
              stamnt = cstring
              lenst = lenst + 11
            endif
          endif
        endif char_function_check
      case (8,27,29,36)
!       Type complex, integer, logical, and real
        call convcase(stamnt(1:lenst),cstring,lenst,.true.)
        ifunction = index(cstring,'FUNCTION')
        function_check: if(ifunction == 0) then
          iasterix = index(stamnt(1:lenst),' * ')
          ivararray = 0
          if(iasterix > 0 .and. iasterix-1/=ltype) then
!           Check if actually a variable array
            itest = iachar(stamnt(iasterix-1:iasterix-1))
            if(itest < iachar('0') .or. itest > iachar('9') ) ivararray = 1
          endif
          asterix_check: if(iasterix == 0 .or. ivararray == 1) then
            iblank = index(stamnt(1:lenst),' ')
            cstring = stamnt(1:iblank)//':: '//stamnt(iblank+1:lenst)
            stamnt = cstring
            lenst = lenst + 3
          else
!           Determine if numeric or variable length assignment
            lnumeric = .true.
            ifirstdigit = iasterix + 3
            do idigits=ifirstdigit,lenst
              if(stamnt(idigits:idigits) == ' ' .or. &
                 stamnt(idigits:idigits) == '(') then
                 ifirstdigit = ifirstdigit + 1
                 cycle
              endif
              itest = iachar(stamnt(idigits:idigits))
              if(itest>=iachar('0') .and. itest <= iachar('9') ) then
                 lnumeric = .true. ! Numeric length
                 exit
              endif
              iletterlow = iachar(stamnt(idigits:idigits)) - ilowr0
              iletterup  = iachar(stamnt(idigits:idigits)) - iuppr0
              if((iletterlow>=1 .and. iletterlow<=26) .or. &
                 (iletterup>=1 .and. iletterup<=26) ) then
                 lnumeric = .false. ! Variable specified length
                 exit
              endif
            enddo

            if(lnumeric) then
!             Find location of last integer digit declaring type length
              do idigits=ifirstdigit,lenst
                iletterlow = iachar(stamnt(idigits:idigits)) - ilowr0
                iletterup  = iachar(stamnt(idigits:idigits)) - iuppr0
                itest = iachar(stamnt(idigits:idigits))
                if(itest>=iachar('0') .and. itest <= iachar('9') ) then
                   cycle
                else
                   exit
                endif
              enddo
            else
!             Find location of last variable letter declaring type length
              do idigits=ifirstdigit,lenst
                iletterlow = iachar(stamnt(idigits:idigits)) - ilowr0
                iletterup  = iachar(stamnt(idigits:idigits)) - iuppr0
                if(stamnt(idigits:idigits) == ')' .or. &
                   stamnt(idigits:idigits) == ' ') then
                   exit
                endif
              enddo
            endif
            idigits = idigits - 1
            cstring = stamnt(1:idigits)//' :: '//stamnt(idigits+1:)
            stamnt = cstring
            lenst = lenst + 4
          endif asterix_check
        endif function_check
    end select type_declare

    END SUBROUTINE TYPE_DECLARATIONS
    SUBROUTINE USAGE
!
!     Report on usage for program and exit.

      WRITE(*,10) ' '
      WRITE(*,10) 'SYNOPSIS'
      WRITE(*,10) ' '
      WRITE(*,10) 'f2f90 filename ishift indent_level blank_flag interface_flag'
      WRITE(*,10) ' '
      WRITE(*,10) 'OPTIONS'
      WRITE(*,10) ' '
      WRITE(*,10) '  filename     - name of f77 file, with or without &
                  &extension'
      WRITE(*,10) '  ishift       - column indentation depth for bodies &
                  &of do-loops '
      WRITE(*,10) '                 and if-blocks (default 3)'
      WRITE(*,10) '  indent_level - maximum indentation level for bodies'
      WRITE(*,10) '  blank_flag   - flag for handling of significant blanks'
      WRITE(*,10) '               = t for true (default)'
      WRITE(*,10) '               = f for false'
      WRITE(*,10) '  interface_flag - whether interface blocks only are &
                  &required '
      WRITE(*,10) '               = t for true'
      WRITE(*,10) '               = f for false (default)'
      WRITE(*,10) ' '
      WRITE(*,10) 'COMMENTS'
      WRITE(*,10) ' '
      WRITE(*,10) 'Type name of file, shift, max. indent level, T or F &
                  &for blank treatment'
      WRITE(*,10) '  T or F for interface blocks only.'
      WRITE(*,10) 'For simple use type only the name of the file followed &
                  &by a slash (/) and RETURN.'
      WRITE(*,10) ' '
      WRITE(*,10) '@(#)RELEASE 1.6         1997.08.12' 
      WRITE(*,10) '@(#)AUTHOR P. Rochford of Sverdrup Technology, Inc.'
      WRITE(*,10) ' '
!
   10 FORMAT(5X,A)
!
      CALL EXIT(1)
    END SUBROUTINE USAGE
   END MODULE ALL_PROCEDURES
   PROGRAM F2F90
   USE ALL_PROCEDURES
   implicit none
!
!   Initialize
      CALL START( )
!
!   Process the lines of program units
      CALL PROGRAM_UNITS( )
!
!   Print some statistics
      CALL TERMINATE(0)
   END PROGRAM F2F90
