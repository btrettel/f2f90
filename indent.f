      PROGRAM INDENT                                                            
*                                                                               
************************************************************************        
*                                                                      *        
*   A PROGRAM TO REFORMAT FORTRAN PROGRAM UNITS BY INDENTING THE BODY  *        
*   OF ALL DO-LOOPS AND IF-BLOCKS BY ISHIFT COLUMNS. THIS PERMITS A    *        
*   RAPID VISUAL INSPECTION FOR THE MOST DEEPLY NESTED PARTS OF A      *        
*   PROGRAM, AS WELL AS BEING A MEANS OF IMPROVING ITS LAYOUT.         *        
*                                                                      *        
*   USAGE: THE PROGRAM READS ONE DATA RECORD IN FREE FORMAT FROM THE   *        
*          DEFAULT INPUT UNIT. THIS CONTAINS:                          *        
*                              INDENTATION DEPTH                       *        
*                              MAXIMUM INDENTATION LEVEL               *        
*                              THE UNIT NO. OF THE INPUT SOURCE STREAM *        
*                              THE UNIT NO. OF THE OUTPUT STREAM       *        
*        THE DEFAULT VALUES IN THE ABSENCE OF THIS RECORD ARE 3 10 5 6 *        
*                                                                      *        
*   RESTRICTIONS: THE PROGRAM DOES NOT INDENT FORMAT STATEMENTS OR     *        
*                 ANY STATEMENT CONTAINING A CHARACTER STRING WITH AN  *        
*                 EMBEDDED MULTIPLE BLANK.                             *        
*                 THE ORDER OF COMMENT LINES AND FORTRAN STATEMENTS    *        
*                 IS SLIGHTLY MODIFIED IF THERE ARE SEQUENCES OF       *        
*                 MORE THAN KKLIM (=200) COMMENT LINES.                *        
*                                                                      *        
*   REMEMBER - THE FIRST COLUMN OF THE OUTPUT FILE WILL BE TAKEN AS    *        
*   CARRIAGE CONTROL INFORMATION IF THE OUTPUT UNIT IS A PRINTER       *        
*                                                                      *        
*                       VERSION OF 04/01/84                            *        
*                                                                      *        
************************************************************************        
*                                                                               
*   INITIALIZE                                                                  
      CALL START                                                                
*                                                                               
*   PROCESS THE LINES OF PROGRAM UNITS                                          
      CALL PUNITS                                                               
*                                                                               
*   PRINT SOME STATISTICS                                                       
      CALL TERMIN                                                               
      END                                                                       
      SUBROUTINE PUNITS                                                         
*                                                                               
************************************************************************        
*   THE PRINCIPAL SUBROUTINE OF INDENT PROCESSES THE                   *        
*   INPUT STREAM, WHICH IS ASSUMED TO CONTAIN SYNTACTICALLY CORRECT    *        
*   FORTRAN PROGRAM UNITS. TO PROTECT ITSELF FROM BAD DATA, FAILURE    *        
*   TO PASS A PRIMITIVE SYNTAX CHECK WILL CAUSE THE PROGRAM TO COPY    *        
*   THE INPUT STREAM TO THE OUTPUT UNIT UNCHANGED, UNTIL AN END LINE IS*        
*   ENCOUNTERED.                                                       *        
************************************************************************        
*                                                                               
      COMMON/DATA/ISHIFT , MXDPTH , NIN , NOUT                                  
*                                                                               
      COMMON/STATS/MXDO , MXIF , KARD , KNTPU , SYNTAX                          
*                                                                               
      SAVE /DATA/ , /STATS/                                                     
*                                                                               
************************************************************************        
*   DEFINE MAXIMUM LEVEL OF DO-LOOP NESTING, AND MAXIMUM LENGTH OF     *        
*   A FORTRAN STATEMENT. LEN MAY BE REDUCED FOR                        *        
*   COMPILERS ACCEPTING A MAXIMUM CHARACTER                            *        
*   LENGTH BELOW 1320 AND THIS WILL CAUSE ANY EXCESS                   *        
*   CONTINUATION LINES AND ALL FOLLOWING LINES TO BE COPIED UNCHANGED. *        
*   NEST AND LEN APPEAR IN S/R IDENT AND REFORM ALSO.                  *        
*   KKLIM DEFINES THE LENGTH OF THE COMMENT LINE BUFFER. IF THIS       *        
*   LENGTH IS EXCEEDED, THE STATEMENT PRECEDING THE COMMENTS WILL      *        
*   APPEAR AFTER THEM.                                                 *        
************************************************************************        
*                                                                               
      PARAMETER (NEST = 32 , LEN = 1320 , KKLIM = 200)                          
      PARAMETER (KLEN = 72*KKLIM)                                               
*                                                                               
      COMMON/S1/KNTDO , KNTIF , KNTCOM , LABEL , LENST , SYNERR , LABLNO        
     + , LABLDO(NEST)                                                           
*                                                                               
      COMMON/S2/STAMNT , CBUF                                                   
*                                                                               
************************************************************************        
*   USER IS A CHARACTER WHICH MAY BE DEFINED TO IDENTIFY LINES         *        
*   IN THE INPUT STREAM WHICH ARE TO BE TREATED AS                     *        
*   COMMENT LINES ( + IN THIS EXAMPLE).                                *        
************************************************************************        
*                                                                               
      CHARACTER CONTIN , USER , FIN*3 , FIELD*66 , LINE*72 ,                    
     +          STAMNT*(LEN) , CBUF*(KLEN)                                      
*                                                                               
      LOGICAL SYNERR , NEWDO , NEWIF , SYNTAX , FORM , STAT , ELSEBL            
*                                                                               
      DATA FIN/'END'/ , USER/'+'/ , STAT/.FALSE./                               
*                                                                               
*   START PROCESSING PROGRAM UNITS                                              
      MXDO = 0                                                                  
      MXIF = 0                                                                  
      KARD = 0                                                                  
      KNTPU = 0                                                                 
      SYNTAX = .FALSE.                                                          
      SYNERR = .FALSE.                                                          
      KNTDO = 0                                                                 
      KNTIF = 0                                                                 
      KNTCOM = 0                                                                
*                                                                               
*   SET CONTINUATION LINE COUNTER                                               
    1 KNTCON = 0                                                                
*                                                                               
*   SET STATEMENT LENGTH COUNTER                                                
      LENST = 0                                                                 
*                                                                               
*   READ ONE LINE INTO AN INTERNAL FILE                                         
*   COLUMNS 73-80 OF ALL LINES ARE IGNORED.                                     
    2 READ (NIN , '(A)' , END = 100 , ERR = 100) LINE                           
      KARD = KARD+1                                                             
*                                                                               
*   CHECK WHETHER A COMMENT LINE AND IF SO COPY TO BUFFER.                      
      IF (LINE(:1) .EQ. 'C' .OR. LINE(:1) .EQ. '*' .OR. LINE(:1) .EQ.           
     +USER .OR. LINE .EQ. ' ') THEN                                             
         IF (KNTCOM .EQ. KKLIM) THEN                                            
            WRITE (NOUT , '(A72)') (CBUF(72*L5-71:72*L5) , L5 = 1 ,             
     +      KNTCOM) , LINE                                                      
            KNTCOM = 0                                                          
         ELSEIF (SYNERR .OR. .NOT.STAT) THEN                                    
            WRITE (NOUT , '(A72)') LINE                                         
         ELSE                                                                   
            KNTCOM = KNTCOM+1                                                   
            CBUF(72*KNTCOM-71:72*KNTCOM) = LINE                                 
         ENDIF                                                                  
         GO TO 2                                                                
      ENDIF                                                                     
*                                                                               
*   LINE IS SOME FORM OF STATEMENT. RE-READ.                                    
      READ (LINE , '(BN , I5 , A1 , A66)') LAB , CONTIN , FIELD                 
      STAT = .TRUE.                                                             
*                                                                               
*   CHECK ON SYNTAX AND COPY TO STATEMENT BUFFER                                
    3 IF (CONTIN .NE. ' ') THEN                                                 
         IF (SYNERR) THEN                                                       
            GO TO 6                                                             
         ELSEIF (LENST .EQ. 0 .OR. LENST+66 .GT. LEN .OR. LAB .NE. 0)           
     +   THEN                                                                   
            SYNERR = .TRUE.                                                     
            IF (LENST .GT. 0) THEN                                              
               IF (LABEL .NE. 0) THEN                                           
                  WRITE (NOUT , '(I5 , 1X , A66:/(5X , ''+'' , A66))')          
     +            LABEL , (STAMNT(66*L9-65:66*L9) , L9 = 1 , (LENST+65)         
     +            /66)                                                          
               ELSE                                                             
                  WRITE (NOUT , '(6X , A66:/(5X , ''+'' , A66))')               
     +            (STAMNT(66*L9-65:66*L9) , L9 = 1 , (LENST+65)/66)             
               ENDIF                                                            
            ENDIF                                                               
            IF (LAB .NE. 0) THEN                                                
               WRITE (NOUT , 1000) LAB , CONTIN , FIELD                         
            ELSE                                                                
               WRITE (NOUT , 1006) CONTIN , FIELD                               
            ENDIF                                                               
            GO TO 1                                                             
         ELSE                                                                   
            KNTCON = KNTCON+1                                                   
            STAMNT(LENST+1:LENST+66) = FIELD                                    
            LENST = LENST+66                                                    
            GO TO 2                                                             
         ENDIF                                                                  
      ELSEIF (KNTCON .EQ. 0) THEN                                               
         IF (LENST .NE. 0) GO TO 4                                              
         STAMNT(1:66) = FIELD                                                   
         LENST = 66                                                             
         LABEL = LAB                                                            
         IF (SYNERR) GO TO 4                                                    
         GO TO 2                                                                
      ENDIF                                                                     
      IF (KNTCON .GT. 0) GO TO 6                                                
*                                                                               
*   HAVE A COMPLETE STATEMENT READY FOR PROCESSING ( THE LAST LINE              
*   READ IS STILL WAITING IN LINE). THE STATEMENT NOW NEEDS TO BE               
*   IDENTIFIED.                                                                 
*   THE END STATEMENT IS A SPECIAL CASE - IF FOUND IT WILL BE COPIED            
*   AND THE NEXT PROGRAM UNIT PROCESSED.                                        
    4 K1 = 1                                                                    
      DO 5 L1 = 1 , LENST                                                       
         IF (STAMNT(L1:L1) .EQ. ' ') GO TO 5                                    
         IF (STAMNT(L1:L1) .NE. FIN(K1:K1)) THEN                                
            GO TO 6                                                             
         ELSE                                                                   
            K1 = K1+1                                                           
            IF (K1 .GT. 3 .AND. (L1 .GE. LENST .OR. STAMNT(L1+1:LENST)          
     +      .EQ. ' ')) THEN                                                     
               IF (.NOT.SYNERR) THEN                                            
                  KNTPU=KNTPU+1                                                 
                  IF (LABEL .EQ. 0) THEN                                        
                     WRITE (NOUT , 1001) FIN                                    
                  ELSE                                                          
                     WRITE (NOUT , 1002) LABEL , FIN                            
                  ENDIF                                                         
               ENDIF                                                            
*                                                                               
*   SET COUNTERS FOR NEW PROGRAM UNIT                                           
               SYNTAX = SYNTAX .OR. SYNERR                                      
               KNTDO = 0                                                        
               KNTIF = 0                                                        
               SYNERR = .FALSE.                                                 
               KNTCON = 0                                                       
               LENST = 0                                                        
               IF (KNTCOM .NE. 0) WRITE (NOUT , '(A72)') (CBUF(72*L5-71:        
     +         72*L5) , L5 = 1 , KNTCOM)                                        
               KNTCOM = 0                                                       
               GO TO 3                                                          
            ELSE                                                                
               IF (K1 .GT. 3) GO TO 6                                           
            ENDIF                                                               
         ENDIF                                                                  
    5 CONTINUE                                                                  
*                                                                               
*   IF SYNTAX ERROR FLAG SET, COPY AND TAKE NEXT STATEMENT                      
    6 IF (SYNERR) THEN                                                          
         IF (LAB .NE. 0) THEN                                                   
            WRITE (NOUT , 1000) LAB , CONTIN , FIELD                            
         ELSE                                                                   
            WRITE (NOUT , 1006) CONTIN , FIELD                                  
         ENDIF                                                                  
         LENST = 0                                                              
         GO TO 2                                                                
      ENDIF                                                                     
*                                                                               
*   HAVE A VALID STATEMENT WHICH IS NOT AN END LINE                             
*   IDENTIFY STATEMENT AS    DO                                                 
*                            IF ( ) THEN                                        
*                            DO TERMINATOR                                      
*                            ENDIF                                              
*                            FORMAT                                             
*                            ELSE OR ELSEIF                                     
*                            NONE OF THESE.                                     
      NEWDO = .FALSE.                                                           
      NEWIF = .FALSE.                                                           
      FORM  = .FALSE.                                                           
      ELSEBL = .FALSE.                                                          
      CALL IDENT(*7 , *8 , *10 , *11 , *12 , *13)                               
      GO TO 14                                                                  
*                                                                               
*   NEW DO-LOOP                                                                 
    7 IF (KNTDO .EQ. NEST) GO TO 14                                             
      NEWDO = .TRUE.                                                            
      LABLDO(KNTDO+1) = LABLNO                                                  
      GO TO 14                                                                  
*                                                                               
*   END OF DO-LOOP(S)                                                           
    8 DO 9 L5 = KNTDO , 1 , -1                                                  
         IF (LABLDO(L5) .NE. LABEL) GO TO 14                                    
         KNTDO = KNTDO-1                                                        
    9 CONTINUE                                                                  
      GO TO 14                                                                  
*                                                                               
*   BEGINNING OF IF-BLOCK                                                       
   10 NEWIF = .TRUE.                                                            
      GO TO 14                                                                  
*                                                                               
*   END OF IF-BLOCK                                                             
   11 KNTIF = KNTIF-1                                                           
      IF (KNTIF .LT. 0) THEN                                                    
         SYNERR = .TRUE.                                                        
         KNTIF = 0                                                              
      ENDIF                                                                     
      GO TO 14                                                                  
*                                                                               
*   FORMAT STATEMENT                                                            
   12 FORM =.TRUE.                                                              
      GO TO 14                                                                  
*                                                                               
*   BEGINNING OF ELSE-BLOCK                                                     
   13 ELSEBL = .TRUE.                                                           
*                                                                               
*   REFORMAT STATEMENTS AND WRITE                                               
   14 CALL REFORM (FORM , ELSEBL)                                               
*                                                                               
*   SET VARIABLES FOR NEXT STATEMENT                                            
      IF (NEWDO) KNTDO = KNTDO+1                                                
      IF (NEWIF) KNTIF = KNTIF+1                                                
      KNTCON = 0                                                                
      LENST = 0                                                                 
      MXDO = MAX(MXDO , KNTDO)                                                  
      MXIF = MAX(MXIF , KNTIF)                                                  
      GO TO  3                                                                  
*                                                                               
*   END OF DATA. LAST LINE MUST BE AN END.                                      
  100 IF (LABEL .EQ. 0) WRITE (NOUT , 1001) FIN                                 
      IF (LABEL .NE. 0) WRITE (NOUT , 1002) LABEL , FIN                         
      KNTPU=KNTPU+1                                                             
*                                                                               
 1000 FORMAT(I5 , A1 , A)                                                       
 1001 FORMAT(6X , A3 , 63X , '**IF**DO')                                        
 1002 FORMAT(I5 , 1X , A3 , 63X , '**IF**DO')                                   
 1006 FORMAT(5X , A1 , A66)                                                     
*                                                                               
      END                                                                       
      SUBROUTINE REFORM (FORM , ELSEBL)                                         
*                                                                               
*   PERFORMS REFORMATTING AND OUTPUT OF ACCEPTED STATEMENTS                     
*                                                                               
      PARAMETER (NEST = 32 , LEN = 1320 , KKLIM = 200)                          
      PARAMETER (KLEN = 72*KKLIM)                                               
*                                                                               
      COMMON/DATA/ISHIFT , MXDPTH , NIN , NOUT                                  
*                                                                               
      COMMON/S1/KNTDO , KNTIF , KNTCOM , LABEL , LENST , SYNERR , LABLNO        
     + , LABLDO(NEST)                                                           
*                                                                               
      COMMON/S2/STAMNT , CBUF                                                   
*                                                                               
      CHARACTER STAMNT*(LEN) , OUT*(LEN) , CBUF*(KLEN)                          
*                                                                               
      LOGICAL FORM , SYNERR , ELSEBL                                            
*                                                                               
*   IF FORMAT STATEMENT, DO NOT INDENT                                          
      IF (FORM) GO TO 9                                                         
*                                                                               
*   REFORMAT INDENTED STATEMENT AND WRITE. IF REFORMATTING CAUSES IT            
*   TO EXCEED LEN CHARACTERS, IT WILL BE COPIED UNCHANGED.                      
      IDEPTH = MIN(KNTDO+KNTIF , MXDPTH)                                        
      IF (IDEPTH .EQ. 0) GO TO  9                                               
      IF (ELSEBL) IDEPTH = IDEPTH-1                                             
      IPNT = 1                                                                  
      JPNT = 1                                                                  
    1 IF (MOD(IPNT , 66) .EQ. 1) THEN                                           
         IF (IPNT+65 .GT. LEN) GO TO 9                                          
         OUT(IPNT:IPNT+65) = ' '                                                
         IPNT = IPNT+IDEPTH*ISHIFT                                              
      ENDIF                                                                     
*                                                                               
*   FIND FIRST NON-BLANK CHARACTER                                              
      DO 2 L2 = JPNT , LENST                                                    
         IF (STAMNT(L2:L2) .NE. ' ') GO TO 3                                    
    2 CONTINUE                                                                  
      IF (JPNT .EQ. 1) THEN                                                     
         SYNERR = .TRUE.                                                        
         GO TO 9                                                                
      ELSE                                                                      
         GO TO 10                                                               
      ENDIF                                                                     
*                                                                               
*   FIND FIRST MULTIPLE BLANK (BUT NOT IN A CHARACTER STRING)                   
    3 KNTAP = 0                                                                 
      DO 4 L3 = L2+1 , LENST-1                                                  
         IF (STAMNT(L3:L3) .EQ. '''') KNTAP = 1-KNTAP                           
         IF (STAMNT(L3:L3+1) .EQ. '  ') THEN                                    
            IF (KNTAP .EQ. 0) GO TO 5                                           
            GO TO 9                                                             
         ENDIF                                                                  
    4 CONTINUE                                                                  
      L3 = LENST                                                                
*                                                                               
*   HAVE SECTION WITH NO MULTIPLE BLANKS. THIS CAN BE COPIED TO OUT             
*   IF THERE IS ROOM ON THE CURRENT LINE. OTHERWISE CUT THE                     
*   SECTION AFTER THE NON-ALPHANUMERIC CHARACTER NEAREST TO THE END OF          
*   THE LINE, IF ONE EXISTS.                                                    
*   AN APOSTROPHE IS CONSIDERED TO BE AN ALPHANUMERIC CHARACTER, IN             
*   ORDER TO HOLD CHARACTER STRINGS TOGETHER.                                   
    5 KADD = 0                                                                  
      IF (L3-L2 .LE. 66-MOD(IPNT , 66)) GO TO  8                                
      DO 6 L4 = 66+L2-MOD(IPNT , 66) , L2 , -1                                  
         IF (STAMNT(L4:L4) .EQ. '''') GO TO 6                                   
         IF (LGE(STAMNT(L4:L4) , 'A') .AND. LLE(STAMNT(L4:L4) , 'Z'))           
     +   GO TO 6                                                                
         IF (LLT(STAMNT(L4:L4) , '0') .OR. LGT(STAMNT(L4:L4) , '9'))            
     +   GO TO 7                                                                
    6 CONTINUE                                                                  
      L4 = 66-MOD(IPNT , 66)+L2                                                 
    7 L3 = L4                                                                   
      KADD = 1                                                                  
    8 LOUT = IPNT+L3-L2                                                         
      IF (LOUT .GT. LEN) GO TO  9                                               
      OUT(IPNT:LOUT) = STAMNT(L2:L3)                                            
      IF (L3 .EQ. LENST) GO TO 10                                               
*                                                                               
*   SET POINTERS FOR NEXT SECTION OF STATEMENT                                  
      IPNT = LOUT+1                                                             
      IF (KADD .EQ. 1 .AND. MOD(IPNT , 66) .NE. 1 .OR. MOD(IPNT , 66)           
     + .GE. 60) IPNT = ((IPNT+65)/66)*66+1                                      
      IF (MOD(IPNT , 66) .EQ. 0) IPNT = IPNT+1                                  
      JPNT = L3+1                                                               
      IF (KADD .EQ. 0) JPNT = JPNT+1                                            
      GO TO  1                                                                  
*                                                                               
*   COPIED STATEMENT                                                            
    9 IF (LABEL .NE. 0) THEN                                                    
         WRITE (NOUT , 1003) LABEL , STAMNT(:66)                                
      ELSE                                                                      
         WRITE (NOUT , 1004) STAMNT(:66)                                        
      ENDIF                                                                     
      IF (LENST .GT. 66) WRITE (NOUT , 1005)                                    
     +(STAMNT(66*L6-65:66*L6) , L6 = 2 , (LENST+65)/66)                         
      GO TO 11                                                                  
*                                                                               
*   WRITE OUT, KNTIF AND KNTDO TO OUTPUT UNIT                                   
   10 IF (LABEL .NE. 0) THEN                                                    
         WRITE (NOUT , 1003) LABEL , OUT(:66) , KNTIF , KNTDO                   
      ELSE                                                                      
         WRITE (NOUT , 1004) OUT(:66) , KNTIF , KNTDO                           
      ENDIF                                                                     
      IF (LOUT .GT. 66) WRITE (NOUT , 1005)                                     
     + (OUT(66*L5-65:66*L5) , L5 = 2 , (LOUT+65)/66)                            
*                                                                               
*   WRITE ANY COMMENTS FOLLOWING STATEMENT                                      
   11 IF (KNTCOM .NE. 0) THEN                                                   
         WRITE (NOUT , '(A72)') (CBUF(72*L5-71:72*L5) , L5 = 1 , KNTCOM)        
         KNTCOM = 0                                                             
      ENDIF                                                                     
*                                                                               
 1003 FORMAT(I5 , 1X , A66 , 2I4)                                               
 1004 FORMAT(6X , A66 , 2I4)                                                    
 1005 FORMAT(5X , '+' , A66)                                                    
*                                                                               
      END                                                                       
      SUBROUTINE IDENT (* , * , * , * , * , *)                                  
*                                                                               
************************************************************************        
*   TO IDENTIFY STATEMENT AS BEGINNING OR END OF DO-LOOP OR            *        
*   IF-BLOCK, OR AS PROBABLE FORMAT.                                   *        
*   ATTEMPT TO SCAN AS FEW OF THE INPUT CHARACTERS AS POSSIBLE.        *        
************************************************************************        
*                                                                               
      PARAMETER (NEST = 32 , LEN = 1320 , KKLIM = 200)                          
      PARAMETER (KLEN = 72*KKLIM)                                               
*                                                                               
      COMMON/S1/KNTDO , KNTIF , KNTCOM , LABEL , LENST , SYNERR , LABLNO        
     + , LABLDO(NEST)                                                           
*                                                                               
      COMMON/S2/STAMNT , CBUF                                                   
      CHARACTER *(LEN) STAMNT  , CBUF*(KLEN)                                    
*                                                                               
      CHARACTER *5 INTFIL , ENDIF , BIF*3 , THEN , DO*2 , FORMAT*7 ,            
     +             ELSE*4                                                       
*                                                                               
      LOGICAL SYNERR                                                            
*                                                                               
      DATA ENDIF/'ENDIF'/ , BIF/'IF('/ , THEN/'NEHT)'/ , DO/'DO'/ ,             
     +FORMAT/'FORMAT('/ , ELSE/'ELSE'/                                          
*                                                                               
*   CHECK WHETHER END OF DO-LOOP                                                
      IF (KNTDO .NE. 0) THEN                                                    
         IF (LABEL .EQ. LABLDO(KNTDO)) RETURN 2                                 
      ENDIF                                                                     
*                                                                               
*   CHECK WHETHER ANY OF REMAINING POSSIBILITIES                                
      DO 1 L7 = 1 , LENST                                                       
         IF (STAMNT(L7:L7) .EQ. ' ') GO TO 1                                    
         IF (STAMNT(L7:L7) .EQ. 'E') THEN                                       
            DO 2 L11 = L7+1 , LENST                                             
               IF (STAMNT(L11:L11) .EQ. ' ') GO TO 2                            
               IF (STAMNT(L11:L11) .EQ. ENDIF(2:2)) GO TO 6                     
               IF (STAMNT(L11:L11) .EQ. ELSE(2:2)) GO TO 3                      
               GO TO 99                                                         
    2       CONTINUE                                                            
         ENDIF                                                                  
         IF (STAMNT(L7:L7) .EQ. BIF(:1)) GO TO 9                                
         IF (STAMNT(L7:L7) .EQ. DO(:1)) GO TO 15                                
         IF (STAMNT(L7:L7) .EQ. FORMAT(:1)) GO TO 31                            
         GO TO 99                                                               
    1 CONTINUE                                                                  
      GO TO 99                                                                  
*                                                                               
*   CHECK WHETHER ELSE OR ELSEIF                                                
    3 K8 = 3                                                                    
      DO 4 L12 = L11+1 , LENST                                                  
         IF (STAMNT(L12:L12) .EQ. ' ') GO TO 4                                  
         IF (STAMNT(L12:L12) .NE. ELSE(K8:K8)) GO TO 99                         
         IF (K8 .EQ. 4) GO TO 5                                                 
         K8 = K8+1                                                              
    4 CONTINUE                                                                  
      GO TO 99                                                                  
    5 IF (L12 .GE. LENST) RETURN 6                                              
      IF (STAMNT(L12+1:LENST) .EQ. ' ') RETURN 6                                
      K2 = 1                                                                    
      IRET = 6                                                                  
      L7 = L12                                                                  
      GO TO 10                                                                  
*                                                                               
*   CHECK WHETHER END OF IF-BLOCK                                               
    6 K1 = 3                                                                    
      DO 7 L1 = L11+1,LENST                                                     
         IF (STAMNT(L1:L1) .EQ. ' ') GO TO 7                                    
         IF (STAMNT(L1:L1) .NE. ENDIF (K1:K1)) GO TO 99                         
         IF (K1 .EQ. 5) GO TO 8                                                 
         K1 = K1+1                                                              
    7 CONTINUE                                                                  
    8 IF (L1 .GE. LENST) RETURN 4                                               
      IF (STAMNT(L1+1:LENST) .EQ. ' ') RETURN 4                                 
      GO TO 99                                                                  
*                                                                               
*   CHECK WHETHER BEGINNING OF IF-BLOCK                                         
    9 K2 = 2                                                                    
      IRET = 3                                                                  
   10 DO 11 L2 = L7+1 , LENST                                                   
         IF (STAMNT(L2:L2) .EQ. ' ') GO TO 11                                   
         IF (STAMNT(L2:L2) .NE. BIF (K2:K2)) GO TO 99                           
         IF (K2 .EQ. 3) GO TO 12                                                
         K2 = K2+1                                                              
   11 CONTINUE                                                                  
      GO TO 99                                                                  
*                                                                               
*   BACKWARD SEARCH FOR )THEN AT END OF IF STATEMENT (TO SAVE                   
*   SCANNING THE CONDITION)                                                     
*   AVOID PLACING RETURN STATEMENT INSIDE THE LOOP                              
   12 K3 = 1                                                                    
      DO 13 L3 = LENST , L2+1 , -1                                              
         IF (STAMNT(L3:L3) .EQ. ' ') GO TO 13                                   
         IF (STAMNT(L3:L3) .NE. THEN(K3:K3)) GO TO 99                           
         IF (K3 .EQ. 5) GO TO 14                                                
         K3 = K3+1                                                              
   13 CONTINUE                                                                  
      GO TO 99                                                                  
   14 RETURN IRET                                                               
*                                                                               
*   CHECK WHETHER BEGINNING OF DO-LOOP                                          
   15 DO 16 L4 = L7+1 , LENST                                                   
         IF (STAMNT(L4:L4) .EQ. ' ') GO TO 16                                   
         IF (STAMNT(L4:L4) .EQ. DO(2:2)) GO TO 17                               
         GO TO 99                                                               
   16 CONTINUE                                                                  
      GO TO 99                                                                  
*                                                                               
*   HAVE DO - CHECK LABEL                                                       
   17 K5 = 0                                                                    
      INTFIL = ' '                                                              
      DO 18 L5 = L4+1 , LENST                                                   
         IF (STAMNT(L5:L5) .EQ. ' ') GO TO 18                                   
         IF (LLT(STAMNT(L5:L5) , '0') .OR. LGT(STAMNT(L5:L5) , '9'))            
     +   GO TO 19                                                               
         K5 = K5+1                                                              
         IF (K5 .GT. 5) GO TO 20                                                
         INTFIL(K5:K5) = STAMNT(L5:L5)                                          
   18 CONTINUE                                                                  
   19 IF (K5 .EQ. 0) GO TO 99                                                   
   20 READ (INTFIL , '(BN , I5)') LABLNO                                        
      IF (LABLNO .EQ. 0) GO TO 99                                               
*                                                                               
*   HAVE LABEL - CHECK COMMA                                                    
      DO 21 L8 = L5 , LENST                                                     
         IF (STAMNT(L8:L8) .EQ. ' ') GO TO 21                                   
         IF (STAMNT(L8:L8) .EQ. ',') GO TO 22                                   
         GO TO 23                                                               
   21 CONTINUE                                                                  
   22 RETURN 1                                                                  
*                                                                               
*   HAVE A DO AND LABEL WITH NO COMMA.                                          
*   CHECK FOR VARIABLE WHOSE FIRST OF MAXIMUM OF SIX                            
*   CHARACTERS IS ALPHABETIC, FOLLOWED BY AN EQUALS SIGN,                       
*   FOLLOWED BY A CHARACTER STRING CONTAINING A COMMA WHICH IS                  
*   NOT ENCLOSED IN PARENTHESES.                                                
   23 K6 = 0                                                                    
      DO 24 L9 = L8 , LENST                                                     
         IF (STAMNT(L9:L9) .EQ. ' ') GO TO 24                                   
         IF (K6 .EQ. 0) THEN                                                    
            IF (LLT(STAMNT(L9:L9) , 'A') .OR. LGT(STAMNT(L9:L9) , 'Z'))         
     +      GO TO 99                                                            
            K6 = 1                                                              
         ELSEIF (LGE(STAMNT(L9:L9) , 'A') .AND. LLE(STAMNT(L9:L9) , 'Z')        
     +   .OR. LGE(STAMNT(L9:L9) , '0') .AND. LLE(STAMNT(L9:L9) , '9'))          
     +   THEN                                                                   
            K6 = K6+1                                                           
            IF (K6 .EQ. 6) GO TO 26                                             
         ELSE                                                                   
            IF (K6 .EQ. 0) GO TO 99                                             
            GO TO 25                                                            
         ENDIF                                                                  
   24 CONTINUE                                                                  
      GO TO 99                                                                  
*                                                                               
*   EXPECT AN EQUALS SIGN                                                       
   25 L9=L9-1                                                                   
   26 DO 27 L10 = L9+1 , LENST                                                  
         IF (STAMNT(L10:L10) .EQ. ' ') GO TO 27                                 
         IF (STAMNT(L10:L10) .EQ. '=') GO TO 28                                 
         GO TO 99                                                               
   27 CONTINUE                                                                  
      GO TO 99                                                                  
*                                                                               
*   SEARCH FOR BARE COMMA                                                       
   28 LPAREN = 0                                                                
      KNTCH = 0                                                                 
      DO 29 L6 = L10+1 , LENST                                                  
         IF (STAMNT(L6:L6) .EQ. ' ') GO TO 29                                   
         IF (STAMNT(L6:L6) .EQ. ',') THEN                                       
            IF (KNTCH .NE. 0) THEN                                              
               IF (LPAREN .EQ. 0) GO TO 30                                      
               GO TO 29                                                         
            ELSE                                                                
               GO TO 99                                                         
            ENDIF                                                               
         ELSEIF (STAMNT(L6:L6) .EQ. '(') THEN                                   
            LPAREN = LPAREN+1                                                   
         ELSEIF (STAMNT(L6:L6) .EQ. ')') THEN                                   
            LPAREN = LPAREN-1                                                   
         ENDIF                                                                  
         KNTCH = 1                                                              
   29 CONTINUE                                                                  
      GO TO 99                                                                  
   30 RETURN 1                                                                  
*                                                                               
*   IDENTIFY FORMAT STATEMENT                                                   
   31 IF (LABEL .EQ. 0) GO TO 99                                                
      K7 = 2                                                                    
      DO 32 L11 = L7+1 , LENST                                                  
         IF (STAMNT(L11:L11) .EQ. ' ') GO TO 32                                 
         IF (STAMNT(L11:L11) .NE. FORMAT(K7:K7)) GO TO 99                       
         IF (K7 .EQ. 7) GO TO 33                                                
         K7 = K7+1                                                              
   32 CONTINUE                                                                  
      GO TO 99                                                                  
   33 RETURN 5                                                                  
*                                                                               
   99 END                                                                       
      SUBROUTINE START                                                          
*                                                                               
*   TO PREPARE FOR PUNITS                                                       
*                                                                               
      COMMON/DATA/ISHIFT , MXDPTH , NIN , NOUT                                  
      SAVE /DATA/                                                               
*                                                                               
*   PROMPT FOR INTERACTIVE USE                                                  
      WRITE (*,'(''1PLEASE TYPE SHIFT, MAX. INDENT LEVEL, INPUT UNIT NO.        
     +, OUTPUT UNIT NO.'')')                                                    
*                                                                               
*   DOES STANDARD INPUT UNIT CONTAIN AN INPUT RECORD                            
      READ (* , * , END = 1 , ERR = 1) ISHIFT , MXDPTH , NIN , NOUT             
*                                                                               
*   IF RECORD PRESENT, CHECK INPUT VALUES ARE REASONABLE                        
      ISHIFT = MIN(MAX(ISHIFT , 1) , 10)                                        
      MXDPTH = MIN(MAX(MXDPTH , 1) , 36/ISHIFT)                                 
      IF (NIN .LE. 0 .OR. NIN .GT. 99) NIN = 5                                  
      IF (NOUT .LE. 0 .OR. NOUT .EQ. NIN .OR. NOUT .GT. 99) NOUT = 6            
      GO TO 2                                                                   
*                                                                               
*   SET DEFAULT VALUES                                                          
    1 ISHIFT = 3                                                                
      MXDPTH = 10                                                               
      NIN = 5                                                                   
      NOUT = 6                                                                  
*                                                                               
*   PRINT VALUES TO BE USED                                                     
    2 WRITE (*,'(''0LOOP BODIES WILL BE INDENTED BY'',I3/                       
     +           '' MAXIMUM INDENTING LEVEL IS     '',I3/                       
     +           '' INPUT UNIT EXPECTED IS         '',I3/                       
     +           '' OUTPUT UNIT EXPECTED IS        '',I3)')                     
     +        ISHIFT , MXDPTH , NIN , NOUT                                      
*                                                                               
      END                                                                       
      SUBROUTINE TERMIN                                                         
*                                                                               
*   TO PRINT THE FINAL SUMMARY                                                  
*                                                                               
      COMMON/STATS/MXDO , MXIF , KARD , KNTPU , SYNTAX                          
      LOGICAL SYNTAX                                                            
*                                                                               
      SAVE /STATS/                                                              
*                                                                               
      WRITE (*,'(''0MAXIMUM DEPTH OF DO-LOOP NESTING '',I3/                     
     +           '' MAXIMUM DEPTH OF IF-BLOCK NESTING'',I3/                     
     +'' NO. OF LINES READ  '',I17/'' NO. OF PROGRAM UNITS READ   '',I8/        
     +           '' GLOBAL SYNTAX ERROR FLAG'',L12)')                           
     +          MXDO , MXIF , KARD , KNTPU , SYNTAX                             
*                                                                               
      END                                                                       
