010000 ID  DIVISION.                                                            
020000     SKIP2                                                                
030017 PROGRAM-ID.    W4612L00.                                                 
040013*AUTHOR.        GERRY CARMICHAEL                                          
050017*DATE-WRITTEN.  DEC 1999                                                  
060000                                                                          
060100                                                                          
070000*REMARKS.                                                                 
080017*        IMPORTÖRSBEROENDE BEHANDLING (BRASIL, NEW CONCEPT)               
090000*        ALLA POSTER IN OCH UT HAR KORTFORMAT                             
100000*        SKAPAR START OCH SLUTKORT                                        
110000                                                                          
120000*        PT RIF SKALL ENDAST SISTA "RIF KORTET"                           
130000*        SKRIVAS I EN OBRUTEN SERIE  AV DYLIKA KORT                       
140000*        (ENDAST ENGELSK TEXT).                                           
150000*    ABENDKODER:                                                          
160000                                                                          
170000     EJECT                                                                
180000 ENVIRONMENT DIVISION.                                                    
190000     SKIP2                                                                
200000 INPUT-OUTPUT SECTION.                                                    
210000                                                                          
220000 FILE-CONTROL.                                                            
230000     SKIP2                                                                
240000*- - - - - - - - - - - - INFIL:                                           
250000*                        - -  FIL TILL VIPS                               
260017     SELECT W4612L                       ASSIGN TO UT-S-W4612LD1.         
270000     SKIP2                                                                
280000*- - - - - - - - - - - - UTFIL:                                           
290000*                        - -  FIL TILL VIPS                               
300017     SELECT W4612W                       ASSIGN TO UT-S-W4612LD2.         
310000     EJECT                                                                
320000 DATA DIVISION.                                                           
330000     SKIP2                                                                
340000 FILE SECTION.                                                            
350000     SKIP3                                                                
360017 FD  W4612L                                                               
370004     RECORDING      V                                                     
380000     BLOCK CONTAINS 0.                                                    
390004     SKIP2                                                                
400021 01  INPOST         PIC X(211).                                           
410004     EJECT                                                                
420017 FD  W4612W                                                               
430008     RECORDING       V                                                    
440000     BLOCK CONTAINS 0.                                                    
450008 01  UTPOST                       PIC X(80).                              
460008     SKIP2                                                                
470008 01  RID-POST      -COPY W461RIDN       -L.                               
480008     SKIP2                                                                
490008 01  RIE-POST      -COPY W461RIEN       -L.                               
500008     SKIP2                                                                
510008 01  RIH-POST      -COPY W461RIHN       -L.                               
520008     SKIP2                                                                
530019 01  RIO-POST      -COPY W461RIO2       -L.                               
531020     SKIP2                                                                
532020 01  RKB-POST      -COPY W461RKBN       -L.                               
533020     SKIP2                                                                
534020 01  RKC-POST      -COPY W461RKCN       -L.                               
535020     SKIP2                                                                
536020 01  RKD-POST      -COPY W461RKDN       -L.                               
540008     EJECT                                                                
550000 WORKING-STORAGE SECTION.                                                 
560008                                                                          
570008*    -- CHECKED BY WY2000                                                 
580000*- - - - - - - - - - - - - -  PROGRAM-NAMN                                
590017 77  PROGRAM-NAMN                PIC X(8)    VALUE 'W4612L00'.            
600000     SKIP2                                                                
610000*- - - - - - - - - - - - - -  GENERELLA KONSTANTER                        
620000                                                                          
630000 77  JA                          PIC X(1)    VALUE 'J'.                   
640000 77  NEJ                         PIC X(1)    VALUE 'N'.                   
650000     SKIP2                                                                
660000*- - - - - - - - - - - - - -  END-OF-FILE-SWITCHAR                        
670000                                                                          
680017 77  W4612L-EOF                  PIC X(1)    VALUE 'N'.                   
690000     SKIP2                                                                
700000*- - - - - - - - - - - - - -  ANDRA SWITCHAR                              
710000                                                                          
720000 77  W-ANT-POSTER                PIC S9(5)   COMP-3 VALUE +0.             
730000 77  W-MAX-ANT-POSTER            PIC S9(5)   COMP-3 VALUE +9000.          
740000                                                                          
750000*- - - - - - - - - - - - - -                                              
760000                                                                          
770000     EJECT                                                                
771014*      --- VALID IDDC CODES                                               
772014*                                                                         
773014*01    -COPY WWDCKONS                                                     
774014       EJECT                                                              
780000 01  DAGENS-DATUM.                                                        
790000   03  DAGENS-DATUM-AR           PIC 9(2).                                
800000   03  DAGENS-DATUM-MANAD        PIC 9(2).                                
810000   03  DAGENS-DATUM-DAG          PIC 9(2).                                
820000                                                                          
830000 01  DAGENS-TID.                                                          
840000   03  DAGENS-TID-TIM            PIC 9(2).                                
850000   03  DAGENS-TID-MIN            PIC 9(2).                                
860000   03  DAGENS-TID-SEK            PIC 9(2).                                
870000                                                                          
880000 01  DYNAMISKA-SUBPROGRAM.                                                
890000   03  ABEND                     PIC X(8)    VALUE 'ABEND'.               
900000   03  DATKORT                   PIC X(8)    VALUE 'DATKORT'.             
910000   03  POSTSUM                   PIC X(8)    VALUE 'POSTSUM'.             
920000     SKIP3                                                                
930000*- - - - - - - - - - - - - -  PARAMETRAR TILL ABEND                       
940000                                                                          
950000 01  RETURKODER.                                                          
960000   03  RKOD                      PIC S9(4)  COMP SYNC VALUE ZERO.         
970000   03  RKOD-ABEND-UTAN-DUMP      PIC S9(4)  COMP SYNC VALUE +16.          
980000   03  RKOD-ABEND-MED-DUMP       PIC S9(4)  COMP SYNC VALUE +1000.        
990000     EJECT                                                                
000000*- - - - - - - - - - - - - -  ARBETSAREAOR                                
010000                                                                          
020004******************************************************************        
030004                                                                          
040004 01  W-ARBAREA.                                                           
050021 03  W-ARBAREA-X                 PIC X(211).                              
060004     SKIP2                                                                
070009*                                                                         
080009*03  FILLER  -COPY W461RIFN        -PRE W- -RED W-ARBAREA-X               
090009     EJECT                                                                
100004*                                                                         
110004*01  -COPY W461RIFN        -PRE JFR-.                                     
120000     EJECT                                                                
130000*                             STARTKORT                                   
140009*01  -COPY W461RI0N                                                       
150000     EJECT                                                                
160000*                             SLUTKORT                                    
170000*01  -COPY W461RI9                                                        
180000     EJECT                                                                
190000*- - - - - - - - - - - - - -  PARAMETRAR TILL DATKORT                     
200000                                                                          
210000 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
220000     SKIP2                                                                
230000*01  -COPY WDATKORT                                                       
240000     EJECT                                                                
250000*- - - - - - - - - - - - - -  PARAMETRAR TILL POSTSUM                     
260000                                                                          
270000*01  -COPY W0005       -PRE  POSTSUM-.                                    
280000     EJECT                                                                
290000 PROCEDURE DIVISION.                                                      
300000     SKIP2                                                                
310000     PERFORM A-INIT                                                       
320000     PERFORM B-BEHANDLA                                                   
330000     PERFORM Z-FINIT                                                      
340000     MOVE ZERO TO RETURN-CODE                                             
350000     GOBACK                                                               
360000     .                                                                    
370000                                                                          
380000     SKIP3                                                                
390000 A-INIT SECTION.                                                          
400000     SKIP2                                                                
410017     OPEN INPUT W4612L OUTPUT W4612W                                      
420000     SKIP2                                                                
430000     MOVE PROGRAM-NAMN TO POSTSUM-PROGNAMN                                
440000     SKIP2                                                                
450000*    DATUM-INFORMATION HÄMTAS FRÅN DATUMKORTET                            
460000     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
470000     MOVE D-AAR TO DAGENS-DATUM-AR                                        
480000     MOVE D-MAANAD TO DAGENS-DATUM-MANAD                                  
490000     MOVE D-DAG TO DAGENS-DATUM-DAG                                       
500000     SKIP2                                                                
510000*- - - - - - - - - - - - - - - - TID                                      
520000*                                                                         
530000     ACCEPT   DAGENS-TID FROM TIME                                        
540000     .                                                                    
550000     EJECT                                                                
560010                                                                          
570002 B-BEHANDLA SECTION.                                                      
580002     SKIP2                                                                
590002     PERFORM BA-START-KORT                                                
600017     PERFORM S01-LAS-W4612L                                               
610002     PERFORM UNTIL                                                        
620017      NOT ( W4612L-EOF = NEJ )                                            
630002       IF W-RIF-IDPTYP  = 'RIF'                                           
640004         MOVE W-ARBAREA TO JFR-RIF-W461RIFN-CTX                           
650017         PERFORM S01-LAS-W4612L                                           
660002       ELSE                                                               
670002         IF JFR-RIF-IDPTYP   = 'RIF'                                      
680002           ADD     +1           TO W-ANT-POSTER                           
690009           WRITE   UTPOST   FROM JFR-RIF-W461RIFN-CTX                     
700004           MOVE SPACE TO JFR-RIF-W461RIFN-CTX                             
710002         END-IF                                                           
720016         IF W-RIF-IDPTYP = 'RIC' OR 'RIS' OR 'RKA' OR 'RKO'               
730017           PERFORM S01-LAS-W4612L                                         
740002         ELSE                                                             
750009           EVALUATE    W-RIF-IDPTYP                                       
760004                                                                          
770004             WHEN  'RID'                                                  
780009                  WRITE RID-POST FROM INPOST                              
790004                                                                          
800004             WHEN  'RIE'                                                  
810009                  WRITE RIE-POST FROM INPOST                              
820009                                                                          
830009             WHEN  'RIH'                                                  
840009                  WRITE RIH-POST FROM INPOST                              
850004                                                                          
860009             WHEN  'RIO'                                                  
870009                  WRITE RIO-POST FROM INPOST                              
880009                                                                          
881020             WHEN  'RKB'                                                  
882020                  WRITE RKB-POST FROM INPOST                              
883020                                                                          
884020             WHEN  'RKC'                                                  
885020                  WRITE RKC-POST FROM INPOST                              
886020                                                                          
887020             WHEN  'RKD'                                                  
888020                  WRITE RKD-POST FROM INPOST                              
889020                                                                          
890009             WHEN  OTHER                                                  
900009                  WRITE UTPOST   FROM INPOST                              
910004                                                                          
920004           END-EVALUATE                                                   
930004                                                                          
940004           ADD     +1           TO W-ANT-POSTER                           
950017           PERFORM S01-LAS-W4612L                                         
960004         END-IF                                                           
970004       END-IF                                                             
980004     END-PERFORM                                                          
990004     PERFORM BB-SLUT-KORT                                                 
000004     .                                                                    
010004     EJECT                                                                
020009                                                                          
030000 BA-START-KORT SECTION.                                                   
040000     SKIP2                                                                
050000     MOVE     'RI0'          TO START-IDPTYP                              
060022     MOVE     7050           TO START-IDDISTR                             
070014     MOVE     WC-CDC-SE      TO START-IDDC                                
080000     MOVE     DAGENS-DATUM   TO START-TIFILDAT                            
090000*                                                                         
100000     MOVE     DAGENS-TID     TO START-TIHHMMSS                            
110000*                                                                         
120009     DISPLAY  'START-KORT  ' START-W461RI0N-CTX                           
130009     WRITE    UTPOST         FROM  START-W461RI0N-CTX                     
140000     .                                                                    
150000     SKIP2                                                                
160000 BB-SLUT-KORT SECTION.                                                    
170000     SKIP2                                                                
180000     MOVE     'RI9'          TO SLUT-IDPTYP                               
190000     MOVE     W-ANT-POSTER   TO SLUT-KVTRANS                              
200000*                                                                         
210000     WRITE    UTPOST         FROM  SLUT-W461RI9                           
220000     DISPLAY  'SLUT-KORT   ' SLUT-W461RI9                                 
230000     .                                                                    
240000     SKIP2                                                                
250017 S01-LAS-W4612L SECTION.                                                  
260000     SKIP2                                                                
270017     READ   W4612L INTO W-ARBAREA                                         
280017     AT END MOVE JA TO W4612L-EOF                                         
290000     END-READ                                                             
300000                                                                          
310017     IF W4612L-EOF = NEJ                                                  
320000                                                                          
330017       MOVE 'W4612L'            TO POSTSUM-FDNAMN                         
340017       MOVE 'W4612LD1'          TO POSTSUM-DDNAMN2                        
350000       MOVE SPACE               TO POSTSUM-TRANSTYP                       
360000       CALL POSTSUM             USING POSTSUM-PARM                        
370000                                                                          
380000     END-IF                                                               
390000     .                                                                    
400000     EJECT                                                                
410000 Z-FINIT SECTION.                                                         
420000     SKIP2                                                                
430000                                                                          
440017     CLOSE W4612L W4612W                                                  
450000     SKIP2                                                                
460000*- - - - - - - - - - - - - - - - - - SKRIV UT ANTAL LÄSTA OCH             
470000*                                    SKRIVNA POSTER                       
480000                                                                          
490000     MOVE 'S' TO POSTSUM-OPKOD                                            
500000     CALL POSTSUM USING POSTSUM-PARM                                      
510000     .                                                                    
