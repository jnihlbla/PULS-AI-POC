001000 PROCESS DYNAM                                                            
010000 ID DIVISION.                                                             
020002 PROGRAM-ID.     WF102600.                                                
030000 AUTHOR.         BARSHARANI BISHOYE.                                      
040001 DATE-WRITTEN.   01/09/2020.                                              
050000 DATE-COMPILED.                                                           
060000                                                                          
070000                                                                          
080000*    FUNCTION:                                                            
090000*        GET ONE YEAR OF RECORD FROM DLIN_ARC                             
100000*        DB2 TABLE FOR VCCS VCIN VCCN VCUS VCKR                           
110000*                                                                         
120000*        THE PROGRAM READS   TABLE T01DLIN_ARC                            
130000*                                                                         
140000                                                                          
150000     SKIP3                                                                
160000 ENVIRONMENT DIVISION.                                                    
170000     SKIP2                                                                
180000 INPUT-OUTPUT SECTION.                                                    
190000                                                                          
200000 FILE-CONTROL.                                                            
210000     SKIP2                                                                
220000*          --- ONE YEAR OF VCCS RECORDS AS PER DC                         
230002     SELECT W51293C                    ASSIGN TO WF1026D1.                
240000     EJECT                                                                
250000*          --- ONE YEAR OF NON-VCCS RECORDS AS PER DC                     
260002     SELECT W51294C                    ASSIGN TO WF1026D2.                
270000     EJECT                                                                
280000*          --- ONE YEAR OF VCCS RECORDS AS PER COMPANY                    
290002     SELECT W51295C                    ASSIGN TO WF1026D3.                
300000     EJECT                                                                
310000*          --- ONE YEAR OF NON-VCCS RECORDS AS PER COMPANY                
320002     SELECT W51296C                    ASSIGN TO WF1026D4.                
330000     EJECT                                                                
340000 DATA DIVISION.                                                           
350000     SKIP3                                                                
360000 FILE SECTION.                                                            
370000     SKIP3                                                                
380000 FD  W51293C                                                              
390000     RECORDING       F                                                    
400000     BLOCK CONTAINS  0.                                                   
410000                                                                          
420000*01 POST-W51293C -COPY W51293   -L.                                       
430000     EJECT                                                                
440000     SKIP3                                                                
450000 FD  W51294C                                                              
460000     RECORDING       F                                                    
470000     BLOCK CONTAINS  0.                                                   
480000                                                                          
490000*01 POST-W51294C -COPY W51293   -L.                                       
500000     EJECT                                                                
510000     SKIP3                                                                
520000 FD  W51295C                                                              
530000     RECORDING       F                                                    
540000     BLOCK CONTAINS  0.                                                   
550000                                                                          
560000*01 POST-W51295C -COPY W51293   -L.                                       
570000     EJECT                                                                
580000     SKIP3                                                                
590000 FD  W51296C                                                              
600000     RECORDING       F                                                    
610000     BLOCK CONTAINS  0.                                                   
620000                                                                          
630000*01 POST-W51296C -COPY W51293   -L.                                       
640000     EJECT                                                                
650000 WORKING-STORAGE SECTION.                                                 
660000                                                                          
670002 77  IDPGM                       PIC X(8)    VALUE 'WF102600'.            
680000     SKIP2                                                                
690000 77  WS-IX                       PIC S9(3) COMP-3 VALUE ZERO.             
700000 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
710000 01  WS-CURRENT-DATE             PIC X(8)  VALUE SPACE.                   
720000 01  WS-RUNDATUM-TO              PIC X(8)  VALUE SPACE.                   
730000 01  WS-RUNDATUM-FROM            PIC X(8)  VALUE SPACE.                   
740000 01  WS-RUNDATUM                 PIC X(8)  VALUE SPACE.                   
750000 01  WS-IDEXCUST-MIN             PIC X(15) VALUE SPACE.                   
760000 01  WS-IDEXCUST-MAX             PIC X(15) VALUE SPACE.                   
760100 01  WS-SULEVANT                 PIC S9(9) COMP-3.                        
770000     EJECT                                                                
780000 01  GENERAL-SUBPROGRAMS.                                                 
790000*                                                                         
800000     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
810000     EJECT                                                                
820002 01  PROGRAM-NAMN                PIC X(6)    VALUE 'WF1026'.              
830000                                                                          
840000 01  MESSAGE-CODES.                                                       
850000     03  ERR-WRONG-KEY           PIC X(3)    VALUE '022'.                 
860000     EJECT                                                                
870000 01  WZ20DAYS                    PIC X(8)    VALUE 'WZ20DAYS'.            
880000     SKIP3                                                                
890000*    -COPY WZ20DAYS                                                       
900000     EJECT                                                                
910000                                                                          
920000*    --- PARAMETRAR TILL ABEND                                            
930000*                                                                         
940000 01  RKOD-ABEND-DB2            PIC S9(4)   VALUE +998 COMP SYNC.          
950000 01  RKOD-ABEND-WITH-DUMP      PIC S9(4)   VALUE +999 COMP SYNC.          
960000     EJECT                                                                
970000*    --- PARAMETRAR TILL POSTSUM                                          
980000*                                                                         
990000*01  -COPY W0005   -PRE  POSTSUM-                                         
000000     EJECT                                                                
010000*01  -COPY W51293  -PRE UT-                                               
020000*                                                                         
030000     EJECT                                                                
040000*                                                                         
050000*        WORK-AREAS FOR DB2-SECTIONS                                      
060000*                                                                         
070000 01  FILLER                    PIC X(16) VALUE 'T01LSEL-AREA    '.        
080000*01  -COPY T01LSEL        -PRE LSEL-                                      
090000     EXEC SQL INCLUDE T01LSEL   END-EXEC.                                 
100000     EJECT                                                                
110000 01  FILLER                    PIC X(16) VALUE 'T01DLINARC-AREA '.        
120000*01  -COPY T01DLIN        -PRE DLIN-                                      
130000     EXEC SQL INCLUDE T01DLIN   END-EXEC.                                 
140000     EJECT                                                                
150000 01  FILLER                    PIC X(16)   VALUE 'SQLCA-AREA'.            
160000       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
170000*                        **** STATUS-CODE FROM DB2                        
180000                                                                          
190000 01  FILLER                    PIC X(16)   VALUE 'SQLCODE-WS'.            
200000 01  DB2-WS.                                                              
210000   03  SQLCODE-WS              PIC S9(3)   VALUE ZERO.                    
220000     88  ROW-FOUND                         VALUE +000.                    
230000     88  ROW-MISSING                       VALUE +100.                    
240000   03  GOOD-SQLCODES.                                                     
250000     05  GOOD-SQLCODE OCCURS 5                                            
260000         INDEXED BY SQLCODE-IX PIC 999.                                   
270000     EJECT                                                                
280000 LINKAGE SECTION.                                                         
290000                                                                          
300000*01  -COPY W0009   -PRE MSG-                                              
310000     EJECT                                                                
320000 PROCEDURE DIVISION  USING MSG-PCB.                                       
330000 MAIN SECTION.                                                            
340000     ENTRY 'DLITCBL' USING MSG-PCB.                                       
350000                                                                          
360000     PERFORM A-INIT                                                       
370000                                                                          
380000     PERFORM B-EXECUTE                                                    
390000                                                                          
400000     PERFORM Z-FINISH                                                     
410000     MOVE ZERO TO RETURN-CODE                                             
420000     GOBACK                                                               
430000     .                                                                    
440000     EJECT                                                                
450000 A-INIT SECTION.                                                          
460000     OPEN OUTPUT W51293C                                                  
470000                 W51294C                                                  
480000                 W51295C                                                  
490000                 W51296C                                                  
500000                                                                          
510000     MOVE FUNCTION CURRENT-DATE (1:8) TO WS-CURRENT-DATE                  
520000     MOVE WS-CURRENT-DATE (1:8)       TO DAYS-TIDATE2                     
530000     MOVE 'YYYYMMDD'                  TO DAYS-KDDATFMT2                   
540000     MOVE 368                         TO DAYS-KVDAYS                      
550000     MOVE ' '                         TO DAYS-IDCALEND                    
560000     MOVE SPACE                       TO DAYS-TIDATE1                     
570000     MOVE 'YYYYMMDD'                  TO DAYS-KDDATFMT1                   
580000     CALL WZ20DAYS USING                                                  
590000          DAYS-WZ20DAYS                                                   
600000     IF DAYS-KDRC = ZERO                                                  
610000       MOVE DAYS-TIDATE2              TO WS-RUNDATUM-TO                   
620000       MOVE DAYS-TIDATE1              TO WS-RUNDATUM-FROM                 
630000     END-IF                                                               
640000     INITIALIZE GOOD-SQLCODES                                             
650000     .                                                                    
660000     EJECT                                                                
670000 B-EXECUTE SECTION.                                                       
680000     PERFORM DB2-DCL-OPN-T01LSEL-CRS                                      
690000     PERFORM DB2-FETCH-T01LSEL-CRS                                        
700000                                                                          
710000       PERFORM UNTIL ROW-MISSING                                          
720000         IF LSEL-IDLEGSEL = 'VCCS'                                        
730001           MOVE '70' TO  WS-IDEXCUST-MIN                                  
740001           MOVE '74' TO  WS-IDEXCUST-MAX                                  
750000         ELSE                                                             
760001           MOVE '70' TO  WS-IDEXCUST-MIN                                  
770001           MOVE '74' TO  WS-IDEXCUST-MAX                                  
780000         END-IF                                                           
790000         PERFORM DB2-OPEN-CRS1-DLIN-DC                                    
800000         PERFORM DB2-FETCH-CRS1-DLIN-DC                                   
810000           PERFORM UNTIL ROW-MISSING                                      
820000             PERFORM C-MOVE-OUTPUT                                        
830000             PERFORM S11-WRITE-W51293C-W51294C                            
840000             PERFORM DB2-FETCH-CRS1-DLIN-DC                               
850000           END-PERFORM                                                    
860000           PERFORM DB2-CLOSE-CRS1-DLIN-DC                                 
870000                                                                          
880000         PERFORM DB2-OPEN-CRS2-DLIN-LSEL                                  
890000         PERFORM DB2-FETCH-CRS2-DLIN-LSEL                                 
900000           PERFORM UNTIL ROW-MISSING                                      
910000             PERFORM C-MOVE-OUTPUT                                        
920000             MOVE SPACE   TO UT-IDDC                                      
930000             PERFORM S11-WRITE-W51295C-W51296C                            
940000             PERFORM DB2-FETCH-CRS2-DLIN-LSEL                             
950000         END-PERFORM                                                      
960000         PERFORM DB2-CLOSE-CRS2-DLIN-LSEL                                 
970000         PERFORM DB2-FETCH-T01LSEL-CRS                                    
980000       END-PERFORM                                                        
990000     PERFORM DB2-CLOSE-T01LSEL-CRS                                        
000000     .                                                                    
010000     EJECT                                                                
020000 C-MOVE-OUTPUT SECTION.                                                   
030000     MOVE DLIN-IDARTNR-FINANCE TO UT-IDARTNR-FINANCE                      
040000     MOVE DLIN-IDLEGSEL        TO UT-IDLEGSEL                             
050000     MOVE DLIN-IDDC            TO UT-IDDC                                 
060000     MOVE WS-SULEVANT          TO UT-SULEVANT                             
070000     .                                                                    
080000     EJECT                                                                
090000 S11-WRITE-W51293C-W51294C SECTION.                                       
100000     IF DLIN-IDLEGSEL  = 'VCCS'                                           
110000       WRITE POST-W51293C FROM UT-W51293                                  
120000     ELSE                                                                 
130000       WRITE POST-W51294C FROM UT-W51293                                  
140000     END-IF                                                               
150000     .                                                                    
160000     EJECT                                                                
170000 S11-WRITE-W51295C-W51296C SECTION.                                       
180000     SKIP2                                                                
190000     IF DLIN-IDLEGSEL  = 'VCCS'                                           
200000       WRITE POST-W51295C FROM UT-W51293                                  
210000     ELSE                                                                 
220000       WRITE POST-W51296C FROM UT-W51293                                  
230000     END-IF                                                               
240000     .                                                                    
250000     EJECT                                                                
260000 Z-FINISH SECTION.                                                        
270000     CLOSE W51293C                                                        
280000           W51294C                                                        
290000           W51295C                                                        
300000           W51296C                                                        
310000     .                                                                    
320000     EJECT                                                                
330000* --- DB2 SECTIONS  ---                                                   
340000 DB2-DCL-OPN-T01LSEL-CRS SECTION.                                         
350000     MOVE 000100 TO GOOD-SQLCODES                                         
360000                                                                          
370000     EXEC SQL                                                             
380000         DECLARE LSEL-CRS CURSOR FOR                                      
390000            SELECT  IDLEGSEL                                              
400000                                                                          
410000            FROM     T01LSEL                                              
420000                                                                          
430000     END-EXEC                                                             
440000                                                                          
450000     MOVE 000100 TO GOOD-SQLCODES                                         
460000                                                                          
470000     EXEC SQL                                                             
480000        OPEN LSEL-CRS                                                     
490000     END-EXEC                                                             
500000                                                                          
510000     MOVE SQLCODE TO SQLCODE-WS                                           
520000     PERFORM DB2-STATUS-CHECK                                             
530000     .                                                                    
540000                                                                          
550000 DB2-FETCH-T01LSEL-CRS SECTION.                                           
560000     MOVE 000100  TO GOOD-SQLCODES                                        
570000                                                                          
580000     EXEC SQL                                                             
590000         FETCH LSEL-CRS                                                   
600000                                                                          
610000         INTO :LSEL-IDLEGSEL                                              
620000     END-EXEC                                                             
630000                                                                          
640000     MOVE SQLCODE TO SQLCODE-WS                                           
650000     PERFORM DB2-STATUS-CHECK                                             
660000     .                                                                    
670000 DB2-CLOSE-T01LSEL-CRS SECTION.                                           
680000                                                                          
690000     EXEC SQL                                                             
700000        CLOSE LSEL-CRS                                                    
710000     END-EXEC                                                             
720000     .                                                                    
730000                                                                          
740000 DB2-OPEN-CRS1-DLIN-DC SECTION.                                           
750000     MOVE 000100 TO GOOD-SQLCODES                                         
760000     EXEC SQL                                                             
770000        DECLARE CRS1 CURSOR FOR                                           
780000        SELECT IDDC,SUM(KVLEVART)                                         
790000             , IDLEGSEL                                                   
800000             , IDARTNR_FINANCE                                            
810000                                                                          
820000        FROM   T01DLIN_ARC                                                
830000                                                                          
840001        WHERE    KDFINDOC         = 'INT'                                 
850000        AND      IDEXCUST_1       > :WS-IDEXCUST-MIN                      
860000        AND      IDEXCUST_1       < :WS-IDEXCUST-MAX                      
870000        AND      IDARTNR_FINANCE  > ' '                                   
880000        AND      FLSOFT           = 'N'                                   
890000        AND      IDLEGSEL         = :LSEL-IDLEGSEL                        
900000        AND   (DAEXDAT    =  :WS-RUNDATUM-FROM                            
910000        OR     DAEXDAT    >  :WS-RUNDATUM-FROM)                           
920000        AND   (DAEXDAT    =  :WS-RUNDATUM-TO                              
930000        OR     DAEXDAT    <  :WS-RUNDATUM-TO)                             
940000        GROUP BY                                                          
950000                 T01DLIN_ARC.IDDC                                         
960000                ,T01DLIN_ARC.IDARTNR_FINANCE                              
970000                ,T01DLIN_ARC.IDLEGSEL                                     
980000     END-EXEC                                                             
990000                                                                          
000000     EXEC SQL                                                             
010000        OPEN CRS1                                                         
020000     END-EXEC                                                             
030000                                                                          
040000     MOVE SQLCODE        TO SQLCODE-WS                                    
050000     PERFORM DB2-STATUS-CHECK                                             
060000     .                                                                    
070000 DB2-FETCH-CRS1-DLIN-DC  SECTION.                                         
080000     MOVE 000100         TO GOOD-SQLCODES                                 
090000                                                                          
100000     EXEC SQL                                                             
110000              FETCH CRS1                                                  
120000              INTO  :DLIN-IDDC                                            
130000                  , :WS-SULEVANT                                          
140000                  , :DLIN-IDLEGSEL                                        
150000                  , :DLIN-IDARTNR-FINANCE                                 
160000     END-EXEC                                                             
170000                                                                          
180000     MOVE SQLCODE        TO SQLCODE-WS                                    
190000     PERFORM DB2-STATUS-CHECK                                             
200000     .                                                                    
210000 DB2-CLOSE-CRS1-DLIN-DC  SECTION.                                         
220000                                                                          
230000     EXEC SQL                                                             
240000        CLOSE CRS1                                                        
250000     END-EXEC                                                             
260000     .                                                                    
270000 DB2-OPEN-CRS2-DLIN-LSEL SECTION.                                         
280000     MOVE 000100 TO GOOD-SQLCODES                                         
290000     EXEC SQL                                                             
300000        DECLARE CRS2 CURSOR FOR                                           
310000        SELECT IDLEGSEL,SUM(KVLEVART)                                     
320000             , IDARTNR_FINANCE                                            
330000                                                                          
340000        FROM   T01DLIN_ARC                                                
350000                                                                          
360001        WHERE    KDFINDOC         = 'INT'                                 
370000        AND      IDEXCUST_1       > :WS-IDEXCUST-MIN                      
380000        AND      IDEXCUST_1       < :WS-IDEXCUST-MAX                      
390000        AND      IDARTNR_FINANCE  > ' '                                   
400000        AND      FLSOFT           = 'N'                                   
410000        AND      IDLEGSEL         = :LSEL-IDLEGSEL                        
420000        AND   (DAEXDAT    =  :WS-RUNDATUM-FROM                            
430000        OR     DAEXDAT    >  :WS-RUNDATUM-FROM)                           
440000        AND   (DAEXDAT    =  :WS-RUNDATUM-TO                              
450000        OR     DAEXDAT    <  :WS-RUNDATUM-TO)                             
460000        GROUP BY                                                          
470000                 T01DLIN_ARC.IDARTNR_FINANCE                              
480000                ,T01DLIN_ARC.IDLEGSEL                                     
490000     END-EXEC                                                             
500000                                                                          
510000     EXEC SQL                                                             
520000        OPEN CRS2                                                         
530000     END-EXEC                                                             
540000                                                                          
550000     MOVE SQLCODE        TO SQLCODE-WS                                    
560000     PERFORM DB2-STATUS-CHECK                                             
570000     .                                                                    
580000 DB2-FETCH-CRS2-DLIN-LSEL SECTION.                                        
590000     MOVE 000100         TO GOOD-SQLCODES                                 
600000                                                                          
610000     EXEC SQL                                                             
620000              FETCH CRS2                                                  
630000              INTO  :DLIN-IDLEGSEL                                        
640000                  , :WS-SULEVANT                                          
650000                  , :DLIN-IDARTNR-FINANCE                                 
660000     END-EXEC                                                             
670000                                                                          
680000     MOVE SQLCODE        TO SQLCODE-WS                                    
690000     PERFORM DB2-STATUS-CHECK                                             
700000     .                                                                    
710000 DB2-CLOSE-CRS2-DLIN-LSEL SECTION.                                        
720000                                                                          
730000     EXEC SQL                                                             
740000        CLOSE CRS2                                                        
750000     END-EXEC                                                             
760000     .                                                                    
770000 DB2-STATUS-CHECK  SECTION.                                               
780000                                                                          
790000     SET SQLCODE-IX TO 1                                                  
800000     SEARCH GOOD-SQLCODE                                                  
810000       AT END                                                             
820000          STRING 'INVALID DB2 SQL STATUS CODE: ' SQLCODE-WS               
830000          DELIMITED BY SIZE INTO ERROR-TEXT                               
840000          CALL ABEND USING RKOD-ABEND-DB2                                 
850000       WHEN GOOD-SQLCODE (SQLCODE-IX) = SQLCODE-WS CONTINUE               
860000     END-SEARCH                                                           
870000     .                                                                    
