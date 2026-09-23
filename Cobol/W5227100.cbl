000100 PROCESS DYNAM                                                            
000201*   -THE OPTION ABOVE IS NEEDED TO LINK A BATCH-DB2-PGM                   
000301 ID DIVISION.                                                             
000401 PROGRAM-ID.     W5227100.                                                
000502 AUTHOR.         MAMATHA SHETTY.                                          
000602 DATE-WRITTEN.   24/11/11.                                                
000701 DATE-COMPILED.                                                           
000801                                                                          
000901*                                                                         
001001*    FUNCTION:                                                            
001101*        CREATES DAILY INTRASTAT FILE                                     
001201*        SELECTS DATA FROM DB2-TABLE T01IVW AND                           
001301*        CREATES A SEQUENCE-FILE FOR INT                                  
001401*                                                                         
001501*    ABENDCODES:                                                          
001601*        U0016 -  . . . .                                                 
001701*        U1000 -  . . . .                                                 
001801*                                                                         
001900                                                                          
002001     SKIP3                                                                
002101 ENVIRONMENT DIVISION.                                                    
002201     SKIP2                                                                
002301 INPUT-OUTPUT SECTION.                                                    
002401                                                                          
002501 FILE-CONTROL.                                                            
002601     SKIP2                                                                
002702*          --- SEQUENCEFILE I.N.T.                                        
002802     SELECT W52271                     ASSIGN TO W52271D1.                
002901     EJECT                                                                
003001 DATA DIVISION.                                                           
003101     SKIP3                                                                
003201 FILE SECTION.                                                            
003302 FD  W52271                                                               
003401     RECORDING       F                                                    
003501     BLOCK CONTAINS  0.                                                   
003601                                                                          
003702*01  POST    -COPY W522INT    -L.                                         
003801     EJECT                                                                
003901 WORKING-STORAGE SECTION.                                                 
004001                                                                          
004102 77  IDPGM                       PIC X(8)    VALUE 'W5227100'.            
004201*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND                
004301 77  ERROR-TEXT                  PIC X(80)   VALUE SPACE.                 
004401                                                                          
004501 77  YES                         PIC X       VALUE 'J'.                   
004601 77  NOO                         PIC X       VALUE 'N'.                   
004701                                                                          
004801 77  KEYS-SW                     PIC X       VALUE 'J'.                   
004901     88  KEYS-OK                             VALUE 'J'.                   
005001     88  KEYS-WRONG                          VALUE 'N'.                   
005101     EJECT                                                                
005201                                                                          
005301*    --- WS-AREA FOR SEQUENCEFILE                                         
005401 01  WS-TODAY                    PIC X(8)    VALUE SPACE.                 
005501 01  WS-YESTERDAY                PIC X(8)    VALUE SPACE.                 
005602 01  WS-IDPTYP                   PIC X(3)    VALUE 'INT'.                 
005701*                                                                         
005801*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
005901 01  GENERAL-SUBPROGRAMS.                                                 
006001     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
006101     03  WZ20DAYS                PIC X(8)    VALUE 'WZ20DAYS'.            
006201     SKIP3                                                                
006301*    --- PARAMETERS TO ABEND                                              
006401                                                                          
006501 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
006601 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
006701 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
006801 77  RKOD-ABEND-DB2              PIC S9(4)   COMP VALUE +998.             
006901     SKIP3                                                                
007001 01  MESSAGE-CODES.                                                       
007101     03  ERR-WRONG-KEY           PIC X(3)    VALUE '022'.                 
007201     EJECT                                                                
007301*                                                                         
007401*    --- PARAMETRAR TILL SUBPROGRAM WZ20DAYS "ADDERA DAGAR DATUM"         
007501 01  FILLER                      PIC X(16)   VALUE 'WZ20DAYS'.            
007601*01 -COPY WZ20DAYS                                                        
007701     EJECT                                                                
007801*                                                                         
007902 01  INT-AREA                    PIC X(24)   VALUE 'INT-AREA'.            
008001                                                                          
008102*01  -COPY W522INT -PRE INT-                                              
008201     EJECT                                                                
008301                                                                          
008401 01  FILLER                      PIC X(16)   VALUE 'SQLCA-AREA'.          
008501       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
008601                                                                          
008701 01  FILLER                      PIC X(16)   VALUE 'SQLCODE-WS'.          
008801 01  DB2-WS.                                                              
008901     03  SQLCODE-WS              PIC 9(3)    VALUE ZERO.                  
009001         88  CURSOR-OK                       VALUE 000.                   
009101         88  LINES-FOUND                     VALUE 000.                   
009201         88  LINES-MISSING                   VALUE 100.                   
009301         88  DOUBLE-LINES                    VALUE 811.                   
009401         88  RESOURCE-WRONG                  VALUE 904.                   
009501     03  GOOD-SQLCODECODES.                                               
009601         05  GOOD-SQLCODE OCCURS 5                                        
009701             INDEXED BY SQLCODE-IX PIC 9(3).                              
009801     EJECT                                                                
009901                                                                          
010001     EJECT                                                                
010101 01  FILLER                      PIC X(16)  VALUE 'T01IVW-AREA'.          
010201                                                                          
010301*01  -COPY T01IVW   -PRE T01IVW-                                          
010401     EJECT                                                                
010501     EXEC SQL INCLUDE T01IVW END-EXEC.                                    
010601     EJECT                                                                
010701                                                                          
010801 LINKAGE SECTION.                                                         
010901 PROCEDURE DIVISION.                                                      
011001     PERFORM A-INIT                                                       
011101     PERFORM B-PROCESS-LINES                                              
011201     PERFORM Z-FINISH                                                     
011301     MOVE ZERO TO RETURN-CODE                                             
011401     GOBACK                                                               
011501     .                                                                    
011601     EJECT                                                                
011701 A-INIT SECTION.                                                          
011801                                                                          
011902     OPEN OUTPUT W52271                                                   
012001                                                                          
012101     MOVE FUNCTION  CURRENT-DATE(1:8)  TO WS-TODAY                        
012201                                                                          
012301     MOVE WS-TODAY     TO DAYS-TIDATE1                                    
012401     MOVE 'YYYYMMDD'   TO DAYS-KDDATFMT1                                  
012501     MOVE 'YYYYMMDD'   TO DAYS-KDDATFMT2                                  
012601     MOVE SPACE        TO DAYS-TIDATE2                                    
012701                          DAYS-IDCALEND                                   
012801     MOVE -1           TO DAYS-KVDAYS                                     
012901                                                                          
013001     CALL WZ20DAYS USING DAYS-WZ20DAYS                                    
013101                                                                          
013201     MOVE DAYS-TIDATE2(1:8) TO WS-YESTERDAY                               
013301     INITIALIZE GOOD-SQLCODECODES                                         
013401     .                                                                    
013501     EJECT                                                                
013601 B-PROCESS-LINES  SECTION.                                                
013701                                                                          
013801     PERFORM DB2-DCL-OPEN-CRS                                             
013901     PERFORM DB2-FETCH-CRS                                                
014001     PERFORM UNTIL LINES-MISSING                                          
014102       MOVE T01IVW-IV-DATA TO INT-W522INT-001 OF INT-W522INT              
014103       MOVE T01IVW-IV-DATA2 TO INT-W522INT-002 OF INT-W522INT             
014202       PERFORM S11-WRITE-W52271                                           
014301       PERFORM DB2-FETCH-CRS                                              
014401     END-PERFORM                                                          
014501     PERFORM DB2-CLOSE-CRS                                                
014601     .                                                                    
014701     EJECT                                                                
014801 Z-FINISH SECTION.                                                        
014901                                                                          
015002     CLOSE W52271                                                         
015101     SKIP2                                                                
015201     .                                                                    
015301     EJECT                                                                
015402 S11-WRITE-W52271 SECTION.                                                
015501                                                                          
015602     WRITE POST FROM INT-W522INT                                          
015701     SKIP2                                                                
015801     .                                                                    
015901     EJECT                                                                
016001 DB2-DCL-OPEN-CRS SECTION.                                                
016101                                                                          
016201     MOVE 000100 TO GOOD-SQLCODECODES                                     
016301                                                                          
016401     EXEC SQL DECLARE T01IVW-CRS CURSOR FOR                               
016501         SELECT DAREGDAT                                                  
016601              , TIREGTID                                                  
016701              , IDLOPNR                                                   
016801              , IDPTYP                                                    
016901              , FLKLAR                                                    
017001              , IV_DATA                                                   
017101              , IV_DATA2                                                  
017201                                                                          
017301         FROM T01IVW                                                      
017401                                                                          
017501         WHERE DAREGDAT = :WS-YESTERDAY                                   
017601           AND IDPTYP   = :WS-IDPTYP                                      
017701                                                                          
017801     END-EXEC                                                             
017901                                                                          
018001     MOVE 000100 TO GOOD-SQLCODECODES                                     
018101                                                                          
018201     EXEC SQL                                                             
018301        OPEN T01IVW-CRS                                                   
018401     END-EXEC                                                             
018501                                                                          
018601     MOVE SQLCODE TO SQLCODE-WS                                           
018701     PERFORM DB2-STATUS-CHECK                                             
018801     .                                                                    
018901     EJECT                                                                
019001 DB2-FETCH-CRS SECTION.                                                   
019101                                                                          
019201     MOVE 000100  TO GOOD-SQLCODECODES                                    
019301     EXEC SQL                                                             
019401         FETCH T01IVW-CRS                                                 
019501         INTO   :T01IVW-DAREGDAT                                          
019601              , :T01IVW-TIREGTID                                          
019701              , :T01IVW-IDLOPNR                                           
019801              , :T01IVW-IDPTYP                                            
019901              , :T01IVW-FLKLAR                                            
020001              , :T01IVW-IV-DATA                                           
020101              , :T01IVW-IV-DATA2                                          
020201     END-EXEC                                                             
020301                                                                          
020401     MOVE SQLCODE TO SQLCODE-WS                                           
020501     PERFORM DB2-STATUS-CHECK                                             
020601     .                                                                    
020701     EJECT                                                                
020801 DB2-CLOSE-CRS SECTION.                                                   
020901                                                                          
021001     EXEC SQL                                                             
021101         CLOSE T01IVW-CRS                                                 
021201     END-EXEC                                                             
021301     .                                                                    
021401     EJECT                                                                
021501 DB2-STATUS-CHECK SECTION.                                                
021601                                                                          
021701     SET SQLCODE-IX TO 1                                                  
021801     SEARCH GOOD-SQLCODE                                                  
021900       AT END                                                             
022001          STRING 'INVALID DB2 SQL STATUS CODE: ' SQLCODE-WS               
022101          DELIMITED BY SIZE INTO ERROR-TEXT                               
022201          CALL ABEND USING RKOD-ABEND-DB2                                 
022301       WHEN GOOD-SQLCODE (SQLCODE-IX) = SQLCODE-WS                        
022401       CONTINUE                                                           
022501     END-SEARCH                                                           
022601     .                                                                    
