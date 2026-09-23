000100 PROCESS DYNAM                                                            
000200*        - THE OPTION ABOVE IS NEEDED TO LINK A BATCH-DB2-PGM             
000300*                                                                         
000400 ID DIVISION.                                                             
000500 PROGRAM-ID.     WF108100.                                                
000600 AUTHOR.         ANDERS HENRIKSSON                                        
000700 DATE-WRITTEN.   07/06/19.                                                
000800 DATE-COMPILED.                                                           
000900                                                                          
001000                                                                          
001100*    FUNCTION:                                                            
001200*        PGM READS   ROWS IN TABLE  T01PDEV                               
001300*                                                                         
001400*        PGM TAKES CURRENT MONTH MINUS NUMBER OF MONTH THE                
001500*        DATA WILL BE KEPT (KVMANAD FROM T01PDEV) AND CREATES             
001600*        AN OUTPUT FILE FOR DELETE OF ROWS IN                             
001700*        - TABLE T01SDEV                                                  
002400*                                                                         
002500                                                                          
002600     SKIP3                                                                
002700 ENVIRONMENT DIVISION.                                                    
002800 INPUT-OUTPUT SECTION.                                                    
002900 FILE-CONTROL.                                                            
003000     SELECT WF10FIL2                   ASSIGN TO WF1081D1.                
003100 DATA DIVISION.                                                           
003200 FILE SECTION.                                                            
003300                                                                          
003400 FD  WF10FIL2                                                             
003500     RECORDING       F                                                    
003600     BLOCK CONTAINS  0.                                                   
003700                                                                          
003800 01  WF10POST                    PIC X(80).                               
003900 WORKING-STORAGE SECTION.                                                 
004000                                                                          
004100 77  IDPGM                       PIC X(8)    VALUE 'WF108100'.            
004200                                                                          
004300 77  YES                         PIC X       VALUE 'J'.                   
004400 77  NOO                         PIC X       VALUE 'N'.                   
004500     EJECT                                                                
004600                                                                          
004700 77  KEYS-SW                     PIC X      VALUE SPACE.                  
004800     88  KEYS-OK                            VALUE 'J'.                    
004900     88  KEYS-ERROR                         VALUE 'N'.                    
005000                                                                          
005100 01  WS-IDREFRAD            PIC S9(5) COMP-3 VALUE ZERO.                  
005200                                                                          
005300 01  ERRORTEXT.                                                           
005400     03  FILLER                  PIC X(9)    VALUE 'ERRORTEXT'.           
005500     03  ERRORTEXT-STR           PIC X(72)   VALUE SPACE.                 
005600     EJECT                                                                
005700                                                                          
005800*    --- SUBPROGRAMS OCH PARAMETER AREAS.                                 
005900 01  GENERAL-SUBPROGRAMS.                                                 
006000     03  ABEND                   PIC X(8)   VALUE 'ABEND   '.             
006100     EJECT                                                                
006200                                                                          
006300*    --- PARAMETERS FOR SUBPROGRAM ABEND                                  
006400 77  RKOD-ABEND                  PIC S9(4)  COMP VALUE +0.                
006500 77  RKOD-ABEND-NO-DUMP          PIC S9(4)  COMP VALUE +16.               
006600 77  RKOD-ABEND-DB2              PIC S9(4)  COMP VALUE +998.              
006700 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)  COMP VALUE +1000.             
006800     SKIP2                                                                
006900                                                                          
007000 01  MESSAGE-CODES.                                                       
007100     03  ERR-WRONG-KEY           PIC X(3)    VALUE '022'.                 
007200     EJECT                                                                
007300                                                                          
007400 01  WZ20DAYS PIC X(8) VALUE 'WZ20DAYS'.                                  
007500     SKIP3                                                                
007600*    -COPY WZ20DAYS                                                       
007700     EJECT                                                                
007800                                                                          
007900 01  FILLER                      PIC X(16)   VALUE 'SQLCA-AREA'.          
008000       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
008100                                                                          
008200 01  FILLER                      PIC X(16)   VALUE 'SQLCODE-WS'.          
008300 01  DB2-WS.                                                              
008400     03  SQLCODE-WS              PIC 9(3)    VALUE ZERO.                  
008500         88  CURSOR-OK                       VALUE 000.                   
008600         88  LINES-FOUND                     VALUE 000.                   
008700         88  LINES-MISSING                   VALUE 100.                   
008800         88  RESOURCE-WRONG                  VALUE 904.                   
008900                                                                          
009000     03  T01SYST-WS              PIC 9(3)   VALUE ZERO.                   
009100        88 T01SYST-OK                       VALUE 000.                    
009200        88 T01SYST-MISSING                  VALUE 100.                    
009300        88 T01SYST-ERROR                    VALUE 904.                    
009400                                                                          
009500     03  GOOD-SQLCODECODES.                                               
009600         05  GOOD-SQLCODE OCCURS 5                                        
009700             INDEXED BY SQLCODE-IX PIC 9(3).                              
009800                                                                          
009900 01  FILLER                    PIC X(16) VALUE 'WS-AREA         '.        
010000 01  WS-AREA.                                                             
010100     03 WS-FILLER1             PIC X(8)  VALUE SPACE.                     
010200     03 WS-DAEXDAT             PIC X(4) VALUE " < '".                     
010300     03 WS-DELDATUM            PIC X(8)  VALUE SPACE.                     
010400     03 WS-FILLER2             PIC X(60) VALUE "')".                      
010500                                                                          
010600 01  WS-CURRENT-DATE           PIC X(8).                                  
010700 01  WS-KVDAGAR                PIC S9(3) VALUE ZERO COMP-3.               
010800                                                                          
010900     EJECT                                                                
011000                                                                          
011100 01  FILLER                    PIC X(16)    VALUE 'T01PDEV-AREA'.         
011200*01  -COPY T01PDEV -PRE PDEV-                                             
011300     EJECT                                                                
011400                                                                          
011500     EJECT                                                                
011600                                                                          
011700     EXEC SQL INCLUDE T01PDEV END-EXEC.                                   
011800     EJECT                                                                
011900                                                                          
012000 PROCEDURE DIVISION.                                                      
012100 MAIN SECTION.                                                            
012200     PERFORM A-INIT                                                       
012300                                                                          
012400     WRITE WF10POST FROM WS-AREA                                          
012500     CLOSE WF10FIL2                                                       
012600     MOVE ZERO TO RETURN-CODE                                             
012700     GOBACK                                                               
012800     .                                                                    
012900     EJECT                                                                
013000                                                                          
013100 A-INIT SECTION.                                                          
013200     OPEN OUTPUT WF10FIL2                                                 
013300                                                                          
013400     PERFORM DB2-SELECT-T01PDEV                                           
013500                                                                          
013510     COMPUTE WS-KVDAGAR = (PDEV-KVMANAD * 28)                             
013600     MOVE YES TO KEYS-SW                                                  
013700     MOVE FUNCTION CURRENT-DATE (1:8) TO WS-CURRENT-DATE                  
013800     MOVE WS-CURRENT-DATE (1:8)       TO DAYS-TIDATE2                     
013900     MOVE 'YYYYMMDD'                  TO DAYS-KDDATFMT2                   
014000     MOVE WS-KVDAGAR                  TO DAYS-KVDAYS                      
014100     MOVE 'WEEKDAYS'                  TO DAYS-IDCALEND                    
014200     MOVE SPACE                       TO DAYS-TIDATE1                     
014300     MOVE 'YYYYMMDD'                  TO DAYS-KDDATFMT1                   
014400     CALL WZ20DAYS USING                                                  
014500          DAYS-WZ20DAYS                                                   
014600     IF DAYS-KDRC = ZERO                                                  
014700       MOVE DAYS-TIDATE1(1:6)         TO WS-DELDATUM(1:6)                 
014710       MOVE '01'                      TO WS-DELDATUM(7:2)                 
014800     ELSE                                                                 
014900       DISPLAY ' ERROR IN WZ20DAYS ' DAYS-KDRC                            
015000     END-IF                                                               
015100     .                                                                    
015200     EJECT                                                                
015300                                                                          
015400     EJECT                                                                
015500                                                                          
015600 DB2-SELECT-T01PDEV SECTION.                                              
015700     MOVE 000     TO GOOD-SQLCODECODES                                    
015800     EXEC SQL                                                             
015900         SELECT KVMANAD                                                   
016000                                                                          
016100         INTO :PDEV-KVMANAD                                               
016200                                                                          
016300         FROM    T01PDEV                                                  
016400                                                                          
016500         WHERE   IDLEGSEL = 'VCCS'                                        
016510         AND     KDBEHX   = 'S'                                           
016600     END-EXEC                                                             
016700     MOVE SQLCODE TO SQLCODE-WS                                           
016800                     T01SYST-WS                                           
016900     PERFORM DB2-STATUS-CHECK                                             
017000     .                                                                    
017100     EJECT                                                                
017200                                                                          
017300                                                                          
017400 DB2-STATUS-CHECK     SECTION.                                            
017500     SET SQLCODE-IX TO 1                                                  
017600     SEARCH GOOD-SQLCODE                                                  
017700       AT END                                                             
017800          STRING 'INVALID DB2 SQL STATUS CODE: ' SQLCODE-WS               
017900          DELIMITED BY SIZE INTO ERRORTEXT                                
018000          CALL ABEND USING RKOD-ABEND-DB2                                 
018100       WHEN GOOD-SQLCODE (SQLCODE-IX) = SQLCODE-WS CONTINUE               
018200     END-SEARCH                                                           
018300     .                                                                    
