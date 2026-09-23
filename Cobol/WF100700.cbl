000100 PROCESS DYNAM                                                            
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     WF100700.                                                
000400 AUTHOR.         BO HAMMARIN.                                             
000500 DATE-WRITTEN.   NOV 2001.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*                                                                         
000900*  FUNKTION:                                                              
001000*                                                                         
001100*  RECEIVING ROUTINE FOR PERIOD FILE FROM SAP/R3                          
001200*                                                                         
001300*  PROGRAMMET                                                             
001400*  - READS FILE FROM SAP/R3                                               
001500*  - CREATES FILE CONTAINING CURRENCY INFO                                
001600*                                                                         
001700*        PGM READS                                                        
001800*        - DB2-TABLE T01CURY                                              
001900                                                                          
002000 ENVIRONMENT DIVISION.                                                    
002100                                                                          
002200 INPUT-OUTPUT SECTION.                                                    
002300 FILE-CONTROL.                                                            
002400                                                                          
002500*          --- CURRENCY FILE FROM SAP R/3                                 
002600     SELECT WF1012                     ASSIGN TO WF1007D1.                
002700                                                                          
002800*          --- CURRENCY INFO                                              
002900     SELECT WF1013                     ASSIGN TO WF1007D2.                
003000*          --- ERROR FILE                                                 
003100     SELECT WF1013A                    ASSIGN TO WF1007D3.                
003200     EJECT                                                                
003300                                                                          
003400 DATA DIVISION.                                                           
003500                                                                          
003600 FILE SECTION.                                                            
003700 FD  WF1012                                                               
003800     RECORDING       F                                                    
003900     BLOCK CONTAINS  0.                                                   
004000 01  IN-POST                     PIC X(46).                               
004100                                                                          
004200 FD  WF1013                                                               
004300     RECORDING       F                                                    
004400     BLOCK CONTAINS  0.                                                   
004500*01  POST -COPY WF10P002        -PRE  UT-  -L.                            
004600     EJECT                                                                
004700 FD  WF1013A                                                              
004800     RECORDING       V                                                    
004900     BLOCK CONTAINS  0.                                                   
005000 01  UT-ERR-POST                 PIC X(41).                               
005100                                                                          
005200                                                                          
005300 WORKING-STORAGE SECTION.                                                 
005400 77  ERROR-TEXT                  PIC X(80)   VALUE SPACE.                 
005500 77  IDPGM                       PIC X(8)    VALUE 'WF100700'.            
005600 77  JA                          PIC X       VALUE 'J'.                   
005700 77  NEJ                         PIC X       VALUE 'N'.                   
005800 77  WS-JFR-DASTADAT             PIC 9(8)    VALUE ZERO.                  
005900                                                                          
006000 77  WF1012-EOF-SW               PIC X       VALUE 'N'.                   
006100     88  END-OF-WF1012                       VALUE 'J'.                   
006200     EJECT                                                                
006300                                                                          
006400 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
006500 01  FILLER REDEFINES DAGENS-DATUM.                                       
006600     03  DAGENS-DATUM-AAR        PIC 9(2).                                
006700     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
006800     03  DAGENS-DATUM-DAG        PIC 9(2).                                
006900     EJECT                                                                
007000                                                                          
007100 01  WS-DASTADAT                 PIC X(8)  VALUE SPACE.                   
007200 01  FILLER REDEFINES WS-DASTADAT.                                        
007300       03  WS-DASTAYEAR          PIC 9(4).                                
007400       03  WS-DASTAMMDD          PIC 9(4).                                
007500                                                                          
007600 01  GENERAL-SUBPROGRAMS.                                                 
007700     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
007800                                                                          
007900*    --- PARAMETERS TO ABEND                                              
008000 01  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
008100 01  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
008200 01  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
008300 01  RKOD-ABEND-DB2              PIC S9(4)   VALUE +998 COMP SYNC.        
008400                                                                          
008500 01  IN-AREA-START               PIC X(24)   VALUE                        
008600                                 'IN-AREA-START  '.                       
008700*01  AREA -COPY WF10CURR -PRE IN02-                                       
008800     EJECT                                                                
008900                                                                          
009000 01  UT-AREA-START               PIC X(24)   VALUE                        
009100                                 'UT-AREA-START  '.                       
009200                                                                          
009300*01  AREA -COPY WF10P002 -PRE UT02-                                       
009400     EJECT                                                                
009500                                                                          
009600 01  WF1013A-DATA.                                                        
009700     03  WF1013A-ERR-LINE        PIC X(41) VALUE SPACES.                  
009800                                                                          
009900*    --- WORK-AREAS FOR DB2-SECTIONS                                      
010000 01  FILLER                       PIC X(16)  VALUE 'CURY-TAB   '.         
010100*01  -COPY T01CURY    -PRE CURY-                                          
010200                                                                          
010300 01  FILLER                       PIC X(16)  VALUE 'LSEL-TAB   '.         
010400*01  -COPY T01LSEL    -PRE LSEL-                                          
010500                                                                          
010600 01  FILLER                       PIC X(16)  VALUE 'CURY-AREA'.           
010700       EXEC SQL INCLUDE T01CURY  END-EXEC.                                
010800                                                                          
010900 01  FILLER                       PIC X(16)  VALUE 'LSEL-AREA'.           
011000       EXEC SQL INCLUDE T01LSEL  END-EXEC.                                
011100                                                                          
011200 01  FILLER                       PIC X(16)  VALUE 'SQLCA-AREA'.          
011300       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
011400                                                                          
011500*                        **** STATUS-CODE FROM DB2                        
011600 01  FILLER                       PIC X(16)  VALUE 'SQLCODE-WS'.          
011700 01  DB2-WS.                                                              
011800   03  SQLCODE-WS                 PIC S9(3)  VALUE ZERO.                  
011900     88  LINES-FOUND                         VALUE +000.                  
012000     88  LINES-MISSING                       VALUE +100.                  
012100     88  RESOURCE-WRONG                      VALUE 904.                   
012200   03  GOOD-SQLCODES.                                                     
012300     05  GOOD-SQLCODE OCCURS 5                                            
012400         INDEXED BY SQLCODE-IX    PIC 999.                                
012500                                                                          
012600 PROCEDURE DIVISION.                                                      
012700                                                                          
012800 MAIN SECTION.                                                            
012900     PERFORM A-INIT                                                       
013000     PERFORM S01-READ-WF1012                                              
013100                                                                          
013200     PERFORM UNTIL END-OF-WF1012                                          
013300       PERFORM B-CREATE-CURRENCY                                          
013400                                                                          
013500       PERFORM S01-READ-WF1012                                            
013600                                                                          
013700     END-PERFORM                                                          
013800                                                                          
013900     PERFORM Z-FINIT                                                      
014000     MOVE ZERO TO RETURN-CODE                                             
014100     GOBACK                                                               
014200     .                                                                    
014300     EJECT                                                                
014400                                                                          
014500 A-INIT SECTION.                                                          
014600     OPEN INPUT  WF1012                                                   
014700     OPEN OUTPUT WF1013                                                   
014800                 WF1013A                                                  
014900                                                                          
015000     ACCEPT DAGENS-DATUM  FROM DATE                                       
015100     .                                                                    
015200     EJECT                                                                
015300                                                                          
015400 B-CREATE-CURRENCY SECTION.                                               
015500     MOVE SPACE                TO UT02-WF10P002                           
015600     MOVE IN02-KDVALISO        TO UT02-KDVALISO                           
015700     MOVE IN02-IDLEGSEL        TO UT02-IDLEGSEL                           
015800                                                                          
015900     IF IN02-DASTADAT < WS-JFR-DASTADAT                                   
016000       MOVE WS-JFR-DASTADAT    TO UT02-DASTADAT                           
016100     ELSE                                                                 
016200       MOVE IN02-DASTADAT      TO UT02-DASTADAT                           
016300                             WS-JFR-DASTADAT                              
016400     END-IF                                                               
016500     IF IN02-DAREGDAT < UT02-DASTADAT                                     
016600       MOVE IN02-DAREGDAT      TO UT02-DAREGDAT                           
016700                                                                          
016800**** CONTROL IF THE MONTLY CURRENCY SHOULD BE CHANGED TO YEARLY           
016900       MOVE UT02-DASTADAT(1:4) TO WS-DASTAYEAR                            
017000       MOVE 0101               TO WS-DASTAMMDD                            
017100       PERFORM DB2-SELECT-T01CURY                                         
017200                                                                          
017300       IF LINES-FOUND                                                     
017400         COMPUTE CURY-PRKURS   = CURY-PRKURS  / CURY-REVALUTA             
017500         COMPUTE CURY-REVALUTA = CURY-REVALUTA / CURY-REVALUTA            
017600         MOVE CURY-REVALUTA    TO UT02-REVALUTA-FROM                      
017610         MOVE CURY-REVALUTA    TO UT02-REVALUTA                           
017700         MOVE CURY-PRKURS      TO UT02-PRKURS                             
017800         MOVE 1                TO UT02-REVALUTA-TO                        
017900       ELSE                                                               
017910         MOVE 1                     TO UT02-REVALUTA                      
018000         IF IN02-REVALUTA-FROM > 1                                        
018100            COMPUTE IN02-PRKURS =                                         
018200                 IN02-PRKURS / IN02-REVALUTA-FROM                         
018300         END-IF                                                           
018400         IF IN02-REVALUTA-TO > 1                                          
018500            COMPUTE IN02-PRKURS =                                         
018600                 IN02-PRKURS * IN02-REVALUTA-TO                           
018700         END-IF                                                           
018800         IF IN02-REVALUTA-FROM > 1                                        
018900            MOVE 1                  TO UT02-REVALUTA-FROM                 
019000         ELSE                                                             
019100            MOVE IN02-REVALUTA-FROM TO UT02-REVALUTA-FROM                 
019200         END-IF                                                           
019300         IF IN02-REVALUTA-TO > 1                                          
019400            MOVE 1                  TO UT02-REVALUTA-TO                   
019500         ELSE                                                             
019600            MOVE IN02-REVALUTA-TO   TO UT02-REVALUTA-TO                   
019700         END-IF                                                           
019800         MOVE IN02-PRKURS           TO UT02-PRKURS                        
019900       END-IF                                                             
020000       PERFORM S11-WRITE-WF1013                                           
020100     ELSE                                                                 
020200       MOVE IN02-AREA          TO WF1013A-ERR-LINE                        
020300       WRITE UT-ERR-POST     FROM WF1013A-ERR-LINE                        
020400     END-IF                                                               
020500     .                                                                    
020600     EJECT                                                                
020700                                                                          
020800 Z-FINIT SECTION.                                                         
020900     CLOSE WF1012                                                         
021000           WF1013                                                         
021100           WF1013A                                                        
021200     .                                                                    
021300     EJECT                                                                
021400                                                                          
021500 S01-READ-WF1012  SECTION.                                                
021600     READ WF1012          INTO IN02-AREA                                  
021700     AT END                                                               
021800        MOVE HIGH-VALUE   TO IN02-AREA                                    
021900        SET END-OF-WF1012 TO TRUE                                         
022000     END-READ                                                             
022100     .                                                                    
022200     EJECT                                                                
022300                                                                          
022400 S11-WRITE-WF1013 SECTION.                                                
022500     WRITE UT-POST   FROM UT02-AREA                                       
022600     .                                                                    
022700                                                                          
022800 DB2-SELECT-T01CURY SECTION.                                              
022900     MOVE 000100305      TO GOOD-SQLCODES                                 
023000                                                                          
023100     EXEC SQL                                                             
023200     SELECT   PRKURS                                                      
023300             ,REVALUTA                                                    
023400                                                                          
023500     INTO     :CURY-PRKURS                                                
023600             ,:CURY-REVALUTA                                              
023700                                                                          
023800     FROM     T01CURY                                                     
023900                                                                          
024000     WHERE    IDLEGSEL = :UT02-IDLEGSEL                                   
024100     AND      KDVALISO = :UT02-KDVALISO                                   
024200     AND      DASTADAT LIKE :WS-DASTADAT                                  
024300     AND      DADELDAT = '00000000'                                       
024400     END-EXEC                                                             
024500                                                                          
024600     MOVE SQLCODE        TO SQLCODE-WS                                    
024700     PERFORM DB2-STATUS-CHECK                                             
024800     .                                                                    
024900                                                                          
025000 DB2-STATUS-CHECK  SECTION.                                               
025100     SET SQLCODE-IX TO 1                                                  
025200     SEARCH GOOD-SQLCODE                                                  
025300       AT END                                                             
025400          STRING 'INVALID DB2 SQL STATUS CODE: ' SQLCODE-WS               
025500          DELIMITED BY SIZE INTO ERROR-TEXT                               
025600          CALL ABEND USING RKOD-ABEND-DB2                                 
025700       WHEN GOOD-SQLCODE (SQLCODE-IX) = SQLCODE-WS                        
025800          CONTINUE                                                        
025900     END-SEARCH                                                           
026000     .                                                                    
