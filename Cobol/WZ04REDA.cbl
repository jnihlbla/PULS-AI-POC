000100 PROCESS DYNAM                                                            
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     WZ04REDA.                                                
000400 AUTHOR.         RAHUL REDDY.                                             
000500 DATE-WRITTEN.   2023/01/18.                                              
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    FUNCTION:                                                            
000900*      RETRIEVE DATA FROM DISTRIBUTION & PRINT RESTART DB                 
001000*                                                                         
001100*      THE PROGRAM READS  TABLE TZ4REDA                                   
001200*                                                                         
001300*      CALL WZ04REDA USING WZ04REDA                                       
001400*                                                                         
001500*                                                                         
001600                                                                          
001700 DATA DIVISION.                                                           
001800                                                                          
001900 WORKING-STORAGE SECTION.                                                 
002000 77  IDPGM                       PIC X(08)   VALUE 'WZ04REDA'.            
002100                                                                          
002200*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND.               
002300 77  ERROR-TEXT                  PIC X(80)  VALUE SPACE.                  
002400 77  KDRC-DISPLAY                PIC Z(5).                                
002500                                                                          
002600*    --- CONSTANT WORK FIELDS                                             
002700 77  JA                          PIC X       VALUE 'J'.                   
002800 77  YES                         PIC X       VALUE 'Y'.                   
002900 77  NOO                         PIC X       VALUE 'N'.                   
003000*                                                                         
003100 77  MAX-KVRADER                 PIC S9(3) COMP-3 VALUE +20.              
003200 01  WORK-KVPOST.                                                         
003300     03  WS-KVPOST               PIC S9(7) COMP-3 OCCURS 20 TIMES.        
003400                                                                          
003500 77  KEYS-SW                     PIC X       VALUE SPACE.                 
003600     88  KEYS-OK                             VALUE 'Y'.                   
003700     88  KEYS-WRONG                          VALUE 'N'.                   
003800                                                                          
003900                                                                          
004000*    --- WORK-FIELDS                                                      
004100 01  WS-REKY-IDLOPNR             PIC S9(3)   VALUE ZERO COMP-3.           
004200 01  WS-REDA-IDLOPNR             PIC S9(3)   VALUE ZERO COMP-3.           
004300 01  WS-KVPOST-LAST              PIC S9(7)   VALUE ZERO COMP-3.           
004400 01  WS-KVRADER                  PIC S9(5)   VALUE ZERO COMP-3.           
004500                                                                          
004600*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
004700 01  GENERAL-SUBPROGRAMS.                                                 
004800     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
004900                                                                          
005000                                                                          
005100*    --- PARAMETERS TO ABEND                                              
005200 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
005300 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
005400 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
005500 77  RKOD-ABEND-DB2              PIC S9(4)   COMP VALUE +998.             
005600                                                                          
005700 01  MESSAGE-CODES.                                                       
005800     03  ERROR-CODES.                                                     
005900         05  ERR-AUTHORIZATION-MISSING PIC X(3)    VALUE '00A'.           
006000         05  ERR-INVALID-KEY           PIC X(3)    VALUE '022'.           
006100         05  ERR-SYSTEM-ERROR          PIC X(3)    VALUE '099'.           
006200     03  INFO-CODES.                                                      
006300         05  INF-MORE-LINES-EXIST      PIC X(3)    VALUE '011'.           
006400                                                                          
006500                                                                          
006600 01  FILLER                      PIC X(16)   VALUE 'SQLCA-AREA'.          
006700       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
006800                                                                          
006900 01  FILLER                      PIC X(16)   VALUE 'SQLCODE-WS'.          
007000 01  DB2-WS.                                                              
007100     03  SQLCODE-WS              PIC 9(3)    VALUE ZERO.                  
007200         88  LINES-FOUND                     VALUE 000.                   
007300         88  LINES-MISSING                   VALUE 100.                   
007400         88  RESOURCE-WRONG                  VALUE 904.                   
007500     03  GOOD-SQLCODECODES.                                               
007600         05  GOOD-SQLCODE OCCURS 5                                        
007700             INDEXED BY SQLCODE-IX PIC 9(3).                              
007800                                                                          
007900 01  FILLER                      PIC X(16)   VALUE 'TZ4REDA-AREA'.        
008000*01  -COPY TZ4REDA                                                        
008100                                                                          
008200      EXEC SQL INCLUDE TZ4REDA END-EXEC.                                  
008300                                                                          
008400 LINKAGE SECTION.                                                         
008500                                                                          
008600 COPY WZ04REDA.                                                           
008700                                                                          
008800 PROCEDURE DIVISION USING REDA-WZ04REDA.                                  
008900 MAIN SECTION.                                                            
009000                                                                          
009100     PERFORM A-INIT                                                       
009200     PERFORM B-CHECK-KEYS                                                 
009300     IF REDA-IDMSG = SPACE                                                
009400       PERFORM F-RETRIEVE-DATA                                            
009500     END-IF                                                               
009600                                                                          
009700     GOBACK                                                               
009800     .                                                                    
009900                                                                          
010000 A-INIT SECTION.                                                          
010100                                                                          
010200     INITIALIZE GOOD-SQLCODECODES                                         
010300     MOVE SPACES                 TO REDA-IDMSG                            
010400     MOVE SPACES                 TO REDA-IDELMT-ERROR                     
010500     .                                                                    
010600                                                                          
010700*** - CHECK REQUESTED KEYS AND COMPULSORY FIELDS                          
010800 B-CHECK-KEYS SECTION.                                                    
010900                                                                          
011000     MOVE YES TO KEYS-SW                                                  
011100                                                                          
011200     IF REDA-IDOUTTYPE-KEY = SPACES OR LOW-VALUES                         
011300       MOVE NOO                  TO KEYS-SW                               
011400       MOVE 'IDOUTTYPE'          TO REDA-IDELMT-ERROR                     
011500     END-IF                                                               
011600                                                                          
011700     IF REDA-IDMSG = SPACE                                                
011800       IF REDA-IDOUTREC-KEY = SPACES OR LOW-VALUES                        
011900         MOVE NOO             TO KEYS-SW                                  
012000         MOVE 'IDOUTREC'      TO REDA-IDELMT-ERROR                        
012100       END-IF                                                             
012200     END-IF                                                               
012300                                                                          
012400     IF REDA-IDMSG = SPACE                                                
012500       IF REDA-IDLIST-KEY = SPACES OR LOW-VALUES                          
012600         MOVE NOO             TO KEYS-SW                                  
012700         MOVE 'IDLIST'        TO REDA-IDELMT-ERROR                        
012800       END-IF                                                             
012900     END-IF                                                               
013000                                                                          
013100     IF REDA-IDMSG = SPACE                                                
013200       IF REDA-TIREGDAT-KEY NUMERIC                                       
013300         CONTINUE                                                         
013400       ELSE                                                               
013500         MOVE NOO             TO KEYS-SW                                  
013600         MOVE 'TIREGDAT'      TO REDA-IDELMT-ERROR                        
013700       END-IF                                                             
013800     END-IF                                                               
013900                                                                          
014000     IF REDA-IDMSG = SPACE                                                
014100       IF  REDA-TIKLOCK-KEY NUMERIC                                       
014200         CONTINUE                                                         
014300       ELSE                                                               
014400         MOVE NOO             TO KEYS-SW                                  
014500         MOVE 'TIKLOCK'       TO REDA-IDELMT-ERROR                        
014600       END-IF                                                             
014700     END-IF                                                               
014800                                                                          
014900     IF REDA-IDMSG = SPACE                                                
015000       IF REDA-IDLOPNR-KEY NUMERIC                                        
015100         PERFORM BA-GET-TZ4REDA-IDLOPNR                                   
015200       ELSE                                                               
015300         MOVE NOO             TO KEYS-SW                                  
015400         MOVE 'IDLOPNR'       TO REDA-IDELMT-ERROR                        
015500       END-IF                                                             
015600     END-IF                                                               
015700                                                                          
015800     IF REDA-IDMSG = SPACE                                                
015900       IF REDA-KVPOST-LAST NUMERIC                                        
016000         MOVE REDA-KVPOST-LAST                                            
016100                              TO WS-KVPOST-LAST                           
016200       ELSE                                                               
016300         MOVE NOO             TO KEYS-SW                                  
016400         MOVE 'KVPOST'        TO REDA-IDELMT-ERROR                        
016500       END-IF                                                             
016600     END-IF                                                               
016700                                                                          
016800     IF KEYS-WRONG                                                        
016900       MOVE ERR-INVALID-KEY TO REDA-IDMSG                                 
017000     END-IF                                                               
017100                                                                          
017200     .                                                                    
017300                                                                          
017400 BA-GET-TZ4REDA-IDLOPNR SECTION.                                          
017500                                                                          
017600*    -- REDA-IDLOPNR ORIGINATES FROM REKY TABLE, BUT                      
017700*    -- REKY AND REDA USE DIFFERENT VALUES OF IDLOPNR.                    
017800*    -- REKY USE THE SPECIFIELD VALUE, WHICH MAY BE > 100                 
017900*    -- BUT REDA ALWAYS USE A VALUE < 100.                                
018000*    -- IF REKY-IDLOPNR IS 000, 100, 200 ETC, REDA-IDLOPNR = 000          
018100*    -- IF REKY-IDLOPNR IS 001, 101, 201 ETC, REDA-IDLOPNR = 001          
018200*    -- AND SO ON.                                                        
018300     MOVE REDA-IDLOPNR-KEY    TO WS-REKY-IDLOPNR                          
018400     IF REDA-TIREGDAT-KEY < 151012                                        
018500       COMPUTE WS-REDA-IDLOPNR =                                          
018600             FUNCTION REM (WS-REKY-IDLOPNR, 100)                          
018700     ELSE                                                                 
018800       COMPUTE WS-REDA-IDLOPNR =                                          
018900             FUNCTION REM (WS-REKY-IDLOPNR, 50)                           
019000     END-IF                                                               
019100                                                                          
019200     .                                                                    
019300                                                                          
019400 F-RETRIEVE-DATA SECTION.                                                 
019500                                                                          
019600     MOVE ZERO                   TO REDA-KVRADER                          
019700     PERFORM DB2-DCL-OPN-TZ4REDA-CRS                                      
019800                                                                          
019900     IF LINES-FOUND                                                       
020000       PERFORM DB2-FETCH-TZ4REDA-CRS                                      
020100       IF LINES-FOUND OR LINES-MISSING                                    
020200         MOVE SQLERRD(3)         TO REDA-KVRADER                          
020300         IF REDA-KVRADER > 0                                              
020400           MOVE WS-KVPOST (REDA-KVRADER)                                  
020500                                 TO REDA-KVPOST-LAST                      
020600                                    WS-KVPOST-LAST                        
020700         END-IF                                                           
020800       END-IF                                                             
020900     END-IF                                                               
021000     IF REDA-KVRADER = MAX-KVRADER                                        
021100       MOVE INF-MORE-LINES-EXIST TO REDA-IDMSG                            
021200     END-IF                                                               
021300                                                                          
021400     PERFORM DB2-CLOSE-TZ4REDA-CRS                                        
021500                                                                          
021600     .                                                                    
021700                                                                          
021800*    --- DB2 SECTIONS                                                     
021900                                                                          
022000 DB2-DCL-OPN-TZ4REDA-CRS SECTION.                                         
022100                                                                          
022200                                                                          
022300     MOVE 000100                 TO GOOD-SQLCODECODES                     
022400                                                                          
022500     EXEC SQL                                                             
022600       DECLARE TZ4REDA-CRS CURSOR WITH ROWSET POSITIONING FOR             
022700                                                                          
022800       SELECT KVPOST                                                      
022900            , TEOUTDATA                                                   
023000                                                                          
023100       FROM    TZ4REDA                                                    
023200                                                                          
023300       WHERE  IDOUTTYPE = :REDA-IDOUTTYPE-KEY                             
023400        AND   IDOUTREC  = :REDA-IDOUTREC-KEY                              
023500        AND   IDLIST    = :REDA-IDLIST-KEY                                
023600        AND   TIREGDAT  = :REDA-TIREGDAT-KEY                              
023700        AND   TIKLOCK   = :REDA-TIKLOCK-KEY                               
023800        AND   IDLOPNR   = :WS-REDA-IDLOPNR                                
023900        AND   KVPOST    > :WS-KVPOST-LAST                                 
024000                                                                          
024100       ORDER BY IDOUTTYPE                                                 
024200              , IDOUTREC                                                  
024300              , IDLIST                                                    
024400              , TIREGDAT                                                  
024500              , TIKLOCK                                                   
024600              , IDLOPNR                                                   
024700              , KVPOST                                                    
024800                                                                          
024900       FOR READ ONLY                                                      
025000     END-EXEC                                                             
025100                                                                          
025200     MOVE 000100                 TO GOOD-SQLCODECODES                     
025300                                                                          
025400     EXEC SQL                                                             
025500       OPEN TZ4REDA-CRS                                                   
025600     END-EXEC                                                             
025700                                                                          
025800     MOVE SQLCODE                TO SQLCODE-WS                            
025900     PERFORM DB2-STATUS-CHECK                                             
026000     .                                                                    
026100                                                                          
026200                                                                          
026300 DB2-FETCH-TZ4REDA-CRS SECTION.                                           
026400                                                                          
026500     MOVE 000100                 TO GOOD-SQLCODECODES                     
026600                                                                          
026700     EXEC SQL                                                             
026800                                                                          
026900       FETCH NEXT ROWSET FROM TZ4REDA-CRS FOR 20 ROWS                     
027000       INTO :WS-KVPOST                                                    
027100          , :REDA-OUTDATA-GROUP                                           
027200                                                                          
027300     END-EXEC                                                             
027400                                                                          
027500     MOVE SQLCODE                TO SQLCODE-WS                            
027600     PERFORM DB2-STATUS-CHECK                                             
027700     .                                                                    
027800                                                                          
027900                                                                          
028000 DB2-CLOSE-TZ4REDA-CRS SECTION.                                           
028100                                                                          
028200     EXEC SQL                                                             
028300        CLOSE TZ4REDA-CRS                                                 
028400     END-EXEC                                                             
028500     .                                                                    
028600                                                                          
028700 DB2-STATUS-CHECK  SECTION.                                               
028800                                                                          
028900     SET SQLCODE-IX TO 1                                                  
029000     SEARCH GOOD-SQLCODE                                                  
029100       AT END                                                             
029200          STRING 'INVALID DB2 SQL STATUS CODE: ' SQLCODE-WS               
029300          DELIMITED BY SIZE INTO ERROR-TEXT                               
029400          CALL ABEND USING RKOD-ABEND-DB2                                 
029500       WHEN GOOD-SQLCODE (SQLCODE-IX) = SQLCODE-WS CONTINUE               
029600     END-SEARCH                                                           
029700     .                                                                    
