000100 PROCESS DYNAM                                                            
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     WZ04REKY.                                                
000400 AUTHOR.         RAHUL REDDY.                                             
000500 DATE-WRITTEN.   2023/01/18.                                              
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    FUNCTION:                                                            
000900*      RETRIEVE KEYS FROM DISTRIBUTION & PRINT RESTART DB                 
001000*                                                                         
001100*      THE PROGRAM READS  TABLE TZ4REKY                                   
001200*                                                                         
001300*      CALL WZ04REKY USING WZ04REKY                                       
001400*                                                                         
001500*                                                                         
001600                                                                          
001700 DATA DIVISION.                                                           
001800                                                                          
001900 WORKING-STORAGE SECTION.                                                 
002000 77  IDPGM                       PIC X(08)   VALUE 'WZ04REKY'.            
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
003200                                                                          
003300 77  KEYS-SW                     PIC X       VALUE SPACE.                 
003400     88  KEYS-OK                             VALUE 'Y'.                   
003500     88  KEYS-WRONG                          VALUE 'N'.                   
003600                                                                          
003700*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
003800 01  GENERAL-SUBPROGRAMS.                                                 
003900     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
004000                                                                          
004100                                                                          
004200*    --- PARAMETERS TO ABEND                                              
004300 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
004400 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
004500 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
004600 77  RKOD-ABEND-DB2              PIC S9(4)   COMP VALUE +998.             
004700                                                                          
004800 01  MESSAGE-CODES.                                                       
004900     03  ERROR-CODES.                                                     
005000         05  ERR-AUTHORIZATION-MISSING PIC X(3)    VALUE '00A'.           
005100         05  ERR-INVALID-KEY           PIC X(3)    VALUE '022'.           
005200         05  ERR-SYSTEM-ERROR          PIC X(3)    VALUE '099'.           
005300     03  INFO-CODES.                                                      
005400         05  INF-MORE-LINES-EXIST      PIC X(3)    VALUE '011'.           
005500                                                                          
005600                                                                          
005700 01  FILLER                      PIC X(16)   VALUE 'SQLCA-AREA'.          
005800       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
005900                                                                          
006000 01  FILLER                      PIC X(16)   VALUE 'SQLCODE-WS'.          
006100 01  DB2-WS.                                                              
006200     03  SQLCODE-WS              PIC 9(3)    VALUE ZERO.                  
006300         88  LINES-FOUND                     VALUE 000.                   
006400         88  LINES-MISSING                   VALUE 100.                   
006500         88  RESOURCE-WRONG                  VALUE 904.                   
006600     03  GOOD-SQLCODECODES.                                               
006700         05  GOOD-SQLCODE OCCURS 5                                        
006800             INDEXED BY SQLCODE-IX PIC 9(3).                              
006900                                                                          
007000 01  FILLER                      PIC X(16)   VALUE 'TZ4REKY-AREA'.        
007100*01  -COPY TZ4REKY                                                        
007200                                                                          
007300      EXEC SQL INCLUDE TZ4REKY END-EXEC.                                  
007400                                                                          
007500 LINKAGE SECTION.                                                         
007600                                                                          
007700 COPY WZ04REKY.                                                           
007800                                                                          
007900 PROCEDURE DIVISION USING REKY-WZ04REKY.                                  
008000 MAIN SECTION.                                                            
008100                                                                          
008200     PERFORM A-INIT                                                       
008300     PERFORM B-CHECK-KEYS                                                 
008400     IF REKY-IDMSG = SPACE                                                
008500       PERFORM F-RETRIEVE-DATA                                            
008600     END-IF                                                               
008700                                                                          
008800     GOBACK                                                               
008900     .                                                                    
009000                                                                          
009100 A-INIT SECTION.                                                          
009200                                                                          
009300     INITIALIZE GOOD-SQLCODECODES                                         
009400     MOVE SPACES                 TO REKY-IDMSG                            
009500     MOVE SPACES                 TO REKY-IDELMT-ERROR                     
009600     .                                                                    
009700                                                                          
009800*** - CHECK REQUESTED KEYS AND COMPULSORY FIELDS                          
009900 B-CHECK-KEYS SECTION.                                                    
010000                                                                          
010100     MOVE YES TO KEYS-SW                                                  
010200                                                                          
010300     IF REKY-IDOUTTYPE-KEY = SPACES OR LOW-VALUES                         
010400       MOVE NOO                  TO KEYS-SW                               
010500       MOVE 'IDOUTTYPE'          TO REKY-IDELMT-ERROR                     
010600     END-IF                                                               
010700                                                                          
010800     IF REKY-IDMSG = SPACE                                                
010900       STRING FUNCTION TRIM(REKY-IDOUTREC-KEY)                            
011000              '%'      DELIMITED BY SIZE                                  
011100                               INTO REKY-IDOUTREC-KEY                     
011200     END-IF                                                               
011300                                                                          
011400     IF REKY-IDMSG = SPACE                                                
011500       STRING FUNCTION TRIM(REKY-IDLIST-KEY)                              
011600              '%'      DELIMITED BY SIZE                                  
011700                               INTO REKY-IDLIST-KEY                       
011800     END-IF                                                               
011900                                                                          
012000     IF REKY-IDMSG = SPACE                                                
012100       IF REKY-TIREGDAT-MIN-KEY NUMERIC AND                               
012200          REKY-TIREGDAT-MAX-KEY NUMERIC                                   
012300         CONTINUE                                                         
012400       ELSE                                                               
012500         MOVE NOO             TO KEYS-SW                                  
012600         MOVE 'TIREGDAT'      TO REKY-IDELMT-ERROR                        
012700       END-IF                                                             
012800     END-IF                                                               
012900                                                                          
013000     IF REKY-IDMSG = SPACE                                                
013100       IF REKY-TIKLOCK-MIN-KEY NUMERIC AND                                
013200          REKY-TIKLOCK-MAX-KEY NUMERIC                                    
013300         CONTINUE                                                         
013400       ELSE                                                               
013500         MOVE NOO             TO KEYS-SW                                  
013600         MOVE 'TIKLOCK'       TO REKY-IDELMT-ERROR                        
013700       END-IF                                                             
013800     END-IF                                                               
013900                                                                          
014000     IF REKY-IDMSG = SPACE                                                
014100       IF REKY-IDLOPNR-MIN-KEY NUMERIC AND                                
014200          REKY-IDLOPNR-MAX-KEY NUMERIC                                    
014300         CONTINUE                                                         
014400       ELSE                                                               
014500         MOVE NOO             TO KEYS-SW                                  
014600         MOVE 'IDLOPNR'       TO REKY-IDELMT-ERROR                        
014700       END-IF                                                             
014800     END-IF                                                               
014900                                                                          
015000     IF KEYS-WRONG                                                        
015100       MOVE ERR-INVALID-KEY TO REKY-IDMSG                                 
015200     END-IF                                                               
015300     .                                                                    
015400                                                                          
015500 F-RETRIEVE-DATA SECTION.                                                 
015600                                                                          
015700     MOVE ZERO                   TO REKY-KVRADER                          
015800     PERFORM DB2-DCL-OPN-TZ4REKY-CRS                                      
015900                                                                          
016000     IF LINES-FOUND                                                       
016100       PERFORM DB2-FETCH-TZ4REKY-CRS                                      
016200       IF LINES-FOUND OR LINES-MISSING                                    
016300         MOVE SQLERRD(3)         TO REKY-KVRADER                          
016400       END-IF                                                             
016500     END-IF                                                               
016600     IF REKY-KVRADER = MAX-KVRADER                                        
016700       MOVE INF-MORE-LINES-EXIST TO REKY-IDMSG                            
016800     END-IF                                                               
016900                                                                          
017000     PERFORM DB2-CLOSE-TZ4REKY-CRS                                        
017100     .                                                                    
017200                                                                          
017300*    --- DB2 SECTIONS                                                     
017400                                                                          
017500 DB2-DCL-OPN-TZ4REKY-CRS SECTION.                                         
017600                                                                          
017700                                                                          
017800     MOVE 000100                 TO GOOD-SQLCODECODES                     
017900                                                                          
018000     EXEC SQL                                                             
018100       DECLARE TZ4REKY-CRS CURSOR WITH ROWSET POSITIONING FOR             
018200                                                                          
018300       SELECT          IDOUTTYPE                                          
018400                     , IDOUTREC                                           
018500                     , IDLIST                                             
018600                     , TIREGDAT                                           
018700                     , TIKLOCK                                            
018800                     , IDLOPNR                                            
018900                                                                          
019000       FROM    TZ4REKY                                                    
019100                                                                          
019200       WHERE  IDOUTTYPE    = :REKY-IDOUTTYPE-KEY                          
019300        AND   IDOUTREC  LIKE :REKY-IDOUTREC-KEY                           
019400        AND   IDLIST    LIKE :REKY-IDLIST-KEY                             
019500        AND   TIREGDAT    >= :REKY-TIREGDAT-MIN-KEY                       
019600        AND   TIREGDAT    <= :REKY-TIREGDAT-MAX-KEY                       
019700        AND   TIKLOCK     >= :REKY-TIKLOCK-MIN-KEY                        
019800        AND   TIKLOCK     <= :REKY-TIKLOCK-MAX-KEY                        
019900        AND   IDLOPNR     >= :REKY-IDLOPNR-MIN-KEY                        
020000        AND   IDLOPNR     <= :REKY-IDLOPNR-MAX-KEY                        
020100                                                                          
020200       ORDER BY IDOUTTYPE                                                 
020300              , IDOUTREC                                                  
020400              , IDLIST                                                    
020500              , TIREGDAT                                                  
020600              , TIKLOCK                                                   
020700              , IDLOPNR                                                   
020800                                                                          
020900       FOR READ ONLY                                                      
021000     END-EXEC                                                             
021100                                                                          
021200     MOVE 000100                 TO GOOD-SQLCODECODES                     
021300                                                                          
021400     EXEC SQL                                                             
021500       OPEN TZ4REKY-CRS                                                   
021600     END-EXEC                                                             
021700                                                                          
021800     MOVE SQLCODE                TO SQLCODE-WS                            
021900     PERFORM DB2-STATUS-CHECK                                             
022000     .                                                                    
022100                                                                          
022200                                                                          
022300 DB2-FETCH-TZ4REKY-CRS SECTION.                                           
022400                                                                          
022500     MOVE 000100                 TO GOOD-SQLCODECODES                     
022600                                                                          
022700     EXEC SQL                                                             
022800                                                                          
022900       FETCH NEXT ROWSET FROM TZ4REKY-CRS FOR 20 ROWS                     
023000       INTO :REKY-IDOUTTYPE                                               
023100          , :REKY-IDOUTREC                                                
023200          , :REKY-IDLIST                                                  
023300          , :REKY-TIREGDAT                                                
023400          , :REKY-TIKLOCK                                                 
023500          , :REKY-IDLOPNR                                                 
023600                                                                          
023700     END-EXEC                                                             
023800                                                                          
023900     MOVE SQLCODE                TO SQLCODE-WS                            
024000     PERFORM DB2-STATUS-CHECK                                             
024100     .                                                                    
024200                                                                          
024300                                                                          
024400 DB2-CLOSE-TZ4REKY-CRS SECTION.                                           
024500                                                                          
024600     EXEC SQL                                                             
024700        CLOSE TZ4REKY-CRS                                                 
024800     END-EXEC                                                             
024900     .                                                                    
025000                                                                          
025100 DB2-STATUS-CHECK  SECTION.                                               
025200                                                                          
025300     SET SQLCODE-IX TO 1                                                  
025400     SEARCH GOOD-SQLCODE                                                  
025500       AT END                                                             
025600          STRING 'INVALID DB2 SQL STATUS CODE: ' SQLCODE-WS               
025700          DELIMITED BY SIZE INTO ERROR-TEXT                               
025800          CALL ABEND USING RKOD-ABEND-DB2                                 
025900       WHEN GOOD-SQLCODE (SQLCODE-IX) = SQLCODE-WS CONTINUE               
026000     END-SEARCH                                                           
026100     .                                                                    
