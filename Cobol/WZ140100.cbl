000100 PROCESS DYNAM                                                            
000200*        - THE OPTION ABOVE IS NEEDED TO LINK A BATCH-DB2-PGM             
000300*                                                                         
000400 ID DIVISION.                                                             
000500 PROGRAM-ID.     WZ140100.                                                
000600 AUTHOR.         LUNDH BERNT.                                             
000700 DATE-WRITTEN.   02/09/27.                                                
000800 DATE-COMPILED.                                                           
000900                                                                          
001000*    FUNCTION:                                                            
001100*        THE PROGRAM DELETE OLD ROWS IN D&P RESTARTING-TABLES             
001200*                                                                         
001300*        - TZ4REKY (DELETE)                                               
001400*        - TZ4REDA (DELETE)                                               
001500                                                                          
001600 ENVIRONMENT DIVISION.                                                    
001700 INPUT-OUTPUT SECTION.                                                    
001800 FILE-CONTROL.                                                            
001900 DATA DIVISION.                                                           
002000 FILE SECTION.                                                            
002100 WORKING-STORAGE SECTION.                                                 
002200                                                                          
002300 77  IDPGM                       PIC X(8)   VALUE 'WZ140100'.             
002400                                                                          
002500*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND.               
002600 77  KDRC-DISPLAY                PIC Z(5).                                
002700     EJECT                                                                
002800                                                                          
002900 01  ERROR-TEXT.                                                          
003000     03  FILLER                  PIC X(10)  VALUE 'ERROR-TEXT'.           
003100     03  ERROR-TEXT-STR          PIC X(72)  VALUE SPACE.                  
003200                                                                          
003300 01  WS-COMMITTED                PIC X      VALUE 'N'.                    
003400 01  WS-CURRENT-DATE             PIC S9(7)  VALUE ZERO COMP-3.            
003500 01  W-COMMIT-MAX                PIC S9(9)  VALUE 100  COMP-3.            
003600 01  W-COUNTC                    PIC S9(9)  VALUE ZERO COMP-3.            
003700 01  W-COUNT1                    PIC S9(9)  VALUE ZERO COMP-3.            
003800 01  W-COUNT2                    PIC S9(9)  VALUE ZERO COMP-3.            
003900 01  W-COUNT3                    PIC S9(9)  VALUE ZERO COMP-3.            
004000                                                                          
004100*    --- SUBPROGRAMS OCH PARAMETER AREAS                                  
004200 01  GENERAL-SUBPROGRAMS.                                                 
004300     03  ABEND                   PIC X(8)   VALUE 'ABEND   '.             
004400     EJECT                                                                
004500                                                                          
004600*    --- PARAMETERS FOR SUBPROGRAM ABEND                                  
004700 77  RKOD-ABEND                  PIC S9(4)  COMP VALUE +0.                
004800 77  RKOD-ABEND-NO-DUMP          PIC S9(4)  COMP VALUE +16.               
004900 77  RKOD-ABEND-DB2              PIC S9(4)  COMP VALUE +998.              
005000 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)  COMP VALUE +1000.             
005100     SKIP2                                                                
005200                                                                          
005300 01  FILLER                      PIC X(16)  VALUE 'SQLCA-AREA'.           
005400       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
005500                                                                          
005600 01  FILLER                      PIC X(16)  VALUE 'SQLCODE-WS'.           
005700 01  DB2-WS.                                                              
005800     03  SQLCODE-WS              PIC 9(3)   VALUE ZERO.                   
005900         88  CURSOR-OK                      VALUE 000.                    
006000         88  LINES-FOUND                    VALUE 000.                    
006100         88  LINES-MISSING                  VALUE 100.                    
006200         88  RESOURCE-WRONG                 VALUE 904.                    
006300                                                                          
006400     03  GOOD-SQLCODECODES.                                               
006500         05  GOOD-SQLCODE OCCURS 5                                        
006600             INDEXED BY SQLCODE-IX PIC 9(3).                              
006700                                                                          
006800     EJECT                                                                
006900                                                                          
007000 01  FILLER                      PIC X(16)  VALUE 'TZ4REKY-AREA'.         
007100*01  -COPY TZ4REKY -PRE REKY-                                             
007200                                                                          
007300 01  FILLER                      PIC X(16)  VALUE 'TZ4REDA-AREA'.         
007400*01  -COPY TZ4REDA -PRE REDA-                                             
007500     EJECT                                                                
007600                                                                          
007700     EXEC SQL INCLUDE TZ4REKY END-EXEC.                                   
007800                                                                          
007900     EXEC SQL INCLUDE TZ4REDA END-EXEC.                                   
008000                                                                          
008100     EJECT                                                                
008200                                                                          
008300 PROCEDURE DIVISION.                                                      
008400 MAIN SECTION.                                                            
008500                                                                          
008600     PERFORM A-INIT                                                       
008700     PERFORM B-EXECUTE                                                    
008800                                                                          
008900     DISPLAY 'ANTAL COMMIT ' W-COUNT1                                     
009000     DISPLAY 'DELETE REKY  ' W-COUNT2                                     
009100     DISPLAY 'DELETE REDA  ' W-COUNT3                                     
009200     MOVE ZERO TO RETURN-CODE                                             
009300     GOBACK                                                               
009400     .                                                                    
009500 A-INIT SECTION.                                                          
009600                                                                          
009700     INITIALIZE GOOD-SQLCODECODES                                         
009800     MOVE FUNCTION CURRENT-DATE (3:6) TO WS-CURRENT-DATE                  
009900     .                                                                    
010000 B-EXECUTE SECTION.                                                       
010100                                                                          
010200     PERFORM DB2-DCL-OPN-TZ4REKY-CRS                                      
010300     PERFORM DB2-FETCH-TZ4REKY-CRS                                        
010400                                                                          
010500     PERFORM UNTIL LINES-MISSING                                          
010600                                                                          
010700       MOVE 'N'                  TO WS-COMMITTED                          
010800       PERFORM DB2-DCL-OPN-TZ4REDA-CRS                                    
010900       PERFORM DB2-FETCH-TZ4REDA-CRS                                      
011000                                                                          
011100       PERFORM UNTIL LINES-MISSING                                        
011200         PERFORM DB2-DELETE-TZ4REDA-TAB                                   
011300         PERFORM DB2-FETCH-TZ4REDA-CRS                                    
011400       END-PERFORM                                                        
011500                                                                          
011600       PERFORM DB2-CLOSE-TZ4REDA-CRS                                      
011700                                                                          
011800       IF WS-COMMITTED = 'J'                                              
011900         PERFORM DB2-SINGLE-DELETE-TZ4REKY-TAB                            
012000       ELSE                                                               
012100         PERFORM DB2-DELETE-TZ4REKY-TAB                                   
012200       END-IF                                                             
012300                                                                          
012400       PERFORM DB2-FETCH-TZ4REKY-CRS                                      
012500                                                                          
012600     END-PERFORM                                                          
012700                                                                          
012800     PERFORM DB2-CLOSE-TZ4REKY-CRS                                        
012900     .                                                                    
013000                                                                          
013100*   --- DB2 SECTIONS                                                      
013200                                                                          
013300* * * * * * * * * * * * * * HANDLE TZ4REKY * * * * * * * * * * * *        
013400 DB2-DCL-OPN-TZ4REKY-CRS SECTION.                                         
013500                                                                          
013600     MOVE 000100 TO GOOD-SQLCODECODES                                     
013700                                                                          
013800     EXEC SQL                                                             
013900         DECLARE REKY-CRS CURSOR WITH HOLD FOR                            
014000           SELECT   IDOUTTYPE                                             
014100                  , IDOUTREC                                              
014200                  , IDLIST                                                
014300                  , TIREGDAT                                              
014400                  , TIKLOCK                                               
014500                  , IDLOPNR                                               
014600                                                                          
014700           FROM     TZ4REKY                                               
014800                                                                          
014900           WHERE    NOT IDOUTTYPE     = 'NA-PROFORMA'                     
015000                    AND TIAAMMDD_RENS < :WS-CURRENT-DATE                  
015100                                                                          
015200     END-EXEC                                                             
015300                                                                          
015400     MOVE 000100 TO GOOD-SQLCODECODES                                     
015500                                                                          
015600     EXEC SQL                                                             
015700        OPEN REKY-CRS                                                     
015800     END-EXEC                                                             
015900                                                                          
016000     MOVE SQLCODE TO SQLCODE-WS                                           
016100     PERFORM DB2-STATUS-CHECK                                             
016200     .                                                                    
016300 DB2-FETCH-TZ4REKY-CRS SECTION.                                           
016400                                                                          
016500     MOVE 000100  TO GOOD-SQLCODECODES                                    
016600                                                                          
016700     EXEC SQL                                                             
016800         FETCH REKY-CRS                                                   
016900                                                                          
017000         INTO :REKY-IDOUTTYPE                                             
017100            , :REKY-IDOUTREC                                              
017200            , :REKY-IDLIST                                                
017300            , :REKY-TIREGDAT                                              
017400            , :REKY-TIKLOCK                                               
017500            , :REKY-IDLOPNR                                               
017600     END-EXEC                                                             
017700                                                                          
017800     MOVE SQLCODE TO SQLCODE-WS                                           
017900     PERFORM DB2-STATUS-CHECK                                             
018000     .                                                                    
018100 DB2-SINGLE-DELETE-TZ4REKY-TAB SECTION.                                   
018200                                                                          
018300     MOVE 000100  TO GOOD-SQLCODECODES                                    
018400                                                                          
018500     EXEC SQL                                                             
018600         DELETE FROM TZ4REKY                                              
018700         WHERE                                                            
018800               IDOUTTYPE  = :REKY-IDOUTTYPE                               
018900           AND IDOUTREC   = :REKY-IDOUTREC                                
019000           AND IDLIST     = :REKY-IDLIST                                  
019100           AND TIREGDAT   = :REKY-TIREGDAT                                
019200           AND TIKLOCK    = :REKY-TIKLOCK                                 
019300           AND IDLOPNR    = :REKY-IDLOPNR                                 
019400     END-EXEC                                                             
019500                                                                          
019600     MOVE SQLCODE TO SQLCODE-WS                                           
019700     PERFORM DB2-STATUS-CHECK                                             
019800                                                                          
019900     ADD +1 TO W-COUNT2                                                   
020000     ADD +1 TO W-COUNTC                                                   
020100                                                                          
020200     IF W-COUNTC > W-COMMIT-MAX                                           
020300       PERFORM DB2-COMMIT-WORK                                            
020400     END-IF                                                               
020500     .                                                                    
020600 DB2-DELETE-TZ4REKY-TAB SECTION.                                          
020700                                                                          
020800     MOVE 000   TO GOOD-SQLCODECODES                                      
020900                                                                          
021000     EXEC SQL                                                             
021100         DELETE FROM TZ4REKY                                              
021200                                                                          
021300         WHERE CURRENT OF REKY-CRS                                        
021400     END-EXEC                                                             
021500                                                                          
021600     MOVE SQLCODE TO SQLCODE-WS                                           
021700     PERFORM DB2-STATUS-CHECK                                             
021800                                                                          
021900     ADD +1 TO W-COUNT2                                                   
022000     ADD +1 TO W-COUNTC                                                   
022100                                                                          
022200     IF W-COUNTC > W-COMMIT-MAX                                           
022300       PERFORM DB2-COMMIT-WORK                                            
022400     END-IF                                                               
022500     .                                                                    
022600 DB2-CLOSE-TZ4REKY-CRS SECTION.                                           
022700                                                                          
022800     EXEC SQL                                                             
022900        CLOSE REKY-CRS                                                    
023000     END-EXEC                                                             
023100     .                                                                    
023200* * * * * * * * * * * * * * HANDLE TZ4REDA * * * * * * * * * * * *        
023300 DB2-DCL-OPN-TZ4REDA-CRS SECTION.                                         
023400                                                                          
023500     MOVE 000100 TO GOOD-SQLCODECODES                                     
023600                                                                          
023700     EXEC SQL                                                             
023800         DECLARE REDA-CRS CURSOR WITH HOLD FOR                            
023900            SELECT  KVPOST                                                
024000                                                                          
024100           FROM     TZ4REDA                                               
024200                                                                          
024300           WHERE    IDOUTTYPE = :REKY-IDOUTTYPE                           
024400           AND      IDOUTREC  = :REKY-IDOUTREC                            
024500           AND      IDLIST    = :REKY-IDLIST                              
024600           AND      TIREGDAT  = :REKY-TIREGDAT                            
024700           AND      TIKLOCK   = :REKY-TIKLOCK                             
024800           AND      IDLOPNR   = :REKY-IDLOPNR                             
024900     END-EXEC                                                             
025000                                                                          
025100     MOVE 000100 TO GOOD-SQLCODECODES                                     
025200                                                                          
025300     EXEC SQL                                                             
025400        OPEN REDA-CRS                                                     
025500     END-EXEC                                                             
025600                                                                          
025700     MOVE SQLCODE TO SQLCODE-WS                                           
025800     PERFORM DB2-STATUS-CHECK                                             
025900     .                                                                    
026000 DB2-FETCH-TZ4REDA-CRS SECTION.                                           
026100                                                                          
026200     MOVE 000100  TO GOOD-SQLCODECODES                                    
026300                                                                          
026400     EXEC SQL                                                             
026500         FETCH REDA-CRS                                                   
026600                                                                          
026700         INTO :REDA-KVPOST                                                
026800     END-EXEC                                                             
026900                                                                          
027000     MOVE SQLCODE TO SQLCODE-WS                                           
027100     PERFORM DB2-STATUS-CHECK                                             
027200     .                                                                    
027300 DB2-DELETE-TZ4REDA-TAB SECTION.                                          
027400                                                                          
027500     MOVE 000   TO GOOD-SQLCODECODES                                      
027600                                                                          
027700     EXEC SQL                                                             
027800         DELETE FROM TZ4REDA                                              
027900                                                                          
028000         WHERE CURRENT OF REDA-CRS                                        
028100     END-EXEC                                                             
028200                                                                          
028300     MOVE SQLCODE TO SQLCODE-WS                                           
028400     PERFORM DB2-STATUS-CHECK                                             
028500                                                                          
028600     ADD +1 TO W-COUNT3                                                   
028700     ADD +1 TO W-COUNTC                                                   
028800                                                                          
028900     IF W-COUNTC > W-COMMIT-MAX                                           
029000       PERFORM DB2-COMMIT-WORK                                            
029100     END-IF                                                               
029200     .                                                                    
029300 DB2-CLOSE-TZ4REDA-CRS SECTION.                                           
029400                                                                          
029500     EXEC SQL                                                             
029600        CLOSE REDA-CRS                                                    
029700     END-EXEC                                                             
029800     .                                                                    
029900 DB2-COMMIT-WORK  SECTION.                                                
030000                                                                          
030100     MOVE ZERO                       TO GOOD-SQLCODE (1)                  
030200     MOVE ZERO                       TO W-COUNTC                          
030300     MOVE 'J'                        TO WS-COMMITTED                      
030400     ADD +1 TO W-COUNT1                                                   
030500                                                                          
030600     EXEC SQL                                                             
030700            COMMIT WORK                                                   
030800     END-EXEC                                                             
030900     MOVE SQLCODE TO SQLCODE-WS                                           
031000     PERFORM DB2-STATUS-CHECK                                             
031100     .                                                                    
031200* *                                                                       
031300 DB2-STATUS-CHECK     SECTION.                                            
031400     SET SQLCODE-IX TO 1                                                  
031500     SEARCH GOOD-SQLCODE                                                  
031600       AT END                                                             
031700          STRING 'INVALID DB2 SQL STATUS CODE: ' SQLCODE-WS               
031800          DELIMITED BY SIZE INTO ERROR-TEXT                               
031900          CALL ABEND USING RKOD-ABEND-DB2                                 
032000       WHEN GOOD-SQLCODE (SQLCODE-IX) = SQLCODE-WS CONTINUE               
032100     END-SEARCH                                                           
032200     .                                                                    
