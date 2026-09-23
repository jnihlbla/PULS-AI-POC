000100 PROCESS DYNAM                                                            
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     W5129300.                                                
000400 AUTHOR.         BARSHARANI BISHOYE.                                      
000500 DATE-WRITTEN.   20/03/16.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800                                                                          
000900*    FUNCTION:                                                            
001000*        GET ONE YEAR OF RECORD FROM DLIN_ARC                             
001100*        DB2 TABLE FOR VCCS VCIN VCCN VCUS VCKR                           
001200*                                                                         
001300*        THE PROGRAM READS   TABLE T01DLIN_ARC                            
001400*                                                                         
001500                                                                          
001600     SKIP3                                                                
001700 ENVIRONMENT DIVISION.                                                    
001800     SKIP2                                                                
001900 INPUT-OUTPUT SECTION.                                                    
002000                                                                          
002100 FILE-CONTROL.                                                            
002200     SKIP2                                                                
002300*          --- ONE YEAR OF VCCS RECORDS AS PER DC                         
002400     SELECT W51293                     ASSIGN TO W51293D1.                
002500     EJECT                                                                
002600*          --- ONE YEAR OF NON-VCCS RECORDS AS PER DC                     
002700     SELECT W51294                     ASSIGN TO W51293D2.                
002800     EJECT                                                                
002900*          --- ONE YEAR OF VCCS RECORDS AS PER COMPANY                    
003000     SELECT W51295                     ASSIGN TO W51293D3.                
003100     EJECT                                                                
003200*          --- ONE YEAR OF NON-VCCS RECORDS AS PER COMPANY                
003300     SELECT W51296                     ASSIGN TO W51293D4.                
003400     EJECT                                                                
003500 DATA DIVISION.                                                           
003600     SKIP3                                                                
003700 FILE SECTION.                                                            
003800     SKIP3                                                                
003900 FD  W51293                                                               
004000     RECORDING       F                                                    
004100     BLOCK CONTAINS  0.                                                   
004200                                                                          
004300*01 POST-W51293 -COPY W51293   -L.                                        
004400     EJECT                                                                
004500     SKIP3                                                                
004600 FD  W51294                                                               
004700     RECORDING       F                                                    
004800     BLOCK CONTAINS  0.                                                   
004900                                                                          
005000*01 POST-W51294 -COPY W51293   -L.                                        
005100     EJECT                                                                
005200     SKIP3                                                                
005300 FD  W51295                                                               
005400     RECORDING       F                                                    
005500     BLOCK CONTAINS  0.                                                   
005600                                                                          
005700*01 POST-W51295 -COPY W51293   -L.                                        
005800     EJECT                                                                
005900     SKIP3                                                                
006000 FD  W51296                                                               
006100     RECORDING       F                                                    
006200     BLOCK CONTAINS  0.                                                   
006300                                                                          
006400*01 POST-W51296 -COPY W51293   -L.                                        
006500     EJECT                                                                
006600 WORKING-STORAGE SECTION.                                                 
006700                                                                          
006800 77  IDPGM                       PIC X(8)    VALUE 'W5129300'.            
006900     SKIP2                                                                
007000 77  WS-IX                       PIC S9(3) COMP-3 VALUE ZERO.             
007100 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
007200 01  WS-CURRENT-DATE             PIC X(8)  VALUE SPACE.                   
007300 01  WS-RUNDATUM-TO              PIC X(8)  VALUE SPACE.                   
007400 01  WS-RUNDATUM-FROM            PIC X(8)  VALUE SPACE.                   
007500 01  WS-RUNDATUM                 PIC X(8)  VALUE SPACE.                   
007510 01  WS-IDEXCUST-MIN             PIC X(15) VALUE SPACE.                   
007520 01  WS-IDEXCUST-MAX             PIC X(15) VALUE SPACE.                   
007600     EJECT                                                                
007700 01  GENERAL-SUBPROGRAMS.                                                 
007800*                                                                         
007900     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
008000     EJECT                                                                
008100 01  PROGRAM-NAMN                PIC X(6)    VALUE 'W51293'.              
008200                                                                          
008300 01  MESSAGE-CODES.                                                       
008400     03  ERR-WRONG-KEY           PIC X(3)    VALUE '022'.                 
008500     EJECT                                                                
008600 01  WZ20DAYS                    PIC X(8)    VALUE 'WZ20DAYS'.            
008700     SKIP3                                                                
008800*    -COPY WZ20DAYS                                                       
008900     EJECT                                                                
009000                                                                          
009100*    --- PARAMETRAR TILL ABEND                                            
009200*                                                                         
009300 01  RKOD-ABEND-DB2            PIC S9(4)   VALUE +998 COMP SYNC.          
009400 01  RKOD-ABEND-WITH-DUMP      PIC S9(4)   VALUE +999 COMP SYNC.          
009500     EJECT                                                                
009600*    --- PARAMETRAR TILL POSTSUM                                          
009700*                                                                         
009800*01  -COPY W0005   -PRE  POSTSUM-                                         
009900     EJECT                                                                
010000*01  -COPY W51293  -PRE UT-                                               
010100*                                                                         
010200     EJECT                                                                
010300*                                                                         
010400*        WORK-AREAS FOR DB2-SECTIONS                                      
010500*                                                                         
010600 01  FILLER                    PIC X(16) VALUE 'T01LSEL-AREA    '.        
010700*01  -COPY T01LSEL        -PRE LSEL-                                      
010800     EXEC SQL INCLUDE T01LSEL   END-EXEC.                                 
010900     EJECT                                                                
011000 01  FILLER                    PIC X(16) VALUE 'T01DLINARC-AREA '.        
011100*01  -COPY T01DLIN        -PRE DLIN-                                      
011200     EXEC SQL INCLUDE T01DLIN   END-EXEC.                                 
011300     EJECT                                                                
011400 01  FILLER                    PIC X(16)   VALUE 'SQLCA-AREA'.            
011500       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
011600*                        **** STATUS-CODE FROM DB2                        
011700                                                                          
011800 01  FILLER                    PIC X(16)   VALUE 'SQLCODE-WS'.            
011900 01  DB2-WS.                                                              
012000   03  SQLCODE-WS              PIC S9(3)   VALUE ZERO.                    
012100     88  ROW-FOUND                         VALUE +000.                    
012200     88  ROW-MISSING                       VALUE +100.                    
012300   03  GOOD-SQLCODES.                                                     
012400     05  GOOD-SQLCODE OCCURS 5                                            
012500         INDEXED BY SQLCODE-IX PIC 999.                                   
012600     EJECT                                                                
012700 LINKAGE SECTION.                                                         
012800                                                                          
012900*01  -COPY W0009   -PRE MSG-                                              
013000     EJECT                                                                
013100 PROCEDURE DIVISION  USING MSG-PCB.                                       
013200 MAIN SECTION.                                                            
013300     ENTRY 'DLITCBL' USING MSG-PCB.                                       
013400                                                                          
013500     PERFORM A-INIT                                                       
013600                                                                          
013700     PERFORM B-EXECUTE                                                    
013800                                                                          
013900     PERFORM Z-FINISH                                                     
014000     MOVE ZERO TO RETURN-CODE                                             
014100     GOBACK                                                               
014200     .                                                                    
014300     EJECT                                                                
014400 A-INIT SECTION.                                                          
014500     OPEN OUTPUT W51293                                                   
014600                 W51294                                                   
014610                 W51295                                                   
014620                 W51296                                                   
014700                                                                          
014800     MOVE FUNCTION CURRENT-DATE (1:8) TO WS-CURRENT-DATE                  
014900     MOVE WS-CURRENT-DATE (1:8)       TO DAYS-TIDATE2                     
015000     MOVE 'YYYYMMDD'                  TO DAYS-KDDATFMT2                   
015100     MOVE 368                         TO DAYS-KVDAYS                      
015200     MOVE ' '                         TO DAYS-IDCALEND                    
015300     MOVE SPACE                       TO DAYS-TIDATE1                     
015400     MOVE 'YYYYMMDD'                  TO DAYS-KDDATFMT1                   
015500     CALL WZ20DAYS USING                                                  
015600          DAYS-WZ20DAYS                                                   
015700     IF DAYS-KDRC = ZERO                                                  
015800       MOVE DAYS-TIDATE2              TO WS-RUNDATUM-TO                   
015900       MOVE DAYS-TIDATE1              TO WS-RUNDATUM-FROM                 
016000     END-IF                                                               
016100     INITIALIZE GOOD-SQLCODES                                             
016200     .                                                                    
016300     EJECT                                                                
016400 B-EXECUTE SECTION.                                                       
016500     PERFORM DB2-DCL-OPN-T01LSEL-CRS                                      
016600     PERFORM DB2-FETCH-T01LSEL-CRS                                        
016700                                                                          
016800       PERFORM UNTIL ROW-MISSING                                          
016810         IF LSEL-IDLEGSEL = 'VCCS'                                        
016811           MOVE '100' TO  WS-IDEXCUST-MIN                                 
016812           MOVE '9000' TO  WS-IDEXCUST-MAX                                
016813         ELSE                                                             
016814           MOVE '1000' TO  WS-IDEXCUST-MIN                                
016815           MOVE '9999' TO  WS-IDEXCUST-MAX                                
016820         END-IF                                                           
016900         PERFORM DB2-OPEN-CRS1-DLIN-DC                                    
017000         PERFORM DB2-FETCH-CRS1-DLIN-DC                                   
017100           PERFORM UNTIL ROW-MISSING                                      
017200             PERFORM C-MOVE-OUTPUT                                        
017300             PERFORM S11-WRITE-W51293-W51294                              
017400             PERFORM DB2-FETCH-CRS1-DLIN-DC                               
017500           END-PERFORM                                                    
017600           PERFORM DB2-CLOSE-CRS1-DLIN-DC                                 
017700                                                                          
017800         PERFORM DB2-OPEN-CRS2-DLIN-LSEL                                  
017900         PERFORM DB2-FETCH-CRS2-DLIN-LSEL                                 
018000           PERFORM UNTIL ROW-MISSING                                      
018100             PERFORM C-MOVE-OUTPUT                                        
018110             MOVE SPACE   TO UT-IDDC                                      
018200             PERFORM S11-WRITE-W51295-W51296                              
018300             PERFORM DB2-FETCH-CRS2-DLIN-LSEL                             
018400         END-PERFORM                                                      
018500         PERFORM DB2-CLOSE-CRS2-DLIN-LSEL                                 
018510         PERFORM DB2-FETCH-T01LSEL-CRS                                    
018600       END-PERFORM                                                        
018700     PERFORM DB2-CLOSE-T01LSEL-CRS                                        
018800     .                                                                    
018900     EJECT                                                                
019000 C-MOVE-OUTPUT SECTION.                                                   
019100     MOVE DLIN-IDARTNR-FINANCE TO UT-IDARTNR-FINANCE                      
019200     MOVE DLIN-IDLEGSEL        TO UT-IDLEGSEL                             
019300     MOVE DLIN-IDDC            TO UT-IDDC                                 
019400     MOVE DLIN-KVLEVART        TO UT-KVLEVART                             
019500     .                                                                    
019600     EJECT                                                                
019810 S11-WRITE-W51293-W51294 SECTION.                                         
019900     IF DLIN-IDLEGSEL  = 'VCCS'                                           
020000       WRITE POST-W51293 FROM UT-W51293                                   
020100     ELSE                                                                 
020200       WRITE POST-W51294 FROM UT-W51293                                   
020300     END-IF                                                               
020400     .                                                                    
020500     EJECT                                                                
020600 S11-WRITE-W51295-W51296 SECTION.                                         
020700     SKIP2                                                                
020800     IF DLIN-IDLEGSEL  = 'VCCS'                                           
020900       WRITE POST-W51295 FROM UT-W51293                                   
021000     ELSE                                                                 
021100       WRITE POST-W51296 FROM UT-W51293                                   
021200     END-IF                                                               
021300     .                                                                    
021400     EJECT                                                                
021500 Z-FINISH SECTION.                                                        
021600     CLOSE W51293                                                         
021700           W51294                                                         
021710           W51295                                                         
021720           W51296                                                         
021800     .                                                                    
021900     EJECT                                                                
022000* --- DB2 SECTIONS  ---                                                   
022100 DB2-DCL-OPN-T01LSEL-CRS SECTION.                                         
022200     MOVE 000100 TO GOOD-SQLCODES                                         
022300                                                                          
022400     EXEC SQL                                                             
022500         DECLARE LSEL-CRS CURSOR FOR                                      
022600            SELECT  IDLEGSEL                                              
022700                                                                          
022800            FROM     T01LSEL                                              
022900                                                                          
023000     END-EXEC                                                             
023100                                                                          
023200     MOVE 000100 TO GOOD-SQLCODES                                         
023300                                                                          
023400     EXEC SQL                                                             
023500        OPEN LSEL-CRS                                                     
023600     END-EXEC                                                             
023700                                                                          
023800     MOVE SQLCODE TO SQLCODE-WS                                           
023900     PERFORM DB2-STATUS-CHECK                                             
024000     .                                                                    
024100                                                                          
024200 DB2-FETCH-T01LSEL-CRS SECTION.                                           
024300     MOVE 000100  TO GOOD-SQLCODES                                        
024400                                                                          
024500     EXEC SQL                                                             
024600         FETCH LSEL-CRS                                                   
024700                                                                          
024800         INTO :LSEL-IDLEGSEL                                              
024900     END-EXEC                                                             
025000                                                                          
025100     MOVE SQLCODE TO SQLCODE-WS                                           
025200     PERFORM DB2-STATUS-CHECK                                             
025300     .                                                                    
025400 DB2-CLOSE-T01LSEL-CRS SECTION.                                           
025500                                                                          
025600     EXEC SQL                                                             
025700        CLOSE LSEL-CRS                                                    
025800     END-EXEC                                                             
025900     .                                                                    
026000                                                                          
026100 DB2-OPEN-CRS1-DLIN-DC SECTION.                                           
026200     MOVE 000100 TO GOOD-SQLCODES                                         
026300     EXEC SQL                                                             
026400        DECLARE CRS1 CURSOR FOR                                           
026500        SELECT IDDC,SUM(KVLEVART)                                         
026600             , IDLEGSEL                                                   
026800             , IDARTNR_FINANCE                                            
026900                                                                          
027000        FROM   T01DLIN_ARC                                                
027100                                                                          
027200        WHERE    KDFINDOC         = 'INV'                                 
027300        AND      IDEXCUST_1       > :WS-IDEXCUST-MIN                      
027400        AND      IDEXCUST_1       < :WS-IDEXCUST-MAX                      
027500        AND      IDARTNR_FINANCE  > ' '                                   
027600        AND      FLSOFT           = 'N'                                   
027700        AND      IDLEGSEL         = :LSEL-IDLEGSEL                        
027800        AND   (DAEXDAT    =  :WS-RUNDATUM-FROM                            
027900        OR     DAEXDAT    >  :WS-RUNDATUM-FROM)                           
028000        AND   (DAEXDAT    =  :WS-RUNDATUM-TO                              
028100        OR     DAEXDAT    <  :WS-RUNDATUM-TO)                             
028110        GROUP BY                                                          
028120                 T01DLIN_ARC.IDDC                                         
028130                ,T01DLIN_ARC.IDARTNR_FINANCE                              
028140                ,T01DLIN_ARC.IDLEGSEL                                     
028200     END-EXEC                                                             
028300                                                                          
028400     EXEC SQL                                                             
028500        OPEN CRS1                                                         
028600     END-EXEC                                                             
028700                                                                          
028800     MOVE SQLCODE        TO SQLCODE-WS                                    
028900     PERFORM DB2-STATUS-CHECK                                             
029000     .                                                                    
029100 DB2-FETCH-CRS1-DLIN-DC  SECTION.                                         
029200     MOVE 000100         TO GOOD-SQLCODES                                 
029300                                                                          
029400     EXEC SQL                                                             
029500              FETCH CRS1                                                  
029600              INTO  :DLIN-IDDC                                            
029610                  , :DLIN-KVLEVART                                        
029700                  , :DLIN-IDLEGSEL                                        
029800                  , :DLIN-IDARTNR-FINANCE                                 
030000     END-EXEC                                                             
030100                                                                          
030200     MOVE SQLCODE        TO SQLCODE-WS                                    
030300     PERFORM DB2-STATUS-CHECK                                             
030400     .                                                                    
030500 DB2-CLOSE-CRS1-DLIN-DC  SECTION.                                         
030600                                                                          
030700     EXEC SQL                                                             
030800        CLOSE CRS1                                                        
030900     END-EXEC                                                             
031000     .                                                                    
031100 DB2-OPEN-CRS2-DLIN-LSEL SECTION.                                         
031200     MOVE 000100 TO GOOD-SQLCODES                                         
031300     EXEC SQL                                                             
031400        DECLARE CRS2 CURSOR FOR                                           
031500        SELECT IDLEGSEL,SUM(KVLEVART)                                     
031800             , IDARTNR_FINANCE                                            
031900                                                                          
032000        FROM   T01DLIN_ARC                                                
032100                                                                          
032200        WHERE    KDFINDOC         = 'INV'                                 
032300        AND      IDEXCUST_1       > :WS-IDEXCUST-MIN                      
032400        AND      IDEXCUST_1       < :WS-IDEXCUST-MAX                      
032500        AND      IDARTNR_FINANCE  > ' '                                   
032600        AND      FLSOFT           = 'N'                                   
032700        AND      IDLEGSEL         = :LSEL-IDLEGSEL                        
032800        AND   (DAEXDAT    =  :WS-RUNDATUM-FROM                            
032900        OR     DAEXDAT    >  :WS-RUNDATUM-FROM)                           
033000        AND   (DAEXDAT    =  :WS-RUNDATUM-TO                              
033100        OR     DAEXDAT    <  :WS-RUNDATUM-TO)                             
033120        GROUP BY                                                          
033140                 T01DLIN_ARC.IDARTNR_FINANCE                              
033150                ,T01DLIN_ARC.IDLEGSEL                                     
033200     END-EXEC                                                             
033300                                                                          
033400     EXEC SQL                                                             
033500        OPEN CRS2                                                         
033600     END-EXEC                                                             
033700                                                                          
033800     MOVE SQLCODE        TO SQLCODE-WS                                    
033900     PERFORM DB2-STATUS-CHECK                                             
034000     .                                                                    
034100 DB2-FETCH-CRS2-DLIN-LSEL SECTION.                                        
034200     MOVE 000100         TO GOOD-SQLCODES                                 
034300                                                                          
034400     EXEC SQL                                                             
034500              FETCH CRS2                                                  
034600              INTO  :DLIN-IDLEGSEL                                        
034610                  , :DLIN-KVLEVART                                        
034900                  , :DLIN-IDARTNR-FINANCE                                 
035000     END-EXEC                                                             
035100                                                                          
035200     MOVE SQLCODE        TO SQLCODE-WS                                    
035300     PERFORM DB2-STATUS-CHECK                                             
035400     .                                                                    
035500 DB2-CLOSE-CRS2-DLIN-LSEL SECTION.                                        
035600                                                                          
035700     EXEC SQL                                                             
035800        CLOSE CRS2                                                        
035900     END-EXEC                                                             
036000     .                                                                    
036100 DB2-STATUS-CHECK  SECTION.                                               
036200                                                                          
036300     SET SQLCODE-IX TO 1                                                  
036400     SEARCH GOOD-SQLCODE                                                  
036500       AT END                                                             
036600          STRING 'INVALID DB2 SQL STATUS CODE: ' SQLCODE-WS               
036700          DELIMITED BY SIZE INTO ERROR-TEXT                               
036800          CALL ABEND USING RKOD-ABEND-DB2                                 
036900       WHEN GOOD-SQLCODE (SQLCODE-IX) = SQLCODE-WS CONTINUE               
037000     END-SEARCH                                                           
037100     .                                                                    
