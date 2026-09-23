000100 PROCESS DYNAM                                                            
000200*        - THE OPTION ABOVE IS NEEDED TO LINK A BATCH-DB2-PGM             
000300*                                                                         
000400 ID DIVISION.                                                             
000500 PROGRAM-ID.     WF105100.                                                
000600 AUTHOR.         BARSHARANI BISHOYE.                                      
000700 DATE-WRITTEN.   20/10/28.                                                
000800 DATE-COMPILED.                                                           
000900                                                                          
001000*                                                                         
001100*    FUNCTION:                                                            
001200*        CREATE REPORT FOR SAFT POLAND                                    
001300*                                                                         
001400*                                                                         
001500*    ABENDCODES:                                                          
001600*        U0016 -  . . . .                                                 
001700*        U1000 -  . . . .                                                 
001800*                                                                         
001900                                                                          
002000     SKIP3                                                                
002100 ENVIRONMENT DIVISION.                                                    
002200     SKIP2                                                                
002300 INPUT-OUTPUT SECTION.                                                    
002400                                                                          
002500 FILE-CONTROL.                                                            
002600     SKIP2                                                                
002700*          --- RECORDS WITH SAFT POLAND                                   
002800     SELECT WF1051                     ASSIGN TO WF1051D1.                
002900     EJECT                                                                
003000 DATA DIVISION.                                                           
003100     SKIP3                                                                
003200 FILE SECTION.                                                            
003300     SKIP3                                                                
003400 FD  WF1051                                                               
003500     RECORDING       F                                                    
003600     BLOCK CONTAINS  0.                                                   
003700                                                                          
003800*01  RECORD -COPY WF1051  -PRE  UT-   -L.                                 
003900     EJECT                                                                
004000 WORKING-STORAGE SECTION.                                                 
004100                                                                          
004200 77  IDPGM                       PIC X(8)    VALUE 'WF105100'.            
004300 77  YES                         PIC X       VALUE 'J'.                   
004400 77  NOO                         PIC X       VALUE 'N'.                   
004500 01  WS-CURRENT-DATE             PIC X(8)  VALUE SPACE.                   
004600 01  WS-RUNDATUM-FROM            PIC X(8)  VALUE SPACE.                   
004610 01  WS-RUNDATUM-TO              PIC X(8)  VALUE SPACE.                   
004700 01  WS-RUNDATUM                 PIC X(8)  VALUE SPACE.                   
004800     EJECT                                                                
005100 01  GENERAL-SUBPROGRAMS.                                                 
005200*                                                                         
005300     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
005400     EJECT                                                                
005500 01  PROGRAM-NAMN                PIC X(6)    VALUE 'WF1051'.              
005600                                                                          
005700 01  MESSAGE-CODES.                                                       
005800     03  ERR-WRONG-KEY           PIC X(3)    VALUE '022'.                 
005900     EJECT                                                                
006000 01  WZ20DAYS                    PIC X(8)    VALUE 'WZ20DAYS'.            
006100     SKIP3                                                                
006200*    -COPY WZ20DAYS                                                       
006300     EJECT                                                                
006400                                                                          
006500*    --- PARAMETRAR TILL ABEND                                            
006600*                                                                         
006700 01  ERROR-TEXT.                                                          
006800     03  FILLER                PIC X(10)    VALUE 'ERROR-TEXT'.           
006900     03  ERROR-TEXT-STR        PIC X(72)   VALUE SPACE.                   
007000 01  RKOD-ABEND-DB2            PIC S9(4)   VALUE +998 COMP SYNC.          
007100 01  RKOD-ABEND-WITH-DUMP      PIC S9(4)   VALUE +999 COMP SYNC.          
007200     EJECT                                                                
007300*    --- PARAMETRAR TILL POSTSUM                                          
007400*                                                                         
007500*01  -COPY W0005   -PRE  POSTSUM-                                         
007600     EJECT                                                                
007700*01  AREA -COPY WF1051     -PRE UT-                                       
007800     EJECT                                                                
007900*                                                                         
008000*        WORK-AREAS FOR DB2-SECTIONS                                      
008100*                                                                         
008200 01  FILLER                    PIC X(16) VALUE 'T01DHEAARC-AREA'.         
008300*01  -COPY T01DHEA        -PRE DHEA-                                      
008400     EXEC SQL INCLUDE T01DHEA END-EXEC.                                   
008500     EJECT                                                                
008600 01  FILLER                    PIC X(16) VALUE 'T01DLINARC-AREA'.         
008700*01  -COPY T01DLIN        -PRE DLIN-                                      
008800     EXEC SQL INCLUDE T01DLIN END-EXEC.                                   
008900     EJECT                                                                
009000 01  FILLER                    PIC X(16)   VALUE 'SQLCA-AREA'.            
009100       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
009200*                        **** STATUS-CODE FROM DB2                        
009300                                                                          
009400 01  FILLER                    PIC X(16)   VALUE 'SQLCODE-WS'.            
009500 01  DB2-WS.                                                              
009600   03  SQLCODE-WS              PIC S9(3)   VALUE ZERO.                    
009700     88  ROW-FOUND                         VALUE +000.                    
009800     88  ROW-MISSING                       VALUE +100.                    
009900   03  GOOD-SQLCODES.                                                     
010000     05  GOOD-SQLCODE OCCURS 5                                            
010100         INDEXED BY SQLCODE-IX PIC 999.                                   
010200     EJECT                                                                
010300 PROCEDURE DIVISION.                                                      
010400 MAIN SECTION.                                                            
010500                                                                          
010600      PERFORM A-INIT                                                      
010700                                                                          
010800      PERFORM B-EXECUTE                                                   
010900                                                                          
011000      PERFORM Z-FINISH                                                    
011100                                                                          
011200      MOVE ZERO TO RETURN-CODE                                            
011300      GOBACK                                                              
011400      .                                                                   
011500      EJECT                                                               
011600 A-INIT SECTION.                                                          
011700                                                                          
011800     OPEN OUTPUT WF1051                                                   
011900                                                                          
012000     MOVE FUNCTION CURRENT-DATE (1:8) TO WS-CURRENT-DATE                  
012100     MOVE WS-CURRENT-DATE (1:8)       TO DAYS-TIDATE2                     
012200     MOVE 'YYYYMMDD'                  TO DAYS-KDDATFMT2                   
012300     MOVE 45                          TO DAYS-KVDAYS                      
012400     MOVE ' '                         TO DAYS-IDCALEND                    
012500     MOVE SPACE                       TO DAYS-TIDATE1                     
012600     MOVE 'YYYYMMDD'                  TO DAYS-KDDATFMT1                   
012700     CALL WZ20DAYS USING                                                  
012800          DAYS-WZ20DAYS                                                   
012900     IF DAYS-KDRC = ZERO                                                  
013102       MOVE DAYS-TIDATE1              TO WS-RUNDATUM-FROM                 
013103       MOVE '31'                      TO WS-RUNDATUM-FROM(7:2)            
013104       MOVE WS-CURRENT-DATE           TO WS-RUNDATUM-TO                   
013105       MOVE '01'                      TO WS-RUNDATUM-TO(7:2)              
013200     END-IF                                                               
013300     INITIALIZE GOOD-SQLCODES                                             
013400     .                                                                    
013500     EJECT                                                                
013600 B-EXECUTE SECTION.                                                       
013700     PERFORM DB2-OPEN-CRS1-DLIN-DHEA                                      
013800     PERFORM DB2-FETCH-CRS1-DLIN-DHEA                                     
013900     PERFORM UNTIL ROW-MISSING                                            
014000       PERFORM C-MOVE-OUTPUT                                              
014100       PERFORM S11-WRITE-WF1051                                           
014200       PERFORM DB2-FETCH-CRS1-DLIN-DHEA                                   
014300     END-PERFORM                                                          
014400     PERFORM DB2-CLOSE-CRS1-DLIN-DHEA                                     
014500     .                                                                    
014600     EJECT                                                                
014700 C-MOVE-OUTPUT SECTION.                                                   
014800     MOVE DHEA-IDFINDOC     TO  UT-IDFINDOC                               
014900     MOVE DLIN-IDSTATNR     TO  UT-IDSTATNR                               
015000     MOVE DLIN-IDEXCUST-1   TO  UT-IDEXCUST                               
015100     MOVE DHEA-DAFINDOC     TO  UT-DAFINDOC                               
015200     MOVE DHEA-SUNTO-TOT    TO  UT-SUNTO-TOT                              
015300     .                                                                    
015400     EJECT                                                                
015500 S11-WRITE-WF1051 SECTION.                                                
015600                                                                          
015700     WRITE UT-RECORD FROM UT-WF1051                                       
015800                                                                          
015900     .                                                                    
016000     EJECT                                                                
016100 Z-FINISH SECTION.                                                        
016200     CLOSE WF1051                                                         
016300     .                                                                    
016400     EJECT                                                                
016500 DB2-OPEN-CRS1-DLIN-DHEA SECTION.                                         
016600     MOVE 000100 TO GOOD-SQLCODES                                         
016700     EXEC SQL                                                             
016800        DECLARE CRS1 CURSOR FOR                                           
016900        SELECT DISTINCT B.IDFINDOC                                        
017000              ,A.IDSTATNR                                                 
017100              ,A.IDEXCUST_1                                               
017200              ,B.DAFINDOC                                                 
017300              ,B.SUNTO_TOT                                                
017400                                                                          
017500        FROM  T01DLIN_ARC A                                               
017600             ,T01DHEA_ARC B                                               
017700                                                                          
017800        WHERE  A.IDEXCUST_1    = '2878'                                   
017900        AND    B.IDLEGSEL      = A.IDLEGSEL                               
018000        AND    B.DAEXDAT       = A.DAEXDAT                                
018100        AND    B.TIEXTID       = A.TIEXTID                                
018200        AND    B.KDVALISO      = A.KDVALISO                               
018300        AND    B.IDLANDX3_SEND = A.IDLANDX3_SEND                          
018400        AND    B.IDLEVNR       = A.IDLEVNR                                
018500        AND    B.IDPARTNR      = A.IDPARTNR                               
018600        AND    B.KDFINDOC      = A.KDFINDOC                               
018700        AND    B.FLSOFT        = A.FLSOFT                                 
018800        AND    B.FLFREE        = A.FLFREE                                 
018900        AND    B.FLPRIV        = A.FLPRIV                                 
019000        AND    B.IDBREAK_1     = A.IDBREAK_1                              
019100        AND    B.IDBREAK_2     = A.IDBREAK_2                              
019200        AND    B.DAFINDOC      > :WS-RUNDATUM-FROM                        
019210        AND    B.DAFINDOC      < :WS-RUNDATUM-TO                          
019300     END-EXEC                                                             
019400                                                                          
019500     EXEC SQL                                                             
019600        OPEN CRS1                                                         
019700     END-EXEC                                                             
019800                                                                          
019900     MOVE SQLCODE        TO SQLCODE-WS                                    
020000     PERFORM DB2-STATUS-CHECK                                             
020100     .                                                                    
020200 DB2-FETCH-CRS1-DLIN-DHEA  SECTION.                                       
020300     MOVE 000100         TO GOOD-SQLCODES                                 
020400                                                                          
020500     EXEC SQL                                                             
020600              FETCH CRS1                                                  
020700              INTO  :DHEA-IDFINDOC                                        
020800                  , :DLIN-IDSTATNR                                        
020900                  , :DLIN-IDEXCUST-1                                      
021000                  , :DHEA-DAFINDOC                                        
021100                  , :DHEA-SUNTO-TOT                                       
021200     END-EXEC                                                             
021300                                                                          
021400     MOVE SQLCODE        TO SQLCODE-WS                                    
021500     PERFORM DB2-STATUS-CHECK                                             
021600     .                                                                    
021700 DB2-CLOSE-CRS1-DLIN-DHEA  SECTION.                                       
021800                                                                          
021900     EXEC SQL                                                             
022000        CLOSE CRS1                                                        
022100     END-EXEC                                                             
022200     .                                                                    
022300 DB2-STATUS-CHECK  SECTION.                                               
022400                                                                          
022500     SET SQLCODE-IX TO 1                                                  
022600     SEARCH GOOD-SQLCODE                                                  
022700       AT END                                                             
022800          STRING 'INVALID DB2 SQL STATUS CODE: ' SQLCODE-WS               
022900          DELIMITED BY SIZE INTO ERROR-TEXT                               
023000          CALL ABEND USING RKOD-ABEND-DB2                                 
023100       WHEN GOOD-SQLCODE (SQLCODE-IX) = SQLCODE-WS CONTINUE               
023200     END-SEARCH                                                           
023300     .                                                                    
