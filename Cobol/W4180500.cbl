000100 PROCESS DYNAM                                                            
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     W4180500.                                                
000400 AUTHOR.         PRIYASOPHIA GALBAO.                                      
000500 DATE-WRITTEN.   20/02/27.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*                                                                         
000900*    FUNCTION:                                                            
001000*        CREATE AN EMAIL(D&P) REPORT FIRST OF EVERY MONTH WITH            
001100*        ALL DISTRICTS AND VALUE OF WAITING TO BE INVOICED                
001200*                                                                         
001300                                                                          
001400     SKIP3                                                                
001500 ENVIRONMENT DIVISION.                                                    
001600     SKIP2                                                                
001700 INPUT-OUTPUT SECTION.                                                    
001800                                                                          
001900 FILE-CONTROL.                                                            
002000     SELECT W41805              ASSIGN TO UT-S-W41805D1.                  
002100     EJECT                                                                
002200 DATA DIVISION.                                                           
002300     SKIP3                                                                
002400 FILE SECTION.                                                            
002500 FD  W41805                                                               
002600     RECORDING V                                                          
002700     BLOCK CONTAINS 0.                                                    
002800 01  W41805TXT                    PIC X(80).                              
002900     EJECT                                                                
003000 WORKING-STORAGE SECTION.                                                 
003100                                                                          
003200 77  IDPGM                      PIC X(8)     VALUE 'W4180500'.            
003300 77  WS-SUMMA-TOT               PIC S9(9)V9(2) VALUE ZERO COMP-3.         
003400     EJECT                                                                
003500 01  FELTEXT.                                                             
003600     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
003700     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
003800 01  UT-AREA.                                                             
003900     03  FILLER                 PIC X(52)    VALUE                        
004000         'Sum of handling fee waiting to be processed for CC '.           
004100     03  UT-IDFTG               PIC X(2).                                 
004200     03  FILLER                 PIC X(8)    VALUE ' is SEK '.             
004300     03  UT-SUMMA-TOT           PIC Z(9)9.9(2).                           
004400     EJECT                                                                
004500 01  UT-AREA1.                                                            
004600     03  FILLER                 PIC X(30)    VALUE                        
004700         'Everything has been Invoiced '.                                 
004800     EJECT                                                                
004900 01 DB2-LASNING.                                                          
005000     03 FILLER                   PIC X(16)   VALUE                        
005100                                             'WS-DB2-SEKTION'.            
005200     03 WS-DB2-SEKTION           PIC X(24)   VALUE SPACE.                 
005300     EJECT                                                                
005400 01  GENERAL-SUBPROGRAMS.                                                 
005500*                                                                         
005600     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
005700     SKIP2                                                                
005800*    --- PARAMETERS TO ABEND                                              
005900                                                                          
006000 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
006100 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
006200 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
006300 77  RKOD-ABEND-DB2              PIC S9(4)   COMP VALUE +998.             
006400     SKIP2                                                                
006500     EJECT                                                                
006600*    ---  DB2 HOST-COPYTEXTER                                             
006700                                                                          
006800     EJECT                                                                
006900 01  FILLER                      PIC X(16)  VALUE 'TP8LRET-AREA'.         
007000*01  -COPY TP8LRET -PRE LRET-                                             
007100                                                                          
007200 01  FILLER                      PIC X(16)  VALUE 'TP8LRET-DCL'.          
007300       EXEC SQL INCLUDE TP8LRET END-EXEC.                                 
007400                                                                          
007500     EJECT                                                                
007600                                                                          
007700*                            DB2 CURSOR                                   
007800 01  FILLER                      PIC X(16)   VALUE 'TP8LRET-CRS'.         
007900       EXEC SQL DECLARE TP8LRET-CRS CURSOR FOR                            
008000       SELECT   IDFTG,                                                    
008100                SUM(PRARTNTO * KVLEVART)                                  
008200                                                                          
008300       FROM     TP8LRET                                                   
008400                                                                          
008500       WHERE    KDRAPPSTA = 'W '                                          
008600                                                                          
008700       GROUP BY IDFTG                                                     
008800       ORDER BY IDFTG                                                     
008900                                                                          
009000       FOR FETCH ONLY                                                     
009100       END-EXEC                                                           
009200*                            DB2 FUNKTIONSKODER                           
009300 01  FILLER                      PIC X(16)   VALUE 'SQLCA-AREA'.          
009400       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
009500                                                                          
009600 01  FILLER                      PIC X(16)   VALUE 'SQLCODE-WS'.          
009700 01  DB2-WS.                                                              
009800     03  SQLCODE-WS              PIC S9(3)    VALUE ZERO.                 
009900         88  ROW-FOUND                           VALUE +000.              
010000         88  ROW-MISSING                         VALUE +100.              
010100         88  MANY-ROWS                           VALUE +305.              
010200     03  GOOD-SQLCODES.                                                   
010300       05  GOOD-SQLCODE OCCURS 5                                          
010400           INDEXED BY SQLCODE-IX    PIC 999.                              
010500     EJECT                                                                
010600                                                                          
010700 PROCEDURE DIVISION.                                                      
010800 MAIN SECTION.                                                            
010900     SKIP2                                                                
011000                                                                          
011100     PERFORM A-INIT                                                       
011200                                                                          
011300     PERFORM B-EXECUTE                                                    
011400                                                                          
011500     PERFORM Z-FINISH                                                     
011600                                                                          
011700     MOVE ZERO TO RETURN-CODE                                             
011800     GOBACK                                                               
011900     .                                                                    
012000     EJECT                                                                
012100 A-INIT SECTION.                                                          
012200     SKIP2                                                                
012300     OPEN OUTPUT W41805                                                   
012400                                                                          
012500     INITIALIZE GOOD-SQLCODES                                             
012600                                                                          
012700     MOVE ZEROS          TO WS-SUMMA-TOT                                  
012800                            UT-SUMMA-TOT                                  
012900     .                                                                    
013000     EJECT                                                                
013100 B-EXECUTE SECTION.                                                       
013200     SKIP2                                                                
013300     PERFORM DB2-OPEN-CRS-TP8LRET                                         
013400     PERFORM DB2-FETCH-CRS-TP8LRET                                        
013500      IF ROW-MISSING                                                      
013600        PERFORM S11-WRITE-W41806                                          
013700      END-IF                                                              
013800     PERFORM UNTIL ROW-MISSING                                            
013900       MOVE LRET-IDFTG           TO UT-IDFTG                              
014000       MOVE WS-SUMMA-TOT         TO UT-SUMMA-TOT                          
014100       DISPLAY UT-AREA                                                    
014200       IF LRET-IDFTG = '57'                                               
014300          PERFORM S11-WRITE-W41805                                        
014400       END-IF                                                             
014500       PERFORM DB2-FETCH-CRS-TP8LRET                                      
014600     END-PERFORM                                                          
014700       PERFORM DB2-CLOSE-CRS-TP8LRET                                      
014800     .                                                                    
014900     EJECT                                                                
015000 S11-WRITE-W41805 SECTION.                                                
015100     WRITE W41805TXT          FROM UT-AREA                                
015200                                                                          
015300     .                                                                    
015400     EJECT                                                                
015500 S11-WRITE-W41806 SECTION.                                                
015600     WRITE W41805TXT          FROM UT-AREA1                               
015700                                                                          
015800     .                                                                    
015900     EJECT                                                                
016000* --- DB2 SECTIONS  ---                                                   
016100*                                                                         
016200 DB2-OPEN-CRS-TP8LRET SECTION.                                            
016300                                                                          
016400      MOVE 000            TO GOOD-SQLCODES                                
016500      EXEC SQL OPEN TP8LRET-CRS END-EXEC                                  
016600      MOVE SQLCODE        TO SQLCODE-WS                                   
016700      PERFORM DB2-STATUS-CHECK                                            
016800      .                                                                   
016900     EJECT                                                                
017000 DB2-FETCH-CRS-TP8LRET SECTION.                                           
017100                                                                          
017200     EXEC SQL FETCH TP8LRET-CRS INTO                                      
017300            :LRET-IDFTG,                                                  
017400            :WS-SUMMA-TOT                                                 
017500     END-EXEC                                                             
017600                                                                          
017700     MOVE 000100         TO GOOD-SQLCODES                                 
017800     MOVE SQLCODE        TO SQLCODE-WS                                    
017900     PERFORM DB2-STATUS-CHECK                                             
018000     .                                                                    
018100     EJECT                                                                
018200 DB2-CLOSE-CRS-TP8LRET SECTION.                                           
018300                                                                          
018400     EXEC SQL CLOSE TP8LRET-CRS                                           
018500     END-EXEC                                                             
018600     .                                                                    
018700     EJECT                                                                
018800                                                                          
018900 DB2-STATUS-CHECK SECTION.                                                
019000     SET SQLCODE-IX         TO 1                                          
019100     SEARCH GOOD-SQLCODE                                                  
019200       AT END                                                             
019300          STRING 'INVALID DB2 SQL STATUS CODE: ' SQLCODE-WS               
019400          DELIMITED BY SIZE INTO FELTEXT-STR                              
019500          DISPLAY FELTEXT                                                 
019600          CALL ABEND USING RKOD-ABEND-DB2                                 
019700        WHEN GOOD-SQLCODE (SQLCODE-IX) = SQLCODE-WS                       
019800           CONTINUE                                                       
019900     END-SEARCH                                                           
020000     .                                                                    
020100 Z-FINISH SECTION.                                                        
020200     CLOSE W41805                                                         
020300     .                                                                    
020400     EJECT                                                                
