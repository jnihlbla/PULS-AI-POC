000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W4122100.                                                
000300 AUTHOR.         P. GALBAO.                                               
000400 DATE-WRITTEN.   APR 2021.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    REMARKS.                                                             
000800*                                                                         
000900*    FUNCTION.                                                            
001000*                                                                         
001100*    THE PROGRAM READS SORTED EXTRACT FILE WITH ORDER INFORMATION         
001100*    IN STATUS='P' (PRINTED AND PACKED) AND HAVING REPAIR-DATE            
001300*    PER DISTRICT=778/CUSTOMER/DC=11/RFS=TODAY'S DATE                     
001400*    SEND 1 DAP MAIL PER CUSTOMER WITH ALL LINES.                         
001500*    NO MAIL WILL BE SENT, IF THE CUSTOMER DOES NOT HAVE A LINE.          
001600*                                                                         
001700*    THE PROGRAM RUNS DAILY AT 20:00 CET.                                 
001800     EJECT                                                                
001900                                                                          
002000 ENVIRONMENT DIVISION.                                                    
002100                                                                          
002200 INPUT-OUTPUT SECTION.                                                    
002300                                                                          
002400 FILE-CONTROL.                                                            
002500                                                                          
002600*    --- ORDER INFORMATION WITH STATUS='P' AND REPAIR-DATE                
002700*        PER DISTRICT=778/CUSTOMER/DC=11/RFS=TODAY'S DATE                 
002800     SELECT W41221                     ASSIGN TO W41221D1.                
002900*          ---                                                            
003000     SELECT W41221UT-ALL               ASSIGN TO W41221D2.                
003100                                                                          
003200     EJECT                                                                
003300 DATA DIVISION.                                                           
003400                                                                          
003500 FILE SECTION.                                                            
003600                                                                          
003800 FD  W41221                                                               
003900     RECORDING F                                                          
004000     LABEL RECORD STANDARD                                                
004100     BLOCK CONTAINS  0.                                                   
004200                                                                          
004300*01 -COPY W4122A    -L.                                                   
004400                                                                          
004500 FD  W41221UT-ALL                                                         
004600     RECORDING  V                                                         
004700     BLOCK CONTAINS  0 RECORDS.                                           
004800 01  ORDER-P-STATUS-ALL    PIC X(86).                                     
004900                                                                          
005000     EJECT                                                                
005100 WORKING-STORAGE SECTION.                                                 
005200*    -- CHECKED BY WY2000                                                 
005300*    -COPY WY2000W1                                                       
005400                                                                          
005500 77  PROGRAM-NAME                PIC X(8)    VALUE 'W4122100'.            
005600 77  CURRENT-SECTION             PIC X(20)   VALUE SPACE.                 
005700 77  WS-IDKUNDNR                 PIC S9(7)   VALUE ZERO COMP-3.           
005800*                                                                         
005900 77  W41221-EOF-SW               PIC X       VALUE 'N'.                   
006000     88  END-OF-W41221                       VALUE 'J'.                   
006100                                                                          
006200     EJECT                                                                
006300*                                                                         
006400 01  WS-MISC.                                                             
006500     03  WS-DAP-SUBTYPE.                                                  
006600         05  WS-IDDISTR-X        PIC 9(4)    VALUE ZERO.                  
006700         05  WS-IDKUNDNR-X       PIC 9(6)    VALUE ZERO.                  
006800                                                                          
006900     EJECT                                                                
007000 01  W41221-AREA-START           PIC X(24)   VALUE                        
007100                                 'W41221-AREA-START  '.                   
007200                                                                          
007300*01  AREA -COPY W4122A     -PRE IN-                                       
007400                                                                          
007500     EJECT                                                                
007600 01  W41221UT-AREA-START         PIC X(24)   VALUE                        
007700                                 'W41221UT-AREA-START'.                   
007800                                                                          
007900 01  W001-DAP.                                                            
008000     03  FILLER                  PIC X(165)  VALUE SPACE.                 
008100                                                                          
008100 01  UT-FILLER                   PIC X(86)   VALUE SPACE.                 
008100                                                                          
008200 01  UT-AREA.                                                             
008300     03 UT-RPT-PRINT-LINES.                                               
008400       05 UT-HEADING.                                                     
008500         10 FILLER               PIC X(10) VALUE ' DISTRICT '.            
008700         10 FILLER               PIC X(10) VALUE 'CUSTOMER  '.            
010000         10 FILLER               PIC X(13) VALUE 'NEW ORDER NO.'.         
009100         10 FILLER               PIC X(10) VALUE ' PART NO. '.            
009300         10 FILLER               PIC X(10) VALUE 'QUANTITY  '.            
009500         10 FILLER               PIC X(13) VALUE 'REPAIR DATE  '.         
009700         10 FILLER               PIC X(16) VALUE                          
009800                                              'WORKSHOP ORDER  '.         
010200       05 UT-DETAIL.                                                      
010400         10 FILLER               PIC X(4)  VALUE SPACES.                  
010300         10 UT-IDDISTR           PIC Z(3)9 VALUE ZERO.                    
010600         10 FILLER               PIC X(2)  VALUE SPACES.                  
010600         10 FILLER               PIC X(2)  VALUE SPACES.                  
010500         10 UT-IDKUNDNR          PIC Z(5)9 VALUE ZERO.                    
010800         10 FILLER               PIC X(2)  VALUE SPACES.                  
011700         10 UT-IDORDNR           PIC X(10) VALUE SPACES.                  
011600         10 FILLER               PIC X(3)  VALUE SPACES.                  
011000         10 FILLER               PIC X(1)  VALUE SPACES.                  
010900         10 UT-IDARTNR           PIC Z(7)9 VALUE ZERO.                    
011000         10 FILLER               PIC X(2)  VALUE SPACES.                  
011000         10 FILLER               PIC X(1)  VALUE SPACES.                  
011100         10 UT-KVLEVART          PIC Z(6)9 VALUE ZERO.                    
011200         10 FILLER               PIC X(2)  VALUE SPACES.                  
011200         10 FILLER               PIC X(4)  VALUE SPACES.                  
011300         10 UT-TIREPDAT          PIC Z(6)9 VALUE ZERO.                    
011400         10 FILLER               PIC X(2)  VALUE SPACES.                  
011400         10 FILLER               PIC X(4)  VALUE SPACES.                  
011500         10 UT-BERADREF          PIC X(10) VALUE SPACES.                  
011600         10 FILLER               PIC X(2)  VALUE SPACES.                  
011800     EJECT                                                                
011900 PROCEDURE DIVISION.                                                      
012000 MAIN SECTION.                                                            
012100                                                                          
012200     PERFORM A-INIT                                                       
012300     PERFORM B-CREATE-OUTPUT                                              
012400     PERFORM Z-FINIT                                                      
012500                                                                          
012600     MOVE ZERO                        TO RETURN-CODE                      
012700     GOBACK                                                               
012800     .                                                                    
012900     EJECT                                                                
013000                                                                          
013100                                                                          
013200 A-INIT SECTION.                                                          
013300     MOVE 'A-INIT                   ' TO CURRENT-SECTION                  
013500                                                                          
013400     OPEN INPUT  W41221                                                   
013500                                                                          
013600     OPEN OUTPUT W41221UT-ALL                                             
013700     .                                                                    
013800     EJECT                                                                
013900 B-CREATE-OUTPUT SECTION.                                                 
014000     MOVE 'B-CREATE-OUTPUT          ' TO CURRENT-SECTION                  
013500                                                                          
014000     PERFORM S01-READ-W41221                                              
014100                                                                          
014200     PERFORM UNTIL END-OF-W41221                                          
014300       IF IN-IDKUNDNR NOT = WS-IDKUNDNR                                   
014400          PERFORM S02-WRITE-DAP1                                          
014600          MOVE IN-IDDISTR             TO WS-IDDISTR-X                     
014700          MOVE IN-IDKUNDNR            TO UT-IDKUNDNR                      
014800                                         WS-IDKUNDNR                      
014900                                         WS-IDKUNDNR-X                    
014500          PERFORM S03-WRITE-DAP2                                          
015000          WRITE ORDER-P-STATUS-ALL  FROM UT-FILLER                        
015000          WRITE ORDER-P-STATUS-ALL  FROM UT-HEADING                       
015100       END-IF                                                             
015200       MOVE IN-IDDISTR                TO UT-IDDISTR                       
015300       MOVE IN-IDDISTR                TO UT-IDDISTR                       
015400       MOVE IN-IDKUNDNR               TO UT-IDKUNDNR                      
016000       MOVE IN-IDORDNR                TO UT-IDORDNR                       
015600       MOVE IN-IDARTNR                TO UT-IDARTNR                       
015700       MOVE IN-KVLEVART               TO UT-KVLEVART                      
015800       MOVE IN-TIREPDAT               TO UT-TIREPDAT                      
015900       MOVE IN-BERADREF               TO UT-BERADREF                      
016100       MOVE UT-DETAIL                 TO ORDER-P-STATUS-ALL               
016200       WRITE ORDER-P-STATUS-ALL     FROM UT-DETAIL                        
016300       PERFORM S01-READ-W41221                                            
016400     END-PERFORM                                                          
016500     .                                                                    
016600     EJECT                                                                
016700                                                                          
016800                                                                          
016900 Z-FINIT SECTION.                                                         
017000     CLOSE W41221                                                         
017100           W41221UT-ALL                                                   
017200     .                                                                    
017300                                                                          
017400     EJECT                                                                
017500 S01-READ-W41221  SECTION.                                                
017600     READ W41221 INTO IN-AREA                                             
017700     AT END                                                               
017800        SET END-OF-W41221             TO TRUE                             
017900                                                                          
018000     END-READ                                                             
018100     .                                                                    
018200                                                                          
018300                                                                          
018400 S02-WRITE-DAP1 SECTION.                                                  
018500                                                                          
018600     MOVE ' ¤DAPW41221'               TO W001-DAP                         
018700     WRITE ORDER-P-STATUS-ALL       FROM W001-DAP                         
018800                                                                          
018900     MOVE SPACE                       TO W001-DAP                         
019000     .                                                                    
019100                                                                          
019200 S03-WRITE-DAP2 SECTION.                                                  
019300                                                                          
019400     STRING ' ¤DAP' WS-DAP-SUBTYPE                                        
019500            DELIMITED BY SIZE INTO W001-DAP                               
019600     WRITE ORDER-P-STATUS-ALL       FROM W001-DAP                         
019700                                                                          
019800     MOVE SPACE                       TO W001-DAP                         
019900     .                                                                    
