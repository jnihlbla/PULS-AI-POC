000100 ID DIVISION.                                                             
000200 PROGRAM-ID.    W5708400.                                                 
000300                                                                          
000400*    AUTHOR.        MAMATHA SHETTY.                                       
000500*    DATE-WRITTEN   AUG     2022.                                         
000600*                                                                         
000700**** PROGRAM SOM AGGREGERAR FAKTURARADER ÅT PEDAL                         
000800*    DÄREFTER SKER SORTERING OCH SUMMERING AV FAKTRADER.                  
000900*    SLUTLIGEN LÄGGS ALLA POSTER FKT. SOM ÖVRIGA PÅ UTFIL                 
001000*                                                                         
001100     EJECT                                                                
001200 ENVIRONMENT DIVISION.                                                    
001300                                                                          
001400 INPUT-OUTPUT SECTION.                                                    
001500 FILE-CONTROL.                                                            
001600                                                                          
001700**** COMBINED NON VCCS INPUT FILE -CN,KR,TR,MY,AE                         
001800     SELECT W57071  ASSIGN       TO W57084D1.                             
001900**** OUTPUT FILE TO SAP                                                   
002000     SELECT W57084  ASSIGN       TO W57084D5.                             
002100                                                                          
002200     EJECT                                                                
002300 DATA DIVISION.                                                           
002400 FILE SECTION.                                                            
002500                                                                          
002600 FD  W57071                                                               
002700     RECORDING       V                                                    
002800     BLOCK CONTAINS  0.                                                   
002900*01  71HEAD-POST -COPY R3HEAD20   -PRE 71A-    -L.                        
003000*01  71LINE-POST -COPY R3LINE20   -PRE 71B-    -L.                        
003100                                                                          
003200 FD  W57084                                                               
003300     RECORDING       V                                                    
003400     BLOCK CONTAINS  0.                                                   
003500 01  W57084-001              PIC X(751).                                  
003600*01  25INIT-POST -COPY R3INIT20   -PRE 25A-   -L.                         
003700*01  25HEAD-POST -COPY R3LINE20   -PRE 25A-   -L.                         
003800*01  25LINE-POST -COPY R3LINE20   -PRE 25A-   -L.                         
003900     SKIP2                                                                
004000                                                                          
004100 WORKING-STORAGE SECTION.                                                 
004200     SKIP3                                                                
004300 77  IDPGM                   PIC X(8)      VALUE 'W5708400'.              
004400 77  JA                      PIC X         VALUE 'J'.                     
004500 77  NEJ                     PIC X         VALUE 'N'.                     
004600 77  WS-PREV-KDTRADP         PIC X(4)      VALUE SPACES.                  
004700 77  WS-CURRENT-TIME-MS      PIC 9(2)      VALUE ZERO.                    
004800 77  INIT-POST               PIC X         VALUE 'N'.                     
004900 77  EOF-W57071              PIC X         VALUE 'N'.                     
005000                                                                          
005100 01  WS-CURRENT-TIME             PIC 9(8)    VALUE ZERO.                  
005200 01  FILLER REDEFINES WS-CURRENT-TIME.                                    
005300     03  CURRENT-TIME-HH          PIC 9(2).                               
005400     03  CURRENT-TIME-MM          PIC 9(2).                               
005500     03  CURRENT-TIME-SS          PIC 9(2).                               
005600     03  CURRENT-TIME-MS          PIC 9(2).                               
005700     EJECT                                                                
005800 01  W-DOC-REF                   PIC X(16).                               
005900 01  FILLER REDEFINES W-DOC-REF.                                          
006000     03  W-DOC-REF1              PIC X(9).                                
006100     03  W-DOC-REF2              PIC X(1).                                
006200     03  W-DOC-REF3              PIC X(6).                                
006300     EJECT                                                                
006400 01  SUBPROGRAM.                                                          
006500     03  POSTSUM             PIC X(8)    VALUE 'POSTSUM'.                 
006600                                                                          
006700 01  W57071-TRANSID.                                                      
006800     03  FILLER              PIC X(6) VALUE 'W57071'.                     
006900     03  FILLER              PIC X(8) VALUE 'W57084D1'.                   
007000     03  FILLER              PIC X(4) VALUE ' IN1'.                       
007100                                                                          
007200 01  W57084-TRANSID.                                                      
007300     03  FILLER              PIC X(6) VALUE 'W57084'.                     
007400     03  FILLER              PIC X(8) VALUE 'W57084D5'.                   
007500     03  FILLER              PIC X(4) VALUE ' UT2'.                       
007600                                                                          
007700     EJECT                                                                
007800*   -COPY W0005  -PRE POSTSUM-                                            
007900     EJECT                                                                
008000                                                                          
008100 01  W001-DAP.                                                            
008200     03  FILLER                  PIC X(165)  VALUE SPACE.                 
008300     EJECT                                                                
008400                                                                          
008500 01  UT-CONTROL.                                                          
008600     03  FILLER                  PIC X(16)   VALUE                        
008700                                  '¤MQMPROP Market='.                     
008800     EJECT                                                                
008900 01  FILLER                  PIC X(16)   VALUE 'IN-AREA1   '.             
009000 01  INAREA1.                                                             
009100*03  -COPY R3HEAD20              -PRE 71A-                                
009200     EJECT                                                                
009300                                                                          
009400 01  FILLER                  PIC X(16)   VALUE 'IN-AREA2   '.             
009500 01  INAREA2.                                                             
009600*03  -COPY R3LINE20              -PRE 71B-                                
009700     EJECT                                                                
009800                                                                          
009900 01  FILLER                  PIC X(16)   VALUE 'IN-AREA4   '.             
010000 01  INAREA4.                                                             
010100     03   IN-KDTRADP          PIC X(4).                                   
010200     EJECT                                                                
010300                                                                          
010400 01  FILLER                  PIC X(16)   VALUE 'UT-AREA1    '.            
010500 01  UTAREA1.                                                             
010600*03  -COPY R3HEAD20              -PRE UTA-                                
010700                                                                          
010800 01  FILLER                  PIC X(16)   VALUE 'UT-AREA2    '.            
010900 01  UTAREA2.                                                             
011000*03  -COPY R3LINE20              -PRE UTB-                                
011100                                                                          
011200 01  FILLER                  PIC X(16)   VALUE 'UT-AREA3    '.            
011300 01  UTAREA3.                                                             
011400*03  -COPY R3INIT20              -PRE UTC-                                
011500     EJECT                                                                
011600                                                                          
011700 PROCEDURE DIVISION.                                                      
011800 MAIN SECTION.                                                            
011900                                                                          
012000     PERFORM A-INIT                                                       
012100     MOVE SPACES TO WS-PREV-KDTRADP                                       
012200                                                                          
012300     PERFORM S01-READ-W57071-POST                                         
012400     PERFORM UNTIL   EOF-W57071 = JA                                      
012500       MOVE INAREA2           TO INAREA1                                  
012600       MOVE 71A-HEAD-COMPANY-CODE TO IN-KDTRADP                           
012700       IF IN-KDTRADP      NOT = WS-PREV-KDTRADP  AND                      
012800          IN-KDTRADP(1:2) NOT = WS-PREV-KDTRADP(1:2)                      
012900         MOVE IN-KDTRADP       TO WS-PREV-KDTRADP                         
013000         PERFORM S10-WRITE-DAP                                            
013100         PERFORM S06-CREATE-FILE-W57084-INIT                              
013200       END-IF                                                             
013300       IF 71A-HEAD-RECORD-TYPE = '200'                                    
013400       OR 71A-HEAD-RECORD-TYPE = '300'                                    
013500       OR 71A-HEAD-RECORD-TYPE = '600'                                    
013600         MOVE 71A-HEAD-DOCUMENT-NO-REF   TO W-DOC-REF1                    
013700         MOVE SPACES                     TO W-DOC-REF2                    
013800         MOVE W-DOC-REF1             TO 71A-HEAD-DOCUMENT-NO-REF          
013900         MOVE INAREA1                TO UTAREA1                           
014000         PERFORM S06-CREATE-FILE-W57084-HEAD                              
014100       ELSE                                                               
014200         MOVE 71B-LINE-DOCUMENT-NO-REF   TO W-DOC-REF1                    
014300         MOVE SPACES                     TO W-DOC-REF2                    
014400         MOVE W-DOC-REF1             TO 71B-LINE-DOCUMENT-NO-REF          
014500         MOVE INAREA2                TO UTAREA2                           
014600         PERFORM S06-CREATE-FILE-W57084-LINE                              
014700       END-IF                                                             
014800     PERFORM S01-READ-W57071-POST                                         
014900     END-PERFORM                                                          
015000                                                                          
015100     PERFORM Z-END                                                        
015200                                                                          
015300     MOVE ZERO TO RETURN-CODE                                             
015400     GOBACK                                                               
015500     .                                                                    
015600     EJECT                                                                
015700                                                                          
015800 A-INIT SECTION.                                                          
015900     OPEN INPUT  W57071                                                   
016000                                                                          
016100     OPEN OUTPUT W57084                                                   
016200                                                                          
016300     MOVE IDPGM           TO POSTSUM-PROGNAMN                             
016400     .                                                                    
016500     EJECT                                                                
016600                                                                          
016700 Z-END SECTION.                                                           
016800     CLOSE W57071                                                         
016900           W57084                                                         
017000                                                                          
017100     MOVE 'S'        TO POSTSUM-OPKOD                                     
017200     CALL POSTSUM USING POSTSUM-PARM                                      
017300     .                                                                    
017400     EJECT                                                                
017500                                                                          
017600 S01-READ-W57071-POST SECTION.                                            
017700     READ W57071 INTO INAREA2                                             
017800     AT END                                                               
017900       MOVE JA TO EOF-W57071                                              
018000     NOT AT END                                                           
018100       MOVE W57071-TRANSID TO POSTSUM-TRANSID                             
018200       CALL POSTSUM USING POSTSUM-PARM                                    
018300     END-READ                                                             
018400     .                                                                    
018500     SKIP2                                                                
018600 S06-CREATE-FILE-W57084-INIT SECTION.                                     
018700                                                                          
018800     MOVE IN-KDTRADP       TO UTC-INIT-COMPANY-CODE                       
018900     MOVE 'PULS'           TO UTC-INIT-FEEDER-SYSTEM                      
019000     MOVE '0001'           TO UTC-INIT-FILE-ID                            
019100     MOVE 'AXF5022'        TO UTC-INIT-USER                               
019200     MOVE 'INTEREST'       TO UTC-INIT-PASSWORD                           
019300     MOVE FUNCTION CURRENT-DATE (1:8) TO UTC-INIT-TIME-STAMP-DATE         
019400     MOVE FUNCTION CURRENT-DATE (9:4)  TO                                 
019500                                   UTC-INIT-TIME-STAMP-TIME(1:4)          
019600     MOVE FUNCTION CURRENT-DATE (15:2) TO CURRENT-TIME-MS                 
019700     COMPUTE CURRENT-TIME-MS    =  WS-CURRENT-TIME-MS + 1                 
019800     MOVE CURRENT-TIME-MS  TO UTC-INIT-TIME-STAMP-TIME(5:2)               
019900     MOVE CURRENT-TIME-MS  TO WS-CURRENT-TIME-MS                          
020000     MOVE ' '              TO UTC-INIT-FILLER                             
020100     WRITE 25A-25INIT-POST FROM UTAREA3                                   
020200     MOVE W57084-TRANSID   TO POSTSUM-TRANSID                             
020300     CALL POSTSUM USING POSTSUM-PARM                                      
020400     .                                                                    
020500     EJECT                                                                
020600 S06-CREATE-FILE-W57084-LINE SECTION.                                     
020700     WRITE 25A-25LINE-POST  FROM UTAREA2                                  
020800     MOVE W57084-TRANSID TO POSTSUM-TRANSID                               
020900     CALL POSTSUM USING POSTSUM-PARM                                      
021000     .                                                                    
021100     EJECT                                                                
021200 S06-CREATE-FILE-W57084-HEAD SECTION.                                     
021300     WRITE 25A-25HEAD-POST  FROM UTAREA1                                  
021400     MOVE W57084-TRANSID TO POSTSUM-TRANSID                               
021500     CALL POSTSUM USING POSTSUM-PARM                                      
021600     .                                                                    
021700     EJECT                                                                
021800 S10-WRITE-DAP SECTION.                                                   
021900                                                                          
022000     STRING '¤MQMPROP Market=' WS-PREV-KDTRADP(1:2)                       
022100            DELIMITED BY SIZE INTO W001-DAP                               
022200     WRITE W57084-001 FROM W001-DAP                                       
022300     .                                                                    
022400     EJECT                                                                
