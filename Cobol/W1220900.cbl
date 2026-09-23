000100 ID DIVISION.                                                             
000300 PROGRAM-ID.    W1220900.                                                 
000400                                                                          
000500*    AUTHOR.        ANDERS HENRIKSSON.                                    
000600*    DATE-WRITTEN   APRIL   2010.                                         
000700*                                                                         
002900**** ARTIKLAR SKALL INTE SKALAS OM DE FINNS PÅ LEVERANSANMÄRKNINGS        
002901**** REGISTRET UTAN SKALL HAMNA PÅ LARMLISTA                              
002910*                                                                         
003000     EJECT                                                                
003100 ENVIRONMENT DIVISION.                                                    
003200                                                                          
003300 INPUT-OUTPUT SECTION.                                                    
003400 FILE-CONTROL.                                                            
003500                                                                          
003600     SELECT W12201  ASSIGN       TO W12209D1.                             
003700                                                                          
003800     SELECT W12202  ASSIGN       TO W12209D2.                             
003900                                                                          
004000     SELECT W41842  ASSIGN       TO W12209D3.                             
004100                                                                          
004200     SELECT W12205  ASSIGN       TO W12209D4.                             
004210                                                                          
004220     SELECT W12207  ASSIGN       TO W12209D5.                             
004300                                                                          
004400     EJECT                                                                
004500 DATA DIVISION.                                                           
004600 FILE SECTION.                                                            
004700                                                                          
004800 FD  W12201                                                               
004900     RECORDING F                                                          
005000     BLOCK CONTAINS 0.                                                    
005100                                                                          
005300*01  -COPY W12205    -L                                                   
005400     SKIP2                                                                
005500                                                                          
005600 FD  W12202                                                               
005700     RECORDING F                                                          
005800     BLOCK CONTAINS 0.                                                    
005900                                                                          
006100*01  -COPY W12207    -L                                                   
006200     SKIP2                                                                
006300                                                                          
006400 FD  W41842                                                               
006500     RECORDING V                                                          
006600     BLOCK CONTAINS 0.                                                    
006700                                                                          
006710 01  WDA201-AREA.                                                         
006720   03  FILLER                    PIC X(3).                                
006730*03  A01-AREA  -COPY WDA201  -L                                           
006740                                                                          
006750 01  WDA211-AREA.                                                         
006760   03  FILLER                    PIC X(3).                                
006770*03  A11-AREA  -COPY WDA211  -L                                           
006780                                                                          
006790 01  WDA221-AREA.                                                         
006791   03  FILLER                    PIC X(3).                                
006792*03  A21-AREA  -COPY WDA221  -L                                           
006793     EJECT                                                                
007100                                                                          
007200 FD  W12205                                                               
007300     RECORDING F                                                          
007400     BLOCK CONTAINS 0.                                                    
007500                                                                          
007600*01  UTPOST1     -COPY W12205    -L                                       
007700     SKIP2                                                                
007800                                                                          
007810 FD  W12207                                                               
007820     RECORDING F                                                          
007830     BLOCK CONTAINS 0.                                                    
007840                                                                          
007850*01  UTPOST2     -COPY W12207    -L                                       
007860     SKIP2                                                                
007870                                                                          
007900 WORKING-STORAGE SECTION.                                                 
008000     SKIP3                                                                
008100 77  IDPGM                   PIC X(8)      VALUE 'W1220900'.              
008200 77  JA                      PIC X         VALUE 'J'.                     
008300 77  NEJ                     PIC X         VALUE 'N'.                     
008400 77  EOF-W12201              PIC X         VALUE 'N'.                     
008500 77  EOF-W12202              PIC X         VALUE 'N'.                     
008600 77  EOF-W41842              PIC X         VALUE 'N'.                     
011000                                                                          
011100 01  SUBPROGRAM.                                                          
011300     03  POSTSUM             PIC X(8)    VALUE 'POSTSUM'.                 
011400                                                                          
011500 01  W12201-TRANSID.                                                      
011600     03  FILLER              PIC X(6) VALUE 'W12201'.                     
011700     03  FILLER              PIC X(8) VALUE 'W12201D1'.                   
011800     03  FILLER              PIC X(4) VALUE ' IN1'.                       
011900                                                                          
012000 01  W12202-TRANSID.                                                      
012100     03  FILLER              PIC X(6) VALUE 'W12202'.                     
012200     03  FILLER              PIC X(8) VALUE 'W12202D2'.                   
012300     03  FILLER              PIC X(4) VALUE ' IN2'.                       
012400                                                                          
012500 01  W41842-TRANSID.                                                      
012600     03  FILLER              PIC X(6) VALUE 'W41842'.                     
012700     03  FILLER              PIC X(8) VALUE 'W12203D3'.                   
012800     03  FILLER              PIC X(4) VALUE ' IN3'.                       
012900                                                                          
013000 01  W12205-TRANSID.                                                      
013100     03  FILLER              PIC X(6) VALUE 'W12205'.                     
013200     03  FILLER              PIC X(8) VALUE 'W12204D4'.                   
013300     03  FILLER              PIC X(4) VALUE ' UT1'.                       
013400                                                                          
013410 01  W12207-TRANSID.                                                      
013420     03  FILLER              PIC X(6) VALUE 'W12207'.                     
013430     03  FILLER              PIC X(8) VALUE 'W12205D4'.                   
013440     03  FILLER              PIC X(4) VALUE ' UT2'.                       
013450                                                                          
013500     EJECT                                                                
013600*   -COPY W0005  -PRE POSTSUM-                                            
013700     EJECT                                                                
014000                                                                          
014100 01  FILLER                  PIC X(16)   VALUE 'IN-AREA1   '.             
014200 01  INAREA1.                                                             
014300*    03  -COPY W12205 -PRE IN1-                                           
014400     EJECT                                                                
014500                                                                          
014600 01  FILLER                  PIC X(16)   VALUE 'IN-AREA2   '.             
014700 01  INAREA2.                                                             
014800*    03  -COPY W12207 -PRE IN2-                                           
014900     EJECT                                                                
015000                                                                          
015100 01  FILLER                  PIC X(16)   VALUE 'IN-AREA3    '.            
015200 01  INAREA3.                                                             
015320     03  IN-AREA3            PIC X(1200).                                 
015330     03  ANM-AREA REDEFINES IN-AREA3.                                     
015340         05  IN3-ANM-IDPTYP   PIC X(3).                                   
015350*        05  -COPY WDA201 -PRE IN3-                                       
015360     EJECT                                                                
015370     03  LEV-AREA REDEFINES IN-AREA3.                                     
015380         05  IN3-LEV-IDPTYP   PIC X(3).                                   
015390*        05  -COPY WDA211 -PRE IN3-                                       
015391     EJECT                                                                
015392     03  TXT-AREA REDEFINES IN-AREA3.                                     
015393         05  IN3-TXT-IDPTYP   PIC X(3).                                   
015394*        05  -COPY WDA221 -PRE IN3-                                       
015395     EJECT                                                                
015400                                                                          
015500 01  FILLER                  PIC X(16)   VALUE 'UT-AREA1    '.            
015600 01  UTAREA1.                                                             
015700*    03  -COPY W12205 -PRE UT1-                                           
015800     EJECT                                                                
015900                                                                          
015910 01  FILLER                  PIC X(16)   VALUE 'UT-AREA2    '.            
015920 01  UTAREA2.                                                             
015930*    03  -COPY W12207 -PRE UT2-                                           
015940     EJECT                                                                
015950                                                                          
016000 PROCEDURE DIVISION.                                                      
016100 MAIN SECTION.                                                            
016200                                                                          
016300     PERFORM A-INIT                                                       
016400                                                                          
016600     PERFORM S02-READ-W12202-POST                                         
016700     PERFORM S03-READ-W41842-POST                                         
016800                                                                          
017000     PERFORM B-BEHANDLA                                                   
017100                                                                          
017410     PERFORM S01-READ-W12201-POST                                         
017411     PERFORM UNTIL  EOF-W12201 = JA                                       
017412       MOVE INAREA1 TO UTAREA1                                            
017420       PERFORM S06-CREATE-FILE-W12205                                     
017421       PERFORM S01-READ-W12201-POST                                       
017430     END-PERFORM                                                          
017500                                                                          
017600     PERFORM Z-END                                                        
017700                                                                          
017800     MOVE ZERO TO RETURN-CODE                                             
017900     GOBACK                                                               
018000     .                                                                    
018100     EJECT                                                                
018200                                                                          
018300 A-INIT SECTION.                                                          
018400     OPEN INPUT  W12201                                                   
018500                 W12202                                                   
018600                 W41842                                                   
018700     OPEN OUTPUT W12205                                                   
018710                 W12207                                                   
018800                                                                          
018900     MOVE IDPGM           TO POSTSUM-PROGNAMN                             
019600     .                                                                    
019700     EJECT                                                                
019800                                                                          
019900 B-BEHANDLA SECTION.                                                      
019910     PERFORM UNTIL  EOF-W12202 = JA                                       
019920       IF EOF-W41842 = JA                                                 
019930         PERFORM S07-CREATE-FILE-W12207                                   
019940         PERFORM S02-READ-W12202-POST                                     
019950       ELSE                                                               
021503         IF IN3-LEV-IDARTNR = IN2-IDARTNR                                 
021505           MOVE SPACE     TO UT1-UTFIL-TYP                                
021506           MOVE SPACE     TO UT1-FLERS                                    
021507           MOVE IN2-IDARTNR TO UT1-IDARTNR                                
021508           MOVE IN2-IDLEVNR TO UT1-IDLEVNR                                
021509           MOVE ZERO      TO UT1-IDANSK                                   
021510           MOVE ZERO      TO UT1-KVLS                                     
021511           MOVE ZERO      TO UT1-KDERS                                    
021512           MOVE ZERO      TO UT1-KVAKS                                    
021513           MOVE ZERO      TO UT1-KVEFRS                                   
021514           MOVE ZERO      TO UT1-KVROS                                    
021515           MOVE ZERO      TO UT1-SUTPO-TOT                                
021516           MOVE ZERO      TO UT1-KVRESS                                   
021517           MOVE ZERO      TO UT1-KVBR                                     
021518           MOVE ZERO      TO UT1-KDPRODSL                                 
021519           MOVE SPACE     TO UT1-SALDO-SDC(1)                             
021520           MOVE SPACE     TO UT1-SALDO-SDC(2)                             
021521           MOVE SPACE     TO UT1-SALDO-SDC(3)                             
021522           MOVE 'J'       TO UT1-FLDISC                                   
021523           PERFORM S06-CREATE-FILE-W12205                                 
021524           PERFORM S02-READ-W12202-POST                                   
021525           PERFORM S03-READ-W41842-POST                                   
021526         ELSE                                                             
021534           IF IN3-LEV-IDARTNR > IN2-IDARTNR                               
021535             PERFORM S07-CREATE-FILE-W12207                               
021536             PERFORM S02-READ-W12202-POST                                 
021537           ELSE                                                           
021547             IF IN3-LEV-IDARTNR < IN2-IDARTNR                             
021548               PERFORM S03-READ-W41842-POST                               
021549             END-IF                                                       
021550           END-IF                                                         
021551         END-IF                                                           
021552       END-IF                                                             
021560     END-PERFORM                                                          
021600     .                                                                    
021700     EJECT                                                                
021800                                                                          
024100                                                                          
024200 Z-END SECTION.                                                           
024300     CLOSE W12201                                                         
024400           W12202                                                         
024500           W41842                                                         
024600           W12205                                                         
024610           W12207                                                         
024700                                                                          
024800     MOVE 'S'        TO POSTSUM-OPKOD                                     
024900     CALL POSTSUM USING POSTSUM-PARM                                      
025000     .                                                                    
025100     EJECT                                                                
025200                                                                          
025300 S01-READ-W12201-POST SECTION.                                            
025400     READ W12201 INTO INAREA1                                             
025500     AT END                                                               
025600       MOVE JA TO EOF-W12201                                              
025700     NOT AT END                                                           
025800       MOVE W12201-TRANSID TO POSTSUM-TRANSID                             
025900       CALL POSTSUM USING POSTSUM-PARM                                    
026000     END-READ                                                             
026100     .                                                                    
026200     SKIP2                                                                
026300                                                                          
026400 S02-READ-W12202-POST SECTION.                                            
026500     READ W12202 INTO INAREA2                                             
026600     AT END                                                               
026700       MOVE JA TO EOF-W12202                                              
026800     NOT AT END                                                           
026900       MOVE W12202-TRANSID TO POSTSUM-TRANSID                             
027000       CALL POSTSUM USING POSTSUM-PARM                                    
027100     END-READ                                                             
027200     .                                                                    
027300     SKIP2                                                                
027400                                                                          
027500 S03-READ-W41842-POST SECTION.                                            
027501     READ W41842 INTO INAREA3                                             
027502     AT END                                                               
027503       MOVE JA TO EOF-W41842                                              
027507     END-READ                                                             
027508     IF IN3-LEV-IDPTYP = '211'                                            
027510       MOVE W41842-TRANSID TO POSTSUM-TRANSID                             
027511       CALL POSTSUM USING POSTSUM-PARM                                    
027512     ELSE                                                                 
027520       PERFORM UNTIL IN3-LEV-IDPTYP = '211' OR EOF-W41842 = JA            
027600         READ W41842 INTO INAREA3                                         
027700         AT END                                                           
027800           MOVE JA TO EOF-W41842                                          
028200         END-READ                                                         
028201         IF IN3-LEV-IDPTYP = '211'                                        
028203           MOVE W41842-TRANSID TO POSTSUM-TRANSID                         
028204           CALL POSTSUM USING POSTSUM-PARM                                
028205         END-IF                                                           
028210       END-PERFORM                                                        
028220     END-IF                                                               
028300     .                                                                    
028400     EJECT                                                                
028500                                                                          
037100 S06-CREATE-FILE-W12205 SECTION.                                          
038500     WRITE UTPOST1 FROM UTAREA1                                           
038600     MOVE W12205-TRANSID TO POSTSUM-TRANSID                               
038700     CALL POSTSUM USING POSTSUM-PARM                                      
038800     .                                                                    
038900     EJECT                                                                
038910                                                                          
039000 S07-CREATE-FILE-W12207 SECTION.                                          
039200     MOVE INAREA2 TO UTAREA2                                              
040400                                                                          
040500     WRITE UTPOST2 FROM UTAREA2                                           
040600     MOVE W12207-TRANSID TO POSTSUM-TRANSID                               
040700     CALL POSTSUM USING POSTSUM-PARM                                      
040800     .                                                                    
040900     EJECT                                                                
