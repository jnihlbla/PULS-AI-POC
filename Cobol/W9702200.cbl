000100 ID  DIVISION.                                                            
000200     SKIP2                                                                
000300 PROGRAM-ID.  W9702200.                                                   
000400     SKIP2                                                                
000500 AUTHOR.        KARIN OLSSON.                                             
000600     DATE-WRITTEN.  APR 1993.                                             
000700*                                                                         
000800*                                                                         
000900*    PLOCKAR UT INFORMATION UR ACF2-TRANSMEDLEMMAR I V2.                  
001000*                                                                         
001100     EJECT                                                                
001200 ENVIRONMENT DIVISION.                                                    
001300     SKIP2                                                                
001400 INPUT-OUTPUT  SECTION.                                                   
001500*                                                                         
001600 FILE-CONTROL.                                                            
001700*                                                                         
001800     SELECT REGELFIL        ASSIGN TO W97022D1.                           
001900*                                                                         
002000     SELECT BESKRFIL        ASSIGN TO W97022D2.                           
002100*                                                                         
002200     SELECT MSCFIL          ASSIGN TO W97022D3.                           
002300*                                                                         
002400     SELECT UTFIL           ASSIGN TO W97022D4.                           
002500*                                                                         
002600*                                                                         
002700 DATA DIVISION.                                                           
002800                                                                          
002900 FILE  SECTION.                                                           
003000*                                                                         
003100 FD  REGELFIL                                                             
003200     LABEL RECORD   STANDARD                                              
003300     RECORDING      F                                                     
003400     BLOCK CONTAINS 0.                                                    
003500                                                                          
003600 01  FILLER                  PIC X(121).                                  
003700*                                                                         
003800 FD  BESKRFIL                                                             
003900     LABEL RECORD   STANDARD                                              
004000     RECORDING      F                                                     
004100     BLOCK CONTAINS 0.                                                    
004200                                                                          
004300 01  FILLER                  PIC X(133).                                  
004400*                                                                         
004500 FD  MSCFIL                                                               
004600     LABEL RECORD   STANDARD                                              
004700     RECORDING      F                                                     
004800     BLOCK CONTAINS 0.                                                    
004900                                                                          
005000 01  FILLER                  PIC X(133).                                  
005100*                                                                         
005200 FD  UTFIL                                                                
005300     LABEL RECORD   STANDARD                                              
005400     RECORDING      F                                                     
005500     BLOCK CONTAINS 0.                                                    
005600                                                                          
005700 01  UT-POST                 PIC X(60).                                   
005800     EJECT                                                                
005900 WORKING-STORAGE  SECTION.                                                
005901                                                                          
005910*    -- CHECKED BY WY2000                                                 
006000*                                                                         
006100 01  W-REGEL-AREA.                                                        
006200     03  W-REGEL-RAD             PIC X(121).                              
006300*                                                                         
006400     03  W-REGEL-AREA1 REDEFINES W-REGEL-RAD.                             
006500*                                                                         
006600         05  FILLER              PIC X(2).                                
006700         05  PDSSCAN-ORD         PIC X(7).                                
006800         05  FILLER              PIC X(70).                               
006900         05  MEDLEM-ORD          PIC X(7).                                
007000         05  FILLER              PIC X.                                   
007100         05  MEDLEM              PIC X(8).                                
007200         05  FILLER              PIC X(26).                               
007300*                                                                         
007400     03  W-REGEL-AREA2 REDEFINES W-REGEL-RAD.                             
007500*                                                                         
007600         05  FILLER              PIC X(10).                               
007700         05  FILLER              PIC X.                                   
007800         05  UID-TEXT            PIC X(4).                                
007900         05  UID-STR             PIC X(40).                               
008000         05  FILLER              PIC X(66).                               
008100*                                                                         
008200 01  W-BESKR-AREA.                                                        
008300     03  FILLER                  PIC X(37).                               
008400     03  W-BESKR-TRANS           PIC X(8).                                
008500     03  FILLER                  PIC X(17).                               
008600     03  W-BESKR-TEXT            PIC X(30).                               
008700     03  FILLER                  PIC X(41).                               
008800*                                                                         
008900 01  W-BESKR-AREA-2 REDEFINES W-BESKR-AREA.                               
009000     03  FILLER                  PIC X(37).                               
009100     03  W-TRANSACTION           PIC X(11).                               
009200     03  FILLER                  PIC X(3).                                
009300     03  W-ACCOUNT               PIC X(7).                                
009400     03  FILLER                  PIC X(15).                               
009500     03  W-COMMENTS              PIC X(8).                                
009600     03  FILLER                  PIC X(52).                               
009700*                                                                         
009800 01  W-MSC-AREA.                                                          
009900     03  FILLER                  PIC X(37).                               
010000     03  W-MSC-TRANS             PIC X(8).                                
010110     03  FILLER                  PIC X(17).                               
010120     03  W-MSC-TEXT              PIC X(30).                               
010130     03  FILLER                  PIC X(41).                               
010200*                                                                         
010300 01  W-MSC-AREA-2 REDEFINES W-MSC-AREA.                                   
010400     03  FILLER                  PIC X(37).                               
010500     03  W-TRANSACTION2          PIC X(11).                               
010600     03  FILLER                  PIC X(3).                                
010700     03  W-ACCOUNT2              PIC X(7).                                
010800     03  FILLER                  PIC X(15).                               
010900     03  W-COMMENTS2             PIC X(8).                                
011000     03  FILLER                  PIC X(52).                               
011100*                                                                         
011200 01  W-UID.                                                               
011300*                                                                         
011400     03  W-BOLAG             PIC X(5).                                    
011500     03  W-AVD               PIC X(5).                                    
011600     03  W-JOBFUNC           PIC X(2).                                    
011700     03  W-DIVMISC           PIC X(3).                                    
011800     03  FILLER              PIC X(4).                                    
011900     03  W-ANSTNR            PIC X(5).                                    
012000*                                                                         
012100 01  UT-AREA.                                                             
012200*                                                                         
012300     03  UT-TRANS            PIC X(8)   VALUE SPACE.                      
012400     03  UT-BESKR            PIC X(30)  VALUE SPACE.                      
012500     03  UT-BOLAG            PIC X(3)   VALUE SPACE.                      
012600     03  UT-LAND             PIC X(2)   VALUE SPACE.                      
012700     03  UT-JOBFUNC          PIC X(2)   VALUE SPACE.                      
012800     03  UT-DIVMISC          PIC X(3)   VALUE SPACE.                      
012900     03  UT-ANSTNR           PIC X(5)   VALUE SPACE.                      
013000     03  FILLER              PIC X(7)   VALUE SPACE.                      
013100                                                                          
013200 01  TRANSACTION-TXT         PIC X(11) VALUE 'TRANSACTION'.               
013300 01  ACCOUNT-TXT             PIC X(7)  VALUE 'ACCOUNT'.                   
013400 01  COMMENTS-TXT            PIC X(8)  VALUE 'COMMENTS'.                  
013500 01  W-TRANS-BESKR           PIC X(30).                                   
013600 01  W-ACCEPT                PIC X(30).                                   
013700 01  W-SKRAEP                PIC X(1).                                    
013800 01  W-SKRAEPP               PIC X(1).                                    
013900 01  W-GRUPPP                PIC X(8).                                    
014000 01  UID-PTR                 PIC S9(4) COMP-3.                            
014100 01  STR-PTR                 PIC S9(4) COMP-3.                            
014200*                                                                         
014300 01  GENERELLA-SUBPGM.                                                    
014400*                                                                         
014500     03  ABEND               PIC X(8)    VALUE 'ABEND'.                   
014600     03  W009PDSR            PIC X(8)    VALUE 'W009PDSR'.                
014700     EJECT                                                                
014800* --- PARAMETRAR TILL ABEND                                               
014900 01  RKOD-ABEND-UTAN-DUMP    PIC S9(4)   COMP VALUE +16.                  
015000 01  RKOD-ABEND-MED-DUMP     PIC S9(4)   COMP VALUE +1000.                
015100*                                                                         
015200* --- PARAMETRAR TILL W009PDSR                                            
015300 01  DDMBR.                                                               
015400     03  DDNAMN              PIC X(8)  VALUE 'RACF2DD'.                   
015500     03  W-GRUPP             PIC X(8)  VALUE SPACE.                       
015600     SKIP2                                                                
015700 01  GRUPP-POST              PIC X(80) VALUE SPACE.                       
015800*                                                                         
015900 01  GRUPP-POST2  REDEFINES GRUPP-POST.                                   
016000*                                                                         
016100     05  FILLER              PIC X.                                       
016200     05  GRUPP-UID-TEXT      PIC X(4).                                    
016300     05  GRUPP-UID-STR       PIC X(40).                                   
016400     05  FILLER              PIC X(35).                                   
016500     SKIP2                                                                
016600 77  FUNK-OPEN               PIC X     VALUE 'O'.                         
016700 77  FUNK-READ               PIC X     VALUE SPACE.                       
016800 77  FUNK-CLOSE              PIC X     VALUE 'C'.                         
016900*                                                                         
017000 01  GENERELLA-KONSTANTER.                                                
017100*                                                                         
017200     03  JA                  PIC X(1)    VALUE 'Y'.                       
017300     03  NEJ                 PIC X(1)    VALUE 'N'.                       
017400     03  ASTERISKER          PIC X(24)   VALUE                            
017500                             '************************'.                  
017600                                                                          
017700 01  REGELFIL-EOF            PIC X(1)    VALUE 'N'.                       
017800 01  BESKRFIL-EOF            PIC X(1)    VALUE 'N'.                       
017900 01  MSCFIL-EOF              PIC X(1)    VALUE 'N'.                       
018000*                                                                         
018100 01  MEDLEM-OK               PIC X(8).                                    
018110*                                                                         
018120 01  OLD-MEDLEM.                                                          
018200     03  FILLER              PIC X(2).                                    
018300     03  OLD-MEDLEM-3        PIC X(1).                                    
018400     03  FILLER              PIC X(5).                                    
018500     EJECT                                                                
018600*                                                                         
018700     SKIP2                                                                
018800 01  RETURKODER.                                                          
018900*                                                                         
019000     03  RKOD                PIC S9(4)   COMP SYNC VALUE ZERO.            
019100     EJECT                                                                
019200 PROCEDURE DIVISION.                                                      
019300*                                                                         
019400     PERFORM A-INIT                                                       
019500     PERFORM S11-LAES-BESKRFIL                                            
019600     PERFORM B-LAES-FRAM-BESKRFIL                                         
019700     PERFORM S12-LAES-MSCFIL                                              
019800     PERFORM C-LAES-FRAM-MSCFIL                                           
019900     PERFORM UNTIL REGELFIL-EOF = JA                                      
020000       IF PDSSCAN-ORD = 'PDSSCAN' AND MEDLEM-ORD = 'MEDLEM:'              
020100         IF MEDLEM NOT = OLD-MEDLEM                                       
020200           IF MEDLEM = W-BESKR-TRANS                                      
020300             IF MEDLEM = W-MSC-TRANS                                      
020400               MOVE MEDLEM TO OLD-MEDLEM MEDLEM-OK                        
020500               MOVE W-BESKR-TEXT TO W-TRANS-BESKR                         
020600               PERFORM S10-LAES-REGELFIL                                  
020700             ELSE                                                         
020800               IF (W-TRANSACTION2 = TRANSACTION-TXT AND                   
020900                  W-ACCOUNT2      = ACCOUNT-TXT AND                       
021000                  W-COMMENTS2     = COMMENTS-TXT) OR                      
021100                  (W-TRANSACTION2  = ALL '-' OR SPACES)                   
021200                 PERFORM S12-LAES-MSCFIL                                  
021300               ELSE                                                       
021400                 IF MEDLEM < W-MSC-TRANS                                  
021410                   MOVE SPACE TO UT-AREA                                  
021500                   MOVE MEDLEM TO OLD-MEDLEM UT-TRANS                     
021600                   MOVE SPACE TO W-TRANS-BESKR                            
021700                   STRING 'ingen msc '                                    
021800                          W-BESKR-TEXT DELIMITED BY SIZE                  
021900                          INTO W-TRANS-BESKR                              
021910                   IF OLD-MEDLEM-3 = 'T'                                  
021920                     MOVE W-BESKR-TEXT TO UT-BESKR                        
022000                     MOVE 'V2N' TO UT-BOLAG                               
022100                     MOVE 'OM'  TO UT-LAND                                
022200                     MOVE 'SC'  TO UT-JOBFUNC                             
022300                     PERFORM S20-SKRIV-UTFIL                              
022310                   END-IF                                                 
022400                   PERFORM S10-LAES-REGELFIL                              
022500                 ELSE                                                     
022510                   IF W-MSC-TRANS (3:1) = 'T'                             
022511                   AND (W-MSC-TRANS NOT = OLD-MEDLEM)                     
022512                   AND (W-MSC-TRANS NOT = MEDLEM-OK)                      
022520                     MOVE SPACE TO UT-AREA                                
022530                     MOVE W-MSC-TRANS TO UT-TRANS                         
022540                     MOVE W-MSC-TEXT  TO UT-BESKR                         
022550                     MOVE 'V2N' TO UT-BOLAG                               
022560                     MOVE 'OR'  TO UT-LAND                                
022570                     MOVE 'ES'  TO UT-JOBFUNC                             
022591                     PERFORM S20-SKRIV-UTFIL                              
022592                   END-IF                                                 
022594                   PERFORM S12-LAES-MSCFIL                                
022700                 END-IF                                                   
022800               END-IF                                                     
022900             END-IF                                                       
023000           ELSE                                                           
023100             IF BESKRFIL-EOF = JA                                         
023110               MOVE SPACE TO UT-AREA                                      
023200               MOVE MEDLEM TO OLD-MEDLEM UT-TRANS                         
023300               MOVE 'okänd trans' TO W-TRANS-BESKR UT-BESKR               
023310               IF MEDLEM (3:1) = 'T'                                      
023320                 MOVE 'V2N' TO UT-BOLAG                                   
023330                 MOVE 'OG'  TO UT-LAND                                    
023340                 MOVE 'EN'  TO UT-JOBFUNC                                 
023360                 PERFORM S20-SKRIV-UTFIL                                  
023370               END-IF                                                     
023400               PERFORM S10-LAES-REGELFIL                                  
023500             ELSE                                                         
023600               IF (W-TRANSACTION = TRANSACTION-TXT AND                    
023700                  W-ACCOUNT = ACCOUNT-TXT AND                             
023800                  W-COMMENTS = COMMENTS-TXT) OR                           
023900                  (W-TRANSACTION = ALL '-' OR SPACES)                     
024000                 PERFORM S11-LAES-BESKRFIL                                
024100               ELSE                                                       
024200                 IF MEDLEM < W-BESKR-TRANS                                
024210                   MOVE SPACE TO UT-AREA                                  
024300                   MOVE MEDLEM TO OLD-MEDLEM UT-TRANS                     
024400                   MOVE 'okänd trans' TO W-TRANS-BESKR UT-BESKR           
024410                   IF MEDLEM (3:1) = 'T'                                  
024500                     MOVE 'V2N' TO UT-BOLAG                               
024600                     MOVE 'OG'  TO UT-LAND                                
024700                     MOVE 'EN'  TO UT-JOBFUNC                             
024800                     PERFORM S20-SKRIV-UTFIL                              
024810                   END-IF                                                 
024900                   PERFORM S10-LAES-REGELFIL                              
025000                 ELSE                                                     
025340                   PERFORM S11-LAES-BESKRFIL                              
025600                 END-IF                                                   
025700               END-IF                                                     
025800             END-IF                                                       
025900           END-IF                                                         
026000         ELSE                                                             
026100           PERFORM S10-LAES-REGELFIL                                      
026200         END-IF                                                           
026300       ELSE                                                               
026400         IF (OLD-MEDLEM-3 = 'T' AND UID-TEXT = 'UID(' )                   
026410         AND (OLD-MEDLEM (7:1) NOT = 'V' AND 'X')                         
026500           PERFORM D-KONTROLLERA-UID                                      
026600         END-IF                                                           
026700         PERFORM S10-LAES-REGELFIL                                        
026800       END-IF                                                             
026900     END-PERFORM                                                          
027000                                                                          
027100     PERFORM Z-FINIT                                                      
027200     MOVE RKOD TO RETURN-CODE                                             
027300     GOBACK.                                                              
027400     EJECT                                                                
027500 A-INIT  SECTION.                                                         
027600     SKIP2                                                                
027700     OPEN INPUT REGELFIL BESKRFIL MSCFIL                                  
027800                                                                          
027900     OPEN OUTPUT UTFIL                                                    
028000     MOVE NEJ TO REGELFIL-EOF BESKRFIL-EOF MSCFIL-EOF                     
028100     MOVE SPACE TO OLD-MEDLEM                                             
028200     .                                                                    
028300     EJECT                                                                
028400 B-LAES-FRAM-BESKRFIL  SECTION.                                           
028500     SKIP2                                                                
028600     PERFORM S11-LAES-BESKRFIL                                            
028700     PERFORM UNTIL BESKRFIL-EOF = JA                                      
028800              OR (W-TRANSACTION = TRANSACTION-TXT                         
028900                AND W-ACCOUNT = ACCOUNT-TXT                               
029000                AND W-COMMENTS = COMMENTS-TXT)                            
029100       PERFORM S11-LAES-BESKRFIL                                          
029200     END-PERFORM                                                          
029300     .                                                                    
029400     EJECT                                                                
029500 C-LAES-FRAM-MSCFIL  SECTION.                                             
029600     SKIP2                                                                
029700     PERFORM S12-LAES-MSCFIL                                              
029800     PERFORM UNTIL MSCFIL-EOF = JA                                        
029900              OR (W-TRANSACTION2 = TRANSACTION-TXT                        
030000                AND W-ACCOUNT2 = ACCOUNT-TXT                              
030100                AND W-COMMENTS2 = COMMENTS-TXT)                           
030200       PERFORM S12-LAES-MSCFIL                                            
030300     END-PERFORM                                                          
030400     .                                                                    
030500     EJECT                                                                
030600 D-KONTROLLERA-UID   SECTION.                                             
030700     SKIP2                                                                
030800     MOVE SPACE      TO UT-AREA                                           
030900     MOVE OLD-MEDLEM TO UT-TRANS                                          
031000     MOVE SPACE      TO W-UID W-ACCEPT                                    
031100     MOVE +1         TO UID-PTR                                           
031200                                                                          
031300     UNSTRING UID-STR DELIMITED BY ')' INTO W-UID                         
031400              WITH POINTER UID-PTR                                        
031500                                                                          
031600     MOVE UID-PTR TO STR-PTR                                              
031700     SUBTRACT 1 FROM STR-PTR                                              
031800     STRING   ASTERISKER DELIMITED BY SIZE                                
031900              INTO W-UID WITH POINTER STR-PTR                             
032000                                                                          
032100     UNSTRING UID-STR INTO W-ACCEPT                                       
032200              WITH POINTER UID-PTR                                        
032300                                                                          
032400     IF W-ACCEPT(1:5) = 'ALLOW' OR W-ACCEPT(2:5) = 'ALLOW'                
032500       PERFORM E-SKAPA-UTPOST                                             
032600     ELSE                                                                 
032700       IF W-ACCEPT(1:18) = 'PREVENT DATA(ÅKEY('                           
032800         OR W-ACCEPT(2:18) = 'PREVENT DATA(ÅKEY('                         
032900         PERFORM DA-KONTROLLERA-GRUPP                                     
033000       END-IF                                                             
033100     END-IF                                                               
033200     .                                                                    
033300     EJECT                                                                
033400 DA-KONTROLLERA-GRUPP   SECTION.                                          
033500     SKIP2                                                                
033600     MOVE SPACE TO W-GRUPP W-GRUPPP                                       
033700     UNSTRING W-ACCEPT DELIMITED BY ALL '('                               
033800              INTO W-SKRAEP W-SKRAEPP W-GRUPPP                            
033900                                                                          
034000     UNSTRING W-GRUPPP DELIMITED BY ')'                                   
034100              INTO W-GRUPP                                                
034200                                                                          
034300                                                                          
034400       CALL W009PDSR USING DDMBR FUNK-OPEN                                
034500       IF RETURN-CODE = 8                                                 
034600       DISPLAY 'HITTAR INTE GRUPP ' W-GRUPP ' MEDLEM ' UT-TRANS           
034700       ELSE                                                               
034800         IF RETURN-CODE = 12                                              
034900           DISPLAY 'KAN EJ ÖPPNA ACF2-BIBLIOTEKETET'                      
035000           PERFORM S99-ABEND                                              
035100         ELSE                                                             
035200                                                                          
035300           CALL W009PDSR USING DDMBR FUNK-READ GRUPP-POST                 
035400           PERFORM UNTIL RETURN-CODE > 0                                  
035500             IF GRUPP-UID-TEXT = 'UID('                                   
035600               PERFORM DAA-KONTROLLERA-GRUPP-UID                          
035700             END-IF                                                       
035800             CALL W009PDSR USING DDMBR FUNK-READ GRUPP-POST               
035900           END-PERFORM                                                    
036000           CALL W009PDSR USING DDMBR FUNK-CLOSE                           
036100           IF RETURN-CODE > 0                                             
036200             DISPLAY 'KAN EJ STÄNGA ACF2-BIBLIOTEKET'                     
036300             PERFORM S99-ABEND                                            
036400           END-IF                                                         
036500         END-IF                                                           
036600       END-IF                                                             
036700                                                                          
036800     .                                                                    
036900     EJECT                                                                
037000 DAA-KONTROLLERA-GRUPP-UID   SECTION.                                     
037100     SKIP2                                                                
037200     MOVE SPACE      TO W-UID W-ACCEPT                                    
037300     MOVE +1         TO UID-PTR                                           
037400                                                                          
037500     UNSTRING GRUPP-UID-STR DELIMITED BY ')' INTO W-UID                   
037600              WITH POINTER UID-PTR                                        
037700                                                                          
037800     MOVE UID-PTR TO STR-PTR                                              
037900     SUBTRACT 1 FROM STR-PTR                                              
038000     STRING   ASTERISKER DELIMITED BY SIZE                                
038100              INTO W-UID WITH POINTER STR-PTR                             
038200                                                                          
038300     UNSTRING GRUPP-UID-STR INTO W-ACCEPT                                 
038400              WITH POINTER UID-PTR                                        
038500                                                                          
038600     IF W-ACCEPT(1:5) = 'ALLOW' OR W-ACCEPT(2:5) = 'ALLOW'                
038700       PERFORM E-SKAPA-UTPOST                                             
038800     END-IF                                                               
038900     .                                                                    
039000     EJECT                                                                
039100 E-SKAPA-UTPOST   SECTION.                                                
039200     SKIP2                                                                
039300     MOVE SPACE TO UT-JOBFUNC UT-DIVMISC UT-LAND                          
039400     EVALUATE TRUE                                                        
039500                                                                          
039600       WHEN (W-BOLAG = 'AC GP' OR 'AP') AND                               
039700            W-JOBFUNC = 'IS'                                              
039800         CONTINUE                                                         
039900                                                                          
040000       WHEN W-BOLAG (1:4) = 'AC G'                                        
040100         MOVE 'ACG' TO UT-BOLAG                                           
040200         MOVE W-TRANS-BESKR TO UT-BESKR                                   
040300         IF W-ANSTNR = '*****'                                            
040400           MOVE SPACE TO UT-ANSTNR                                        
040500         ELSE                                                             
040600           MOVE 'ind' TO UT-ANSTNR                                        
040700         END-IF                                                           
040800         PERFORM S20-SKRIV-UTFIL                                          
040900                                                                          
041000       WHEN W-BOLAG (1:2) = 'AC'                                          
041100         MOVE W-BOLAG (1:2) TO UT-BOLAG                                   
041200         MOVE W-TRANS-BESKR TO UT-BESKR                                   
041300         IF W-ANSTNR = '*****'                                            
041400           MOVE SPACE TO UT-ANSTNR                                        
041500         ELSE                                                             
041600           MOVE 'ind' TO UT-ANSTNR                                        
041700         END-IF                                                           
041800         PERFORM S20-SKRIV-UTFIL                                          
041900                                                                          
042000       WHEN W-BOLAG (1:3) = 'AVU'                                         
042100         MOVE W-BOLAG (1:3) TO UT-BOLAG                                   
042200         MOVE W-TRANS-BESKR TO UT-BESKR                                   
042300         IF W-ANSTNR = '*****'                                            
042400           MOVE SPACE TO UT-ANSTNR                                        
042500         ELSE                                                             
042600           MOVE 'ind' TO UT-ANSTNR                                        
042700         END-IF                                                           
042800         PERFORM S20-SKRIV-UTFIL                                          
042900                                                                          
044110       WHEN OTHER                                                         
044120         MOVE W-BOLAG(1:3) TO UT-BOLAG                                    
044130         MOVE W-TRANS-BESKR TO UT-BESKR                                   
044140         MOVE W-BOLAG(4:2) TO UT-LAND                                     
044150         MOVE W-JOBFUNC TO UT-JOBFUNC                                     
044160         IF W-ANSTNR = '*****'                                            
044170           MOVE SPACE TO UT-ANSTNR                                        
044180         ELSE                                                             
044190           MOVE 'ind' TO UT-ANSTNR                                        
044191         END-IF                                                           
044192         PERFORM S20-SKRIV-UTFIL                                          
044193                                                                          
044400     END-EVALUATE                                                         
044500     .                                                                    
044600     EJECT                                                                
044700 Z-FINIT  SECTION.                                                        
044800     SKIP2                                                                
044900     CLOSE REGELFIL                                                       
045000           BESKRFIL                                                       
045100           MSCFIL                                                         
045200           UTFIL                                                          
045300     .                                                                    
045400 S10-LAES-REGELFIL SECTION.                                               
045500     SKIP2                                                                
045600     READ REGELFIL INTO W-REGEL-AREA                                      
045700       AT END MOVE JA TO REGELFIL-EOF                                     
045800     .                                                                    
045900 S11-LAES-BESKRFIL SECTION.                                               
046000     SKIP2                                                                
046100     READ BESKRFIL INTO W-BESKR-AREA                                      
046200       AT END                                                             
046300         MOVE JA TO BESKRFIL-EOF                                          
046400         MOVE '99999999' TO W-BESKR-TRANS                                 
046500       END-READ                                                           
046600     .                                                                    
046700 S12-LAES-MSCFIL SECTION.                                                 
046800     SKIP2                                                                
046900     READ MSCFIL INTO W-MSC-AREA                                          
047000       AT END                                                             
047100         MOVE JA TO MSCFIL-EOF                                            
047200         MOVE '99999999' TO W-MSC-TRANS                                   
047300       END-READ                                                           
047400     .                                                                    
047500 S20-SKRIV-UTFIL  SECTION.                                                
047600     SKIP2                                                                
047700     WRITE UT-POST   FROM UT-AREA                                         
047800     .                                                                    
047900 S99-ABEND  SECTION.                                                      
048000     SKIP2                                                                
048100     CALL ABEND USING RKOD-ABEND-UTAN-DUMP                                
048200     .                                                                    
