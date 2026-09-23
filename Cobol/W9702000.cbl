000100 ID  DIVISION.                                                            
000200     SKIP2                                                                
000300 PROGRAM-ID.  W9702000.                                                   
000400     SKIP2                                                                
000500 AUTHOR.        KARIN OLSSON.                                             
000600     DATE-WRITTEN.  JAN 1993.                                             
000700*                                                                         
000800*                                                                         
000900*    PLOCKAR UT INFORMATION UR ACF2-TRANSMEDLEMMAR                        
001000*                                                                         
001100*    FIX 930420. OM IMPORTÖR ELLER ÅTERFÖRSÄLJARE SÅ                      
001200*                LÄGGS DIVMISC POS 2+3 I JOBFUNC-FÄLTET.                  
001300*                                                                         
001400     EJECT                                                                
001500 ENVIRONMENT DIVISION.                                                    
001600     SKIP2                                                                
001700 INPUT-OUTPUT  SECTION.                                                   
001800*                                                                         
001900 FILE-CONTROL.                                                            
002000*                                                                         
002100     SELECT REGELFIL        ASSIGN TO W97020D1.                           
002200*                                                                         
002300     SELECT BESKRFIL        ASSIGN TO W97020D2.                           
002400*                                                                         
002500     SELECT UTFIL           ASSIGN TO W97020D3.                           
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
004500 FD  UTFIL                                                                
004600     LABEL RECORD   STANDARD                                              
004700     RECORDING      F                                                     
004800     BLOCK CONTAINS 0.                                                    
004900                                                                          
005000 01  UT-POST                 PIC X(60).                                   
005100*                                                                         
005200     EJECT                                                                
005300 WORKING-STORAGE  SECTION.                                                
005301                                                                          
005310*    -- CHECKED BY WY2000                                                 
005400*                                                                         
005500 01  W-REGEL-AREA.                                                        
005600     03  W-REGEL-RAD             PIC X(121).                              
005700*                                                                         
005800     03  W-REGEL-AREA1 REDEFINES W-REGEL-RAD.                             
005900*                                                                         
006000         05  FILLER              PIC X(2).                                
006100         05  PDSSCAN-ORD         PIC X(7).                                
006200         05  FILLER              PIC X(70).                               
006300         05  MEDLEM-ORD          PIC X(7).                                
006400         05  FILLER              PIC X.                                   
006500         05  MEDLEM              PIC X(8).                                
006600         05  FILLER              PIC X(26).                               
006700*                                                                         
006800     03  W-REGEL-AREA2 REDEFINES W-REGEL-RAD.                             
006900*                                                                         
007000         05  FILLER              PIC X(10).                               
007100         05  FILLER              PIC X.                                   
007200         05  UID-TEXT            PIC X(4).                                
007300         05  UID-STR             PIC X(40).                               
007400         05  FILLER              PIC X(66).                               
007500*                                                                         
007600 01  W-BESKR-AREA.                                                        
007700     03  FILLER                  PIC X(37).                               
007800     03  W-BESKR-TRANS           PIC X(8).                                
007900     03  FILLER                  PIC X(17).                               
008000     03  W-BESKR-TEXT            PIC X(30).                               
008100     03  FILLER                  PIC X(41).                               
008200*                                                                         
008300 01  W-BESKR-AREA-2 REDEFINES W-BESKR-AREA.                               
008400     03  FILLER                  PIC X(37).                               
008500     03  W-TRANSACTION           PIC X(11).                               
008600     03  FILLER                  PIC X(3).                                
008700     03  W-ACCOUNT               PIC X(7).                                
008800     03  FILLER                  PIC X(15).                               
008900     03  W-COMMENTS              PIC X(8).                                
009000     03  FILLER                  PIC X(52).                               
009100*                                                                         
009200 01  W-UID.                                                               
009300*                                                                         
009400     03  W-BOLAG             PIC X(5).                                    
009500     03  W-AVD               PIC X(5).                                    
009600     03  W-JOBFUNC           PIC X(2).                                    
009700     03  W-DIVMISC           PIC X(3).                                    
009800     03  FILLER              PIC X(4).                                    
009900     03  W-ANSTNR            PIC X(5).                                    
010000*                                                                         
010100 01  UT-AREA.                                                             
010200*                                                                         
010300     03  UT-TRANS            PIC X(8)   VALUE SPACE.                      
010400     03  UT-BESKR            PIC X(30)  VALUE SPACE.                      
010500     03  UT-BOLAG            PIC X(3)   VALUE SPACE.                      
010600     03  UT-LAND             PIC X(2)   VALUE SPACE.                      
010700     03  UT-JOBFUNC          PIC X(2)   VALUE SPACE.                      
010800     03  UT-DIVMISC          PIC X(3)   VALUE SPACE.                      
010900     03  UT-ANSTNR           PIC X(5)   VALUE SPACE.                      
011000     03  FILLER              PIC X(7)   VALUE SPACE.                      
011100                                                                          
011200 01  TRANSACTION-TXT         PIC X(11) VALUE 'TRANSACTION'.               
011300 01  ACCOUNT-TXT             PIC X(7)  VALUE 'ACCOUNT'.                   
011400 01  COMMENTS-TXT            PIC X(8)  VALUE 'COMMENTS'.                  
011500 01  W-TRANS-BESKR           PIC X(30).                                   
011600 01  W-ACCEPT                PIC X(30).                                   
011700 01  W-SKRAEP                PIC X(1).                                    
011800 01  W-SKRAEPP               PIC X(1).                                    
011900 01  W-GRUPPP                PIC X(8).                                    
012000 01  UID-PTR                 PIC S9(4) COMP-3.                            
012100 01  STR-PTR                 PIC S9(4) COMP-3.                            
012200*                                                                         
012300 01  GENERELLA-SUBPGM.                                                    
012400*                                                                         
012500     03  ABEND               PIC X(8)    VALUE 'ABEND'.                   
012600     03  W009PDSR            PIC X(8)    VALUE 'W009PDSR'.                
012700     EJECT                                                                
012800* --- PARAMETRAR TILL ABEND                                               
012900 01  RKOD-ABEND-UTAN-DUMP    PIC S9(4)   COMP VALUE +16.                  
013000 01  RKOD-ABEND-MED-DUMP     PIC S9(4)   COMP VALUE +1000.                
013100*                                                                         
013200* --- PARAMETRAR TILL W009PDSR                                            
013300 01  DDMBR.                                                               
013400     03  DDNAMN              PIC X(8)  VALUE 'RACF2DD'.                   
013500     03  W-GRUPP             PIC X(8)  VALUE SPACE.                       
013600     SKIP2                                                                
013700 01  GRUPP-POST              PIC X(80) VALUE SPACE.                       
013800*                                                                         
013900 01  GRUPP-POST2  REDEFINES GRUPP-POST.                                   
014000*                                                                         
014100     05  FILLER              PIC X.                                       
014200     05  GRUPP-UID-TEXT      PIC X(4).                                    
014300     05  GRUPP-UID-STR       PIC X(40).                                   
014400     05  FILLER              PIC X(35).                                   
014500     SKIP2                                                                
014600 77  FUNK-OPEN               PIC X     VALUE 'O'.                         
014700 77  FUNK-READ               PIC X     VALUE SPACE.                       
014800 77  FUNK-CLOSE              PIC X     VALUE 'C'.                         
014900*                                                                         
015000 01  GENERELLA-KONSTANTER.                                                
015100*                                                                         
015200     03  JA                  PIC X(1)    VALUE 'Y'.                       
015300     03  NEJ                 PIC X(1)    VALUE 'N'.                       
015400     03  ASTERISKER          PIC X(24)   VALUE                            
015500                             '************************'.                  
015600                                                                          
015700 01  REGELFIL-EOF            PIC X(1)    VALUE 'N'.                       
015800 01  BESKRFIL-EOF            PIC X(1)    VALUE 'N'.                       
015900*                                                                         
016000 01  OLD-MEDLEM.                                                          
016100     03  FILLER              PIC X(2).                                    
016200     03  OLD-MEDLEM-3        PIC X(1).                                    
016300     03  FILLER              PIC X(5).                                    
016400     EJECT                                                                
016500*                                                                         
016600     SKIP2                                                                
016700 01  RETURKODER.                                                          
016800*                                                                         
016900     03  RKOD                PIC S9(4)   COMP SYNC VALUE ZERO.            
017000     EJECT                                                                
017100 PROCEDURE DIVISION.                                                      
017200*                                                                         
017300     PERFORM A-INIT                                                       
017400     PERFORM S11-LAES-BESKRFIL                                            
017500     PERFORM B-LAES-FRAM-BESKRFIL                                         
017600     PERFORM UNTIL REGELFIL-EOF = JA                                      
017700       IF PDSSCAN-ORD = 'PDSSCAN' AND MEDLEM-ORD = 'MEDLEM:'              
017800         IF MEDLEM NOT = OLD-MEDLEM                                       
017900           IF MEDLEM = W-BESKR-TRANS                                      
018000             MOVE MEDLEM TO OLD-MEDLEM                                    
018100             MOVE W-BESKR-TEXT TO W-TRANS-BESKR                           
018200             PERFORM S10-LAES-REGELFIL                                    
018300           ELSE                                                           
018400             IF BESKRFIL-EOF = JA                                         
018500               MOVE SPACE TO UT-AREA                                      
018600               MOVE MEDLEM TO OLD-MEDLEM UT-TRANS                         
018700               MOVE 'okänd trans' TO W-TRANS-BESKR UT-BESKR               
018800               IF OLD-MEDLEM-3 = 'T' OR OLD-MEDLEM (1:4) = 'W903'         
018900                 MOVE 'V1N' TO UT-BOLAG                                   
019000                 MOVE 'OG'  TO UT-LAND                                    
019100                 MOVE 'EN'  TO UT-JOBFUNC                                 
019200                 PERFORM S20-SKRIV-UTFIL                                  
019300               END-IF                                                     
019400               PERFORM S10-LAES-REGELFIL                                  
019500             ELSE                                                         
019700               IF (W-TRANSACTION = TRANSACTION-TXT AND                    
019800                  W-ACCOUNT = ACCOUNT-TXT AND                             
019900                  W-COMMENTS = COMMENTS-TXT) OR                           
020000                  (W-TRANSACTION = '-' OR SPACES)                         
020100                 PERFORM S11-LAES-BESKRFIL                                
020200               ELSE                                                       
020300                 IF MEDLEM < W-BESKR-TRANS                                
020400                   MOVE SPACE TO UT-AREA                                  
020500                   MOVE MEDLEM TO OLD-MEDLEM UT-TRANS                     
020600                   MOVE 'okänd trans' TO W-TRANS-BESKR UT-BESKR           
020700                   IF OLD-MEDLEM-3 = 'T'                                  
020800                   OR OLD-MEDLEM (1:4) = 'W903'                           
020900                     MOVE 'V1N' TO UT-BOLAG                               
021000                     MOVE 'OG'  TO UT-LAND                                
021100                     MOVE 'EN'  TO UT-JOBFUNC                             
021200                     PERFORM S20-SKRIV-UTFIL                              
021300                   END-IF                                                 
021400                   PERFORM S10-LAES-REGELFIL                              
021500                 ELSE                                                     
021600                   IF W-BESKR-TRANS (3:1) = 'T'                           
021610                   AND W-BESKR-TRANS NOT = OLD-MEDLEM                     
021700                     MOVE SPACE TO UT-AREA                                
021800                     MOVE W-BESKR-TRANS TO UT-TRANS                       
021900                     MOVE W-BESKR-TEXT  TO UT-BESKR                       
022000                     MOVE 'V1N' TO UT-BOLAG                               
022100                     MOVE 'OR'  TO UT-LAND                                
022200                     MOVE 'ES'  TO UT-JOBFUNC                             
022300                     MOVE SPACE TO UT-DIVMISC                             
022600                     PERFORM S20-SKRIV-UTFIL                              
022700                   END-IF                                                 
022900                   PERFORM S11-LAES-BESKRFIL                              
023100                 END-IF                                                   
023200               END-IF                                                     
023300             END-IF                                                       
023400           END-IF                                                         
023500         ELSE                                                             
023600           PERFORM S10-LAES-REGELFIL                                      
023700         END-IF                                                           
023800       ELSE                                                               
023900         IF (OLD-MEDLEM-3 = 'T' OR OLD-MEDLEM (1:4) = 'W903')             
023901         AND (OLD-MEDLEM (7:1) NOT = 'X')                                 
023910         AND UID-TEXT = 'UID('                                            
024000           PERFORM C-KONTROLLERA-UID                                      
024100         END-IF                                                           
024200         PERFORM S10-LAES-REGELFIL                                        
024300       END-IF                                                             
024400     END-PERFORM                                                          
024500                                                                          
024600     PERFORM Z-FINIT                                                      
024700     MOVE RKOD TO RETURN-CODE                                             
024800     GOBACK.                                                              
024900     EJECT                                                                
025000 A-INIT  SECTION.                                                         
025100     SKIP2                                                                
025200     OPEN INPUT REGELFIL BESKRFIL                                         
025300                                                                          
025400     OPEN OUTPUT UTFIL                                                    
025500     MOVE NEJ TO REGELFIL-EOF                                             
025600     MOVE SPACE TO OLD-MEDLEM                                             
025700     .                                                                    
025800     EJECT                                                                
025900 B-LAES-FRAM-BESKRFIL  SECTION.                                           
026000     SKIP2                                                                
026100     PERFORM S11-LAES-BESKRFIL                                            
026200     PERFORM UNTIL BESKRFIL-EOF = JA                                      
026300              OR (W-TRANSACTION = TRANSACTION-TXT                         
026400                AND W-ACCOUNT = ACCOUNT-TXT                               
026500                AND W-COMMENTS = COMMENTS-TXT)                            
026600       PERFORM S11-LAES-BESKRFIL                                          
026700     END-PERFORM                                                          
026800     .                                                                    
026900     EJECT                                                                
027000 C-KONTROLLERA-UID   SECTION.                                             
027100     SKIP2                                                                
027200     MOVE SPACE      TO UT-AREA                                           
027300     MOVE OLD-MEDLEM TO UT-TRANS                                          
027400     MOVE SPACE      TO W-UID W-ACCEPT                                    
027500     MOVE +1         TO UID-PTR                                           
027600                                                                          
027700     UNSTRING UID-STR DELIMITED BY ')' INTO W-UID                         
027800              WITH POINTER UID-PTR                                        
027900                                                                          
028000     MOVE UID-PTR TO STR-PTR                                              
028100     SUBTRACT 1 FROM STR-PTR                                              
028200     STRING   ASTERISKER DELIMITED BY SIZE                                
028300              INTO W-UID WITH POINTER STR-PTR                             
028400                                                                          
028500     UNSTRING UID-STR INTO W-ACCEPT                                       
028600              WITH POINTER UID-PTR                                        
028700                                                                          
028800     IF W-ACCEPT(1:5) = 'ALLOW' OR W-ACCEPT(2:5) = 'ALLOW'                
028900       PERFORM D-SKAPA-UTPOST                                             
029000     ELSE                                                                 
029100       IF W-ACCEPT(1:18) = 'PREVENT DATA(ÅKEY('                           
029200         OR W-ACCEPT(2:18) = 'PREVENT DATA(ÅKEY('                         
029300         PERFORM CA-KONTROLLERA-GRUPP                                     
029400       END-IF                                                             
029500     END-IF                                                               
029600     .                                                                    
029700     EJECT                                                                
029800 CA-KONTROLLERA-GRUPP   SECTION.                                          
029900     SKIP2                                                                
030000     MOVE SPACE TO W-GRUPP W-GRUPPP                                       
030100     UNSTRING W-ACCEPT DELIMITED BY ALL '('                               
030200              INTO W-SKRAEP W-SKRAEPP W-GRUPPP                            
030300                                                                          
030400     UNSTRING W-GRUPPP DELIMITED BY ')'                                   
030500              INTO W-GRUPP                                                
030600                                                                          
030700     EVALUATE TRUE                                                        
030800       WHEN W-GRUPP = 'WLVPARTS'                                          
030900         MOVE 'VTP' TO UT-BOLAG                                           
031000         MOVE W-TRANS-BESKR TO UT-BESKR                                   
031100         MOVE 'FR'  TO UT-JOBFUNC                                         
031200         MOVE 'SE'  TO UT-LAND                                            
031300         PERFORM S20-SKRIV-UTFIL                                          
031400                                                                          
031410       WHEN W-GRUPP = 'WBEPARTS'                                          
031420         MOVE 'VTP' TO UT-BOLAG                                           
031430         MOVE W-TRANS-BESKR TO UT-BESKR                                   
031440         MOVE 'FR'  TO UT-JOBFUNC                                         
031450         MOVE 'BE'  TO UT-LAND                                            
031460         PERFORM S20-SKRIV-UTFIL                                          
031470                                                                          
031480       WHEN W-GRUPP = 'WBRPARTS'                                          
031490         MOVE 'VTP' TO UT-BOLAG                                           
031491         MOVE W-TRANS-BESKR TO UT-BESKR                                   
031492         MOVE 'FR'  TO UT-JOBFUNC                                         
031493         MOVE 'BR'  TO UT-LAND                                            
031494         PERFORM S20-SKRIV-UTFIL                                          
031495                                                                          
031500       WHEN W-GRUPP = 'WPVPARTS'                                          
031600         MOVE 'VCP' TO UT-BOLAG                                           
031700         MOVE W-TRANS-BESKR TO UT-BESKR                                   
031800         MOVE 'FR'  TO UT-JOBFUNC                                         
031900         PERFORM S20-SKRIV-UTFIL                                          
032000                                                                          
032010       WHEN W-GRUPP = 'WPVLAGER'                                          
032020         MOVE 'VCP' TO UT-BOLAG                                           
032030         MOVE W-TRANS-BESKR TO UT-BESKR                                   
032040         MOVE 'LF'  TO UT-JOBFUNC                                         
032050         PERFORM S20-SKRIV-UTFIL                                          
032060                                                                          
032100       WHEN OTHER                                                         
032200         CALL W009PDSR USING DDMBR FUNK-OPEN                              
032300         IF RETURN-CODE = 8                                               
032400       DISPLAY 'HITTAR INTE GRUPP ' W-GRUPP ' MEDLEM ' UT-TRANS           
032500         ELSE                                                             
032600           IF RETURN-CODE = 12                                            
032700             DISPLAY 'KAN EJ ÖPPNA ACF2-BIBLIOTEKETET'                    
032800             PERFORM S99-ABEND                                            
032900           ELSE                                                           
033000                                                                          
033100             CALL W009PDSR USING DDMBR FUNK-READ GRUPP-POST               
033200             PERFORM UNTIL RETURN-CODE > 0                                
033300               IF GRUPP-UID-TEXT = 'UID('                                 
033400                 PERFORM CAA-KONTROLLERA-GRUPP-UID                        
033500               END-IF                                                     
033600               CALL W009PDSR USING DDMBR FUNK-READ GRUPP-POST             
033700             END-PERFORM                                                  
033800             CALL W009PDSR USING DDMBR FUNK-CLOSE                         
033900             IF RETURN-CODE > 0                                           
034000               DISPLAY 'KAN EJ STÄNGA ACF2-BIBLIOTEKET'                   
034100               PERFORM S99-ABEND                                          
034200             END-IF                                                       
034300           END-IF                                                         
034400         END-IF                                                           
034500                                                                          
034600     END-EVALUATE                                                         
034700     .                                                                    
034800     EJECT                                                                
034900 CAA-KONTROLLERA-GRUPP-UID   SECTION.                                     
035000     SKIP2                                                                
035100     MOVE SPACE      TO W-UID W-ACCEPT                                    
035200     MOVE +1         TO UID-PTR                                           
035300                                                                          
035400     UNSTRING GRUPP-UID-STR DELIMITED BY ')' INTO W-UID                   
035500              WITH POINTER UID-PTR                                        
035600                                                                          
035700     MOVE UID-PTR TO STR-PTR                                              
035800     SUBTRACT 1 FROM STR-PTR                                              
035900     STRING   ASTERISKER DELIMITED BY SIZE                                
036000              INTO W-UID WITH POINTER STR-PTR                             
036100                                                                          
036200     UNSTRING GRUPP-UID-STR INTO W-ACCEPT                                 
036300              WITH POINTER UID-PTR                                        
036400                                                                          
036500     IF W-ACCEPT(1:5) = 'ALLOW' OR W-ACCEPT(2:5) = 'ALLOW'                
036600       PERFORM D-SKAPA-UTPOST                                             
036700     END-IF                                                               
036800     .                                                                    
036900     EJECT                                                                
037000 D-SKAPA-UTPOST   SECTION.                                                
037100     SKIP2                                                                
037200     EVALUATE TRUE                                                        
037300                                                                          
037400       WHEN W-BOLAG(1:3) = 'AP '                                          
037500         MOVE 'VTP' TO UT-BOLAG                                           
037600         MOVE W-TRANS-BESKR TO UT-BESKR                                   
037700         MOVE W-BOLAG(4:2) TO UT-LAND                                     
037800         IF UT-LAND = SPACE                                               
037900           MOVE 'SE' TO UT-LAND                                           
038000         END-IF                                                           
038100         MOVE W-DIVMISC TO UT-DIVMISC                                     
038200         MOVE W-JOBFUNC TO UT-JOBFUNC                                     
038300         IF W-ANSTNR = '*****'                                            
038400           MOVE SPACE TO UT-ANSTNR                                        
038500         ELSE                                                             
038600           MOVE 'ind' TO UT-ANSTNR                                        
038700         END-IF                                                           
038800         PERFORM S20-SKRIV-UTFIL                                          
038900                                                                          
039000       WHEN W-BOLAG = 'AC GP'                                             
039100         MOVE 'VCP' TO UT-BOLAG                                           
039200         MOVE W-TRANS-BESKR TO UT-BESKR                                   
039300         MOVE W-DIVMISC TO UT-DIVMISC                                     
039400         MOVE W-JOBFUNC TO UT-JOBFUNC                                     
039500         IF W-ANSTNR = '*****'                                            
039600           MOVE SPACE TO UT-ANSTNR                                        
039700         ELSE                                                             
039800           MOVE 'ind' TO UT-ANSTNR                                        
039900         END-IF                                                           
040000         PERFORM S20-SKRIV-UTFIL                                          
040100                                                                          
040200       WHEN W-BOLAG(1:3) = 'AIC'                                          
040300         MOVE 'IMC' TO UT-BOLAG                                           
040400         MOVE W-TRANS-BESKR TO UT-BESKR                                   
040500         MOVE W-BOLAG(4:2) TO UT-LAND                                     
040600         MOVE W-DIVMISC TO UT-DIVMISC                                     
040700         MOVE W-JOBFUNC TO UT-JOBFUNC                                     
040800         IF W-DIVMISC NOT = '***'                                         
040900           MOVE W-DIVMISC (2:2) TO UT-JOBFUNC                             
041000         END-IF                                                           
041100         IF W-ANSTNR = '*****'                                            
041200           MOVE SPACE TO UT-ANSTNR                                        
041300         ELSE                                                             
041400           MOVE 'ind' TO UT-ANSTNR                                        
041500         END-IF                                                           
041600         PERFORM S20-SKRIV-UTFIL                                          
041700                                                                          
041800       WHEN W-BOLAG(1:3) = 'AIT'                                          
041900         MOVE 'IMT' TO UT-BOLAG                                           
042000         MOVE W-TRANS-BESKR TO UT-BESKR                                   
042100         MOVE W-BOLAG(4:2) TO UT-LAND                                     
042200         MOVE W-DIVMISC TO UT-DIVMISC                                     
042300         MOVE W-JOBFUNC TO UT-JOBFUNC                                     
042400         IF W-DIVMISC NOT = '***'                                         
042500           MOVE W-DIVMISC (2:2) TO UT-JOBFUNC                             
042600         END-IF                                                           
042700         IF W-ANSTNR = '*****'                                            
042800           MOVE SPACE TO UT-ANSTNR                                        
042900         ELSE                                                             
043000           MOVE 'ind' TO UT-ANSTNR                                        
043100         END-IF                                                           
043200         PERFORM S20-SKRIV-UTFIL                                          
043300                                                                          
043400       WHEN W-BOLAG(1:3) = 'AIV'                                          
043500         MOVE 'IMV' TO UT-BOLAG                                           
043600         MOVE W-TRANS-BESKR TO UT-BESKR                                   
043700         MOVE W-BOLAG(4:2) TO UT-LAND                                     
043800         MOVE W-DIVMISC TO UT-DIVMISC                                     
043900         MOVE W-JOBFUNC TO UT-JOBFUNC                                     
044000         IF W-DIVMISC NOT = '***'                                         
044100           MOVE W-DIVMISC (2:2) TO UT-JOBFUNC                             
044200         END-IF                                                           
044300         IF W-ANSTNR = '*****'                                            
044400           MOVE SPACE TO UT-ANSTNR                                        
044500         ELSE                                                             
044600           MOVE 'ind' TO UT-ANSTNR                                        
044700         END-IF                                                           
044800         PERFORM S20-SKRIV-UTFIL                                          
044900                                                                          
045000       WHEN W-BOLAG(1:3) = 'ADC'                                          
045100         MOVE 'AFC' TO UT-BOLAG                                           
045200         MOVE W-TRANS-BESKR TO UT-BESKR                                   
045300         MOVE W-BOLAG(4:2) TO UT-LAND                                     
045400         MOVE W-DIVMISC TO UT-DIVMISC                                     
045500         MOVE W-JOBFUNC TO UT-JOBFUNC                                     
045600         IF W-DIVMISC NOT = '***'                                         
045700           MOVE W-DIVMISC (2:2) TO UT-JOBFUNC                             
045800         END-IF                                                           
045900         IF W-ANSTNR = '*****'                                            
046000           MOVE SPACE TO UT-ANSTNR                                        
046100         ELSE                                                             
046200           MOVE 'ind' TO UT-ANSTNR                                        
046300         END-IF                                                           
046400         PERFORM S20-SKRIV-UTFIL                                          
046500                                                                          
046600       WHEN W-BOLAG(1:3) = 'ADT'                                          
046700         MOVE 'AFT' TO UT-BOLAG                                           
046800         MOVE W-TRANS-BESKR TO UT-BESKR                                   
046900         MOVE W-BOLAG(4:2) TO UT-LAND                                     
047000         MOVE W-DIVMISC TO UT-DIVMISC                                     
047100         MOVE W-JOBFUNC TO UT-JOBFUNC                                     
047200         IF W-DIVMISC NOT = '***'                                         
047300           MOVE W-DIVMISC (2:2) TO UT-JOBFUNC                             
047400         END-IF                                                           
047500         IF W-ANSTNR = '*****'                                            
047600           MOVE SPACE TO UT-ANSTNR                                        
047700         ELSE                                                             
047800           MOVE 'ind' TO UT-ANSTNR                                        
047900         END-IF                                                           
048000         PERFORM S20-SKRIV-UTFIL                                          
048100                                                                          
048200       WHEN W-BOLAG(1:3) = 'ADV'                                          
048300         MOVE 'AFV' TO UT-BOLAG                                           
048400         MOVE W-TRANS-BESKR TO UT-BESKR                                   
048500         MOVE W-BOLAG(4:2) TO UT-LAND                                     
048600         MOVE W-DIVMISC TO UT-DIVMISC                                     
048700         MOVE W-JOBFUNC TO UT-JOBFUNC                                     
048800         IF W-DIVMISC NOT = '***'                                         
048900           MOVE W-DIVMISC (2:2) TO UT-JOBFUNC                             
049000         END-IF                                                           
049100         IF W-ANSTNR = '*****'                                            
049200           MOVE SPACE TO UT-ANSTNR                                        
049300         ELSE                                                             
049400           MOVE 'ind' TO UT-ANSTNR                                        
049500         END-IF                                                           
049600         PERFORM S20-SKRIV-UTFIL                                          
049700                                                                          
049710       WHEN W-BOLAG(1:3) = 'AVZ'                                          
049720         MOVE 'AVZ' TO UT-BOLAG                                           
049730         MOVE W-TRANS-BESKR TO UT-BESKR                                   
049740         MOVE W-BOLAG(4:2) TO UT-LAND                                     
049750         MOVE W-DIVMISC TO UT-DIVMISC                                     
049760         MOVE W-JOBFUNC TO UT-JOBFUNC                                     
049791         IF W-ANSTNR = '*****'                                            
049792           MOVE SPACE TO UT-ANSTNR                                        
049793         ELSE                                                             
049794           MOVE 'ind' TO UT-ANSTNR                                        
049795         END-IF                                                           
049796         PERFORM S20-SKRIV-UTFIL                                          
049797                                                                          
049800       WHEN OTHER                                                         
049900         MOVE W-TRANS-BESKR TO UT-BESKR                                   
050000         MOVE W-BOLAG(1:3) TO UT-BOLAG                                    
050100         MOVE W-BOLAG(4:1) TO UT-LAND                                     
050200         MOVE W-JOBFUNC TO UT-JOBFUNC                                     
050300         IF W-ANSTNR = '*****'                                            
050400           MOVE SPACE TO UT-ANSTNR                                        
050500         ELSE                                                             
050600           MOVE 'ind' TO UT-ANSTNR                                        
050700         END-IF                                                           
050800         PERFORM S20-SKRIV-UTFIL                                          
050900                                                                          
051000     END-EVALUATE                                                         
051100     .                                                                    
051200     EJECT                                                                
051300 Z-FINIT  SECTION.                                                        
051400     SKIP2                                                                
051500     CLOSE REGELFIL                                                       
051600           BESKRFIL                                                       
051700           UTFIL                                                          
051800     .                                                                    
051900 S10-LAES-REGELFIL SECTION.                                               
052000     SKIP2                                                                
052100     READ REGELFIL INTO W-REGEL-AREA                                      
052200       AT END MOVE JA TO REGELFIL-EOF                                     
052300     .                                                                    
052400 S11-LAES-BESKRFIL SECTION.                                               
052500     SKIP2                                                                
052600     READ BESKRFIL INTO W-BESKR-AREA                                      
052700       AT END MOVE JA TO BESKRFIL-EOF                                     
052800     .                                                                    
052900 S20-SKRIV-UTFIL  SECTION.                                                
053000     SKIP2                                                                
053100     WRITE UT-POST   FROM UT-AREA                                         
053200     .                                                                    
053300 S99-ABEND  SECTION.                                                      
053400     SKIP2                                                                
053500     CALL ABEND USING RKOD-ABEND-UTAN-DUMP                                
053600     .                                                                    
