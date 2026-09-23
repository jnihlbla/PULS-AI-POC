000100 ID  DIVISION.                                                            
000200     SKIP2                                                                
000300 PROGRAM-ID.  W9702300.                                                   
000400     SKIP2                                                                
000500 AUTHOR.        KARIN OLSSON.                                             
000600     DATE-WRITTEN.  APR 1993.                                             
000700*                                                                         
000800*                                                                         
000900*    PLOCKAR UT INFORMATION UR ACF2-TRANSMEDLEMMAR I V4.                  
001000*                                                                         
001100     EJECT                                                                
001200 ENVIRONMENT DIVISION.                                                    
001300     SKIP2                                                                
001400 INPUT-OUTPUT  SECTION.                                                   
001500*                                                                         
001600 FILE-CONTROL.                                                            
001700*                                                                         
001800     SELECT REGELFIL        ASSIGN TO W97023D1.                           
001900*                                                                         
002000     SELECT BESKRFIL        ASSIGN TO W97023D2.                           
002100*                                                                         
002110     SELECT MSCFIL          ASSIGN TO W97023D3.                           
002120*                                                                         
002200     SELECT UTFIL           ASSIGN TO W97023D4.                           
002300*                                                                         
002400*                                                                         
002500 DATA DIVISION.                                                           
002600                                                                          
002700 FILE  SECTION.                                                           
002800*                                                                         
002900 FD  REGELFIL                                                             
003000     LABEL RECORD   STANDARD                                              
003100     RECORDING      F                                                     
003200     BLOCK CONTAINS 0.                                                    
003300                                                                          
003400 01  FILLER                  PIC X(121).                                  
003500*                                                                         
003600 FD  BESKRFIL                                                             
003700     LABEL RECORD   STANDARD                                              
003800     RECORDING      F                                                     
003900     BLOCK CONTAINS 0.                                                    
004000                                                                          
004100 01  FILLER                  PIC X(133).                                  
004200*                                                                         
004210 FD  MSCFIL                                                               
004220     LABEL RECORD   STANDARD                                              
004230     RECORDING      F                                                     
004240     BLOCK CONTAINS 0.                                                    
004250                                                                          
004260 01  FILLER                  PIC X(133).                                  
004270*                                                                         
004300 FD  UTFIL                                                                
004400     LABEL RECORD   STANDARD                                              
004500     RECORDING      F                                                     
004600     BLOCK CONTAINS 0.                                                    
004700                                                                          
004800 01  UT-POST                 PIC X(60).                                   
004900     EJECT                                                                
005000 WORKING-STORAGE  SECTION.                                                
005001                                                                          
005010*    -- CHECKED BY WY2000                                                 
005100*                                                                         
005200 01  W-REGEL-AREA.                                                        
005300     03  W-REGEL-RAD             PIC X(121).                              
005400*                                                                         
005500     03  W-REGEL-AREA1 REDEFINES W-REGEL-RAD.                             
005600*                                                                         
005700         05  FILLER              PIC X(2).                                
005800         05  PDSSCAN-ORD         PIC X(7).                                
005900         05  FILLER              PIC X(70).                               
006000         05  MEDLEM-ORD          PIC X(7).                                
006100         05  FILLER              PIC X.                                   
006200         05  MEDLEM              PIC X(8).                                
006300         05  FILLER              PIC X(26).                               
006400*                                                                         
006500     03  W-REGEL-AREA2 REDEFINES W-REGEL-RAD.                             
006600*                                                                         
006700         05  FILLER              PIC X(10).                               
006800         05  FILLER              PIC X.                                   
006900         05  UID-TEXT            PIC X(4).                                
007000         05  UID-STR             PIC X(40).                               
007100         05  FILLER              PIC X(66).                               
007200*                                                                         
007300 01  W-BESKR-AREA.                                                        
007400     03  FILLER                  PIC X(37).                               
007500     03  W-BESKR-TRANS           PIC X(8).                                
007600     03  FILLER                  PIC X(17).                               
007700     03  W-BESKR-TEXT            PIC X(30).                               
007800     03  FILLER                  PIC X(41).                               
007900*                                                                         
008000 01  W-BESKR-AREA-2 REDEFINES W-BESKR-AREA.                               
008100     03  FILLER                  PIC X(37).                               
008200     03  W-TRANSACTION           PIC X(11).                               
008300     03  FILLER                  PIC X(3).                                
008400     03  W-ACCOUNT               PIC X(7).                                
008500     03  FILLER                  PIC X(15).                               
008600     03  W-COMMENTS              PIC X(8).                                
008700     03  FILLER                  PIC X(52).                               
008800*                                                                         
008810 01  W-MSC-AREA.                                                          
008820     03  FILLER                  PIC X(37).                               
008830     03  W-MSC-TRANS             PIC X(8).                                
008850     03  FILLER                  PIC X(17).                               
008860     03  W-MSC-TEXT              PIC X(30).                               
008861     03  FILLER                  PIC X(41).                               
008870*                                                                         
008880 01  W-MSC-AREA-2 REDEFINES W-MSC-AREA.                                   
008890     03  FILLER                  PIC X(37).                               
008891     03  W-TRANSACTION2          PIC X(11).                               
008892     03  FILLER                  PIC X(3).                                
008893     03  W-ACCOUNT2              PIC X(7).                                
008894     03  FILLER                  PIC X(15).                               
008895     03  W-COMMENTS2             PIC X(8).                                
008896     03  FILLER                  PIC X(52).                               
008897*                                                                         
008900 01  W-UID.                                                               
009000*                                                                         
009100     03  W-BOLAG             PIC X(5).                                    
009200     03  W-AVD               PIC X(5).                                    
009300     03  W-JOBFUNC           PIC X(2).                                    
009400     03  W-DIVMISC           PIC X(3).                                    
009500     03  FILLER              PIC X(4).                                    
009600     03  W-ANSTNR            PIC X(5).                                    
009700*                                                                         
009800 01  UT-AREA.                                                             
009900*                                                                         
010000     03  UT-TRANS            PIC X(8)   VALUE SPACE.                      
010100     03  UT-BESKR            PIC X(30)  VALUE SPACE.                      
010200     03  UT-BOLAG            PIC X(3)   VALUE SPACE.                      
010300     03  UT-LAND             PIC X(2)   VALUE SPACE.                      
010400     03  UT-JOBFUNC          PIC X(2)   VALUE SPACE.                      
010500     03  UT-DIVMISC          PIC X(3)   VALUE SPACE.                      
010600     03  UT-ANSTNR           PIC X(5)   VALUE SPACE.                      
010700     03  FILLER              PIC X(7)   VALUE SPACE.                      
010800                                                                          
010900 01  TRANSACTION-TXT         PIC X(11) VALUE 'TRANSACTION'.               
011000 01  ACCOUNT-TXT             PIC X(7)  VALUE 'ACCOUNT'.                   
011100 01  COMMENTS-TXT            PIC X(8)  VALUE 'COMMENTS'.                  
011200 01  W-TRANS-BESKR           PIC X(30).                                   
011300 01  W-ACCEPT                PIC X(30).                                   
011400 01  W-SKRAEP                PIC X(1).                                    
011500 01  W-SKRAEPP               PIC X(1).                                    
011600 01  W-GRUPPP                PIC X(8).                                    
011700 01  UID-PTR                 PIC S9(4) COMP-3.                            
011800 01  STR-PTR                 PIC S9(4) COMP-3.                            
011900*                                                                         
012000 01  GENERELLA-SUBPGM.                                                    
012100*                                                                         
012200     03  ABEND               PIC X(8)    VALUE 'ABEND'.                   
012300     03  W009PDSR            PIC X(8)    VALUE 'W009PDSR'.                
012400     EJECT                                                                
012500* --- PARAMETRAR TILL ABEND                                               
012600 01  RKOD-ABEND-UTAN-DUMP    PIC S9(4)   COMP VALUE +16.                  
012700 01  RKOD-ABEND-MED-DUMP     PIC S9(4)   COMP VALUE +1000.                
012800*                                                                         
012900* --- PARAMETRAR TILL W009PDSR                                            
013000 01  DDMBR.                                                               
013100     03  DDNAMN              PIC X(8)  VALUE 'RACF2DD'.                   
013200     03  W-GRUPP             PIC X(8)  VALUE SPACE.                       
013300     SKIP2                                                                
013400 01  GRUPP-POST              PIC X(80) VALUE SPACE.                       
013500*                                                                         
013600 01  GRUPP-POST2  REDEFINES GRUPP-POST.                                   
013700*                                                                         
013800     05  FILLER              PIC X.                                       
013900     05  GRUPP-UID-TEXT      PIC X(4).                                    
014000     05  GRUPP-UID-STR       PIC X(40).                                   
014100     05  FILLER              PIC X(35).                                   
014200     SKIP2                                                                
014300 77  FUNK-OPEN               PIC X     VALUE 'O'.                         
014400 77  FUNK-READ               PIC X     VALUE SPACE.                       
014500 77  FUNK-CLOSE              PIC X     VALUE 'C'.                         
014600*                                                                         
014700 01  GENERELLA-KONSTANTER.                                                
014800*                                                                         
014900     03  JA                  PIC X(1)    VALUE 'Y'.                       
015000     03  NEJ                 PIC X(1)    VALUE 'N'.                       
015100     03  ASTERISKER          PIC X(24)   VALUE                            
015200                             '************************'.                  
015300                                                                          
015400 01  REGELFIL-EOF            PIC X(1)    VALUE 'N'.                       
015500 01  BESKRFIL-EOF            PIC X(1)    VALUE 'N'.                       
015510 01  MSCFIL-EOF              PIC X(1)    VALUE 'N'.                       
015600*                                                                         
015700 01  MEDLEM-OK               PIC X(8).                                    
015710*                                                                         
015720 01  OLD-MEDLEM.                                                          
015800     03  FILLER              PIC X(2).                                    
015900     03  OLD-MEDLEM-3        PIC X(1).                                    
016000     03  FILLER              PIC X(5).                                    
016100     EJECT                                                                
016200*                                                                         
016300     SKIP2                                                                
016400 01  RETURKODER.                                                          
016500*                                                                         
016600     03  RKOD                PIC S9(4)   COMP SYNC VALUE ZERO.            
016700     EJECT                                                                
016800 PROCEDURE DIVISION.                                                      
016900*                                                                         
017000     PERFORM A-INIT                                                       
017100     PERFORM S11-LAES-BESKRFIL                                            
017200     PERFORM B-LAES-FRAM-BESKRFIL                                         
017210     PERFORM S12-LAES-MSCFIL                                              
017220     PERFORM C-LAES-FRAM-MSCFIL                                           
017300     PERFORM UNTIL REGELFIL-EOF = JA                                      
017400       IF PDSSCAN-ORD = 'PDSSCAN' AND MEDLEM-ORD = 'MEDLEM:'              
017500         IF MEDLEM NOT = OLD-MEDLEM                                       
017600           IF MEDLEM = W-BESKR-TRANS                                      
017610             IF MEDLEM = W-MSC-TRANS                                      
017700               MOVE MEDLEM TO OLD-MEDLEM MEDLEM-OK                        
017800               MOVE W-BESKR-TEXT TO W-TRANS-BESKR                         
017900               PERFORM S10-LAES-REGELFIL                                  
017910             ELSE                                                         
017920               IF (W-TRANSACTION2 = TRANSACTION-TXT AND                   
017930                  W-ACCOUNT2      = ACCOUNT-TXT AND                       
017940                  W-COMMENTS2     = COMMENTS-TXT) OR                      
017950                  (W-TRANSACTION2  = ALL '-' OR SPACES)                   
017960                 PERFORM S12-LAES-MSCFIL                                  
017970               ELSE                                                       
017980                 IF MEDLEM < W-MSC-TRANS                                  
017981                   MOVE SPACE TO UT-AREA                                  
017990                   MOVE MEDLEM TO OLD-MEDLEM UT-TRANS                     
017991                   MOVE SPACE TO W-TRANS-BESKR                            
017992                   STRING 'ingen msc ' DELIMITED BY SIZE                  
017993                          W-BESKR-TEXT DELIMITED BY SIZE                  
017994                          INTO W-TRANS-BESKR                              
017996                   IF OLD-MEDLEM-3 = 'T'                                  
017997                     MOVE W-BESKR-TEXT TO UT-BESKR                        
017998                     MOVE 'V4N' TO UT-BOLAG                               
017999                     MOVE 'OM' TO UT-LAND                                 
018000                     MOVE 'SC' TO UT-JOBFUNC                              
018002                     PERFORM S20-SKRIV-UTFIL                              
018003                   END-IF                                                 
018004                   PERFORM S10-LAES-REGELFIL                              
018005                 ELSE                                                     
018006                   IF W-MSC-TRANS (3:1) = 'T'                             
018007                   AND (W-MSC-TRANS NOT = OLD-MEDLEM)                     
018008                   AND (W-MSC-TRANS NOT = MEDLEM-OK)                      
018011                     MOVE SPACE TO UT-AREA                                
018012                     MOVE W-MSC-TRANS TO UT-TRANS                         
018013                     MOVE W-MSC-TEXT  TO UT-BESKR                         
018014                     MOVE 'V4N' TO UT-BOLAG                               
018015                     MOVE 'OR' TO UT-LAND                                 
018016                     MOVE 'ES' TO UT-JOBFUNC                              
018017                     PERFORM S20-SKRIV-UTFIL                              
018018                   END-IF                                                 
018019                   PERFORM S12-LAES-MSCFIL                                
018021                 END-IF                                                   
018022               END-IF                                                     
018023             END-IF                                                       
018030           ELSE                                                           
018100             IF BESKRFIL-EOF = JA                                         
018110               MOVE SPACE TO UT-AREA                                      
018200               MOVE MEDLEM TO OLD-MEDLEM UT-TRANS                         
018300               MOVE 'okänd trans' TO W-TRANS-BESKR UT-BESKR               
018310               IF MEDLEM (3:1) = 'T'                                      
018320                 MOVE 'V4N' TO UT-BOLAG                                   
018330                 MOVE 'OG' TO UT-LAND                                     
018340                 MOVE 'EN' TO UT-JOBFUNC                                  
018360                 PERFORM S20-SKRIV-UTFIL                                  
018370               END-IF                                                     
018400               PERFORM S10-LAES-REGELFIL                                  
018500             ELSE                                                         
018600               IF (W-TRANSACTION = TRANSACTION-TXT AND                    
018700                  W-ACCOUNT = ACCOUNT-TXT AND                             
018800                  W-COMMENTS = COMMENTS-TXT) OR                           
018900                  (W-TRANSACTION = ALL '-' OR SPACES)                     
019000                 PERFORM S11-LAES-BESKRFIL                                
019100               ELSE                                                       
019200                 IF MEDLEM < W-BESKR-TRANS                                
019210                   MOVE SPACE TO UT-AREA                                  
019300                   MOVE MEDLEM TO OLD-MEDLEM UT-TRANS                     
019400                   MOVE 'okänd trans' TO W-TRANS-BESKR UT-BESKR           
019401                   IF MEDLEM (3:1) = 'T'                                  
019410                     MOVE 'V4N' TO UT-BOLAG                               
019420                     MOVE 'OG' TO UT-LAND                                 
019430                     MOVE 'EN' TO UT-JOBFUNC                              
019440                     PERFORM S20-SKRIV-UTFIL                              
019450                   END-IF                                                 
019500                   PERFORM S10-LAES-REGELFIL                              
019600                 ELSE                                                     
019643                   PERFORM S11-LAES-BESKRFIL                              
019800                 END-IF                                                   
019900               END-IF                                                     
020000             END-IF                                                       
020100           END-IF                                                         
020200         ELSE                                                             
020300           PERFORM S10-LAES-REGELFIL                                      
020400         END-IF                                                           
020500       ELSE                                                               
020600         IF OLD-MEDLEM-3 = 'T' AND UID-TEXT = 'UID('                      
020700           PERFORM D-KONTROLLERA-UID                                      
020800         END-IF                                                           
020900         PERFORM S10-LAES-REGELFIL                                        
021000       END-IF                                                             
021100     END-PERFORM                                                          
021200                                                                          
021300     PERFORM Z-FINIT                                                      
021400     MOVE RKOD TO RETURN-CODE                                             
021500     GOBACK.                                                              
021600     EJECT                                                                
021700 A-INIT  SECTION.                                                         
021800     SKIP2                                                                
021900     OPEN INPUT REGELFIL BESKRFIL MSCFIL                                  
022000                                                                          
022100     OPEN OUTPUT UTFIL                                                    
022200     MOVE NEJ TO REGELFIL-EOF BESKRFIL-EOF MSCFIL-EOF                     
022300     MOVE SPACE TO OLD-MEDLEM                                             
022400     .                                                                    
022500     EJECT                                                                
022600 B-LAES-FRAM-BESKRFIL  SECTION.                                           
022700     SKIP2                                                                
022800     PERFORM S11-LAES-BESKRFIL                                            
022900     PERFORM UNTIL BESKRFIL-EOF = JA                                      
023000              OR (W-TRANSACTION = TRANSACTION-TXT                         
023100                AND W-ACCOUNT = ACCOUNT-TXT                               
023200                AND W-COMMENTS = COMMENTS-TXT)                            
023300       PERFORM S11-LAES-BESKRFIL                                          
023400     END-PERFORM                                                          
023500     .                                                                    
023600     EJECT                                                                
023610 C-LAES-FRAM-MSCFIL  SECTION.                                             
023620     SKIP2                                                                
023630     PERFORM S12-LAES-MSCFIL                                              
023640     PERFORM UNTIL MSCFIL-EOF = JA                                        
023650              OR (W-TRANSACTION2 = TRANSACTION-TXT                        
023660                AND W-ACCOUNT2 = ACCOUNT-TXT                              
023670                AND W-COMMENTS2 = COMMENTS-TXT)                           
023680       PERFORM S12-LAES-MSCFIL                                            
023690     END-PERFORM                                                          
023691     .                                                                    
023692     EJECT                                                                
023700 D-KONTROLLERA-UID   SECTION.                                             
023800     SKIP2                                                                
023900     MOVE SPACE      TO UT-AREA                                           
024000     MOVE OLD-MEDLEM TO UT-TRANS                                          
024100     MOVE SPACE      TO W-UID W-ACCEPT                                    
024200     MOVE +1         TO UID-PTR                                           
024300                                                                          
024400     UNSTRING UID-STR DELIMITED BY ')' INTO W-UID                         
024500              WITH POINTER UID-PTR                                        
024600                                                                          
024700     MOVE UID-PTR TO STR-PTR                                              
024800     SUBTRACT 1 FROM STR-PTR                                              
024900     STRING   ASTERISKER DELIMITED BY SIZE                                
025000              INTO W-UID WITH POINTER STR-PTR                             
025100                                                                          
025200     UNSTRING UID-STR INTO W-ACCEPT                                       
025300              WITH POINTER UID-PTR                                        
025400                                                                          
025500     IF W-ACCEPT(1:5) = 'ALLOW' OR W-ACCEPT(2:5) = 'ALLOW'                
025600       PERFORM E-SKAPA-UTPOST                                             
025700     ELSE                                                                 
025800       IF W-ACCEPT(1:18) = 'PREVENT DATA(ÅKEY('                           
025900         OR W-ACCEPT(2:18) = 'PREVENT DATA(ÅKEY('                         
026000         PERFORM DA-KONTROLLERA-GRUPP                                     
026100       END-IF                                                             
026200     END-IF                                                               
026300     .                                                                    
026400     EJECT                                                                
026500 DA-KONTROLLERA-GRUPP   SECTION.                                          
026600     SKIP2                                                                
026700     MOVE SPACE TO W-GRUPP W-GRUPPP                                       
026800     UNSTRING W-ACCEPT DELIMITED BY ALL '('                               
026900              INTO W-SKRAEP W-SKRAEPP W-GRUPPP                            
027000                                                                          
027100     UNSTRING W-GRUPPP DELIMITED BY ')'                                   
027200              INTO W-GRUPP                                                
027300                                                                          
027400                                                                          
027500       CALL W009PDSR USING DDMBR FUNK-OPEN                                
027600       IF RETURN-CODE = 8                                                 
027700       DISPLAY 'HITTAR INTE GRUPP ' W-GRUPP ' MEDLEM ' UT-TRANS           
027800       ELSE                                                               
027900         IF RETURN-CODE = 12                                              
028000           DISPLAY 'KAN EJ ÖPPNA ACF2-BIBLIOTEKETET'                      
028100           PERFORM S99-ABEND                                              
028200         ELSE                                                             
028300                                                                          
028400           CALL W009PDSR USING DDMBR FUNK-READ GRUPP-POST                 
028500           PERFORM UNTIL RETURN-CODE > 0                                  
028600             IF GRUPP-UID-TEXT = 'UID('                                   
028700               PERFORM DAA-KONTROLLERA-GRUPP-UID                          
028800             END-IF                                                       
028900             CALL W009PDSR USING DDMBR FUNK-READ GRUPP-POST               
029000           END-PERFORM                                                    
029100           CALL W009PDSR USING DDMBR FUNK-CLOSE                           
029200           IF RETURN-CODE > 0                                             
029300             DISPLAY 'KAN EJ STÄNGA ACF2-BIBLIOTEKET'                     
029400             PERFORM S99-ABEND                                            
029500           END-IF                                                         
029600         END-IF                                                           
029700       END-IF                                                             
029800                                                                          
029900     .                                                                    
030000     EJECT                                                                
030100 DAA-KONTROLLERA-GRUPP-UID   SECTION.                                     
030200     SKIP2                                                                
030300     MOVE SPACE      TO W-UID W-ACCEPT                                    
030400     MOVE +1         TO UID-PTR                                           
030500                                                                          
030600     UNSTRING GRUPP-UID-STR DELIMITED BY ')' INTO W-UID                   
030700              WITH POINTER UID-PTR                                        
030800                                                                          
030900     MOVE UID-PTR TO STR-PTR                                              
031000     SUBTRACT 1 FROM STR-PTR                                              
031100     STRING   ASTERISKER DELIMITED BY SIZE                                
031200              INTO W-UID WITH POINTER STR-PTR                             
031300                                                                          
031400     UNSTRING GRUPP-UID-STR INTO W-ACCEPT                                 
031500              WITH POINTER UID-PTR                                        
031600                                                                          
031700     IF W-ACCEPT(1:5) = 'ALLOW' OR W-ACCEPT(2:5) = 'ALLOW'                
031800       PERFORM E-SKAPA-UTPOST                                             
031900     END-IF                                                               
032000     .                                                                    
032100     EJECT                                                                
032200 E-SKAPA-UTPOST   SECTION.                                                
032300     SKIP2                                                                
032400     MOVE SPACE TO UT-JOBFUNC UT-DIVMISC UT-LAND                          
032500     EVALUATE TRUE                                                        
032600                                                                          
032700       WHEN (W-BOLAG = 'AC GP' OR 'AP') AND                               
032800            W-JOBFUNC = 'IS'                                              
032900         CONTINUE                                                         
033000                                                                          
033100       WHEN W-BOLAG(1:3) = 'ATB'                                          
033200         MOVE W-BOLAG TO UT-BOLAG                                         
033300         MOVE W-TRANS-BESKR TO UT-BESKR                                   
033400         IF W-ANSTNR = '*****'                                            
033500           MOVE SPACE TO UT-ANSTNR                                        
033600         ELSE                                                             
033700           MOVE 'ind' TO UT-ANSTNR                                        
033800         END-IF                                                           
033900         PERFORM S20-SKRIV-UTFIL                                          
034000                                                                          
034100       WHEN W-BOLAG = 'ATCCR'                                             
034200         MOVE W-BOLAG (1:3) TO UT-BOLAG                                   
034300         MOVE W-TRANS-BESKR TO UT-BESKR                                   
034400         MOVE W-BOLAG(4:2) TO UT-LAND                                     
034500         IF W-ANSTNR = '*****'                                            
034600           MOVE SPACE TO UT-ANSTNR                                        
034700         ELSE                                                             
034800           MOVE 'ind' TO UT-ANSTNR                                        
034900         END-IF                                                           
035000         PERFORM S20-SKRIV-UTFIL                                          
035100                                                                          
035200       WHEN W-BOLAG (1:3) = 'ATC'                                         
035300         MOVE W-BOLAG (1:3) TO UT-BOLAG                                   
035400         MOVE W-TRANS-BESKR TO UT-BESKR                                   
035500         IF W-ANSTNR = '*****'                                            
035600           MOVE SPACE TO UT-ANSTNR                                        
035700         ELSE                                                             
035800           MOVE 'ind' TO UT-ANSTNR                                        
035900         END-IF                                                           
036000         PERFORM S20-SKRIV-UTFIL                                          
036100                                                                          
036200       WHEN W-BOLAG (1:5) = 'ATECB' OR 'ATECM'                            
036300         MOVE W-BOLAG (1:3) TO UT-BOLAG                                   
036400         MOVE W-TRANS-BESKR TO UT-BESKR                                   
036500         MOVE W-BOLAG (4:2) TO UT-LAND                                    
036600         IF W-ANSTNR = '*****'                                            
036700           MOVE SPACE TO UT-ANSTNR                                        
036800         ELSE                                                             
036900           MOVE 'ind' TO UT-ANSTNR                                        
037000         END-IF                                                           
037100         PERFORM S20-SKRIV-UTFIL                                          
037200                                                                          
037300       WHEN W-BOLAG (1:4) = 'ATE1' OR 'ATE2' OR 'ATE3' OR 'ATE4'          
037400         MOVE W-BOLAG (1:3) TO UT-BOLAG                                   
037500         MOVE W-TRANS-BESKR TO UT-BESKR                                   
037600         MOVE W-BOLAG (4:1) TO UT-LAND                                    
037700         IF W-ANSTNR = '*****'                                            
037800           MOVE SPACE TO UT-ANSTNR                                        
037900         ELSE                                                             
038000           MOVE 'ind' TO UT-ANSTNR                                        
038100         END-IF                                                           
038200         PERFORM S20-SKRIV-UTFIL                                          
038300                                                                          
038400       WHEN W-BOLAG (1:3) = 'ATE' OR 'ATH' OR 'ATN' OR 'ATP'              
038500                         OR 'ATT' OR 'ATU' OR 'ATW' OR 'ATX'              
038700         MOVE W-BOLAG (1:3) TO UT-BOLAG                                   
038800         MOVE W-TRANS-BESKR TO UT-BESKR                                   
038900         IF W-ANSTNR = '*****'                                            
039000           MOVE SPACE TO UT-ANSTNR                                        
039100         ELSE                                                             
039200           MOVE 'ind' TO UT-ANSTNR                                        
039300         END-IF                                                           
039400         PERFORM S20-SKRIV-UTFIL                                          
039500                                                                          
039510       WHEN W-BOLAG (1:3) = 'AT ' OR 'AT*'                                
039530         MOVE W-BOLAG (1:2) TO UT-BOLAG                                   
039540         MOVE W-TRANS-BESKR TO UT-BESKR                                   
039550         IF W-ANSTNR = '*****'                                            
039560           MOVE SPACE TO UT-ANSTNR                                        
039570         ELSE                                                             
039580           MOVE 'ind' TO UT-ANSTNR                                        
039590         END-IF                                                           
039591         PERFORM S20-SKRIV-UTFIL                                          
039592                                                                          
039600       WHEN W-BOLAG (1:3) = 'ABC' OR 'ABL' OR 'ABV'                       
039700         MOVE W-BOLAG (1:3) TO UT-BOLAG                                   
039800         MOVE W-BOLAG (4:1) TO UT-LAND                                    
039900         MOVE W-TRANS-BESKR TO UT-BESKR                                   
040000         IF W-ANSTNR = '*****'                                            
040100           MOVE SPACE TO UT-ANSTNR                                        
040200         ELSE                                                             
040300           MOVE 'ind' TO UT-ANSTNR                                        
040400         END-IF                                                           
040500         PERFORM S20-SKRIV-UTFIL                                          
040600                                                                          
040610       WHEN W-BOLAG (1:3) = 'AB ' OR 'AB*'                                
040620         MOVE W-BOLAG (1:2) TO UT-BOLAG                                   
040630         MOVE W-TRANS-BESKR TO UT-BESKR                                   
040640         IF W-ANSTNR = '*****'                                            
040650           MOVE SPACE TO UT-ANSTNR                                        
040660         ELSE                                                             
040670           MOVE 'ind' TO UT-ANSTNR                                        
040680         END-IF                                                           
040690         PERFORM S20-SKRIV-UTFIL                                          
040691                                                                          
040692       WHEN W-BOLAG (1:3) = 'AVU'                                         
040693         MOVE W-BOLAG (1:3) TO UT-BOLAG                                   
040694         MOVE W-TRANS-BESKR TO UT-BESKR                                   
040695         IF W-ANSTNR = '*****'                                            
040696           MOVE SPACE TO UT-ANSTNR                                        
040697         ELSE                                                             
040698           MOVE 'ind' TO UT-ANSTNR                                        
040699         END-IF                                                           
040700         PERFORM S20-SKRIV-UTFIL                                          
040701                                                                          
041810       WHEN OTHER                                                         
041820         MOVE W-BOLAG(1:3) TO UT-BOLAG                                    
041830         MOVE W-TRANS-BESKR TO UT-BESKR                                   
041840         MOVE W-BOLAG(4:2) TO UT-LAND                                     
041850         MOVE W-JOBFUNC TO UT-JOBFUNC                                     
041860         IF W-ANSTNR = '*****'                                            
041870           MOVE SPACE TO UT-ANSTNR                                        
041880         ELSE                                                             
041890           MOVE 'ind' TO UT-ANSTNR                                        
041891         END-IF                                                           
041892         PERFORM S20-SKRIV-UTFIL                                          
041893                                                                          
042100     END-EVALUATE                                                         
042200     .                                                                    
042300     EJECT                                                                
042400 Z-FINIT  SECTION.                                                        
042500     SKIP2                                                                
042600     CLOSE REGELFIL                                                       
042700           BESKRFIL                                                       
042710           MSCFIL                                                         
042800           UTFIL                                                          
042900     .                                                                    
043000 S10-LAES-REGELFIL SECTION.                                               
043100     SKIP2                                                                
043200     READ REGELFIL INTO W-REGEL-AREA                                      
043300       AT END MOVE JA TO REGELFIL-EOF                                     
043400     .                                                                    
043500 S11-LAES-BESKRFIL SECTION.                                               
043600     SKIP2                                                                
043700     READ BESKRFIL INTO W-BESKR-AREA                                      
043800       AT END                                                             
043801         MOVE JA TO BESKRFIL-EOF                                          
043810         MOVE '99999999' TO W-BESKR-TRANS                                 
043820       END-READ                                                           
043900     .                                                                    
043910 S12-LAES-MSCFIL SECTION.                                                 
043920     SKIP2                                                                
043930     READ MSCFIL INTO W-MSC-AREA                                          
043940       AT END                                                             
043941         MOVE JA TO MSCFIL-EOF                                            
043942         MOVE '99999999' TO W-MSC-TRANS                                   
043943       END-READ                                                           
043950     .                                                                    
044000 S20-SKRIV-UTFIL  SECTION.                                                
044100     SKIP2                                                                
044200     WRITE UT-POST   FROM UT-AREA                                         
044300     .                                                                    
044400 S99-ABEND  SECTION.                                                      
044500     SKIP2                                                                
044600     CALL ABEND USING RKOD-ABEND-UTAN-DUMP                                
044700     .                                                                    
