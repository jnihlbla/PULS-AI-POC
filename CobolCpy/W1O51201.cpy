000100 01  MOD-W1O51201.                                                        
000200*                                 MOD-COPYTEXT FÖR BILD   1512            
000300*                                 KATALOGRAD                              
000400     03 MOD-IDTRANS          PIC X(4).                                    
000500*                                 BILDNUMMER                              
000600*                                 SCREEN NUMBER                           
000700     03 MOD-TEMFSFEL         PIC X(40).                                   
000800*                                 MFS FELMEDDELANDE                       
000900*                                 MFS ERROR MESSAGE                       
001000     03 MOD-IDCATNR-IN       PIC X(5).                                    
001100*                                 KATALOG-ID                              
001200*                                 CATALOG-ID                              
001300     03 MOD-IDCATNR-UT       PIC X(5).                                    
001400*                                 KATALOG-ID                              
001500*                                 CATALOG-ID                              
001600     03 MOD-IDCATGRP-IN      PIC X(2).                                    
001700*                                 KATALOG-GRUPP                           
001800*                                 CATALOG-GROUP                           
001900     03 MOD-IDCATGRP-UT      PIC X(2).                                    
002000*                                 KATALOG-GRUPP                           
002100*                                 CATALOG-GROUP                           
002200     03 MOD-IDCATAVS-IN      PIC X(4).                                    
002300*                                 KATALOG-AVSNITT                         
002400*                                 CATALOG TEXT BLOCK                      
002500     03 MOD-IDCATAVS-UT      PIC X(4).                                    
002600*                                 KATALOG-AVSNITT                         
002700*                                 CATALOG TEXT BLOCK                      
002800     03 MOD-IDSKYLT-IN       PIC X(3).                                    
002900*                                 NATIONALITETSTECKEN                     
003000*                                 SPRÅKIDENTIFIKATION                     
003100*                                 NATIONALITY SIGN                        
003200*                                 LANGUAGE IDENTIFIER                     
003300     03 MOD-IDSKYLT-UT       PIC X(3).                                    
003400*                                 NATIONALITETSTECKEN                     
003500*                                 SPRÅKIDENTIFIKATION                     
003600*                                 NATIONALITY SIGN                        
003700*                                 LANGUAGE IDENTIFIER                     
003800     03 MOD-IDCATRAD-IN      PIC X(4).                                    
003900*                                 RADNUMMER                               
004000*                                 ROW NUMBER IN TEXT BLOCK                
004100     03 MOD-IDCATRAD-UT      PIC X(4).                                    
004200*                                 RADNUMMER                               
004300*                                 ROW NUMBER IN TEXT BLOCK                
004400     03 MOD-KDCATPUB-R-FOM-IN                                             
004500                             PIC X(3).                                    
004600*                                 3 HÖGRASTE TECKNEN I KDCATPUB           
004700*                                 3 RIGHTMOST CHAR  IN KDCATPUB           
004800     03 MOD-KDCATPUB-R-FOM-UT                                             
004900                             PIC X(3).                                    
005000*                                 3 HÖGRASTE TECKNEN I KDCATPUB           
005100*                                 3 RIGHTMOST CHAR  IN KDCATPUB           
005200     03 MOD-KDCATPUB-R-MIN-IN                                             
005300                             PIC X(3).                                    
005400*                                 3 HÖGRASTE TECKNEN I KDCATPUB           
005500*                                 3 RIGHTMOST CHAR  IN KDCATPUB           
005600     03 MOD-KDCATPUB-R-MIN-UT                                             
005700                             PIC X(3).                                    
005800*                                 3 HÖGRASTE TECKNEN I KDCATPUB           
005900*                                 3 RIGHTMOST CHAR  IN KDCATPUB           
006000     03 MOD-KDCATPUB-R-MAX-IN                                             
006100                             PIC X(3).                                    
006200*                                 3 HÖGRASTE TECKNEN I KDCATPUB           
006300*                                 3 RIGHTMOST CHAR  IN KDCATPUB           
006400     03 MOD-KDCATPUB-R-MAX-UT                                             
006500                             PIC X(3).                                    
006600*                                 3 HÖGRASTE TECKNEN I KDCATPUB           
006700*                                 3 RIGHTMOST CHAR  IN KDCATPUB           
006800     03 MOD-IDCATRAD-PF7     PIC X(4).                                    
006900*                                 KATALOG-RADNR-SPAR                      
007000     03 MOD-IDCATRAD-FROM    PIC X(4).                                    
007100*                                 KATALOG-RADNR-SPAR                      
007200     03 MOD-IDCATRAD-MITT    PIC X(4).                                    
007300*                                 KATALOG-RADNR-SPAR                      
007400     03 MOD-IDCATRAD-TOM     PIC X(4).                                    
007500*                                 KATALOG-RADNR-SPAR                      
007600     03 MOD-IDCATRAD-FIRST   PIC X(4).                                    
007700*                                 KATALOG-RADNR-SPAR                      
007800     03 MOD-IDCATRAD-SISTA   PIC X(4).                                    
007900*                                 KATALOG-RADNR-SPAR                      
008000     03 MOD-FILLER           OCCURS 12 TIMES.                             
008100        05 MOD-IDCATRAD      PIC Z(3)9.                                   
008200*                                 RADNUMMER                               
008300*                                 ROW NUMBER IN TEXT BLOCK                
008400        05 MOD-KDCATPUB-R-FOM                                             
008500                             PIC X(3).                                    
008600*                                 3 HÖGRASTE TECKNEN I KDCATPUB           
008700*                                 3 RIGHTMOST CHAR  IN KDCATPUB           
008800        05 MOD-KDCATPUB-R-TOM                                             
008900                             PIC X(3).                                    
009000*                                 3 HÖGRASTE TECKNEN I KDCATPUB           
009100*                                 3 RIGHTMOST CHAR  IN KDCATPUB           
009200        05 MOD-KDFBX         PIC X.                                       
009300*                                 FBX-KOD                                 
009400*                                 FBX CODE VALUES:                        
009500*                                 H = MAIN LINE                           
009600*                                 F = CONTINUED LINE (NO BREAK)           
009700*                                 B = PAGE BREAK AFTER THIS LINE          
009800*                                 X = NO PAGE BREAK HERE                  
009900*                                     (MUST BE PRECEDED BY "H")           
010000*                                 SPACE = NORMAL BREAK RULES              
010100        05 MOD-IDCATPOS      PIC X(3).                                    
010200*                                 POSITIONSNUMMER                         
010300*                                 FIGURE NUMBER                           
010400        05 MOD-IDARTNR       PIC Z(9).                                    
010500*                                 ARTIKELNUMMER                           
010600*                                 PART NUMBER                             
010700        05 MOD-KVKOL         OCCURS 5 TIMES                               
010800                             PIC X(3).                                    
010900*                                 ANTAL AV ARTIKEL I RESP KOLUMN          
011000*                                 QTY. OF PARTS IN RESP. COLUMN           
011100        05 MOD-KDPS          PIC X(2).                                    
011200*                                 ARTIKELSTATUS                           
011300*                                 PART STATUS CODE                        
011400*                                 SPACE = NORMAL STORED PART              
011500*                                 LS= LOCALLY STORED (NOT STORED          
011600*                                 BY C1/C2)                               
011700*                                 NS= NEVER STORED BY VOLVO               
011800*                                 OP= OBSOLETED PART                      
011900*                                 SP= SUPERSEDED PART                     
012000*                                 IK= INCLUDED IN KIT                     
012100*                                 EU= EXCHANGE UNIT                       
012200*                                 KS= PART NOT YET STORED                 
012300*                                 XX= PART DESCR. NOT TRANSLATABL         
012400*                                 E                                       
012500*                                 KN= "IK" AND "NS" TOGETHER              
012600*                                 KL= "IK" AND "LS" TOGETHER              
012700*                                 SW= SOFTWARE SERVICED BY VADIS          
012800        05 MOD-KVPUNKT       PIC Z.                                       
012900*                                 ANTAL INDRAGNINGSPUNKTER                
013000*                                 QTY. INDENTING DOTS                     
013100*                                 0 = COMPLETE PART                       
013200*                                 1 = 1ST LEVEL OF INCL. PART             
013300*                                 2 = 2ND LEVEL OF INCL. PART             
013400*                                 3 = 3RD LEVEL OF INCL. PART             
013500*                                 4 = 4TH LEVEL OF INCL. PART             
013600        05 MOD-BEART         PIC X(25).                                   
013700*                                 ARTIKELBENÄMNING                        
013800*                                 PART DESCRIPTION                        
013900        05 MOD-TEKATANM      PIC X(23).                                   
014000*                                 ANMÄRKNINGSTEXT                         
014100*                                 NOTES TEXT                              
014200        05 MOD-KDRADST       PIC X.                                       
014300*                                 RAD-     L = LÅNAD.                     
014400*                                 STATUS   Ä = ÄNDRAD.                    
014500*                                          N = NYREGISTRERAD.             
014600*                                          C = L,Ä,N EFTER OMBRYT         
014700*                                              FRAM TILL VADGEN.          
014800*                                      SPACE = OFÖRÄNDRAD.                
014900*                                 LINE-    L = LOAN LINE.                 
015000*                                 STATUS   Ä = AMENDED LINE.              
015100*                                          N = NEW LINE.                  
015200*                                          C = CHANGED LINE - EG,         
015300*                                              SUBSCR.OF ABOVE 3.         
015400*                                      SPACE = UNCHANGED LINE.            
015500        05 MOD-FLNOTE        PIC X.                                       
015600*                                 NOTERINGSFLAGGA                         
015700        05 MOD-KDERS         PIC Z(2).                                    
015800*                                 ERSÄTTNINGSKOD                          
015900*                                 SUPERSESSION CODE                       
016000        05 MOD-FLH-REF       PIC X.                                       
016100*                                 FINNS HÄNV / REFERENS HIT ?             
016200        05 MOD-FLH           PIC X.                                       
016300*                                 FINNS HÄNVISNING ?                      
016400     03 MOD-IDCATRAD-UPD-ATTR                                             
016500                             PIC X(2).                                    
016600*                                 MFS ATTRIBUTFÄLT                        
016700     03 MOD-IDCATRAD-UPD     PIC X(4).                                    
016800*                                 RADNUMMER                               
016900*                                 ROW NUMBER IN TEXT BLOCK                
017000     03 MOD-KDCATPUB-R-FOM-UPD-ATTR                                       
017100                             PIC X(2).                                    
017200*                                 MFS ATTRIBUTFÄLT                        
017300     03 MOD-KDCATPUB-R-FOM-UPD                                            
017400                             PIC X(3).                                    
017500*                                 3 HÖGRASTE TECKNEN I KDCATPUB           
017600*                                 3 RIGHTMOST CHAR  IN KDCATPUB           
017700     03 MOD-KDCATPUB-R-TOM-UPD-ATTR                                       
017800                             PIC X(2).                                    
017900*                                 MFS ATTRIBUTFÄLT                        
018000     03 MOD-KDCATPUB-R-TOM-UPD                                            
018100                             PIC X(3).                                    
018200*                                 3 HÖGRASTE TECKNEN I KDCATPUB           
018300*                                 3 RIGHTMOST CHAR  IN KDCATPUB           
018400     03 MOD-KDFBX-UPD-ATTR   PIC X(2).                                    
018500*                                 MFS ATTRIBUTFÄLT                        
018600     03 MOD-KDFBX-UPD        PIC X.                                       
018700*                                 FBX-KOD                                 
018800*                                 FBX CODE VALUES:                        
018900*                                 H = MAIN LINE                           
019000*                                 F = CONTINUED LINE (NO BREAK)           
019100*                                 B = PAGE BREAK AFTER THIS LINE          
019200*                                 X = NO PAGE BREAK HERE                  
019300*                                     (MUST BE PRECEDED BY "H")           
019400*                                 SPACE = NORMAL BREAK RULES              
019500     03 MOD-IDCATPOS-UPD-ATTR                                             
019600                             PIC X(2).                                    
019700*                                 MFS ATTRIBUTFÄLT                        
019800     03 MOD-IDCATPOS-UPD     PIC X(3).                                    
019900*                                 POSITIONSNUMMER                         
020000*                                 FIGURE NUMBER                           
020100     03 MOD-IDARTNR-UPD-ATTR PIC X(2).                                    
020200*                                 MFS ATTRIBUTFÄLT                        
020300     03 MOD-IDARTNR-UPD      PIC X(9).                                    
020400*                                 ARTIKELNUMMER                           
020500*                                 PART NUMBER                             
020600     03 MOD-KVKOL-GRP.                                                    
020700        05 MOD-FILLER        OCCURS 5 TIMES.                              
020800           07 MOD-KVKOL-UPD-ATTR                                          
020900                             PIC X(2).                                    
021000*                                 MFS ATTRIBUTFÄLT                        
021100           07 MOD-KVKOL-UPD  PIC X(3).                                    
021200*                                 ANTAL AV ARTIKEL I RESP KOLUMN          
021300*                                 QTY. OF PARTS IN RESP. COLUMN           
021400     03 MOD-KDPS-UPD-ATTR    PIC X(2).                                    
021500*                                 MFS ATTRIBUTFÄLT                        
021600     03 MOD-KDPS-UPD         PIC X(2).                                    
021700*                                 ARTIKELSTATUS                           
021800*                                 PART STATUS CODE                        
021900*                                 SPACE = NORMAL STORED PART              
022000*                                 LS= LOCALLY STORED (NOT STORED          
022100*                                 BY C1/C2)                               
022200*                                 NS= NEVER STORED BY VOLVO               
022300*                                 OP= OBSOLETED PART                      
022400*                                 SP= SUPERSEDED PART                     
022500*                                 IK= INCLUDED IN KIT                     
022600*                                 EU= EXCHANGE UNIT                       
022700*                                 KS= PART NOT YET STORED                 
022800*                                 XX= PART DESCR. NOT TRANSLATABL         
022900*                                 E                                       
023000*                                 KN= "IK" AND "NS" TOGETHER              
023100*                                 KL= "IK" AND "LS" TOGETHER              
023200*                                 SW= SOFTWARE SERVICED BY VADIS          
023300     03 MOD-KVPUNKT-UPD-ATTR PIC X(2).                                    
023400*                                 MFS ATTRIBUTFÄLT                        
023500     03 MOD-KVPUNKT-UPD      PIC 9.                                       
023600*                                 ANTAL INDRAGNINGSPUNKTER                
023700*                                 QTY. INDENTING DOTS                     
023800*                                 0 = COMPLETE PART                       
023900*                                 1 = 1ST LEVEL OF INCL. PART             
024000*                                 2 = 2ND LEVEL OF INCL. PART             
024100*                                 3 = 3RD LEVEL OF INCL. PART             
024200*                                 4 = 4TH LEVEL OF INCL. PART             
024300     03 MOD-BEART-UPD-ATTR   PIC X(2).                                    
024400*                                 MFS ATTRIBUTFÄLT                        
024500     03 MOD-BEART-UPD        PIC X(25).                                   
024600*                                 ARTIKELBENÄMNING                        
024700*                                 PART DESCRIPTION                        
024800     03 MOD-KDHOM-UPD-ATTR   PIC X(2).                                    
024900*                                 MFS ATTRIBUTFÄLT                        
025000     03 MOD-KDHOM-UPD        PIC 9.                                       
025100*                                 HOMONYMKOD                              
025200*                                 HOMONYMOUS CODE                         
025300     03 MOD-TEKATANM-UPD-ATTR                                             
025400                             PIC X(2).                                    
025500*                                 MFS ATTRIBUTFÄLT                        
025600     03 MOD-TEKATANM-UPD     PIC X(23).                                   
025700*                                 ANMÄRKNINGSTEXT                         
025800*                                 NOTES TEXT                              
025900     03 MOD-IDCATGRP-H-UPD-ATTR                                           
026000                             PIC X(2).                                    
026100*                                 MFS ATTRIBUTFÄLT                        
026200     03 MOD-IDCATGRP-H-UPD   PIC X(2).                                    
026300*                                 KATALOG-GRUPP                           
026400*                                 CATALOG-GROUP                           
026500     03 MOD-IDCATAVS-H-UPD-ATTR                                           
026600                             PIC X(2).                                    
026700*                                 MFS ATTRIBUTFÄLT                        
026800     03 MOD-IDCATAVS-H-UPD   PIC X(4).                                    
026900*                                 KATALOG-AVSNITT                         
027000*                                 CATALOG TEXT BLOCK                      
027100     03 MOD-IDCATRAD-H-UPD-ATTR                                           
027200                             PIC X(2).                                    
027300*                                 MFS ATTRIBUTFÄLT                        
027400     03 MOD-IDCATRAD-H-UPD   PIC X(4).                                    
027500*                                 RADNUMMER                               
027600*                                 ROW NUMBER IN TEXT BLOCK                
027700     03 MOD-KDCATPUB-R-H-UPD-ATTR                                         
027800                             PIC X(2).                                    
027900*                                 MFS ATTRIBUTFÄLT                        
028000     03 MOD-KDCATPUB-R-H-UPD PIC X(3).                                    
028100*                                 3 HÖGRASTE TECKNEN I KDCATPUB           
028200*                                 3 RIGHTMOST CHAR  IN KDCATPUB           
028300     03 MOD-KDHAEN-UPD-ATTR  PIC X(2).                                    
028400*                                 MFS ATTRIBUTFÄLT                        
028500     03 MOD-KDHAEN-UPD       PIC X.                                       
028600*                                 HÄNVISNINGSKODER VÄRDEN:                
028700*                                  A = HÄNVISNING I ANM. FÄLTET           
028800*                                  B = HÄNVISNING I BEN. FÄLTET           
028900*                                  * = HÄNVISNING, EJ KLAR DEST.          
029000*                                  SPACE = INGEN HÄNVISNING               
029100*                                 REFERENCE CODE   VALUES:                
029200*                                  A = REFERENCE IN NOTE-FIELD            
029300*                                  B = REFERENCE IN DESCR-FIELD           
029400*                                  * = REFERENCE DURING EDITING           
029500*                                  SPACE = NO REFERENCE                   
029600     03 MOD-IDCATRAD-BORT-UPD-ATTR                                        
029700                             PIC X(2).                                    
029800*                                 MFS ATTRIBUTFÄLT                        
029900     03 MOD-IDCATRAD-BORT-UPD                                             
030000                             PIC X(4).                                    
030100*                                 RADNUMMER                               
030200*                                 ROW NUMBER IN TEXT BLOCK                
030300     03 MOD-KDCATPUB-R-BORT-UPD-ATTR                                      
030400                             PIC X(2).                                    
030500*                                 MFS ATTRIBUTFÄLT                        
030600     03 MOD-KDCATPUB-R-BORT-UPD                                           
030700                             PIC X(3).                                    
030800*                                 3 HÖGRASTE TECKNEN I KDCATPUB           
030900*                                 3 RIGHTMOST CHAR  IN KDCATPUB           
031000     03 MOD-IDCATPOS-SOEK-UPD-ATTR                                        
031100                             PIC X(2).                                    
031200*                                 MFS ATTRIBUTFÄLT                        
031300     03 MOD-IDCATPOS-SOEK-UPD                                             
031400                             PIC X(3).                                    
031500*                                 POSITIONSNUMMER                         
031600*                                 FIGURE NUMBER                           
031700     03 MOD-FILLER           OCCURS 3 TIMES.                              
031800        05 MOD-IDRUBNR-UPD-ATTR                                           
031900                             PIC X(2).                                    
032000*                                 MFS ATTRIBUTFÄLT                        
032100        05 MOD-IDRUBNR-UPD   PIC Z(5).                                    
032200*                                 RUBRIKNUMMER                            
032300*                                 HEADLINE ID NUMBER                      
032400     03 MOD-IDTTEXNR-UPD-ATTR                                             
032500                             PIC X(2).                                    
032600*                                 MFS ATTRIBUTFÄLT                        
032700     03 MOD-IDTTEXNR-UPD     PIC 9(5).                                    
032800*                                 TILLÄGGSTEXT-NR                         
032900*                                 ADDITIONAL TEXT, ID NUMBER              
033000     03 MOD-FILLER           OCCURS 3 TIMES.                              
033100        05 MOD-IDFOTNR-UPD-ATTR                                           
033200                             PIC X(2).                                    
033300*                                 MFS ATTRIBUTFÄLT                        
033400        05 MOD-IDFOTNR-UPD   PIC Z(5).                                    
033500*                                 FOTNOTSNUMMER                           
033600*                                 FOOT NOTE ID NUMBER                     
033700     03 MOD-TEMFSINF         PIC X(55).                                   
033800*                                 INFORMATIONSMEDDELANDE                  
033900*                                 INFORMATION MESSAGE                     
034000*** END OF VILMAII-COPY LENGTH= 1528 BYTES                                
