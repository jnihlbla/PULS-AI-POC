000100 01  MID-W1I51201.                                                        
000200*                                 MID-COPYTEXT FÖR BILD                   
000300*                                 KATALOGRAD, UPPDATERING                 
000400     03 MID-IDCATNR-IN       PIC X(5).                                    
000500*                                 KATALOG-ID                              
000600*                                 CATALOG-ID                              
000700     03 MID-IDCATNR-UT       PIC X(5).                                    
000800*                                 KATALOG-ID                              
000900*                                 CATALOG-ID                              
001000     03 MID-IDCATGRP-IN      PIC X(2).                                    
001100*                                 KATALOG-GRUPP                           
001200*                                 CATALOG-GROUP                           
001300     03 MID-IDCATGRP-UT      PIC X(2).                                    
001400*                                 KATALOG-GRUPP                           
001500*                                 CATALOG-GROUP                           
001600     03 MID-IDCATAVS-IN      PIC X(4).                                    
001700*                                 KATALOG-AVSNITT                         
001800*                                 CATALOG TEXT BLOCK                      
001900     03 MID-IDCATAVS-UT      PIC X(4).                                    
002000*                                 KATALOG-AVSNITT                         
002100*                                 CATALOG TEXT BLOCK                      
002200     03 MID-IDSKYLT-IN       PIC X(3).                                    
002300*                                 NATIONALITETSTECKEN                     
002400*                                 SPRÅKIDENTIFIKATION                     
002500*                                 NATIONALITY SIGN                        
002600*                                 LANGUAGE IDENTIFIER                     
002700     03 MID-IDSKYLT-UT       PIC X(3).                                    
002800*                                 NATIONALITETSTECKEN                     
002900*                                 SPRÅKIDENTIFIKATION                     
003000*                                 NATIONALITY SIGN                        
003100*                                 LANGUAGE IDENTIFIER                     
003200     03 MID-IDCATRAD-IN      PIC X(4).                                    
003300*                                 RADNUMMER                               
003400*                                 ROW NUMBER IN TEXT BLOCK                
003500     03 MID-IDCATRAD-UT      PIC X(4).                                    
003600*                                 RADNUMMER                               
003700*                                 ROW NUMBER IN TEXT BLOCK                
003800     03 MID-KDCATPUB-R-FOM-IN                                             
003900                             PIC X(3).                                    
004000*                                 3 HÖGRASTE TECKNEN I KDCATPUB           
004100*                                 3 RIGHTMOST CHAR  IN KDCATPUB           
004200     03 MID-KDCATPUB-R-FOM-UT                                             
004300                             PIC X(3).                                    
004400*                                 3 HÖGRASTE TECKNEN I KDCATPUB           
004500*                                 3 RIGHTMOST CHAR  IN KDCATPUB           
004600     03 MID-KDCATPUB-R-MIN-IN                                             
004700                             PIC X(3).                                    
004800*                                 3 HÖGRASTE TECKNEN I KDCATPUB           
004900*                                 3 RIGHTMOST CHAR  IN KDCATPUB           
005000     03 MID-KDCATPUB-R-MIN-UT                                             
005100                             PIC X(3).                                    
005200*                                 3 HÖGRASTE TECKNEN I KDCATPUB           
005300*                                 3 RIGHTMOST CHAR  IN KDCATPUB           
005400     03 MID-KDCATPUB-R-MAX-IN                                             
005500                             PIC X(3).                                    
005600*                                 3 HÖGRASTE TECKNEN I KDCATPUB           
005700*                                 3 RIGHTMOST CHAR  IN KDCATPUB           
005800     03 MID-KDCATPUB-R-MAX-UT                                             
005900                             PIC X(3).                                    
006000*                                 3 HÖGRASTE TECKNEN I KDCATPUB           
006100*                                 3 RIGHTMOST CHAR  IN KDCATPUB           
006200     03 MID-IDCATRAD-PF7     PIC 9(4).                                    
006300*                                 KATALOG-RADNR-SPAR                      
006400     03 MID-IDCATRAD-FROM    PIC 9(4).                                    
006500*                                 KATALOG-RADNR-SPAR                      
006600     03 MID-IDCATRAD-MITT    PIC 9(4).                                    
006700*                                 KATALOG-RADNR-SPAR                      
006800     03 MID-IDCATRAD-TOM     PIC 9(4).                                    
006900*                                 KATALOG-RADNR-SPAR                      
007000     03 MID-IDCATRAD-FIRST   PIC 9(4).                                    
007100*                                 KATALOG-RADNR-SPAR                      
007200     03 MID-IDCATRAD-SISTA   PIC 9(4).                                    
007300*                                 KATALOG-RADNR-SPAR                      
007400     03 MID-IDCATRAD-UPD     PIC 9(4).                                    
007500*                                 RADNUMMER                               
007600*                                 ROW NUMBER IN TEXT BLOCK                
007700     03 MID-KDCATPUB-R-FOM-UPD                                            
007800                             PIC X(3).                                    
007900*                                 3 HÖGRASTE TECKNEN I KDCATPUB           
008000*                                 3 RIGHTMOST CHAR  IN KDCATPUB           
008100     03 MID-KDCATPUB-R-TOM-UPD                                            
008200                             PIC X(3).                                    
008300*                                 3 HÖGRASTE TECKNEN I KDCATPUB           
008400*                                 3 RIGHTMOST CHAR  IN KDCATPUB           
008500     03 MID-KDFBX-UPD        PIC X.                                       
008600*                                 FBX-KOD                                 
008700*                                 FBX CODE VALUES:                        
008800*                                 H = MAIN LINE                           
008900*                                 F = CONTINUED LINE (NO BREAK)           
009000*                                 B = PAGE BREAK AFTER THIS LINE          
009100*                                 X = NO PAGE BREAK HERE                  
009200*                                     (MUST BE PRECEDED BY "H")           
009300*                                 SPACE = NORMAL BREAK RULES              
009400     03 MID-IDCATPOS-UPD     PIC X(3).                                    
009500*                                 POSITIONSNUMMER                         
009600*                                 FIGURE NUMBER                           
009700     03 MID-IDARTNR-UPD      PIC 9(9).                                    
009800*                                 ARTIKELNUMMER                           
009900*                                 PART NUMBER                             
010000     03 MID-KVKOL-GRP.                                                    
010100        05 MID-KVKOL-UPD     OCCURS 5 TIMES                               
010200                             PIC X(3).                                    
010300*                                 ANTAL AV ARTIKEL I RESP KOLUMN          
010400*                                 QTY. OF PARTS IN RESP. COLUMN           
010500     03 MID-KDPS-UPD         PIC X(2).                                    
010600*                                 ARTIKELSTATUS                           
010700*                                 PART STATUS CODE                        
010800*                                 SPACE = NORMAL STORED PART              
010900*                                 LS= LOCALLY STORED (NOT STORED          
011000*                                 BY C1/C2)                               
011100*                                 NS= NEVER STORED BY VOLVO               
011200*                                 OP= OBSOLETED PART                      
011300*                                 SP= SUPERSEDED PART                     
011400*                                 IK= INCLUDED IN KIT                     
011500*                                 EU= EXCHANGE UNIT                       
011600*                                 KS= PART NOT YET STORED                 
011700*                                 XX= PART DESCR. NOT TRANSLATABL         
011800*                                 E                                       
011900*                                 KN= "IK" AND "NS" TOGETHER              
012000*                                 KL= "IK" AND "LS" TOGETHER              
012100*                                 SW= SOFTWARE SERVICED BY VADIS          
012200     03 MID-KVPUNKT-UPD      PIC 9.                                       
012300*                                 ANTAL INDRAGNINGSPUNKTER                
012400*                                 QTY. INDENTING DOTS                     
012500*                                 0 = COMPLETE PART                       
012600*                                 1 = 1ST LEVEL OF INCL. PART             
012700*                                 2 = 2ND LEVEL OF INCL. PART             
012800*                                 3 = 3RD LEVEL OF INCL. PART             
012900*                                 4 = 4TH LEVEL OF INCL. PART             
013000     03 MID-BEART-UPD        PIC X(25).                                   
013100*                                 ARTIKELBENÄMNING                        
013200*                                 PART DESCRIPTION                        
013300     03 MID-KDHOM-UPD        PIC 9.                                       
013400*                                 HOMONYMKOD                              
013500*                                 HOMONYMOUS CODE                         
013600     03 MID-TEKATANM-UPD     PIC X(23).                                   
013700*                                 ANMÄRKNINGSTEXT                         
013800*                                 NOTES TEXT                              
013900     03 MID-IDCATGRP-H-UPD   PIC 9(2).                                    
014000*                                 KATALOG-GRUPP                           
014100*                                 CATALOG-GROUP                           
014200     03 MID-IDCATAVS-H-UPD   PIC 9(4).                                    
014300*                                 KATALOG-AVSNITT                         
014400*                                 CATALOG TEXT BLOCK                      
014500     03 MID-IDCATRAD-H-UPD   PIC 9(4).                                    
014600*                                 RADNUMMER                               
014700*                                 ROW NUMBER IN TEXT BLOCK                
014800     03 MID-KDCATPUB-R-H-UPD PIC X(3).                                    
014900*                                 3 HÖGRASTE TECKNEN I KDCATPUB           
015000*                                 3 RIGHTMOST CHAR  IN KDCATPUB           
015100     03 MID-KDHAEN-UPD       PIC X.                                       
015200*                                 HÄNVISNINGSKODER VÄRDEN:                
015300*                                  A = HÄNVISNING I ANM. FÄLTET           
015400*                                  B = HÄNVISNING I BEN. FÄLTET           
015500*                                  * = HÄNVISNING, EJ KLAR DEST.          
015600*                                  SPACE = INGEN HÄNVISNING               
015700*                                 REFERENCE CODE   VALUES:                
015800*                                  A = REFERENCE IN NOTE-FIELD            
015900*                                  B = REFERENCE IN DESCR-FIELD           
016000*                                  * = REFERENCE DURING EDITING           
016100*                                  SPACE = NO REFERENCE                   
016200     03 MID-IDCATRAD-BORT-UPD                                             
016300                             PIC 9(4).                                    
016400*                                 RADNUMMER                               
016500*                                 ROW NUMBER IN TEXT BLOCK                
016600     03 MID-KDCATPUB-R-BORT-UPD                                           
016700                             PIC X(3).                                    
016800*                                 3 HÖGRASTE TECKNEN I KDCATPUB           
016900*                                 3 RIGHTMOST CHAR  IN KDCATPUB           
017000     03 MID-IDCATPOS-SOEK-UPD                                             
017100                             PIC X(3).                                    
017200*                                 POSITIONSNUMMER                         
017300*                                 FIGURE NUMBER                           
017400     03 MID-IDRUBNR-UPD      OCCURS 3 TIMES                               
017500                             PIC 9(5).                                    
017600*                                 RUBRIKNUMMER                            
017700*                                 HEADLINE ID NUMBER                      
017800     03 MID-IDTTEXNR-UPD     PIC 9(5).                                    
017900*                                 TILLÄGGSTEXT-NR                         
018000*                                 ADDITIONAL TEXT, ID NUMBER              
018100     03 MID-IDFOTNR-UPD      OCCURS 3 TIMES                               
018200                             PIC 9(5).                                    
018300*                                 FOTNOTSNUMMER                           
018400*                                 FOOT NOTE ID NUMBER                     
018500*** END OF VILMAII-COPY LENGTH= 227 BYTES                                 
