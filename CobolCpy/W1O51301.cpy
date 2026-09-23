000100 01  MOD-W1O51301.                                                        
000200*                                 MOD-COPYTEXT FÖR BILD                   
000300*                                 RADNOTERING, HÄNVISNING, REF            
000400     03 MOD-IDTRANS          PIC X(4).                                    
000500*                                 BILDNUMMER                              
000600     03 MOD-TEMFSFEL         PIC X(40).                                   
000700*                                 MFS FELMEDDELANDE                       
000800     03 MOD-IDCATNR-IN       PIC X(5).                                    
000900*                                 KATALOG-ID                              
001000     03 MOD-IDCATNR-UT       PIC X(5).                                    
001100*                                 KATALOG-ID                              
001200     03 MOD-IDCATGRP-IN      PIC X(2).                                    
001300*                                 KATALOG-GRUPP                           
001400     03 MOD-IDCATGRP-UT      PIC X(2).                                    
001500*                                 KATALOG-GRUPP                           
001600     03 MOD-IDCATAVS-IN      PIC X(4).                                    
001700*                                 KATALOG-AVSNITT                         
001800     03 MOD-IDCATAVS-UT      PIC X(4).                                    
001900*                                 KATALOG-AVSNITT                         
002000     03 MOD-IDSKYLT-IN       PIC X(3).                                    
002100*                                 NATIONALITETSTECKEN                     
002200*                                 SPRÅKIDENTIFIKATION                     
002300     03 MOD-IDSKYLT-UT       PIC X(3).                                    
002400*                                 NATIONALITETSTECKEN                     
002500*                                 SPRÅKIDENTIFIKATION                     
002600     03 MOD-IDCATRAD-IN      PIC X(4).                                    
002700*                                 RADNUMMER                               
002800     03 MOD-IDCATRAD-UT      PIC X(4).                                    
002900*                                 RADNUMMER                               
003000     03 MOD-KDCATPUB-R-FOM-IN                                             
003100                             PIC X(3).                                    
003200*                                 3 HÖGRASTE TECKNEN I KDCATPUB           
003300     03 MOD-KDCATPUB-R-FOM-UT                                             
003400                             PIC X(3).                                    
003500*                                 3 HÖGRASTE TECKNEN I KDCATPUB           
003600     03 MOD-KDCATPUB-R-MIN   PIC X(3).                                    
003700*                                 3 HÖGRASTE TECKNEN I KDCATPUB           
003800     03 MOD-KDCATPUB-R-MAX   PIC X(3).                                    
003900*                                 3 HÖGRASTE TECKNEN I KDCATPUB           
004000     03 MOD-TENOTE-ATTR      PIC X(2).                                    
004100*                                 MFS ATTRIBUTFÄLT                        
004200     03 MOD-TENOTE           PIC X(40).                                   
004300*                                 NOTERINGSFÄLT                           
004400     03 MOD-IDCATGRP         PIC Z9.                                      
004500*                                 KATALOG-GRUPP                           
004600     03 MOD-IDCATAVS         PIC Z(3)9.                                   
004700*                                 KATALOG-AVSNITT                         
004800     03 MOD-IDCATRAD         PIC Z(3)9.                                   
004900*                                 RADNUMMER                               
005000     03 MOD-KDCATPUB-R-FOM   PIC X(3).                                    
005100*                                 3 HÖGRASTE TECKNEN I KDCATPUB           
005200     03 MOD-KDCATPUB-R-TOM   PIC X(3).                                    
005300*                                 3 HÖGRASTE TECKNEN I KDCATPUB           
005400     03 MOD-KDHAEN           PIC X.                                       
005500*                                 HÄNVISNINGSKODER VÄRDEN:                
005600*                                  A = HÄNVISNING I ANM. FÄLTET           
005700*                                  B = HÄNVISNING I BEN. FÄLTET           
005800*                                  * = HÄNVISNING, EJ KLAR DEST.          
005900*                                  SPACE = INGEN HÄNVISNING               
006000     03 MOD-REF-GRP-RAD      OCCURS 28 TIMES.                             
006100        05 MOD-REF-IDCATGRP  PIC Z9.                                      
006200*                                 KATALOG-GRUPP                           
006300        05 MOD-REF-IDCATAVS  PIC Z(3)9.                                   
006400*                                 KATALOG-AVSNITT                         
006500        05 MOD-REF-IDCATRAD  PIC Z(3)9.                                   
006600*                                 RADNUMMER                               
006700        05 MOD-REF-KDCATPUB-R-FOM                                         
006800                             PIC X(3).                                    
006900*                                 3 HÖGRASTE TECKNEN I KDCATPUB           
007000        05 MOD-REF-KDHAEN    PIC X.                                       
007100*                                 HÄNVISNINGSKODER VÄRDEN:                
007200*                                  A = HÄNVISNING I ANM. FÄLTET           
007300*                                  B = HÄNVISNING I BEN. FÄLTET           
007400*                                  * = HÄNVISNING, EJ KLAR DEST.          
007500*                                  SPACE = INGEN HÄNVISNING               
007600     03 MOD-REF-KDHAEN-AVS   PIC X.                                       
007700*                                 HÄNVISNINGSKODER VÄRDEN:                
007800*                                  A = HÄNVISNING I ANM. FÄLTET           
007900*                                  B = HÄNVISNING I BEN. FÄLTET           
008000*                                  * = HÄNVISNING, EJ KLAR DEST.          
008100*                                  SPACE = INGEN HÄNVISNING               
008200     03 MOD-REF-OCH-PUB-KOLUMN                                            
008300                             OCCURS 5 TIMES.                              
008400        05 FILLER            OCCURS 9 TIMES.                              
008500           07 MOD-REF-KDHAEN-KOL                                          
008600                             PIC X.                                       
008700*                                 HÄNVISNINGSKODER VÄRDEN:                
008800*                                  A = HÄNVISNING I ANM. FÄLTET           
008900*                                  B = HÄNVISNING I BEN. FÄLTET           
009000*                                  * = HÄNVISNING, EJ KLAR DEST.          
009100*                                  SPACE = INGEN HÄNVISNING               
009200           07 MOD-REF-KDCATPUB-R-FOM-KOL                                  
009300                             PIC X(3).                                    
009400*                                 3 HÖGRASTE TECKNEN I KDCATPUB           
009500     03 MOD-TEMFSINF         PIC X(55).                                   
009600*                                 INFORMATIONSMEDDELANDE                  
009700*** END OF VILMAII-COPY LENGTH= 779 BYTES                                 
