000100 01  MID-W1I51401.                                                        
000200*                                 MID-COPYTEXT FÖR BILD                   
000300*                                 AVSNITTSHUVUD                           
000400     03 MID-IDCATNR-IN       PIC X(5).                                    
000500*                                 KATALOG-ID                              
000600     03 MID-IDCATNR-UT       PIC X(5).                                    
000700*                                 KATALOG-ID                              
000800     03 MID-IDCATGRP-IN      PIC X(2).                                    
000900*                                 KATALOG-GRUPP                           
001000     03 MID-IDCATGRP-UT      PIC X(2).                                    
001100*                                 KATALOG-GRUPP                           
001200     03 MID-IDCATAVS-IN      PIC X(4).                                    
001300*                                 KATALOG-AVSNITT                         
001400     03 MID-IDCATAVS-UT      PIC X(4).                                    
001500*                                 KATALOG-AVSNITT                         
001600     03 MID-IDSKYLT-IN       PIC X(3).                                    
001700*                                 NATIONALITETSTECKEN                     
001800*                                 SPRÅKIDENTIFIKATION                     
001900     03 MID-IDSKYLT-UT       PIC X(3).                                    
002000*                                 NATIONALITETSTECKEN                     
002100*                                 SPRÅKIDENTIFIKATION                     
002200     03 MID-IDCATRAD-IN      PIC X(4).                                    
002300*                                 RADNUMMER                               
002400     03 MID-IDCATRAD-UT      PIC X(4).                                    
002500*                                 RADNUMMER                               
002600     03 MID-KDCATPUB-R-IN    PIC X(3).                                    
002700*                                 3 HÖGRASTE TECKNEN I KDCATPUB           
002800     03 MID-KDCATPUB-R-UT    PIC X(3).                                    
002900*                                 3 HÖGRASTE TECKNEN I KDCATPUB           
003000     03 MID-KDCATPUB-R-NEXT  PIC X(3).                                    
003100*                                 3 HÖGRASTE TECKNEN I KDCATPUB           
003200     03 MID-KDCATPUB-R-COPY  PIC X(3).                                    
003300*                                 3 HÖGRASTE TECKNEN I KDCATPUB           
003400     03 MID-BORT             PIC X.                                       
003500*                                 ALLMÄN FLAGGA                           
003600     03 MID-KDCATPUB-R-FROM  PIC X(3).                                    
003700*                                 3 HÖGRASTE TECKNEN I KDCATPUB           
003800     03 MID-KDCATPUB-R-TOM   PIC X(3).                                    
003900*                                 3 HÖGRASTE TECKNEN I KDCATPUB           
004000     03 MID-IDRUBNR          OCCURS 5 TIMES                               
004100                             PIC 9(5).                                    
004200*                                 RUBRIKNUMMER                            
004300     03 MID-BERUBTEXT-GRP.                                                
004400        05 MID-BERUBTEXT-1   PIC X(30).                                   
004500*                                 RUBRIKTEXT                              
004600        05 MID-BERUBTEXT-2   PIC X(30).                                   
004700*                                 RUBRIKTEXT                              
004800     03 MID-VADIS-BERUBTEXT-GRP.                                          
004900        05 MID-BERUBTEXT-3   PIC X(30).                                   
005000*                                 RUBRIKTEXT                              
005100        05 MID-BERUBTEXT-4   PIC X(30).                                   
005200*                                 RUBRIKTEXT                              
005300     03 MID-TEKOL            OCCURS 5 TIMES                               
005400                             PIC X(25).                                   
005500*                                 KOLUMNTEXT                              
005600     03 MID-IDFOTNR-RAD4     OCCURS 3 TIMES                               
005700                             PIC 9(5).                                    
005800*                                 FOTNOTSNUMMER                           
005900     03 MID-IDFOTNR-GRP      OCCURS 5 TIMES.                              
006000        05 MID-IDFOTNR       OCCURS 3 TIMES                               
006100                             PIC 9(5).                                    
006200*                                 FOTNOTSNUMMER                           
006300     03 MID-IDILLU           PIC 9(5).                                    
006400*                                 ILLUSTRATIONENS NR                      
006500*** END OF VILMAII-COPY LENGTH= 420 BYTES                                 
