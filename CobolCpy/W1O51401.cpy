000100 01  MOD-W1O51401.                                                        
000200*                                 MOD-COPYTEXT FÖR BILD                   
000300*                                 AVSNITTSHUVUD KATALOG                   
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
003000     03 MOD-KDCATPUB-R-IN    PIC X(3).                                    
003100*                                 3 HÖGRASTE TECKNEN I KDCATPUB           
003200     03 MOD-KDCATPUB-R-UT    PIC X(3).                                    
003300*                                 3 HÖGRASTE TECKNEN I KDCATPUB           
003400     03 MOD-KDCATPUB-R-NEXT  PIC X(3).                                    
003500*                                 3 HÖGRASTE TECKNEN I KDCATPUB           
003600     03 MOD-KDCATPUB-R-COPY-ATTR                                          
003700                             PIC X(2).                                    
003800*                                 MFS ATTRIBUTFÄLT                        
003900     03 MOD-KDCATPUB-R-COPY  PIC X(3).                                    
004000*                                 3 HÖGRASTE TECKNEN I KDCATPUB           
004100     03 MOD-BORT-ATTR        PIC X(2).                                    
004200*                                 MFS ATTRIBUTFÄLT                        
004300     03 MOD-BORT             PIC X.                                       
004400*                                 ALLMÄN FLAGGA                           
004500     03 MOD-KDCATPUB-R-FROM-ATTR                                          
004600                             PIC X(2).                                    
004700*                                 MFS ATTRIBUTFÄLT                        
004800     03 MOD-KDCATPUB-R-FROM  PIC X(3).                                    
004900*                                 3 HÖGRASTE TECKNEN I KDCATPUB           
005000     03 MOD-KDCATPUB-R-TOM   PIC X(3).                                    
005100*                                 3 HÖGRASTE TECKNEN I KDCATPUB           
005200     03 MOD-KDCATPUB-R-TOM-ATTR                                           
005300                             PIC X(2).                                    
005400*                                 MFS ATTRIBUTFÄLT                        
005500     03 MOD-KDCATPUB-R-TOM-IN                                             
005600                             PIC X(3).                                    
005700*                                 3 HÖGRASTE TECKNEN I KDCATPUB           
005800     03 MOD-IDRUBNR-GRP      OCCURS 5 TIMES.                              
005900        05 MOD-IDRUBNR-ATTR  PIC X(2).                                    
006000*                                 MFS ATTRIBUTFÄLT                        
006100        05 MOD-IDRUBNR       PIC Z(5).                                    
006200*                                 RUBRIKNUMMER                            
006300     03 MOD-BERUBTEXT-GRP.                                                
006400        05 MOD-BERUBTEXT-1-ATTR                                           
006500                             PIC X(2).                                    
006600*                                 MFS ATTRIBUTFÄLT                        
006700        05 MOD-BERUBTEXT-1   PIC X(30).                                   
006800*                                 RUBRIKTEXT                              
006900        05 MOD-BERUBTEXT-2-ATTR                                           
007000                             PIC X(2).                                    
007100*                                 MFS ATTRIBUTFÄLT                        
007200        05 MOD-BERUBTEXT-2   PIC X(30).                                   
007300*                                 RUBRIKTEXT                              
007400     03 MOD-VADIS-BERUBTEXT-GRP.                                          
007500        05 MOD-BERUBTEXT-3-ATTR                                           
007600                             PIC X(2).                                    
007700*                                 MFS ATTRIBUTFÄLT                        
007800        05 MOD-BERUBTEXT-3   PIC X(30).                                   
007900*                                 RUBRIKTEXT                              
008000        05 MOD-BERUBTEXT-4-ATTR                                           
008100                             PIC X(2).                                    
008200*                                 MFS ATTRIBUTFÄLT                        
008300        05 MOD-BERUBTEXT-4   PIC X(30).                                   
008400*                                 RUBRIKTEXT                              
008500     03 MOD-TEKOL-GRP        OCCURS 5 TIMES.                              
008600        05 MOD-TEKOL-ATTR    PIC X(2).                                    
008700*                                 MFS ATTRIBUTFÄLT                        
008800        05 MOD-TEKOL         PIC X(25).                                   
008900*                                 KOLUMNTEXT                              
009000     03 MOD-IDFOTNR-RAD4-GRP OCCURS 3 TIMES.                              
009100        05 MOD-IDFOTNR-RAD4-ATTR                                          
009200                             PIC X(2).                                    
009300*                                 MFS ATTRIBUTFÄLT                        
009400        05 MOD-IDFOTNR-RAD4  PIC Z(5).                                    
009500*                                 FOTNOTSNUMMER                           
009600     03 MOD-IDFOTNR-GRP      OCCURS 5 TIMES.                              
009700        05 FILLER            OCCURS 3 TIMES.                              
009800           07 MOD-IDFOTNR-ATTR                                            
009900                             PIC X(2).                                    
010000*                                 MFS ATTRIBUTFÄLT                        
010100           07 MOD-IDFOTNR    PIC Z(5).                                    
010200*                                 FOTNOTSNUMMER                           
010300     03 MOD-IDILLU-ATTR      PIC X(2).                                    
010400*                                 MFS ATTRIBUTFÄLT                        
010500     03 MOD-IDILLU           PIC Z(5).                                    
010600*                                 ILLUSTRATIONENS NR                      
010700     03 MOD-TEMFSINF         PIC X(55).                                   
010800*                                 INFORMATIONSMEDDELANDE                  
010900*** END OF VILMAII-COPY LENGTH= 596 BYTES                                 
