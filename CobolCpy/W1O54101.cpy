000100 01  MOD-W1O54101.                                                        
000200*                                 MOD-COPYTEXT FÖR BILD 1541              
000300*                                 ARTIKELFRÅGA I KATALOG                  
000400     03 MOD-IDTRANS          PIC X(4).                                    
000500*                                 BILDNUMMER                              
000600     03 MOD-TEMFSFEL         PIC X(40).                                   
000700*                                 MFS FELMEDDELANDE                       
000800     03 MOD-IDARTNR-IN       PIC X(9).                                    
000900*                                 ARTIKELNUMMER                           
001000     03 MOD-IDARTNR-UT       PIC X(9).                                    
001100*                                 ARTIKELNUMMER                           
001200     03 MOD-IDCATNR-IN-ATTR  PIC X(2).                                    
001300*                                 MFS ATTRIBUTFÄLT                        
001400     03 MOD-IDCATNR-IN       PIC X(5).                                    
001500*                                 KATALOG-ID                              
001600     03 MOD-IDCATNR-UT       PIC X(5).                                    
001700*                                 KATALOG-ID                              
001800     03 MOD-SEGM-NYCKEL-SPAR.                                             
001900*                                 DOLD SEGMENT NYCKEL                     
002000        05 MOD-IDCATGRP-SPAR PIC 9(2).                                    
002100*                                 KATALOG-GRUPP                           
002200        05 MOD-IDCATAVS-SPAR PIC 9(4).                                    
002300*                                 KATALOG-AVSNITT                         
002400        05 MOD-IDCATRAD-SPAR PIC 9(4).                                    
002500*                                 RADNUMMER                               
002600     03 MOD-SEGM-NYCKEL-NEXT.                                             
002700*                                 DOLD SEGMENT NYCKEL                     
002800        05 MOD-IDCATGRP-NEXT PIC 9(2).                                    
002900*                                 KATALOG-GRUPP                           
003000        05 MOD-IDCATAVS-NEXT PIC 9(4).                                    
003100*                                 KATALOG-AVSNITT                         
003200        05 MOD-IDCATRAD-NEXT PIC 9(4).                                    
003300*                                 RADNUMMER                               
003400     03 MOD-IDCATNR-NEXT     PIC X(5).                                    
003500*                                 KATALOG-ID                              
003600     03 MOD-SIDRAEK          PIC X.                                       
003700     03 MOD-KDCATPUB-R-SPAR  PIC X(3).                                    
003800*                                 3 HÖGRASTE TECKNEN I KDCATPUB           
003900     03 MOD-KDCATPUB-R-NEXT  PIC X(3).                                    
004000*                                 3 HÖGRASTE TECKNEN I KDCATPUB           
004100     03 MOD-BEART            PIC X(25).                                   
004200*                                 ARTIKELBENÄMNING                        
004300     03 MOD-IDCATNR          PIC Z(4)9.                                   
004400*                                 KATALOG-ID                              
004500     03 MOD-BEMASTER         PIC X(12).                                   
004600*                                 MASTERNAMN FÖR FORDON                   
004700     03 MOD-UTRAD            OCCURS 13 TIMES.                             
004800        05 MOD-KOL           OCCURS 3 TIMES.                              
004900           07 MOD-IDCATGRP   PIC Z9.                                      
005000*                                 KATALOG-GRUPP                           
005100           07 FILLER         PIC X.                                       
005200           07 MOD-IDCATAVS   PIC Z(3)9.                                   
005300*                                 KATALOG-AVSNITT                         
005400           07 FILLER         PIC X.                                       
005500           07 MOD-IDCATRAD   PIC Z(3)9.                                   
005600*                                 RADNUMMER                               
005700           07 FILLER         PIC X.                                       
005800           07 MOD-KDCATPUB-R PIC X(3).                                    
005900*                                 3 HÖGRASTE TECKNEN I KDCATPUB           
006000           07 FILLER         PIC X(2).                                    
006100           07 MOD-IDCATPOS   PIC X(3).                                    
006200*                                 POSITIONSNUMMER                         
006300           07 FILLER         PIC X.                                       
006400           07 MOD-KVKOL      PIC X(3).                                    
006500*                                 ANTAL AV ARTIKEL I RESP KOLUMN          
006600     03 MOD-TEMFSINF         PIC X(55).                                   
006700*                                 INFORMATIONSMEDDELANDE                  
006800*** END OF VILMAII-COPY LENGTH= 1178 BYTES                                
