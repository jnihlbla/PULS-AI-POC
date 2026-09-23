000100 01  MID-W1I54101.                                                        
000200*                                 MID-COPYTEXTEN FÖR BILD                 
000300*                                 ARTIKELFRÅGA I KATALOG                  
000400     03 MID-IDARTNR-IN       PIC X(9).                                    
000500*                                 ARTIKELNUMMER                           
000600     03 MID-IDARTNR-UT       PIC X(9).                                    
000700*                                 ARTIKELNUMMER                           
000800     03 MID-IDCATNR-IN       PIC X(5).                                    
000900*                                 KATALOG-ID                              
001000     03 MID-IDCATNR-UT       PIC X(5).                                    
001100*                                 KATALOG-ID                              
001200     03 MID-SEGM-NYCKEL-SPAR.                                             
001300*                                 DOLD SEGMENT NYCKEL                     
001400        05 MID-IDCATGRP-SPAR PIC 9(2).                                    
001500*                                 KATALOG-GRUPP                           
001600        05 MID-IDCATAVS-SPAR PIC 9(4).                                    
001700*                                 KATALOG-AVSNITT                         
001800        05 MID-IDCATRAD-SPAR PIC 9(4).                                    
001900*                                 RADNUMMER                               
002000     03 MID-SEGM-NYCKEL-NEXT.                                             
002100*                                 DOLD SEGMENT NYCKEL                     
002200        05 MID-IDCATGRP-NEXT PIC 9(2).                                    
002300*                                 KATALOG-GRUPP                           
002400        05 MID-IDCATAVS-NEXT PIC 9(4).                                    
002500*                                 KATALOG-AVSNITT                         
002600        05 MID-IDCATRAD-NEXT PIC 9(4).                                    
002700*                                 RADNUMMER                               
002800     03 MID-IDCATNR-NEXT     PIC 9(5).                                    
002900*                                 KATALOG-ID                              
003000     03 MID-SIDRAEK          PIC X.                                       
003100     03 MID-KDCATPUB-R-SPAR  PIC X(3).                                    
003200*                                 3 HÖGRASTE TECKNEN I KDCATPUB           
003300     03 MID-KDCATPUB-R-NEXT  PIC X(3).                                    
003400*                                 3 HÖGRASTE TECKNEN I KDCATPUB           
003500     03 MID-IDCATNR          PIC 9(5).                                    
003600*                                 KATALOG-ID                              
003700*** END OF VILMAII-COPY LENGTH= 65 BYTES                                  
