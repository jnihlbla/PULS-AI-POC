000100 01  MID-W90421I1.                                                        
000200*                                 MID-COPYTEXTEN FÖR BILD                 
000300*                                 ARTIKELFRÅGA I KATALOG                  
000400     03 MID-IDARTNR-IN       PIC X(9).                                    
000500*                                 ARTIKELNUMMER                           
000600     03 MID-FILLER           PIC X(9).                                    
000700     03 MID-FILLER           PIC X(5).                                    
000800     03 MID-FILLER           PIC X(5).                                    
000900     03 MID-SEGM-NYCKEL-SPAR.                                             
001000*                                 DOLD SEGMENT NYCKEL                     
001100        05 MID-IDCATGRP-SPAR PIC 9(2).                                    
001200*                                 KATALOG-GRUPP                           
001300        05 MID-IDCATAVS-SPAR PIC 9(4).                                    
001400*                                 KATALOG-AVSNITT                         
001500        05 MID-IDCATRAD-SPAR PIC 9(4).                                    
001600*                                 RADNUMMER                               
001700     03 MID-SEGM-NYCKEL-NEXT.                                             
001800*                                 DOLD SEGMENT NYCKEL                     
001900        05 MID-IDCATGRP-NEXT PIC 9(2).                                    
002000*                                 KATALOG-GRUPP                           
002100        05 MID-IDCATAVS-NEXT PIC 9(4).                                    
002200*                                 KATALOG-AVSNITT                         
002300        05 MID-IDCATRAD-NEXT PIC 9(4).                                    
002400*                                 RADNUMMER                               
002500     03 MID-IDCATNR-NEXT     PIC 9(5).                                    
002600*                                 KATALOG-ID                              
002700     03 MID-FILLER           PIC X.                                       
002800     03 MID-KDCATPUB-R-SPAR  PIC X(3).                                    
002900*                                 DE 3 HÖGRA TECKNEN I KDCATPUB           
003000     03 MID-KDCATPUB-R-NEXT  PIC X(3).                                    
003100*                                 DE 3 HÖGRA TECKNEN I KDCATPUB           
003200     03 MID-IDCATNR          PIC 9(5).                                    
003300*                                 KATALOG-ID                              
003400*** END OF VILMAII-COPY LENGTH= 65 BYTES                                  
