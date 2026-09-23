000100 01  MOD-W90421O1.                                                        
000200*                                 MOD-COPYTEXT FÖR W9042100               
000300*                                 ARTIKELFRÅGA I KATALOG                  
000400     03 MOD-IDTRANS          PIC X(4).                                    
000500*                                 BILDNUMMER                              
000600     03 MOD-TEMFSFEL         PIC X(40).                                   
000700*                                 MFS FELMEDDELANDE                       
000800     03 FILLER               PIC X(9).                                    
000900     03 FILLER               PIC X(9).                                    
001000     03 FILLER               PIC X(2).                                    
001100     03 FILLER               PIC X(5).                                    
001200     03 MOD-IDCATNR-UT       PIC X(5).                                    
001300*                                 KATALOG-ID                              
001400     03 MOD-SEGM-NYCKEL-SPAR.                                             
001500*                                 DOLD SEGMENT NYCKEL                     
001600        05 MOD-IDCATGRP-SPAR PIC 9(2).                                    
001700*                                 KATALOG-GRUPP                           
001800        05 MOD-IDCATAVS-SPAR PIC 9(4).                                    
001900*                                 KATALOG-AVSNITT                         
002000        05 MOD-IDCATRAD-SPAR PIC 9(4).                                    
002100*                                 RADNUMMER                               
002200     03 MOD-SEGM-NYCKEL-NEXT.                                             
002300*                                 DOLD SEGMENT NYCKEL                     
002400        05 MOD-IDCATGRP-NEXT PIC 9(2).                                    
002500*                                 KATALOG-GRUPP                           
002600        05 MOD-IDCATAVS-NEXT PIC 9(4).                                    
002700*                                 KATALOG-AVSNITT                         
002800        05 MOD-IDCATRAD-NEXT PIC 9(4).                                    
002900*                                 RADNUMMER                               
003000     03 MOD-IDCATNR-NEXT     PIC X(5).                                    
003100*                                 KATALOG-ID                              
003200     03 FILLER               PIC X.                                       
003300     03 MOD-KDCATPUB-R-SPAR  PIC X(3).                                    
003400*                                 DE 3 HÖGRA TECKNEN I KDCATPUB           
003500     03 MOD-KDCATPUB-R-NEXT  PIC X(3).                                    
003600*                                 DE 3 HÖGRA TECKNEN I KDCATPUB           
003700     03 FILLER               PIC X(25).                                   
003800     03 FILLER               PIC Z(4)9.                                   
003900     03 MOD-BEMASTER         PIC X(12).                                   
004000*                                 MASTERNAMN FÖR FORDON                   
004100     03 MOD-UTRAD            OCCURS 13 TIMES.                             
004200        05 MOD-KOL           OCCURS 3 TIMES.                              
004300           07 FILLER         PIC 9(2).                                    
004400           07 FILLER         PIC X.                                       
004500           07 FILLER         PIC 9(4).                                    
004600           07 FILLER         PIC X.                                       
004700           07 FILLER         PIC 9(4).                                    
004800           07 FILLER         PIC X.                                       
004900           07 FILLER         PIC X(3).                                    
005000           07 FILLER         PIC X(2).                                    
005100           07 FILLER         PIC X(3).                                    
005200           07 FILLER         PIC X.                                       
005300           07 FILLER         PIC X(3).                                    
005400     03 MOD-TEMFSINF         PIC X(55).                                   
005500*                                 INFORMATIONSMEDDELANDE                  
005600*** END OF VILMAII-COPY LENGTH= 1178 BYTES                                
