000100 01  MOD-W90415O1.                                                        
000200*                                 MOD-COPYTEXT FÖR W90415O1               
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDARTNR-IN       PIC X(9).                                    
000800*                                 ARTIKELNUMMER                           
000900     03 MOD-IDARTNR-UT       PIC X(9).                                    
001000*                                 ARTIKELNUMMER                           
001100     03 MOD-FILLER           PIC X(3).                                    
001200     03 MOD-FILLER           PIC X(3).                                    
001300     03 MOD-IDRADNR-IN       PIC X(4).                                    
001400*                                 RADNUMMER                               
001500     03 MOD-IDRADNR-UT       PIC X(4).                                    
001600*                                 RADNUMMER                               
001700     03 MOD-IDRADNR-DOLT     PIC 9(4).                                    
001800*                                 RADNUMMER                               
001900     03 MOD-IDRADNR-DOLT2    PIC 9(4).                                    
002000*                                 RADNUMMER                               
002100     03 MOD-FILLER           PIC X(9).                                    
002200     03 MOD-FILLER           PIC X(25).                                   
002300     03 MOD-FILLER           PIC X(2).                                    
002400     03 MOD-FILLER           PIC X(4).                                    
002500     03 MOD-FILLER           PIC X.                                       
002600     03 MOD-FILLER           PIC X(2).                                    
002700     03 MOD-OUTPUT           OCCURS 12 TIMES.                             
002800*                                                                         
002900        05 MOD-FILLER        PIC X(2).                                    
003000        05 MOD-FILLER        PIC X.                                       
003100        05 MOD-IDRADNR       PIC 9(4).                                    
003200*                                 RADNUMMER                               
003300        05 MOD-UTRAD         PIC X(72).                                   
003400     03 MOD-FILLER           PIC X(2).                                    
003500     03 MOD-FILLER           PIC X.                                       
003600     03 MOD-TEMFSINF         PIC X(55).                                   
003700*                                 INFORMATIONSMEDDELANDE                  
003800*** END OF VILMAII-COPY LENGTH= 1133 BYTES                                
