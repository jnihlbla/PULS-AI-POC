000100 01  MOD-W3O31401.                                                        
000200*                                 MOD-COPYTEXT                            
000300*                                 FÖR W3O314                              
000400     03 MOD-IDTRANS          PIC X(4).                                    
000500*                                 BILDNUMMER                              
000600     03 MOD-TEMFSFEL         PIC X(40).                                   
000700*                                 MFS FELMEDDELANDE                       
000800     03 MOD-IDPROMR-IN.                                                   
000900*                                 PRISOMRÅDE (RABATTSTRUKTUR)             
001000        05 MOD-IDMARKBO      PIC X.                                       
001100*                                 MARKNADSBOLAGSKOD                       
001200        05 MOD-IDPROMRN      PIC X(2).                                    
001300*                                 PRISOMRÅDE LÖPNUMMER                    
001400     03 MOD-IDPROMR-UT.                                                   
001500*                                 PRISOMRÅDE (RABATTSTRUKTUR)             
001600        05 MOD-IDMARKBO      PIC X.                                       
001700*                                 MARKNADSBOLAGSKOD                       
001800        05 MOD-IDPROMRN      PIC X(2).                                    
001900*                                 PRISOMRÅDE LÖPNUMMER                    
002000     03 MOD-IDARTNR-IN       PIC X(9).                                    
002100*                                 ARTIKELNUMMER                           
002200     03 MOD-IDARTNR-UT       PIC X(9).                                    
002300*                                 ARTIKELNUMMER                           
002400     03 MOD-KDARTKAM-IN      PIC X(5).                                    
002500*                                 TRANSFER KOD                            
002600     03 MOD-KDARTKAM-UT      PIC X(5).                                    
002700*                                 TRANSFER KOD                            
002800     03 MOD-IDDISTR-IN       PIC X(4).                                    
002900*                                 DISTRIKTNUMMER                          
003000     03 MOD-IDDISTR-UT       PIC X(4).                                    
003100*                                 DISTRIKTNUMMER                          
003200     03 MOD-IDARTNR-ENTER    PIC X(9).                                    
003300*                                 ARTIKELNUMMER                           
003400     03 MOD-IDARTNR-NEXT     PIC X(9).                                    
003500*                                 ARTIKELNUMMER                           
003600     03 MOD-IDMARKBO-ENTER   PIC X.                                       
003700*                                 MARKNADSBOLAGSKOD                       
003800     03 MOD-IDMARKBO-NEXT    PIC X.                                       
003900*                                 MARKNADSBOLAGSKOD                       
004000     03 MOD-W30314           OCCURS 13 TIMES.                             
004100        05 MOD-IDARTNR       PIC Z(8)9.                                   
004200*                                 ARTIKELNUMMER                           
004300        05 MOD-IDFKNGRP      PIC Z(3)9.                                   
004400*                                 FUNKTIONSGRUPP                          
004500        05 MOD-KDPRODSL      PIC Z9.                                      
004600*                                 PRODUKTSLAG                             
004700        05 MOD-NORMAL-RAB    PIC 9(2).                                    
004800*                                 RABATTKOD (ARTIKELPRIS)                 
004900        05 MOD-KAMPANJ-RAB   PIC Z(4)9.                                   
005000*                                 TRANSFER KOD                            
005100        05 MOD-RETAIL-PRIS   PIC Z(6)9.9(2).                              
005200*                                 BRUTTOPRIS PER MARKNAD (FOB)            
005300        05 MOD-KDVALISO      PIC X(3).                                    
005400*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
005500        05 MOD-BENAEMNING    PIC X(20).                                   
005600*                                 BENÄMNING            BEART-003          
005700     03 MOD-TEMFSINF         PIC X(55).                                   
005800*                                 INFORMATIONSMEDDELANDE                  
005900*** END OF VILMAII-COPY LENGTH= 876 BYTES                                 
