000100 01  MOD-W9O22101.                                                        
000200*                                 MODCOPYTEXT TILL W9022100.              
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-IDMFSFEL         PIC X(3).                                    
000600*                                 MFS FELMEDDELANDE NUMMER                
000700     03 MOD-IDARTNR          PIC 9(9).                                    
000800*                                 ARTIKELNUMMER                           
000900     03 MOD-IDRADNR-NEXT     PIC 9(5).                                    
001000*                                 RADNUMMER                               
001100     03 MOD-BEART-SATS       PIC X(25).                                   
001200*                                 ARTIKELBENÄMNING                        
001300     03 MOD-KDSORT-SATS      PIC X(2).                                    
001400*                                 SORT-KOD                                
001500     03 MOD-RAD              OCCURS 13 TIMES.                             
001600        05 MOD-IDARTNR-RAD   PIC 9(9).                                    
001700*                                 ARTIKELNUMMER                           
001800        05 MOD-REANTPSA-RAD  PIC Z9.9(3).                                 
001900*                                 ANTAL PER SATS                          
002000        05 MOD-BEART-RAD     PIC X(25).                                   
002100*                                 ARTIKELBENÄMNING                        
002200*** END OF VILMAII-COPY LENGTH= 568 BYTES                                 
