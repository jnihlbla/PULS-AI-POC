000100 01  MOD-W9O22201.                                                        
000200*                                 MODCOPYTEXT TILL W9022200.              
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-IDMFSFEL         PIC X(3).                                    
000600*                                 MFS FELMEDDELANDE NUMMER                
000700     03 MOD-IDARTNR          PIC 9(9).                                    
000800*                                 ARTIKELNUMMER                           
000900     03 MOD-IDARTNR-NEXT     PIC 9(9).                                    
001000*                                 ARTIKELNUMMER                           
001100     03 MOD-IDRADNR-NEXT     PIC 9(5).                                    
001200*                                 RADNUMMER                               
001300     03 MOD-KDSTRRAD-NEXT    PIC X.                                       
001400*                                 TYP AV STRUKTURRAD                      
001500     03 MOD-BEART-ING        PIC X(25).                                   
001600*                                 ARTIKELBENÄMNING                        
001700     03 MOD-RAD              OCCURS 13 TIMES.                             
001800        05 MOD-IDARTNR-RAD   PIC 9(9).                                    
001900*                                 ARTIKELNUMMER                           
002000        05 MOD-REANTPSA-RAD  PIC Z9.9(3).                                 
002100*                                 ANTAL PER SATS                          
002200        05 MOD-BEART-RAD     PIC X(25).                                   
002300*                                 ARTIKELBENÄMNING                        
002400        05 MOD-KDSORT-RAD    PIC X(2).                                    
002500*                                 SORT-KOD                                
002600*** END OF VILMAII-COPY LENGTH= 602 BYTES                                 
