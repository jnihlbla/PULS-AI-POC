000100 01  MID-W1I15501.                                                        
000200*                                 MID-COPYTEXT FÖR W1015500               
000300     03 MID-INPUT.                                                        
000400        05 MID-KDARBTYP-IN   PIC X(4).                                    
000500*                                 TYP AV ARBETE                           
000600        05 MID-IDPERSON-IN   PIC X(3).                                    
000700*                                 PERSONKOD                               
000800        05 MID-KDPRODSL-IN   PIC X(2).                                    
000900*                                 PRODUKTSLAG                             
001000        05 MID-KDSORT-INCL-IN                                             
001100                             OCCURS 5 TIMES                               
001200                             PIC X(2).                                    
001300*                                 SORT-KOD                                
001400        05 MID-KDSORT-EXCL-IN                                             
001500                             OCCURS 5 TIMES                               
001600                             PIC X(2).                                    
001700*                                 SORT-KOD                                
001800        05 MID-FLAGGA-GCP-IN PIC X.                                       
001900*                                 ALLMÄN FLAGGA                           
002000        05 MID-IDFKNGRP-FOM-IN                                            
002100                             PIC X(4).                                    
002200*                                 FUNKTIONSGRUPP                          
002300        05 MID-IDFKNGRP-TOM-IN                                            
002400                             PIC X(4).                                    
002500*                                 FUNKTIONSGRUPP                          
002600        05 MID-IDBERED-IN    PIC X(2).                                    
002700*                                 BEREDARENUMMER                          
002800*** END OF VILMAII-COPY LENGTH= 40 BYTES                                  
