000100 01  MOD-W1O15501.                                                        
000200*                                 MOD-COPYTEXT FÖR W1015500               
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-KDARBTYP-IN-ATTR PIC X(2).                                    
000800*                                 MFS ATTRIBUTFÄLT                        
000900     03 MOD-KDARBTYP-IN      PIC X(4).                                    
001000*                                 TYP AV ARBETE                           
001100     03 MOD-IDPERSON-IN-ATTR PIC X(2).                                    
001200*                                 MFS ATTRIBUTFÄLT                        
001300     03 MOD-IDPERSON-IN      PIC Z(2)9.                                   
001400*                                 PERSONKOD                               
001500     03 MOD-KDPRODSL-IN-ATTR PIC X(2).                                    
001600*                                 MFS ATTRIBUTFÄLT                        
001700     03 MOD-KDPRODSL-IN      PIC Z9.                                      
001800*                                 PRODUKTSLAG                             
001900     03 MOD-SORT-INCL        OCCURS 5 TIMES.                              
002000        05 MOD-KDSORT-INCL-IN-ATTR                                        
002100                             PIC X(2).                                    
002200*                                 MFS ATTRIBUTFÄLT                        
002300        05 MOD-KDSORT-INCL-IN                                             
002400                             PIC X(2).                                    
002500*                                 SORT-KOD                                
002600     03 MOD-SORT-EXCL        OCCURS 5 TIMES.                              
002700        05 MOD-KDSORT-EXCL-IN-ATTR                                        
002800                             PIC X(2).                                    
002900*                                 MFS ATTRIBUTFÄLT                        
003000        05 MOD-KDSORT-EXCL-IN                                             
003100                             PIC X(2).                                    
003200*                                 SORT-KOD                                
003300     03 MOD-FLAGGA-GCP-IN-ATTR                                            
003400                             PIC X(2).                                    
003500*                                 MFS ATTRIBUTFÄLT                        
003600     03 MOD-FLAGGA-GCP-IN    PIC X.                                       
003700*                                 ALLMÄN FLAGGA                           
003800     03 MOD-IDFKNGRP-FOM-IN-ATTR                                          
003900                             PIC X(2).                                    
004000*                                 MFS ATTRIBUTFÄLT                        
004100     03 MOD-IDFKNGRP-FOM-IN  PIC Z(3)9.                                   
004200*                                 FUNKTIONSGRUPP                          
004300     03 MOD-IDFKNGRP-TOM-IN-ATTR                                          
004400                             PIC X(2).                                    
004500*                                 MFS ATTRIBUTFÄLT                        
004600     03 MOD-IDFKNGRP-TOM-IN  PIC Z(3)9.                                   
004700*                                 FUNKTIONSGRUPP                          
004800     03 MOD-IDBERED-IN-ATTR  PIC X(2).                                    
004900*                                 MFS ATTRIBUTFÄLT                        
005000     03 MOD-IDBERED-IN       PIC Z9.                                      
005100*                                 BEREDARENUMMER                          
005200     03 MOD-TEMFSINF         PIC X(55).                                   
005300*                                 INFORMATIONSMEDDELANDE                  
005400*** END OF VILMAII-COPY LENGTH= 173 BYTES                                 
