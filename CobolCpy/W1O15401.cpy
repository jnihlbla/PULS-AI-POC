000100 01  MOD-W1O15401.                                                        
000200*                                 MOD-COPYTEXT FÖR W1015400               
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-KDPRODSL-IN      PIC X(2).                                    
000800*                                 PRODUKTSLAG                             
000900     03 MOD-KDPRODSL-UT      PIC X(2).                                    
001000*                                 PRODUKTSLAG                             
001100     03 MOD-IDFKNGRP-FOM-LO  PIC X(4).                                    
001200*                                 FUNKTIONSGRUPP                          
001300     03 MOD-IDFKNGRP-FOM-HI  PIC X(4).                                    
001400*                                 FUNKTIONSGRUPP                          
001500     03 MOD-INFO-COL         OCCURS 3 TIMES                               
001600                             INDEXED MOD-COL-INDX.                        
001700*                                 COLINFORMATION                          
001800        05 MOD-INFO-RAD      OCCURS 11 TIMES                              
001900                             INDEXED MOD-RAD-INDX.                        
002000*                                 RADINFORMATION                          
002100           07 MOD-RAD-ATTR   PIC X(2).                                    
002200*                                 MFS ATTRIBUTFÄLT                        
002300           07 MOD-IDFKNGRP-FOM                                            
002400                             PIC Z(3)9.                                   
002500*                                 FUNKTIONSGRUPP                          
002600           07 MOD-FILLER     PIC X(2).                                    
002700           07 MOD-IDFKNGRP-TOM                                            
002800                             PIC Z(3)9.                                   
002900*                                 FUNKTIONSGRUPP                          
003000           07 MOD-FILLER     PIC X(7).                                    
003100           07 MOD-IDBERED    PIC Z9.                                      
003200*                                 BEREDARENUMMER                          
003300     03 MOD-IDFKNGRP-FOM-IN-ATTR                                          
003400                             PIC X(2).                                    
003500*                                 MFS ATTRIBUTFÄLT                        
003600     03 MOD-IDFKNGRP-FOM-IN  PIC X(2).                                    
003700*                                 MFS BEHANDLING AV INPUTFÄLT             
003800     03 MOD-IDFKNGRP-TOM-IN-ATTR                                          
003900                             PIC X(2).                                    
004000*                                 MFS ATTRIBUTFÄLT                        
004100     03 MOD-IDFKNGRP-TOM-IN  PIC X(2).                                    
004200*                                 MFS BEHANDLING AV INPUTFÄLT             
004300     03 MOD-IDBERED-IN-ATTR  PIC X(2).                                    
004400*                                 MFS ATTRIBUTFÄLT                        
004500     03 MOD-IDBERED-IN       PIC X(2).                                    
004600*                                 MFS BEHANDLING AV INPUTFÄLT             
004700     03 MOD-KDSVAR-IN-ATTR   PIC X(2).                                    
004800*                                 MFS ATTRIBUTFÄLT                        
004900     03 MOD-KDSVAR-IN        PIC X(2).                                    
005000*                                 MFS BEHANDLING AV INPUTFÄLT             
005100     03 MOD-TEMFSINF         PIC X(61).                                   
005200*                                 INFORMATIONSMEDDELANDE                  
005300*** END COPY W1O15401C0  LENGTH=826                                       
