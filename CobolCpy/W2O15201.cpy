000100 01  MOD-W2O15201.                                                        
000200*                                 MOD-COPYTEXT FÖR W2015200               
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
001500     03 MOD-INFO-COL         OCCURS 2 TIMES                               
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
003000           07 MOD-FILLER     PIC X(5).                                    
003100           07 MOD-IDANSK     PIC Z(2)9.                                   
003200*                                 ANSKAFFARNUMMER                         
003300           07 MOD-FILLER     PIC X(2).                                    
003400           07 MOD-IDUSER     PIC X(8).                                    
003500*                                 ANVÄNDARIDENTITET I RACF                
003600     03 MOD-IDFKNGRP-FOM-IN-ATTR                                          
003700                             PIC X(2).                                    
003800*                                 MFS ATTRIBUTFÄLT                        
003900     03 MOD-IDFKNGRP-FOM-IN  PIC X(2).                                    
004000*                                 MFS BEHANDLING AV INPUTFÄLT             
004100     03 MOD-IDFKNGRP-TOM-IN-ATTR                                          
004200                             PIC X(2).                                    
004300*                                 MFS ATTRIBUTFÄLT                        
004400     03 MOD-IDFKNGRP-TOM-IN  PIC X(2).                                    
004500*                                 MFS BEHANDLING AV INPUTFÄLT             
004600     03 MOD-IDANSK-IN-ATTR   PIC X(2).                                    
004700*                                 MFS ATTRIBUTFÄLT                        
004800     03 MOD-IDANSK-IN        PIC X(2).                                    
004900*                                 MFS BEHANDLING AV INPUTFÄLT             
005000     03 MOD-IDUSER-IN-ATTR   PIC X(2).                                    
005100*                                 MFS ATTRIBUTFÄLT                        
005200     03 MOD-IDUSER-IN        PIC X(2).                                    
005300*                                 MFS BEHANDLING AV INPUTFÄLT             
005400     03 MOD-KDSVAR-IN-ATTR   PIC X(2).                                    
005500*                                 MFS ATTRIBUTFÄLT                        
005600     03 MOD-KDSVAR-IN        PIC X(2).                                    
005700*                                 MFS BEHANDLING AV INPUTFÄLT             
005800     03 MOD-TEMFSINF         PIC X(61).                                   
005900*                                 INFORMATIONSMEDDELANDE                  
006000*** END COPY W2O15201C0  LENGTH=797                                       
