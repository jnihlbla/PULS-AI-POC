000100 01  MOD-W3O21201.                                                        
000200*                                 MOD-COPYTEXT                            
000300*                                 FOR W3F212                              
000400     03 MOD-IDTRANS          PIC X(4).                                    
000500*                                 BILDNUMMER                              
000600     03 MOD-TEMFSFEL         PIC X(40).                                   
000700*                                 MFS FELMEDDELANDE                       
000800     03 MOD-KDMARK-BUDG-ENTER                                             
000900                             PIC X(3).                                    
001000*                                 MARKNADSKOD BUDGET 96 MARKNADER         
001100     03 MOD-KDPRODSL-ENTER   PIC X(2).                                    
001200*                                 PRODUKTSLAG                             
001300     03 MOD-IDFKNGRP-ENTER   PIC X(4).                                    
001400*                                 FUNKTIONSGRUPP                          
001500     03 MOD-KDMARK-BUDG-PFK8 PIC X(3).                                    
001600*                                 MARKNADSKOD BUDGET 96 MARKNADER         
001700     03 MOD-KDPRODSL-PFK8    PIC X(2).                                    
001800*                                 PRODUKTSLAG                             
001900     03 MOD-IDFKNGRP-PFK8    PIC X(4).                                    
002000*                                 FUNKTIONSGRUPP                          
002100     03 MOD-UTRAD            OCCURS 20 TIMES.                             
002200        05 MOD-UTRAD-KDMARK-BUDG                                          
002300                             PIC X(3).                                    
002400*                                 MARKNADSKOD BUDGET 96 MARKNADER         
002500        05 MOD-UTRAD-KDPRODSL                                             
002600                             PIC Z9.                                      
002700*                                 PRODUKTSLAG                             
002800        05 MOD-UTRAD-IDFKNGRP                                             
002900                             PIC Z(3)9.                                   
003000*                                 FUNKTIONSGRUPP                          
003100        05 MOD-UTRAD-SUTOTFSG-BUDG                                        
003200                             PIC Z(10)9.9(2).                             
003300*                                 BUDGETERAT FÖRSÄLJNINGSVÄRDE            
003400        05 MOD-UTRAD-IDSKURVA                                             
003500                             PIC 9(2).                                    
003600*                                 SÄSONGSKURVA                            
003700     03 MOD-INRAD-KDMARK-BUDG-ATTR                                        
003800                             PIC X(2).                                    
003900*                                 MFS ATTRIBUTFÄLT                        
004000     03 MOD-INRAD-KDMARK-BUDG                                             
004100                             PIC X(2).                                    
004200*                                 MFS BEHANDLING AV INPUTFÄLT             
004300     03 MOD-INRAD-KDPRODSL-ATTR                                           
004400                             PIC X(2).                                    
004500*                                 MFS ATTRIBUTFÄLT                        
004600     03 MOD-INRAD-KDPRODSL   PIC X(2).                                    
004700*                                 MFS BEHANDLING AV INPUTFÄLT             
004800     03 MOD-INRAD-IDFKNGRP-ATTR                                           
004900                             PIC X(2).                                    
005000*                                 MFS ATTRIBUTFÄLT                        
005100     03 MOD-INRAD-IDFKNGRP   PIC X(2).                                    
005200*                                 MFS BEHANDLING AV INPUTFÄLT             
005300     03 MOD-INRAD-SUTOTFSG-BUDG-ATTR                                      
005400                             PIC X(2).                                    
005500*                                 MFS ATTRIBUTFÄLT                        
005600     03 MOD-INRAD-SUTOTFSG-BUDG                                           
005700                             PIC X(2).                                    
005800*                                 MFS BEHANDLING AV INPUTFÄLT             
005900     03 MOD-INRAD-SUTOTFSG-NOLLFAELT                                      
006000                             PIC X(6).                                    
006100     03 MOD-INRAD-IDSKURVA-ATTR                                           
006200                             PIC X(2).                                    
006300*                                 MFS ATTRIBUTFÄLT                        
006400     03 MOD-INRAD-IDSKURVA   PIC X(2).                                    
006500*                                 MFS BEHANDLING AV INPUTFÄLT             
006600     03 MOD-INRAD-KDCMD-ATTR PIC X(2).                                    
006700*                                 MFS ATTRIBUTFÄLT                        
006800     03 MOD-INRAD-KDCMD      PIC X(2).                                    
006900*                                 MFS BEHANDLING AV INPUTFÄLT             
007000     03 MOD-TEMFSINF         PIC X(61).                                   
007100*                                 INFORMATIONSMEDDELANDE                  
007200*** END COPY W3O21201C0  LENGTH=653                                       
