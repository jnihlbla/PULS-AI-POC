000100 01  MOD-W2O14501.                                                        
000200*                                 MODCOPYTEXT TILL W20145.                
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDLANDX2-IN      PIC X(2).                                    
000800*                                 2-STÄLLIG LANDSBETECKNINGSKOD           
000900     03 MOD-IDLANDX2-UT      PIC X(2).                                    
001000*                                 2-STÄLLIG LANDSBETECKNINGSKOD           
001100     03 MOD-OUTPUT.                                                       
001200        05 MOD-RAD           OCCURS 20 TIMES.                             
001300           07 MOD-CMD-ATTR   PIC X(2).                                    
001400*                                 MFS ATTRIBUTFÄLT                        
001500           07 MOD-CMD        PIC X(2).                                    
001600*                                 MFS BEHANDLING AV INPUTFÄLT             
001700           07 MOD-DADATUM-HELG-ATTR                                       
001800                             PIC X(2).                                    
001900*                                 MFS ATTRIBUTFÄLT                        
002000           07 MOD-DADATUM-HELG                                            
002100                             PIC Z(8).                                    
002200*                                 HELGDAGAR (ÅÅÅÅMMDD)                    
002300        05 MOD-DADATUM-HELG-NY-ATTR                                       
002400                             PIC X(2).                                    
002500*                                 MFS ATTRIBUTFÄLT                        
002600        05 MOD-DADATUM-HELG-NY                                            
002700                             PIC Z(8).                                    
002800*                                 HELGDAGAR (ÅÅÅÅMMDD)                    
002900     03 MOD-TEMFSINF         PIC X(55).                                   
003000*                                 INFORMATIONSMEDDELANDE                  
003100*** END OF VILMAII-COPY LENGTH= 393 BYTES                                 
