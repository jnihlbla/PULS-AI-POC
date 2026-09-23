000100 01  MOD-W2O31601.                                                        
000200*                                 MOD-COPYTEXT TILL W2031600              
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDKAMP-IN        PIC X(7).                                    
000800*                                 SERVICEKAMPANJ                          
000900     03 MOD-IDKAMP-UT        PIC X(7).                                    
001000*                                 SERVICEKAMPANJ                          
001100     03 MOD-IDKAMP-GRP-IN    PIC X(7).                                    
001200*                                 ID FÖR KAMPANJGRUPPER                   
001300     03 MOD-IDKAMP-GRP-UT    PIC X(7).                                    
001400*                                 ID FÖR KAMPANJGRUPPER                   
001500     03 MOD-RAD              OCCURS 13 TIMES.                             
001600        05 MOD-CMD-ATTR      PIC X(2).                                    
001700*                                 MFS ATTRIBUTFÄLT                        
001800        05 MOD-CMD           PIC X(2).                                    
001900*                                 MFS BEHANDLING AV INPUTFÄLT             
002000        05 MOD-IDKAMP        PIC X(7).                                    
002100*                                 SERVICEKAMPANJ                          
002200        05 MOD-KDKAMP        PIC X.                                       
002300*                                                                         
002400     03 MOD-IDKAMP-NY-ATTR   PIC X(2).                                    
002500*                                 MFS ATTRIBUTFÄLT                        
002600     03 MOD-IDKAMP-NY        PIC X(7).                                    
002700*                                 SERVICEKAMPANJ                          
002800     03 MOD-TEMFSINF         PIC X(55).                                   
002900*                                 INFORMATIONSMEDDELANDE                  
003000*** END OF VILMAII-COPY LENGTH= 292 BYTES                                 
