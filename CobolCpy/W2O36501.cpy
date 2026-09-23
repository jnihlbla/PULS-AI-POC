000100 01  MOD-W2O36501.                                                        
000200*                                 MOD-COPYTEXT FÖR W2036500               
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDDC-IN          PIC X(2).                                    
000800*                                 IDENTIFIERARE LAGER                     
000900     03 MOD-IDDC-UT          PIC X(2).                                    
001000*                                 IDENTIFIERARE LAGER                     
001100     03 MOD-TABELLRAD        OCCURS 11 TIMES.                             
001200        05 MOD-CMD-ATTR      PIC X(2).                                    
001300*                                 MFS ATTRIBUTFÄLT                        
001400        05 MOD-CMD           PIC X(2).                                    
001500*                                 MFS BEHANDLING AV INPUTFÄLT             
001600        05 MOD-IDDC          PIC X(2).                                    
001700*                                 IDENTIFIERARE LAGER                     
001800     03 MOD-NY-IDDC-ATTR     PIC X(2).                                    
001900*                                 MFS ATTRIBUTFÄLT                        
002000     03 MOD-NY-IDDC          PIC X(2).                                    
002100*                                 IDENTIFIERARE LAGER                     
002200     03 MOD-TEMFSINF         PIC X(55).                                   
002300*                                 INFORMATIONSMEDDELANDE                  
002400*** END OF VILMAII-COPY LENGTH= 173 BYTES                                 
