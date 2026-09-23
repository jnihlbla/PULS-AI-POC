000100 01  MOD-0832-W0O83201-CTX.                                               
000200*                                 MOD-COPYTEXT                            
000300*                                 FÖR W0O832                              
000400     03 MOD-0832-IDTRANS     PIC X(4).                                    
000500*                                 BILDNUMMER                              
000600     03 MOD-0832-TEMFSFEL    PIC X(40).                                   
000700*                                 MFS FELMEDDELANDE                       
000800     03 MOD-0832-W0O83201-001-GRP                                         
000900                             OCCURS 4 TIMES.                              
001000        05 MOD-0832-KDSVAR-ATTR                                           
001100                             PIC X(2).                                    
001200*                                 MFS ATTRIBUTFÄLT                        
001300        05 MOD-0832-KDSVAR   PIC X(2).                                    
001400*                                 MFS BEHANDLING AV INPUTFÄLT             
001500     03 MOD-0832-TEMFSINF    PIC X(55).                                   
001600*                                 INFORMATIONSMEDDELANDE                  
001700*** END OF VILMAII-COPY LENGTH= 115 BYTES                                 
