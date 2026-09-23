000100 01  MOD-R02-W0OR0201.                                                    
000200*                                 MOD-COPYTEXT                            
000300*                                 FÖR W0OR02                              
000400     03 MOD-R02-IDTRANS      PIC X(4).                                    
000500*                                 BILDNUMMER                              
000600     03 MOD-R02-TEMFSFEL     PIC X(40).                                   
000700*                                 MFS FELMEDDELANDE                       
000800     03 MOD-R02-IDLEVNR-ATTR PIC X(2).                                    
000900*                                 MFS ATTRIBUTFÄLT                        
001000     03 MOD-R02-IDLEVNR      PIC X(2).                                    
001100*                                 MFS BEHANDLING AV INPUTFÄLT             
001200     03 MOD-R02-PG-TABELL    OCCURS 8 TIMES.                              
001300        05 MOD-R02-IDANSK-ATTR                                            
001400                             PIC X(2).                                    
001500*                                 MFS ATTRIBUTFÄLT                        
001600        05 MOD-R02-IDANSK    PIC X(2).                                    
001700*                                 MFS BEHANDLING AV INPUTFÄLT             
001800     03 MOD-R02-TEMFSINF     PIC X(61).                                   
001900*                                 INFORMATIONSMEDDELANDE                  
002000*** END COPY W0OR0201    LENGTH=141                                       
