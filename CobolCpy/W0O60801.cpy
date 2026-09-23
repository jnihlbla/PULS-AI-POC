000100 01  MOD-W0O60801.                                                        
000200*                                 MOD-COPYTEXT FÖR W0060800               
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-W0O60801-001     OCCURS 2 TIMES.                              
000800*                                                                         
000900        05 MOD-KDCMD-ATTR    PIC X(2).                                    
001000*                                 MFS ATTRIBUTFÄLT                        
001100        05 MOD-KDCMD         PIC X.                                       
001200*                                 RAD-UPPDATERINGSKOMMANDO                
001300     03 MOD-TEMFSINF         PIC X(61).                                   
001400*                                 INFORMATIONSMEDDELANDE                  
001500*** END COPY W0O60801C0  LENGTH=111                                       
