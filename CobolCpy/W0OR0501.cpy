000100 01  MOD-R05-W0OR0501.                                                    
000200*                                 MOD-COPYTEXT                            
000300*                                 FÖR W0OR05                              
000400     03 MOD-R05-IDTRANS      PIC X(4).                                    
000500*                                 BILDNUMMER                              
000600     03 MOD-R05-TEMFSFEL     PIC X(40).                                   
000700*                                 MFS FELMEDDELANDE                       
000800     03 MOD-R05-IDARTNR-ATTR PIC X(2).                                    
000900*                                 MFS ATTRIBUTFÄLT                        
001000     03 MOD-R05-IDARTNR      PIC X(2).                                    
001100*                                 MFS BEHANDLING AV INPUTFÄLT             
001200     03 MOD-R05-IDFTG-ATTR   PIC X(2).                                    
001300*                                 MFS ATTRIBUTFÄLT                        
001400     03 MOD-R05-IDFTG        PIC X(2).                                    
001500*                                 MFS BEHANDLING AV INPUTFÄLT             
001600     03 MOD-R05-IDLKTO-OLD-ATTR                                           
001700                             PIC X(2).                                    
001800*                                 MFS ATTRIBUTFÄLT                        
001900     03 MOD-R05-IDLKTO-OLD   PIC X(2).                                    
002000*                                 MFS BEHANDLING AV INPUTFÄLT             
002100     03 MOD-R05-IDLKTO-NEW-ATTR                                           
002200                             PIC X(2).                                    
002300*                                 MFS ATTRIBUTFÄLT                        
002400     03 MOD-R05-IDLKTO-NEW   PIC X(2).                                    
002500*                                 MFS BEHANDLING AV INPUTFÄLT             
002600     03 MOD-R05-TEMFSINF     PIC X(61).                                   
002700*                                 INFORMATIONSMEDDELANDE                  
002800*** END COPY W0OR0501    LENGTH=121                                       
