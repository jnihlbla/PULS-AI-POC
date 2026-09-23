000100 01  MOD-W4O71801.                                                        
000200*                                 MODCOPYTEXT TILL W40718.                
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDDISTR-IN       PIC X(2).                                    
000800*                                 MFS BEHANDLING AV INPUTFÄLT             
000900     03 MOD-IDDISTR-UT       PIC X(4).                                    
001000*                                 DISTRIKTNUMMER                          
001100     03 MOD-IDKUNDNR-IN      PIC X(2).                                    
001200*                                 MFS BEHANDLING AV INPUTFÄLT             
001300     03 MOD-IDKUNDNR-UT      PIC X(6).                                    
001400*                                 KUNDNUMMER                              
001500     03 MOD-IDRAPPNR-IN      PIC X(2).                                    
001600*                                 MFS BEHANDLING AV INPUTFÄLT             
001700     03 MOD-IDRAPPNR-UT      PIC X(7).                                    
001800*                                 RAPPORT NUMMER                          
001900     03 MOD-IDDC-IN          PIC X(2).                                    
002000*                                 MFS BEHANDLING AV INPUTFÄLT             
002100     03 MOD-IDDC-UT          PIC X(2).                                    
002200*                                 IDENTIFIERARE LAGER                     
002300     03 MOD-KDKRENOT-IN      PIC X(2).                                    
002400*                                 MFS BEHANDLING AV INPUTFÄLT             
002500     03 MOD-KDKRENOT-UT      PIC X(2).                                    
002600*                                 TYP AV KREDITERING                      
002700     03 MOD-INPUT            OCCURS 5 TIMES.                              
002800*                                 LINE INFORMATION                        
002900        05 MOD-TEMEMO-ATTR   PIC X(2).                                    
003000*                                 MFS ATTRIBUTFÄLT                        
003100        05 MOD-TEMEMO        PIC X(66).                                   
003200*                                 TEXTRAD MAIL                            
003300     03 MOD-TEMFSINF         PIC X(55).                                   
003400*                                 INFORMATIONSMEDDELANDE                  
003500*** END OF VILMAII-COPY LENGTH= 470 BYTES                                 
