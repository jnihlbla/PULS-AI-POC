000100 01  MOD-W4O30301.                                                        
000200*                                 MOD FÖR BILD PACKNINGSRAPP.             
000300*                                 DIV UDDA ORDER GRUND                    
000400     03 MOD-IDTRANS          PIC X(4).                                    
000500*                                 BILDNUMMER                              
000600     03 MOD-TEMFSFEL         PIC X(40).                                   
000700*                                 MFS FELMEDDELANDE                       
000800     03 MOD-IDDC-IN          PIC X(2).                                    
000900*                                 IDENTIFIERARE LAGER                     
001000     03 MOD-IDDC-UT          PIC X(2).                                    
001100*                                 IDENTIFIERARE LAGER                     
001200     03 MOD-RAD              OCCURS 13 TIMES.                             
001300        05 MOD-IDDISTR-ATTR  PIC X(2).                                    
001400*                                 MFS ATTRIBUTFÄLT                        
001500        05 MOD-IDDISTR       PIC X(2).                                    
001600*                                 MFS BEHANDLING AV INPUTFÄLT             
001700        05 MOD-IDPRODNR-ATTR PIC X(2).                                    
001800*                                 MFS ATTRIBUTFÄLT                        
001900        05 MOD-IDPRODNR      PIC X(2).                                    
002000*                                 MFS BEHANDLING AV INPUTFÄLT             
002100        05 MOD-FLAVVPACK-ATTR                                             
002200                             PIC X(2).                                    
002300*                                 MFS ATTRIBUTFÄLT                        
002400        05 MOD-FLAVVPACK     PIC X(2).                                    
002500*                                 MFS BEHANDLING AV INPUTFÄLT             
002600     03 MOD-TEMFSINF         PIC X(55).                                   
002700*                                 INFORMATIONSMEDDELANDE                  
002800*** END OF VILMAII-COPY LENGTH= 259 BYTES                                 
