000100 01  4426-WDGX4426-CTX.                                                   
000200*                                 STATISTISKA NUMMER NL-TULL              
000300*                                 STATNR - TARIC CODES                    
000400*                                 FYSISK NYCKEL: IDSTATNR                 
000500*                                                                         
000600     03 4426-IDSTATNR        PIC S9(9)           COMP-3.                  
000700*                                 STATISTISKT NUMMER                      
000800*                                 1 = NORSKT                              
000900*                                 2 = ENGELSKT                            
001000*                                 3 = BELGISKT                            
001100*                                 4 = PERUANSKT                           
001200*                                 5 = SVENSKT                             
001300*                                 6 =                                     
001400*                                 STATISTICAL NO.                         
001500     03 4426-IDTARIC         PIC 9(2).                                    
001600*                                 TARIC ID NL-TULL                        
001700*                                 TARIC ID NL-CUSTOM                      
001800     03 4426-IDTARIC1        PIC 9(4).                                    
001900*                                 EXTRA-1 TARIC ID NL-TULL                
002000*                                 ADD-1 TARIC ID NL-CUSTOM                
002100     03 4426-IDTARIC2        PIC 9(4).                                    
002200*                                 EXTRA-2 TARIC ID NL-TULL                
002300*                                 ADD-2 TARIC ID NL-CUSTOM                
002400*** END OF VILMAII-COPY LENGTH= 15 BYTES                                  
