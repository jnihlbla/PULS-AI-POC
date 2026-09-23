000100 01  MOD-W4O72601.                                                        
000200*                                 MODCOPYTEXT TILL W40726.                
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDKNOTNR-IN      PIC X(7).                                    
000800*                                 KREDITNOTANUMMER                        
000900     03 MOD-IDKNOTNR-UT      PIC X(7).                                    
001000*                                 KREDITNOTANUMMER                        
001100     03 MOD-RADER            OCCURS 4 TIMES.                              
001200*                                 RADINFORMATION                          
001300        05 MOD-KDCMD-ATTR    PIC X(2).                                    
001400*                                 MFS ATTRIBUTFÄLT                        
001500        05 MOD-KDCMD         PIC X(4).                                    
001600        05 MOD-IDDISTR       PIC Z(3)9.                                   
001700*                                 DISTRIKTNUMMER                          
001800        05 MOD-IDKUNDNR      PIC Z(5)9.                                   
001900*                                 KUNDNUMMER                              
002000        05 MOD-IDRAPPNR      PIC Z(6)9.                                   
002100*                                 RAPPORT NUMMER                          
002200        05 MOD-TILEVANM      PIC 9(6).                                    
002300*                                 DATUM LEVERANSANMÄRKNING                
002400     03 MOD-TEMFSINF         PIC X(55).                                   
002500*                                 INFORMATIONSMEDDELANDE                  
002600*** END OF VILMAII-COPY LENGTH= 229 BYTES                                 
