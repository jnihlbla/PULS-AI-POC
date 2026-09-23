000100 01  MID-W4I41801.                                                        
000200*                                                                         
000300     03 MID-IDDC-IN          PIC X(2).                                    
000400*                                 IDENTIFIERARE LAGER                     
000500     03 MID-IDDC-UT          PIC X(2).                                    
000600*                                 IDENTIFIERARE LAGER                     
000700     03 MID-KDFRAKT-IN       PIC X(2).                                    
000800*                                 FRAKTSÄTT DC TILL KUND                  
000900     03 MID-KDFRAKT-UT       PIC X(2).                                    
001000*                                 FRAKTSÄTT DC TILL KUND                  
001100     03 MID-IDDISTR-IN       PIC X(4).                                    
001200*                                 DISTRIKTNUMMER                          
001300     03 MID-IDDISTR-UT       PIC X(4).                                    
001400*                                 DISTRIKTNUMMER                          
001500     03 MID-IDKUNDNR-IN      PIC X(6).                                    
001600*                                 KUNDNUMMER                              
001700     03 MID-IDKUNDNR-UT      PIC X(6).                                    
001800*                                 KUNDNUMMER                              
001900     03 MID-INPUT.                                                        
002000*                                                                         
002100        05 MID-KDCMD-IN      PIC X.                                       
002200*                                 RAD-UPPDATERINGSKOMMANDO                
002300        05 MID-KUNDNR-IN     PIC X(7).                                    
002400*                                 KUNDNUMMER                              
002500        05 MID-BEGMRK-RAD1-IN                                             
002600                             PIC X(30).                                   
002700*                                 GODSMÄRKE  RAD1                         
002800        05 MID-BEGMRK-RAD2-IN                                             
002900                             PIC X(30).                                   
003000*                                 GODSMÄRKE  RAD2                         
003100*** END OF VILMAII-COPY LENGTH= 96 BYTES                                  
