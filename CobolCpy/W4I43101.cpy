000100 01  MID-W4I43101.                                                        
000200*                                 MID-COPYTEXT FÖR W40431                 
000300     03 MID-IDDIRLEV-IN      PIC X(5).                                    
000400*                                 LEVERANTÖRNUMMER                        
000500     03 MID-IDDIRLEV-UT      PIC X(5).                                    
000600*                                 LEVERANTÖRNUMMER                        
000700     03 MID-IDDISTR-IN       PIC 9(4).                                    
000800*                                 DISTRIKTNUMMER                          
000900     03 MID-IDDISTR-UT       PIC X(4).                                    
001000*                                 DISTRIKTNUMMER                          
001100     03 MID-KDORDKL-IN       PIC 9.                                       
001200*                                 ORDERKLASS                              
001300     03 MID-KDORDKL-UT       PIC X.                                       
001400*                                 ORDERKLASS                              
001500     03 MID-CMD              PIC X.                                       
001600     03 MID-KDORDKL-E        PIC 9.                                       
001700*                                 ORDERKLASS                              
001800     03 MID-IDDISTR-E        PIC 9(4).                                    
001900*                                 DISTRIKTNUMMER                          
002000     03 MID-IDKUNDNR-E       PIC 9(6).                                    
002100*                                 KUNDNUMMER                              
002200     03 MID-IDDC-E           PIC X(2).                                    
002300*                                 IDENTIFIERARE LAGER                     
002400     03 MID-KDVIA-E          PIC X(2).                                    
002500*                                 KOD FöR LEVERANS VIA                    
002600     03 MID-TIMINUT-CUT-E    PIC X(5).                                    
002700     03 MID-KVDAGAR-LEV-E    PIC 9(3).                                    
002800*                                 WORKDAYS TO DELIVERY                    
002900     03 MID-KVDAGAR-TPO-E    PIC 9(3).                                    
003000*                                 ANTAL DAGAR TILL START AV TPO           
003100     03 MID-TECKEN           PIC X.                                       
003200     03 MID-KVDAGAR-DIFF-E   PIC 9(3).                                    
003300*                                 TRANSPORT DAY DIFF. (+/- CONTRA         
003400*                                  CDC)                                   
003500*** END OF VILMAII-COPY LENGTH= 51 BYTES                                  
