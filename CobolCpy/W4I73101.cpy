000100 01  MID-W4I73101.                                                        
000200*                                 MID-COPYTEXT FÖR W40731                 
000300     03 MID-KDRETSTA-IN      PIC X.                                       
000400*                                 STATUS RETURER                          
000500     03 MID-KDRETSTA-UT      PIC X.                                       
000600*                                 STATUS RETURER                          
000700     03 MID-IDRT-IN          PIC X(3).                                    
000800*                                 RETURTERMINAL                           
000900     03 MID-IDRTLOP-IN       PIC X(3).                                    
001000*                                 RETUR TERMINAL LÖPNUMMER                
001100     03 MID-IDRT-UT          PIC X(3).                                    
001200*                                 RETURTERMINAL                           
001300     03 MID-IDRTLOP-UT       PIC X(3).                                    
001400*                                 RETUR TERMINAL LÖPNUMMER                
001500     03 MID-INPUT.                                                        
001600*                                 INMATNINGSFÄLT                          
001700        05 MID-IDANSTNR      PIC 9(5).                                    
001800*                                 ANSTÄLLNINGSNUMMER                      
001900        05 MID-ADINLOMR      PIC X(4).                                    
002000*                                 INLEVERANSOMRÅDE                        
002100        05 MID-KDCMD         OCCURS 11 TIMES                              
002200                             PIC X(4).                                    
002300     03 MID-KEYFIELD         OCCURS 11 TIMES.                             
002400*                                 NYCKELFÄLT PÅ RADEN                     
002500        05 MID-IDRT          PIC X(3).                                    
002600*                                 RETURTERMINAL                           
002700        05 MID-IDRTLOP       PIC X(3).                                    
002800*                                 RETUR TERMINAL LÖPNUMMER                
002900*** END OF VILMAII-COPY LENGTH= 133 BYTES                                 
