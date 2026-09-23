000100 01  REQU-WL0146I1.                                                       
000200*                                 REQUEST TO PGM WL0146                   
000300     03 REQU-IDDC-KEY        PIC X(2).                                    
000400*                                 IDENTIFIERARE LAGER                     
000500     03 REQU-KDRETSTA-KEY    PIC X.                                       
000600*                                 STATUS RETURER                          
000700     03 REQU-IDRT-KEY        PIC X(3).                                    
000800*                                 RETURTERMINAL                           
000900     03 REQU-IDRTLOP-KEY     PIC 9(3).                                    
001000*                                 RETUR TERMINAL LÖPNUMMER                
001100     03 REQU-INPUT.                                                       
001200*                                 INMATNINGSFÄLT                          
001300        05 REQU-IDANSTNR     PIC 9(5).                                    
001400*                                 ANSTÄLLNINGSNUMMER                      
001500        05 REQU-ADINLOMR     PIC X(4).                                    
001600*                                 INLEVERANSOMRÅDE                        
001700     03 REQU-KVRADER         PIC 9(5).                                    
001800*                                 ANTAL RADER                             
001900     03 REQU-RAD             OCCURS 500 TIMES.                            
002000*                                 RAD                                     
002100        05 REQU-KDCMD        PIC X(4).                                    
002200        05 REQU-IDRT         PIC X(3).                                    
002300*                                 RETURTERMINAL                           
002400        05 REQU-IDRTLOP      PIC 9(3).                                    
002500*                                 RETUR TERMINAL LÖPNUMMER                
002600*** END OF VILMAII-COPY LENGTH= 5023 BYTES                                
