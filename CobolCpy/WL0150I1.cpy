000100 01  REQU-WL0150I1.                                                       
000200*                                 REQUEST TO PGM WL0150                   
000300     03 REQU-IDDC-KEY        PIC X(2).                                    
000400*                                 IDENTIFIERARE LAGER                     
000500     03 REQU-IDRT-KEY        PIC X(3).                                    
000600*                                 RETURTERMINAL                           
000700     03 REQU-IDRTLOP-KEY     PIC 9(3).                                    
000800*                                 RETUR TERMINAL LÖPNUMMER                
000900     03 REQU-IDKOLLI-KEY     PIC 9(5).                                    
001000*                                 KOLLINUMMER                             
001100     03 REQU-IDDISTR-KEY     PIC 9(4).                                    
001200*                                 DISTRIKTNUMMER                          
001300     03 REQU-IDKUNDNR-KEY    PIC 9(6).                                    
001400*                                 KUNDNUMMER                              
001500     03 REQU-IDRAPPNR-KEY    PIC 9(7).                                    
001600*                                 RAPPORT NUMMER                          
001700     03 REQU-KVRADER-MAX1    PIC 9(5).                                    
001800*                                 MAX INDEX KOPPLAT TILL OCCURS N         
001900*                                 EDAN.                                   
002000     03 REQU-RAD             OCCURS 1 TO 500 TIMES                        
002100                             DEPENDING ON REQU-KVRADER-MAX1.              
002200*                                 RADFÄLT                                 
002300        05 REQU-KDCMD        PIC X(4).                                    
002400        05 REQU-IDKOLLI      PIC 9(5).                                    
002500*                                 KOLLINUMMER                             
002600        05 REQU-IDDISTR      PIC 9(4).                                    
002700*                                 DISTRIKTNUMMER                          
002800        05 REQU-IDKUNDNR     PIC 9(6).                                    
002900*                                 KUNDNUMMER                              
003000        05 REQU-IDRAPPNR     PIC 9(7).                                    
003100*                                 RAPPORT NUMMER                          
003200        05 REQU-TERETNOT     PIC X(20).                                   
003300*                                 FRI NOTERING RETURER                    
003400*** END OF VILMAII-COPY LENGTH= 23035 BYTES                               
