000100 01  REQU-WL0149I1.                                                       
000200*                                 REQUEST TO PGM WL0149                   
000300     03 REQU-IDDC-KEY        PIC X(2).                                    
000400*                                 IDENTIFIERARE LAGER                     
000500     03 REQU-IDANSV-KEY      PIC X(6).                                    
000600     03 REQU-IDRT-KEY        PIC X(3).                                    
000700*                                 RETURTERMINAL                           
000800     03 REQU-IDRTLOP-KEY     PIC 9(3).                                    
000900*                                 RETUR TERMINAL LÖPNUMMER                
001000     03 REQU-IDKOLLI-KEY     PIC 9(5).                                    
001100*                                 KOLLINUMMER                             
001200     03 REQU-IDDISTR-KEY     PIC 9(4).                                    
001300*                                 DISTRIKTNUMMER                          
001400     03 REQU-FLVISAAV-KEY    PIC X.                                       
001500*                                 ALLMÄN FLAGGA                           
001600     03 REQU-INPUT.                                                       
001700*                                 INMATNINGSFÄLT                          
001800        05 REQU-IDANSTNR     PIC 9(5).                                    
001900*                                 ANSTÄLLNINGSNUMMER                      
002000     03 REQU-KVRADER         PIC 9(5).                                    
002100*                                 ANTAL RADER                             
002200     03 REQU-RAD             OCCURS 500 TIMES.                            
002300*                                 RAD                                     
002400        05 REQU-KDCMD        PIC X(4).                                    
002500        05 REQU-IDRT         PIC X(3).                                    
002600*                                 RETURTERMINAL                           
002700        05 REQU-IDRTLOP      PIC 9(3).                                    
002800*                                 RETUR TERMINAL LÖPNUMMER                
002900        05 REQU-IDKOLLI      PIC 9(5).                                    
003000*                                 KOLLINUMMER                             
003100*** END OF VILMAII-COPY LENGTH= 7534 BYTES                                
