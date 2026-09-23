000100 01  REQU-WL0151I1.                                                       
000200*                                 REQUEST TO PGM WL0151                   
000300     03 REQU-IDDC-KEY        PIC X(2).                                    
000400*                                 IDENTIFIERARE LAGER                     
000500     03 REQU-IDFTG-KEY       PIC 9(2).                                    
000600*                                 FÖRETAGSID EKONOM REDOVISNING           
000700     03 REQU-IDDISTR-KEY     PIC 9(4).                                    
000800*                                 DISTRIKTNUMMER                          
000900     03 REQU-IDKUNDNR-KEY    PIC 9(6).                                    
001000*                                 KUNDNUMMER                              
001100     03 REQU-KDLEVANM-KEY    PIC X.                                       
001200*                                 STATUS LEVERANSANMÄRKNING               
001300     03 REQU-FLSUM-KEY       PIC X.                                       
001400*                                 ALLMÄN FLAGGA                           
001500     03 REQU-KVRADER         PIC 9(5).                                    
001600*                                 ANTAL RADER                             
001700     03 REQU-RAD             OCCURS 2000 TIMES.                           
001800*                                 RAD                                     
001900        05 REQU-FLCMD        PIC X.                                       
002000        05 REQU-IDDISTR      PIC 9(4).                                    
002100*                                 DISTRIKTNUMMER                          
002200        05 REQU-IDKUNDNR     PIC 9(6).                                    
002300*                                 KUNDNUMMER                              
002400        05 REQU-IDRAPPNR     PIC 9(7).                                    
002500*                                 RAPPORT NUMMER                          
002600*** END OF VILMAII-COPY LENGTH= 36021 BYTES                               
