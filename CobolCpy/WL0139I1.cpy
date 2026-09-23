000100 01  REQU-WL0139I1.                                                       
000200*                                 REQUEST FROM PGM WL0139                 
000300     03 REQU-IDDC-KEY3       PIC X(2).                                    
000400*                                 IDENTIFIERARE LAGER                     
000500     03 REQU-IDFTG-KEY       PIC 9(2).                                    
000600*                                 FÖRETAGSID EKONOM REDOVISNING           
000700     03 REQU-IDARTNR-KEY3    PIC 9(8).                                    
000800*                                 ARTIKELNUMMER                           
000900     03 REQU-IDDISTR-KEY3    PIC 9(4).                                    
001000*                                 DISTRIKTNUMMER                          
001100     03 REQU-IDKUNDNR-KEY3   PIC 9(6).                                    
001200*                                 KUNDNUMMER                              
001300     03 REQU-KDANMORS-KEY3   PIC 9(2).                                    
001400*                                 ORSAK TILL LEVERAN KDANMORS-002         
001500     03 REQU-FL-IDDC         PIC X.                                       
001600*                                 ALLMÄN FLAGGA                           
001700     03 REQU-KVRADER         PIC 9(5).                                    
001800*                                 ANTAL RADER                             
001900     03 REQU-INPUTTABELRAD   OCCURS 500 TIMES.                            
002000        05 REQU-FLCMD        PIC X.                                       
002100*                                 ALLMÄN FLAGGA                           
002200        05 REQU-IDDISTR      PIC 9(4).                                    
002300*                                 DISTRIKTNUMMER                          
002400        05 REQU-IDKUNDNR     PIC 9(6).                                    
002500*                                 KUNDNUMMER                              
002600        05 REQU-IDRAPPNR     PIC 9(7).                                    
002700*                                 RAPPORT NUMMER                          
002800        05 REQU-IDRADNR      PIC 9(4).                                    
002900*                                 RADNUMMER                               
003000*** END OF VILMAII-COPY LENGTH= 11030 BYTES                               
