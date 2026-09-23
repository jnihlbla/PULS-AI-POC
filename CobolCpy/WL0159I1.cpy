000100 01  REQU-WL0159I1.                                                       
000200*                                 REQUEST TO PGM WL0159                   
000300     03 REQU-IDDC-KEY        PIC X(2).                                    
000400*                                 IDENTIFIERARE LAGER                     
000500     03 REQU-IDRT-KEY        PIC X(3).                                    
000600*                                 RETURTERMINAL                           
000700     03 REQU-IDDISTR-KEY     PIC 9(4).                                    
000800*                                 DISTRIKTNUMMER                          
000900     03 REQU-IDKUNDNR-KEY    PIC 9(6).                                    
001000*                                 KUNDNUMMER                              
001100     03 REQU-KDRETSTA-KEY    PIC X.                                       
001200*                                 STATUS RETURER                          
001300     03 REQU-VKORDBTO        PIC X(8).                                    
001400*                                 ORDERVIKT BRUTTO (KG)                   
001500     03 REQU-VLORDBTO        PIC X(8).                                    
001600*                                 ORDERVOLYM BRUTTO (M3)                  
001700     03 REQU-IDRTLOP         PIC 9(3).                                    
001800*                                 RETUR TERMINAL LÖPNUMMER                
001900     03 REQU-KVRADER         PIC 9(5).                                    
002000*                                 ANTAL RADER                             
002100     03 REQU-INPUT.                                                       
002200*                                 INMATNINGSFÄLT                          
002300        05 REQU-INPUTRAD     OCCURS 500 TIMES.                            
002400*                                 INMATNINGS-/NYCKELFÄLT PÅ RADEN         
002500           07 REQU-FLCMD     PIC X.                                       
002600           07 REQU-IDDISTR   PIC 9(4).                                    
002700*                                 DISTRIKTNUMMER                          
002800           07 REQU-IDKUNDNR  PIC 9(6).                                    
002900*                                 KUNDNUMMER                              
003000           07 REQU-IDRAPPNR  PIC 9(7).                                    
003100*                                 RAPPORT NUMMER                          
003200*** END OF VILMAII-COPY LENGTH= 9040 BYTES                                
