000100 01  REQU-WL0157I1.                                                       
000200*                                 REQUEST TO PGM WL0157                   
000300     03 REQU-IDDC-KEY        PIC X(2).                                    
000400*                                 IDENTIFIERARE LAGER                     
000500     03 REQU-IDRT-KEY        PIC X(3).                                    
000600*                                 RETURTERMINAL                           
000700     03 REQU-IDKOLLI-KEY     PIC 9(5).                                    
000800*                                 KOLLINUMMER                             
000900     03 REQU-FLVISA-KEY      PIC X.                                       
001000*                                 ALLMÄN FLAGGA                           
001100     03 REQU-IDDC-RET        PIC X(2).                                    
001200*                                 IDENTIFIERARE LAGER                     
001300     03 REQU-KVRADER         PIC 9(5).                                    
001400*                                 ANTAL RADER                             
001500     03 REQU-INPUT.                                                       
001600*                                 INMATNINGSFÄLT                          
001700        05 REQU-INPUTLINE-1.                                              
001800*                                 INMATNINGSFÄLT                          
001900           07 REQU-KVKOLLI   PIC 9(4).                                    
002000*                                 ANTAL KOLLI                             
002100           07 REQU-IDDISTR-IN                                             
002200                             PIC 9(4).                                    
002300*                                 DISTRIKTNUMMER                          
002400           07 REQU-IDKUNDNR-IN                                            
002500                             PIC 9(6).                                    
002600*                                 KUNDNUMMER                              
002700           07 REQU-IDRAPPNR-IN                                            
002800                             PIC 9(7).                                    
002900*                                 RAPPORT NUMMER                          
003000           07 REQU-IDKOLLI-FOM                                            
003100                             PIC 9(5).                                    
003200*                                 KOLLINUMMER                             
003300           07 REQU-IDKOLLI-TOM                                            
003400                             PIC 9(5).                                    
003500*                                 KOLLINUMMER                             
003600        05 REQU-INPUTLINE-X  OCCURS 500 TIMES.                            
003700*                                 INMATNINGS-/NYCKELFÄLT PÅ RADEN         
003800           07 REQU-KDCMD     PIC X(4).                                    
003900           07 REQU-IDDISTR   PIC 9(4).                                    
004000*                                 DISTRIKTNUMMER                          
004100           07 REQU-IDKUNDNR  PIC 9(6).                                    
004200*                                 KUNDNUMMER                              
004300           07 REQU-IDRAPPNR  PIC 9(7).                                    
004400*                                 RAPPORT NUMMER                          
004500*** END OF VILMAII-COPY LENGTH= 10549 BYTES                               
