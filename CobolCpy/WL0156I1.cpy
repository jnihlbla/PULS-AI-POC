000100 01  REQU-WL0156I1.                                                       
000200*                                 REQUEST TO PGM WL0156                   
000300     03 REQU-IDDC-KEY        PIC X(2).                                    
000400*                                 IDENTIFIERARE LAGER                     
000500     03 REQU-IDFTG-KEY       PIC 9(2).                                    
000600*                                 FÖRETAGSID EKONOM REDOVISNING           
000700     03 REQU-IDRT-KEY        PIC X(3).                                    
000800*                                 RETURTERMINAL                           
000900     03 REQU-IDDISTR-KEY     PIC 9(4).                                    
001000*                                 DISTRIKTNUMMER                          
001100     03 REQU-IDRT-IN-KEY     PIC X(3).                                    
001200*                                 RETURTERMINAL                           
001300     03 REQU-IDRTLOP-IN-KEY  PIC 9(3).                                    
001400*                                 RETUR TERMINAL LÖPNUMMER                
001500     03 REQU-IDANSTNR        PIC 9(5).                                    
001600*                                 ANSTÄLLNINGSNUMMER                      
001700     03 REQU-KVRADER         PIC 9(5).                                    
001800*                                 ANTAL RADER                             
001900     03 REQU-INPUT.                                                       
002000*                                 INMATNINGSFÄLT                          
002100        05 REQU-INPUT-RAD    OCCURS 14 TIMES.                             
002200*                                 INMATNINGSFÄLT                          
002300           07 REQU-IDKUNDNR  PIC 9(6).                                    
002400*                                 KUNDNUMMER                              
002500           07 REQU-IDRAPPNR  PIC 9(7).                                    
002600*                                 RAPPORT NUMMER                          
002700           07 REQU-KVKOLLI   PIC 9(4).                                    
002800*                                 ANTAL KOLLI                             
002900           07 REQU-TERETNOT  PIC X(20).                                   
003000*                                 FRI NOTERING RETURER                    
003100*** END OF VILMAII-COPY LENGTH= 545 BYTES                                 
