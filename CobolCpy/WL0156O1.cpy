000100 01  RESP-WL0156O1.                                                       
000200*                                 RESPONS TO PGM WL0156                   
000300     03 RESP-IDDC-KEY        PIC X(2).                                    
000400*                                 IDENTIFIERARE LAGER                     
000500     03 RESP-IDDISTR-KEY     PIC Z(3)9.                                   
000600*                                 DISTRIKTNUMMER                          
000700     03 RESP-IDRT-IN-KEY     PIC X(3).                                    
000800*                                 RETURTERMINAL                           
000900     03 RESP-IDRTLOP-IN-KEY  PIC 9(3).                                    
001000*                                 RETUR TERMINAL LÖPNUMMER                
001100     03 RESP-IDANSTNR        PIC Z(4)9.                                   
001200*                                 ANSTÄLLNINGSNUMMER                      
001300     03 RESP-IDDC-RET-UT     PIC X(2).                                    
001400*                                 MOTTAGANDE LAGER FÖR RETURER            
001500     03 RESP-IDRT            PIC X(3).                                    
001600*                                 RETURTERMINAL                           
001700     03 RESP-IDRTLOP         PIC 9(3).                                    
001800*                                 RETUR TERMINAL LÖPNUMMER                
001900     03 RESP-KVRADER         PIC 9(5).                                    
002000*                                 ANTAL RADER                             
002100     03 RESP-INPUT.                                                       
002200*                                                                         
002300        05 RESP-RADER        OCCURS 14 TIMES.                             
002400*                                                                         
002500           07 RESP-IDKUNDNR  PIC Z(5)9.                                   
002600*                                 KUNDNUMMER                              
002700           07 RESP-IDRAPPNR  PIC Z(6)9.                                   
002800*                                 RAPPORT NUMMER                          
002900           07 RESP-KVKOLLI   PIC Z(3)9.                                   
003000*                                 ANTAL KOLLI                             
003100           07 RESP-TERETNOT  PIC X(20).                                   
003200*                                 FRI NOTERING RETURER                    
003300           07 RESP-IDDC-RET  PIC X(2).                                    
003400*                                 MOTTAGANDE LAGER FÖR RETURER            
003500           07 RESP-IDMSG-ERROR-LINE                                       
003600                             PIC X(3).                                    
003700*                                 FELMEDDELANDE ID                        
003800           07 RESP-MESSAGE-LINE                                           
003900                             PIC X(100).                                  
004000*                                 MEDDELANDE                              
004100*** END OF VILMAII-COPY LENGTH= 2018 BYTES                                
