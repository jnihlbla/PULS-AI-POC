000100 01  RESP-WL0150O1.                                                       
000200*                                 RESPONS FROM PGM WL0150                 
000300     03 RESP-IDDC-KEY        PIC X(2).                                    
000400*                                 IDENTIFIERARE LAGER                     
000500     03 RESP-IDRT-KEY        PIC X(3).                                    
000600*                                 RETURTERMINAL                           
000700     03 RESP-IDRTLOP-KEY     PIC 9(3).                                    
000800*                                 RETUR TERMINAL LÖPNUMMER                
000900     03 RESP-IDKOLLI-KEY     PIC Z(5).                                    
001000*                                 KOLLINUMMER                             
001100     03 RESP-IDDISTR-KEY     PIC Z(4).                                    
001200*                                 DISTRIKTNUMMER                          
001300     03 RESP-IDKUNDNR-KEY    PIC Z(6).                                    
001400*                                 KUNDNUMMER                              
001500     03 RESP-IDRAPPNR-KEY    PIC Z(7).                                    
001600*                                 RAPPORT NUMMER                          
001700     03 RESP-WL0154I1.                                                    
001800        05 RESP-IDPGM-REP    PIC X(8).                                    
001900*                                 PROGRAM IDENTITET                       
002000        05 RESP-L154-RT-POST OCCURS 10 TIMES.                             
002100           07 RESP-IDDISTR-REP                                            
002200                             PIC Z(3)9.                                   
002300*                                 DISTRIKTNUMMER                          
002400           07 RESP-IDKUNDNR-REP                                           
002500                             PIC Z(5)9.                                   
002600*                                 KUNDNUMMER                              
002700           07 RESP-IDRAPPNR-REP                                           
002800                             PIC Z(6)9.                                   
002900*                                 RAPPORT NUMMER                          
003000     03 RESP-KVRADER-MAX1    PIC 9(5).                                    
003100*                                 MAX INDEX KOPPLAT TILL OCCURS N         
003200*                                 EDAN.                                   
003300     03 RESP-INPUT.                                                       
003400*                                                                         
003500        05 RESP-RADER        OCCURS 1 TO 500 TIMES                        
003600                             DEPENDING ON RESP-KVRADER-MAX1.              
003700*                                                                         
003800           07 RESP-KDCMD     PIC X(4).                                    
003900           07 RESP-IDRETSND.                                              
004000*                                 RETURSÄNDNING                           
004100              09 RESP-IDRT   PIC X(3).                                    
004200*                                 RETURTERMINAL                           
004300              09 RESP-IDRTLOP                                             
004400                             PIC Z(2)9.                                   
004500*                                 RETUR TERMINAL LÖPNUMMER                
004600           07 RESP-IDKOLLI   PIC Z(4)9.                                   
004700*                                 KOLLINUMMER                             
004800           07 RESP-IDDISTR   PIC Z(3)9.                                   
004900*                                 DISTRIKTNUMMER                          
005000           07 RESP-IDKUNDNR  PIC Z(5)9.                                   
005100*                                 KUNDNUMMER                              
005200           07 RESP-IDRAPPNR  PIC Z(6)9.                                   
005300*                                 RAPPORT NUMMER                          
005400           07 RESP-FLFARLIG  PIC X.                                       
005500*                                 FARLIGT GODS-FLAGGA                     
005600           07 RESP-TERETNOT  PIC X(20).                                   
005700*                                 FRI NOTERING RETURER                    
005800           07 RESP-KDLEVANM  PIC X.                                       
005900*                                 STATUS LEVERANSANMÄRKNING               
006000           07 RESP-FLKLAR    PIC X.                                       
006100*                                 AVSLUTNINGSMARKERING                    
006200           07 RESP-IDMSG-ERROR-LINE                                       
006300                             PIC X(3).                                    
006400*                                 FELMEDDELANDE ID                        
006500*** END OF VILMAII-COPY LENGTH= 29213 BYTES                               
