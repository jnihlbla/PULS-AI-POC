000100 01  RESP-WL0162O1.                                                       
000200*                                 RESPONS FROM PGM WL0162                 
000300     03 RESP-IDDC-KEY        PIC X(2).                                    
000400*                                 IDENTIFIERARE LAGER                     
000500     03 RESP-IDDISTR-KEY     PIC Z(3)9.                                   
000600*                                 DISTRIKTNUMMER                          
000700     03 RESP-IDKUNDNR-KEY    PIC Z(5)9.                                   
000800*                                 KUNDNUMMER                              
000900     03 RESP-IDRAPPNR-KEY    PIC Z(6)9.                                   
001000*                                 RAPPORT NUMMER                          
001100     03 RESP-IDARTNR-KEY     PIC Z(7)9.                                   
001200*                                 ARTIKELNUMMER                           
001300     03 RESP-IDRADNR-KEY     PIC Z(3)9.                                   
001400*                                 RADNUMMER                               
001500     03 RESP-INPUT           OCCURS 3 TIMES.                              
001600*                                                                         
001700        05 RESP-TEANMNOT-REG PIC X(70).                                   
001800*                                 FRI TEXT FRÅN REGISTRERINGEN            
001900        05 RESP-TEANMNOT-DLR PIC X(70).                                   
002000*                                 FRI TEXT FRÅN ADM. TILL DEALER          
002100        05 RESP-TEANMNOT-ADM PIC X(70).                                   
002200*                                 FRI TEXT FRÅN ADMINISTRATION            
002300        05 RESP-TEANMNOT-REM PIC X(70).                                   
002400*                                 FRI TEXT FRÅN REMISSINSTANS             
002500        05 RESP-TEANMNOT-RET PIC X(70).                                   
002600*                                 FRI TEXT FRÅN RETURAVDELNINGEN          
002700*** END OF VILMAII-COPY LENGTH= 1081 BYTES                                
