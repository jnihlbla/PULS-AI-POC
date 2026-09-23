000100 01  REQU-WL0162I1.                                                       
000200*                                 REQUEST TO PGM WL0162                   
000300     03 REQU-IDDC-KEY        PIC X(2).                                    
000400*                                 IDENTIFIERARE LAGER                     
000500     03 REQU-IDFTG-KEY       PIC 9(2).                                    
000600*                                 FÖRETAGSID EKONOM REDOVISNING           
000700     03 REQU-IDDISTR-KEY     PIC 9(4).                                    
000800*                                 DISTRIKTNUMMER                          
000900     03 REQU-IDKUNDNR-KEY    PIC 9(6).                                    
001000*                                 KUNDNUMMER                              
001100     03 REQU-IDRAPPNR-KEY    PIC 9(7).                                    
001200*                                 RAPPORT NUMMER                          
001300     03 REQU-IDARTNR-KEY     PIC 9(8).                                    
001400*                                 ARTIKELNUMMER                           
001500     03 REQU-IDRADNR-KEY     PIC 9(4).                                    
001600*                                 RADNUMMER                               
001700     03 REQU-INPUT           OCCURS 3 TIMES.                              
001800*                                 INMATNINGSFÄLT                          
001900        05 REQU-TEANMNOT-REG PIC X(70).                                   
002000*                                 FRI TEXT FRÅN REGISTRERINGEN            
002100        05 REQU-TEANMNOT-DLR PIC X(70).                                   
002200*                                 FRI TEXT FRÅN ADM. TILL DEALER          
002300        05 REQU-TEANMNOT-ADM PIC X(70).                                   
002400*                                 FRI TEXT FRÅN ADMINISTRATION            
002500        05 REQU-TEANMNOT-REM PIC X(70).                                   
002600*                                 FRI TEXT FRÅN REMISSINSTANS             
002700        05 REQU-TEANMNOT-RET PIC X(70).                                   
002800*                                 FRI TEXT FRÅN RETURAVDELNINGEN          
002900*** END OF VILMAII-COPY LENGTH= 1083 BYTES                                
