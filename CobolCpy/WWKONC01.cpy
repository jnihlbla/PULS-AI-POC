000010*** EDIT ALLOWED                                                          
000100*                        *************************************84/1        
000200*                        *** ANVÄNDS VID FÖLJANDE NYS-TESTER              
000300*                        ***                                              
000400*                        ***  - KONC01-NYS = SAMTLIGA TP-KONC             
000500*                        ***    FÖR SPECIALPRISER (FILUT)                 
000600*                        ***  - KONC01-NYS-DANMARK =                      
000700*                        ***    TP AV SPECIALPRISER (FILUT)               
000800*                        ***    (EG PACKE PÅ VOLVO-DATA)                  
000900*                        ***  - KONC01-NYS-FINLAND =                      
001000*                        ***    TP AV SPECIALPRISER (FILUT)               
001100*                        *************************************            
001200 01  KONC01-IDKONCNR                PIC 9(3)    COMP-3.                   
001300*                                                                         
001400       88  KONC01-NYS   VALUE  300 330 350 352 355 405 955.               
001500*                                                                         
001600       88  KONC01-NYS-DANMARK        VALUE  300.                          
001700*                                                                         
001800       88  KONC01-NYS-FINLAND        VALUE  330.                          
001900*                                                                         
002000       88  KONC01-NYS-BELGIEN        VALUE  350.                          
002100*                                                                         
002200       88  KONC01-NYS-FRANKR         VALUE  352.                          
002300*                                                                         
002400       88  KONC01-NYS-ITALIEN        VALUE  355.                          
002500*                                                                         
002600       88  KONC01-NYS-SCHWEIZ        VALUE  405.                          
002700*                                                                         
002800       88  KONC01-NYS-AUSTRAL        VALUE  955.                          
002900*** END COPY WWKONC01C0  LENGTH=2     OLD LENGTH=2                        
