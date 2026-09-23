000100 01  4122-WDGX4122.                                                       
000200*                                 ÅTERSTART RETURTILLST/KNOTA             
000300     03 4122-KDSEGKEY        PIC X.                                       
000400*                                 TEKNISK SEGMENT-NYCKEL                  
000500*                                 TECHNICAL SEGMENT KEY                   
000600     03 4122-KVPOST          PIC S9(7)           COMP-3.                  
000700*                                 RÄKNARE, ANTAL POSTER                   
000800*                                 RECORD COUNTER                          
000900     03 4122-TIUPPDAT        PIC S9(7)           COMP-3.                  
001000*                                 UPPDATERINGSDATUM  (ÅÅMMDD)             
001100*                                 UPDATING DATE     (YYMMDD)              
001200     03 4122-TIUPPTID        PIC S9(9)           COMP-3.                  
001300*                                 UPPDATERINGSTID  (TTMMSSTH)             
001400*                                 UPDATING TIME    (HHMMSSTH)             
001500     03 4122-IDLEVANM.                                                    
001600*                                 LEVERANSANMÄRKNINGSIDENTITET            
001700*                                 DISCREPANCY REPORT IDENTITY             
001800        05 4122-IDDISTR      PIC S9(5)           COMP-3.                  
001900*                                 DISTRIKTNUMMER                          
002000*                                 DISTRICT NUMBER                         
002100        05 4122-IDKUNDNR     PIC S9(7)           COMP-3.                  
002200*                                 KUNDNUMMER                              
002300*                                 CUSTOMER NO                             
002400        05 4122-IDRAPPNR     PIC 9(7).                                    
002500*                                 RAPPORT NUMMER                          
002600*                                 DISCREPANCY REPORT NUMBER               
002700     03 FILLER               PIC X(52).                                   
002800*** END OF VILMAII-COPY LENGTH= 80 BYTES                                  
