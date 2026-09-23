000100 01  W418717.                                                             
000200*                                 HUVUD RETURTILLSTÅND                    
000300*                                                                         
000400     03 IDPTYP               PIC X(3).                                    
000500*                                 POSTTYP                                 
000600     03 IDLEVANM.                                                         
000700*                                 LEVERANSANMÄRKNINGSIDENTITET            
000800        05 IDDISTR           PIC S9(5)           COMP-3.                  
000900*                                 DISTRIKTNUMMER                          
001000        05 IDKUNDNR          PIC S9(7)           COMP-3.                  
001100*                                 KUNDNUMMER                              
001200        05 IDRAPPNR          PIC 9(7).                                    
001300*                                 RAPPORT NUMMER                          
001400     03 IDDC                 PIC X(2).                                    
001500*                                 IDENTIFIERARE LAGER                     
001600     03 REEMBHNT             PIC S9(2)V9(1)      COMP-3.                  
001700*                                 EMB OCH HANTERINGSKOST (%)              
001800     03 PRFRAKT              PIC S9(7)V9(2)      COMP-3.                  
001900*                                 FRAKTKOSTNAD                            
002000     03 PRLEGKST             PIC S9(7)V9(2)      COMP-3.                  
002100*                                 LEGALISERINSKOSTNAD                     
002200     03 PRFOERS              PIC S9(7)V9(2)      COMP-3.                  
002300*                                 FÖRSÄKRINGSPREMIE                       
002400     03 RELANDCO             PIC S9(3)V9(2)      COMP-3.                  
002500*                                 LANDING COST PROCENT                    
002600     03 TILEVANM             PIC S9(7)           COMP-3.                  
002700*                                 DATUM LEVERANSANMÄRKNING                
002800*** END OF VILMAII-COPY LENGTH= 43 BYTES                                  
