000100 01  W41876A.                                                             
000200*                                 HUVUDPOST RETURTILLSTÅND                
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
001600     03 BEKOPARE-1           PIC X(27).                                   
001700*                                 DEL AV KÖPARNAMN                        
001800     03 BEKOPARE-2           PIC X(27).                                   
001900*                                 DEL AV KÖPARNAMN                        
002000     03 ADKOPARE-1           PIC X(27).                                   
002100*                                 DEL AV KÖPARADRESS                      
002200     03 ADKOPARE-2           PIC X(27).                                   
002300*                                 DEL AV KÖPARADRESS                      
002400     03 DATUM                PIC S9(7)           COMP-3.                  
002500     03 PRFOERS              PIC S9(7)V9(2)      COMP-3.                  
002600*                                 FÖRSÄKRINGSPREMIE                       
002700     03 PRFRAKT              PIC S9(7)V9(2)      COMP-3.                  
002800*                                 FRAKTKOSTNAD                            
002900     03 PRLEGKST             PIC S9(7)V9(2)      COMP-3.                  
003000*                                 LEGALISERINSKOSTNAD                     
003100     03 REEMBHNT             PIC S9(2)V9(1)      COMP-3.                  
003200*                                 EMB OCH HANTERINGSKOST (%)              
003300     03 RELANDCO             PIC S9(3)V9(2)      COMP-3.                  
003400*                                 LANDING COST PROCENT                    
003500     03 TILEVANM             PIC S9(7)           COMP-3.                  
003600*                                 DATUM LEVERANSANMÄRKNING                
003700*** END OF VILMAII-COPY LENGTH= 155 BYTES                                 
