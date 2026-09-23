000100 01  W418712.                                                             
000200*                                 HUVUD KREDITNOTA                        
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
001600     03 IDKNOTNR             PIC S9(7)           COMP-3.                  
001700*                                 KREDITNOTANUMMER                        
001800     03 TILEVANM             PIC S9(7)           COMP-3.                  
001900*                                 DATUM LEVERANSANMÄRKNING                
002000     03 TIRETILL             PIC S9(7)           COMP-3.                  
002100*                                 RETURTILLSTÅNDSDATUM                    
002200     03 TIKNOTA              PIC S9(7)           COMP-3.                  
002300*                                 KREDITNOTADATUM                         
002400     03 IDUSER-ADM           PIC X(8).                                    
002500*                                 ANVÄNDAR-ID ADMINISTRATIV KONTR         
002600     03 BEANST               PIC X(25).                                   
002700*                                 ANSTÄLLDS NAMN                          
002800     03 TILLAEGGSKOSTNADER.                                               
002900        05 PRFOERS           PIC S9(7)V9(2)      COMP-3.                  
003000*                                 FÖRSÄKRINGSPREMIE                       
003100        05 PRFRAKT           PIC S9(7)V9(2)      COMP-3.                  
003200*                                 FRAKTKOSTNAD                            
003300        05 PRLEGKST          PIC S9(7)V9(2)      COMP-3.                  
003400*                                 LEGALISERINSKOSTNAD                     
003500        05 REEMBHNT          PIC S9(2)V9(1)      COMP-3.                  
003600*                                 EMB OCH HANTERINGSKOST (%)              
003700        05 RELANDCO          PIC S9(3)V9(2)      COMP-3.                  
003800*                                 LANDING COST PROCENT                    
003900     03 ATTESTANSVARIGA      OCCURS 5 TIMES.                              
004000        05 IDUSER-GODK       PIC X(8).                                    
004100*                                 ANVÄNDAR-ID GODKÄNNARE                  
004200        05 FILLER            PIC X.                                       
004300        05 BEANST-GODK       PIC X(25).                                   
004400*                                 GODKÄNNARES NAMN                        
004500        05 FILLER            PIC X.                                       
004600        05 TIUPPDAT          PIC 9(6).                                    
004700*                                 UPPDATERINGSDATUM  (ÅÅMMDD)             
004800*** END OF VILMAII-COPY LENGTH= 293 BYTES                                 
