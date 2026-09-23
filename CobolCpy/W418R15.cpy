000100 01  W418R15.                                                             
000200*                                 R15 LEVERANSANMÄRKNING 2                
000300*                                                                         
000400     03 IDPTYP               PIC X(3).                                    
000500*                                 POSTTYP                                 
000600     03 SORTAREA             PIC X(25).                                   
000700     03 FILLER REDEFINES SORTAREA.                                        
000800        05 IDDISTR           PIC S9(5)           COMP-3.                  
000900*                                 DISTRIKTNUMMER                          
001000        05 IDKUNDNR          PIC S9(7)           COMP-3.                  
001100*                                 KUNDNUMMER                              
001200        05 KDCLAGER          PIC S9              COMP-3.                  
001300*                                 CENTRALLAGERKOD                         
001400        05 IDLEVANM          PIC X(7).                                    
001500*                                 LEVERANSANMÄRKNINGSNUMMER               
001600        05 KDFRAKT           PIC S9(3)           COMP-3.                  
001700*                                 FRAKTSÄTT C1-C2 TILL KUND               
001800        05 FILLER            PIC X(8).                                    
001900     03 TIM-LEVANM           PIC S9(7)           COMP-3.                  
002000*                                 DATUM LEVERANSANMÄRKN. (ÅÅMMDD)         
002100     03 RELANDCO             PIC S9(3)           COMP-3.                  
002200*                                 LANDING COST PROCENT                    
002300     03 PREMBHNT             PIC S9(7)V9(2)      COMP-3.                  
002400*                                 EMBALLAGE & HANTERINGSKOST (KR)         
002500*                                 INKLUDERAR TILLÄGG FÖR DAGORDER         
002600*                                 (SUFKTTILL)                             
002700     03 PRFRAKT              PIC S9(7)V9(2)      COMP-3.                  
002800*                                 FRAKTKOSTNAD (KR)                       
002900     03 PRLEGKST             PIC S9(7)V9(2)      COMP-3.                  
003000*                                 LEGALISERINSKOSTNAD (KR)                
003100     03 PRFOERS              PIC S9(7)V9(2)      COMP-3.                  
003200*                                 FÖRSÄKRINGSPREMIE (KR)                  
003300     03 PREXPKST             PIC S9(7)V9(2)      COMP-3.                  
003400*                                 EXPEDITIONSKOSTNADER (KR)               
003500     03 PRMOMS               PIC S9(7)V9(2)      COMP-3.                  
003600*                                 MERVÄRDESSKATT                          
003700*** END COPY W418W15CC0  LENGTH=64                                        
