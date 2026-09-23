000100 01  SEQD-WDA2D1.                                                         
000200*                                 LEVERANSANMÄRKNING                      
000300*                                 ARTIKEL                                 
000400*                                 FYSISK NYCKEL: WDA2D1KY                 
000500*                                  (IDFTG    + IDARTNR                    
000600*                                   IDLEVANM + IDRADNR                    
000700*                                 SEKUNDÄR NYCKEL: WDA2DSEQ               
000800*                                  (IDFTG + IDARTNR)                      
000900     03 SEQD-IDFTG           PIC 9(2).                                    
001000*                                 FÖRETAGSID EKONOM REDOVISNING           
001100*                                 COMPANY IDENTITY ACCOUNTING             
001200     03 SEQD-IDARTNR         PIC S9(9)           COMP-3.                  
001300*                                 ARTIKELNUMMER                           
001400*                                 PART NUMBER                             
001500     03 SEQD-IDLEVANM.                                                    
001600*                                 LEVERANSANMÄRKNINGSIDENTITET            
001700*                                 DISCREPANCY REPORT IDENTITY             
001800        05 SEQD-IDDISTR      PIC S9(5)           COMP-3.                  
001900*                                 DISTRIKTNUMMER                          
002000*                                 DISTRICT NUMBER                         
002100        05 SEQD-IDKUNDNR     PIC S9(7)           COMP-3.                  
002200*                                 KUNDNUMMER                              
002300*                                 CUSTOMER NO                             
002400        05 SEQD-IDRAPPNR     PIC 9(7).                                    
002500*                                 RAPPORT NUMMER                          
002600*                                 DISCREPANCY REPORT NUMBER               
002700     03 SEQD-IDRADNR         PIC S9(5)           COMP-3.                  
002800*                                 RADNUMMER                               
002900*                                 LINE NO                                 
003000     03 SEQD-KDANMORS        PIC X(2).                                    
003100*                                 ORSAK TILL LEVERANSANMÄRKNING           
003200*                                 DISCREPANCY REPORT REASON CODE          
003300     03 SEQD-KVLEVANM-BEKR   PIC S9(7)           COMP-3.                  
003400*                                 BEKRÄFTAT RETURANTAL                    
003500*** END OF VILMAII-COPY LENGTH= 30 BYTES                                  
