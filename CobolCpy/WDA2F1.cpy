000100 01  SEQF-WDA2F1.                                                         
000200*                                 LEVERANSANMÄRKNING                      
000300*                                 KREDITNOT NUMMER                        
000400*                                 FYSISK NYCKEL: WDA2F1KY                 
000500*                                  (IDKNOTNR            +                 
000600*                                   IDLEVANM + IDARTNR  +                 
000700*                                   IDRADNR)                              
000800*                                 SEKUNDÄR NYCKEL: WDA2FSEQ               
000900*                                   IDKNOTNR                              
001000     03 SEQF-IDKNOTNR        PIC S9(7)           COMP-3.                  
001100*                                 KREDITNOTANUMMER                        
001200*                                 CREDIT NOTE NUMBER                      
001300     03 SEQF-IDLEVANM.                                                    
001400*                                 LEVERANSANMÄRKNINGSIDENTITET            
001500*                                 DISCREPANCY REPORT IDENTITY             
001600        05 SEQF-IDDISTR      PIC S9(5)           COMP-3.                  
001700*                                 DISTRIKTNUMMER                          
001800*                                 DISTRICT NUMBER                         
001900        05 SEQF-IDKUNDNR     PIC S9(7)           COMP-3.                  
002000*                                 KUNDNUMMER                              
002100*                                 CUSTOMER NO                             
002200        05 SEQF-IDRAPPNR     PIC 9(7).                                    
002300*                                 RAPPORT NUMMER                          
002400*                                 DISCREPANCY REPORT NUMBER               
002500     03 SEQF-IDARTNR         PIC S9(9)           COMP-3.                  
002600*                                 ARTIKELNUMMER                           
002700*                                 PART NUMBER                             
002800     03 SEQF-IDRADNR         PIC S9(5)           COMP-3.                  
002900*                                 RADNUMMER                               
003000*                                 LINE NO                                 
003100     03 SEQF-DALEVANM        PIC 9(8).                                    
003200*                                 DATUM LEV.ANMÄRKNING(YYYYMMDD)          
003300*                                 DISCREPANCY REPORT DATE                 
003400*** END OF VILMAII-COPY LENGTH= 34 BYTES                                  
