000100 01  W225LI02.                                                            
000200*                                 LISTRECORD FÖR RANKING                  
000300*                                                                         
000400     03 SORTARGUMENT.                                                     
000500        05 IDARTNR           PIC S9(9)           COMP-3.                  
000600*                                 ARTIKELNUMMER                           
000700        05 KDCLAGER          PIC S9              COMP-3.                  
000800*                                 CENTRALLAGERKOD                         
000900        05 SUROBEL           PIC S9(7)V9(2)      COMP-3.                  
001000*                                 RESTORDERVÄRDE STANDARDPRIS             
001100        05 KVRORAD           PIC S9(7)V9(2)      COMP-3.                  
001200*                                 RESTNOTERADE RADER  KVRORAD-003         
001300        05 TIRODAT-ORDER     PIC S9(5)           COMP-3.                  
001400*                                               TIRODAT-ORDER-002         
001500*                                 ÄLDSTA RESTORDERDATUM (AAVVD)           
001600     03 RANKING-INFO.                                                     
001700        05 IDLEVNR           PIC X(5).                                    
001800*                                 LEVERANTÖRNUMMER                        
001900        05 KDPRIO            PIC S9              COMP-3.                  
002000*                                 PRIORITETSKOD                           
002100        05 FLTOPP            PIC X.                                       
002200*                                 TOPP-200-ARTIKEL                        
002300        05 IDANSK            PIC S9(3)           COMP-3.                  
002400*                                 ANSKAFFARNUMMER                         
002500        05 KVROS             PIC S9(7)           COMP-3.                  
002600*                                 RESTORDERSALDO                          
002700        05 SUAKBEL           PIC S9(7)V9(2)      COMP-3.                  
002800*                                 AK-VÄRDE                                
002900        05 KVAKS-CDC         PIC S9(7)           COMP-3.                  
003000*                                 DEL AV AK SOM LIGGER I CDC              
003100        05 KVAKS-PAV         PIC S9(7)           COMP-3.                  
003200*                                 DEL AV AK PÅ VÄG                        
003300        05 KVAKS-T           PIC S9(7)           COMP-3.                  
003400*                                 DEL AV AK I EN TERMINAL                 
003500*** END OF VILMAII-COPY LENGTH= 49 BYTES                                  
