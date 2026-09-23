000100 01  4103-WDGX4103.                                                       
000200*                                 ATTEST AV KREDITNOTA                    
000300*                                 WDR5                                    
000400*                                 FYSISK NYCKEL: WDGXKEY                  
000500*                                 (IDHTYP + IDLEVANM + LOW-VALUE)         
000600     03 4103-IDHTYP          PIC X(4).                                    
000700*                                 HÄNDELSETYP                             
000800     03 4103-IDLEVANM.                                                    
000900*                                 LEVERANSANMÄRKNINGSIDENTITET            
001000*                                 DISCREPANCY REPORT IDENTITY             
001100        05 4103-IDDISTR      PIC S9(5)           COMP-3.                  
001200*                                 DISTRIKTNUMMER                          
001300*                                 DISTRICT NUMBER                         
001400        05 4103-IDKUNDNR     PIC S9(7)           COMP-3.                  
001500*                                 KUNDNUMMER                              
001600*                                 CUSTOMER NO                             
001700        05 4103-IDRAPPNR     PIC 9(7).                                    
001800*                                 RAPPORT NUMMER                          
001900*                                 DISCREPANCY REPORT NUMBER               
002000     03 4103-LOW-VALUE       PIC X(12).                                   
002100*** END OF VILMAII-COPY LENGTH= 30 BYTES                                  
