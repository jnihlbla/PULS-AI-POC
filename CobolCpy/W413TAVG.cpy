000100 01  TAVG-W413TAVG.                                                       
000200*                                 LÄNKAREA TILL W413TAVG -                
000300*                                 PRELIMINÄR BERÄKNING AV TRANSPO         
000400*                                 RTAVGÅNGSTID                            
000500     03 TAVG-IDGMTREF.                                                    
000600*                                 GODSMOTTAGAREREFERENS                   
000700        05 TAVG-IDDISTR      PIC S9(5)           COMP-3.                  
000800*                                 DISTRIKTNUMMER                          
000900        05 TAVG-IDKUNDNR     PIC S9(7)           COMP-3.                  
001000*                                 KUNDNUMMER                              
001100        05 TAVG-IDKUNDRF-GRP.                                             
001200*                                 KUNDENS REFERENS (ORDERID)              
001300           07 TAVG-IDKUNDRF  PIC X(10).                                   
001400*                                 KUNDENS REFERENS (ORDERID)              
001500           07 TAVG-IDORDNR5-FILLER REDEFINES TAVG-IDKUNDRF.               
001600              09 TAVG-IDORDNR5                                            
001700                             PIC 9(5).                                    
001800*                                 ORDERNUMMER                             
001900              09 FILLER      PIC X(5).                                    
002000           07 TAVG-IDORDNR7-FILLER REDEFINES TAVG-IDKUNDRF.               
002100              09 TAVG-IDORDNR7                                            
002200                             PIC 9(7).                                    
002300*                                 ORDERNUMMER                             
002400              09 FILLER      PIC X(3).                                    
002500     03 TAVG-KDORDKL         PIC S9              COMP-3.                  
002600*                                 ORDERKLASS                              
002700     03 TAVG-KDFRAKT         PIC S9(3)           COMP-3.                  
002800*                                 FRAKTSÄTT DC TILL KUND                  
002900     03 TAVG-IDDC            PIC X(2).                                    
003000*                                 IDENTIFIERARE LAGER                     
003100     03 TAVG-DATRPAVT.                                                    
003200*                                 TRANSPORTAVGÅNGSTIDPUNKT                
003300        05 TAVG-DATRPAVD     PIC 9(8).                                    
003400*                                 TRANSPORTAVGÅNGSDATUM                   
003500        05 TAVG-TIHHMM       PIC S9(5)           COMP-3.                  
003600*                                 KLOCKSLAG (TIMMAR OCH MINUTER)          
003700*** END OF VILMAII-COPY LENGTH= 33 BYTES                                  
