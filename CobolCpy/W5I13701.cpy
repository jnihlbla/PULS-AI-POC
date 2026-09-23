000100 01  MID-W5I13701.                                                        
000200     03 MID-IDFKNGRP-IN      PIC X(4).                                    
000300*                                 FUNCTION GROUP                          
000400     03 MID-KDCMD            PIC X.                                       
000500*                                 LINE UPDATE COMMAND                     
000600     03 MID-INPUT.                                                        
000700        05 MID-IDFKNGRP      PIC X(4).                                    
000800*                                 FUNCTION GROUP                          
000900        05 MID-RELANDCO-EOCF-FROM                                         
001000                             PIC X(7).                                    
001100*                                 LANDING COST/FG EO AFTER TITULF         
001200        05 MID-RELANDCO-EITX-FROM                                         
001300                             PIC X(7).                                    
001400*                                 LANDING COST/FG EI AFTER TITULF         
001500        05 MID-RELANDCO-EGTX-FROM                                         
001600                             PIC X(7).                                    
001700*                                 LANDING COST/FG EG AFTER TITULF         
001800        05 MID-TILANDCO      PIC 9(6).                                    
001900*                                 START DATE LANDING COST FACTOR          
002000*** END OF VILMAII-COPY LENGTH= 36 BYTES                                  
