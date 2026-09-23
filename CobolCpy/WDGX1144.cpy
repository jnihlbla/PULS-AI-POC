000100 01  1144-WDGX1144.                                                       
000200*                                 ANTAL ÅR LAGERFÖRING                    
000300*                                 PER FUNKTIONSGRUPP INTERVALL            
000400*                                 FYSISK NYCKEL: KY1144                   
000500*                                 (IDFKNGRF + IDFKNGRT)                   
000600*                                                                         
000700     03 1144-IDFKNGRP-FOM    PIC S9(5)           COMP-3.                  
000800*                                 FUNKTIONSGRUPP-FROM                     
000900*                                 FUNCTION-GROUP FROM                     
001000     03 1144-IDFKNGRP-TOM    PIC S9(5)           COMP-3.                  
001100*                                 FUNKTIONSGRUPP-TOM                      
001200*                                 FUNCTION-GROUP UP TO                    
001300     03 1144-KVAARLF         PIC 9(2).                                    
001400*                                 ANTAL ÅR LAGERFÖRING EFTER EOP          
001500*                                 NO OF YEARS AFTER EOP                   
001600*** END OF VILMAII-COPY LENGTH= 8 BYTES                                   
