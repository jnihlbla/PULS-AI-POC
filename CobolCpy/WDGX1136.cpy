000100 01  1136-WDGX1136.                                                       
000200*                                 NYA ARTIKLAR FRÅN PV, LV                
000300*                                 FUNKTIONSGRUPP TABELL                   
000400*                                 BEREDARE                                
000500*                                 FYSISK NYCKEL: WDGXKEY                  
000600*                                 (IDFKNGRP + LOWVALUE)                   
000700     03 1136-IDFKNGRP-FOM    PIC S9(5)           COMP-3.                  
000800*                                 FUNKTIONSGRUPP                          
000900*                                 FUNCTION GROUP                          
001000     03 1136-LOWVALUE        PIC X(2).                                    
001100     03 1136-IDFKNGRP-TOM    PIC S9(5)           COMP-3.                  
001200*                                 FUNKTIONSGRUPP                          
001300*                                 FUNCTION GROUP                          
001400     03 1136-IDBERED         PIC S9(3)           COMP-3.                  
001500*                                 BEREDARENUMMER                          
001600     03 FILLER               PIC X(15).                                   
001700*** END COPY WDGX1136C0  LENGTH=25                                        
