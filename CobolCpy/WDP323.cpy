000100 01  IFKN-WDP323.                                                         
000200*                                 PERSONKODSREGISTER                      
000300*                                 FUNKTIONSINTERVALL PER IDPERSON         
000400*                                 FYSISK NYCKEL WDP323KY:                 
000500*                                 (IDLANDX2 + IDFKNGRP-FOM                
000600*                                  IDFKNGRP-TOM)                          
000700     03 IFKN-IDLANDX2        PIC X(2).                                    
000800*                                 2-STÄLLIG LANDSBETECKNINGSKOD           
000900     03 IFKN-IDFKNGRP-FOM    PIC S9(5)           COMP-3.                  
001000*                                 FUNKTIONSGRUPP-FROM                     
001100     03 IFKN-IDFKNGRP-TOM    PIC S9(5)           COMP-3.                  
001200*                                 FUNKTIONSGRUPP-TOM                      
001300*** END OF VILMAII-COPY LENGTH= 8 BYTES                                   
