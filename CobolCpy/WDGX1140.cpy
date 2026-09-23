000100 01  1140-WDGX1140.                                                       
000200*                                 NYA ARTIKLAR FRÅN PV-CN                 
000300*                                 FUNKTIONSGRUPP TABELL                   
000400*                                 ANSKAFFARE                              
000500*                                 FYSISK NYCKEL: KY1140                   
000600*                                 (IDLANDX2+IDFKNGRF+IDFKNGRT)            
000700     03 1140-IDLANDX2        PIC X(2).                                    
000800*                                 2-STÄLLIG LANDSBETECKNINGSKOD           
000900*                                 2-LETTER CODE FOR COUNTRY               
001000     03 1140-IDFKNGRP-FOM    PIC S9(5)           COMP-3.                  
001100*                                 FUNKTIONSGRUPP-FROM                     
001200*                                 FUNCTION-GROUP FROM                     
001300     03 1140-IDFKNGRP-TOM    PIC S9(5)           COMP-3.                  
001400*                                 FUNKTIONSGRUPP-TOM                      
001500*                                 FUNCTION-GROUP UP TO                    
001600     03 1140-IDANSK          PIC S9(3)           COMP-3.                  
001700*                                 ANSKAFFARNUMMER                         
001800*                                 PROCURER NO.                            
001900*** END OF VILMAII-COPY LENGTH= 10 BYTES                                  
