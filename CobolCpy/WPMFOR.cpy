000010*** EDIT ALLOWED                                                          
000100 01  WPMFORC0.                                                            
000200*                            INNEHÅLLER KONSTANTER PER PRODUKT-           
000300*                            SLAG OCH MARKNAD SOM BESKRIVER HUR           
000400*                            STOR DEL AV RESP PRODUKTSLAG SOM             
000500*                            SKA TILLFALLA RESP PRODUKTGRUPP              
000600*                            VID EN OMVANDLING FRÅN PRODUKTSLAG           
000700*                            TILL PRODUKTGRUPP                            
000800*                                                                         
000900     10 KDPRODSL             PIC S9(3)         COMP-3.                    
001000*                                PRODUKTSLAG                              
001100     10 IDKOMARK             PIC S9(3)         COMP-3.                    
001200*                                MARKNADSKOD FÖR VEGA                     
001300     10 PROGRP-FORD-VCC      PIC S9V9(4)       COMP-3.                    
001400*                                FÖRDELNINGSNYCKLAR PRODUKTION            
001500     10 PROGRP-FORD-VTC      PIC S9V9(4)       COMP-3.                    
001600*                                FÖRDELNINGSNYCKLAR PRODUKTION            
001700     10 PROGRP-FORD-BUSS     PIC S9V9(4)       COMP-3.                    
001800*                                FÖRDELNINGSNYCKLAR PRODUKTION            
001900*** END COPY WPMFORCCC0  LENGTH=13    OLD LENGTH=                         
