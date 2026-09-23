000100 01  PROGRP-TAB.                                                          
000200*                                 INNEH≈LLER KONSTANTER PER PRODU         
000300*                                 KTSLAG OCH                              
000400*                                 MARKNAD SOM BESKRIVER HUR STOR          
000500*                                 DEL AV RESP                             
000600*                                 PRODUKTSLAG SOM SKALL TILLFALLA         
000700*                                  RESPEKTIVE                             
000800*                                 PRODUKTGRUPP VID EN OMVANDLING          
000900*                                 FR≈N PRODUKT-                           
001000*                                 SLAG TILL PRODUKTGRUPP                  
001100*                                                                         
001200     03 KDPRODKT             PIC S9(3)           COMP-3.                  
001300*                                 PRODUKTKOD                              
001400     03 KDMARK-BUDG          PIC S9(3)           COMP-3.                  
001500*                                 MARKNADSKOD BUDGET 96 MARKNADER         
001600     03 TIAA                 PIC S9(3)           COMP-3.                  
001700*                                 ≈R    (≈≈)                              
001800     03 KDPLATYP             PIC X(3).                                    
001900*                                 PLANERINGSTYP BUDGET, PROGNOS,          
002000*                                 UTFALL                                  
002100     03 PROGRP-FORD-VCC      PIC S9V9(4)         COMP-3.                  
002200*                                 F÷RDELNINGSNYCKLAR PRODUKTKOD           
002300     03 PROGRP-FORD-VTC      PIC S9V9(4)         COMP-3.                  
002400*                                 F÷RDELNINGSNYCKLAR PRODUKTKOD           
002500     03 PROGRP-FORD-BUSS     PIC S9V9(4)         COMP-3.                  
002600*                                 F÷RDELNINGSNYCKLAR PRODUKTKOD           
002700*** END COPY PROGRPCCC0  LENGTH=18                                        
