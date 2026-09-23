000100 01  W351VR.                                                              
000200*                                 RECORD FÖR SYSTEM: VR ÅT RS.            
000300*                                 KVLEVART ANGER LEVERERAT ANTAL          
000400*                                 UNDER DE FYRA SENASTE 4-VECKORS         
000500*                                 PERIODERNA                              
000600     03 IDART.                                                            
000700*                                 ARTIKELIDENTITET EFTERMARKNAD           
000800*                                 PARTIDENTITY AFTERMARKET                
000900        05 IDARTPRE          PIC X(3).                                    
001000*                                 IDENTIFIERARE ARTIKELSORTIMENT          
001100*                                 PARTS RANGE IDENTIFIER                  
001200        05 IDARTBET          PIC X(17).                                   
001300*                                 ARTIKELBETECKNING EFTERMARKNAD          
001400*                                 AFTERMARKET PARTNUMBER                  
001500     03 IDAIGRP.                                                          
001600*                                 ARTIKEL/LAGERINFO GRUPP                 
001700*                                 PARTS LOCATOR GROUP                     
001800        05 IDLANDX2          PIC X(2).                                    
001900*                                 2-STÄLLIG LANDSBETECKNINGSKOD           
002000*                                 2-LETTER CODE FOR COUNTRY               
002100        05 KDAIKTYP          PIC X.                                       
002200*                                 KUNDTYP ARTIKEL/LAGER DSP/VR            
002300*                                 CUSTOMERTYPE PARTS LOCATOR              
002400     03 IDKUNDNR             PIC S9(7)           COMP-3.                  
002500*                                 KUNDNUMMER                              
002600*                                 CUSTOMER NO                             
002700     03 KVBEST               PIC S9(7)           COMP-3.                  
002800*                                 BESTÄLLT ANTAL                          
002900     03 KVLS                 PIC S9(7)           COMP-3.                  
003000*                                 LAGERSALDO                              
003100*                                 STOCK BALANCE                           
003200     03 KVROS                PIC S9(7)           COMP-3.                  
003300*                                 RESTORDERSALDO                          
003400*                                 BACKORDER QTY                           
003500     03 KVPROGNFSG           PIC S9(5)V9(2)      COMP-3.                  
003600*                                 FÖRSÄLJNINGSPROGNOS                     
003700     03 KVLEVART             OCCURS 2 TIMES                               
003800                             PIC S9(7)           COMP-3.                  
003900*                                 LEVERERAT ANTAL STYCK                   
004000*                                 DELIVERED QUANTITY                      
004100     03 KVLEVART-INNEV       PIC S9(7)           COMP-3.                  
004200*                                 LEVERERAT ANTAL ARTIKLAR                
004300*                                 INNEVARANDE ÅR                          
004400*                                 NUMBER OF DELIVERED QTY                 
004500*                                 THIS YEAR                               
004600     03 KVLEVART-FOREG       PIC S9(7)           COMP-3.                  
004700*                                 LEVERERAT ANTAL ARTIKLAR                
004800*                                 FÖREGÅENDE ÅR                           
004900*                                 NUMBER OF DELIVERED QTY                 
005000*                                 LAST YEAR                               
005100     03 KVLEVART-F-FOREG     PIC S9(7)           COMP-3.                  
005200*                                 LEVERERAT ANTAL ARTIKLAR                
005300*                                 FÖRRFÖRRA ÅRET                          
005400*                                 NUMBER OF DELIVERED QTY                 
005500*                                 YEAR MINUS 2                            
005600     03 KDSHELFL             PIC 9(2).                                    
005700*                                 HYLLVÄRMARKOD DSP/VR                    
005800*                                 SHELF LIFE CODE DSP/VR                  
005900*** END OF VILMAII-COPY LENGTH= 65 BYTES                                  
