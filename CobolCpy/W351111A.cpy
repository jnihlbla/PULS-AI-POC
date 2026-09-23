000100 01  W351111A.                                                            
000200*                                 LAGERSTYRNINGSINFORMATION               
000300*                                 FRÅN INPORTÖRER OCH ÅF.                 
000400*                                 KVLEVART ANGER LEVERERAT ANTAL          
000500*                                 UNDER DE FYRA SENASTE 4-VECKORS         
000600*                                 PERIODERNA                              
000700     03 IDART.                                                            
000800*                                 ARTIKELIDENTITET EFTERMARKNAD           
000900*                                 PARTIDENTITY AFTERMARKET                
001000        05 IDARTPRE          PIC X(3).                                    
001100*                                 IDENTIFIERARE ARTIKELSORTIMENT          
001200*                                 PARTS RANGE IDENTIFIER                  
001300        05 IDARTBET          PIC X(17).                                   
001400*                                 ARTIKELBETECKNING EFTERMARKNAD          
001500*                                 AFTERMARKET PARTNUMBER                  
001600     03 KDAIKTYP             PIC X.                                       
001700*                                 KUNDTYP ARTIKEL/LAGER DSP/VR            
001800*                                 CUSTOMERTYPE PARTS LOCATOR              
001900     03 IDDISTR              PIC S9(5)           COMP-3.                  
002000*                                 DISTRIKTNUMMER                          
002100*                                 DISTRICT NUMBER                         
002200     03 IDKUNDNR             PIC S9(7)           COMP-3.                  
002300*                                 KUNDNUMMER                              
002400*                                 CUSTOMER NO                             
002500     03 KVBEST               PIC S9(7)           COMP-3.                  
002600*                                 BESTÄLLT ANTAL                          
002700     03 KVLS                 PIC S9(7)           COMP-3.                  
002800*                                 LAGERSALDO                              
002900*                                 STOCK BALANCE                           
003000     03 KVROS                PIC S9(7)           COMP-3.                  
003100*                                 RESTORDERSALDO                          
003200*                                 BACKORDER QTY                           
003300     03 KVPROGNFSG           PIC S9(5)V9(2)      COMP-3.                  
003400*                                 FÖRSÄLJNINGSPROGNOS                     
003500     03 KVLEVART             OCCURS 2 TIMES                               
003600                             PIC S9(7)           COMP-3.                  
003700*                                 LEVERERAT ANTAL STYCK                   
003800*                                 DELIVERED QUANTITY                      
003900     03 KVLEVART-INNEV       PIC S9(7)           COMP-3.                  
004000*                                 LEVERERAT ANTAL ARTIKLAR                
004100*                                 INNEVARANDE ÅR                          
004200*                                 NUMBER OF DELIVERED QTY                 
004300*                                 THIS YEAR                               
004400     03 KVLEVART-FOREG       PIC S9(7)           COMP-3.                  
004500*                                 LEVERERAT ANTAL ARTIKLAR                
004600*                                 FÖREGÅENDE ÅR                           
004700*                                 NUMBER OF DELIVERED QTY                 
004800*                                 LAST YEAR                               
004900     03 KVLEVART-F-FOREG     PIC S9(7)           COMP-3.                  
005000*                                 LEVERERAT ANTAL ARTIKLAR                
005100*                                 FÖRRFÖRRA ÅRET                          
005200*                                 NUMBER OF DELIVERED QTY                 
005300*                                 YEAR MINUS 2                            
005400     03 KDSHELFL             PIC 9(2).                                    
005500*                                 HYLLVÄRMARKOD DSP/VR                    
005600*                                 SHELF LIFE CODE DSP/VR                  
005700*** END OF VILMAII-COPY LENGTH= 66 BYTES                                  
