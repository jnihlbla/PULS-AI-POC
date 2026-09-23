000100 01  4012-WDGX4012.                                                       
000200*                                 TEMPORÄR LAGRING AV PLOCKSATSER         
000300*                                 FYSISK NYCKEL: KDSEGKEY                 
000400     03 4012-KDSEGKEY        PIC X.                                       
000500*                                 TEKNISK SEGMENT-NYCKEL                  
000600*                                 TECHNICAL SEGMENT KEY                   
000700     03 4012-IXHEL           PIC S9(9)           COMP.                    
000800*                                 INDEX HELORD                            
000900*                                 INDEX FULLWORD                          
001000     03 4012-ORDDEL          OCCURS 99 TIMES.                             
001100        05 4012-IDORDER      PIC S9(7)           COMP-3.                  
001200*                                 VOLVO PARTS ORDERNUMMER                 
001300*                                 VOLVO PARTS ORDER NUMBER                
001400        05 4012-IDDC         PIC X(2).                                    
001500*                                 IDENTIFIERARE LAGER                     
001600*                                 WAREHOUSE IDENTIFIER                    
001700        05 4012-IDPRODNR     PIC S9(7)           COMP-3.                  
001800*                                 PRODUKTIONSNUMMER                       
001900*                                 PRODUCTION NUMBER                       
002000        05 4012-IDPLKLST     PIC S9(3)           COMP-3.                  
002100*                                 PLOCKLISTNUMMER                         
002200*                                 PICKING LIST NUMBER                     
002300*** END OF VILMAII-COPY LENGTH= 1193 BYTES                                
